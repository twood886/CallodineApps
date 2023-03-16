library(tidyverse)
library(lubridate)
library(gt)
library(shiny)
library(gtExtras)
library(plotly)
library(tidyquant)

source("Functions.R")
source("Functions_Intraday.R")

.tq_get_return <- function(x, get = "stock.prices", complete_cases = T, from, to){
  tidyquant::tq_get(
    x = x,
    get = get,
    complete_cases = complete_cases,
    from = as.character(as.Date(from) - 4),
    to = as.character(as.Date(to) + 4)) %>%
    mutate(`return.daily` = lag(lead(`adjusted`, n = 1)/ `adjusted` - 1, n = 1, default =0)) %>%
    filter(
      `date`>= as.Date(from),
      `date` <= as.Date(to)) %>%
    mutate(`return.cumulative` = cumprod(1 + `return.daily`) - 1)}


setDateButton <- function(x){
  date.start <- case_when(
    x == "MTD" ~ as.character(floor_date(Sys.Date() -1, "month")),
    x == "1M" ~ as.character(floor_date((Sys.Date()-1) %m-% months(1) + 1)),
    x == "QTD" ~ as.character(floor_date(Sys.Date()-1, "quarter")),
    x == "YTD" ~ as.character(floor_date((Sys.Date()-1), "year")),
    x == "1Y" ~ as.character(floor_date((Sys.Date() -1) %m-% years(1) + 1)),
    x == "ITD" ~ as.character(as.Date("2018-07-02")))
  return(as.Date(date.start))}

shinyServer(function(input, output, session) {

  ##### Intra-day Module
  # Set Timer for Auto Data Updates
  timer.min <- reactiveTimer(120000)
  timer.10min <- reactiveTimer(600000)

  # Import Holdings for Intra-day
  data.holdings <- reactive({
    timer.10min()
    getCCMFholdings()})

  # Calculate Returns every 2 Min for Intra-day
  data.returns <- reactive({
    timer.min()
    calcReturnsIntraday(data.holdings())})

  data.spx <- reactive({
    timer.min()
    calcSecReturnIntraday("SPY")})

  # Generate Plot for Intra-day Return
  output$idayPlot <- renderPlotly({
    data.returns() %>%
      select(`symbol.yahoo`, `return.intraday`) %>%
      unnest(cols = c(`symbol.yahoo`, `return.intraday`)) %>%
      ungroup() %>%
      select(`symbol.yahoo`, `date`, `contr`) %>%
      complete(`symbol.yahoo`, `date`) %>%
      group_by(`symbol.yahoo`) %>%
      arrange(`date`) %>%
      fill(`contr`, .direction = "down") %>%
      group_by(`date`) %>%
      summarize(`return` = sum(`contr`), .groups = "drop") %>%
      arrange(`date`) %>%
      slice(-1) %>%
      add_row(
        `date` = as.POSIXct(paste(Sys.Date(), "09:29:00")),
        `return` = 0,
        .before = 1) %>%
      plot_ly() %>%
      add_trace(
        x = ~ `date`,
        y = ~ `return`,
        name = "CCMF Intraday Return",
        type = "scatter",
        mode = "lines",
        line = list(color = "rgb(0,47,86)"),
        text) %>%
      add_trace(
        data = data.spx(),
        x = ~ `date`,
        y = ~ `return`,
        name = "S&P 500 Return",
        type = "scatter",
        mode = "lines",
        line = list(color = "rgb(44,132,134)"),
        text) %>%
      layout(
        yaxis = list(
          title = "",
          tickformat = ".2%",
          hoverformat = ".2%"),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = .5,
          yanchor = "top",
          y = 100,
          automargin = T))})

  # Generate Long/Short Sector Contribution Table
  output$idayTableLS <- gt::render_gt({
    data.returns() %>%
      select(`symbol.yahoo`,`sector`,`NMV.start`, `return.intraday`) %>%
      unnest(cols = c(`symbol.yahoo`, `sector`, `NMV.start`, `return.intraday`)) %>%
      ungroup() %>%
      complete(`symbol.yahoo`, `date`) %>%
      fill(everything(), .direction = "down") %>%
      filter(`date` == max(`date`)) %>%
      mutate(`L/S` = ifelse(`NMV.start` > 0, "Long", "Short")) %>%
      mutate(`contr` = `contr` * 10000) %>%
      group_by(`sector`, `L/S`) %>%
      summarise(`contr` = sum(`contr`, na.rm = T), .groups = "drop") %>%
      pivot_wider(names_from = `L/S`, values_from = `contr`) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(`Total` = sum(`Long`, `Short`, na.rm=T)) %>%
      rename(`Sector` = `sector`) %>%
      gt() %>%
      fmt_number(columns = c(`Long`, `Short`, `Total`), decimals = 0) %>%
      grand_summary_rows(
        columns = c(`Long`, `Short`, `Total`),
        fns = list(`Total` = ~sum(., na.rm = T)),
        formatter = fmt_number,
        decimals = 0) %>%
      gt_theme_538()})

  output$idayTableSecContr <- gt::render_gt({
    data.returns() %>%
      mutate(
        `description` = map_chr(`data`, \(x) first(x$Description)),
        `return` = map_dbl(`return.intraday`, \(x) last(x$return)),
        `contribution` = map_dbl(`return.intraday`, \(x) last(x$contr)),
        `price` = map_dbl(`price.intraday`, \(x) last(x$close)),
        `gl` = map_dbl(`return.intraday`, \(x) last(x$gl))) %>%
      mutate(`weight` = `NMV.start` / `NAV.start`) %>%
      mutate(`contribution` = `contribution` * 10000) %>%
      select(`symbol.yahoo`, `description`, `sector`, `weight`, `price`, `return`, `contribution`) %>%
      group_by(`sector`) %>%
      arrange(`sector`, desc(`contribution`)) %>%
      gt() %>%
      fmt_percent(columns = c(`weight`, `return`, decimals = 0)) %>%
      fmt_currency(columns = c(`price`)) %>%
      fmt_integer(columns = c(`contribution`)) %>%
      cols_label(`symbol.yahoo` = "") %>%
      gt_theme_538() %>%
      data_color(
        columns = c(`contribution`),
        colors = col_numeric(
          c("#B3000C", "#FFFFFF", "#00B32C"),
          domain = c(-20, 0, 20),
          alpha = 1))})


  #####
  # Download Security Daily Files
  load(paste0(Sys.getenv("USERPROFILE"),"\\Callodine Capital Management, LP\\Main - Documents\\TWOOD-Bloomberg\\MSFS_Data.Rda"))


  # Performance Summary Tab ####################################################
  .psumUpdateRange <- function(x, session){
    updateDateRangeInput(
      session,
      "dateRange.psum",
      start = setDateButton(x),
      end = Sys.Date() -1)}

  observeEvent(input$buttonMTD.psum, {.psumUpdateRange("MTD", session)})
  observeEvent(input$button1M.psum, {.psumUpdateRange("1M", session)})
  observeEvent(input$buttonQTD.psum, {.psumUpdateRange("QTD", session)})
  observeEvent(input$buttonYTD.psum, {.psumUpdateRange("YTD", session)})
  observeEvent(input$button1Y.psum, {.psumUpdateRange("1Y", session)})
  observeEvent(input$buttonITD.psum, {.psumUpdateRange("ITD", session)})

  date.start.psum <- reactive(as.character(input$dateRange.psum[[1]]))
  date.end.psum <- reactive(as.character(input$dateRange.psum[[2]]))

  # Calculate Performance
  data.portreturn <- reactive({CalcAggMetrics(data.dailysec, date.start.psum(), date.end.psum())})
  data.lsreturn <- reactive({CalcAggMetrics(data.dailysec, date.start.psum(), date.end.psum(), col.group = "L/S Exp", filter.sec = T)})
  data.gicsreturn <- reactive({CalcAggMetrics(data.dailysec, date.start.psum(), date.end.psum(), col.group = c("Sector (Time Based)"), filter.sec = T)})
  data.lsgicreturn <- reactive({CalcAggMetrics(data.dailysec, date.start.psum(), date.end.psum(), col.group = c("L/S Exp", "Sector (Time Based)"), filter.sec = T)})
  data.spyreturn <- reactive({.tq_get_return(x = "^SP500TR", get = "stock.prices", complete_cases = T, from = date.start.psum(), to = date.end.psum())})
  data.dvyreturn <- reactive({.tq_get_return(x = "dvy",      get = "stock.prices", complete_cases = T, from = date.start.psum(), to = date.end.psum())})

  # Summary Table
  data.tableSummary <- reactive({
    full_join(
      data.portreturn()$summary %>%
        pivot_longer(
          cols = everything(),
          names_to = "metric",
          values_to = "Total"),
      data.lsreturn()$summary %>%
        pivot_longer(
          cols = c(everything(), -`L/S Exp`),
          names_to = "metric",
          values_to = "value") %>%
        pivot_wider(
          names_from = `L/S Exp`,
          values_from = `value`),
      by = "metric")})

  output$downloadtableSummary <- downloadHandler(
    filename = function(){"summarytable.csv"},
    content = function(fname){
      write.csv(data.tableSummary(), fname)})

  output$tableSummary <- gt::render_gt({
    data.tableSummary() %>%
      gt() %>%
      fmt_currency(
        columns = c(everything(), -`metric`),
        rows = `metric` == "P/L $",
        decimals = 0) %>%
      fmt_percent(
        columns = c(everything(), -`metric`),
        rows = `metric` %in%  c("Return" ,"Contribution", "Net Exp % (Avg)", "Gross Exp % (Avg)", "Batting"),
        decimals = 2) %>%
      fmt_integer(
        columns = c(everything(), -`metric`),
        rows = `metric` %in% c("# Win", "# Loss")) %>%
      fmt_number(
        columns = c(everything(), -`metric`),
        rows = `metric` %in% c("Slugging"),
        decimals = 2) %>%
      cols_label(
        `L` = "Long Exp",
        `S` = "Short Exp") %>%
      gt_theme_538()})

  # Plot Return Charts
  output$chartCumRet <- renderPlotly({
    plot_ly() %>%
      add_trace(
        data = data.portreturn()$data,
        x = ~ `date`,
        y = ~ `return.cumulative`,
        name = "Portfolio Monthly Return",
        type = "scatter",
        mode = "lines",
        line = list(color = "rgb(0,47,86)"),
        text) %>%
      add_trace(
        data = data.spyreturn(),
        x = ~ `date`,
        y = ~ `return.cumulative`,
        name = "S&P 500 Return",
        type = "scatter",
        mode = "lines",
        line = list(color = "rgb(44,132,134)"),
        text) %>%
      add_trace(
        data = data.dvyreturn(),
        x = ~ `date`,
        y = ~ `return.cumulative`,
        name = "DVY Return",
        type = "scatter",
        mode = "lines",
        line = list(color = "rgb(165,165,165)"),
        text) %>%
      layout(
        xaxis = list(
          title = ""),
        yaxis = list(
          title = "",
          tickformat = ".1%",
          hoverformat = ".2%"),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = .5,
          yanchor = "top",
          y = 100,
          automargin = T))})
  output$chartWeekReturn <- renderPlotly({
    plot_ly() %>%
      add_trace(
        data = data.portreturn()$data %>%
          group_by(`week` = lubridate::floor_date(`date`, "week")) %>%
          summarize(`return` = prod(1+`return.daily`) -1),
        x = ~ `week`,
        y = ~ `return`,
        name = "Portfolio Monthly Return",
        type = "bar",
        #mode = "lines",
        marker = list(color = "rgb(0,47,86)"),
        text) %>%
      add_trace(
        data = data.spyreturn() %>%
          group_by(`week` = lubridate::floor_date(`date`, "week")) %>%
          summarize(`return` = prod(1+`return.daily`)-1),
        x = ~ `week`,
        y = ~ `return`,
        name = "S&P 500 Return",
        type = "bar",
        #mode = "lines",
        marker = list(color = "rgb(44,132,134)"),
        text) %>%
      add_trace(
        data = data.dvyreturn() %>%
          group_by(`week` = lubridate::floor_date(`date`, "week")) %>%
          summarize(`return` = prod(1+`return.daily`)-1),
        x = ~ `week`,
        y = ~ `return`,
        name = "DVY Return",
        type = "bar",
        #mode = "lines",
        marker = list(color = "rgb(165,165,165)"),
        text) %>%
      layout(
        xaxis = list(
          title = ""
          #dtick = "M1",
          #tickformat="%b<br>%Y"
          ),
        yaxis = list(
          title = "",
          tickformat = ".1%",
          hoverformat = ".2%"),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = .5,
          yanchor = "top",
          y = 100,
          automargin = T))})
  output$chartMonthReturn <- renderPlotly({
    plot_ly() %>%
      add_trace(
        data = data.portreturn()$data %>%
          group_by(`month` = lubridate::floor_date(`date`, "month")) %>%
          summarize(`return` = prod(1+`return.daily`) -1),
        x = ~ `month`,
        y = ~ `return`,
        name = "Portfolio Monthly Return",
        type = "bar",
        #mode = "lines",
        marker = list(color = "rgb(0,47,86)"),
        text) %>%
      add_trace(
        data = data.spyreturn() %>%
          group_by(`month` = lubridate::floor_date(`date`, "month")) %>%
          summarize(`return` = prod(1+`return.daily`)-1),
        x = ~ `month`,
        y = ~ `return`,
        name = "S&P 500 Return",
        type = "bar",
        #mode = "lines",
        marker = list(color = "rgb(44,132,134)"),
        text) %>%
      add_trace(
        data = data.dvyreturn() %>%
          group_by(`month` = lubridate::floor_date(`date`, "month")) %>%
          summarize(`return` = prod(1+`return.daily`)-1),
        x = ~ `month`,
        y = ~ `return`,
        name = "DVY Return",
        type = "bar",
        #mode = "lines",
        marker = list(color = "rgb(165,165,165)"),
        text) %>%
      layout(
        xaxis = list(
          title = "",
          dtick = "M1",
          tickformat="%b<br>%Y"),
        yaxis = list(
          title = "",
          tickformat = ".1%",
          hoverformat = ".2%"),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = .5,
          yanchor = "top",
          y = 100,
          automargin = T))})
  output$chartYearReturn <- renderPlotly({
    plot_ly() %>%
      add_trace(
        data = data.portreturn()$data %>%
          group_by(`year` = lubridate::floor_date(`date`, "year")) %>%
          summarize(`return` = prod(1+`return.daily`) -1),
        x = ~ `year`,
        y = ~ `return`,
        name = "Portfolio Monthly Return",
        type = "bar",
        marker = list(color = "rgb(0,47,86)"),
        text) %>%
      add_trace(
        data = data.spyreturn() %>%
          group_by(`year` = lubridate::floor_date(`date`, "year")) %>%
          summarize(`return` = prod(1+`return.daily`)-1),
        x = ~ `year`,
        y = ~ `return`,
        name = "S&P 500 Return",
        type = "bar",
        marker = list(color = "rgb(44,132,134)"),
        text) %>%
      add_trace(
        data = data.dvyreturn() %>%
          group_by(`year` = lubridate::floor_date(`date`, "year")) %>%
          summarize(`return` = prod(1+`return.daily`)-1),
        x = ~ `year`,
        y = ~ `return`,
        name = "DVY Return",
        type = "bar",
        marker = list(color = "rgb(165,165,165)"),
        text) %>%
      layout(
        xaxis = list(
          title = "",
          #dtick = "Y1",
          tickformat="%Y"),
        yaxis = list(
          title = "",
          tickformat = ".1%",
          hoverformat = ".2%"),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = .5,
          yanchor = "top",
          y = 100,
          automargin = T))})

  # Sector Summary Table
  output$sectableSummary <- gt::render_gt({
    data.gicsreturn()$summary %>%
      gt() %>%
      cols_label(`Sector (Time Based)` = "Sector") %>%
      fmt_currency(
        columns = `P/L $`,
        decimals = 0) %>%
      fmt_percent(
        columns = c(`Return` ,`Contribution`, `Net Exp % (Avg)`, `Gross Exp % (Avg)`, `Batting`),
        decimals = 2) %>%
      fmt_integer(
        columns = c(`# Win`, `# Loss`)) %>%
      fmt_number(
        columns = `Slugging`,
        decimals = 2) %>%
      gt_theme_538() %>%
      tab_style(
        locations = cells_column_labels(columns = everything()),
        style = list(
          cell_borders(sides = "bottom", weight = px(3)),
          cell_text(weight = "bold"))) %>%
      opt_row_striping() %>%
      tab_options(row.striping.background_color = "#dddddd")})

  # Long-Short Contribution Chart
  output$chartLSCont <- plotly::renderPlotly({
    data.lsreturn()$data %>%
      group_by(`L/S Exp`) %>%
      mutate(`contribution.cumulative` = cumsum(`return.contribution.daily.total.time`)) %>%
      select(`date`, `L/S Exp`, `contribution.cumulative`) %>%
      pivot_wider(names_from = `L/S Exp`, values_from = `contribution.cumulative`) %>%
      plot_ly() %>%
      add_trace(
        x = ~ `date`,
        y = ~ `L`,
        name = "Long Portfolio Contribution",
        type = "scatter",
        mode = "lines",
        line = list(color = "rgb(0,47,86)"),
        text)  %>%
      add_trace(
        x = ~ `date`,
        y = ~ `S`,
        name = "Short Portfolio Contribution",
        type = "scatter",
        mode = "lines",
        line = list(color = "rgb(44,132,134)"),
        text) %>%
      layout(
        yaxis = list(
          title = "",
          tickformat = ".1%",
          hoverformat = ".2%"),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = .5,
          yanchor = "top",
          y = 100,
          automargin = T))})

  # Exposure Tab ###############################################################
  # Plot Exposure
  .pexpUpdateRange <- function(x, session){
    updateDateRangeInput(
      session,
      "dateRange.pexp",
      start = setDateButton(x),
      end = Sys.Date() -1)}

  observeEvent(input$buttonMTD.pexp, {.pexpUpdateRange("MTD", session)})
  observeEvent(input$button1M.pexp, {.pexpUpdateRange("1M", session)})
  observeEvent(input$buttonQTD.pexp, {.pexpUpdateRange("QTD", session)})
  observeEvent(input$buttonYTD.pexp, {.pexpUpdateRange("YTD", session)})
  observeEvent(input$button1Y.pexp, {.pexpUpdateRange("1Y", session)})
  observeEvent(input$buttonITD.pexp, {.pexpUpdateRange("ITD", session)})

  date.start.pexp <- reactive(as.character(input$dateRange.pexp[[1]]))
  date.end.pexp <- reactive(as.character(input$dateRange.pexp[[2]]))

  # Calculate Performance
  data.portreturn.pexp <- reactive({CalcAggMetrics(data.dailysec, date.start.pexp(), date.end.pexp())})

  output$chartGrossExp <- renderPlotly({
    data.portreturn.pexp()$data %>%
      select(`date`, `mkt.val.gross.pct`) %>%
      plot_ly() %>%
      add_trace(
        x = ~`date`,
        y = ~`mkt.val.gross.pct`,
        name = "Gross Exposure",
        type = "scatter",
        mode = "lines",
        line = list(color = "rgb(0,47,86)")) %>%
      layout(
        xaxis = list(
          title = ""),
        yaxis = list(
          title = "Gross Exposure",
          overlaying = "y",
          side = "left",
          tickformat = "0%",
          hoverformat = ".1%"),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = .5,
          yanchor = "top",
          y = 100,
          automargin = T))})
  output$chartNetExp <- renderPlotly({
    data.portreturn.pexp()$data %>%
      select(`date`, `mkt.val.net.pct`) %>%
      plot_ly() %>%
      add_trace(
        x = ~`date`,
        y = ~`mkt.val.net.pct`,
        name = "Gross Exposure",
        type = "scatter",
        mode = "lines",
        line = list(color = "rgb(44,132,134)")) %>%
      layout(
        xaxis = list(
          title = ""),
        yaxis = list(
          title = "Net Exposure",
          overlaying = "y",
          side = "left",
          tickformat = "0%",
          hoverformat = ".1%"),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = .5,
          yanchor = "top",
          y = 100,
          automargin = T))})

  # Sector Exposure Tab ########################################################

  data.sectorweightsnap <- reactive({
    AggSec2Group(
      data.dailysec,
      min(input$SectorExpSlider, max(data.dailysec$Date)) - 7,
      min(input$SectorExpSlider, max(data.dailysec$Date)),
      col.group = c("L/S Exp", "Sector (Time Based)"),
      filter.sec = T) %>%
    filter(`date` == max(`date`)) %>%
    group_by(`L/S Exp`) %>%
    mutate(`port.exp` = sum(`mkt.val.gross.pct`)) %>%
    group_by(`L/S Exp`, `Sector (Time Based)`) %>%
    transmute(`Exp` = `mkt.val.gross.pct` / `port.exp`) %>%
    pivot_wider(names_from = `L/S Exp`, values_from = `Exp`)})

  # data.port <-
  #   AggSec2Group(
  #     data.dailysec,
  #     min(data.dailysec$Date),
  #     max(data.dailysec$Date))
  #
  # data.sectorweight <-
  #   AggSec2Group(
  #     data.dailysec,
  #     min(data.dailysec$Date),
  #     max(data.dailysec$Date),
  #     col.group = c("Sector (Time Based)"),
  #     filter.sec = T) %>%
  #
  #   select(`date`, `Sector (Time Based)`, `net.assets.end`) %>%



  output$chartSectorWeightSnapshot <- renderPlotly({
    data.sectorweightsnap() %>%
      plot_ly() %>%
      add_pie(
        labels = ~`Sector (Time Based)`,
        values = ~`L`,
        domain = list(row = 0, column = 0),
        name = "Long Exposure",
        hole = 0.5,
        title = "Long Exposure",
        textinfo = "none",
        hovertemplate = paste('<i>%{label}</i>','<br><b>Weight</b>: %{percent}<br>'),
        marker = list(colors = viridisLite::viridis(13))) %>%
      add_pie(
        labels = ~`Sector (Time Based)`,
        values = ~`S`,
        domain = list(row = 0, column = 1),
        name = "Short Exposure",
        hole = 0.5,
        title = "Short Exposure",
        textinfo = "none",
        hovertemplate = paste('<i>%{label}</i>','<br><b>Weight</b>: %{percent}<br>')) %>%
      layout(
        grid = list(rows = 1, columns = 2),
        legend = list(orientation = "h",
                      xanchor = "center",
                      x = 0.5))})

})
