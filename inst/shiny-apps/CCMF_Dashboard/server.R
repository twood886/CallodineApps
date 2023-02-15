library(tidyverse)
library(lubridate)
library(gt)
library(shiny)
library(tidyquant)
library(gtExtras)
library(extrafont)
library(plotly)

#source("Functions.R")
#source("Functions_Intraday.R")

.tq_get_return <- function(x, get = "stock.prices", complete_cases = T, from, to){
  tq_get(
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
        `date` = as.POSIXct(paste(Sys.Date(), "09:29:00"),tz="EST"),
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


  #####
  # Download Security Daily Files
  load(paste0(Sys.getenv("USERPROFILE"),"\\Callodine Capital Management, LP\\Main - Documents\\TWOOD-Bloomberg\\MSFS_Data.Rda"))

  #####
  #Set Input for Date Range
  observeEvent(input$buttonMTD, {
    updateDateRangeInput(
      session,
      "dateRange",
      start = floor_date(max(data.dailysec$Date), "month"),
      end = max(data.dailysec$Date))})
  observeEvent(input$button1M, {
    updateDateRangeInput(
      session,
      "dateRange",
      start = floor_date(max(data.dailysec$Date) %m-% months(1) + 1),
      end = max(data.dailysec$Date))})
  observeEvent(input$buttonQTD, {
    updateDateRangeInput(
      session,
      "dateRange",
      start = floor_date(max(data.dailysec$Date), 'quarter'),
      end = max(data.dailysec$Date))})
  observeEvent(input$buttonYTD, {
    updateDateRangeInput(
      session,
      "dateRange",
      start = floor_date(max(data.dailysec$Date), 'year'),
      end = max(data.dailysec$Date))})
  observeEvent(input$button1Y, {
    updateDateRangeInput(
      session,
      "dateRange",
      start = floor_date(max(data.dailysec$Date) %m-% years(1) + 1),
      end = max(data.dailysec$Date))})
  observeEvent(input$buttonITD, {
    updateDateRangeInput(
      session,
      "dateRange",
      start = floor_date(min(data.dailysec$Date)),
      end = max(data.dailysec$Date))})

  date.start <- reactive(as.character(input$dateRange[[1]]))
  date.end <- reactive(as.character(input$dateRange[[2]]))

  #####
  # Calculate Performance
  data.portreturn <- reactive({CalcAggMetrics(data.dailysec, date.start(), date.end())})
  data.lsreturn <- reactive({CalcAggMetrics(data.dailysec, date.start(), date.end(), col.group = "L/S Exp", filter.sec = T)})
  data.gicsreturn <- reactive({CalcAggMetrics(data.dailysec, date.start(), date.end(), col.group = c("Sector (Time Based)"), filter.sec = T)})
  data.lsgicreturn <- reactive({CalcAggMetrics(data.dailysec, date.start(), date.end(), col.group = c("L/S Exp", "Sector (Time Based)"), filter.sec = T)})
  data.spyreturn <- reactive({.tq_get_return(x = "^SP500TR", get = "stock.prices", complete_cases = T, from = date.start(), to = date.end())})
  data.dvyreturn <- reactive({.tq_get_return(x = "dvy",      get = "stock.prices", complete_cases = T, from = date.start(), to = date.end())})

  #####
  # Summary Table
  output$tableSummary <- gt::render_gt({
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
      by = "metric") %>%
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

  #####
  # Plot Cumulative Return
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


  #####
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

  #####
  # Plot Exposure
  output$chartGrossExp <- renderPlotly({
    data.portreturn()$data %>%
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
    data.portreturn()$data %>%
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




})






