#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(zoo)
library(scales)
library(magrittr)
library(httr)
library(jsonlite)
library(lubridate)
library(gt)


get_history_v2 <- function(symbol, period = "1d", interval = "1m", start = NULL, end = NULL) {
  symbol <- str_remove_all(symbol,"/")
  symbol <- str_remove_all(symbol," ")

  if (!is.null(start)) {start_date <- as.numeric(as.POSIXct(ymd(start)))}

  if (!is.null(end)) {end_date <- as.numeric(as.POSIXct(ymd(end)))}

  path <- "v8/finance/chart/"
  end_point <- paste0(path, symbol)
  url <- httr::modify_url(url = 'https://query1.finance.yahoo.com', path = end_point)

  if (!is.null(start) && !is.null(end)) {
    qlist <- list(period1 = start_date, period2 = end_date, interval = interval)
  }else if (!is.null(start) && is.null(end)) {
    qlist <- list(period1 = start_date, period2 = round(as.numeric(as.POSIXct(now()))), interval = interval)
  }else {
    qlist <- list(range = period, interval = interval)
  }

  resp <- httr::GET(url, query = qlist)

  if(resp$status_code != 200){
    return(data.frame(
      date = as.POSIXct(NULL),
      #volume = NULL,
      #high = NULL,
      #low = NULL,
      #open = NULL,
      close = as.numeric(NULL)))}

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  data <-
    parsed %>%
    magrittr::use_series(chart) %>%
    magrittr::use_series(result) %>%
    magrittr::extract2(1)

  indicators <-
    data %>%
    magrittr::use_series(indicators) %>%
    magrittr::use_series(quote) %>%
    magrittr::extract2(1)

  # if(length(as_datetime(unlist(data$timestamp))) > length(unlist(indicators$close))){
  #   dates.array <- as_datetime(unlist(data$timestamp))[-length(data$timestamp)]
  # }else{
  #   dates.array <- as_datetime(unlist(data$timestamp))
  # }

  result <- data.frame(
    date =  lubridate::with_tz(as_datetime(unlist(data$timestamp)), tz="EST"),
    #volume = na.locf(unlist(as.numeric(as.character(indicators$volume)))),
    #high = na.locf(unlist(as.numeric(as.character(indicators$high)))),
    #low = na.locf(unlist(as.numeric(as.character(indicators$low)))),
    #open = na.locf(unlist(as.numeric(as.character(indicators$open)))),
    close = suppressWarnings(na.locf(unlist(as.numeric(as.character(indicators$close))))))


  intervals <- c("1d", "5d", "1wk", "1mo", "3mo")

  if (interval %in% intervals) {
    adj_close <-
      data %>%
      magrittr::use_series(indicators) %>%
      magrittr::use_series(adjclose) %>%
      magrittr::extract2(1) %>%
      magrittr::use_series(adjclose) %>%
      unlist()

    result$adj_close <- adj_close
  }
  return(result)
}
validate_symbol <- function(symbol = NULL) {

  base_url <- 'https://query2.finance.yahoo.com'
  path <- 'v6/finance/quote/validate'
  url <- httr::modify_url(url = base_url, path = path)
  qlist     <- list(symbols = symbol)
  resp      <- httr::GET(url, query = qlist)
  parsed    <- jsonlite::fromJSON(
    httr::content(
      resp,
      "text",
      encoding = "UTF-8"),
      simplifyVector = FALSE)

  if (httr::http_error(resp)) {
    stop(
      cat(
        "Yahoo Finance API request failed", '\n',
        paste('Status:', status_code(resp)), '\n',
        paste('Type:', http_status(resp)$category), '\n',
        paste('Mesage:', parsed$quoteSummary$error$code), '\n',
        paste('Description:', parsed$quoteSummary$error$description, '\n'),
        sep = ''
      ),
      call. = FALSE)
  } else {
    parsed %>%
      magrittr::use_series(symbolsValidation) %>%
      magrittr::use_series(result) %>%
      magrittr::extract2(1) %>%
      names()
  }

}

get_holdings <- function(
    fileloc = paste0(
      Sys.getenv("USERPROFILE"),
      "\\",
      "Callodine Capital Management, LP\\Main - Documents\\TWOOD-Bloomberg\\",
      "Daily_Trade_Report\\Daily_Trade_Report_Macro.xlsm"),
    sheet = "Holdings",
    skip = 10){

  read_excel(
    fileloc,
    sheet = sheet,
    skip = skip) %>%
  filter(`Quantity` != 0) %>%
  mutate(
    `Option Type` =
      case_when(
        `Option Type` == "Call" ~ "C",
        `Option Type` == "Put" ~ "P"),
    `Ticker` =
      case_when(
        `Instrument Type` == "Listed Option" ~
          paste0(`Underlying Ticker`,
                 format(`Option Expiration Date`, "%Y%m%d"),
                 `Option Type`,
                 str_pad(`Option Strike`*1000, 8, pad="0")),
        `Instrument Type` == "Bond" ~ `Description`,
        TRUE ~ `Ticker`)) %>%
  select(-c(contains("Option"), "Underlying Ticker")) %>%
  mutate(`symbol.yahoo` = map(`Ticker`, validate_symbol)) %>%
  unnest(`symbol.yahoo`)
}

calc_returns <- function(data.holdings){

  data.holdings %>%
    group_by(`symbol.yahoo`) %>%
    nest() %>%
    mutate(
      `sector` = map_chr(`data`, \(x) first(x$`GIC Sector`)),
      `quantity` = map_dbl(`data`, \(x) sum(x$Quantity)),
      `price.start` = map_dbl(`data`, \(x) first(x$`Fair Value`)),
      `NMV.start` = map_dbl(`data`, \(x) sum(x$`$ NMV`)),
      `NAV.start` = map_dbl(`data`, \(x) first(x$`Selected GLs SOD NAV`)),
      `price.intraday` = map(`symbol.yahoo`, get_history_v2),
      `price.intraday` = map2(
        `price.intraday`,
        `price.start`,
        \(.x,.y) .x %>%
          add_row(
            `date` = as.POSIXct(paste(Sys.Date()-1, "16:00:00"), tx = "EST"),
            `close` = .y,
            .before = 1) %>%
        mutate(`date` = round(`date`,"mins")) %>%
        distinct(`date`, .keep_all = T)),
      `return.intraday` = pmap(
        list(`price.intraday`, `quantity`, `price.start`, `NAV.start`),
        \(.a,.b,.c,.d) .a %>%
          mutate(
            `return` = `close` / .c - 1,
            `gl` = (`close` * .b) - (.b * .c),
            `contr` = `gl` / .d) %>%
          select(`date`, `return`, `gl`, `contr`)))
}
gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>%
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    )
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # Set Timer for Auto Data Updates
  timer.min <- reactiveTimer(120000)
  timer.10min <- reactiveTimer(600000)

  # Import Holdings
  data.holdings <- reactive({
    timer.10min()
    get_holdings()})

  # Calculate Returns
  data.returns <- reactive({
    timer.min()
    calc_returns(data.holdings())})


  output$Plot <- renderPlot({
    data.returns() %>%
      select(`return.intraday`) %>%
      unnest(cols = c(`symbol.yahoo`, `return.intraday`)) %>%
      ungroup() %>%
      select(`symbol.yahoo`, `date`, `contr`) %>%
      complete(`symbol.yahoo`, `date`) %>%
      group_by(`symbol.yahoo`) %>%
      arrange(`date`) %>%
      fill(`contr`, .direction = "down") %>%
      group_by(`date`) %>%
      summarize(`return` = sum(`contr`)) %>%
      arrange(`date`) %>%
      slice(-1) %>%
      add_row(
        `date` = as.POSIXct(paste(Sys.Date(), "09:29:00"),tz="EST"),
        `return` = 0,
        .before = 1) %>%
      ggplot() +
      aes(x = date, y = return) +
      geom_line(size = 0.5, colour = "#112446") +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent)
    })

  output$TableLS <- gt::render_gt({
    data.returns() %>%
      select(`sector`,`NMV.start`, `return.intraday`) %>%
      unnest(cols = c(`symbol.yahoo`, `sector`, `NMV.start`, `return.intraday`)) %>%
      ungroup() %>%
      complete(`symbol.yahoo`, `date`) %>%
      fill(everything(), .direction = "down") %>%
      filter(`date` == max(`date`)) %>%
      mutate(`L/S` = ifelse(`NMV.start` > 0, "Long", "Short")) %>%
      mutate(`contr` = `contr` * 10000) %>%
      group_by(`sector`, `L/S`) %>%
      summarise(`contr` = sum(`contr`, na.rm = T)) %>%
      pivot_wider(names_from = `L/S`, values_from = `contr`) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(`Total` = sum(`Long`, `Short`, na.rm=T)) %>%
      rename(`Sector` = `sector`) %>%
      gt() %>%
      tab_header(
        title = "Sector L/S Contribution to Intraday Return (bps)") %>%
      fmt_number(columns = c(`Long`, `Short`, `Total`), decimals = 0) %>%
      grand_summary_rows(
        columns = c(`Long`, `Short`, `Total`),
        fns = list(`Total` = ~sum(., na.rm = T)),
        formatter = fmt_number,
        decimals = 0) %>%
      gt_theme_538()
  })

})


