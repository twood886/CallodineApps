#' @title Get Yahoo Finance Price and Return History
#' @description Get Yahoo Finance Price and Return History
#' @import tidyverse
#' @import zoo
#' @import scales
#' @import magrittr
#' @import lubridate
#' @import httr
#' @import jsonlite
#' @param symbol
#' @param period
#' @param interval
#' @param start
#' @param end
#' @export
get_history_v2 <- function(
    symbol = NULL,
    period = "1d",
    interval = "1m",
    start = NULL,
    end = NULL) {

  symbol <-
    symbol %>%
    str_remove_all("/") %>%
    str_remove_all(" ")

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

  timezone <- data$meta$exchangeTimezoneName

  result <- data.frame(
    date =  lubridate::with_tz(as_datetime(unlist(data$timestamp)), tz=timezone),
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


#' @title Validate Symbol
#' @description Validate Symbo
#' @import magrittr
#' @import httr
#' @import jsonlite
#' @param symbol
#' @export
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

#' @title Read Callodine Holdings
#' @description Read in Callodine CCMF Holdings from Trade Email File
#' @import tidyverse
#' @import readxl
#' @param fileloc
#' @param sheet
#' @param skip
#' @export
getCCMFholdings <- function(
    fileloc = paste0(
      Sys.getenv("USERPROFILE"),
      "\\",
      "Callodine Capital Management, LP\\Investing - Documents\\",
      "Daily_Trade_Report\\Daily_Trade_Report_Macro_v2.xlsm"),
    sheet = "Holdings",
    skip = 10){

  readxl::read_excel(
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

#' @title Read Callodine Trades
#' @description Read in Callodine CCMF Trades from Trade Email File
#' @import tidyverse
#' @import readxl
#' @param fileloc
#' @param sheet
#' @param skip
#' @export
getCCMFTrades <- function(
    fileloc = paste0(
      Sys.getenv("USERPROFILE"),
      "\\",
      "Callodine Capital Management, LP\\Investing - Documents\\",
      "Daily_Trade_Report\\Daily_Trade_Report_Macro_v2.xlsm"),
    sheet = "Enfusion",
    skip = 10){

  readxl::read_excel(
    fileloc,
    sheet = sheet,
    skip = skip) %>%
    filter(`Trade Date` == Sys.Date()) %>%
    mutate(`symbol.yahoo` = map(`Ticker`, validate_symbol)) %>%
    unnest(`symbol.yahoo`)
}

#' @title Calculate Intraday Returns
#' @description Calculated Intraday Returns
#' @import tidyverse
#' @param data.holdings
#' @export
calcReturnsIntraday <- function(data.holdings){

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

#' @title Calculate Intraday Return for Security
#' @description Calculate Intraday Return for Security
#' @import tidyverse
#' @param sec.id
calcSecReturnIntraday <- function(sec.id){

  price.intraday <- get_history_v2(`sec.id`)
  price.start <- get_history_v2(`sec.id`,  period = "2d", interval = "1d") %>%
    select(`close`) %>%
    first() %>%
    pull()

  return.intraday <- price.intraday %>%
    mutate(`return` = `close` / price.start - 1)

}


