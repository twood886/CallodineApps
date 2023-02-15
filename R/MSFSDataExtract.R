#' @title Unpack MSFS "CCMF_DAILY_GL_SEC" Report from FTP into Daily Data File
#' @description Reads Latest FTP File from MSFS for Callodine Dashboard
#' @import tidyverse
#' @import readr
#' @export
unpackMSFSFTP <- function(
    FTPDir = paste0(Sys.getenv("USERPROFILE"), "\\Callodine Capital Management, LP\\TWood - Documents\\MSFS_FTP\\"),
    OutDir = paste0(Sys.getenv("USERPROFILE"), "\\Callodine Capital Management, LP\\TWood - Documents\\MSFS_HoldingsData_Daily\\")){


  # Get Top Level Folders (Named by Month)
  FTPDir <- paste0(
    FTPDir,
    dir(FTPDir)[which(as.numeric(str_remove(dir(FTPDir), "-")) == max(as.numeric(str_remove(dir(FTPDir), "-"))))],
    "\\")

  folders.report <-
    dir(FTPDir) %>%
    str_subset("AnalyticsBookmarks")

  files.date <-
    lapply(
      folders.report,
      function(FTPDir, folder)
        unlist(unzip(paste0(FTPDir,folder))) %>%
        str_extract("(?<=\\.)[0-9]{8}(?=\\.)"),
      FTPDir = FTPDir) %>%
    unlist() %>%
    as.numeric()

  file.data <- unzip(
      paste0(
        FTPDir,
        folders.report[which(files.date == max(files.date))]))

  data.raw <-
    readr::read_csv(
      file.data,
      skip = 1) %>%
    select_if(\(x) all(!is.na(x))) %>%
    rename_all(\(x) str_extract(x, "(?<=\\\").+(?=\\\")")) %>%
    rename_all(\(x) str_remove(x, " current")) %>%
    rename_all(\(x) str_replace(x, "Beg", "Start")) %>%
    rename_all(str_trim) %>%
    mutate(`End Date` = as.Date(`End Date`, format = "%m/%d/%Y")) %>%
    mutate_if(is.character, \(x) str_extract(x, "(?<=\\\").+(?=\\\")")) %>%
    filter(`Decomposed Security` != "N/A") %>%
    rename(
      `Security Description` = `Name`,
      `Security Description (Short)` = `Decomposed Security`,
      `Date` = `End Date`,
      `L/S Exp` = `Long/Short Exp`,
      `Investment Type` = `Investment Type Name`,
      `Ticker` = `Ticker (symbol)`,
      `Market Value Net Start` = `Mkt Value Start`,
      `G/L Period` = `Period G/L`,
      `Market Value Net` = `Mkt Value`,
      `Quantity Start` = `Qty Start`,
      `Quantity` = `Qty`,
      `Return` = `Period Rtn`,
      `G/L Unrealized Start` = `Unreal P&L Start`,
      `G/L Unrealized End` = `Unreal P&L`)

  list.data <- named_group_split(data.raw, `Date`)

  for(i in 1:length(list.data)){
    write.csv(
      list.data[[i]],
      file = paste0(
        OutDir,
        "CCMF_DAILY_GL_SEC_",
        format(as.character(format(as.Date(names(list.data[i])),"%Y%m%d"))),
        ".csv"),
      row.names = F)
  }
}

#' @title Unpack MSFS "CCMF_DAILY_GL_SEC" Report from Portal into Daily Data File
#' @description Reads Latest FTP File from MSFS for Callodine Dashboard
#' @import tidyverse
#' @import readr
#' @export
unpackMSFSFile <- function(
    InpDir,
    OutDir = paste0(Sys.getenv("USERPROFILE"), "\\Callodine Capital Management, LP\\TWood - Documents\\MSFS_HoldingsData_Daily\\")){

  data.raw <-
    readr::read_csv(
      InpDir,
      col_types = cols(
        .default = col_double(),
        `Day` = col_skip(),
        #`L/S Exp...2` = col_skip(),
        `Security Description` = col_character(),
        `Date` = col_date(format = "%m/%d/%Y"),
        `Security Description (Short)` = col_character(),
        `Bloomberg Code` = col_character(),
        `Asset Class` = col_character(),
        `Asset Category Level1` = col_character(),
        `Asset Category Level2` = col_character(),
        `Asset Category Level3` = col_character(),
        `L/S Exp` = col_character(),
        `Investment Type` = col_character(),
        `Mkt Cap Category (Time Based)` = col_character(),
        `Sector (Time Based)` = col_character(),
        `Industry (Time Based)` = col_character(),
        `Ticker` = col_character())) %>%
    #rename(`L/S Exp` = `L/S Exp...11`) %>%
    select(`Date`, everything()) %>%
    filter(!is.na(`Security Description`))

  list.data <- named_group_split(data.raw, `Date`)

  for(i in 1:length(list.data)){
    write.csv(
      list.data[[i]],
      file = paste0(
        OutDir,
        "CCMF_DAILY_GL_SEC_",
        format(as.character(format(as.Date(names(list.data[i])),"%Y%m%d"))),
        ".csv"),
      row.names = F)
  }
}

#' @title Combine Daily MSFS Files
#' @description Combines csv files containing daily MSFS Data
#' @import tidyverse
#' @import readr
#' @export
combineMSFSDailyFiles <- function(
    InpDir = paste0(Sys.getenv("USERPROFILE"), "\\Callodine Capital Management, LP\\TWood - Documents\\MSFS_HoldingsData_Daily\\")){
  lapply(
    dir(path = InpDir),
    function(file.name, file.path)
      readr::read_csv(
        paste0(file.path, file.name),
        col_types = cols()),
    file.path = InpDir) %>%
    bind_rows() %>%
    arrange(`Date`)
  }
