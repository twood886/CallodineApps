### Functions for Performance Reports
library(tidyverse)
library(readr)

AggSec2Group <- function(
    data.dailysec,
    date.start,
    date.end,
    col.group = NULL,
    filter.sec = F){

  # Calculates Returns for Long and Short Portfolios
  #
  # Args:
  #   data.dailysec:    daily security level data frame
  #   data.port:        daily portfolio level data
  #   date.start:       beginning date for analysis
  #   date.end:         end date for analysis
  #   col.group:        column to use for grouping for analysis
  #   filter.sec:       should only securities be used for analysis
  #
  # Returns:
  #

  # Function to fix some common problems of returns that mess up analysis
  .returnFix <- function(x){
    case_when(
      x == -1 ~ 0,
      is.na(x) ~ 0,
      is.infinite(x) ~ 0,
      TRUE ~ x)}

  # If running function for grouped columns, run first for portfolio
  if(length(col.group) != 0){data.port <- AggSec2Group(data.dailysec, date.start, date.end)}


  data.dailysec <- data.dailysec %>%
    rename(`date` = `Date`) %>%
    # Filter Date
    filter(between(`date`, as.Date(date.start), as.Date(date.end))) %>%
    # Filter for Securities if Needed
    {if(filter.sec == T) filter(., `Investment Type` == "Securities") else (.)} %>%
    mutate(
      `L/S Exp` = ifelse(`L/S Exp` == "N/A", "L", `L/S Exp`),
      `purchase.sale` = ifelse(`Quantity` < 0 | `Quantity Start` < 0, `Sales`, `Purchases`)) %>%
    group_by_at(vars(all_of(col.group),`date`)) %>%
    summarise(
      `accrual.start` = sum(abs(`Accrual Start`), na.rm = T),
      `gl.period` = sum(`G/L Period`, na.rm = T),
      `net.assets.start` = round(sum(`Net Assets Start`, na.rm =T), digits = 2),
      `net.assets.end` = round(sum(`Net Assets`, na.rm = T), digits = 2),
      `mkt.val.gross.start` = `Market Value Net Start` %>%
        ifelse(`Investment Type` == "Securities", ., 0) %>%
        abs() %>%
        sum(na.rm = T) %>%
        round(digits = 2),
      `mkt.val.gross.end` = `Market Value Net` %>%
        ifelse(`Investment Type` == "Securities", ., 0) %>%
        abs() %>%
        sum(na.rm = T) %>%
        round(digits = 2),
      `mkt.val.net.start` = `Market Value Net Start` %>%
        ifelse(`Investment Type` == "Securities", ., 0) %>%
        sum(na.rm = T) %>%
        round(digits = 2),
      `mkt.val.net.end` = `Market Value Net` %>%
        ifelse(`Investment Type` == "Securities", ., 0) %>%
        sum(na.rm = T) %>%
        round(digits = 2),
      `purchase.sale` = sum(abs(`purchase.sale`), na.rm = T),
      .groups = "drop_last")

  if(is.null(col.group)){
    data.dailysec <- data.dailysec %>%
      arrange(`date`) %>%
      mutate(
        `mkt.val.gross.pct` = `mkt.val.gross.end` / `net.assets.end`,
        `mkt.val.net.pct` = `mkt.val.net.end` / `net.assets.end`,
        `net.contribution` = `net.assets.end` - `net.assets.start` - `gl.period`,
        `return.daily` = .returnFix(`gl.period`/ (`net.assets.start` + `net.contribution`)),
        `return.cumulative` = cumprod(1 + `return.daily`) - 1,
        `return.contribution.daily` =
          (1 + `return.daily`) *
          (1 + lag(`return.cumulative`, n = 1, default = 0)) -
          (1 + lag(`return.cumulative`, n = 1, default = 0)),
        `return.contribution.daily.total` =
          `gl.period` / (`net.assets.start` + `net.contribution`),
        `return.contribution.daily.total.time` =
          (1 + `return.contribution.daily.total`) *
          (1 + lag(`return.cumulative`, n = 1, default = 0)) -
          (1 + lag(`return.cumulative`, n = 1, default = 0)))
  }else{
    data.dailysec <- data.dailysec %>%
      group_by_at(vars(all_of(col.group))) %>%
      arrange(`date`) %>%
      mutate(
        `net.contribution` = `net.assets.end` - `net.assets.start` - `gl.period`,
        `return.daily` = .returnFix(`gl.period`/(`mkt.val.gross.start` + abs(`accrual.start`) + `purchase.sale`)),
        `return.cumulative` = cumprod(1 + `return.daily`) - 1,
        `return.contribution.daily` =
          (1 + `return.daily`) *
          (1 + lag(`return.cumulative`, n = 1, default = 0)) -
          (1 + lag(`return.cumulative`, n = 1, default = 0)))

    if("L/S Exp" %in% col.group){
      data.dailysec <- data.dailysec %>%
        mutate(
          `return.cumulative` = case_when(
            `L/S Exp` == "S" ~ cumprod(1 - `return.daily`) -1,
            TRUE ~ cumprod(1 + `return.daily`) -1))}

    data.dailysec <-
      left_join(
        data.dailysec,
        data.port %>%
          select(`date`, `net.assets.start`, `net.contribution`, `return.cumulative`, `net.assets.end`) %>%
          rename_at(vars(-`date`),function(x) paste0(x,".port")),
        by = "date") %>%
      mutate(
        `mkt.val.gross.pct` = `mkt.val.gross.end` / `net.assets.end.port`,
        `mkt.val.net.pct` = `mkt.val.net.end` / `net.assets.end.port`,
        `return.contribution.daily.total` =
          `gl.period` / (`net.assets.start.port` + `net.contribution.port`),
        `return.contribution.daily.total.time` =
          (1 + `return.contribution.daily.total`) *
          (1 + lag(`return.cumulative.port`, n = 1, default = 0)) -
          (1 + lag(`return.cumulative.port`, n = 1, default = 0))) %>%
      select(-contains(".port"))
    }

  return(data.dailysec)
}

CalcAggMetrics <- function(
  data.dailysec,
  date.start,
  date.end,
  col.group = NULL,
  filter.sec = F){

  data.agg <- list(
    "data" = AggSec2Group(
      data.dailysec,
      date.start,
      date.end,
      col.group,
      filter.sec))

  data.agg$summary <-
    data.agg$data %>%
    summarise(
      `P/L $` = sum(`gl.period`, na.rm = T),
      `Return` = last(`return.cumulative`, order_by = `date`),
      `Contribution` = sum(`return.contribution.daily.total.time`, na.rm=T),
      `Net Exp % (Avg)` = mean(`mkt.val.net.pct`, na.rm = T),
      `Gross Exp % (Avg)` = mean(`mkt.val.gross.pct`, na.rm = T),
      .groups = "drop_last")

  if(!("Security Description" %in% col.group)){
    sec.agg <-
      AggSec2Group(
        data.dailysec,
        date.start,
        date.end,
        c(col.group, "Security Description"),
        filter.sec = T) %>%
      summarise(
        `contr` = sum(`return.contribution.daily.total.time`, na.rm =T),
        .groups = "drop_last") %>%
      summarise(
        `# Win` = sum(ifelse(`contr`>0,1,0), na.rm = T),
        `# Loss` = sum(ifelse(`contr`<0,1,0), na.rm = T),
        `Batting` = `# Win` / sum(`# Win`, `# Loss`),
        `Slugging` = sum(ifelse(`contr` > 0,`contr`,0))/ sum(ifelse(`contr` < 0, abs(`contr`),0)),
        .groups = "drop_last")

    if(is.null(col.group)){
      data.agg$summary <- bind_cols(data.agg$summary, sec.agg)
    }else{
      data.agg$summary <- left_join(data.agg$summary, sec.agg)}
  }
  return(data.agg)
}
