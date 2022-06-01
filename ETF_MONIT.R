

aa.monit.func <- function(aa.etf.ticker, etf.profile, 
                          bond.opt = F, commo.opt = F,
                          start.date = "2015-12-31", end.date = Sys.Date()) {
  
  # Get historical prices
  aa.etf.price <- tq_get(aa.etf.ticker, 
                         get = "stock.prices", 
                         from = start.date, to = end.date)
  
  # Calculate daily returns
  aa.etf.return <- aa.etf.price %>% 
    group_by(symbol) %>% 
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn,
                 period     = "daily",
                 type       = "arithmetic") %>% ungroup()
  
  # Find the end date
  tmp.max.date <- aa.etf.price %>% 
    filter(date == max(date)) %>% 
    distinct(date) %>% pull
  
  #----------------------------------------------------------------
  # Calculate the cumulative returns of each period(1d,1w,1m,3m,ytd,1y)
  #----------------------------------------------------------------
  aa.etf.1d.return <- aa.etf.return %>% 
    filter(date == tmp.max.date) %>% 
    select(-date) %>% 
    rename(returns.1d = daily.returns)
  
  aa.etf.1w.return <- aa.etf.return %>% 
    filter(date <= tmp.max.date, date > tmp.max.date-days(6)) %>% 
    group_by(symbol) %>% 
    tq_transmute(select     = daily.returns,
                 mutate_fun = Return.cumulative) %>%
    rename(returns.1w = nested.col) %>% 
    ungroup()
  
  aa.etf.1m.return <- aa.etf.return %>% 
    filter(date <= tmp.max.date, date > tmp.max.date-days(30)) %>% 
    group_by(symbol) %>% 
    tq_transmute(select     = daily.returns,
                 mutate_fun = Return.cumulative) %>% 
    rename(returns.1m = nested.col) %>% 
    ungroup()
  
  aa.etf.3m.return <- aa.etf.return %>% 
    filter(date <= tmp.max.date, date > tmp.max.date-days(90)) %>% 
    group_by(symbol) %>% 
    tq_transmute(select     = daily.returns,
                 mutate_fun = Return.cumulative) %>% 
    rename(returns.3m = nested.col) %>% 
    ungroup()
  
  aa.etf.ytd.return <- aa.etf.return %>% 
    filter(date <= tmp.max.date, date >= "2019-01-01") %>% 
    group_by(symbol) %>% 
    tq_transmute(select     = daily.returns,
                 mutate_fun = Return.cumulative) %>% 
    rename(returns.ytd = nested.col) %>% 
    ungroup()
  
  aa.etf.1y.return <- aa.etf.return %>% 
    filter(date <= tmp.max.date, date > tmp.max.date-days(364)) %>% 
    group_by(symbol) %>% 
    tq_transmute(select     = daily.returns,
                 mutate_fun = Return.cumulative) %>% 
    rename(returns.1y = nested.col) %>% 
    ungroup()
  #----------------------------------------------------------------
  
  # Bind each data set to summarized form
  aa.etf.tot.data <- aa.etf.price %>% 
    select(symbol, date, volume, adjusted) %>% 
    filter(date >= "2018-01-01") %>% 
    left_join(aa.etf.return,     by = c("symbol","date")) %>% 
    left_join(aa.etf.1d.return,  by = "symbol") %>% 
    left_join(aa.etf.1w.return,  by = "symbol") %>% 
    left_join(aa.etf.1m.return,  by = "symbol") %>% 
    left_join(aa.etf.3m.return,  by = "symbol") %>% 
    left_join(aa.etf.ytd.return, by = "symbol") %>% 
    left_join(aa.etf.1y.return,  by = "symbol")
  
  #----------------------------------------------------------------
  # Draw price sparkcharts of each period 
  #----------------------------------------------------------------
  min.col <- "#0000ff"
  max.col <- "#ff0000"
  
  # https://omnipotent.net/jquery.sparkline/#s-about
  
  spk.1m.chr <- aa.etf.price %>% 
    filter(date <= tmp.max.date, date > tmp.max.date-days(31)) %>% 
    group_by(symbol) %>% 
    summarize(Price.1M = spk_chr(adjusted,
                                 lineColor = 'black', 
                                 fillColor = '#e5e5e5',
                                 width = 60,
                                 height = 45,
                                 highlightLineColor = 'orange', 
                                 highlightSpotColor = 'orange',
                                 fillColor = '#ffaaaa',
                                 spotColor = '#7fff00',
                                 minSpotColor = min.col,
                                 maxSpotColor = max.col)) 
  
  spk.3m.chr <- aa.etf.price %>% 
    filter(date <= tmp.max.date, date > tmp.max.date-days(91)) %>% 
    group_by(symbol) %>% 
    summarize(Price.3M = spk_chr(adjusted,
                                 lineColor = 'black', 
                                 fillColor = '#e5e5e5',
                                 width = 60,
                                 height = 45,
                                 highlightLineColor = 'orange', 
                                 highlightSpotColor = 'orange',
                                 fillColor = '#ffaaaa',
                                 spotColor = '#7fff00',
                                 minSpotColor = min.col,
                                 maxSpotColor = max.col)) 
  
  spk.ytd.chr <- aa.etf.price %>% 
    filter(date <= tmp.max.date, date >= "2019-01-01") %>% 
    group_by(symbol) %>% 
    summarize(Price.YTD = spk_chr(adjusted,
                                  lineColor = 'black', 
                                  fillColor = '#e5e5e5',
                                  width = 60,
                                  height = 45,
                                  highlightLineColor = 'orange', 
                                  highlightSpotColor = 'orange',
                                  fillColor = '#ffaaaa',
                                  spotColor = '#7fff00',
                                  minSpotColor = min.col,
                                  maxSpotColor = max.col)) 
  
  spk.1y.chr <- aa.etf.price %>% 
    filter(date <= tmp.max.date, date > tmp.max.date-days(366)) %>% 
    group_by(symbol) %>% 
    summarize(Price.1Y = spk_chr(adjusted,
                                 lineColor = 'black', 
                                 fillColor = '#e5e5e5',
                                 width = 60,
                                 height = 45,
                                 highlightLineColor = 'orange', 
                                 highlightSpotColor = 'orange',
                                 fillColor = '#ffaaaa',
                                 spotColor = '#7fff00',
                                 minSpotColor = min.col,
                                 maxSpotColor = max.col)) 
  
  #----------------------------------------------------------------
  
  if (bond.opt) {
    
    # Extract main information from ETF profile
    etf.info.bond <- etf.profile %>% 
      unnest() %>% 
      filter(TICKER %in% aa.etf.ticker) %>% 
      filter(CLASS %in% c("Asset Class",
                          "Region (General)",
                          "ETFdb.com Category",
                          "Bond Type(s)",
                          "Bond Duration",
                          "Tracks This Index")) %>% 
      spread(CLASS, VALUE) %>% 
      rename(symbol           = TICKER,
             Asset.Class      = `Asset Class`,
             Region           = `Region (General)`,
             Asset.Category   = `ETFdb.com Category`,
             Bond.Type        = `Bond Type(s)`,
             Bond.Duration    = `Bond Duration`,
             Underlying.Index = `Tracks This Index`) %>% 
      select(symbol, 
             Asset.Class, Asset.Category, Underlying.Index, Region,
             Bond.Type, Bond.Duration)
    
    # 
    fin.res <- aa.etf.tot.data %>% 
      group_by(symbol) %>%
      summarize(Ret.1D  = mean(returns.1d),
                Ret.1W  = mean(returns.1w),
                Ret.1M  = mean(returns.1m),
                Ret.3M  = mean(returns.3m),
                Ret.YTD = mean(returns.ytd),
                Ret.1Y  = mean(returns.1y)) %>% 
      left_join(spk.1m.chr, "symbol") %>% 
      left_join(spk.3m.chr, "symbol") %>%
      left_join(spk.ytd.chr, "symbol") %>%
      left_join(spk.1y.chr, "symbol") %>%
      left_join(etf.info.bond, "symbol") %>%
      select(symbol, Asset.Class:Region, 
             Bond.Type, Bond.Duration,
             Ret.1D:Ret.1Y, 
             Price.1M:Price.1Y,
             everything()) %>%
      arrange(factor(symbol, levels = aa.etf.ticker)) %>% 
      datatable(escape = F, rownames = F, extensions = "Buttons",
                options = list(
                  dom = "Bfrtip",
                  buttons = c("Excel"),
                  pageLength = 100,
fnDrawCallback = htmlwidgets::JS('function(){
                                                              HTMLWidgets.staticRender();
}'))
      ) %>% 
      formatPercentage(
        c("Ret.1D","Ret.1W","Ret.1M","Ret.3M","Ret.YTD","Ret.1Y"), 2) %>% 
      formatStyle(
        c("Ret.1D","Ret.1W","Ret.1M","Ret.3M","Ret.YTD","Ret.1Y"),
        color = styleInterval(c(0), c('blue', 'red'))) %>%
      spk_add_deps()
    
  } else if(commo.opt) {
    
    # Extract main information from ETF profile
    etf.info <- etf.profile %>% 
      unnest() %>% 
      filter(TICKER %in% aa.etf.ticker) %>% 
      filter(CLASS %in% c("Asset Class",
                          "ETFdb.com Category",
                          "Tracks This Index")) %>% 
      spread(CLASS, VALUE) %>% 
      rename(symbol           = TICKER,
             Asset.Class      = `Asset Class`,
             Asset.Category   = `ETFdb.com Category`,
             Underlying.Index = `Tracks This Index`) %>% 
      select(symbol, 
             Asset.Class, Asset.Category, Underlying.Index)
    
    # 
    fin.res <- aa.etf.tot.data %>% 
      group_by(symbol) %>%
      summarize(Ret.1D  = mean(returns.1d),
                Ret.1W  = mean(returns.1w),
                Ret.1M  = mean(returns.1m),
                Ret.3M  = mean(returns.3m),
                Ret.YTD = mean(returns.ytd),
                Ret.1Y  = mean(returns.1y)) %>% 
      left_join(spk.1m.chr, "symbol") %>% 
      left_join(spk.3m.chr, "symbol") %>%
      left_join(spk.ytd.chr, "symbol") %>%
      left_join(spk.1y.chr, "symbol") %>%
      left_join(etf.info, "symbol") %>%
      select(symbol, Asset.Class:Underlying.Index, 
             Ret.1D:Ret.1Y, 
             Price.1M:Price.1Y,
             everything()) %>%
      arrange(factor(symbol, levels = aa.etf.ticker)) %>% 
      datatable(escape = F, rownames = F, extensions = "Buttons",
                options = list(
                  dom = "Bfrtip",
                  buttons = c("Excel"),
                  pageLength = 100,
                  fnDrawCallback = htmlwidgets::JS('function(){
                                                              HTMLWidgets.staticRender();
}'))
      ) %>% 
      formatPercentage(
        c("Ret.1D","Ret.1W","Ret.1M","Ret.3M","Ret.YTD","Ret.1Y"), 2) %>% 
      formatStyle(
        c("Ret.1D","Ret.1W","Ret.1M","Ret.3M","Ret.YTD","Ret.1Y"),
        color = styleInterval(c(0), c('blue', 'red'))) %>%
      spk_add_deps()
    
  } else {
    
    # Extract main information from ETF profile
    etf.info <- etf.profile %>% 
      unnest() %>% 
      filter(TICKER %in% aa.etf.ticker) %>% 
      filter(CLASS %in% c("Asset Class",
                          "Region (General)",
                          "ETFdb.com Category",
                          "Bond Type(s)",
                          "Bond Duration",
                          "Tracks This Index")) %>% 
      spread(CLASS, VALUE) %>% 
      rename(symbol           = TICKER,
             Asset.Class      = `Asset Class`,
             Region           = `Region (General)`,
             Asset.Category   = `ETFdb.com Category`,
             Underlying.Index = `Tracks This Index`) %>% 
      select(symbol, 
             Asset.Class, Asset.Category, Underlying.Index, Region)
    
    # 
    fin.res <- aa.etf.tot.data %>% 
      group_by(symbol) %>%
      summarize(Ret.1D  = mean(returns.1d),
                Ret.1W  = mean(returns.1w),
                Ret.1M  = mean(returns.1m),
                Ret.3M  = mean(returns.3m),
                Ret.YTD = mean(returns.ytd),
                Ret.1Y  = mean(returns.1y)) %>% 
      left_join(spk.1m.chr, "symbol") %>% 
      left_join(spk.3m.chr, "symbol") %>%
      left_join(spk.ytd.chr, "symbol") %>%
      left_join(spk.1y.chr, "symbol") %>%
      left_join(etf.info, "symbol") %>%
      select(symbol, Asset.Class:Region, 
             Ret.1D:Ret.1Y, 
             Price.1M:Price.1Y,
             everything()) %>%
      arrange(factor(symbol, levels = aa.etf.ticker)) %>% 
      datatable(escape = F, rownames = F, extensions = "Buttons",
                options = list(
                  dom = "Bfrtip",
                  buttons = c("Excel"),
                  pageLength = 100,
fnDrawCallback = htmlwidgets::JS('function(){
                                                              HTMLWidgets.staticRender();
}'))
      ) %>% 
      formatPercentage(
        c("Ret.1D","Ret.1W","Ret.1M","Ret.3M","Ret.YTD","Ret.1Y"), 2) %>% 
      formatStyle(
        c("Ret.1D","Ret.1W","Ret.1M","Ret.3M","Ret.YTD","Ret.1Y"),
        color = styleInterval(c(0), c('blue', 'red'))) %>%
      spk_add_deps()
    
  }
  
  return(fin.res)
  
}
