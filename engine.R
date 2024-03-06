# Import necessary libraries
library(tidyverse)
library(tidyquant)

# Importing config file
source('modular_code/input/config.yml')

# Importing required scripts from ML_pipeline
source('modular_code/src/ml_pipeline/utils.R')

weburl<-config::get("weburl")
tablenumber<-config::get("tablenumber")
startdate<-config::get("startdate")
enddate<-config::get("enddate")

# Set random seed for reproducibility reasons
set.seed(301)

# Our first step is to parse an HTML table hosted on Wikipedia. We are interested in table 2, so we parse 
# the URL and table number to htmltab function, which requests and downloads HTML data.
ticker_names_table<- htmltab::htmltab(weburl,tablenumber)

# Transforming tickers so that they can be recognized by yahoo finance api
ticker_transforms_names<-transform_ticker_name(ticker_names_table)

# Extracting 'log returns' of the stocks
period_returns_TSX <-  ticker_transform_names %>%
  tq_get(get  = "stock.prices",
         from = startdate,
         to   = enddate) %>%
  group_by(symbol) %>% # allows us to perform the periodReturn per Ticker and not the whole dataset
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily",
               type = "log", 
               col_rename = "Ra") # Ra stands for Returns group a

# Extracting benchmark data. The XLK is commonly use for this purpose, and it follows the technology companies listed on
# the S&P500.
period_returns_XLK <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = startdate,
         to   = enddate) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn,
               type = "log",
               period     = "daily", 
               col_rename = "Rb") # Returns for comparison group b

# Performing a simple left join on date as to compare the log returns from the companies on the TSX vs our Benchmark Index.
RaRb_preselection <- left_join(period_returns_TSX, 
                               period_returns_XLK,
                               by = "date") %>% group_by(symbol)

# PRELIMINARY FINANCIAL ANALYSIS

# CAPM analysis, which returns several financial indicators 
CAPM_stock <- RaRb_preselection %>% tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM) 

# Saving analysis as a .csv file
write.csv(CAPM_stock,"modular_code/output/CAPM_stock.csv")

# Selecting the top quintile of top returns per Ticker, depending on your personal risk aversion
stock_top_percentile <- CAPM_stock %>% filter(AnnualizedAlpha > quantile(CAPM_stock$AnnualizedAlpha , .80))

# Filtering our initial tibble and only keeping the Ticker names present in both sets.
quintile_ticker_names <- stock_top_percentile$symbol
stock_returns_quintile <- period_returns_TSX %>%  filter(symbol %in% quintile_ticker_names) 

# Creating portfolio with 100 randomly selected examples. 
set.seed(20)  
repetitions<-config::get('repetitions')
weights<-c(1:100)

# We need a vector with the same Tickers as before but multiplied by our desired number of repetitions.
stock_returns_quintile <- stock_returns_quintile %>%tq_repeat_df(n = repetitions)

# Finding random weights
random_weights_multiple = finding_random_weights(quintile_ticker_names, repetitions, weights)

# Binding the vector containing our weights to the Tickers, grouping by portfolio and normalizing our weights to sum to 1.
weights_table <-  tibble(quintile_ticker_names) %>%
  tq_repeat_df(n = repetitions) %>%
  bind_cols(tibble(random_weights_multiple)) %>%
  group_by(portfolio) %>% 
  mutate(random_weights_multiple = random_weights_multiple / sum(random_weights_multiple))

# Parsing multiple weights which map to a specific portfolio to our function. 
portfolio_returns_multi <- stock_returns_quintile %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = weights_table, 
               col_rename  = "Ra")

# Joining all the examples to the respective return from the Benchmark Index.
RaRb_multiple_portfolios <- left_join(portfolio_returns_multi, 
                                      period_returns_XLK,
                                      by = "date")

# Here we compute some useful financial indicators as before, but this time for each of the portfolios, which
# contain a different selection of Tickers and in different ratios.
CAPM_multiple_portfolios <- RaRb_multiple_portfolios %>%tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

# Saving analysis as a .csv file
write.csv(CAPM_multiple_portfolios ,"modular_code/output/CAPM_multiple_portfolios.csv")

# With help of the CAPM table, choose the portfolio of interest and look it up on the key matrix table. 
options(digits=2)
keymatrix <- weights_table %>% spread(key = portfolio, value = random_weights_multiple)

# Saving keymatrix table as a .csv file
write.csv(keymatrix ,"modular_code/output/keymatrix.csv")
