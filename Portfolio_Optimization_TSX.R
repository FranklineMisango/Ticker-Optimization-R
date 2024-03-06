### Description:
# For this exercise, we will create a set of portfolios and compare their performance to an Index of choice. 
# First, we will read an HTML table from Wikipedia and parse the Tickers to one of our tidyquant functions,
# tq_get. We will then use common financial metrics to pre-select our top performing stock. Throughout this
# activity, the incumbent will learn about financial indicators, portfolio optimization and tibble usage.

# DISCLAIMER: This exercise does not constitute any type of financial advise and should be considered
# didactic material. Past performance does not indicate future returns, and proper investment should
# be discussed with certified advisory. After this important note, it is time to jump in!

# Import required packages. Since throughout this exercise we will only be using two libraries regularly,
# we can use double colon whenever we require a miscellaneous function. 
  
  # DATA ACQUSITION AND PREPARATION

# Import necessary libraries

library(tidyverse)
library(tidyquant)

# Set random seed for reproducibility reasons

set.seed(301)  

# Our first step is to parse an HTML table hosted on Wikipedia. We are interested in table 2, so we parse 
# the URL and table number to htmltab function, which requests and downloads HTML data. You could
# potentially select any Index of choice and provide the corresponding URL. just be sure to parse the 
# correct table number on the second argument of the function.

ticker_names_table <-htmltab::htmltab("https://en.wikipedia.org/w/index.php?title=S%26P/TSX_Composite_Index", 2)

# Some of you might already be familiar with the pipe operator. We use it to parse the value in 
# the left-hand side into the function in the right-hand side. One can see how this becomes
# quite useful when creating flows.

# Once we have our table, we can select the column(s) we are interested in. We want to know the Ticker names
# in this case, so let us cast the 'Symbol' column into a vector with characters as elements.

ticker_names <- as.vector(ticker_names_table %>% select(Symbol))

# Note that the tq_get function that we will use to download information about public companies uses
# Yahoo Finance API. For this reason, the Tickers must be parsed in a specific format. Some of our
# Tickers contain a period, we must change that into a hyphen and the append .TO to each of the tickers. 
# Depending on your Index and API of choice, you should modify the code below as to reach correct
# formatting.

ticker_transform_names <-  gsub("[[:punct:]]", "-", ticker_names$Symbol)
ticker_transform_names <- as.vector(stringi::stri_paste(ticker_transform_names, ".TO"))

# We can now parse the Tickers and they should be recognized by the Yahoo Finance API. For this purpose we
# use tidyquant's function tq_get, which allows us to perform certain operations on the financial data we 
# require. Usually, we would get the closing price for each of the Tickers per day, however, while that
# value can certainly be useful at times, financial analysis is usually performed on the 'log returns' of 
# the stock. It allows us to understand the change in price with respect to the day before.

period_returns_TSX <-  ticker_transform_names %>%
tq_get(get  = "stock.prices",
from = "2021-07-01",
to   = "2021-11-10") %>%
group_by(symbol) %>% # allows us to perform the periodReturn per Ticker and not the whole dataset
tq_transmute(select     = adjusted, 
mutate_fun = periodReturn, 
period     = "daily",
type = "log", 
col_rename = "Ra") # Ra stands for Returns group a

# Many of the financial indicators we will be deriving require us to provide an Index that will act as a 
# Benchmark. The XLK is commonly use for this purpose, and it follows the technology companies listed on
# the S&P500. We perform the same set of activities as above, however, tq_get can also be parsed the name
# of the Index instead of a list of Tickers (this only works for USA Index).

period_returns_XLK <- "XLK" %>%
  tq_get(get  = "stock.prices",
         from = "2021-07-01",
         to   = "2021-11-10") %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn,
               type = "log",
               period     = "daily", 
               col_rename = "Rb") # Returns for comparison group b
  
# With the following lines of code we perform a simple left join on date as to compare the log returns from 
# the companies on the TSX vs our Benchmark Index.

RaRb_preselection <- left_join(period_returns_TSX, 
                               period_returns_XLK,
                               by = "date") %>% group_by(symbol)
  # PRELIMINARY FINANCIAL ANALYSIS

# We are now ready to perform financial operations on our dataset to obtain valuable information that could
# potentially help us produce outperforming portfolios. We will perform CAPM analysis, which returns several
# financial indicators (please refer to 'Introduction to Financial Indicators Part 1 and Part 2) such as
# Alpha, Beta and Information Ratio.

CAPM_stock <- RaRb_preselection %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM) 

# Depending on your personal risk aversion, we can select the top quintile of top returns per Ticker. Others
# might prefer to select least volatile stock, so they may choose Beta indicator instead. A combination of 
# both could be obtained by using the Information Ratio. At the end, this decision depends on the Utility 
# function of each person, of the form f(U) = a*Returns - b*Risk. The ratio Returns/Risk is unique to each
# person.

stock_top_percentile <- CAPM_stock %>% filter(AnnualizedAlpha > quantile(CAPM_stock$AnnualizedAlpha , .80))

# We now know which companies are the ones with highest annual returns, however, the function above was
# applied to the tibble (dataframe for those with python background), and no longer contains our useful
# log returns. So let us filter our initial tibble and only keep the Ticker names present in both sets.

quintile_ticker_names <- stock_top_percentile$symbol

stock_returns_quintile <- period_returns_TSX %>%  filter(symbol %in% quintile_ticker_names) 

# We can now proceed to create portfolios based on our top performing tickers. We will firstly show how this
# is achieved for a single case and then show the generalized method. Unfortunately, the tq_portfoilio 
# function does not have a an argument to select only a subset of values. For this reason we have to create
# a vector with random weights, and have a weight of 0 for the tickers we do not want to include in our 
# portfolio.
set.seed(4)
weights <- c(1:100)
random_weights <- sample(weights, size=length(quintile_ticker_names), replace=TRUE)
index_for_zeroes <- sample(c(1:length(random_weights)), round(length(quintile_ticker_names)*.78, 0)) # 37 out of 47 Tickers will be 0
random_weights[index_for_zeroes]<-0 # Insert zeroes 

# Now that we have our Tickers, we bind them to the randomly selected weights and normalize them to 0. Some
# may be wondering why use Randomly created portfolios instead of using quadratic optimization. It can be 
# shown that Randomly creating portfolios will eventually (and faster) yield the optimal combination of 
# Tickers and weights. This is a similar technique to Gaussian Hyper Parameter Search, for those familiar
# with Machine Learning.

weights_mapping_single <- tibble(
  symbols = quintile_ticker_names,
  weights = random_weights
) %>% 
  mutate(weights = weights / sum(weights)) # We do want our weights to sum up to one, this will most likely
# not be the case because we randomly chose them. Lets divide by the sum of weights to normalize the values.

# The function below, tq_portfolio, will now create a portfolio with the Tickers that have non-zero weights.
# Then, it will calculate the respective returns by summing up the product of the log retun times the 
# respective weight. Finally, we rename the combined log return column 'Ra'.

portfolio_returns_monthly <- stock_returns_quintile %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra,
               weights     = weights_mapping_single,
               col_rename  = "Ra")


# Similarly to the process we followed to derive financial indicators necessary for our pre-selection of 
# companies, we join the Benchmark returns to the tibble which contains our portfolio returns. we then apply
# the same CAPM analysis to compare performance.

# Left join on date before applying CAPM function 

RaRb_single_portfolio <- left_join(portfolio_returns_monthly, 
                                   period_returns_XLK,
                                   by = "date")

RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
RaRb_single_portfolio %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = InformationRatio)

# Using ggplot, we can plot the daily returns of our portfolio in the shape of a barplot, although we could 
# also have used a regular lineplot. We add a regression line in order to see the general trend produced by
# our values.

portfolio_returns_monthly %>%
  ggplot(aes(x = date, y = Ra)) +
  geom_bar(stat = "identity", fill = palette_light()[[1]]) +
  labs(title = "Portfolio Returns",
       x = "", y = "Monthly Returns") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

# It'd be useful to see how a 10, 000 dollar investment would have grown through time. We can also add the 
# same investment growth but with regards to returns from Benchmark Index. Note we are parsing the tibble
# which contains returns for the entirety of the TSX, as we had already done, however this time we specify 
# wealth.index as TRUE, which gives us the return percentage at that point in time with respect to initial 
# investment. Once we have this, it is as easy as multiplying the initial amount times that column by using
# mutate function.

symbol_a = c(rep('PRT', length(portfolio_returns_monthly$Ra)))

portfolio_asset_name = cbind(portfolio_returns_monthly, symbol_a)

portfolio_growth_single <- portfolio_asset_name %>%
  tq_portfolio(assets_col   = symbol_a, 
               returns_col  = Ra, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * 10000)

# As always it doesn't really matter how well our portfolio performed if we don't have a point of comparison,
# so let us also see how the same investment would have grown if we had bought a fund tied to our Benchmark
# Index.

symbol_b = c(rep('XLK', length(period_returns_XLK$Rb))) # tq_portfolio does require us to parse assets_col
# so we need to create a vector with an identifier of choice and bind it to the tibble which contains log
#returns.
XLK_asset_name = cbind(period_returns_XLK, symbol_b)

portfolio_growth_XLK <- XLK_asset_name %>%
  tq_portfolio(assets_col   = symbol_b, 
               returns_col  = Rb, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * 10000)

# We can plot both lines in a single image. Blue line represents our portfolio and red line shows investment 
# growth for Index fund.

ggplot() + 
geom_line(data = portfolio_growth_single, aes(x = date, y = investment.growth), color = "blue") +
geom_line(data = portfolio_growth_XLK, aes(x = date, y = investment.growth), color = "red") +
labs(title = "Portfolio Growth", x = "", y = "Portfolio Value") +
theme_tq() +
scale_color_tq() +
scale_y_continuous(labels = scales::dollar)

# Let's now repeat the same portfolio creation but this time with 100 randomly selected examples. 
# Here we perform our random vector creation and repeat the same steps as before.

set.seed(20)  
repetitions <- 100

# We need a vector with the same Tickers as before but multiplied by our desired number of repetitions.

stock_returns_quintile <- stock_returns_quintile %>%
  tq_repeat_df(n = repetitions)


random_weights_multiple <- sample(weights, length(quintile_ticker_names)*repetitions, replace=TRUE)
index_zeroes_multiple <- sample(c(1:length(random_weights_multiple)), round(length(random_weights_multiple)*.75, 0))
random_weights_multiple[index_zeroes_multiple]<-0

# We now have a vector where 3525 out of 4700 Tickers have a value of 0. We can now multiply the tibble which
# contains our top performing companies times as many portfolios as we want, in this case 100. We then bind
# the vector containing our weights to the Tickers, group by portfolio and normalize our weights to sum to 1.

weights_table <-  tibble(quintile_ticker_names) %>%
  tq_repeat_df(n = repetitions) %>%
  bind_cols(tibble(random_weights_multiple)) %>%
  group_by(portfolio) %>% 
  mutate(random_weights_multiple = random_weights_multiple / sum(random_weights_multiple))

# As before, we are interested in the performance of each portfolio. We can parse multiple weights which map
# to a specific portfolio to our function. This results in a tibble three columns containing the date, 
# portfolio number and log return for each example.

portfolio_returns_multi <- stock_returns_quintile %>%
  tq_portfolio(assets_col  = symbol, 
               returns_col = Ra, 
               weights     = weights_table, 
               col_rename  = "Ra")

# For the same reasons as explained before, we must join all the examples to the respective return from 
# the Benchmark Index.

RaRb_multiple_portfolios <- left_join(portfolio_returns_multi, 
                                     period_returns_XLK,
                                     by = "date")

# Here we compute some useful financial indicators as before, but this time for each of the portfolios, which
# contain a different selection of Tickers and in different ratios.

CAPM_multiple_portfolios <- RaRb_multiple_portfolios %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)

# Let us see which portfolios returned the most. We can create a tibble with each portfolio and its associated
# Annualized alpha, then sort by descending order.

annualized_returns <- RaRb_multiple_portfolios %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns)

# Since we would like to plot all of our portfolios' investment growth, we convert log returns into a wealth
# index value as we did before. Not that since we had previously grouped this tibble by portfolio, the function
# is a applied by group.

portfolio_growth_monthly_multi <- stock_returns_quintile %>%
  tq_portfolio(assets_col   = symbol, 
               returns_col  = Ra, 
               weights      = weights_table, 
               col_rename   = "investment.growth",
               wealth.index = TRUE) %>%
  mutate(investment.growth = investment.growth * 10000)

# Let us now plot our investment growth. Since we do have several lines, it will be easier to study each 
# portfolio through a table.

portfolio_growth_monthly_multi %>%
  ggplot(aes(x = date, y = investment.growth, color = factor(portfolio), legend.position = "none")) +
  geom_line(size = 2, legend.position = "none") +
  labs(title = "Randomly Optimized Portfolios",
       subtitle = "Comparing Multiple Portfolios",
       x = "", y = "Portfolio Value",
       color = "Portfolio",
       legend.position = "none") +
  geom_smooth(method = "loess") +
  theme_tq() +
  scale_color_tq() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::dollar)

# With help of the CAPM table, choose the portfolio of interest and look it up on the key matrix table. As
# before, one can sort values per financial indicator of interest, then move to the key matrix and sort each 
# column (which represent portfolio number), and see what are the weights associated with each ticker.

options(digits=2)
keymatrix <- weights_table %>% spread(key = portfolio, value = random_weights_multiple)
