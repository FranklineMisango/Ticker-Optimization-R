# Portfolio Optimization using R

Portfolio optimization is the process of choosing the optimal portfolio (asset distribution) from a set of all possible portfolios based on some criterion. Typically, the goal is to optimise parameters like expected return while minimising variables like financial risk. 

This repository contains the code for Portfolio Optimization in R by performing CAPM analysis on stocks being traded in Canadian Stock Market. The data for the analysis will be extracted using the Yahoo Finanace API.

# About the data

We will be extracting the log returns of stocks being traded in Canadian Stock Market using the Yahoo Finance API. Just in case if you are not able to extract these, you can use the files avaible in the input folder as input.

# To find a suitable portfolio - (R studio is used for running the modular code)

To find a suitable portfolio, run the "engine.R" file. 

You will end up with CAPM_multiple_portfolio.csv and keymatrix.csv files in the output folder. With help of the CAPM table, you can choose the portfolio of interest and look it up on the key matrix table. 
