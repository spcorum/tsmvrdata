#' Process the Stock Portfolio Performance dataset
#'
#' Processes the Stock Portfolio Performance Dataset datset
#'
#' This function must be run in the same directory as the
#' 'stock_portfolio_performance_data_set.xlsx' file.
#'
#' This is a multivariate dataset of simulated US stock market
#' historical data designed for finding the optimal mixture of stock
#' picking concepts for predicting stock performace. The first
#' column is metadata (stock ID's). Columns 2-7 are predictors
#' (stock picking concepts). Columns 8-13 are responses (investment
#' performance indicators). All data columns are mean centered. The
#' dataset is saved as 'stocks.Rda'. The original data is the file
#' named 'stock_portfolio_performance_data_set.xlsx'.
#'
#' Citation: Liu, Y. C., & Yeh, I. C. Using mixture design and neural networks to build stock selection decision support systems. Neural Computing and Applications, 1-15.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Stock+portfolio+performance}
"Stock Portfolio Performance Dataset at UC Irvine Machine Learning Data Repository"
#'
#' @return a dataframe of the processed data

process_stocks = function() {

    library(readxl)

    # Read data
    stocks = read_xlsx('stock_portfolio_performance_data_set.xlsx')

    # Fix column names
    colnames(stocks) <- stocks[1,]
    stocks = stocks[-1,]

    # Trim last six repeated columns
    stocks = stocks[,c(-14,-15,-16,-17,-18,-19)]

    # Convert character dataframe to numeric
    stocks = as.data.frame(lapply(stocks,function(x) as.numeric(x)))

    # Normalize
    for (i in 2:length(stocks)) stocks[,i] = (stocks[,i] - mean(stocks[,i]))/sd(stocks[,i])

    # Save
    usethis::use_data(stocks, overwrite = TRUE)

    return(stocks)
}
