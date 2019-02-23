#' Process "Concrete Slump Tests" dataset
#'
#' Processes the "Concrete Slump Tests" dataset
#'
#' This functions processes the "Concrete Slump Tests" dataset.
#' For data description, see "Web location" and "Citation" below.
#'
#' This function should be run in the same directory as the
#' 'slump_test.data.txt' file.
#'
#' There are no missing values. The first column is ID number
#' (metadata). Columns 2-8 are the predictors. Columns 9-11
#' are the response (SLUMP,FLOW, and 28-day Compressive Strength).
#' All data columns are Z normalized.
#'
#' Citation: Yeh, I-Cheng, "Modeling slump flow of concrete using second-order regressions and artificial neural networks," Cement and Concrete Composites, Vol.29, No. 6, 474-480, 2007.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Concrete+Slump+Test}
"Concrete Slump Dataset at UC Irvince Machine Learning Data Repository"
#'
#' @return a dataframe of the processed data

process_concrete = function() {

    # Read data
    concrete = read.csv('slump_test.data.txt')

    # Z normalize variables
    for (i in 2:length(concrete)) concrete[,i] = (concrete[,i] - mean(concrete[,i]))/sd(concrete[,i])

    # Save
    usethis::use_data(concrete, overwrite = TRUE)
    return(concrete)
}
