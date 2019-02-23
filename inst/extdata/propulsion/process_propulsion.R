#' Process "Condition Based Maintenance of Naval propulsion Plants" dataset
#'
#' Processes the "Condition Based Maintenance of Naval propulsion Plants" dataset
#'
#' This functions processes the "Condition Based Maintenance of
#' Naval propulsion Plants" dataset. For data description, see
#' "Web location" and "Citation" below. See also 'Features.txt' and
#' the 'README.txt' files inside the 'UCI CBM Dataset.zip' archive.
#'
#' This function must be run in the same directory as the
#' 'UCI CBM Dataset.zip' archive.
#'
#' The dataset has no missing values. In the original dataset of 18
#' columns, columns 9 and 12 are removed because they exhibit no
#' variability, for a total of 16 columns remaining. The rest of the
#' columns are Z normalized. Of the 16 remaining columns, columns
#' 2 (Ship speed), 15 (GT Compressor decay state coefficient), and
#' 16 (Turbine decay state coefficient) comprise the response.
#' The rest of the columns are predictors.
#'
#' Citation: A. Coraddu, L. Oneto, A. Ghio, S. Savio, D. Anguita, M. Figari, Machine Learning Approaches for Improving Condition?Based Maintenance of Naval propulsion Plants, Journal of Engineering for the Maritime Environment, 2014, DOI: 10.1177/1475090214540874
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Condition+Based+Maintenance+of+Naval+propulsion+Plants}
"Navel Propulsion Plants Dataset at UC Irvine Machine Learning Data Repository"
#'
#' @param featurefile file location/name for file listing the
#' column / feature labels ('Features.txt inside the 'UCI CBM
#' Dataset.zip'archive)
#'
#' @return a dataframe of the processed data

process_propulsion = function() {

    # Unzip archive.
    unzip('UCI CBM Dataset.zip')

    # Read feature names to vector.
    cnx = file('UCI CBM Dataset/Features.txt')
    fl = readLines(con = cnx, warn = FALSE)
    close(cnx)
    fl = sapply(fl, FUN = function(x) strsplit(x, split = ' - '))
    column_names = rep(0,length(fl))
    for (i in 1:length(column_names)) column_names[i] = fl[[i]][2]

    # Read data
    propulsion = read.table('UCI CBM Dataset/data.txt', col.names = column_names)

    # Delete extracted archive.
    unlink('__MACOSX', recursive = TRUE)
    unlink('UCI CBM Dataset', recursive = TRUE)

    # Remove variables with no variability
    propulsion = propulsion[,c(-9, -12)]

    # Z normalize variables
    for (i in 1:length(propulsion)) propulsion[,i] = (propulsion[,i] - mean(propulsion[,i]))/sd(propulsion[,i])

    # Save
    usethis::use_data(propulsion, overwrite = TRUE)
    return(propulsion)
}
