#' Process the UJIIndoorLoc dataset
#'
#' This function must be run in the same directory as the
#' 'UJIndoorLoc.zip' archive.
#'
#' Processes the UJIIndoorLoc dataset. For dataset description,
#' see Web link and Citation below. Metadata and variance-free
#' columns are removed, and the remaining columns max and then
#' z-normalized. The data is then saved as an Rda file.
#' Columns 1-465 are the predictors. Columns 466-467 are the
#' response.
#'
#' Citation: J. Torres-Sospedra et al., UJIIndoorLoc: A New Multi-building and Multi-floor Database for WLAN Fingerprint-based Indoor Localization Problems, In Proceedings of the Fifth International Conference on Indoor Positioning and Indoor Navigation, 2014.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc}
"UJ Indoor Location Dataset at UC Irvince Machine Learning Data Repository"
#' @return a dataframe of the processed data

process_location = function() {

    library(rcompanion)

    # Unzip archive.
    unzip('UJIndoorLoc.zip')

    # Read data from file
    location = read.csv('UJIndoorLoc/trainingData.csv')

    # Delete extracted archive.
    unlink('UJIndoorLoc', recursive = TRUE)

    # Remove metatata and columns for classification
    location = location[,1:(length(df)-7)]

    # Remove columns with no variance
    idx = which( sapply (location,sd) == 0 )
    location = location[,-idx]

    # Max normalize then Z normalize
    for (i in 1:length(location)) {
        location[,i] = location[,i] / max(location[,i])
        location[,i] = (location[,i] - mean(location[,i]))/sd(location[,i])
    }

    usethis::use_data(location, overwrite = TRUE)
    return(location)
}
