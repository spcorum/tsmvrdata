#' Process the UJIIndoorLoc data set
#'
#' Processes the UJIIndoorLoc. Metadata and varaince-free columns
#' are removed, and the remaining columns z-normalized. The
#' data is then saved as an Rda file. The originial data
#' needs to be unzipped .. trainingData.csv and validationData.csv
#' are in the archive.
#'
#' Citation: https://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc
#'
#' @param savefile file location/name for processed data file (string)
#' @param trainfile file location/name for trainingData.csv from UJIndoorLoc.zip (string)
#' @param valfile file location/name for validationData.csv from UJIndoorLoc.zip (string)
#'
#' @return a dataframe of the processed data

process_ujindoorloc = function(savefile, trainfile, valfile = NULL) {

    # Read in training and testing
    df = read.csv(trainfile)
    # df = rbind(read.csv(trainfile), read.csv(valfile))

    # Remove metatata and columns for classification
    df = df[,1:(length(df)-7)]

    # Remove columns with no variance
    idx = which(sapply(df,sd)==0)
    df = df[,-idx]

    # Normalize
    for (i in 1:length(df)) {
        df[,i] = (df[,i] - mean(df[,i])) / sd(df[,i])
    }

    save(df, file = 'location.Rda')
    return(df)
}
