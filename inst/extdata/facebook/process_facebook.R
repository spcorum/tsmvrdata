#' Process Facebook Metrics dataset
#'
#' Processes the Facebook Metrics dataset
#'
#' This functions processes the Facebook Metrics dataset.
#' For data description, see "Web location" below. See also the
#' 'facebook_Metrics.txt' file inside the 'facebook_Metrics.zip'
#' archive.
#'
#' This function should be run in the same directory as the
#' 'facebook_Metrics.zip' archive.
#'
#' After deleveling, columns 1-30 are predictors, and columns 31-42
#' are responses. NAs are removed by mean or median subsitution.
#' Columns 1 and 31-42 normalized by Tukey transformation and then
#' mean-centered. Columns 2-30 are simply mean centered. Finally,
#' the dataset is saved as 'data/facebook.rda'.
#'
#' Citation:  Cedric Nugteren and Valeriu Codreanu, CLTune: A Generic Auto-Tuner for OpenCL Kernels, MCSoC: 9th International Symposium on Embedded Multicore/Many-core Systems-on-Chip. IEEE, 2015
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/SGEMM+GPU+kernel+performance}
"Facebook Metrics Dataset at UC Irvince Machine Learning Data Repository"
#' @return a dataframe of the processed data

process_facebook = function() {

    library(rcompanion)

    # Unzip archive.
    unzip('Facebook_metrics.zip')

    # Read data from file
    facebook = read.csv('dataset_Facebook.csv', sep = ';')

    # Delete extracted archive.
    unlink('dataset_Facebook.csv')
    unlink('Facebook_metrics.txt')

    # One hot encode categorical varaibles.
    facebook$Category = as.factor(facebook$Category)
    facebook$Post.Month = as.factor(facebook$Post.Month)
    facebook$Post.Weekday = as.factor(facebook$Post.Weekday)
    facebook$Post.Hour = as.factor(facebook$Post.Weekday)
    dmy <- caret::dummyVars(" ~ .", data = facebook, fullRank = TRUE)
    facebook <- data.frame(predict(dmy, newdata = facebook))

    # Remove NAs from integer / numerical variables.
    facebook$like[is.na(facebook$like)] = mean(facebook$like, na.rm = T)
    facebook$share[is.na(facebook$share)] = mean(facebook$share, na.rm = T)
    facebook$Paid[is.na(facebook$Paid)] = median(facebook$Paid, na.rm = T)

    # Center one hot encoded variabes.
    for (i in 2:30) facebook[,i] = facebook[,i] - mean(facebook[,i])

    # Normalize integer / numeric variables with Tukey's method.
    idx = c(1, 31:42)
    for (i in idx) {
        if (i == 1) facebook[,i] = facebook[,i]/max(facebook[,i])
        facebook[,i] = transformTukey(facebook[,i], plotit = F, quiet = T)
        facebook[,i] = facebook[,i] - mean(facebook[,i])
    }

    # Adjust column names to reflect data have been normalized.
    labs = colnames(facebook)
    idx = 1:length(labs)
    idx = idx[-30]
    for (i in idx) labs[i] = paste0(labs[i],'_normalized')
    colnames(facebook) <- labs

    # Save
    usethis::use_data(facebook, overwrite = TRUE)
    return(facebook)
}
