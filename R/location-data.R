#' UJIIndoorLoc Dataset (processed)
#'
#' The UJIIndoorLoc data conntects WLAN fingerprint features
#' to longitude and latitude locations. This dataset is processed
#' processed for normalization and to remove features with no
#' variance.
#'
#' @docType data
#'
#' @usage data(location)
#'
#' @format A dataframe with 19937 rows and 467 attributes The first
#' 465 attributes are predictors (normalized WLAN fingerprints),
#' and the last two varaibles are responses (latitude and
#' longitude).
#'
#' @keywords dataset
#'
#' @references
#' J. Torres-Sospedra et al. "UJIIndoorLoc: A New Multi-building and Multi-floor Database for WLAN Fingerprint-based Indoor Localization Problems", Proceedings of the Fifth International Conference on Indoor Positioning and Indoor Navigation (2014).

#' @source \href{https://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc}{UJIndoorLoc Dataset, UCI Machine Learning Repository}
#'
#' @examples
#' data(crime)
