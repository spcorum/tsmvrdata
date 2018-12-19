#' Evaluate a sparse model
#'
#' Calculates evaluation metrics between estimated and ground truth
#' sparse models (i.e., matrices)
#'
#' @param Star matrix of true values (m-by-n)
#' @param Hat matrix of fitted values (m-by-n)
#' @param Sigma positive definite covariance matrix (m-by-m)
#' @return A list of evaluation metrics, including: \cr \cr
#' \code{se}, squared error \cr
#' \code{me}, model error \cr
#' \code{tp}, number of true positives \cr
#' \code{tn}, number of true negatives \cr
#' \code{fp}, number of false positives \cr
#' \code{fn}, number of false negatives \cr
#' \code{tpr}, true positive rate \cr
#' \code{tnr}, true negative rate \cr
#' \code{acc}, accuracy \cr
#' \code{auc}, Area Under Curve \cr
#' \code{f1}, F1 score \cr
#'
#' @note
#' See also \code{\link{squared_error}} and
#' \code{\link{model_error}}.
#'
#' @export
evaluate_model = function(Star,Hat,Sigma=NULL) {

    se <- squared_error(Star,Hat)
    me <- model_error(Star,Hat,Sigma)
    P <- sum(Star!=0)
    N <- sum(Star==0)
    TP <- sum(Star!=0 & Hat!=0)
    TN <- sum(Star==0 & Hat==0)
    FP <- sum(Star==0 & Hat!=0)
    FN <- sum(Star!=0 & Hat==0)
    TPR = TP/P
    TNR = TN/N
    F1 = 2*TP/(2*TP+FP+FN)
    ACC <- (TP+TN) / (TP+TN+FP+FN)
    roc.object <- roc(as.vector((Hat!=0)*1),as.vector((Star!=0)*1))
    AUC <- auc(roc.object)[[1]]

    return(list(squared_error=se, model_error=me, tp=TP, tn=TN, fp=FP,
                fn=FN, tpr=TPR, tnr=TNR, acc=ACC, auc=AUC, f1=F1))

}
