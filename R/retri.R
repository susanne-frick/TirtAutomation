#' Recode triplets: ranks to binary outcomes
#'
#'
#'
#' @param df data frame with raw data
#' @param fbname name of the questionnaire
#'
#' @return data frame with binary coded data
#' @export
#'
retri <- function(df, fbname){
  spalte <- seq(1,ncol(df), by=3)
  out <- data.frame(matrix(NA, nrow = nrow(df), ncol = ncol(df)))
  colnames(out) <- paste0("b.",fbname,".",c(1:ncol(df)))
  out[spalte]   <- ifelse(df[spalte+1]-df[spalte]  > 0, 1, 0)
  out[spalte+1] <- ifelse(df[spalte+2]-df[spalte]  > 0, 1, 0)
  out[spalte+2] <- ifelse(df[spalte+2]-df[spalte+1]> 0, 1, 0)
  out
}
