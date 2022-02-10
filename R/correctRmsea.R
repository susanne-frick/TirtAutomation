#' correctRmsea
#'
#' In questionnaires with block size n > 2, the degrees of freedom need to
#' be corrected and the RMSEA value need to be
#' recalculated (Brown & Maydeu-Olivares, 2012).
#' This function can be used to recalculate the RMSEA value of a TIRT model.
#'
#' @param n block size
#' @param p no of blocks in questionnaire
#' @param chisq chi square value
#' @param df degrees of freedom
#' @param N sample size
#'
#' @return corrected RMSEA
#'
correctRmsea <- function(n, p, chisq, df, N){
  r <- n*(n-1)*(n-2)/6
  df_corrected <- df-(p*r)
  sqrt((chisq-df_corrected)/(df_corrected*N-1))
}
