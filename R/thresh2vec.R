# Thresholds aus Mplus auslesen und in einem Vektor abspeichern

#' thresh2vec
#'
#' TIRT-thresholds from Mplus output to vector
#'
#' @param samplestats sample statistics from an TIRT model (Mplus output)
#'
#' @export
#'
thresh2vec <- function(samplestats){
  v.thresholds <- samplestats$sampstat$means.intercepts.thresholds
}
