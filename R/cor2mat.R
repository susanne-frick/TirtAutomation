#' Correlations to matrix
#'
#' @param samplestats sample statistics from an TIRT model (Mplus output)
#'
#' @return correlation matrix of tetrachoric correlation
#' @export
#'
cor2mat <- function(samplestats){
  # correlations to matrix
  m.cor <- samplestats$sampstat$correlations.vardiag
  #add diagonal
  diag(m.cor) <- 1
  #transpose lower.tri
  m.cor[upper.tri(m.cor)] <- t(m.cor)[upper.tri(m.cor)]
  m.cor
}
