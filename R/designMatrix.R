#' Design matrix
#'
#' In the TIRT model some factor loadings are negative per design. This function
#' creates a matrix for a questionnaire with no.b blocks which contains the loadings
#' which a set per design of the TIRT model.
#'
#' @param no.b number of blocks in the questionnaire
#'
#' @return matrix with loadings per design
#' @export
#'
#'
designMatrix <- function(no.b)
{
  no.i <- no.b*3
  #1#m.design:
  #design matrix of mfc: rows=pairwise comparisons, cols=items
  m.design <- matrix(0,nrow=no.i,ncol=no.i)
  #design.block: part of design matrix for one block
  design.block <- matrix(c(1,-1,0,1,0,-1,0,1,-1), nrow=3, ncol=3, byrow=TRUE)
  #fill blocks in m.design with block design
  for (i in 1:no.b) {m.design[(3*i-2):(3*i),(3*i-2):(3*i)] <- design.block}
  return(m.design)
}
