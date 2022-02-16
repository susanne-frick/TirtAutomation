#' Recode quads: ranks to binary outcomes (partial ranking)
#'
#'
#'
#' @param df data frame with raw data
#' @param fbname name of the questionnaire
#'
#' @return data frame with binary coded data
#' @export
#'
reQuads <- function(df, fbname){
  spalte <- seq(1, ncol(df), by=4)
  out <- data.frame(matrix(NA, nrow = nrow(df), ncol = length(spalte)*6))
  a <- seq(1, ncol(df), by=6)
  colnames(out) <- paste0("b.", fbname, ".", c(1:(length(spalte)*6)))

  #a,b
  i <- 1
  out[a]   <- ifelse(is.na(df[spalte]) & is.na(df[spalte+i]), NA,
                          ifelse(is.na(df[spalte]) & df[spalte+i] == 1 |
                                   df[spalte] == 2 & is.na(df[spalte+i]) |
                                   df[spalte] == 2 & df[spalte+i] == 1, 0,
                                 ifelse(is.na(df[spalte]) & df[spalte+i] == 2 |
                                          df[spalte] == 1 & is.na(df[spalte+i]) |
                                          df[spalte] == 1 & df[spalte+i] == 2, 1, 55)))

  #a,c
  i <- 2
  out[a+1]   <- ifelse(is.na(df[spalte]) & is.na(df[spalte+i]), NA,
                          ifelse(is.na(df[spalte]) & df[spalte+i] == 1 |
                                   df[spalte] == 2 & is.na(df[spalte+i]) |
                                   df[spalte] == 2 & df[spalte+i] == 1, 0,
                                 ifelse(is.na(df[spalte]) & df[spalte+i] == 2 |
                                          df[spalte] == 1 & is.na(df[spalte+i]) |
                                          df[spalte] == 1 & df[spalte+i] == 2, 1, 55)))

  #a,d
  i <- 3
  out[a+2]   <- ifelse(is.na(df[spalte]) & is.na(df[spalte+i]), NA,
                          ifelse(is.na(df[spalte]) & df[spalte+i] == 1 |
                                   df[spalte] == 2 & is.na(df[spalte+i]) |
                                   df[spalte] == 2 & df[spalte+i] == 1, 0,
                                 ifelse(is.na(df[spalte]) & df[spalte+i] == 2 |
                                          df[spalte] == 1 & is.na(df[spalte+i]) |
                                          df[spalte] == 1 & df[spalte+i] == 2, 1, 55)))

  #b,c
  i <- 2
  j <- 1
  out[a+3]   <- ifelse(is.na(df[spalte+j]) & is.na(df[spalte+i]), NA,
                          ifelse(is.na(df[spalte+j]) & df[spalte+i] == 1 |
                                   df[spalte+j] == 2 & is.na(df[spalte+i]) |
                                   df[spalte+j] == 2 & df[spalte+i] == 1, 0,
                                 ifelse(is.na(df[spalte+j]) & df[spalte+i] == 2 |
                                          df[spalte+j] == 1 & is.na(df[spalte+i]) |
                                          df[spalte+j] == 1 & df[spalte+i] == 2, 1, 55)))

  #b,d
  i <- 3
  j <- 1
  out[a+4] <- ifelse(is.na(df[spalte+j]) & is.na(df[spalte+i]), NA,
                         ifelse(is.na(df[spalte+j]) & df[spalte+i] == 1 |
                                  df[spalte+j] == 2 & is.na(df[spalte+i]) |
                                  df[spalte+j] == 2 & df[spalte+i] == 1, 0,
                                ifelse(is.na(df[spalte+j]) & df[spalte+i] == 2 |
                                         df[spalte+j] == 1 & is.na(df[spalte+i]) |
                                         df[spalte+j] == 1 & df[spalte+i] == 2, 1, 55)))

  #c,d
  i <- 3
  j <- 2
  out[a+5]   <- ifelse(is.na(df[spalte+j]) & is.na(df[spalte+i]), NA,
                            ifelse(is.na(df[spalte+j]) & df[spalte+i] == 1 |
                                     df[spalte+j] == 2 & is.na(df[spalte+i]) |
                                     df[spalte+j] == 2 & df[spalte+i] == 1, 0,
                                   ifelse(is.na(df[spalte+j]) & df[spalte+i] == 2 |
                                            df[spalte+j] == 1 & is.na(df[spalte+i]) |
                                            df[spalte+j] == 1 & df[spalte+i] == 2, 1, 55)))
  out
}

df <- data.frame(A1 = c(1, NA), B1 = c(NA, NA), C1 = c(2, NA), D1 = c(NA, NA),
                 A2 = c(2, NA), B2 = c(NA, 1), C2 = c(1, NA), D2 = c(NA, 2))

test <- requads(df = df, fbname = "tee")
df
test



#end
