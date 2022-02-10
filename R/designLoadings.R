#' Design Loadings
#'
#' Generates a matrix which contains information about which trait is measured by which
#' item and is it positively or negatively worded.
#'
#' @param all.items item coding table (item-trait correspondence, coding of item, ...)
#' @param quest quest which questionnaire "BT"/"SD"/"BI"/"OS"... (name in all.items)
#' @param no.traits number of traits in the questionnaire
#'
#' @return matrix of design loadings of a questionnaire of interest
#' @export
#'
#'
designLoadings <- function(all.items, quest, no.traits){
  #1# select all items from questionnaire "quest"
  items.quest <- all.items[all.items$questionnaire == quest,]

  #2# build a matrix filled item codings (1=pos, -1=neg, 0=trait was not measured by this item)
  m.des.load <- matrix(0, nrow=nrow(items.quest), ncol=no.traits)
  for (i in 1:nrow(items.quest)) {
    m.des.load[i,items.quest[i,"trait_number"]] <- items.quest[i,"pos_neg"]}

  #3# name items (rows) and traits (cols)
  rownames(m.des.load) <- ifelse((1:nrow(m.des.load))<10,paste0(quest,"i","0",1:nrow(m.des.load)),paste0(quest,"i",1:nrow(m.des.load)))
  colnames(m.des.load) <- paste0("trait_",1:no.traits)

  return(m.des.load)
}
