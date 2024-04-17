#' Slice Rows from Dataframe Based on Variable Values
#'
#' @description
#' `filter` takes `data`, a dataframe object, and a series of `name == value` logical expressions. If `mode` is set to `any`, `filter` will produce a dataframe for which every row abides by at least one of the logical statements. If `mode` is set to `all`, `filter` will produce a dataframe for which every row abides by all of the logical statements.
#'
#' @param data a dataframe to be assigned a groups attribute
#' @param ... a series of `name == value` logical expressions. see examples below.
#' @param mode `any` or `all` to determine the selection mode.
#' @return A dataframe subset of `data` that contains only those rows abiding by the selection mode and the logical expressions provided.
#' @examples
#' # any mode:
#' mtcars |> filter(gear==4, disp>385, mode=any)
#' # notice we get a data set containing all rows for which gear = 4 OR disp > 385
#' 
#' # all mode:
#' mtcars |> filter(gear>3, cyl==8, mode=all)
#' # notice we get a data set containing all rows for which gear > 3 AND cyl = 8
#' @export
filter <- function(data, ..., mode=any){
  df = data
  all.quosures = rlang::enquos(...)
  mode = deparse(rlang::enexpr(mode))
  
  l <- list()
  length(l) = length(all.quosures)
  
  for(i in 1:length(all.quosures)){
    quos = all.quosures[[i]]
    l[[i]] = rlang::eval_tidy(quos, df)
  }
  
  logical.indexer = l[[1]]
  for(i in 2:length(l)){
    if(mode=="any"){
      logical.indexer = logical.indexer + l[[i]]
      logical.indexer = as.logical(logical.indexer)
    }
    if(mode=="all"){
      for(j in 1:length(logical.indexer)){
        logical.indexer[j] = logical.indexer[j] && l[[i]][j] # and together each element of the logicals
      }
    }
  }
  
  return(df[logical.indexer,])
}