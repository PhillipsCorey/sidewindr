#' Rename Variables in Columns
#'
#' @description
#' `rename.cols` takes `data`, a dataframe object, and `name = newname` expressions. `rename.cols` will try to reassign every `name` in `colnames(data)` to `newname`.
#'
#' @param data a dataframe with columns to be renamed
#' @param ... a series of `name = newname` expression pairs, where `name` is a column in `data`, and `newname` is the desired name of `name`. Neither `name` nor `newname` should be strings.
#' @return The `data` dataframe with the specified `name` columns renamed to `newname`.
#' @examples
#' mtcars |> rename.cols(mpg=Miles.Per.Gallon, hp=Horses.Per.Car)
#' # give the dataset some.. more clear names.
#' @export
rename.cols <- function(data, ...){
  df = data
  targets = rlang::enexprs(...)
  new.names = c()
  curr.colnames = colnames(df)
  for(i in 1:length(targets)){
    new.names = append(new.names, deparse(targets[[i]]))
  }
  targets = names(targets)
  
  for(i in 1:length(targets)){
    hit = match(targets[i], curr.colnames, nomatch=-1)
    if(hit == -1){
      warning(paste(targets[i], "is not a column of the dataframe."))
      stop()
    }
    curr.colnames[hit] = new.names[i]
  }
  colnames(df) = curr.colnames
  return(df)
}