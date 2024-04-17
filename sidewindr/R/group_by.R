#' Group Dataframe Rows by Unique Values
#'
#' @description
#' `group.by` takes `data`, a dataframe object, and one column name in the dataframe. It will assign a `groups` attribute to the object. The `groups` attribute will be a list of double lists, where each list will contain all row indicies of one unique value of `col`.
#' 
#' Once the `groups` attribute has been set on a dataframe, passing it into other `sidewindr` functions will make them behave groupwise instead of against the whole dataset. `group.by` only supports single level grouping. Grouping an already grouped dataframe will overwrite the current groupings.
#'
#' @param data a dataframe to be assigned a groups attribute
#' @param col an unquoted expression for the name of a column in the dataframe
#' @return The `data` dataframe, with a new `groups` attribute that can be used manually, but will primarily be used by other sidewindr functions.
#' @examples
#' mtcars |> group.by(drat)
#' # notice: group.by does not inherently do anything on its own!
#' 
#' # to visualize the effects, let's use slice.head,
#' mtcars |> group.by(cyl) |> slice.head(3)
#' # now we see 3 cars from each of the groups in mtcars!
#' @export
group.by <- function(data, col){
  df = data
  column = deparse(rlang::enexpr(col))
  if(!(column %in% colnames(df))){
    warning("Provided col is not a variable of data.")
    stop()
  }
  unique.vals = unique(df[,column])
  num.unique = length(unique.vals)
  groups = list()
  length(groups) = num.unique
  for(i in 1:num.unique){
    target = unique.vals[i]
    hits = c()
    for(j in 1:nrow(df)){
      if(df[j,column] == target){
        hits <- append(hits, j)
      }
    }
    groups[[i]] = hits
  }
  names(groups) = unique.vals
  attributes(df)$groups = groups
  return(df)
}