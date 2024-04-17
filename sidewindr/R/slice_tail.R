#' Pull Bottom Rows of a Dataframe
#'
#' @description
#' `slice.tail` takes two parameters, a dataframe `data` and a number `n`. It will attempt to slice `n` rows out of the dataframe from the bottom of the dataframe and return these sliced rows as a new dataframe. If `data` is grouped, it will slice `n` rows for each group in `data`.
#'
#' Note: Slicing a dataframe will remove the grouping indicies imposed by `group.by`. Please regroup dataframes following this operation.
#'
#' @param data a dataframe from which rows should be sliced.
#' @param n a double representing the number of rows to slice.
#' @return
#' The `data` dataframe, now sliced to only contain `n` rows per group in the dataframe.
#' 
#' If `n` is larger than the size of the total dataframe, it will return the entire dataframe.
#' 
#' If `n` is larger than a group when performing the operation groupwise, it will select the entire group.
#' @examples
#' mtcars |> slice.tail(5)
#' # slice the last 5 rows of mtcars
#' 
#' mtcars |> group.by(cyl) |> slice.tail(2)
#' # slice the last 2 rows for each amount of cyl in the dataset
#' 
#' mtcars |> group.by(cyl) |> slice.tail(15)
#' # notice: 15 is larger than the size of our groups, so some group slices may include the entire group!
#' @export
slice.tail <- function(data, n){
  df = data
  rn = as.double(n)
  if(is.null(attr(df, "groups"))){
    if(rn >= nrow(df)){
      return(df)
    }
    return(df[1:n,]) 
  }else{
    num.groups = length(attr(df, "groups"))
    g1 = attr(df, "groups")[[1]]
    g1.size = length(g1)
    if(n >= g1.size){ # make the first dataframe
      df1 = df[g1,]
    }
    else{
      g1 = g1[(g1.size+1-n):g1.size]
      df1 = df[g1,]
    }
    for(j in 2:num.groups){
      gnext = attr(df, "groups")[[j]]
      gnext.size = length(gnext)
      if(n>=gnext.size){#next group to process is smaller than n
        dfnext = df[gnext,]
      }else{ #next group to process is larger than n
        gnext = gnext[(gnext.size+1-n):gnext.size]
        dfnext = df[gnext,]
      }
      df1 = rbind(df1, dfnext) #add dfnext onto the bottom of df1
    }
    attr(df1, "groups") = NULL
    return(df1)
  }
}