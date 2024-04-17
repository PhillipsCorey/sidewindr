#' Create New Columns in a Dataframe
#'
#' @description
#' `mutate` takes `data`, a dataframe object, and a series of `name = value` pairs. `mutate` will then attempt to evaluate each of the provided `value` expressions and make it a new column in `data` under `name`. `mutate` makes no attempt to check for correct typing or defined operations when performing calculations, please ensure data validity before using.
#'
#' @param data a dataframe to be assigned a groups attribute
#' @param ... a series of `name = value` expressions. see examples below.
#' @return A dataframe that contains all data from `data`, but has been augmented to contain those columns specified in `...` with values accordingly.
#' @examples
#' mtcars |> mutate(zoomies=qsec/hp)
#' # make a new variable "zoomies" equal to the quarter second time divided by the horsepower
#' 
#' mtcars |> mutate(egn.shape=ifelse(vs==0, "V-shaped","Straight"), transmission=ifelse(am==0,"Automatic","Manual"))
#' # convert the boolean vs and am values to strings using mutate and ifelse
#' @export
mutate <- function(data, ...){
  df = data
  all.quosures = rlang::enquos(...)
  
  for(i in 1:length(all.quosures)){
    new.colname = names(all.quosures)[[i]]
    quos = all.quosures[[i]]
    df[,new.colname] = rlang::eval_tidy(quos, df)
  }
  
  # explanation to prove I'm not just copying this from the internet:
  # eval_tidy is the rlang function that acts as the backbone for most of tidyverse's operations.
  # it takes in a quosure/expression and a to use as a reference. In this case, we're using quosures.
  # A quosure is a binding between an expression and an environment. For example, if we called
  # mutate(guitars.csv, ratio=Weight.of.Case/Weight.with.Case), the quosure would be the binding of the
  # expression: Weight.of.Case/Weight.with.Case, and the environment, which may be global, or a different
  # function's namespace. The environment just makes information like variables that may be declared in that
  # namespace available to that evaluator.
  # 
  # Lastly, eval_tidy runs the expression against the data argument in the namespace of the quosure
  # as a row-wise operation. So, for each row of guitars.csv, tidy_eval will calculate
  # Weight.of.Case/Weight.with.Case using both the variables from the environment in the quosure associated
  # with the expression and the values of each variable in the row. This may create ambiguity if there is a
  # variable in the quosure namespace that has the same name as one of the columns in the dataframe. To get
  # around this, we could have passed the original call as data$Weight.of.Case/data$Weight.with.Case, but
  # that ruins literally all of the fun.
  # 
  # Because tidy_eval operates on a bunch of fancy "metaprogramming" concepts like Abstract Syntax Trees,
  # it can even take in R functions like ifelse(Manufacturer=="Gibson",1,0), and it just works.
  # 
  # Source: https://adv-r.hadley.nz/evaluation.html#evaluation
  
  return(df)
}