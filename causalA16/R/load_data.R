# ' Load dataset from inst/extdata
#'
#' Loads a dataset from inst/extdata into memory.
#' @param dataset the name of the dataset to load
#' @return data set
#' @import data.table
#' @export
load_dataset <- function(dataset)
{
  fn <- system.file("extdata",
    paste0(dataset,".csv"),
    package = "causalA16")
  DATA <- read.csv(fn)
}

