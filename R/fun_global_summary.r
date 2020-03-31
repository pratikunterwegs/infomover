#' Print a global summary.
#'
#' @param data_path Where to search for data.
#'
#' @return Prints figures and saves data providing a global summary of all data in the data folder.
#' @import data.table
#' @export
#'
#' @examples
print_global_summary <- function(data_path = "data/"){
  id <- value <- gen <- variable <- summary_data <- NULL
  # asserts for file path
  {
    # check the data path is in the current working directory
    assertthat::assert_that(assertthat::is.dir(data_path),
    msg = glue::glue('global_summary: {data_path} not found in wd: {getwd()}'))
    # list agent data files
    data_files <- list.files(path = data_path,
                             pattern = "agent",
                             full.names = TRUE)
    # check that there is some data
    assertthat::assert_that(length(data_files) >= 1,
                          msg = "global_summary: no agent files in data path")
    # acquire a lookup file
    lookup <- list.files(path = data_path,
                         pattern = "lookup",
                         full.names = TRUE)
    # check that there is some data
    assertthat::assert_that(length(lookup) == 1,
                          msg = "global_summary: no or multiple lookups found")
  }
  # read in lookup and match to data files by filename
  {
    data <- data.table::data.table(filename = data_files)
    lookup <- data.table::fread(lookup)

    data <- data.table::merge.data.table(data, lookup, by = "filename")

    # check that merging worked by testing col names
    dfnames <- colnames(data)
    namesReq <- c("phi", "rho", "rep")
    for (i in 1:length(namesReq)) {
      assertthat::assert_that(namesReq[i] %in% dfnames,
                              msg = glue::glue('global_summary: {namesReq[i]} is
                         required but missing from data'))
    }
  }
  # read in data
  {
    # read the data and summarise
    agent_data <- purrr::map(data$filename, data.table::fread)
    agent_summary <- purrr::map(agent_data, function(dt){
      dt[,id:=NULL]

      # convert all cols to numeric
      dt[,names(dt) := lapply(.SD, as.numeric)]
      dt <- data.table::melt(dt, id.vars = "gen")
      dt[,.(mean = mean(value),
            sd = stats::sd(value)),
         by = .(gen, variable)]
    })

    # remove the filename
    data[,`:=`(filename=NULL,
               summary_data = agent_summary)]

    # unlist the list column
    data <- data[, unlist(summary_data, recursive = FALSE),
         by = setdiff(names(data), "summary_data")]
  }
  return(data)
}
