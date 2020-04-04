#' Print a global summary.
#'
#' @param data_path Where to search for data.
#'
#' @return Prints figures and saves data providing a global summary of all data in the data folder.
#' @import data.table
#' @export
#'
#' @examples
print_global_summary <- function(data_path = "data"){
  id <- value <- round_value <- gen <- variable <- summary_data <- NULL
  # asserts for file path
  {
    # check the data path is in the current working directory
    assertthat::assert_that(assertthat::is.dir(data_path),
    msg = glue::glue('global_summary: {data_path} not found in wd: {getwd()}'))

    # list agent data folders
    data_folders <- list.dirs(path = data_path,
                            recursive = F,
                            full.names = TRUE)
    # check that there is some data
    assertthat::assert_that(length(data_folders) >= 1,
                          msg = "global_summary: no output folders in data path")
  }
  # walk over the type folders
  purrr::walk(data_folders, function(fol){
    # acquire a lookup file
    lookup <- list.files(path = fol,
                         pattern = "lookup",
                         full.names = TRUE)
    # check that there is some data
    assertthat::assert_that(length(lookup) == 1,
                            msg = "global_summary: no or multiple lookups found")
    lookup <- data.table::fread(lookup)

    # list the data files
    data_files <- list.files(path = glue::glue('{fol}/agent_summary'),
                             pattern = ".csv")

    # read in lookup and match to data files by filename
    {
      data <- data.table::data.table(filename = data_files)
      data <- data.table::merge.data.table(data, lookup,
                                           by = "filename")

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
      agent_data <- purrr::map(glue::glue('{fol}/agent_summary/{data$filename}'),
                               data.table::fread)
      agent_summary <- purrr::map(agent_data, function(dt){
        dt[,id:=NULL]

        # convert all cols to numeric
        dt <- dt[,lapply(.SD, as.numeric)]
        dt <- data.table::melt(dt, id.vars = "gen")
        dt[,.(mean = mean(value),
              sd = stats::sd(value)),
           by = .(gen, variable)]
      })

      # remove the filename
      tmp_data <- data
      tmp_data[,`:=`(filename=NULL,
                     summary_data = agent_summary)]

      # unlist the list column
      tmp_data <- tmp_data[, unlist(summary_data, recursive = FALSE),
                           by = setdiff(names(tmp_data), "summary_data")]
    }
    # save summary to file
    {
      data.table::fwrite(x = tmp_data, file = glue::glue('{fol}/data_global_summary.csv'))
    }
    # get proportion data for plots
    {
      agent_summary <- purrr::map(agent_data, function(dt){
        # convert all cols to numeric
        dt <- dt[,lapply(.SD, as.numeric)]
        dt <- data.table::melt(dt, id.vars = "gen")
        dt[,round_value := round(value, 1)]

        dt[,.(count = .N),
           by = .(gen, variable, round_value)]
      })
    }
    # add to parameter data
    tmp_data <- data
    tmp_data[,`:=`(summary_data = agent_summary)]

    # unlist the list column
    tmp_data <- tmp_data[, unlist(summary_data, recursive = FALSE),
                         by = setdiff(names(tmp_data), "summary_data")]
    # save summary to file
    {
      data.table::fwrite(x = tmp_data, file = glue::glue('{fol}/data_global_counts.csv'))
    }
  })
}
