#' Collect data for the fitness landscape.
#'
#' @param data_path Where to search for data. Must be run in the infomove folder on the cluster.
#' @param type The type of simulation to search for.
#'
#' @return Prints figures and saves data providing a global summary of all data in the data folder.
#' @import data.table
#' @export
print_fitness_landscape <- function(data_path = "data",
                                    type = "info"){
  n_count <- flr <- a <- b <- mut_combo <- fitness_data <- NULL
  energy <- filename <- NULL
  # asserts for file path
  {
    # check the data path is in the current working directory
    assertthat::assert_that(assertthat::is.dir(data_path),
                            msg = glue::glue('fitness_landscape: {data_path} not found in wd: {getwd()}'))

    # list agent data folders
    data_folders <- list.dirs(path = data_path,
                              recursive = F,
                              full.names = TRUE)

    # check if type is not ALL and subset
    {
      if(type != "all"){
        data_folders <- as.character(glue::glue('{data_path}/{type}'))
      }

      # check that these folders actually exist
      purrr::walk(data_folders, function(fol){
        assertthat::assert_that(dir.exists(fol),
                                msg = glue::glue('sim type {stringr::str_remove(fol, "data/")}
                         does not exist'))
      })
    }

    # check that there is some data
    assertthat::assert_that(length(data_folders) >= 1,
                            msg = "fitness_landscape: no output folders in data path")
  }

  # ignore folders where there is no data
  {
    data_folders <- purrr::keep(data_folders, function(fol){
      n_files <- length(list.files(path = glue::glue('{fol}/fitness_landscape'),
                                   pattern = ".csv"))
      if(n_files < 1){
        warning(glue::glue('no fitness landscape in {fol}'))
      }
      return(n_files >= 1)
    })
  }

  # walk over the type folders
  purrr::walk(data_folders, function(fol)
  {
    # acquire a lookup file
    lookup <- list.files(path = fol,
                         pattern = "lookup_eco",
                         full.names = TRUE)
    # check that there is some data
    assertthat::assert_that(length(lookup) == 1,
                            msg = "global_summary: no or multiple lookups found")
    lookup <- data.table::fread(lookup)
    # list the data files
    data_files <- list.files(path = glue::glue('{fol}/fitness_landscape'),
                             pattern = ".csv")

    # read in lookup and match to data files by filename
    {
      data <- data.table::data.table(filename = data_files)
      data <- data.table::merge.data.table(data, lookup,
                                           by = "filename")

      # check that merging worked by testing col names
      dfnames <- colnames(data)
      namesReq <- c("phi", "rho")
      for (i in 1:length(namesReq)) {
        assertthat::assert_that(namesReq[i] %in% dfnames,
                                msg = glue::glue('global_summary: {namesReq[i]} is
                         required but missing from data'))
      }
      # read the data and summarise
      agent_data <- purrr::map(glue::glue('{fol}/fitness_landscape/{data$filename}'),
                               function(df){
                                 df <- data.table::fread(df)
                                 m <- lm(energy ~ a + b, data = df)
                                 coef_data <- data.table::data.table(value = coef(m),
                                                         param = c("coef_intercept",
                                                                   "coef_a", "coef_b"))
                                 coef_data <- data.table::transpose(coef_data,
                                                                    make.names = "param")
                                 return(coef_data)
                               })
    }

    # rbind the list
    agent_data <- data.table::rbindlist(agent_data)

    # add to lookup data
    data <- cbind(data, agent_data)

    # save to file
    data.table::fwrite(x = data,
                       file = glue::glue('{fol}/data_fitness_landscape.csv'))
  })
}
