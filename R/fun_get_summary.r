#' Get summary data from the infomove simulation.
#' @param ssh_con The ssh address. Typically a p-number at peregrine.hpc.rug.nl. This function is meant to be used within infomove.
#' @param password Connection password.
#' @param type The simulation type. Currently "info" or "noinfo". May also be all.
#'
#' @return Nothing. Gets summary data from the infomove simulation.
#' @export
get_summary <- function(ssh_con = "some server",
                         password = "some password",
                         type = "noinfo"){

    # get data folders
    if(type != "all"){
        data_folders <- as.character(glue::glue('data/{type}'))
      }

    # check parameters are okay
    # check that these folders actually exist locally
    purrr::walk(data_folders, function(fol){
        assertthat::assert_that(dir.exists(fol),
                                msg = glue::glue('sim type {stringr::str_remove(fol, "data/")} does not exist'))
    })

    # connect over ssh and get files
    s <- ssh::ssh_connect(ssh_con, passwd = password)

    # list files
    purrr::walk(data_folders, function(fol){
        # find the correct files on peregrine home
        a <- ssh::ssh_exec_internal(s, glue::glue('find ./infomove/{fol} -maxdepth 1 -type f'))
        a <- unlist(strsplit(rawToChar(a$stdout), "\n"))

        # walk over the files and get them into the right local dir
        purrr::walk(a, function(this_file){
            ssh::scp_download(s, this_file, to = glue::glue('{fol}'))
        })
    })
}

# ends here
