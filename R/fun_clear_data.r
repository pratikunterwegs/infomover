
#' Clear simulation data.
#'
#' @param ssh_con Peregrine account to which to connect.
#' @param password Peregrine password.
#'
#' @return Checks out the infomove branch.
#' @export
#'
clear_infomove_data <- function(ssh_con = "some server",
                         password = "some password"){
  # ssh checks the connection already
  {
    s <- ssh::ssh_connect(ssh_con, passwd = password)
  }

  # run git pull in infomove after cleaning the builds
  # and also remove job files and output files
  {
    ssh::ssh_exec_wait(s, c("cd infomove/data",
                       'find . -name "*.csv" -type f -delete',
                       'cd ..',
                       "cd jobs",
                       "rm *.sh *.out"))
  }

  ssh::ssh_disconnect(s)

  message("infomove data cleared from Peregrine")
}

# ends here
