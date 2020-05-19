
#' Select infomove version.
#'
#' @param ssh_con Peregrine account to which to connect.
#' @param password Peregrine password.
#' @param branch Branch to be checked out.
#'
#' @return Checks out the infomove branch.
#' @export
#'
select_which_infomove <- function(ssh_con = "some server",
                         password = "some password",
                         branch = "allow_D"){
  # ssh checks the connection already
  {
    s <- ssh::ssh_connect(ssh_con, passwd = password)
  }

  # run git pull in infomove after cleaning the builds
  # and also remove job files and output files
  {
    ssh::ssh_exec_wait(s,
                  command = c("cd infomove/",
                              glue::glue('git checkout {branch}'),
                              "ml load GCC/8.3.0",
                              "ml load GSL/2.6-GCC-8.3.0",
                              "qmake infomove.pro",
                              "make clean -j4",
                              "make -j4"))
  }

  ssh::ssh_disconnect(s)
}
