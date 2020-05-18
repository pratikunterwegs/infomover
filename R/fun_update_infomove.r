
#' Update infomove from Github.
#'
#' @param ssh_con Peregrine account on which infomove lives.
#' @param password Peregrine account password.
#'
#' @return Updates infomove using a git pull.
#' @export
update_infomove <- function(ssh_con = "some server",
                         password = "some password"){
  # ssh checks the connection already
  {
    s <- ssh::ssh_connect(ssh_con, passwd = password)
  }

  # run git pull in infomove
  {
    ssh::ssh_exec_wait(s,
                  command = c("cd infomove/",
                              "rm Makefile infomove",
                              "cd jobs",
                              "rm *.sh",
                              "rm *.out",
                              "cd ..",
                              "git pull"))
  }

  ssh::ssh_disconnect(s)
}
