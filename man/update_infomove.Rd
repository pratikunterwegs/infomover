% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_update_infomove.r
\name{update_infomove}
\alias{update_infomove}
\title{Update infomove from Github.}
\usage{
update_infomove(ssh_con = "some server", password = "some password")
}
\arguments{
\item{ssh_con}{Peregrine account on which infomove lives.}

\item{password}{Peregrine account password.}
}
\value{
Updates infomove using a git pull.
}
\description{
Update infomove from Github.
}
