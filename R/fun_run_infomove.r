#' Run the infomove simulation.
#' @param ssh_con The ssh address. Typically a p-number at peregrine.hpc.rug.nl
#' @param password Connection password.
#' @param type The simulation type. Currently "info" or "noinfo".
#' @param phi Value (vector) of PHI values.
#' @param rho Value (vector) of RHO values.
#' @param gens Maximum number of generations.
#' @param timesteps Timesteps per generation.
#' @param init_d Initial Value of aspiration level D.
#' @param leader_choices How many leaders can be assessed.
#' @param rep_n How many rep_n to run.
#'
#' @return Nothing. Runs the infomove simulation.
#' @export
run_infomove <- function(ssh_con = "some server",
                         password = "some password",
                         type = "noinfo",
                         phi = 15,
                         rho = 0.1,
                         gens = "100000",
                         timesteps = 100,
                         init_d = 0.5,
                         leader_choices = 2,
                         rep_n = 5){
  # check all params are okay
  {
    assertthat::assert_that(all(c(phi, rho, timesteps, init_d, leader_choices,
                                  rep_n) > 0),
                            msg = "run_infomove: some arguments are negative")
  }
  # check ssh connection
  # this is automatically checked by ssh
  {
    s <- ssh::ssh_connect(ssh_con, passwd = password)
  }

  # make crossing
  sim_params = data.table::CJ(type, phi, rho, gens, timesteps,
                              init_d, leader_choices, rep=1:rep_n)

  # make job files and run
  {
    shebang <- readLines("code_analysis/template_job.sh")
    purrr::pwalk(sim_params, function(type, phi,rho,gens,timesteps,
                                      init_d,leader_choices,rep_n){
      if(!dir.exists("jobs")){
        dir.create("jobs")
      }

      # read basic settings
      shebang[2] <- glue::glue('#SBATCH --job-name=run_infomove_type-{type}_phi{phi}_nlead{leader_choices}_rep{rep_n}')

      # make command and write to file, then upload file to connection s
      {
        command <- glue::glue('./infomove {type} {phi} {rho} {gens} {timesteps} {init_d} {leader_choices} {rep_n}')

        jobfile <- glue::glue('job_infomove_type{type}_phi{phi}_nlead{leader_choices}_rep{rep_n}.sh')

        writeLines(c(shebang, command), con = glue::glue('jobs/{jobfile}'))
        ssh::scp_upload(s, glue::glue('jobs/{jobfile}'), to = "infomove/jobs/")
        file.remove(glue::glue('jobs/{jobfile}'))
      }

      # schedule job
      ssh::ssh_exec_wait(s, command = c("cd infomove/jobs",
                                   glue::glue('dos2unix {jobfile}'),
                                   glue::glue('sbatch {jobfile}')))

    })

    ssh::ssh_disconnect(s)
  }
}
