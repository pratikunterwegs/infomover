#' Run the infomove simulation.
#'
#' @param phi Value (vector) of PHI values.
#' @param rho Value (vector) of RHO values.
#' @param gens Maximum number of generations.
#' @param timesteps Timesteps per generation.
#' @param init_d Initial value of giving up density D.
#' @param replicates How many replicates to run.
#'
#' @return Nothing. Runs the infomove simulation.
#' @export
run_infomove <- function(phi = 15,
             rho = 0.09,
             gens = "100000",
             timesteps = 100,
             init_d = 0.99,
             replicates = 10){
  # check all params are okay
  {
    assertthat::assert_that(all(c(phi, rho, gens, timesteps, init_d, replicates) > 0),
                            msg = "run_infomove: some arguments are negative")

  }
  # make crossing
  sim_params = data.table::CJ(phi, rho, gens, timesteps, init_d, rep=1:replicates)

  # make job files and run
  {
    shebang <- readLines("code_analysis/template_job.sh")
    purrr::pwalk(sim_params, function(phi,rho,gens,timesteps,init_d,replicates){
      if(!dir.exists("jobs")){
        dir.create("jobs")
      }
      # read basic settings
      shebang[2] <- glue::glue('#SBATCH --job-name=run_infomove_phi{phi}_rho{rho}_time{timesteps}_init_d{init_d}_rep{rep}')
      {
        command <- glue::glue('./infomove {phi} {rho} {gens} {timesteps} {init_d} {rep}')
        jobfile <- glue::glue('job_infomove_phi{phi}_rho{rho}_time{timesteps}_init_d{init_d}_rep{rep}.sh')

        writeLines(c(shebang, command), con = glue::glue('jobs/{jobfile}'))

        # run as job and delete file
        # system(command = glue::glue('sbatch {jobfile}'))
        file.remove(glue::glue('jobs/{jobfile}'))
      }
    })
  }
}
