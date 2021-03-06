#' Get the local fitness landscape.
#'
#' @param type The simulation type. Currently "info" or "noinfo".
#' @param phi Value (vector) of PHI values.
#' @param rho Value (vector) of RHO values.
#' @param timesteps Timesteps per generation.
#' @param a_res The resident a strategy.
#' @param b_res The resident b strategy.
#' @param M_res The resident M strategy.
#' @param leader_choices How many leaders can be assessed.
#' @param n_rep How many replicates to run.
#' @param gradient The gradient increment by which to shift the parameters.
#' @param gradient_m The gradient increment for M
#'
#' @return Nothing. Runs the infomove simulation for local fitness landscape.
#' @export

run_infomove_fl <- function(type = "noinfo",
                            phi = 15,
                            rho = 0.1,
                            timesteps = 500,
                            a_res = 1.0,
                            b_res = 1.0,
                            M_res = 2,
                            leader_choices = c(1,2,5),
                            gradient = 0.5,
                            gradient_m = 2,
                            n_rep = 10){
  # make crossing of parameters
  sim_params <- data.table::CJ(type, phi, rho, timesteps,
                              a_res, b_res, M_res,
                              leader_choices, gradient, gradient_m,
                              n_rep)

  # first check the branch is correct
  branches <- system("git branch", intern = TRUE)
  # select the branch with the asterik
  this_branch <- branches[stringr::str_detect(branches, "\\*")]
  assertthat::assert_that(stringr::str_detect(this_branch, "eco"),
                          msg = "this is not the eco branch!")

  # prepare for sim
  sim_prep <- glue::glue('ml purge
                          module load GCC/8.3.0
                          module load GSL/2.6-GCC-8.3.0
                          ml list')

  # assuming the right branch, print commands to a shell script
  sim_commands <- glue::glue_data(sim_params,
                  './infomove {type} {phi} {rho} {timesteps} {a_res} \\
                  {b_res} {M_res} {leader_choices} {gradient} {gradient_m} {n_rep}')

  # write to a shell script after clearing the old one
  if(file.exists("jobs/infomove_fitness_landscapes.sh")){
    file.remove("jobs/infomove_fitness_landscapes.sh")
  }
  writeLines(text = c(sim_prep,
                      sim_commands),
             con = "jobs/infomove_fitness_landscapes.sh")

  # convert to executable
  system(command = "chmod +x jobs/infomove_fitness_landscapes.sh")

  # message that things have happened
  message("written sim shell commands to file")

  # now run the function
  system(command = "./jobs/infomove_fitness_landscapes.sh")

}
