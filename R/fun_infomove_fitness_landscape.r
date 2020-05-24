#' Get the local fitness landscape.
#'
#' @param type The simulation type. Currently "info" or "noinfo".
#' @param phi Value (vector) of PHI values.
#' @param rho Value (vector) of RHO values.
#' @param timesteps Timesteps per generation.
#' @param a_res The resident a strategy.
#' @param b_res The resident b strategy.
#' @param leader_choices How many leaders can be assessed.
#' @param n_rep How many replicates to run.
#' @param gradient The gradient increment by which to shift the parameters.
#'
#' @return Nothing. Runs the infomove simulation for local fitness landscape.
#' @export

run_infomove_fl <- function(type = "noinfo",
                            phi = 15,
                            rho = 0.1,
                            timesteps = 500,
                            a_res = 1.0,
                            b_res = 1.0,
                            leader_choices = c(1,2,5),
                            gradient = 0.5,
                            n_rep = 10){
  # make crossing of parameters
  sim_params <- data.table::CJ(type, phi, rho, timesteps, a_res, b_res,
                              leader_choices, gradient, n_rep)

  # first check the branch is correct
  branches <- system("git branch", intern = TRUE)
  # select the branch with the asterik
  this_branch <- branches[stringr::str_detect(branches, "\\*")]
  assertthat::assert_that(stringr::str_detect(this_branch, "eco"),
                          msg = "this is not the eco branch!")

  # assuming the right branch, print commands to a shell script
  sim_commands <- glue::glue_data(sim_params,
                  'infomove {type} {phi} {rho} {timesteps} {a_res} \\
                  {b_res} {leader_choices} {gradient} {n_rep}')

  # write to a shell script after clearing the old one
  if(file.exists("jobs/infomove_fitness_landscapes.sh")){
    file.remove("jobs/infomove_fitness_landscapes.sh")
  }
  writeLines(text = sim_commands,
             con = "jobs/infomove_fitness_landscapes.sh")

  # convert to executable
  system(command = "chmod +x jobs/infomove_fitness_landscapes.sh")

  # message that things have happened
  message("written sim shell commands to file")

  # now run the function
  system("./jobs/infomove_fitness_landscapes.sh")

}
