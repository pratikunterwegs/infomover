#' Make landscape values.
#'
#' @param size Total number of landscape patches.
#' @param phi_peaks Number of resource peaks.
#' @param rho_error Error around the sine function.
#'
#' @return A vector of landscape values.
#' @export
make_land <- function(size = 5000,
                      phi_peaks = 100,
                      rho_error = 0.1)
{
  b = scales::rescale(sin(2*pi*c(1:size)*phi_peaks/size) + stats::rnorm(size, rho_error))

  return(b)
}
