#' Draw a normal curve and return key values
#'
#' Plots a normal density with mean `mu` and sd `sigma`, and returns a list
#' containing the inputs and a `probability` field (used by course tests).
#'
#' @param mu    numeric, mean of the normal.
#' @param sigma numeric > 0, standard deviation.
#' @param prob  numeric in (0,1); tail/coverage value used by tests.
#'
#' @return list with elements \code{mu}, \code{sigma}, \code{probability}.
#'
#' @examples
#' \dontrun{
#'   out <- mycurve(mu = 0, sigma = 1, prob = 0.95)
#'   str(out)
#' }
#'
#' @export
myncurve <- function(mu, sigma, a) {
  L <- mu - 3 * sigma
  U <- mu + 3 * sigma

  graphics::curve(stats::dnorm(x, mean = mu, sd = sigma),
                  from = L, to = U,
                  xlab = "x", ylab = "density",
                  main = paste0("N(", mu, ",", sigma, "^2)"))

  xx <- seq(L, min(a, U), length.out = 400)
  yy <- stats::dnorm(xx, mean = mu, sd = sigma)
  graphics::polygon(c(L, xx, min(a, U)), c(0, yy, 0))

  p <- stats::pnorm(a, mean = mu, sd = sigma)
  graphics::mtext(paste0("P(X \u2264 ", a, ") = ", round(p, 4)))

  list(mu = mu, sigma = sigma, a = a, prob = p)
}
