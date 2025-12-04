#' Simulate sampling distribution of Poisson sample means
#' @param n Sample size
#' @param iter Number of iterations
#' @param lambda Poisson mean
#' @return Numeric vector of sample means (and histogram)
#' @export
mycltp <- function(n, iter, lambda) {
  y    <- rpois(n * iter, lambda)
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  w    <- apply(data, 2, mean)
  hist(w, probability = TRUE,
       main = paste0("n=", n, ", λ=", lambda),
       xlab = "Sample mean")
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda / n)),
        add = TRUE, col = "red", lty = 2, lwd = 3)
  w
}
