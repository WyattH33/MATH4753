#' Optimal tickets to sell with overbooking
#'
#' Computes the optimal number of tickets to sell when a flight has N seats,
#' show-up probability is \code{p}, and overbooking risk tolerance is \code{gamma}.
#' Returns the discrete solution (binomial) and the normal-approximation solution,
#' and draws both objective plots.
#'
#' @param N integer seats on the plane.
#' @param gamma numeric in (0, 1); allowed probability of overbooking (e.g., 0.02).
#' @param p numeric in (0, 1); probability a ticketed passenger shows (e.g., 0.95).
#'
#' @return A named list with elements \code{nd}, \code{nc}, \code{N}, \code{p}, \code{gamma}.
#' @examples
#' \dontrun{
#'   ntickets(N = 400, gamma = 0.02, p = 0.95)
#' }
#' @export
ntickets <- function(N, gamma, p){
  stopifnot(length(N)==1, length(gamma)==1, length(p)==1)
  stopifnot(is.finite(N), is.finite(gamma), is.finite(p))
  stopifnot(N == as.integer(N), N > 0, 0 < gamma & gamma < 1, 0 < p & p < 1)

  # ----- DISCRETE (binomial) -----
  # Objective: find the **largest** n with P(X <= N | X~Bin(n,p)) >= 1 - gamma
  ngrid  <- seq.int(N, ceiling(N/p) + 50L)         # safe search window
  Fvals  <- stats::pbinom(q = N, size = ngrid, prob = p)
  target <- 1 - gamma
  feas   <- which(Fvals >= target)
  if (length(feas) == 0L) stop("Search grid too small; increase upper bound.")
  nd <- ngrid[tail(feas, 1L)]                      # <-- correct: LAST feasible n

  # Plot (discrete)
  plot(ngrid, Fvals, type = "p", xlab = "n", ylab = "Objective",
       main = sprintf("Objective Vs n to find optimal tickets sold\n(%d) gamma=%.02f N=%d discrete", nd, gamma, N))
  abline(h = target, col = "red", lwd = 2)
  abline(v = nd, col = "red", lwd = 2)

  # ----- CONTINUOUS (normal approx) -----
  # Solve 1 - gamma = Phi( (N + 0.5 - n*p)/sqrt(n*p*(1-p)) )
  # Rearranged via uniroot on f(n) = (1-gamma) - pnorm(...)
  f <- function(n){
    mu  <- n * p
    sd  <- sqrt(n * p * (1 - p))
    (1 - gamma) - stats::pnorm((N + 0.5 - mu) / sd)
  }
  nc <- stats::uniroot(f, interval = c(N, ceiling(N/p) + 50))$root

  # Plot (continuous)
  nseq <- seq(N, ceiling(N/p) + 50, by = 0.1)
  obj  <- (1 - gamma) - stats::pnorm((N + 0.5 - nseq * p) / sqrt(nseq * p * (1 - p)))
  plot(nseq, obj, type = "l", xlab = "n", ylab = "Objective",
       main = sprintf("Objective Vs n to find optimal tickets sold\n(%.3f) gamma=%.02f N=%d continuous", nc, gamma, N))
  abline(h = 0, col = "blue", lwd = 2)
  abline(v = nc, col = "blue", lwd = 2)

  list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
}
