#' Plot error curves
#'
#' Plots the estimated and true error curves for iterates of an
#' optimization algorithm using ggplot2.
#'
#'
#' @param hat numeric vector representing the errors between iterates
#' and the final iterate
#' @param true numeric vector representing the errors between iterates
#' and the ground trush
#' @param up upper limit of vertical axis
#' @param low lower limit of vertical axis
#' @param left left limit of horizontal axis
#' @param right right limit of horizontal axis
#' @param auto calculate limit automatically (default FALSE; if TRUE axes limits are ignored)
#'
#' @return A ggplot object.
#'
#' @note
#' See also \code{\link[tsmvr]{tsmvr_solve}} and
#' \code{\link{error_curve}}.
#'
#' @export
plot_error_curve <- function(hat, star, up = 1, low = -5, left = 0, right = 100,
                             auto = T) {
  k <- length(hat)
  stopifnot(k == length(star))
  hat <- log2(hat)
  star <- log2(star)

  # If auto == T, calculate automatic axes limits.
  if (auto == T) {
    left <- 0

    # Tricky one is the left horizontal axis limit. First find
    # where the star error curve flattens within a tolerance "x",
    # and then plot some extra to the right of that (5 extra
    # iterations if x < 25 or 20% more if x > 25).
    tol <- 1e-3
    r.logical <- which((gradient(star) < tol) == T)
    if (length(r.logical) == 0) {
      right <- length(star)
    } else {
      x <- min(r.logical)
      if (x < 25) {
        right <- x + 5
      } else {
        right <- round(1.2 * x)
      }
    }

    # Find general place of left-right axis limits, and then
    # scale so there is some scaled white space below and above
    # the two curves.
    slace <- 0.15
    y <- max(c(hat[0], star[0]))
    z <- min(c(hat[right], star[right]))
    if (y <= z) { # (enzure y > z, and if not flip)
      t <- y
      y <- z
      z <- t
    }
    if (y >= 0) {
      up <- (1 + scale) * y
    } else {
      up <- (1 - scale) * y
    }
    if (z <= 0) {
      low <- (1 + scale) * z
    } else {
      low <- (1 - scale) * z
    }
  }

  # Place error curve vectors into dataframe for plotting.
  df <- as.data.frame(star)
  df$hat <- hat
  names(df) <- c("Truth", "Estimate")
  df <- melt(df)
  df$iteration <- 1:k

  # Plot the error curves.
  plt <- ggplot(df, aes(x = iteration, y = value, colour = variable)) +
    geom_line(size = 1) +
    xlab("t") +
    xlim(left, right) +
    ylab("log2 Frobenius norm") +
    ylim(low, up) +
    scale_color_manual(
      labels =
        c(
          expression(group("|", B^t - B^"*", "|")[F]),
          expression(group("|", B^t - hat(B), "|")[F])
        ),
      values = c("blue", "red")
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = c(0.22, 0.25),
      legend.background = element_rect(
        color = "black",
        linetype = "solid", size = 0.3
      ),
      axis.text = element_text(size = 20),
      axis.title = element_text(size = 24, face = "bold"),
      legend.text = element_text(size = 20, face = "bold")
    )

  # Return the plot handle.
  return(plt)
}
