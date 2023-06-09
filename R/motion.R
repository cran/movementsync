# Functions to build a motion_gram style plot

#' Plot a set of data points over time
#'
#' @param obj `View` object.
#' @param maxpts maximum number of points to plot.
#'
#' @return a `ggplot` object.
#' @export
#'
#' @examples
#' r1 <- get_sample_recording()
#' rv1 <- get_raw_view(r1, "Central", "", "Sitar")
#' pv1 <- get_processed_view(rv1)
#' fv1 <- apply_filter_sgolay(pv1, data_points = c("LElbow", "RElbow"), n = 41, p = 3)
#' sub_fv1 <- subset(fv1, Time >= 0 & Time <= 100, by = 10)
#' plot_history_xy(sub_fv1)
plot_history_xy <- function(obj, maxpts=10000) {
  df <- obj$df
  stopifnot(nrow(df) < maxpts)

  data_point <- get_data_points(obj)
  x_dp <- paste(data_point, "x", sep = "_")
  y_dp <- paste(data_point, "y", sep = "_")

  df_list <- list()
  for (i in seq_along(data_point)) {
    xend <- c(df[[x_dp[i]]][-1], NA)
    yend <- c(df[[y_dp[i]]][-1], NA)
    df_list[[data_point[i]]] <- cbind(df[c("Time", x_dp[i], y_dp[i])], xend, yend)
    names(df_list[[data_point[i]]]) <- c("Time", "x", "y", "xend", "yend")
  }
  df1 <- dplyr::bind_rows(df_list, .id = "DataPoint")

  subtitle <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  ggplot2::ggplot(df1, ggplot2::aes(.data$x, .data$y, alpha = .data$Time, colour = .data$DataPoint)) +
    ggplot2::labs(title = paste("DataPoint History of", class(obj)[1]), subtitle = subtitle) +
    ggplot2::geom_point(alpha = 0.1, shape = 19) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"), type = "closed"))

}


#' Distribution plot of a view object
#'
#' @param obj `View` object.
#' @param alpha ggplot aesthetic value.
#' @param maxpts maximum number of points to plot.
#' @param ... passed to [ggplot2::geom_point()],
#'
#' @return a `ggplot` object.
#' @export
#'
#' @examples
#' r1 <- get_sample_recording()
#' rv1 <- get_raw_view(r1, "Central", "", "Sitar")
#' pv1 <- get_processed_view(rv1)
#' dp <- c("LWrist", "RWrist", "LElbow", "RElbow", "LEye", "REye", "Neck", "MidHip")
#' fv1 <- apply_filter_sgolay(pv1, data_point = dp, n = 41, p = 4)
#' distribution_dp(fv1)
distribution_dp <- function(obj, maxpts = 50000, alpha = 0.1, ...) {
  df <- obj$df
  stopifnot(nrow(df) < maxpts)

  data_point <- get_data_points(obj)
  x_dp <- paste(data_point, "x", sep = "_")
  y_dp <- paste(data_point, "y", sep = "_")

  df_list <- list()
  for (i in seq_along(data_point)) {
    df_list[[data_point[i]]] <- df[c("Time", x_dp[i], y_dp[i])]
    names(df_list[[data_point[i]]]) <- c("Time", "x", "y")
  }
  df1 <- dplyr::bind_rows(df_list, .id = "DataPoint")

  subtitle <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  ggplot2::ggplot(df1, ggplot2::aes(.data$x, .data$y, colour = .data$DataPoint, stroke = NA)) +
    ggplot2::labs(title = paste("Distribution of", class(obj)[1]), subtitle = subtitle) +
    ggplot2::geom_point(alpha = alpha, shape = 19, ...) +
    ggplot2::guides(colour=ggplot2::guide_legend(override.aes=list(alpha=1, size=3)))
}


#' Velocity plot of a view object
#'
#' @param obj View object.
#' @param alpha ggplot aesthetic value.
#' @param maxpts maximum number of points to plot.
#' @param ... passed to [ggplot2::geom_point()],
#' @param add_mean add the mean to each line? (default is TRUE).
#' @param vscale a vertical scaling to apply to the plot (default is 5).
#'
#' @return a `ggplot` object.
#' @export
#'
#' @examples
#' r1 <- get_sample_recording()
#' rv1 <- get_raw_view(r1, "Central", "", "Sitar")
#' pv1 <- get_processed_view(rv1)
#' dp <- c("LWrist", "RWrist", "LElbow", "RElbow", "LEye", "REye", "Neck", "MidHip")
#' fv1 <- apply_filter_sgolay(pv1, data_point = dp, n = 41, p = 4)
#' sub_fv1 <- subset(fv1, Time >= 10 & Time <= 20, by = 2)
#' velocity_dp(sub_fv1)
velocity_dp <- function(obj, add_mean = TRUE, vscale = 5, maxpts = 10000, alpha = 0.5, ...) {
  df <- obj$df
  stopifnot(nrow(df) < maxpts)

  data_point <- get_data_points(obj)
  y_dp <- paste(data_point, "y", sep = "_")
  d_dp <- paste(data_point, "d", sep = "_")

  if (add_mean) {
    col_means <- colMeans(df[y_dp], na.rm = TRUE)
    print(col_means)
    for (i in seq_along(d_dp)) {
      df[d_dp[i]] <- df[d_dp[i]] * vscale + col_means[y_dp[i]]
    }
  }

  long_df <- tidyr::pivot_longer(df[c("Time", d_dp)], cols = d_dp,
                                 names_to = "DataPoint", values_to = "d")

  subtitle <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  ggplot2::ggplot(long_df, ggplot2::aes(.data$Time, .data$d, colour = .data$DataPoint)) +
    ggplot2::labs(title = paste("Velocity of", class(obj)[1]), subtitle = subtitle) +
    ggplot2::geom_point(alpha = alpha, ...) +
    ggplot2::geom_line() +
    ggplot2::guides(colour=ggplot2::guide_legend(override.aes=list(alpha=1, size=3))) +
    ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) # error

}

#' Motion gram of a view object
#'
#' @param obj view object
#' @param alpha ggplot aesthetic value.
#' @param maxpts maximum number of points to plot.
#' @param ... passed to [ggplot2::geom_point()],
#'
#' @return a `gtable` object.
#' @export
#'
#' @examples
#' r1 <- get_sample_recording()
#' rv1 <- get_raw_view(r1, "Central", "", "Sitar")
#' pv1 <- get_processed_view(rv1)
#' dp <- c("LWrist", "RWrist", "LElbow", "RElbow", "LEye", "REye", "MidHip")
#' fv1 <- apply_filter_sgolay(pv1, data_point = dp, n = 41, p = 4)
#' sub_fv1 <- subset(fv1, Time >= 0 & Time <= 20, dp, by = 2)
#' motion_gram(sub_fv1)
motion_gram <- function(obj, maxpts = 10000, alpha =0.5, ...) {
  subtitle <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  p1 <- distribution_dp(obj, maxpts = 10000, alpha = alpha)
  g <- ggplot2::ggplotGrob(p1 + ggplot2::theme(legend.position = "right"))$grobs

  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  p1 <- p1 + ggplot2::theme(legend.position = "none") +
    ggplot2::xlab(NULL) + ggplot2::ylab(NULL) +
    ggplot2::labs(title = NULL, subtitle = NULL) + ggplot2::ylim(c(0, 1))

  p2 <- velocity_dp(obj) +
    ggplot2::xlab(NULL) +  ggplot2::ylim(c(0, 1)) + ggplot2::ylab(NULL) +
    ggplot2::labs(title = NULL, subtitle = NULL) +
    ggplot2::theme(axis.title.y=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank(),
          axis.ticks.y=ggplot2::element_blank())

  grided <- gridExtra::grid.arrange(p1, p2, nrow = 1,
                                    top = paste("Motiongram:", subtitle),
                                    widths=c(0.4, 0.6))

  invisible(grided)
}

