# Functions to assess and plot periodicity of movement data

#' Estimate the spectral density of data points
#'
#' Estimates the periodicity of data points in a `View` object.
#'
#' @param view `ProcessedView` or `FilteredView` object.
#' @param data_points data points to process e.g. Nose.
#' @param ... passed to [stats::spectrum()].
#' @param columns names of data columns e.g. Nose_x.
#'
#' @return `SpectralDensityView` object.
#' @export
#' @examples
#' r<-get_recording("NIR_ABh_Puriya", fps=25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' sd1 <- spectral_density(pv, columns = "LEar_x", spans = 5)
#'
#' fv <- apply_filter_sgolay(pv, data_points = c("LEye"), n = 19, p = 4)
#' sd1 <- spectral_density(fv, data_points = c("LEye"), spans = 5)

spectral_density <- function(view, columns = NULL, data_points = NULL, ...) {
  stopifnot(any(c("ProcessedView", "FilteredView") %in% class(view)))

  df <- view$df
  if (!is.null(data_points)) {
    x_colnames <- paste0(data_points, "_x")
    y_colnames <- paste0(data_points, "_y")
    d_colnames <- paste0(data_points, "_d")
    cn <- c(colnames(df)[1:2], rbind(x_colnames, y_colnames, d_colnames))
    df <- df[, cn, drop=FALSE]
  } else if (!is.null(columns)) {
    cn <- c(colnames(df)[1:2], columns)
    df <- df[, cn, drop=FALSE]
  } else stop()

  sampling_rate <-view$recording$fps

  # periodigram using fft with smoothing
  spec <-
    stats::spectrum(
      df[, -(1:2), drop=FALSE],
      plot = FALSE,
      na.action = stats::na.omit,
      log = 'no',
      ...
    )

  spx <- spec$freq * sampling_rate # to get cycles per second
  spy <- as.data.frame(spec$spec)
  colnames(spy) <- colnames(df)[-(1:2)]
  output_df <- data.frame(Period = 1 / spx, spy)

  l <- list(df = output_df, view = view)
  class(l) <- "SpectralDensityView"

  l
}


#' Autoplot a SpectralDensityView S3 object
#'
#' @param object `SpectralDensityView` object.
#' @param period_range tuple for limiting range of periods.
#' @param colour name of line colour.
#' @param ... ignored.
#'
#' @return a `ggplot` object.
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' sd1 <- spectral_density(pv, columns = c("LElbow_x", "LEye_x"), spans = 5)
#' autoplot(sd1)
#'
#' fv <- apply_filter_sgolay(pv, data_points = c("LElbow", "LEye"), n = 19, p = 4)
#' sd2 <- spectral_density(fv, data_points = c("LElbow", "LEye"), spans = c(3, 3))
#' autoplot(sd2)

autoplot.SpectralDensityView <- function(object, period_range = c(0, 10), colour = "blue", ...) {

  view <- object$view
  df <- object$df

  long_df <- tidyr::pivot_longer(df, colnames(df[-1]))
  colnames(long_df) <- c("Period", "DataPoint", "Density")

  title <- c(view$recording$stem, view$vid, view$direct, view$inst)
  title <- paste(title[title != ""], collapse="_")

  # Restrict time domain
  long_df <- dplyr::filter(long_df, .data$Period >= period_range[1] &
                             .data$Period <= period_range[2])

  ggplot2::ggplot(long_df, ggplot2::aes(x = .data$Period, y = .data$Density)) +
  ggplot2::geom_line(colour = colour) +
  ggplot2::xlab('Period / min:sec') +
  ggplot2::ylab('Spectral Density') +
  ggplot2::labs(title = class(object)[1], subtitle = title) +
  ggplot2::facet_grid(DataPoint ~ ., scales = "free") +
  ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) # error
}


#' Specgram Plot
#'
#' @param obj `View` object.
#' @param ... passed to [signal::specgram()].
#'
#' @return a `ggplot` object.
#' @export
#'
#' @examples
#' \donttest{
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' sub_pv <- subset(pv, Time >= 15 & Time <= 25, columns = c("RWrist_x", "RWrist_y"))
#' specgram_plot(sub_pv)
#' fv <- apply_filter_sgolay(pv, data_points = c("RWrist"), n = 11, p = 4)
#' sub_fv <- subset(fv, Time >= 15 & Time <= 25, columns = c("RWrist_x", "RWrist_y"))
#' specgram_plot(sub_fv)
#' specgram_plot(sub_fv, window = 200) + ggplot2::scale_fill_gradient(low = "white", high = "black")
#' }
specgram_plot <- function(obj, ...) {
  stopifnot("View" %in% class(obj))

  start_time <- obj$df[1, "Time"]
  df <- obj$df[-(1:2)]
  data_point <- colnames(df)

  sp_list <- lapply(df, signal::specgram, Fs = obj$recording$fps, ...)
  df_list <- list()
  for (i in seq_along(data_point)) {
    df_list[[data_point[i]]] <- expand.grid(X = sp_list[[i]]$t + start_time, Y = sp_list[[i]]$f)
    df_list[[data_point[i]]]$Z <- as.numeric(20 * log10(t(abs(sp_list[[i]]$S))))
  }
  long_df <- dplyr::bind_rows(df_list, .id = "DataPoint")

  subtitle <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  jet <- grDevices::colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
                                       "yellow", "#FF7F00", "red", "#7F0000"))

  ggplot2::ggplot(long_df) +
    ggplot2::geom_tile(ggplot2::aes(.data$X, .data$Y, fill= .data$Z)) +
    ggplot2::scale_fill_gradientn(colours = jet(20)) +
    ggplot2::labs(title = paste("Specgram for", class(obj)[1]), subtitle = subtitle) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("Time (min:sec)") +
    ggplot2::ylab("Frequency / per sec") +
    ggplot2::scale_x_time(expand = c(0,0), labels = function(l) strftime(hms::as_hms(l), '%M:%S')) + # error
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$DataPoint))
}


