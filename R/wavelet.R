# Functions for wavelet analysis

#' Analyze Wavelet from View object
#'
#' @param obj View object.
#' @param column Column in view to analyse.
#' @param loess.span parameter alpha in loess controlling the degree of time series smoothing, if the time series is to be detrended; no detrending if loess.span = 0.
#' Default: 0.
#' @param dj frequency resolution. Default 1/20.
#' @param lowerPeriod lower Fourier period in seconds. Defaults to 2/fps.
#' @param upperPeriod upper Fourier period in seconds. Defaults to 5s.
#' @param make.pval see [WaveletComp::analyze.wavelet()].
#' @param method see [WaveletComp::analyze.wavelet()].
#' @param params see [WaveletComp::analyze.wavelet()].
#' @param n.sim number of simulations (default 1).
#' @param date.format see [WaveletComp::analyze.wavelet()].
#' @param date.tz see [WaveletComp::analyze.wavelet()].
#' @param verbose see [WaveletComp::analyze.wavelet()].
#'
#' @return an `analyze.wavelet` object.
#' @family wavelet functions
#' @export
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' w <- analyze_wavelet(pv, "Nose_y")

analyze_wavelet <- function(obj, column, loess.span = 0, dj = 1/20,
                            lowerPeriod = 2 / obj$recording$fps, upperPeriod = 5,
                            make.pval = TRUE, method = "white.noise", params = NULL,
                            n.sim = 1, date.format = NULL, date.tz = NULL, verbose = TRUE) {
  stopifnot("View" %in% class(obj), column %in% colnames(obj$df))

  df <- obj$df
  fps <- obj$recording$fps
  dt <- 1 / fps

  w <- WaveletComp::analyze.wavelet(
    df, column, loess.span = loess.span, dt = dt, dj = dj,
    lowerPeriod = lowerPeriod * fps, upperPeriod = upperPeriod * fps, make.pval = make.pval,
    n.sim = n.sim, date.format = date.format, date.tz = date.tz, verbose = verbose
  )

  subtitle <- c(obj$vid, obj$direct, obj$inst, column)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")
  w$subtitle <- subtitle

  invisible(w)
}


#' Analyze Coherency from View object
#'
#' @param obj View object.
#' @param columns Two column names.
#' @param loess.span parameter alpha in loess controlling the degree of time series smoothing, if the time series is to be detrended; no detrending if loess.span = 0.
#' Default: 0.
#' @param dj frequency resolution. Default 1/20.
#' @param lowerPeriod in seconds
#' @param upperPeriod in seconds
#' @param window.type.t see [WaveletComp::analyze.coherency()].
#' @param window.type.s see [WaveletComp::analyze.coherency()].
#' @param window.size.t see [WaveletComp::analyze.coherency()].
#' @param window.size.s see [WaveletComp::analyze.coherency()].
#' @param make.pval see [WaveletComp::analyze.coherency()].
#' @param method see [WaveletComp::analyze.coherency()].
#' @param params see [WaveletComp::analyze.coherency()].
#' @param n.sim number of simulations (default 1).
#' @param date.format see [WaveletComp::analyze.coherency()].
#' @param date.tz see [WaveletComp::analyze.coherency()].
#' @param verbose see [WaveletComp::analyze.coherency()].
#'
#' @return an `analyze_coherency` object.
#' @family wavelet functions
#' @export
#'
#' @examples
#'
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, c("Nose_x", "Nose_y"))
#' 

analyze_coherency <- function(obj, columns, loess.span = 0,
  dj = 1/50, lowerPeriod = 2 / obj$recording$fps, upperPeriod = 5, window.type.t = 1,
  window.type.s = 1, window.size.t = 5,
  window.size.s = 1/4, make.pval = TRUE, method = "white.noise",
  params = NULL, n.sim = 1, date.format = NULL, date.tz = NULL,
  verbose = FALSE) {

  stopifnot("View" %in% class(obj), length(columns) == 2,
            all(columns %in% colnames(obj$df)))

  df <- obj$df
  fps <- obj$recording$fps
  dt <- 1 / fps

  co <- WaveletComp::analyze.coherency(
    df, my.pair = columns, loess.span = loess.span, dt = dt,
    dj = dj, lowerPeriod = lowerPeriod * fps, upperPeriod = upperPeriod * fps, window.type.t = window.type.t,
    window.type.s = window.type.s, window.size.t = window.size.t,
    window.size.s = window.size.s, make.pval = make.pval, method = method,
    params = params, n.sim = n.sim, date.format = date.format, date.tz = date.tz,
    verbose = verbose)

  subtitle <- c(obj$vid, obj$direct, obj$inst, columns)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")
  co$subtitle <- subtitle

  invisible(co)

}


#' Plot a power spectrum of a wavelet object
#'
#' @param obj analyze.wavelet object.
#' @param view View object.
#' @param ... passed to [WaveletComp::wt.image()].
#'
#' @return a list of class `graphical parameters`.
#' @export
#' @family wavelet functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time >= 30)
#' w <- analyze_wavelet(pv1, "Nose_y")
#' plot_power_spectrum(w, pv1)
#' w <- analyze_wavelet(pv1, "Nose_y", lowerPeriod = 0.01, upperPeriod = 10)
#' plot_power_spectrum(w, pv1)
plot_power_spectrum <- function(obj, view, ...) {
  stopifnot("analyze.wavelet" %in% class(obj), "View" %in% class(view))

  df <- view$df
  fps <- view$recording$fps
  spec_time_axis <- make_time_axis(df, fps)

  axis.2 <- obj$axis.2
  period.tick <- unique(trunc(axis.2))
  period.tick[period.tick < log2(obj$Period[1])] <- NA
  period.tick <- stats::na.omit(period.tick)
  period.tick.label <- 2^(period.tick) / fps
  spec_period_axis <-  list(at = 2^period.tick, labels = period.tick.label)

  WaveletComp::wt.image(
    obj,
    n.levels = 250,
    spec.time.axis = spec_time_axis,
    spec.period.axis = spec_period_axis,
    legend.params = list(lab = "Wavelet Power Levels", mar = 4.7),
    periodlab = "Period / sec",
    timelab = spec_time_axis$time_lab,
    main = paste("Power Spectrum for", view$recording$stem, "-", obj$subtitle),
    ...
  )

}


#' Plot a coherency of a wavelet object
#'
#' @param obj analyze.coherency object.
#' @param view View object.
#' @param ... passed to [WaveletComp::wc.image()].
#'
#' @return a list of class `graphical parameters`,
#' @export
#' @family wavelet functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time >= 10)
#' co <- analyze_coherency(pv1, c("Nose_x", "Nose_y"))
#' plot_cross_spectrum(co, pv1)
#' plot_coherence(co, pv1)
plot_cross_spectrum <- function(obj, view,  ...) {
  stopifnot("analyze.coherency" %in% class(obj), "View" %in% class(view))

  arg_list <- list(...)
  df <- view$df
  fps <- view$recording$fps
  spec_time_axis <- make_time_axis(df, fps)

  axis.2 <- obj$axis.2
  period.tick <- unique(trunc(axis.2))
  period.tick[period.tick < log2(obj$Period[1])] <- NA
  period.tick <- stats::na.omit(period.tick)
  period.tick.label <- 2^(period.tick) / fps
  spec_period_axis <-  list(at = 2^period.tick, labels = period.tick.label)

  if ("which.image" %in% names(arg_list) && arg_list$which.image == "wc") {
    main_title <-paste("Wavelet Coherence for", view$recording$stem, "-", obj$subtitle)
    legend_label <- "Wavelet Coherence Levels"
  } else {
    main_title <-paste("Cross Wavelet Power Spectrum for", view$recording$stem, "-", obj$subtitle)
    legend_label <- "Cross Wavelet Power Levels"
  }

  WaveletComp::wc.image(obj, n.levels = 250,
    spec.time.axis = spec_time_axis,
    spec.period.axis = spec_period_axis,
    legend.params = list(lab = legend_label, mar = 4.7),
    periodlab = "Period",
    main = main_title,
    timelab = spec_time_axis$time_lab,
    ...
  )
}

#' @rdname plot_cross_spectrum
#' @export
plot_coherence <- function(obj, view, ...) {
  plot_cross_spectrum(obj, view, which.image = "wc")
}

#' Plot average power of a wavelet object
#'
#' @param obj analyze.wavelet object.
#' @param view View object.
#' @param ... passed to [WaveletComp::wt.avg()].
#'
#' @return a ggplot object.
#' @export
#' @family wavelet functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time >= 10)
#' w <- analyze_wavelet(pv1, "Nose_x")
#' plot_average_power(w, pv1)
#' w <- analyze_wavelet(pv1, "Nose_y")
#' plot_average_power(w, pv1)

plot_average_power <- function(obj, view, ...) {
  stopifnot("analyze.wavelet" %in% class(obj), "View" %in% class(view))

  df <- view$df
  fps <- view$recording$fps

  axis.2 <- obj$axis.2
  period.tick <- unique(trunc(axis.2))
  period.tick[period.tick < log2(obj$Period[1])] <- NA
  period.tick <- stats::na.omit(period.tick)
  period.tick.label <- 2^(period.tick) / fps
  spec_period_axis <-  list(at = 2^period.tick, labels = period.tick.label)

  WaveletComp::wt.avg(
    obj,
    spec.period.axis = spec_period_axis,
    main = paste("Average Wavelet Power for", view$recording$stem, "-", obj$subtitle),
    averagelab = "Average Wavelet Power",
    periodlab = "Period / sec",
    ...
  )
}


#' Plot wavelet energy of a wavelet object
#'
#' @param obj analyze.wavelet object.
#' @param view View object.
#'
#' @return a ggplot object.
#' @export
#' @family wavelet functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time >= 10)
#' w <- analyze_wavelet(pv1, "Nose_x")
#' plot_wt_energy(w, pv1)

plot_wt_energy <- function(obj, view) {
  stopifnot("analyze.wavelet" %in% class(obj), "View" %in% class(view))

  df <- view$df
  fps <- view$recording$fps
  spec_time_axis <- make_time_axis(df, fps)
  min_time <- min(df$Time, na.rm = TRUE)

  power_sum <- colSums(obj$Power)

  dfr <- data.frame(x = obj$axis.1 - 1 + min_time,
                    WT_Energy = power_sum / max(power_sum, na.rm = TRUE))

  ggplot2::ggplot(dfr) +
    ggplot2::geom_line(ggplot2::aes(x = .data$x, y = .data$WT_Energy)) +
    ggplot2::labs(title = "WT Energy Time Series", subtitle = paste(view$recording$stem, "-", obj$subtitle)) +
    ggplot2::xlab("Time (min:sec)") +
    ggplot2::ylab("WT Energy") +
    ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) # error

}


#' Plot cross wavelet energy of a wavelet object
#'
#' @param obj analyze.wavelet object.
#' @param view View object.
#'
#' @return a ggplot object.
#' @export
#' @family wavelet functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, columns = c("Nose_x", "Nose_y"))
#' plot_cwt_energy(co, pv)

plot_cwt_energy <- function(obj, view) {
  stopifnot("analyze.coherency" %in% class(obj), "View" %in% class(view))

  df <- view$df
  fps <- view$recording$fps
  spec_time_axis <- make_time_axis(df, fps)
  min_time <- min(df$Time, na.rm = TRUE)

  power_sum <- colSums(obj$Power.xy)

  dfr <- data.frame(x = obj$axis.1 - 1 + min_time,
                    WT_Energy = power_sum / max(power_sum, na.rm = TRUE))

  ggplot2::ggplot(dfr) +
    ggplot2::geom_line(ggplot2::aes(x = .data$x, y = .data$WT_Energy)) +
    ggplot2::labs(title = "CWT Energy Time Series", subtitle = paste(view$recording$stem, "-", obj$subtitle)) +
    ggplot2::xlab("Time (min:sec)") +
    ggplot2::ylab("CWT Energy") +
    ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) # error

}


#' Plot average coherency of a coherency object
#'
#' @param obj analyze.coherency object.
#' @param view View object.
#' @param ... passed to [WaveletComp::wc.avg()].
#'
#' @return a ggplot object.
#' @export
#' @family wavelet functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, columns = c("Nose_x", "Nose_y"))
#' plot_average_coherency(co, pv)
plot_average_coherency <- function(obj, view, ...) {
  stopifnot("analyze.coherency" %in% class(obj), "View" %in% class(view))

  df <- view$df
  fps <- view$recording$fps

  axis.2 <- obj$axis.2
  period.tick <- unique(trunc(axis.2))
  period.tick[period.tick < log2(obj$Period[1])] <- NA
  period.tick <- stats::na.omit(period.tick)
  period.tick.label <- 2^(period.tick) / fps
  spec_period_axis <-  list(at = 2^period.tick, labels = period.tick.label)

  WaveletComp::wc.avg(
    obj,
    spec.period.axis = spec_period_axis,
    main = paste("Average Coherency for", view$recording$stem, "-", obj$subtitle),
    averagelab = "Average Coherency",
    periodlab = "Period / sec",
    ...
  )

}


#' Comparison plot of phases of a coherency object
#'
#' @param obj coherency object.
#' @param view View object.
#' @param sel.period a single number which determines the (closest available) Fourier period to be selected. Default: NULL.
#' @param sel.lower a number to define a lower Fourier period (or the closest available)
#'  for the selection of a band of periods (effective if sel.period is NULL). Default: NULL.
#' @param sel.upper a number to define an upper Fourier period (or the closest available)
#'  for the selection of a band of periods (effective if sel.period is NULL). Default: NULL.
#' @param ... passed to [WaveletComp::wc.sel.phases()].
#'
#' @return an object of class `sel.phases`.
#' @export
#' @family wavelet functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, columns = c("Nose_x", "Nose_y"))
#' plot_cross_spectrum(co, pv)
#' plot_sel_phases(co, pv, sel.period = 0.64)
#' plot_sel_phases(co, pv, sel.lower = 0.6, sel.upper = 0.8)
plot_sel_phases <- function(obj, view, sel.period = NULL, sel.upper = NULL,
                            sel.lower = NULL, ...) {
  stopifnot("analyze.coherency" %in% class(obj), "View" %in% class(view))
  stopifnot(!is.null(sel.period) || (!is.null(sel.lower) && !is.null(sel.upper)),
            !(is.null(sel.lower) && is.null(sel.upper) && is.null(sel.period)))

  df <- view$df
  fps <- view$recording$fps
  spec_time_axis <- make_time_axis(df, fps)

  sub_title <- if (is.null(sel.period))
    paste("Selected Period Interval:", sel.lower, "to", sel.upper, "secs") else
      paste("Selected Period:", sel.period, "secs")

  sel_phases <- WaveletComp::wc.sel.phases(
    obj,
    sel.period = sel.period * fps,
    sel.upper = sel.upper * fps,
    sel.lower = sel.lower * fps,
    spec.time.axis = spec_time_axis,
    main = paste("Phase comparison for", view$recording$stem, "-", obj$subtitle),
    phaselab = "Phase",
    timelab = spec_time_axis$time_lab,
    sub = sub_title,
    ...
  )
  sel_phases$subtitle <- paste(view$recording$stem, "-", obj$subtitle)

  invisible(sel_phases)
}


#' Plot a coherency of a wavelet object
#'
#' @param obj analyze.coherency object.
#' @param view View object.
#' @param ... passed to [WaveletComp::wc.phasediff.image()].
#'
#' @return a list of class `graphical parameters`
#' @export
#' @family wavelet functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time >= 10 & Time <= 20)
#' co <- analyze_coherency(pv1, c("Nose_x", "Nose_y"))
#' plot_phase_difference(co, pv1)
plot_phase_difference <- function(obj, view,  ...) {
  stopifnot("analyze.coherency" %in% class(obj), "View" %in% class(view))

  arg_list <- list(...)
  df <- view$df
  fps <- view$recording$fps
  spec_time_axis <- make_time_axis(df, fps)

  axis.2 <- obj$axis.2
  period.tick <- unique(trunc(axis.2))
  period.tick[period.tick < log2(obj$Period[1])] <- NA
  period.tick <- stats::na.omit(period.tick)
  period.tick.label <- 2^(period.tick) / fps
  spec_period_axis <-  list(at = 2^period.tick, labels = period.tick.label)

  main_title <-paste("Global phase differences for", view$recording$stem, "-", obj$subtitle)
  legend_label <- "Phase difference levels"

  WaveletComp::wc.phasediff.image(
    obj, n.levels = 250,
    spec.time.axis = spec_time_axis,
    spec.period.axis = spec_period_axis,
    legend.params = list(lab = legend_label, mar = 4.7),
    periodlab = "Period",
    main = main_title,
    timelab = spec_time_axis$time_lab,
    ...
  )
}


# helper to generate a time axis in min/sec for wavelet plots
make_time_axis <- function(df, fps, num_tlabels = 10) {

  min_time <- min(df$Time, na.rm = TRUE)
  max_time <- max(df$Time, na.rm = TRUE)

  min_frame <- min(df$Frame, na.rm = TRUE)
  max_frame <- max(df$Frame, na.rm = TRUE)
  labels_at <- seq(0, (max_frame - min_frame), by = (max_frame - min_frame) / num_tlabels)
  labels_sec <- labels_at / fps + min_time

  if (max_time - min_time < 60) {
    labels_to <- labels_sec
    time_lab <- "Time / sec"
  } else {
    labels_to <- paste0(
      formatC(labels_sec %/% 60, width=2, flag = 0),
      ":",
      formatC(floor(labels_sec %% 60), width = 2, flag = 0))
    time_lab <- "Time (min:sec)"
  }
  list(at = labels_at + 1, labels = labels_to, time_lab = time_lab)
}


#' Plot windowed resultant length
#'
#' @param obj a `sel.phases` object.
#' @param window_duration duration of window over which to take mean (default is 1 sec).
#' @param smooth use the smoothed phase angle data (default is FALSE).
#' @param by calculate resultant length at every `by`-th time point rather than every point.
#' @param ref_lines names list of reference line values (default is `c(W = 0.7, M = 0.85, H = 0.95)`).
#' @param align alignment of window (default is 'right').
#' @param na.rm Remove NAs from the circular mean (default is TRUE).
#'
#' @return a `ggplot` object.
#' @export
#' @family wavelet functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, columns = c("Nose_x", "Nose_y"))
#' sp <- plot_sel_phases(co, pv, sel.period = 0.64)
#' plot_roll_resultant_length(sp, ref_lines = c(H = 0.9998))
plot_roll_resultant_length <- function(obj, window_duration = 1, smooth = FALSE,
                                       by = 1, ref_lines = c(W = 0.7, M = 0.85, H = 0.95),
                                       align = 'right', na.rm = TRUE) {
  stopifnot(class(obj) == 'sel.phases')

  angle <- if (smooth) obj$sAngle else obj$Angle
  tm <- obj$axis.1
  max_tm <- max(tm, na.rm = TRUE)
  min_tm <- min(tm, na.rm = TRUE)

  # Convert to zoo and rollapply rho.circular
  window_length <- round(window_duration / (max_tm - min(tm))  * (length(tm) - 1))
  z <- zoo::zoo(angle, order.by = tm - 1)
  rollz <- zoo::rollapply(z, width = window_length,
                          FUN = function(x) circular::rho.circular(circular::circular(x), na.rm = na.rm),
                          by = by, align = align)

  ref_line_df <- data.frame(Sync = paste(names(ref_lines), ' '), Value = ref_lines)

  g <- autoplot(rollz) +
    ggplot2::labs(title = "Relative Phase Analysis", subtitle = obj$subtitle) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = .data$Value, colour = .data$Sync, linetype = .data$Sync),
                        linewidth = 1, alpha = 0.9, data = ref_line_df, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(Inf, .data$Value, label = .data$Sync, hjust = 'inward'),
                       vjust = -0.5, data = ref_line_df) +
    ggplot2::scale_colour_brewer(palette = 'Reds', direction = -1) +
    ggplot2::xlab("Time (min:sec)") + ggplot2::ylab("Rolling Mean Resultant Length")  +
    ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) + # error
    ggplot2::theme_bw()

  g
}

#' Get periods locally maximal average power
#'
#' @param obj `analyze.wavelet` object.
#' @param v `View` object.
#'
#' @return data.frame of Period and Local Maxima.
#'
#' @export
#' @family wavelet functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time >= 10)
#' w <- analyze_wavelet(pv1, "Nose_x")
#' plot_average_power(w, pv1)
#' get_local_max_average_power(w, pv1)

get_local_max_average_power <- function(obj, v) {
  stopifnot(class(obj) == "analyze.wavelet", 'View' %in% class(v))

  fps <- v$recording$fps
  period <- 2^obj$axis.2 / fps
  ave_power <- zoo::zoo(obj$Power.avg)
  is_peak <- zoo::rollapply(ave_power, 3, function(x) which.max(x) == 2)
  peak_index <- zoo::index(is_peak)[zoo::coredata(is_peak)]

  data.frame(
    Local_Maxmima = ave_power[peak_index],
    Period = period[peak_index]
  )

}
