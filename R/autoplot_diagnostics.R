# Diagnostic plots of S3 objects

#' Diagnostic plots
#'
#' Autoplot methods for S3 objects in the movementsync package.
#' @param object S3 object
#' @param columns names of columns in input data.
#' @param maxpts maximum number of points to plot
#' @param time_limits tuple to restrict the timeline or a duration object.
#' @param time_breaks suggests the number of major time tick marks (Default is NULL).
#' @param ... passed to [zoo::plot.zoo()].
#' @param segments only include these segments in a SplicedView plot.
#' @param instrument instrument column name.
#' @param tactus beat column name.
#' @param horizontal make the barchart horizontal? (Default is FALSE).
#' @param expr an R expression that sets the time scale using a duration object (Default is NULL).
#'
#' @return a ggplot object.
#' @importFrom ggplot2 autoplot
#' @importFrom methods is
#' @name autoplot
#' @export
#' @examples
#' r <- get_sample_recording()
#' d <- get_duration_annotation_data(r)
#' autoplot(d)
#' o <- get_onsets_selected_data(r)
#' autoplot(o)
#' m <- get_metre_data(r)
#' autoplot(m)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y"), time_limits = c(20, 40))
#' l <- list(a = c(0, 10), b = c(20, 30), c = c(30, 60))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(v, splicing_df)
#' autoplot(sv, columns = c("LEar_x", "LEar_y", "Nose_x", "Nose_y"), time_breaks = 4, maxpts = 1000)
NULL


#' @exportS3Method
#' @rdname autoplot
autoplot.Duration <- function(object, horizontal = FALSE, ...) {

  if (horizontal) {
    ggplot2::ggplot(object) +
      ggplot2::geom_col(ggplot2::aes(x = .data$Duration, y = .data$Tier, fill = .data$In),
                        position = "stack") +
      ggplot2::labs(title = "Duration Object") +
      ggplot2::scale_fill_viridis_b() +
      ggplot2::xlab("Duration (min:sec)") +
      ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) # error

  } else {
    ggplot2::ggplot(object) +
      ggplot2::geom_col(ggplot2::aes(x = .data$Tier, y = .data$Duration, fill = .data$In),
                        position = "stack") +
      ggplot2::labs(title = "Duration Object") +
      ggplot2::scale_fill_viridis_b() +
      ggplot2::ylab("Duration (min:sec)") +
      ggplot2::scale_y_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) # error

  }

}


#' @exportS3Method
#' @rdname autoplot
autoplot.OnsetsSelected <- function(object, instrument = 'Inst', tactus = 'Matra', ...) {

  dfr_list <- object[sapply(object, class) == 'data.frame']
  df <- dplyr::bind_rows(dfr_list, .id = "Metre")
  stopifnot(instrument %in% colnames(df), tactus %in% colnames(df))

  df <- dplyr::rename(df, 'Tactus' = tactus)
  df <- df[!is.na(df[instrument]),,drop=FALSE]
  min_x <- min(df$Tactus, na.rm = TRUE)
  max_x <- max(df$Tactus, na.rm = TRUE)

  g <- if (length(unique(df$Metre)) == 1) ggplot2::theme(legend.position = 'none') else
    ggplot2::facet_wrap(~.data$Metre)

  ggplot2::ggplot(df) +
    ggplot2::geom_bar(ggplot2::aes(x = .data$Tactus, fill = .data$Metre), stat = "count") +
  #  ggplot2::scale_x_continuous(breaks = min_x:max_x, labels = base::as.character(min_x:max_x)) # + error related to hms?
    ggplot2::ylab("Onset Count") +
    ggplot2::labs(title = paste("OnsetsSelected Object:", instrument)) +
    g

}


#' @exportS3Method
#' @rdname autoplot
autoplot.Metre <- function(object, ...) {
  zoo_list <- lapply(object, function(x) zoo::zoo(c(diff(x$Time), NA), order.by = x$Time))
  z <- do.call(merge, zoo_list)

  g <- if (is.null(ncol(z))) NULL else ggplot2::facet_grid(Series ~ ., scales="free_y")
  autoplot(z) + g +
    ggplot2::labs(title = "Metre Object", subtitle = "Cycle Length") +
    ggplot2::xlab("Time (min:sec)") + ggplot2::ylab("Duration / sec") +
    ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) # hms related error

}


#' @exportS3Method
#' @rdname autoplot
autoplot.View <- function(object, columns=NULL, maxpts=1000, time_limits = c(-Inf, Inf),
                          time_breaks = NULL, expr = NULL, ...) {

  max_num_cols <- 9
  breaks <- if (is.null(time_breaks)) ggplot2::waiver() else scales::pretty_breaks(time_breaks)

  # Restrict points and columns to plot
  columns <- if (is.null(columns)) {
    if (ncol(object$df) > max_num_cols + 2)
      warning(paste("Only plotting first", max_num_cols, "data columns"))
    seq_len(min(ncol(object$df), max_num_cols + 2))[-1]
  } else c("Time", columns)
  sp <- if (nrow(object$df) > maxpts) {
    warning("Sampling rows for plotting")
    sample(nrow(object$df), maxpts)
  } else seq_len(nrow(object$df))

  df <- object$df[sp, columns, drop = FALSE]

  #if (class(time_limits) == "numeric") { # ORIG
  if (is(time_limits, "numeric")) {       # Replaced with is (TE)
    df <- df[df$Time >= time_limits[1] & df$Time <= time_limits[2],, drop = FALSE]
#  } else if (class(time_limits) == "Duration") { # ORIG
  } else if (is(time_limits, "Duration")) {       # Replaced with is (TE)     
    e <- substitute(expr)
    if (is.null(e)) stop('Need expr to determine time limits from Duration object')
    rects <- dplyr::filter(time_limits, !!e)
    time_limits <- c(-Inf, Inf)
    time_limits[1] <- min(rects$In, na.rm = TRUE)
    time_limits[2] <- max(rects$Out, na.rm = TRUE)
    df <- df[df$Time >= time_limits[1] & df$Time <= time_limits[2],, drop = FALSE]
  } else {
    stop('Cannot restrict time using this object')
  }

  zoo_list <- lapply(df[-1], function(x) zoo::zoo(x, order.by = df$Time))
  z <- do.call(merge, zoo_list)

  subtitle <- c(object$recording$stem, object$vid, object$direct, object$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  if (is.null(ncol(z))) {
    g_wrap <- NULL
    ylab <- ggplot2::ylab(columns[-1])
  } else {
    g_wrap <- ggplot2::facet_wrap(Series ~ ., scales="free_y")
    ylab <- NULL
  }

  autoplot(z) + g_wrap + ylab +
    ggplot2::labs(title = class(object)[1], subtitle = subtitle) +
    ggplot2::xlab("Time (min:sec)") +
    ggplot2::scale_x_time(breaks = breaks, labels = function(l) strftime(hms::as_hms(l), '%M:%S')) # error
}


#' @exportS3Method
#' @rdname autoplot
autoplot.SplicedView <- function(object, columns=NULL, segments=NULL,
                                 time_breaks = NULL, time_limits = c(-Inf, Inf), maxpts=1000, ...) {

  breaks <- if (is.null(time_breaks)) ggplot2::waiver() else scales::pretty_breaks(time_breaks)
  max_num_segments <- 10
  max_num_cols <- 9
  df <- object$df

  # Restrict points, columns, splices to plot
  columns <- if (is.null(columns)) {
    if (ncol(df) > max_num_cols + 3)
      warning(paste("Only plotting first", max_num_cols, "data columns"))
    colnames(df)[seq_len(min(ncol(df), max_num_cols + 3))]
  } else c("Segment", "Frame", "Time", columns)

  stopifnot(all(columns %in% colnames(df)))

  df_segments <- unique(df$Segment)
  num_segments <- length(df_segments)
  if (is.null(segments)) {
    if (num_segments > max_num_segments) {
      warning(paste("Only plotting the first", max_num_segments, "segments"))
      df <- df[df$Segment %in% df_segments[seq_len(max_num_segments)], , drop=FALSE]
      num_segments <- max_num_segments
    }
  } else {
    if (!all(segments %in% df_segments)) stop('Segments not found in SplitView')
    df <- df[df$Segment %in% segments, , drop=FALSE]
  }

  sp <- if (nrow(df) > maxpts) {
    warning("Sampling rows for plotting")
    sample(seq_len(nrow(df)), maxpts)
  } else seq_len(nrow(df))
  df <- df[sp, columns, drop = FALSE]
  df <- df[df$Time >= time_limits[1] & df$Time <= time_limits[2],, drop = FALSE]

  # Convert data to long form
  columns_to_remove <- match(c("Segment", "Frame", "Time"), colnames(df), nomatch = 0)
  long_df <- tidyr::pivot_longer(df, cols = -columns_to_remove,
                                 names_to = "Series", values_to = "Value")

  # Find Start and Duration of each Segment
  start_df <- dplyr::group_by(long_df, .data$Segment)
  start_df <- dplyr::summarize(start_df, Start = min(.data$Time, na.rm=TRUE),
                               Duration = max(.data$Time, na.rm=TRUE) - .data$Start)
  start_df <- dplyr::arrange(start_df, .data$Start)

  long_df$Segment_f <- factor(long_df$Segment, levels = start_df$Segment)

  subtitle <- c(object$recording$stem, object$vid, object$direct, object$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  # Use seconds to time scale if max Duration less than a minute
  if (max(start_df$Duration, na.rm = TRUE) < 60) {
    xlab <- ggplot2::xlab("Time (sec)")
    scale_x_time <- ggplot2::scale_x_continuous(breaks = breaks, labels = ggplot2::waiver()) # error
  } else {
    xlab <- ggplot2::xlab("Time (min:sec)")
    scale_x_time <- ggplot2::scale_x_time(breaks = breaks, labels = function(l) strftime(hms::as_hms(l), '%M:%S')) # error
  }

  ggplot2::ggplot(long_df, ggplot2::aes(x = .data$Time, y = .data$Value, col = .data$Series)) +
    ggplot2::geom_point() + ggplot2::geom_line() +
    ggplot2::labs(title = class(object)[1], subtitle = subtitle) +
    xlab + scale_x_time + # error
    ggplot2::facet_wrap(~Segment_f, scales = "free_x")
}


#' Autolayer methods
#'
#' Layers of annotation data to add to ggplots in `movementsync.
#' @param object S3 object
#' @param alpha aesthetic
#' @param fill name of column for filling.
#' @param instrument_cols instrument column names.
#' @param ... passed to geom.
#' @param time_limits tuple of time limits.
#' @param colour name of column for colouring.
#' @param expr unquoted R expression for filtering data (default is Tier =='FORM').
#' @param fill_column data column used for fill.
#' @param geom 'rect' or 'vline'.
#' @param vline_column column name for position of vertical lines.
#' @param tempo do we plot tempo with a Metre layer? (Default is FALSE).
#' @param view view object for a tempo Metre layer (Default is NULL).
#' @param columns columns for view for a tempo Metre layer (Default is NULL).
#'
#' @return ggplot geom object
#'
#' @importFrom ggplot2 autolayer
#' @name autolayer
#' @export
#' @examples
#' \donttest{
#' r<-get_recording("NIR_ABh_Puriya", fps=25)
#' o <- get_onsets_selected_data(r)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y"), maxpts=5000) + autolayer(o)
#'
#' m <- get_metre_data(r)
#' autoplot(v, columns = c("LEar_x", "LEar_y"), time_limits = c(1000, 2000)) +
#'   autolayer(m, time_limits = c(1000, 2000))
#' autoplot(v, columns = c("LEar_x", "LEar_y"), time_limits = c(1000, 2000)) +
#'   autolayer(m, tempo = TRUE, time_limits = c(1000, 2000), view = v,
#'             columns = c("LEar_x", "LEar_y"))
#'
#' d <- get_duration_annotation_data(r)
#' autoplot(m)
#' autoplot(m) + autolayer(d)
#' autoplot(m) + autolayer(d, fill_col = "Tier")
#'
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y")) +
#'   autolayer(d)
#' autoplot(v, columns = c("LEar_x", "LEar_y")) +
#'   autolayer(d, expr = Tier == "FORM" & substr(Comments, 1, 1) == "J")
#' autoplot(v, columns = c("LEar_x", "LEar_y")) +
#'   autolayer(d, geom = "vline", nudge_x = -60, size = 3, colour = "blue")
#' }
NULL

#' @exportS3Method
#' @rdname autolayer
autolayer.OnsetsSelected <- function(object, time_limits = c(-Inf, Inf), colour = "Inst.Name",
                                     fill = "Metre", alpha = 0.4, instrument_cols = NULL, ...) {
  dfr_list <- object[sapply(object, class) == 'data.frame']
  df <- dplyr::bind_rows(dfr_list, .id = "Metre")
  if (!is.null(instrument_cols)) {
    df <- tidyr::pivot_longer(df, cols = instrument_cols, names_to = "Inst.Name",
                              values_to = "Inst")
  }
  df <- dplyr::group_by(df, .data$Metre, .data$Inst.Name)
  rects <- dplyr::summarise(df, Inst_Min=min(.data$Inst, na.rm=TRUE), Inst_Max=max(.data$Inst, na.rm=TRUE))

  # Subset based on limits
  rects <- dplyr::filter(rects, .data$Inst_Max >= time_limits[1] & .data$Inst_Min <= time_limits[2])
  rects$Inst_Min <- ifelse(rects$Inst_Min < time_limits[1], time_limits[1], rects$Inst_Min)
  rects$Inst_Max <- ifelse(rects$Inst_Max > time_limits[2], time_limits[2], rects$Inst_Max)

  ggplot2::geom_rect(
    data = rects,
    ggplot2::aes(xmin = .data$Inst_Min, xmax = .data$Inst_Max, ymin = -Inf, ymax = Inf,
                 colour = !!ggplot2::sym(colour), fill = !!ggplot2::sym(fill)),
    alpha = alpha)
}


#' @exportS3Method
#' @rdname autolayer
autolayer.Metre <- function(object, time_limits = c(-Inf, Inf), colour = "hotpink", alpha = 0.5,
                            tempo = FALSE, view = NULL, columns = NULL, ...) {
  if (tempo) {
    is_tempo_available <- all(sapply(object, function(r) "Tempo_Hz" %in% colnames(r)))
    if (!is_tempo_available) stop("No tempo data for this recording")
    if (is.null(view) || is.null(columns) || !"View" %in% class(view))
      stop("Need a view object and columns for a tempo layer")
    l <- lapply(object, function(y) y[c("Time", "Tempo_Hz")])
    dfr <- dplyr::bind_rows(l)
    dfr <- dplyr::filter(dfr, .data$Time >= time_limits[1] & .data$Time <= time_limits[2])

    view_df <- view$df[columns]
    max_view <- apply(view_df, 2, max, na.rm = TRUE)
    min_view <- apply(view_df, 2, min, na.rm = TRUE)
    max_tempo <- max(dfr$Tempo_Hz, na.rm = TRUE)
    min_tempo <- min(dfr$Tempo_Hz, na.rm = TRUE)
    scale_factor <- (max_view - min_view) / (max_tempo - min_tempo)
    scale_view <- sapply(names(max_view),
                         function(x)  scale_factor[x] * (dfr$Tempo_Hz - min_tempo) + min_view[x])
    scaled_dfr <- data.frame(Time = dfr$Time, scale_view)
    long_dfr <- tidyr::pivot_longer(scaled_dfr, columns, names_to = 'Series', values_to = 'Value')

    ggplot2::geom_line(ggplot2::aes(x = .data$Time, y = .data$Value),
                       colour = colour, alpha = alpha, data = long_dfr, ...)
  } else {
    x <- unlist(lapply(object, function(y) y[["Time"]]))

    x[x < time_limits[1]] <- NA
    x[x > time_limits[2]] <- NA

    ggplot2::geom_vline(xintercept = x, colour = colour, alpha = alpha, ...)
  }
}


#' @exportS3Method
#' @rdname autolayer
autolayer.Duration <- function(object, time_limits = c(-Inf, Inf), expr = .data$Tier == "FORM",
                               fill_column = "Comments", geom = "rect", vline_column = "In", ...) {
  df <- as.data.frame(object)
  e <- substitute(expr)
  rects <- dplyr::filter(df, !!e)

  # Subset based on limits
  rects <- dplyr::filter(rects, .data$Out >= time_limits[1] & .data$In <= time_limits[2])
  rects$In <- ifelse(rects$In < time_limits[1], time_limits[1], rects$In)
  rects$Out <- ifelse(rects$Out > time_limits[2], time_limits[2], rects$Out)

  # order the fill column for legend
  rects[fill_column] <- factor(rects[[fill_column]], levels = unique(rects[[fill_column]]))

  l <- list(...)

  if (geom == "rect") {
    ggplot2::geom_rect(
      data = rects,
      ggplot2::aes(xmin = .data$In, xmax = .data$Out, ymin = -Inf, ymax = Inf, fill = .data[[fill_column]]),
      alpha = 0.4)
  } else if (geom == "vline") {
    colour <-  if ("colour" %in% names(l)) l[["colour"]] else "black"
    c(ggplot2::geom_vline(data = rects, linetype = 3, colour = colour, ggplot2::aes(xintercept = .data[[vline_column]])),
      ggplot2::geom_text(
        data = rects,
        ggplot2::aes(x = .data[[vline_column]], y = Inf, angle = 90, hjust = "inward", label = paste(vline_column, .data[[fill_column]])),
        ...)
      )
  } else stop("Unsupported geom")
}


#' @exportS3Method
#' @rdname autolayer
autolayer.Splice <- function(object, geom = "rect", vline_column = "Start", ...) {

  rects <- object
  # order the Segment column for legend
  rects['Segment'] <- factor(rects[['Segment']], levels = unique(rects[['Segment']]))

  l <- list(...)

  if (geom == "rect") {
    ggplot2::geom_rect(
      data = rects,
      ggplot2::aes(xmin = .data$Start, xmax = .data$End, ymin = -Inf, ymax = Inf,
                   fill = .data[['Segment']]),
      alpha = 0.4)
  } else if (geom == "vline") {
    colour <-  if ("colour" %in% names(l)) l[["colour"]] else "black"
    c(ggplot2::geom_vline(data = rects, linetype = 3, colour = colour, ggplot2::aes(xintercept = .data[[vline_column]])),
      ggplot2::geom_text(
        data = rects,
        ggplot2::aes(x = .data[[vline_column]], y = -Inf, angle = 90, hjust = "inward", label = paste(vline_column, .data[['Segment']])),
        ...)
    )
  } else stop("Unsupported geom")
}


#' Get a ggplot2 xlim object based on duration data
#'
#' @param object Duration object.
#' @param expr R expression to subset rows.
#'
#' @return a 'Duration' object.
#' @examples
#' \donttest{
#' r<-get_recording("NIR_ABh_Puriya", fps=25)
#' m <- get_metre_data(r)
#' d <- get_duration_annotation_data(r)
#' autoplot(m)
#' autoplot(m) + autolayer(d)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y")) + autolayer(d)
#' autoplot(v, columns = c("LEar_x", "LEar_y")) +
#' xlim_duration(d, expr = Tier == "FORM" & substr(Comments, 1, 1) == "J") +
#' autolayer(d, expr = Tier == "FORM" & substr(Comments, 1, 1) == "J")
#' }
#' @export
xlim_duration <- function(object, expr = .data$Tier == "Form") {
  e <- substitute(expr)
  rects <- dplyr::filter(object, !!e)
  xmin <- min(rects$In, na.rm = TRUE)
  xmax <- max(rects$Out, na.rm = TRUE)

  ggplot2::xlim(xmin, xmax)
}


