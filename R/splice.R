# Functions to return a splice: a set of time intervals for analysis

#' S3 generic function to splice a timeline
#'
#' @param x S3 object.
#' @param ... passed to relevant method.
#'
#' @return a `Splice` object.
#' @export
#' @family splicing functions

splice_time <- function(x, ...) {
  UseMethod("splice_time", x)
}


#' Generate spliced timeline using an `OnsetsDifference` object
#'
#' @param x `OnsetsDifference` object.
#' @param window_duration duration of window around onset point in seconds.
#' @param ... passed to [make.unique()].
#' @param metres vector of metres to subset.
#' @param make.unique give unique names to each segment?
#'
#' @return a `Splice` object.
#' @exportS3Method
#' @family splicing functions
#'
#' @examples
#' r <- get_sample_recording()
#' o1 <- get_onsets_selected_data(r)
#' po1 <- difference_onsets(o1, instruments = c('Inst', 'Tabla'))
#' splicing_df <- splice_time(po1, window_duration = 1)
#' head(splicing_df)
splice_time.OnsetsDifference <- function(x, window_duration, metres = NULL, make.unique = TRUE, ...) {
  stopifnot(all(metres %in% unique(x$Metre)))
  if (!is.null(metres)) {
    df <- dplyr::filter(x, .data$Metre %in% unique(x$Metre))
  } else {
    df <- x
  }

  # Generate Splicing table
  df$Start <- df$Ref_Beat_Time - window_duration / 2
  df$End <- df$Ref_Beat_Time + window_duration / 2
  df$Segment <- paste('Reference_Beat', base::as.character(df$Segment), sep="_")
  df <- df[match(c("Segment", "Start", "End"), colnames(df), nomatch = 0)]
  df <- stats::na.omit(df)

  if (make.unique) df$Segment <- make.unique(df$Segment, ...)
  output_df <- dplyr::arrange(df, .data$Start)

  class(output_df) <- c('Splice', 'data.frame')

  output_df
}


#' Generate spliced timeline using a Metre object
#'
#' @param x `Metre` object.
#' @param window_duration duration of window around beat
#' (may lead to overlapping windows if large).
#' @param window_proportion sets the window duration around beat based on a
#' proportion (0, 0.5] of the gap to the previous and following cycles. The first
#' and last beats in each Metre are removed.
#' @param tactus vector of Metres to subset on.
#' @param ... ignored.
#'
#' @return a `Splice` object.
#' @exportS3Method
#' @family splicing functions
#'
#' @examples
#' r <- get_sample_recording()
#' m <- get_metre_data(r)
#' splicing_df <- splice_time(m, window_duration = 1)
#' head(splicing_df)
#' splicing_df <- splice_time(m, window_proportion = 0.25)
#' head(splicing_df)
splice_time.Metre <- function(x, window_duration = NULL, window_proportion = NULL,
                              tactus = NULL, ...) {
  if (is.null(tactus)) tactus <- names(x)
  stopifnot(
    all(tactus %in% names(x)),
    (!is.null(window_duration) || !is.null(window_proportion)),
    !(!is.null(window_duration) && !is.null(window_proportion))
  )

  df_list <- list()
  if (!is.null(window_duration)) {
    for (j in seq_along(tactus)) {
      df <- x[[j]]
      df$Start <- df$Time - window_duration / 2
      df$End <- df$Time + window_duration / 2
      df <- df[-match(c("Time", "Notes", "Beats"), colnames(df), nomatch = 0)]
      colnames(df)[1] <- "Segment"
      df$Segment <- paste(tactus[j], 'Cycle', base::as.character(df$Segment), sep="_")
      df_list[[j]] <- df
    }

  } else {
    for (j in seq_along(tactus)) {
      df <- x[[j]]
      diff_time <- diff(df$Time)
      diff_time1 <- c(diff_time[1], diff_time)
      diff_time2 <- c(diff_time, diff_time[length(diff_time)])
      df$Start <- df$Time - window_proportion * diff_time1
      df$End <- df$Time + window_proportion * diff_time2
      df <- df[-match(c("Time", "Notes", "Beats"), colnames(df), nomatch = 0)]
      colnames(df)[1] <- "Segment"
      df$Segment <- paste(tactus[j], 'Cycle', base::as.character(df$Segment), sep="_")
      # Remove first and last beat
      df <- df[-c(1, nrow(df)),, drop = FALSE]
      df_list[[j]] <- df
    }
  }

  output_df <- dplyr::bind_rows(df_list)
  output_df <- dplyr::arrange(output_df, .data$Start)
  if (is_splice_overlapping(output_df)) warning("Splice has overlapping segments")

  class(output_df) <- c('Splice', 'data.frame')

  output_df
}


#' Generate spliced timeline using a list
#'
#' @param x named list.
#' @param ... ignored.
#'
#' @return a `Splice` object.
#' @exportS3Method
#' @family splicing functions
#'
#' @examples
#' l <- list(a = c(0, 10), b = c(10, 20), c = c(20, 30))
#' splice_time(l)
splice_time.list <- function(x, ...) {
  stopifnot(length(x) > 0, all(sapply(x, length) == 2), all(sapply(x, function(x) x[1] < x[2])))

  df <- t(as.data.frame(x))
  rownames(df) <- NULL
  colnames(df) <- c("Start", "End")
  df <- cbind.data.frame(Segment = names(x), df)
  df <- df[c("Segment", "Start", "End")]

  stopifnot(!is_splice_overlapping(df))
  class(df) <- c('Splice', 'data.frame')

  df
}


#' Generate spliced timeline using a Duration object
#'
#' @param x `Duration` object.
#' @param expr R expression to filter data on.
#' @param make.unique make the segments unique? (Default is TRUE).
#' @param ... passed to [make.unique()]
#' @param tier exact tier name to filter on.
#' @param comments exact comment to filter on.
#'
#' @return a `Splice` object.
#' @exportS3Method
#' @family splicing functions
#'
#' @examples
#' r <- get_sample_recording()
#' d <- get_duration_annotation_data(r)
#' splice_time(d, tier = 'Event', comments = 'tabla solo')
splice_time.Duration <- function(x, expr = NULL, make.unique = TRUE,
                                 tier = NULL, comments = NULL, ...) {
  stopifnot(!is.null(expr) || (!is.null(tier) || !is.null(comments)))

  if (is.null(expr)) {
    expr_list <- list()
    if (!is.null(tier)) {
      tier <- paste0('c(', paste0(shQuote(tier), collapse = ","), ')')
      expr_list[[1]] <- paste0('Tier %in% ', tier)
    } else {
      expr_list[[1]] <- NA
    }
    if (!is.null(comments)) {
      comments <- paste0('c(', paste0(shQuote(comments), collapse = ","), ')')
      expr_list[[2]] <- paste0('Comments %in% ', comments)
    } else {
      expr_list[[2]] <- NA
    }
    expr_list <- expr_list[!is.na(expr_list)]
    expr <- if (length(expr_list) > 1) paste0(expr_list, collapse = " & ") else expr_list[[1]]
  }

  expr <- rlang::parse_expr(expr)
  df <- dplyr::filter(as.data.frame(x), !!expr)
  df <- df[c("Comments", "In", "Out")]
  if (make.unique) df$Comments <- make.unique(df$Comments, ...)
  colnames(df) <- c("Segment", "Start", "End")

  class(df) <- c('Splice', 'data.frame')

  df
}


#' Generate spliced timeline using a view
#'
#' @param x `View` object.
#' @param ... ignored.
#' @param win_size duration of window segment in seconds.
#' @param step_size increment in seconds between segments.
#'
#' @return a `Splice` object.
#' @exportS3Method
#' @family splicing functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' df <- splice_time(rv, win_size = 3, step_size = 0.5)
#' head(df)
splice_time.View <- function(x, win_size, step_size, ...) {
  stopifnot(win_size > 0, step_size > 0)

  tm <- x$df$Time
  min_time <- min(tm, na.rm = TRUE)
  max_time <- max(tm, na.rm = TRUE)

  if (max_time > win_size) {
    offset <- seq(min_time, max_time - win_size, by = step_size)
  } else {
    stop("Time series length too small for window")
  }

  df <- data.frame(Segment = paste0("w", seq_along(offset)), Start = offset, End = offset + win_size)
  class(df) <- c('Splice', 'data.frame')

  df
}


#' Get spliced view from view object
#'
#' @param v View object
#' @param splicing_df `Splice` object.
#'
#' @return a `SplicedView` object.
#' @export
#' @family splicing functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' l <- list(a = c(0, 10), b = c(10, 20), c = c(20, 30))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(rv, splicing_df)
get_spliced_view <- function(v, splicing_df) {
  stopifnot("View" %in% class(v), "Splice" %in% class(splicing_df),
            class(splicing_df[['Segment']]) == "character")
  df <- v$df
  fps <- v$recording$fps

  df_list <- list()
  for (r in seq_len(nrow(splicing_df))) {
    segment <- splicing_df$Segment[r]
    spliced_df <- df[df$Time >= splicing_df$Start[r] & df$Time <= splicing_df$End[r],, drop = FALSE]
    # Segments with the same name get appended to df_list[[segment]]
    df_list[[segment]] <- dplyr::bind_rows(df_list[[segment]], spliced_df)

  }
  output_df <- dplyr::bind_rows(df_list, .id = "Segment")
  output_df <- dplyr::arrange(output_df, .data$Frame, .data$Segment)

  l <- list(df = output_df, splicing_df = splicing_df, vid = v$vid,
            direct = v$direct, inst = v$inst, recording = v$recording)
  class(l) <- c("SplicedView", class(v))

  invisible(l)
}


#' Get a list of Views from a SplicedView
#'
#' @param x `SplicedView` object.
#' @param f ignored.
#' @param drop ignored.
#' @param ... ignored.
#'
#' @return list of `View` objects.
#' @exportS3Method
#' @family splicing functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' l <- list(a = c(0, 10), b = c(10, 20), c = c(20, 30))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(pv, splicing_df)
#' v_list <- split(sv)
split.SplicedView <- function(x, f, drop, ...) {

  # Preserve the ordering in the data.frame when splitting
  fct <- factor(x$df[['Segment']], levels = unique(x$df[['Segment']]))
  df_list <- split(x$df, fct)

  v_list <- lapply(df_list, function(df) {
    new_dfr <- df[, colnames(df) != "Segment", drop = FALSE]
    l <- list(df = new_dfr, vid = x$vid, direct = x$direct,
         inst = x$inst, recording = x$recording)
    class(l) <- class(x)[-1]
    l
  })

  v_list
}


#' Sample the time line from a list of Views
#'
#' @param ... names arguments of `SplicedView` objects.
#' @param num_samples number of time points to sample
#' @param replace sample with replacement (default is FALSE)?
#' @param na.action function to deal with NAs in data (default is na.pass).
#'
#' @return a list of `SplitView` object or a `SplitView` object
#' @export
#'
#' @examples
#' r1 <- get_sample_recording()
#' fv1_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p = 3)
#' jv1 <- get_joined_view(fv1_list)
#' l <- list(a=c(1, 2), b = c(2, 3))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(jv1, splicing_df = splicing_df)
#' autoplot(sv)
#' sv_new <- sample_time_spliced_views(sv, num_samples = 10, replace = FALSE)
#' autoplot(sv_new)
#' sv_new <- sample_time_spliced_views(sv, num_samples = 10, replace = TRUE)
#' autoplot(sv_new)
#' l <- list(a=c(1, 2), a = c(10, 20), b = c(30, 40))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(jv1, splicing_df = splicing_df)
#' sv_new <- sample_time_spliced_views(sv, num_samples = 20, replace = TRUE)
#' autoplot(sv_new)
sample_time_spliced_views <- function(..., num_samples, replace = FALSE, na.action = stats::na.pass) {
  input_sv <- list(...)
  stopifnot(all(sapply(input_sv, function(x) "SplicedView" %in% class(x))))
  stopifnot(num_samples > 0)

  sv_list <- list()
  i <- 1
  for (sv in input_sv) {
    dfr <- sv$df
    keys <- match(c('Segment', 'Frame', 'Time'), colnames(dfr), nomatch = 0)
    data_columns <- colnames(dfr)[-keys]

    # invert the CDF to pick equally between intervals?
    if (replace) {
      # stack the time intervals
      duration_dfr <- dplyr::mutate(
        sv$splicing_df,
        Duration = .data$End - .data$Start,
        Cumulative_Duration = cumsum(.data$Duration),
        Start_Duration = dplyr::lag(.data$Cumulative_Duration, 1, default = 0)
      )
      cumduration_times <- duration_dfr[, 'Cumulative_Duration']
      # Sample from [0, total length of intervals]
      new_cumdurations <- stats::runif(num_samples, 0, cumduration_times[length(cumduration_times)])
      # Find the interval each new time belongs to
      interval_idx <- findInterval(new_cumdurations, c(0, cumduration_times))
      # Generate a list of linear approximation functions for each data column
      new_times <- duration_dfr[interval_idx, 'Start'] + new_cumdurations -
        duration_dfr[interval_idx, 'Start_Duration']

      new_data <- sapply(
        data_columns,
        function(col) stats::approx(x = dfr[['Time']], y = dfr[[col]], xout = new_times, ties = 'ordered')$y
        )

      # Build new sampled data.frame
      new_dfr <- cbind.data.frame(Segment = duration_dfr[interval_idx, 'Segment'], Frame = NA, Time = new_times, new_data)

    } else {
      dfr <- na.action(sv$df)
      row_nums <- sample(seq_len(nrow(dfr)), size = num_samples, replace = replace)
      new_dfr <- dfr[row_nums,,drop = FALSE]
    }

    new_dfr <- new_dfr[order(new_dfr[['Time']]),,drop=FALSE]
    rownames(new_dfr) <- NULL
    sv_list[[i]] <- sv
    sv_list[[i]]$df <- new_dfr
    i <- i + 1
  }
  names(sv_list) <- names(input_sv)

  if (length(sv_list) == 1) sv_list <- sv_list[[1]]
  sv_list
}


#' Checks if splicing data.frames overlap
#'
#' @param ... Each argument can be a data frame or a list of data frames
#'
#' @return logical
#' @export
#' @family splicing functions
#'
#' @examples
#' l1 <- list(a=c(1, 10), a = c(20, 30), b = c(30, 40))
#' dfr1 <- splice_time(l1)
#' l2 <- list(a=c(10, 15), b = c(15, 25))
#' dfr2 <- splice_time(l2)
#' is_splice_overlapping(dfr1, dfr2)
is_splice_overlapping <- function(...) {

  splice_dfr <- dplyr::bind_rows(...)
  splice_dfr <- dplyr::arrange(splice_dfr, .data$Start)
  overlap <- splice_dfr[['End']] > dplyr::lead(splice_dfr[['Start']], 1)

  any(overlap, na.rm = TRUE)
}


#' Clip a splice so segments are of fixed duration
#'
#' @param splice_dfr `Splice` object.
#' @param duration window duration in seconds.
#' @param location 'beginning', 'middle' or 'end'.
#'
#' @return a `Splice` object.
#' @export
#' @family splicing functions
#'
#' @examples
#' l <- list(a = c(10, 20), b = c(30, 40),c = c(50, 55))
#' splice_dfr <- splice_time(l)
#' clip_splice(splice_dfr, duration = 1)
#' clip_splice(splice_dfr, duration = 6)
#' clip_splice(splice_dfr, duration = 1, location = 'beginning')
#' clip_splice(splice_dfr, duration = 10, location = 'beginning')
#' clip_splice(splice_dfr, duration = 1, location = 'end')
#' clip_splice(splice_dfr, duration = 10, location = 'end')
clip_splice <- function(splice_dfr, duration, location = 'middle') {
  stopifnot(is.data.frame(splice_dfr), duration > 0, location %in% c('beginning', 'middle', 'end'))
  new_splice_dfr <- splice_dfr

  if (location == 'middle') {
    mid_time <- (splice_dfr$Start + splice_dfr$End) / 2
    start_time <- mid_time - duration / 2
    end_time <- mid_time + duration / 2
    is_before_start <- start_time < splice_dfr$Start
    is_after_end <- end_time > splice_dfr$End
    if (any(is_before_start | is_after_end)) {
      warning('Segments too short to clip to duration - using start or end point of segment')
    }
    new_splice_dfr$Start <- ifelse(is_before_start, splice_dfr$Start, start_time)
    new_splice_dfr$End <- ifelse(is_after_end, splice_dfr$End, end_time)

  } else if (location == 'beginning') {
    end_time <- splice_dfr$Start + duration
    is_after_end <- end_time > splice_dfr$End
    if (any(is_after_end)) {
      warning('Segments too short to clip to duration - using end point of segment')
    }
    new_splice_dfr$Start <- splice_dfr$Start
    new_splice_dfr$End <- ifelse(is_after_end, splice_dfr$End, end_time)

  } else if (location == 'end') {
    start_time <- splice_dfr$End - duration
    is_before_start <- start_time < splice_dfr$Start
    if (any(is_before_start)) {
      warning('Segments too short to clip to duration - using start point of segment')
    }
    new_splice_dfr$Start <- ifelse(is_before_start, splice_dfr$Start, start_time)
    new_splice_dfr$End <- splice_dfr$End
  } else stop()

  class(new_splice_dfr) <- c('Splice', 'data.frame')

  new_splice_dfr
}


#' Merge splices together using set operations
#'
#' @param ... a collection of named `Splice` objects.
#' @param operation either 'union' or 'intersection'.
#'
#' @return a `Splice` object.
#' @export
#' @family splicing functions
#'
#' @examples
#' l1 <- list(a1 = c(100, 200), a2 = c(250, 300), a3 = c(400, 550), a4 = c(600, 650))
#' split1_dfr <- splice_time(l1)
#' split1_dfr
#'
#' l2 <- list(b1 = c(150, 275), b2 = c(610, 640))
#' split2_dfr <- splice_time(l2)
#' split2_dfr
#'
#' l3 <- list(c1 = c(275, 325), c2 = c(600, 675), c3 = c(700, 725))
#' split3_dfr <- splice_time(l3)
#' split3_dfr
#'
#' merge_splice(x = split1_dfr, y = split2_dfr, z = split3_dfr, operation = 'union')
#' merge_splice(x = split1_dfr, y = split2_dfr, z = split3_dfr, operation = 'intersection')
merge_splice <- function(..., operation) {
  l <- list(...)

  # Checks
  stopifnot(operation %in% c('union', 'intersection'))
  stopifnot(all(sapply(l, function(x) class(x)[1] == 'Splice')))
  stopifnot(all(sapply(l, Negate(is_splice_overlapping))))
  if (length(l) == 1) return(l[[1]])

  overlap <- switch(operation, 'union' = 1, 'intersection' = length(l))
  segment_name <- switch(operation, 'union' = paste(names(l), collapse = " | "),
                         'intersection' = paste(names(l), collapse = " & "))

  dfr <- dplyr::bind_rows(l, .id = 'Splice')
  dd <- rbind(data.frame(pos = dfr$Start, event = 1),
              data.frame(pos = dfr$End, event = -1))

  dd <- stats::aggregate(event ~ pos, dd, sum)
  dd <- dd[order(dd$pos), , drop=FALSE]
  dd$open <- cumsum(dd$event)
  r <- rle(dd$open >= overlap)
  ex <- cumsum(r$lengths - 1 + rep(1, length(r$lengths)))
  sx <- ex - r$lengths + 1

  output_dfr <- cbind.data.frame(
    Segment = make.unique(rep(segment_name, length(sx[r$values]))),
    Start = dd$pos[sx[r$values]],
    End = dd$pos[ex[r$values] + 1]
  )

  class(output_dfr) <- c('Splice', 'data.frame')

  output_dfr

}


