# Diagnostic plots of S3 objects


#' Plot a Duration S3 object
#'
#' @param x S3 object
#' @param ... passed to [barplot()]
#' @return a plot object with durations.
#'
#' @examples
#' r <- get_sample_recording()
#' d <- get_duration_annotation_data(r)
#' plot(d)
#' @exportS3Method
plot.Duration <- function(x, ...) {
  graphics::barplot(Duration ~ In + Tier, main = "Duration Object", data = x, ...)
}

#' Plot a OnsetsSelected S3 object
#'
#' @param x S3 object.
#' @param ... passed to [barplot()].
#' @param instrument column name.
#' @param tactus beat column name (defaults to "Matra").
#'
#' @return Return an 'OnsetsSelected' object.
#' @examples
#' r <- get_sample_recording()
#' o <- get_onsets_selected_data(r)
#' plot(o)
#' @exportS3Method
plot.OnsetsSelected <- function(x, instrument = 'Inst', tactus = 'Matra', ...) {

  dfr_list <- x[sapply(x, class) == 'data.frame']
  df <- dplyr::bind_rows(dfr_list, .id = 'Metre')
  stopifnot(instrument %in% colnames(df), tactus %in% colnames(df))

  df <- dplyr::rename(df, 'Tactus' = tactus)
  df['is_na_column'] <- !is.na(df[instrument])

  group_df <- dplyr::group_by(df, .data$Tactus, .data$Metre)
  output_df <- dplyr::summarise(group_df, Number_of_Onsets = sum(.data$is_na_column))

  graphics::barplot(Number_of_Onsets ~ Metre + Tactus, beside = T, legend.text = T, data = output_df,
          main = paste("OnsetsSelected Object:", instrument))
}


#' Plot a Metre S3 object
#'
#' @param x S3 object.
#' @param ... ignored.
#' @return a plot object with metre.
#'
#' @examples
#' r <- get_sample_recording()
#' m <- get_metre_data(r)
#' plot(m)
#' @exportS3Method
plot.Metre <- function(x, ...) {
  zoo_list <- lapply(x, function(x) zoo::zoo(c(diff(x$Time), NA), order.by = x$Time))
  z <- do.call(merge, zoo_list)
  ylab <- if (is.null(ncol(z))) "" else NULL
  plot(z, yax.flip = TRUE, xlab = "Time / s", ylab = ylab, main = "Metre Object - Cycle Length", ...)
}


#' Plot a View S3 object
#'
#' @param x S3 object
#' @param columns names of columns
#' @param maxpts maximum number of points to plot.
#' @param ... passed to [plot.zoo()]
#' @return a plot object.
#'
#' @examples
#' r <- get_sample_recording()
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' plot(v, columns = "LEar_x")
#' @exportS3Method
plot.View <- function(x, columns=NULL, maxpts = 1000, ...) {

  max_num_cols <- 9

  # Restrict points and columns to plot
  columns <- if (is.null(columns)) {
    if (ncol(x$df) > max_num_cols + 2)
      warning(paste("Only plotting first", max_num_cols, "data columns"))
    seq_len(min(ncol(x$df), max_num_cols + 2))[-1]
  } else c("Time", columns)
  sp <- if (nrow(x$df) > maxpts) sample(nrow(x$df), maxpts) else seq_len(nrow(x$df))

  df <- x$df[sp, columns, drop = FALSE]
  df <- df[, colSums(is.na(df)) < (nrow(df) - 1), drop=FALSE] # more than one point
  zoo_list <- lapply(df[-1], function(x) zoo::zoo(x, order.by = df$Time))
  z <- do.call(merge, zoo_list)

  title <- c(x$recording$stem, x$vid, x$direct, x$inst)
  title <- paste(title[title != ""], collapse="_")
  ylab <- if (is.null(ncol(z))) columns[-1] else NULL
  plot(z, xlab = "Time / s", ylab = ylab,
       main = paste(class(x)[1], "Object for", title), ...)

}
