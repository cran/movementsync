# Functions to summarise data objects


#' Summarise Recording object
#'
#' @param object `Recording` object.
#' @param ... ignored.
#'
#' @return list
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' summary(r)
summary.Recording <- function(object, ...) {
  object
}


#' Summarise Duration object
#'
#' @param object `Duration` object.
#' @param ... ignored.
#'
#' @return data.frame
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' d <- get_duration_annotation_data(r)
#' head(summary(d))
summary.Duration <- function(object, ...) {
  object
}


#' Summarise OnsetsSelected object
#'
#' @param object `OnsetsSelected` object.
#' @param ... ignored.
#'
#' @return list of summaries.
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' o <- get_onsets_selected_data(r)
#' summary(o)
summary.OnsetsSelected <- function(object, ...) {
  lapply(object, function(x) if (is.data.frame(x)) summary(x))
}


#' Summarise Metre object
#'
#' Summarises the cycle length for each Metre.
#'
#' @param object `Metre` object.
#' @param ... ignored.
#'
#' @return list of summaries.
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' m <- get_metre_data(r)
#' summary(m)
summary.Metre <- function(object, ...) {
  df_list <- lapply(object, function(x) if (is.data.frame(x)) diff(x$Time))
  lapply(df_list, summary)
}


#' Summarise a View object
#'
#' @param object `View` object.
#' @param ... ignored.
#'
#' @return summary of data.frame.
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' fv <- apply_filter_sgolay(pv, c("Nose", "RWrist", "LWrist"), n=19, p=4)
#' summary(rv)
#' summary(pv)
#' summary(fv)
summary.View <- function(object, ...) {
  summary(object$df)
}


#' Summarise an analyze.wavelet object
#'
#' @param object `analyze.wavelet` object.
#' @param v `View` object
#' @param ... ignored.
#'
#' @return data.frame
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' w <- analyze_wavelet(pv, "Nose_x")
#' summary(w, pv)
summary.analyze.wavelet <- function(object, v, ...) {
  d <- data.frame(Period = object$Period / v$recording$fps,
             Average_Power = object$Power.avg)
  summary(d)
}


#' Summarises a sel.phases object
#'
#' @param na.rm remove missings?
#' @param object `sel.phases` object.
#' @param ... ignored.
#'
#' @return list of `Circular` statistics.
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, columns = c("Nose_x", "Nose_y"))
#' sp <- plot_sel_phases(co, pv, sel.period = NULL, sel.lower = 0.5, sel.upper = 0.7)
#' summary(sp)
summary.sel.phases <- function(object, na.rm = TRUE, ...) {

  angle_circular <- circular::circular(object$Angle)

  list(Mean_Phase_Angle = circular::mean.circular(angle_circular, na.rm = na.rm),
       SD_Phase_Angle = circular::sd.circular(angle_circular, na.rm = na.rm),
       Mean_Phase_Angle_Degrees = 180 / pi * circular::mean.circular(angle_circular, na.rm = na.rm),
       SD_Phase_Angle_Degrees = 180 / pi * circular::sd.circular(angle_circular, na.rm = na.rm),
       Mean_Resultant_Length = circular::rho.circular(angle_circular, na.rm = na.rm))
}
