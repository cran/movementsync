# Statistical and analysis functions


#' Apply summary function to the columns in each segment of a SpliceView object
#'
#' Apply summary function to each data point column in a SplicedView and return list of output data.
#' @param sv `SplicedView` object.
#' @param FUN function to apply.
#' @param simplify see [sapply()].
#' @param USE.NAMES see [sapply()].
#' @param ... passed to FUN.
#'
#' @return see [sapply()].
#' @export
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' d1 <- get_duration_annotation_data(r)
#' # only one relevant section for sample data
#' splicing_smile_df <- splice_time(d1, tier ='INTERACTION',
#'   comments = 'Mutual look and smile')
#'
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' sv_duration_smile <- get_spliced_view(jv, splicing_df = splicing_smile_df)
#' mean_mat <- apply_column_spliceview(sv_duration_smile, mean, na.rm=TRUE)
apply_column_spliceview <- function(sv, FUN, simplify = FALSE, USE.NAMES = FALSE, ...) {
  v_list <- split(sv)
  sapply(v_list, function(x) {
    keys <- match(c('Segment', 'Frame', 'Time'), colnames(x$df), nomatch = 0)
    dfr <- x$df[-keys]
    apply(dfr, 2, function(y) FUN(y, ...))
  }, simplify = simplify, USE.NAMES = USE.NAMES)
}

#' @export
#' @rdname apply_column_spliceview
sapply_column_spliceview <- function(sv, FUN, simplify = TRUE, USE.NAMES = TRUE, ...) {
  apply_column_spliceview(sv, FUN, simplify = simplify, USE.NAMES = USE.NAMES, ...)
}


#' Apply complex function to each segment in a SpliceView object
#'
#' @param sv `SplicedView` object.
#' @param FUN function to apply.
#' @param ... passed to FUN.
#'
#' @return list of two elements: 'output' containing results of apply FUN to 'input'
#' @export
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' d1 <- get_duration_annotation_data(r)
#' # only one relevant section for sample data
#' splicing_smile_df <- splice_time(d1, tier ='INTERACTION',
#'   comments = 'Mutual look and smile')
#'
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' sv_duration_smile <- get_spliced_view(jv, splicing_df = splicing_smile_df)
#' wavelet_smile_list <- apply_segment_spliceview(sv_duration_smile, analyze_wavelet,
#'   column = "Nose_x_Central_Sitar")
#' names(wavelet_smile_list)
apply_segment_spliceview <- function(sv, FUN, ...) {
  view_list <- split(sv)
  output_list <- lapply(view_list, FUN = FUN, ...)
  list(output = output_list, input = view_list)
}


#' Compare average power distribution using a splicing table
#'
#' @param jv `JoinedView` object.
#' @param splicing_df `Splice` object.
#' @param splice_name Name to give randomly spliced segments.
#' @param num_segment_samples number of segments to randomly sample.
#' @param num_splice_samples number of randomly chosen splices.
#' @param rejection_list list of splice objects that random splices must not overlap.
#' @param show_plot show the plot? (Default  is TRUE).
#' @param sampling_type either 'offset' or 'gap'.
#' @param column name of data column on which to calculate average power.
#'
#' @export
#' @return list of two data frames: one containing average power on the first
#' splice and the other containing the average power on randomly generated splices.
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' fv_list <- get_filtered_views(r, data_points = 'Nose', n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' splicing_df <- splice_time(list(a = c(0, 5), b = c(10, 15)))
#' output_list <- compare_ave_power1(jv, splicing_df, 'Random Splices', 5, 5, 'Nose_x_Central_Tabla')
compare_ave_power1 <- function(jv, splicing_df, splice_name, num_segment_samples,
                              num_splice_samples,
                              column, sampling_type = 'offset', rejection_list = list(),
                              show_plot = TRUE) {
  stopifnot(class(jv)[1] == "JoinedView",
            class(splicing_df)[1] == "Splice",
            sampling_type %in% c('offset', 'gap'),
            is.list(rejection_list))

  if (sampling_type == 'offset') {
    splicing_list <- sample_offset_splice(splicing_df, jv, num_splices = num_splice_samples,
                                          rejection_list = rejection_list)
  } else if (sampling_type == 'gap') {
    splicing_list <- sample_gap_splice(splicing_df, jv, num_splices = num_splice_samples,
                                       rejection_list = rejection_list)
  }

  # Original splice
  sv_orig <- get_spliced_view(jv, splicing_df = splicing_df)
  av_power_orig <- ave_power_spliceview(sv_orig, column = column)
  segment_samples <- sample(2:ncol(av_power_orig), num_segment_samples, replace = TRUE)
  period_samples <- sample(nrow(av_power_orig), num_segment_samples, replace = TRUE)
  original_dfr <- cbind.data.frame(
    Period = av_power_orig[period_samples, 'Period'],
    Average_Power = av_power_orig[cbind(period_samples, segment_samples)]
  )

  # Random splices
  sv_list <- lapply(splicing_list, function(x) get_spliced_view(jv, splicing_df = x))
  df_list <- lapply(sv_list, ave_power_spliceview, column = column)
  ave_power_df <- dplyr::bind_rows(df_list, .id = 'Sample')
  ave_power_data <- ave_power_df[-c(1:2)]
  segment_samples <- sample(ncol(ave_power_df) - 2, num_segment_samples, replace = TRUE)
  period_samples <- sample(nrow(ave_power_df), num_segment_samples, replace = TRUE)
  random_splice_dfr <- cbind.data.frame(
    Period = ave_power_df[period_samples, 'Period'],
    Average_Power = ave_power_data[cbind(period_samples, segment_samples)]
  )

  l <- list(original_dfr, random_splice_dfr)
  names(l) <- c(splice_name, 'Sampled Splices')

  if (show_plot) {
    long_dfr <- dplyr::bind_rows(l, .id = 'Sampled_From')

    subtitle <- c(jv$recording$stem, jv$vid, jv$direct, jv$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    g <- ggplot2::ggplot(long_dfr, ggplot2::aes(x = .data$Period, colour = .data$Sampled_From)) +
      ggplot2::xlab("Period / sec") +
      ggplot2::labs(title = "Comparison of Average Power on Sampled Segments",
                    subtitle = paste(subtitle, ':', column)) +
      ggplot2::scale_x_continuous(trans='log2') +
      ggplot2::geom_line(ggplot2::aes(y = .data$Average_Power)) +
      ggplot2::facet_wrap(~Sampled_From)
    print(g)
  }

  l
}


#' Calculate average power distribution using a splicing table
#'
#' @param jv `JoinedView` object.
#' @param splicing_df `Splice` object.
#' @param splice_name Name to give randomly spliced segments.
#' @param num_segment_samples number of segments to randomly sample.
#' @param show_plot show the plot? (Default is TRUE).
#' @param column name of data column on which to calculate average power.
#'
#' @export
#' @return a data frame: containing average power on the spliced JoinedView.
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' fv_list <- get_filtered_views(r, data_points = 'Nose', n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' splicing_df <- splice_time(list(a = c(0, 5), b = c(10, 15)))
#' output_dfr <- calculate_ave_power1(jv, splicing_df, 'Splice', 10, 'Nose_x_Central_Tabla')
calculate_ave_power1 <- function(jv, splicing_df, splice_name, num_segment_samples,
                                 column, show_plot = TRUE) {
  stopifnot(class(jv)[1] == "JoinedView",
            class(splicing_df)[1] == "Splice")

  sv_orig <- get_spliced_view(jv, splicing_df = splicing_df)
  av_power_orig <- ave_power_spliceview(sv_orig, column = column)
  segment_samples <- sample(2:ncol(av_power_orig), num_segment_samples, replace = TRUE)
  period_samples <- sample(nrow(av_power_orig), num_segment_samples, replace = TRUE)
  original_dfr <- cbind.data.frame(
    Period = av_power_orig[period_samples, 'Period'],
    Average_Power = av_power_orig[cbind(period_samples, segment_samples)]
  )

  if (show_plot) {

    subtitle <- c(jv$recording$stem, jv$vid, jv$direct, jv$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    g <- ggplot2::ggplot(original_dfr, ggplot2::aes(x = .data$Period)) +
      ggplot2::xlab("Period / sec") +
      ggplot2::labs(title = "Average Power on Sampled Segments",
                    subtitle = paste(subtitle, ':', column)) +
      ggplot2::scale_x_continuous(trans='log2') +
      ggplot2::geom_line(ggplot2::aes(y = .data$Average_Power))
    print(g)
  }

  original_dfr
}


#' Compare average cross power distribution using a splicing table
#'
#' @param jv `JoinedView` object.
#' @param splicing_df `Splice` object.
#' @param splice_name Name to give randomly spliced segments.
#' @param num_segment_samples number of segments to randomly sample.
#' @param num_splice_samples number of randomly chosen splices.
#' @param rejection_list list of splice objects that random splices must not overlap.
#' @param show_plot show the plot? (Default  is TRUE).
#' @param sampling_type either 'offset' or 'gap'.
#' @param columns name of data columns on which to calculate cross average power.
#'
#' @export
#' @return list of two data frames: one containing average cross power on the first
#' splice and the other containing the average cross power on randomly generated splices.
#' @family statistical and analysis functions
#'
#' @examples
#' \donttest{
#' r <- get_sample_recording()
#' fv_list <- get_filtered_views(r, data_points = 'Nose', n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' splicing_df <- splice_time(list(a = c(0, 5), b = c(10, 15)))
#' output_list <- compare_ave_cross_power1(jv, splicing_df, 'Random Splices', 5, 5,
#' c('Nose_x_Central_Tabla', 'Nose_y_Central_Tabla'))
#' }
compare_ave_cross_power1 <- function(jv, splicing_df, splice_name, num_segment_samples,
                               num_splice_samples, columns, sampling_type = 'offset',
                               rejection_list = list(), show_plot = TRUE) {
  stopifnot(class(jv)[1] == "JoinedView",
            class(splicing_df)[1] == "Splice",
            sampling_type %in% c('offset', 'gap'),
            is.list(rejection_list))

  if (sampling_type == 'offset') {
    splicing_list <- sample_offset_splice(splicing_df, jv, num_splices = num_splice_samples,
                                          rejection_list = rejection_list)
  } else if (sampling_type == 'gap') {
    splicing_list <- sample_gap_splice(splicing_df, jv, num_splices = num_splice_samples,
                                       rejection_list = rejection_list)
  }

  # Original splice
  sv_orig <- get_spliced_view(jv, splicing_df = splicing_df)
  av_cross_power_orig <- ave_cross_power_spliceview(sv_orig, columns = columns)
  segment_samples <- sample(2:ncol(av_cross_power_orig), num_segment_samples, replace = TRUE)
  period_samples <- sample(nrow(av_cross_power_orig), num_segment_samples, replace = TRUE)
  original_dfr <- cbind.data.frame(
    Period = av_cross_power_orig[period_samples, 'Period'],
    Average_Cross_Power = av_cross_power_orig[cbind(period_samples, segment_samples)]
  )

  # Random splices
  sv_list <- lapply(splicing_list, function(x) get_spliced_view(jv, splicing_df = x))
  df_list <- lapply(sv_list, ave_cross_power_spliceview, columns = columns)
  ave_cross_power_df <- dplyr::bind_rows(df_list, .id = 'Sample')
  ave_cross_power_data <- ave_cross_power_df[-c(1:2)]
  segment_samples <- sample(ncol(ave_cross_power_df) - 2, num_segment_samples, replace = TRUE)
  period_samples <- sample(nrow(ave_cross_power_df), num_segment_samples, replace = TRUE)
  random_splice_dfr <- cbind.data.frame(
    Period = ave_cross_power_df[period_samples, 'Period'],
    Average_Cross_Power = ave_cross_power_data[cbind(period_samples, segment_samples)]
  )

  l <- list(original_dfr, random_splice_dfr)
  names(l) <- c(splice_name, 'Sampled Splices')

  if (show_plot) {
    long_dfr <- dplyr::bind_rows(l, .id = 'Sampled_From')

    subtitle <- c(jv$recording$stem, jv$vid, jv$direct, jv$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    g <- ggplot2::ggplot(long_dfr, ggplot2::aes(x = .data$Period, colour = .data$Sampled_From)) +
      ggplot2::xlab("Period / sec") +
      ggplot2::labs(title = "Comparison of Average Cross Power on Sampled Segments",
                    subtitle = paste(subtitle, ':', paste0(columns, collapse = ", "))) +
      ggplot2::scale_x_continuous(trans='log2') +
      ggplot2::geom_line(ggplot2::aes(y = .data$Average_Cross_Power)) +
      ggplot2::facet_wrap(~Sampled_From)
    print(g)
  }

  l
}


#' Calculate average cross power distribution using a splicing table
#'
#' @param jv `JoinedView` object.
#' @param splicing_df `Splice` object.
#' @param splice_name Name to give randomly spliced segments.
#' @param num_segment_samples number of segments to randomly sample.
#' @param show_plot show the plot? (Default is TRUE).
#' @param columns name of data columns on which to calculate average cross power.
#'
#' @export
#' @return a data frame: containing average cross power on the spliced JoinedView.
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' fv_list <- get_filtered_views(r, data_points = 'Nose', n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' splicing_df <- splice_time(list(a = c(0, 5), b = c(10, 15)))
#' output_dfr <- calculate_ave_cross_power1(jv, splicing_df, 'Splice', 10,
#'   c('Nose_x_Central_Tabla', 'Nose_y_Central_Tabla'))
calculate_ave_cross_power1 <- function(jv, splicing_df, splice_name, num_segment_samples,
                                 columns, show_plot = TRUE) {
  stopifnot(class(jv)[1] == "JoinedView",
            class(splicing_df)[1] == "Splice")

  sv_orig <- get_spliced_view(jv, splicing_df = splicing_df)
  av_cross_power_orig <- ave_cross_power_spliceview(sv_orig, columns = columns)
  segment_samples <- sample(2:ncol(av_cross_power_orig), num_segment_samples, replace = TRUE)
  period_samples <- sample(nrow(av_cross_power_orig), num_segment_samples, replace = TRUE)
  original_dfr <- cbind.data.frame(
    Period = av_cross_power_orig[period_samples, 'Period'],
    Average_Cross_Power = av_cross_power_orig[cbind(period_samples, segment_samples)]
  )

  if (show_plot) {

    subtitle <- c(jv$recording$stem, jv$vid, jv$direct, jv$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    g <- ggplot2::ggplot(original_dfr, ggplot2::aes(x = .data$Period)) +
      ggplot2::xlab("Period / sec") +
      ggplot2::labs(title = "Average Cross Power on Sampled Segments",
                    subtitle = paste(subtitle, ':', paste0(columns, collapse = ", "))) +
      ggplot2::scale_x_continuous(trans='log2') +
      ggplot2::geom_line(ggplot2::aes(y = .data$Average_Cross_Power))
    print(g)
  }

  original_dfr
}


#' Calculate mean average power over splices using a splicing table
#'
#' Randomly generates splices from a splicing table and calculates average
#' power for each segment and splice. Calculates the mean average power
#' over the random splices for each segment and period. Compares with the
#' average power for the original splice.
#'
#' @param jv `JoinedView` object.
#' @param splicing_df `Splice` object.
#' @param column name of data column on which to calculate average power.
#' @param sampling_type either 'offset' or 'gap'.
#' @param include_original include the original splice in output? (Default is TRUE).
#' @param rejection_list list of splice objects that random splices must not overlap.
#' @param show_plot show a plot? (Default is TRUE).
#' @param num_splices number of randomly chosen splices.
#'
#' @return data.frame of splice segments and their average power.
#' @export
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#'
#' d <- get_duration_annotation_data(r)
#' splicing_tabla_solo_df <- splice_time(d,
#'   expr = "Tier == 'INTERACTION' & Comments == 'Mutual look and smile'")
#'
#' # Only do the first splice for sample data
#' mean_ave_power_df <- ave_power_over_splices(jv, splicing_tabla_solo_df[1,], num_splices = 10,
#' column = 'Nose_x_Central_Sitar', show_plot = TRUE)
ave_power_over_splices <- function(jv, splicing_df, num_splices, column, sampling_type = 'offset',
                                   rejection_list = list(), include_original = TRUE,
                                   show_plot = TRUE) {

  stopifnot(class(jv)[1] == "JoinedView",
            sampling_type %in% c('offset', 'gap'),
            is.list(rejection_list))

  if (sampling_type == 'offset') {
    splicing_list <- sample_offset_splice(splicing_df, jv, num_splices = num_splices,
                                          rejection_list = rejection_list)
  } else if (sampling_type == 'gap') {
    splicing_list <- sample_gap_splice(splicing_df, jv, num_splices = num_splices,
                                          rejection_list = rejection_list)
  } else stop()
  if (include_original) splicing_list$Original <- splicing_df

  sv_list <- lapply(splicing_list, function(x) get_spliced_view(jv, splicing_df = x))
  df_list <- lapply(sv_list, ave_power_spliceview, column = column)
  ave_power_df <- dplyr::bind_rows(df_list, .id = 'Sample')

  sample_ave_power <- ave_power_df
  sample_ave_power <- dplyr::filter(sample_ave_power, .data$Sample != 'Original')
  sample_ave_power <- dplyr::group_by(sample_ave_power, .data$Period)
  sample_ave_power <- dplyr::summarise(sample_ave_power, dplyr::across(!.data$Sample, mean, na.rm = TRUE))

  original_ave_power <- ave_power_df
  original_ave_power <- dplyr::filter(original_ave_power, .data$Sample == 'Original')

  ave_power_df <- dplyr::bind_rows(
    'Random Splices' = sample_ave_power,
    'Original Splice' = original_ave_power,
  .id = 'Sample')

  long_ave_power_df <- tidyr::pivot_longer(ave_power_df, cols = !c(.data$Sample, .data$Period),
                                           names_to = 'Segment', values_to = 'Average_Power')

  if (show_plot) {

    subtitle <- paste(jv$recording$stem, column, sep = ' - ')

    g <- ggplot2::ggplot(long_ave_power_df) +
      ggplot2::geom_line(ggplot2::aes(x = .data$Period, y = .data$Average_Power, colour = .data$Sample)) +
      ggplot2::labs(title = "Mean Average Power Over Random Splices", subtitle = subtitle) +
      ggplot2::xlab("Period / sec") +
      ggplot2::ylab("Mean Average Power") +
      ggplot2::scale_x_continuous(trans='log2') +
      ggplot2::facet_wrap(~.data$Segment)
    print(g)

  }

  as.data.frame(long_ave_power_df)
}


#' Calculate mean average cross power over splices using a splicing table
#'
#' Randomly generates splices from a splicing table and calculates average
#' cross power for each segment and splice. Calculates the mean average cross power
#' over the random splices for each segment and period. Compares with the
#' average cross power for the original splice.
#'
#' @param jv `JoinedView` object.
#' @param splicing_df `Splice` object.
#' @param columns name of data columns on which to calculate average cross power.
#' @param sampling_type either 'offset' or 'gap'.
#' @param include_original include the original splice in output? (Default is TRUE).
#' @param rejection_list list of splice objects that random splices must not overlap.
#' @param show_plot show a plot? (Default is TRUE).
#' @param num_splices number of randomly chosen splices.
#'
#' @return data.frame of splice segments and their average cross power.
#' @export
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#'
#' d <- get_duration_annotation_data(r)
#' splicing_tabla_solo_df <- splice_time(d,
#'   expr = "Tier == 'INTERACTION' & Comments == 'Mutual look and smile'")
#'
#' # Only do the first splice for sample data
#' mean_ave_cross_power_df <- ave_cross_power_over_splices(jv,
#'   splicing_tabla_solo_df[1,], num_splices = 10,
#'   columns = c('Nose_x_Central_Sitar', 'Nose_y_Central_Sitar'), show_plot = TRUE)
ave_cross_power_over_splices <- function(jv, splicing_df, num_splices, columns, sampling_type = 'offset',
                                   rejection_list = list(), include_original = TRUE,
                                   show_plot = TRUE) {

  stopifnot(class(jv)[1] == "JoinedView",
            sampling_type %in% c('offset', 'gap'),
            is.list(rejection_list))

  if (sampling_type == 'offset') {
    splicing_list <- sample_offset_splice(splicing_df, jv, num_splices = num_splices,
                                          rejection_list = rejection_list)
  } else if (sampling_type == 'gap') {
    splicing_list <- sample_gap_splice(splicing_df, jv, num_splices = num_splices,
                                       rejection_list = rejection_list)
  } else stop()
  if (include_original) splicing_list$Original <- splicing_df

  sv_list <- lapply(splicing_list, function(x) get_spliced_view(jv, splicing_df = x))
  df_list <- lapply(sv_list, ave_cross_power_spliceview, columns = columns)
  ave_cross_power_df <- dplyr::bind_rows(df_list, .id = 'Sample')

  sample_ave_cross_power <- ave_cross_power_df
  sample_ave_cross_power <- dplyr::filter(sample_ave_cross_power, .data$Sample != 'Original')
  sample_ave_cross_power <- dplyr::group_by(sample_ave_cross_power, .data$Period)
  sample_ave_cross_power <- dplyr::summarise(sample_ave_cross_power, dplyr::across(!.data$Sample, mean, na.rm = TRUE))

  original_ave_cross_power <- ave_cross_power_df
  original_ave_cross_power <- dplyr::filter(original_ave_cross_power, .data$Sample == 'Original')

  ave_cross_power_df <- dplyr::bind_rows(
    'Random Splices' = sample_ave_cross_power,
    'Original Splice' = original_ave_cross_power,
    .id = 'Sample')

  long_ave_cross_power_df <- tidyr::pivot_longer(ave_cross_power_df, cols = !c(.data$Sample, .data$Period),
                                           names_to = 'Segment', values_to = 'Average_Cross_Power')

  if (show_plot) {

    subtitle <- paste(jv$recording$stem, paste0(columns, collapse = ", "), sep = ' - ')

    g <- ggplot2::ggplot(long_ave_cross_power_df) +
      ggplot2::geom_line(ggplot2::aes(x = .data$Period, y = .data$Average_Cross_Power, colour = .data$Sample)) +
      ggplot2::labs(title = "Mean Average Cross Power Over Random Splices", subtitle = subtitle) +
      ggplot2::xlab("Period / sec") +
      ggplot2::ylab("Mean Average Cross Power") +
      ggplot2::scale_x_continuous(trans='log2') +
      ggplot2::facet_wrap(~.data$Segment)
    print(g)

  }

  as.data.frame(long_ave_cross_power_df)
}


#' Apply function to SplicedView and pull out element from output
#'
#' @param sv `SplicedView` object.
#' @param FUN function to apply.
#' @param element name of element to pull out from output object.
#' @param ... passed to function.
#'
#' @export
#' @return list with output and input fields.
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' d1 <- get_duration_annotation_data(r)
#' # only one relevant section for sample data
#' splicing_smile_df <- splice_time(d1, tier ='INTERACTION',
#'   comments = 'Mutual look and smile')
#'
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' sv_duration_smile <- get_spliced_view(jv, splicing_df = splicing_smile_df)
#' pull_segment_spliceview(sv_duration_smile, FUN = analyze_wavelet,
#'                         column = "Nose_x_Central_Sitar", element = 'Power')
pull_segment_spliceview <- function(sv, FUN, element, ...) {
  view_list <- split(sv)
  output_list <- lapply(view_list, FUN = FUN, ...)
  invisible(list(output = lapply(output_list, function(x) x[[element]]), input = view_list))
}

#' Get the average power on each segment in a SplicedView
#'
#' @param sv `SplicedView` object
#' @param column name of data column on which to calculate average power.
#' @param colour name of colour on plots (default is 'blue').
#' @param segments indices of segments to plot e.g. 1:10 (default plots up to first 10).
#' @param show_plot show a plot? (Default is FALSE).
#' @param ... passed to [analyze_wavelet()].
#'
#' @export
#' @return data.frame with columns containing Average Power for each segment.
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' d1 <- get_duration_annotation_data(r)
#' # only one relevant section for sample data
#' splicing_smile_df <- splice_time(d1, tier ='INTERACTION',
#'   comments = 'Mutual look and smile')
#'
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' sv_duration_smile <- get_spliced_view(jv, splicing_df = splicing_smile_df)
#' ave_power_smile <- ave_power_spliceview(sv_duration_smile,
#'   column = "Nose_x_Central_Sitar", show_plot=TRUE)
#' head(ave_power_smile)
ave_power_spliceview <- function(sv, column, colour = 'blue', segments = NULL,
                                 show_plot = FALSE, ...) {
  stopifnot("SplicedView" %in% class(sv))

  wavelet_list <- apply_segment_spliceview(sv, FUN = analyze_wavelet, column = column, ...)
  output_mat <- sapply(wavelet_list$output, function(x) x$Power.avg)

  obj <- wavelet_list$output[[1]]
  period.tick.value <- 2^(obj$axis.2) / sv$recording$fps

  output_dfr <- cbind.data.frame(Period = period.tick.value, output_mat)

  if (show_plot) {

    subtitle <- c(sv$recording$stem, sv$vid, sv$direct, sv$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    z <- zoo::zoo(output_dfr[-1], order.by = output_dfr[[1]])

    if (is.null(segments) && ncol(z) > 10) {
      warning("Too many segments - only showing first 10")
      z <- z[, 1:10]
    } else if (!is.null(segments)) {
      if (length(setdiff(segments, seq_len(ncol(z)))) > 0)
        warning("Some segments not in SplicedView")
      z <- z[, intersect(seq_len(ncol(z)), segments)]
    }

    g <- autoplot(z, facets = ~ Series, col = I(colour)) +
      ggplot2::labs(title = "Average Power on Segments",
                    subtitle = paste(subtitle, ':', column)) +
      ggplot2::coord_trans(x = "log2") +
      ggplot2::xlab("Period / sec") +
      ggplot2::ylab("Average Power")

    print(g)
  }

  output_dfr
}


#' Get the average cross power on each segment in a SplicedView
#'
#' @param sv `SplicedView` object
#' @param columns column names in the data of each `SplicedView` object.
#' @param colour name of colour on plots (default is 'blue').
#' @param show_plot show a plot (default is FALSE).
#' @param segments indices of segments to plot e.g. 1:10 (default plots up to first 10).
#' @param ... passed to [analyze_coherency()].
#'
#' @export
#' @return data.frame with columns containing Average Cross Power for each segment.
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' d1 <- get_duration_annotation_data(r)
#' # only one relevant section for sample data
#' splicing_smile_df <- splice_time(d1, tier ='INTERACTION',
#'   comments = 'Mutual look and smile')
#'
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' sv_duration_smile <- get_spliced_view(jv, splicing_df = splicing_smile_df)
#' ave_cross_power_smile <- ave_cross_power_spliceview(
#'   sv_duration_smile, columns = c("Nose_x_Central_Sitar", "Nose_y_Central_Sitar"), show_plot = TRUE)
#' head(ave_cross_power_smile)
ave_cross_power_spliceview <- function(sv, columns, colour = 'blue', segments = NULL,
                                       show_plot = FALSE, ...) {

  coherency_list <- apply_segment_spliceview(sv, FUN = analyze_coherency, columns = columns, ...)
  output_mat <- sapply(coherency_list$output, function(x) x$Power.xy.avg)

  obj <- coherency_list$output[[1]]
  period.tick.value <- 2^(obj$axis.2) / sv$recording$fps

  output_dfr <- cbind.data.frame(Period = period.tick.value, output_mat)

  if (show_plot) {

    subtitle <- c(sv$recording$stem, sv$vid, sv$direct, sv$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    z <- zoo::zoo(output_dfr[-1], order.by = output_dfr[[1]])

    if (is.null(segments) && ncol(z) > 10) {
      warning("Too many segments - only showing first 10")
      z <- z[, 1:10]
    } else if (!is.null(segments)) {
      if (length(setdiff(segments, seq_len(ncol(z)))) > 0)
        warning("Some segments not in SplicedView")
      z <- z[, intersect(seq_len(ncol(z)), segments)]
    }

    g <- autoplot(z, facets = ~ Series, col = I(colour)) +
      ggplot2::labs(title = "Average Cross Power on Segments",
                    subtitle = paste(subtitle, ':', paste(columns, collapse = ", "))) +
      ggplot2::coord_trans(x = "log2") +
      ggplot2::xlab("Period / sec") +
      ggplot2::ylab("Average Cross Power")

    print(g)

  }

  output_dfr
}


# Samples the average cross power in segments of a SplicedView object
sample_ave_cross_power_spliceview <- function(sv, num_samples, replace = TRUE, ...) {
  cross_wavelet_list <- apply_segment_spliceview(sv, FUN = analyze_coherency, ...)
  output_mat <- sapply(cross_wavelet_list$output, function(x) x$Power.xy.avg)

  # Each element in cross_wavelet_list$output has the same frequency scale so take first
  axis_2_mat  <- sapply(cross_wavelet_list$output, function(x) x$axis.2)
  period_value <- 2^(axis_2_mat[,1]) / sv$recording$fps

  period_sample <- sample(nrow(output_mat), num_samples, replace = replace)
  segment_sample <- sample(ncol(output_mat), num_samples, replace = replace)
  sampled_cross_power <- cbind.data.frame(
    Period = period_value[period_sample],
    Average_Cross_Power = output_mat[cbind(period_sample, segment_sample)]
  )
  class(sampled_cross_power) <- c("SampledAverageCrossPowerSpliceView", "data.frame")

  sampled_cross_power
}


# Samples average power in segments of a SplicedView object
sample_ave_power_spliceview <- function(sv, num_samples, replace = TRUE, ...) {
  wavelet_list <- apply_segment_spliceview(sv, FUN = analyze_wavelet, ...)
  output_mat <- sapply(wavelet_list$output, function(x) x$Power.avg)

  # Each element in wavelet_list$output has the same frequency scale so take first
  axis_2_mat  <- sapply(wavelet_list$output, function(x) x$axis.2)
  period_value <- 2^(axis_2_mat[,1]) / sv$recording$fps

  period_sample <- sample(nrow(output_mat), num_samples, replace = replace)
  segment_sample <- sample(ncol(output_mat), num_samples, replace = replace)
  sampled_power <- cbind.data.frame(
    Period = period_value[period_sample],
    Average_Power = output_mat[cbind(period_sample, segment_sample)]
  )
  class(sampled_power) <- c("SampledAveragePowerSpliceView", "data.frame")

  sampled_power
}


#' Compare the average power distribution of two SplicedViews using sampling on
#' each segment
#'
#' @param sv1 `SplicedView` object.
#' @param sv2 `SplicedView` object.
#' @param name1 name for first object.
#' @param name2 name for second object.
#' @param num_samples number of samples to draw from segments.
#' @param column column name in the data e.g. 'Nose_x_Central_Sitar'.
#' @param show_plot show the plot?
#'
#' @export
#' @return list of two data.frames containing the sampled data.
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' d1 <- get_duration_annotation_data(r)
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#'
#' # only one relevant section for sample data
#' splicing_smile_df <- splice_time(d1, tier ='INTERACTION',
#'   comments = 'Mutual look and smile')
#' sv_duration_smile <- get_spliced_view(jv, splicing_df = splicing_smile_df)
#'
#' splicing_alap_df <- splice_time(
#'   d1, tier = 'FORM', comments = 'Alap'
#' )
#' sv_duration_alap <- get_spliced_view(jv, splicing_df = splicing_alap_df)
#'
#' sample_list <- compare_avg_power2(
#' sv_duration_smile, sv_duration_alap, 'Smile', 'Alap', num_samples = 100,
#'   column = "Nose_x_Central_Sitar")
compare_avg_power2 <- function(sv1, sv2, name1, name2, num_samples,
                                       column, show_plot = TRUE) {
  stopifnot(class(sv1)[1] == 'SplicedView', class(sv2)[1] == 'SplicedView',
            sv1$recording$stem == sv2$recording$stem, length(column) == 1)

  sampled1_dfr <- sample_ave_power_spliceview(sv1, num_samples = num_samples, column = column)
  sampled2_dfr <- sample_ave_power_spliceview(sv2, num_samples = num_samples, column = column)

  l <- list(sampled1_dfr, sampled2_dfr)
  names(l) <- c(name1, name2)

  if (show_plot) {

    long_dfr <- dplyr::bind_rows(l, .id = 'Sampled_From')

    subtitle <- c(sv1$recording$stem, sv1$vid, sv1$direct, sv1$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    g <- ggplot2::ggplot(long_dfr, ggplot2::aes(x = .data$Period, colour = .data$Sampled_From)) +
      ggplot2::xlab("Period / sec") +
      ggplot2::labs(title = "Comparison of Average Power on Sampled Segments",
                    subtitle = paste(subtitle, ':', column)) +
      ggplot2::scale_x_continuous(trans='log2') +
      ggplot2::geom_line(ggplot2::aes(y = .data$Average_Power)) +
      ggplot2::facet_wrap(~.data$Sampled_From)
    print(g)
  }

  invisible(l)
}


#' Compare the average cross power distribution of two SplicedViews using
#' sampling on each segment
#'
#' @param sv1 `SplicedView` object.
#' @param sv2 `SplicedView` object.
#' @param name1 name for first object.
#' @param name2 name for second object.
#' @param num_samples number of samples to draw from segments.
#' @param columns column names in the data e.g. c('Nose_x', 'Nose_y').
#' @param show_plot show the plot?
#'
#' @return list of two data.frames containing the sampled data.
#' @export
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' d1 <- get_duration_annotation_data(r)
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#'
#' # only one relevant section for sample data
#' splicing_smile_df <- splice_time(d1, tier ='INTERACTION',
#'   comments = 'Mutual look and smile')
#' sv_duration_smile <- get_spliced_view(jv, splicing_df = splicing_smile_df)
#'
#' splicing_alap_df <- splice_time(
#'   d1, tier = 'FORM', comments = 'Alap'
#' )
#' sv_duration_alap <- get_spliced_view(jv, splicing_df = splicing_alap_df)
#'
#' sample_list <- compare_avg_cross_power2(
#' sv_duration_smile, sv_duration_alap, 'Smile', 'Alap', num_samples = 100,
#'   columns = c("Nose_x_Central_Sitar", "Nose_y_Central_Sitar"))
compare_avg_cross_power2 <- function(sv1, sv2, name1, name2, num_samples,
                                       columns, show_plot = TRUE) {
  stopifnot(class(sv1)[1] == 'SplicedView', class(sv2)[1] == 'SplicedView',
            sv1$recording$stem == sv2$recording$stem, length(columns) == 2)

  sampled1_dfr <- sample_ave_cross_power_spliceview(sv1, num_samples = num_samples,
                                                    columns = columns)
  sampled2_dfr <- sample_ave_cross_power_spliceview(sv2, num_samples = num_samples,
                                                    columns = columns)
  l <- list(sampled1_dfr, sampled2_dfr)
  names(l) <- c(name1, name2)

  if (show_plot) {

    long_dfr <- dplyr::bind_rows(l, .id = 'Sampled_From')

    subtitle <- c(sv1$recording$stem, sv1$vid, sv1$direct, sv1$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    g <- ggplot2::ggplot(long_dfr, ggplot2::aes(x = .data$Period, colour = .data$Sampled_From)) +
      ggplot2::xlab("Period / sec") +
      ggplot2::labs(title = "Comparison of Average Cross Power on Sampled Segments",
                    subtitle = paste(subtitle, ':', paste0(columns, collapse = ", "))) +
      ggplot2::scale_x_continuous(trans='log2') +
      ggplot2::geom_line(ggplot2::aes(y = .data$Average_Cross_Power)) +
      ggplot2::facet_wrap(~Sampled_From)
    print(g)
  }

  invisible(l)
}


#' Randomly create matching segments from a splicing table without overlaps
#'
#' Works by adding a random offset to each start time in the splice. Uses rejection
#' sampling to avoid overlaps with the input segments and additional segments
#' from a list of splices.
#'
#' @param splicing_dfr `Splice` object.
#' @param v `View` object.
#' @param rejection_list list of `Splice` objects for rejection.
#' @param num_splices number of random splices to generate.
#'
#' @return list of splicing data.frames.
#' @export
#' @family statistical and analysis functions
#'
#' @examples
#' r1 <- get_sample_recording()
#' d1 <- get_duration_annotation_data(r1)
#' rv1 <- get_raw_view(r1, "Central", "", "Sitar")
#' splicing_df <- splice_time(d1, tier ='INTERACTION', comments = 'Mutual look and smile')
#' # Only first segment relevant for sample data
#' x <- sample_offset_splice(splicing_df[1,], rv1, num_splices = 100)
sample_offset_splice <- function(splicing_dfr, v, num_splices, rejection_list = list()) {
  stopifnot("Splice" %in% class(splicing_dfr), "View" %in% class(v),
            num_splices > 0, is.list(rejection_list))

  # Discard random splices that appear in the rejection list - includes the original splice
  rejection_splices <- c(list(splicing_dfr), rejection_list)

  # Find max possible offset based on recording length
  max_time <- max(v$df[['Time']], na.rm = TRUE)

  # Span of the splice
  max_splice <- max(splicing_dfr[['End']], na.rm = TRUE)
  min_splice <- min(splicing_dfr[['Start']], na.rm = TRUE)
  stopifnot(min_splice >= 0, max_splice <= max_time)

  splicing_list <- list()
  current_num_splices <- 0

  # Repeat until we get the desired number of splices
  while(current_num_splices < num_splices) {

    # Random start times
    start_times <- stats::runif(num_splices, min = -min_splice, max = max_time - max_splice)

    # Generate a list of new sampling data.frames
    new_splicing_list <- lapply(start_times, function(x) {
      new_splicing_dfr <- splicing_dfr
      new_splicing_dfr[c('Start', 'End')] <- x + new_splicing_dfr[c('Start', 'End')]
      if (new_splicing_dfr[nrow(new_splicing_dfr), 'End'] > max_time) browser()
      new_splicing_dfr
    })

    # Which ones overlap the original splicing?
    is_overlapped <- rep(FALSE, length(new_splicing_list))
    for (rsp in rejection_splices) {
      is_overlapped <- is_overlapped |
        sapply(new_splicing_list, function(x) is_splice_overlapping(x, rsp))
    }

    # Remove the overlapping ones
    splicing_list <- c(splicing_list, new_splicing_list[!is_overlapped])
    current_num_splices <- length(splicing_list)
    message("Accepted splices: ", current_num_splices)
  }

  splicing_list <- splicing_list[seq_len(num_splices)]
  names(splicing_list) <- paste('Sample splice', seq_along(splicing_list))
  splicing_list
}


#' Randomly create matching segments from a splicing table without overlaps
#'
#' Works by randomly varying the gaps between segments assuming that the gap number
#' follow a Poisson process with rate given by the average sample gap length in
#' the input splice. Durations of segments remain the same.
#'
#' Uses rejection sampling to avoid overlaps with the input segments and
#' additional segments from a list of splices.
#'
#' @param splicing_dfr `Splice` object.
#' @param v `View` object.
#' @param rejection_list list of `Splice` objects for rejection.
#' @param num_splices number of random splices to generate.
#'
#' @return list of splicing data.frames.
#' @export
#' @family statistical and analysis functions
#'
#' @examples
#' r1 <- get_sample_recording()
#' d1 <- get_duration_annotation_data(r1)
#' rv1 <- get_raw_view(r1, "Central", "", "Sitar")
#' splicing_df <- splice_time(d1, tier ='INTERACTION', comments = 'Mutual look and smile')
#' # Only first segment relevant for sample data
#' x <- sample_gap_splice(splicing_df[1,], rv1, num_splices = 10)
sample_gap_splice <- function(splicing_dfr, v, num_splices, rejection_list = list()) {

  stopifnot("Splice" %in% class(splicing_dfr), "View" %in% class(v),
            num_splices > 0, is.list(rejection_list))

  # Discard random splices that appear in the rejection list - includes the original splice
  rejection_splices <- c(list(splicing_dfr), rejection_list)

  # Find max possible offset based on recording length
  max_time <- max(v$df[['Time']], na.rm = TRUE)

  # Total span of segments
  total_span <- max(splicing_dfr[['Start']], na.rm = TRUE) -
    min(splicing_dfr[['Start']], na.rm = TRUE)
  stopifnot(total_span <= max_time)

  splicing_list <- list()
  current_num_splices <- 0

  # Number of gaps follows Poisson process
  duration <- splicing_dfr$End - splicing_dfr$Start
  lagged_duration <- dplyr::lag(duration, default = 0)
  gaps <- dplyr::lead(splicing_dfr$Start, default = max_time) - splicing_dfr$Start
  gaps <- c(splicing_dfr$Start[1], gaps)
  ave_gap_splice <- mean(gaps, na.rm = TRUE) # seconds

  # Repeat until we get the desired number of splices
  while(current_num_splices < num_splices) {

    # Generate a list of new sampling data.frames
    new_splicing_list <- lapply(seq_len(num_splices), function(x) {
      # gap is interarrival time and exponentially distributed
      new_gaps <- stats::rexp(nrow(splicing_dfr), rate = 1 / ave_gap_splice)
      new_splicing_dfr <- splicing_dfr
      new_splicing_dfr$Start <- cumsum(new_gaps + lagged_duration)
      new_splicing_dfr$End <- new_splicing_dfr$Start + duration
      new_splicing_dfr
    })

    # Which ones overlap the original splicing or go beyond recording?
    is_rejected <- rep(FALSE, length(new_splicing_list))
    for (rsp in rejection_splices) {
      is_rejected <- is_rejected |
        sapply(new_splicing_list, function(x) is_splice_overlapping(x, rsp)) |
        sapply(new_splicing_list, function(x) max(x[nrow(x), 'End'], na.rm = TRUE) > max_time)
    }

    # Remove the overlapping ones
    splicing_list <- c(splicing_list, new_splicing_list[!is_rejected])
    current_num_splices <- length(splicing_list)
    message("Accepted splices: ", current_num_splices)
  }

  splicing_list <- splicing_list[seq_len(num_splices)]
  names(splicing_list) <- paste('Sample splice', seq_along(splicing_list))
  splicing_list
}


#' Get onset differences
#'
#' Calculates the difference in onset times for each instrument pair in
#' milli-seconds.
#' @param onset_obj `OnsetsSelected` object.
#' @param instruments character vector of instrument names.
#' @param expr R expression to subset onsets (not required).
#' @param splicing_dfr `Splice` object (not required).
#'
#' @return `OnsetsDifference` object.
#' @export
#' @family statistical and analysis functions
#'
#' @examples
#' r1 <- get_sample_recording()
#' o1 <- get_onsets_selected_data(r1)
#' head(difference_onsets(o1, instruments = c('Inst', 'Tabla')))
#' head(difference_onsets(o1, instruments = c('Inst', 'Tabla'), expr = 'Matra == 3'))
difference_onsets <- function(onset_obj, instruments, expr = NULL, splicing_dfr = NULL) {
  stopifnot("OnsetsSelected" %in% class(onset_obj),
            is.null(splicing_dfr) || "Splice" %in% class(splicing_dfr))

  dfr_list <- onset_obj[sapply(onset_obj, is.data.frame)]
  dfr <- dplyr::bind_rows(dfr_list, .id = 'Metre')

  if (!is.null(expr)) {
    parsed_expr <- rlang::parse_expr(expr)
    dfr <- dplyr::filter(dfr, !!parsed_expr)
  }

  dfr <- dfr[, c('Metre', instruments), drop=FALSE]

  # Calculate onset differences for each instrument pair
  instrument_combn <- utils::combn(instruments, 2)
  output_dfr <- data.frame(Metre = dfr['Metre'],
                           Ref_Beat_Time = rowMeans(dfr[instruments], na.rm = TRUE))
  for (j in seq_len(ncol(instrument_combn))) {
    inst1 <- instrument_combn[1, j]
    inst2 <- instrument_combn[2, j]
    col_name <- paste(inst1, inst2, sep = "-")
    output_dfr[col_name] <- dfr[inst1] - dfr[inst2]
  }

  # Splice the time line if required
  if (!is.null(splicing_dfr)) {
    segment_list <- list()
    for (r in seq_len(nrow(splicing_dfr))) {
      a <- splicing_dfr$Start[r]
      b <- splicing_dfr$End[r]
      segment <-  if (is.null(expr)) splicing_dfr$Segment[r] else paste(expr, splicing_dfr$Segment[r], sep = " & ")
      segment_list[[segment]] <- output_dfr[
        !is.na(output_dfr$Ref_Beat_Time) & output_dfr$Ref_Beat_Time >= a &
          output_dfr$Ref_Beat_Time <= b, ,drop=FALSE]
    }
    output_dfr <- dplyr::bind_rows(segment_list, .id = 'Segment')
  } else {
    output_dfr$Segment <- if (!is.null(expr)) expr else 'All'
  }

  # Convert differences to ms
  pair_cols <- apply(instrument_combn, 2, paste, collapse = "-")
  output_dfr[pair_cols] <- output_dfr[pair_cols] * 1000

  class(output_dfr) <- c('OnsetsDifference', 'data.frame')
  output_dfr
}


#' Summary of difference in onsets
#'
#' @param onset_obj `OnsetsSelected` object.
#' @param splicing_dfr `Splice` object
#' @param instruments character vector of instrument names.
#' @param expr R expression to subset onsetsSelected
#' @param recording `Recording` object.
#' @param show_plot show a plot? (Default is FALSE).
#' @param filter_pair regular expression to filter instrument pair names.
#' @param na_omit omit NAs (Default is TRUE).
#' @param time_breaks suggests the number of major time tick marks (default is NULL).
#'
#' @return a summary data frame of onset difference statistics.
#' @export
#' @family statistical and analysis functions
#'
#' @examples
#' r1 <- get_sample_recording()
#' o1 <- get_onsets_selected_data(r1)
#' d1 <- get_duration_annotation_data(r1)
#' splice_dfr <- splice_time(d1, tier = 'FORM')
#' summary_onsets(o1, r1, instruments = c('Inst', 'Tabla'),
#'   splicing_dfr = splice_dfr, show_plot = TRUE)
summary_onsets <- function(onset_obj, recording, instruments, splicing_dfr = NULL, expr = NULL,
                           show_plot = FALSE, filter_pair = NULL, na_omit = TRUE, time_breaks = NULL) {

  stopifnot("OnsetsSelected" %in% class(onset_obj),
            "Recording" %in% class(recording),
            is.null(splicing_dfr) || "Splice" %in% class(splicing_dfr))

  breaks <- if (is.null(time_breaks)) ggplot2::waiver() else scales::pretty_breaks(time_breaks)

  dfr <- difference_onsets(onset_obj, instruments = instruments, splicing_dfr = splicing_dfr, expr = expr)
  if (nrow(dfr) == 0) stop('No data from splice to summarise')

  long_dfr <- tidyr::pivot_longer(dfr, cols = -c(.data$Metre, .data$Ref_Beat_Time, .data$Segment),
                                  names_to = 'Instrument_Pair', values_to = 'Value')

  if (!is.null(filter_pair)) {
    long_dfr <- dplyr::filter(long_dfr, grepl(filter_pair, .data$Instrument_Pair))
  }

  long_dfr$Segment <- factor(long_dfr$Segment, unique(long_dfr$Segment))
  summary_dfr <- dplyr::select(long_dfr, -c(.data$Metre, .data$Ref_Beat_Time))
  summary_dfr <- dplyr::group_by(summary_dfr, .data$Instrument_Pair, .data$Segment)
  summary_dfr <- dplyr::summarise(
    summary_dfr,
    'N' = sum(!is.na(.data$Value)),
    'Mean Difference' = mean(.data$Value, na.rm = TRUE),
    'Mean Absolute Difference' = mean(abs(.data$Value), na.rm = TRUE),
    'SD Difference' = stats::sd(.data$Value, na.rm = TRUE)
  )

  if (na_omit) {
    summary_dfr <- dplyr::filter(summary_dfr, .data$N > 0)
  }


  if (show_plot) {

    long_dfr <- tidyr::pivot_longer(summary_dfr, cols = -c('Segment', 'Instrument_Pair'),
                                    names_to = 'Statistic', values_to = 'Value')
    long_dfr$Statistic_f <- factor(long_dfr$Statistic, unique(long_dfr$Statistic))

    g <- ggplot2::ggplot(long_dfr) +
      ggplot2::geom_col(ggplot2::aes(x = .data$Value, y = .data$Instrument_Pair, fill = .data$Statistic)) +
      ggplot2::xlab('Value / number or ms') +
      ggplot2::scale_x_continuous(breaks = breaks) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::facet_grid(Segment ~ Statistic_f,
                          labeller = ggplot2::labeller(Statistic_f = ggplot2::label_wrap_gen(12),
                                                       Segment = ggplot2::label_wrap_gen(12)), scales = 'free_x') +
      ggplot2::ggtitle("Summary of Onset Difference Statistic for Instrument Pairs",
                       subtitle = recording$stem)
    print(g)
  }

  invisible(as.data.frame(summary_dfr))
}


#' Visualise random splices
#'
#' @param splicing_df `Splice` object.
#' @param splicing_list a list of `Splice` objects.
#' @param jv `JoinedView` object.
#' @param overlay overlay the segments for a density plot?
#' @param avoid_splice_list list of `Splice objects` that determine times not to sample.
#' @param unstack overlay segments on top of each other? (default is FALSE).
#'
#' @return a `ggplot` object.
#' @export
#' @family statistical and analysis functions
#'
#' @examples
#' r <- get_sample_recording()
#' fv_list <- get_filtered_views(r, data_points = 'Nose', n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' splicing_df <- splice_time(list(a = c(0, 5), b = c(10, 15)))
#' splicing_list <- sample_offset_splice(splicing_df, jv, num_splices = 20)
#' visualise_sample_splices(splicing_df, splicing_list, jv)

visualise_sample_splices <- function(splicing_df, splicing_list, jv, overlay = TRUE,
                                     avoid_splice_list = list(), unstack = FALSE) {
  stopifnot("Splice" %in% class(splicing_df),
            is.list(splicing_list), 'View' %in% class(jv),
            is.list(avoid_splice_list))

  subtitle <- c(jv$recording$stem, jv$vid, jv$direct, jv$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  df <- dplyr::bind_rows(splicing_list, .id = 'Sample')

  if (unstack) {
    g <- ggplot2::ggplot(df) +
      ggplot2::geom_linerange(ggplot2::aes(y = .data$Sample, xmin = .data$Start, xmax = .data$End, colour = .data$Segment)) +
      ggplot2::theme(axis.ticks.y=ggplot2::element_blank(), axis.text.y=ggplot2::element_blank(),
            panel.background = ggplot2::element_blank()) +
      ggplot2::facet_wrap(~.data$Segment) +
      ggplot2::geom_rect(data = splicing_df,
                ggplot2::aes(xmin = .data$Start, xmax = .data$End, ymin = 0, ymax = Inf, fill = .data$Segment), alpha = 0.5)

  } else {
    g <- ggplot2::ggplot(df, ggplot2::aes(y = .data$Segment)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$Start, xmax = .data$End), linewidth = 3, alpha = 0.1) +
      ggplot2::geom_rect(data = splicing_df,
                         ggplot2::aes(xmin = .data$Start, xmax = .data$End, ymin = 0,
                                      ymax = Inf, fill = .data$Segment), alpha = 0.5)
  }

  # Add scale and title
  g <- g + ggplot2::labs(title = "Visualisation of Random Splices", subtitle = subtitle) +
    ggplot2::xlab("Time (min:sec)") +
    ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) # error

  if (length(avoid_splice_list) > 0) {
    avoid_splice_dfr <-do.call(merge_splice, c(avoid_splice_list, operation = 'union'))
    if (unstack) avoid_splice_dfr <- avoid_splice_dfr[-1] else avoid_splice_dfr$Segment <- NA
    g <- g + ggplot2::geom_rect(data = avoid_splice_dfr,
                       ggplot2::aes(xmin = .data$Start, xmax = .data$End, ymin = 0, ymax = Inf), alpha = 0.5)
  }

  g
}

