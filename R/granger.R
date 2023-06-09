# Functions to assess Granger causality

#' Granger causality tests applied to a SplicedView
#'
#' @param obj SplicedView object
#' @param var1 column name of response
#' @param var2 column name of predictor
#' @param var3 column name of conditioning
#' @param lag in seconds (rounded to nearest frame)
#' @param granger_fn function to perform Granger test (defaults to ms_grangertest2)
#' @param cond_granger_fn function to perform conditional Granger test (defaults to ms_condgrangertest)
#'
#' @return GrangerTime object
#' @export
#' @family Granger Causality
#'
#' @examples
#'
#' r1 <- get_sample_recording()
#' rv_list <- get_raw_views(r1)
#' pv_list <- lapply(rv_list, get_processed_view)
#' get_data_points(pv_list$Central_Sitar)
#' fv_list <- lapply(pv_list, apply_filter_sgolay, data_points = "Nose", n = 41, p = 3)
#' jv_sub <- get_joined_view(fv_list)
#' splicing_df <- splice_time(jv_sub, win_size = 5, step_size = 0.5)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 1/25)
#' granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", "Nose_y_Central_Tabla", lag = 1/25)

granger_test <- function(obj, var1, var2, var3 = "", lag = 1, granger_fn = ms_grangertest2,
                         cond_granger_fn = ms_condgrangertest) {
  stopifnot("SplicedView" %in% class(obj))
  df <- obj$df
  stopifnot(var1 %in% colnames(df), var2 %in% colnames(df),
            (var3 == "") || (var3 %in% colnames(df)) )

  # Calculate frame lag
  order <- round(lag * obj$recording$fps)

  if (var3 == "") {
    df <- dplyr::select(df, .data$Frame, .data$Segment, !!var1, !!var2)
  } else {
    df <- dplyr::select(df, .data$Frame, .data$Segment, !!var1, !!var2, !!var3)
  }
  df <- dplyr::group_by(df, .data$Segment)

  # Error tests
  splicing_df <- obj$splicing_df
  if (any(duplicated(splicing_df[['Segment']]))) {
    stop("Not a valid splicing data.frame for Granger testing")
  }

  n_df_group <- dplyr::pull(dplyr::summarise(df, n = dplyr::n()), "n")
  if (any(n_df_group <= 1)) {
    stop("There must be more than one data point in all time slices")
  }

  if (var3 == "") {
    l1 <- dplyr::group_map(df, ~ granger_fn(.[[var1]], .[[var2]], order = order))
    l2 <- dplyr::group_map(df, ~ granger_fn(.[[var2]], .[[var1]], order = order))
  } else {
    l1 <- dplyr::group_map(df, ~ cond_granger_fn(.[[var1]], .[[var2]], .[[var3]], order = order))
    l2 <- dplyr::group_map(df, ~ cond_granger_fn(.[[var2]], .[[var1]], .[[var3]], order = order))
  }

  df1 <- as.data.frame(t(sapply(l1, function(x) as.numeric(x[2,]))))
  df2 <- as.data.frame(t(sapply(l2, function(x) as.numeric(x[2,]))))
  colnames(df1) <- c("Res.Df", "Df", "F", "P_Value")
  colnames(df2) <- c("Res.Df", "Df", "F", "P_Value")
  df1_add <- data.frame(Segment = unique(df$Segment), Var1 = var1, Var2 = var2, Var3 = var3)
  df2_add <- data.frame(Segment = unique(df$Segment), Var1 = var2, Var2 = var1, Var3 = var3)
  output_df <- dplyr::bind_rows(dplyr::bind_cols(df1, df1_add),
                                dplyr::bind_cols(df2, df2_add))


  l <- list(df = output_df, var1 = var1, var2 = var2, var3 = var3,
            recording = obj$recording, order = order)
  class(l) <- "GrangerTime"

  l
}


#' Plot a Granger S3 object
#'
#' @param object S3 object.
#' @param splicing_df Splicing data.frame object.
#' @param lev_sig significance level.
#' @param ... ignored.
#'
#' @return a `ggplot` object.
#' @exportS3Method
#' @family Granger Causality
#'
#' @examples
#' r1 <- get_sample_recording()
#' fv_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p = 3)
#' jv_sub <- get_joined_view(fv_list)
#' splicing_df <- splice_time(jv_sub, win_size = 3, step_size = 0.5)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 3/25)
#' autoplot(g, splicing_df)

autoplot.GrangerTime <- function(object, splicing_df, lev_sig = 0.05, ...) {
  stopifnot("Splice" %in% class(splicing_df))

  df <- object$df
  condition_vars <- paste(unique(df$Var3), collapse = ", ")
  if (condition_vars == "") {
    title <- paste0(class(object)[1], ": Lagged at ", object$order / object$recording$fps, "s")
  } else {
    title <- paste0(class(object)[1], ": Conditioned on ", condition_vars,
                    ", Lagged at ", object$order / object$recording$fps, "s")
  }

  splicing_df$Centre <- (splicing_df$Start + splicing_df$End) / 2
  splicing_df$Width <- splicing_df$End - splicing_df$Start
  df <- dplyr::inner_join(df, splicing_df[c('Segment', 'Centre', 'Width')], by = 'Segment')
  df$Test <- paste(df$Var1, df$Var2, sep = ' <- \n')

  ggplot2::ggplot(df) +
    ggplot2::geom_col(ggplot2::aes(x = .data$Centre, y = .data$P_Value),
                      width = max(df$Width)/10, fill = 'black') +
    ggplot2::geom_hline(yintercept = lev_sig, colour = 'blue') +
    ggplot2::labs(title = title, subtitle = object$recording$stem) +
    ggplot2::xlab("Time (min:sec)") +
    ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) + # error without explicit reference to hms
    ggplot2::facet_grid(rows = ggplot2::vars(.data$Test))
}


#' Plot influence diagram from a GrangerTest object
#'
#' Arrows show causality (influencing) direction.
#'
#' By default `two_arrows` is TRUE and an influencing arrow is drawn for each
#' significant p-value. If `two_arrows` is FALSE and one
#' of the p-values is signficant then -log10(p_value) difference is plotted i.e
#  an insignificant p-value may be used in the difference.
#'
#' @param obj GrangerTest object
#' @param splicing_df Splicing data.frame object
#' @param lev_sig significance level
#' @param two_arrows plot influence arrows both ways? (Default is TRUE).
#'
#' @return ggplot object
#' @export
#' @family Granger Causality
#'
#' @examples
#' r1 <- get_sample_recording()
#' fv_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p = 3)
#' jv_sub <- get_joined_view(fv_list)
#' splicing_df <- splice_time(jv_sub, win_size = 3, step_size = 0.5)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 3/25)
#'
#' plot_influence_diagram(g, splicing_df)
#' plot_influence_diagram(g, splicing_df, two_arrows = TRUE)
#'
#' d1 <- get_duration_annotation_data(r1)
#' plot_influence_diagram(g, splicing_df) +
#' autolayer(d1, expr = (Tier == "Influence S>T" | Tier == "Influence T>S") & Out <= 60,
#'           fill_col = "Tier")

plot_influence_diagram <- function(obj, splicing_df, two_arrows = TRUE, lev_sig = 0.05) {
  stopifnot(class(obj) == 'GrangerTime', "Splice" %in% class(splicing_df))

  df <- obj$df
  condition_vars <- paste(unique(df$Var3), collapse = ", ")
  if (condition_vars == "") {
    title <- "Influence Diagram"
  } else {
    title <- paste("Influence Diagram Conditioned on", condition_vars)
  }

  splicing_df$Centre <- (splicing_df$Start + splicing_df$End) / 2
  df <- dplyr::inner_join(df, splicing_df[c('Segment', 'Centre')], by = 'Segment')
  x <- df[c("Var1", "Centre", "P_Value")]
  wide_df <- tidyr::pivot_wider(x, names_from = "Var1", values_from = "P_Value")

  vars <- colnames(wide_df)[-1]
  text_posx <- (max(df$Centre) - min(df$Centre)) / 2 + min(df$Centre)

  wide_df <- dplyr::mutate(
    wide_df,
    Value = dplyr::if_else(.data[[vars[1]]] < lev_sig | .data[[vars[2]]] < lev_sig,
    log10(.data[[vars[2]]]/.data[[vars[1]]]), NA_real_),
    UpValue = dplyr::if_else(
      .data[[vars[1]]] < lev_sig, -log10(.data[[vars[1]]]), NA_real_),
    DownValue = dplyr::if_else(
      .data[[vars[2]]] < lev_sig, log10(.data[[vars[2]]]), NA_real_)
  )
  wide_df <- dplyr::select(wide_df, .data$Centre, .data$Value, .data$UpValue, .data$DownValue)

  if (two_arrows) {

    ggplot2::ggplot(wide_df) +
      ggplot2::geom_segment(colour="black", ggplot2::aes(x=.data$Centre, xend=.data$Centre, y=0, yend=.data$UpValue),
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed")) +
      ggplot2::geom_segment(colour="black", ggplot2::aes(x=.data$Centre, xend=.data$Centre, y=0, yend=.data$DownValue),
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed")) +
      ggplot2::labs(title = title, subtitle = obj$recording$stem) +
      ggplot2::xlab("Time (min:sec)") +
      ggplot2::ylab("-log10(P_Value) if significant") +
      ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) + # error
      ggplot2::annotate("text", label = vars[1], x=text_posx, y=max(-log10(df$P_Value))) +
      ggplot2::annotate("text", label = vars[2], x=text_posx, y=min(log10(df$P_Value)))
  } else {

    ggplot2::ggplot(wide_df) +
      ggplot2::geom_segment(colour="black", ggplot2::aes(x=.data$Centre, xend=.data$Centre, y=0, yend=.data$Value),
                   arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed")) +
      ggplot2::labs(title = title, subtitle = obj$recording$stem) +
      ggplot2::xlab("Time (min:sec)") +
      ggplot2::ylab("-log10(P_Value) difference if one significant") +
      ggplot2::scale_x_time(labels = function(l) strftime(hms::as_hms(l), '%M:%S')) + # error
      ggplot2::annotate("text", label = vars[1], x=text_posx, y=max(-log10(df$P_Value))) +
      ggplot2::annotate("text", label = vars[2], x=text_posx, y=min(log10(df$P_Value)))

  }
}


#' Map duration object comments to a Granger Test object
#'
#' @param d DurationObject
#' @param g GrangerTest object
#' @param influence1 Comment X>Y string in the Granger Test of Y~X i.e. X causes Y
#' @param influence2 Comment X>Y string in the Granger Test of Y~X i.e. X causes Y
#'
#' @return modified Duration object
#' @export
#' @family Granger Causality
#'
#' @examples
#' r <- get_sample_recording()
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv_sub <- get_joined_view(fv_list)
#' splicing_df <- splice_time(jv_sub, win_size = 5, step_size = 0.5)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 1/25)
#' d <- get_duration_annotation_data(r)
#' map_to_granger_test(d, g, "Influence T>S", "Influence S>T")

map_to_granger_test <- function(d, g, influence1, influence2) {

  d <- dplyr::mutate(d, Test = dplyr::case_when(
        Tier == !!influence1 ~ paste0(!!g$var1, " <- \n", !!g$var2),
        Tier == !!influence2 ~ paste0(!!g$var2, " <- \n", !!g$var1)
      ))

  d
}


#' Get Granger Causality interactions
#'
#' @param sv SplicedView object
#' @param columns vector of column names
#' @param sig_level significance level
#' @param lag in seconds (rounded to nearest frame)
#' @param cond_column name of conditioning column
#' @param granger_fn function to perform Granger test (defaults to ms_grangertest2)
#'
#' @return GrangerInteraction object
#' @export
#' @family Granger Causality
#'
#' @examples
#' r <- get_sample_recording()
#' fv_list <- get_filtered_views(r, "Nose", n = 41, p = 3)
#' jv_sub <- get_joined_view(fv_list)
#' l <- list(a = c(0, 300), b = c(300, 600), c = c(600, 900))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' g <- get_granger_interactions(sv, c("Nose_x_Central_Sitar", "Nose_x_Central_Tabla"), lag = 1/25)
#' print(g)

get_granger_interactions <- function(sv, columns, cond_column = "", sig_level = 0.05, lag = 1,
                                     granger_fn = ms_grangertest2) {
  stopifnot(all(c("SplicedView", "JoinedView") %in% class(sv)))

  # Calculate granger tests for all combinations of columns
  a <- utils::combn(columns, 2)

  gc_list <- list()
  for (j in seq_len(ncol(a))) {
    var1 <- a[1, j]
    var2 <- a[2, j]
    g_test <- paste0(var1, " <--> ", var2)
    if (cond_column != "") g_test <- paste0(g_test, " | ", cond_column)
    message("Calculating Granger Test: ", g_test)
    gc_list[[g_test]] <- granger_test(sv, var1, var2, cond_column, lag = lag,
                                      granger_fn = granger_fn)
  }
  l <- list(gc_list = gc_list, sig_level = sig_level, lag = lag, cond_column = cond_column)
  class(l) <- "GrangerInteraction"

  invisible(l)
}


#' Plot network diagram of Granger Causalities
#'
#' @param x GrangerInteration object
#' @param mfrow passed to [par()]
#' @param mar passed to [par()]
#' @param oma passed to [par()]
#' @param ... passed through to [plot.igraph()]
#'
#' @return data.frame of P-Values
#' @export
#' @family Granger Causality
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' fv_list <- get_filtered_views(r, "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' jv <- subset(jv, Time <= 5*60)
#' l <- list(a = c(0, 100), b = c(100, 200), c = c(200, 300))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(jv, splicing_df)
#' gi <- get_granger_interactions(sv, c("Nose_x_Central_Sitar", "Nose_x_Central_Tabla"), lag = 1/25)
#' print(gi)
plot.GrangerInteraction <- function(x, mfrow = NULL, mar = c(1, 1, 1, 1),
                                    oma = c(1, 1, 1, 1), ...) {

  gc_list <- x$gc_list

  # Single data frame of P-values
  df <- dplyr::bind_rows(lapply(gc_list, function(x) x$df), .id = "Test")
  df$mlog10pv <- ifelse(df$P_Value < x$sig_level, -log10(df$P_Value), NA_real_)

  if (is.null(mfrow)) {
    num_segments <- length(unique(df$Segment))
    mfrow <- c(num_segments %/% 4 + (num_segments %% 4 > 0), min(num_segments, 4))
  }

  oldpar <- graphics::par(no.readonly = TRUE)    # code line i
  on.exit(graphics::par(oldpar))                 # code line i + 1
  newpar <- graphics::par(mfrow=mfrow, oma=oma, mar=mar) # 
  
  # Extract the node names
  nodes <- data.frame(id = sapply(strsplit(unique(df$Var1), "_"), function(x) x[4]))

  # Loop through Segments
  for (segment in unique(df$Segment)) {

    # Extract the links and removing missings
    splice_df <- dplyr::filter(df, .data$Segment == !!segment)
    splice_df$Var1 <- sapply(strsplit(splice_df$Var1, "_"), function(x) x[4])
    splice_df$Var2 <- sapply(strsplit(splice_df$Var2, "_"), function(x) x[4])
    links <- data.frame(from = splice_df$Var2, to = splice_df$Var1, x = splice_df$mlog10pv)
    links <- links[!is.na(links$x),,drop = FALSE]
    net <- igraph::graph_from_data_frame(d=links, vertices=nodes, directed=T)

    l <- igraph::layout_in_circle(net)
    igraph::V(net)$size <- max(nchar(nodes$id)) * 10
    igraph::V(net)$label.cex <- 1.25
    if (nrow(links) > 0) {
      igraph::E(net)$color <- "grey"
      igraph::E(net)$label <- round(igraph::E(net)$x, 1)
      igraph::E(net)$label.color <- "red"
    }
    plot(net, layout=l, edge.curved=.4, main=segment, ...)
  }

  conditional <- if (x$cond_column != "") paste0(", Conditional on ", x$cond_column) else ""
  main_title <- paste0("Recording: ", x$gc_list[[1]]$recording$stem, conditional,
                       ", Lag = ", x$lag, "s")
  graphics::mtext(text = main_title, side = 1, line = -1, outer = TRUE)
#  graphics::par(old_params) # opted out by TE 29/5
  graphics::par(oldpar)      # added by TE 29/5
  
  invisible(df)
}


#' Test for Granger Causality
#'
#' Faster implementation of the vector version of  [lmtest::grangertest()]
#' which uses a vectorised lag operation.
#'
#' @param x either a bivariate series (in which case y has to be missing) or a univariate series of observations.
#' @param y a univariate series of observations (if x is univariate, too).
#' @param order number of lags (in frames).
#' @param na.action a function for eliminating NAs after aligning the series x and y.
#' @param ... passed to [lmtest::waldtest()].
#'
#' @return Anova object
#' @export
#' @family Granger Causality
#'
#' @examples
#' data(ChickEgg, package = "lmtest")
#' ms_grangertest1(ChickEgg, order = 3)

ms_grangertest1 <- function(x, y, order = 1, na.action = stats::na.omit, ...) {
  ## either x is a 2-column time series
  ## or x and y are univariate time series
  if((NCOL(x) == 2) && missing(y)) {
    xnam <- colnames(x)[1]
    ynam <- colnames(x)[2]
    x <- zoo::as.zoo(x)
    y <- x[,2]
    x <- x[,1]
  } else {
    xnam <- deparse(substitute(x))
    ynam <- deparse(substitute(y))
    x <- zoo::as.zoo(x)
    y <- zoo::as.zoo(y)
    stopifnot((NCOL(x) == 1), (NCOL(y) == 1))
  }

  ## compute lagged observations
  #lagX <- do.call("merge", lapply(1:order, function(k) stats::lag(x, -k)))
  #lagY <- do.call("merge", lapply(1:order, function(k) stats::lag(y, -k)))
  lagX <- stats::lag(x, -seq_len(order))
  lagY <- stats::lag(y, -seq_len(order))

  ## collect series, handle NAs and separate results again
  all <- zoo::merge.zoo(x, y, lagX, lagY)
  colnames(all) <- c("x", "y", paste("x", 1:order, sep = "_"), paste("y", 1:order, sep = "_"))
  all <- na.action(all)
  y <- as.vector(all[,2])
  lagX <- as.matrix(all[,(1:order + 2)])
  lagY <- as.matrix(all[,(1:order + 2 + order)])

  ## fit full model
  fm <- stats::lm(y ~ lagY + lagX)

  ## compare models with waldtest
  rval <- lmtest::waldtest(fm, 2, ...)

  ## adapt annotation
  attr(rval, "heading") <- c("Granger causality test\n",
                             paste("Model 1: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order, ") + Lags(", xnam, ", 1:", order,
                                   ")\nModel 2: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order, ")", sep = ""))

  return(rval)
}


#' Test for Granger Causality
#'
#' Faster implementation of the vector version of  [lmtest::grangertest()]. The
#' function assumes time series always have the same start date and periodicity,
#' which is true for the data in this package.
#'
#' @param x either a bivariate series (in which case y has to be missing) or a univariate series of observations
#' @param y a univariate series of observations (if x is univariate, too).
#' @param order number of lags (in frames).
#' @param na.action a function for eliminating NAs after aligning the series x and y.
#' @param ... passed to [lmtest::waldtest()].
#'
#' @return Anova object
#' @export
#' @family Granger Causality
#'
#' @examples
#' data(ChickEgg, package = "lmtest")
#' ms_grangertest2(ChickEgg, order = 3)


ms_grangertest2 <- function(x, y, order = 1, na.action = stats::na.omit, ...) {

  ## either x is a 2-column time series
  ## or x and y are univariate time series
  if((NCOL(x) == 2) && missing(y)) {
    xnam <- colnames(x)[1]
    ynam <- colnames(x)[2]
    y <- x[,2]
    x <- x[,1]
  } else {
    xnam <- deparse(substitute(x))
    ynam <- deparse(substitute(y))
    stopifnot((NCOL(x) == 1), (NCOL(y) == 1))
  }

  ## compute lagged observations
  x1 <- c(rep(NA, order), x)
  y1 <- c(rep(NA, order), y)
  lagX <- stats::embed(x1, order + 1) # first column contains x
  lagY <- stats::embed(y1, order + 1) # first column contains y

  # Apply na.action
  numcolX <- ncol(lagX)
  numcolY <- ncol(lagY)
  lag_temp <- na.action(cbind(lagY, lagX))
  y <- as.vector(lag_temp[,1])
  lagY <- as.matrix(lag_temp[,2:numcolY,drop=FALSE]) # skip y
  lagX <- as.matrix(lag_temp[,(numcolY + 2):ncol(lag_temp),drop=FALSE]) # skip x

  ## fit full model
  fm <- stats::lm(y ~ lagY + lagX)

  ## compare models with waldtest
  rval <- lmtest::waldtest(fm, 2, ...)

  ## adapt annotation
  attr(rval, "heading") <- c("Granger causality test\n",
                             paste("Model 1: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order, ") + Lags(", xnam, ", 1:", order,
                                   ")\nModel 2: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order, ")", sep = ""))

  return(rval)
}


#' Test for Conditional Granger Causality
#'
#' Faster implementation of the vector version of  [lmtest::grangertest()] with
#' conditioning on the *history* of a third variable. The function assumes time
#' series always have the same start date and periodicity, which is true for the data in this
#' package.
#'
#' @param x response vector of observations.
#' @param y explanatory vector of observations.
#' @param z conditioning vector of observations
#' @param order number of lags (in frames).
#' @param na.action a function for eliminating NAs after aligning the series x and y.
#' @param ... passed to [lmtest::waldtest()].
#'
#' @return Anova object
#' @export
#' @family Granger Causality
#'
#' @examples
#' data(wages, package = "lmtest")
#' diff_wages <- diff(wages)
#'
#' # Granger tests
#' lmtest::grangertest(diff_wages[, 'w'], diff_wages[, 'CPI'], order = 3)
#' ms_grangertest1(diff_wages[, 'w'], diff_wages[, 'CPI'], order = 3)
#' ms_grangertest2(diff_wages[, 'w'], diff_wages[, 'CPI'], order = 3)
#'
#' ms_condgrangertest(diff_wages[, 'w'], diff_wages[, 'CPI'], diff_wages[, 'u'], order = 3)

ms_condgrangertest <- function(x, y, z, order = 1, na.action = stats::na.omit, ...) {

  xnam <- deparse(substitute(x))
  ynam <- deparse(substitute(y))
  znam <- deparse(substitute(z))
  stopifnot((NCOL(x) == 1), (NCOL(y) == 1), (NCOL(z) == 1))

  ## compute lagged observations
  x1 <- c(rep(NA, order), as.vector(x))
  y1 <- c(rep(NA, order), as.vector(y))
  z1 <- c(rep(NA, order), as.vector(z))
  lagX <- stats::embed(x1, order + 1)
  lagY <- stats::embed(y1, order + 1)
  lagZ <- stats::embed(z1, order + 1)

  # Apply na.action
  numcolX <- ncol(lagX)
  numcolY <- ncol(lagY)
  numcolZ <- ncol(lagZ)
  lag_temp <- na.action(cbind(lagY, lagX, lagZ))

  y <- as.vector(lag_temp[,1])
  lagY <- as.matrix(lag_temp[,2:numcolY,drop=FALSE]) # skip y
  lagX <- as.matrix(lag_temp[,(numcolY + 2):(numcolY + numcolX),drop=FALSE]) # skip x
  lagZ <- as.matrix(lag_temp[,(numcolY + numcolX + 2):ncol(lag_temp),drop=FALSE]) # skip z

  ## fit full model
  fm <- stats::lm(y ~ lagY + lagX + lagZ)

  ## compare models with waldtest with lagX removed
  rval <- lmtest::waldtest(fm, 2, ...)

  ## adapt annotation
  model_1 <- paste0("Model 1: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order,
                    ") + Lags(", xnam, ", 1:", order, ") + Lags(", znam, ", 1:", order, ")")
  model_2 <- paste0("Model 2: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order,
                    ") + Lags(", znam, ", 1:", order, ")")
  attr(rval, "heading") <- paste0(
    "Conditional Granger causality test\n",
    model_1, "\n", model_2, "\n"
    )

  return(rval)
}


