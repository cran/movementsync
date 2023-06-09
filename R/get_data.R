# Functions to load data

#' Get sample meta-data recording object
#'
#' @param stem recording identifier.
#'
#' @return a `Recording` object.
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
get_sample_recording <- function(stem = "NIR_ABh_Puriya") {
  get_recording(stem, fps = 25, folder_in = "data",
                path = system.file(package = "movementsync"), filetype = 'csv')
}


#' Get a meta-data recording object
#'
#' @param stem recording identifier.
#' @param folder_in input folder relative to recording home (default is 'Original').
#' @param path recording home folder.
#' @param fps frames per second.
#' @param filetype type of file ('rda' as default), can be 'csv'.
#' @param verbose messages the specific data loaded (default is 'FALSE').
#'
#' @return a `Recording` object.
#' @export
#' @family data functions
#'
#' @examples
#' # Get the details of one recording
#' r <- get_recording("NIR_ABh_Puriya", fps=25)

get_recording <- function(stem, fps, folder_in = "data", path = system.file(package = "movementsync"), filetype = 'csv', verbose = FALSE) {
  
  # The default filetype is csv, but for internal data needs to be rda
  if(folder_in=='data' & path==system.file(package = "movementsync")){
    filetype <- 'rda'
  }
  
  if(verbose==TRUE){
    message("Loading ", paste0(path,'/',folder_in)) # Diagnostic
  }
  stopifnot(dir.exists(path))

  data_path <- file.path(path, folder_in)
  data_files <- list.files(data_path, pattern = paste0("^", stem, ".*\\.",filetype) )
  l <- list(data_home = path, data_path = data_path, data_files = data_files, stem = stem,
            fps = fps)
  class(l) <- "Recording"
  l
}


#' Get onsets selected files
#'
#' @param recording `Recording` object.
#' @param tactus optional name of the beat column to ensure it is turned into integer.
#' @param filetype type of file ('rda' as default), can be 'csv'.
#' @param verbose messages the specific data loaded (default is 'FALSE').
#'
#' @return list of data.frames
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' o <- get_onsets_selected_data(r)
get_onsets_selected_data <- function(recording, tactus = "Matra", filetype = 'rda', verbose = FALSE) {

  ### get the filetype from the recording
  file<-recording$data_files[1]
  file2 <- strsplit(basename(file), split="\\.")[[1]]
  filetype <- file2[-1]
  ###
  # Identify onset files
  is_onsets_selected_file <- grepl(
    paste0("^", recording$stem, ".*_Onsets_Selected_.*\\.",filetype),
    recording$data_files)
  onsets_selected_files <- file.path(recording$data_path, recording$data_files[is_onsets_selected_file])
  if(verbose==TRUE){
    message("Loading ", paste(basename(onsets_selected_files), collapse = ", "))
  }

  output_onsets_selected <- list()
  for (fil in onsets_selected_files) {
    if(filetype=='csv'){
      df <- utils::read.csv(fil)
    #  df <- utils::read.csv(data_file_name, colClasses = "numeric")   # for external data (TE May 2023)  
    #df <- load(fil)                                       # for internal data (TE May 2023)
    }
    if(filetype=='rda'){
      x <- load(fil)
      df <- get(x)
    }
    # specify colClasses?
    if (tactus %in% colnames(df)) {
      df[[tactus]] <- suppressWarnings(as.integer(df[[tactus]]))
    }
    output_onsets_selected[[basename(fil)]] <- df
  }
  #names(output_onsets_selected) <- sub(".*_Onsets_Selected_(.*)\\.rda", "\\1", names(output_onsets_selected))
  names(output_onsets_selected) <- sub(".*_Onsets_Selected_(.*)\\....", "\\1", names(output_onsets_selected))
  
  output_onsets_selected$recording <- recording

  class(output_onsets_selected) <- "OnsetsSelected"
  invisible(output_onsets_selected)
}


#' Get metre files
#'
#' @param recording `Recording` object.
#' @param filetype type of file ('rda' as default), can be 'csv'.
#' @param verbose messages the specific data loaded (default is 'FALSE').
#'
#' @return list of data.frames.
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' m <- get_metre_data(r)
get_metre_data <- function(recording, filetype = 'rda', verbose = FALSE) {

  ### get the filetype from the recording
  file <- recording$data_files[1]
  file2 <- strsplit(basename(file), split="\\.")[[1]]
  filetype <- file2[-1]
  ###
  
  
  # Identify metre files
  is_metre_file <- grepl(
    paste0("^", recording$stem, ".*_Metre(_|).*\\.",filetype),
    recording$data_files)
  metre_files <- file.path(recording$data_path, recording$data_files[is_metre_file])
  if(verbose==TRUE){
    message("Loading ", paste(basename(metre_files), collapse = ", "))
    }
  output_metre <- list()
  for (fil in metre_files) {
    if(filetype=='csv'){
      dfr <- utils::read.csv(fil)
    }
    #  df <- utils::read.csv(data_file_name, colClasses = "numeric")   # for external data (TE May 2023)  
    #dfr <- load(fil)                                       # for internal data (TE May 2023)
    if(filetype=='rda'){
      x <- load(fil) 
      dfr <- get(x)
    }
    if ('Beats' %in% colnames(dfr)) {
      dfr$Tempo_Hz <- c(1 / (diff(dfr$Time) / utils::head(dfr$Beats, -1)), NA)
    }
    output_metre[[basename(fil)]] <- dfr
  }
  #names(output_metre) <- sub(".*_Metre(_|)(.*)\\.rda", "\\2", names(output_metre))
  names(output_metre) <- sub(".*_Metre(_|)(.*)\\....", "\\2", names(output_metre))
  
  # Order on time
  min_time <- sapply(output_metre, function(x) min(x$Time, na.rm = TRUE))
  output_metre <- output_metre[order(min_time)]

  class(output_metre) <- "Metre"
  invisible(output_metre)
}


#' Get duration annotation data
#'
#' @param recording `Recording` object.
#' @param filetype type of file ('rda' as default), can be 'csv'.
#' @param verbose messages the specific data loaded (default is 'FALSE').
#'
#' @return list of data.frames.
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' df <- get_duration_annotation_data(r)
get_duration_annotation_data <- function(recording,  filetype = 'rda', verbose = FALSE) {

  ### get the filetype from the recording
  file <- recording$data_files[1]
  file2 <- strsplit(basename(file), split="\\.")[[1]]
  filetype <- file2[-1]
  ###
  
  # Identify duration files
  is_duration_file <- grepl(
    paste0("^", recording$stem, ".*_(Annotation|Annotation_Influence)\\.",filetype),
    recording$data_files)
  duration_files <- file.path(recording$data_path, recording$data_files[is_duration_file])
  if(verbose==TRUE){
    message("Loading ", paste(basename(duration_files), collapse = ", "))
  }

  output_list <- list()
  for (fil in duration_files) {
    if(filetype=='csv'){
      df <- utils::read.csv(fil, header = FALSE)
    }
      #df <- load(data_file_name)                                       # for internal data (TE May 2023)
    if(filetype=='rda'){
      x <- load(fil)
      df <- get(x)
    }
    df <- df[, colSums(is.na(df)) != nrow(df), drop=FALSE] # remove any all NA columns
    is_numeric_col <- sapply(df, function(x) class(x) == "numeric")
    is_character_col <- sapply(df, function(x) class(x) == "character")
    colnames(df)[is_numeric_col] <- c("In", "Out", "Duration")
    colnames(df)[is_character_col] <- c("Tier", "Comments")
    output_list[[basename(fil)]] <- df
  }

  output_dfr <- do.call(rbind.data.frame, c(output_list, make.row.names = FALSE))
  class(output_dfr) <- c("Duration", class(output_dfr))

  output_dfr
}


#' Get view from Pose video data
#'
#' Creates time reference and displacement from raw csv data for the view.
#'
#' @param recording `Recording` object.
#' @param vid video camera.
#' @param direct direction.
#' @param inst instrument.
#' @param save_output save the output?
#' @param out_folder output folder (tempdir if nothing is given).
#' @param filetype type of file ('rda' as default), can be 'csv'.
#' @param verbose messages the specific data loaded (default is 'FALSE').
#'
#' @return a `RawView` object.
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' v <- get_raw_view(r, "Central", "", "Sitar")
get_raw_view <- function(recording, vid, direct, inst,
                         out_folder = tempdir(), save_output = FALSE,  filetype = 'rda', verbose = FALSE) {

  ### get the filetype from the recording
  file <- recording$data_files[1]
  file2 <- strsplit(basename(file), split="\\.")[[1]]
  filetype <- file2[-1]
  ###
  
  fn_cpts <- c(recording$stem, vid, direct, 'Pose', inst)
  fn <- paste0(paste(fn_cpts[fn_cpts != ""], collapse = "_"), paste0(".",filetype))
  data_file_name <- file.path(recording$data_path, fn)
  if(verbose==TRUE){
    message("Loading ", data_file_name)
  }
  stopifnot(file.exists(data_file_name))

  if(filetype=='csv'){
    df <- utils::read.csv(data_file_name, colClasses = "numeric")   # for external data (TE May 2023)
  }
  if(filetype=='rda'){
    x <- load(data_file_name)
    df <- get(x)                                 # for internal data (TE May 2023)
  }  
  first_col <- "Frame"
  colnames(df)[1] <- first_col
  data_points <- unique(sapply(strsplit(colnames(df), "_"), function(x) x[1]))[-1]
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")
  z_colnames <- paste0(data_points, '_z')

  # Add a time column
  df <- cbind(df[1], Time = df[[1]] / recording$fps, df[-1])

  # Add a displacement column (make first lagged value same as second)
  dx <- as.data.frame(lapply(df[x_colnames], function(x) c(x[2] - x[1], diff(x))))
  dy <- as.data.frame(lapply(df[y_colnames], function(x) c(x[2] - x[1], diff(x))))
  dz <- as.data.frame(sapply(z_colnames, function(x) {
    if (x %in% colnames(df)) c(df[[x]][2] - df[[x]][1], diff(df[[x]])) else rep(0, nrow(df))
  }))

  disp <- sqrt(dx^2 + dy^2 + dz^2)
  colnames(disp) <- paste0(data_points, "_d")
  df <- cbind(df, disp)
  selected_cols <- c(first_col, "Time", rbind(x_colnames, y_colnames, z_colnames, colnames(disp)))
  df <- df[match(selected_cols, colnames(df), nomatch = 0)]

  if (save_output) {
    out_folder <- out_folder
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(recording$stem, "_", vid , "_", inst, '_RAW.rda'))
    utils::write.csv(df, out_file_name, row.names=FALSE)
  }

  l <- list(df = df, vid = vid, direct = direct,
       inst = inst, recording = recording)
  class(l) <- c("RawView", "View")

  invisible(l)
}


#' Creates time reference and displacement from raw csv optflow data
#'
#' Used to load OptFlow data.
#' @param recording `Recording` object.
#' @param vid camera.
#' @param direct direction.
#' @param inst instrument.
#' @param save_output save the output?
#' @param folder_out output folder relative to recording home  (default is 'tempdir()').
#' @param filetype type of file ('rda' as default), can be 'csv'.
#' @param verbose messages the specific data loaded (default is 'FALSE').
#'
#' @return an `OptFlowView` object.
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rov <- get_raw_optflow_view(r, "Central" ,"", "Sitar")
#' pov <- get_processed_view(rov)
#' fv1 <- apply_filter_sgolay(pov, c("Head"), n=19, p=4)
#' autoplot(fv1)
get_raw_optflow_view <- function(recording, vid, direct, inst,
                         folder_out = tempdir(), save_output = FALSE,  filetype = 'rda', verbose = FALSE) {
  ### get the filetype from the recording
  file <- recording$data_files[1]
  file2 <- strsplit(basename(file), split="\\.")[[1]]
  filetype <- file2[-1]
  ###
  
  fn_cpts <- c(recording$stem, 'OptFlow', vid, direct, inst)
#  fn <- paste0(paste(fn_cpts[fn_cpts != ""], collapse = "_"), ".rda")
  fn <- paste0(paste(fn_cpts[fn_cpts != ""], collapse = "_"), paste0(".",filetype))
  data_file_name <- file.path(recording$data_path, fn)
  if(verbose==TRUE){
    message("Loading ", data_file_name)
  }
  stopifnot(file.exists(data_file_name))

  if(filetype=='csv'){
    df <- utils::read.csv(data_file_name, colClasses = "numeric")   # for external data (TE May 2023)
  }
  #df <- load(data_file_name)                                       # for internal data (TE May 2023)
  if(filetype=='rda'){
    x <- load(data_file_name)
    df <- get(x)
  }
  colnames(df) <- c("Frame", "Time", "Head_x", "Head_y")

  # Add a displacement column
  dx <- c(df[["Head_x"]][2] - df[["Head_x"]][1], diff(df[["Head_x"]]))
  dy <- c(df[["Head_y"]][2] - df[["Head_y"]][1], diff(df[["Head_y"]]))
  disp <- sqrt(dx^2 + dy^2)
  df <- cbind(df, 'Head_d' = disp)

  if (save_output) {
    out_folder <- file.path(recording$data_home, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(recording$stem, "_", inst, '_RAW.csv'))
    utils::write.csv(df, out_file_name, row.names=FALSE)
  }

  l <- list(df = df, vid = "", direct = direct,
            inst = inst, recording = recording)
  class(l) <- c("OptFlowView", "RawView", "View")

  invisible(l)
}


#' Get Pose views from a recording
#'
#' @param recording `Recording` object.
#' @param filetype type of file ('rda' as default), can be 'csv'.
#'
#' @return named list of views
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' v_list <- get_raw_views(r)
get_raw_views <- function(recording, filetype = 'rda') {

  ### get the filetype from the recording
  file <- recording$data_files[1]
  file2 <- strsplit(basename(file), split="\\.")[[1]]
  filetype <- file2[-1]
  ###
  
  # Identify view files
  is_view_file <- grepl(
    paste0("^", recording$stem, ".*_Pose.*\\.",filetype),
    recording$data_files)
  view_files <- file.path(recording$data_path, recording$data_files[is_view_file])

  output_views <- list()
  for (fil in view_files) {
    tail_str <- substr(basename(fil), nchar(recording$stem) + 2, nchar(basename(fil)) - 4)
    tail_lead <- sub("^(.*)_Pose_.*", "\\1", tail_str)
    inst <- sub("^.*_Pose_(.*)", "\\1", tail_str)

    tail_cpts <- strsplit(tail_lead, "_")[[1]]
    if (length(tail_cpts) == 2) {
      vid <- tail_cpts[1]
      direct <- tail_cpts[2]
      id <- paste(vid, direct, inst, sep = "_")
    } else if (length(tail_cpts) == 1) {
      vid <- tail_cpts[1]
      direct <- ""
      id <- paste(vid, inst, sep = "_")
    } else stop("Unrecognised file format")

    output_views[[id]] <- get_raw_view(recording, vid, direct, inst,filetype = filetype)
  }

  output_views
}


#' Get processed view from Pose video data
#'
#' Normalises and interpolates missing data in the view.
#'
#' @param rv `RawView` object.
#' @param save_output save the output?
#' @param folder_out output folder relative to recording home (default is 'Normalized').
#' @param verbose messages the specific data loaded (default is 'FALSE').
#'
#' @return a `ProcessedView` object.
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
get_processed_view <- function(rv, folder_out = tempdir(),
                               save_output = FALSE, verbose = FALSE) {
  stopifnot("RawView" %in% class(rv))

  first_cols <- colnames(rv$df)[1:2]
  df <- rv$df
  data_points <- unique(sapply(strsplit(colnames(df), "_"), function(x) x[1]))[-(1:2)]
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")
  z_colnames <- paste0(data_points, "_z")

  # If there is a Head data_point we need to remove linear drift
  if ("Head" %in% data_points) {
    fit_head_x <- stats::lm(Head_x ~ Time, data = df, na.action = stats::na.exclude)
    fit_head_y <- stats::lm(Head_y ~ Time, data = df, na.action = stats::na.exclude)
    df[['Head_x']] <- as.numeric(stats::residuals(fit_head_x))
    df[['Head_y']] <- as.numeric(stats::residuals(fit_head_y))
    if(verbose==TRUE){
      message("Removed linear trend in Head data point")
    }
  }

  # Split dataframe into x- and y- columns and z- cplumns and determine
  # which dimension has the larger extent
  dfx <- df[x_colnames]
  dfy <- df[y_colnames]
  dfz <- df[colnames(df) %in% z_colnames]

  max_x <- max(dfx, na.rm = TRUE)
  min_x <- min(dfx, na.rm = TRUE)
  max_y <- max(dfy, na.rm = TRUE)
  min_y <- min(dfy, na.rm = TRUE)
  if (ncol(dfz) > 0) {
    max_z <- max(dfz, na.rm = TRUE)
    min_z <- min(dfz, na.rm = TRUE)
  } else {
    max_z <- -Inf
    min_z <- Inf
  }

  size <- max((max_x - min_x), (max_y - min_y), (max_z - min_z))

  # Normalise position data of all columns to the larger dimension (=0:1) and move midpoint to 0.5
  # Y-dimension inverted to make (0,0) bottom left of plots
  dfx_norm <- (dfx - min_x)/size
  dfx_norm <- dfx_norm + 0.5 - (max(dfx_norm, na.rm=TRUE) + min(dfx_norm, na.rm=TRUE))/2

  dfy_norm <- 1 - (dfy - min_y)/size
  dfy_norm <- dfy_norm + 0.5 - (max(dfy_norm, na.rm=TRUE) + min(dfy_norm, na.rm=TRUE))/2

  if (ncol(dfz) > 0) {
    dfz_norm <- (dfz - min_z)/size
    dfz_norm <- dfz_norm + 0.5 - (max(dfz_norm, na.rm=TRUE) + min(dfz_norm, na.rm=TRUE))/2
  } else {
    dfz_norm <- dfz
  }

  # Recombine dataframe
  X <- df[first_cols]
  df_norm <- cbind(X, dfx_norm, dfy_norm, dfz_norm)
  cn <- c(colnames(X), x_colnames, y_colnames, z_colnames)
  df_norm <- df_norm[match(cn, colnames(df_norm), nomatch = 0)]

  # Interpolate missing data
  df_norm <- replace(zoo::na.spline(df_norm), is.na(zoo::na.approx(df_norm, na.rm=FALSE)), NA)
  df_norm <- as.data.frame(df_norm)

  # Recompute a displacement column (make first lagged value same as second)
  dx <- as.data.frame(lapply(df_norm[x_colnames], function(x) c(x[2] - x[1], diff(x))))
  dy <- as.data.frame(lapply(df_norm[y_colnames], function(x) c(x[2] - x[1], diff(x))))
  dz <- as.data.frame(sapply(z_colnames, function(x) {
    if (x %in% colnames(df_norm)) c(df_norm[[x]][2] - df_norm[[x]][1], diff(df_norm[[x]])) else rep(0, nrow(df_norm))
  }))
  disp <- sqrt(dx^2 + dy^2 + dz^2)
  colnames(disp) <- paste0(data_points, "_d")
  df_norm <- cbind(df_norm, disp)
  selected_cols <- c(first_cols, rbind(x_colnames, y_colnames, z_colnames, colnames(disp)))
  df_norm <- df_norm[match(selected_cols, colnames(df_norm), nomatch = 0)]

  if (save_output) {
    out_folder <- file.path(rv$recording$data_home, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(rv$recording$stem, "_", rv$vid , "_", rv$inst, '_NORM.csv'))
    utils::write.csv(df_norm, out_file_name, row.names=FALSE)
  }
  l <- list(df = df_norm, vid = rv$vid, direct = rv$direct,
            inst = rv$inst, recording = rv$recording)
  class(l) <- c("ProcessedView", "View")

  invisible(l)
}


#' Apply a Savitzky-Golay filter to a view
#'
#' @param view `View` object.
#' @param data_points body parts e.g. 'Nose'.
#' @param n window size.
#' @param p poly order.
#' @param save_output save the output?
#' @param folder_out output folder relative to recording home (default is 'Filtered').
#'
#' @export
#' @return a `FilteredView` object.
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#'
#' set.seed(1)
#' fv1 <- apply_filter_sgolay(pv, c("Nose", "RWrist", "LWrist"), n = 19, p = 4)
#' fv2 <- apply_filter_sgolay(pv, c("Nose", "RWrist", "LWrist"), n = 41, p = 3)
#'
#' set.seed(1) # to reproduce with S3 filter object
#' fv3 <- apply_filter(pv, c("Nose", "RWrist", "LWrist"), signal::sgolay(4, 19))
apply_filter_sgolay <- function(view, data_points, n, p, folder_out = "Filtered",
                                save_output = FALSE) {
  apply_filter(view, data_points, signal::sgolay(p, n), param_str = paste(n, p, sep = "_"),
               folder_out, save_output)
}


#' Apply a filter to a View
#'
#' @param view `ProcessedView` object.
#' @param data_points body parts e.g. 'Nose'.
#' @param sig_filter S3 filter object from signals package.
#' @param param_str string of parameter values to add to output file if desired.
#' @param folder_out output folder relative to recording home (default is 'Filtered').
#' @param save_output save the output?
#' 
#' @return a filtered object.
#'
#' @export
apply_filter <- function(view, data_points, sig_filter, param_str = "", folder_out = "Filtered",
                         save_output = FALSE) {
  stopifnot("ProcessedView" %in% class(view))

  df_norm <- view$df
  first_cols <- colnames(df_norm[1:2])
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")
  z_colnames <- paste0(data_points, "_z")
  d_colnames <- paste0(data_points, "_d")
  selected_cols <- c(first_cols, rbind(x_colnames, y_colnames, z_colnames, d_colnames))
  df_selected <- df_norm[match(selected_cols, colnames(df_norm), nomatch = 0)]

    # Apply filter
  df_filt <- df_selected
  se_dfr <- get_start_end(df_filt[-c(1:2)])
  for (cn in colnames(df_filt)[-c(1:2)]) {
    start_pos <- se_dfr[cn, 'Start']
    end_pos <- se_dfr[cn, 'End']
    if (!is.na(start_pos) && !is.na(end_pos)) {
      df_filt[[cn]][start_pos:end_pos] <- signal::filter(sig_filter, df_selected[[cn]][start_pos:end_pos])
    }
  }

  # Save version
  if (save_output) {
    out_folder <- file.path(view$recording$data_home, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder,
      paste0(view$recording$stem, "_", view$vid, "_", view$inst, '_SEL_', param_str,'.csv')
    )
    utils::write.csv(df_filt, out_file_name, row.names=FALSE)
  }
  l <- list(df = df_filt, vid = view$vid, direct = view$direct,
            inst = view$inst, recording = view$recording)
  class(l) <- c("FilteredView", "View")

  invisible(l)
}


#' Get the data points held in a view
#'
#' @param obj `View` object.
#'
#' @return character vector of body parts.
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' get_data_points(rv)
get_data_points <- function(obj) {
  stopifnot("View" %in% class(obj))
  unique(sapply(strsplit(colnames(obj$df), "_"), function(x) x[1]))[-(1:2)]
}


#' Get joined view from multiple views from the same recording
#'
#' @param l named list of `View` objects.
#' @param save_output save the output?
#' @param folder_out output folder relative to recording home (default is 'Joined').
#'
#' @return JoinedView object
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' rv_list <- get_raw_views(r)
#' jv <- get_joined_view(rv_list)
#' plot(jv, columns = c("LEar_x_Central_Sitar", "LEar_x_Central_Tabla"), yax.flip=TRUE)
get_joined_view <- function(l, folder_out = "Joined", save_output = FALSE) {
  stopifnot(is.list(l), length(l) > 1)
  stopifnot(all(sapply(l, function(x) "View" %in% class(x))))

  # Rename the columns to reflect the view
  nl <- names(l)
  for (nm in names(l)) {
    colnames(l[[nm]]$df)[-(1:2)] <- paste0(colnames(l[[nm]]$df)[-(1:2)], paste0("_", nm))
  }

  # Inner joins for each element in list
  joined_df <- l[[1]]$df
  for (i in seq_along(nl)[-1]) {
    joined_df <- dplyr::inner_join(joined_df, l[[i]]$df, by = c("Frame", "Time"), copy = TRUE)
  }

  if (save_output) {
    out_folder <- file.path(l[[1]]$recording$data_home, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(l[[1]]$recording$stem, "_", l[[1]]$vid , "_", l[[1]]$inst, '_JOINED.csv'))
    utils::write.csv(joined_df, out_file_name, row.names=FALSE)
  }
  output_list <- list(df = joined_df, vid = "", direct = "", inst = "",
                      recording = l[[1]]$recording)
  class(output_list) <- c("JoinedView", class(l[[1]]))

  invisible(output_list)
}


#' Get filtered views
#'
#' @param r `Recording` object.
#' @param data_points vector of body parts e.g. 'Nose'.
#' @param n window size.
#' @param p poly order.
#' @param filetype type of file ('rda' as default), can be 'csv'.
#'
#' @return list of `FilteredView` objects.
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' fv_list <- get_filtered_views(r, "Nose", n = 41, p = 3)
#' plot(fv_list$Central_Tabla)
get_filtered_views <- function(r, data_points, n, p,filetype='rda') {
  stopifnot("Recording" == class(r))

  ### get the filetype from the recording
  file <- r$data_files[1]
  file2 <- strsplit(basename(file), split="\\.")[[1]]
  filetype <- file2[-1]
  ###
  
  
  rv_list <- get_raw_views(r,filetype = filetype)
  pv_list <- lapply(rv_list, get_processed_view)
  fv_list <- lapply(pv_list, apply_filter_sgolay, data_points = data_points, n = n, p = p)

  invisible(fv_list)
}


#' Get processed views
#'
#' @param r `Recording` object.
#' @param data_points vector of body parts e.g. 'Nose'.
#' @param filetype type of file ('rda' as default), can be 'csv'.
#'
#' @return list of `ProcessedView` objects.
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' pv_list <- get_processed_views(r)
#' plot(pv_list$Central_Tabla)
get_processed_views <- function(r, data_points,filetype='rda') {
  stopifnot("Recording" == class(r))

  ### get the filetype from the recording
  file <- r$data_files[1]
  file2 <- strsplit(basename(file), split="\\.")[[1]]
  filetype <- file2[-1]
  ###
  
  
  rv_list <- get_raw_views(r, filetype = filetype)
  pv_list <- lapply(rv_list, get_processed_view)

  invisible(pv_list)
}


#' Get Feature Data
#'
#' Output from new analysis process that generates data at the same sample
#' rate as the video data. The user is responsible for ensuring that this
#' data is continuous before using this function.
#'
#' @param recording `Recording` object.
#' @param vid camera.
#' @param direct direction.
#' @param inst instrument.
#' @param save_output save the output?
#' @param folder_out output folder relative to recording home (default is 'tempdir()').
#' @param interpolate_data should the data be interpolated? (default is FALSE).
#' @param filetype type of file ('rda' as default), can be 'csv'.
#' @param verbose messages the specific data loaded (default is 'FALSE').
#'
#' @return a `FilteredView` object.
#' @export
#' @family data functions
#'
#' @examples
#' r <- get_sample_recording()
#' fd <- get_feature_data(r, "Central" ,"", "Sitar")
#' fv_list <- get_filtered_views(r, 'LEar', n = 41, p =3)
#' fv_list$Feature <- fd
#' jv <- get_joined_view(fv_list)
#' get_data_points(jv)
#' autoplot(jv)
get_feature_data <- function(recording, vid, direct, inst, interpolate_data = FALSE,
                                 folder_out = tempdir(), save_output = FALSE, filetype = 'rda', verbose = FALSE) {
  
  ### get the filetype from the recording
  file <- recording$data_files[1]
  file2 <- strsplit(basename(file), split="\\.")[[1]]
  filetype <- file2[-1]
  ###
  
  fn_cpts <- c(recording$stem, vid, direct, 'Feature', inst)
  fn <- paste0(paste(fn_cpts[fn_cpts != ""], collapse = "_"), paste0(".",filetype))
  data_file_name <- file.path(recording$data_path, fn)
  if(verbose==TRUE){
    message("Loading ", data_file_name)
  }
  stopifnot(file.exists(data_file_name))

  if(filetype=='csv'){
    df <- utils::read.csv(data_file_name, colClasses = "numeric")   # for external data (TE May 2023)  
  }
  #df <- load(data_file_name)                                       # for internal data (TE May 2023)
  if(filetype=='rda'){
    x <- load(data_file_name)
    df <- get(x)
  }
  first_col <- "Frame"
  colnames(df)[1] <- first_col

  # Add a time column
  df <- cbind(df[1], Time = df[[1]] / recording$fps, df[-1])

  # Interpolate missing data
  if (interpolate_data) {
    df <- replace(zoo::na.spline(df), is.na(zoo::na.approx(df, na.rm=FALSE)), NA)
    df <- as.data.frame(df)
  }

  if (save_output) {
    out_folder <- file.path(recording$data_home, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(recording$stem, "_", inst, '_FEATURE.csv'))
    utils::write.csv(df, out_file_name, row.names=FALSE)
  }

  l <- list(df = df, vid = vid, direct = direct,
            inst = inst, recording = recording)
  class(l) <- c("FilteredView", "ProcessedView", "RawView", "View")

  invisible(l)
}


# Find the start and end row of each view data column for filtering
get_start_end <- function(dfr) {
  is_not_na_dfr <- !is.na(dfr)
  start_pos <- apply(is_not_na_dfr, 2, which.max)
  end_pos <- apply(is_not_na_dfr, 2, function(x) nrow(dfr) + 1 - which.max(rev(x)))

  is_col_all_na <- colSums(is.na(dfr)) == nrow(dfr)
  start_pos[is_col_all_na] <- NA
  end_pos[is_col_all_na] <- NA

  data.frame(Start = start_pos, End = end_pos)
}
