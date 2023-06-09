% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistical.R
\name{ave_cross_power_spliceview}
\alias{ave_cross_power_spliceview}
\title{Get the average cross power on each segment in a SplicedView}
\usage{
ave_cross_power_spliceview(
  sv,
  columns,
  colour = "blue",
  segments = NULL,
  show_plot = FALSE,
  ...
)
}
\arguments{
\item{sv}{\code{SplicedView} object}

\item{columns}{column names in the data of each \code{SplicedView} object.}

\item{colour}{name of colour on plots (default is 'blue').}

\item{segments}{indices of segments to plot e.g. 1:10 (default plots up to first 10).}

\item{show_plot}{show a plot (default is FALSE).}

\item{...}{passed to \code{\link[=analyze_coherency]{analyze_coherency()}}.}
}
\value{
data.frame with columns containing Average Cross Power for each segment.
}
\description{
Get the average cross power on each segment in a SplicedView
}
\examples{
r <- get_sample_recording()
d1 <- get_duration_annotation_data(r)
# only one relevant section for sample data
splicing_smile_df <- splice_time(d1, tier ='INTERACTION',
  comments = 'Mutual look and smile')

fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
jv <- get_joined_view(fv_list)
sv_duration_smile <- get_spliced_view(jv, splicing_df = splicing_smile_df)
ave_cross_power_smile <- ave_cross_power_spliceview(
  sv_duration_smile, columns = c("Nose_x_Central_Sitar", "Nose_y_Central_Sitar"), show_plot = TRUE)
head(ave_cross_power_smile)
}
\seealso{
Other statistical and analysis functions: 
\code{\link{apply_column_spliceview}()},
\code{\link{apply_segment_spliceview}()},
\code{\link{ave_cross_power_over_splices}()},
\code{\link{ave_power_over_splices}()},
\code{\link{ave_power_spliceview}()},
\code{\link{calculate_ave_cross_power1}()},
\code{\link{calculate_ave_power1}()},
\code{\link{compare_ave_cross_power1}()},
\code{\link{compare_ave_power1}()},
\code{\link{compare_avg_cross_power2}()},
\code{\link{compare_avg_power2}()},
\code{\link{difference_onsets}()},
\code{\link{pull_segment_spliceview}()},
\code{\link{sample_gap_splice}()},
\code{\link{sample_offset_splice}()},
\code{\link{summary_onsets}()},
\code{\link{visualise_sample_splices}()}
}
\concept{statistical and analysis functions}
