% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wavelet.R
\name{plot_average_coherency}
\alias{plot_average_coherency}
\title{Plot average coherency of a coherency object}
\usage{
plot_average_coherency(obj, view, ...)
}
\arguments{
\item{obj}{analyze.coherency object.}

\item{view}{View object.}

\item{...}{passed to \code{\link[WaveletComp:wc.avg]{WaveletComp::wc.avg()}}.}
}
\value{
a ggplot object.
}
\description{
Plot average coherency of a coherency object
}
\examples{
r <- get_sample_recording()
rv <- get_raw_view(r, "Central", "", "Sitar")
pv <- get_processed_view(rv)
co <- analyze_coherency(pv, columns = c("Nose_x", "Nose_y"))
plot_average_coherency(co, pv)
}
\seealso{
Other wavelet functions: 
\code{\link{analyze_coherency}()},
\code{\link{analyze_wavelet}()},
\code{\link{get_local_max_average_power}()},
\code{\link{plot_average_power}()},
\code{\link{plot_cross_spectrum}()},
\code{\link{plot_cwt_energy}()},
\code{\link{plot_phase_difference}()},
\code{\link{plot_power_spectrum}()},
\code{\link{plot_roll_resultant_length}()},
\code{\link{plot_sel_phases}()},
\code{\link{plot_wt_energy}()}
}
\concept{wavelet functions}
