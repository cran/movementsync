#' NIR_ABh_Puriya_Onsets_Selected_VilambitTeental
#'
#' A subset of data from NIR_ABh_Puriya containing
#' information about selected onsets for Vilambit 
#' Teental section (sitar and tabla).
#' The data comes from a collection of audiovisual recordings of 
#' North Indian (Hindustani) raga performances which are
#' part of IEMP North Indian Raga collection, collected and
#' curated by Martin Clayton, Laura Leante, and Simone Tarsitani. 
#'
#' @format ## `rda`
#' A data frame with 2,275 rows and 20 columns:
#' \describe{
#'   \item{Session}{Session name}
#'   \item{Inst.Name}{Instrument Name}
#'   \item{Tala}{Tala name}
#'   \item{Label}{Label for beat (1|1)}
#'   \item{Matra}{Matra number}
#'   \item{Half.beat}{logical On or Off}
#'   \item{Half}{integer (1) for logical on or Off}
#'   \item{Misc.1}{Descriptor e.g. 'Gat'}
#'   \item{Misc.2}{Another descriptor, usually missing}
#'   \item{Cadence}{Descriptor}
#'   \item{Tabla.solo}{Descriptor where N is 'No'}
#'   \item{Inst}{Onset time in seconds}
#'   \item{Tabla}{Onset time in seconds of tabla}
#'   \item{Inst.Density}{Calculated density of onsets (no/s)}
#'   \item{Tabla.Density}{Calculated density of onsets (no/s)}
#'   \item{Inst.Peak}{Peak of the onset (onset strength)}
#'   \item{Tabla.Peak}{Peak of the onset (onset strength)}
#'   \item{Inst.Player}{Name of the performer (sitar)}
#'   \item{Tabla.Player}{Name of the performer (tabla)}
#'   \item{Chunk}{Chunk name}
#'   ...
#' }
#' @usage data(NIR_ABh_Puriya_Onsets_Selected_VilambitTeental)
#' @source \url{https://osf.io/xcefp}
"NIR_ABh_Puriya_Onsets_Selected_VilambitTeental"
