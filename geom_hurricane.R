#' Load hurricane data
#' 
#' 
#' 
#'@importFrom readr read.fwf
#'
#'@param filename the file hurricane data location
#' 
#'@export
#'
load_hdata <- function(filename) {
  
  # From instructions provided
  
  ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                         4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
  ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                           "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                           paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                           "storm_type", "distance_to_land", "final")
  
  ext_tracks <- readr::read_fwf(filename, 
                         fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                         na = "-99")
  
  return(ext_tracks)
  
}

