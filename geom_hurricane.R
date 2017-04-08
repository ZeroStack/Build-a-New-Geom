#' Load hurricane data
#' 
#' @importFrom readr read.fwf
#'
#' @param filename a character vector of filename of hurricane data file to read
#' 
#' @example 
#' \dontrun{
#'   load_hdata("data/ebtrk_atlc_1988_2015.txt")
#' }

#' @export
#' 
#'
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

#' Tidy hurricane data that conforms to assignment criteria
#' \donotrun{tidy_hdata} takes the raw hurricane data and cleans it to a more usable format for the assignment
#' 
#' @importFrom stringr str_c str_to_title
#' @importFrom dplyr mutate_ select_
#' @importFrom tidyr gather spread
#' 
#' @param data data to tidy
#' @export
tidy_hdata <- function(data) {
  data %>% 
    # Configure the storm_id and date 
    dplyr::mutate_(storm_id = ~stringr::str_c(stringr::str_to_title(storm_name), year, sep = '-'),
                   date = ~stringr::str_c(year, '-', month, '-', day, ' ', hour, ':', '00', ':', '00')
    ) %>% 
    # Select only the relevant columns
    dplyr::select_(.dots = c('storm_id', 'date', 'longitude', 'latitude', 
                            'radius_34_ne', 'radius_34_se', 'radius_34_sw', 'radius_34_nw',
                            'radius_50_ne', 'radius_50_se', 'radius_50_sw', 'radius_50_nw',
                            'radius_64_ne', 'radius_64_se', 'radius_64_sw', 'radius_64_nw')
    ) %>%
    
    #There is a better way to do this part, this is the wide to long transfmration
    tidyr::gather(variable, value, -storm_id, -date,-latitude, -longitude, -storm_id, -date) %>% mutate_(wind_speed = ~str_extract(variable, "(34|50|64)"),
              variable = ~str_extract(variable, "(ne|nw|se|sw)")) %>% tidyr::spread(variable, value) %>% select_(.dots = c('storm_id', 'date', 'latitude', 'longitude', 'wind_speed', 'ne', 'nw', 'se', 'sw'))
  
  
}

#' Filter the data by hurricane and time
#' 
#' @importFrom dplyr filter_
#' 
#' @param data input data to filter
#' @hurricane id to filter the data by
#' @observation time to filter the data by
#' 
#' @export
filter_hdata <- function(data, hurricane, observation) {
  data <- filter_(data, ~storm_id == hurricane & date == observation)
  
}

# List relevant packages
packages <- c('readr', 'dplyr', 'stringr', 'tidyr', 'ggmap', 'geosphere')

# Load packages
lapply(packages, require, character.only = TRUE)


# Read data into R, tidy it and filter it for Ike-2008
data <- load_hdata('data/ebtrk_atlc_1988_2015.txt') %>% 
  tidy_hdata() %>% 
  filter_hdata(hurricane = 'Ike-2008', observation = '2008-09-12 00:00:00')


geom_hurricane <- ggproto("geom_hurricane", Geom,
                          required_aes = c("x", "y"
                                           #,
                                           #"r_ne", "r_se", "r_nw", "r_sw",
                                           #"fill", "color"
                          ),
                          default_aes = aes(shape = 1),
                          draw_key = draw_key_point,
                          draw_panel = function(data, panel_scales, coord) {
                            ## Transform the data first
                            coords <- coord$transform(data, panel_scales)
                            
                            ## Let's print out the structure of the 'coords' object
                            str(coords)
                            
                            ## Construct a grid grob
                            circleGrob(
                              x = coords$x,
                              y = coords$y
                              #,
                              #pch = coords$shape
                            )
                            
                          }

)

geom_mypoint <- function(mapping = NULL, data = NULL, stat = 'identity',
                         position = 'identity', na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = geom_hurricane, mapping = mapping,
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
    
  )
}

ggplot(data = data, aes(x = longitude, y = latitude)) + geom_mypoint()


map <- get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_point(data = data, aes(x = longitude, y = latitude),
               size = 20, shape = 1,  color = "#ff0000")

  # geom_hurricane(data = storm_observation,
  #                aes(x = longitude, y = latitude, 
  #                    r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
  #                    fill = wind_speed, color = wind_speed)) + 
  # scale_color_manual(name = "Wind speed (kts)", 
  #                    values = c("red", "orange", "yellow")) + 
  # scale_fill_manual(name = "Wind speed (kts)", 
  #                   values = c("red", "orange", "yellow"))
  # 
