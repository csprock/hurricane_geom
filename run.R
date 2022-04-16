library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(geosphere)
library(ggmap)
library(grid)

source("~/hurricane_geom.R")

### define parameters ###
FILE_LOCATION <- "data/ebtrk_atlc_1988_2015.txt"
PROCESSED_DATA_LOCATION <- "katrina_sample.csv"
PICTURE_LOCATION <- "katrina_picture.png"
GOOGLEMAPS_API_KEY=""
MILES_TO_METERS <- 1609.34

ggmap::register_google(GOOGLEMAPS_API_KEY)

### load data ####

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

ext_tracks <- readr::read_fwf(FILE_LOCATION, 
                              fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                              na = "-99")


#### transform data #### 

processed_data <- ext_tracks %>%
  dplyr::select(-max_wind, -min_pressure, -rad_max_wind, -eye_diameter, -pressure_1, -pressure_2) %>%
  tidyr::unite(date, c("year", "month", "day"), sep="-", remove=FALSE) %>%
  dplyr::mutate(time=paste(hour,"00:00", sep=":"), storm_name=str_to_title(storm_name)) %>%
  tidyr::unite(storm_id, c("storm_name", "year"), sep="-") %>%
  tidyr::unite(date, c("date", "time"), sep=" ") %>%
  dplyr::mutate(date = lubridate::as_datetime(date)) %>%
  tidyr::pivot_longer(matches("radius_[0-9]{2}_[a-z]{2}"), names_pattern = "radius_([0-9]{2}_[a-z]{2})") %>%
  tidyr::separate(name, sep="_", into=c("wind_speed", "temp")) %>%
  tidyr::pivot_wider(
    id_cols=c("date", "storm_id", "wind_speed", "temp", "latitude", "longitude"), 
    values_from=value, names_from=temp
  ) %>%
  dplyr::mutate(wind_speed=as.numeric(wind_speed), longitude=-longitude, wind_speed=as.factor(wind_speed)) %>%
  dplyr::filter(storm_id=="Katrina-2005")


katrina_sample <- processed_data %>% 
  dplyr::filter(storm_id=="Katrina-2005") %>%
  dplyr::slice(70:72)

print(katrina_sample)

readr::write_csv(katrina_sample, PROCESSED_DATA_LOCATION)


#### create the image ####

base_map <- get_map(p, zoom=6, source="stamen", maptype="toner")

katrina_picture <- base_map %>%
  ggmap()  + geom_hurricane(
    data=katrina_sample,
    alpha=0.5,
    aes(
      x=longitude, 
      y=latitude, 
      ne=ne, 
      se=se, 
      sw=sw, 
      nw=nw, 
      fill=wind_speed, 
      color=wind_speed, 
      scale_radii=0.9)
  ) + 
  scale_fill_manual(values=c("red", "orange", "yellow"), name="Wind speed (kts)") + 
  scale_color_manual(values=c("red", "orange", "yellow"), name="Wind speed (kts)")


ggsave(PICTURE_LOCATION, katrina_picture)