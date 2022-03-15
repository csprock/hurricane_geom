library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(geosphere)
library(ggmap)
library(grid)

### define parameters ###
FILE_LOCATION <- "data/ebtrk_atlc_1988_2015.txt"
PROCESSED_DATA_LOCATION <- "katrina_sample.csv"
PICTURE_LOCATION <- "katrina_picture.png"
GOOGLEMAPS_API_KEY="AIzaSyCYj7_cl6Nlp5Sjn1SU13TpQTDey5iJ_Mw"
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

#### geometry helper functions ####

generate_quarter_circle <- function(p, r, start_deg=0, scale_radii=1){
  
  points <- cbind(start_deg, seq(start_deg, start_deg + 90, 1))
  return(destPoint(p=p, b=points, d=r*MILES_TO_METERS*scale_radii))
  
}

generate_radii <- function(p, radii, scale_radii=1) {
  
  data <- do.call(
    rbind, 
    list(
      p, generate_quarter_circle(p, radii[1], start_deg=0, scale_radii),
      p, generate_quarter_circle(p, radii[2], start_deg=90, scale_radii),
      p, generate_quarter_circle(p, radii[3], start_deg=180, scale_radii),
      p, generate_quarter_circle(p, radii[4], start_deg=270, scale_radii)
    )
  )
  return(as_tibble(data))
}

#### the geom ####

create_grob <- function(data, panel_scales, coord){
  
  radii <- c(data$ne, data$se, data$sw, data$nw)
  center_point <- c(data$x, data$y)
  scale_radii <- data$scale_radii[1]

  radii_data <- generate_radii(center_point, radii, scale_radii) %>%
    rename(x=lon, y=lat) %>%
    coord$transform(panel_scales)
  
  grid::polygonGrob(
    x = radii_data$x,
    y = radii_data$y,
    gp = grid::gpar(fill=data$fill, color=data$color, alpha=data$alpha[1]))
}

draw_panel <- function(data, panel_scales, coord) {

  gTree(children=gList(
    create_grob(data[1,], panel_scales, coord),
    create_grob(data[2,], panel_scales, coord),
    create_grob(data[3,], panel_scales, coord)
  ))
  
  
}

GeomHurricane <- ggproto("GeomHurricane", GeomPolygon,
          required_aes = c("x","y", "ne", "se", "sw", "nw"),
          default_aes = aes(color="yellow", alpha=0.75, fill="yellow", size=0.5, scale_radii=1),
          draw_key = draw_key_polygon,
          draw_panel = draw_panel)

geom_hurricane <- function(
  mapping=NULL, 
  data=NULL, 
  stat="identity", 
  position="identity", 
  na.rm=FALSE,
  show.legend=NA, 
  inherit.aes=TRUE, ...) {
  layer(
    geom=GeomHurricane, 
    mapping=mapping, 
    data=data, 
    stat=stat, 
    position=position, 
    show.legend=show.legend,
    inherit.aes = inherit.aes, 
    params=list(na.rm=na.rm, ...)
  )
}

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