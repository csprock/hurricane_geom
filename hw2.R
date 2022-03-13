library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(geosphere)
library(ggmap)


MILES_TO_METERS <- 1609.34

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

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")


#### now the fun starts 

ext_tracks_2 <- ext_tracks %>%
  select(-max_wind, -min_pressure, -rad_max_wind, -eye_diameter, -pressure_1, -pressure_2) %>%
  unite(date, c("year", "month", "day"), sep="-", remove=FALSE) %>%
  mutate(time=paste(hour,"00:00", sep=":"), storm_name=str_to_title(storm_name)) %>%
  unite(storm_id, c("storm_name", "year"), sep="-") %>%
  unite(date, c("date", "time"), sep=" ") %>%
  mutate(date = lubridate::as_datetime(date)) %>%
  pivot_longer(matches("radius_[0-9]{2}_[a-z]{2}"), names_pattern = "radius_([0-9]{2}_[a-z]{2})") %>%
  separate(name, sep="_", into=c("wind_speed", "temp")) %>%
  pivot_wider(id_cols=c("date", "storm_id", "wind_speed", "temp", "latitude", "longitude"), 
              values_from=value, names_from=temp) %>%
  mutate(wind_speed=as.numeric(wind_speed), longitude=-longitude) %>%
  filter(storm_id=="Katrina-2005")


sample_slice <- ext_tracks_2[46:48, ]

generate_quarter_circle <- function(p, r, n, start_deg=0) {
  increment <- 90 / n
  
  points <- matrix(nrow=n+1, ncol=2)
  
  for (i in 1:(n+1)) {
    b <- (i-1)*increment + start_deg
    points[i, c(1,2)] <- destPoint(
      p=p, 
      b=b ,
      d=r*MILES_TO_METERS)
  }
  return(points)
}



generate_radii <- function(p, radii) {
  
  start_degrees <- c(0, 90, 180, 270)
  
  sample_points <- generate_quarter_circle(p, radii[1], n=20, start_deg=start_degrees[1])
  for (i in 2:4) {
    temp <- generate_quarter_circle(p, radii[i], n=20, start_deg=start_degrees[i])
    sample_points <- rbind(sample_points, temp)
  }
  
  return(sample_points)
  
}


p <- c(-84,24.4)

sample_points <- generate_quarter_circle(p, 200, 20)

sample_points_1 <- data.frame(rbind(p, generate_radii(p, c(130, 90, 90, 130))))
colnames(sample_points_1) <- c("lon", "lat")
sample_points_2 <- data.frame(rbind(p,generate_radii(p, c(60,60,45,60))))
colnames(sample_points_2) <- c("lon", "lat")
sample_points_3 <- data.frame(rbind(p,generate_radii(p, c(35,30,30,25))))
colnames(sample_points_3) <- c("lon", "lat")


ggplot() + 
  geom_polygon(data=sample_points_1, aes(x=lon, y=lat))


my_poly <- polygonGrob(
  x=c(0.25,0.25,0.75), y=c(0.25,0.75,0.75),
  gp= gpar(col="black", fill="gray0", alpha=0.5)
)

grid.draw(my_poly)




temp <- data.frame(rbind(p, sample_points))
colnames(temp) <- c("lon", "lat")

base_map <- get_map(p, zoom=7, source="stamen", maptype="toner")

# base_map %>%
#   ggmap() +
#   geom_polygon(data=sample_points_1, aes(x=lon, y=lat), fill="yellow", alpha=0.5) + 
#   geom_polygon(data=sample_points_2, aes(x=lon, y=lat), fill="orange", alpha=0.5) + 
#   geom_polygon(data=sample_points_3, aes(x=lon, y=lat), fill="red", alpha=0.5)


GOOGLEMAPS_API_KEY = "AIzaSyCYj7_cl6Nlp5Sjn1SU13TpQTDey5iJ_Mw"
ggmap::register_google(GOOGLEMAPS_API_KEY)

#### the geom

draw_panel <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  
  radii <- c(
    data$ne,
    data$se,
    data$sw,
    data$nw
  )

  center_point <- c(data$x, data$y)

  radii_data <- data.frame(rbind(center_point, generate_radii(center_point, radii)))
  colnames(radii_data) <- c("x", "y")
  rownames(radii_data) <- NULL
  
 radii_coords <- coord$transform(radii_data, panel_scales)

  grid::polygonGrob(
    x = radii_data$x,
    y = radii_data$y,
    default.units="native",
    gp = grid::gpar(fill="black", col="black", alpha=0.5)
  )

  ggplot2:::ggname("geom_polygon", GeomPolygon$draw_panel(radii_data, panel_scales, coord))
}

GeomHurricane <- ggproto("GeomHurricane", GeomPolygon,
          required_aes = c("x","y", "ne", "se", "sw", "nw"),
          default_aes = aes(colour="black", alpha=0.5, fill="black"),
          draw_key = draw_key_polygon,
          draw_panel = draw_panel)

geom_hurricane <- function(mapping=NULL, data=NULL, stat="identity", position="identity", na.rm=FALSE,
                           show.legend=NA, inherit.aes=TRUE, ...) {
  
  layer(
    geom=GeomHurricane, mapping=mapping, data=data, stat=stat, position=position, show.legend=show.legend,
    inherit.aes = inherit.aes, params=list(na.rm=na.rm, ...)
  )
  
}

base_map %>%
  ggmap()

ggplot() + 
  geom_hurricane(data=sample_slice,
            aes(x=longitude, y=latitude, ne=ne, se=se, sw=sw, nw=nw, group=wind_speed))













