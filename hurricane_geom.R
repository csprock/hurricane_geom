#' Generate a single quadrant
#' 
#' This function generates the coordinates of a single quandrant 
#' of the hurricane wind figure. The function sweeps out a 90 degree
#' quarter circle about a centeral point starting with a user-specified
#' start degree. 
#' 
#' Under the hood this function calls geosphere::destPoint.
#' 
#' @param p vector of length 2 containing longitude and latitude
#' @param r radius in miles
#' @param start_deg degree of the start 
#' @param scale_radii multiple to shrink or expand radii
#' 
#' @return a 2-column matrix coordinate of points (longitude/latitude)
#' 
#' @example 
#' generate_quarter_circle(c(-84, 25), 1000, 180)
generate_quarter_circle <- function(p, r, start_deg=0, scale_radii=1){
  
  points <- cbind(start_deg, seq(start_deg, start_deg + 90, 1))
  return(destPoint(p=p, b=points, d=r*MILES_TO_METERS*scale_radii))
  
}


#' Generate all the quandrants.
#' 
#' Generates each quandrant separately by calling
#' generate_quarter_circle on the radii of each quandrant starting 
#' with the NE and moving clockwise ending with the NW. 
#' 
#' @param p vector of length 2 containing longitude and latitude
#' @param radii vector of length 4 containing the radius of each quandrant
#' in clockwise order (ne, se, sw, nw)
#' @param scale_radii multiple to shrink or expand radii, default is 1
#' 
#' @return a tibble containing the coordinates of the quandrants (lon/lat)
#' 
#' @example
#' generate_radii(c(-84, 25), c(30, 60, 30, 90))
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


#' Creates a single windspeed grob
#' 
#' Creates a single windspeed grob with all 4 quandrants by for 
#' a single windspeed. This function is called by draw_panel
#' on all three windspeeds to create the composite grob. 
#' 
#' @param data
#' @param panel_scales
#' @param coord
#' 
#' @seealso generate_radii
#' @seealso draw_panel
#' 
#' @return a polygon grob representing the windspeed figure
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

#' Creates hurricane grob
#'
#' Is a wrapper around \code{create_grob} which creates a grob tree
#' for each row in the passed dataframe representing each windspeed
#' level.
#' 
#' @param data 3-row dataframe, each row represents a windspeed level
#' @param panel_scales object containing information about the draw scale
#' @param coord object containing information about coordinates
#' 
#' @return a grob tree
draw_panel <- function(data, panel_scales, coord) {

  gTree(children=gList(
    create_grob(data[1,], panel_scales, coord),
    create_grob(data[2,], panel_scales, coord),
    create_grob(data[3,], panel_scales, coord)
  ))
  
}

#' 
#' @param required_aes a character vector of required aesthetics
#' \enumerate{
#'  \item x, y, longitude and latitude representing the center of the hurricane 
#'  \item ne, se, sw, nw. The radius of the windspeed in miles for the northeast, southeast, southwest, and northwest directions, respectively. 
#' }
#' @param default_aes default aesthetics, which include
#' \enumerate{
#'  \item color border color
#'  \item alpha transparency
#'  \item fill fill color
#'  \item scale_radii factor by which to scale the radii of windspeed. This controls size of the hurricane windspeed radii relative to the base map. 
#' }
#' @param draw_key the function that draws the legend key, defaults to the polygon glyph key
#' @param draw_panel function that generates the grobs that are added to the figure
GeomHurricane <- ggproto("GeomHurricane", GeomPolygon,
          required_aes = c("x","y", "ne", "se", "sw", "nw"),
          default_aes = aes(color="yellow", alpha=0.75, fill="yellow", scale_radii=1),
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
