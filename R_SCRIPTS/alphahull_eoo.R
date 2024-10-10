library(alphahull)
library(terra)
library(rangeBuilder)
library(raster)
library(sf)

point_2_alphahull_line <-
  function(csv_path, alpha = 2.5,
           long = 'Longitude',
           lat = 'Latitude',
           crs = 'EPSG:4326',
           point_shp = 'test_point.shp',
           hull_shp = 'test_hull.shp') {

  data <- read.csv(csv_path, encoding = 'UTF-8')
  
  point <- vect(data, geom=c(long, lat), crs=crs)

  writeVector(point, point_shp, overwrite=TRUE)

  # alphahull 
  ahull.obj <- ashape(data[, c(long, lat)], alpha = alpha)
  plot(ahull.obj)
  data_hull <- as.data.frame(ahull.obj$alpha)
  hull_point <- vect(data_hull, geom=c('c1', 'c2'), crs=crs)
  plot(hull_point)
  b <- as.lines(hull_point)
  plot(b)
  hull_polygon <- as.polygons(as.lines(hull_point))
  plot(hull_polygon)
  writeVector(hull_polygon, hull_shp, overwrite=TRUE)

  plot(point)
  plot(hull_polygon, add=TRUE)
}


point_2_alphahull_curve <-
  function(csv_path,
           long = 'Longitude',
           lat = 'Latitude',
           crs = 'EPSG:4326',
           point_shp = 'test_point.shp',
           hull_shp = 'test_hull.shp') {
    data <- read.csv(csv_path, encoding = 'UTF-8')
    # write vector
    point <- vect(data, geom = c(long, lat), crs = crs)
    writeVector(point, point_shp, overwrite = TRUE)

    # alphahull analysis
    range <- getDynamicAlphaHull(data,
                                 coordHeaders = c(long, lat),
                                 clipToCoast = 'no',
                                 fraction = 1,
                                 partCount = 1,
                                 buff = 200000)

    plot(range[[1]], col = transparentColor('dark green', 0.5), border = NA)
    points(data[, c(long, lat)], cex = 0.5, pch = 3)

    #  Export Results
    st_write(range[[1]], hull_shp, overwrite = TRUE)
  
  }


# main --------------------------------------------------------------------
csv_path <- "hunanensis.csv"
point_2_alphahull_curve(csv_path, 'Longitude', 'Latitude',
                        hull_shp = 'hunanensis.shp')



