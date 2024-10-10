library(terra)
library(tidyverse)
library(stringr)


# make sure binary tif has same ext ---------------------------------------

# get the extent of all tif
path <- './Binary_result'
tif_files <- list.files(path, full.names = T)
df <- data.frame(matrix(ncol = 4, nrow = length(tif_files)))
colnames(df) <- c('xmin', 'xmax', 'ymin', 'ymax')
for (index in 1:length(tif_files)){
  rst_temp <- rast(tif_files[index])
  df[index, ] <- as.vector(ext(rst_temp)) 
}
my_xmin <- min(df[1])
my_xmax <- max(df[2])
my_ymin <- min(df[3])
my_ymax <- max(df[4])


# write out with same extent, zero with NA, 
# need crop with continental land for final result
for (index in 1:length(tif_files)){
  rst_file <- tif_files[index]
  rst_temp <- extend(rast(rst_file), c(my_xmin, my_xmax, my_ymin, my_ymax))
  rst_temp[][is.na(rst_temp[])] <- 0
  rst_path <- paste0('./Binary_same_ext/', basename(rst_file))
  writeRaster(rst_temp, rst_path)
}



# start running analysis --------------------------------------------------



# List of species
path <- './Binary_same_ext'
tif_files <- list.files(path)
sp_name <- unique(str_replace(tif_files, '_.*.tif', ''))
sp_name





data <- as_tibble(data.frame(Species=sp_name))

# Create data frame
period <- c('_Cu.tif', '_126.tif', '_585.tif', '_LGM.tif')

df <- data.frame(matrix(nrow = 5*4, ncol = 9))
names(df) <- c('Species', 'tif_path', 'shp_path', 'mc_path', 'period', 'Area (km2)',
               'Mean_long',	'Mean_lat',	'dem_mean (m)')


dem <- rast("elev/elev.tif")
plot(dem)

index_i <- 1
for (index in 1:nrow(data)){
  sp <- data$Species[index]
  
  for (index_j in seq_along(period)) {
    print(index_i)
    df[index_i, 1] <- sp
    p_temp <- period[index_j]
    df[index_i, 5] <- str_sub(p_temp, 2, -5)
    #  Binary results are stored under the Binary_result folder
    tif_path <- paste0('./Binary_same_ext/', sp, p_temp)
    df[index_i, 2] <- tif_path

    # 0-1
    rst <- rast(tif_path)
    # remove 0 as NA
    rst[][rst[] == 0 ] <- NA
    
    # Extract polygon with value 1
    pol_p <- as.polygons(rst)
    df[index_i, 6] <- expanse(pol_p, 'km')
    # Other results are written under the results folder
    shp_path <- paste0('result/', sp, str_sub(p_temp, 1, -5), '_pre.shp')
    df[index_i, 3] <- shp_path
    if (!file.exists(shp_path)){
      writeVector(pol_p, shp_path)
    }

    # Direction from the centroid
    mc <- centroids(pol_p)
    mc_path <- paste0('result/', sp, str_sub(p_temp, 1, -5), '_mc.shp')
    df[index_i, 4] <- mc_path
    if (!file.exists(mc_path)){
      writeVector(mc, mc_path)
    }

    # mean x, y
    xy_rst <- crds(rst)
    df[index_i, 7] <- mean(xy_rst[, 1])
    df[index_i, 8] <- mean(xy_rst[, 2])

    # mean dem
    dem_t <- mask(crop(dem, rst), rst)
    df[index_i, 9] <- mean(values(dem_t, na.rm=T))
    
    index_i <- index_i+1
  }
}


write_csv(df, 'Carya_summary.csv')





