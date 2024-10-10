library(terra)
library(tidyverse)

##Read Data
data <- read_csv('Carya_summary.csv')


#  Merge 3 Period# Extract 3 Periods data
data_2 <- data%>%
  select(c(1, 2, 5))%>%
  filter(period == 'Current' | period == 'LGM' |  period == 'SSP126' | period == 'SSP585')

# Specific time period
F_path <- data_2[data_2$period == 'Current', ]$tif_path
rst_all <- 0
for (index in seq_along(F_path)){
  print(index)
  rst_p <- F_path[index]
  
  rst_temp <- rast(rst_p)
  temp_values <- values(rst_temp)
  temp_values[is.na(temp_values)] <- 0
  values(rst_temp) <- temp_values
  
  rst_all <- rst_all+rst_temp
}
#Mask
plot(rst_all)

writeRaster(rst_all, 'Carya_Current_richness.tif')

#Specific time period
F_path <- data_2[data_2$period == 'LGM', ]$tif_path
rst_all <- 0
for (index in seq_along(F_path)){
  print(index)
  rst_p <- F_path[index]
  
  rst_temp <- rast(rst_p)
  temp_values <- values(rst_temp)
  temp_values[is.na(temp_values)] <- 0
  values(rst_temp) <- temp_values
  
  rst_all <- rst_all+rst_temp
}

plot(rst_all)

writeRaster(rst_all, 'Carya_LGM_richness.tif')

# Specific time period
F_path <- data_2[data_2$period == 'SSP126', ]$tif_path
rst_all <- 0
for (index in seq_along(F_path)){
  print(index)
  rst_p <- F_path[index]
  
  rst_temp <- rast(rst_p)
  temp_values <- values(rst_temp)
  temp_values[is.na(temp_values)] <- 0
  values(rst_temp) <- temp_values
  
  rst_all <- rst_all+rst_temp
}

plot(rst_all)

writeRaster(rst_all, 'Carya_SSP126_richness.tif')

# Specific time period
F_path <- data_2[data_2$period == 'SSP585', ]$tif_path
rst_all <- 0
for (index in seq_along(F_path)){
  print(index)
  rst_p <- F_path[index]
  
  rst_temp <- rast(rst_p)
  temp_values <- values(rst_temp)
  temp_values[is.na(temp_values)] <- 0
  values(rst_temp) <- temp_values
  
  rst_all <- rst_all+rst_temp
}

plot(rst_all)

writeRaster(rst_all, 'Carya_SSP585_richness.tif')


#  Period Subtraction -------------------------------------------------------------------
rst1 <- rast('Carya_LGM_richness.tif')
rst2 <- rast('Carya_Current_richness.tif')
crs(rst1) <- crs(rst2)
plot(rst2)
rst3 <- rast('Carya_SSP126_richness.tif')
rst4 <- rast('Carya_SSP585_richness.tif')


writeRaster(rst2-rst1, 'Carya_LGM-Current_richness.tif')
writeRaster(rst3-rst2, 'Carya_Current-SSP126_richness.tif')
writeRaster(rst4-rst2, 'Carya_Current-SSP585_richness.tif')


