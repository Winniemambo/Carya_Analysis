library(terra)
library(tidyverse)
library(stringr)
library(NISTunits)


#--------------------------------------------------------------------
point2deg <- function(point1, point2){
  p1 <- vect(point1, crs='EPSG:4326')
  p1_xy <- as.matrix(crds(p1))
  p2 <- vect(point2, crs='EPSG:4326')
  p2_xy <- as.matrix(crds(p2))
  
  # Write the line file
  lines <- as.lines(p1+p2)
  
  part_1 <- str_split(point1, '_', simplify = T)
  part_2 <- str_split(point2, '_', simplify = T)
  shp_path <- paste0(part_1[1], '_', part_1[length(part_1)-1], '-',
                     part_2[length(part_2)-1], '_line.shp')
  writeVector(lines, shp_path, overwrite=TRUE)
  
  
  del_x <- p2_xy[1]-p1_xy[1]
  del_y <- p2_xy[2]-p1_xy[2]
  
  # 1
  if (del_x>0 & del_y>0){
    deg <- 90 - NISTradianTOdeg(atan(abs(del_y/del_x)))
  # 2
  } else if (del_x>0 & del_y<0){ 
    deg <- 90 + NISTradianTOdeg(atan(abs(del_y/del_x)))
  # 3
  } else if (del_x<0 & del_y<0) {
    deg <- 270 - NISTradianTOdeg(atan(abs(del_y/del_x)))
  } else {
    deg <- 270 + NISTradianTOdeg(atan(abs(del_y/del_x)))
  }
  result <- NULL
  result[1] <- deg
  result[2] <- terra::distance(p1, p2)
  result[3] <- NISTdegTOradian(deg)
  result[4] <- shp_path
  return(result)
}



data <- read_csv('Carya_summary.csv')

data2 <- data%>%
  select(Species, mc_path, period)%>%
  filter(period == 'Current' | period == 'LGM' | period == 'SSP126' | period == 'SSP585')


df <- data.frame(matrix(nrow = 5*3, ncol = 6))
names(df) <- c('Species', 'Period_group', 'Shift_shp', 'Shift_distance', 
               'Shift_angle', 'Shift_radian')


sp_all <- unique(data2$Species)
mark <- 1
for (index in seq_along(sp_all)){
  sp <- sp_all[index]
  data_temp <- data2[data2$Species == sp, ]
  # print(data_temp)
  p1 <- data_temp[data_temp$period == 'LGM', ]$mc_path[1]
  p2 <- data_temp[data_temp$period == 'Current', ]$mc_path[1]
  p3 <- data_temp[data_temp$period == 'SSP126', ]$mc_path[1]
  p4 <- data_temp[data_temp$period == 'SSP585', ]$mc_path[1]
  result_1 <- point2deg(p1, p2)
  for (i in result_1){
    df[mark, ] <- c(sp, 'LGM-Current', result_1[4], result_1[2], 
                    result_1[1], result_1[3])
  }
  
  mark <- mark+1
  result_2 <- point2deg(p2, p3)
  for (i in result_2){
    df[mark, ] <- c(sp, 'Current-SSP126', result_2[4], result_2[2], 
                    result_2[1], result_2[3])
  }
  mark <- mark+1
  result_3 <- point2deg(p2, p4)
  for (i in result_3){
    df[mark, ] <- c(sp, 'Current-SSP585', result_3[4], result_3[2], 
                    result_3[1], result_3[3])
  }
  mark <- mark+1
  print(p1)
}

# export table
write_csv(df, 'Carya_summary_pair.csv')



# Merging line ------------------------------------------------------------------

library(raster)
lines_1 <- list.files('result/', '-Cu_line.shp$', full.names = T)


l_all <- shapefile(lines_1[1])
for (index in 2:5){
  print(index)
  l_temp <- shapefile(lines_1[index])
  # l_all[[index]] <- 
  l_all <- l_all + l_temp
}

plot(l_all)
shapefile(l_all, 'dynamic_all/line_LGM-Current.shp')

# Merging line ------------------------------------------------------------------
library(raster)
lines_1 <- list.files('result/', '-126_line.shp$', full.names = T)


l_all <- shapefile(lines_1[1])
for (index in 2:5){
  print(index)
  l_temp <- shapefile(lines_1[index])
  # l_all[[index]] <- 
  l_all <- l_all + l_temp
}

plot(l_all)
shapefile(l_all, 'dynamic_all/line_Current-SSP126.shp')

# Merging line ------------------------------------------------------------------
library(raster)
lines_1 <- list.files('result/', '-585_line.shp$', full.names = T)


l_all <- shapefile(lines_1[1])
for (index in 2:5){
  print(index)
  l_temp <- shapefile(lines_1[index])
  # l_all[[index]] <- 
  l_all <- l_all + l_temp
}

plot(l_all)
shapefile(l_all, 'dynamic_all/line_Current-SSP585.shp')
