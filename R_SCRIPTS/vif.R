
library(raster)
library(tidyverse)
library(usdm)
library(stringr)

# Current -------------------------------------------------------------------
# Variable path
rst_path <- read_csv('current.csv')[[1]]

# grid
current <- stack(rst_path)
plot(current[[2]])

# Study area
study_area <- shapefile("hunanensis.shp")
plot(study_area, add=T)


# Tiles change the extent of the original raster object, but result in a rectangular extent;
# The mask does not change the extent of the original raster object, it simply turns the data outside the vector extent null。

# Extraction by mask (inside）
var_all <- mask(crop(current, study_area), study_area)
names(var_all) <- str_replace(basename(rst_path), '.tif', '')
plot(var_all[[2]])
plot(study_area, add=TRUE)

# VIF Remove collinearity
vif_re <- vifstep(stack(var_all), 5)


# Write the results
for (var in names(var_all)){
  if (var %in% vif_re@results$Variables) {
    writeRaster(var_all[[var]],
                paste0('Current_autocorrelated/', var, '.tif')
    )}
}



# Other -------------------------------------------------------------------
# rst_path <- read_csv('12_vars_LGM.csv')[[1]]
rst_path <- read_csv('LGM.csv')[[1]]
# stack
rst_other <- stack(rst_path)

# study_area
study_area <- shapefile("hunanensis.shp")

# extract by mask
var_all <- mask(crop(rst_other, study_area), study_area)
names(var_all) <- str_replace(basename(rst_path), '.tif', '')

# write the results
for (var in names(var_all)) {
  writeRaster(var_all[[var]],
              paste0('LGM_autocorrelated/', var, '.tif'))
}

# rst_path <- read_csv('12_vars_LGM.csv')[[1]]
rst_path <- read_csv('LIG.csv')[[1]]
# stack
rst_other <- stack(rst_path)

# study_area
study_area <- shapefile("hunanensis.shp")

# extract by mask
var_all <- mask(crop(rst_other, study_area), study_area)
names(var_all) <- str_replace(basename(rst_path), '.tif', '')

# write results
for (var in names(var_all)) {
  writeRaster(var_all[[var]],
              paste0('LIG_autocorrelated/', var, '.tif'))
}

# rst_path <- read_csv('12_vars_LGM.csv')[[1]]
rst_path <- read_csv('SSP126.csv')[[1]]
# stack
rst_other <- stack(rst_path)

# study_area
study_area <- shapefile("hunanensis.shp")

# extract by mask
var_all <- mask(crop(rst_other, study_area), study_area)
names(var_all) <- str_replace(basename(rst_path), '.tif', '')

# write results
for (var in names(var_all)) {
  writeRaster(var_all[[var]],
              paste0('SSP126_autocorrelated/', var, '.tif'))
}

# rst_path <- read_csv('12_vars_LGM.csv')[[1]]
rst_path <- read_csv('SSP585.csv')[[1]]
# stack
rst_other <- stack(rst_path)

# study_area
study_area <- shapefile("hunanensis.shp")

# extract by mask）
var_all <- mask(crop(rst_other, study_area), study_area)
names(var_all) <- str_replace(basename(rst_path), '.tif', '')

# write results
for (var in names(var_all)) {
  writeRaster(var_all[[var]],
              paste0('SSP585_autocorrelated/', var, '.tif'))
}





