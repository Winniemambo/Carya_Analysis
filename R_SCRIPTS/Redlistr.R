
library(raster)
library(rgeos)
library(rgdal)
library(redlistr)
library(dplyr)

species_data <- read.csv("hunanensis.csv")
species_list <- unique(species_data$species)

#Binary raster files
#package = "redlistr"))
Carya_current <- raster("Cu_suitable.tif")
plot(Carya_current)
Carya_future <- raster("585_suitable.tif")
plot(Carya_future)

#CRS Projection
crs.target <- "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# projectRaster
Carya_current.reproject <- projectRaster(Carya_current, crs = crs.target)
Carya_future.reproject <- projectRaster(Carya_future, crs = crs.target)

# Get the area for both periods
a.2000 <- getArea(Carya_current.reproject)
a.2000
a.2050 <- getArea(Carya_future.reproject)
a.2050

# getAreaLoss。
area.lost <- getAreaLoss(a.2000, a.2050)
area.lost

#decline.stats <- getDeclineStats 
methods = c('ARD', 'PRD', 'ARC')
#decline.stats
decline.stats <- getDeclineStats(a.2000, a.2050, 2000, 2050, 
                                 methods = c('ARD', 'PRD', 'ARC'))
decline.stats

#extrapolated.area <- futureAreaEstimate 
#WARD = decline.stats$ARD, 
#PRD = decline.stats$PRD, 
#ARC = decline.stats$ARC, 
#nYears = 50)
#extrapolated.area
#(https://portals.iucn.org/library/sites/library/files/documents/2016-010.pdf)）。

extrapolated.area <- extrapolateEstimate(a.2000, year.t1 = 2000, 
                                         ARD = decline.stats$ARD, 
                                         PRD = decline.stats$PRD, 
                                         ARC = decline.stats$ARC, 
                                         nYears = 50)
extrapolated.area

predicted.percent.loss <- (extrapolated.area$A.PRD.t3 - a.2000)/a.2000 * 100
predicted.percent.loss

#Get EOO
EOO.polygon <- makeEOO(Carya_current.reproject)
plot(EOO.polygon)
plot(Carya_current.reproject, add = T, col = "green", legend = FALSE)
EOO.area <- getAreaEOO(EOO.polygon)

EOO.area#(km2)

#Get AOO
AOO.grid <- makeAOOGrid(Carya_current.reproject, grid.size = 10000,
                        min.percent.rule = F)
plot(AOO.grid)
plot(Carya_current.reproject, add = T, col = "green", legend = FALSE)

AOO.area <- length(AOO.grid)

AOO.area

# determine status based on area
if (EOO.area >= 45000){
  blo <- "Least Concern (LC)"
  eVal <- 1 }
if (EOO.area < 45000){
  blo <- "Possible Near Threatened (NT)"
  eVal <- 2 }
if (EOO.area < 20000){
  blo <- "Vulnerable (VU)"
  eVal <- 3} # 20000
if (EOO.area < 5000){
  blo <- "Endangered (EN)"
  eVal <- 4} # 5000
if (EOO.area < 100)
{blo <- "Critically Endangered (CR)"
eVal <- 5} # 100
if (EOO.area == "NA"){blo <- "Critically Endangered (CR)"
eVal <- 6}

# determine status based on area
if (AOO.area >= 4500){
  AOO_cat <- "Least Concern (LC)"
  aVal <- 1 } # <
if (AOO.area < 4500){
  AOO_cat <- "Possible Near Threatened (NT)"
  aVal <- 2 }
if (AOO.area < 2000){AOO_cat <- "Vulnerable (VU)"
aVal <- 3 } # < 2000
if (AOO.area < 500){AOO_cat <- "Endangered (EN)"
aVal <- 4 }# < 500
if (AOO.area < 10){AOO_cat  <- "Critically Endangered (CR)"
aVal <- 5 }# < 10
if (AOO.area == "NA"){AOO_cat <- "Critically Endangered (CR)"
aVal <- 6 }

# dataframe to save items
df <- data.frame(matrix(data = NA, nrow = 1, ncol = 6))
colnames(df) <- c("taxon", "EOO Area km2","EOO Status", "AOO Area km2",
                   "AOO Status", "Combined Status")
df$taxon <- species_list
df$`EOO Area km2` <- EOO.area
df$`EOO Status`<- blo
df$`AOO Area km2` <- AOO.area
df$`AOO Status` <- AOO_cat

# the names for combined status
status <- c("Least Concern (LC)","Possible Near Threatened (NT)",
            "Vulnerable (VU)", "Endangered (EN)"
            ,"Critically Endangered (CR)","Critically Endangered (CR)")

# Select the lowest status and use that to define the overall status
if(eVal >= aVal){
  stat <- status[eVal]
}else{
  stat <- status[aVal]
}

df$`Combined Status` <- stat
write.csv(x = df, file = paste0 ('preliminary_threat.csv'))

#Combine AOO,EOO and threats assessments
results <- bind_cols(
  species = species_list,
  as.data.frame(AOO.area),
  as.data.frame(EOO.area),
  decline.stats, 
  percent_loss = predicted.percent.loss
)

#save csv results
write.csv(results,"threat_assessment.csv", row.names = F)

