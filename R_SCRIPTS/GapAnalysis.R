##Load package
library(raster)
library(GapAnalysis)
library(kableExtra)


##Obtaining occurrences from example
occurrence_file <- "D:\\Gap _analysis\\hunanensis_ga.csv"
hunanensisData <- read.csv(occurrence_file)

##Obtaining species names from the data
speciesList <- unique(hunanensisData$species)

##Obtaining raster_list
Carya_hunanensis <- raster("D:\\Gap _analysis\\hunanensis_Cu.tif")
hunanensisRasters <- Carya_hunanensis
hunanensisRasters
plot(hunanensisRasters)

##Obtaining protected areas raster
ProtectedAreas<- raster("D:\\Gap _analysis\\wdpa.tif")
plot(ProtectedAreas)
#shapefile(ecoregions, "C:/Users/Administrator/Desktop/ecoregions.shp")
ecoregions<- raster::shapefile("D:\\Gap _analysis\\ecoregion.shp")
plot(ecoregions)

#Running all three ex situ gap analysis steps using FCSex function
FCSex_df <- FCSex(Species_list=speciesList,
                  Occurrence_data=hunanensisData,
                  Raster_list=hunanensisRasters, 
                  Buffer_distance=50000,
                  Ecoregions_shp=ecoregions
)

#Running all three in situ gap analysis steps using FCSin function
FCSin_df <- FCSin(Species_list=speciesList,
                  Occurrence_data=hunanensisData,
                  Raster_list=hunanensisRasters,
                  Ecoregions_shp=ecoregions,
                  Pro_areas=ProtectedAreas)


## Combine gap analysis metrics
FCSc_mean_df <- FCSc_mean(FCSex_df = FCSex_df,FCSin_df = FCSin_df)

write.csv(FCSin_df, 'summary/FCSin_df.csv')

##Running Conservation indicator across taxa
indicator_df  <- indicator(FCSc_mean_df)

## Generate summary HTML file with all result
GetDatasets()
SummaryHTML <- ('summary/SummaryHTML')
summaryHTML_file <- SummaryHTML(Species_list=speciesList,
                                Occurrence_data = hunanensisData,
                                Raster_list=hunanensisRasters,
                                Buffer_distance=50000,
                                Ecoregions_shp=ecoregions,
                                Pro_areas=ProtectedAreas,
                                Output_Folder=".",
                                writeRasters=FALSE)

