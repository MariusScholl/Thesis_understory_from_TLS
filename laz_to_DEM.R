setwd("C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/R")

library(lidR)
library(sp)
library(raster)
library(terra)
library(sf)
library(rgdal)
library(fs)  # For directory manipulation functions
library(stringr)

input_folder <- "M:/Masterarbeit/TLS/Scans_010_clipped_filtered_classified/"
output_folder <- "M:/Masterarbeit/TLS/Scans_010_DEM/"

las_files <- fs::dir_ls(input_folder, regexp = "\\.laz$")


for (las_file in las_files){
  
  Wolke = readLAS(las_file, select = '*')
  
  #Pfadbase extrahieren
  base = fs::path_ext_remove(fs::path_file(las_file))
  
  # DGM rasterisieren
  DGM <- rasterize_terrain(Wolke,
                           res = 0.2,
                           algorithm = tin())
  
  # speicherpfad zusammenwursteln
  pfad_output = paste0(output_folder,base,".tif")
  
  # Raster schreiben
  writeRaster(DGM, pfad_output, overwrite = T)

}





