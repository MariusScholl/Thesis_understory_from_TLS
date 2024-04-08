setwd("C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/R")

library(lidR)
library(sp)
library(raster)
library(terra)
library(sf)
library(rgdal)
library(fs)  # For directory manipulation functions
library(stringr)


# Define paths to LAS and shapefiles folders
las_folder <- "M:/Masterarbeit/TLS/Scans_010_clipped_buffer/"
shp_folder <- "M:/Masterarbeit/TLS/Polygon aus Stangen/"
output_folder <- "M:/Masterarbeit/TLS/Scans_010_clipped_exakt/"

# Get list of LAS and shapefiles
shp_files <- fs::dir_ls(shp_folder, regexp = "\\.shp$")

length(shp_files)

# Schleife über alle Shapefiles
for(shp_file in shp_files) {
  #______________________________________________________
  # Pfad zu jew. shapefile defineren
  path_shp = shp_file
  
  buffer_scanpos <- st_read(path_shp)                           #Buffer einladen
  buffer_scanpos <- st_transform(buffer_scanpos, crs = "EPSG:32628")# CRS
  
  #______________________________________________________
  # nur den Basenamen, also den Dateinamen ohne Pfad
  base = fs::path_ext_remove(fs::path_file(shp_file)) #plotname extrahieren
                                                      #ohne .shp Endung
  
  #______________________________________________________
  # Pfad zu jew. laz definieren
  path_laz = paste0(las_folder,base,".laz")
  
  Wolke <- readLAS(path_laz, select = "xyzi", filter = "-drop_z_below 0")
  lidR::crs(Wolke) <- crs("EPSG:32628")
  
  #______________________________________________________
  # Punktwolke zuschneiden
  
  Wolke_clipped <- clip_roi(Wolke, buffer_scanpos)
  
  #______________________________________________________
  
  # Pfad zur ablage festlegen
  path_ablage = paste0(output_folder,base,".laz")
  print(path_ablage)
  
  #schreiben
  writeLAS(Wolke_clipped,path_ablage)
  
  # 
}

