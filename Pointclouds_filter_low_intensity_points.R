setwd("C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/R")

library(lidR)
library(sp)
library(raster)
library(terra)
library(sf)
library(rgdal)
library(fs)  # For directory manipulation functions
library(stringr)


las_folder <- "M:/Masterarbeit/TLS/Scans_010_clipped_exakt"
output_folder <- "M:/Masterarbeit/TLS/Scans_010_clipped_filtered/"

las_files <- fs::dir_ls(las_folder, regexp = "\\.laz$")


#Loop starten
for (las_file in las_files){
  
  base = fs::path_ext_remove(fs::path_file(las_file))          #plotname
  
  Punktwolke = readLAS(las_file)                               #las einlesen
  t = quantile(Punktwolke$Intensity,probs = c(0.05))           #treshold festlegen
  Punktwolke_filtered = filter_poi(Punktwolke, Intensity >= t) #filtern
  
  pfad_output = paste0(output_folder, base, ".laz")            # pfad konstruieren
  
  writeLAS(Punktwolke_filtered, pfad_output)                   # laz ablegen
}






