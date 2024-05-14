library(lidR)
library(sp)
library(raster)
library(terra)
library(sf)
library(rgdal)
library(fs)
library(RANN)

wolke_folder <- "M:/Masterarbeit/TLS/Scans_010_normalisiert"
stems_folder <- "M:/Masterarbeit/TLS/Scans_010_3dFin/"

# Get list of LAS files in wolke folder
las_files <- fs::dir_ls(wolke_folder, regexp = "\\.laz$")

# Loop der alle Baumstämme aus der original punktwolke entfernt
# anhand der Punktposition der 3DFin stems
for (las_file in las_files) {
  
  # Base filename without extension
  base <- fs::path_ext_remove(fs::path_file(las_file))
  
  # Path to corresponding stems file
  stems_path <- paste0(stems_folder, base, ".laz")
  print(paste("Pfad festgelegt:", stems_path))
  
  #Pfad zu Ablage festlegen
  pfad_output = "M:/Masterarbeit/TLS/Scans_010_stems_removed/"
  pfad_output_merged = paste0(pfad_output,base,".laz")
  print(paste("Ablagepfad: ", pfad_output_merged))
  
  # Read the point clouds
  wolke <- readLAS(las_file)
  stems <- readLAS(stems_path)
  
  
  # Koordinaten der Punktwolken in Matrix überführen
  pc1_coords <- as.matrix(wolke@data[, 1:3])
  pc2_coords <- as.matrix(stems@data[, 1:3])
  
  # nearest neighbor analyse
  nn_results <- nn2(pc2_coords, pc1_coords, k = 1)
  
  # Distanzen extrahieren
  distances <- nn_results$nn.dists
  
  # Distanz Threshold festlegen, zum Punkte matchen
  threshold <- 0.1
  
  # Filter points that are farther than the threshold distance
  filtered_indices <- which(distances > threshold)
  filtered_points <- wolke@data[filtered_indices, ]
  
  # Create a new LAS object with the filtered points
  pointcloud_difference <- LAS(filtered_points)
  
  writeLAS(pointcloud_difference, pfad_output_merged)
  
}

