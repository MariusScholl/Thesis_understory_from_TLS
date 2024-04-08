setwd("C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/R/Skripte/Thesis_understory_from_TLS")

# Load necessary libraries for data processing
library(lidR)
library(sp)
library(raster)
library(terra)
library(sf)
library(rgdal)

#_______________________________________________________________________________

# Define file paths
Pfad <- "M:/Masterarbeit/TLS/Scans_010_clipped_exakt/las_animas01.laz"
Pfad_ROI <- "M:/Masterarbeit/TLS/Polygon aus Stangen/las_animas01.shp"

# Read and check LiDAR data
Wolke <- readLAS(Pfad, select = "xyzi", filter = "-drop_z_below 0")
Wolke_crs <- st_crs(Wolke); Wolke_crs


# are of interest einlesen (meine sind schon geclippt, daher #)
#Roi <- st_read(Pfad_ROI)
#Roi <- st_transform(Roi, crs = Wolke_crs)

#Clippen der Punktwolke auf Roi
#Wolke <- clip_roi(Wolke, Roi)


#LAS checken
las_check(Wolke)

# Ground classification
mycsf <- csf(sloop_smooth = F,
             class_threshold = 0.3,
             cloth_resolution = 0.05,
             rigidness = 2)

Wolke_ground <- classify_ground(Wolke, mycsf)

#________________________________________________________
#   Vllt zwischenspeichern ???

pfad_output <- "Wolke_ground.las"

#writeLAS(Wolke_ground, pfad_output)      # On / Off

#________________________________________________________

# Normalisieren der Punktwolke im nächsten Schritt,
# rechenintensiv, dauert etwas ;)

Wolke_nml <- normalize_height(Wolke_ground, tin()) # alternativly use
                                                           # kirging() comp. but
                                                           # intenisv
#________________________________________________________
# Speichern wer will
pfad_output <- "Wolke_nml.las"
#writeLAS(Wolke_nml, pfad_output)
#________________________________________________________


# Zuerst die Z Values extrahieren
z_values <- Wolke_nml@data$Z

# Filter points < 0 m and > 32 m
min_m = 0 
max_m = 35

# normalized & gefilterte Wolke erstellen & abspeichern
Wolke_nmlf <- filter_poi(Wolke_nml, Z >= min_m, Z <= max_m)

#______________________________________
#                        speichern
#______________________________________
pfad_output <- "Wolke_nmlf.las"
writeLAS(Wolke_nmlf, pfad_output)

#_______________________________________________________________________________
#                         Jetzt erstellen wir das DTM
#_______________________________________________________________________________

# Create DTM
Wolke_dtm <- rasterize_terrain(Wolke_ground,
                                   res = 0.1,
                                   algorithm = kriging(k = 40))

# Optionally save the DTM
writeRaster(Wolke_dtm, filename="Wolke_DTM.tif", overwrite=TRUE)

# Create CHM
Wolke_chm <- rasterize_canopy(Wolke_nmlf,
                                  res = 0.1,
                                  algorithm = p2r(subcircle = 0.05),
                                  pkg = "terra")

# Optionally save the CHM
writeRaster(Wolke_chm, "Wolke_chm.tif", overwrite = TRUE)
#_____________________________

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

#                              Forest gap analysis 


library(ForestGapR)
library(raster)
library(viridis)
library(terra)


# setting height thresholds -----------------------------------------------

threshold<-20 
size<-c(1,1000) # m2


# detecting forest gaps ---------------------------------------------------
Wolke_chm_raster <- raster(Wolke_chm)

Wolke_gaps <- getForestGaps(chm_layer=Wolke_chm_raster,
                                threshold=threshold,
                                size=size)

# plotting gaps (FUNKTIONIERT IRGENDWIE NICHT !!!!!!!!!!!!!!!!!!!!!!!!!)
plot(Wolke_gaps, col="red", add=TRUE, legend=FALSE)

# computing basic statistis of forest gap ---------------------------------

Wolke_gstats<-GapStats(gap_layer=Wolke_gaps, chm_layer=Wolke_chm)

# Export statistics
write_xlsx(Wolke_gstats,"/Wolke_gapstats.xlsx")



#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#                       Fuel Type Cover OLD approach
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________


# Calculate percentual fuel type cover (steps included without the plotting parts)
# Filter points by Layer
Wolke_grd <- filter_poi(Wolke_nml, Classification == 2)
Wolke_hrb <- filter_poi(Wolke_nml, Classification != 2, Classification != 7, Z <= 0.25)
Wolke_grs <- filter_poi(Wolke_nml, Classification != 7, Z >= 0.25, Z <= 0.5)
Wolke_shr <- filter_poi(Wolke_nml, Classification != 7, Z >= 0.5, Z <= 5)
Wolke_tre <- filter_poi(Wolke_nml, Classification != 7, Z >= 5)

# Save filtered LAS files
writeLAS(Wolke_hrb, "Wolke_hrb_final.las")
writeLAS(Wolke_grs, "Wolke_grs_final.las")
writeLAS(Wolke_shr, "Wolke_shr_final.las")




# Step 1: Convert LAS dataset to raster dataset ---------------------------

# Herb
Wolke_hrbL <- readLAS("Wolke_hrb_final.las")
raster8h <- grid_metrics(Wolke_hrbL, ~mean(Z), 0.1)
raster8h[is.na(raster8h)] <- 0      # Set no data values to 0
raster8h[raster8h > 0] <- 2         # Replace filled cells (> 0m) with 2
plot(raster8h)

# Grass
Wolke_grsL <- readLAS("Wolke_grs_final.las")
raster8g <- grid_metrics(Wolke_grsL, ~mean(Z), 0.1)
raster8g[is.na(raster8g)] <- 0      # Set no data values to 0
raster8g[raster8g >= 0.25] <- 2     # Replace filled cells (>= 0.25m) with 2
plot(raster8g)

# Shrub
Wolke_shrL <- readLAS("Wolke_shr_final.las")
raster8s <- grid_metrics(Wolke_shrL, ~mean(Z), 0.1)
raster8s[is.na(raster8s)] <- 0      # Set no data values to 0
raster8s[raster8s >= 0.5] <- 2      # Replace filled cells (>= 0.5m) with 2
plot(raster8s)
