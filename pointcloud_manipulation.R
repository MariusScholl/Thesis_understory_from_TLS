# Load necessary libraries for data processing
library(lidR)
library(sp)
library(raster)
library(terra)
library(sf)
library(rgdal)

# Define file paths
Pfad <- "C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/Geodaten/06_TLS_Punktwolken/2024-03-04_del_banco01_0_010_32628.laz"
Pfad_Testgebiet <- "C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/Geodaten/07_Punktwolkenbearbeitung_in_CC/Poles/Del_Banco01_Poles/DelBanco_01_Poles_UTM_Polygon.shp"

# Read and check LiDAR data
Testwolke <- readLAS(Pfad, select = "xyzi", filter = "-drop_z_below 0")
Testwolke_crs <- st_crs(Testwolke); Testwolke_crs

# Testgebiet einlesen
Testgebiet <- st_read(Pfad_Testgebiet)
Testgebiet <- st_transform(Testgebiet, crs = Testwolke_crs)

#Clippen der Punktwolke auf Testgebiet
Testwolke <- clip_roi(Testwolke, Testgebiet)


#LAS checken
las_check(Testwolke)

# Ground classification
mycsf <- csf(sloop_smooth = F,
             class_threshold = 0.3,
             cloth_resolution = 0.05,
             rigidness = 2)

Testwolke_ground <- classify_ground(Testwolke, mycsf)

#________________________________________________________
#   Vllt zwischenspeichern ???

pfad_output <- "Testwolke_ground.las"

#writeLAS(Testwolke_ground, pfad_output)      # On / Off

#________________________________________________________

# Normalisieren der Punktwolke im nächsten Schritt,
# rechenintensiv, dauert etwas ;)

Testwolke_nml <- normalize_height(Testwolke_ground, tin()) # alternativly use
                                                           # kirging() comp. but
                                                           # intenisv
#________________________________________________________
# Speichern wer will
pfad_output <- "Testwolke_nml.las"
#writeLAS(Testwolke_nml, pfad_output)
#________________________________________________________


# Zuerst die Z Values extrahieren
z_values <- Testwolke_nml@data$Z

# Filter points < 0 m and > 32 m
min_m = 0 
max_m = 35

# normalized & gefilterte Testwolke erstellen & abspeichern
Testwolke_nmlf <- filter_poi(Testwolke_nml, Z >= min_m, Z <= max_m)

#______________________________________
#                        speichern
#______________________________________
pfad_output <- "Testwolke_nmlf.las"
writeLAS(Testwolke_nmlf, pfad_output)

#_______________________________________________________________________________
#                         Jetzt erstellen wir das DTM
#_______________________________________________________________________________

# Create DTM
Testwolke_dtm <- rasterize_terrain(Testwolke_ground,
                                   res = 0.1,
                                   algorithm = kriging(k = 40))

# Optionally save the DTM
writeRaster(Testwolke_dtm, filename="Testwolke_DTM.tif", overwrite=TRUE)

# Create CHM
Testwolke_chm <- rasterize_canopy(Testwolke_nmlf,
                                  res = 0.1,
                                  algorithm = p2r(subcircle = 0.05),
                                  pkg = "terra")

# Optionally save the CHM
writeRaster(Testwolke_chm, "Testwolke_chm.tif", overwrite = TRUE)
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
Testwolke_chm_raster <- raster(Testwolke_chm)

Testwolke_gaps <- getForestGaps(chm_layer=Testwolke_chm_raster,
                                threshold=threshold,
                                size=size)

# plotting gaps (FUNKTIONIERT IRGENDWIE NICHT !!!!!!!!!!!!!!!!!!!!!!!!!)
plot(Testwolke_gaps, col="red", add=TRUE, legend=FALSE)

# computing basic statistis of forest gap ---------------------------------

Testwolke_gstats<-GapStats(gap_layer=Testwolke_gaps, chm_layer=Testwolke_chm)

# Export statistics
write_xlsx(Testwolke_gstats,"/Testwolke_gapstats.xlsx")



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
Testwolke_grd <- filter_poi(Testwolke_nml, Classification == 2)
Testwolke_hrb <- filter_poi(Testwolke_nml, Classification != 2, Classification != 7, Z <= 0.25)
Testwolke_grs <- filter_poi(Testwolke_nml, Classification != 7, Z >= 0.25, Z <= 0.5)
Testwolke_shr <- filter_poi(Testwolke_nml, Classification != 7, Z >= 0.5, Z <= 5)
Testwolke_tre <- filter_poi(Testwolke_nml, Classification != 7, Z >= 5)

# Save filtered LAS files
writeLAS(Testwolke_hrb, "Testwolke_hrb_final.las")
writeLAS(Testwolke_grs, "Testwolke_grs_final.las")
writeLAS(Testwolke_shr, "Testwolke_shr_final.las")




# Step 1: Convert LAS dataset to raster dataset ---------------------------

# Herb
Testwolke_hrbL <- readLAS("Testwolke_hrb_final.las")
raster8h <- grid_metrics(Testwolke_hrbL, ~mean(Z), 0.1)
raster8h[is.na(raster8h)] <- 0      # Set no data values to 0
raster8h[raster8h > 0] <- 2         # Replace filled cells (> 0m) with 2
plot(raster8h)

# Grass
Testwolke_grsL <- readLAS("Testwolke_grs_final.las")
raster8g <- grid_metrics(Testwolke_grsL, ~mean(Z), 0.1)
raster8g[is.na(raster8g)] <- 0      # Set no data values to 0
raster8g[raster8g >= 0.25] <- 2     # Replace filled cells (>= 0.25m) with 2
plot(raster8g)

# Shrub
Testwolke_shrL <- readLAS("Testwolke_shr_final.las")
raster8s <- grid_metrics(Testwolke_shrL, ~mean(Z), 0.1)
raster8s[is.na(raster8s)] <- 0      # Set no data values to 0
raster8s[raster8s >= 0.5] <- 2      # Replace filled cells (>= 0.5m) with 2
plot(raster8s)
