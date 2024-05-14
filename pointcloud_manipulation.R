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
Pfad <- "M:/Masterarbeit/TLS/Scans_010_clipped_filtered_classified/enrique01_classified.laz"
#Pfad_ROI <- "M:/Masterarbeit/TLS/Polygon aus Stangen/las_animas01.shp"


# Read and check LiDAR data
Wolke <- readLAS(Pfad,
                 select = "xyzi",
                 filter = "-drop_z_below 0 -drop_intensity_below 5941")

#quantile(Wolke$Intensity,probs = c(0.05))

Wolke_crs <- st_crs(Wolke); Wolke_crs


# are of interest einlesen (meine sind schon geclippt, daher #)
#Roi <- st_read(Pfad_ROI)
#Roi <- st_transform(Roi, crs = Wolke_crs)

#Clippen der Punktwolke auf Roi
#Wolke <- clip_roi(Wolke, Roi)

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

#LAS checken
las_check(Wolke)

#_______________________________________________________________________________
#                         Jetzt erstellen wir das CHM
#_______________________________________________________________________________


# sloop_smooth:
# Wenn steile Flanken vorhanden sind, setzen Sie diesen Parameter auf TRUE,
# um Fehler bei der Nachbearbeitung zu reduzieren.

# class_threshold:
# Der Abstand zum simulierten Stoff, um eine Punktwolke in Boden und Nicht-Boden
# zu klassifizieren. Der Standardwert ist 0,5.

#cloth_resolution	:
# Der Abstand zwischen den Partikeln im Stoff. Dies wird normalerweise auf den
# durchschnittlichen Abstand der Punkte in der Punktwolke eingestellt.
# Der Standardwert ist 0,5.

# rigidness	
# Die Steifheit des Stoffes. 1 steht für sehr weich (für raues Gelände geeignet),
# 2 steht für mittel und 3 steht für hartes Tuch (für flaches Gelände).
# Der Standardwert ist 1.


# Ground classification
mycsf <- csf(sloop_smooth = F,
             class_threshold = 0.1,
             cloth_resolution = 0.1,
             rigidness = 1)

Wolke_ground <- classify_ground(Wolke, mycsf)

#_______________________________________________________
#   Vllt zwischenspeichern ???

pfad_output <- "M:/Masterarbeit/TLS/pinocchio01_ground_class0_01_rigidnesS_1_cloth_res0_1.las"

writeLAS(Wolke_ground, pfad_output)      # On / Off
#_______________________________________________________

# Create DTM
Wolke_dtm <- rasterize_terrain(Wolke_ground,
                               res = 0.2,
                               algorithm = tin())

plot_dtm3d(Wolke_dtm, bg = "white")

# Optionally save the DTM
writeRaster(Wolke_dtm,
            filename="G:/Masterarbeit/TLS/Wolke_DTM.tif",
            overwrite=TRUE)


#________________________________________________________

# Normalisieren der Punktwolke im nächsten Schritt,
# rechenintensiv, dauert etwas ;)

Wolke_nml <- normalize_height(Wolke_ground, tin()) # alternativly use
                                                           # kirging() comp. but
                                                           # intenisv

# Speichern wer will
pfad_output <- "M:/Masterarbeit/TLS/monte_santo01_nml.las"
#writeLAS(Wolke_nml, pfad_output)
#________________________________________________________


# Zuerst die Z Values extrahieren
z_values <- Wolke_nml@data$Z

summary(z_values)

# Filter points < 0 m and > 32 m
min_m = 0
max_m = 32

# normalized & gefilterte Wolke erstellen & abspeichern
Wolke_nmlf <- filter_poi(Wolke_nml,
                         Z >= min_m,
                         Z <= max_m)

#______________________________________
#                        speichern
#______________________________________
pfad_output <- "m:/Masterarbeit/TLS/monte_santo01_nmlf.las"

writeLAS(Wolke_nmlf, pfad_output)

#_______________________________________________________________________________
#                         Jetzt erstellen wir das CHM
#_______________________________________________________________________________



# Create CHM
Wolke_chm <- rasterize_canopy(Wolke_nmlf,
                                  res = 0.1,
                                  algorithm = p2r(subcircle = 0.05),
                                  pkg = "terra")

# Optionally save the CHM
writeRaster(Wolke_chm, "G:/Masterarbeit/TLS/Wolke_chm.tif", overwrite = TRUE)
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

threshold<-7 
size<-c(1,1000) # m2


# detecting forest gaps ---------------------------------------------------
Wolke_chm_raster <- raster(Wolke_chm)

plot(Wolke_chm_raster)

Wolke_gaps <- getForestGaps(chm_layer=Wolke_chm_raster,
                                threshold=threshold,
                                size=size)

# plotting gaps (FUNKTIONIERT IRGENDWIE NICHT !!!!!!!!!!!!!!!!!!!!!!!!!)
plot(Wolke_gaps, col="brown" , add=TRUE, legend=FALSE)

# computing basic statistis of forest gap ---------------------------------

Wolke_gstats<-GapStats(gap_layer=Wolke_gaps, chm_layer=Wolke_chm)

# Export statistics
#write_xlsx(Wolke_gstats,"G:/Masterarbeit/TLS/Wolke_gapstats.xlsx")



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
writeLAS(Wolke_hrb, "M:/Masterarbeit/TLS/Wolke_hrb_final.las")
writeLAS(Wolke_grs, "M:/Masterarbeit/TLS/Wolke_grs_final.las")
writeLAS(Wolke_shr, "M:/Masterarbeit/TLS/Wolke_shr_final.las")




# Step 1: Convert LAS dataset to raster dataset ---------------------------

# Herb
Wolke_hrbL <- readLAS("M:/Masterarbeit/TLS/Wolke_hrb_final.las")
raster8h <- grid_metrics(Wolke_hrbL, ~mean(Z), 0.1)
raster8h[is.na(raster8h)] <- 0      # Set no data values to 0
raster8h[raster8h > 0] <- 2         # Replace filled cells (> 0m) with 2

plot(raster8h >= 2, legend = FALSE, main ="herblayer")

# Grass
Wolke_grsL <- readLAS("M:/Masterarbeit/TLS/Wolke_grs_final.las")
raster8g <- grid_metrics(Wolke_grsL, ~mean(Z), 0.1)
raster8g[is.na(raster8g)] <- 0      # Set no data values to 0
raster8g[raster8g >= 0.25] <- 2     # Replace filled cells (>= 0.25m) with 2

plot(raster8g, main = "grass")

# Shrub
Wolke_shrL <- readLAS("M:/Masterarbeit/TLS/Wolke_shr_final.las")
raster8s <- grid_metrics(Wolke_shrL, ~mean(Z), 0.1)
raster8s[is.na(raster8s)] <- 0      # Set no data values to 0
raster8s[raster8s >= 0.5] <- 2      # Replace filled cells (>= 0.5m) with 2

plot(raster8s, main = "shrub")
