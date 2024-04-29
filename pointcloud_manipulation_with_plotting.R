# Create a vector of package names
packages <- c("lidR", "ggplot2", "sp", "raster", "terra")

# Install all packages in the vector
install.packages(packages, dependencies = T)


library(lidR)
library(ggplot2)
library(sp)
library(raster)
library(terra)
library(sf)
#_________________________________________
#_________________________________________
#_________________________________________
#_________________________________________
#_________________________________________



# Point Cloud Prosessing Script

# Pfad für Testszene einladen
Pfad <- "C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/Geodaten/06_TLS_Punktwolken/2024-03-04_del_banco01_0_010_cropped.laz"
Pfad_Testgebiet <- "C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/Geodaten/07_Punktwolkenbearbeitung_in_CC/Poles/Del_Banco01_Poles/DelBanco_01_Poles_UTM.shp"

# load and check data -----------------------------------------------------

Testwolke <- readLAS(Pfad, select = "xyzi", filter = "-drop_z_below 0")
lidR::crs(Testwolke) <- crs("EPSG:25833")

#Testgebiet vektor einladen
Testgebiet <- st_read(Pfad_Testgebiet)
Testgebiet <- st_transform(Testgebiet, crs = Testwolke_crs)

#Clippen der Punktwolke auf Testgebiet
Testwolke <- clip_roi(Testwolke, Testgebiet)

las_check(Testwolke)
print(Testwolke)

plot(Testwolke,
     bg = "white", 
     axis = TRUE, 
     legend = TRUE
)


# ground classification ---------------------------------------------------

mycsf <- csf(sloop_smooth = F,
             class_threshold = 0.3,
             cloth_resolution = 0.05,
             rigidness = 2)

Testwolke_ground <- classify_ground(Testwolke, mycsf)


# plot result
plot_crossection <- function(Testwolke_ground,
                             p1 = c(min(Testwolke_ground@data$X), mean(Testwolke_ground@data$Y)),
                             p2 = c(max(Testwolke_ground@data$X), mean(Testwolke_ground@data$Y)),
                             width = 4, colour_by = NULL)

#_________________  
# Vorbereitung Crossection  
{
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(Testwolke_ground, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}


# plotten Crossection
plot_crossection(Testwolke_ground, colour_by = factor(Classification))

plot(Testwolke_ground, color = "Classification", size = 2, bg = "white") 

# plot only ground
gnd <- filter_ground(Testwolke_ground)
plot(gnd, size = 1, bg = "white")

# save classified point cloud
#writeLAS(Testwolke_ground, "/Testwolke_clss.las")


# create DTM --------------------------------------------------------------

Testwolke_dtm <- rasterize_terrain(Testwolke_ground, res = 0.1, algorithm = kriging(k = 40))
plot(Testwolke_dtm, col = gray(1:50/50))


# normalize point cloud ---------------------------------------------------

Testwolke_nml <- normalize_height(Testwolke_ground, kriging())
hist(filter_ground(Testwolke_nml)$Z, main = "", xlab = "Elevation")
plot(Testwolke_nml, size = 2, axis = TRUE, legend = TRUE, bg = "white")

# filter points < 0 m and > 32 m
Testwolke_nmlf <- filter_poi(Testwolke_nml, Z >= 0, Z <= 32)
plot(Testwolke_nmlf, size = 2, axis = TRUE, legend = TRUE, bg = "white")

# Testwolke_nmlf <- readLAS("/Testwolke_nmlf.las") # LAStools


# clip normalized point cloud to 30x30 m plot -----------------------------

Testwolke_norm_circle <- clip_circle(Testwolke_nmlf, 790970, 5845788, 15)
plot(Testwolke_norm_circle, size = 2, axis = TRUE, legend = TRUE, bg = "white")

# save normalized point cloud
writeLAS(Testwolke_norm_circle, "/Testwolke_norm_circle.las")


# create CHM --------------------------------------------------------------

fill.na <- function(x, i=5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else { return(x[i]) }}
w <- matrix(1, 3, 3)

Testwolke_chm <- rasterize_canopy(Testwolke_nmlf, res = 0.1, algorithm = p2r(subcircle = 0.05), pkg = "terra")
filled <- terra::focal(Testwolke_chm, w, fun = fill.na)
Testwolke_chms <- terra::focal(Testwolke_chm, w, fun = mean, na.rm = TRUE)

chms <- c(Testwolke_chm, filled, Testwolke_chms)
names(chms) <- c("Base", "Filled", "Smoothed")
col <- height.colors(25)
plot(chms, col = col)
plot(Testwolke_chms, col = col)

# save chm
writeRaster(Testwolke_chms, "/Testwolke_chm_circle.tif")


# ForestGapR for forest gap analysis from canopy height models ------------

library(ForestGapR)
library(raster)
library(viridis)


# load chm ----------------------------------------------------------------

Testwolke_chm <- raster("/Testwolke_chm_circle.tif")
Testwolke_chm

plot(Testwolke_chm, col=viridis(10))


# setting height thresholds -----------------------------------------------

threshold<-20
size<-c(1,1000) # m2


# detecting forest gaps ---------------------------------------------------

Testwolke_gaps <- getForestGaps(chm_layer=Testwolke_chm, threshold=threshold, size=size)

# plotting gaps
plot(Testwolke_gaps, col="red", add=TRUE, legend=FALSE)


# computing basic statistis of forest gap ---------------------------------

Testwolke_gstats<-GapStats(gap_layer=Testwolke_gaps, chm_layer=Testwolke_chm)

# Export statistics
write_xlsx(Testwolke_gstats,"/Testwolke_gapstats.xlsx")


# gap-size Frequency Distributions ----------------------------------------

GapSizeFDist(gaps_stats=Testwolke_gstats, method="Hanel_2017", col="forestgreen", pch=16, cex=1,
             axes=FALSE,ylab="Gap Frequency",xlab=as.expression(bquote("Gap Size" ~ (m^2) )))
axis(1);axis(2)
grid(4,4)



# calculate percentual fuel type cover ------------------------------------

# filter points by Layer --------------------------------------------------

Testwolke_grd <- filter_poi(Testwolke_nof, Classification == 2)
Testwolke_hrb <- filter_poi(Testwolke_nof, Classification != 2, Classification != 7, Z <= 0.25)
Testwolke_grs <- filter_poi(Testwolke_nof, Classification != 7, Z >= 0.25, Z <= 0.5)
Testwolke_shr <- filter_poi(Testwolke_nof, Classification != 7, Z >= 0.5, Z <= 5)
Testwolke_tre <- filter_poi(Testwolke_nof, Classification != 7, Z >= 5)

writeLAS(Testwolke_hrb, "Testwolke_hrb_final.las")
writeLAS(Testwolke_grs, "Testwolke_grs_final.las")
writeLAS(Testwolke_shr, "Testwolke_shr_final.las")

library(rgdal)
library(rgeos)
library(sf)
library(terra)
library(fasterize)


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



# Step 2: Create Mask based on Point Cloud Center -------------------------

# Set the center coordinates of the circle
center_x <- (extent(Testwolke_nof)[1] + extent(Testwolke_nof)[2]) / 2
center_y <- (extent(Testwolke_nof)[3] + extent(Testwolke_nof)[4]) / 2

# Set the radius of the circle
radius <- 15

# Set the number of points to define the circle
num_points <- 100

# Generate the coordinates for the circle
theta <- seq(0, 2 * pi, length.out = num_points)
x <- center_x + radius * cos(theta)
y <- center_y + radius * sin(theta)

# Combine x and y coordinates into a matrix
coords <- cbind(x, y)

# Create the circular polygon
circle_polygon <- Polygon(coords)

# Create a spatial polygon object
circle_sp <- SpatialPolygons(list(Polygons(list(circle_polygon), ID = "circle")))

# Plot the circular polygon
plot(circle_sp, col = "white", border = "red", lwd = 2) # gut


# Rasterize the Polygon Mask and set outer Area to NA ---------------------

# HERB
SpP_rash <- rasterize(circle_sp, raster8h, getCover=TRUE) # The circle polygon is rasterized
SpP_rash[SpP_rash==0] <- NA                               # Everything outside the circle is set to NA
# GRASS
SpP_rasg <- rasterize(circle_sp, raster8g, getCover=TRUE)
SpP_rasg[SpP_rasg==0] <- NA 
# SHRUB
SpP_rass <- rasterize(circle_sp, raster8s, getCover=TRUE)
SpP_rass[SpP_rass==0] <- NA 

# Plot to check
plot(mask(raster8s, SpP_rass)) # SpP_ras = Mask as Raster
plot(circle_sp,  add=TRUE)


# extract and save masked area from raster --------------------------------

# HERB
masked_h <- mask(raster8h, SpP_rash)
plot(masked_h)
# GRASS
masked_g <- mask(raster8g, SpP_rasg)
plot(masked_g)
#SHRUB
masked_s <- mask(raster8s, SpP_rass)
plot(masked_s)

# Save
writeRaster(masked_h, "Testwolke_masked_h.tif")
writeRaster(masked_g, "Testwolke_masked_g.tif")
writeRaster(masked_s, "Testwolke_masked_s.tif")



# Step 3: Calculate total cells, cells with value 0, and cells wit --------

# HERB
h0 <- length(masked_h[masked_h == 0])
h2 <- length(masked_h[masked_h == 2])
h_total <- h0+h2
h0_perc <- (h0 / h_total) * 100
h2_perc <- (h2 / h_total) * 100
# GRASS
g0 <- length(masked_g[masked_g == 0])
g2 <- length(masked_g[masked_g == 2])
g_total <- g0+g2
g0_perc <- (g0 / g_total) * 100
g2_perc <- (g2 / g_total) * 100
# SHRUB
s0 <- length(masked_s[masked_s == 0])
s2 <- length(masked_s[masked_s == 2])
s_total <- s0+s2
s0_perc <- (s0 / s_total) * 100
s2_perc <- (s2 / s_total) * 100

# END