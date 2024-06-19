# Prediction


#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
# Prediction vorbereiten

# 30.09.2023
#se2 = stack("M:/Masterarbeit/Sentinel2/2023_09_30/subset_2_of_S2A_MSIL2A_20230930T120321_N0509_R023_T28RBS_20230930T161454_resampled.tif")

# 10.10.2023
#se2 = stack ("M:/Masterarbeit/Sentinel2/2023_10_10/subset_1_of_S2A_MSIL2A_20231010T120321_N0509_R023_T28RBS_20231010T161800_resampled.tif")

#02.02.24
#se2 = raster::stack("M:/Masterarbeit/Sentinel2/2024_02_02/subset_0_of_S2B_MSIL2A_20240202T120319_N0510_R023_T28RBS_20240202T131017_resampled.tif")

#22.02.24
se2 = stack("M:/Masterarbeit/Sentinel2/2024_02_22/subset_1_of_S2B_MSIL2A_20240222T120319_N0510_R023_T28RBS_20240222T154314_resampled.tif")

#________________________________________
Umriss_LaPalma = st_read("C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/Geodaten/04_GIS Projekt La Palma/La_Palma_Umriss.gpkg")

se2_masked = mask(se2,Umriss_LaPalma)

plotRGB(se2_masked, r = 4, g = 3, b = 2, stretch = "lin")

names_short <- c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B11","B12")
names(se2_masked) <- names_short

#Indizes hinzufügen
se2_masked$NDVI <- (se2_masked$B8 - se2_masked$B4) / (se2_masked$B8 + se2_masked$B4)
se2_masked$NDWI = (se2_masked$B8 - se2_masked$B11) / (se2_masked$B8 + se2_masked$B11)
se2_masked$SAVI = ((se2_masked$B8 - se2_masked$B4) * (1 + 0.5)) / (se2_masked$B8 + se2_masked$B4 + 0.5)
se2_masked$GNDVI = (se2_masked$B8 - se2_masked$B3) / (se2_masked$B8 + se2_masked$B3)
se2_masked$MSAVI = (2 * se2_masked$B8 + 1 - sqrt((2 * se2_masked$B8 + 1)^2 - 8 * (se2_masked$B8 - se2_masked$B4))) / 2
se2_masked$RENDVI = (se2_masked$B5 - se2_masked$B6) / (se2_masked$B5 + se2_masked$B6)

#_______________________________________________________________________________

# "DEMPixel_mean"  ||| "Aspect_mean" ||| "Slope_mean"

DEM_pfad = "C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/Geodaten/04_GIS Projekt La Palma/DEM/DEM_32628.tif"
DEM = raster(DEM_pfad)
crs(DEM) <- crs(se2)


# Calculate slope
slope <- terrain(DEM, opt = 'slope', unit = 'degrees')
# Calculate aspect
aspect <- terrain(DEM, opt = 'aspect', unit = 'degrees')

# Resample and mask the DEM-related data to match the Sentinel-2 data
DEM_res <- mask(resample(DEM, se2), Umriss_LaPalma)
slope_res <- mask(resample(slope, se2), Umriss_LaPalma)
aspect_res <- mask(resample(aspect, se2), Umriss_LaPalma)

# Add DEM, slope, and aspect to the Sentinel-2 stack
se2_stack <- stack(se2_masked, DEM_res, slope_res, aspect_res)
names(se2_stack) <- c(names(se2_masked), "DEMPixel_mean", "Slope_mean", "Aspect_mean")

print("zu predictende Daten vorbereitet")

par(mfrow = c(1,2))

plotRGB(se2_stack, r = 4, g = 3, b = 2, stretch = "lin")
plot(se2_stack$DEMPixel_mean)

dev.off()

#writeRaster(se2_stack,"M:/Masterarbeit/Raster Stacks prediction/se2_stack_22_02_2024.tif")

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

pine_cover_pfad = "M:/Masterarbeit/Files_für_Masterarbeit/Pine_Forest_vegclass_okt_2022.gpkg"
pine_cover = st_read(pine_cover_pfad)

se2_aoi = mask(se2_stack, pine_cover)
plotRGB(se2_aoi, r = 4, g = 3, b = 2, stretch = "lin")

# Predict and create prediction raster
predictors <- as.data.frame(getValues(se2_aoi))

# Handle NA values
valid_idx <- !is.na(rowSums(predictors))
valid_predictors <- predictors[valid_idx, ]

# Predict
predictions <- rep(NA, nrow(predictors))
predictions[valid_idx] <- predict(model_rf_vsurf, valid_predictors)

# Convert predictions back to raster
prediction_raster <- setValues(raster(se2_aoi), predictions)

# Plot results
plot(prediction_raster, main = "Prediction Results")

writeRaster(prediction_raster, "C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/Geodaten/04_GIS Projekt La Palma/prediction_rf_0p39_vsurf_shrubbush.tif")
