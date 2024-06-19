#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

#                               Model trainieren
#                                   Versuche

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________

# VERSUCH 1:

require(sf)
require(terra)
require(ggplot2)
require(dplyr)
require(fs)
require(raster)



# Trainingsdaten einladen
poly = st_read("C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/Geodaten/04_GIS Projekt La Palma/Trainingsdaten/Subpixels_V1/5_10x10_pixels_from_each_plot_with_all_layers_and_fgap_V3.gpkg")

# filter out irrelevant columns
poly = poly[,-c(6,8,10,11,12,14,15)]


#filter out NA plotnames
poly <- poly %>%
  filter(!is.na(plotname))
# ____________________________
#         Bild auswählen
# 30.09.2023
#se2 = stack("M:/Masterarbeit/Sentinel2/2023_09_30/subset_2_of_S2A_MSIL2A_20230930T120321_N0509_R023_T28RBS_20230930T161454_resampled.tif")

# 10.10.2023
#se2 = stack ("M:/Masterarbeit/Sentinel2/2023_10_10/subset_1_of_S2A_MSIL2A_20231010T120321_N0509_R023_T28RBS_20231010T161800_resampled.tif")

# 02.02.24
se2 = raster::stack("M:/Masterarbeit/Sentinel2/2024_02_02/subset_0_of_S2B_MSIL2A_20240202T120319_N0510_R023_T28RBS_20240202T131017_resampled.tif")

#22.02.24
#se2 = stack("M:/Masterarbeit/Sentinel2/2024_02_22/subset_1_of_S2B_MSIL2A_20240222T120319_N0510_R023_T28RBS_20240222T154314_resampled.tif")

#
# ____________________________

# und auf Küste zuschneiden
geopackage_path <- "C:/Users/mariu/OneDrive/Dokumente/_Masterarbeit/Geodaten/04_GIS Projekt La Palma/Layer.gpkg"
la_palma_umriss <- st_read(dsn = geopackage_path, layer = "LaPalma_Umriss")

la_palma_umriss <- st_transform(la_palma_umriss, crs(se2))

se2 = mask(se2,la_palma_umriss)

names_short <- c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B11","B12")
names(se2) <- names_short

#Indizes hinzufügen
se2$NDVI <- (se2$B8 - se2$B4) / (se2$B8 + se2$B4)
se2$NDWI = (se2$B8 - se2$B11) / (se2$B8 + se2$B11)
se2$SAVI = ((se2$B8 - se2$B4) * (1 + 0.5)) / (se2$B8 + se2$B4 + 0.5)
se2$GNDVI = (se2$B8 - se2$B3) / (se2$B8 + se2$B3)
se2$MSAVI = (2 * se2$B8 + 1 - sqrt((2 * se2$B8 + 1)^2 - 8 * (se2$B8 - se2$B4))) / 2
se2$RENDVI = (se2$B5 - se2$B6) / (se2$B5 + se2$B6)

plot(se2$NDVI, main = "NDVI Sentinel 2 vom 22-22-2024")

#_______________________________________________________________________________
#_______________________________________________________________________________

# Bandvalues für Polygone extrahieren
# hier nochmal checken wie das genau abläuft, und ob ich nicht lieber den Centroid
# statt polygon verwenden soll um nur von einem Pixel den Wert zu grabben

extract_band_values <- function(polygon, raster_stack) {
  extracted_values <- extract(raster_stack, polygon, df = TRUE, fun = "mean")
  extracted_values <- cbind(polygon, extracted_values)
  return(extracted_values)
}

# Funktion auf Polygone anwenden
training_data <- extract_band_values(poly, se2)

# Zu Data Frame & Geometrie Column entfernen
training_data_df <- as.data.frame(training_data)
#training_data_df <- training_data_df %>%
  #select(-geom)


#_______________________________________________________________________________
#_______________________________________________________________________________

# Daten splitten into Training & Validation
set.seed(124)
train_index <- sample(seq_len(nrow(training_data_df)), size = 0.7 * nrow(training_data_df))
train_data <- training_data_df[train_index, ]
test_data <- training_data_df[-train_index, ]

#________

# Namen als Faktor
train_data$plotname <- as.factor(train_data$plotname)
test_data$plotname <- as.factor(test_data$plotname)


#_______________________________________________________________________________
#_______________________________________________________________________________
#
#                   LM Versuch

#       5 = shrubush
#       10 = 3.5-10
#       9 = 10-30m
#       11 = fgap

# Spalten filtern die verwendet werden   
train_data <- train_data[,c(6:8,11, 13:30)]
test_data <- test_data[,c(6:8,11, 13:30)]

model_lm <- lm(fgap_cover_m2 ~ ., data = train_data)

pred_lm <- predict(model_lm, newdata = test_data)
mse_lm <- mean((test_data$fgap_cover_m2 - pred_lm)^2)
mae_lm <- mean(abs(test_data$fgap_cover_m2 - pred_lm))
#_______________________________________________________________________________
# Ausklappen für Versuch 2
#####


# weniger variablen testen, z.B. nur NDVI und die Bänder,
# die in scatterplot ein zusammenhang hatten 
# schauen ob das besser wird

pred_choice = c(7,9,13,16,29,30,35)

train_data <- train_data[,pred_choice]
test_data <- test_data[,pred_choice]

model_lm <- lm(V3_10.30mcover_m2 ~ ., data = train_data)

pred_lm <- predict(model_lm, newdata = test_data)
mse_lm <- mean((test_data$V3_10.30mcover_m2 - pred_lm)^2)
mae_lm <- mean(abs(test_data$V3_10.30mcover_m2 - pred_lm))

#####

#                   Kontrolle
# Berechnung von R² für das lineare Modell
ss_total <- sum((test_data$fgap_cover_m2 - mean(test_data$fgap_cover_m2))^2)
ss_res <- sum((test_data$fgap_cover_m2 - pred_lm)^2)
r2_lm <- 1 - (ss_res / ss_total)


print(paste("Linear Regression MSE:", round(mse_lm,2)))
print(paste("Linear Regression MAE:", round(mae_lm,2)))
print(paste("Linear Regression R²:", round(r2_lm,2)))

#---
#---
#_____________________________

# Load necessary libraries
library(ggplot2)

# Create a data frame with true and predicted values
results <- data.frame(True = test_data$fgap_cover_m2, Predicted = pred_lm)

# Plot predicted vs true values
ggplot(results, aes(x = True, y = Predicted)) +
  geom_point(color = 'blue') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed') +
  labs(title = "Predicted vs True Values LM",
       x = "True Values",
       y = "Predicted Values") +
  theme_minimal()

#####
#_______________________________________________________________________________
#_______________________________________________________________________________
#
#                   Random Forest Versuch

#####
require(randomForest)

# Trainingsdaten resetten
set.seed(420)
train_index <- sample(seq_len(nrow(training_data_df)),
                      size = 0.7 * nrow(training_data_df))

train_data <- training_data_df[train_index, ]
test_data <- training_data_df[-train_index, ]

# Spalten filtern die verwendet werden
pred_choice = c(6:8, 12, 15:32) # grass
pred_choice = c(5:8, 15:32) # Shrubush
pred_choice = c(6:8,13,15:32) # lowmid 1.5 - 3.5m
pred_choice = c(6:8,10, 15:32) # highmid 3.5m - 10m
pred_choice = c(6:9, 15:32) # 10m-30m
pred_choice = c(6:8,11, 15:32) # fgap
train_data <- train_data[,pred_choice] 
test_data <- test_data[,pred_choice]

#_____________________________

# Train a Random Forest regression model
#V2_cover_m2
#V3_10.30mcover_m2
model_rf <- randomForest(V2_cover_m2 ~ ., data = train_data, ntree = 500)

# Predict on the test set
pred_rf <- predict(model_rf, newdata = test_data)

# Evaluate the model
mse_rf <- mean((test_data$V2_cover_m2 - pred_rf)^2)
mae_rf <- mean(abs(test_data$V2_cover_m2 - pred_rf))

# R² Calculation
ss_total_rf <- sum((test_data$V2_cover_m2 - mean(test_data$V2_cover_m2))^2)
ss_res_rf <- sum((test_data$V2_cover_m2 - pred_rf)^2)
r2_rf <- 1 - (ss_res_rf / ss_total_rf)


print(paste("Random Forest MSE:", round(mse_rf,2)))
print(paste("Random Forest MAE:", round(mae_rf,2)))
print(paste("Random Forest R²:", round(r2_rf,2)))

#_____________________________

# Load necessary libraries
library(ggplot2)

# Create a data frame with true and predicted values
results <- data.frame(True = test_data$V2_cover_m2, Predicted = pred_rf)

# Plot predicted vs true values
ggplot(results, aes(x = True, y = Predicted)) +
  geom_point(color = 'blue') +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed') +
  labs(title = "Predicted vs True Values RF Regressor",
       x = "True Values",
       y = "Predicted Values") +
  theme_minimal()

#_____________________________

# Berechnung der Feature Importance für das Random Forest Modell
feature_importance_rf <- importance(model_rf)
feature_importance_df_rf <- data.frame(Feature = rownames(feature_importance_rf), Importance = feature_importance_rf[, 1])

# Ausgabe der Feature Importance
print("Feature Importance for Random Forest Model:")
print(feature_importance_df_rf)

varImpPlot(model_rf)

#_____________________________

#####




