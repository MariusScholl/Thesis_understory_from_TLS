require(sf)
require(terra)
require(ggplot2)
require(dplyr)
require(fs)
require(raster)
library(gridExtra)

# Script works with variables from script named "Model_entwickliung.R"

#_______________________________________________________________________________
# Response Variable testen auf Normalverteilung
colnames(training_data)
par(mfrow = c(2,3))

hist(training_data$grass_cover_m2,
     main = "grass_cover",
     col = "orange",
     xlab = "m² grass layer")

hist(training_data$V2_cover_m2,
     main = "shrubush_cover",
     col = "lightblue",
     xlab = "m² shrubush layer")

hist(training_data$lowmid_cover_m2,
     col = "lightpink",
     main = "lowmid_cover",
     xlab = "m² 1.5m-3.5m layer")

hist(training_data$highmidcover_m2,
     main ="3.5m-10m Layer",
     col = "lightgrey",
     xlab = "m² 3.5m-10m layer")

hist(training_data$V3_10.30mcover_m2,
     main ="10-30m Layer",
     col = "lightgreen",
     xlab = "m² 10-30m layer")

hist(training_data$fgap_cover_m2,
     main ="fgap Layer",
     col = "lightyellow",
     xlab = "m² fgap layer")

par(mfrow = c(1,1))

# Beide Responsevariablen sind augenscheinlich nicht normalverteilt

#_______________________________________________________________________________

#     Scatterplot 

shrubush = "V2_cover_m2"
mid = "highmidcover_m2"       # 3.5 m -> 10 m
high = "V3_10.30mcover_m2"    # 10m -> 30m
fgap = "fgap_cover_m2"


plots <- list()

# Hier werden Scatterplots von allen Predictoren gegen gewünschte response
# variable geplottet

# einzelne plots
#####
for (pred in colnames(train_data[,c(5:11, 13:30)])){
  
  plot <- ggplot(training_data, aes_string(x = pred, y = shrubush)) +
    geom_point(aes(color = plotname)) +
    geom_smooth(method = "lm") +
    labs(title = paste0(pred, "~ shrubush cover"))
  
  pfad_ablage = paste0("M:/Masterarbeit/statistische_Erg/Scatterplots trainnigsdata vs präditkoren/", shrubush, "/_scatterplot_",pred,".png" )
  
  ggsave(pfad_ablage, plot = plot, width = 12 , height = 8)
  
  print(plot)
  
  Sys.sleep(2)
  
  # abspeichern
  
}

#####
#__________________________

# Hier wird ein großer Plot erstellt

#####

# Create a list to store plots
plots <- list()

# Generate and store individual plots
for (pred in colnames(train_data[,c(5:11, 13:30)])){
  
  plot <- ggplot(train_data, aes_string(x = pred, y = shrubush)) +
    geom_point(aes(color = plotname)) +
    geom_smooth(method = "lm") +
    labs(title = paste0(pred, " ~ shrubush cover in m²"))
  
  plots[[pred]] <- plot
}

# Arrange all plots in one A3-sized plot
pfad_ablage <- "M:/Masterarbeit/statistische_Erg/Scatterplots trainnigsdata vs präditkoren/scatterplots_shrubush_V2.pdf"
pdf(pfad_ablage, width = 42.0, height = 59.4) 
do.call(grid.arrange, c(plots, ncol = 3)) # Adjust ncol to control the number of columns
dev.off()



#####


#_______________________________________________________________________________


# Plot the residuals: the red line should be nearly flat

# indicate how well the model fits the data and whether the assumptions
# of linear regression are met

#####
# Linearity:
# The residuals should be randomly scattered around the horizontal axis
# (the fitted values) with no clear pattern. If there is a pattern
# (e.g., a curve), it suggests that the relationship between the predictors
# and the response variable is not linear

# Homoscedasticity:
# The residuals should have constant variance. This means that the spread of
# the residuals should be roughly the same across all levels of the fitted
# values. If the residuals spread out more widely or narrow down as the fitted
# values increase, it indicates heteroscedasticity.

#####

# Plotting Residuals vs Fitted
plot(model_lm, which = 1)

#Plotting QQ plot
plot(model_lm, which = 2)

#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
mapview(training_data[111,])



# alt:
#####
library(lattice)  #for plotting
library(DHARMa)   #for checking model assumptions
library(glmmTMB)  
library(lme4) 
#________________________________

# LM checken
sim_res_lm <- simulateResiduals(fittedModel = model_lm)

# Plot residual diagnostics
plot(sim_res_lm)

#________________________________

# DHARMa for Random Forest Model
# Create a pseudo-model with predictions
pseudo_model_rf <- lm(test_data$V3_10.30mcover_m2 ~ pred_rf)

# Simulate residuals for pseudo-model
sim_res_rf <- simulateResiduals(fittedModel = pseudo_model_rf)

# Plot residual diagnostics for pseudo-model
plot(sim_res_rf)

#________________________________

#####



