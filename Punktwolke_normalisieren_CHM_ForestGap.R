# normalisieren und dann CHM erstellen
# 

las_folder <- "M:/Masterarbeit/TLS/Scans_010_clipped_filtered_classified"
output_folder <- "M:/Masterarbeit/TLS/Scans_010_normalisiert"

las_files <- fs::dir_ls(las_folder, regexp = "\\.laz$")


#Loop starten um Punktwolken zu normalisieren -> RECHENINTENSIV
for (las_file in las_files){
  
  base = fs::path_ext_remove(fs::path_file(las_file))          #plotname
  
  Punktwolke = readLAS(las_file)                               #las einlesen
  
  #Wolke normalisieren
  Wolke_nml <- normalize_height(Punktwolke, tin())
  
  #_________________________________________________________
  #             Z values filtern
  #_________________________________________________________
  # Zuerst die Z Values extrahieren
  z_values <- Wolke_nml@data$Z
  
  # Filter points < 0 m and > 32 m
  min_m = 0
  max_m = 32
  
  # normalized & gefilterte Wolke erstellen & abspeichern
  Wolke_nml <- filter_poi(Wolke_nml,
                           Z >= min_m,
                           Z <= max_m)
  #_________________________________________________________
  
  
  
  pfad_output = paste0(output_folder, base, ".laz")            # pfad konstruieren
  writeLAS(Wolke_nml, pfad_output)                   # laz ablegen

}



#_______________________________________________________
# Create CHM
#_______________________________________________________

las_folder <- "M:/Masterarbeit/TLS/Scans_010_normalisiert"
output_folder <-"M:/Masterarbeit/TLS/Scans_010_CHM/"


las_files <- fs::dir_ls(las_folder, regexp = "\\.laz$"); las_files


for (las_file in las_files){
  
  base = fs::path_ext_remove(fs::path_file(las_file))
                             
  Wolke <- readLAS(las_file)
  
  # rasterisieren
  Wolke_chm <- rasterize_canopy(Wolke,
                                res = 0.1,
                                algorithm = p2r(subcircle = 0.05),
                                pkg = "terra")
  
  pfad_output = paste0(output_folder, base, ".tif")
  
  # chm schreiben
  writeRaster(Wolke_chm,
              pfad_output,
              overwrite = TRUE)
  
  print(paste(base , "CHM finished"))
  
  }




#_______________________________________________________
# Create ForestGap Analysis
#_______________________________________________________

library(ForestGapR)
library(raster)
library(terra)
library(openxlsx)

chm_folder <- "M:/Masterarbeit/TLS/Scans_010_CHM"
output_folder <-"M:/Masterarbeit/TLS/Scans_010_Forest_Gaps/"

chm_files <- fs::dir_ls(chm_folder, regexp = "\\.tif$"); chm_files

# Parameter setzen

threshold <- 5     # Height threshold for gap detection, Default is 10m
size <- c(0.5,1000)   # Vector containing min and max gap size in m²

# Loop der Forest Gap Analys auf allen Tifs (CHM) durchführt
for (chm_file in chm_files){
  
  #basis extrahieren
  base = fs::path_ext_remove(fs::path_file(chm_file))
  #chm einlesen
  chm = raster(chm_file)
  
  #Forest Gaps berechnen
  ForestGap <- getForestGaps(chm_layer = chm,
                             threshold = threshold,
                             size = size)
  #Plotting chm
  plot(chm, main = base)
  Sys.sleep(3)   # Enough time to visually check the CHM
  #Adding Forest Gaps to plot
  plot(ForestGap, col="red" , add=TRUE, legend=FALSE)
  Sys.sleep(2)  # Enough time to visually check the Forest Gaps
  
  # Gap stats berechnen
  Wolke_gstats<-GapStats(gap_layer= ForestGap, chm_layer=chm)
  
  # Pfad konstruieren
  xlsx_pfad = paste0(output_folder,base,".xlsx")
  write.xlsx(Wolke_gstats, xlsx_pfad, overwrite = T)
  
}







