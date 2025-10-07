
library(terra)
setwd("G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/2_Monthly/Latam")
dir()

dir_col <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/2_Monthly/Colombia"

chirps_files <-  list.files(pattern = ".tif") 
chirps_files

chirps_filenames <- substr(chirps_files,1,19)
chirps_filenames

aoi_col <- ext(-81, -65., -5.00000, 13.5)

for(i in 1:length(chirps_files)){
  #i <- 1
  chirps_files_1981_2023 <- (rast(chirps_files[i]))
  chirps_files_1981_2023_crop <- crop(chirps_files_1981_2023,aoi_col )
  chirps_files_1981_2023_crop[chirps_files_1981_2023_crop<0] <- NA
  plot(chirps_files_1981_2023_crop)
  
  writeCDF(chirps_files_1981_2023_crop,
           paste0(dir_col,"/",chirps_filenames[i],".nc"),overwrite=TRUE)
  print(i)
}



setwd(dir_col)

chirps_files <-  list.files(pattern = ".nc") 
chirps_files

merge_file <- c(rast(chirps_files))
merge_file
writeCDF(merge_file,
         paste0(dir_col,"/","chirps-v3_1981_2024_Colombia.nc"),overwrite=TRUE)

#////////////////////////////////////////////////////////////////////////////
# CHIRPS3 Pentad
setwd("D:/CHIRPSv3-Pentad/Latam")

chirps_files <-  unique(list.files(pattern = ".tif"))
chirps_files

chirps_filenames <- substr(chirps_files,1,21)
chirps_filenames

aoi_col <- ext(-81, -65., -5.00000, 13.5)

chirps_files_1981_2023_stack <- c()
for(i in 1:length(chirps_files)){
  #i <- 1
  chirps_files_1981_2023 <- (rast(chirps_files[i]))
  chirps_files_1981_2023_crop <- crop(chirps_files_1981_2023,aoi_col )
  chirps_files_1981_2023_crop[chirps_files_1981_2023_crop<0] <- NA
  plot(chirps_files_1981_2023_crop)
  
  #chirps_files_1981_2023_stack <- c(chirps_files_1981_2023_crop,chirps_files_1981_2023_stack)
  
  writeCDF(chirps_files_1981_2023_crop,
           paste0("D:/CHIRPSv3-Pentad/colombia","/",chirps_filenames[i],".nc"),overwrite=TRUE)
  print(i)
}



dir_pentad_col <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/3_Pentad/Colombia"
setwd("D:/CHIRPSv3-Pentad/colombia")

chirps_files <-  unique(list.files(pattern = ".nc"))
chirps_files

writeCDF(c(rast(chirps_files)),
         paste0(dir_pentad_col,"/","chirps-v3_1981_2024_Colombia.nc"),overwrite=TRUE)



#===============================================================================
#CHIRPSv2
setwd("G:/My Drive/99_PhD/01_CH1_Valencia_Et_Al_Forest_Savanna_Transition/2_DATA/2_PRECIPITATION/CHIRPS/MONTHLY")
dir()

dir_col_v2 <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/2_Monthly"

chirps_files <-  list.files(pattern = ".nc") 
chirps_files

chirps_filenames <- substr(chirps_files,1,24)
chirps_filenames


aoi_col <- ext(-81, -65., -5.00000, 13.5)

for(i in 20:length(chirps_files)){
  #i <- 1
  chirps_files_1981_2023 <- (rast(chirps_files[i]))
  chirps_files_1981_2023_crop <- crop(chirps_files_1981_2023,aoi_col )
  #chirps_files_1981_2023_crop[chirps_files_1981_2023_crop<0] <- NA
  plot(chirps_files_1981_2023_crop[[1]])
  
  writeCDF(chirps_files_1981_2023_crop,
           paste0(dir_col_v2,"/",chirps_filenames[i],".nc"),overwrite=TRUE)
  print(i)
}



#===============================================================================
#CHIRPv2
dir_daily_chirp_col <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/4_CHIRPv2/1_Daily/Colombia"
setwd("C:/Users/santiagovalencia/Desktop/DATABASE/Latam")
dir()

chirp_files <-  list.files(pattern = ".nc") 
chirp_files

chirp_filenames <- substr(chirp_files,1,19)
chirp_filenames

aoi_col <- ext(-81, -65., -5.00000, 13.5)
for(i in 1:length(chirp_files)){  
  i <- 6
  chirp_files_1981_2023 <- (rast(chirp_files[i]))
  chirp_files_1981_2023_crop <- crop(chirp_files_1981_2023,aoi_col )
  #chirps_files_1981_2023_crop[chirps_files_1981_2023_crop<0] <- NA
  #plot(chirp_files_1981_2023_crop[[1]])
  
  writeCDF(chirp_files_1981_2023_crop,
           paste0(dir_daily_chirp_col,"/",chirp_filenames[i],".nc"),overwrite=TRUE)
  print(i)
}

# stack files
dir_daily_chirp_col_res <- "C:/Users/santiagovalencia/Desktop/DATABASE"
setwd(dir_daily_chirp_col)
dir()

chirp_files <-  list.files(pattern = ".nc") 
chirp_files

merge_file <- c(rast(chirp_files))
merge_file
writeCDF(merge_file,
         paste0(dir_daily_chirp_col_res,"/","chirp-v2_1994_2023_Colombia.nc"),
         overwrite=TRUE)



#===============================================================================
#CHIRPv2 - pentad
dir_daily_chirp_col <- "D:/2_CHIRPSv2/3_Pentad/Colombia"
setwd("D:/2_CHIRPSv2/3_Pentad/Global")

chirp_files <-  list.files(pattern = ".nc") 
chirp_files

chirp_filenames <- substr(chirp_files,1,24)
chirp_filenames

aoi_col <- ext(-81, -65., -5.00000, 13.5)
for(i in 9:length(chirp_files)){  
  i <- 12
  chirp_files_1981_2023 <- (rast(chirp_files[i]))
  chirp_files_1981_2023_crop <- crop(chirp_files_1981_2023,aoi_col )
  #chirps_files_1981_2023_crop[chirps_files_1981_2023_crop<0] <- NA
  #plot(chirp_files_1981_2023_crop[[1]])
  
  writeCDF(chirp_files_1981_2023_crop,
           paste0(dir_daily_chirp_col,"/",chirp_filenames[i],".nc"),overwrite=TRUE)
  print(i)
}






#===============================================================================
#Gauge density CHIRPSv3
dir_chirpsv3_gauge_density_global <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/0_Gauges_density/0_Global"
dir_chirpsv3_gauge_density_col    <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/0_Gauges_density/1_Colombia"

setwd(dir_chirpsv3_gauge_density_global)
gauge_density_files <-  list.files(pattern = ".tif") 
gauge_density_files

gauge_density_files_filenames <- substr(gauge_density_files,1,22)
gauge_density_files_filenames


aoi_col <- ext(-81, -65., -5.00000, 13.5)
for(i in 530:length(gauge_density_files)){
  #i <- 534
  gauge_density_files_i <- (rast(gauge_density_files[i]))
  gauge_density_files_i_crop <- crop(gauge_density_files_i,aoi_col )
  plot(gauge_density_files_i_crop)
  
  writeCDF(gauge_density_files_i_crop,
           paste0(dir_chirpsv3_gauge_density_col,"/",
                  gauge_density_files_filenames[i],"_Colombia.nc"),overwrite=TRUE)
  print(i)
}

setwd(dir_chirpsv3_gauge_density_col )
gauge_density_files <-  list.files(pattern = ".nc") 
gauge_density_files

gauge_density_1994_2023 <- c(rast(gauge_density_files[181:540]))
gauge_density_1994_2023

max_gauge_density_1994_2023  <- max(gauge_density_1994_2023, na.rm = TRUE)
mean_gauge_density_1994_2023 <- mean(gauge_density_1994_2023, na.rm = TRUE)
max_gauge_density_1994_2003  <- max(gauge_density_1994_2023[[1:120]], na.rm = TRUE)
mean_gauge_density_1994_2003 <- mean(gauge_density_1994_2023[[1:120]], na.rm = TRUE)
max_gauge_density_2004_2013  <- max(gauge_density_1994_2023[[121:240]], na.rm = TRUE)
mean_gauge_density_2004_2013 <- mean(gauge_density_1994_2023[[121:240]], na.rm = TRUE)
max_gauge_density_2014_2023  <- max(gauge_density_1994_2023[[241:360]], na.rm = TRUE)
mean_gauge_density_2014_2023 <- mean(gauge_density_1994_2023[[241:360]], na.rm = TRUE)


plot(max_gauge_density_1994_2023,breaks=c(0,1,2,3,4,5,10,20,50))
plot(mean_gauge_density_1994_2023,breaks=c(0,1,2,3,4,5,10,20,50))
plot(max_gauge_density_1994_2003,breaks=c(0,1,2,3,4,5,10,20,50))
plot(max_gauge_density_2004_2013,breaks=c(0,1,2,3,4,5,10,20,50))
plot(max_gauge_density_2014_2023,breaks=c(0,1,2,3,4,5,10,20,50))

dir_chirpsv3_gauge_density    <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/0_Gauges_density"
writeCDF(max_gauge_density_1994_2023,paste0(dir_chirpsv3_gauge_density,
                                            "/max_chirpsv3_gauge_density_1994_2023.nc"),overwrite=TRUE)
writeCDF(max_gauge_density_1994_2003,paste0(dir_chirpsv3_gauge_density,
                                            "/max_chirpsv3_gauge_density_1994_2003.nc"),overwrite=TRUE)
writeCDF(max_gauge_density_2004_2013,paste0(dir_chirpsv3_gauge_density,
                                            "/max_chirpsv3_gauge_density_2004_2013.nc"),overwrite=TRUE)
writeCDF(max_gauge_density_2014_2023,paste0(dir_chirpsv3_gauge_density,
                                            "/max_chirpsv3_gauge_density_2014_2023.nc"),overwrite=TRUE)

writeCDF(mean_gauge_density_1994_2023,paste0(dir_chirpsv3_gauge_density,
                                            "/mean_chirpsv3_gauge_density_1994_2023.nc"),overwrite=TRUE)
writeCDF(mean_gauge_density_1994_2003,paste0(dir_chirpsv3_gauge_density,
                                            "/mean_chirpsv3_gauge_density_1994_2003.nc"),overwrite=TRUE)
writeCDF(mean_gauge_density_2004_2013,paste0(dir_chirpsv3_gauge_density,
                                            "/mean_chirpsv3_gauge_density_2004_2013.nc"),overwrite=TRUE)
writeCDF(mean_gauge_density_2014_2023,paste0(dir_chirpsv3_gauge_density,
                                            "/mean_chirpsv3_gauge_density_2014_2023.nc"),overwrite=TRUE)


#===============================================================================
#Gauge density CHIRPSv2
dir_chirpsv2_gauge_density_global <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/0_Gauges_density/0_Global"
dir_chirpsv2_gauge_density_col    <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/0_Gauges_density/1_Colombia"

setwd(dir_chirpsv2_gauge_density_global)
gauge_density_files <-  list.files(pattern = ".tif") 
gauge_density_files

gauge_density_files_filenames <- substr(gauge_density_files,1,19)
gauge_density_files_filenames

aoi_col <- ext(-81, -65., -5.00000, 13.5)
for(i in 1:length(gauge_density_files)){
  #i <- 534
  gauge_density_files_i <- (rast(gauge_density_files[i]))
  gauge_density_files_i_crop <- crop(gauge_density_files_i,aoi_col )
  plot(gauge_density_files_i_crop)
  
  writeCDF(gauge_density_files_i_crop,
           paste0(dir_chirpsv2_gauge_density_col,"/",
                  gauge_density_files_filenames[i],"_Colombia.nc"),overwrite=TRUE)
  print(i)
}



setwd(dir_chirpsv2_gauge_density_col )
gauge_density_files <-  list.files(pattern = ".nc") 
gauge_density_files

gauge_density_1994_2023 <- c(rast(gauge_density_files[157:515]))
gauge_density_1994_2023

max_gauge_density_1994_2023  <- max(gauge_density_1994_2023, na.rm = TRUE)
mean_gauge_density_1994_2023 <- mean(gauge_density_1994_2023, na.rm = TRUE)
max_gauge_density_1994_2003  <- max(c(rast(gauge_density_files[157:276])), na.rm = TRUE)
mean_gauge_density_1994_2003 <- mean(c(rast(gauge_density_files[157:276])), na.rm = TRUE)
max_gauge_density_2004_2013  <- max(c(rast(gauge_density_files[277:396])), na.rm = TRUE)
mean_gauge_density_2004_2013 <- mean(c(rast(gauge_density_files[277:396])), na.rm = TRUE)
max_gauge_density_2014_2023  <- max(c(rast(gauge_density_files[397:515])), na.rm = TRUE)
mean_gauge_density_2014_2023 <- mean(c(rast(gauge_density_files[397:515])), na.rm = TRUE)

plot(mean_gauge_density_1994_2023,breaks=c(0,1,2,3,4,5,10,20,50))
plot(mean_gauge_density_1994_2003,breaks=c(0,1,2,3,4,5,10,20,50))
plot(mean_gauge_density_2004_2013,breaks=c(0,1,2,3,4,5,10,20,50))
plot(mean_gauge_density_2014_2023,breaks=c(0,1,2,3,4,5,10,20,50))

dir_chirpsv2_gauge_density    <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/0_Gauges_density"
writeCDF(max_gauge_density_1994_2023,paste0(dir_chirpsv2_gauge_density,
                                            "/max_chirpsv2_gauge_density_1994_2023.nc"),overwrite=TRUE)
writeCDF(max_gauge_density_1994_2003,paste0(dir_chirpsv2_gauge_density,
                                            "/max_chirpsv2_gauge_density_1994_2003.nc"),overwrite=TRUE)
writeCDF(max_gauge_density_2004_2013,paste0(dir_chirpsv2_gauge_density,
                                            "/max_chirpsv2_gauge_density_2004_2013.nc"),overwrite=TRUE)
writeCDF(max_gauge_density_2014_2023,paste0(dir_chirpsv2_gauge_density,
                                            "/max_chirpsv2_gauge_density_2014_2023.nc"),overwrite=TRUE)

writeCDF(mean_gauge_density_1994_2023,paste0(dir_chirpsv2_gauge_density,
                                            "/mean_chirpsv2_gauge_density_1994_2023.nc"),overwrite=TRUE)
writeCDF(mean_gauge_density_1994_2003,paste0(dir_chirpsv2_gauge_density,
                                            "/mean_chirpsv2_gauge_density_1994_2003.nc"),overwrite=TRUE)
writeCDF(mean_gauge_density_2004_2013,paste0(dir_chirpsv2_gauge_density,
                                            "/mean_chirpsv2_gauge_density_2004_2013.nc"),overwrite=TRUE)
writeCDF(mean_gauge_density_2014_2023,paste0(dir_chirpsv2_gauge_density,
                                            "/mean_chirpsv2_gauge_density_2014_2023.nc"),overwrite=TRUE)


#////////////////////////////////////////////////////////////////////////////////
# CHIRPv2 
dir_chirpv2_global <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/4_CHIRPv2/2_Monthly/0_Global"
dir_chirpv2_colombia <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/4_CHIRPv2/2_Monthly/1_Colombia"

setwd(dir_chirpv2_global)
chirp_files <-  list.files(pattern = ".tif") 
chirp_files

chirp_filenames <- substr(chirp_files,1,13)
chirp_filenames

aoi_col <- ext(-81, -65., -5.00000, 13.5)
for(i in 1:length(chirp_files)){
  #i <- 1
  chirp_files_i <- (rast(chirp_files[i]))
  chirp_files_i_crop <- crop(chirp_files_i,aoi_col )
  #chirps_files_1981_2023_crop[chirps_files_1981_2023_crop<0] <- NA
  plot(chirp_files_i_crop[[1]])
  
  writeCDF(chirp_files_i_crop,
           paste0(dir_chirpv2_colombia,"/",chirp_filenames[i],".nc"),overwrite=TRUE)
  print(i)
}


dir_chirpv2_col <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/4_CHIRPv2/2_Monthly"
setwd(dir_chirpv2_colombia)

chirp_files <-  list.files(pattern = ".nc") 
chirp_files

chirp_file <- c(rast(chirp_files[1:516]))
chirp_file
writeCDF(chirp_file,
         paste0(dir_chirpv2_col ,"/","chirp-v2_1981_2023_Colombia.nc"),overwrite=TRUE)


#///////////////////////////////////////////////////////////////////////////////
# global climatological gauges network

library(GHCNr)
library(terra)  # for handling countries geometries

setwd("C:/Users/santiagovalencia/Downloads")
inventory_file <- download_inventory("~/ghcn-inventory.txt")
stations <- stations(inventory_file, variables = "PRCP")

stations <- stations[stations$startYear <= 2000, ]
stations <- stations[stations$endYear >= 2022, ]
stations

italy <- get_country("USA")
stations <- filter_stations(stations, italy)
plot(italy)
points(stations[, c("longitude", "latitude")], pch = 20, col = "dodgerblue")














