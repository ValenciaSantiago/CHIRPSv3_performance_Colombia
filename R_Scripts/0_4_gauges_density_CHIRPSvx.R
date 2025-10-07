

install.packages("terra")
library(R.utils)


dir_v2_global <- 'G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/0_Gauges_density/005' 
dir_v3_global <- 'G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/0_Gauges_density/005' 

files_gauge_v2 <- list.files(path=paste0(dir_v2_global,"/Global"),
                             pattern = ".tif",full.name = TRUE )
files_gauge_v2 <- grep("stn_density\\.(200[1-9]|20[1][0-9]|202[0-3])\\.",
                       files_gauge_v2, value = TRUE)


files_gauge_v3 <- list.files(path=paste0(dir_v3_global,"/Global"),
                             pattern = ".tif",full.name=TRUE)
files_gauge_v3 <- grep("v3\\.stn_density\\.(200[1-9]|20[1][0-9]|202[0-3])\\.",
                       files_gauge_v3, value = TRUE)




colombia_area <- ext(-80, -65., -5, 13)
for(i in 1:length(files_gauge_v3)){
  crop_file_v3 <- crop(rast(files_gauge_v3[i]),colombia_area)
  names(crop_file_v3)
  
  tmp_file <- tempfile(fileext = ".tif")
  gunzip(files_gauge_v2[i], destname = tmp_file, remove = FALSE,
         overwrite = TRUE)
  crop_file_v2 <- crop(rast(tmp_file),colombia_area)
  

  writeRaster(crop_file_v3,paste0(dir_v3_global,
                      "/Colombia/",names(crop_file_v3),".tif"),overwrite=TRUE)
  writeRaster(crop_file_v2,paste0(dir_v2_global,
                      "/Colombia/",substr(names(crop_file_v3),4,22),
                      ".tif"),overwrite=TRUE)
  
print(i)
}


#______________________________________________________________
# Load files mask Colombia
files_gauge_v2 <- list.files(path=paste0(dir_v2_global,"/Colombia"),
                             pattern = ".tif",full.name=TRUE)                                                                 
files_gauge_v3 <- list.files(path=paste0(dir_v3_global,"/Colombia"),
                             pattern = ".tif",full.name=TRUE)

files_gauge_v2_p1 <- grep("stn_density\\.(200[1-9]|20[1][0-1])\\.",
                       files_gauge_v2, value = TRUE)
files_gauge_v2_p2 <- grep("stn_density\\.(20[1][2-9]|202[0-3])\\.",
                          files_gauge_v2, value = TRUE)

files_gauge_v3_p1 <- grep("stn_density\\.(200[1-9]|20[1][0-1])\\.",
                          files_gauge_v3, value = TRUE)
files_gauge_v3_p2 <- grep("stn_density\\.(20[1][2-9]|202[0-3])\\.",
                          files_gauge_v3, value = TRUE)




mean_v2      <- mean(c(rast(files_gauge_v2)))
mean_v2_p1   <- mean(c(rast(files_gauge_v2_p1)))
mean_v2_p2   <- mean(c(rast(files_gauge_v2_p2)))
max_v2       <- max(c(rast(files_gauge_v2)))
sd_v2        <- app(c(rast(files_gauge_v2)), fun = sd, na.rm = TRUE)
gauges_df_v2 <- as.data.frame(c(rast(files_gauge_v2)))
gauges_df_v2 <- t(colSums(gauges_df_v2))
gauges_df_v2


mean_v3      <- mean(c(rast(files_gauge_v3)))
mean_v3_p1   <- mean(c(rast(files_gauge_v3_p1)))
mean_v3_p2   <- mean(c(rast(files_gauge_v3_p2)))
max_v3       <- max(c(rast(files_gauge_v3)))
sd_v3        <- app(c(rast(files_gauge_v3)), fun = sd, na.rm = TRUE)
gauges_df_v3 <- as.data.frame(c(rast(files_gauge_v3)))
gauges_df_v3 <- t(colSums(gauges_df_v3))
gauges_df_v3


# Save results
writeRaster(max_v2,paste0(dir_v2_global,"/max_chirpsv2_gauge_density_2001_2023.tif"))
writeRaster(mean_v2,paste0(dir_v2_global,"/mean_chirpsv2_gauge_density_2001_2023.tif"))
writeRaster(sd_v2,paste0(dir_v2_global,"/sd_chirpsv2_gauge_density_2001_2023.tif"))
writeRaster(mean_v2_p1,paste0(dir_v2_global,"/mean_p1_chirpsv2_gauge_density_2001_2011.tif"))
writeRaster(mean_v2_p2,paste0(dir_v2_global,"/mean_p2_chirpsv2_gauge_density_2012_2023.tif"))

writeRaster(max_v3,paste0(dir_v3_global,"/max_chirpsv3_gauge_density_2001_2023.tif"))
writeRaster(mean_v3,paste0(dir_v3_global,"/mean_chirpsv3_gauge_density_2001_2023.tif"))
writeRaster(sd_v3,paste0(dir_v3_global,"/sd_chirpsv3_gauge_density_2001_2023.tif"))
writeRaster(mean_v3_p1,paste0(dir_v3_global,"/mean_p1_chirpsv3_gauge_density_2001_2011.tif"))
writeRaster(mean_v3_p2,paste0(dir_v3_global,"/mean_p2_chirpsv3_gauge_density_2012_2023.tif"))


gauges_df <- data.frame(v2=gauges_df_v2[1,],
                        v3=gauges_df_v3[1,])
gauges_df$date <- seq(as.Date('2001-01-01'),as.Date('2023-12-01'),by='month')
gauges_df$id   <- seq(1,length(gauges_df$date),1)

tail(gauges_df)
fwrite(gauges_df,paste0(dir_v3_global,"/gauges_df.csv"))






