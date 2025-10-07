

#------------------------------
# libraries
require(terra)
require(utils)
require(terra)
require(base)
require(dplyr)
require(rvest)


# tiempo de esperar para la descarga
options(timeout = 1e800) 
timeout_seconds <- 1e800
#=================================================
#=================================================
#-------------------------------------------------
# download and clip CHIRPS-GEFS data
# open file
dir_global   <- 'D:/CHIRPS/CHIRPSv3_Daily_ERA5/Global'
dir_colombia <- 'D:/CHIRPS/CHIRPSv3_Daily_ERA5/Colombia'

# Ruta de la base de datos
chirps_url <- "https://data.chc.ucsb.edu/experimental/CHIRPS/v3.0/daily/final/ERA5/"
aoi_col <- terra::ext(-82, -64, -6, 14) # poligono sobre Colombia que se esta descargando


# FILE NAMES
dates_seq <- seq(as.Date("1981-01-01"), as.Date("2024-12-31"), by = "day") %>% # Modificar periodo de descarga
  as_tibble() %>%
  mutate(year = substr(value,1,4),
         month = substr(value,6,7),
         day = substr(value,9,10)) #%>%
 # filter((month != "02") | (day != "29")) 
years <- c(dates_seq$year)
month <- c(dates_seq$month)
days  <- c(dates_seq$day)

url <- paste0(chirps_url,years,"/chirps-v3.0.",years,".",month,".",days,".tif")
url[1]

filenames <- paste0("chirps-v3.0.",years,".",month,".",days,"_ERA5.tif")
filenames[1]


for(k in 2300:length(filenames)){
  #gc()
  #k <- 1
  # DOWNLOAD FILE     
  # Para descargar n dias de pronostico, cambiar length(links) por (e.g., 3) 
  #for(l in 1:3){ #-->  "data.2000.0110.tif" , "data.2000.0111.tif","data.2000.0112.tif"
  #--> Modificar
    #l <- 1
    #download.file(paste0(url_inp,'/',links[l],sep=''),paste('data_',filenames[l],'_created_',
    #                         filenames[1],'.tif',sep=''),method = 'wget',overwrite='TRUE',
    #                          timeout = timeout_seconds,progress=FALSE)
    download.file(url[k],paste0(dir_global,"/",filenames[k]),
                  method = 'wget',overwrite='TRUE',
                  timeout = timeout_seconds,progress=FALSE)
    
    # save to colombia area
    file_col_k <- crop(rast(paste0(dir_global,"/",filenames[k])),aoi_col)
    file_col_k[file_col_k==-9999] <- NA
    writeRaster(file_col_k, 
                filename = paste0(dir_colombia, "/", filenames[k]), 
                overwrite = TRUE)
    
  print(k)
}





#////////////////////////////////////////////////////////////////////////////////


# download and clip CHIRP pentad and monthly data
# open file
dir_global   <- 'C:/Users/santiagovalencia/Desktop/5_CHIRPv3/3_Pentad/Global'
dir_colombia <- 'C:/Users/santiagovalencia/Desktop/5_CHIRPv3/3_Pentad/Colombia'

# Ruta de la base de datos
chirps_url <- "https://data.chc.ucsb.edu/experimental/CHIRPS/v3.0/daily/final/ERA5/"
aoi_col <- terra::ext(-82, -64, -6, 14) # poligono sobre Colombia que se esta descargando


links_pentad <- fread('C:/Users/santiagovalencia/Desktop/5_CHIRPv3/links_pentad.txt',head=TRUE)
#links_pentad[1]




for(k in 1770:dim(links_pentad)[1]){
  # k <- 2
  filenames <- paste0("chirpv3_",substr(links_pentad[k],80,101),".tif")
  filenames
  
  url <- paste0("https://data.chc.ucsb.edu/products/EWX2/CHIRPv3p0/chirpv3p0_global_pentad_data/",
                substr(links_pentad[k],80,101))
  
  download.file(url,paste0(dir_global,"/",filenames),
                method = 'wget',overwrite='TRUE',
                timeout = timeout_seconds,progress=FALSE)
  
  
  # save to colombia area
  file_col_k <- crop(rast(paste0(dir_global,"/",filenames)),aoi_col)
  #file_col_k[file_col_k==-9999] <- NA
  writeRaster(file_col_k, 
              filename = paste0(dir_colombia, "/", filenames), 
              overwrite = TRUE)
  
  print(k)
}



#////////////////////////////////////////////////////////////////////////////////


# tiempo de esperar para la descarga
options(timeout = 1e800) 
timeout_seconds <- 1e800
# download and clip CHIRP pentad and monthly data
# open file
dir_global   <- 'C:/Users/santiagovalencia/Desktop/5_CHIRPv3/2_Monthly/Global'
dir_colombia <- 'C:/Users/santiagovalencia/Desktop/5_CHIRPv3/2_Monthly/Colombia'

# Ruta de la base de datos
chirps_url <- "https://data.chc.ucsb.edu/experimental/CHIRPS/v3.0/daily/final/ERA5/"
aoi_col <- terra::ext(-82, -64, -6, 14) # poligono sobre Colombia que se esta descargando


links_monthly <- fread('C:/Users/santiagovalencia/Desktop/5_CHIRPv3/links_monthly.txt',head=TRUE)
links_monthly[1]




for(k in 1:dim(links_monthly)[1]){
  # k <- 2
  filenames <- paste0("chirpv3_",substr(links_monthly[k],88,104),".tif")
  # filenames
  
  url <- paste0("https://data.chc.ucsb.edu/products/EWX2/CHIRPv3p0/chirpv3p0_global_1_monthly_data/data_",
                substr(links_monthly[k],88,104))
  
  download.file(url,paste0(dir_global,"/",filenames),
                method = 'wget',overwrite='TRUE',
                timeout = timeout_seconds,progress=FALSE)
  
  
  # save to colombia area
  file_col_k <- crop(rast(paste0(dir_global,"/",filenames)),aoi_col)
  #file_col_k[file_col_k==-9999] <- NA
  writeRaster(file_col_k, 
              filename = paste0(dir_colombia, "/", filenames), 
              overwrite = TRUE)
  
  print(k)
}



















