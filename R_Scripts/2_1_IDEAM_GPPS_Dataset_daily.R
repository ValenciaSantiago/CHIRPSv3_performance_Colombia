

#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,
       dplyr,stringr,quantmod,rnaturalearth,exactextractr,hydroGOF,
       future.apply,profvis,rnaturalearthdata,glue)
terra::gdalCache(12000)

#//////////////////////////////////////////////////
# directories
ideam_pcp_data      <- 'G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/PCP_IDEAM_TIME_SERIES_QUALITY'
nat_reg_col         <- 'G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS'
dir_ideam_summary   <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023"
dir_chirpv2_day     <- "D:/4_CHIRPv2/1_Daily/Colombia"
dir_chirpsv2_day    <- "D:/2_CHIRPSv2/1_Daily"
#dir_chirpv3_day     <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/4_CHIRPv2/1_Daily"
#dir_chirpsv3_day    <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/1_Daily"
dir_chirpsv2_month  <- "D:/2_CHIRPSv2/2_Monthly"
dir_chirpsv3_month  <- "D:/3_CHIRPSv3/2_Monthly/Colombia"
dir_chirpv2_month   <- "D:/4_CHIRPv2/2_Monthly/1_Colombia"
dir_chirpv3_month   <- "D:/5_CHIRPv3/2_Monthly/Colombia"
dir_chirpsv2_st_den <- "D:/2_CHIRPSv2/0_Gauges_density"
dir_chirpsv3_st_den <- "D:/3_CHIRPSv3/0_Gauges_density"
dir_chirpsv2_st_den_005 <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/0_Gauges_density/005"
dir_chirpsv3_st_den_005 <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/0_Gauges_density/005"


dir_plots_chirpsvx  <- "G:/My Drive/R4C_et_al/3_PLOTS/0_Comparison_CHIRPSv2_v3"
dir_chirpsv2_pentad <- 'D:/2_CHIRPSv2/3_Pentad/Colombia'
dir_chirpsv3_pentad <-  'D:/3_CHIRPSv3/3_Pentad/Colombia'
dir_chirpv2_pentad  <- 'D:/4_CHIRPv2/3_Pentad/Colombia'
dir_chirpv3_pentad  <- 'D:/5_CHIRPv3/3_Pentad/Colombia'
dir_chirps_v3_day_era5  <- "D:/3_CHIRPSv3/1_Daily/CHIRPSv3_Daily_ERA5/Colombia"
dir_chirps_v3_day_imerg <- "D:/3_CHIRPSv3/1_Daily/CHIRPSv3_Daily_IMERG/Colombia"
dir_chirps_v3_cor_fact  <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/4_Correction_Factors"


#////////////////////////////////////////////////////////////////////////
# Save outputs
#dir_results_daily   <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/1_Daily'
#dir_results_pentad  <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/2_Pentad'
#dir_results_month   <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/3_Monthly'
#dir_results_dry_sea <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/4_1_Dry_Season'
#dir_results_wet_sea <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/4_2_Wet_Season'
#dir_results_annual  <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/5_Annual'

dir_results_daily   <- 'D:/4_IDEAM_GPPs/1_Daily'
dir_results_pentad  <- 'D:/4_IDEAM_GPPs/2_Pentad'
dir_results_month   <- 'D:/4_IDEAM_GPPs/3_Monthly'
dir_results_dry_sea <- 'D:/4_IDEAM_GPPs/4_1_Dry_Season'
dir_results_wet_sea <- 'D:/4_IDEAM_GPPs/4_2_Wet_Season'
dir_results_annual  <- 'D:/4_IDEAM_GPPs/5_Annual'



#///////////////////////////////////////////////////////////////////////////////
# load IDEAM gauges summary
ideam_gauges_metadata <- read.csv2(paste0(dir_ideam_summary ,
                                  "/summary_IDEAM_gauges_2001_2023_10p_v2.csv"),sep=",",head=TRUE)
colnames(ideam_gauges_metadata)

# start and end study period
start_date <- "1994-01-01"
end_date   <- "2023-12-31"
mis_values_threshold  <- 10 # selected threshold
ideam_gauges_metadata      <- filter(ideam_gauges_metadata,as.numeric(p_sd_q)<=10)
ideam_gauges_metadata      <- filter(ideam_gauges_metadata,
                                state!="Archipiélago de San Andres, Providencia y Santa Catalina")
ideam_gauges_metadata$lat  <- as.numeric(ideam_gauges_metadata$lat)
ideam_gauges_metadata$long <- as.numeric(ideam_gauges_metadata$long)
ideam_gauges_coord         <- vect(ideam_gauges_metadata[,c(33,32)], geom = c("long", "lat"))
ideam_gauges_coord         <- buffer(ideam_gauges_coord, 0.000000000001)
crs(ideam_gauges_coord)    <- "EPSG:4326"
plot(ideam_gauges_coord)


#/////////////////////////////////////////////////////////////////////
# Load daily GPPs data
daily_chirpsv2        <- c(rast(paste0(dir_chirpsv2_day,"/","Daily_CHIRPSv2_1990_2005_Colombia.nc")),
                           rast(paste0(dir_chirpsv2_day,"/","Daily_CHIRPSv2_2006_2023_Colombia.nc")))
names(daily_chirpsv2) <- paste0("pcp_", seq_along(names(daily_chirpsv2)))
#daily_chirpsv2        <- daily_chirpsv2[[1462:12418]] # filter study period --> 1994 - 2023
dates <- time(daily_chirpsv2)
start_date <- as.Date("2001-01-01")
end_date   <- as.Date("2023-12-31")
date_filter <- dates >= start_date & dates <= end_date
daily_chirpsv2       <- daily_chirpsv2[[date_filter]]


# CHIRPv2
setwd(dir_chirpv2_day)
daily_chirpv2_files   <- list.files(pattern = ".nc")
daily_chirpv2         <- c(rast(daily_chirpv2_files[8:30]))
daily_chirpv2

chirps_v3_day_era5_files  <- list.files(path = dir_chirps_v3_day_era5,
                                        pattern = "\\.tif$", full.names = TRUE)
chirps_v3_day_imerg_files <- list.files(path =
                                          dir_chirps_v3_day_imerg, pattern = "\\.tif$", full.names = TRUE)

# Filter for years 2001 to 2023 using regex
chirps_v3_day_era5_files_2001_2023 <- chirps_v3_day_era5_files[
  grepl("200[1-9]|201[0-9]|202[0-3]", chirps_v3_day_era5_files)]

chirps_v3_day_imerg_files_2001_2023 <- chirps_v3_day_imerg_files[
  grepl("200[1-9]|201[0-9]|202[0-3]", chirps_v3_day_imerg_files)]

# Sort to ensure correct date order
chirps_v3_day_era5_files_2001_2023 <- sort(chirps_v3_day_era5_files_2001_2023)
chirps_v3_day_imerg_files_2001_2023 <- sort(chirps_v3_day_imerg_files_2001_2023)

# Read the rasters
setwd('C:/Users/santiagovalencia/Desktop')
daily_chirpsv3_era5 <- rast(chirps_v3_day_era5_files_2001_2023)
daily_chirpsv3_imerg <- rast(chirps_v3_day_imerg_files_2001_2023)


setwd(dir_chirps_v3_day_imerg)
#daily_chirpsv3_imerg           <- c(rast(chirps_v3_day_imerg_files))
daily_chirpsv3_imerg <- c(rast(chirps_v3_day_imerg_files_2001_2023))
#daily_chirpsv3_imerg            <- c(daily_chirpsv3_era5_1994_2000,
#                                     daily_chirpsv3_imerg_2001_2023)


gauges_vect <- (st_as_sf(ideam_gauges_coord))
daily_chirpv2_df        <- exact_extract(daily_chirpv2,gauges_vect,'mean')
daily_chirpsv2_df       <- exact_extract(daily_chirpsv2,gauges_vect,'mean')
daily_chirpsv3_era5_df  <- exact_extract(daily_chirpsv3_era5,gauges_vect,'mean')
daily_chirpsv3_imerg_df <- exact_extract(daily_chirpsv3_imerg,gauges_vect,'mean')



# combine data
# ////////////////////////////////////////////////////////////////////////
# load pentad GPPs data -- 2001-2023 period
chirpsv2_pentad_files  <- list.files(path=dir_chirpsv2_pentad,
                                     pattern = ".nc",full.name=TRUE)
pentad_chirpsv2        <- c(rast(chirpsv2_pentad_files[16:38]))
pentad_chirpsv2

chirpsv3_pentad_files  <- list.files(path=dir_chirpsv3_pentad,
                                     pattern = ".nc",full.name=TRUE)
chirpsv3_pentad_files <- chirpsv3_pentad_files[grepl("200[1-9]|201[0-9]|202[0-3]",
                                                     chirpsv3_pentad_files)]
pentad_chirpsv3        <- c(rast(chirpsv3_pentad_files))
pentad_chirpsv3

chirpv2_pentad_files  <- list.files(path=dir_chirpv2_pentad,pattern = ".nc",
                                    full.name=TRUE)
chirpv2_pentad_files <- chirpv2_pentad_files[grepl("200[1-9]|201[0-9]|202[0-3]",
                                                   chirpv2_pentad_files)]
pentad_chirpv2        <- c(rast(chirpv2_pentad_files))
pentad_chirpv2


chirpv3_pentad_files  <- list.files(path=dir_chirpv3_pentad,pattern = ".tif",
                                    full.name=TRUE)
chirpv3_pentad_files <- chirpv3_pentad_files[grepl("200[1-9]|201[0-9]|202[0-3]",
                                                   chirpv3_pentad_files)]
pentad_chirpv3        <- c(rast(chirpv3_pentad_files))
pentad_chirpv3

pentad_chirpv2_df    <- exact_extract(pentad_chirpv2,st_as_sf(ideam_gauges_coord),'mean')
pentad_chirpsv2_df   <- exact_extract(pentad_chirpsv2,st_as_sf(ideam_gauges_coord),'mean')
pentad_chirpv3_df    <- exact_extract(pentad_chirpv3,st_as_sf(ideam_gauges_coord),'mean')
pentad_chirpsv3_df   <- exact_extract(pentad_chirpsv3,st_as_sf(ideam_gauges_coord),'mean')



#/////////////////////////////////////////////////////////////////////
# Load monthly GPPs data
#setwd(dir_chirpsv2_month)
chirpsv2_month_files  <- list.files(path=dir_chirpsv2_month,pattern = ".nc",
                                    full.name=TRUE)
chirpsv2_month_files <- chirpsv2_month_files[grepl("0.200[1-9]|201[0-9]|0.202[0-3]",
                                                   chirpsv2_month_files)]
month_chirpsv2        <- c(rast(chirpsv2_month_files))
month_chirpsv2

#setwd(dir_chirpsv3_month )
chirpsv3_month_files  <- list.files(path=dir_chirpsv3_month,pattern = ".nc",
                                    full.name=TRUE)
chirpsv3_month_files <- chirpsv3_month_files[grepl("200[1-9]|201[0-9]|202[0-3]",
                                                   chirpsv3_month_files)]
month_chirpsv3        <- c(rast(chirpsv3_month_files))
month_chirpsv3 

#setwd(dir_chirpv2_month)
chirpv2_month_files  <- list.files(path=dir_chirpv2_month,pattern = ".nc",
                                   full.name=TRUE)
chirpv2_month_files <- chirpv2_month_files[grepl("200[1-9]|201[0-9]|202[0-3]",
                                                 chirpv2_month_files)]
month_chirpv2        <- c(rast(chirpv2_month_files))
month_chirpv2

#setwd(dir_chirpv3_month)
chirpv3_month_files  <- list.files(path=dir_chirpv3_month,pattern = ".tif",
                                   full.name=TRUE)
chirpv3_month_files <- chirpv3_month_files[grepl("200[1-9]|201[0-9]|202[0-3]",
                                                 chirpv3_month_files)]
month_chirpv3        <- c(rast(chirpv3_month_files))
month_chirpv3

month_chirpv2_df    <- exact_extract(month_chirpv2,st_as_sf(ideam_gauges_coord),'mean')
month_chirpsv2_df   <- exact_extract(month_chirpsv2,st_as_sf(ideam_gauges_coord),'mean')
month_chirpv3_df    <- exact_extract(month_chirpv3,st_as_sf(ideam_gauges_coord),'mean')
month_chirpsv3_df   <- exact_extract(month_chirpsv3,st_as_sf(ideam_gauges_coord),'mean')


#/////////////////////////////////////////////////////////////////////
# load CHIRPSv2 and CHIRPSv3 gauges density at 0.25 and 0.05 degrees
#setwd('C:/Users/santiagovalencia/Desktop')
max_gauge_density_1994_2023_v2  <- rast(paste0(dir_chirpsv2_st_den,"/",
                                              "max_chirpsv2_gauge_density_2001_2003.tif"))
max_gauge_density_1994_2023_v3  <- rast(paste0(dir_chirpsv3_st_den,"/",
                                              "max_chirpsv3_gauge_density_2001_2003.tif"))
mean_gauge_density_1994_2023_v2 <- rast(paste0(dir_chirpsv2_st_den,"/",
                                              "mean_chirpsv2_gauge_density_2001_2003.tif"))
mean_gauge_density_1994_2023_v3  <- rast(paste0(dir_chirpsv3_st_den,"/",
                                              "mean_chirpsv3_gauge_density_2001_2003.tif"))


max_gauge_density_1994_2023_v2_005  <- rast(paste0(dir_chirpsv2_st_den_005,"/",
                                               "max_chirpsv2_gauge_density_2001_2023.tif"))
max_gauge_density_1994_2023_v3_005  <- rast(paste0(dir_chirpsv3_st_den_005,"/",
                                               "max_chirpsv3_gauge_density_2001_2023.tif"))
mean_gauge_density_1994_2023_v2_005 <- rast(paste0(dir_chirpsv2_st_den_005,"/",
                                               "mean_chirpsv2_gauge_density_2001_2023.tif"))
mean_gauge_density_1994_2023_v3_005  <- rast(paste0(dir_chirpsv3_st_den_005,"/",
                                                "mean_chirpsv3_gauge_density_2001_2023.tif"))



#/////////////////////////////////////////////////////////////////
# CHIRPSv3 correctopm factor
#setwd(dir_chirps_v3_cor_fact)
cor_factor_v3 <- c(rast(list.files(path=dir_chirps_v3_cor_fact,
                                   pattern = ".tif",full.name=TRUE)))
plot(cor_factor_v3[[1]])

#///////////////////////////////////////////////////////

# extract daily precipitation for all pixels / gauges
#daily_chirpv2_df        <- exact_extract(daily_chirpv2,st_as_sf(ideam_gauges_coord),'mean')
#daily_chirpsv2_df       <- exact_extract(daily_chirpsv2,st_as_sf(ideam_gauges_coord),'mean')
#daily_chirpsv3_era5_df  <- exact_extract(daily_chirpsv3_era5,st_as_sf(ideam_gauges_coord),'mean')
#daily_chirpsv3_imerg_df <- exact_extract(daily_chirpsv3_imerg,st_as_sf(ideam_gauges_coord),'mean')


#fwrite(daily_chirpv2_df,paste0("G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/",
#               "daily_chirpv2_df.csv"))
#head(ideam_gauges_metadata)
library(lubridate)
pentad <- function(date) {
  day_of_month <- mday(date)
  temp_pentad <- 6 * month(date) - 5 + (day_of_month > 5) + (day_of_month > 10) + (day_of_month > 15) + (day_of_month > 20) + (day_of_month > 25)
  return(temp_pentad)
}



terra::gdalCache(1000000)
# merge IDEAM and GPPs at daily,3-day, monthly, seasonal, and annual scales
for(j in 1:length(ideam_gauges_metadata[,1])){  #39
#j <- 10
#gc()

pcp_daily_i  <- as.data.frame(lapply(paste0(paste0(ideam_pcp_data,"/PCP_DAILY_",
                               ideam_gauges_metadata$gauge_code[j],
                               "_1990-01-01_2023-12-31.csv")),read.csv))

pcp_daily_i         <- filter(pcp_daily_i,date>=start_date & date<=end_date)
pcp_daily_i$nat_reg <- ideam_gauges_metadata$region[j] # add natural region
pcp_daily_i$day     <- substr(pcp_daily_i$date,9,10)
pcp_daily_i$month   <- substr(pcp_daily_i$date,6,7)
pcp_daily_i$year    <- substr(pcp_daily_i$date,1,4)
pcp_daily_i         <- pcp_daily_i[!duplicated(pcp_daily_i$date), ]
#head(pcp_daily_i)

ds <- 4*sd(pcp_daily_i$pcp_ideam,na.rm=TRUE)
pcp_daily_i$pcp_ideam_flag <- pcp_daily_i$pcp_ideam
pcp_daily_i$pcp_ideam_flag <- ifelse(pcp_daily_i$pcp_ideam_flag>ds & 
                                       pcp_daily_i$flag_ind=="Preliminar",
                                       NA,pcp_daily_i$pcp_ideam_flag)
pcp_daily_i$na_count_ideam <- ifelse(is.na(pcp_daily_i$pcp_ideam_flag),1,0)



pcp_chirpv2        <- t(daily_chirpv2_df[j,]);colnames(pcp_chirpv2)   <- c("chirpv2")
pcp_chirpv3        <- t(daily_chirpv2_df[j,]);colnames(pcp_chirpv3)   <- c("chirpv3")
pcp_chirpsv2       <- t(daily_chirpsv2_df[j,]);colnames(pcp_chirpsv2) <- c("chirpsv2")
pcp_chirpsv3_era5  <- t(daily_chirpsv3_era5_df[j,]);colnames(pcp_chirpsv3_era5) <- c("chirpsv3_era5")
pcp_chirpsv3_imerg <- t(daily_chirpsv3_imerg_df[j,]);colnames(pcp_chirpsv3_imerg) <- c("chirpsv3_imerg")

pcp_daily_i$chirpv2        <- pcp_chirpv2
pcp_daily_i$chirpv3        <- pcp_chirpv3
pcp_daily_i$chirpsv2       <- pcp_chirpsv2
pcp_daily_i$chirpsv3_era5  <- pcp_chirpsv3_era5
pcp_daily_i$chirpsv3_imerg <- pcp_chirpsv3_imerg
#head(pcp_daily_i)

# number of gauges
pcp_daily_i$mean_gauges_v2 <- t(exact_extract(mean_gauge_density_1994_2023_v2,st_as_sf(ideam_gauges_coord[j]),'max'))[,1]
pcp_daily_i$mean_gauges_v3 <- t(exact_extract(mean_gauge_density_1994_2023_v3,st_as_sf(ideam_gauges_coord[j]),'max'))[,1]
pcp_daily_i$max_gauges_v2  <- t(exact_extract(max_gauge_density_1994_2023_v2,st_as_sf(ideam_gauges_coord[j]),'max'))[,1]
pcp_daily_i$max_gauges_v3  <- t(exact_extract(max_gauge_density_1994_2023_v3,st_as_sf(ideam_gauges_coord[j]),'max'))[,1]
pcp_daily_i$mean_gauges_v2_005 <- t(exact_extract(mean_gauge_density_1994_2023_v2_005,st_as_sf(ideam_gauges_coord[j]),'max'))[,1]
pcp_daily_i$mean_gauges_v3_005 <- t(exact_extract(mean_gauge_density_1994_2023_v3_005,st_as_sf(ideam_gauges_coord[j]),'max'))[,1]
pcp_daily_i$max_gauges_v2_005  <- t(exact_extract(max_gauge_density_1994_2023_v2_005,st_as_sf(ideam_gauges_coord[j]),'max'))[,1]
pcp_daily_i$max_gauges_v3_005  <- t(exact_extract(max_gauge_density_1994_2023_v3_005,st_as_sf(ideam_gauges_coord[j]),'max'))[,1]


#pentad_seq                 <- rep(seq(1,10960/5,1),each=5)
pcp_daily_i$pentad_id      <- pentad(as.Date(pcp_daily_i$date))
#head(pcp_daily_i)
#tail(pcp_daily_i)

# Scale factos
cor_fact_df           <- as.data.frame(t(exact_extract(cor_factor_v3,st_as_sf(ideam_gauges_coord[j]),'mean')))
rownames(cor_fact_df ) <- seq(1,12,1)
colnames(cor_fact_df) <- c("factor")
cor_fact_df$month     <- unique(pcp_daily_i$month)


pcp_daily_i <- merge(pcp_daily_i ,cor_fact_df,by="month",all.x=TRUE)
pcp_daily_i$pcp_ideam_flag_scale <- pcp_daily_i$pcp_ideam_flag * 
                                    pcp_daily_i$factor


#/////////////////////////////////////////////////////////////////////////
# 5-days (pentad) scale
pcp_pentad_i <- pcp_daily_i %>%
  # mutate(year = year(date), 
  #       month = month(date)) %>%
  group_by(gauge_code,category,tecnology, state,municipality,
           latitude,
           longitude, elevation,instalation_date, suspention_date,
           nat_region,hidrographic_area,
           hidrographic_zone, hidrographic_subzone,
           mean_gauges_v2,mean_gauges_v3,
           max_gauges_v2,max_gauges_v3,
           mean_gauges_v2_005,mean_gauges_v3_005,
           max_gauges_v2_005,max_gauges_v3_005,factor,
           year,month,pentad_id) %>%
  summarise(
    pcp_ideam = sum(pcp_ideam, na.rm = TRUE),    # Summing the precipitation, ignoring NAs
    pcp_ideam_flag = sum(pcp_ideam_flag, na.rm = TRUE),
    pcp_ideam_flag_scale = sum(pcp_ideam_flag_scale, na.rm = TRUE),   
    na_count_ideam = sum(na_count_ideam),  # Counting the number of NAs in each group
    chirpv2  = sum(chirpv2, na.rm = TRUE),
    #chirpv3  = sum(chirpv3, na.rm = TRUE),
    #chirpsv2 = sum(chirpsv2, na.rm = TRUE),
    #chirpsv3_era5 = sum(chirpsv3_era5, na.rm = TRUE),
    #chirpsv3_imerg = sum(chirpsv3_imerg, na.rm = TRUE),
    .groups = "drop"                   # Drops the grouping after summarization
  ) %>%
  mutate(date = as.Date(paste0(year,"-",month,"-","01"))) %>%
  arrange(date)      # Orders by date
#pcp_pentad_i$date <- seq(from = as.Date("1994-01-01"), to = as.Date("2023-12-31"), by = "5 days")
#hist(pcp_pentad_i$na_count_ideam)
#View(pcp_pentad_i)


pcp_chirpv2        <- t(pentad_chirpv2_df[j,]);colnames(pcp_chirpv2)   <- c("chirpv2")
pcp_chirpv3        <- t(pentad_chirpv3_df[j,]);colnames(pcp_chirpv3)   <- c("chirpv3")
pcp_chirpsv2       <- t(pentad_chirpsv2_df[j,]);colnames(pcp_chirpsv2) <- c("chirpsv2")
pcp_chirpsv3       <- t(pentad_chirpsv3_df[j,]);colnames(pcp_chirpsv3) <- c("chirpsv3")

pcp_pentad_i$chirpv2        <- pcp_chirpv2
pcp_pentad_i$chirpv3        <- pcp_chirpv3
pcp_pentad_i$chirpsv2       <- pcp_chirpsv2
pcp_pentad_i$chirpsv3       <- pcp_chirpsv3
#View(pcp_pentad_i)


#/////////////////////////////////////////////////////////////////////////
# monthly precipitation
pcp_monthly_i <- pcp_daily_i %>%
 # mutate(year = year(date), 
  #       month = month(date)) %>%
  group_by(gauge_code,category,tecnology, state,municipality,
           latitude,
           longitude, elevation,instalation_date, suspention_date,
           nat_region,hidrographic_area,
           hidrographic_zone, hidrographic_subzone,
           mean_gauges_v2,mean_gauges_v3,
           max_gauges_v2,max_gauges_v3,
           mean_gauges_v2_005,mean_gauges_v3_005,
           max_gauges_v2_005,max_gauges_v3_005,factor,
           year,month) %>%
  summarise(
    pcp_ideam = sum(pcp_ideam, na.rm = TRUE),    # Summing the precipitation, ignoring NAs
    pcp_ideam_flag = sum(pcp_ideam_flag, na.rm = TRUE),
    pcp_ideam_flag_scale = sum(pcp_ideam_flag_scale, na.rm = TRUE),   
    na_count_ideam = sum(na_count_ideam),  # Counting the number of NAs in each group
    chirpv2  = sum(chirpv2, na.rm = TRUE),
    #chirpv3 = sum(chirpv3, na.rm = TRUE),
    #chirpsv2 = sum(chirpsv2, na.rm = TRUE),
    #chirpsv3 = sum(chirpsv3, na.rm = TRUE),
    .groups = "drop"                   
  ) %>%
  mutate(date = as.Date(paste0(year,"-",month,"-","01"))) %>%
  arrange(date)      # Orders by date
#View(pcp_monthly_i)
#hist(pcp_monthly_i$na_count_ideam)


pcp_chirpv2        <- t(month_chirpv2_df[j,])
colnames(pcp_chirpv2)   <- c("chirpv2")
pcp_chirpv3        <- t(month_chirpv3_df[j,])
colnames(pcp_chirpv3)   <- c("chirpv3")
pcp_chirpsv2       <- t(month_chirpsv2_df[j,])
colnames(pcp_chirpsv2) <- c("chirpsv2")
pcp_chirpsv3       <- t(month_chirpsv3_df[j,])
colnames(pcp_chirpsv3) <- c("chirpsv3")


pcp_monthly_i$chirpv2        <- pcp_chirpv2
pcp_monthly_i$chirpv3        <- pcp_chirpv3
pcp_monthly_i$chirpsv2       <- pcp_chirpsv2
pcp_monthly_i$chirpsv3       <- pcp_chirpsv3
#head(pcp_monthly_i)
#View(pcp_monthly_i)
#sapply(pcp_monthly_i,length)



# annual cycle --> identify the 3 driest and wettest months
pcp_mean_monthly <- pcp_monthly_i %>%
  # mutate(year = year(date), 
  #       month = month(date)) %>%
  group_by(gauge_code,
           month) %>%
  summarise(
    pcp_ideam = mean(pcp_ideam, na.rm = TRUE),    
    .groups = "drop")


pcp_mean_monthly
pcp_mean_monthly$pcp_ideam

wettest_months <- pcp_mean_monthly %>% arrange(desc(pcp_ideam)) %>% head(3) %>% arrange(month) 
driest_months  <- pcp_mean_monthly %>% arrange(pcp_ideam) %>% head(3) %>% arrange(month) 


#/////////////////////////////////////////////////////////////////////////
# dry & wet season
pcp_dry_season_i <- pcp_monthly_i %>% filter(month %in% c(driest_months$month))
pcp_wet_season_i <- pcp_monthly_i %>% filter(month %in% c(wettest_months$month))
pcp_dry_season_i$ID <- rep(seq(1,dim(pcp_dry_season_i)[1]/3,1),each=3)
pcp_wet_season_i$ID <- rep(seq(1,dim(pcp_wet_season_i)[1]/3,1),each=3)


pcp_wet_season_acum_i <- pcp_wet_season_i %>%
          # mutate(year = year(date), 
          #       month = month(date)) %>%
          group_by(gauge_code,category,tecnology, state,municipality,
                   latitude,
                   longitude, elevation,instalation_date, suspention_date,
                   nat_region,hidrographic_area,
                   hidrographic_zone, hidrographic_subzone,
                   mean_gauges_v2,mean_gauges_v3,
                   max_gauges_v2,max_gauges_v3,
                   mean_gauges_v2_005,mean_gauges_v3_005,
                   max_gauges_v2_005,max_gauges_v3_005,
                   #factor,
                   ID) %>%
          summarise(
            pcp_ideam = sum(pcp_ideam, na.rm = TRUE),    # Summing the precipitation, ignoring NAs
            pcp_ideam_flag = sum(pcp_ideam_flag, na.rm = TRUE),
            pcp_ideam_flag_scale = sum(pcp_ideam_flag_scale, na.rm = TRUE),   
            na_count_ideam = sum(na_count_ideam),  # Counting the number of NAs in each group
            chirpv2  = sum(chirpv2, na.rm = TRUE),  # Counting the number of NAs in each group
            chirpv3  = sum(chirpv3, na.rm = TRUE),  # Counting the number of NAs in each group
            chirpsv2  = sum(chirpsv2, na.rm = TRUE),  # Counting the number of NAs in each group
            chirpsv3  = sum(chirpsv3, na.rm = TRUE),  # Counting the number of NAs in each group
            .groups = "drop"                   # Drops the grouping after summarization
          )
pcp_wet_season_acum_i$year <- seq(2001,2023,1)


pcp_dry_season_acum_i <- pcp_dry_season_i %>%
                    # mutate(year = year(date), 
                    #       month = month(date)) %>%
                    group_by(gauge_code,category,tecnology, state,municipality,
                             latitude,
                             longitude, elevation,instalation_date, suspention_date,
                             nat_region,hidrographic_area,
                             hidrographic_zone, hidrographic_subzone,
                             mean_gauges_v2,mean_gauges_v3,
                             max_gauges_v2,max_gauges_v3,
                             mean_gauges_v2_005,mean_gauges_v3_005,
                             max_gauges_v2_005,max_gauges_v3_005,
                             ID) %>%
                    summarise(
                      pcp_ideam = sum(pcp_ideam, na.rm = TRUE),    # Summing the precipitation, ignoring NAs
                      pcp_ideam_flag = sum(pcp_ideam_flag, na.rm = TRUE),
                      pcp_ideam_flag_scale = sum(pcp_ideam_flag_scale, na.rm = TRUE),   
                      na_count_ideam = sum(na_count_ideam),  # Counting the number of NAs in each group
                      chirpv2  = sum(chirpv2, na.rm = TRUE),  # Counting the number of NAs in each group
                      chirpv3  = sum(chirpv3, na.rm = TRUE),  # Counting the number of NAs in each group
                      chirpsv2  = sum(chirpsv2, na.rm = TRUE),  # Counting the number of NAs in each group
                      chirpsv3  = sum(chirpsv3, na.rm = TRUE),  # Counting the number of NAs in each group
                      .groups = "drop"                   # Drops the grouping after summarization
                    )
pcp_dry_season_acum_i$year <- seq(2001,2023,1)




#/////////////////////////////////////////////////////////////////////////
# annual precipitation
pcp_annual_i <- pcp_monthly_i %>%
  # mutate(year = year(date), 
  #       month = month(date)) %>%
  group_by(gauge_code,category,tecnology, state,municipality,
           latitude,
           longitude, elevation,instalation_date, suspention_date,
           nat_region,hidrographic_area,
           hidrographic_zone, hidrographic_subzone,
           mean_gauges_v2,mean_gauges_v3,
           max_gauges_v2,max_gauges_v3,mean_gauges_v2_005,mean_gauges_v3_005,
           max_gauges_v2_005,max_gauges_v3_005,
           year) %>%
  summarise(
    pcp_ideam = sum(pcp_ideam, na.rm = TRUE),    # Summing the precipitation, ignoring NAs
    pcp_ideam_flag = sum(pcp_ideam_flag, na.rm = TRUE),
    pcp_ideam_flag_scale = sum(pcp_ideam_flag_scale, na.rm = TRUE),   
    na_count_ideam = sum(na_count_ideam),  # Counting the number of NAs in each group
    chirpv2  = sum(chirpv2, na.rm = TRUE),  # Counting the number of NAs in each group
    chirpv3  = sum(chirpv3, na.rm = TRUE),  # Counting the number of NAs in each group
    chirpsv2  = sum(chirpsv2, na.rm = TRUE),  # Counting the number of NAs in each group
    chirpsv3  = sum(chirpsv3, na.rm = TRUE),  # Counting the number of NAs in each group
    .groups = "drop"                   # Drops the grouping after summarization
  ) 
#View(pcp_annual_i)
#hist(pcp_annual_i$na_count_ideam)



# Daily
fwrite(pcp_daily_i,paste0(dir_results_daily,"/",
                            paste0("PCP_DAILY_IDEAM_GPPs_",
                            ideam_gauges_metadata$gauge_code[j],"_2001_2023.csv")))
# Pentad
fwrite(pcp_pentad_i,paste0(dir_results_pentad,"/",
                          paste0("PCP_PENTAD_IDEAM_GPPs_",
                          ideam_gauges_metadata$gauge_code[j],"_2001_2023.csv")))
# Month
fwrite(pcp_monthly_i,paste0(dir_results_month,"/",
                            paste0("PCP_MONTHLY_IDEAM_GPPs_",
                            ideam_gauges_metadata$gauge_code[j],"_2001_2023.csv")))

# Dry and wet season
fwrite(pcp_dry_season_i,paste0(dir_results_dry_sea,"/",
                            paste0("PCP_DRY_SEASON_IDEAM_GPPs_",
                            ideam_gauges_metadata$gauge_code[j],"_2001_2023.csv")))
fwrite(pcp_wet_season_i,paste0(dir_results_wet_sea,"/",
                            paste0("PCP_WET_SEASON_IDEAM_GPPs_",
                            ideam_gauges_metadata$gauge_code[j],"_2001_2023.csv")))

# Dry and wet season accumulate
fwrite(pcp_dry_season_acum_i,paste0(dir_results_dry_sea,"/",
                               paste0("PCP_DRY_SEASON_ACUM_IDEAM_GPPs_",
                                      ideam_gauges_metadata$gauge_code[j],"_2001_2023.csv")))
fwrite(pcp_wet_season_acum_i,paste0(dir_results_wet_sea,"/",
                               paste0("PCP_WET_SEASON_ACUM_IDEAM_GPPs_",
                                      ideam_gauges_metadata$gauge_code[j],"_2001_2023.csv")))

# Annual
fwrite(pcp_annual_i,paste0(dir_results_annual,"/",
                           paste0("PCP_ANNUAL_IDEAM_GPPs_",
                           ideam_gauges_metadata$gauge_code[j],"_2001_2023.csv")))

print(j)
}



#//////////////////////////////////////////////////
# stack .csv files by temporal scale
dir_IDEAM_GPPs <- "D:/4_IDEAM_GPPs"

pcp_annual       <- list.files(path = dir_results_annual,
                               pattern = "2001_2023.csv", full.names = TRUE)
pcp_annual_merge <- lapply(pcp_annual, read.csv) %>% bind_rows()
fwrite(pcp_annual_merge,paste0(dir_IDEAM_GPPs,"/","IDEAM_GPPs_annual_2001_2023.csv"))

#pcp_dry_season       <- list.files(path = dir_results_dry_sea,
#                                   pattern = "2001_2023.csv", full.names = TRUE)
#pcp_dry_season_merge <- lapply(pcp_dry_season, read.csv) %>% bind_rows()
#fwrite(pcp_dry_season_merge,paste0(dir_IDEAM_GPPs,"/","IDEAM_GPPs_dry_season.csv"))

pcp_dry_season       <- list.files(path = dir_results_dry_sea,
                                   pattern = "_ACUM.*2001_2023", full.names = TRUE)
pcp_dry_season_merge <- lapply(pcp_dry_season, read.csv) %>% bind_rows()
fwrite(pcp_dry_season_merge,paste0(dir_IDEAM_GPPs,"/",
                                   "IDEAM_GPPs_dry_season_ACUM_2001_2023.csv"))

#pcp_wet_season       <- list.files(path = dir_results_wet_sea, pattern = ".csv", full.names = TRUE)
#pcp_wet_season_merge <- lapply(pcp_wet_season, read.csv) %>% bind_rows()
#fwrite(pcp_wet_season_merge,paste0(dir_IDEAM_GPPs,"/","IDEAM_GPPs_wet_season.csv"))

pcp_wet_season       <- list.files(path = dir_results_wet_sea,
                                   pattern = "_ACUM.*2001_2023", full.names = TRUE)
pcp_wet_season_merge <- lapply(pcp_wet_season, read.csv) %>% bind_rows()
fwrite(pcp_wet_season_merge,paste0(dir_IDEAM_GPPs,"/",
                                   "IDEAM_GPPs_wet_season_ACUM_2001_2023.csv"))


#dir_IDEAM_GPPs <- "G:/My Drive/R4C_et_al/4_IDEAM_GPPs"
pcp_month       <- list.files(path = dir_results_month,
                              pattern = "2001_2023.csv", full.names = TRUE)
pcp_month_merge <- lapply(pcp_month, read.csv) %>% bind_rows()
fwrite(pcp_month_merge,paste0(dir_IDEAM_GPPs,"/","IDEAM_GPPs_month_2001_2023.csv"))

pcp_pentad       <- list.files(path = dir_results_pentad, 
                               pattern = "2001_2023.csv", full.names = TRUE)
pcp_pentad_merge <- lapply(pcp_pentad, read.csv) %>% bind_rows()
fwrite(pcp_pentad_merge,paste0(dir_IDEAM_GPPs,"/","IDEAM_GPPs_pentad_2001_2023.csv"))


pcp_daily <- list.files(path=dir_results_daily,pattern='2001_2023.csv',full.names=TRUE)
# Use lapply to read the CSV files and remove the 'stream' column from each data frame
pcp_daily_merge <- lapply(pcp_daily, function(x) {
  x <- read.csv(x)
  x <- x %>% select(-stream)  # Exclude the 'stream' column
  return(x)
}) %>% bind_rows()
fwrite(pcp_daily_merge,paste0(dir_IDEAM_GPPs,"/","IDEAM_GPPs_daily_2001_2023.csv"))




#






