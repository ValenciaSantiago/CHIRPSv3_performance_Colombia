
rm(list = ls())
# load original ideam time series
library(pacman)
p_load(tidyverse, lubridate, glue,janitor,dplyr,data.table,terra)


# directories
dir_ideam_data     <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/DATA_ORIGINAL_2025/Estaciones"
dir_save_proc_data_2024 <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/PCP_IDEAM_TIME_SERIES"
dir_save_proc_data_2025 <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/PCP_IDEAM_TIME_SERIES_2025"
dir_ideam_summary   <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023"
dir_save_proc_data_quality <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/PCP_IDEAM_TIME_SERIES_QUALITY"

setwd(dir_save_proc_data_2024)  
dir()

file_list_2024   <- list.files(pattern = "_1990-01")
file_list_2024

#/////////////////////////////////////////////
setwd(dir_save_proc_data_2025)  
dir()

file_list_2025   <- list.files(pattern = "*.csv")

#////////////////////////////////////////////////////
# loop to merge data
for (i in 1520:length(file_list_2025)){
 # i <- 1
  pcp_file_2025 <- fread(file_list_2025[i])
  pcp_file_2025 <- pcp_file_2025 %>% filter(date <= "2023-12-31")
  #head(pcp_file_2025)
  
  pcp_file_2024 <- fread(paste0(dir_save_proc_data_2024,"/",
                   "PCP_DAILY_",pcp_file_2025$gauge_code[1],"_1950-01-01_2023-12-31.csv"))
  pcp_file_2024 <- pcp_file_2024 %>% filter(date >= "1990-01-01" & date <= "2023-12-31")
  #head(pcp_file_2024)
  
  pcp_file_2025$pcp_ideam <- pcp_file_2024$pcp
  pcp_file_2025 <- pcp_file_2025 %>% select(-na_missing,-na_flag,-na_combined)
  
  # save results
  fwrite(pcp_file_2025,paste0(dir_save_proc_data_quality,"/",
                              "PCP_DAILY_",pcp_file_2025$gauge_code[1],"_1990-01-01_2023-12-31.csv"))      
  
  print(i)
  
}



#/////////////////////////////////////////////////////////////////////////
# Load all data 
#setwd(dir_save_proc_data_quality)
#dir()

file_list   <- list.files(path=dir_save_proc_data_quality,pattern = "*.csv")
file_list

#start_period <- "2000-01-01"
#end_period   <- "2020-12-31"
#gauge_code   <- 53115010
#gauge_code   <- 21150030
pcp_ideam_summary_df <- list()
gauge_codes <- substr(file_list,11,18)
gauge_missing_values_gauge <- function(gauge_code){
  pcp_file        <- list.files(path=dir_save_proc_data_quality,
                                pattern=paste0("PCP_DAILY_",gauge_code))
  pcp_ideam_gauge <- as.data.frame(fread(paste0(dir_save_proc_data_quality,
                                                "/",pcp_file),head=TRUE))
  
  
  pcp_ideam_summary_df <- data.frame()
  missing_values_func <- function(start_period,end_period){
    full_dates                <- seq(as.Date(start_period),as.Date(end_period),by="day")
    pcp_ideam_gauge_p         <- filter(pcp_ideam_gauge,date>=start_period,date<=end_period)
    percent_missing_values_na <- round(sum(is.na(pcp_ideam_gauge_p$pcp_ideam))/length(full_dates)*100,3)
    
    #///////////////////////////////////////////////////////////////////////
    pcp_4sd <- dim(pcp_ideam_gauge_p[pcp_ideam_gauge_p$pcp_ideam > 4 * sd(pcp_ideam_gauge_p$pcp_ideam, na.rm=TRUE), ])[1]
    pcp_4sd_quality <- dim(pcp_ideam_gauge_p[pcp_ideam_gauge_p$pcp_ideam >
                                               4 * sd(pcp_ideam_gauge_p$pcp_ideam, na.rm=TRUE) &
                                               pcp_ideam_gauge_p$flag_ind != "Definitivo", ])[1]
    
    percent_missing_values_sd         <- round((sum(is.na(pcp_ideam_gauge_p$pcp_ideam))+pcp_4sd)/length(full_dates)*100,3)
    percent_missing_values_sd_quality <- round((sum(is.na(pcp_ideam_gauge_p$pcp_ideam))+pcp_4sd_quality)/length(full_dates)*100,3)

    
    p_definitivo <- round(dim(pcp_ideam_gauge_p[pcp_ideam_gauge_p$flag_ind == "Definitivo", ])[1]/length(full_dates)*100,3)
    p_revision   <- round(dim(pcp_ideam_gauge_p[pcp_ideam_gauge_p$flag_ind == "En revisión", ])[1]/length(full_dates)*100,3)
    p_preliminar <- round(dim(pcp_ideam_gauge_p[pcp_ideam_gauge_p$flag_ind == "Preliminar", ])[1]/length(full_dates)*100,3)
    
                                    
    return(data.frame(p_na   = percent_missing_values_na,
                      p_sd   = percent_missing_values_sd,
                      p_sd_q = percent_missing_values_sd_quality,
                      p_def  = p_definitivo,
                      p_rev  = p_revision,
                      p_prel = p_preliminar))}
  
  
  summary_i                           <- missing_values_func("2001-01-301","2023-12-31")
  pcp_ideam_summary                   <- cbind(pcp_ideam_gauge[1,], summary_i)
  pcp_ideam_summary_df                <- pcp_ideam_summary
  message(paste("gauge code:", gauge_code))
  return(data.frame(pcp_ideam_summary_df))
  
}
results_df                     <- lapply(gauge_codes, function(gauge_code) 
                                        {gauge_missing_values_gauge(gauge_code)})
pcp_ideam_summary_1994_2023_df <- do.call(rbind, results_df)
head(pcp_ideam_summary_1994_2023_df)

n <- 10
dim(filter(pcp_ideam_summary_1994_2023_df,p_na<n))[1]
dim(filter(pcp_ideam_summary_1994_2023_df,p_sd<n))[1]
dim(filter(pcp_ideam_summary_1994_2023_df,p_sd_q<n))[1]


coordinates <- pcp_ideam_summary_1994_2023_df %>%
  filter(p_na < 10) %>%
  select(latitude, longitude) %>%
  distinct()
dim(coordinates)


world_map <- map_data("world")

# Plot
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_point(data = coordinates, aes(x = longitude, y = latitude), color = "red", size = 1) +
  coord_fixed(xlim = c(-82, -66), ylim = c(-5, 13)) +  # Adjust limits based on your data
  labs(title = "Coordinates on Map", x = "Longitude", y = "Latitude") +
  theme_minimal()


fwrite(filter(pcp_ideam_summary_1994_2023_df,p_sd_q<10),
       paste0("G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/",
              "summary_IDEAM_gauges_2001_2023_10p_v2.csv"))


data_df <- filter(pcp_ideam_summary_1994_2023_df,p_sd_q<n)
hist(data_df$p_def)
hist(data_df$p_rev)
hist(data_df$p_prel)



#///////////////////////////////////////////////////////////////////////////////////
pcp_ideam_summary_df <- list()
gauge_codes <- substr(file_list,11,18)
gauge_missing_values_gauge <- function(gauge_code){
  pcp_file        <- list.files(pattern=paste0("PCP_DAILY_",gauge_code,"_"))
  pcp_ideam_gauge <- as.data.frame(fread(pcp_file,head=TRUE))
  
  
  pcp_ideam_summary_df <- list()
  missing_values_func <- function(start_period,end_period){
    full_dates                <- seq(as.Date(start_period),as.Date(end_period),by="day")
    pcp_ideam_gauge_p         <- filter(pcp_ideam_gauge,date>=start_period,date<=end_period)
    percent_missing_values_na <- round(sum(is.na(pcp_ideam_gauge_p$pcp_ideam))/length(full_dates)*100,3)
    
    #///////////////////////////////////////////////////////////////////////
    pcp_4sd <- dim(pcp_ideam_gauge_p[pcp_ideam_gauge_p$pcp_ideam > 4 * sd(pcp_ideam_gauge_p$pcp_ideam, na.rm=TRUE), ])[1]
    pcp_4sd_quality <- dim(pcp_ideam_gauge_p[pcp_ideam_gauge_p$pcp_ideam >
                                               4 * sd(pcp_ideam_gauge_p$pcp_ideam, na.rm=TRUE) &
                                               pcp_ideam_gauge_p$flag_ind != "Definitivo", ])[1]
    
    percent_missing_values_sd         <- round((sum(is.na(pcp_ideam_gauge_p$pcp_ideam))+pcp_4sd)/length(full_dates)*100,3)
    percent_missing_values_sd_quality <- round((sum(is.na(pcp_ideam_gauge_p$pcp_ideam))+pcp_4sd_quality)/length(full_dates)*100,3)
    
    
    p_definitivo <- round(dim(pcp_ideam_gauge_p[pcp_ideam_gauge_p$flag_ind == "Definitivo", ])[1]/length(full_dates)*100,3)
    p_revision   <- round(dim(pcp_ideam_gauge_p[pcp_ideam_gauge_p$flag_ind == "En revisión", ])[1]/length(full_dates)*100,3)
    p_preliminar <- round(dim(pcp_ideam_gauge_p[pcp_ideam_gauge_p$flag_ind == "Preliminar", ])[1]/length(full_dates)*100,3)
    
    
    return(data.frame(p_na   = percent_missing_values_na,
                      p_sd   = percent_missing_values_sd,
                      p_sd_q = percent_missing_values_sd_quality,
                      p_def  = p_definitivo,
                      p_rev  = p_revision,
                      p_prel = p_preliminar))}
  
  
  summary_i                           <- missing_values_func("2004-12-31","2023-12-31")
  pcp_ideam_summary                   <- cbind(pcp_ideam_gauge[1,], summary_i)
  pcp_ideam_summary_df                <- pcp_ideam_summary
  message(paste("gauge code:", gauge_code))
  return(data.frame(pcp_ideam_summary_df))
  
}
results_df                     <- lapply(gauge_codes, function(gauge_code) 
{gauge_missing_values_gauge(gauge_code)})
pcp_ideam_summary_2004_2023_df <- do.call(rbind, results_df)
head(pcp_ideam_summary_2004_2023_df)

n <- 10
dim(filter(pcp_ideam_summary_2004_2023_df,p_na<n))[1]
dim(filter(pcp_ideam_summary_2004_2023_df,p_sd<n))[1]
dim(filter(pcp_ideam_summary_2004_2023_df,p_sd_q<n))[1]


coordinates <- pcp_ideam_summary_2004_2023_df %>%
  filter(p_sd_q < 10) %>%
  select(latitude, longitude) %>%
  distinct()
dim(coordinates)


world_map <- map_data("world")

# Plot
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_point(data = coordinates, aes(x = longitude, y = latitude), color = "red", size = 1) +
  coord_fixed(xlim = c(-82, -66), ylim = c(-5, 13)) +  # Adjust limits based on your data
  labs(title = "Coordinates on Map", x = "Longitude", y = "Latitude") +
  theme_minimal()

#"Pacifico"  "Andes"     "Caribe"    "Amazonia"  "Orinoquia"
b <- filter(pcp_ideam_summary_2004_2023_df,p_sd<n)
dim(filter(b,nat_region=="Andes"))


