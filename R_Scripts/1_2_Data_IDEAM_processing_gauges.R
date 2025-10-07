


library(pacman)
p_load(tidyverse, lubridate, glue,janitor,dplyr,data.table,terra)


# directories
dir_ideam_data     <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/DATA_ORIGINAL_2025/Estaciones"
dir_save_proc_data <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/PCP_IDEAM_TIME_SERIES"
dir_ideam_summary   <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023"

# extract IDEAM metadata
ideam_metadata <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/",
                                             "catalogo_nacional_de_Estaciones_del_IDEAM_20241016.csv")))
head(ideam_metadata)


# load natural regions information
nat_reg_shp <- vect(paste0("G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS/",
                           "shp_regiones_naturales_colombia_ediatada.shp"))
nat_reg_shp


setwd(dir_ideam_data)  
dir()

file_list   <- list.files(pattern = "*.csv")
merged_data <- lapply(file_list, read.csv) %>% bind_rows()
colnames(merged_data) <- c("gauge_code","gauge_name","variable","parameter",
                           "date","unit","pcp","quality_flag")
year  <- substr(merged_data$date,7,10)
month <- substr(merged_data$date,4,5)
day   <- substr(merged_data$date,1,2)
merged_data$date     <- as.Date(paste0(year,"-",month,"-",day))
merged_data          <- merged_data %>%
                          filter(!(gauge_code %in% c("2620500209", "2620500211", "2620500210",
                             "3602700101", "3516500203", "2403500041",
                             "2311500200", "2401500040","27015070","17010010",
                             "17010020","17020040","17015010")))
head(merged_data)
#dim(merged_data)
unique(merged_data$quality_flag)

# extract each gauge by code and add metadata information
gauge_codes_df <- unique(merged_data$gauge_code)
gauge_codes_df


# Define the function to process multiple gauge codes
process_multiple_gauges <- function(gauge_codes,
                                    pcp_df,
                                    start_date,
                                    end_date,
                                    dir_save_proc_data) {
    results <- lapply(gauge_codes, function(gauge_code) {
    # Filter the DataFrame for the specified gauge code
    pcp_gauge_i <- pcp_df %>% filter(gauge_code == !!gauge_code)
    message(paste("Saved data for gauge code:", gauge_code))
    pcp_gauge_i$date <- as.Date(pcp_gauge_i$date)
    meta_data_i <- ideam_metadata %>% filter(gauge_code== !!gauge_code)
    point <- vect(as.matrix(meta_data_i[, c("longitude", "latitude")]), 
                  type = "points", crs = crs(nat_reg_shp))
    

    
    # Create a DataFrame with all dates
    dates <- data.frame(date = seq(as.Date(start_date), as.Date(end_date), by = 'day'))
    # Merge with the filtered data
    pcp_gauge_full_i <- merge(dates, pcp_gauge_i, by = 'date', all.x = TRUE)
    pcp_gauge_full_i              <- pcp_gauge_full_i[,c(1,2,7,8)]
    pcp_gauge_full_i[,c(5:19)]    <- meta_data_i[,c(2:16)]
    pcp_gauge_full_i$year         <- substr(pcp_gauge_full_i$date,1,4)
    pcp_gauge_full_i$month        <- substr(pcp_gauge_full_i$date,6,7)
    pcp_gauge_full_i$day          <- substr(pcp_gauge_full_i$date,9,10)
    pcp_gauge_full_i$gauge_code   <- meta_data_i$gauge_code
    pcp_gauge_full_i$nat_region   <- extract(nat_reg_shp,point)$layer
    pcp_gauge_full_i$na_missing   <- as.integer(is.na(pcp_gauge_full_i$pcp))
    pcp_gauge_full_i$quality_flag <- ifelse(is.na(pcp_gauge_full_i$quality_flag),
                                            "Preliminar", pcp_gauge_full_i$quality_flag)
    pcp_gauge_full_i$na_flag      <- as.integer(pcp_gauge_full_i$quality_flag != "Preliminar")
    pcp_gauge_full_i$na_combined  <- pmin(pcp_gauge_full_i$na_flag + pcp_gauge_full_i$na_missing, 1)
    head(pcp_gauge_full_i)
    
    # Create the filename and save the DataFrame
    filename <- paste0("PCP_DAILY_", gauge_code, '_', start_date, "_", end_date, ".csv")
    fwrite(pcp_gauge_full_i, file.path(dir_save_proc_data, filename))
    return(pcp_gauge_full_i)
  })
  
  return(results)  # Return a list of results for all gauge codes
}





# Process multiple gauge codes
start_date <- '1990-01-01'
end_date   <- '2024-12-31'
# Unique gauge codes
results <- process_multiple_gauges(gauge_codes_df, 
                                   merged_data, 
                                   start_date,
                                   end_date,
                                   dir_save_proc_data)



#////////////////////////////////////////////////////////////////////
setwd(dir_save_proc_data)
dir()

pcp_ideam_gauge_meta <- list()
start_period <- "2000-01-01"
end_period   <- "2020-12-31"
gauge_code   <- 23080750
gauge_missing_values_gauge <- function(gauge_code){
  pcp_file        <- list.files(pattern=paste0("PCP_DAILY_",gauge_code,"_"))
  pcp_ideam_gauge <- as.data.frame(fread(pcp_file,head=TRUE))
  
  missing_values_func <- function(start_period,end_period){
    full_dates <- seq(as.Date(start_period),as.Date(end_period),by="day")
    pcp_ideam_gauge_p <- filter(pcp_ideam_gauge,date>=start_period,
                                date<=end_period)
    percent_missing_values <- round(sum((pcp_ideam_gauge_p$na_flag))/length(full_dates)*100,3)
    return( percent_missing_values)}
  
  pcp_ideam_gauge$p_2000_2015 <- missing_values_func("2000-01-01","2015-12-31")
  pcp_ideam_gauge$p_1994_2020 <- missing_values_func("1994-01-01","2020-12-31")
  pcp_ideam_gauge$p_1994_2015 <- missing_values_func("1994-01-01","2015-12-31")
  pcp_ideam_gauge$p_1995_2020 <- missing_values_func("1995-01-01","2020-12-31")
  pcp_ideam_gauge$p_1990_2023 <- missing_values_func("1990-01-01","2023-12-31")
  pcp_ideam_gauge$p_1990_2022 <- missing_values_func("1990-01-01","2022-12-31")
  pcp_ideam_gauge$p_1990_2021 <- missing_values_func("1990-01-01","2021-12-31")
  pcp_ideam_gauge$p_1990_2020 <- missing_values_func("1990-01-01","2020-12-31")
  pcp_ideam_gauge$p_1994_2023 <- missing_values_func("1994-01-01","2023-12-31")
  pcp_ideam_gauge$p_2000_2020 <- missing_values_func("2000-01-01","2020-12-31")
  pcp_ideam_gauge$p_2000_2023 <- missing_values_func("2000-01-01","2023-12-31")
  #print(pcp_ideam_gauge[1,])
  
  pcp_ideam_gauge_meta <- pcp_ideam_gauge[1,c(3:37)]
  message(paste("gauge code:", gauge_code))
  return(data.frame(pcp_ideam_gauge_meta))
  
}



gauge_codes <- unique(merged_data$gauge_code)
results_df <- lapply(gauge_codes, function(gauge_code) {gauge_missing_values_gauge(gauge_code)})
gauges_missing_values_df <- do.call(rbind, results_df)
head(gauges_missing_values_df)
dim(gauges_missing_values_df)


n <- 5
dim(filter(gauges_missing_values_df,p_2000_2015<n))[1]
dim(filter(gauges_missing_values_df,p_1994_2020<n))[1]
dim(filter(gauges_missing_values_df,p_1994_2015<n))[1]
dim(filter(gauges_missing_values_df,p_1995_2020<n))[1]
dim(filter(gauges_missing_values_df,p_1990_2023<n))[1]
dim(filter(gauges_missing_values_df,p_1990_2020<n))[1]
dim(filter(gauges_missing_values_df,p_1994_2023<n))[1]
dim(filter(gauges_missing_values_df,p_2000_2023<n))[1]
dim(filter(gauges_missing_values_df,p_2000_2020<n))[1]





coordinates <- gauges_missing_values_df %>%
  filter(p_2000_2020<10) %>%
  distinct(latitude, longitude)
dim(coordinates)

world_map <- map_data("world")

# Plot
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_point(data = coordinates, aes(x = longitude, y = latitude), color = "red", size = 1) +
  coord_fixed(xlim = c(-82, -66), ylim = c(-5, 13)) +  # Adjust limits based on your data
  labs(title = "Coordinates on Map", x = "Longitude", y = "Latitude") +
  theme_minimal()














