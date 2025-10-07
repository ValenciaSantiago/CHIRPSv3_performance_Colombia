
rm(list = ls())
library(pacman)
p_load(tidyverse, lubridate, glue,janitor,dplyr,data.table)


# directories
dir_ideam_data     <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023"
dir_save_proc_data <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/PCP_IDEAM_TIME_SERIES"

# Load data
setwd(dir_ideam_data)
pcp_df <- as.data.frame(read_csv(paste0(dir_ideam_data,"/","df_pcp_day_full_COL.csv")))
colnames(pcp_df) <- c("date","pcp","gauge_code","gauge_category","condition",
                      "state","lat","long","elevation","instalation_date",
                      "suspension_date","hidrographic_area")
head(pcp_df)



# Define the function to process multiple gauge codes
process_multiple_gauges <- function(gauge_codes,
                                    pcp_df,
                                    start_date,
                                    end_date,
                                    dir_save_proc_data) {
  results <- lapply(gauge_codes, function(gauge_code) {
    
    # Filter the DataFrame for the specified gauge code
    pcp_gauge_i <- pcp_df %>% filter(gauge_code == !!gauge_code)
    
    
    pcp_gauge_i$date <- as.Date(pcp_gauge_i$date)
    
    # Create a DataFrame with all dates
    dates <- data.frame(date = seq(as.Date(start_date), as.Date(end_date), by = 'day'))
    
    # Merge with the filtered data
    pcp_gauge_full_i <- merge(dates, pcp_gauge_i, by = 'date', all.x = TRUE)
    pcp_gauge_full_i[,c(3:12)] <- pcp_gauge_i[1,c(3:12)]
    
    # Create the filename and save the DataFrame
    filename <- paste0("PCP_DAILY_", gauge_code, '_', start_date, "_", end_date, ".csv")
    fwrite(pcp_gauge_full_i, file.path(dir_save_proc_data, filename))
    message(paste("Saved data for gauge code:", gauge_code))
    return(pcp_gauge_full_i)
  })
  
  return(results)  # Return a list of results for all gauge codes
}



# Process multiple gauge codes
start_date <- '1990-01-01'
end_date   <- '2024-12-31'
# Unique gauge codes

cod_gauges <- unique(pcp_df$gauge_code)
results <- process_multiple_gauges(cod_gauges,
                                   pcp_df, 
                                   start_date,
                                   end_date,
                                   dir_save_proc_data)




#/////////////////////////////////////////////////////////

setwd(dir_save_proc_data)
dir()
pcp_ideam_gauge_meta <- list()
start_period <- "2000-01-01"
end_period   <- "2020-12-31"
#gauge_code   <- 25025000

gauge_missing_values_gauge <- function(gauge_code){
#gauge_code <- gauge_codes[50]
pcp_file <- list.files(pattern=paste0("PCP_DAILY_",gauge_code,"_1950"))
pcp_ideam_gauge <- as.data.frame(fread(pcp_file,head=TRUE))
                                 
missing_values_func <- function(start_period,end_period){
  full_dates <- seq(as.Date(start_period),as.Date(end_period),by="day")
  pcp_ideam_gauge_p <- filter(pcp_ideam_gauge,date>=start_period,
                                            date<=end_period)
  percent_missing_values <- round(sum(is.na(pcp_ideam_gauge_p$pcp))/length(full_dates)*100,3)
  return( percent_missing_values)}

pcp_ideam_gauge$p_1970_2023 <- missing_values_func("1970-01-01","2023-12-31")
pcp_ideam_gauge$p_1980_2023 <- missing_values_func("1980-01-01","2023-12-31")
pcp_ideam_gauge$p_1985_2023 <- missing_values_func("1985-01-01","2023-12-31")
pcp_ideam_gauge$p_1985_2020 <- missing_values_func("1985-01-01","2020-12-31")
pcp_ideam_gauge$p_1990_2023 <- missing_values_func("1990-01-01","2023-12-31")
pcp_ideam_gauge$p_1990_2022 <- missing_values_func("1990-01-01","2022-12-31")
pcp_ideam_gauge$p_1990_2021 <- missing_values_func("1990-01-01","2021-12-31")
pcp_ideam_gauge$p_1994_2023 <- missing_values_func("1994-01-01","2023-12-31")
pcp_ideam_gauge$p_1995_2023 <- missing_values_func("1995-01-01","2023-12-31")
pcp_ideam_gauge$p_2000_2020 <- missing_values_func("2000-01-01","2020-12-31")
pcp_ideam_gauge$p_2000_2023 <- missing_values_func("2000-01-01","2023-12-31")
#print(pcp_ideam_gauge[1,])

pcp_ideam_gauge_meta <- pcp_ideam_gauge[1,c(3:23)]
message(paste("gauge code:", gauge_code))
return(data.frame(pcp_ideam_gauge_meta))

}


gauge_codes <- unique(pcp_df$gauge_code)
results_df <- lapply(gauge_codes, function(gauge_code) {gauge_missing_values_gauge(gauge_code)})
gauges_missing_values_df <- do.call(rbind, results_df)
head(gauges_missing_values_df)
dim(gauges_missing_values_df)


n <- 5
dim(filter(gauges_missing_values_df,p_1985_2023<n))[1]
dim(filter(gauges_missing_values_df,p_1985_2020<n))[1]
dim(filter(gauges_missing_values_df,p_1990_2023<n))[1]
dim(filter(gauges_missing_values_df,p_1990_2022<n))[1]
dim(filter(gauges_missing_values_df,p_1990_2021<n))[1]
dim(filter(gauges_missing_values_df,p_1994_2023<n))[1]
dim(filter(gauges_missing_values_df,p_1995_2023<n))[1]
dim(filter(gauges_missing_values_df,p_2000_2023<n))[1]
dim(filter(gauges_missing_values_df,p_2000_2020<n))[1]




n <- 5
coordinates <- gauges_missing_values_df %>%
  filter(p_1990_2020<n) %>%
  distinct(lat, long)
dim(coordinates)


world_map <- map_data("world")

# Plot
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgrey") +
  geom_point(data = coordinates, aes(x = long, y = lat), color = "red", size = 1) +
  coord_fixed(xlim = c(-82, -66), ylim = c(-5, 13)) +  # Adjust limits based on your data
  labs(title = "Coordinates on Map", x = "Longitude", y = "Latitude") +
  theme_minimal()






