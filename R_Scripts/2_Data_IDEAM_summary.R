

#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,
       dplyr,stringr,quantmod,rnaturalearth,
       future.apply,profvis,rnaturalearthdata,glue)



#//////////////////////////////////////////////////
# directories
ideam_pcp_data <- 'G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/PCP_IDEAM_TIME_SERIES'
nat_reg_col    <- 'G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS'

#//////////////////////////////////////////////////
# load data 
setwd(ideam_pcp_data)
dir()

gauge_files <- list.files(pattern = "*.csv")
length(gauge_files)

summary_missing_values <- function(n_file){
#n_file <- 10
pcp_i  <- as.data.frame(lapply(gauge_files[n_file], read.csv))
#dim(pcp_i)

# Gauge metadata
meta_data_i <- pcp_i[1,c(3:12)]
# Analyze missing values for some periods
  # p1 <- 2004-2015
  # p2 <- 2000-2021
  # p3 <- 1995-2021
  # p4 <- 1990-2021
  # p5 <- 1985-2021
  # p6 <- 1981-2021

# Define the function
count_missing_values <- function(data, start_date, end_date) {
  filtered_data           <- filter(data, date >= start_date & date <= end_date)
  total_missing_values    <- sum(is.na(filtered_data$pcp))
  fraction_missing_values <- round((total_missing_values/dim(data)[1])*100,3)
  return(fraction_missing_values)
}

meta_data_i$p1_2004_2015 <- count_missing_values(pcp_i,'2004-01-01','2015-12-31')
meta_data_i$p2_2000_2021 <- count_missing_values(pcp_i,'2000-01-01','2021-12-31')
meta_data_i$p3_1995_2021 <- count_missing_values(pcp_i,'1995-01-01','2021-12-31')
meta_data_i$p4_1990_2021 <- count_missing_values(pcp_i,'1990-01-01','2021-12-31')
meta_data_i$p5_1985_2021 <- count_missing_values(pcp_i,'1985-01-01','2021-12-31')
meta_data_i$p6_1981_2021 <- count_missing_values(pcp_i,'1981-01-01','2021-12-31')
meta_data_i$p7_2000_2023 <- count_missing_values(pcp_i,'2000-01-01','2023-12-31')
meta_data_i$p8_1995_2020 <- count_missing_values(pcp_i,'1995-01-01','2020-12-31')
meta_data_i$p9_1995_2023 <- count_missing_values(pcp_i,'1995-01-01','2023-12-31')
meta_data_i$p10_1996_2023 <- count_missing_values(pcp_i,'1996-01-01','2023-12-31')
meta_data_i$p11_1994_2023 <- count_missing_values(pcp_i,'1994-01-01','2023-12-31')
meta_data_i$p11_1990_2023 <- count_missing_values(pcp_i,'1990-01-01','2023-12-31')
return(meta_data_i)
}


results_df <- lapply(1:length(gauge_files), summary_missing_values)
#results_df <- lapply(1:1000, summary_missing_values)

# Combine all results
gauge_summary_df <- do.call(rbind, results_df)
head(gauge_summary_df)
dim(gauge_summary_df)

par(mfrow=c(4,3))
hist(gauge_summary_df$p1_2004_2015,main=paste0('2004-2015; ',
          "n=",sum(gauge_summary_df$p1_2004_2015 <= 5))) 
hist(gauge_summary_df$p2_2000_2021,main=paste0('2000-2021; ',
          "n=",sum(gauge_summary_df$p2_2000_2021 <= 5)))  
hist(gauge_summary_df$p3_1995_2021,main=paste0('1995-2021; ',
     "n=",sum(gauge_summary_df$p3_1995_2021 <= 5))) 
hist(gauge_summary_df$p4_1990_2021,main=paste0('1990-2021; ',
     "n=",sum(gauge_summary_df$p4_1990_2021 <= 5))) 
hist(gauge_summary_df$p5_1985_2021,main=paste0('1985-2021; ',
     "n=",sum(gauge_summary_df$p5_1985_2021 <= 5)))  
hist(gauge_summary_df$p6_1981_2021,main=paste0('1981-2021; ',
     "n=",sum(gauge_summary_df$p6_1981_2021 <= 5)))  
hist(gauge_summary_df$p7_2000_2023,main=paste0('2000-2023; ',
    "n=",sum(gauge_summary_df$p7_2000_2023 <= 5)))  
hist(gauge_summary_df$p8_1995_2020,main=paste0('1995-2020; ',
    "n=",sum(gauge_summary_df$p8_1995_2020 <= 5)))  
hist(gauge_summary_df$p9_1995_2023,main=paste0('1995-2023; ',
    "n=",sum(gauge_summary_df$p9_1995_2023 <= 5))) 
hist(gauge_summary_df$p10_1996_2023,main=paste0('1996-2023; ',
    "n=",sum(gauge_summary_df$p10_1996_2023 <= 5)))
hist(gauge_summary_df$p11_1994_2023,main=paste0('1994-2023; ',
    "n=",sum(gauge_summary_df$p11_1994_2023 <= 5)))
hist(gauge_summary_df$p12_1990_2023,main=paste0('1990-2023; ',
    "n=",sum(gauge_summary_df$p11_1990_2023 <= 5)))


par(mfrow=c(4,3))
hist(gauge_summary_df$p1_2004_2015,main=paste0('2004-2015; ',
          "n=",sum(gauge_summary_df$p1_2004_2015 <= 2))) 
hist(gauge_summary_df$p2_2000_2021,main=paste0('2000-2021; ',
          "n=",sum(gauge_summary_df$p2_2000_2021 <= 2)))  
hist(gauge_summary_df$p3_1995_2021,main=paste0('1995-2021; ',
     "n=",sum(gauge_summary_df$p3_1995_2021 <= 2))) 
hist(gauge_summary_df$p4_1990_2021,main=paste0('1990-2021; ',
     "n=",sum(gauge_summary_df$p4_1990_2021 <= 2))) 
hist(gauge_summary_df$p5_1985_2021,main=paste0('1985-2021; ',
     "n=",sum(gauge_summary_df$p5_1985_2021 <= 2)))  
hist(gauge_summary_df$p6_1981_2021,main=paste0('1981-2021; ',
     "n=",sum(gauge_summary_df$p6_1981_2021 <= 2)))  
hist(gauge_summary_df$p7_2000_2023,main=paste0('2000-2023; ',
    "n=",sum(gauge_summary_df$p7_2000_2023 <= 2)))  
hist(gauge_summary_df$p8_1995_2020,main=paste0('1995-2020; ',
    "n=",sum(gauge_summary_df$p8_1995_2020 <= 2)))  
hist(gauge_summary_df$p9_1995_2023,main=paste0('1995-2023; ',
    "n=",sum(gauge_summary_df$p9_1995_2023 <= 2))) 
hist(gauge_summary_df$p10_1996_2023,main=paste0('1996-2023; ',
    "n=",sum(gauge_summary_df$p10_1996_2023 <= 2)))
hist(gauge_summary_df$p11_1994_2023,main=paste0('1994-2023; ',
    "n=",sum(gauge_summary_df$p11_1994_2023 <= 2)))
hist(gauge_summary_df$p12_1990_2023,main=paste0('1990-2023; ',
    "n=",sum(gauge_summary_df$p12_1990_2023 <= 2 )))


#//////////////////////////////////////////////////////////////////
# natural regions Colombia
nat_reg      <- vect(paste0(nat_reg_col,"/shp_regiones_naturales_colombia_ediatada.shp"))
nat_reg_orig <- (vect(paste0(nat_reg_col,"/shp_regiones_naturales_colombia.shp")))
ideam_gauges <- vect(gauge_summary_df, geom = c("long", "lat"))
nat_reg_df <- extract(nat_reg,ideam_gauges)
gauge_summary_df$region <- nat_reg_df$layer # add to gauges dataset


# filter by missing values threshold and spatial distribution 
gauge_summary_2p_df <- filter(gauge_summary_df,p11_1994_2023<= 5)
head(gauge_summary_2p_df)
dim(filter(gauge_summary_2p_df,region == "Orinoquia"))
dim(filter(gauge_summary_2p_df,region == "Amazonia"))
dim(filter(gauge_summary_2p_df,region == "Pacifico"))
dim(filter(gauge_summary_2p_df,region == "Caribe"))
dim(filter(gauge_summary_2p_df,region == "Andes"))
dim(gauge_summary_2p_df)
hist(gauge_summary_2p_df$p11_1994_2023)
gauge_summary_2p_df$mis_values <- cut(gauge_summary_2p_df$p11_1994_2023,
                                   breaks = c(-Inf, 1, 2, 3, 4, 5, Inf),
                                   labels = c("<1", "1-2", "2-3", "3-4", "4-5", ">5"),
                                   right = TRUE)
dim(filter(gauge_summary_2p_df,p11_1994_2023<=4))

library(sf)
gauge_summary_2p <- vect(gauge_summary_2p_df, geom = c("long", "lat"))
gauge_sf <- st_as_sf(gauge_summary_2p)
# Plot using ggplot2 and sf
ggplot() +
  # Add the polygons for natural regions
  #geom_sf(data = nat_reg_orig, fill = "lightblue", color = "black", alpha = 0.5) +
  # Add the scatter plot for the weather gauges
  geom_sf(data = gauge_sf, aes(color = region), size = .5) +
  labs(title = "Gauges by Natural Region (Total = 1364)", 
       x = "Longitude", 
       y = "Latitude", 
       color = " Natural Region") +
  theme_minimal()



dir_ideam_summary <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023"
fwrite(gauge_summary_df,paste0(dir_ideam_summary ,"/IDEAM_gauges_data_availability_summary.csv"))





