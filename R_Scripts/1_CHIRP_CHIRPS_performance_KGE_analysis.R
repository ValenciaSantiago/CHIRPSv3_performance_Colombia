


#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,
       dplyr,stringr,quantmod,rnaturalearth,exactextractr,hydroGOF,
       future.apply,profvis,rnaturalearthdata,glue)

#//////////////////////////////////////////////////
# directories
ideam_pcp_data      <- 'G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/PCP_IDEAM_TIME_SERIES'
nat_reg_col         <- 'G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS'
dir_ideam_summary   <- "G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023"
dir_chirpv2_day     <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/4_CHIRPv2/1_Daily"
dir_chirpsv2_day    <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/1_Daily"
dir_chirpsv2_month  <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/2_Monthly"
dir_chirpsv3_month  <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/2_Monthly"
dir_chirpv2_month   <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/4_CHIRPv2/2_Monthly"
dir_chirpsv2_st_den <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/0_Gauges_density"
dir_chirpsv3_st_den <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/0_Gauges_density"
dir_results_month   <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/3_Monthly'
dir_results_season  <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/4_Seasonal'
dir_results_dry_sea <- "G:/My Drive/R4C_et_al/4_IDEAM_GPPs/4_1_Dry_Season"
dir_results_wet_sea <- "G:/My Drive/R4C_et_al/4_IDEAM_GPPs/4_2_Wet_Season"
dir_results_annual  <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/5_Annual'
dir_results_pentad  <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/2_Pentad'
dir_results_daily   <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs/1_Daily'
dir_plots_chirpsvx  <- "G:/My Drive/R4C_et_al/3_PLOTS/0_Comparison_CHIRPSv2_v3"
dir_plots           <- "G:/My Drive/R4C_et_al/3_PLOTS"
dir_gis             <- "G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS"




#///////////////////////////////////////////////////////////////////////////////
# Natural regions shapefile
nat_reg_pol <- vect(paste0(dir_gis,"/shp_regiones_naturales_colombia.shp"))

#///////////////////////////////////////////////////////////////////////////////
# load IDEAM gauges summary
ideam_gauges_metadata <- read.csv2(paste0(dir_ideam_summary ,
                                          "/summary_IDEAM_gauges_2001_2023_10p_v2.csv"),sep=",",head=TRUE)
ideam_gauges_metadata <- filter(ideam_gauges_metadata,
                                state!="Archipiélago de San Andres, Providencia y Santa Catalina")
# start and end study period
start_date <- "2001-01-01"
end_date   <- "2023-12-31"
mis_values_threshold  <- 10 # selected threshold
ideam_gauges_metadata      <- filter(ideam_gauges_metadata,as.numeric(p_sd_q)<10)
ideam_gauges_metadata$lat  <- as.numeric(ideam_gauges_metadata$lat)
ideam_gauges_metadata$long <- as.numeric(ideam_gauges_metadata$long)
dim(ideam_gauges_metadata)

#ideam_gauges_metadata <- ideam_gauges_metadata[1:1004,]

#/////////////////////////////////////////////////////////////////////////////////
performance_metrics <- function(n_file,
                                dir_scale,    # work directory
                                file_name,    # file names
                                na_threshold,  # maximum number of allow missing values
                                start_period,end_period) { # select analysis period

  # Load the data
  pcp_file_i <- as.data.frame(read.csv(paste0(dir_scale, "/PCP_", file_name, "_IDEAM_GPPs_",
                                ideam_gauges_metadata$gauge_code[n_file], "_2001_2023.csv")))
  # excluded values with many NA
  pcp_file_i <- pcp_file_i %>% mutate(pcp_ideam_flag = if_else(na_count_ideam > na_threshold,
                                                                NA, pcp_ideam_flag))
  pcp_file_i <- filter(pcp_file_i,year>= start_period & year <=end_period)
  
  # Function to calculate KGE for multiple variables
  calc_kge <- function(sim, obs) {
    return(KGE(sim = sim, obs = obs, method = "2012", out.type = "full"))}
  
  # Calculate KGE for each model
  kge_chirpsv2  <- calc_kge(pcp_file_i$chirpsv2, pcp_file_i$pcp_ideam_flag)
  kge_chirpsv3  <- calc_kge(pcp_file_i$chirpsv3, pcp_file_i$pcp_ideam_flag)
  kge_chirpv2   <- calc_kge(pcp_file_i$chirpv2, pcp_file_i$pcp_ideam_flag)
  kge_chirpv3   <- calc_kge(pcp_file_i$chirpv3, pcp_file_i$pcp_ideam_flag) 
  rmse_chirpv2  <- sqrt(mean((pcp_file_i$chirpv2 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpv3  <- sqrt(mean((pcp_file_i$chirpv3 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpsv2 <- sqrt(mean((pcp_file_i$chirpsv2 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpsv3 <- sqrt(mean((pcp_file_i$chirpsv3 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  
  # Scale IDEAM data
  kge_chirpsv3_scale  <- calc_kge(pcp_file_i$chirpsv3, pcp_file_i$pcp_ideam_flag_scale)
  kge_chirpv3_scale   <- calc_kge(pcp_file_i$chirpv3, pcp_file_i$pcp_ideam_flag_scale)
  
  
  return(data.frame(
    # Store basic statistics
    mean_gauges_v2 = pcp_file_i$mean_gauges_v2[1], mean_gauges_v3 = pcp_file_i$mean_gauges_v3[1],
    max_gauges_v2  = pcp_file_i$max_gauges_v2[1],  max_gauges_v3  = pcp_file_i$max_gauges_v3[1],
    mean_gauges_v2_005 = pcp_file_i$mean_gauges_v2_005[1], mean_gauges_v3_005 = pcp_file_i$mean_gauges_v3_005[1],
    max_gauges_v2_005  = pcp_file_i$max_gauges_v2_005[1],  max_gauges_v3_005  = pcp_file_i$max_gauges_v3_005[1],
    kge_chirp_v2   = kge_chirpv2$KGE.value,        r_chirp_v2     = kge_chirpv2$KGE.elements[1],
    B_chirp_v2     = kge_chirpv2$KGE.elements[2],  G_chirp_v2     = kge_chirpv2$KGE.elements[3],
    kge_chirp_v3   = kge_chirpv3$KGE.value,        r_chirp_v3     = kge_chirpv3$KGE.elements[1],
    B_chirp_v3     = kge_chirpv3$KGE.elements[2],  G_chirp_v3     = kge_chirpv3$KGE.elements[3],
    kge_chirps_v2  = kge_chirpsv2$KGE.value,       r_chirps_v2    = kge_chirpsv2$KGE.elements[1],
    B_chirps_v2    = kge_chirpsv2$KGE.elements[2], G_chirps_v2    = kge_chirpsv2$KGE.elements[3],
    kge_chirps_v3  = kge_chirpsv3$KGE.value,       r_chirps_v3    = kge_chirpsv3$KGE.elements[1],
    B_chirps_v3    = kge_chirpsv3$KGE.elements[2], G_chirps_v3    = kge_chirpsv3$KGE.elements[3],
    rmse_chirpv2   = rmse_chirpv2,
    rmse_chirpv3   = rmse_chirpv3,
    rmse_chirpsv2   = rmse_chirpsv2,
    rmse_chirpsv3   = rmse_chirpsv3,
    kge_chirp_v3_scale   = kge_chirpv3_scale$KGE.value,        r_chirp_v3_scale     = kge_chirpv3_scale$KGE.elements[1],
    B_chirp_v3_scale     = kge_chirpv3_scale$KGE.elements[2],  G_chirp_v3_scale     = kge_chirpv3_scale$KGE.elements[3],
    kge_chirps_v3_scale  = kge_chirpsv3_scale$KGE.value,       r_chirps_v3_scale    = kge_chirpsv3_scale$KGE.elements[1],
    B_chirps_v3_scale    = kge_chirpsv3_scale$KGE.elements[2], G_chirps_v3_scale    = kge_chirpsv3_scale$KGE.elements[3]
    ))
}

#///////////////////////////////////////////////////////////////////////
# monthly scale results
performance_metrics_monthly <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), function(n_file) {
  performance_metrics(n_file,
                      dir_results_month,
                      "MONTHLY",
                      5,"2001","2023")})
res_month_2001_2023_df <- do.call(rbind, performance_metrics_monthly)
res_month_2001_2023_df <- cbind(ideam_gauges_metadata,res_month_2001_2023_df)
View(res_month_2001_2023_df)


fwrite(res_month_2001_2023_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                             "res_performance_monthly_2001_2023_df.csv"))
#                              "/res_performance_monthly_2004_2015_df.csv"))



# 2001-2011
performance_metrics_monthly <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), function(n_file) {
  performance_metrics(n_file,
                      dir_results_month,
                      "MONTHLY",
                      5,"2001","2011")})
res_month_2001_2011_df <- do.call(rbind, performance_metrics_monthly)
res_month_2001_2011_df <- cbind(ideam_gauges_metadata,res_month_2001_2011_df)
#View(res_month_2001_2011_df)


fwrite(res_month_2001_2011_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                     "res_performance_monthly_2001_2011_df.csv"))


#2012-2023
performance_metrics_monthly <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), function(n_file) {
  performance_metrics(n_file,
                      dir_results_month,
                      "MONTHLY",
                      5,"2012","2023")})
res_month_2012_2023_df <- do.call(rbind, performance_metrics_monthly)
res_month_2012_2023_df <- cbind(ideam_gauges_metadata,res_month_2012_2023_df)
#View(res_month_2012_2023_df)


fwrite(res_month_2012_2023_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                     "res_performance_monthly_2012_2023_df.csv"))




#///////////////////////////////////////////////////////////////////////
# annual scale results
performance_metrics_annual <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), 
                                     function(n_file) {
                                      performance_metrics(n_file,
                                                          dir_results_annual,
                                                          "ANNUAL",
                                                          30,"2001","2023")})
res_annual_2001_2023_df <- do.call(rbind, performance_metrics_annual)
res_annual_2001_2023_df <- cbind(ideam_gauges_metadata,res_annual_2001_2023_df)
View(res_annual_2001_2023_df)

fwrite(res_annual_2001_2023_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                             "res_performance_annual_2001_2023_df.csv"))
#                             "/res_performance_annual_2004_2015_df.csv"))



#///////////////////////////////////////////////////////////////////////
# pentad scale results
performance_metrics_pentad <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), 
                                     function(n_file) {
                                       performance_metrics(n_file,
                                                           dir_results_pentad,
                                                           "PENTAD",
                                                           1,"2001","2023")})

res_pentad_2001_2023_df <- do.call(rbind, performance_metrics_pentad)
res_pentad_2001_2023_df <- cbind(ideam_gauges_metadata,res_pentad_2001_2023_df)
View(res_pentad_2001_2023_df)

fwrite(res_pentad_2001_2023_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                      "/res_performance_pentad_2001_2023_df.csv"))

# 2001-2011
performance_metrics_pentad <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), 
                                     function(n_file) {
                                       performance_metrics(n_file,
                                                           dir_results_pentad,
                                                           "PENTAD",
                                                           1,"2001","2011")})

res_pentad_2001_2011_df <- do.call(rbind, performance_metrics_pentad)
res_pentad_2001_2011_df <- cbind(ideam_gauges_metadata,res_pentad_2001_2011_df)
View(res_pentad_2001_2011_df)

fwrite(res_pentad_2001_2011_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                      "/res_performance_pentad_2001_2011_df.csv"))


# 2012-2023
performance_metrics_pentad <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), 
                                     function(n_file) {
                                       performance_metrics(n_file,
                                                           dir_results_pentad,
                                                           "PENTAD",
                                                           1,"2012","2023")})

res_pentad_2012_2023_df <- do.call(rbind, performance_metrics_pentad)
res_pentad_2012_2023_df <- cbind(ideam_gauges_metadata,res_pentad_2012_2023_df)
View(res_pentad_2012_2023_df)

fwrite(res_pentad_2012_2023_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                      "/res_performance_pentad_2012_2023_df.csv"))




#///////////////////////////////////////////////////////////////////////
# daily scale results
performance_metrics_daily <- function(n_file,
                                dir_scale,    # work directory
                                file_name,    # file names
                                na_threshold,  # maximum number of allow missing values
                                start_period,end_period) { # select analysis period
  
  # Load the data
  pcp_file_i <- as.data.frame(read.csv(paste0(dir_scale, "/PCP_", file_name, "_IDEAM_GPPs_",
                                              ideam_gauges_metadata$gauge_code[n_file], "_2001_2023.csv")))
  # excluded values with many NA
  pcp_file_i <- pcp_file_i %>% filter(na_count_ideam==0)
  pcp_file_i <- filter(pcp_file_i,date>= start_period & date <=end_period)
  
  # Function to calculate KGE for multiple variables
  calc_kge <- function(sim, obs) {
    return(KGE(sim = sim, obs = obs, method = "2012", out.type = "full"))
  }
  
  # Calculate KGE for each model
  kge_chirpv2        <- calc_kge(pcp_file_i$chirpv2, pcp_file_i$pcp_ideam_flag)
  kge_chirpv3        <- calc_kge(pcp_file_i$chirpv3, pcp_file_i$pcp_ideam_flag) 
  kge_chirpsv2       <- calc_kge(pcp_file_i$chirpsv2, pcp_file_i$pcp_ideam_flag)
  kge_chirpsv3_era5  <- calc_kge(pcp_file_i$chirpsv3_era5, pcp_file_i$pcp_ideam_flag)
  kge_chirpsv3_imerg <- calc_kge(pcp_file_i$chirpsv3_imerg, pcp_file_i$pcp_ideam_flag)
  
  rmse_chirpv2        <- sqrt(mean((pcp_file_i$chirpv2 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpv3        <- sqrt(mean((pcp_file_i$chirpv3 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpsv2       <- sqrt(mean((pcp_file_i$chirpsv2 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpsv3_era5  <- sqrt(mean((pcp_file_i$chirpsv3_era5 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpsv3_imerg <- sqrt(mean((pcp_file_i$chirpsv3_imerg - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))  
  
  
  kge_chirpv3_scale        <- calc_kge(pcp_file_i$chirpv3, pcp_file_i$pcp_ideam_flag_scale) 
  kge_chirpsv3_era5_scale  <- calc_kge(pcp_file_i$chirpsv3_era5, pcp_file_i$pcp_ideam_flag_scale)
  kge_chirpsv3_imerg_scale <- calc_kge(pcp_file_i$chirpsv3_imerg, pcp_file_i$pcp_ideam_flag_scale)
  
  
  return(data.frame(
    # Store basic statistics
    mean_gauges_v2 = pcp_file_i$mean_gauges_v2[1], mean_gauges_v3 = pcp_file_i$mean_gauges_v3[1],
    max_gauges_v2  = pcp_file_i$max_gauges_v2[1],  max_gauges_v3  = pcp_file_i$max_gauges_v3[1],
    mean_gauges_v2_005 = pcp_file_i$mean_gauges_v2_005[1], mean_gauges_v3_005 = pcp_file_i$mean_gauges_v3_005[1],
    max_gauges_v2_005  = pcp_file_i$max_gauges_v2_005[1],  max_gauges_v3_005  = pcp_file_i$max_gauges_v3_005[1],
    
    
    kge_chirp_v2   = kge_chirpv2$KGE.value,        r_chirp_v2     = kge_chirpv2$KGE.elements[1],
    B_chirp_v2     = kge_chirpv2$KGE.elements[2],  G_chirp_v2     = kge_chirpv2$KGE.elements[3],
    kge_chirp_v3   = kge_chirpv3$KGE.value,        r_chirp_v3     = kge_chirpv3$KGE.elements[1],
    B_chirp_v3     = kge_chirpv3$KGE.elements[2],  G_chirp_v3     = kge_chirpv3$KGE.elements[3],
    kge_chirp_v3_scale   = kge_chirpv3_scale$KGE.value,        r_chirp_v3_scale     = kge_chirpv3_scale$KGE.elements[1],
    B_chirp_v3_scale     = kge_chirpv3_scale$KGE.elements[2],  G_chirp_v3_scale     = kge_chirpv3_scale$KGE.elements[3],
    kge_chirps_v2  = kge_chirpsv2$KGE.value,       r_chirps_v2    = kge_chirpsv2$KGE.elements[1],
    B_chirps_v2    = kge_chirpsv2$KGE.elements[2], G_chirps_v2    = kge_chirpsv2$KGE.elements[3],
    
    
    kge_chirps_v3_era5  = kge_chirpsv3_era5$KGE.value,       
    r_chirps_v3_era5    = kge_chirpsv3_era5$KGE.elements[1],
    B_chirps_v3_era5    = kge_chirpsv3_era5$KGE.elements[2],
    G_chirps_v3_era5    = kge_chirpsv3_era5$KGE.elements[3],
    kge_chirps_v3_imerg = kge_chirpsv3_imerg$KGE.value,       
    r_chirps_v3_imerg   = kge_chirpsv3_imerg$KGE.elements[1],
    B_chirps_v3_imerg   = kge_chirpsv3_imerg$KGE.elements[2],
    G_chirps_v3_imerg   = kge_chirpsv3_imerg$KGE.elements[3],
    
    
    kge_chirps_v3_era5_scale  = kge_chirpsv3_era5_scale$KGE.value,       
    r_chirps_v3_era5_scale    = kge_chirpsv3_era5_scale$KGE.elements[1],
    B_chirps_v3_era5_scale    = kge_chirpsv3_era5_scale$KGE.elements[2],
    G_chirps_v3_era5_scale    = kge_chirpsv3_era5_scale$KGE.elements[3],
    kge_chirps_v3_imerg_scale = kge_chirpsv3_imerg_scale$KGE.value,       
    r_chirps_v3_imerg_scale   = kge_chirpsv3_imerg_scale$KGE.elements[1],
    B_chirps_v3_imerg_scale   = kge_chirpsv3_imerg_scale$KGE.elements[2],
    G_chirps_v3_imerg_scale   = kge_chirpsv3_imerg_scale$KGE.elements[3],
    
    
    rmse_chirpv2        = rmse_chirpv2,
    rmse_chirpv3        = rmse_chirpv3,
    rmse_chirpsv2       = rmse_chirpsv2,
    rmse_chirpsv3_era5  = rmse_chirpsv3_era5,
    rmse_chirpsv3_imerg = rmse_chirpsv3_imerg
  ))
}


performance_metrics_day <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), 
                                    function(n_file) {
                                      performance_metrics_daily(n_file,
                                                                dir_results_daily,
                                                                "DAILY",
                                                                1,"2001-01-01","2023-12-31")})

res_daily_2001_2023_df <- do.call(rbind, performance_metrics_day)
res_daily_2001_2023_df <- cbind(ideam_gauges_metadata,res_daily_2001_2023_df)
View(res_daily_2001_2023_df)

fwrite(res_daily_2001_2023_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                     "/res_performance_daily_2001_2023_df.csv"))







#//////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////
# dry and wet seasons
performance_metrics_dry_wet_season <- function(n_file,
                                dir_scale,    # work directory
                                file_name,    # file names
                                na_threshold,  # maximum number of allow missing values
                                start_period,end_period,season) { # select analysis period
  
  # Load the data
  pcp_file_i <- as.data.frame(read.csv(paste0(dir_scale, "/PCP_", file_name, "_ACUM_IDEAM_GPPs_",
                                              ideam_gauges_metadata$gauge_code[n_file], "_1994_2023.csv")))
  # excluded values with many NA
  pcp_file_i <- pcp_file_i %>% mutate(pcp_ideam_flag = if_else(na_count_ideam > na_threshold,
                                                               NA, pcp_ideam_flag))
  pcp_file_i <- filter(pcp_file_i,year>= start_period & year <=end_period)
  
  # Function to calculate KGE for multiple variables
  calc_kge <- function(sim, obs) {
    return(KGE(sim = sim, obs = obs, method = "2012", out.type = "full"))
  }
  
  #season_months <- sort(order(pcp_annual_cycle$pcp_ideam_flag, decreasing = ifelse(season=="wet",TRUE,FALSE),na.last = NA)[1:3])
  #pcp_file_i <- pcp_file_i %>% filter(month %in% season_months)
  
  
  # Calculate KGE for each model
  kge_chirpsv2  <- calc_kge(pcp_file_i$chirpsv2, pcp_file_i$pcp_ideam_flag)
  kge_chirpsv3  <- calc_kge(pcp_file_i$chirpsv3, pcp_file_i$pcp_ideam_flag)
  kge_chirpv2   <- calc_kge(pcp_file_i$chirpv2, pcp_file_i$pcp_ideam_flag)
  kge_chirpv3   <- calc_kge(pcp_file_i$chirpv3, pcp_file_i$pcp_ideam_flag) # Corrected the variable here
  rmse_chirpv2  <- sqrt(mean((pcp_file_i$chirpv2 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpv3  <- sqrt(mean((pcp_file_i$chirpv3 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpsv2 <- sqrt(mean((pcp_file_i$chirpsv2 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpsv3 <- sqrt(mean((pcp_file_i$chirpsv3 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  
  return(data.frame(
    # Store basic statistics
    mean_gauges_v2 = pcp_file_i$mean_gauges_v2[1], mean_gauges_v3 = pcp_file_i$mean_gauges_v3[1],
    max_gauges_v2  = pcp_file_i$max_gauges_v2[1],  max_gauges_v3  = pcp_file_i$max_gauges_v3[1],
    kge_chirp_v2   = kge_chirpv2$KGE.value,        r_chirp_v2     = kge_chirpv2$KGE.elements[1],
    B_chirp_v2     = kge_chirpv2$KGE.elements[2],  G_chirp_v2     = kge_chirpv2$KGE.elements[3],
    kge_chirp_v3   = kge_chirpv3$KGE.value,        r_chirp_v3     = kge_chirpv3$KGE.elements[1],
    B_chirp_v3     = kge_chirpv3$KGE.elements[2],  G_chirp_v3     = kge_chirpv3$KGE.elements[3],
    kge_chirps_v2  = kge_chirpsv2$KGE.value,       r_chirps_v2    = kge_chirpsv2$KGE.elements[1],
    B_chirps_v2    = kge_chirpsv2$KGE.elements[2], G_chirps_v2    = kge_chirpsv2$KGE.elements[3],
    kge_chirps_v3  = kge_chirpsv3$KGE.value,       r_chirps_v3    = kge_chirpsv3$KGE.elements[1],
    B_chirps_v3    = kge_chirpsv3$KGE.elements[2], G_chirps_v3    = kge_chirpsv3$KGE.elements[3],
    rmse_chirpv2   = rmse_chirpv2,
    rmse_chirpv3   = rmse_chirpv3,
    rmse_chirpsv2   = rmse_chirpsv2,
    rmse_chirpsv3   = rmse_chirpsv3
  ))
}





# dry season
performance_metrics_season <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), function(n_file) {
  performance_metrics_dry_wet_season(n_file,
                      dir_results_dry_sea,
                      "DRY_SEASON",
                      15,"2001","2023","dry")})

res_season_dry_df <- do.call(rbind, performance_metrics_season)
res_season_dry_df <- cbind(ideam_gauges_metadata,res_season_dry_df)


fwrite(res_season_dry_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                     "/res_performance_dry_season_acum_2001_2023_df.csv"))


# wet season
performance_metrics_season <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), function(n_file) {
  performance_metrics_dry_wet_season(n_file,
                      dir_results_wet_sea,
                      "WET_SEASON",
                      15,"2001","2023","wet")})

res_season_wet_df <- do.call(rbind, performance_metrics_season)
res_season_wet_df <- cbind(ideam_gauges_metadata,res_season_wet_df)


fwrite(res_season_wet_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                "/res_performance_wet_season_acum_2001_2023_df.csv"))



#///////////////////////////////////////////////////////////////////////////////////
# Combined dry and wet season
performance_metrics_dry_wet_season_comb <- function(n_file,
                                               dir_scale_1,    # work directory
                                               dir_scale_2,
                                               file_name_1,    # file names
                                               file_name_2,
                                               na_threshold,  # maximum number of allow missing values
                                               start_period,end_period,season) { # select analysis period
  
  # Load the data
  pcp_file_d <- as.data.frame(read.csv(paste0(dir_scale_1, "/PCP_", file_name_1, "_ACUM_IDEAM_GPPs_",
                                              ideam_gauges_metadata$gauge_code[n_file], "_1994_2023.csv")))
  pcp_file_w <- as.data.frame(read.csv(paste0(dir_scale_2, "/PCP_", file_name_2, "_ACUM_IDEAM_GPPs_",
                                              ideam_gauges_metadata$gauge_code[n_file], "_1994_2023.csv")))
  
  pcp_file_i <- rbind(pcp_file_d,pcp_file_w)
  # excluded values with many NA
  pcp_file_i <- pcp_file_i %>% arrange(year)
  pcp_file_i <- pcp_file_i %>% mutate(pcp_ideam_flag = if_else(na_count_ideam > na_threshold,
                                                               NA, pcp_ideam_flag))
  pcp_file_i <- filter(pcp_file_i,year>= start_period & year <=end_period)
  
  
  # Function to calculate KGE for multiple variables
  calc_kge <- function(sim, obs) {
    return(KGE(sim = sim, obs = obs, method = "2012", out.type = "full"))}
  
  #season_months <- sort(order(pcp_annual_cycle$pcp_ideam_flag, decreasing = ifelse(season=="wet",TRUE,FALSE),na.last = NA)[1:3])
  #pcp_file_i <- pcp_file_i %>% filter(month %in% season_months)
  
  
  # Calculate KGE for each model
  kge_chirpsv2  <- calc_kge(pcp_file_i$chirpsv2, pcp_file_i$pcp_ideam_flag)
  kge_chirpsv3  <- calc_kge(pcp_file_i$chirpsv3, pcp_file_i$pcp_ideam_flag)
  kge_chirpv2   <- calc_kge(pcp_file_i$chirpv2, pcp_file_i$pcp_ideam_flag)
  kge_chirpv3   <- calc_kge(pcp_file_i$chirpv3, pcp_file_i$pcp_ideam_flag) # Corrected the variable here
  rmse_chirpv2  <- sqrt(mean((pcp_file_i$chirpv2 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpv3  <- sqrt(mean((pcp_file_i$chirpv3 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpsv2 <- sqrt(mean((pcp_file_i$chirpsv2 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  rmse_chirpsv3 <- sqrt(mean((pcp_file_i$chirpsv3 - pcp_file_i$pcp_ideam_flag)^2, na.rm = TRUE))
  
  return(data.frame(
    # Store basic statistics
    mean_gauges_v2 = pcp_file_i$mean_gauges_v2[1], mean_gauges_v3 = pcp_file_i$mean_gauges_v3[1],
    max_gauges_v2  = pcp_file_i$max_gauges_v2[1],  max_gauges_v3  = pcp_file_i$max_gauges_v3[1],
    kge_chirp_v2   = kge_chirpv2$KGE.value,        r_chirp_v2     = kge_chirpv2$KGE.elements[1],
    B_chirp_v2     = kge_chirpv2$KGE.elements[2],  G_chirp_v2     = kge_chirpv2$KGE.elements[3],
    kge_chirp_v3   = kge_chirpv3$KGE.value,        r_chirp_v3     = kge_chirpv3$KGE.elements[1],
    B_chirp_v3     = kge_chirpv3$KGE.elements[2],  G_chirp_v3     = kge_chirpv3$KGE.elements[3],
    kge_chirps_v2  = kge_chirpsv2$KGE.value,       r_chirps_v2    = kge_chirpsv2$KGE.elements[1],
    B_chirps_v2    = kge_chirpsv2$KGE.elements[2], G_chirps_v2    = kge_chirpsv2$KGE.elements[3],
    kge_chirps_v3  = kge_chirpsv3$KGE.value,       r_chirps_v3    = kge_chirpsv3$KGE.elements[1],
    B_chirps_v3    = kge_chirpsv3$KGE.elements[2], G_chirps_v3    = kge_chirpsv3$KGE.elements[3],
    rmse_chirpv2   = rmse_chirpv2,
    rmse_chirpv3   = rmse_chirpv3,
    rmse_chirpsv2   = rmse_chirpsv2,
    rmse_chirpsv3   = rmse_chirpsv3
  ))
}


performance_metrics_season <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), function(n_file) {
  performance_metrics_dry_wet_season_comb(n_file,
                                     dir_results_wet_sea,
                                     dir_results_dry_sea,
                                     "WET_SEASON",
                                     "Dry_SEASON",
                                     15,"2001","2023","wet")})

res_season_dry_wet_df <- do.call(rbind, performance_metrics_season)
res_season_dry_wet_df <- cbind(ideam_gauges_metadata,res_season_dry_wet_df)

fwrite(res_season_dry_wet_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                "/res_performance_dry_wet_season_acum_2001_2023_df.csv"))





#///////////////////////////////////////////////////////////////////////////////
# categorical indices
# pcp intensities: <1 ,1-5, 5-20, 20-40, > 40
# Daily and pentad scales
# Calculate POD, FAR, ETS, fBIAS


categorical_ind_function <- function(n_file,
                                     var_name,
                                     dir_scale,    # work directory
                                     file_name,    # file names
                                     na_threshold,  # maximum number of allow missing values
                                     start_period,end_period) { # select analysis period
#n_file <- 1
#dir_scale <- dir_results_daily
#file_name <- "DAILY"
#na_threshold <- 1
#start_period <- "2001"
#end_period   <- "2023"

# Load the data
  cat_ind_res_df     <- data.frame(id=1)
  low_pcp_threshold  <- c(0,1,5, 20,40)
  high_pcp_threshold <- c(1,5,20,40,10000)
  p_levels           <- c('i1','i2','i3','i4','i5')
  for (p in 1:length(low_pcp_threshold)){
  
  pcp_file_i <- as.data.frame(read.csv(paste0(dir_scale, "/PCP_", file_name, "_IDEAM_GPPs_",
                                                ideam_gauges_metadata$gauge_code[n_file], "_1994_2023.csv")))
  # excluded values with many NA
  pcp_file_i <- pcp_file_i %>%
    mutate(!!sym(var_name) := if_else(na_count_ideam >= na_threshold,
                                      NA_real_, .data[[var_name]]))
  
  pcp_file_i <- filter(pcp_file_i,year>= start_period & year <=end_period)
  pcp_file_i <- pcp_file_i %>% filter(!is.na(!!sym(var_name)))
    #summary(pcp_file_i)
      
  pcp_file_ii <- pcp_file_i %>%
    mutate(
      ideam_cat = if_else(between(!!sym(var_name), low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
      chirpsv2_cat = if_else(between(chirpsv2, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
      chirpsv3_era5_cat = if_else(between(chirpsv3_era5, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
      chirpsv3_imerg_cat = if_else(between(chirpsv3_imerg, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0)
    )

  #////////////////////////////////////////////////////////////////////////////////////////////
    #metrics
    # Probability of Detection (POD) - fraction of actual events correctly identified
    pcp_file_ii$Hv2        <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv2_cat == 1,1,0)
    pcp_file_ii$Mv2        <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv2_cat == 0,1,0)
    pcp_file_ii$Hv3_era5   <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_era5_cat == 1,1,0)
    pcp_file_ii$Mv3_era5   <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_era5_cat == 0,1,0)
    pcp_file_ii$Hv3_imerg  <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_imerg_cat == 1,1,0)
    pcp_file_ii$Mv3_imerg  <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_imerg_cat == 0,1,0)
    
    POD_v2       <- sum(pcp_file_ii$Hv2) / (sum(pcp_file_ii$Hv2)  + sum(pcp_file_ii$Mv2))
    POD_v3_era5  <- sum(pcp_file_ii$Hv3_era5) / (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5))
    POD_v3_imerg <- sum(pcp_file_ii$Hv3_imerg) / (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg))

    # False Alarm Ratio (FAR) - fraction of predicted events incorrectly identified
    pcp_file_ii$FAv2       <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv2_cat == 1,1,0)
    pcp_file_ii$FAv3_era5  <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv3_era5_cat == 1,1,0)
    pcp_file_ii$FAv3_imerg <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv3_imerg_cat == 1,1,0)
    
    FAR_v2       <-  sum(pcp_file_ii$FAv2) / (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2))
    FAR_v3_era5  <-  sum(pcp_file_ii$FAv3_era5) / (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$FAv3_era5))
    FAR_v3_imerg <-  sum(pcp_file_ii$FAv3_imerg) / (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$FAv3_imerg))
    
    # Equitable Threat Score (ETS) - fraction of correctly predicted events adjusted by chance
    Hev2       <- ((sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2))*
                   (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2)))/dim(pcp_file_ii)[1]
    Hev3_era5  <- ((sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5))*
                   (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$FAv3_era5)))/dim(pcp_file_ii)[1]
    Hev3_imerg <- ((sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg5))*
                   (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$FAv3_imerg)))/dim(pcp_file_ii)[1]
 
    ETSv2       <- (sum(pcp_file_ii$Hv2) - Hev2)/((sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2) + sum(pcp_file_ii$FAv2))- Hev2)
    ETSv3_era5  <- (sum(pcp_file_ii$Hv3_era5) - Hev3_era5)/
                   ((sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5) + sum(pcp_file_ii$FAv3_era5))- Hev3_era5)
    ETSv3_imerg <- (sum(pcp_file_ii$Hv3_imerg) - Hev3_imerg)/
                   ((sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg) + sum(pcp_file_ii$FAv3_imerg))- Hev3_imerg)
    
    # Frequency Bias (fBIAS) - ratio of predicted events to actual events
    fBIASv2       <- (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2))/(sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2))
    fBIASv3_era5  <- (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$FAv3_era5))/(sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5))
    fBIASv3_imerg <- (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$FAv3_imerg))/(sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg))
    
    
# Create a summary table of the results
results <- data.frame(
  PODv2       = POD_v2,
  PODv3_era5  = POD_v3_era5,
  PODv3_imerg = POD_v3_imerg,
  FARv2       = FAR_v2,
  FARv3_era5  = FAR_v3_era5,
  FARv3_imerg = FAR_v3_imerg,
  ETSv2       = ETSv2,
  ETSv3_era5  = ETSv3_era5,
  ETSv3_imerg = ETSv3_imerg,
  fBIASv2     = fBIASv2,
  fBIASv3_era5 = fBIASv3_era5,
  fBIASv3_imerg = fBIASv3_imerg
)
colnames(results) <- paste0(colnames(results),"_",p_levels[p])
#results

# merge all intensities
cat_ind_res_df <- cbind(results,cat_ind_res_df) 
 #print(p)
 }

return(cat_ind_res_df)
}


performance_cat_daily <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), 
                                  function(n_file) {
                                    categorical_ind_function(n_file,
                                                             "pcp_ideam_flag",
                                                              dir_results_daily,
                                                              "DAILY",
                                                              1,"2001","2023")})

performance_cat_daily 
performance_cat_daily_df <- do.call(rbind, performance_cat_daily)
performance_cat_daily_df <- cbind(ideam_gauges_metadata,performance_cat_daily_df)
colnames(performance_cat_daily_df)

fwrite(performance_cat_daily_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                "/res_performance_daily_categorical_indices_2001_2023_df.csv"))


#_______________________________________________________________________
# Wind-correction factor
performance_cat_daily <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), 
                                function(n_file) {
                                  categorical_ind_function(n_file,
                                                           "pcp_ideam_flag_scale",
                                                           dir_results_daily,
                                                           "DAILY",
                                                           1,"2001","2023")})

performance_cat_daily 
performance_cat_daily_df <- do.call(rbind, performance_cat_daily)
performance_cat_daily_df <- cbind(ideam_gauges_metadata,performance_cat_daily_df)
colnames(performance_cat_daily_df)

fwrite(performance_cat_daily_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                  "/res_performance_daily_categorical_indices_2001_2023_df_scale.csv"))


#___________________________________________________________________
# Categorical indices Dry season
cat_daily_dry_season <- function(n_file,
                                     var_name,
                                     dir_scale,    # work directory
                                     file_name,    # file names
                                     na_threshold,  # maximum number of allow missing values
                                     start_period,end_period) { # select analysis period
 # n_file <- 1
 #  dir_scale <- dir_results_daily
 # file_name <- "DAILY"
 # na_threshold <- 1
 # start_period <- "2001"
 #end_period   <- "2023"
  
  # Load the data
  cat_ind_res_df     <- data.frame(id=1)
  low_pcp_threshold  <- c(0,1,5, 20,40)
  high_pcp_threshold <- c(1,5,20,40,10000)
  p_levels           <- c('i1','i2','i3','i4','i5')
  for (p in 1:length(low_pcp_threshold)){
    
    pcp_file_i <- as.data.frame(read.csv(paste0(dir_scale, "/PCP_", file_name, "_IDEAM_GPPs_",
                                                ideam_gauges_metadata$gauge_code[n_file], "_1994_2023.csv")))
    # excluded values with many NA
    pcp_file_i <- pcp_file_i %>%
      mutate(!!sym(var_name) := if_else(na_count_ideam >= na_threshold,
                                        NA_real_, .data[[var_name]]))
    
    pcp_file_i <- filter(pcp_file_i,year>= start_period & year <=end_period)
    pcp_file_i <- pcp_file_i %>% filter(!is.na(!!sym(var_name)))
    
    mean_monthly <- pcp_file_i %>%
                    group_by(month,year) %>%
                    summarise(pcp_month = sum(pcp_ideam_flag,na.rm=TRUE),
                              .groups = 'drop') %>%
                    group_by(month) %>%
                    summarise(pcp_mean = mean(pcp_month,na.rm=TRUE),
                              .groups = 'drop')
    
    dry_season <- mean_monthly %>% arrange(pcp_mean) %>% slice(1:3)
    wet_season <- mean_monthly %>% arrange(desc(pcp_mean)) %>% slice(1:3) 
    
    # filter dry/wet season
    pcp_file_dry_wet <-  pcp_file_i %>% filter(month %in% c(dry_season$month))
    
    pcp_file_ii <- pcp_file_dry_wet %>%
      mutate(
        ideam_cat = if_else(between(!!sym(var_name), low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
        chirpsv2_cat = if_else(between(chirpsv2, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
        chirpsv3_era5_cat = if_else(between(chirpsv3_era5, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
        chirpsv3_imerg_cat = if_else(between(chirpsv3_imerg, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0)
      )
    
    #////////////////////////////////////////////////////////////////////////////////////////////
    #metrics
    # Probability of Detection (POD) - fraction of actual events correctly identified
    pcp_file_ii$Hv2        <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv2_cat == 1,1,0)
    pcp_file_ii$Mv2        <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv2_cat == 0,1,0)
    pcp_file_ii$Hv3_era5   <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_era5_cat == 1,1,0)
    pcp_file_ii$Mv3_era5   <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_era5_cat == 0,1,0)
    pcp_file_ii$Hv3_imerg  <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_imerg_cat == 1,1,0)
    pcp_file_ii$Mv3_imerg  <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_imerg_cat == 0,1,0)
    
    POD_v2       <- sum(pcp_file_ii$Hv2) / (sum(pcp_file_ii$Hv2)  + sum(pcp_file_ii$Mv2))
    POD_v3_era5  <- sum(pcp_file_ii$Hv3_era5) / (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5))
    POD_v3_imerg <- sum(pcp_file_ii$Hv3_imerg) / (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg))
    
    # False Alarm Ratio (FAR) - fraction of predicted events incorrectly identified
    pcp_file_ii$FAv2       <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv2_cat == 1,1,0)
    pcp_file_ii$FAv3_era5  <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv3_era5_cat == 1,1,0)
    pcp_file_ii$FAv3_imerg <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv3_imerg_cat == 1,1,0)
    
    FAR_v2       <-  sum(pcp_file_ii$FAv2) / (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2))
    FAR_v3_era5  <-  sum(pcp_file_ii$FAv3_era5) / (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$FAv3_era5))
    FAR_v3_imerg <-  sum(pcp_file_ii$FAv3_imerg) / (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$FAv3_imerg))
    
    # Equitable Threat Score (ETS) - fraction of correctly predicted events adjusted by chance
    Hev2       <- ((sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2))*
                     (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2)))/dim(pcp_file_ii)[1]
    Hev3_era5  <- ((sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5))*
                     (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$FAv3_era5)))/dim(pcp_file_ii)[1]
    Hev3_imerg <- ((sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg5))*
                     (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$FAv3_imerg)))/dim(pcp_file_ii)[1]
    
    ETSv2       <- (sum(pcp_file_ii$Hv2) - Hev2)/((sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2) + sum(pcp_file_ii$FAv2))- Hev2)
    ETSv3_era5  <- (sum(pcp_file_ii$Hv3_era5) - Hev3_era5)/
      ((sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5) + sum(pcp_file_ii$FAv3_era5))- Hev3_era5)
    ETSv3_imerg <- (sum(pcp_file_ii$Hv3_imerg) - Hev3_imerg)/
      ((sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg) + sum(pcp_file_ii$FAv3_imerg))- Hev3_imerg)
    
    # Frequency Bias (fBIAS) - ratio of predicted events to actual events
    fBIASv2       <- (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2))/(sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2))
    fBIASv3_era5  <- (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$FAv3_era5))/(sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5))
    fBIASv3_imerg <- (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$FAv3_imerg))/(sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg))
    
    
    # Create a summary table of the results
    results <- data.frame(
      PODv2       = POD_v2,
      PODv3_era5  = POD_v3_era5,
      PODv3_imerg = POD_v3_imerg,
      FARv2       = FAR_v2,
      FARv3_era5  = FAR_v3_era5,
      FARv3_imerg = FAR_v3_imerg,
      ETSv2       = ETSv2,
      ETSv3_era5  = ETSv3_era5,
      ETSv3_imerg = ETSv3_imerg,
      fBIASv2     = fBIASv2,
      fBIASv3_era5 = fBIASv3_era5,
      fBIASv3_imerg = fBIASv3_imerg
    )
    colnames(results) <- paste0(colnames(results),"_",p_levels[p])
    #results
    
    # merge all intensities
    cat_ind_res_df <- cbind(results,cat_ind_res_df) 
    #print(p)
  }
  
  return(cat_ind_res_df)
}

performance_cat_daily <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), 
                                function(n_file) {
                                  cat_daily_dry_season(n_file,
                                                           "pcp_ideam_flag_scale",
                                                           dir_results_daily,
                                                           "DAILY",
                                                           1,"2001","2023")})

#performance_cat_daily 
performance_cat_daily_df <- do.call(rbind,performance_cat_daily)
performance_cat_daily_df <- cbind(ideam_gauges_metadata,performance_cat_daily_df)
colnames(performance_cat_daily_df)

fwrite(performance_cat_daily_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
      "/res_performance_daily_categorical_indices_2001_2023_dry_season_df.csv"))


#///////////////////////////////////////////////////////////
# Categorical indices Wet season
cat_daily_wet_season <- function(n_file,
                                 var_name,
                                 dir_scale,    # work directory
                                 file_name,    # file names
                                 na_threshold,  # maximum number of allow missing values
                                 start_period,end_period) { # select analysis period
  # n_file <- 1
  #  dir_scale <- dir_results_daily
  # file_name <- "DAILY"
  # na_threshold <- 1
  # start_period <- "2001"
  #end_period   <- "2023"
  
  # Load the data
  cat_ind_res_df     <- data.frame(id=1)
  low_pcp_threshold  <- c(0,1,5, 20,40)
  high_pcp_threshold <- c(1,5,20,40,10000)
  p_levels           <- c('i1','i2','i3','i4','i5')
  for (p in 1:length(low_pcp_threshold)){
    
    pcp_file_i <- as.data.frame(read.csv(paste0(dir_scale, "/PCP_", file_name, "_IDEAM_GPPs_",
                                                ideam_gauges_metadata$gauge_code[n_file], "_1994_2023.csv")))
    # excluded values with many NA
    pcp_file_i <- pcp_file_i %>%
      mutate(!!sym(var_name) := if_else(na_count_ideam >= na_threshold,
                                        NA_real_, .data[[var_name]]))
    
    pcp_file_i <- filter(pcp_file_i,year>= start_period & year <=end_period)
    pcp_file_i <- pcp_file_i %>% filter(!is.na(!!sym(var_name)))
    
    mean_monthly <- pcp_file_i %>%
      group_by(month,year) %>%
      summarise(pcp_month = sum(pcp_ideam_flag,na.rm=TRUE),
                .groups = 'drop') %>%
      group_by(month) %>%
      summarise(pcp_mean = mean(pcp_month,na.rm=TRUE),
                .groups = 'drop')
    
    dry_season <- mean_monthly %>% arrange(pcp_mean) %>% slice(1:3)
    wet_season <- mean_monthly %>% arrange(desc(pcp_mean)) %>% slice(1:3) 
    
    # filter dry/wet season
    pcp_file_dry_wet <-  pcp_file_i %>% filter(month %in% c(wet_season$month))
    
    pcp_file_ii <- pcp_file_dry_wet %>%
      mutate(
        ideam_cat = if_else(between(!!sym(var_name), low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
        chirpsv2_cat = if_else(between(chirpsv2, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
        chirpsv3_era5_cat = if_else(between(chirpsv3_era5, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
        chirpsv3_imerg_cat = if_else(between(chirpsv3_imerg, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0)
      )
    
    #////////////////////////////////////////////////////////////////////////////////////////////
    #metrics
    # Probability of Detection (POD) - fraction of actual events correctly identified
    pcp_file_ii$Hv2        <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv2_cat == 1,1,0)
    pcp_file_ii$Mv2        <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv2_cat == 0,1,0)
    pcp_file_ii$Hv3_era5   <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_era5_cat == 1,1,0)
    pcp_file_ii$Mv3_era5   <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_era5_cat == 0,1,0)
    pcp_file_ii$Hv3_imerg  <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_imerg_cat == 1,1,0)
    pcp_file_ii$Mv3_imerg  <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_imerg_cat == 0,1,0)
    
    POD_v2       <- sum(pcp_file_ii$Hv2) / (sum(pcp_file_ii$Hv2)  + sum(pcp_file_ii$Mv2))
    POD_v3_era5  <- sum(pcp_file_ii$Hv3_era5) / (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5))
    POD_v3_imerg <- sum(pcp_file_ii$Hv3_imerg) / (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg))
    
    # False Alarm Ratio (FAR) - fraction of predicted events incorrectly identified
    pcp_file_ii$FAv2       <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv2_cat == 1,1,0)
    pcp_file_ii$FAv3_era5  <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv3_era5_cat == 1,1,0)
    pcp_file_ii$FAv3_imerg <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv3_imerg_cat == 1,1,0)
    
    FAR_v2       <-  sum(pcp_file_ii$FAv2) / (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2))
    FAR_v3_era5  <-  sum(pcp_file_ii$FAv3_era5) / (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$FAv3_era5))
    FAR_v3_imerg <-  sum(pcp_file_ii$FAv3_imerg) / (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$FAv3_imerg))
    
    # Equitable Threat Score (ETS) - fraction of correctly predicted events adjusted by chance
    Hev2       <- ((sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2))*
                     (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2)))/dim(pcp_file_ii)[1]
    Hev3_era5  <- ((sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5))*
                     (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$FAv3_era5)))/dim(pcp_file_ii)[1]
    Hev3_imerg <- ((sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg5))*
                     (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$FAv3_imerg)))/dim(pcp_file_ii)[1]
    
    ETSv2       <- (sum(pcp_file_ii$Hv2) - Hev2)/((sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2) + sum(pcp_file_ii$FAv2))- Hev2)
    ETSv3_era5  <- (sum(pcp_file_ii$Hv3_era5) - Hev3_era5)/
      ((sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5) + sum(pcp_file_ii$FAv3_era5))- Hev3_era5)
    ETSv3_imerg <- (sum(pcp_file_ii$Hv3_imerg) - Hev3_imerg)/
      ((sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg) + sum(pcp_file_ii$FAv3_imerg))- Hev3_imerg)
    
    # Frequency Bias (fBIAS) - ratio of predicted events to actual events
    fBIASv2       <- (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2))/(sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2))
    fBIASv3_era5  <- (sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$FAv3_era5))/(sum(pcp_file_ii$Hv3_era5) + sum(pcp_file_ii$Mv3_era5))
    fBIASv3_imerg <- (sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$FAv3_imerg))/(sum(pcp_file_ii$Hv3_imerg) + sum(pcp_file_ii$Mv3_imerg))
    
    
    # Create a summary table of the results
    results <- data.frame(
      PODv2       = POD_v2,
      PODv3_era5  = POD_v3_era5,
      PODv3_imerg = POD_v3_imerg,
      FARv2       = FAR_v2,
      FARv3_era5  = FAR_v3_era5,
      FARv3_imerg = FAR_v3_imerg,
      ETSv2       = ETSv2,
      ETSv3_era5  = ETSv3_era5,
      ETSv3_imerg = ETSv3_imerg,
      fBIASv2     = fBIASv2,
      fBIASv3_era5 = fBIASv3_era5,
      fBIASv3_imerg = fBIASv3_imerg
    )
    colnames(results) <- paste0(colnames(results),"_",p_levels[p])
    #results
    
    # merge all intensities
    cat_ind_res_df <- cbind(results,cat_ind_res_df) 
    #print(p)
  }
  
  return(cat_ind_res_df)
}

performance_cat_daily <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), 
                                function(n_file) {
                                  cat_daily_wet_season(n_file,
                                  "pcp_ideam_flag_scale",
                                   dir_results_daily,
                                  "DAILY",
                                  1,"2001","2023")})

#performance_cat_daily 
performance_cat_daily_df <- do.call(rbind,performance_cat_daily)
performance_cat_daily_df <- cbind(ideam_gauges_metadata,performance_cat_daily_df)
colnames(performance_cat_daily_df)

fwrite(performance_cat_daily_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
     "/res_performance_daily_categorical_indices_2001_2023_wet_season_df.csv"))






#//////////////////////////////////////////////////////////////////////////////////
# Pentad scale

categorical_ind_function_pentad <- function(n_file,
                                     dir_scale,    # work directory
                                     file_name,    # file names
                                     na_threshold,  # maximum number of allow missing values
                                     start_period,end_period) { # select analysis period
  #n_file <- 1
  #dir_scale <- dir_results_pentad
  #file_name <- "PENTAD"
  #na_threshold <- 1
  #start_period <- "2001"
  #end_period   <- "2023"
  
  # Load the data
  cat_ind_res_df     <- data.frame(id=1)
  low_pcp_threshold  <- c(0,1,5, 20,40)
  high_pcp_threshold <- c(1,5,20,40,10000)
  p_levels           <- c('i1','i2','i3','i4','i5')
  for (p in 1:length(low_pcp_threshold)){
    
    pcp_file_i <- as.data.frame(read.csv(paste0(dir_scale, "/PCP_", file_name, "_IDEAM_GPPs_",
                                                ideam_gauges_metadata$gauge_code[n_file], "_1994_2023.csv")))
    # excluded values with many NA
    pcp_file_i <- pcp_file_i %>% mutate(pcp_ideam_flag = if_else(na_count_ideam >= na_threshold,
                                                                 NA, pcp_ideam_flag))
    pcp_file_i <- filter(pcp_file_i,year>= start_period & year <=end_period)
    pcp_file_i <- pcp_file_i %>% filter(!is.na(pcp_ideam_flag_scale))
    #summary(pcp_file_i)
    
    pcp_file_ii <- pcp_file_i %>%
      mutate(
        ideam_cat    = if_else(between(pcp_ideam_flag_scale, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
        chirpsv2_cat = if_else(between(chirpsv2, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
        chirpsv3_cat = if_else(between(chirpsv3, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
        chirpv2_cat  = if_else(between(chirpv2, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0),
        chirpv3_cat  = if_else(between(chirpv3, low_pcp_threshold[p], high_pcp_threshold[p]), 1, 0))
    

    #////////////////////////////////////////////////////////////////////////////////////////////
    #metrics
    # Probability of Detection (POD) - fraction of actual events correctly identified
    pcp_file_ii$Hv2        <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv2_cat == 1,1,0)
    pcp_file_ii$Mv2        <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv2_cat == 0,1,0)
    pcp_file_ii$Hv3        <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_cat == 1,1,0)
    pcp_file_ii$Mv3        <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpsv3_cat == 0,1,0)
    pcp_file_ii$Hv2s       <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpv2_cat == 1,1,0)
    pcp_file_ii$Mv2s       <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpv2_cat == 0,1,0)
    pcp_file_ii$Hv3s       <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpv3_cat == 1,1,0)
    pcp_file_ii$Mv3s       <- ifelse(pcp_file_ii$ideam_cat == 1 & pcp_file_ii$chirpv3_cat == 0,1,0)

    
    POD_v2       <- sum(pcp_file_ii$Hv2) / (sum(pcp_file_ii$Hv2)  + sum(pcp_file_ii$Mv2))
    POD_v3       <- sum(pcp_file_ii$Hv3) / (sum(pcp_file_ii$Hv3)  + sum(pcp_file_ii$Mv3))
    POD_v2s       <- sum(pcp_file_ii$Hv2s) / (sum(pcp_file_ii$Hv2s)  + sum(pcp_file_ii$Mv2s))
    POD_v3s       <- sum(pcp_file_ii$Hv3s) / (sum(pcp_file_ii$Hv3s)  + sum(pcp_file_ii$Mv3s))

    # False Alarm Ratio (FAR) - fraction of predicted events incorrectly identified
    pcp_file_ii$FAv2       <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv2_cat == 1,1,0)
    pcp_file_ii$FAv3       <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpsv3_cat == 1,1,0)
    pcp_file_ii$FAv2s      <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpv2_cat == 1,1,0)
    pcp_file_ii$FAv3s      <- ifelse(pcp_file_ii$ideam_cat == 0 & pcp_file_ii$chirpv3_cat == 1,1,0)
    
    
    FAR_v2       <-  sum(pcp_file_ii$FAv2) / (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2))
    FAR_v3       <-  sum(pcp_file_ii$FAv3) / (sum(pcp_file_ii$Hv3) + sum(pcp_file_ii$FAv3))
    FAR_v2s      <-  sum(pcp_file_ii$FAv2s) / (sum(pcp_file_ii$Hv2s) + sum(pcp_file_ii$FAv2s))
    FAR_v3s      <-  sum(pcp_file_ii$FAv3s) / (sum(pcp_file_ii$Hv3s) + sum(pcp_file_ii$FAv3s))
    
    
    # Equitable Threat Score (ETS) - fraction of correctly predicted events adjusted by chance
    Hev2       <- ((sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2))*
                     (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2)))/dim(pcp_file_ii)[1]
    Hev3       <- ((sum(pcp_file_ii$Hv3) + sum(pcp_file_ii$Mv3))*
                     (sum(pcp_file_ii$Hv3) + sum(pcp_file_ii$FAv3)))/dim(pcp_file_ii)[1]
    Hev2s      <- ((sum(pcp_file_ii$Hv2s) + sum(pcp_file_ii$Mv2s))*
                     (sum(pcp_file_ii$Hv2s) + sum(pcp_file_ii$FAv2s)))/dim(pcp_file_ii)[1]
    Hev3s      <- ((sum(pcp_file_ii$Hv3s) + sum(pcp_file_ii$Mv3s))*
                     (sum(pcp_file_ii$Hv3s) + sum(pcp_file_ii$FAv3s)))/dim(pcp_file_ii)[1]
    
    
    ETSv2       <- (sum(pcp_file_ii$Hv2) - Hev2)/((sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2) + sum(pcp_file_ii$FAv2))- Hev2)
    ETSv3       <- (sum(pcp_file_ii$Hv3) - Hev3)/((sum(pcp_file_ii$Hv3) + sum(pcp_file_ii$Mv3) + sum(pcp_file_ii$FAv3))- Hev3)
    ETSv2s      <- (sum(pcp_file_ii$Hv2s) - Hev2s)/((sum(pcp_file_ii$Hv2s) + sum(pcp_file_ii$Mv2s) + sum(pcp_file_ii$FAv2s))- Hev2s)
    ETSv3s      <- (sum(pcp_file_ii$Hv3s) - Hev3s)/((sum(pcp_file_ii$Hv3s) + sum(pcp_file_ii$Mv3s) + sum(pcp_file_ii$FAv3s))- Hev3s)
    
        
    # Frequency Bias (fBIAS) - ratio of predicted events to actual events
    fBIASv2       <- (sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$FAv2))/(sum(pcp_file_ii$Hv2) + sum(pcp_file_ii$Mv2))
    fBIASv3       <- (sum(pcp_file_ii$Hv3) + sum(pcp_file_ii$FAv3))/(sum(pcp_file_ii$Hv3) + sum(pcp_file_ii$Mv3))
    fBIASv2s      <- (sum(pcp_file_ii$Hv2s) + sum(pcp_file_ii$FAv2s))/(sum(pcp_file_ii$Hv2s) + sum(pcp_file_ii$Mv2s))
    fBIASv3s      <- (sum(pcp_file_ii$Hv3s) + sum(pcp_file_ii$FAv3s))/(sum(pcp_file_ii$Hv3s) + sum(pcp_file_ii$Mv3s))    
    
    # Create a summary table of the results
    results <- data.frame(
      PODv2       = POD_v2,
      PODv3       = POD_v3,
      PODv2s      = POD_v2s,
      PODv3s      = POD_v3s,
      FARv2       = FAR_v2,
      FARv3       = FAR_v3,
      FARv2s      = FAR_v2s,
      FARv3s      = FAR_v3s,
      ETSv2       = ETSv2,
      ETSv3       = ETSv3,
      ETSv2s      = ETSv2s,
      ETSv3s      = ETSv3s,
      fBIASv2     = fBIASv2,
      fBIASv3     = fBIASv3,
      fBIASv2s    = fBIASv2s,
      fBIASv3s    = fBIASv3s)
    
    colnames(results) <- paste0(colnames(results),"_",p_levels[p])
    #results
    
    # merge all intensities
    cat_ind_res_df <- cbind(results,cat_ind_res_df) 
    #print(p)
  }
  
  return(cat_ind_res_df)
}


performance_cat_pentad <- lapply(seq(1, length(ideam_gauges_metadata[, 1])), 
                                function(n_file) {
                                  categorical_ind_function_pentad(n_file,
                                                           dir_results_pentad,
                                                           "PENTAD",
                                                           1,"2001","2023")})
performance_cat_pentad 


performance_cat_pentad_df <- do.call(rbind, performance_cat_pentad)
performance_cat_pentad_df <- cbind(ideam_gauges_metadata,performance_cat_pentad_df)
colnames(performance_cat_pentad_df)

summary(performance_cat_pentad_df)
summary(performance_cat_pentad_df$ETSv2_i1)
summary(performance_cat_pentad_df$ETSv3_i1)
summary(performance_cat_pentad_df$ETSv2s_i1)
summary(performance_cat_pentad_df$ETSv3s_i1)

summary(performance_cat_pentad_df$fBIASv2_i1)
summary(performance_cat_pentad_df$fBIASv3_i1)
summary(performance_cat_pentad_df$fBIASv2s_i1)
summary(performance_cat_pentad_df$fBIASv3s_i1)

summary(performance_cat_pentad_df$FARv2_i1)
summary(performance_cat_pentad_df$FARv3_i1)
summary(performance_cat_pentad_df$FARv2s_i1)
summary(performance_cat_pentad_df$FARv3s_i1)

fwrite(performance_cat_pentad_df,paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                       "/res_performance_daily_categorical_indices_pentad_2001_2023_df.csv"))






