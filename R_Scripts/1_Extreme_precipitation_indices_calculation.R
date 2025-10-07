

library('pacman')
p_load(terra, tidyverse, rnaturalearth, glue, lubridate, zoo,rlang,
       data.table)

## loading pcp data (daily scale)
dir_datasets <- 'C:/Users/santiagovalencia/Documents/GitHub/CHIRPSv3_performance_Colombia/Datasets'
pcp_data <- fread("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/IDEAM_GPPs_daily_2001_2023.csv",
                         select=c('date','gauge_code','nat_region','pcp_ideam_flag',
                           'latitude','longitude','hidrographic_area',
                           'chirpv2','chirpv3', 'chirpsv2', 'chirpsv3_era5',
                           'chirpsv3_imerg', 'month', 'year'))

##filtering by study period (2,183 gauges)
pcp_daily <- pcp_data %>% filter(date >= as.Date('2001-01-01'))
#View(pcp_daily)

## 1004 pcp gauges across Colombia
pcp_daily %>% distinct(gauge_code)
pcp_daily %>% summarise_all(~sum(is.na(.))) %>% View()


# ETCCDI indices
# https://etccdi.pacificclimate.org/list_27_indices.shtml
df_pcp_day <- pcp_daily %>% 
  dplyr::select(date,gauge_code, pcp_ideam_flag,latitude, longitude,
                hidrographic_area, chirpv2,chirpv3, chirpsv2, chirpsv3_era5,
                chirpsv3_imerg, month, year )


df_pcp_day %>% group_by(gauge_code)
### 1) R10mm: Number of days with precipitation ≥10 mm.
r10mm_by_year <- df_pcp_day %>%
  #mutate(year = year(date)) %>%
  group_by(year,gauge_code) %>%
  summarise(R10mm_chirpv2 = sum(chirpv2 >= 10),
            R10mm_chirpsv2 = sum(chirpsv2 >= 10),
            R10mm_chirpsv3_era5 = sum(chirpsv3_era5 >= 10),
            R10mm_chirpsv3_imerg = sum(chirpsv3_imerg >= 10),
            R10mm_ideam = sum(pcp_ideam_flag >= 10, na.rm = TRUE),
            .groups = 'drop')

# 2) R20mm: Number of days with precipitation ≥ 20 mm.
r20mm_by_year <- df_pcp_day %>%
  #mutate(year = year(date)) %>%
  group_by(year,gauge_code) %>%
  summarise(R20mm_chirpv2 = sum(chirpv2 >= 20),
            R20mm_chirpsv2 = sum(chirpsv2 >= 20),
            R20mm_chirpsv3_era5 = sum(chirpsv3_era5 >= 20),
            R20mm_chirpsv3_imerg = sum(chirpsv3_imerg >= 20),
            R20mm_ideam = sum(pcp_ideam_flag >= 20, na.rm = TRUE),
            .groups = 'drop')

## 3) RX1day: Maximum Precipitation amount in one year
rx1_by_year <- df_pcp_day %>% 
 # mutate(year = year(date)) %>%
  group_by(year,gauge_code) %>% 
  summarise(Rx1_chirpv2 = max(chirpv2),
            Rx1_chirpsv2 = max(chirpsv2),
            Rx1_chirpsv3_era5 = max(chirpsv3_era5),
            Rx1_chirpsv3_imerg = max(chirpsv3_imerg),
            Rx1_ideam = max(pcp_ideam_flag, na.rm = TRUE),
            .groups = 'drop')


## 4) RX5day: Highest 5 day precipitation amount
rx5day_by_year <- df_pcp_day %>%
  mutate(#year = year(date),
         roll_chirpv2 = rollapply(chirpv2, width = 5, FUN = sum, align = "right", fill = NA),
         roll_chirpsv2 = rollapply(chirpsv2, width = 5, FUN = sum, align = "right", fill = NA),
         roll_chirpv3_era5 = rollapply(chirpsv3_era5, width = 5, FUN = sum, align = "right", fill = NA),
         roll_chirpv3_imerg = rollapply(chirpsv3_imerg, width = 5, FUN = sum, align = "right", fill = NA),
         roll_ideam = rollapply(pcp_ideam_flag, width = 5, FUN = sum, align = "right", fill = NA)) %>%
  group_by(year,gauge_code) %>%
  summarise(Rx5_chirpv2 = max(roll_chirpv2, na.rm = TRUE),
            Rx5_chirpsv2 = max(roll_chirpsv2, na.rm = TRUE),
            Rx5_chirpsv3_era5 = max(roll_chirpv3_era5, na.rm = TRUE),
            Rx5_chirpsv3_imerg = max(roll_chirpv3_imerg, na.rm = TRUE),
            Rx5_ideam = max(roll_ideam, na.rm = TRUE),
            .groups = 'drop')


# 5) CDD: consecutive dry days
compute_dry_spell <- function(x) {
  if (all(is.na(x))) return(NA)  # All data missing
  
  dry <- x < 1
  dry[is.na(dry)] <- FALSE  # Treat NA as non-dry to avoid breaking sequences
  
  r <- rle(dry)
  dry_lengths <- r$lengths[r$values]  # Get lengths of dry spells
  
  if (length(dry_lengths) == 0) {
    return(0)  # No dry days found
  } else {
    return(max(dry_lengths, na.rm = TRUE))
  }
}

# Summarise longest CDD per year and gauge
CDD_by_year <- df_pcp_day %>%
  group_by(year, gauge_code) %>%
  summarise(
    CDD_chirpv2 = compute_dry_spell(chirpv2),
    CDD_chirpsv2 = compute_dry_spell(chirpsv2),
    CDD_chirpsv3_era5 = compute_dry_spell(chirpsv3_era5),
    CDD_chirpsv3_imerg = compute_dry_spell(chirpsv3_imerg),
    CDD_ideam = compute_dry_spell(pcp_ideam_flag),
    .groups = 'drop')



## 6) CW : maximum  of consecutive days with precipitation >= 1

compute_cwd <- function(x) {
  if (all(is.na(x))) return(NA)  # Handle all missing values
  
  wet <- x >= 1
  wet[is.na(wet)] <- FALSE       # Treat NA as non-wet to not break sequences
  
  r <- rle(wet)
  wet_lengths <- r$lengths[r$values]  # Lengths of wet spells
  
  if (length(wet_lengths) == 0) {
    return(0)  # No wet days found
  } else {
    return(max(wet_lengths, na.rm = TRUE))
  }
}

CWD_by_year <- df_pcp_day %>%
  #mutate(year = year(date)) %>%
  group_by(year,gauge_code) %>%
  summarise(CWD_chirpv2 = compute_cwd(chirpv2),
            CWD_chirpsv2 = compute_cwd(chirpsv2),
            CWD_chirpsv3_era5 = compute_cwd(chirpsv3_era5),
            CWD_chirpsv3_imerg = compute_cwd(chirpsv3_imerg),
            CWD_ideam = compute_cwd(pcp_ideam_flag),
            .groups = 'drop')




## 7) R95 annual total precipitation from days where daily precipitation 
#is greater than the 95th percentile of a reference period (2001-2023)

## computing percentiles thresholds
quantiles_by_gauge <- df_pcp_day %>%
  group_by(gauge_code) %>%
  summarise(
    q95_chirpv2 = quantile(chirpv2[chirpv2 >= 1], 0.95, na.rm = TRUE),
    q95_chirpsv2 = quantile(chirpsv2[chirpsv2 >= 1], 0.95, na.rm = TRUE),
    q95_chirpsv3_era5 = quantile(chirpsv3_era5[chirpsv3_era5 >= 1], 0.95, na.rm = TRUE),
    q95_chirpsv3_imerg = quantile(chirpsv3_imerg[chirpsv3_imerg >= 1], 0.95, na.rm = TRUE),
    q95_ideam = quantile(pcp_ideam_flag[pcp_ideam_flag >= 1], 0.95, na.rm = TRUE),
    .groups = "drop")

df_with_q95 <- df_pcp_day %>% left_join(quantiles_by_gauge, by = "gauge_code")

r95_by_year <- df_with_q95 %>%
  group_by(year, gauge_code) %>%
  summarise(
    r95_chirpv2 = sum(chirpv2[chirpv2 > q95_chirpv2], na.rm = TRUE),
    r95_chirpsv2 = sum(chirpsv2[chirpsv2 > q95_chirpsv2], na.rm = TRUE),
    r95_chirpsv3_era5 = sum(chirpsv3_era5[chirpsv3_era5 > q95_chirpsv3_era5], na.rm = TRUE),
    r95_chirpsv3_imerg = sum(chirpsv3_imerg[chirpsv3_imerg > q95_chirpsv3_imerg], na.rm = TRUE),
    r95_ideam = sum(pcp_ideam_flag[pcp_ideam_flag > q95_ideam], na.rm = TRUE),
    .groups = "drop")


## 8) R99: extremely wet days

## computing percentiles thresholds by gauge
quantiles_by_gauge <- df_pcp_day %>%
  group_by(gauge_code) %>%
  summarise(
    q99_chirpv2 = quantile(chirpv2[chirpv2 >= 1], 0.99, na.rm = TRUE),
    q99_chirpsv2 = quantile(chirpsv2[chirpsv2 >= 1], 0.99, na.rm = TRUE),
    q99_chirpsv3_era5 = quantile(chirpsv3_era5[chirpsv3_era5 >= 1], 0.99, na.rm = TRUE),
    q99_chirpsv3_imerg = quantile(chirpsv3_imerg[chirpsv3_imerg >= 1], 0.99, na.rm = TRUE),
    q99_ideam = quantile(pcp_ideam_flag[pcp_ideam_flag >= 1], 0.99, na.rm = TRUE),
    .groups = "drop")

df_with_q99 <- df_pcp_day %>% left_join(quantiles_by_gauge, by = "gauge_code")

r99_by_year <- df_with_q99 %>%
  group_by(year, gauge_code) %>%
  summarise(
    r99_chirpv2 = sum(chirpv2[chirpv2 > q99_chirpv2], na.rm = TRUE),
    r99_chirpsv2 = sum(chirpsv2[chirpsv2 > q99_chirpsv2], na.rm = TRUE),
    r99_chirpsv3_era5 = sum(chirpsv3_era5[chirpsv3_era5 > q99_chirpsv3_era5], na.rm = TRUE),
    r99_chirpsv3_imerg = sum(chirpsv3_imerg[chirpsv3_imerg > q99_chirpsv3_imerg], na.rm = TRUE),
    r99_ideam = sum(pcp_ideam_flag[pcp_ideam_flag > q99_ideam], na.rm = TRUE),
    .groups = "drop")


# 9) Rtotal (Total Precipitation in Wet Days, rain >= 1 mm)
Rtotal <-  df_pcp_day %>%
  group_by(year,gauge_code) %>%
  summarise(rtotal_chirpv2 = sum(chirpv2[chirpsv2 >= 1], na.rm = TRUE),
            rtotal_chirpsv2 = sum(chirpsv2[chirpsv2 >= 1], na.rm = TRUE),
            rtotal_chirpsv3_era5  = sum(chirpsv3_era5[chirpsv3_era5 >= 1], na.rm = TRUE),
            rtotal_chirpsv3_imerg = sum(chirpsv3_imerg[chirpsv3_imerg >= 1], na.rm = TRUE),
            rtotal_ideam = sum(pcp_ideam_flag[pcp_ideam_flag >= 1], na.rm = TRUE),
            .groups = 'drop')


### 10) SDI

compute_sdii <- function(df, var) {
  var_name <- deparse(substitute(var))         # Get variable name as string
  sdii_colname <- glue("SDII_{var_name}")      # Construct new column name
  
  df %>%
    mutate(wet = {{ var }} >= 1) %>%           # Curly-curly to evaluate column name
    group_by(year,gauge_code) %>%
    summarise(
      wet_day_count = sum(wet, na.rm = TRUE),
      total_precip_wet = sum({{ var }}[wet], na.rm = TRUE),
      SDII = total_precip_wet / wet_day_count,
      .groups = "drop"
    ) %>%
    rename(!!sdii_colname := SDII) %>%
    select(year,gauge_code, !!sdii_colname)
}
chirpv2 <- compute_sdii(df_pcp_day, chirpv2)
chirpsv2 <- compute_sdii(df_pcp_day, chirpsv2)
chirpsv3_era5 <- compute_sdii(df_pcp_day, chirpsv3_era5)
chirpsv3_imerg <- compute_sdii(df_pcp_day, chirpsv3_imerg)
ideam <- compute_sdii(df_pcp_day,pcp_ideam_flag)
colnames(ideam) <- c('year','gauge_code','SDII_ideam')

SDII_by_year <- left_join(chirpv2,chirpsv2,
                        by = c('year','gauge_code')) %>% 
                left_join(., chirpsv3_era5, by = c('year','gauge_code')) %>% 
                left_join(., chirpsv3_imerg, by = c('year','gauge_code')) %>% 
                left_join(., ideam, by = c('year','gauge_code'))



## 11) Precp 90p: 90ptile calculated for wet days 
compute_prec90p <- function(df, var) {
  
  var_name <- deparse(substitute(var))
  prec90p_colname <- glue("Prec90p_{var_name}")
  
  # Step 2: Compute 90th percentile for wet days only
  vector_wet <- df %>% 
    filter({{var}} >= 1) %>% 
    pull({{ var }})
  
  threshold <- quantile(vector_wet, probs = 0.90, na.rm = TRUE)
  
  # Step 3: Sum precip on days > threshold (per year)
  df %>%
    group_by(year,gauge_code) %>%
    summarise(
      !!prec90p_colname := sum({{ var }}[{{ var }} > threshold], na.rm = TRUE),
      .groups = "drop"
    )
}

chirpv2_90 <- compute_prec90p(df_pcp_day, chirpv2)
chirpsv2_90 <- compute_prec90p(df_pcp_day, chirpsv2)
chirpsv3_era5_90 <- compute_prec90p(df_pcp_day, chirpsv3_era5)
chirpsv3_imerg_90 <- compute_prec90p(df_pcp_day, chirpsv3_imerg)
ideam_90 <- compute_prec90p(df_pcp_day,pcp_ideam_flag)



pcp90p_by_year <- bind_cols(chirpv2_90, chirpsv2_90$Prec90p_chirpsv2, chirpsv3_era5_90$Prec90p_chirpsv3_era5, 
                            chirpsv3_imerg_90$Prec90p_chirpsv3_imerg, ideam_90$Prec90p_pcp_ideam_flag) %>% 
  rename(pcp90p_chirpsv2 = ...3,
         pcp90p_chirpsv3_era5 = ...4,
         pcp90p_chirpsv3_imerg = ...5,
         pcp90p_pcp_ideam_flag_scale = ...6)


dim(r10mm_by_year)
dim(r20mm_by_year)
dim(rx1_by_year)
dim(rx5day_by_year)
dim(CDD_by_year)
dim(CWD_by_year)
dim(r95_by_year)
dim(r99_by_year)
dim(Rtotal)
dim(SDII_by_year)
dim(pcp90p_by_year)

df_indices <- left_join(r10mm_by_year, r20mm_by_year,
                        by = c('year','gauge_code')) %>% 
  left_join(., rx1_by_year, by = c('year','gauge_code')) %>% 
  left_join(., rx5day_by_year, by = c('year','gauge_code')) %>% 
  left_join(., CDD_by_year, by = c('year','gauge_code')) %>% 
  left_join(., CWD_by_year, by = c('year','gauge_code')) %>% 
  left_join(., r95_by_year, by = c('year','gauge_code')) %>% 
  left_join(., r99_by_year, by = c('year','gauge_code')) %>% 
  left_join(., Rtotal, by = c('year','gauge_code')) %>% 
  left_join(., SDII_by_year, by = c('year','gauge_code')) ## %>% 
  #left_join(., pcp90p_by_year, by = 'year')

dim(df_indices)
head(df_indices)
colnames(df_indices)

gauges_summary <- fread(paste0(dir_datasets,'/gauges_summary.csv'),head=TRUE)
gauges_summary <- gauges_summary[,c('gauge_code','municipality','latitude',
                                    'longitude','elevation','p_sd_q','p_def','p_rev',
                                    'mean_gauges_v2','mean_gauges_v3','nat_region','map')]

df_indices <- merge(df_indices,gauges_summary,by='gauge_code')
fwrite(df_indices,paste0(dir_datasets,
                 '/daily_extreme_precipitation_indices_2001_2023.csv'))



#___________________________________________________________________________
# Extreme precipitation events performance
# KGE and its components


calc_kge <- function(sim, obs) {
  if (sd(sim, na.rm = TRUE) == 0 || sd(obs, na.rm = TRUE) == 0) {
    return(data.frame(
      KGE = NA, r = NA, Beta = NA, Gamma = NA
    ))
  }
  kge_result <- KGE(sim = sim, obs = obs, method = "2012", out.type = "full")
  return(data.frame(
    KGE = kge_result$KGE.value,
    r = kge_result$KGE.elements["r"],
    Beta = kge_result$KGE.elements["Beta"],
    Gamma = kge_result$KGE.elements["Gamma"]
  ))
}


# R10mm
metrics_R10mm <- df_indices %>%
  group_by(gauge_code) %>%
  summarise(
    v2       = list(calc_kge(R10mm_chirpsv2, R10mm_ideam)),
    v3_era   = list(calc_kge(R10mm_chirpsv3_era5, R10mm_ideam)),
    v3_imerg = list(calc_kge(R10mm_chirpsv3_imerg, R10mm_ideam)),
    .groups = "drop"
  ) %>%
  unnest_wider(v2, names_sep = "_R10mm_") %>%
  unnest_wider(v3_era, names_sep = "_R10mm_") %>%
  unnest_wider(v3_imerg, names_sep = "_R10mm_")

# R20mm
metrics_R20mm <- df_indices %>%
  group_by(gauge_code) %>%
  summarise(
    v2       = list(calc_kge(R20mm_chirpsv2, R20mm_ideam)),
    v3_era   = list(calc_kge(R20mm_chirpsv3_era5, R20mm_ideam)),
    v3_imerg = list(calc_kge(R20mm_chirpsv3_imerg, R20mm_ideam)),
    .groups = "drop"
  ) %>%
  unnest_wider(v2, names_sep = "_R20mm_") %>%
  unnest_wider(v3_era, names_sep = "_R20mm_") %>%
  unnest_wider(v3_imerg, names_sep = "_R20mm_")
summary(metrics_R20mm)



# Rx1
metrics_Rx1 <- df_indices %>%
  group_by(gauge_code) %>%
  summarise(
    v2       = list(calc_kge(Rx1_chirpsv2, Rx1_ideam)),
    v3_era   = list(calc_kge(Rx1_chirpsv3_era5, Rx1_ideam)),
    v3_imerg = list(calc_kge(Rx1_chirpsv3_imerg, Rx1_ideam)),
    .groups = "drop"
  ) %>%
  unnest_wider(v2, names_sep = "_Rx1_") %>%
  unnest_wider(v3_era, names_sep = "_Rx1_") %>%
  unnest_wider(v3_imerg, names_sep = "_Rx1_")
summary(metrics_Rx1)




# Rx5
metrics_Rx5 <- df_indices %>%
  group_by(gauge_code) %>%
  summarise(
    v2       = list(calc_kge(Rx5_chirpsv2, Rx5_ideam)),
    v3_era   = list(calc_kge(Rx5_chirpsv3_era5, Rx5_ideam)),
    v3_imerg = list(calc_kge(Rx5_chirpsv3_imerg, Rx5_ideam)),
    .groups = "drop"
  ) %>%
  unnest_wider(v2, names_sep = "_Rx5_") %>%
  unnest_wider(v3_era, names_sep = "_Rx5_") %>%
  unnest_wider(v3_imerg, names_sep = "_Rx5_")
summary(metrics_Rx5)



# CDD
metrics_CDD <- df_indices %>%
  group_by(gauge_code) %>%
  summarise(
    v2       = list(calc_kge(CDD_chirpsv2, CDD_ideam)),
    v3_era   = list(calc_kge(CDD_chirpsv3_era5, CDD_ideam)),
    v3_imerg = list(calc_kge(CDD_chirpsv3_imerg, CDD_ideam)),
    .groups = "drop"
  ) %>%
  unnest_wider(v2, names_sep = "_CDD_") %>%
  unnest_wider(v3_era, names_sep = "_CDD_") %>%
  unnest_wider(v3_imerg, names_sep = "_CDD_")
summary(metrics_CDD)


# CWD
metrics_CWD <- df_indices %>%
  group_by(gauge_code) %>%
  summarise(
    v2       = list(calc_kge(CWD_chirpsv2, CWD_ideam)),
    v3_era   = list(calc_kge(CWD_chirpsv3_era5, CWD_ideam)),
    v3_imerg = list(calc_kge(CWD_chirpsv3_imerg, CWD_ideam)),
    .groups = "drop"
  ) %>%
  unnest_wider(v2, names_sep = "_CWD_") %>%
  unnest_wider(v3_era, names_sep = "_CWD_") %>%
  unnest_wider(v3_imerg, names_sep = "_CWD_")
summary(metrics_CWD)


# r95
metrics_r95 <- df_indices %>%
  group_by(gauge_code) %>%
  summarise(
    v2       = list(calc_kge(r95_chirpsv2, r95_ideam)),
    v3_era   = list(calc_kge(r95_chirpsv3_era5, r95_ideam)),
    v3_imerg = list(calc_kge(r95_chirpsv3_imerg, r95_ideam)),
    .groups = "drop"
  ) %>%
  unnest_wider(v2, names_sep = "_r95_") %>%
  unnest_wider(v3_era, names_sep = "_r95_") %>%
  unnest_wider(v3_imerg, names_sep = "_r95_")
summary(metrics_r95)


# r99
metrics_r99 <- df_indices %>%
  group_by(gauge_code) %>%
  summarise(
    v2       = list(calc_kge(r99_chirpsv2, r99_ideam)),
    v3_era   = list(calc_kge(r99_chirpsv3_era5, r99_ideam)),
    v3_imerg = list(calc_kge(r99_chirpsv3_imerg, r99_ideam)),
    .groups = "drop"
  ) %>%
  unnest_wider(v2, names_sep = "_r99_") %>%
  unnest_wider(v3_era, names_sep = "_r99_") %>%
  unnest_wider(v3_imerg, names_sep = "_r99_")
summary(metrics_r99)


# rtotal
metrics_rtotal <- df_indices %>%
  group_by(gauge_code) %>%
  summarise(
    v2       = list(calc_kge(rtotal_chirpsv2,rtotal_ideam)),
    v3_era   = list(calc_kge(rtotal_chirpsv3_era5,rtotal_ideam)),
    v3_imerg = list(calc_kge(rtotal_chirpsv3_imerg,rtotal_ideam)),
    .groups = "drop"
  ) %>%
  unnest_wider(v2, names_sep = "_rtotal_") %>%
  unnest_wider(v3_era, names_sep = "_rtotal_") %>%
  unnest_wider(v3_imerg, names_sep = "_rtotal_")
summary(metrics_rtotal)


# SDII
metrics_SDII <- df_indices %>%
  group_by(gauge_code) %>%
  summarise(
    v2       = list(calc_kge(SDII_chirpsv2,SDII_ideam)),
    v3_era   = list(calc_kge(SDII_chirpsv3_era5,SDII_ideam)),
    v3_imerg = list(calc_kge(SDII_chirpsv3_imerg,SDII_ideam)),
    .groups = "drop"
  ) %>%
  unnest_wider(v2, names_sep = "_SDII_") %>%
  unnest_wider(v3_era, names_sep = "_SDII_") %>%
  unnest_wider(v3_imerg, names_sep = "_SDII_")
summary(metrics_SDII)


metrics_indices <- left_join(metrics_R10mm, metrics_R20mm,
                          by = c('gauge_code')) %>% 
  left_join(., metrics_Rx1, by = c('gauge_code')) %>% 
  left_join(., metrics_Rx5, by = c('gauge_code')) %>% 
  left_join(., metrics_CDD, by = c('gauge_code')) %>% 
  left_join(., metrics_CWD, by = c('gauge_code')) %>% 
  left_join(., metrics_r95, by = c('gauge_code')) %>% 
  left_join(., metrics_r99, by = c('gauge_code')) %>% 
  left_join(., metrics_rtotal, by = c('gauge_code')) %>% 
  left_join(., metrics_SDII, by = c('gauge_code')) 


metrics_indices <- merge(metrics_indices,gauges_summary,by='gauge_code')
fwrite(metrics_indices,paste0(dir_datasets,
        '/res_performance_extreme_precipitation_indices_2001_2023.csv'))









