# Folder
pcp_time_series      files with the time series from 2001-2023 for for the used 1,004 stations: IDEAM, CHIRPS (v2), CHIRPS (v2 & v3) at daily, pentad, monthly, and annual scales

# .csv files
 res_performance     files with the results of performance metrics (Kling-Gupta Efficiency and its components) at daily, pentad, monthly, seasonal, and annual scales


# Description of variable names
date                     
gauge_code
pcp              data provided by IDEAM via DHIME in 2025
quality_flag     Data quality provided by DHIME  
name             gauge name
category
tecnology
operation
state
municipality
latitude
longitude
elevation
instalation_date
suspention_date
stream
hidrographic_area
hidrographic_zone
hidrographic_subzone
year
month
day
nat_region        Corresponding natural region proposed by IGAC
flag_ind          Data quality provided by DHIME; Missing values were completed with "Preliminar"
pcp_ideam         data provided by IDEAM via full request in 2024 (data used in this analysis)
pcp_ideam_flag    daily precipitation filtering P > 4 sd & preliminar data
pcp_ideam_flag_scale    daily precipitation filtering P > 4 sd & preliminar data multiply by chirps v3 scale factor
chirpv2           daily CHIRPv2 data
chirpv3           daily CHIRPv3 data - Not provided yet
chirpsv2          daily CHIRPSv2 data
chirpsv3_era5     daily CHIRPv3 data from ERA5 integration (1994-2023)
chirpsv3_imerg    daily CHIRPv3 data from IMERG integration (2001-2023). 1994-2000 from ERA5
mean_gauges_v2    Mean number of ground gauges used to developed V2
mean_gauges_v3    Mean number of ground gauges used to developed V3
max_gauges_v2     Max number of ground gauges used to developed V2
max_gauges_v3     Max number of ground gauges used to developed V3
factor            Scale factor in CHIRPSv3
pentad_id         Pentad id
na_count_ideam    Missing values in pcp_ideam_flag data
