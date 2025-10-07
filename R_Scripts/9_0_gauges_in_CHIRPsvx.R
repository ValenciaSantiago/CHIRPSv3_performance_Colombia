


#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,
       dplyr,stringr,quantmod,rnaturalearth,exactextractr,hydroGOF,GHCNr ,
       future.apply,profvis,rnaturalearthdata,glue)


#//////////////////////////////////////////////////
# directories
ideam_pcp_data      <- 'G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/PCP_IDEAM_TIME_SERIES_QUALITY'
dir_chirpsv2_st_den <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/0_Gauges_density/025"
dir_chirpsv3_st_den <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/0_Gauges_density/025"
dir_plots           <- "G:/My Drive/R4C_et_al/3_PLOTS"
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
#dir_IDEAM_GPPs      <- "G:/My Drive/R4C_et_al/4_IDEAM_GPPs"
dir_IDEAM_GPPs      <- "D:/4_IDEAM_GPPs"
dir_gis             <- 'G:/My Drive/R4C_et_al/1_DATA/9_GIS'

countries  <- ne_countries(type = "countries",scale = "medium")[1]
colombia <- countries[countries$name == "Colombia", ]
nat_reg_shp <- st_read("G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS/shp_regiones_naturales_colombia.shp")
chirp_pixel_int <- st_read(paste0(dir_gis,"/CHIRPS_pixels_IDEAM_interception.shp"))


pcp_gauges_points <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/",
                                         "summary_IDEAM_gauges_2001_2023_10p_v2.csv")))
pcp_gauges_sf <- st_as_sf(pcp_gauges_points, coords = c("longitude", "latitude"), crs = 4326)  
pcp_gauges_sf$latitude <- pcp_gauges_points$latitude
pcp_gauges_sf$longitude <- pcp_gauges_points$longitude
plot(pcp_gauges_sf[1])
head(pcp_gauges_points)



# load CHIRPSv2 and CHIRPSv3 gauges density at 0.25 degrees
max_gauge_density_1994_2023_v2  <- rast(paste0(dir_chirpsv2_st_den,"/",
                                               "max_chirpsv2_gauge_density_2001_2003.tif"))
max_gauge_density_1994_2023_v3  <- rast(paste0(dir_chirpsv3_st_den,"/",
                                               "max_chirpsv3_gauge_density_2001_2003.tif"))
mean_gauge_density_1994_2023_v2 <- rast(paste0(dir_chirpsv2_st_den,"/",
                                               "mean_chirpsv2_gauge_density_2001_2003.tif"))
mean_gauge_density_1994_2023_v3  <- rast(paste0(dir_chirpsv3_st_den,"/",
                                                "mean_chirpsv3_gauge_density_2001_2003.tif"))
sd_gauge_density_1994_2023_v3  <- rast(paste0(dir_chirpsv3_st_den,"/",
                                                "sd_chirpsv3_gauge_density_2001_2003.tif"))
sd_gauge_density_1994_2023_v2  <- rast(paste0(dir_chirpsv2_st_den,"/",
                                              "sd_chirpsv2_gauge_density_2001_2003.tif"))


plot(mask(max_gauge_density_1994_2023_v3,get_country("COL")))
plot(sd_gauge_density_1994_2023_v3)

#////////////////////////////////////////////////////////////////////////
#plot

gauges_plot    <- function(data_df,title,title_legend){
  
#data_df <- max_gauge_density_1994_2023_v3
gauges_df <- as.data.frame(data_df, xy = TRUE)
colnames(gauges_df) <- c("x","y","gauges")
gauges_df <- gauges_df %>% filter(gauges!=0)
head(gauges_df)

col_gauges <- c('#FFEC9DFF', '#FAC881FF', '#F4A464FF', '#E87444FF', '#D9402AFF', '#BF2729FF',
                '#912534FF', '#64243EFF', '#3D1B28FF', '#161212FF')
col_gauges_2 <- c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3ff',
                 '#abd9e9','#74add1','#4575b4')


# Plot the raster using ggplot2
plot <- ggplot()+
  geom_tile(data = gauges_df, 
            aes(x = x, 
                y = y, 
                fill = gauges)) +
  # geom_sf(data = colombia, color = "black", fill = NA, size = 0.4)+
  geom_sf(data = nat_reg_shp, color = adjustcolor("#c51b7d",
                 alpha.f = .8), fill = NA, size = 1)+
  geom_sf(data=chirp_pixel_int[1],color ='black',
          fill = NA, size = 1)+
  #geom_sf(data = pcp_gauges_sf, color = "black", fill = NA, size = 1.4,shape=19)+
  #geom_text(data = pcp_gauges_sf, 
  #          aes(x = longitude, y = latitude, label = label), 
  #          color = "black", size = 5, vjust = .2, hjust = 1.4) +
  scale_fill_stepsn(name = "", 
                    colors = rev(col_gauges_2),
                    limits=c(1,11),
                    breaks = seq(1,11,1),
                    #trans = scales::rescale(),
                    # trans = "log",
                    #trans = scales::pseudo_log_trans(), #  ADDED THIS LINE
                    labels=c('1','2','3','4','5','6','7','8','9','10','>11'),
                    guide = guide_colorbar(title = title_legend,
                                           title.position = "top", 
                                           label.position = "bottom",
                                           direction = "horizontal",
                                           barheight = unit(.4, "cm"),
                                           barwidth = unit(11, "cm"))) +
  #c('#3FB8A6', '#50C0A1', '#6BC8A1', '#88D1A2', '#A9D8A7', '#C0E0B1', 
  #  '#D1E6B8', '#E4EAA0', '#E9D27F', '#F0B94E')
  
  #geom_sf(data = pcp_gauges_sf, color = "#50C0A1", fill = NA, size = .1,shape=19)+
  theme_minimal() +
  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black"),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5, color = "black"),
        legend.text = element_text(size = 11, color = "black"),
        axis.text  =element_text(size=12, color = "black"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title=title)

return(plot)
}

gauges_plot_sd <- function(data_df,title,title_legend){
  
  #data_df <- max_gauge_density_1994_2023_v3
  gauges_df <- as.data.frame(data_df, xy = TRUE)
  colnames(gauges_df) <- c("x","y","gauges")
  gauges_df <- gauges_df %>% filter(gauges!=0)
  head(gauges_df)
  
  col_gauges <- c('#FFEC9DFF', '#FAC881FF', '#F4A464FF', '#E87444FF', '#D9402AFF', '#BF2729FF',
                  '#912534FF', '#64243EFF', '#3D1B28FF', '#161212FF')
  
  
  
  # Plot the raster using ggplot2
  plot <- ggplot()+
    geom_tile(data = gauges_df, 
              aes(x = x, 
                  y = y, 
                  fill = gauges)) +
    # geom_sf(data = colombia, color = "black", fill = NA, size = 0.4)+
    geom_sf(data = nat_reg_shp, color = adjustcolor("black", alpha.f = 0.25), fill = NA, size = 0.01)+
    #geom_sf(data = pcp_gauges_sf, color = "black", fill = NA, size = 1.4,shape=19)+
    #geom_text(data = pcp_gauges_sf, 
    #          aes(x = longitude, y = latitude, label = label), 
    #          color = "black", size = 5, vjust = .2, hjust = 1.4) +
    scale_fill_stepsn(name = "", 
                      colors = (col_gauges),
                      limits=c(0,5),
                      breaks = seq(0,5,.5),
                      #trans = scales::rescale(),
                      # trans = "log",
                      #trans = scales::pseudo_log_trans(), #  ADDED THIS LINE
                      labels=seq(0,5,.5),
                      guide = guide_colorbar(title = title_legend,
                                             title.position = "top", 
                                             label.position = "bottom",
                                             direction = "horizontal",
                                             barheight = unit(.4, "cm"),
                                             barwidth = unit(11, "cm"))) +
    #coord_sf(xlim = c(-80, -66), ylim = c(-5, 13), expand = FALSE) +  # Limit plot to extent
    theme_minimal() +
    scale_x_continuous(breaks = seq(-80, -66, by = 3))+
    #labels = c("78°W", "75°W", "72°W","69°W","X")) +
    scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
    theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
          legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5),
          legend.text = element_text(size = 11),
          axis.text  =element_text(size=12),
          legend.position = "bottom",
          legend.title.align = 1,
          legend.box.spacing = unit(0, "pt")) +
    labs(x="",y="",title=title)
  
  return(plot)
}


plot_max_v2 <- gauges_plot(max_gauge_density_1994_2023_v2,'CHIRPSv2',
                        "Maximum number of gauges by pixel between 2001-2023") 
plot_max_v3 <- gauges_plot(max_gauge_density_1994_2023_v3,'CHIRPSv3',
                           "Maximum number of gauges by pixel between 2001-2023") 

plot_mean_v2 <- gauges_plot(mean_gauge_density_1994_2023_v2,'CHIRPSv2',
                           "Mean number of gauges by pixel between 2001-2023") 
plot_mean_v3 <- gauges_plot(mean_gauge_density_1994_2023_v3,'CHIRPSv3',
                           "Mean number of gauges by pixel between 2001-2023") 

plot_sd_v2 <- gauges_plot_sd(c(sd_gauge_density_1994_2023_v2),
                            'CHIRPSv2',"Mean number of gauges by pixel between 2001-2023") 
plot_sd_v3 <- gauges_plot_sd(c(sd_gauge_density_1994_2023_v3),
                          'CHIRPSv3',"Mean number of gauges by pixel between 2001-2023") 

mean_p1_gauge_density_1994_2023_v2 <- rast(paste0(dir_chirpsv2_st_den,"/",
                                                  "mean_p1_chirpsv2_gauge_density_2001_2003.tif"))
mean_p2_gauge_density_1994_2023_v2 <- rast(paste0(dir_chirpsv2_st_den,"/",
                                                  "mean_p2_chirpsv2_gauge_density_2001_2003.tif"))
mean_p1_gauge_density_1994_2023_v3 <- rast(paste0(dir_chirpsv3_st_den,"/",
                                                  "mean_p1_chirpsv3_gauge_density_2001_2003.tif"))
mean_p2_gauge_density_1994_2023_v3 <- rast(paste0(dir_chirpsv3_st_den,"/",
                                                  "mean_p2_chirpsv3_gauge_density_2001_2003.tif"))

plot_mean_v2_p1 <- gauges_plot(mean_p1_gauge_density_1994_2023_v2,'CHIRPSv2 - 2001-2011',
                            "Mean number of gauges by 0.25° pixel") 
plot_mean_v3_p1 <- gauges_plot(mean_p1_gauge_density_1994_2023_v3,'CHIRPSv3 - 2001-2011',
                            "Mean number of gauges by 0.25° pixel") 
plot_mean_v2_p2 <- gauges_plot(mean_p2_gauge_density_1994_2023_v2,'CHIRPSv2 - 2012-2023',
                            "mean number of gauges by 0.25° pixel") 
plot_mean_v3_p2 <- gauges_plot(mean_p2_gauge_density_1994_2023_v3,'CHIRPSv3 - 2012-2023',
                            "Mean number of gauges by 0.25° pixel")


library(ggpubr)
maps_density_1 <- ggarrange(plot_mean_v2_p1,plot_mean_v2_p2,
                            plot_mean_v3_p1,plot_mean_v3_p2,
                  ncol = 2, nrow = 2, align = "hv", 
                  labels = c("b","c","d","e"),
                  common.legend = TRUE, legend = "bottom", 
                  hjust = -3.5, vjust = 0.85)

#///////////////////////////////////////////////////////////////////////////////////
gauges_df <- fread(paste0("G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/0_Gauges_density/025",
                        "/gauges_df.csv"),head=TRUE)

gauges_df_long <- data.frame(
  date = rep(gauges_df$date, 2),
  value = c(gauges_df$v3, gauges_df$v2),
  variable = rep(c('v3', 'v2'), each = nrow(gauges_df)))


# Calculate the means for the two ranges
m_v2_1 <- round(mean(gauges_df$v2[1:132]), 1)
m_v3_1 <- round(mean(gauges_df$v3[1:132]), 1)
m_v2_2 <- round(mean(gauges_df$v2[133:275]), 1)
m_v3_2 <- round(mean(gauges_df$v3[133:275]), 1)

# Plot with ggplot
plot_ts <- ggplot(gauges_df_long, aes(x = date, y = value, color = variable)) +
  geom_line(size = 1.2) + 
  scale_color_manual(values = c('#7a3d8d','#3d8d7a'), 
                     name = "Version",  # Change the legend title here
                     labels = c("v2", "v3")) +
  geom_vline(xintercept = as.Date('2011-12-01'), linetype = "dashed",
             color = 'black',size=.8) +  
  annotate("text", x = as.Date('2006-06-01'), y = max(gauges_df$v3) + 60, 
           label = paste0("Mean = ", m_v3_1), color = '#3d8d7a', hjust = 0) +
  annotate("text", x = as.Date('2006-06-01'), y = max(gauges_df$v2) - 35, 
           label = paste0("Mean = ", m_v2_1), color = '#7a3d8d', hjust = 0) +
  
  annotate("text", x = as.Date('2015-07-01'), y = max(gauges_df$v3) - 750, 
           label = paste0("Mean = ", m_v3_2), color = '#3d8d7a', hjust = 0) +
  annotate("text", x = as.Date('2015-07-01'), y = max(gauges_df$v2) - 1250, 
           label = paste0("Mean = ", m_v2_2), color = '#7a3d8d', hjust = 0) +
  
  labs(x = '', y = 'Number of ground \n gauges', title = '') +  # Change y-axis label here
  scale_x_date(breaks = "2 year", labels = scales::date_format("%Y")) +  # Add x-axis labels for each year
  scale_y_continuous(breaks = seq(0,2500,500))+
  theme_classic() +
  theme(plot.margin = unit(c(.001, .001, .001, .001), "cm"), # Reduce margin equally
        plot.title = element_text(size = 10,color='black'),
        legend.title = element_text(size = 15, vjust = 0.0, hjust = 0.5,color='black'),
        legend.text = element_text(size = 14,color='black'),
        legend.position = c(0.9, .9),
        legend.title.align = 1,
        axis.title.y = element_text(size = 13, vjust = 0.5,color='black',face='bold'),
        axis.title.x = element_text(size = 13,color='black'),
        axis.text = element_text(size = 12,color='black'),
        legend.box.spacing = unit(-1, "pt"), 
        legend.margin = margin(1, 0.3, 0., 0.25),  # unit(c(top, right, bottom, left)
        legend.box.margin = margin(0, 0, 0, 0))

plot_ts




#///////////////////////////////////////////////////////////////////////////////
# Example of common labels and title
library(cowplot)
#pcp_dataset_code <- 'median'
png(paste(dir_plots_supp, "Fig_gauges_density_2001_2023.png",
          sep = '/'), units = "in",width = 7, height = 10., 
          res = 600, pointsize = 11)#, bg = "transparent")
ggarrange(plot_ts,maps_density_1,#maps_density_2,
          heights = c(4.,11),
          widths=8,
          nrow = 2,
          labels = c("a",""))
dev.off()











