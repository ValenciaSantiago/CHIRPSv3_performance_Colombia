


#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,
       dplyr,stringr,quantmod,rnaturalearth,exactextractr,hydroGOF,
       future.apply,profvis,rnaturalearthdata,glue,ggpubr,GHCNr )


#//////////////////////////////////////////////////
# directories and data
dir_plots           <- "G:/My Drive/R4C_et_al/3_PLOTS"
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
dir_annual_data     <-  "C:/Users/santiagovalencia/OneDrive - University of Arizona/Documents/GitHub/CHIRPSv3_performance_Colombia/Datasets"
dir_IDEAM_GPPs     <-  "C:/Users/santiagovalencia/OneDrive - University of Arizona/Documents/GitHub/CHIRPSv3_performance_Colombia/Datasets"


countries  <- ne_countries(type = "countries",scale = "medium")[1]
colombia <- countries[countries$name == "Colombia", ]
#//////////////////////////////////////////////////////////////////////////////////
map_diff <- rast(paste0("G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/",
               "MAP_CHIRPSv3-CHIRPSv2_2001_2023.nc"))
map_diff <- mask(map_diff,get_country("COL"))


dry_year_v3 <- crop(rast(paste0(dir_annual_data,'/chirps-v3.0.2015.tif')),get_country("COL"))
dry_year_v3[dry_year_v3 < 0] <- NA
dry_year_v2 <- crop(rast(paste0(dir_annual_data,'/chirps-v2.0.2015.tif')),get_country("COL"))
dry_year_v2[dry_year_v2 < 0] <- NA
dry_year <- mask((dry_year_v3-dry_year_v2),get_country("COL"))
  
                
wet_year_v3 <- crop(rast(paste0(dir_annual_data,'/chirps-v3.0.2022.tif')),get_country("COL"))
wet_year_v3[dry_year_v3 < 0] <- NA
wet_year_v2 <- crop(rast(paste0(dir_annual_data,'/chirps-v2.0.2022.tif')),get_country("COL"))
wet_year_v2[dry_year_v2 < 0] <- NA
wet_year <- mask((wet_year_v3-wet_year_v2),get_country("COL"))

plot(dry_year)
plot(wet_year)



nat_reg_shp <- st_read("G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS/shp_regiones_naturales_colombia.shp")

pcp_gauges <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/",
                                         "summary_IDEAM_gauges_2001_2023_10p_v2.csv")))
pcp_gauges_points <- pcp_gauges %>% filter(gauge_code %in% c(35260070,42060010,47100020,
                                                             52040070,53080020,21140080,
                                                             11110030,15070080,25020860))
pcp_gauges_points$label <- c("f","d", "h","e","g","k","l","j","i")  # Example: labeling with letters A, B
pcp_gauges_sf <- st_as_sf(pcp_gauges_points, coords = c("longitude", "latitude"), crs = 4326)  
pcp_gauges_sf$latitude <- pcp_gauges_points$latitude
pcp_gauges_sf$longitude <- pcp_gauges_points$longitude
plot(pcp_gauges_sf[1])

#============================================================================


map_diff_df <- as.data.frame(map_diff, xy = TRUE)
colnames(map_diff_df) <- c("x","y","map_dif")
head(map_diff_df)

col_pal <- rev(c("#67001f","#b2182b","#d6604d","#f4a582","#fddbc7","#f7f7f7","#d1e5f0",
                 "#92c5de","#4393c3","#2166ac","#053061"))
col_pal_2 <- c('#EF8A47FF', '#F7AA58FF', '#FFD06FFF', 
               '#FFE6B7FF', '#AADCE0FF', '#72BCD5FF', '#528FADFF', '#376795FF')

# Plot the raster using ggplot2
map_dif_plot <- ggplot()+
  geom_tile(data = map_diff_df, 
            aes(x = x, 
                y = y, 
                fill = map_dif)) +
 # geom_sf(data = colombia, color = "black", fill = NA, size = 0.4)+
  geom_sf(data = nat_reg_shp, color = adjustcolor("black", alpha.f = 0.25), fill = NA, size = 0.01)+
  geom_sf(data = pcp_gauges_sf, color = "black", fill = NA, size = 1.4,shape=19)+
  geom_text(data = pcp_gauges_sf, 
            aes(x = longitude, y = latitude, label = label), 
            color = "black", size = 5, vjust = .2, hjust = 1.4) +
  scale_fill_stepsn(name = "", 
                    colors = (col_pal_2),
                    limits=c(-2000,2000),
                    breaks = seq(-2000,2000,500),
                    #trans = scales::rescale(),
                    # trans = "log",
                    #trans = scales::pseudo_log_trans(), #  ADDED THIS LINE
                    labels=c("≤ -2000","-1500","-1000","-500","0",
                             "500","1000","1500","≥ 2000"),
                    guide = guide_colorbar(title = "CHIRPSv3 minus CHIRPSv2 (mm/year)", 
                                           title.position = "top", 
                                           label.position = "bottom",
                                           direction = "horizontal",
                                           barheight = unit(.3, "cm"),
                                           barwidth = unit(12, "cm"),
                    label.theme = element_text(angle = 0, size = 13)))+
  #coord_sf(xlim = c(-80, -66), ylim = c(-5, 13), expand = FALSE) +  # Limit plot to extent
  theme_minimal() +
  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
                     #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5),
        legend.text = element_text(size = 11),
        axis.text  =element_text(size=12,color='black'),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title="Mean annual precipitation \n 2001-2023")
  


wet_year_df <- as.data.frame(wet_year, xy = TRUE)
colnames(wet_year_df) <- c("x","y","map_dif")
head(wet_year_df)

wet_year_plot <- ggplot()+
  geom_tile(data = wet_year_df, 
            aes(x = x, 
                y = y, 
                fill = map_dif)) +
  # geom_sf(data = colombia, color = "black", fill = NA, size = 0.4)+
  geom_sf(data = nat_reg_shp, color = adjustcolor("black", alpha.f = 0.25), fill = NA, size = 0.01)+
  geom_sf(data = pcp_gauges_sf, color = "black", fill = NA, size = 1.4,shape=19)+
  geom_text(data = pcp_gauges_sf, 
            aes(x = longitude, y = latitude, label = label), 
            color = "black", size = 5, vjust = .2, hjust = 1.4) +
  scale_fill_stepsn(name = "", 
                    colors = (col_pal_2),
                    limits=c(-2000,2000),
                    breaks = seq(-2000,2000,500),
                    #trans = scales::rescale(),
                    # trans = "log",
                    #trans = scales::pseudo_log_trans(), #  ADDED THIS LINE
                    labels=c("≤ -2000","","-1000","","0",
                             "","1000","","≥ 2000"),
                    guide = guide_colorbar(title = "CHIRPSv3 minus CHIRPSv2 (mm/year)", 
                                           title.position = "top", 
                                           label.position = "bottom",
                                           direction = "horizontal",
                                           barheight = unit(.3, "cm"),
                                           barwidth = unit(12, "cm"),
                                           label.theme = element_text(angle = 0, size = 13)))+
  #coord_sf(xlim = c(-80, -66), ylim = c(-5, 13), expand = FALSE) +  # Limit plot to extent
  theme_minimal() +
  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5),
        legend.text = element_text(size = 11),
        axis.text  =element_text(size=12,color='black'),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title="Annual precipitation  \n Wet year (2021)")




dry_year_df <- as.data.frame(dry_year, xy = TRUE)
colnames(dry_year_df) <- c("x","y","map_dif")
head(dry_year_df)

dry_year_plot <- ggplot()+
  geom_tile(data = dry_year_df, 
            aes(x = x, 
                y = y, 
                fill = map_dif)) +
  # geom_sf(data = colombia, color = "black", fill = NA, size = 0.4)+
  geom_sf(data = nat_reg_shp, color = adjustcolor("black", alpha.f = 0.25), fill = NA, size = 0.01)+
  geom_sf(data = pcp_gauges_sf, color = "black", fill = NA, size = 1.4,shape=19)+
  geom_text(data = pcp_gauges_sf, 
            aes(x = longitude, y = latitude, label = label), 
            color = "black", size = 5, vjust = .2, hjust = 1.4) +
  scale_fill_stepsn(name = "", 
                    colors = (col_pal_2),
                    limits=c(-2000,2000),
                    breaks = seq(-2000,2000,500),
                    #trans = scales::rescale(),
                    # trans = "log",
                    #trans = scales::pseudo_log_trans(), #  ADDED THIS LINE
                    labels=c("≤ -2000","","-1000","","0",
                             "","1000","","≥ 2000"),
                    guide = guide_colorbar(title = "CHIRPSv3 minus CHIRPSv2 (mm/year)", 
                                           title.position = "top", 
                                           label.position = "bottom",
                                           direction = "horizontal",
                                           barheight = unit(.3, "cm"),
                                           barwidth = unit(12, "cm"),
                                           label.theme = element_text(angle = 0, size = 13)))+
  #coord_sf(xlim = c(-80, -66), ylim = c(-5, 13), expand = FALSE) +  # Limit plot to extent
  theme_minimal() +
  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5),
        legend.text = element_text(size = 11),
        axis.text  =element_text(size=12,color='black'),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title="Annual precipitation  \n Dry year (2015)")


panel_top <- ggarrange(map_dif_plot,dry_year_plot,wet_year_plot,ncol=3,
                       common.legend = TRUE, legend = "bottom",labels=c('a','b','c'))



#///////////////////////////////////////////////////////////////////////////////
# load monthly precipitation
pcp_month_data      <- fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_month.csv"),head=TRUE)%>%
                          filter(na_count_ideam <= 5)
pcp_month_data      <- filter(pcp_month_data ,date>"2000-12-31")
unique(pcp_month_data $gauge_code)

plot_annual_cycle <- function(code,ylabel){
 # code <- 26150150
pcp_month_i      <-   pcp_month_data %>% filter(gauge_code == code)
pcp_mean_monthly <- pcp_month_i %>%
                       group_by(gauge_code, municipality,longitude,latitude,
                                elevation,
                                month) %>%
                        summarise(
                          mean_ideam    = mean(pcp_ideam_flag, na.rm = TRUE), 
                          mean_chirpsv2 = mean(chirpsv2, na.rm = TRUE),
                          mean_chirpsv3 = mean(chirpsv3, na.rm = TRUE),
                          pctl_25_ideam = quantile(pcp_ideam_flag, 0.25, na.rm = TRUE),
                          pctl_75_ideam = quantile(pcp_ideam_flag, 0.75, na.rm = TRUE),
                          pctl_25_chirpsv2 = quantile(chirpsv2, 0.25, na.rm = TRUE),
                          pctl_75_chirpsv2 = quantile(chirpsv2, 0.75, na.rm = TRUE),
                          pctl_25_chirpsv3 = quantile(chirpsv3, 0.25, na.rm = TRUE),
                          pctl_75_chirpsv3 = quantile(chirpsv3, 0.75, na.rm = TRUE),
                          .groups  = "drop")


plot_p <- ggplot(pcp_mean_monthly, aes(x = month)) +
            geom_ribbon(aes(ymin = pctl_25_ideam, ymax = pctl_75_ideam), fill = "black", alpha = 0.25) +
            geom_ribbon(aes(ymin = pctl_25_chirpsv2, ymax = pctl_75_chirpsv2), fill = "#7a3d8d", alpha = 0.25) +
            geom_ribbon(aes(ymin = pctl_25_chirpsv3, ymax = pctl_75_chirpsv3), fill = "#3d8d52", alpha = 0.25) +
            
            # Add mean lines
            geom_line(aes(y = mean_ideam, color = "Ground gauges"), size = 1) +
            geom_line(aes(y = mean_chirpsv2, color = "CHIRPSv2"), size = 1) +
            geom_line(aes(y = mean_chirpsv3, color = "CHIRPSv3"), size = 1) +
            
            # Customize plot labels and themes
            labs(
              x = "",
              y = ylabel,
              title = paste0("",round(pcp_mean_monthly$latitude,3),"°N, ",
                             "",round(pcp_mean_monthly$longitude,3),"°W; ",
                             "",pcp_mean_monthly$elevation," m.a.s.l"),
              color = "",
              fill=""
            ) +
  
 # '#9970ab','#c2a5cf','#a6dba0','#5aae61'

  
            scale_color_manual(values = c("Ground gauges" = "black",
                                          "CHIRPSv2" = "#7a3d8d",
                                          "CHIRPSv3" = "#3d8d52")) +
           scale_fill_manual(values = c("Ground gauges" = "black", "CHIRPSv2" = "#7a3d8d", "CHIRPSv3" = "#3d8d7a")) +
  
   scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                        "Sep", "Oct", "Nov", "Dec"),  # Adjust the month range
                             labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
              theme_classic2() +
  theme(plot.margin = unit(c(.001, .001, .001, .001), "cm"),# Reduce margin equally
        plot.title = element_text(size = 10),
        legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5),
        legend.text = element_text(size = 14),
        #axis.text  =element_text(size=12),
        legend.position = "bottom",
        legend.title.align = 1,
        axis.title.y = element_text(size = 12.5,vjust = 0.,color='black',face = "bold"),
        axis.title.x = element_text(size = 13,color='black'),
        axis.text = element_text(size = 12,color='black'),
        legend.box.spacing = unit(-1, "pt"), 
        legend.margin = margin(1, 0.3, 0., 0.25),  # unit(c(top, right, bottom, left)
        legend.box.margin = margin(0, 0, 0, 0))

max_value <- max(
  c(pcp_mean_monthly$pctl_75_ideam,
  pcp_mean_monthly$pctl_75_chirpsv2,
  pcp_mean_monthly$pctl_75_chirpsv3))

plot_p1 <- plot_p +  
  annotate("text", 
           x = 1,
           y = c(max_value*1.22,
                 max_value*1.1,
                 max_value*.98),
           label = c(
             #paste0("MAP-", expression(IDEAM), " = ", round(sum(pcp_mean_monthly$mean_ideam)), 3),
             label = paste0("MAP[\"IDEAM\"] ==", round(sum(pcp_mean_monthly$mean_ideam),0)),
             label = paste0("MAP[\"CHIRPSv2\"] ==", round(sum(pcp_mean_monthly$mean_chirpsv2),0)),
             label = paste0("MAP[\"CHIRPSv3\"] ==", round(sum(pcp_mean_monthly$mean_chirpsv3),0))
           ), 
           parse=TRUE,
           color = c("black","#7a3d8d","#3d8d52"),
           size = 4, 
           hjust = 0,       
           vjust = 0.5) 

return(plot_p1)

}

p_b <- plot_annual_cycle(15070080,"")
p_c <- plot_annual_cycle(25020860,"")
p_d <- plot_annual_cycle(11110030,"")
p_f <- plot_annual_cycle(21140080,"") 
p_e <- plot_annual_cycle(35260070,"Monthly precipitation (mm)")
p_i <- plot_annual_cycle(42060010,"")
p_j <- plot_annual_cycle(47100020,"")
p_h <- plot_annual_cycle(52040070,"")
p_g <- plot_annual_cycle(53080020,"")


panel_rigth <- ggarrange(p_b, p_c, p_d, p_e, p_f, p_g, p_h, p_i, p_j,
                         ncol = 3, nrow = 3, align = "hv", 
                         labels = c("d", "e", "f", "g", "h", "i", "j", "k", "l"),
                         common.legend = TRUE, legend = "bottom",
                         hjust = c(-3,-3.5,-3.5), vjust = 0.83)


# Example of common labels and title
library(cowplot)
#pcp_dataset_code <- 'median'
png(paste("G:/My Drive/R4C_et_al/3_PLOTS", "Fig22_MAP_differences.png",
          sep = '/'), units = "in",width = 15., height = 7., 
    res = 600, pointsize = 11)#, bg = "transparent")
ggarrange(map_dif_plot,panel_rigth, widths = c(3.65,8), ncol = 2, labels = c("a",""))


dev.off()


library(cowplot)
png(paste("G:/My Drive/R4C_et_al/3_PLOTS", "Fig2_MAP_differences.png",
          sep = '/'), units = "in",width = 12.15, height = 6.5, 
    res = 600, pointsize = 5)#, bg = "transparent")
ggarrange(map_dif_plot,panel_rigth, widths = c(3.9,8), ncol = 2, labels = c("a",""))


dev.off()


library(cowplot)
blank_plot <- ggplot() + theme_void()
png(paste("G:/My Drive/R4C_et_al/3_PLOTS", "Fig2_MAP_differences2.png",
          sep = '/'), units = "in",width = 10, height = 12, 
    res = 600, pointsize = 5)#, bg = "transparent")

ggarrange(panel_top,blank_plot,panel_rigth, heights=c(5,.2,6.5), nrow=3,
          ncol = 1)

dev.off()





#////////////////////////////////////////////////////////
# Canvas panel

# Example of common labels and title
library(cowplot)
#pcp_dataset_code <- 'median'
png(paste("G:/My Drive/R4C_et_al/3_PLOTS", "Fig2_panel_bottom.png",
          sep = '/'), units = "in",width = 7., height = 5, 
    res = 600, pointsize = 11)#, bg = "transparent")

plot_annual_cycle <- function(code,ylabel){
  # code <- 26150150
  pcp_month_i      <-   pcp_month_data %>% filter(gauge_code == code)
  pcp_mean_monthly <- pcp_month_i %>%
    group_by(gauge_code, municipality,longitude,latitude,
             elevation,
             month) %>%
    summarise(
      mean_ideam    = mean(pcp_ideam_flag, na.rm = TRUE), 
      mean_chirpsv2 = mean(chirpsv2, na.rm = TRUE),
      mean_chirpsv3 = mean(chirpsv3, na.rm = TRUE),
      pctl_25_ideam = quantile(pcp_ideam_flag, 0.25, na.rm = TRUE),
      pctl_75_ideam = quantile(pcp_ideam_flag, 0.75, na.rm = TRUE),
      pctl_25_chirpsv2 = quantile(chirpsv2, 0.25, na.rm = TRUE),
      pctl_75_chirpsv2 = quantile(chirpsv2, 0.75, na.rm = TRUE),
      pctl_25_chirpsv3 = quantile(chirpsv3, 0.25, na.rm = TRUE),
      pctl_75_chirpsv3 = quantile(chirpsv3, 0.75, na.rm = TRUE),
      .groups  = "drop")
  
  
  plot_p <- ggplot(pcp_mean_monthly, aes(x = month)) +
    geom_ribbon(aes(ymin = pctl_25_ideam, ymax = pctl_75_ideam), fill = "black", alpha = 0.25) +
    geom_ribbon(aes(ymin = pctl_25_chirpsv2, ymax = pctl_75_chirpsv2), fill = "#7a3d8d", alpha = 0.25) +
    geom_ribbon(aes(ymin = pctl_25_chirpsv3, ymax = pctl_75_chirpsv3), fill = "#3d8d52", alpha = 0.25) +
    
    # Add mean lines
    geom_line(aes(y = mean_ideam, color = "Ground gauges"), size = 1) +
    geom_line(aes(y = mean_chirpsv2, color = "CHIRPSv2"), size = 1) +
    geom_line(aes(y = mean_chirpsv3, color = "CHIRPSv3"), size = 1) +
    
    # Customize plot labels and themes
    labs(
      x = "",
      y = ylabel,
      title = paste0("",round(pcp_mean_monthly$latitude,3),"°N, ",
                     "",round(pcp_mean_monthly$longitude,3),"°W; ",
                     "",pcp_mean_monthly$elevation," m.a.s.l"),
      color = "",
      fill=""
    ) +
    
    # '#9970ab','#c2a5cf','#a6dba0','#5aae61'
    
    
    scale_color_manual(values = c("Ground gauges" = "black",
                                  "CHIRPSv2" = "#7a3d8d",
                                  "CHIRPSv3" = "#3d8d52")) +
    scale_fill_manual(values = c("Ground gauges" = "black", "CHIRPSv2" = "#7a3d8d", "CHIRPSv3" = "#3d8d7a")) +
    
    scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                "Sep", "Oct", "Nov", "Dec"),  # Adjust the month range
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    theme_classic2() +
    theme(plot.margin = unit(c(.01, .01, .01, .01), "cm"),# Reduce margin equally
          plot.title = element_text(size = 8),
          legend.title = element_text(size = 12, vjust = 0.0,hjust=0.5),
          legend.text = element_text(size = 10),
          #axis.text  =element_text(size=12),
          legend.position = "bottom",
          legend.title.align = 1,
          axis.title.y = element_text(size = 8,vjust = 0.,color='black',face = "bold"),
          axis.title.x = element_text(size = 8,color='black'),
          axis.text = element_text(size = 8,color='black'),
          legend.box.spacing = unit(-1, "pt"), 
          legend.margin = margin(1, 0.3, 0., 0.25),  # unit(c(top, right, bottom, left)
          legend.box.margin = margin(0, 0, 0, 0))
  
  max_value <- max(
    c(pcp_mean_monthly$pctl_75_ideam,
      pcp_mean_monthly$pctl_75_chirpsv2,
      pcp_mean_monthly$pctl_75_chirpsv3))
  
  plot_p1 <- plot_p +  
    annotate("text", 
             x = 1,
             y = c(max_value*1.22,
                   max_value*1.1,
                   max_value*.98),
             label = c(
               #paste0("MAP-", expression(IDEAM), " = ", round(sum(pcp_mean_monthly$mean_ideam)), 3),
               label = paste0("MAP[\"IDEAM\"] ==", round(sum(pcp_mean_monthly$mean_ideam),0)),
               label = paste0("MAP[\"CHIRPSv2\"] ==", round(sum(pcp_mean_monthly$mean_chirpsv2),0)),
               label = paste0("MAP[\"CHIRPSv3\"] ==", round(sum(pcp_mean_monthly$mean_chirpsv3),0))
             ), 
             parse=TRUE,
             color = c("black","#7a3d8d","#3d8d52"),
             size = 3, 
             hjust = 0,       
             vjust = 0.5) 
  
  return(plot_p1)
  
}

p_b <- plot_annual_cycle(15070080,"")
p_c <- plot_annual_cycle(25020860,"")
p_d <- plot_annual_cycle(11110030,"")
p_f <- plot_annual_cycle(21140080,"") #35030020
p_e <- plot_annual_cycle(35260070,"Monthly precipitation (mm)")
p_i <- plot_annual_cycle(42060010,"")
p_j <- plot_annual_cycle(47100020,"")
p_h <- plot_annual_cycle(52040070,"")
p_g <- plot_annual_cycle(53080020,"")


panel_rigth <- ggarrange(p_b, p_c, p_d, p_e, p_f, p_g, p_h, p_i, p_j,
                         ncol = 3, nrow = 3, align = "hv", 
                         labels = c("d", "e", "f", "g", "h", "i", "j", "k", "l"),
                         common.legend = TRUE, legend = "bottom",
                        hjust = c(-3,-3.5,-3.5), vjust = 0.83,
                        font.label = list(size = 10)) 
print(panel_rigth)
dev.off()








png(paste("G:/My Drive/R4C_et_al/3_PLOTS", "Fig2_panel_top.png",
          sep = '/'), units = "in",width = 8., height = 4, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(map_dif_plot,dry_year_plot,wet_year_plot,ncol=3,
          common.legend = TRUE, legend = "right",labels=c('a','b','c'))
dev.off()



#/////////////////////////////////////////////////////////////////////





