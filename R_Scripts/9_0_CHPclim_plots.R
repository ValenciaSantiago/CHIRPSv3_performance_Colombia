


#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,ggpubr,
       dplyr,stringr,quantmod,rnaturalearth,exactextractr,hydroGOF,GHCNr ,
       future.apply,profvis,rnaturalearthdata,glue)


#//////////////////////////////////////////////////
dir_CHPclim <- 'G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/5_CHPclim'
dir_pcp_data <- 'G:/My Drive/R4C_et_al/4_IDEAM_GPPs'

nat_reg_shp <- vect(paste0('G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS/',
                           'shp_regiones_naturales_colombia.shp'))


# Load CHPclim v1 and v2
CHPclim_files_v1 <- list.files(path = paste0(dir_CHPclim,"/v1/"),pattern = ".tif",full.names=TRUE)
CHPclim_files_v2 <- list.files(path = paste0(dir_CHPclim,"/v2/"),pattern = ".tif",full.names=TRUE)

CHPclim_rast_v1  <- rast(CHPclim_files_v1)
CHPclim_rast_v2  <- rast(CHPclim_files_v2)

NAflag <- -9999
CHPclim_rast_v1[CHPclim_rast_v1 == NAflag] <- NA
CHPclim_rast_v2[CHPclim_rast_v2 == NAflag] <- NA
CHPclim_dif      <- CHPclim_rast_v2 - CHPclim_rast_v1



colombia_extent <- ext(-80, -66.5, -4.5, 15)  # xmin, xmax, ymin, ymax


# Plot the raster using ggplot2
plot_CHPclim_function <- function(m,month){
  
  # 3. Crop the raster to Colombia
  CHPclim_colombia <- mask(CHPclim_dif[[m]], nat_reg_shp)
  
  # 4. Convert one or more layers to data frame
  # For example, let's take layer 1 (January)
  r_df <- as.data.frame(CHPclim_colombia, xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  
  col_pal_2 <- c('#EF8A47FF', '#F7AA58FF', '#FFD06FFF', 
                 '#FFE6B7FF', '#AADCE0FF', '#72BCD5FF', '#528FADFF', '#376795FF')

  
ggplot()+
  geom_tile(data = r_df, 
            aes(x = x, 
                y = y, 
                fill = value)) +
  # geom_sf(data = colombia, color = "black", fill = NA, size = 0.4)+
  geom_sf(data = st_as_sf(nat_reg_shp), color = adjustcolor("black",
                 alpha.f = 0.75), fill = NA, size = 0.01) +
  #geom_sf(data = pcp_gauges_sf, color = "black", fill = NA, size = 1.4,shape=19)+
  #geom_text(data = pcp_gauges_sf, 
  #          aes(x = longitude, y = latitude, label = ''), 
  #          color = "black", size = 5, vjust = .2, hjust = 1.4) +
  scale_fill_stepsn(name = "", 
                    colors = (col_pal_2),
                    limits=c(-200,200),
                    breaks = seq(-200,200,25),
                    #trans = scales::rescale(),
                    #trans = "log",
                    #trans = scales::pseudo_log_trans(), #  ADDED THIS LINE
                    labels = c("< -200","-175","-150","-125", "-100", "-75", "-50", "-25", "0", 
                               "25", "50", "75", "100","125","150","175", "> 200"),
                    guide = guide_colorbar(title = "CHPclimv2 minus CHPclimv1 (mm/month)", 
                                           title.position = "top", 
                                           label.position = "bottom",
                                           direction = "horizontal",
                                           barheight = unit(.4, "cm"),
                                           barwidth = unit(16, "cm"))) +
  #coord_sf(xlim = c(-80, -66), ylim = c(-5, 13), expand = FALSE) +  # Limit plot to extent
  theme_void() +
  scale_x_continuous(breaks = seq(-80, -66, by = 3))+
  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5),
        legend.text = element_text(size = 11),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title=month)

}


library(cowplot)
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_CHPclim_differences.png",
          sep = '/'), units = "in",width = 12, height = 6., 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(plot_CHPclim_function(1,"January"),plot_CHPclim_function(2,"February"),
          plot_CHPclim_function(3,'March'),plot_CHPclim_function(4,"April"),
          plot_CHPclim_function(5,'May'),plot_CHPclim_function(6,"June"),
          plot_CHPclim_function(7,"July"),plot_CHPclim_function(8,"August"),
          plot_CHPclim_function(9,'September'),plot_CHPclim_function(10,"October"),
          plot_CHPclim_function(11,'November'),plot_CHPclim_function(12,"December"),
          ncol=6,nrow=2,common.legend = TRUE,legend = 'bottom')

dev.off()



#____________________________________________________________________________
# Annual cycle CHPclimv2, CHPclimv1, and IDEAM gauges
# Extract CHPclimv2 and v1 for ideam gauges by region

pcp_month_df <- as.data.frame(fread(paste0(dir_pcp_data,'/IDEAM_GPPs_month.csv'),head=TRUE))
head(pcp_month_df)
colnames(pcp_month_df)


region_CHPclim_data <- function(region,region_name){
  
  pcp_month_region <- filter(pcp_month_df, nat_region %in% region)
  pcp_monthly      <- pcp_month_region %>%
    group_by(month,gauge_code) %>% 
    summarise(pcp_month = mean(pcp_ideam_flag,na.rm=TRUE)
              #pcp_month_25p = quantile(pcp_ideam_flag, 0.25, na.rm = TRUE),
              #pcp_month_75p = quantile(pcp_ideam_flag, 0.75, na.rm = TRUE),
              #pcp_month_sd = sd(pcp_ideam_flag,na.rm=TRUE)
    ) %>%
    group_by(month) %>%
    summarise(
      pcp_month_mean = mean(pcp_month,na.rm=TRUE),
      pcp_month_25p = quantile(pcp_month,0.25, na.rm = TRUE),
      pcp_month_75p = quantile(pcp_month,0.75, na.rm = TRUE),
      .groups = 'drop')
  #pcp_monthly
  
  
  
  # Extract CHPclim data
  points_vect <- vect(pcp_month_region, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  CHPclimv1_df <- terra::extract(CHPclim_rast_v1, points_vect)
  CHPclimv2_df <- terra::extract(CHPclim_rast_v2, points_vect)
  
  
  CHPclimv1_df <- CHPclimv1_df %>%
    pivot_longer(
      cols = starts_with("CHPclim."),
      names_to = "month",
      values_to = "CHPclimv1") %>%
    mutate(month = as.integer(sub("CHPclim\\.", "", month)))
  
  
  CHPclimv2_df <- CHPclimv2_df %>%
    pivot_longer(
      cols = starts_with("CHPclim2."),
      names_to = "month",
      values_to = "CHPclimv2") %>%
    mutate(month = as.integer(sub("CHPclim2.90-90\\.", "", month)))
  
  CHPclim_df <- merge(CHPclimv2_df,CHPclimv1_df,by=c('ID','month'))
  CHPclim_df <- CHPclim_df %>% arrange(ID,month)
  #head(CHPclim_df)
  
  
  
  pcp_monthly_CHPclim  <- CHPclim_df %>%
    group_by(ID,month) %>% 
    summarise(pcp_month_v1 = mean(CHPclimv1,na.rm=TRUE),
              pcp_month_v2 = mean(CHPclimv2,na.rm=TRUE)
              #pcp_month_v1_25p = quantile(CHPclimv1, 0.25, na.rm = TRUE),
              #pcp_month_v1_75p = quantile(CHPclimv1, 0.75, na.rm = TRUE),
              #pcp_month_v2_25p = quantile(CHPclimv2, 0.25, na.rm = TRUE),
              #pcp_month_v2_75p = quantile(CHPclimv2, 0.75, na.rm = TRUE
    ) %>%
    group_by(month) %>%
    summarise(pcp_month_mean_v1 = mean(pcp_month_v1,na.rm=TRUE),
              pcp_month_mean_v2 = mean(pcp_month_v2,na.rm=TRUE),
              pcp_month_v1_25p = quantile(pcp_month_v1, 0.25, na.rm = TRUE),
              pcp_month_v1_75p = quantile(pcp_month_v1, 0.75, na.rm = TRUE),
              pcp_month_v2_25p = quantile(pcp_month_v2, 0.25, na.rm = TRUE),
              pcp_month_v2_75p = quantile(pcp_month_v2, 0.75, na.rm = TRUE),
              .groups = 'drop')
  #head(pcp_monthly_CHPclim )
  
  pcp_df <- merge(pcp_monthly,pcp_monthly_CHPclim,by='month')
  return(pcp_df)}

pcp_data_and <- region_CHPclim_data("Andes","Andes")
pcp_data_pac <- region_CHPclim_data("Pacifico","Pacific")
pcp_data_car <- region_CHPclim_data("Caribe","Caribbean")
pcp_data_ama <- region_CHPclim_data("Amazonia","Amazonas")
pcp_data_ori <- region_CHPclim_data("Orinoquia","Orinoco")

pcp_data <- rbind(pcp_data_and,pcp_data_pac,pcp_data_car,
                  pcp_data_ama,pcp_data_ori)

KGE(pcp_data$pcp_month_mean_v1, pcp_data$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))
KGE(pcp_data$pcp_month_mean_v2, pcp_data$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))



KGE(pcp_data_and$pcp_month_mean_v1, pcp_data_and$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))
KGE(pcp_data_and$pcp_month_mean_v2, pcp_data_and$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))
KGE(pcp_data_pac$pcp_month_mean_v1, pcp_data_pac$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))
KGE(pcp_data_pac$pcp_month_mean_v2, pcp_data_pac$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))
KGE(pcp_data_car$pcp_month_mean_v1, pcp_data_car$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))
KGE(pcp_data_car$pcp_month_mean_v2, pcp_data_car$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))
KGE(pcp_data_ama$pcp_month_mean_v1, pcp_data_ama$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))
KGE(pcp_data_ama$pcp_month_mean_v2, pcp_data_ama$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))
KGE(pcp_data_ori$pcp_month_mean_v1, pcp_data_ori$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))
KGE(pcp_data_ori$pcp_month_mean_v2, pcp_data_ori$pcp_month_mean,
    s=c(1,1,1), na.rm=TRUE, method=c("2012"), out.type=c("full"))


region_CHPclim_plot <- function(region,region_name,ylabel,pmin,pmax,int){
  
  pcp_month_region <- filter(pcp_month_df, nat_region %in% region)
  pcp_monthly      <- pcp_month_region %>%
                          group_by(month,gauge_code) %>% 
                          summarise(pcp_month = mean(pcp_ideam_flag,na.rm=TRUE)
                                    #pcp_month_25p = quantile(pcp_ideam_flag, 0.25, na.rm = TRUE),
                                    #pcp_month_75p = quantile(pcp_ideam_flag, 0.75, na.rm = TRUE),
                                    #pcp_month_sd = sd(pcp_ideam_flag,na.rm=TRUE)
                                    ) %>%
                         group_by(month) %>%
                                    summarise(
                                    pcp_month_mean = mean(pcp_month,na.rm=TRUE),
                                    pcp_month_25p = quantile(pcp_month,0.25, na.rm = TRUE),
                                    pcp_month_75p = quantile(pcp_month,0.75, na.rm = TRUE),
                                    .groups = 'drop')
  #pcp_monthly
  
  
  
  # Extract CHPclim data
  points_vect <- vect(pcp_month_region, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  CHPclimv1_df <- terra::extract(CHPclim_rast_v1, points_vect)
  CHPclimv2_df <- terra::extract(CHPclim_rast_v2, points_vect)
  
  
  CHPclimv1_df <- CHPclimv1_df %>%
    pivot_longer(
      cols = starts_with("CHPclim."),
      names_to = "month",
      values_to = "CHPclimv1") %>%
    mutate(month = as.integer(sub("CHPclim\\.", "", month)))
  
  
  CHPclimv2_df <- CHPclimv2_df %>%
    pivot_longer(
      cols = starts_with("CHPclim2."),
      names_to = "month",
      values_to = "CHPclimv2") %>%
    mutate(month = as.integer(sub("CHPclim2.90-90\\.", "", month)))
  
  CHPclim_df <- merge(CHPclimv2_df,CHPclimv1_df,by=c('ID','month'))
  CHPclim_df <- CHPclim_df %>% arrange(ID,month)
  #head(CHPclim_df)
  
  
  
  pcp_monthly_CHPclim  <- CHPclim_df %>%
    group_by(ID,month) %>% 
    summarise(pcp_month_v1 = mean(CHPclimv1,na.rm=TRUE),
              pcp_month_v2 = mean(CHPclimv2,na.rm=TRUE)
              #pcp_month_v1_25p = quantile(CHPclimv1, 0.25, na.rm = TRUE),
              #pcp_month_v1_75p = quantile(CHPclimv1, 0.75, na.rm = TRUE),
              #pcp_month_v2_25p = quantile(CHPclimv2, 0.25, na.rm = TRUE),
              #pcp_month_v2_75p = quantile(CHPclimv2, 0.75, na.rm = TRUE
              ) %>%
    group_by(month) %>%
    summarise(pcp_month_mean_v1 = mean(pcp_month_v1,na.rm=TRUE),
              pcp_month_mean_v2 = mean(pcp_month_v2,na.rm=TRUE),
              pcp_month_v1_25p = quantile(pcp_month_v1, 0.25, na.rm = TRUE),
              pcp_month_v1_75p = quantile(pcp_month_v1, 0.75, na.rm = TRUE),
              pcp_month_v2_25p = quantile(pcp_month_v2, 0.25, na.rm = TRUE),
              pcp_month_v2_75p = quantile(pcp_month_v2, 0.75, na.rm = TRUE),
              .groups = 'drop')
  #head(pcp_monthly_CHPclim )
  
  pcp_df <- merge(pcp_monthly,pcp_monthly_CHPclim,by='month')
  
  
  plot_p <- ggplot(pcp_df, aes(x = month)) +
    geom_ribbon(aes(ymin = pcp_month_25p, ymax = pcp_month_75p), 
                fill = "black", alpha = 0.15) +
    geom_ribbon(aes(ymin = pcp_month_v1_25p, ymax = pcp_month_v1_75p),
                fill = "#7a3d8d", alpha = 0.25) +
    geom_ribbon(aes(ymin = pcp_month_v2_25p, ymax = pcp_month_v2_75p),
                fill = "#3d8d7a", alpha = 0.25) +
    
    # Add mean lines
    geom_line(aes(y = pcp_month_mean, color = "Ground gauges"), size = 1) +
    geom_line(aes(y = pcp_month_mean_v1, color = "CHPclimv1"), size = 1) +
    geom_line(aes(y = pcp_month_mean_v2, color = "CHPclimv2"), size = 1) +
    
    # Customize plot labels and themes
    labs(
      x = "",
      y = ylabel,
      title = region_name,
      color = "",
      fill="") +
    
    scale_color_manual(values = c("Ground gauges" = "black",
                                  "CHPclimv1" = "#7a3d8d",
                                  "CHPclimv2" = "#3d8d7a")) +
    scale_fill_manual(values = c("Ground gauges" = "black", "CHPclimv1" = "#7a3d8d", "CHPclimv2" = "#3d8d7a")) +
    
    scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                "Sep", "Oct", "Nov", "Dec"),  # Adjust the month range
                     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
    scale_y_continuous(breaks = seq(pmin,pmax,int),limits = c(pmin,pmax))+
    theme_classic2() +
    theme(plot.margin = unit(c(.001, .001, .001, .001), "cm"),# Reduce margin equally
          plot.title = element_text(size = 13,face='bold',color='black',
                                    hjust=0.5),
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
    
  print(plot_p)
  
  
}

pcp_plot_and <- region_CHPclim_plot("Andes","Andes","Precipitation (mm)",0,450,100)
pcp_plot_car <- region_CHPclim_plot("Caribe","Caribbean","Precipitation (mm)",0,300,100)
pcp_plot_ama <- region_CHPclim_plot(c("Amazonia","Amazonas"),"Amazon","",0,450,100)
pcp_plot_ori <- region_CHPclim_plot("Orinoquia","Orinoco","",0,450,100)
pcp_plot_pac <- region_CHPclim_plot("Pacifico","Pacific","",0,850,200)




png(paste(dir_plots_supp, "Fig_CHPclim_precipitation_regions.png",
          sep = '/'), units = "in",width = 11, height = 5, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(pcp_plot_and,pcp_plot_ama,pcp_plot_ori,pcp_plot_car,pcp_plot_pac,
          ncol=3,nrow=2,common.legend = TRUE,legend = 'bottom')

dev.off()



#/////////////////////////////////////////////////////////////////////////
dir_chirps_v3_cor_fact  <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/4_Correction_Factors"
cor_factor_v3 <- c(rast(list.files(path=dir_chirps_v3_cor_fact,
                                   pattern = ".tif",full.name=TRUE)))
plot(cor_factor_v3[[1]])

plot_cor_fact_function <- function(m,month){
  
  # 3. Crop the raster to Colombia
  CHPclim_colombia <- mask(cor_factor_v3[[m]], nat_reg_shp)
  
  # 4. Convert one or more layers to data frame
  # For example, let's take layer 1 (January)
  r_df <- as.data.frame(CHPclim_colombia, xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c("x", "y", "value")
  
  
  col_pal_2 <- c('#b35806','#e08214','#fdb863','#fee0b6','#d8daeb',
                 '#b2abd2','#8073ac','#542788')
  
  ggplot()+
    geom_tile(data = r_df, 
              aes(x = x, 
                  y = y, 
                  fill = value)) +
    # geom_sf(data = colombia, color = "black", fill = NA, size = 0.4)+
    geom_sf(data = st_as_sf(nat_reg_shp), color = adjustcolor("black",
                            alpha.f = 0.75), fill = NA, size = 0.01) +
    #geom_sf(data = pcp_gauges_sf, color = "black", fill = NA, size = 1.4,shape=19)+
    #geom_text(data = pcp_gauges_sf, 
    #          aes(x = longitude, y = latitude, label = ''), 
    #          color = "black", size = 5, vjust = .2, hjust = 1.4) +
    scale_fill_stepsn(name = "", 
                      colors = (col_pal_2),
                      limits=c(.8,1.2),
                      breaks = seq(.8,1.2,.05),
                      #trans = scales::rescale(),
                      #trans = "log",
                      #trans = scales::pseudo_log_trans(), #  ADDED THIS LINE
                      #labels = c("0","0.2","0.4","0.6", "0.8", "1.0", 
                      #           "1.2", "1.4", "1.6", "1.8", "2.0"),
                      guide = guide_colorbar(title = "Wind-correction factor", 
                                             title.position = "top", 
                                             label.position = "bottom",
                                             direction = "horizontal",
                                             barheight = unit(.4, "cm"),
                                             barwidth = unit(15, "cm"))) +
    #coord_sf(xlim = c(-80, -66), ylim = c(-5, 13), expand = FALSE) +  # Limit plot to extent
    theme_void() +
    scale_x_continuous(breaks = seq(-80, -66, by = 3))+
    #labels = c("78°W", "75°W", "72°W","69°W","X")) +
    scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
    theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
          legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5),
          legend.text = element_text(size = 11),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom",
          legend.title.align = 1,
          legend.box.spacing = unit(0, "pt")) +
    labs(x="",y="",title=month)
  
}


dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_Wind-correction_factor.png",
          sep = '/'), units = "in",width = 12, height = 6., 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(plot_cor_fact_function(1,"January"),plot_cor_fact_function(2,"February"),
          plot_cor_fact_function(3,'March'),plot_cor_fact_function(4,"April"),
          plot_cor_fact_function(5,'May'),plot_cor_fact_function(6,"June"),
          plot_cor_fact_function(7,"July"),plot_cor_fact_function(8,"August"),
          plot_cor_fact_function(9,'September'),plot_cor_fact_function(10,"October"),
          plot_cor_fact_function(11,'November'),plot_cor_fact_function(12,"December"),
          ncol=6,nrow=2,common.legend = TRUE,legend = 'bottom')

dev.off()









