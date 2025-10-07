

#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,
       dplyr,stringr,quantmod,rnaturalearth,exactextractr,hydroGOF,
       future.apply,profvis,rnaturalearthdata,glue,ggpubr, RColorBrewer )

#setwd("C:/Users/santiagovalencia/Documents")

#//////////////////////////////////////////////////
# directories and data
dir_plots           <- "G:/My Drive/R4C_et_al/3_PLOTS"
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
dir_IDEAM_GPPs      <- "G:/My Drive/R4C_et_al/4_IDEAM_GPPs"
#dir_IDEAM_GPPs      <- "D:/4_IDEAM_GPPs"

pcp_annual_data     <- fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_annual.csv"),head=TRUE) %>%
                            filter(na_count_ideam <= 30)
pcp_annual_data$date <- as.Date(paste0(pcp_annual_data$year,"-01-01"))
pcp_annual_data      <- filter(pcp_annual_data,year>'2000-01-01')

pcp_dry_season_data <- fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_dry_season_ACUM.csv"),head=TRUE)%>%
                            filter(na_count_ideam <= 15)
pcp_dry_season_data <- filter(pcp_dry_season_data,year>2000)
head(pcp_dry_season_data)


pcp_wet_season_data <- fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_wet_season_ACUM.csv"),head=TRUE) %>%
                             filter(na_count_ideam <= 15)
pcp_wet_season_data <- filter(pcp_wet_season_data,year>2000)


pcp_month_data      <- fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_month.csv"),head=TRUE)%>%
                             filter(na_count_ideam <= 5)
pcp_month_data      <- filter(pcp_month_data,date>'2000-12-31')


pcp_pentad_data     <- fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_pentad.csv"),head=TRUE)%>%
                             filter(na_count_ideam <= 1)
pcp_pentad_data     <- pcp_pentad_data

pcp_daily_data      <- fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_daily.csv"),head=TRUE)
pcp_daily_data$date <- as.Date(pcp_daily_data$date)
pcp_daily_data      <- pcp_daily_data[order(pcp_daily_data$date,pcp_daily_data$gauge_code), ]
pcp_daily_data     <- filter(pcp_daily_data,date > '2000-12-31')


#//////////////////////////////////////////////////////////////////////////////
# Scatterplots 
plot_scatterplot_function <- function(dataset, obs, gpp, start_date,
                                      end_date, title_plot, x_label,
                                      y_label, low_r, high_r,n,low_p,regions) {
  
  # Prepare data
  pcp_data_i <- as.data.frame(dataset %>% dplyr::select(!!sym(obs), !!sym(gpp),
                                                        !!sym("date"),!!sym("nat_region")))
  pcp_data_i <- filter(pcp_data_i, date >= start_date & date <= end_date)
  pcp_data_i <- pcp_data_i %>% filter(nat_region %in% regions)
  
  # Model and metrics
  lm_model <- lm(pcp_data_i[,1] ~ pcp_data_i[,2], data = pcp_data_i)
  r2   <- summary(lm_model)$r.squared
  r    <- cor(pcp_data_i[,1], pcp_data_i[,2],use='complete.obs')
  rmse <- sqrt(mean((pcp_data_i[,1] - pcp_data_i[,2])^2, na.rm = TRUE))
  pbias <- 100 * sum(pcp_data_i[,2] - pcp_data_i[,1], na.rm = TRUE) /
                sum(pcp_data_i[,1], na.rm = TRUE)
  
  
  # Generate the plot
  plot <- ggplot(pcp_data_i, aes(x = pcp_data_i[, 1], y = pcp_data_i[, 2])) +
    # geom_point(size=0.001,pch=0.1,alpha=0.5)+
    geom_bin2d(binwidth = c(n, n), alpha = 1,
               aes(fill = ..count../max(..count..)),  # Use bin count for color scale
               data = pcp_data_i) +  # Ensure data is used correctly
    #geom_smooth(aes(y = pcp_data_i[, 2]), method = "lm", color = 'black', lwd = .5,
    #           se = TRUE, linetype = "solid") +
    geom_abline(slope = 1, intercept = 0, lwd = 0.5, 
                color = 'black', linetype = 'dashed') +
    scale_fill_gradientn(
      #colors = c("#5495CFFF", "#FFEF42", "#FA0C08"),  # Define color scale from low (grey) to high (red)
      colors = rev(brewer.pal(11,"Spectral")),
      trans = 'log',  # Normal scale (not log scale)
      name = 'Density',  # Custom name for the legend
      breaks = c(low_p, 1),  # Set breaks at 0 and 1
      labels = c("Low", "High"),
      guide = guide_colorbar(title = "Density", 
                             title.position = "top", 
                             label.position = "bottom",
                             direction = "horizontal",
                             barheight = unit(.4, "cm"),
                             barwidth = unit(9, "cm")))+# Customize legend labels
    scale_y_continuous(limits = c(low_r , high_r)) +  # Set y-axis limits
    scale_x_continuous(limits = c(low_r , high_r)) +  # Set x-axis limits
    labs(
      title = title_plot,
      x = x_label, y = y_label,
      fill = 'Density'  # Change the legend title
    ) +
    theme_classic() +
   theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),# Reduce margin equally
                        plot.title = element_text(size = 12, face = "bold"),
                        legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
                        legend.text = element_text(size = 14,color='black'),
                        #axis.text  =element_text(size=12),
                        legend.position = "bottom",
                        legend.title.align = 1,
                        legend.box.spacing = unit(0, "pt"),
                        axis.title.y = element_text(size = 13,vjust = 0.5,color='black'),
                        axis.title.x = element_text(size = 13,color='black'),
                        axis.text = element_text(size = 12,color='black')) 
  #          axis.title.y = element_text(size = 12, angle = 0, hjust = 1, vjust = 0.5, margin = margin(r = 10)))  # Move y-label to the right

  
  # Adding R², RMSE, and N annotations to the plot
  plot <- plot +  
    annotate("text", 
             x = high_r * 0.01, 
             y = c(high_r * 0.9,
                   high_r * 0.82,
                   high_r * 0.74,
                   high_r * 0.66),
             label = c(paste0("N = ", dim(pcp_data_i)[1]),
                       #paste0("R² = ", round(r2, 2)),
                       paste0("r = ", round(r, 2)),
                       paste("RMSE = ", round(rmse, 2)," mm"),
                       paste("BIAS = ", round(pbias, 2)," %")),  
             color = "black", 
             size = 4,        
             hjust = 0,       
             vjust = 0.5)   
  
  # Return the plot
  return(plot)
}

# daily scale
daily_chirpv2 <- plot_scatterplot_function(pcp_daily_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                           "2023-12-31","","Ground observations (mm/day)",
                                           "CHIRPv2 (mm/day)",0,320,2.5,0.0000068,
                                           c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

daily_chirpv3 <- plot_scatterplot_function(pcp_daily_data,"pcp_ideam_flag","chirpv3","2001-01-01",
                                           "2023-12-31","","Ground observations (mm/day)",
                                           "CHIRPv3 (mm/day)",0,320,2.5,0.0000115,
                                           c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

daily_chirpsv2 <- plot_scatterplot_function(pcp_daily_data,"pcp_ideam_flag","chirpsv2","2001-01-01",
                                           "2023-12-31","","Ground observations (mm/day)",
                                           "CHIRPSv2 (mm/day)",0,320,2.5,0.0000115,
                                           c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))


daily_chirpsv3_era <- plot_scatterplot_function(pcp_daily_data,"pcp_ideam_flag","chirpsv3_era5","2001-01-01",
                                            "2023-12-31","","Ground observations (mm/day)",
                                            "CHIRPSv3-ERA5 (mm/day)",0,320,2.5,0.0000115,
                                            c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

daily_chirpsv3_imerg <- plot_scatterplot_function(pcp_daily_data,"pcp_ideam_flag","chirpsv3_imerg","2001-01-01",
                                                "2023-12-31","","Ground observations (mm/day)",
                                                "CHIRPSv3-IMERG (mm/day)",0,320,2.5,0.0000115,
                                                c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

daily_chirpsv3_era5_2 <- plot_scatterplot_function(pcp_daily_data,"pcp_ideam_flag","chirpsv3_era5","2001-01-01",
                                                  "2023-12-31","","Ground observations (mm/day)",
                                                  "CHIRPSv3-ERA5 (mm/day)",0,320,2.5,0.0000115,
                                                  c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))


png(paste0(dir_plots ,"/", "Fig_daily.png"), units = "in",
    width = 15, height = 8, res = 600, pointsize = 11)
ggarrange(daily_chirpv2,daily_chirpv3,daily_chirpsv2,
          daily_chirpsv3_era,daily_chirpsv3_era5_2 ,daily_chirpsv3_imerg,
          ncol = 3, nrow = 2, align = "hv", 
          labels = c("a", "b", "c","d",
                     "e", "f"),
          common.legend = TRUE, legend = "bottom", hjust = -0.5, vjust = 3)


dev.off()




#////////////////////////////////////////////
# pentad scale
pentad_chirpv2 <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                           "2023-12-31","","Ground observations (mm)",
                                           "CHIRPv2 (mm)",0,600,5,0.0000056,
                                           c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

pentad_chirpv3 <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpv3","2001-01-01",
                                           "2023-12-31","","Ground observations (mm)",
                                           "CHIRPv3 (mm",0,600,5,0.0000115,
                                           c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

pentad_chirpsv2 <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpsv2","2001-01-01",
                                            "2023-12-31","","Ground observations (mm)",
                                            "CHIRPSv2 (mm)",0,600,5,0.0000115,
                                            c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

pentad_chirpsv3 <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpsv3","2001-01-01",
                                            "2023-12-31","","Ground observations (mm)",
                                            "CHIRPSv3 (mm)",0,600,5,0.0000115,
                                            c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))



# monthly scale
month_chirpv2 <- plot_scatterplot_function(pcp_month_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                          "2023-12-31","","Ground observations (mm)",
                          "CHIRPv2 (mm)",0,2000,15,0.0000115,
                          c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

month_chirpv3 <- plot_scatterplot_function(pcp_month_data,"pcp_ideam_flag","chirpv3","2001-01-01",
                                           "2023-12-31","","Ground observations (mm)",
                                           "CHIRPv3 (mm)",0,2000,15,0.0000115,
                                           c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

month_chirpsv2 <- plot_scatterplot_function(pcp_month_data,"pcp_ideam_flag","chirpsv2","2001-01-01",
                                     "2023-12-31","","Ground observations (mm)",
                                     "CHIRPSv2 (mm)",0,2000,15,0.0000115,
                                     c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

month_chirpsv3 <- plot_scatterplot_function(pcp_month_data,"pcp_ideam_flag","chirpsv3","2001-01-01",
                                      "2023-12-31","","Ground observations (mm)",
                                      "CHIRPSv3 (mm)",0,2000,15,0.0000115,
                                      c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))


# annual scale
annual_chirpv2 <- plot_scatterplot_function(pcp_annual_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                           "2023-12-31","","Ground observations (mm)",
                                           "CHIRPv2 (mm)",0,9000,100,0.0000115,
                                           c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

annual_chirpv3 <- plot_scatterplot_function(pcp_annual_data,"pcp_ideam_flag","chirpv3","2001-01-01",
                                            "2023-12-31","","Ground observations (mm)",
                                            "CHIRPv3 (mm)",0,9000,100,0.0000115,
                                            c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

annual_chirpsv2 <- plot_scatterplot_function(pcp_annual_data,"pcp_ideam_flag","chirpsv2","2001-01-01",
                                            "2023-12-31","","Ground observations (mm)",
                                            "CHIRPSv2 (mm)",0,9000,100,0.0000115,
                                            c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

annual_chirpsv3 <- plot_scatterplot_function(pcp_annual_data,"pcp_ideam_flag","chirpsv3","2001-01-01",
                                            "2023-12-31","","Ground observations (mm)",
                                            "CHIRPSv3 (mm)",0,9000,100,0.0000115,
                                            c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))


# Use ggarrange to arrange the plots in a grid
pentad_annual_plot <- ggarrange(pentad_chirpv2,pentad_chirpv3,pentad_chirpsv2,pentad_chirpsv3,
                      month_chirpv2,month_chirpv3,month_chirpsv2,month_chirpsv3,
                      #month_chirpv2,month_chirpv3,month_chirpsv2,month_chirpsv3,
                      #month_chirpv2,month_chirpv3,month_chirpsv2,month_chirpsv3,
                      annual_chirpv2,annual_chirpv3,annual_chirpsv2,annual_chirpsv3,
                      ncol = 4, nrow = 3, align = "hv", 
                      labels = c("a", "b", "c","d",
                                 "e", "f","g","h",
                                 "i","j","k","l"),
                      common.legend = TRUE, legend = "bottom", hjust = -0.5, vjust = 3)

dir_plots_supp <- "G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS"
png(paste0(dir_plots_supp ,"/", "Fig_pentad_to_annual.png"), units = "in",
    width = 15, height = 10, res = 600, pointsize = 11)
pentad_annual_plot +   theme(
  plot.margin = unit(c(.01, .5, .01, .01), "cm"))+ 
  draw_plot_label(
    label = c("Pentad", "Monthly", "Annual"),
    size = 16,
    x = c(.99),  # Move the labels outside to the right
    y = c(0.79, 0.47, 0.185),  # Adjust y position for each label
    angle = 90)

dev.off()



#//////////////////////////////////////////////////////////////
plot_scatterplot_function_sea <- function(dataset, obs, gpp, start_date,
                                      end_date, title_plot, x_label,
                                      y_label, low_r, high_r,n,low_p,regions) {
  
  # Prepare data
  #dataset <- pcp_dry_season_data
  pcp_data_i <- as.data.frame(dataset %>% dplyr::select(!!sym(obs), !!sym(gpp),'nat_region'))
  #pcp_data_i <- pcp_data_i[,-c(1)]
  #pcp_data_i <- filter(pcp_data_i, date >= start_date & date <= end_date)
  pcp_data_i <- pcp_data_i %>% filter(nat_region %in% regions)
  
  # Model and metrics
  lm_model <- lm(pcp_data_i[,1] ~ pcp_data_i[,2], data = pcp_data_i)
  r2   <- summary(lm_model)$r.squared
  r    <- cor(pcp_data_i[,1], pcp_data_i[,2],use='complete.obs')
  rmse <- sqrt(mean((pcp_data_i[,1] - pcp_data_i[,2])^2, na.rm = TRUE))
  pbias <- 100 * sum(pcp_data_i[,2] - pcp_data_i[,1], na.rm = TRUE) /
    sum(pcp_data_i[,1], na.rm = TRUE)
  
  # Generate the plot
  plot <- ggplot(pcp_data_i, aes(x = pcp_data_i[, 1], y = pcp_data_i[, 2])) +
    # geom_point(size=0.001,pch=0.1,alpha=0.5)+
    geom_bin2d(binwidth = c(n, n), alpha = 1,
               aes(fill = ..count../max(..count..)),  # Use bin count for color scale
               data = pcp_data_i) +  # Ensure data is used correctly
    #geom_smooth(aes(y = pcp_data_i[, 2]), method = "lm", color = 'black', lwd = .5,
    #           se = TRUE, linetype = "solid") +
    geom_abline(slope = 1, intercept = 0, lwd = 0.5, 
                color = 'black', linetype = 'dashed') +
    scale_fill_gradientn(
      #colors = c("#5495CFFF", "#FFEF42", "#FA0C08"),  # Define color scale from low (grey) to high (red)
      colors = rev(brewer.pal(11,"Spectral")),
      trans = 'log',  # Normal scale (not log scale)
      name = 'Density',  # Custom name for the legend
      breaks = c(low_p, 1),  # Set breaks at 0 and 1
      labels = c("Low", "High"),
      guide = guide_colorbar(title = "Density", 
                             title.position = "top", 
                             label.position = "bottom",
                             direction = "horizontal",
                             barheight = unit(.4, "cm"),
                             barwidth = unit(9, "cm")))+# Customize legend labels
    scale_y_continuous(limits = c(low_r , high_r)) +  # Set y-axis limits
    scale_x_continuous(limits = c(low_r , high_r)) +  # Set x-axis limits
    labs(
      title = title_plot,
      x = x_label, y = y_label,
      fill = 'Density'  # Change the legend title
    ) +
    theme_classic() +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),# Reduce margin equally
          plot.title = element_text(size = 12, face = "bold"),
          legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
          legend.text = element_text(size = 14,color='black'),
          #axis.text  =element_text(size=12),
          legend.position = "bottom",
          legend.title.align = 1,
          legend.box.spacing = unit(0, "pt"),
          axis.title.y = element_text(size = 13,vjust = 0.5,color='black'),
          axis.title.x = element_text(size = 13,color='black'),
          axis.text = element_text(size = 12,color='black')) 
  #          axis.title.y = element_text(size = 12, angle = 0, hjust = 1, vjust = 0.5, margin = margin(r = 10)))  # Move y-label to the right
  
  
  # Adding R², RMSE, and N annotations to the plot
  plot <- plot +  
    annotate("text", 
             x = high_r * 0.01, 
             y = c(high_r * 0.9,
                   high_r * 0.82,
                   high_r * 0.74,
                   high_r * 0.66),
             label = c(paste0("N = ", dim(pcp_data_i)[1]),
                       #paste0("R² = ", round(r2, 2)),
                       paste0("r = ", round(r, 2)),
                       paste("RMSE = ", round(rmse, 2)," mm"),
                       paste("BIAS = ", round(pbias, 2)," %")),  
             color = "black", 
             size = 4,        
             hjust = 0,       
             vjust = 0.5)    
  
  # Return the plot
  return(plot)
}


dry_month_chirpv2 <- plot_scatterplot_function_sea(pcp_dry_season_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                               "2023-12-31","","Ground observations (mm)",
                                               "CHIRPv2 (mm)",0,4000,50,0.000515,
                                               c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))


dry_month_chirpv3 <- plot_scatterplot_function_sea(pcp_dry_season_data,"pcp_ideam_flag","chirpv3","2001-01-01",
                                               "2023-12-31","","Ground observations (mm)",
                                               "CHIRPv3 (mm)",0,4000,50,0.0000115,
                                               c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

dry_month_chirpsv2 <- plot_scatterplot_function_sea(pcp_dry_season_data,"pcp_ideam_flag","chirpsv2","2001-01-01",
                                                "2023-12-31","","Ground observations (mm)",
                                                "CHIRPSv2 (mm)",0,4000,50,0.0000115,
                                                c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

dry_month_chirpsv3 <- plot_scatterplot_function_sea(pcp_dry_season_data,"pcp_ideam_flag","chirpsv3","2001-01-01",
                                                "2023-12-31","","Ground observations (mm)",
                                                "CHIRPSv3 (mm)",0,4000,50,0.0000115,
                                                c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))


wet_month_chirpv2 <- plot_scatterplot_function_sea(pcp_wet_season_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                           "2023-12-31","","Ground observations (mm)",
                                           "CHIRPv2 (mm)",0,4000,50,0.0000115,
                                           c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

wet_month_chirpv3 <- plot_scatterplot_function_sea(pcp_wet_season_data,"pcp_ideam_flag","chirpv3","2001-01-01",
                                           "2023-12-31","","Ground observations (mm)",
                                           "CHIRPv3 (mm)",0,4000,50,0.0000115,
                                           c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

wet_month_chirpsv2 <- plot_scatterplot_function_sea(pcp_wet_season_data,"pcp_ideam_flag","chirpsv2","2001-01-01",
                                            "2023-12-31","","Ground observations (mm)",
                                            "CHIRPSv2 (mm)",0,4000,50,0.0000115,
                                            c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))

wet_month_chirpsv3 <- plot_scatterplot_function_sea(pcp_wet_season_data,"pcp_ideam_flag","chirpsv3","2001-01-01",
                                            "2023-12-31","","Ground observations (mm)",
                                            "CHIRPSv3 (mm)",0,4000,50,0.0000115,
                                            c("Andes","Pacifico","Caribe","Amazonas","Orinoquia","Amazonia"))



library(cowplot )
dir_plots_supp <- "G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS"
png(paste0(dir_plots_supp ,"/", "Fig_dry_wet_season.png"), units = "in",
    width = 14.5, height = 6.5, res = 600, pointsize = 11)

ggarrange(dry_month_chirpv2,dry_month_chirpv3,dry_month_chirpsv2,dry_month_chirpsv3,
          wet_month_chirpv2,wet_month_chirpv3,wet_month_chirpsv2,wet_month_chirpsv3,
          ncol = 4, nrow = 2, align = "hv", 
          labels = c("a", "b", "c","d",
                     "e", "f","g","h"),
          common.legend = TRUE, legend = "bottom", hjust = -0.5, vjust = 3) +
     theme(
    plot.margin = unit(c(.01, .5, .01, .01), "cm"))+ 
  draw_plot_label(
    label = c("Dry season", "Wet season"),
    size = 16,
    x = c(.99),  # Move the labels outside to the right
    y = c(0.665, 0.2),  # Adjust y position for each label
    angle = 90  # Rotate labels by 90 degrees
  )

dev.off()





#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////
# Natural regions plots

pentad_chirpv2_and <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                            "2023-12-31","","Ground observations (mm/pentad)",
                                            "CHIRPv2 (mm/pentad)",0,600,5,0.0000115,
                                            c("Andes"))

pentad_chirpv3_and <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                            "2023-12-31","","Ground observations (mm/pentad)",
                                            "CHIRPv3 (mm/pentad)",0,600,5,0.0000115,
                                            c("Andes"))

pentad_chirpsv2_and <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpsv2","2001-01-01",
                                             "2023-12-31","","Ground observations (mm/pentad)",
                                             "CHIRPSv2 (mm/pentad)",0,600,5,0.0000115,
                                             c("Andes"))

pentad_chirpsv3_and <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpsv3","2001-01-01",
                                             "2023-12-31","","Ground observations (mm/pentad)",
                                             "CHIRPSv3 (mm/pentad)",0,600,5,0.0000115,
                                             c("Andes"))



pentad_chirpv2_car <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                                "2023-12-31","","Ground observations (mm/pentad)",
                                                "CHIRPv2 (mm/pentad)",0,600,5,0.0000115,
                                                c("Caribe"))

pentad_chirpv3_car <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                                "2023-12-31","","Ground observations (mm/pentad)",
                                                "CHIRPv3 (mm/pentad)",0,600,5,0.0000115,
                                                c("Caribe"))

pentad_chirpsv2_car <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpsv2","2001-01-01",
                                                 "2023-12-31","","Ground observations (mm/pentad)",
                                                 "CHIRPSv2 (mm/pentad)",0,600,5,0.0000115,
                                                 c("Caribe"))

pentad_chirpsv3_car <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpsv3","2001-01-01",
                                                 "2023-12-31","","Ground observations (mm/pentad)",
                                                 "CHIRPSv3 (mm/pentad)",0,600,5,0.0000115,
                                                 c("Caribe"))




pentad_chirpv2_pac <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                                "2023-12-31","","Ground observations (mm/pentad)",
                                                "CHIRPv2 (mm/pentad)",0,600,5,0.0000115,
                                                c("Pacifico"))

pentad_chirpv3_pac <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpv2","2001-01-01",
                                                "2023-12-31","","Ground observations (mm/pentad)",
                                                "CHIRPv3 (mm/pentad)",0,600,5,0.0000115,
                                                c("Pacifico"))

pentad_chirpsv2_pac <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpsv2","2001-01-01",
                                                 "2023-12-31","","Ground observations (mm/pentad)",
                                                 "CHIRPSv2 (mm/pentad)",0,600,5,0.0000115,
                                                 c("Pacifico"))

pentad_chirpsv3_pac <- plot_scatterplot_function(pcp_pentad_data,"pcp_ideam_flag","chirpsv3","2001-01-01",
                                                 "2023-12-31","","Ground observations (mm/pentad)",
                                                 "CHIRPSv3 (mm/pentad)",0,600,5,0.0000115,
                                                 c("Pacifico"))




png(paste0(dir_plots_supp ,"/", "Fig_pentad_regions.png"), units = "in",
    width = 15, height = 20, res = 600, pointsize = 11)
ggarrange(pentad_chirpv2_and,pentad_chirpv3_and,pentad_chirpsv2_and,pentad_chirpsv3_and,
          pentad_chirpv2_and,pentad_chirpv3_and,pentad_chirpsv2_and,pentad_chirpsv3_and,
          pentad_chirpv2_pac,pentad_chirpv3_pac ,pentad_chirpsv2_pac ,pentad_chirpsv3_pac,
          pentad_chirpv2_car,pentad_chirpv3_car,pentad_chirpsv2_car,pentad_chirpsv3_car,
          ncol = 4, nrow = 5, align = "hv", 
          labels = c("a", "b", "c","d",
                     "e", "f","g","h"),
          common.legend = TRUE, legend = "bottom", hjust = -0.5, vjust = 3)

dev.off()









