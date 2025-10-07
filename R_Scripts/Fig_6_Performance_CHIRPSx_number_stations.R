
#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,PupillometryR,
       dplyr,stringr,quantmod,rnaturalearth,exactextractr,hydroGOF,ggpubr,grid,
       future.apply,profvis,rnaturalearthdata,glue,ggpubr,GHCNr,gridExtra,cowplot   )


#//////////////////////////////////////////////////
# directories and data
dir_plots           <- "G:/My Drive/R4C_et_al/3_PLOTS"
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
dir_IDEAM_GPPs      <- "G:/My Drive/R4C_et_al/4_IDEAM_GPPs"

countries  <- ne_countries(type = "countries",scale = "medium")[1]
colombia <- countries[countries$name == "Colombia", ]
nat_reg_shp <- st_read("G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS/shp_regiones_naturales_colombia.shp")


#///////////////////////////////////////////////////////////////////////////////
# load performance results

res_daily_data  <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                               "/res_performance_daily_2001_2023_df.csv")),head=TRUE)

res_pentad_data  <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                               "/res_performance_pentad_2001_2023_df.csv")),head=TRUE)

res_month_data   <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                             "/res_performance_monthly_2001_2023_df.csv")),head=TRUE)
#                                             "/res_performance_monthly_2004_2015_df.csv")),head=TRUE)
res_annual_data <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                             "/res_performance_annual_2001_2023_df.csv")),head=TRUE)

res_month_data_p1  <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                                 "/res_performance_monthly_2001_2011_df.csv")),head=TRUE)
res_month_data_p2  <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                                 "/res_performance_monthly_2012_2023_df.csv")),head=TRUE)

res_pentad_data_p1  <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                                 "/res_performance_pentad_2001_2011_df.csv")),head=TRUE)
res_pentad_data_p2  <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                                 "/res_performance_pentad_2012_2023_df.csv")),head=TRUE)




#"/res_performance_annual_2004_2015_df.csv")),head=TRUE)
head(res_month_data)
unique(res_month_data$nat_region)

# load annual precipitation data
pcp_annual      <- fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_annual.csv"),head=TRUE)%>%
                                filter(na_count_ideam <= 30)
pcp_annual_mean <- pcp_annual  %>%
                    group_by(gauge_code) %>%
                    summarise(
                      annual_mean    = mean(pcp_ideam_flag, na.rm = TRUE),  
                      .groups = "drop") 
pcp_annual_mean 


res_month_data <- merge(res_month_data,pcp_annual_mean,by="gauge_code")
head(res_month_data )


#______________________________________________________
# Fig 6
res_month_data$mean_gauges_v3_scale <-  round(res_month_data$mean_gauges_v3,0)
kge_month_v3 <- ggplot(res_month_data, aes(x = as.factor(mean_gauges_v3_scale),
          y = kge_chirps_v3)) +
        geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
                     fill = adjustcolor("#3d8d52", alpha.f = .8)) +
        # Add count annotations above each boxplot
        geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
        geom_text(
          data = res_month_data %>%
            group_by(as.factor(mean_gauges_v3_scale)) %>%
            summarize(count = n()),  # Summarize the count of each max_gauges_v3
          aes(x = seq(1,17,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
          vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
        labs(x = "Mean number of ground stations by 0.25° pixel",
             y = "Kling-Gupta \n Efficiency (KGE)", title = "CHIRPSv3") +
        theme_classic()+
        scale_y_continuous(limits=c(0,1))+
        geom_hline(yintercept = median(res_month_data$kge_chirps_v3), linetype = "dotted", color = "#d95f02", size = .7)+
        theme(plot.margin = unit(c(.2, .25, .5, .2), "cm"),# #c(top, right, bottom, left)
        plot.title = element_text(size = 13,face='bold',color='black'),
              legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
              legend.text = element_text(size = 13,color='black'),
              #axis.text  =element_text(size=12),
              legend.position = "none",
              legend.title.align = 1,
              axis.title.y = element_text(size = 12,vjust = 0.5,color='black'),
              axis.title.x = element_text(size = 12,color='black'),
              axis.text = element_text(size = 12,color='black'),
              legend.box.spacing = unit(-.001, "pt"), 
              legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
              legend.box.margin = margin(1, 1, 1, 1))
kge_month_v3


res_month_data$mean_gauges_v2_scale <-  round(res_month_data$mean_gauges_v2,0)
kge_month_v2 <- ggplot(res_month_data, aes(x = as.factor(mean_gauges_v2_scale),
                                           y = kge_chirps_v2)) +
            geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
                         fill = adjustcolor("#7a3d8d", alpha.f = .8)) +
            # Add count annotations above each boxplot
            geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
            geom_text(
              data = res_month_data %>%
                group_by(as.factor(mean_gauges_v2_scale)) %>%
                summarize(count = n()),  # Summarize the count of each max_gauges_v3
              aes(x = seq(1,15,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
              vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
            labs(x = "Mean number of ground stations by 0.25° pixel",
                 y = "Kling-Gupta \n Efficiency (KGE)", title = "CHIRPSv2") +
            theme_classic()+
            scale_y_continuous(limits=c(0,1))+
            geom_hline(yintercept = median(res_month_data$kge_chirps_v2), linetype = "dotted", color = "#d95f02", size = .7)+
            theme(plot.margin = unit(c(.2, .5, .5, .5), "cm"), #c(top, right, bottom, left)
                  plot.title = element_text(size = 13,face='bold',color='black'),
                  legend.title = element_text(size = 13, vjust = 0.0,hjust=0.5,color='black'),
                  legend.text = element_text(size = 13,color='black'),
                  legend.position = "none",
                  legend.title.align = 1,
                  axis.title.y = element_text(size = 12,vjust = 0.5,color='black'),
                  axis.title.x = element_text(size = 12,color='black'),
                  axis.text = element_text(size = 12,color='black'),
                  legend.box.spacing = unit(-.001, "pt"), 
                  legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
                  legend.box.margin = margin(2, 2, 2, 2)) # top, right, bottom, left







res_pentad_data$mean_gauges_v3_scale <-  round(res_pentad_data$mean_gauges_v3,0)
kge_pentad_v3 <- ggplot(res_pentad_data, aes(x = as.factor(mean_gauges_v3_scale),
                y = kge_chirps_v3)) +
            geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
                         fill = adjustcolor("#3d8d52", alpha.f = .8)) +
            # Add count annotations above each boxplot
            geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
            geom_text(
              data = res_month_data %>%
                group_by(as.factor(mean_gauges_v3_scale)) %>%
                summarize(count = n()),  # Summarize the count of each max_gauges_v3
              aes(x = seq(1,17,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
              vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
            labs(x = "Mean number of ground stations by 0.25° pixel",
                 y = "Kling-Gupta \n Efficiency (KGE)", title = "CHIRPSv3") +
            theme_classic()+
            scale_y_continuous(limits=c(0,1))+
            geom_hline(yintercept = median(res_pentad_data$kge_chirps_v3), linetype = "dotted", color = "#d95f02", size = .7)+
            theme(plot.margin = unit(c(.2, .0, .5, .2), "cm"),#c(top, right, bottom, left)
                  plot.title = element_text(size = 13,face='bold',color='black'),
                  legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
                  legend.text = element_text(size = 13,color='black'),
                  #axis.text  =element_text(size=12),
                  legend.position = "none",
                  legend.title.align = 1,
                  axis.title.y = element_text(size = 12,vjust = 0.5,color='black'),
                  axis.title.x = element_text(size = 12,color='black'),
                  axis.text = element_text(size = 12,color='black'),
                  legend.box.spacing = unit(-.001, "pt"), 
                  legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
                  legend.box.margin = margin(1, 1, 1,1))
kge_pentad_v3



res_pentad_data$mean_gauges_v2_scale <-  round(res_pentad_data$mean_gauges_v2,0)
kge_pentad_v2 <- ggplot(res_pentad_data, aes(x = as.factor(mean_gauges_v2_scale),
                                             y = kge_chirps_v2)) +
                    geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
                                 fill = adjustcolor("#7a3d8d", alpha.f = .8)) +
                    # Add count annotations above each boxplot
                    geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
                    geom_text(
                      data = res_month_data %>%
                        group_by(as.factor(mean_gauges_v2_scale)) %>%
                        summarize(count = n()),  # Summarize the count of each max_gauges_v3
                      aes(x = seq(1,15,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
                      vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
                    labs(x = "Mean number of ground stations by 0.25° pixel",
                         y = "Kling-Gupta \n Efficiency (KGE)", title = "CHIRPSv2") +
                    theme_classic()+
                    scale_y_continuous(limits=c(0,1))+
                    geom_hline(yintercept = median(res_pentad_data$kge_chirps_v2), linetype = "dotted", color = "#d95f02", size = .7)+
                    theme(plot.margin = unit(c(.2, .0, .5, .2), "cm"),
                          plot.title = element_text(size = 13,face='bold',color='black'),
                          legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
                          legend.text = element_text(size = 13,color='black'),
                          #axis.text  =element_text(size=12),
                          legend.position = "none",
                          legend.title.align = 1,
                          axis.title.y = element_text(size = 12,vjust = 0.5,color='black'),
                          axis.title.x = element_text(size = 12,color='black'),
                          axis.text = element_text(size = 12,color='black'),
                          legend.box.spacing = unit(-.001, "pt"), 
                          legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
                          legend.box.margin = margin(2, 2, 2, 2)) # top, right, bottom, left
kge_pentad_v2                  







#//////////////////////////////////////////////////////////////////////////////


r_pentad_v3 <- ggplot(res_pentad_data, aes(x = as.factor(mean_gauges_v3_scale),
                                           y = r_chirps_v3)) +
  geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
               fill = adjustcolor("#3d8d52", alpha.f = .8)) +
  # Add count annotations above each boxplot
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
  geom_text(
    data = res_month_data %>%
      group_by(as.factor(mean_gauges_v3_scale)) %>%
      summarize(count = n()),  # Summarize the count of each max_gauges_v3
    aes(x = seq(1,17,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
    vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
  labs(x = "Mean number of ground stations by 0.25° pixel",
       y = "Correlation coeficient (r)",, title = "CHIRPSv3") +
  theme_classic()+
  scale_y_continuous(limits=c(-0.05,1))+
  geom_hline(yintercept = median(res_pentad_data$r_chirps_v3),
             linetype = "dotted", color = "#d95f02", size = .7) +
  theme(plot.margin = unit(c(.2, .0, .5, .2), "cm"),#c(top, right, bottom, left)
        plot.title = element_text(size = 13,face='bold',color='black'),
        legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
        legend.text = element_text(size = 13,color='black'),
        #axis.text  =element_text(size=12),
        legend.position = "none",
        legend.title.align = 1,
        axis.title.y = element_text(size = 12,vjust = 0.5,color='black'),
        axis.title.x = element_text(size = 12,color='black'),
        axis.text = element_text(size = 12,color='black'),
        legend.box.spacing = unit(-.001, "pt"), 
        legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
        legend.box.margin = margin(1, 1, 1,1))
r_pentad_v3



r_pentad_v2 <- ggplot(res_pentad_data, aes(x = as.factor(mean_gauges_v2_scale),
                                             y = r_chirps_v2)) +
  geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
               fill = adjustcolor("#7a3d8d", alpha.f = .8)) +
  # Add count annotations above each boxplot
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
  geom_text(
    data = res_month_data %>%
      group_by(as.factor(mean_gauges_v2_scale)) %>%
      summarize(count = n()),  # Summarize the count of each max_gauges_v3
    aes(x = seq(1,15,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
    vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
  labs(x = "Mean number of ground stations by 0.25° pixel",
       y = 'Correlation coefficient (r)', title = "CHIRPSv2") +
  theme_classic()+
  scale_y_continuous(limits=c(0,1))+
  geom_hline(yintercept = median(res_pentad_data$r_chirps_v2), linetype = "dotted", color = "#d95f02", size = .7)+
  theme(plot.margin = unit(c(.2, .0, .5, .2), "cm"),
        plot.title = element_text(size = 13,face='bold',color='black'),
        legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
        legend.text = element_text(size = 13,color='black'),
        #axis.text  =element_text(size=12),
        legend.position = "none",
        legend.title.align = 1,
        axis.title.y = element_text(size = 12,vjust = 0.5,color='black'),
        axis.title.x = element_text(size = 12,color='black'),
        axis.text = element_text(size = 12,color='black'),
        legend.box.spacing = unit(-.001, "pt"), 
        legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
        legend.box.margin = margin(2, 2, 2, 2)) # top, right, bottom, left
r_pentad_v2 


B_pentad_v3 <- ggplot(res_pentad_data, aes(x = as.factor(mean_gauges_v3_scale),
                                             y = B_chirps_v3)) +
  geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
               fill = adjustcolor("#3d8d52", alpha.f = .8)) +
  # Add count annotations above each boxplot
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
  #geom_text(
  #  data = res_month_data %>%
  #    group_by(as.factor(mean_gauges_v3_scale)) %>%
  #    summarize(count = n()),  # Summarize the count of each max_gauges_v3
  #  aes(x = seq(1,17,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
  #  vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
  labs(x = "Mean number of ground stations by 0.25° pixel",
       y = paste0("Bias ratio (","\u03B2",")"), title = "") +
  theme_classic()+
  scale_y_continuous(limits=c(0,2))+
  geom_hline(yintercept = median(res_pentad_data$B_chirps_v3),
             linetype = "dotted", color = "#d95f02", size = .7) +
  theme(plot.margin = unit(c(.2, .0, .5, .2), "cm"),#c(top, right, bottom, left)
        plot.title = element_text(size = 13,face='bold',color='black'),
        legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
        legend.text = element_text(size = 13,color='black'),
        #axis.text  =element_text(size=12),
        legend.position = "none",
        legend.title.align = 1,
        axis.title.y = element_text(size = 12,vjust = 0.5,color='black'),
        axis.title.x = element_text(size = 12,color='black'),
        axis.text = element_text(size = 12,color='black'),
        legend.box.spacing = unit(-.001, "pt"), 
        legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
        legend.box.margin = margin(1, 1, 1,1))
B_pentad_v3





B_pentad_v2 <- ggplot(res_pentad_data, aes(x = as.factor(mean_gauges_v2_scale),
                                           y = B_chirps_v2)) +
  geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
               fill = adjustcolor("#7a3d8d", alpha.f = .8)) +
  # Add count annotations above each boxplot
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
  #geom_text(
  #  data = res_month_data %>%
  #    group_by(as.factor(mean_gauges_v2_scale)) %>%
  #    summarize(count = n()),  # Summarize the count of each max_gauges_v3
  #  aes(x = seq(1,15,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
  #  vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
  labs(x = "Mean number of ground stations by 0.25° pixel",
       y = paste0("Bias ratio (","\u03B2",")"), title = "") +
  theme_classic()+
  scale_y_continuous(limits=c(0,2))+
  geom_hline(yintercept = median(res_pentad_data$B_chirps_v2), linetype = "dotted", color = "#d95f02", size = .7)+
  theme(plot.margin = unit(c(.2, .0, .5, .2), "cm"),
        plot.title = element_text(size = 13,face='bold',color='black'),
        legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
        legend.text = element_text(size = 13,color='black'),
        #axis.text  =element_text(size=12),
        legend.position = "none",
        legend.title.align = 1,
        axis.title.y = element_text(size = 12,vjust = 0.5,color='black'),
        axis.title.x = element_text(size = 12,color='black'),
        axis.text = element_text(size = 12,color='black'),
        legend.box.spacing = unit(-.001, "pt"), 
        legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
        legend.box.margin = margin(2, 2, 2, 2)) # top, right, bottom, left
B_pentad_v2





G_pentad_v3 <- ggplot(res_pentad_data, aes(x = as.factor(mean_gauges_v3_scale),
                                           y = G_chirps_v3)) +
  geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
               fill = adjustcolor("#3d8d52", alpha.f = .8)) +
  # Add count annotations above each boxplot
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
  #geom_text(
  #  data = res_month_data %>%
  #    group_by(as.factor(mean_gauges_v3_scale)) %>%
  #    summarize(count = n()),  # Summarize the count of each max_gauges_v3
  #  aes(x = seq(1,17,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
  #  vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
  labs(x = "Mean number of ground stations by 0.25° pixel",
       y = paste0("Variability ratio (","\u03B3",")"), title = "") +
  theme_classic()+
  scale_y_continuous(limits=c(0,1.5))+
  geom_hline(yintercept = median(res_pentad_data$G_chirps_v3),
             linetype = "dotted", color = "#d95f02", size = .7) +
  theme(plot.margin = unit(c(.2, .0, .5, .2), "cm"),#c(top, right, bottom, left)
        plot.title = element_text(size = 13,face='bold',color='black'),
        legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
        legend.text = element_text(size = 13,color='black'),
        #axis.text  =element_text(size=12),
        legend.position = "none",
        legend.title.align = 1,
        axis.title.y = element_text(size = 12,vjust = 0.5,color='black'),
        axis.title.x = element_text(size = 12,color='black'),
        axis.text = element_text(size = 12,color='black'),
        legend.box.spacing = unit(-.001, "pt"), 
        legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
        legend.box.margin = margin(1, 1, 1,1))
G_pentad_v3




G_pentad_v2 <- ggplot(res_pentad_data, aes(x = as.factor(mean_gauges_v2_scale),
                                           y = G_chirps_v2)) +
  geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
               fill = adjustcolor("#7a3d8d", alpha.f = .8)) +
  # Add count annotations above each boxplot
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
  #geom_text(
  #  data = res_month_data %>%
  #    group_by(as.factor(mean_gauges_v2_scale)) %>%
  #    summarize(count = n()),  # Summarize the count of each max_gauges_v3
  #  aes(x = seq(1,15,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
  #  vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
  labs(x = "Mean number of ground stations by 0.25° pixel",
       y = paste0("Variability ratio (","\u03B3",")"), title = "") +
  theme_classic()+
  scale_y_continuous(limits=c(0,1.5))+
  geom_hline(yintercept = median(res_pentad_data$G_chirps_v2), linetype = "dotted", color = "#d95f02", size = .7)+
  theme(plot.margin = unit(c(.2, .0, .5, .2), "cm"),
        plot.title = element_text(size = 13,face='bold',color='black'),
        legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
        legend.text = element_text(size = 13,color='black'),
        #axis.text  =element_text(size=12),
        legend.position = "none",
        legend.title.align = 1,
        axis.title.y = element_text(size = 12,vjust = 0.5,color='black'),
        axis.title.x = element_text(size = 12,color='black'),
        axis.text = element_text(size = 12,color='black'),
        legend.box.spacing = unit(-.001, "pt"), 
        legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
        legend.box.margin = margin(2, 2, 2, 2)) # top, right, bottom, left
G_pentad_v2




png(paste0(dir_plots ,"/SUPP_PLOTS/",
           "Fig_performance_stations_pentad_metrics.png"), units = "in",
    width = 11, height = 7.5, res = 600, pointsize = 11)


ggarrange(r_pentad_v2,r_pentad_v3,
          B_pentad_v2,B_pentad_v3,
          G_pentad_v2,G_pentad_v3,
          nrow=3,ncol=2,labels=c('a','b','c',
                                 'd','e','f'))
dev.off()



#///////////////////////////////////////////////////////////////////////////
#_____________________________________________________________________
# Periods analysis

#res_month_data_p1,res_month_data_p2
function_plot_periods <- function(dataset1,dataset2,names,x_label,r_low,h_low,int,opt_value){
  
  res_month_data_p1_subset <- dataset1 %>% select(all_of(c(names)))
  res_month_data_p2_subset <- dataset2 %>% select(all_of(c(names)))
  head(res_month_data_p1_subset)
  
  colnames(res_month_data_p1_subset ) <- c("CHIRPSv2","CHIRPSv3")
  colnames(res_month_data_p2_subset ) <- c("CHIRPSv2","CHIRPSv3")
  
  
  # Combine the two periods into one data frame for plotting
  df_var <- bind_rows(
   # res_month_data_p1_subset %>%
     # transmute(
     #   period = "2001-2011",
     #   var = CHIRPv2,
     #   product = "CHIRPv2"
   #   ),
   # res_month_data_p1_subset %>%
   #   transmute(
    #    period = "2001-2011",
    #    var = CHIRPv3,
    ##    product = "CHIRPv3"
    #  ),
    res_month_data_p1_subset %>%
      transmute(
        period = "2001-2011",
        var = CHIRPSv2,
        product = "CHIRPSv2"
      ),
    res_month_data_p1_subset %>%
      transmute(
        period = "2001-2011",
        var = CHIRPSv3,
        product = "CHIRPSv3"
      ),
   # res_month_data_p2_subset %>%
   #   transmute(
   #     period = "2012-2023",
   #     var = CHIRPv2,
   #     product = "CHIRPv2"
    #  ),
    #res_month_data_p2_subset %>%
    #  transmute(
    #    period = "2012-2023",
     #   var= CHIRPv3,
     #   product = "CHIRPv3"
    #  ),
    res_month_data_p2_subset %>%
      transmute(
        period = "2012-2023",
        var = CHIRPSv2,
        product = "CHIRPSv2"
      ),
    res_month_data_p2_subset %>%
      transmute(
        period = "2012-2023",
        var= CHIRPSv3,
        product = "CHIRPSv3"
      )
  )
  
  df_var$product <- factor(df_var$product, levels = c("CHIRPSv2", "CHIRPSv3"))
  
  plot_violin <- ggplot(df_var, aes(y = var, x = product, fill = period)) + 
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = .5) +
    geom_flat_violin(
      position = position_nudge(x = .35, y = 0), 
      alpha = 0.5, 
      trim = TRUE,
      stat = "ydensity",
      scale = "area",
      color = NA
    ) +
    coord_flip() + 
    geom_boxplot(
      width = .25, 
      outlier.shape = NA, 
      color = "black",
      position = position_dodge(.5)
    ) + 
    scale_fill_manual(
      name = "Period",
      values = c(
        "2001-2011" = adjustcolor('#BA0C2FFF', alpha.f = 0.6),
        "2012-2023" = adjustcolor('#333F48FF', alpha.f = 0.6)
      )
    ) +
    stat_summary(
      fun = median, 
      geom = "text", 
      aes(label = round(..y.., 2)), 
      position = position_dodge(1.1), 
      colour = "black", 
      size = 2.8, 
      vjust = 0.45
    ) +
    scale_y_continuous(
      limits = c(r_low, h_low),
      breaks = seq(r_low, h_low, by = int),
      labels = seq(r_low, h_low, by = int)
    ) +
    xlab('') +
    ylab(x_label) +
    theme_classic() +  # Apply base theme
    theme(
      plot.margin = unit(c(.2, .2, .2, .2), "cm"),
      plot.title = element_text(size = 10, color = 'black'),
      axis.title.y = element_text(size = 12, vjust = 0.5, color = 'black'),
      axis.title.x = element_text(size = 12, color = 'black'),
      axis.text = element_text(size = 12, color = 'black'),
      legend.title = element_text(size = 16, vjust = 0.0, hjust = 0.5, color = 'black'),
      legend.text = element_text(size = 14, color = 'black'),
      legend.position = "none",
      legend.title.align = 1,
      legend.box.spacing = unit(-.001, "pt"), 
      legend.margin = margin(.001, 0.03, 0., 0.0025),
      legend.box.margin = margin(0, 0, 0, 0))
  return(plot_violin)
}


#_______________________________________________________________________
# Monthly scale

r_periods <- function_plot_periods(res_month_data_p1,res_month_data_p2,
                                    c(#"r_chirp_v2", 
                                     "r_chirps_v2",
                                     #"r_chirp_v3",
                                     "r_chirps_v3"),
                                   'Correlation coefficient (r)',-0.75,1,.25,1)

r_periods  <- r_periods  + 
  theme(
    legend.position = c(0.3, 0.75),  
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
r_periods


B_periods <- function_plot_periods(res_month_data_p1,res_month_data_p2,
                                    c(#"B_chirp_v2",
                                     "B_chirps_v2",
                                     #"B_chirp_v3",
                                     "B_chirps_v3"),
                                   paste0("Bias ratio (","\u03B2",")"),-0,2,.5,1)
#B_periods <- B_periods + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


G_periods <- function_plot_periods(res_month_data_p1,res_month_data_p2,
                                   c(#"G_chirp_v2",
                                     "G_chirps_v2", 
                                     #"G_chirp_v3", 
                                     "G_chirps_v3"),
                                   paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5,1)
#G_periods <- G_periods + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


kge_periods <- function_plot_periods(res_month_data_p1,res_month_data_p2,
                                      c(#"kge_chirp_v2",
                                       "kge_chirps_v2",
                                       #"kge_chirp_v3",
                                       "kge_chirps_v3"),
                                     'Kling-Gupta Efficiency (KGE)',-0.75,1,.25,1)

#kge_periods <- kge_periods + theme(axis.text.y = element_blank(),axis.title.y = element_blank())

r_periods1<- r_periods
r_periods1 <- r_periods1+ scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))

#r_pentad <- r_pentad + theme(legend.position = "none")
B_periods <- B_periods +   theme(legend.position = "none",
                                 axis.title.y = element_blank(),
                                 axis.text.y = element_blank())
G_periods <- G_periods +   theme(legend.position = "none",
                                 axis.title.y = element_blank(),
                                 axis.text.y = element_blank())
kge_periods <- kge_periods +   theme(legend.position = "none",
                                     axis.title.y = element_blank(),
                                     axis.text.y = element_blank())
kge_periods <- kge_periods + scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))





#_______________________________________________________________________
# Pentad scale

r_pentad_periods <- function_plot_periods(res_pentad_data_p1,res_pentad_data_p2,
                                   c(#"r_chirp_v2", 
                                     "r_chirps_v2",
                                     #"r_chirp_v3",
                                     "r_chirps_v3"),
                                   'Correlation coefficient (r)',-0.75,1,.25,1)

r_pentad_periods  <- r_pentad_periods  + 
  theme(
    legend.position = c(0.3, 0.75),  
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
r_pentad_periods


B_pentad_periods <- function_plot_periods(res_pentad_data_p1,res_pentad_data_p2,
                                   c(#"B_chirp_v2",
                                     "B_chirps_v2",
                                     #"B_chirp_v3",
                                     "B_chirps_v3"),
                                   paste0("Bias ratio (","\u03B2",")"),-0,2,.5,1)
#B_periods <- B_periods + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


G_pentad_periods <- function_plot_periods(res_pentad_data_p1,res_pentad_data_p2,
                                   c(#"G_chirp_v2",
                                     "G_chirps_v2", 
                                     #"G_chirp_v3", 
                                     "G_chirps_v3"),
                                   paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5,1)
#G_periods <- G_periods + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


kge_pentad_periods <- function_plot_periods(res_pentad_data_p1,res_pentad_data_p2,
                                     c(#"kge_chirp_v2",
                                       "kge_chirps_v2",
                                       #"kge_chirp_v3",
                                       "kge_chirps_v3"),
                                     'Kling-Gupta Efficiency (KGE)',-0.75,1,.25,1)

#kge_periods <- kge_periods + theme(axis.text.y = element_blank(),axis.title.y = element_blank())

r_pentad_periods1<- r_pentad_periods
r_pentad_periods1 <- r_pentad_periods1+ scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))

#r_pentad <- r_pentad + theme(legend.position = "none")
B_pentad_periods <- B_pentad_periods +   theme(legend.position = "none",
                                 axis.title.y = element_blank(),
                                 axis.text.y = element_blank())
G_pentad_periods <- G_pentad_periods +   theme(legend.position = "none",
                                 axis.title.y = element_blank(),
                                 axis.text.y = element_blank())
kge_pentad_periods <- kge_pentad_periods +   theme(legend.position = "none",
                                     axis.title.y = element_blank(),
                                     axis.text.y = element_blank())
kge_pentad_periods <- kge_pentad_periods + scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))


#____________________________________________________________________________
# plot

#png(paste0(dir_plots ,"/", "Fig6_performance_stations_pentad.png"), units = "in",
#    width = 10, height = 5, res = 600, pointsize = 11)
#pdf(paste0(dir_plots, "/", "Fig_6_performance_CHIRPvx_number_stations_pentad.pdf"),
#    width = 10, height = 5, pointsize = 11)
library(grDevices )
cairo_pdf(paste0(dir_plots, "/", "Fig_6_performance_CHIRPvx_number_stations_pentad.pdf"),
          width = 10, height = 5, pointsize = 11)
r_pentad_periods11 <- r_pentad_periods1 +
                     theme(plot.margin = unit(c(.02, .0, .05, .0), "cm"))

blank_plot <- ggplot() + theme_void()
p1 <- ggarrange(ggarrange(blank_plot,kge_pentad_v2,kge_pentad_v3,
    widths = c(.2,5.6,6.2),ncol = 3, nrow = 1, align = "h", 
    labels = c("","a", "b"),label.x = c(0,0.11,0.09), label.y = 1.06,
    common.legend = TRUE, legend = "bottom", hjust = -0.5, vjust = 3),
  ggarrange(r_pentad_periods11,B_pentad_periods,G_pentad_periods,kge_pentad_periods,
            # r_periods,B_periods,G_periods,kge_periods,
            ncol = 4, nrow = 1,labels = c("e", "f","g",'h'),
            label.x = c(0.27,-0.03,-0.05,-0.05),
                        align='h',widths = c(10.5,8,8,8)),
  ncol=1,nrow=2,align='hv',heights=c(3.,3))

print(p1)
dev.off()





png(paste0(dir_plots ,"/SUPP_PLOTS/",
           "Fig_performance_stations_monthly.png"), units = "in",
    width = 12, height = 6, res = 600, pointsize = 11)

r_periods11 <- r_periods1 +
  theme(plot.margin = unit(c(.02, .0, .05, .0), "cm"))

blank_plot <- ggplot() + theme_void()
p1 <- ggarrange(ggarrange(blank_plot,kge_month_v2,kge_month_v3,
                          widths = c(.12,5.62,6.23),ncol = 3, nrow = 1, align = "h", 
                          labels = c("","a", "b"),label.x = c(0,0.1,0.08), label.y = 1.06,
                          common.legend = TRUE, legend = "bottom", hjust = -0.5, vjust = 3),
                ggarrange(r_periods11,B_periods,G_periods,kge_periods,
                          # r_periods,B_periods,G_periods,kge_periods,
                          ncol = 4, nrow = 1,labels = c("e", "f","g",'h'),
                          label.x = c(0.2,-0.03,-0.05,-0.05),
                          align='h',widths = c(10.5,8,8,8)),
                ncol=1,nrow=2,align='v',heights=c(3.4,2.5))

print(p1)
dev.off()



#___________________________________________________________________________
#///////////////////////////////////////////////////////////////////////////////
gradients_plot_function <- function(data,variables,xlab,ylab,title,xmin,xmax){
  #data <- res_month_data
  data_metrics <- data %>% select(all_of(variables))
  data_metrics$diff_gauges <- data_metrics$mean_gauges_v3 - data_metrics$mean_gauges_v2
  data_metrics$diff_perf   <- data_metrics[,7] - data_metrics[,6]
  #data_metrics <- data_metrics %>% filter(diff_gauges!=6.5)
  
  #data_metrics$diff_gauges <- ifelse(data_metrics$diff_gauges==6.5,NA,
  #                                   data_metrics$diff_gauges)
# Create boxplot with count annotations and flip the coordinates
p <- ggplot(data_metrics, aes(x = as.factor(round(diff_gauges,0)), y = diff_perf)) +
  geom_boxplot(outlier.shape = NA,colour=adjustcolor("black", alpha.f = 0.99),
               fill=adjustcolor("grey", alpha.f = 0.99)) +
  theme_classic() +
  #geom_point()+
  labs(x = xlab, y = ylab, title = title) +
  theme_classic() +
  coord_flip() +  # Flip the coordinates to make the boxplot horizontal
  geom_text(
    data = data_metrics %>%
      group_by(diff_gauges = as.factor(round(diff_gauges,0))) %>% 
      summarize(count = n()),
    aes(x = diff_gauges, y = Inf, label = paste0("[", count,"]")),  # Position the count text
    vjust = 0, hjust = 1.2, color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = .5) + # Add vertical line at x = 0
  scale_y_continuous(limits = c(xmin,xmax))+
  theme(plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 14,face='bold',color='black'),
        legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
        legend.text = element_text(size = 13,color='black'),
        #axis.text  =element_text(size=12),
        legend.position = "none",
        legend.title.align = 1,
        axis.title.y = element_text(size = 13,vjust = 0.5,color='black'),
        axis.title.x = element_text(size = 13,color='black'),
        axis.text = element_text(size = 12,color='black'),
        legend.box.spacing = unit(-.001, "pt"), 
        legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
        legend.box.margin = margin(0, 0, 0, 0))

return(p)
}

gradients_plot_function2 <- function(data,variables,xlab,ylab,title,xmin,xmax){
  #data <- res_month_data
  data_metrics <- data %>% select(all_of(variables))
  data_metrics$diff_gauges <- data_metrics$mean_gauges_v3 - data_metrics$mean_gauges_v2
  data_metrics$diff_perf   <- data_metrics[,7] - data_metrics[,6]
  #data_metrics <- data_metrics %>% filter(diff_gauges!=6.5)
  
  #data_metrics$diff_gauges <- ifelse(data_metrics$diff_gauges==6.5,NA,
  #                                   data_metrics$diff_gauges)
  # Create boxplot with count annotations and flip the coordinates
  ggplot(data_metrics, aes(x = as.factor(round(diff_gauges,0)), y = diff_perf)) +
    geom_boxplot(outlier.shape = NA,colour=adjustcolor("black", alpha.f = 0.99),
                 fill=adjustcolor("grey", alpha.f = 0.99)) +
    theme_classic() +
    #geom_point()+
    labs(x = xlab, y = ylab, title = title) +
    theme_classic() +
    coord_flip() +  # Flip the coordinates to make the boxplot horizontal
   # geom_text(
   #   data = data_metrics %>%
   #     group_by(diff_gauges = as.factor(round(diff_gauges,0))) %>% 
   #     summarize(count = n()),
   #   aes(x = diff_gauges, y = Inf, label = paste0("(", count,")")),  # Position the count text
   #   vjust = 0, hjust = 1.5, color = "black"
   # ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = .5) + # Add vertical line at x = 0
    scale_y_continuous(limits = c(xmin,xmax))+
    theme(plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm"),# Reduce margin equally
          plot.title = element_text(size = 14,face='bold',color='black'),
          legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5,color='black'),
          legend.text = element_text(size = 13,color='black'),
          #axis.text  =element_text(size=12),
          legend.position = "none",
          legend.title.align = 1,
          axis.title.y = element_text(size = 13,vjust = 0.5,color='black'),
          axis.title.x = element_text(size = 13,color='black'),
          axis.text = element_text(size = 12,color='black'),
          legend.box.spacing = unit(-.001, "pt"), 
          legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
          legend.box.margin = margin(0, 0, 0, 0))
  
  
}



# load annual precipitation data
pcp_annual      <- fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_annual.csv"),head=TRUE)%>%
  filter(na_count_ideam <= 30)
pcp_annual_mean <- pcp_annual  %>%
  group_by(gauge_code) %>%
  summarise(
    annual_mean    = mean(pcp_ideam_flag, na.rm = TRUE),  
    .groups = "drop") 
pcp_annual_mean 


res_month_data <- merge(res_month_data,pcp_annual_mean,by="gauge_code")
res_pentad_data <- merge(res_pentad_data,pcp_annual_mean,by="gauge_code")
head(res_month_data )









b1 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) == 0, round(mean_gauges_v3, 0)>0)


b2 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) == 1, round(mean_gauges_v3, 0)>1)

b3 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) == 2, round(mean_gauges_v3, 0)>2)

b4 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) == 3, round(mean_gauges_v3, 0)>3)

b5 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) == 4, round(mean_gauges_v3, 0)>4)

b6 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) == 5, round(mean_gauges_v3, 0)>5)

b7 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) == 6, round(mean_gauges_v3, 0)>6)


b8 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) == 7, round(mean_gauges_v3, 0)>7)

b9 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) == 8, round(mean_gauges_v3, 0)>8)



boxplot(b1$kge_chirp_v3-b1$kge_chirp_v2,
        b2$kge_chirp_v3-b2$kge_chirp_v2,
        b3$kge_chirp_v3-b3$kge_chirp_v2,
        b4$kge_chirp_v3-b4$kge_chirp_v2,
        b5$kge_chirp_v3-b5$kge_chirp_v2,
        b6$kge_chirp_v3-b6$kge_chirp_v2,
        b7$kge_chirp_v3-b7$kge_chirp_v2,
        b8$kge_chirp_v3-b8$kge_chirp_v2,
        b9$kge_chirp_v3-b9$kge_chirp_v2,
        ylim=c(-.2,.4))




b3 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) > 5, round(mean_gauges_v3, 0)>5)
boxplot(b3$kge_chirp_v2,b3$kge_chirp_v3)

b4 <- res_pentad_data %>%
  filter(round(mean_gauges_v2,0) > 10, round(mean_gauges_v3, 0)>10)
boxplot(b4$kge_chirp_v2,b4$kge_chirp_v3)



plot_r <- gradients_plot_function2(filter(res_pentad_data,mean_gauges_v2>=1 | 
                                            mean_gauges_v3 >= 1),
                        c("mean_gauges_v2","mean_gauges_v3","nat_region","elevation",
                          "annual_mean",
                          "r_chirps_v2","r_chirps_v3"),
                        "Change in number of stations (CHIRPSv3 minus CHIRPSv2)",
                        "",
                        "Correlation coefficient (r) ",-0.28,0.28)



plot_G <- gradients_plot_function2(filter(res_pentad_data,mean_gauges_v2>=1 | 
                                            mean_gauges_v3 >= 1),
                                  c("mean_gauges_v2","mean_gauges_v3","nat_region",
                                    "elevation","annual_mean",
                                    "G_chirps_v2","G_chirps_v3"),
                                  "",
                                  "",
                                  "Variability ratio (G) ",-0.28,0.28)


plot_B <- gradients_plot_function2(filter(res_pentad_data,mean_gauges_v2>=1 | 
                                            mean_gauges_v3 >= 1),
                                  c("mean_gauges_v2","mean_gauges_v3","nat_region",
                                    "elevation","annual_mean",
                                    "B_chirps_v2","B_chirps_v3"),
                                  "",
                                  "",
                                  "Bias ratio (B) ",-0.28,0.28)


plot_kge <- gradients_plot_function(filter(res_pentad_data,mean_gauges_v2>=1 | 
                                             mean_gauges_v3 >= 1),
                                  c("mean_gauges_v2","mean_gauges_v3","nat_region",
                                    "elevation","annual_mean",
                                    "kge_chirps_v2","kge_chirps_v3"),
                                  "",
                                  "",
                                  "Kling-Gupta Efficiency (KGE) ",-0.28,0.28)



common_x_label <- textGrob("Performance change (CHIRPSv3 minus CHIRPSv2)", rot = 0, 
                            gp = gpar(fontsize = 14))


png(paste0(dir_plots ,"/SUPP_PLOTS/", "Fig_performance_diff_CHIRPS_gauges_mean_supp.png"), units = "in",
    width = 15, height = 8, res = 600, pointsize = 11)

p2 <- ggarrange(plot_r,plot_B,plot_G,plot_kge,
          ncol = 4, nrow = 1, align = "hv", 
          labels = c("a", "b", "c","d"),
          label.x = 0.1, label.y = 1,
          common.legend = TRUE, legend = "bottom", hjust = -0.5, vjust = 3) +
          annotation_custom(common_x_label, ymin = -.88, ymax = Inf, xmin = .0, xmax = Inf) + 
          theme(
            plot.margin = margin(.1, .1, 0, .1),   # Adjusting the plot margins
            legend.margin = margin(t = -20, b = 0),  # Reduce the margin above the legend (adjust as needed)
            legend.box.spacing = unit(-0.2, "cm"))    # Reduce the spacing between the legend box and plot
print(p2)
dev.off()









#///////////////////////////////////////////////////////////////////////////////

library(dplyr)
library(ggplot2)




p <- ggarrange(r_periods,B_periods,G_periods,kge_periods,
               ncol = 2, nrow = 2,labels = c("a", "b","c",'d'),
               label.x = 0.15,align='hv')
print(p)
dev.off()


