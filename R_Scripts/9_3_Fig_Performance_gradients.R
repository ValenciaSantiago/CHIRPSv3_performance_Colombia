
#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,
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
#                                              "/res_performance_annual_2004_2015_df.csv")),head=TRUE)

region_x <- 'Andes'
dim(filter(res_pentad_data,nat_region==region_x,kge_chirps_v3 >= kge_chirps_v2))/
  dim(filter(res_pentad_data,nat_region==region_x))

dim(filter(res_month_data ,nat_region==region_x,kge_chirps_v3 >= kge_chirps_v2))/
  dim(filter(res_month_data ,nat_region==region_x))






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

dim(filter())


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
  geom_boxplot(outlier.shape = NA,colour=adjustcolor("black", alpha.f = 0.8),
               fill=adjustcolor("white", alpha.f = 0.3)) +
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
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = .5) + # Add vertical line at x = 0
  scale_y_continuous(limits = c(xmin,xmax))+
  theme(plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 14,face='bold'),
        legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5),
        legend.text = element_text(size = 13),
        #axis.text  =element_text(size=12),
        legend.position = "none",
        legend.title.align = 1,
        axis.title.y = element_text(size = 13,vjust = 0.5),
        axis.title.x = element_text(size = 13),
        axis.text = element_text(size = 12),
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
    geom_boxplot(outlier.shape = NA,colour=adjustcolor("black", alpha.f = 0.8),
                 fill=adjustcolor("white", alpha.f = 0.3)) +
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
          plot.title = element_text(size = 14,face='bold'),
          legend.title = element_text(size = 15, vjust = 0.0,hjust=0.5),
          legend.text = element_text(size = 13),
          #axis.text  =element_text(size=12),
          legend.position = "none",
          legend.title.align = 1,
          axis.title.y = element_text(size = 13,vjust = 0.5),
          axis.title.x = element_text(size = 13),
          axis.text = element_text(size = 12),
          legend.box.spacing = unit(-.001, "pt"), 
          legend.margin = margin(.001, 0.03, 0., 0.0025),  # unit(c(top, right, bottom, left)
          legend.box.margin = margin(0, 0, 0, 0))
  
  
}


plot_r <- gradients_plot_function2(res_month_data,
                        c("mean_gauges_v2","mean_gauges_v3","nat_region","elevation","annual_mean",
                          "r_chirps_v2","r_chirps_v3"),
                        "Groung gauges differences (CHIRPSv3 minus CHIRPSv2)",
                        "",
                        "Linear correlation (r) ",-0.28,0.28)

plot_G <- gradients_plot_function2(res_month_data,
                                  c("mean_gauges_v2","mean_gauges_v3","nat_region","elevation","annual_mean",
                                    "G_chirps_v2","G_chirps_v3"),
                                  "",
                                  "",
                                  "Variability ratio (G) ",-0.28,0.28)


plot_B <- gradients_plot_function2(res_month_data,
                                  c("mean_gauges_v2","mean_gauges_v3","nat_region","elevation","annual_mean",
                                    "B_chirps_v2","B_chirps_v3"),
                                  "",
                                  "",
                                  "Bias ratio (B) ",-0.28,0.28)


plot_kge <- gradients_plot_function(res_month_data,
                                  c("mean_gauges_v2","mean_gauges_v3","nat_region","elevation","annual_mean",
                                    "kge_chirps_v2","kge_chirps_v3"),
                                  "",
                                  "",
                                  "Kling Gupta Efficiency (KGE) ",-0.28,0.28)



res_month_data$mean_gauges_v3_scale <-  round(res_month_data$mean_gauges_v3,0)
kge_v3 <- ggplot(res_month_data, aes(x = as.factor(mean_gauges_v3_scale), y = kge_chirps_v3)) +
  geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
               fill = adjustcolor("#3d8d52", alpha.f = 0.5)) +
  # Add count annotations above each boxplot
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey", size = 0.5) +
  geom_text(
    data = res_month_data %>%
      group_by(as.factor(mean_gauges_v3_scale)) %>%
      summarize(count = n()),  # Summarize the count of each max_gauges_v3
    aes(x = seq(1,17,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
    vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
  labs(x = "Mean number of groung gauges by 0.25° pixel",
       y = "Kling Gupta \n Efficiency (KGE)", title = "CHIRPSv3") +
  theme_classic()+
  scale_y_continuous(limits=c(0,1))+
  geom_hline(yintercept = median(res_month_data$kge_chirps_v3), linetype = "dotted", color = "#d95f02", size = .7)+
  theme(plot.margin = unit(c(.01, .01, .01, .01), "cm"),# Reduce margin equally
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


res_month_data$mean_gauges_v2_scale <-  round(res_month_data$max_gauges_v2,0)
#res_month_data$mean_gauges_v2_scale <-  ceiling(res_month_data$mean_gauges_v2)
kge_v2 <- ggplot(res_month_data, aes(x = as.factor(mean_gauges_v2_scale), y = kge_chirps_v2)) +
  geom_boxplot(outlier.shape = NA, colour = adjustcolor("black", alpha.f = 0.8),
               fill = adjustcolor("#7a3d8d", alpha.f = 0.6)) +
  # Add count annotations above each boxplot
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey",
             size = 0.5) +
  geom_text(
    data = res_month_data %>%
      group_by(as.factor(mean_gauges_v2_scale)) %>%
      summarize(count = n()),  # Summarize the count of each max_gauges_v3
    aes(x = seq(1,15,1), y = .005, label = paste0("[", count, "]")),  # Position the count text at the top of the boxplot
    vjust = 0, hjust = 0.5, color = "black",size=2.5)+  # Adjust text position and color
  labs(x = "Mean number of groung gauges by 0.25° pixel",
       y = "Kling Gupta \n Efficiency (KGE)", title = "CHIRPSv2") +
  theme_classic() +
  scale_y_continuous(limits=c(0,1))+
  geom_hline(yintercept = median(res_month_data$kge_chirps_v2), linetype = "dotted", color = "#d95f02", size = .7)+
  theme(plot.margin = unit(c(.01, .01, .01, .01), "cm"),# Reduce margin equally
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
kge_v2


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




png(paste0(dir_plots ,"/", "Fig_performance_diff_CHIRPS_gauges_mean.png"), units = "in",
    width = 9, height = 5.5, res = 600, pointsize = 11)

p1 <- ggarrange(kge_v2,kge_v3,
                ncol = 1, nrow = 2, align = "hv", 
                labels = c("a", "b"),
                label.x = 0.0, label.y = 1.1,
                common.legend = TRUE, legend = "bottom", hjust = -0.5, vjust = 3)

print(p1)
dev.off()




