

library('pacman')
p_load(terra, tidyverse, rnaturalearth, glue, lubridate, zoo,rlang,ggpubr,
       data.table)

dir_datasets <- 'C:/Users/santiagovalencia/OneDrive - University of Arizona/Documents/GitHub/CHIRPSv3_performance_Colombia/Datasets'
ext_ind_df <- as.data.frame(fread(paste0(dir_datasets,
                  "/daily_extreme_precipitation_indices_2001_2023.csv")),head=TRUE)
res_ext_ind_df <- as.data.frame(fread(paste0(dir_datasets,
                  "/res_performance_extreme_precipitation_indices_2001_2023_v2.csv")),head=TRUE)


colnames(ext_ind_df)
#dataset      <- ext_ind_df
#var_v2       <- 'CWD_chirpsv2'
#var_v3_era   <- 'CWD_chirpsv3_era5'
#var_v3_imerg <- 'CWD_chirpsv3_imerg'
#var_ideam    <- 'CWD_ideam'
#var          <- 'R20mm'

ext_ind_plot <- function(dataset,label,var_v2,var_v3_era,var_v3_imerg,var_ideam,
                         r_low, h_low,int){
                          
dataset_subset <- dataset[,c(var_v2,var_v3_era,var_v3_imerg,var_ideam)]
colnames(dataset_subset) <- c('CHIRPSv2','CHIRPSv3-ERA5',"CHIRPSv3-IMERG",'IDEAM')

#head(dataset_subset)

data_long <- dataset_subset %>%
  pivot_longer(cols = everything(),
               names_to = "Dataset",
               values_to = 'metric')

max(data_long$metric)
# Create the boxplot
#x_labels <- c("CHIRPSv2","CHIRPSv3-ERA5","CHIRPSv3-IMERG","IDEAM")
data_long$Dataset <- factor(data_long$Dataset,
                            levels = c("CHIRPSv3-IMERG","CHIRPSv3-ERA5",
                                       "CHIRPSv2","IDEAM"))
x_labels <- c("CHIRPSv3-IMERG","CHIRPSv3-ERA5","CHIRPSv2","IDEAM")


p <- ggplot() +
  geom_boxplot(data=data_long, aes(x = Dataset, y = metric,
                                   fill = Dataset),outlier.shape = NA) +
  theme_classic() +
  labs(title ='',
       x = "",
       y = label) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(
    limits = c(r_low, h_low),
    breaks = seq(r_low, h_low, by = int),
    labels = seq(r_low, h_low, by = int)
  ) +
  scale_x_discrete(labels = x_labels)+
  #geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = 0.5) +
  theme(
    plot.margin = unit(c(.0, .0, .0, .0), "cm"),
    plot.title = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"),
    legend.text = element_text(size = 11, color = "black"),
    legend.position = "none",
    legend.title.align = 1,
    axis.title.y = element_text(size = 12, vjust = 0.5, color = "black"),
    axis.title.x = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.box.spacing = unit(-.001, "pt"),
    legend.margin = margin(.001, 0.03, 0.01, 0.0025),
    legend.box.margin = margin(0, 0, 0, 0)) +
  scale_fill_manual(values = c(
    "CHIRPSv2" = "#7a3d8d", 
    "CHIRPSv3-IMERG" = "#3d8d52",
    "CHIRPSv3-ERA5" = "#9ec6bc",
    'IDEAM' = adjustcolor("black", alpha.f = 0.1)))+ coord_flip()
print(p)
return(p)
}


r10_plot <- ext_ind_plot(ext_ind_df,'R10mm (days)','R10mm_chirpsv2','R10mm_chirpsv3_era5',
                         'R10mm_chirpsv3_imerg','R10mm_ideam',0,200,25)

r20_plot <- ext_ind_plot(ext_ind_df,'R20mm (days)','R20mm_chirpsv2','R20mm_chirpsv3_era5',
                         'R20mm_chirpsv3_imerg','R20mm_ideam',0,100,20)

Rx1_plot <- ext_ind_plot(ext_ind_df,'Rx1day (mm)','Rx1_chirpsv2','Rx1_chirpsv3_era5',
                         'Rx1_chirpsv3_imerg','Rx1_ideam',0,200,25)

Rx5_plot <- ext_ind_plot(ext_ind_df,'Rx5day (mm)','Rx5_chirpsv2','Rx5_chirpsv3_era5',
                         'Rx5_chirpsv3_imerg','Rx5_ideam',0,350,50)

CDD_plot <- ext_ind_plot(ext_ind_df,'CDD (days)','CDD_chirpsv2','CDD_chirpsv3_era5',
                         'CDD_chirpsv3_imerg','CDD_ideam',0,70,10)

CWD_plot <- ext_ind_plot(ext_ind_df,'CWD (days)','CWD_chirpsv2','CWD_chirpsv3_era5',
                         'CWD_chirpsv3_imerg','CWD_ideam',0,130,25)

r95_plot <- ext_ind_plot(ext_ind_df,'R95p (mm)','r95_chirpsv2','r95_chirpsv3_era5',
                         'r95_chirpsv3_imerg','r95_ideam',0,2500,500)

r99_plot <- ext_ind_plot(ext_ind_df,'R99p (mm)','r99_chirpsv2','r99_chirpsv3_era5',
                         'r99_chirpsv3_imerg','r99_ideam',0,800,100)

rtotal_plot <- ext_ind_plot(ext_ind_df,'PRCPTOT (mm)','rtotal_chirpsv2','rtotal_chirpsv3_era5',
                         'rtotal_chirpsv3_imerg','rtotal_ideam',0,5000,1000)

sdii_plot <- ext_ind_plot(ext_ind_df,'SDII (mm/day)','SDII_chirpsv2','SDII_chirpsv3_era5',
                            'SDII_chirpsv3_imerg','SDII_ideam',0,41,5)


png("G:/My Drive/R4C_et_al/3_PLOTS/Fig_extreme_indices_daily.png",
    units = "in",width = 15, height =5.5, 
    res = 600, pointsize = 11)#, bg = "transparent")
#pdf("G:/My Drive/R4C_et_al/3_PLOTS/Fig_5_performance_extreme_indices.pdf",
#    width = 12, height = 10, pointsize = 11)

ggarrange(r10_plot,
          r20_plot + theme(axis.text.y = element_blank()),
          Rx1_plot + theme(axis.text.y = element_blank()),
          Rx5_plot+ theme(axis.text.y = element_blank()),
          CDD_plot + theme(axis.text.y = element_blank()),
          CWD_plot,#+ theme(axis.text.y = element_blank()),
          r95_plot + theme(axis.text.y = element_blank()),
          r99_plot + theme(axis.text.y = element_blank()),
          rtotal_plot + theme(axis.text.y = element_blank()),
          sdii_plot + theme(axis.text.y = element_blank()),
          ncol=5,nrow=2,widths = c(3,2,2,2,2),
          labels=c('a','b','c','d','e','f','g','h','i','j'))

dev.off()


#________________________________________________________________________
# Regional plots

region <- unique(ext_ind_df$nat_region)
for(j in 1:5){
r10_plot_j <- ext_ind_plot(filter(ext_ind_df,nat_region==region[j]),
                          'R10mm (days)','R10mm_chirpsv2','R10mm_chirpsv3_era5',
                         'R10mm_chirpsv3_imerg','R10mm_ideam',0,200,25)

r20_plot_j <- ext_ind_plot(filter(ext_ind_df,nat_region==region[j]),
                           'R20mm (days)','R20mm_chirpsv2','R20mm_chirpsv3_era5',
                         'R20mm_chirpsv3_imerg','R20mm_ideam',0,100,20)

Rx1_plot_j <- ext_ind_plot(filter(ext_ind_df,nat_region==region[j]),
                           'Rx1day (mm)','Rx1_chirpsv2','Rx1_chirpsv3_era5',
                         'Rx1_chirpsv3_imerg','Rx1_ideam',0,200,25)

Rx5_plot_j <- ext_ind_plot(filter(ext_ind_df,nat_region==region[j]),
                           'Rx5day (mm)','Rx5_chirpsv2','Rx5_chirpsv3_era5',
                         'Rx5_chirpsv3_imerg','Rx5_ideam',0,350,50)

CDD_plot_j <- ext_ind_plot(filter(ext_ind_df,nat_region==region[j]),
                           'CDD (days)','CDD_chirpsv2','CDD_chirpsv3_era5',
                         'CDD_chirpsv3_imerg','CDD_ideam',0,70,10)

CWD_plot_j <- ext_ind_plot(filter(ext_ind_df,nat_region==region[j]),
                           'CWD (days)','CWD_chirpsv2','CWD_chirpsv3_era5',
                         'CWD_chirpsv3_imerg','CWD_ideam',0,130,25)

r95_plot_j <- ext_ind_plot(filter(ext_ind_df,nat_region==region[j]),
                           'R95p (mm)','r95_chirpsv2','r95_chirpsv3_era5',
                         'r95_chirpsv3_imerg','r95_ideam',0,2500,500)

r99_plot_j <- ext_ind_plot(filter(ext_ind_df,nat_region==region[j]),
                           'R99p (mm)','r99_chirpsv2','r99_chirpsv3_era5',
                         'r99_chirpsv3_imerg','r99_ideam',0,800,100)

rtotal_plot_j <- ext_ind_plot(filter(ext_ind_df,nat_region==region[j]),
                              'PRCPTOT (mm)','rtotal_chirpsv2','rtotal_chirpsv3_era5',
                            'rtotal_chirpsv3_imerg','rtotal_ideam',0,5000,1000)

sdii_plot_j <- ext_ind_plot(filter(ext_ind_df,nat_region==region[j]),
                            'SDII (mm/day)','SDII_chirpsv2','SDII_chirpsv3_era5',
                          'SDII_chirpsv3_imerg','SDII_ideam',0,41,5)



png(paste0("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS/Fig_extreme_indices_daily_",
           region[j],".png"),
    units = "in",width = 12, height =10, 
    res = 600, pointsize = 11)#, bg = "transparent")

plot <- ggarrange(r10_plot_j,
          r20_plot_j + theme(axis.text.y = element_blank()),
          Rx1_plot_j + theme(axis.text.y = element_blank()),
          Rx5_plot_j,
          CDD_plot_j + theme(axis.text.y = element_blank()),
          CWD_plot_j + theme(axis.text.y = element_blank()),
          r95_plot_j,# + theme(axis.text.y = element_blank()),
          r99_plot_j + theme(axis.text.y = element_blank()),
          rtotal_plot_j + theme(axis.text.y = element_blank()),
          sdii_plot_j,
          ncol=3,nrow=4,widths = c(4.5,3,3),
          labels=c('a','b','c','d','e','f','g','h','i','j'))
print(plot)
dev.off()

}




#/////////////////////////////////////////////////////////////////////////
#_________________________________________________________________________
# Performance metrics -- KGE, r, B, G

regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas")
  performance_plot_ext_indices <- function(data,names,title_plot,x_label,r_low,h_low,
                                            int,opt_value,regions){
    
    reshape_performance_data <- function(data, names) {
      
      #data <- res_ext_ind_df
      #names <- c('v2_SDII_KGE','v3_era_SDII_KGE','v3_imerg_SDII_KGE')

      data <- data %>% filter(nat_region %in% regions)
      performance_long <- data %>%
        select(all_of(names)) %>%
        pivot_longer(cols = everything(), names_to = "dataset", values_to = "kge_value") %>%
        mutate(version = case_when(
         # grepl(names[1], dataset) ~ "v2",  
          grepl(names[1], dataset) ~ "v2",  
          grepl(names[2], dataset) ~ "v3-ERA5",   
          grepl(names[3], dataset) ~ "v3-IMERG",
          TRUE ~ NA_character_  # Add a default case to avoid potential issues
        )) %>%
        mutate(product = case_when(
         # grepl(names[1], dataset) ~ "CHIRP",  
          grepl(names[1], dataset) ~ "CHIRPS",  
          grepl(names[2], dataset) ~ "CHIRPS",   
          grepl(names[3], dataset) ~ "CHIRPS",
          TRUE ~ NA_character_  # Add a default case to avoid potential issues
        ))
    
      return(performance_long)
    }
    
    violin_df         <- reshape_performance_data(data, names )
    violin_df$chirpx  <- violin_df$product
    violin_df         <- violin_df %>%mutate(product2 = paste0(product,version,sep = ""))
    #head(violin_df)
    
    # Plot the violin plot with boxplot and median annotation
    # Calculate the median values for each group (chirpx)
    median_values <- violin_df %>%
      group_by(chirpx, product2) %>%
      summarise(median_kge = median(kge_value, na.rm = TRUE),
                .groups = "drop")
    
    #violin_df$chirpx <- 'CHIRPS'
    # Plot with boxplot and median values
    plot_violin <- ggplot(violin_df, aes(y = kge_value, x = chirpx, fill = version)) + 
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
    theme(legend.position = "none") + 
      geom_boxplot(width = .25, outlier.shape = NA, col = "black",
                   position = position_dodge(.5)) + 
      scale_fill_manual(name = "Version",  # Set the legend title to "version"
                        values = c("v2" = adjustcolor("#7a3d8d", alpha.f = 0.95),  
                                   # "v3" = adjustcolor("#3d8d7a", alpha.f = 0.5),  
                                   "v3-IMERG" = adjustcolor("#3d8d52", alpha.f = .95),
                                   "v3-ERA5" = adjustcolor("#9ec6bc", alpha.f = .95))) +
      stat_summary(
        fun = median, 
        geom = "text", 
        aes(label = round(..y.., 2)), 
        position = position_dodge(.8),  # Keep it fixed relative to the data points
        colour = c("black"), 
        size = 3., 
        vjust = c(-2.775,-1,.40)  # Adjust vertical positioning of the label
      ) + labs(title = title_plot)+
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
        plot.title = element_text(size = 13, color = 'black',face='bold',hjust = 0.5),
        #axis.title.y = element_text(size = 12, vjust = 0.5, color = 'black'),
        axis.title.x = element_text(size = 12, color = 'black'),
        axis.text = element_text(size = 12, color = 'black'),
        legend.title = element_text(size = 16, vjust = 0.0, hjust = 0.5, color = 'black'),
        legend.text = element_text(size = 14, color = 'black'),
        legend.position = "none",
        legend.title.align = 1,
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),    
        axis.ticks.y = element_blank(),
        legend.box.spacing = unit(-.001, "pt"), 
        legend.margin = margin(.001, 0.03, 0., 0.0025),
        legend.box.margin = margin(0, 0, 0, 0)
      )
    

    
    return(plot_violin)
  }
  
  
kge_r95 <-   performance_plot_ext_indices(res_ext_ind_df,
               c('v2_r95_KGE','v3_era_r95_KGE','v3_imerg_r95_KGE'),
               "R95p",'Kling-Gupta Efficiency (KGE)',-1,1,.5,1,
               c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"))

kge_r99 <-   performance_plot_ext_indices(res_ext_ind_df,
               c('v2_r99_KGE','v3_era_r99_KGE','v3_imerg_r99_KGE'),
               'R99p','Kling-Gupta Efficiency (KGE)',-1,1,.5,1,
               c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"))

kge_rtotal <-   performance_plot_ext_indices(res_ext_ind_df,
               c('v2_rtotal_KGE','v3_era_rtotal_KGE','v3_imerg_rtotal_KGE'),
               'PRCPTOT','Kling-Gupta Efficiency (KGE)',-1,1,.5,1,
               c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"))

kge_rtotal <- kge_rtotal + 
  theme(
    legend.position = c(0.25, 0.3),  
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10,color='black'))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
kge_rtotal


kge_CDD <-   performance_plot_ext_indices(res_ext_ind_df,
               c('v2_CDD_KGE','v3_era_CDD_KGE','v3_imerg_CDD_KGE'),
               'CDD','Kling-Gupta Efficiency (KGE)',-1,1,.5,1,
               c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"))

kge_CWD <-   performance_plot_ext_indices(res_ext_ind_df,
               c('v2_CWD_KGE','v3_era_CWD_KGE','v3_imerg_CWD_KGE'),
               'CWD','Kling-Gupta Efficiency (KGE)',-1,1,.5,1,
               c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"))


kge_R10mm <-   performance_plot_ext_indices(res_ext_ind_df,
               c('v2_R10mm_KGE','v3_era_R10mm_KGE','v3_imerg_R10mm_KGE'),
               'R10mm','Kling-Gupta Efficiency (KGE)',-1,1,.5,1,
               c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"))
  
kge_R20mm <-   performance_plot_ext_indices(res_ext_ind_df,
               c('v2_R20mm_KGE','v3_era_R20mm_KGE','v3_imerg_R20mm_KGE'),
               'R20mm','Kling-Gupta Efficiency (KGE)',-1,1,.5,1,
               c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"))
  
kge_Rx1 <-   performance_plot_ext_indices(res_ext_ind_df,
               c('v2_Rx1_KGE','v3_era_Rx1_KGE','v3_imerg_Rx1_KGE'),
               'Rx1day','Kling-Gupta Efficiency (KGE)',-1,1,.5,1,
               c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"))
  
kge_Rx5 <-   performance_plot_ext_indices(res_ext_ind_df,
               c('v2_Rx5_KGE','v3_era_Rx5_KGE','v3_imerg_Rx5_KGE'),
               'Rx5day','Kling-Gupta Efficiency (KGE)',-1,1,.5,1,
               c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"))

kge_SDII <-   performance_plot_ext_indices(res_ext_ind_df,
               c('v2_SDII_KGE','v3_era_SDII_KGE','v3_imerg_SDII_KGE'),
               'SDII','Kling-Gupta Efficiency (KGE)',-1,1,.5,1,
               c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"))
    

#png("G:/My Drive/R4C_et_al/3_PLOTS/Fig_perf_extreme_indices_KGE2.png",
#    units = "in",width = 11, height =6, 
#    res = 600, pointsize = 11)#, bg = "transparent")
pdf("G:/My Drive/R4C_et_al/3_PLOTS/Fig_5_performance_extreme_indices_KGE.pdf",
    width = 12, height = 6.5, pointsize = 11)

ggarrange(kge_rtotal,kge_r95,kge_r99,kge_CDD,kge_CWD,
          kge_R10mm,kge_R20mm,kge_Rx1,kge_Rx5,kge_SDII,ncol=5,nrow=2,
          labels = c("a","b","c","d","e","f","g","h","i","j"))


dev.off()
  

 


#___________________________________________________________________
# Correlation coefficient (r)

regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas")
region_name <- 'COL'

r_r95 <-   performance_plot_ext_indices(res_ext_ind_df,
                                          c('v2_r95_r','v3_era_r95_r','v3_imerg_r95_r'),
                                          "R95p",'Correlation coefficient (r)',-1,1,.5,1,
                                          regions)

r_r99 <-   performance_plot_ext_indices(res_ext_ind_df,
                                          c('v2_r99_r','v3_era_r99_r','v3_imerg_r99_r'),
                                          'R99p','Correlation coefficient (r)',-1,1,.5,1,
                                          regions)

r_rtotal <-   performance_plot_ext_indices(res_ext_ind_df,
                                             c('v2_rtotal_r','v3_era_rtotal_r','v3_imerg_rtotal_r'),
                                             'PRCPTOT','Correlation coefficient (r)',-1,1,.5,1,
                                             regions)

r_rtotal <- r_rtotal + 
  theme(
    legend.position = c(0.25, 0.3),  
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10,color='black'))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
r_rtotal


r_CDD <-   performance_plot_ext_indices(res_ext_ind_df,
                                          c('v2_CDD_r','v3_era_CDD_r','v3_imerg_CDD_r'),
                                          'CDD','Correlation coefficient (r)',-1,1,.5,1,
                                          regions)

r_CWD <-   performance_plot_ext_indices(res_ext_ind_df,
                                          c('v2_CWD_r','v3_era_CWD_r','v3_imerg_CWD_r'),
                                          'CWD','Correlation coefficient (r)',-1,1,.5,1,
                                          regions)


r_R10mm <-   performance_plot_ext_indices(res_ext_ind_df,
                                            c('v2_R10mm_r','v3_era_R10mm_r','v3_imerg_R10mm_r'),
                                            'R10mm','Correlation coefficient (r)',-1,1,.5,1,
                                            regions)

r_R20mm <-   performance_plot_ext_indices(res_ext_ind_df,
                                            c('v2_R20mm_r','v3_era_R20mm_r','v3_imerg_R20mm_r'),
                                            'R20mm','Correlation coefficient (r)',-1,1,.5,1,
                                          regions)
                                            

r_Rx1 <-   performance_plot_ext_indices(res_ext_ind_df,
                                          c('v2_Rx1_r','v3_era_Rx1_r','v3_imerg_Rx1_r'),
                                          'Rx1day','Correlation coefficient (r)',-1,1,.5,1,
                                          regions)

r_Rx5 <-   performance_plot_ext_indices(res_ext_ind_df,
                                          c('v2_Rx5_r','v3_era_Rx5_r','v3_imerg_Rx5_r'),
                                          'Rx5day','Correlation coefficient (r)',-1,1,.5,1,
                                          regions)

r_SDII <-   performance_plot_ext_indices(res_ext_ind_df,
                                           c('v2_SDII_r','v3_era_SDII_r','v3_imerg_SDII_r'),
                                           'SDII','Correlation coefficient (r)',-1,1,.5,1,
                                           regions)


png(paste0("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS/",
           "Fig_perf_extreme_indices_r_",region_name,".png"),
    units = "in",width = 12.5, height =6, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(r_rtotal,r_r95,r_r99,r_CDD,r_CWD,
          r_R10mm,r_R20mm,r_Rx1,r_Rx5,r_SDII,ncol=5,nrow=2,
          labels = c("a","b","c","d","e","f","g","h","i","j"))


dev.off()




#_____________________________________________________________________________
# Bias ratio (B)

regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas")
region_name <- 'COL'

B_r95 <-   performance_plot_ext_indices(res_ext_ind_df,
                      c('v2_r95_Beta','v3_era_r95_Beta','v3_imerg_r95_Beta'),
                      "R95p",paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                      regions)

B_r99 <-   performance_plot_ext_indices(res_ext_ind_df,
                      c('v2_r99_Beta','v3_era_r99_Beta','v3_imerg_r99_Beta'),
                      'R99p',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                      regions)

B_rtotal <-   performance_plot_ext_indices(res_ext_ind_df,
                      c('v2_rtotal_Beta','v3_era_rtotal_Beta','v3_imerg_rtotal_Beta'),
                      'PRCPTOT',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                      regions)

B_rtotal <- B_rtotal + 
  theme(
    legend.position = c(0.25, 0.3),  
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10,color='black'))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
B_rtotal


B_CDD <-   performance_plot_ext_indices(res_ext_ind_df,
                     c('v2_CDD_Beta','v3_era_CDD_Beta','v3_imerg_CDD_Beta'),
                       'CDD',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                      regions)

B_CWD <-   performance_plot_ext_indices(res_ext_ind_df,
                   c('v2_CWD_Beta','v3_era_CWD_Beta','v3_imerg_CWD_Beta'),
                   'CWD',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                   regions)


B_R10mm <-   performance_plot_ext_indices(res_ext_ind_df,
                   c('v2_R10mm_B','v3_era_R10mm_B','v3_imerg_R10mm_B'),
                     'R10mm',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                     regions)

B_R20mm <-   performance_plot_ext_indices(res_ext_ind_df,
                    c('v2_R20mm_Beta','v3_era_R20mm_Beta','v3_imerg_R20mm_Beta'),
                   'R20mm (days)',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                    regions)


B_Rx1 <-   performance_plot_ext_indices(res_ext_ind_df,
                 c('v2_Rx1_Beta','v3_era_Rx1_Beta','v3_imerg_Rx1_Beta'),
                 'Rx1day',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                  regions)

B_Rx5 <-   performance_plot_ext_indices(res_ext_ind_df,
                  c('v2_Rx5_Beta','v3_era_Rx5_Beta','v3_imerg_Rx5_Beta'),
                 'Rx5day',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                  regions)

B_SDII <-   performance_plot_ext_indices(res_ext_ind_df,
                 c('v2_SDII_Beta','v3_era_SDII_Beta','v3_imerg_SDII_Beta'),
                 'SDII',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                  regions)


png(paste0("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS/",
           "Fig_perf_extreme_indices_B_",region_name,".png"),
    units = "in",width = 12.5, height =6, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(B_rtotal,B_r95,B_r99,B_CDD,B_CWD,
          B_R10mm,B_R20mm,B_Rx1,B_Rx5,B_SDII,ncol=5,nrow=2,
          labels = c("a","b","c","d","e","f","g","h","i","j"))


dev.off()




#_____________________________________________________________________________
# Bias ratio (B)

regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas")
region_name <- 'COL'

B_r95 <-   performance_plot_ext_indices(res_ext_ind_df,
                      c('v2_r95_Beta','v3_era_r95_Beta','v3_imerg_r95_Beta'),
                      "R95p",paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                      regions)

B_r99 <-   performance_plot_ext_indices(res_ext_ind_df,
                      c('v2_r99_Beta','v3_era_r99_Beta','v3_imerg_r99_Beta'),
                      'R99p',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                      regions)

B_rtotal <-   performance_plot_ext_indices(res_ext_ind_df,
                      c('v2_rtotal_Beta','v3_era_rtotal_Beta','v3_imerg_rtotal_Beta'),
                      'PRCPTOT',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                      regions)

B_rtotal <- B_rtotal + 
  theme(
    legend.position = c(0.25, 0.3),  
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10,color='black'))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
B_rtotal


B_CDD <-   performance_plot_ext_indices(res_ext_ind_df,
                     c('v2_CDD_Beta','v3_era_CDD_Beta','v3_imerg_CDD_Beta'),
                       'CDD',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                      regions)

B_CWD <-   performance_plot_ext_indices(res_ext_ind_df,
                   c('v2_CWD_Beta','v3_era_CWD_Beta','v3_imerg_CWD_Beta'),
                   'CWD',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                   regions)


B_R10mm <-   performance_plot_ext_indices(res_ext_ind_df,
                   c('v2_R10mm_B','v3_era_R10mm_B','v3_imerg_R10mm_B'),
                     'R10mm',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                     regions)

B_R20mm <-   performance_plot_ext_indices(res_ext_ind_df,
                    c('v2_R20mm_Beta','v3_era_R20mm_Beta','v3_imerg_R20mm_Beta'),
                   'R20mm',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                    regions)


B_Rx1 <-   performance_plot_ext_indices(res_ext_ind_df,
                 c('v2_Rx1_Beta','v3_era_Rx1_Beta','v3_imerg_Rx1_Beta'),
                 'Rx1day',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                  regions)

B_Rx5 <-   performance_plot_ext_indices(res_ext_ind_df,
                  c('v2_Rx5_Beta','v3_era_Rx5_Beta','v3_imerg_Rx5_Beta'),
                 'Rx5day',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                  regions)

B_SDII <-   performance_plot_ext_indices(res_ext_ind_df,
                 c('v2_SDII_Beta','v3_era_SDII_Beta','v3_imerg_SDII_Beta'),
                 'SDII',paste0("Bias ratio (","\u03B2",")"),0,2,.5,1,
                  regions)


png(paste0("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS/",
           "Fig_perf_extreme_indices_B_",region_name,".png"),
    units = "in",width = 12.5, height =6, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(B_rtotal,B_r95,B_r99,B_CDD,B_CWD,
          B_R10mm,B_R20mm,B_Rx1,B_Rx5,B_SDII,ncol=5,nrow=2,
          labels = c("a","b","c","d","e","f","g","h","i","j"))


dev.off()





#_____________________________________________________________________________
# Variability ratio (G)

regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas")
region_name <- 'COL'

G_r95 <-   performance_plot_ext_indices(res_ext_ind_df,
                      c('v2_r95_Gamma','v3_era_r95_Gamma','v3_imerg_r95_Gamma'),
                      "R95p",paste0("Variability ratio (","\u03B3",")"),0,2,.5,1,
                      regions)

G_r99 <-   performance_plot_ext_indices(res_ext_ind_df,
                      c('v2_r99_Gamma','v3_era_r99_Gamma','v3_imerg_r99_Gamma'),
                      'R99p',paste0("Variability ratio (","\u03B3",")"),0,2,.5,1,
                      regions)

G_rtotal <-   performance_plot_ext_indices(res_ext_ind_df,
                      c('v2_rtotal_Gamma','v3_era_rtotal_Gamma','v3_imerg_rtotal_Gamma'),
                      'PRCPTOT',paste0("Variability ratio (","\u03B3",")"),0,2,.5,1,
                      regions)

G_rtotal <- G_rtotal + 
  theme(
    legend.position = c(0.8, 0.3),  
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10,color='black'))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
G_rtotal


G_CDD <-   performance_plot_ext_indices(res_ext_ind_df,
                     c('v2_CDD_Gamma','v3_era_CDD_Gamma','v3_imerg_CDD_Gamma'),
                       'CDD',paste0("Variability ratio (","\u03B3",")"),0,2,.5,1,
                      regions)

G_CWD <-   performance_plot_ext_indices(res_ext_ind_df,
                   c('v2_CWD_Gamma','v3_era_CWD_Gamma','v3_imerg_CWD_Gamma'),
                   'CWD',paste0("Variability ratio (","\u03B3",")"),0,2,.5,1,
                   regions)


G_R10mm <-   performance_plot_ext_indices(res_ext_ind_df,
                   c('v2_R10mm_G','v3_era_R10mm_G','v3_imerg_R10mm_G'),
                     'R10mm',paste0("Variability ratio (","\u03B3",")"),0,2,.5,1,
                     regions)

G_R20mm <-   performance_plot_ext_indices(res_ext_ind_df,
                    c('v2_R20mm_Gamma','v3_era_R20mm_Gamma','v3_imerg_R20mm_Gamma'),
                   'R20mm',paste0("Variability ratio (","\u03B3",")"),0,2,.5,1,
                    regions)


G_Rx1 <-   performance_plot_ext_indices(res_ext_ind_df,
                 c('v2_Rx1_Gamma','v3_era_Rx1_Gamma','v3_imerg_Rx1_Gamma'),
                 'Rx1day',paste0("Variability ratio (","\u03B3",")"),0,2,.5,1,
                  regions)

G_Rx5 <-   performance_plot_ext_indices(res_ext_ind_df,
                  c('v2_Rx5_Gamma','v3_era_Rx5_Gamma','v3_imerg_Rx5_Gamma'),
                 'Rx5day',paste0("Variability ratio (","\u03B3",")"),0,2,.5,1,
                  regions)

G_SDII <-   performance_plot_ext_indices(res_ext_ind_df,
                 c('v2_SDII_Gamma','v3_era_SDII_Gamma','v3_imerg_SDII_Gamma'),
                 'SDII',paste0("Variability ratio (","\u03B3",")"),0,2,.5,1,
                  regions)


png(paste0("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS/",
           "Fig_perf_extreme_indices_G_",region_name,".png"),
    units = "in",width = 12.5, height =6, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(G_rtotal,G_r95,G_r99,G_CDD,G_CWD,
          G_R10mm,G_R20mm,G_Rx1,G_Rx5,G_SDII,ncol=5,nrow=2,
          labels = c("a","b","c","d","e","f","g","h","i","j"))


dev.off()


#___________________________________________________________________
# Spearman Correlation coefficient (r)

regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas")
region_name <- 'COL'

sp_r95 <-   performance_plot_ext_indices(res_ext_ind_df,
                                        c('v2_r95_spearman_cor','v3_era_r95_spearman_cor',
                                          'v3_imerg_r95_spearman_cor'),
                                        "R95p",'',-1,1,.5,1,
                                        regions)

sp_r99 <-   performance_plot_ext_indices(res_ext_ind_df,
                                        c('v2_r99_spearman_cor','v3_era_r99_spearman_cor',
                                          'v3_imerg_r99_spearman_cor'),
                                        'R99p',paste0("Spearman’s rank correlation (","\u03C1",")"),-1,1,.5,1,
                                        regions)

sp_rtotal <-   performance_plot_ext_indices(res_ext_ind_df,
                                           c('v2_rtotal_spearman_cor','v3_era_rtotal_spearman_cor',
                                             'v3_imerg_rtotal_spearman_cor'),
                                           'PRCPTOT','',-1,1,.5,1,
                                           regions)

sp_rtotal <- sp_rtotal + 
  theme(
    legend.position = c(0.25, 0.3),  
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10,color='black'))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
sp_rtotal


sp_CDD <-   performance_plot_ext_indices(res_ext_ind_df,
                                        c('v2_CDD_spearman_cor','v3_era_CDD_spearman_cor',
                                          'v3_imerg_CDD_spearman_cor'),
                                        'CDD','',-1,1,.5,1,
                                        regions)

sp_CWD <-   performance_plot_ext_indices(res_ext_ind_df,
                                        c('v2_CWD_spearman_cor','v3_era_CWD_spearman_cor',
                                          'v3_imerg_CWD_spearman_cor'),
                                        'CWD','',-1,1,.5,1,
                                        regions)


sp_R10mm <-   performance_plot_ext_indices(res_ext_ind_df,
                                          c('v2_R10mm_spearman_cor','v3_era_R10mm_spearman_cor',
                                            'v3_imerg_R10mm_spearman_cor'),
                                          'R10mm','',-1,1,.5,1,
                                          regions)

sp_R20mm <-   performance_plot_ext_indices(res_ext_ind_df,
                                          c('v2_R20mm_spearman_cor','v3_era_R20mm_spearman_cor',
                                            'v3_imerg_R20mm_spearman_cor'),
                                          'R20mm','',-1,1,.5,1,
                                          regions)


sp_Rx1 <-   performance_plot_ext_indices(res_ext_ind_df,
                                        c('v2_Rx1_spearman_cor','v3_era_Rx1_spearman_cor',
                                          'v3_imerg_Rx1_spearman_cor'),
                                        'Rx1day',paste0("Spearman’s rank correlation (","\u03C1",")"),-1,1,.5,1,
                                        regions)

sp_Rx5 <-   performance_plot_ext_indices(res_ext_ind_df,
                                        c('v2_Rx5_spearman_cor','v3_era_Rx5_spearman_cor',
                                          'v3_imerg_Rx5_spearman_cor'),
                                        'Rx5day','',-1,1,.5,1,
                                        regions)

sp_SDII <-   performance_plot_ext_indices(res_ext_ind_df,
                                         c('v2_SDII_spearman_cor','v3_era_SDII_spearman_cor',
                                           'v3_imerg_SDII_spearman_cor'),
                                         'SDII','',-1,1,.5,1,
                                         regions)


png(paste0("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS/",
           "Fig_perf_extreme_indices_spearman_correlation_",region_name,".png"),
    units = "in",width = 13.39, height =6, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(sp_rtotal,sp_r95,sp_r99,sp_CDD,sp_CWD,
          sp_R10mm,sp_R20mm,sp_Rx1,sp_Rx5,sp_SDII,ncol=5,nrow=2,
          labels = c("a","b","c","d","e","f","g","h","i","j"))


dev.off()





#___________________________________________________________________
#  Median percent bias (mBIAS)

regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas")
region_name <- 'COL'


mbias_r95 <-   performance_plot_ext_indices(res_ext_ind_df,
                                         c('v2_r95_mbias','v3_era_r95_mbias',
                                           'v3_imerg_r95_mbias'),
                                         "R95p",'',-100,100,50,0,
                                         regions)

mbias_r99 <-   performance_plot_ext_indices(res_ext_ind_df,
                                         c('v2_r99_mbias','v3_era_r99_mbias',
                                           'v3_imerg_r99_mbias'),
                                         'R99p',"Median percent bias (mBIAS)",-100,100,50,0,
                                         regions)

mbias_rtotal <-   performance_plot_ext_indices(res_ext_ind_df,
                                            c('v2_rtotal_mbias','v3_era_rtotal_mbias',
                                              'v3_imerg_rtotal_mbias'),
                                            'PRCPTOT','',-100,100,50,0,
                                            regions)

mbias_rtotal <- mbias_rtotal + 
  theme(
    legend.position = c(0.25, 0.3),  
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10,color='black'))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
mbias_rtotal


mbias_CDD <-   performance_plot_ext_indices(res_ext_ind_df,
                                         c('v2_CDD_mbias','v3_era_CDD_mbias',
                                           'v3_imerg_CDD_mbias'),
                                         'CDD','',-100,100,50,0,
                                         regions)

mbias_CWD <-   performance_plot_ext_indices(res_ext_ind_df,
                                         c('v2_CWD_mbias','v3_era_CWD_mbias',
                                           'v3_imerg_CWD_mbias'),
                                         'CWD','',-100,400,100,0,
                                         regions)


mbias_R10mm <-   performance_plot_ext_indices(res_ext_ind_df,
                                           c('v2_R10mm_mbias','v3_era_R10mm_mbias',
                                             'v3_imerg_R10mm_mbias'),
                                           'R10mm','',-100,100,50,0,
                                           regions)

mbias_R20mm <-   performance_plot_ext_indices(res_ext_ind_df,
                                           c('v2_R20mm_mbias','v3_era_R20mm_mbias',
                                             'v3_imerg_R20mm_mbias'),
                                           'R20mm','',-100,100,50,0,
                                           regions)


mbias_Rx1 <-   performance_plot_ext_indices(res_ext_ind_df,
                                         c('v2_Rx1_mbias','v3_era_Rx1_mbias',
                                           'v3_imerg_Rx1_mbias'),
                                         'Rx1day',"Median percent bias (mBIAS)",-100,100,50,0,
                                         regions)

mbias_Rx5 <-   performance_plot_ext_indices(res_ext_ind_df,
                                         c('v2_Rx5_mbias','v3_era_Rx5_mbias',
                                           'v3_imerg_Rx5_mbias'),
                                         'Rx5day','',-100,100,50,0,
                                         regions)

mbias_SDII <-   performance_plot_ext_indices(res_ext_ind_df,
                                          c('v2_SDII_mbias','v3_era_SDII_mbias',
                                            'v3_imerg_SDII_mbias'),
                                          'SDII','',-100,100,50,0,
                                          regions)


png(paste0("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS/",
           "Fig_perf_extreme_indices_median_bias_",region_name,".png"),
    units = "in",width = 13.39, height =6, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(mbias_rtotal,mbias_r95,mbias_r99,mbias_CDD,mbias_CWD,
          mbias_R10mm,mbias_R20mm,mbias_Rx1,mbias_Rx5,mbias_SDII,ncol=5,nrow=2,
          labels = c("a","b","c","d","e","f","g","h","i","j"))

dev.off()


##=============================================================================

#//////////////////////////////////////////////////
# directories and data
dir_plots           <- "G:/My Drive/R4C_et_al/3_PLOTS"
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
dir_IDEAM_GPPs      <- "G:/My Drive/R4C_et_al/4_IDEAM_GPPs"

countries  <- ne_countries(type = "countries",scale = "medium")[1]
colombia <- countries[countries$name == "Colombia", ]
nat_reg_shp <- st_read("G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS/shp_regiones_naturales_colombia.shp")



col_pal <- (c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3',
              '#c7eae5','#80cdc1','#35978f','#01665e','#003c30'))

col_pal2 <- rev(c("#67001f","#b2182b","#d6604d","#f4a582","#f7f7f7",
                  "#92c5de","#4393c3","#2166ac","#053061"))

plot_extreme_map_function <- function(dataset,var,title,legend_title){
  
  dataset <- dataset %>% select(latitude, longitude, var_perf = !!sym(var))

  plot <- ggplot() +
    geom_sf(data = nat_reg_shp, color = adjustcolor("black", alpha.f = 0.7),
            fill = NA, size = 0.01)+
    geom_point(
      data = dataset,
      aes(x = longitude, y = latitude,
          color = pmax(pmin(var_perf, 100), -100)),size=.7) +
    scale_color_stepsn(
      name = legend_title,
      colors = col_pal2,
      limits = c(-100, 100),
      breaks = seq(-100, 100, by = 25),
      guide = guide_colorbar(
        title.position = "top", 
        label.position = "bottom",
        direction = "horizontal",
        barheight = unit(0.4, "cm"),
        barwidth = unit(12, "cm"))) +
    scale_x_continuous(breaks = seq(-80, -66, by = 3)) +
    scale_y_continuous(breaks = seq(-4, 12, by = 4),
                       labels = c("-4°S","0°", "4°N", "8°N","12°N")) +
    coord_sf(xlim = c(-80, -66), ylim = c(-5, 13), expand = FALSE) +
    theme_void() +
    theme(
      plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 12, face = "bold", vjust = 0, hjust = 0.5),
      legend.text = element_text(size = 11),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 12, color = 'black'),
      legend.position = "bottom",
      legend.title.align = 1,
      legend.box.spacing = unit(0, "pt")) +
    labs(x = "", y = "", title = title)
  
  return(plot)}


v3_era_cwd <- plot_extreme_map_function(res_ext_ind_df,
                        'v3_era_CWD_mbias','','Median percent bias (mBIAS)')

v3_imerg_cwd <- plot_extreme_map_function(res_ext_ind_df,
                        'v3_imerg_CWD_mbias','','Median percent bias (mBIAS)')

v2_cwd <- plot_extreme_map_function(res_ext_ind_df,
                        'v2_CWD_mbias','','Median percent bias (mBIAS)')


v3_era_cdd <- plot_extreme_map_function(res_ext_ind_df,
                  'v3_era_CDD_mbias','CHIRPSv3-ERA5','Median percent bias (mBIAS)')

v3_imerg_cdd <- plot_extreme_map_function(res_ext_ind_df,
                  'v3_imerg_CDD_mbias','CHIRPSv3-IMERG','Median percent bias (mBIAS)')

v2_cdd <- plot_extreme_map_function(res_ext_ind_df,
                 'v2_CDD_mbias','CHIRPSv2','Median percent bias (mBIAS)')


v3_era_rx1 <- plot_extreme_map_function(res_ext_ind_df,
                                        'v3_era_Rx1_mbias','','Median percent bias (mBIAS)')

v3_imerg_rx1 <- plot_extreme_map_function(res_ext_ind_df,
                                          'v3_imerg_Rx1_mbias','','Median percent bias (mBIAS)')

v2_rx1 <- plot_extreme_map_function(res_ext_ind_df,
                                    'v2_Rx1_mbias','','Median percent bias (mBIAS)')



v3_era_sdii <- plot_extreme_map_function(res_ext_ind_df,
                  'v3_era_SDII_mbias','','Median percent bias (mBIAS)')

v3_imerg_sdii <- plot_extreme_map_function(res_ext_ind_df,
                  'v3_imerg_SDII_mbias','','Median percent bias (mBIAS)')

v2_sdii <- plot_extreme_map_function(res_ext_ind_df,
                 'v2_SDII_mbias','','Median percent bias (mBIAS)')


v3_era_r20mm <- plot_extreme_map_function(res_ext_ind_df,
                                 'v3_era_R20mm_mbias','CHIRPSv3-ERA5 - R20mm','Median percent bias (mBIAS)')

v3_imerg_r20mm <- plot_extreme_map_function(res_ext_ind_df,
                                  'v3_imerg_R20mm_mbias','CHIRPSv3-IMERG - R20mm','Median percent bias (mBIAS)')

v2_r20mm <- plot_extreme_map_function(res_ext_ind_df,
                                     'v2_R20mm_mbias','','Median percent bias (mBIAS)')



png(paste0("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS/",
           "Fig_perf_extreme_indices_median_bias_map.png"),
    units = "in",width = 7, height =10, 
    res = 600, pointsize = 11)#, bg = "transparent")

p <- ggarrange(v2_cdd,v3_imerg_cdd,v3_era_cdd,
          v2_cwd,v3_imerg_cwd,v3_era_cwd,
          v2_rx1,v3_imerg_rx1,v3_era_rx1,
          v2_sdii,v3_imerg_sdii,v3_era_sdii,
          ncol=3,nrow=4,common.legend = TRUE,legend='bottom')

p1 <- p +   theme(
  plot.margin = unit(c(.01, .6, .01, .01), "cm"))+ 
  draw_plot_label(
    label = c("   Consecutive \n dry days (CDD)", 
              "   Consecutive \n wet days (CWD)",
              "Max 1-day precipitation    \n         amount (Rx1day)",
              "        Simple daily \n intensity index (SDII)"),
    size = 14,
    x = c(.93),  # Move the labels outside to the right
    y = c(0.75, 0.5, 0.2,.0),  # Adjust y position for each label
    #y = c(0.83, 0.57, 0.315,0.08),  # Adjust y position for each label
    angle = 90)

print(p1)
dev.off()




