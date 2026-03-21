
#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,PupillometryR ,
       dplyr,stringr,quantmod,rnaturalearth,exactextractr,hydroGOF,tidyr,
       future.apply,profvis,rnaturalearthdata,glue,ggpubr,GHCNr,gridExtra,cowplot   )


#//////////////////////////////////////////////////
# directories and data
dir_plots           <- "G:/My Drive/R4C_et_al/3_PLOTS"
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
dir_IDEAM_GPPs     <-  "C:/Users/santiagovalencia/OneDrive - University of Arizona/Documents/GitHub/CHIRPSv3_performance_Colombia/Datasets"

countries  <- ne_countries(type = "countries",scale = "medium")[1]
colombia <- countries[countries$name == "Colombia", ]
nat_reg_shp <- st_read("G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS/shp_regiones_naturales_colombia.shp")


#///////////////////////////////////////////////////////////////////////////////
# load performance results

#fread("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/IDEAM_GPPs_daily_2001_2023.csv",
data_daily      <-   fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_daily_2001_2023.csv"),
                         select=c('gauge_code','nat_region','pcp_ideam',
                                  'pcp_ideam_flag',"pcp_ideam_flag_scale",
                                  "chirpsv2","chirpsv3_era5","chirpsv3_imerg"))

head(data_daily)
dim(filter(data_daily,pcp_ideam_flag < 1,nat_region=='Caribe'))
dim(filter(data_daily,pcp_ideam_flag_scale < 1,nat_region=='Caribe'))

dim(filter(data_daily,pcp_ideam_flag >= 1 & pcp_ideam_flag < 5))
dim(filter(data_daily,pcp_ideam_flag_scale >= 1 & pcp_ideam_flag_scale < 5))

dim(filter(data_daily,pcp_ideam_flag >= 5 & pcp_ideam_flag < 20))
dim(filter(data_daily,pcp_ideam_flag_scale >= 5 & pcp_ideam_flag_scale < 20))


dim(filter(data_daily,chirpsv2 >= 5 & chirpsv2 < 20))
dim(filter(data_daily,chirpsv3_era5 >= 5 & chirpsv3_era5 < 20))
dim(filter(data_daily,chirpsv3_imerg >= 5 & chirpsv3_imerg < 20))

dim(filter(data_daily,chirpsv2 >= 1 & chirpsv2 < 5))
dim(filter(data_daily,chirpsv3_era5 >= 1 & chirpsv3_era5 < 5))
dim(filter(data_daily,chirpsv3_imerg >= 1 & chirpsv3_imerg < 5))

dim(filter(data_daily,chirpsv2 >= 0 & chirpsv2 < 1))
dim(filter(data_daily,chirpsv3_era5 >= 0 & chirpsv3_era5 < 1))
dim(filter(data_daily,chirpsv3_imerg >= 0 & chirpsv3_imerg < 1))





data_pentad     <- fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs","/IDEAM_GPPs_pentad.csv"),head=TRUE)

res_daily_data  <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                               "/res_performance_daily_2001_2023_df.csv")),head=TRUE)

res_pentad_data  <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                               "/res_performance_pentad_2001_2023_df.csv")),head=TRUE)

res_month_data   <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                             "/res_performance_monthly_2001_2023_df.csv")),head=TRUE)

res_month_data_p1  <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                               "/res_performance_monthly_2001_2011_df.csv")),head=TRUE)
res_month_data_p2  <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                                 "/res_performance_monthly_2012_2023_df.csv")),head=TRUE)


res_annual_data <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                             "/res_performance_annual_2001_2023_df.csv")),head=TRUE)

res_wet_sea_data <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                              "/res_performance_wet_season_acum_2001_2023_df.csv")),head=TRUE)

res_dry_sea_data <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                               "/res_performance_dry_season_acum_2001_2023_df.csv")),head=TRUE)

res_dry_wet_sea_data <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                               "/res_performance_dry_wet_season_acum_2001_2023_df.csv")),head=TRUE)


gauges_summary <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs",
                                             "/gauges_summary.csv")),head=TRUE)
gauges_summary <- gauges_summary[,c('gauge_code','map')]

head(res_month_data)
unique(res_month_data$nat_region)


#///////////////////////////////////////////////////////////////////////////////////
# Pentad, monthly and annual scales
library(tidyr)
library(ggpubr)
library(PupillometryR)

regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas")
plot_data <- function(regions,figure_name,path){

performance_plot_function_agg <- function(data,names,x_label,r_low,h_low,
                                          int,opt_value,regions){

reshape_performance_data <- function(data, names) {
  
  data <- data %>% filter(nat_region %in% regions)
  performance_long <- data %>%
    select(all_of(names)) %>%
    pivot_longer(cols = everything(), names_to = "dataset", values_to = "kge_value") %>%
    mutate(version = case_when(
      grepl(names[1], dataset) ~ "v2",  
      grepl(names[2], dataset) ~ "v2",  
      grepl(names[3], dataset) ~ "v3",   
      grepl(names[4], dataset) ~ "v3",
      TRUE ~ NA_character_  # Add a default case to avoid potential issues
    )) %>%
    mutate(product = case_when(
      grepl(names[1], dataset) ~ "CHIRP",  
      grepl(names[2], dataset) ~ "CHIRPS",  
      grepl(names[3], dataset) ~ "CHIRP",   
      grepl(names[4], dataset) ~ "CHIRPS",
      TRUE ~ NA_character_  # Add a default case to avoid potential issues
    ))
  
  #performance_long$chirpx  <- performance_long$product
#  performance_long$d2 <- paste0(performance_long$product,
 #                                    performance_long$version)
  
  return(performance_long)
}
violin_df         <- reshape_performance_data(data, names )
violin_df$chirpx  <- violin_df$product
violin_df         <- violin_df %>%
                     mutate(product2 = paste0(product,version,sep = ""))
#head(violin_df)

# Plot the violin plot with boxplot and median annotation
# Calculate the median values for each group (chirpx)
median_values <- violin_df %>%
  group_by(chirpx, product2) %>%
  summarise(median_kge = median(kge_value, na.rm = TRUE),
            .groups = "drop")

# Plot with boxplot and median values
plot_violin <- ggplot(violin_df, aes(y = kge_value, x = chirpx, fill = version)) + 
  geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = .5) +
  geom_flat_violin(
    position = position_nudge(x = .35, y = 0), 
    alpha = 0.7, 
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
    name = "Version",
    values = c(
      "v2" = adjustcolor("#7a3d8d", alpha.f = 0.9),
      "v3" = adjustcolor("#3d8d52", alpha.f = 0.9)
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
    legend.box.margin = margin(0, 0, 0, 0)
  )




return(plot_violin)
}

#////////////////////////////////////////////////////////////////////////////////////////////
# pentad scale

r_pentad <- performance_plot_function_agg(res_pentad_data,
                                      c("r_chirp_v2", "r_chirps_v2", "r_chirp_v3", "r_chirps_v3"),
                                      'Correlation coefficient (r)',-0.75,1,.25,1,
                                     regions)
r_pentad



r_pentad  <- r_pentad  + 
  theme(
    legend.position = c(0.2, 0.75),  
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))


B_pentad <- performance_plot_function_agg(res_pentad_data,
                                      c("B_chirp_v2", "B_chirps_v2", "B_chirp_v3", "B_chirps_v3"),
                                      paste0("Bias ratio (","\u03B2",")"),-0,2,.5,1,
                                      regions)
B_pentad <- B_pentad + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


G_pentad <- performance_plot_function_agg(res_pentad_data,
                                      c("G_chirp_v2", "G_chirps_v2", "G_chirp_v3", "G_chirps_v3"),
                                      paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5,1,
                                      regions)
G_pentad <- G_pentad + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


kge_pentad <- performance_plot_function_agg(res_pentad_data,
                                        c("kge_chirp_v2", "kge_chirps_v2", "kge_chirp_v3", "kge_chirps_v3"),
                                        'Kling-Gupta Efficiency (KGE)',-0.75,1,.25,1,
                                        regions)
kge_pentad <- kge_pentad + theme(axis.text.y = element_blank(),axis.title.y = element_blank())




#////////////////////////////////////////////////////////////////////////////////////////////////
# monthly scale
r_month <- performance_plot_function_agg(res_month_data,
                                       c("r_chirp_v2", "r_chirps_v2", "r_chirp_v3", "r_chirps_v3"),
                                       'Correlation coefficient (r)',-0.75,1,.25,1,
                                     regions)

B_month <- performance_plot_function_agg(res_month_data,
                                     c("B_chirp_v2", "B_chirps_v2", "B_chirp_v3", "B_chirps_v3"),
                                     paste0("Bias ratio (","\u03B2",")"),-0,2,.5,1,
                                     regions)
B_month <- B_month + theme(axis.text.y = element_blank(),axis.title.y = element_blank())

G_month <- performance_plot_function_agg(res_month_data,
                                     c("G_chirp_v2", "G_chirps_v2", "G_chirp_v3", "G_chirps_v3"),
                                     paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5,1,
                                     regions)
G_month <- G_month + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


kge_month <- performance_plot_function_agg(res_month_data,
                                       c("kge_chirp_v2", "kge_chirps_v2", "kge_chirp_v3", "kge_chirps_v3"),
                                       'Kling-Gupta Efficiency (KGE)',-0.75,1,.25,1,
                                       regions)
kge_month <- kge_month + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


#////////////////////////////////////////////////////////////////////////////////////////////////
# Seasonal scale
r_seasonal <- performance_plot_function_agg(res_dry_wet_sea_data,
                                         c("r_chirp_v2", "r_chirps_v2", "r_chirp_v3", "r_chirps_v3"),
                                         'Correlation coefficient (r)',-0.75,1,.25,1,
                                         regions)

B_seasonal <- performance_plot_function_agg(res_dry_wet_sea_data,
                                         c("B_chirp_v2", "B_chirps_v2", "B_chirp_v3", "B_chirps_v3"),
                                         paste0("Bias ratio (","\u03B2",")"),-0,2,.5,1,
                                         regions)
B_seasonal <- B_seasonal + theme(axis.text.y = element_blank(),axis.title.y = element_blank())

G_seasonal <- performance_plot_function_agg(res_dry_wet_sea_data,
                                         c("G_chirp_v2", "G_chirps_v2", "G_chirp_v3", "G_chirps_v3"),
                                         paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5,1,
                                         regions)
G_seasonal <- G_seasonal + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


kge_seasonal <- performance_plot_function_agg(res_dry_wet_sea_data,
                                           c("kge_chirp_v2", "kge_chirps_v2", "kge_chirp_v3", "kge_chirps_v3"),
                                           'Kling-Gupta Efficiency (KGE)',-0.75,1,.25,1,
                                           regions)
kge_seasonal <- kge_seasonal + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


#/////////////////////////////////////////////////////////////////////////////////////////
# annual scale
r_annual <- performance_plot_function_agg(res_annual_data,
                                      c("r_chirp_v2", "r_chirps_v2", "r_chirp_v3", "r_chirps_v3"),
                                      'Correlation coefficient (r)',-0.75,1,.25,1,
                                      regions)

B_annual <- performance_plot_function_agg(res_annual_data,
                                      c("B_chirp_v2", "B_chirps_v2", "B_chirp_v3", "B_chirps_v3"),
                                      paste0("Bias ratio (","\u03B2",")"),-0,2,.5,1,
                                      regions)
B_annual <- B_annual + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


G_annual <- performance_plot_function_agg(res_annual_data,
                                      c("G_chirp_v2", "G_chirps_v2", "G_chirp_v3", "G_chirps_v3"),
                                      paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5,1,
                                      regions)
G_annual <- G_annual + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


kge_annual <- performance_plot_function_agg(res_annual_data,
                                        c("kge_chirp_v2", "kge_chirps_v2", "kge_chirp_v3", "kge_chirps_v3"),
                                        'Kling-Gupta Efficiency (KGE)',-0.75,1,.25,1,
                                        regions)
kge_annual <- kge_annual + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


#/////////////////////////////////////////////////////////////////////////////////////////
# PLOTS 

png(paste(paste0("G:/My Drive/R4C_et_al/",path), paste0("Fig3_performance_pentad_to_annual_",figure_name,".png"),
          sep = '/'), units = "in",width = 11, height = 7, 
    res = 600, pointsize = 11)#, bg = "transparent")

r_pentad1<- r_pentad
r_pentad1 <- r_pentad1+ scale_y_continuous(
              limits = c(-0.75, 1),  # Set numeric limits
              breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
              labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))

#r_pentad <- r_pentad + theme(legend.position = "none")
B_pentad <- B_pentad+ theme(legend.position = "none")
G_pentad <- G_pentad + theme(legend.position = "none")
kge_pentad <- kge_pentad + theme(legend.position = "none")
kge_pentad <- kge_pentad + scale_y_continuous(
              limits = c(-0.75, 1),  # Set numeric limits
              breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
              labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))


r_month <- r_month + theme(legend.position = "none")
r_month <- r_month+ scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))

B_month <- B_month + theme(legend.position = "none")
G_month <- G_month + theme(legend.position = "none")
kge_month <- kge_month + theme(legend.position = "none")
kge_month <- kge_month + scale_y_continuous(
          limits = c(-0.75, 1),  # Set numeric limits
          breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
          labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))


r_seasonal <- r_seasonal + theme(legend.position = "none")
r_seasonal <- r_seasonal+ scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))

B_seasonal <- B_seasonal + theme(legend.position = "none")
G_seasonal <- G_seasonal + theme(legend.position = "none")
kge_seasonal <- kge_seasonal + theme(legend.position = "none")
kge_seasonal <- kge_seasonal + scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))




r_annual <- r_annual + theme(legend.position = "none")
r_annual <- r_annual+ scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))

B_annual <- B_annual + theme(legend.position = "none")
G_annual <- G_annual + theme(legend.position = "none")
kge_annual <- kge_annual + theme(legend.position = "none")
kge_annual <- kge_annual + scale_y_continuous(
            limits = c(-0.75, 1),  # Set numeric limits
            breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
            labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))

p <- ggarrange(
      ggarrange(r_pentad1,r_month,r_annual,ncol = 1, nrow = 3, align = "hv",labels = c("a", "e","i"),label.x = 0.2),
      ggarrange(B_pentad,B_month,B_annual,ncol = 1, nrow = 3, align = "hv",labels = c("b", "f","j"),label.x = -0.05), 
      ggarrange(G_pentad,G_month,G_annual,ncol = 1, nrow = 3, align = "hv",labels = c("c", "g","k"),label.x = -0.08),
      ggarrange(kge_pentad,kge_month,kge_annual,ncol = 1, nrow = 3, align = "hv",labels = c("d", "h","l"),
      label.x = c(-.06,-0.-0.07)),
      ncol=4,widths = c(4.1,3,3,3.05,.18),align='hv')

p <- p +   theme(
          plot.margin = unit(c(.01, .5, .01, .01), "cm"))+ 
          draw_plot_label(
            label = c("Pentad", "Monthly","Annual"),
            size = 14,
            x = c(.99),  # Move the labels outside to the right
            y = c(0.77, 0.44, 0.12),  # Adjust y position for each label
            #y = c(0.83, 0.57, 0.315,0.08),  # Adjust y position for each label
            angle = 90)

print(p)   
dev.off()


}


# Colombia plot
plot_data(c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"),"COL","3_PLOTS")
plot_data(c("Pacifico"),"Pacifico","3_PLOTS/SUPP_PLOTS")
plot_data(c("Andes"),"Andes","3_PLOTS/SUPP_PLOTS")
plot_data(c("Amazonas","Amazonia"),"Amazonas","3_PLOTS/SUPP_PLOTS")
plot_data(c("Caribe"),"Caribe","3_PLOTS/SUPP_PLOTS")
plot_data(c("Orinoquia"),"Orinoquia","3_PLOTS/SUPP_PLOTS")



regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas")
plot_data_sea <- function(regions,figure_name,path){

# Seasonal results
performance_plot_function_seasonal <- function(data,names,x_label,r_low,h_low,int,opt_value,regions){
  
  reshape_performance_data <- function(data, names) {
    
    data <- data %>% filter(nat_region %in% regions)
    performance_long <- data %>%
      select(all_of(names)) %>%
      pivot_longer(cols = everything(), names_to = "dataset", values_to = "kge_value") %>%
      mutate(version = case_when(
        grepl(names[1], dataset) ~ "v2",  
        grepl(names[2], dataset) ~ "v2",  
        grepl(names[3], dataset) ~ "v3",   
        grepl(names[4], dataset) ~ "v3",
        TRUE ~ NA_character_  # Add a default case to avoid potential issues
      )) %>%
      mutate(product = case_when(
        grepl(names[1], dataset) ~ "CHIRP",  
        grepl(names[2], dataset) ~ "CHIRPS",  
        grepl(names[3], dataset) ~ "CHIRP",   
        grepl(names[4], dataset) ~ "CHIRPS",
        TRUE ~ NA_character_  # Add a default case to avoid potential issues
      ))
    
    #performance_long$chirpx  <- performance_long$product
    #  performance_long$d2 <- paste0(performance_long$product,
    #                                    performance_long$version)
    
    return(performance_long)
  }
  violin_df         <- reshape_performance_data(data, names )
  violin_df$chirpx  <- violin_df$product
  violin_df         <- violin_df %>%
    mutate(product2 = paste0(product,version,sep = ""))
  #head(violin_df)
  
  # Plot the violin plot with boxplot and median annotation
  # Calculate the median values for each group (chirpx)
  median_values <- violin_df %>%
    group_by(chirpx, product2) %>%
    summarise(median_kge = median(kge_value, na.rm = TRUE),
              .groups = "drop")
  
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
    geom_boxplot(
      width = .25, 
      outlier.shape = NA, 
      color = "black",
      position = position_dodge(.5)
    ) + 
    scale_fill_manual(
      name = "Version",
      values = c(
        "v2" = adjustcolor("#7a3d8d", alpha.f = 0.9),
        "v3" = adjustcolor("#3d8d52", alpha.f = 0.9)
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
      legend.box.margin = margin(0, 0, 0, 0)
    )
  
  
  
  
  return(plot_violin)
}


r_dry_sea <- performance_plot_function_seasonal(res_dry_sea_data,
                                          c("r_chirp_v2", "r_chirps_v2", "r_chirp_v3", "r_chirps_v3"),
                                          'Correlation coefficient (r)',-0.75,1,.25,1,
                                          regions)
r_dry_sea


r_dry_sea  <- r_dry_sea + 
  theme(
    legend.position = c(0.2, 0.75),  
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))


B_dry_sea <- performance_plot_function_seasonal(res_dry_sea_data,
                                          c("B_chirp_v2", "B_chirps_v2", "B_chirp_v3", "B_chirps_v3"),
                                          paste0("Bias ratio (","\u03B2",")"),-0,2,.5,1,
                                          regions)
B_dry_sea <- B_dry_sea + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


G_dry_sea <- performance_plot_function_seasonal(res_dry_sea_data,
                                          c("G_chirp_v2", "G_chirps_v2", "G_chirp_v3", "G_chirps_v3"),
                                          paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5,1,
                                          regions)
G_dry_sea <- G_dry_sea + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


kge_dry_sea <- performance_plot_function_seasonal(res_dry_sea_data,
                                            c("kge_chirp_v2", "kge_chirps_v2", "kge_chirp_v3", "kge_chirps_v3"),
                                            'Kling-Gupta Efficiency (KGE)',-0.75,1,.25,1,
                                            regions)
kge_dry_sea <- kge_dry_sea + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


#///////////////////////////////////////////////////////////////

r_wet_sea <- performance_plot_function_seasonal(res_wet_sea_data,
                                                c("r_chirp_v2", "r_chirps_v2", "r_chirp_v3", "r_chirps_v3"),
                                                'Correlation coefficient (r)',-0.75,1,.25,1,
                                                regions)
r_wet_sea



B_wet_sea <- performance_plot_function_seasonal(res_wet_sea_data,
                                                c("B_chirp_v2", "B_chirps_v2", "B_chirp_v3", "B_chirps_v3"),
                                                paste0("Bias ratio (","\u03B2",")"),-0,2,.5,1,
                                                regions)
B_wet_sea <- B_wet_sea + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


G_wet_sea <- performance_plot_function_seasonal(res_wet_sea_data,
                                                c("G_chirp_v2", "G_chirps_v2", "G_chirp_v3", "G_chirps_v3"),
                                                paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5,1,
                                                regions)
G_wet_sea <- G_wet_sea + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


kge_wet_sea <- performance_plot_function_seasonal(res_wet_sea_data,
                                                  c("kge_chirp_v2", "kge_chirps_v2", "kge_chirp_v3", "kge_chirps_v3"),
                                                  'Kling-Gupta Efficiency (KGE)',-0.75,1,.25,1,
                                                  regions)
kge_wet_sea <- kge_wet_sea + theme(axis.text.y = element_blank(),axis.title.y = element_blank())




png(paste(paste0("G:/My Drive/R4C_et_al/",path), paste0("Fig_performance_seasonal_",figure_name,".png"),
          sep = '/'), units = "in",width = 11, height = 5., 
    res = 600, pointsize = 11)#, bg = "transparent")

r_dry_sea1 <- r_dry_sea
r_dry_sea1 <- r_dry_sea1+ scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))

#r_pentad <- r_pentad + theme(legend.position = "none")
B_dry_sea   <- B_dry_sea + theme(legend.position = "none")
G_dry_sea   <- G_dry_sea + theme(legend.position = "none")
kge_dry_sea <- kge_dry_sea + theme(legend.position = "none")
kge_dry_sea <- kge_dry_sea + scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))
kge_dry_sea <- kge_dry_sea + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


r_wet_sea <- r_wet_sea + theme(legend.position = "none")
r_wet_sea <- r_wet_sea + scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))

B_wet_sea   <- B_wet_sea + theme(legend.position = "none")
G_wet_sea   <- G_wet_sea + theme(legend.position = "none")
kge_wet_sea <- kge_wet_sea + theme(legend.position = "none")
kge_wet_sea <- kge_wet_sea + scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))


p <- ggarrange(
  ggarrange(r_dry_sea1,r_wet_sea,ncol = 1, nrow = 2, align = "hv",labels = c("a", "e"),label.x = 0.2),
  ggarrange(B_dry_sea,B_wet_sea,ncol = 1, nrow = 2, align = "hv",labels = c("b", "f"),label.x = -0.05), 
  ggarrange(G_dry_sea,G_wet_sea,ncol = 1, nrow = 2, align = "hv",labels = c("c", "g"),label.x = -0.07),
  ggarrange(kge_dry_sea,kge_wet_sea,ncol = 1, nrow = 2, align = "hv",labels = c("d", "h"),
            label.x = c(-0.07)),
  ncol=4,widths = c(4.1,3,3,3.02,.08),align='hv')

p <- p +   theme(
  plot.margin = unit(c(.01, .5, .01, .01), "cm"))+ 
  draw_plot_label(
    label = c("Dry season", "Wet season"),
    size = 12,
    x = c(.99),  # Move the labels outside to the right
    y = c(0.62, 0.12),  # Adjust y position for each label
    angle = 90)

print(p) 
dev.off()


}

plot_data_sea(c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"),"COL","3_PLOTS/SUPP_PLOTS")
plot_data_sea(c("Pacifico"),"Pacifico","3_PLOTS/SUPP_PLOTS")
plot_data_sea(c("Andes"),"Andes","3_PLOTS/SUPP_PLOTS")
plot_data_sea(c("Orinoquia"),"Orinoquia","3_PLOTS/SUPP_PLOTS")
plot_data_sea(c("Amazonia","Amazonas"),"Amazonas","3_PLOTS/SUPP_PLOTS")
plot_data_sea(c("Caribe"),"Caribe","3_PLOTS/SUPP_PLOTS")



# Categorical indices
#///////////////////////////////////////////////////////////////////////////////
# Categorical pentad indices
cat_pentad_df <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                                           "/res_performance_daily_categorical_indices_pentad_2001_2023_df.csv"),head=TRUE))

library(colorspace)
cat_ind_pentad_plot <- function(data_df,data_pentad,var,y_min,y_max,int,title,opt_value,
                                second_y_title,second_size,regions,xlabel){
  #var <- 'POD'
  data_pentad_reg <- na.omit(filter(data_pentad,nat_region==regions))
  freq_int_1 <- dim(filter(data_pentad_reg,pcp_ideam_flag<1))[1]/dim(data_pentad_reg)[1]
  freq_int_2 <- dim(filter(data_pentad_reg,pcp_ideam_flag>=1 & pcp_ideam_flag<5))[1]/dim(data_pentad_reg)[1]
  freq_int_3 <- dim(filter(data_pentad_reg,pcp_ideam_flag>=5 & pcp_ideam_flag<20))[1]/dim(data_pentad_reg)[1]
  freq_int_4 <- dim(filter(data_pentad_reg,pcp_ideam_flag>=20 & pcp_ideam_flag<40))[1]/dim(data_pentad_reg)[1]
  freq_int_5 <- dim(filter(data_pentad_reg,pcp_ideam_flag>=40))[1]/dim(data_pentad_reg)[1]
  
  
  # Load pcp frequency
  bar_data <- data.frame(
    intensity = c(1, 2, 3, 4, 5),
    count = c(freq_int_1, freq_int_2, freq_int_3, freq_int_4, freq_int_5))
  
  # Normalize the count to fit on the same plot
  bar_data <- bar_data %>% mutate(scaled_count = count / max(count) * (y_max - y_min) + y_min)
  
  metrics_columns <- c(
    paste0(var,"v2_i1"), paste0(var,"v2_i2"), paste0(var,"v2_i3"), paste0(var,"v2_i4"), paste0(var,"v2_i5"), 
    paste0(var,"v3_i1"), paste0(var,"v3_i2"), paste0(var,"v3_i3"), paste0(var,"v3_i4"), paste0(var,"v3_i5"), 
    paste0(var,"v2s_i1"), paste0(var,"v2s_i2"), paste0(var,"v2s_i3"), paste0(var,"v2s_i4"), paste0(var,"v2s_i5"), 
    paste0(var,"v3s_i1"), paste0(var,"v3s_i2"), paste0(var,"v3s_i3"), paste0(var,"v3s_i4"), paste0(var,"v3s_i5"))
  
  data_df <- filter(data_df,nat_region==regions)
  metrics_data <- data_df[, metrics_columns]
  metrics_long <- metrics_data %>%
    gather(key = "category_intensity", value = "value") %>%
    mutate(
      # Create 'metric', 'category', and 'intensity' columns from the column names
      metric = gsub("v[0-9]+_i[1-5]", "", category_intensity),  
      category = gsub("_i[1-5]", "", category_intensity),
      intensity = as.integer(gsub(".*_i([1-5])", "\\1", category_intensity))
    )
  
  # Calculate the median for each combination of metric, category, and intensity
  metrics_median <- metrics_long %>%
    group_by(metric, category, intensity) %>%
    summarise(median_value = median(value, na.rm = TRUE))
  
  metrics_median <- metrics_median %>%
    filter(category == paste0(var,"v2") | category == paste0(var,"v3") |
             category == paste0(var,"v2s") | category == paste0(var,"v3s"))
  
  
  ggplot() +
    geom_col(data = bar_data, aes(x = intensity, y = scaled_count), 
             fill = "#d95f02", width = 0.5, alpha = 0.2) +
    geom_line(data = metrics_median,aes(x = intensity, y = median_value, color = category, group = category)) + 
    geom_point(data = metrics_median,aes(x = intensity, y = median_value, color = category, shape=category,
                                         size = .1)) +
    scale_shape_manual(
      values = setNames(c(17, 15, 16, 18), unique(metrics_median$category)))+
    labs(
      title = title,
      x = xlabel,
      y = "",
      color = "Version",
      shape="Version"
    ) +
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = .5) +
    theme_classic() +
    # Axis labels and color
    theme_classic() +
    
    # Primary axis (metrics)
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = seq(y_min, y_max, int),
      sec.axis = sec_axis(
        trans = ~ (. - y_min) / (y_max - y_min) * max(bar_data$count),
        name = second_y_title,
      )) +
    
    # X-axis setup
    scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
      labels = c("[0,1)", "[1,5)", "[5,20)", "[20,40)", paste0("\u2265", "40"))
    ) +
    
    # Custom color
    scale_color_manual(
     # values = setNames(c("#7a3d8d","#3d8d52", adjust_transparency("#7a3d8d", alpha =.5),"#9ec6bc"),
      values = setNames(c("#7a3d8d","#3d8d52","#bb8ec5" ,"#9ec6bc"),
                        unique(metrics_median$category)),
      labels = c("CHIRPSv2", "CHIRPv2", "CHIRPSv3", "CHIRPv3")) +
    scale_shape_manual(
      values = setNames(c(17, 15, 16, 18), unique(metrics_median$category)),  # ejemplo
      labels = c("CHIRPSv2", "CHIRPv2", "CHIRPSv3", "CHIRPv3")
    ) +
    guides(size = "none",
           color = guide_legend(
             override.aes = list(size = 4))) +
    
    # Styling
    theme(
      plot.margin = unit(c(.005, .0, .001, .0), "cm"),  ##unit(c(top, right, bottom, left), "cm")
      plot.title = element_text(size = 15, face = 'bold'),
      legend.title = element_text(size = 15, vjust = 0.5, hjust = 0., color = 'black'),
      legend.text = element_text(size = 13.5, color = 'black'),
      legend.position = 'bottom',
      legend.title.align = 1,
      axis.title.y.left = element_text(size = 12, color = "black"),
      axis.text.y.left  = element_text(size = 12, color = "black"),
      # Secondary y-axis styling (right side)
      axis.title.y.right = element_text(size = second_size, color = "#d95f02"),
      axis.text.y.right  = element_text(size = second_size, color = "#d95f02"),
      axis.title.x = element_text(size = 14, color = "black"),
      axis.text.x  = element_text(size = 12, color = "black"),
      legend.box.spacing = unit(-.001, "pt"),
      legend.margin = margin(.6, 0.0, 0., 0.00),
      legend.box.margin = margin(0, 0, 0, 0)
    )
  
  
  
}

pdo_col <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"POD",0,1,.25,"Probability of detection (POD)",1,"",.01,
                    c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"),"")
ets_col <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"ETS",0,1,.25,"Equitable threat score (ETS)",1,"",.01,
                    c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"),"")
far_col <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"FAR",0,1,.25,"False alarm ratio (FAR)",0,"",.01,
                    c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"),"")
fbias_col <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"fBIAS",0,2,.5,"Frequency bias (fBIAS)",1,"Events frequency",12,
                    c("Pacifico","Andes","Caribe","Amazonia","Orinoquia","Amazonas"),"")




pdo_car <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"POD",0,1,.25,"",1,"",.01,
                    c("Caribe"),"")
ets_car <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"ETS",-0.005,1,.25,"",1,"",.01,
                    c("Caribe"),"")
far_car <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"FAR",0,1,.25,"",0,"",.01,
                    c("Caribe"),"")
fbias_car <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"fBIAS",0,5,1,"",1,"Events frequency",12,
                    c("Caribe"),"")


pdo_pac <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"POD",0,1,.25,"",1,"",.01,
                    c("Pacifico"),"Precipitation intensity (mm/day)")
ets_pac <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"ETS",0.,1,.25,"",1,"",.01,
                    c("Pacifico"),"Precipitation intensity (mm/day)")
far_pac <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"FAR",0,1,.25,"",0,"",.01,
                    c("Pacifico"),"Precipitation intensity (mm/day)")
fbias_pac <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"fBIAS",0,2,.5,"",1,"Events frequency",12,
                    c("Pacifico"),"Precipitation intensity (mm/day)")



pdo_amaz <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"POD",0,1,.25,"",1,"",.01,
                    c("Amazonia"),"")
ets_amaz <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"ETS",0.,1,.25,"",1,"",.01,
                    c("Amazonia"),"")
far_amaz <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"FAR",0,1,.25,"",0,"",.01,
                    c("Amazonia"),"")
fbias_amaz <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"fBIAS",0,2,.5,"",1,"Events frequency",12,
                    c("Amazonia"),"")


pdo_and <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"POD",0,1,.25,"",1,"",.01,
                                c("Andes"),"")
ets_and <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"ETS",0.,1,.25,"",1,"",.01,
                                c("Andes"),"")
far_and <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"FAR",0,1,.25,"",0,"",.01,
                                c("Andes"),"")
fbias_and <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"fBIAS",0,2,.5,"",1,"Events frequency",12,
                                  c("Andes"),"")


pdo_orin <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"POD",0,1,.25,"",1,"",.01,
                               c("Orinoquia"),"")
ets_orin <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"ETS",0.,1,.25,"",1,"",.01,
                               c("Orinoquia"),"")
far_orin <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"FAR",0,1,.25,"",0,"",.01,
                               c("Orinoquia"),"")
fbias_orin <- cat_ind_pentad_plot(cat_pentad_df,data_pentad,"fBIAS",0,3,.5,"",1,"Events frequency",12,
                                 c("Orinoquia"),"")


png(paste("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS", paste0("Fig_performance_cat_pentad.png"),
          sep = '/'), units = "in",width = 18, height =13.5, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(pdo_col,ets_col,far_col,fbias_col,
          pdo_and,ets_and,far_and,fbias_and,
          pdo_car,ets_car,far_car,fbias_car,
          pdo_amaz,ets_amaz,far_amaz,fbias_amaz,
          pdo_orin,ets_orin,far_orin,fbias_orin,
          pdo_pac,ets_pac,far_pac,fbias_pac,
          ncol = 4, nrow = 6, align = "hv",labels = c("a","b","c","d",
                                                      "e","f","g","h",
                                                      "i","j","k","l",
                                                      "m","n","o","p",
                                                      "q","r","s","t",
                                                      "u","v","w","x"),
          label.x = 0.1,common.legend = TRUE, legend = "bottom")+
        theme(plot.margin = unit(c(.01, .01, .01, .05), "cm"))+  #unit(c(top, right, bottom, left), "cm")
        draw_plot_label(
          label = c("Colombia", "Andes","Caribbean","Amazon","Orinoco","Pacific"),
          size = 16,
          x = c(-.002),  # Move the labels outside to the right
          y = c(0.845, 0.71,.52,.37,.205,.055),  # Adjust y position for each label
          angle = 90)
  
  
dev.off()




#//////////////////////////////////////////////////////////////////////////
# Elevation and climatic gradients

elev_grad_month_plot <- function(data, var, y_label,x_label, y_n, r_low, h_low, int,
                                 opt_value,x_labels,angle,xsize) {
  
  # Subconjunto dos dados e renomear colunas
  dx <- data[, c("elevation", paste0(var, 'chirp_v2'),paste0(var, 'chirps_v2'),
                 paste0(var, 'chirp_v3'),paste0(var, 'chirps_v3'))]
  colnames(dx) <- c('elevation', 'CHIRPv2','CHIRPSv2', 'CHIRPv3', 'CHIRPSv3')
  
  # Criar classes de elevação (intervalos de 250m)
  dx <- dx %>% mutate(elev_class = floor(elevation / 250))
  
  # Gerar vetor de rótulos para o eixo X (classes)
  max_class <- max(dx$elev_class, na.rm = TRUE)
  breaks_vector <- seq(0, max_class) * 250
  labels_vector <- paste0("[", breaks_vector, ",", breaks_vector + 250, ")")
  
  # Aplicar labels como fator
  dx$elev_class <- factor(dx$elev_class, levels = 0:max_class, labels = labels_vector)
  
  # Reshape dos dados para formato longo
  dx_long <- dx %>%
    pivot_longer(cols = contains("CHIRP"), names_to = "dataset", values_to = "value")
  
  # Contar observações por classe de elevação
  elev_counts <- dx %>%
    group_by(elev_class) %>%
    summarise(n = n(), .groups = "drop")
  
  # Plot
  p <- ggplot(dx_long, aes(x = elev_class, y = value, fill = dataset)) +
    geom_boxplot(alpha = 0.7) +
    geom_text(data = elev_counts, aes(x = elev_class, y = y_n, label = paste0("[", n, "]")),
              inherit.aes = FALSE, size = 3.5, vjust = 1.5, color = "black") +
    labs(x = x_label, y = y_label, fill = "Version") +
    theme_classic() +
    scale_y_continuous(
      limits = c(r_low, h_low),
      breaks = seq(r_low, h_low, by = int),
      labels = seq(r_low, h_low, by = int)
    ) +
    scale_x_discrete(labels = x_labels)+
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = 0.5) +
    theme(
      plot.margin = unit(c(.0, .0, .0, .0), "cm"),
      plot.title = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"),
      legend.text = element_text(size = 11, color = "black"),
      legend.position = "bottom",
      legend.title.align = 1,
      axis.title.y = element_text(size = 13, vjust = 0.5, color = "black"),
      axis.title.x = element_text(size = xsize, color = "black"),
      axis.text.x = element_text(size = 11, color = "black", angle = angle, hjust = 1),
      axis.text.y = element_text(size = 11, color = "black"),
      legend.box.spacing = unit(-.001, "pt"),
      legend.margin = margin(.001, 0.03, 0.01, 0.0025),
      legend.box.margin = margin(0, 0, 0, 0)
    ) +
    scale_fill_manual(values = c(
      "CHIRPSv2" = "#7a3d8d", 
      "CHIRPSv3" = "#3d8d52",
      "CHIRPv3" = "#9ec6bc",
      "CHIRPv2" = "#b580c2"
    ))
  
  return(p)
}


x_labs_num <-  c("[0,250)"  ,   "" ,  "[500,750)" ,  "" ,
                 "[1000,1250)", "" ,"[1500,1750)", "",
                 "[2000,2250)" ,"", "[2500,2750)", "",
                 "[3000,3250)", "" ,"[3500,3750)")
x_labs_na   <-  c(""  ,   "" ,  "" ,  "" ,
                  "", "" ,"", "",
                  "" ,"", "", "",
                  "", "" ,"")


r_elev   <- elev_grad_month_plot(res_month_data,'r_','Correlation \n coefficient (r)',"",
                                 .1,-0.,1,.25,1,x_labs_na,0,.1)
B_elev   <- elev_grad_month_plot(res_month_data,'B_',paste0("Bias \n ratio (","\u03B2",")"),"",
                                 -1,-0,1.5,.25,1,x_labs_na,0,.1)
G_elev   <- elev_grad_month_plot(res_month_data,'G_',paste0("Variability \n ratio (","\u03B3",")"),
                                 "",-2,-0,1.5,.25,1,x_labs_na,0,.1)
kge_elev <- elev_grad_month_plot(res_month_data,'kge_','Kling-Gupta \n Efficiency (KGE)',"Elevation (m.a.s.l.)",
                                 -.99,-0,1,.25,1,x_labs_num,25,14)


r_pentad_elev   <- elev_grad_month_plot(res_pentad_data,'r_','Correlation \n coefficient (r)',"",
                                 .1,-0.,1,.25,1,x_labs_na,0,.1)
B_pentad_elev   <- elev_grad_month_plot(res_pentad_data,'B_',paste0("Bias \n ratio (","\u03B2",")"),"",
                                 -1,-0,1.5,.25,1,x_labs_na,0,.1)
G_pentad_elev   <- elev_grad_month_plot(res_pentad_data,'G_',paste0("Variability \n ratio (","\u03B3",")"),
                                 "",-2,-0,1.5,.25,1,x_labs_na,0,.1)
kge_pentad_elev <- elev_grad_month_plot(res_pentad_data,'kge_','Kling-Gupta \n Efficiency (KGE)',"Elevation (m.a.s.l.)",
                                 -.99,-0,1,.25,1,x_labs_num,25,14)


library(cowplot)
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_daily_elev_grad_month_scale.png",
          sep = '/'), units = "in",width = 10, height = 8, 
          res = 600, pointsize = 11)#, bg = "transparent")

r_elev1 <- r_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm")) #unit(c(top, right, bottom, left), "cm")
B_elev1 <- B_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
G_elev1 <- G_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
kge_elev1 <- kge_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))

ggarrange(
  r_elev1, B_elev1, G_elev1, kge_elev1,
  align = "v",labels = c("a", "b", "c", "d"),
  font.label = list(size = 18, face = "bold"),
  nrow = 4,ncol = 1,label.x = 0.0, label.y = 1.1,
  common.legend = TRUE,legend = "top",
  heights = c(2, 2, 2, 2.5))

dev.off()


library(cowplot)
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_daily_elev_grad_pentad_scale.png",
          sep = '/'), units = "in",width = 10, height = 8, 
    res = 600, pointsize = 11)#, bg = "transparent")

r_elev1 <- r_pentad_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm")) #unit(c(top, right, bottom, left), "cm")
B_elev1 <- B_pentad_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
G_elev1 <- G_pentad_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
kge_elev1 <- kge_pentad_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))

ggarrange(
  r_elev1, B_elev1, G_elev1, kge_elev1,
  align = "v",labels = c("a", "b", "c", "d"),
  font.label = list(size = 18, face = "bold"),
  nrow = 4,ncol = 1,label.x = 0.0, label.y = 1.1,
  common.legend = TRUE,legend = "top",
  heights = c(2, 2, 2, 2.5))

dev.off()


library(cowplot)
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_daily_elev_grad_pentad_monthly_scale.png",
          sep = '/'), units = "in",width = 16, height = 8, 
    res = 600, pointsize = 11)#, bg = "transparent")

r_elev1 <- r_pentad_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm")) #unit(c(top, right, bottom, left), "cm")
B_elev1 <- B_pentad_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
G_elev1 <- G_pentad_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
kge_elev1 <- kge_pentad_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))

r_elev11 <- r_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm")) #unit(c(top, right, bottom, left), "cm")
B_elev11 <- B_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
G_elev11 <- G_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
kge_elev11 <- kge_elev + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))

ggarrange(
ggarrange(
  r_elev1, B_elev1, G_elev1, kge_elev1,
  align = "v",labels = c("a", "b", "c", "d"),
  font.label = list(size = 18, face = "bold"),
  nrow = 4,ncol = 1,label.x = 0.0, label.y = 1.1,
  common.legend = TRUE,legend = "top",
  heights = c(2, 2, 2, 2.5)),
ggarrange(
  r_elev11, B_elev11, G_elev11, kge_elev11,
  align = "v",labels = c("e", "f", "g", "h"),
  font.label = list(size = 18, face = "bold"),
  nrow = 4,ncol = 1,label.x = 0.0, label.y = 1.1,
  common.legend = TRUE,legend = "top",
  heights = c(2, 2, 2, 2.5)),ncol=2,  common.legend = TRUE,legend = "top")

dev.off()




# MAP gradients
res_daily_data_map <- merge(res_daily_data,gauges_summary,by='gauge_code',all.x=TRUE)

map_grad_plot_month <- function(data, var, y_label,x_label, y_n, r_low, h_low, int,
                          opt_value,x_labels,angle,xsize) {
  
  dx <- data[, c("map", paste0(var, 'chirp_v2'),paste0(var, 'chirps_v2'),
                 paste0(var, 'chirp_v3'),paste0(var, 'chirps_v3'))]
  colnames(dx) <- c('map', 'CHIRPv2','CHIRPSv2', 'CHIRPv3', 'CHIRPSv3')
  
  dx$map <- ifelse(dx$map > 7000,7000,dx$map)
  # Criar classes de elevação (intervalos de 250m)
  dx <- dx %>% mutate(map_class = floor(map / 500))
  
  # Gerar vetor de rótulos para o eixo X (classes)
  max_class <- max(dx$map_class, na.rm = TRUE)
  breaks_vector <- seq(0, max_class) * 500
  labels_vector <- paste0("[", breaks_vector, ",", breaks_vector + 500, ")")
  
  # Aplicar labels como fator
  dx$map_class <- factor(dx$map_class, levels = 0:max_class, labels = labels_vector)
  
  # Reshape dos dados para formato longo
  dx_long <- dx %>%
    pivot_longer(cols = contains("CHIRP"), names_to = "dataset", values_to = "value")
  
  # Contar observações por classe de elevação
  map_counts <- dx %>%
    group_by(map_class) %>%
    summarise(n = n(), .groups = "drop")
  
  # Plot
  p <- ggplot(dx_long, aes(x = map_class, y = value, fill = dataset)) +
    geom_boxplot(alpha = 0.7) +
    geom_text(data = map_counts, aes(x = map_class, y = y_n, label = paste0("[", n, "]")),
              inherit.aes = FALSE, size = 3.5, vjust = 1.5, color = "black") +
    labs(x = x_label, y = y_label, fill = "Version") +
    theme_classic() +
    scale_y_continuous(
      limits = c(r_low, h_low),
      breaks = seq(r_low, h_low, by = int),
      labels = seq(r_low, h_low, by = int)
    ) +
    scale_x_discrete(labels = x_labels)+
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = 0.5) +
    theme(
      plot.margin = unit(c(.0, .0, .0, .0), "cm"),
      plot.title = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"),
      legend.text = element_text(size = 11, color = "black"),
      legend.position = "bottom",
      legend.title.align = 1,
      axis.title.y = element_text(size = 13, vjust = 0.5, color = "black"),
      axis.title.x = element_text(size = xsize, color = "black"),
      axis.text.x = element_text(size = 11, color = "black", angle = angle, hjust = 1),
      axis.text.y = element_text(size = 11, color = "black"),
      legend.box.spacing = unit(-.001, "pt"),
      legend.margin = margin(.001, 0.03, 0.01, 0.0025),
      legend.box.margin = margin(0, 0, 0, 0)
    ) +
    scale_fill_manual(values = c(
      "CHIRPSv2" = "#7a3d8d", 
      "CHIRPSv3" = "#3d8d52",
      "CHIRPv3" = "#9ec6bc",
      "CHIRPv2" = "#b580c2"
    ))
  
  return(p)
}


x_labs_num <-   c("[0,500)"  ,   "" , "[1000,1500)",
                  "" ,"[2000,2500)", "",
                  "[3000,3500)" ,"", "[4000,4500)",
                  "" ,"[5000,5500)", "",
                  "[6000,6500)" ,"", "[7000,12400)")

x_labs_na  <-  c("" ,    "",  "",
                 "", "" ,"",
                 "", "", "",
                 "", "", "",
                 "", "" ,"")

res_month_data_map <- merge(res_month_data,gauges_summary,by='gauge_code',all.x=TRUE)
r_map   <- map_grad_plot_month(res_month_data_map,'r_','Correlation \n coefficient (r)',"",
                               .1,-0.,1,.25,1,x_labs_na,0,.1)
B_map   <- map_grad_plot_month(res_month_data_map,'B_',paste0("Bias \n ratio (","\u03B2",")"),"",
                               -10,-0,2.,.5,1,x_labs_na,0,.1)
G_map   <- map_grad_plot_month(res_month_data_map,'G_',paste0("Variability \n ratio (","\u03B3",")"),
                         "",-20,-0,2,.5,1,x_labs_na,0,.1)
kge_map <- map_grad_plot_month(res_month_data_map,'kge_','Kling-Gupta \n Efficiency (KGE)',
                         "Mean annual precipitation (mm)",-99,-0,1,.25,1,x_labs_num,25,14)


res_pentad_data_map <- merge(res_pentad_data,gauges_summary,by='gauge_code',all.x=TRUE)
r_pentad_map   <- map_grad_plot_month(res_pentad_data_map,'r_','Correlation \n coefficient (r)',"",
                               .1,-0.,1,.25,1,x_labs_na,0,.1)
B_pentad_map   <- map_grad_plot_month(res_pentad_data_map,'B_',paste0("Bias \n ratio (","\u03B2",")"),"",
                               -10,-0,2.,.5,1,x_labs_na,0,.1)
G_pentad_map   <- map_grad_plot_month(res_pentad_data_map,'G_',paste0("Variability \n ratio (","\u03B3",")"),
                               "",-20,-0,2,.5,1,x_labs_na,0,.1)
kge_pentad_map <- map_grad_plot_month(res_pentad_data_map,'kge_','Kling-Gupta \n Efficiency (KGE)',
                               "Mean annual precipitation (mm)",-99,-0,1,.25,1,x_labs_num,25,14)




library(cowplot)
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_daily_MAP_grad_month_scale.png",
          sep = '/'), units = "in",width = 10, height = 8, 
    res = 600, pointsize = 11)#, bg = "transparent")

r_map1 <- r_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm")) #unit(c(top, right, bottom, left), "cm")
B_map1 <- B_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
G_map1 <- G_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
kge_map1 <- kge_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))

ggarrange(
  r_map1, B_map1, G_map1, kge_map1,
  align = "v",labels = c("a", "b", "c", "d"),
  font.label = list(size = 18, face = "bold"),
  nrow = 4,ncol = 1,label.x = 0.0, label.y = 1.1,
  common.legend = TRUE,legend = "top",
  heights = c(2, 2, 2, 2.5))

dev.off()




library(cowplot)
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_daily_MAP_grad_pentad_monthly_scale.png",
          sep = '/'), units = "in",width = 16, height = 8, 
    res = 600, pointsize = 11)#, bg = "transparent")

r_map1 <- r_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm")) #unit(c(top, right, bottom, left), "cm")
B_map1 <- B_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
G_map1 <- G_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
kge_map1 <- kge_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))

r_map11 <- r_pentad_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm")) #unit(c(top, right, bottom, left), "cm")
B_map11 <- B_pentad_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
G_map11 <- G_pentad_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))
kge_map11 <- kge_map + theme(plot.margin = unit(c(0.05, 0.2, 0.05, 0.2), "cm"))

ggarrange(
ggarrange(
  r_map11, B_map11, G_map11, kge_map11,
  align = "v",labels = c("a", "b", "c", "d"),
  font.label = list(size = 18, face = "bold"),
  nrow = 4,ncol = 1,label.x = 0.0, label.y = 1.1,
  common.legend = TRUE,legend = "top",
  heights = c(2, 2, 2, 2.5)),
ggarrange(
  r_map1, B_map1, G_map1, kge_map1,
  align = "v",labels = c("e", "f", "g", "h"),
  font.label = list(size = 18, face = "bold"),
  nrow = 4,ncol = 1,label.x = 0.0, label.y = 1.1,
  common.legend = TRUE,legend = "top",
  heights = c(2, 2, 2, 2.5)),ncol=2)
dev.off()







#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////
# daily scale

regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia")
names <- c("r_chirp_v2", "r_chirps_v2", "r_chirp_v3","r_chirps_v3_era5","r_chirps_v3_imerg")


performance_plot_function <- function(data,names,x_label,r_low,h_low,int,
                                      opt_value,regions){
  
  reshape_performance_data <- function(data, names) {
    
    data <- data %>% filter(nat_region %in% regions)
    performance_long <- data %>%
      select(all_of(names)) %>%
      pivot_longer(cols = everything(), names_to = "dataset", values_to = "kge_value") %>%
      mutate(version = case_when(
        grepl(names[1], dataset) ~ "v2", 
        #grepl(names[2], dataset) ~ "v2",  
        #grepl(names[3], dataset) ~ "v3",
        grepl(names[2], dataset) ~ "v3-ERA5",  
        grepl(names[3], dataset) ~ "v3-IMERG",   
        TRUE ~ NA_character_  # Add a default case to avoid potential issues
      )) %>%
      mutate(product = case_when(
        grepl(names[1], dataset) ~ "CHIRPS",  
        #grepl(names[2], dataset) ~ "CHIRP",  
        #grepl(names[3], dataset) ~ "CHIRP",   
        grepl(names[2], dataset) ~ "CHIRPS",
        grepl(names[3], dataset) ~ "CHIRPS",        
        TRUE ~ NA_character_  # Add a default case to avoid potential issues
      ))
    
    #performance_long$chirpx  <- performance_long$product
    #  performance_long$d2 <- paste0(performance_long$product,
    #                                    performance_long$version)
    
    return(performance_long)
  }
  violin_df         <- reshape_performance_data(data, names )
  violin_df$chirpx  <- violin_df$product
  violin_df         <- violin_df %>%
    mutate(product2 = paste0(product,version,sep = ""))
  head(violin_df)
  
  # Plot with boxplot and median values
  plot_violin <- 
    ggplot(violin_df, aes(y = kge_value, x = chirpx, fill = version)) + 
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = .5) +
    geom_flat_violin(position = position_nudge(x = .35, y = 0), 
                     alpha = 0.5, 
                     trim=TRUE,
                     stat = "ydensity",
                     scale = "area", # area
                     color = NA) +  # Set border color to transparent (NA)

    coord_flip() + 
    #geom_jitter(alpha = 0.5, width = 0.15) + 
    theme(legend.position = "none") + 
    geom_boxplot(width = .25, outlier.shape = NA, col = "black",
                 position = position_dodge(.5)) + 
    scale_fill_manual(name = "Version",  # Set the legend title to "version"
                      values = c("v2" = adjustcolor("#7a3d8d", alpha.f = 0.95),  
                                # "v3" = adjustcolor("#3d8d52", alpha.f = 0.5),  
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
    ) +
    scale_y_continuous(
      limits = c(r_low, h_low), # Set y-axis limits
      breaks = seq(r_low, h_low, by = int), # Set breaks at every 0.25 interval
      labels = seq(r_low, h_low, by = int)) +  # Set labels to match the breaks
    # scale_x_discrete(limits = unique(violin_df$product)) +
    ylab(x_label) +
    xlab('') +
    #theme_cowplot() +
    theme_classic() +
    theme(
      plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm"),
      plot.title = element_text(size = 10, color = 'black'),
      
      # Axis titles in black and bold
      axis.title.y = element_text(size = 12, vjust = 0.5, color = 'black'),
      axis.title.x = element_text(size = 12, color = 'black'),
      
      # Axis labels (tick marks) in black
      axis.text = element_text(size = 12, color = 'black'),
      
      # Legend styles
      legend.title = element_text(size = 16, vjust = 0.0, hjust = 0.5, color = 'black'),
      legend.text = element_text(size = 14, color = 'black'),
      legend.position = "none",
      legend.title.align = 1,
      legend.box.spacing = unit(-.001, "pt"), 
      legend.margin = margin(.001, 0.03, 0., 0.0025),
      legend.box.margin = margin(0, 0, 0, 0))
  

  return(plot_violin)
}

r_day_2001 <- performance_plot_function(res_daily_data ,
                                   c("r_chirps_v2",# "r_chirp_v2", "r_chirp_v3",
                                     "r_chirps_v3_era5","r_chirps_v3_imerg"),
                                   'Correlation coefficient (r)',-0.75,1,.25,1,
                                   regions)
r_day_2001


r_day_2001 <- r_day_2001  + 
  theme(
    legend.position = c(0.25, 0.75),  
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10,color='black'))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
r_day_2001


r_day_2001 <- r_day_2001 + theme(axis.text.y = element_blank(),axis.title.y = element_blank())
r_day_2001 <- r_day_2001 + scale_y_continuous(
                limits = c(-0.75, 1),  # Set numeric limits
                breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.00),  # Numeric breaks
                labels = c("", "-0.50", "", "0", "", "0.50", "", "1.00"))

B_day_2001 <- performance_plot_function(res_daily_data,
                                   c("B_chirps_v2",# "B_chirp_v2","B_chirp_v3",
                                     "B_chirps_v3_era5","B_chirps_v3_imerg"),
                                   paste0("Bias ratio (","\u03B2",")"),-0,2,.5,1,
                                   regions)
B_day_2001 <- B_day_2001 + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


G_day_2001 <- performance_plot_function(res_daily_data,
                                   c("G_chirps_v2",#"G_chirp_v2","G_chirp_v3",
                                     "G_chirps_v3_era5","G_chirps_v3_imerg"),
                                   paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5,1,
                                   regions)
G_day_2001 <- G_day_2001 + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


kge_day_2001 <- performance_plot_function(res_daily_data,
                                     c("kge_chirps_v2",#"kge_chirp_v2","kge_chirp_v3",
                                       "kge_chirps_v3_era5","kge_chirps_v3_imerg"),
                                     'Kling-Gupta Efficiency (KGE)',-0.75,1,.25,1,
                                     regions)
kge_day_2001 <- kge_day_2001 + theme(axis.text.y = element_blank(),axis.title.y = element_blank())

kge_day_2001 <- kge_day_2001 + scale_y_continuous(
                  limits = c(-0.75, 1),  # Set numeric limits
                  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.00),  # Numeric breaks
                  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.00"))

#///////////////////////////////////////////////////////////////////////////////
# Categorical indices
cat_daily_df <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                "/res_performance_daily_categorical_indices_2001_2023_df_v2.csv"),head=TRUE))

pcp_int_freq_df <- data.frame(intensity=c(1,2,3,4,5),
                              freq = c(.2,.4,.1,.1,.2))

data_daily_na <- filter(data_daily, !is.na(pcp_ideam_flag))
cat_ind_plot <- function(data_df,daily_data,var,y_min,y_max,int,title,opt_value,second_y_title,second_size){
  #var <- 'POD'
  
  freq_int_1 <- dim(filter(daily_data,pcp_ideam_flag<1))[1]/dim(daily_data)[1]
  freq_int_2 <- dim(filter(daily_data,pcp_ideam_flag>=1 & pcp_ideam_flag<5))[1]/dim(daily_data)[1]
  freq_int_3 <- dim(filter(daily_data,pcp_ideam_flag>=5 & pcp_ideam_flag<20))[1]/dim(daily_data)[1]
  freq_int_4 <- dim(filter(daily_data,pcp_ideam_flag>=20 & pcp_ideam_flag<40))[1]/dim(daily_data)[1]
  freq_int_5 <- dim(filter(daily_data,pcp_ideam_flag>=40))[1]/dim(daily_data)[1]
  
  
  # Load pcp frequency
  bar_data <- data.frame(
    intensity = c(1, 2, 3, 4, 5),
    count = c(freq_int_1, freq_int_2, freq_int_3, freq_int_4, freq_int_5))
  
  # Normalize the count to fit on the same plot
  bar_data <- bar_data %>% mutate(scaled_count = count / max(count) * (y_max - y_min) + y_min)

  metrics_columns <- c(
    paste0(var,"v2_i1"), paste0(var,"v2_i2"), paste0(var,"v2_i3"), paste0(var,"v2_i4"), paste0(var,"v2_i5"), 
    paste0(var,"v3_era5_i1"), paste0(var,"v3_era5_i2"), paste0(var,"v3_era5_i3"), paste0(var,"v3_era5_i4"),
    paste0(var,"v3_era5_i5"),paste0(var,"v3_imerg_i1"), paste0(var,"v3_imerg_i2"), paste0(var,"v3_imerg_i3"),
    paste0(var,"v3_imerg_i4"), paste0(var,"v3_imerg_i5"))
  
  
  metrics_data <- data_df[, metrics_columns]
  metrics_long <- metrics_data %>%
    gather(key = "category_intensity", value = "value") %>%
    mutate(
      # Create 'metric', 'category', and 'intensity' columns from the column names
      metric = gsub("v[0-9]+_i[1-5]", "", category_intensity),  
      category = gsub("_i[1-5]", "", category_intensity),
      intensity = as.integer(gsub(".*_i([1-5])", "\\1", category_intensity))
    )
  
  # Calculate the median for each combination of metric, category, and intensity
  metrics_median <- metrics_long %>%
    group_by(metric, category, intensity) %>%
    summarise(median_value = median(value, na.rm = TRUE))
  
  metrics_median <- metrics_median %>%
    filter(category == paste0(var,"v2") | category == paste0(var,"v3_era5") | category == paste0(var,"v3_imerg"))
  
  
p1 <-  ggplot() +
    geom_col(data = bar_data, aes(x = intensity, y = scaled_count), 
             fill = "#d95f02", width = 0.5, alpha = 0.2) +
    geom_line(data = metrics_median,aes(x = intensity, y = median_value, color = category, group = category)) + 
    geom_point(data = metrics_median,aes(x = intensity, y = median_value, color = category, 
                                         shape=category,size = .1)) +
    labs(
      title = title,
      x = "Precipitation Intensity (mm/day)",
      y = "",
      color = "Version",
      shape= "version"
    ) +
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = .5) +
    theme_classic() +
    # Axis labels and color
    theme_classic() +
    
    # Primary axis (metrics)
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = seq(y_min, y_max, int),
      sec.axis = sec_axis(
        trans = ~ (. - y_min) / (y_max - y_min) * max(bar_data$count),
        name = second_y_title,
      )) +
    
    # X-axis setup
    scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
      labels = c("[0,1)", "[1,5)", "[5,20)", "[20,40)", paste0("\u2265", "40"))
    ) +
    
    # Custom color
    scale_color_manual(
      values = setNames(c("#7a3d8d", "#9ec6bc", "#3d8d52"), unique(metrics_median$category)),
      labels = c("v2", "v3-ERA5", "v3-IMERG")
    ) +
    scale_shape_manual(
      values = setNames(c(17, 15, 16), unique(metrics_median$category)))+
    labs(
      title = title,
      x = "Precipitation Intensity (mm/day)",
      y = "",
      color = "Version",
      shape="Version"
    )+
  scale_shape_manual(
    values = setNames(c(17, 15, 16), unique(metrics_median$category)),  # ejemplo
    labels = c("v2", "v3-ERA5", "v3-IMERG"))+
    guides(size = "none",
           color = guide_legend(
             override.aes = list(size = 4)))+
    # Styling
    theme(
      plot.margin = unit(c(.25, .01, .01, .10), "cm"),
      plot.title = element_text(size = 12, face = 'bold'),
      legend.title = element_text(size = 11, vjust = -0.1, hjust = 0, color = 'black'),
      legend.text = element_text(size = 10, color = 'black'),
      legend.position = c(.78, .7),
      legend.title.align = 1,
      axis.title.y.left = element_text(size = 12, color = "black"),
      axis.text.y.left  = element_text(size = 12, color = "black"),
      # Secondary y-axis styling (right side)
      axis.title.y.right = element_text(size = second_size, color = "#d95f02"),
      axis.text.y.right  = element_text(size = second_size, color = "#d95f02"),
      axis.title.x = element_text(size = 12, color = "black"),
      axis.text.x  = element_text(size = 11, color = "black"),
      legend.box.spacing = unit(-.001, "pt"),
      legend.margin = margin(.001, 0.03, 0., 0.0025),
      legend.box.margin = margin(0, 0, 0, 0)
    )
    
print(p1)    
  
}


POD_plot   <- cat_ind_plot(cat_daily_df,data_daily_na,"POD",0,1,.25,"Probability of detection (POD)",1,"",.01)
ETS_plot   <- cat_ind_plot(cat_daily_df,data_daily_na,"ETS",0,1,.25,"Equitable threat score (ETS)",1,"",.01)
FAR_plot   <- cat_ind_plot(cat_daily_df,data_daily_na,"FAR",0,1,.25,"False alarm ratio (FAR)",0,"",.01)
fBIAS_plot <- cat_ind_plot(cat_daily_df,data_daily_na,"fBIAS",0,3,.5,"Frequency bias (fBIAS)",1,"Events frequency",12)
HK_plot    <- cat_ind_plot(cat_daily_df,data_daily_na,"HK_",0,1,.25,"Hansen-Kuipers discriminant (HK)",1,"",.01)


#///////////////////////////////////////////////////
png(paste("G:/My Drive/R4C_et_al/3_PLOTS", "Fig_performance_daily.png",
          sep = '/'), units = "in",width = 14, height =5.5, res = 600, pointsize = 11)#, bg = "transparent")

B_day_2001   <- B_day_2001 + theme(legend.position = "none")
G_day_2001   <- G_day_2001 + theme(legend.position = "none")
kge_day_2001 <- kge_day_2001 + theme(legend.position = "none")

fBIAS_plot <- fBIAS_plot + theme(legend.position = "none")
ETS_plot   <- ETS_plot+ theme(legend.position = "none")
FAR_plot   <- FAR_plot + theme(legend.position = "none")
HK_plot    <- HK_plot+ theme(legend.position = "none")

# Modify the labels' position to the right
r_day_2001   <- r_day_2001 + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
POD_plot     <- POD_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
B_day_2001   <- B_day_2001 + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
FAR_plot     <- FAR_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
G_day_2001   <- G_day_2001 + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
ETS_plot     <- ETS_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
kge_day_2001 <- kge_day_2001 + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
fBIAS_plot   <- fBIAS_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
HK_plot      <- HK_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin


ggarrange(ggarrange(r_day_2001,POD_plot,ncol = 1, nrow = 2, align = "v",
                    labels = c("a", "e"),label.x = 0.05),
          ggarrange(B_day_2001,FAR_plot,ncol = 1, nrow = 2, align = "v",
                    labels = c("b", "f"),label.x = 0.05),
          ggarrange(G_day_2001,HK_plot,ncol = 1, nrow = 2, align = "v",
                    labels = c("c", "g"),label.x = 0.05),
          ggarrange(kge_day_2001,fBIAS_plot,ncol = 1, nrow = 2, align = "v",
                    labels = c("d", "h"),label.x = 0.05),
          ncol=4,widths = c(3,3,3,3.2))

dev.off()





#___________________________________________________________

cat_daily_scale_df <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                "/res_performance_daily_categorical_indices_2001_2023_df_scale_v2.csv"),head=TRUE))


# Wind-corrected and Uncorrected plot function
cat_ind_plot_correction <- function(data_df_cor, data_df_uncor, daily_data, var,
                                    y_min, y_max, int, title, opt_value,
                                    second_y_title, second_size) {
  
  # === 1. Compute precipitation frequency for each intensity bin ===
  freq_int_1 <- mean(daily_data$pcp_ideam_flag < 1)
  freq_int_2 <- mean(daily_data$pcp_ideam_flag >= 1 & daily_data$pcp_ideam_flag < 5)
  freq_int_3 <- mean(daily_data$pcp_ideam_flag >= 5 & daily_data$pcp_ideam_flag < 20)
  freq_int_4 <- mean(daily_data$pcp_ideam_flag >= 20 & daily_data$pcp_ideam_flag < 40)
  freq_int_5 <- mean(daily_data$pcp_ideam_flag >= 40)
  
  bar_data <- data.frame(
    intensity = 1:5,
    count = c(freq_int_1, freq_int_2, freq_int_3, freq_int_4, freq_int_5)) %>%
    mutate(scaled_count = count / max(count) * (y_max - y_min) + y_min)
  
  # === 2. Gather metrics ===
  categories <- c("v2", "v3_era5", "v3_imerg")
  categories2 <- c("v3_era5", "v3_imerg")
  
  metrics_columns <- unlist(lapply(categories, function(cat) paste0(var, cat, "_i", 1:5)))
  metrics_columns2 <- unlist(lapply(categories2, function(cat) paste0(var, cat, "_i", 1:5)))
  
  metrics_data     <- data_df_uncor[, metrics_columns]
  metrics_data_cor <- data_df_cor[, metrics_columns2]
  
  long_data <- function(df) {
    df %>%
      tidyr::gather("category_intensity", "value") %>%
      mutate(
        metric    = gsub("v[0-9]+_i[1-5]", "", category_intensity),
        category  = gsub("_i[1-5]", "", category_intensity),
        intensity = as.integer(gsub(".*_i([1-5])", "\\1", category_intensity)))
  }
  
  metrics_long     <- long_data(metrics_data)     %>% mutate(cor = "Uncorrected")
  metrics_long_cor <- long_data(metrics_data_cor) %>% mutate(cor = "Wind-corrected")
  
  # === 3. Compute medians and label ===
  metrics_median_df <- bind_rows(metrics_long, metrics_long_cor) %>%
    group_by(metric, category, intensity, cor) %>%
    summarise(median_value = median(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      interaction_cat_cor = interaction(category, cor, drop = TRUE),
      interaction_cat_cor = forcats::fct_recode(interaction_cat_cor,
                                                "CHIRPSv2" = paste0(var, "v2.Uncorrected"),
                                                "v3-ERA5" = paste0(var, "v3_era5.Uncorrected"),
                                                "v3-ERA5 (Corrected)" = paste0(var, "v3_era5.Wind-corrected"),
                                                "v3-IMERG" = paste0(var, "v3_imerg.Uncorrected"),
                                                "v3-IMERG (Corrected)" = paste0(var, "v3_imerg.Wind-corrected")
      )
    )
  
  # === 4. Define shapes and colors ===
  legend_levels <- levels(metrics_median_df$interaction_cat_cor)
  
  # Asegurarse de que la longitud coincida
  shape_vals <- c(17, 15, 16, 20, 22)[seq_along(legend_levels)]
  color_vals <- c("#7a3d8d", "#9ec6bc", "#3d8d52", "#e7298a", "#e78ac3")[seq_along(legend_levels)]
  
  names(shape_vals) <- legend_levels
  names(color_vals) <- legend_levels
  
  # === 5. Plot ===
  ggplot() +
    geom_col(data = bar_data, aes(x = intensity, y = scaled_count),
             fill = "#d95f02", width = 0.5, alpha = 0.2) +
    geom_line(data = metrics_median_df,
              aes(x = intensity, y = median_value,
                  color = interaction_cat_cor,
                  group = interaction(category, cor),
                  linetype = cor), size = 1) +
    geom_point(data = metrics_median_df,
               aes(x = intensity, y = median_value,
                   color = interaction_cat_cor,
                   shape = interaction_cat_cor),
               size = 3.5) +
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = 0.5) +
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = seq(y_min, y_max, int),
      sec.axis = sec_axis(
        trans = ~ (. - y_min) / (y_max - y_min) * max(bar_data$count),
        name = second_y_title
      )
    ) +
    scale_x_continuous(
      breaks = 1:5,
      labels = c("[0,1)", "[1,5)", "[5,20)", "[20,40)", "\u2265 40")
    ) +
    scale_color_manual(name = "Product", values = color_vals) +
    scale_shape_manual(name = "Product", values = shape_vals) +
    scale_linetype_manual(name = "Correction", values = c("Uncorrected" = "solid", "Wind-corrected" = "dashed")) +
    labs(
      title = title,
      x = "Precipitation Intensity (mm/day)",
      y = NULL
    ) +
    theme_classic() +
    theme(
      plot.margin = unit(c(0.25, 0.01, 0.01, 0.10), "cm"),
      plot.title = element_text(size = 12, face = 'bold', color = 'black'),
      legend.title = element_text(size = 11, color = 'black'),
      legend.text = element_text(size = 10, color = 'black'),
      legend.position = c(.82,.65),
      axis.title.y.left = element_text(size = 12, color = 'black'),
      axis.text.y.left = element_text(size = 12, color = 'black'),
      axis.title.y.right = element_text(size = second_size, color = "#d95f02"),
      axis.text.y.right = element_text(size = second_size, color = "#d95f02"),
      axis.title.x = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 11, color = 'black'),
      legend.box.spacing = unit(-0.001, "pt"),
      legend.margin = margin(0.001, 0.03, 0, 0.0025),
      legend.box.margin = margin(0, 0, 0, 0)
    )
}

POD_plot_s   <- cat_ind_plot_correction(cat_daily_scale_df,cat_daily_df,data_daily_na,
                                        "POD",0,1,.25,"Probability of detection (POD)",1,
                                        "",.01)
ETS_plot_s   <- cat_ind_plot_correction(cat_daily_scale_df,cat_daily_df,data_daily_na,
                                        "ETS",0,1,.25,"Equitable threat score (ETS)",1,
                                        "",.012)
FAR_plot_s   <- cat_ind_plot_correction(cat_daily_scale_df,cat_daily_df,data_daily_na,
                                        "FAR",0,1,.25,"False alarm ratio (FAR)",0,
                                        "Events frequency",12)
fBIAS_plot_s <- cat_ind_plot_correction(cat_daily_scale_df,cat_daily_df,data_daily_na,
                                        "fBIAS",0,3,.5,"Frequency bias (fBIAS)",1,
                                        "Events frequency",12)
HK_plot_s   <- cat_ind_plot_correction(cat_daily_scale_df,cat_daily_df,data_daily_na,
                                        "HK",0,1,.25,"Hansen-Kuipers discrimination (HK)",1,
                                        "",.012)

png(paste("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS",
          "Fig_performance_daily_wind_correction.png",sep = '/'),
          units = "in",width = 12, height =8, res = 600, pointsize = 11)


fBIAS_plot <- fBIAS_plot_s + theme(legend.position = "none")
ETS_plot   <- ETS_plot_s + theme(legend.position = "none")
FAR_plot   <- FAR_plot_s + theme(legend.position = "none")
HK_plot    <- HK_plot_s + theme(legend.position = "none")


# Modify the labels' position to the right
POD_plot <- POD_plot_s + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
FAR_plot <- FAR_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
HK_plot  <- HK_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
fBIAS_plot <- fBIAS_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin

ggarrange(POD_plot,FAR_plot,HK_plot,fBIAS_plot,
          ncol = 2, nrow = 2, align = "hv",
          labels = c("", "","",""),label.x = 0.)

dev.off()


#______________________________________________________________________
# Dry and wet seasons

cat_ind_plot_seasons <- function(data_df,daily_data,var,y_min,
                         y_max,int,title,opt_value,second_y_title,
                         second_size){
  #var <- 'POD'
  
  freq_int_1 <- dim(filter(daily_data,pcp_ideam_flag<1))[1]/dim(daily_data)[1]
  freq_int_2 <- dim(filter(daily_data,pcp_ideam_flag>=1 & pcp_ideam_flag<5))[1]/dim(daily_data)[1]
  freq_int_3 <- dim(filter(daily_data,pcp_ideam_flag>=5 & pcp_ideam_flag<20))[1]/dim(daily_data)[1]
  freq_int_4 <- dim(filter(daily_data,pcp_ideam_flag>=20 & pcp_ideam_flag<40))[1]/dim(daily_data)[1]
  freq_int_5 <- dim(filter(daily_data,pcp_ideam_flag>=40))[1]/dim(daily_data)[1]
  
  
  # Load pcp frequency
  bar_data <- data.frame(
    intensity = c(1, 2, 3, 4, 5),
    count = c(freq_int_1, freq_int_2, freq_int_3, freq_int_4, freq_int_5))
  
  # Normalize the count to fit on the same plot
  bar_data <- bar_data %>% mutate(scaled_count = count / max(count) * (y_max - y_min) + y_min)
  
  metrics_columns <- c(
    paste0(var,"v2_i1"), paste0(var,"v2_i2"), paste0(var,"v2_i3"), paste0(var,"v2_i4"), paste0(var,"v2_i5"), 
    paste0(var,"v3_era5_i1"), paste0(var,"v3_era5_i2"), paste0(var,"v3_era5_i3"), paste0(var,"v3_era5_i4"),
    paste0(var,"v3_era5_i5"),paste0(var,"v3_imerg_i1"), paste0(var,"v3_imerg_i2"), paste0(var,"v3_imerg_i3"),
    paste0(var,"v3_imerg_i4"), paste0(var,"v3_imerg_i5"))
  
  
  metrics_data <- data_df[, metrics_columns]
  metrics_long <- metrics_data %>%
    gather(key = "category_intensity", value = "value") %>%
    mutate(
      # Create 'metric', 'category', and 'intensity' columns from the column names
      metric = gsub("v[0-9]+_i[1-5]", "", category_intensity),  
      category = gsub("_i[1-5]", "", category_intensity),
      intensity = as.integer(gsub(".*_i([1-5])", "\\1", category_intensity))
    )
  
  # Calculate the median for each combination of metric, category, and intensity
  metrics_median <- metrics_long %>%
    group_by(metric, category, intensity) %>%
    summarise(median_value = median(value, na.rm = TRUE))
  
  metrics_median <- metrics_median %>%
    filter(category == paste0(var,"v2") | category == paste0(var,"v3_era5") | category == paste0(var,"v3_imerg"))
  
  
  p1 <-  ggplot() +
    #geom_col(data = bar_data, aes(x = intensity, y = scaled_count), 
    #         fill = "#d95f02", width = 0.5, alpha = 0.2) +
    geom_line(data = metrics_median,aes(x = intensity, y = median_value, color = category, group = category)) + 
    geom_point(data = metrics_median,aes(x = intensity, y = median_value, color = category, 
                                         shape=category,size = .1)) +
    labs(
      title = title,
      x = "Precipitation Intensity (mm/day)",
      y = "",
      color = "Version",
      shape= "version"
    ) +
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = .5) +
    theme_classic() +
    # Axis labels and color
    theme_classic() +
    
    # Primary axis (metrics)
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = seq(y_min, y_max, int)
      #sec.axis = sec_axis(
      #  trans = ~ (. - y_min) / (y_max - y_min) * max(bar_data$count),
      #  name = second_y_title,
      ) +
    
    # X-axis setup
    scale_x_continuous(
      breaks = c(1, 2, 3, 4, 5),
      labels = c("[0,1)", "[1,5)", "[5,20)", "[20,40)", paste0("\u2265", "40"))
    ) +
    
    # Custom color
    scale_color_manual(
      values = setNames(c("#7a3d8d", "#9ec6bc", "#3d8d52"), unique(metrics_median$category)),
      labels = c("v2", "v3-ERA5", "v3-IMERG")
    ) +
    scale_shape_manual(
      values = setNames(c(17, 15, 16), unique(metrics_median$category)))+
    labs(
      title = title,
      x = "Precipitation Intensity (mm/day)",
      y = "",
      color = "Version",
      shape="Version"
    )+
    scale_shape_manual(
      values = setNames(c(17, 15, 16), unique(metrics_median$category)),  # ejemplo
      labels = c("v2", "v3-ERA5", "v3-IMERG"))+
    guides(size = "none",
           color = guide_legend(
             override.aes = list(size = 4)))+
    # Styling
    theme(
      plot.margin = unit(c(.25, .01, .01, .10), "cm"),
      plot.title = element_text(size = 12, face = 'bold'),
      legend.title = element_text(size = 11, vjust = -0.1, hjust = 0, color = 'black'),
      legend.text = element_text(size = 10, color = 'black'),
      legend.position = c(.78, .7),
      legend.title.align = 1,
      axis.title.y.left = element_text(size = 12, color = "black"),
      axis.text.y.left  = element_text(size = 12, color = "black"),
      # Secondary y-axis styling (right side)
      axis.title.y.right = element_text(size = second_size, color = "#d95f02"),
      axis.text.y.right  = element_text(size = second_size, color = "#d95f02"),
      axis.title.x = element_text(size = 12, color = "black"),
      axis.text.x  = element_text(size = 11, color = "black"),
      legend.box.spacing = unit(-.001, "pt"),
      legend.margin = margin(.001, 0.03, 0., 0.0025),
      legend.box.margin = margin(0, 0, 0, 0)
    )
  
  print(p1)    
  
}


cat_daily_dry_season_df <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                      "/res_performance_daily_categorical_indices_2001_2023_dry_season_df_v2.csv"),head=TRUE))
cat_daily_wet_season_df <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/" ,
                      "/res_performance_daily_categorical_indices_2001_2023_wet_season_df_v2.csv"),head=TRUE))

#cat_daily_dry_season_df <- filter(cat_daily_dry_season_df,nat_region=='Orinoquia')
#cat_daily_wet_season_df <- filter(cat_daily_wet_season_df,nat_region=='Orinoquia')
POD_plot_dry   <- cat_ind_plot_seasons(cat_daily_dry_season_df,data_daily_na,"POD",
                           0,1,.25,"Probability of detection (POD)",1,"",.01)
ETS_plot_dry   <- cat_ind_plot_seasons(cat_daily_dry_season_df,data_daily_na,"ETS",
                           0,1,.25,"Equitable threat score (ETS)",1,"",.01)
FAR_plot_dry   <- cat_ind_plot_seasons(cat_daily_dry_season_df,data_daily_na,"FAR",
                           0,1,.25,"False alarm ratio (FAR)",0,"",.01)
fBIAS_plot_dry <- cat_ind_plot_seasons(cat_daily_dry_season_df,data_daily_na,"fBIAS",
                           0,5,1,"Frequency bias (fBIAS)",1,"Events frequency",12)
HK_plot_dry   <- cat_ind_plot_seasons(cat_daily_dry_season_df,data_daily_na,"HK_",
                                       0,1,.25,"Hansen-Kuipers discriminant (HK)",1,"",.01)



POD_plot_wet   <- cat_ind_plot_seasons(cat_daily_wet_season_df,data_daily_na,"POD",
                          0,1,.25,"Probability of detection (POD)",1,"",.01)
ETS_plot_wet   <- cat_ind_plot_seasons(cat_daily_wet_season_df,data_daily_na,"ETS",
                          0,1,.25,"Equitable threat score (ETS)",1,"",.01)
FAR_plot_wet   <- cat_ind_plot_seasons(cat_daily_wet_season_df,data_daily_na,"FAR",
                          0,1,.25,"False alarm ratio (FAR)",0,"",.01)
fBIAS_plot_wet <- cat_ind_plot_seasons(cat_daily_wet_season_df,data_daily_na,"fBIAS",
                          0,4,1,"Frequency bias (fBIAS)",1,"Events frequency",12)
HK_plot_wet   <- cat_ind_plot_seasons(cat_daily_wet_season_df,data_daily_na,"HK_",
                                      0,1,.25,"Hansen-Kuipers discriminant (HK)",1,"",.01)



POD_plot_wet <- POD_plot_wet + theme(legend.position = "none") +
                   theme(plot.margin = margin(10, 10, 0, 0))
POD_plot_dry <- POD_plot_dry + #theme(legend.position = "none") +
                   theme(plot.margin = margin(10, 10, 0, 0))
ETS_plot_wet <- ETS_plot_wet + theme(legend.position = "none") +
                       theme(plot.margin = margin(10, 10, 0, 0))
ETS_plot_dry <- ETS_plot_dry + theme(legend.position = "none") +
                       theme(plot.margin = margin(10, 10, 0, 0))
HK_plot_wet  <- HK_plot_wet + theme(legend.position = "none") +
                       theme(plot.margin = margin(10, 10, 0, 0))
HK_plot_dry  <- HK_plot_dry + theme(legend.position = "none") +
                       theme(plot.margin = margin(10, 10, 0, 0))
FAR_plot_wet <- FAR_plot_wet + theme(legend.position = "none") +
                           theme(plot.margin = margin(10, 10, 0, 0))
FAR_plot_dry <- FAR_plot_dry + theme(legend.position = "none") +
                          theme(plot.margin = margin(10, 10, 0, 0))
fBIAS_plot_wet <- fBIAS_plot_wet + theme(legend.position = "none") +
                           theme(plot.margin = margin(10, 10, 0, 0))
fBIAS_plot_dry <- fBIAS_plot_dry + theme(legend.position = "none") +
                           theme(plot.margin = margin(10, 10, 0, 0))


dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_cat_ind_daily_dry_wet_season.png",
          sep = '/'), units = "in",width =13.5, height = 5.5, 
    res = 600, pointsize = 11)#, bg = "transparent")


p <- ggarrange(POD_plot_dry,FAR_plot_dry,HK_plot_dry,fBIAS_plot_dry,
          POD_plot_wet,FAR_plot_wet,HK_plot_wet,fBIAS_plot_wet,
          ncol=4,nrow=2,widths = c(3.2,3.2,3.2,3.0),
          label.x=c(0.05,0.05,0.05,0),label.y=0.98,
          labels=c('a','b','c','d','e','f','g','h'))

p <- p +   theme(plot.margin = unit(c(.01, .5, .01, .01), "cm"))+ 
            draw_plot_label(label = c("Dry season","Wet season"),
              size = 14,x = c(.99),  y = c(0.64, 0.13),  angle = 90)  
print(p)

dev.off()



#/////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////
# regional analysis

# Daily
regions <- c("Pacifico","Andes","Caribe","Amazonia","Orinoquia")
#for(kk in 1:length(regions)){

fbias_low  <- 0
fbias_high <- 9
fbias_int  <- 1
for(kk in c(3)){
#kk <- 3
regions_i <- regions[[kk]]

r_day_2001 <- performance_plot_function(res_daily_data ,
                                        c("r_chirps_v2",# "r_chirp_v2", "r_chirp_v3",
                                          "r_chirps_v3_era5","r_chirps_v3_imerg"),
                                        'Correlation coefficient (r)',-0.75,1,.25,1,
                                        regions_i)

r_day_2001 <- r_day_2001  + 
  theme(
    legend.position = c(0.25, 0.75),  
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10,color='black'))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))
r_day_2001


r_day_2001 <- r_day_2001 + theme(axis.text.y = element_blank(),axis.title.y = element_blank())
r_day_2001 <- r_day_2001 + scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.00),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.00"))


B_day_2001 <- performance_plot_function(res_daily_data,
                                        c("B_chirps_v2",# "B_chirp_v2","B_chirp_v3",
                                          "B_chirps_v3_era5","B_chirps_v3_imerg"),
                                        paste0("Bias ratio (","\u03B2",")"),-0,2,.5,1,
                                        regions_i)
B_day_2001 <- B_day_2001 + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


G_day_2001 <- performance_plot_function(res_daily_data,
                                        c("G_chirps_v2",#"G_chirp_v2","G_chirp_v3",
                                          "G_chirps_v3_era5","G_chirps_v3_imerg"),
                                        paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5,1,
                                        regions_i)
G_day_2001 <- G_day_2001 + theme(axis.text.y = element_blank(),axis.title.y = element_blank())


kge_day_2001 <- performance_plot_function(res_daily_data,
                                          c("kge_chirps_v2",#"kge_chirp_v2","kge_chirp_v3",
                                            "kge_chirps_v3_era5","kge_chirps_v3_imerg"),
                                          'Kling-Gupta Efficiency (KGE)',-0.75,1,.25,1,
                                          regions_i)
kge_day_2001 <- kge_day_2001 + theme(axis.text.y = element_blank(),axis.title.y = element_blank())

kge_day_2001 <- kge_day_2001 + scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.00),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.00"))

#///////////////////////////////////////////////////////////////////////////////
# Categorical indices
cat_daily_i  <- filter(cat_daily_df,nat_region==regions_i)
daily_data_i <- filter(data_daily_na,nat_region==regions_i)
POD_plot   <- cat_ind_plot(cat_daily_i,daily_data_i,"POD",0,1,.25,"Probability of detection (POD)",1,"",.01)
ETS_plot   <- cat_ind_plot(cat_daily_i,daily_data_i,"ETS",0,1,.25,"Equitable threat score (ETS)",1,"",.01)
FAR_plot   <- cat_ind_plot(cat_daily_i,daily_data_i,"FAR",0,1,.25,"False alarm ratio (FAR)",0,"",.01)
fBIAS_plot <- cat_ind_plot(cat_daily_i,daily_data_i,"fBIAS",fbias_low,fbias_high,fbias_int,
                           "Frequency bias (fBIAS)",1,"Events frequency",12)

#///////////////////////////////////////////////////
png(paste("G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS", paste0("Fig_performance_daily_",regions_i,".png"),
          sep = '/'), units = "in",width = 13, height =5.5, 
          res = 600, pointsize = 11)#, bg = "transparent")

B_day_2001 <- B_day_2001 + theme(legend.position = "none")
G_day_2001 <- G_day_2001 + theme(legend.position = "none")
kge_day_2001 <- kge_day_2001 + theme(legend.position = "none")

fBIAS_plot <- fBIAS_plot + theme(legend.position = "none")
ETS_plot <- ETS_plot+ theme(legend.position = "none")
FAR_plot <- FAR_plot + theme(legend.position = "none")

# Modify the labels' position to the right
r_day_2001 <- r_day_2001 + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
POD_plot <- POD_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
B_day_2001 <- B_day_2001 + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
FAR_plot <- FAR_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
G_day_2001 <- G_day_2001 + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
ETS_plot <- ETS_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
kge_day_2001 <- kge_day_2001 + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin
fBIAS_plot <- fBIAS_plot + theme(plot.margin = margin(10, 10, 0, 0))  # Increase right margin


p <- ggarrange(ggarrange(r_day_2001,POD_plot,ncol = 1, nrow = 2, align = "v",labels = c("a", "e"),label.x = 0),
          ggarrange(B_day_2001,FAR_plot,ncol = 1, nrow = 2, align = "v",labels = c("b", "f"),label.x = 0),
          ggarrange(G_day_2001,ETS_plot,ncol = 1, nrow = 2, align = "v",labels = c("c", "g"),label.x = .0),
          ggarrange(kge_day_2001,fBIAS_plot,ncol = 1, nrow = 2, align = "v",labels = c("d", "h"),label.x = .0),
          ncol=4,widths = c(3,3,3,3.15))


print(p)
dev.off()

}




#//////////////////////////////////////////////////////////////////////////////
# fBIAS maps

col_gauges <- c('#FFEC9DFF', '#FAC881FF', '#F4A464FF', '#E87444FF', '#D9402AFF',
                '#BF2729FF', '#912534FF', '#64243EFF', '#3D1B28FF', '#161212FF')


col_gauges2 <- c('#26322CFF', '#FFD480FF', '#E09743FF', '#D25A3EFF', '#D42538FF', '#A6162BFF')


col_gauges3 <- c('#26322CFF', '#384D3FFF', '#777768FF', '#A6B2A5FF',
                 '#FFD480FF', '#E09743FF', '#D25A3EFF', '#D42538FF', '#A6162BFF')

col_gauges4 <- c('#26322CFF', '#A6B2A5FF',
                 '#FFD480FF', '#E09743FF', '#D25A3EFF', '#D42538FF', '#A6162BFF')



chirpsv3_era5_i2 <-    ggplot() +
                  geom_sf(data = nat_reg_shp, 
                          color = adjustcolor("black", alpha.f = 0.25), 
                          fill = NA, size = 0.01) +
                  geom_point(data = cat_daily_df, 
                             aes(x = longitude, 
                                 y = latitude, 
                                 color = fBIASv3_era5_i2)) +
                  scale_color_stepsn(
                    name = "", 
                    colors = col_gauges2,
                    limits = c(0, 10),
                    breaks = seq(0, 10, 1),
                    labels = as.character(0:10),
                    guide = guide_colorbar(
                      title = 'fBIAS',
                      title.position = "top", 
                      label.position = "bottom",
                      direction = "horizontal",
                      barheight = unit(.4, "cm"),
                      barwidth = unit(11, "cm"))) +
                  theme_minimal() +
                  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
                  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
                  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
                  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
                        plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
                        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
                        legend.text = element_text(size = 11,color = "black"),
                        axis.text  =element_text(size=12,color = "black"),
                        legend.position = "bottom",
                        legend.title.align = 1,
                        legend.box.spacing = unit(0, "pt")) +
                  labs(x="",y="",title="CHIRPSv3-ERA5")


chirpsv3_era5_i1 <-    ggplot() +
  geom_sf(data = nat_reg_shp, 
          color = adjustcolor("black", alpha.f = 0.25), 
          fill = NA, size = 0.01) +
  geom_point(data = cat_daily_df, 
             aes(x = longitude, 
                 y = latitude, 
                 color = fBIASv3_era5_i1)) +
  scale_color_stepsn(
    name = "", 
    colors = col_gauges3,
    limits = c(0, 2),
    breaks = seq(0, 2, .25),
    #labels = as.character(0:3),
    guide = guide_colorbar(
      title = 'fBIAS',
      title.position = "top", 
      label.position = "bottom",
      direction = "horizontal",
      barheight = unit(.4, "cm"),
      barwidth = unit(11, "cm"))) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
        legend.text = element_text(size = 11,color = "black"),
        axis.text  =element_text(size=12,color = "black"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title="CHIRPSv3-ERA5")



chirpsv3_era5_i3 <-    ggplot() +
  geom_sf(data = nat_reg_shp, 
          color = adjustcolor("black", alpha.f = 0.25), 
          fill = NA, size = 0.01) +
  geom_point(data = cat_daily_df, 
             aes(x = longitude, 
                 y = latitude, 
                 color = fBIASv3_era5_i3)) +
  scale_color_stepsn(
    name = "", 
    colors = col_gauges4,
    limits = c(0, 5),
    breaks = seq(0, 5 ,.5),
    #labels = as.character(0:3),
    guide = guide_colorbar(
      title = 'fBIAS',
      title.position = "top", 
      label.position = "bottom",
      direction = "horizontal",
      barheight = unit(.4, "cm"),
      barwidth = unit(11, "cm"))) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
        legend.text = element_text(size = 11,color = "black"),
        axis.text  =element_text(size=12,color = "black"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title="CHIRPSv3-ERA5")



chirpsv3_imerg_i2 <-    ggplot() +
                  geom_sf(data = nat_reg_shp, 
                          color = adjustcolor("black", alpha.f = 0.25), 
                          fill = NA, size = 0.01) +
                  geom_point(data = cat_daily_df, 
                             aes(x = longitude, 
                                 y = latitude, 
                                 color = fBIASv3_imerg_i2)) +
                  scale_color_stepsn(
                    name = "", 
                    colors = col_gauges2,
                    limits = c(0, 10),
                    breaks = seq(0, 10, 1),
                    labels = as.character(0:10),
                    guide = guide_colorbar(
                      title = 'fBIAS',
                      title.position = "top", 
                      label.position = "bottom",
                      direction = "horizontal",
                      barheight = unit(.4, "cm"),
                      barwidth = unit(11, "cm"))) +
                  theme_minimal() +
                  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
                  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
                  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
                  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
                        plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
                        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
                        legend.text = element_text(size = 11,color = "black"),
                        axis.text  =element_text(size=12,color = "black"),
                        legend.position = "bottom",
                        legend.title.align = 1,
                        legend.box.spacing = unit(0, "pt")) +
                  labs(x="",y="",title="CHIRPSv3-IMERG")



chirpsv3_imerg_i3 <-    ggplot() +
  geom_sf(data = nat_reg_shp, 
          color = adjustcolor("black", alpha.f = 0.25), 
          fill = NA, size = 0.01) +
  geom_point(data = cat_daily_df, 
             aes(x = longitude, 
                 y = latitude, 
                 color = fBIASv3_imerg_i3)) +
  scale_color_stepsn(
    name = "", 
    colors = col_gauges4,
    limits = c(0, 5),
    breaks = seq(0, 5, .5),
    #labels = as.character(0:5),
    guide = guide_colorbar(
      title = 'fBIAS',
      title.position = "top", 
      label.position = "bottom",
      direction = "horizontal",
      barheight = unit(.4, "cm"),
      barwidth = unit(11, "cm"))) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
        legend.text = element_text(size = 11,color = "black"),
        axis.text  =element_text(size=12,color = "black"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title="CHIRPSv3-IMERG")



chirpsv3_imerg_i1 <-    ggplot() +
  geom_sf(data = nat_reg_shp, 
          color = adjustcolor("black", alpha.f = 0.25), 
          fill = NA, size = 0.01) +
  geom_point(data = cat_daily_df, 
             aes(x = longitude, 
                 y = latitude, 
                 color = fBIASv3_imerg_i1)) +
  scale_color_stepsn(
    name = "", 
    colors = col_gauges3,
    limits = c(0, 2),
    breaks = seq(0, 2, .25),
    #labels = as.character(0:5),
    guide = guide_colorbar(
      title = 'fBIAS',
      title.position = "top", 
      label.position = "bottom",
      direction = "horizontal",
      barheight = unit(.4, "cm"),
      barwidth = unit(11, "cm"))) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
        legend.text = element_text(size = 11,color = "black"),
        axis.text  =element_text(size=12,color = "black"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title="CHIRPSv3-IMERG")




chirpsv2_i2 <-    ggplot() +
                        geom_sf(data = nat_reg_shp, 
                                color = adjustcolor("black", alpha.f = 0.25), 
                                fill = NA, size = 0.01) +
                        geom_point(data = cat_daily_df, 
                                   aes(x = longitude, 
                                       y = latitude, 
                                       color = fBIASv2_i2)) +
                        scale_color_stepsn(
                          name = "", 
                          colors = col_gauges2,
                          limits = c(0, 10),
                          breaks = seq(0, 10, 1),
                          labels = as.character(0:10),
                          guide = guide_colorbar(
                            title = 'fBIAS - light precipitation events - [1,5) mm/day',
                            title.position = "top", 
                            label.position = "bottom",
                            direction = "horizontal",
                            barheight = unit(.4, "cm"),
                            barwidth = unit(11, "cm"))) +
                        theme_minimal() +
                        scale_x_continuous(breaks = seq(-80, -66, by = 4))+
                        #labels = c("78°W", "75°W", "72°W","69°W","X")) +
                        scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
                        theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
                              plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
                              legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
                              legend.text = element_text(size = 11,color = "black"),
                              axis.text  =element_text(size=12,color = "black"),
                              legend.position = "bottom",
                              legend.title.align = 1,
                              legend.box.spacing = unit(0, "pt")) +
                        labs(x="",y="",title="CHIRPSv2")





chirpsv2_i1 <-    ggplot() +
  geom_sf(data = nat_reg_shp, 
          color = adjustcolor("black", alpha.f = 0.25), 
          fill = NA, size = 0.01) +
  geom_point(data = cat_daily_df, 
             aes(x = longitude, 
                 y = latitude, 
                 color = fBIASv2_i1)) +
  scale_color_stepsn(
    name = "", 
    colors = col_gauges3,
    limits = c(0, 2),
    breaks = seq(0, 2, .25),
    #labels = as.character(0:10),
    guide = guide_colorbar(
      title = 'fBIAS - no-precipitation events - [0,1) mm/day',
      title.position = "top", 
      label.position = "bottom",
      direction = "horizontal",
      barheight = unit(.4, "cm"),
      barwidth = unit(11, "cm"))) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
        legend.text = element_text(size = 11,color = "black"),
        axis.text  =element_text(size=12,color = "black"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title="CHIRPSv2")




chirpsv2_i3 <-    ggplot() +
  geom_sf(data = nat_reg_shp, 
          color = adjustcolor("black", alpha.f = 0.25), 
          fill = NA, size = 0.01) +
  geom_point(data = cat_daily_df, 
             aes(x = longitude, 
                 y = latitude,
                 color = fBIASv2_i3)) +
  scale_color_stepsn(
    name = "", 
    colors = col_gauges4,
    limits = c(0, 5),
    breaks = seq(0, 5, .5),
    #labels = as.character(0:10),
    guide = guide_colorbar(
      title = 'fBIAS - moderate precipitation events - [5,20) mm/day',
      title.position = "top", 
      label.position = "bottom",
      direction = "horizontal",
      barheight = unit(.4, "cm"),
      barwidth = unit(11, "cm"))) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(-80, -66, by = 4))+
  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
        legend.text = element_text(size = 11,color = "black"),
        axis.text  =element_text(size=12,color = "black"),
        legend.position = "bottom",
        legend.title.align = 1,
        legend.box.spacing = unit(0, "pt")) +
  labs(x="",y="",title="CHIRPSv2")






library(cowplot)
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_fBIAS_Int_1_3.png",
          sep = '/'), units = "in",width =10, height = 15, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(
ggarrange(chirpsv2_i1,chirpsv3_imerg_i1,chirpsv3_era5_i1,
          align = "hv", labels = c("a","b","c"),
          font.label = list(size = 20, face = "bold"), 
          ncol=3,common.legend = TRUE,legend = "bottom"),
ggarrange(chirpsv2_i2,chirpsv3_imerg_i2,chirpsv3_era5_i2,
          align = "hv", labels = c("d","e","f"),
          font.label = list(size = 20, face = "bold"), 
          ncol=3,common.legend = TRUE,legend = "bottom"),
ggarrange(chirpsv2_i3,chirpsv3_imerg_i3,chirpsv3_era5_i3,
          align = "hv", labels = c("g","h","i"),
          font.label = list(size = 20, face = "bold"), 
          ncol=3,common.legend = TRUE,legend = "bottom"),
          ncol=1)

dev.off()






# KGE maps

col_gauges <- c('#693829FF', '#894B33FF', '#A56A3EFF', '#CFB267FF', '#D9C5B6FF', 
              '#9CA9BAFF', '#5480B5FF', '#3D619DFF', '#405A95FF', '#345084FF')

chirpsv3_kge <-    ggplot() +
                  geom_sf(data = nat_reg_shp, 
                          color = adjustcolor("black", alpha.f = 0.25), 
                          fill = NA, size = 0.01) +
                  geom_point(data = res_month_data, 
                             aes(x = longitude, 
                                 y = latitude, 
                                 color = kge_chirps_v3),size=1) +
                  scale_color_stepsn(
                    name = "", 
                    colors = col_gauges,
                    limits = c(-0, 1),
                    breaks = seq(-0, 1, .1),
                    labels = seq(-0, 1, .1),
                    guide = guide_colorbar(
                      title = 'Kling-Gupta Efficiency (KGE)',
                      title.position = "top", 
                      label.position = "bottom",
                      direction = "horizontal",
                      barheight = unit(.4, "cm"),
                      barwidth = unit(11, "cm"))) +
                  theme_minimal() +
                  scale_x_continuous(breaks = seq(-80, -66, by = 3))+
                  #labels = c("78°W", "75°W", "72°W","69°W","X")) +
                  scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
                  theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
                        plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
                        legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
                        legend.text = element_text(size = 11,color = "black"),
                        axis.text  =element_text(size=12,color = "black"),
                        legend.position = "bottom",
                        legend.title.align = 1,
                        legend.box.spacing = unit(0, "pt")) +
                  labs(x="",y="",title="CHIRPSv3")


chirpsv2_kge <-    ggplot() +
                    geom_sf(data = nat_reg_shp, 
                            color = adjustcolor("black", alpha.f = 0.25), 
                            fill = NA, size = 0.01) +
                    geom_point(data = res_month_data, 
                               aes(x = longitude, 
                                   y = latitude, 
                                   color = kge_chirps_v2),size=1) +
                    scale_color_stepsn(
                      name = "", 
                      colors = col_gauges,
                      limits = c(-0, 1),
                      breaks = seq(-0, 1, .1),
                      labels = seq(-0, 1, .1),
                      guide = guide_colorbar(
                        title = 'Kling-Gupta Efficiency (KGE)',
                        title.position = "top", 
                        label.position = "bottom",
                        direction = "horizontal",
                        barheight = unit(.4, "cm"),
                        barwidth = unit(11, "cm"))) +
                    theme_minimal() +
                    scale_x_continuous(breaks = seq(-80, -66, by = 3))+
                    #labels = c("78°W", "75°W", "72°W","69°W","X")) +
                    scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
                    theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
                          plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
                          legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
                          legend.text = element_text(size = 11,color = "black"),
                          axis.text  =element_text(size=12,color = "black"),
                          legend.position = "bottom",
                          legend.title.align = 1,
                          legend.box.spacing = unit(0, "pt")) +
                    labs(x="",y="",title="CHIRPSv2")


col_dif <- c('#9A133DFF', '#B93961FF', '#D8527CFF', '#F28AAAFF', '#F9B4C9FF', '#F9E0E8FF',
             '#C5DAF6FF', '#A1C2EDFF', '#6996E3FF', '#4060C8FF', '#1A318BFF')

kge_dif <-    ggplot() +
                geom_sf(data = nat_reg_shp, 
                        color = adjustcolor("black", alpha.f = 0.25), 
                        fill = NA, size = 0.01) +
                geom_point(data = res_month_data, 
                           aes(x = longitude, 
                               y = latitude, 
                               size=1,
                               color = kge_chirps_v3 - kge_chirps_v2),size=1) +
                scale_color_stepsn(
                  name = "", 
                  colors = col_dif,
                  limits = c(-.6, .6),
                  breaks = seq(-.6, .6, .1),
                  labels = round(seq(-.6, .6, .1),1),
                  guide = guide_colorbar(
                    title = 'KGE - CHIRPSv3 minus CHIRPSv2',
                    title.position = "top", 
                    label.position = "bottom",
                    direction = "horizontal",
                    barheight = unit(.4, "cm"),
                    barwidth = unit(11, "cm"))) +
                theme_minimal() +
                scale_x_continuous(breaks = seq(-80, -66, by = 3))+
                #labels = c("78°W", "75°W", "72°W","69°W","X")) +
                scale_y_continuous(breaks = seq(-4, 12, by = 4), labels = c("-4°S","0°", "4°N", "8°N","12°N")) +  
                theme(plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),# Reduce margin equally
                      plot.title = element_text(size = 12, face = "bold", hjust = 0.5,color = "black"),
                      legend.title = element_text(size = 12,face = "bold", vjust = 0.0,hjust=0.5,color = "black"),
                      legend.text = element_text(size = 11,color = "black"),
                      axis.text  =element_text(size=12,color = "black"),
                      legend.position = "bottom",
                      legend.title.align = 1,
                      legend.box.spacing = unit(0, "pt")) +
                labs(x="",y="",title="")


res_month_data$kge_diff_color <- ifelse(res_month_data$kge_chirps_v3 > res_month_data$kge_chirps_v2,
                                        "#3d8d52", "#7a3d8d")  # purple tone
kge_cat_map <- ggplot() +
                geom_sf(data = nat_reg_shp, 
                        color = adjustcolor("black", alpha.f = 0.25), 
                        fill = NA, size = 0.01) +
                geom_point(data = res_month_data, 
                           aes(x = longitude, 
                               y = latitude, 
                               color = kge_diff_color), size = 1) +
                scale_color_manual(
                  name = "",
                  values = c("#3d8d52" = "#3d8d52", "#7a3d8d" = "#7a3d8d"),
                  labels = c("v3 > v2 (785/1004)", "v2 ≥ v3 (219/1004)"),
                  guide = guide_legend(
                    title = "KGE performance",
                    title.position = "top",
                    direction = "horizontal",
                    override.aes = list(size = 4) 
                  )
                ) +
                theme_minimal() +
                scale_x_continuous(breaks = seq(-80, -66, by = 3)) +
                scale_y_continuous(
                  breaks = seq(-4, 12, by = 4), 
                  labels = c("-4°S", "0°", "4°N", "8°N", "12°N")
                ) +  
                theme(
                  plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "cm"),
                  plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black"),
                  legend.title = element_text(size = 12, face = "bold", vjust = 0.0, hjust = 0.5, color = "black"),
                  legend.text = element_text(size = 11, color = "black"),
                  axis.text = element_text(size = 12, color = "black"),
                  legend.position = "bottom",
                  legend.title.align = 1,
                  legend.box.spacing = unit(0, "pt")
                ) +
                labs(x = "", y = "", title = "")




library(cowplot)
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_month_performance_maps.png",
          sep = '/'), units = "in",width =8, height = 10, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(
ggarrange(chirpsv2_kge,chirpsv3_kge ,
          ncol=2,nrow=1,
          align = "hv", labels = c("a","b"),
          font.label = list(size = 20, face = "bold"), 
          common.legend = TRUE,legend = "bottom"),
ggarrange(kge_dif,kge_cat_map,
          align = "hv", labels = c("c","d"),
          font.label = list(size = 20, face = "bold"), 
          ncol=2,nrow=1,common.legend = FALSE,legend = "bottom"),
          nrow=2)
          
dev.off()          
          



#/////////////////////////////////////////////////////////////////////////
# Climatic and topographic gradients

elev_grad_plot <- function(data, var, y_label,x_label, y_n, r_low, h_low, int,
                           opt_value,x_labels,angle) {
  
  # Subconjunto dos dados e renomear colunas
  dx <- data[, c("elevation", paste0(var, 'chirps_v2'),
                 paste0(var, 'chirps_v3_era5'), paste0(var, 'chirps_v3_imerg'))]
  colnames(dx) <- c('elevation', 'CHIRPSv2', 'CHIRPSv3-ERA5', 'CHIRPSv3-IMERG')
  
  # Criar classes de elevação (intervalos de 250m)
  dx <- dx %>% mutate(elev_class = floor(elevation / 250))
  
  # Gerar vetor de rótulos para o eixo X (classes)
  max_class <- max(dx$elev_class, na.rm = TRUE)
  breaks_vector <- seq(0, max_class) * 250
  labels_vector <- paste0("[", breaks_vector, ",", breaks_vector + 250, ")")
  
  # Aplicar labels como fator
  dx$elev_class <- factor(dx$elev_class, levels = 0:max_class, labels = labels_vector)
  
  # Reshape dos dados para formato longo
  dx_long <- dx %>%
    pivot_longer(cols = contains("CHIRPS"), names_to = "dataset", values_to = "value")
  
  # Contar observações por classe de elevação
  elev_counts <- dx %>%
    group_by(elev_class) %>%
    summarise(n = n(), .groups = "drop")
  
  # Plot
  p <- ggplot(dx_long, aes(x = elev_class, y = value, fill = dataset)) +
    geom_boxplot(alpha = 0.7) +
    geom_text(data = elev_counts, aes(x = elev_class, y = y_n, label = paste0("[", n, "]")),
              inherit.aes = FALSE, size = 3.5, vjust = 1.5, color = "black") +
    labs(x = x_label, y = y_label, fill = "Version") +
    theme_classic() +
    scale_y_continuous(
      limits = c(r_low, h_low),
      breaks = seq(r_low, h_low, by = int),
      labels = seq(r_low, h_low, by = int)
    ) +
    scale_x_discrete(labels = x_labels)+
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = 0.5) +
    theme(
      plot.margin = unit(c(.0, .0, .0, .0), "cm"),
      plot.title = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"),
      legend.text = element_text(size = 11, color = "black"),
      legend.position = "bottom",
      legend.title.align = 1,
      axis.title.y = element_text(size = 13, vjust = 0.5, color = "black"),
      axis.title.x = element_text(size = 14, color = "black"),
      axis.text.x = element_text(size = 11, color = "black", angle = angle, hjust = 1),
      axis.text.y = element_text(size = 11, color = "black"),
      legend.box.spacing = unit(-.001, "pt"),
      legend.margin = margin(.001, 0.03, 0.01, 0.0025),
      legend.box.margin = margin(0, 0, 0, 0)
    ) +
    scale_fill_manual(values = c(
      "CHIRPSv2" = "#7a3d8d", 
      "CHIRPSv3-IMERG" = "#3d8d52",
      "CHIRPSv3-ERA5" = "#9ec6bc"
    ))
  
  return(p)
}


x_labs_num <-  c("[0,250)"  ,   "" ,  "[500,750)" ,  "" ,
                 "[1000,1250)", "" ,"[1500,1750)", "",
                 "[2000,2250)" ,"", "[2500,2750)", "",
                 "[3000,3250)", "" ,"[3500,3750)")
x_labs_na   <-  c(""  ,   "" ,  "" ,  "" ,
                 "", "" ,"", "",
                 "" ,"", "", "",
                 "", "" ,"")


r_elev   <- elev_grad_plot(res_daily_data,'r_','Correlation \n coefficient (r)',"",.9,-0.,1,.25,1,x_labs_na,0)
B_elev   <- elev_grad_plot(res_daily_data,'B_',paste0("Bias \n ratio (","\u03B2",")"),"",.1,-0,1.5,.25,1,x_labs_na,0)
G_elev   <- elev_grad_plot(res_daily_data,'G_',paste0("Variability \n ratio (","\u03B3",")"),
                           "Elevation (m.a.s.l.)",2,-0,2,.5,1,x_labs_num,25)
kge_elev <- elev_grad_plot(res_daily_data,'kge_','Kling-Gupta \n Efficiency (KGE)',"Elevation (m.a.s.l.)",
                           .9,-0,1,.25,1,x_labs_num,25)


library(cowplot)
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_daily_elev_grad.png",
          sep = '/'), units = "in",width = 14, height = 6, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(ggarrange(r_elev,B_elev,
          align = "hv", labels = c("a","b"),
          font.label = list(size = 18, face = "bold"), nrow=1,
          label.x = 0.0, label.y = 1.,  # mover a la derecha y arriba
          ncol=2,common.legend = FALSE,legend = "none"),
          ggarrange(G_elev,kge_elev,
                    align = "hv", labels = c("c","d"),
                    label.x = 0.0, label.y = 1.,  # mover a la derecha y arriba
                    font.label = list(size = 18, face = "bold"), nrow=1,
                    ncol=2,common.legend = TRUE,legend = "bottom"),
          nrow=2)

dev.off()



# MAP gradients
res_daily_data_map <- merge(res_daily_data,gauges_summary,by='gauge_code',all.x=TRUE)

map_grad_plot <- function(data, var, y_label,x_label, y_n, r_low, h_low, int,
                           opt_value,x_labels,angle) {
  
  # Subconjunto dos dados e renomear colunas
  dx <- data[, c("map", paste0(var, 'chirps_v2'),
                 paste0(var, 'chirps_v3_era5'), paste0(var, 'chirps_v3_imerg'))]
  colnames(dx) <- c('map', 'CHIRPSv2', 'CHIRPSv3-ERA5', 'CHIRPSv3-IMERG')
  
  dx$map <- ifelse(dx$map > 7000,7000,dx$map)
  # Criar classes de elevação (intervalos de 250m)
  dx <- dx %>% mutate(map_class = floor(map / 500))
  
  # Gerar vetor de rótulos para o eixo X (classes)
  max_class <- max(dx$map_class, na.rm = TRUE)
  breaks_vector <- seq(0, max_class) * 500
  labels_vector <- paste0("[", breaks_vector, ",", breaks_vector + 500, ")")
  
  # Aplicar labels como fator
  dx$map_class <- factor(dx$map_class, levels = 0:max_class, labels = labels_vector)
  
  # Reshape dos dados para formato longo
  dx_long <- dx %>%
    pivot_longer(cols = contains("CHIRPS"), names_to = "dataset", values_to = "value")
  
  # Contar observações por classe de elevação
  map_counts <- dx %>%
    group_by(map_class) %>%
    summarise(n = n(), .groups = "drop")
  
  # Plot
  p <- ggplot(dx_long, aes(x = map_class, y = value, fill = dataset)) +
    geom_boxplot(alpha = 0.7) +
    geom_text(data = map_counts, aes(x = map_class, y = y_n, label = paste0("[", n, "]")),
              inherit.aes = FALSE, size = 3.5, vjust = 1.5, color = "black") +
    labs(x = x_label, y = y_label, fill = "Version") +
    theme_classic() +
    scale_y_continuous(
      limits = c(r_low, h_low),
      breaks = seq(r_low, h_low, by = int),
      labels = seq(r_low, h_low, by = int)
    ) +
    scale_x_discrete(labels = x_labels)+
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = 0.5) +
    theme(
      plot.margin = unit(c(.0, .0, .0, .0), "cm"),
      plot.title = element_text(size = 10, color = "black"),
      legend.title = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"),
      legend.text = element_text(size = 11, color = "black"),
      legend.position = "bottom",
      legend.title.align = 1,
      axis.title.y = element_text(size = 13, vjust = 0.5, color = "black"),
      axis.title.x = element_text(size = 14, color = "black"),
      axis.text.x = element_text(size = 11, color = "black", angle = angle, hjust = 1),
      axis.text.y = element_text(size = 11, color = "black"),
      legend.box.spacing = unit(-.001, "pt"),
      legend.margin = margin(.001, 0.03, 0.01, 0.0025),
      legend.box.margin = margin(0, 0, 0, 0)
    ) +
    scale_fill_manual(values = c(
      "CHIRPSv2" = "#7a3d8d", 
      "CHIRPSv3-IMERG" = "#3d8d52",
      "CHIRPSv3-ERA5" = "#9ec6bc"
    ))
  
  return(p)
}


x_labs_num <-   c("[0,500)"  ,   "" , "[1000,1500)",
                  "" ,"[2000,2500)", "",
                  "[3000,3500)" ,"", "[4000,4500)",
                  "" ,"[5000,5500)", "",
                  "[6000,6500)" ,"", "[7000,12400)")

x_labs_na  <-  c("" ,    "",  "",
                 "", "" ,"",
                 "", "", "",
                 "", "", "",
                 "", "" ,"")


r_map   <- map_grad_plot(res_daily_data_map,'r_','Correlation \n coefficient (r)',"",.9,-0.,1,.25,1,x_labs_na,0)
B_map   <- map_grad_plot(res_daily_data_map,'B_',paste0("Bias \n ratio (","\u03B2",")"),"",.1,-0,2.,.5,1,x_labs_na,0)
G_map   <- map_grad_plot(res_daily_data_map,'G_',paste0("Variability \n ratio (","\u03B3",")"),
                         "Mean annual precipitation (mm)",2,-0,2,.5,1,x_labs_num,25)
kge_map <- map_grad_plot(res_daily_data_map,'kge_','Kling-Gupta \n Efficiency (KGE)',
                         "Mean annual precipitation (mm)",.9,-0,1,.25,1,x_labs_num,25)



dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_daily_MAP_grad.png",
          sep = '/'), units = "in",width = 14, height = 6, 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(ggarrange(r_map,B_map,
                    align = "hv", labels = c("a","b"),
                    font.label = list(size = 18, face = "bold"), nrow=1,
                    label.x = 0.0, label.y = 1.,  # mover a la derecha y arriba
                    ncol=2,common.legend = FALSE,legend = "none"),
          ggarrange(G_map,kge_map,
                    align = "hv", labels = c("c","d"),
                    label.x = 0.0, label.y = 1.,  # mover a la derecha y arriba
                    font.label = list(size = 18, face = "bold"), nrow=1,
                    ncol=2,common.legend = TRUE,legend = "bottom"),
          nrow=2)

dev.off()






