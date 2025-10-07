

# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,PupillometryR ,
       dplyr,stringr,quantmod,rnaturalearth,exactextractr,hydroGOF,tidyr,
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

data_daily      <- fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs","/IDEAM_GPPs_daily.csv"),head=TRUE)
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

#///////////////////////////////////////////////////////////////////////////

# Differences between scale and not scale performance

#colnames(res_month_data)
#var1 <- 'kge_chirps_v3_scale'
#var2 <- 'kge_chirps_v3'
#var1_day <- 'kge_chirps_v3_imerg_scale'
#var2_day <- 'kge_chirps_v3_imerg'
#database_daily  <- res_daily_data
#database_pentad <- res_pentad_data
#database_month  <- res_month_data
opt_value <- 1
plot_corr_function <- function(database_daily,database_pentad,
                               database_month,var1,var2,var1_day,var2_day,x_label,
                               r_low,h_low,int){

df_var <- bind_rows(
  database_daily %>%
    transmute(
      period = "Daily",
      var = !!sym(var1_day),
      product = "Yes"),
  database_daily %>%
    transmute(
      period = "Daily",
      var = !!sym(var2_day),
      product = "No"),
  database_pentad %>%
    transmute(
      period = "Pentad",
      var = !!sym(var1),
      product = "Yes"),
  database_pentad %>%
    transmute(
      period = "Pentad",
      var = !!sym(var2),
      product = "No"), 
  database_month %>%
    transmute(
      period = "Monthly",
      var = !!sym(var1),
      product = "Yes"),
  database_month %>%
    transmute(
      period = "Monthly",
      var = !!sym(var2),
      product = "No"))
head(df_var)


df_var$period <- factor(df_var$period, levels = c("Monthly","Pentad","Daily"))
plot_violin <- ggplot(df_var, aes(y = var, x = period, fill = product)) + 
  geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = .5) +
  geom_flat_violin(
    position = position_nudge(x = .35, y = 0), 
    alpha = 0.5, 
    trim = TRUE,
    stat = "ydensity",
    scale = "area",
    color = NA) +
  coord_flip() + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA, 
    color = "black",
    position = position_dodge(.5)) + 
  scale_fill_manual(
    name = "Wind-correction \n factor",
    values = c(
      "Yes" = adjustcolor('#91bfdb', alpha.f = 0.6),      
      "No" = adjustcolor('#fc8d59', alpha.f = 0.6))) +
  stat_summary(
    fun = median, 
    geom = "text", 
    aes(label = round(..y.., 2)), 
    position = position_dodge(1.1), 
    colour = "black", 
    size = 2.8, 
    vjust = 0.45) +
  scale_y_continuous(
    limits = c(r_low, h_low),
    breaks = seq(r_low, h_low, by = int),
    labels = seq(r_low, h_low, by = int)) +
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


r_plot <- plot_corr_function(res_daily_data,res_pentad_data,res_month_data,
                   'r_chirps_v3_scale','r_chirps_v3',
                   'r_chirps_v3_imerg_scale','r_chirps_v3_imerg',
                   'Correlation coefficient (r)',-0.75,1,.25)

r_plot  <- r_plot + 
  theme(
    legend.position = c(0.35, 0.2),  
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9))+  
  guides(fill = guide_legend(ncol = 1)) +
  #theme_minimal(base_size = 14) +
  theme(#legend.position = 'top', 
    legend.spacing.x = unit(.01, 'cm'),
    legend.background = element_rect(fill = NA, color = NA))

r_plot <- r_plot + scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))



B_plot <- plot_corr_function(res_daily_data,res_pentad_data,res_month_data,
                             'B_chirps_v3_scale','B_chirps_v3',
                             'B_chirps_v3_imerg_scale','B_chirps_v3_imerg',
                             paste0("Bias ratio (","\u03B2",")"),-0,2,.5)
B_plot <- B_plot +  theme(legend.position = "none",
                              axis.title.y = element_blank(),
                              axis.text.y = element_blank())

G_plot <- plot_corr_function(res_daily_data,res_pentad_data,res_month_data,
                             'G_chirps_v3_scale','G_chirps_v3',
                             'G_chirps_v3_imerg_scale','G_chirps_v3_imerg',
                             paste0("Variability ratio (","\u03B3",")"),-0,1.5,.5)
G_plot <- G_plot +  theme(legend.position = "none",
                              axis.title.y = element_blank(),
                              axis.text.y = element_blank())


kge_plot <- plot_corr_function(res_daily_data,res_pentad_data,res_month_data,
                               'kge_chirps_v3_scale','kge_chirps_v3',
                               'kge_chirps_v3_imerg_scale','kge_chirps_v3_imerg',
                               'Kling-Gupta Efficiency (KGE)',-0.75,1,.25)

kge_plot <- kge_plot +  theme(legend.position = "none",
                              axis.title.y = element_blank(),
                              axis.text.y = element_blank())
kge_plot <- kge_plot + scale_y_continuous(
  limits = c(-0.75, 1),  # Set numeric limits
  breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0),  # Numeric breaks
  labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0"))



#_________________________________________________________________________
# MAPs plots


col_pal <- (c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3',
              '#c7eae5','#80cdc1','#35978f','#01665e','#003c30'))

col_pal2 <- rev(c("#67001f","#b2182b","#d6604d","#f4a582","#f7f7f7",
                  "#92c5de","#4393c3","#2166ac","#053061"))

wind_plot_map_function <- function(dataset,var1,var2,title){
  
  dataset <- dataset %>%
    select(latitude, longitude, scale = !!sym(var1), normal = !!sym(var2))
  
  plot <- ggplot() +
    geom_sf(data = nat_reg_shp, color = adjustcolor("black", alpha.f = 0.7),
            fill = NA, size = 0.01)+
    geom_point(
      data = dataset,
      aes(x = longitude, y = latitude,
          color = scale - normal),size=.7) +
    scale_color_stepsn(
      name = "Undercatch-correction minus Uncorrected",
      colors = col_pal2,
      limits = c(-0.1, 0.1),
      breaks = seq(-0.1, 0.1, by = 0.02),
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
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
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
map_r_plot <- wind_plot_map_function(res_pentad_data,
              'r_chirps_v3_scale','r_chirps_v3','Correlation coefficient (r)')
map_B_plot <- wind_plot_map_function(res_pentad_data,
              'B_chirps_v3_scale','B_chirps_v3',paste0("Bias ratio (","\u03B2",")"))
map_G_plot <- wind_plot_map_function(res_pentad_data,
              'G_chirps_v3_scale','G_chirps_v3',paste0("Variability ratio (","\u03B3",")"))
map_kge_plot <- wind_plot_map_function(res_pentad_data,
              'kge_chirps_v3_scale','kge_chirps_v3','Kling-Gupta Efficiency (KGE)')



#_____________________________________________________________________________
# Save plots

blank_plot <- ggplot() + theme_void()
#png(paste0(dir_plots ,"/", "Fig_7_performance_wind_correction.png"), units = "in",
#    width = 9.5, height = 3.2, res = 600, pointsize = 11)
pdf(paste0(dir_plots, "/", "Fig_7_performance_wind_correction.pdf"),
    width = 9.5, height = 3.2, pointsize = 11)

ggarrange(r_plot,B_plot,G_plot,kge_plot,
          ncol=4,nrow=1,labels = c("a", "b","c",'d'),
          label.x = c(0.24,-0.06,-0.05,-0.05,0.05),widths = c(10.5,8,8,8,8))
dev.off()


png(paste0(dir_plots ,"/SUPP_PLOTS/",
           "Fig_performance_wind_correction_regions.png"), units = "in",
    width = 10, height = 4., res = 600, pointsize = 11)

ggarrange(map_r_plot,map_B_plot,map_G_plot,map_kge_plot,
          ncol=4,nrow=1,labels = c("", "","",'',''),
          label.x = c(0,-0.05,-.05,-.05,-.05),label.y=0.9,widths = c(8,8,8,8,8),
          common.legend=TRUE,legend='bottom')

dev.off()


