


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




#//////////////////////////////////////////////////////////////////////////


opt_value <- 1
plot_temporal_scales_function <- function(database_daily,database_pentad,
                               database_month,var1,var2,var1_day,var2_day,var3_day,x_label,
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
    database_daily %>%
      transmute(
        period = "Daily",
        var = !!sym(var3_day),
        product = "Si"),
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
  
  df_summary <- df_var %>%
    group_by(period, product) %>%
    summarise(
      median = median(var, na.rm = TRUE),
      q1 = quantile(var, 0.25, na.rm = TRUE),
      q3 = quantile(var, 0.75, na.rm = TRUE),
      .groups = 'drop')
  
  df_summary <- df_summary %>%
    mutate(label_y = ifelse(product == "Yes", median + 0.02, median - 0.02))  # Adjust 0.02 as needed
  
  
  plot_violin <- ggplot(df_summary, aes(y = median, x = period, color = product)) + 
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = 0.5) +
    geom_errorbar(aes(ymin = q1, ymax = q3), 
                  position = position_dodge(width = 0.5), 
                  width = 0.000, size = 0.6) +
    geom_point(position = position_dodge(width = 0.5), size = 2.5) +
    coord_flip() +
    scale_color_manual(
      name = "Wind-correction \n factor",
      values = c(
        "Yes" = adjustcolor('#7a3d8d', alpha.f = 0.95),
        "No" = adjustcolor('#3d8d52', alpha.f = 0.95),
        "Si" = adjustcolor('#9ec6bc', alpha.f = 0.95))
    ) +
    #geom_text(aes(y = label_y, label = round(median, 2)), 
   #           position = position_dodge(width = 0.5),
    #          color = "black", 
    #          size = 2.8) +
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
      axis.title.y = element_text(size = 13.7, vjust = 0.5, color = 'black'),
      axis.title.x = element_text(size = 13.7, color = 'black'),
      axis.text = element_text(size = 13.7, color = 'black'),
      legend.title = element_text(size = 16, vjust = 0.0, hjust = 0.5, color = 'black'),
      legend.text = element_text(size = 14, color = 'black'),
      legend.position = "none",
      legend.title.align = 1,
      legend.box.spacing = unit(-.001, "pt"), 
      legend.margin = margin(.001, 0.03, 0., 0.0025),
      legend.box.margin = margin(0, 0, 0, 0))
  return(plot_violin)
}


kge_plot <- plot_temporal_scales_function(res_daily_data,res_pentad_data,res_month_data,
                               'kge_chirps_v2','kge_chirps_v3',
                               'kge_chirps_v2','kge_chirps_v3_imerg','kge_chirps_v3_era5',
                               'Kling-Gupta Efficiency (KGE)',-0.1,1,.25)
kge_plot <- kge_plot + scale_y_continuous(
  limits = c(-0.0, 1),  # Set numeric limits
  breaks = seq(-0.,1,.2),  # Numeric breaks
  #labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0")
  )

kge_plot



library(cowplot)
dir_abstract <- 'G:/My Drive/R4C_et_al/3_PLOTS/Graphical_Abstract_Paneles'
png(paste(dir_abstract, "Fig_KGE_temporal_scales.png",
          sep = '/'), units = "in",width = 3.8, height = 2., 
    res = 600, pointsize = 11)#, bg = "transparent")
kge_plot
dev.off()




#////////////////////////////////////////////////////////////////

# load annual precipitation data
pcp_annual      <- fread(paste0(dir_IDEAM_GPPs,"/IDEAM_GPPs_annual.csv"),head=TRUE)%>%
  filter(na_count_ideam <= 30)
pcp_annual_mean <- pcp_annual  %>%
  group_by(gauge_code) %>%
  summarise(
    annual_mean    = mean(pcp_ideam_flag, na.rm = TRUE),  
    .groups = "drop") 
pcp_annual_mean 


res_pentad_data <- merge(res_pentad_data,pcp_annual_mean,by="gauge_code")
head(res_pentad_data)



opt_value <- 1
plot_map_gradient_function <- function(database_pentad,var1,var2,x_label,
                                          r_low,h_low,int){
  
 
  df_var <- bind_rows(
    database_pentad %>%
      transmute(
        period = "Pentad",
        var = !!sym(var2),
        annual_mean = annual_mean,  # Include the MAP variable if it's in your data
        product = "CHIRPSv3"
      ),
    database_pentad %>%
      transmute(
        period = "Pentad",
        var = !!sym(var1),
        annual_mean = annual_mean,
        product = "CHIRPSv2"
      )
  ) %>%
    # Categorize by MAP levels
    mutate(
      map_bin = case_when(
        annual_mean < 1000 ~ "< 1000",
        annual_mean >= 1000 & annual_mean < 2000 ~ "1000–2000",
        annual_mean >= 2000 & annual_mean < 3000 ~ "2000–3000",
        annual_mean >= 3000 & annual_mean < 4000 ~ "3000–4000",
        annual_mean >= 4000 ~ "≥ 4000",
        TRUE ~ NA_character_
      ))
  head(df_var)
  
  
  #df_var$period <- factor(df_var$period, levels = c("Monthly","Pentad","Daily"))
  
  df_summary <- df_var %>%
    group_by(map_bin, product) %>%
    summarise(
      median = median(var, na.rm = TRUE),
      q1 = quantile(var, 0.25, na.rm = TRUE),
      q3 = quantile(var, 0.75, na.rm = TRUE),
      .groups = 'drop')

    df_summary <- df_summary %>%
    mutate(
      product = factor(product, levels = c("CHIRPSv3", "CHIRPSv2")),
      map_bin = factor(map_bin, levels = c("< 1000", "1000–2000", "2000–3000",
                                               "3000–4000", "≥ 4000")))
  
  
  
  plot_violin <- ggplot(df_summary, aes(y = median, x = map_bin, color = product)) + 
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = 0.5) +
    geom_errorbar(aes(ymin = q1, ymax = q3), 
                  position = position_dodge(width = 0.5), 
                  width = 0.000, size = 0.6) +
    geom_point(position = position_dodge(width = 0.5), size = 2.5) +
    coord_flip() +
    scale_color_manual(
      name = "Wind-correction \n factor",
      values = c(
        "CHIRPSv2" = adjustcolor('#7a3d8d', alpha.f = 0.95),
        #"CHIRPSv3" = adjustcolor('#3d8d52', alpha.f = 0.95),
        "CHIRPSv3" = adjustcolor('#3d8d52', alpha.f = 0.95)
        )
    ) +
    #geom_text(aes(y = label_y, label = round(median, 2)), 
    #           position = position_dodge(width = 0.5),
    #          color = "black", 
    #          size = 2.8) +
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
      axis.title.y = element_text(size = 13, vjust = 0.5, color = 'black'),
      axis.title.x = element_text(size = 13, color = 'black'),
      axis.text = element_text(size = 13, color = 'black'),
      legend.title = element_text(size = 16, vjust = 0.0, hjust = 0.5, color = 'black'),
      legend.text = element_text(size = 14, color = 'black'),
      legend.position = "none",
      legend.title.align = 1,
      legend.box.spacing = unit(-.001, "pt"), 
      legend.margin = margin(.001, 0.03, 0., 0.0025),
      legend.box.margin = margin(0, 0, 0, 0))
  return(plot_violin)
}


kge_map_plot <- plot_map_gradient_function(res_pentad_data,
                                          'kge_chirps_v2','kge_chirps_v3',
                                          'Kling-Gupta Efficiency (KGE)',-0.1,1,.25)

kge_map_plot <- kge_map_plot + scale_y_continuous(
  limits = c(-0.0, 1),  # Set numeric limits
  breaks = seq(-0.,1,.2),  # Numeric breaks
  #labels = c("", "-0.50", "", "0", "", "0.50", "", "1.0")
)

kge_map_plot


library(cowplot)
dir_abstract <- 'G:/My Drive/R4C_et_al/3_PLOTS/Graphical_Abstract_Paneles'
png(paste(dir_abstract, "Fig_KGE_MAP_gradients.png",
          sep = '/'), units = "in",width = 3.8, height = 2., 
    res = 600, pointsize = 11)#, bg = "transparent")
kge_map_plot
dev.off()



#/////////////////////////////////////////////////////
# Elevation gradients 
opt_value <- 1
plot_elevation_gradient_function <- function(database_pentad,var1,var2,x_label,
                                       r_low,h_low,int){
  
  
  df_var <- bind_rows(
    database_pentad %>%
      transmute(
        period = "Pentad",
        var = !!sym(var1),
        elevation = elevation,  # Include the MAP variable if it's in your data
        product = "CHIRPSv2"
      ),
    database_pentad %>%
      transmute(
        period = "Pentad",
        var = !!sym(var2),
        elevation = elevation,
        product = "CHIRPSv3"
      )
  ) %>%
    # Categorize by MAP levels
    mutate(
      elevation = case_when(
        elevation < 100 ~ "< 100",
        elevation >= 100 & elevation < 500 ~ "100–500",
        elevation >= 500 & elevation < 1000 ~ "500–1000",
        elevation >= 1000 & elevation < 2000 ~ "1000–2000",
        elevation >= 2000 ~ "≥ 2000",
        TRUE ~ NA_character_
      ))
  head(df_var)
  
  
  #df_var$period <- factor(df_var$period, levels = c("Monthly","Pentad","Daily"))
  
  df_summary <- df_var %>%
    group_by(elevation, product) %>%
    summarise(
      median = median(var, na.rm = TRUE),
      q1 = quantile(var, 0.25, na.rm = TRUE),
      q3 = quantile(var, 0.75, na.rm = TRUE),
      .groups = 'drop')
  
  df_summary <- df_summary%>%
    mutate(
      product = factor(product, levels = c("CHIRPSv3", "CHIRPSv2")),
      elevation = factor(elevation, levels = c("< 100", "100–500",
                                                              "500–1000", "1000–2000",
                                                              "≥ 2000")))
  
  
  plot_violin <- ggplot(df_summary, aes(y = median, x = elevation, color = product)) + 
    geom_hline(yintercept = opt_value, linetype = "dashed", color = "darkgrey", size = 0.5) +
    geom_errorbar(aes(ymin = q1, ymax = q3), 
                  position = position_dodge(width = 0.5), 
                  width = 0.000, size = 0.6) +
    geom_point(position = position_dodge(width = 0.5), size = 2.5) +
    coord_flip() +
    scale_color_manual(
      name = "Wind-correction \n factor",
      values = c(
        "CHIRPSv2" = adjustcolor('#7a3d8d', alpha.f = 0.95),
        #"CHIRPSv3" = adjustcolor('#3d8d52', alpha.f = 0.95),
        "CHIRPSv3" = adjustcolor('#3d8d52', alpha.f = 0.95)
      )
    ) +
    #geom_text(aes(y = label_y, label = round(median, 2)), 
    #           position = position_dodge(width = 0.5),
    #          color = "black", 
    #          size = 2.8) +
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
      axis.title.y = element_text(size = 13, vjust = 0.5, color = 'black'),
      axis.title.x = element_text(size = 13, color = 'black'),
      axis.text = element_text(size = 13, color = 'black'),
      legend.title = element_text(size = 16, vjust = 0.0, hjust = 0.5, color = 'black'),
      legend.text = element_text(size = 14, color = 'black'),
      legend.position = "none",
      legend.title.align = 1,
      legend.box.spacing = unit(-.001, "pt"), 
      legend.margin = margin(.001, 0.03, 0., 0.0025),
      legend.box.margin = margin(0, 0, 0, 0))
  return(plot_violin)
}


kge_elevation_plot <- plot_elevation_gradient_function(res_pentad_data,
                                           'kge_chirps_v2','kge_chirps_v3',
                                           'Kling-Gupta Efficiency (KGE)',-0.1,1,.25)

kge_elevation_plot  <- kge_elevation_plot  + scale_y_continuous(
  limits = c(-0.0, 1),  # Set numeric limits
  breaks = seq(-0.,1,.2))

kge_elevation_plot 


library(cowplot)
dir_abstract <- 'G:/My Drive/R4C_et_al/3_PLOTS/Graphical_Abstract_Paneles'
png(paste(dir_abstract, "Fig_KGE_elevation_gradients.png",
          sep = '/'), units = "in",width = 3.8, height = 2., 
    res = 600, pointsize = 11)#, bg = "transparent")
kge_elevation_plot 
dev.off()




