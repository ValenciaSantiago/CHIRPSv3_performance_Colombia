


#//////////////////////////////////////////////////
# Load packages
library(pacman)
p_load(terra,ggplot2,data.table,sf,rnaturalearth,stats,foreach,doParallel,ggpubr,
       dplyr,stringr,quantmod,rnaturalearth,exactextractr,hydroGOF,GHCNr ,
       future.apply,profvis,rnaturalearthdata,glue)


#//////////////////////////////////////////////////
# directories
ideam_pcp_data      <- 'G:/My Drive/R4C_et_al/IDEAM_PRECIPITACION_2023/PCP_IDEAM_TIME_SERIES_QUALITY'
dir_chirpsv2_st_den <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/2_CHIRPSv2/0_Gauges_density"
dir_chirpsv3_st_den <- "G:/My Drive/R4C_et_al/1_DATA/1_PRECIPITATION_SATELLITE/3_CHIRPSv3/0_Gauges_density"
dir_plots           <- "G:/My Drive/R4C_et_al/3_PLOTS"
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
#dir_IDEAM_GPPs      <- "G:/My Drive/R4C_et_al/4_IDEAM_GPPs"
dir_IDEAM_GPPs      <- "D:/4_IDEAM_GPPs"


countries  <- ne_countries(type = "countries",scale = "medium")[1]
colombia <- countries[countries$name == "Colombia", ]
nat_reg_shp <- st_read("G:/My Drive/05_Papers/ValenciaEtAl-SRE/GIS/shp_regiones_naturales_colombia.shp")

gauges_summary_df <- as.data.frame(fread(paste0("G:/My Drive/R4C_et_al/4_IDEAM_GPPs/",
                                         "gauges_summary.csv")))


pcp_gauges_sf <- st_as_sf(gauges_summary_df, coords = c("longitude", "latitude"), crs = 4326)  
#pcp_gauges_sf$latitude <- pcp_gauges_points$latitude
#pcp_gauges_sf$longitude <- pcp_gauges_points$longitude
plot(pcp_gauges_sf[1])
head(gauges_summary_df)

#//////////////////////////////////////////////////////////



col_gauges <- c('#FFEC9DFF', '#FAC881FF', '#F4A464FF', '#E87444FF', '#D9402AFF',
                '#BF2729FF', '#912534FF', '#64243EFF', '#3D1B28FF', '#161212FF', "black")

col_gauges1 <- c('#593722FF', '#834D24FF', '#AB5D26FF', '#C3722AFF', 
                '#E1C473FF', '#E6DAB9FF', '#A4B591FF', '#55804DFF', '#416C39FF', '#2C5724FF')

p_miss <- ggplot() +
        geom_sf(data = nat_reg_shp, 
                color = adjustcolor("black", alpha.f = 0.8), 
                fill = NA, size = 0.02) +
        geom_point(data = gauges_summary_df, 
                   aes(x = longitude, 
                       y = latitude, 
                       color = p_sd_q)) +
        scale_color_stepsn(
          name = "", 
          colors = rev(col_gauges1),
          limits = c(0, 10),
          breaks = seq(0, 10, 1),
          labels = as.character(0:10),
          guide = guide_colorbar(
            title = 'Percentage of low-quality data and missing values',
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






region_colors <- c("Andes" = "#578fca", "Caribe" = "#b5828c", "Pacifico" = "#9f5abb",
                  "Orinoquia" = "#f0a04b","Amazonia" = "#5cb338")

# Custom legend labels (if you want to relabel them nicely)
region_labels <- c(
  "Andes" = "Andes",
  "Caribe" = "Caribbean",
  "Pacifico" = "Pacific",
  "Orinoquia" = "Orinoco",
  "Amazonia" = "Amazon")

hist_map <- ggplot(gauges_summary_df, aes(x = map, fill = nat_region)) +
            geom_histogram(binwidth = 500, color = "black", position = "stack", alpha = 0.9) +
            scale_fill_manual(
              values = region_colors,
              name = "Natural Region",
              labels = region_labels) +
             scale_y_continuous(limits=c(0,260))+
              scale_x_continuous(breaks = seq(0, 12500, 500),
                     labels=c("0","","10","","20","","30","","40","","50","","60",
                              "","70","","80","","90","","100","","110","","120",""))+ 
                    
            labs(
              title = "",
              x = "Mean annual precipitation (cm)",
              y = "Number of gauges"
            ) +
            theme_classic() +
            theme(
              plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"),
              plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black"),
              legend.title = element_text(size = 12, face = "bold", vjust = 0, hjust = 0.5, color = "black"),
              legend.text = element_text(size = 12, color = "black"),
              axis.title = element_text(size = 12, face = "bold",color = "black"),
              axis.text = element_text(size = 12, color = "black"),
              legend.position = c(.8,.5),
              legend.title.align = .5,
              legend.box.spacing = unit(0, "pt"))


hist_mis <- ggplot(gauges_summary_df, aes(x = p_sd_q, fill = nat_region)) +
              geom_histogram(binwidth = 1, color = "black", position = "stack", alpha = 0.9) +
              scale_fill_manual(
                values = region_colors,
                name = "Natural Region",
                labels = region_labels
              ) +
              scale_x_continuous(breaks = seq(0, 10, 1)) +
              labs(
                title = "",
                x = "Percentage of low-quality data and missing values",
                y = "Number of gauges"
              ) +
              theme_classic() +
              theme(
                plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"),
                plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black"),
                legend.title = element_text(size = 12, face = "bold", vjust = 0, hjust = 0.5, color = "black"),
                legend.text = element_text(size = 12, color = "black"),
                axis.title = element_text(size = 12, face = "bold",color = "black"),
                axis.text = element_text(size = 12, color = "black"),
                legend.position = 'none',
                legend.title.align = .5,
                legend.box.spacing = unit(0, "pt"))


hist_elev <- ggplot(gauges_summary_df, aes(x = elevation, fill = nat_region)) +
            geom_histogram(binwidth = 250, color = "black", position = "stack", alpha = 0.9) +
            scale_fill_manual(
              values = region_colors,
              name = "Natural Region",
              labels = region_labels
            ) +
            scale_x_continuous(breaks = seq(0, 4000, 250),
                               labels=c("0","","500","","1000","","1500","","2000",
                                        "","2500","","3000","","3500","","4000")) +
            labs(
              title = "",
              x = "Elevation (m.a.s.l)",
              y = "Number of gauges"
            ) +
            theme_classic() +
            theme(
              plot.margin = unit(c(0.01, 0.01, 0.01, 0.01), "cm"),
              plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "black"),
              legend.title = element_text(size = 12, face = "bold", vjust = 0, hjust = 0.5, color = "black"),
              legend.text = element_text(size = 12, color = "black"),
              axis.title = element_text(size = 12, face = "bold",color = "black"),
              axis.text = element_text(size = 12, color = "black"),
              legend.position = "none",
              legend.title.align = .5,
              legend.box.spacing = unit(0, "pt"))




library(cowplot)
dir_plots_supp      <- 'G:/My Drive/R4C_et_al/3_PLOTS/SUPP_PLOTS'
png(paste(dir_plots_supp, "Fig_gauges_summary.png",
          sep = '/'), units = "in",width = 12, height = 8., 
    res = 600, pointsize = 11)#, bg = "transparent")

ggarrange(ggarrange(hist_map,hist_elev,hist_mis,
                    ncol = 1, nrow = 3, align = "hv", 
                    labels = c("a", "b","c"),
                    label.x = c(0.0,0.0),    # Shift label to the right (0 = left, 1 = right)
                    label.y = 1.0,    # Shift label slightly above plot
                    font.label = list(size = 20, face = "bold")),
          ggarrange(p_miss,ncol=1, align = "hv", labels = c("d"),
                    font.label = list(size = 20, face = "bold")), 
                    ncol=2,widths = c(4, 4))

dev.off()













