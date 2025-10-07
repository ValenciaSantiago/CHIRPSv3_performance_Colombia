

library(terra)
library(ggplot2)
library(rnaturalearth)
pcp_1981_01_v3 <- rast(paste0(
              "D:/3_CHIRPSv3/1_Daily/CHIRPSv3_Daily_ERA5/Colombia/","chirps-v3.0.1981.01.03_ERA5.tif"))

pcp_1981_01_v2 <- rast(paste0(
  "C:/Users/santiagovalencia/Desktop/ERA5/","chirps-v2.0.1981.01.03.tif"))






col_pal <- c("#693829FF", "#894B33FF", "#A56A3EFF", "#CFB267FF", "#D9C5B6FF",
  "#9CA9BAFF", "#5480B5FF", "#3D619DFF", "#405A95FF", "#345084FF")
countries <- ne_countries(type = "countries",scale = "medium")[1]


aoi_col <- ext(-82, -64,-6, 14)
pcp_1981_01_v3_df <- as.data.frame(pcp_1981_01_v3, xy = TRUE, na.rm = TRUE)
pcp_1981_01_v2_df <- as.data.frame(crop(pcp_1981_01_v2,aoi_col), xy = TRUE, na.rm = TRUE)




pcp_1981_01_v3_plot <- ggplot() +
                geom_tile(data = pcp_1981_01_v3_df, 
                          aes(x = x, 
                              y = y, 
                              fill = pcp_1981_01_v3_df[,3])) +
                #geom_sf(data = forest, fill = "black", alpha = 0.1, color = "transparent") +  # Add forest shapefile
                geom_sf(data = countries, color = "black", fill = NA, size = 0.1) +  # Add country boundaries
                theme_bw() +
                scale_fill_stepsn(name = "", 
                                  colors =col_pal,
                                  limits=c(0,50),
                                  breaks = seq(0,50,5))+
                coord_sf(xlim = c(-82, -64), ylim = c(-6, 14), expand = FALSE)   # Limit plot to extent




pcp_1981_01_v2_plot <- ggplot() +
  geom_tile(data = pcp_1981_01_v2_df, 
            aes(x = x, 
                y = y, 
                fill = pcp_1981_01_v2_df[,3])) +
  #geom_sf(data = forest, fill = "black", alpha = 0.1, color = "transparent") +  # Add forest shapefile
  geom_sf(data = countries, color = "black", fill = NA, size = 0.1) +  # Add country boundaries
  theme_bw() +
  scale_fill_stepsn(name = "", 
                    colors =col_pal,
                    limits=c(0,50),
                    breaks = seq(0,50,5))+
  coord_sf(xlim = c(-82, -64), ylim = c(-6, 14), expand = FALSE)   # Limit plot to extent




library(ggpubr )
library(gridExtra)
dir_sup_plots <- "G:/My Drive/R4C_et_al/3_PLOTS"
png(paste(dir_sup_plots, "1981-01-03.png", sep = "/"),
    width = 10, height = 5, units = "in", res = 600)

ggarrange(pcp_1981_01_v2_plot,pcp_1981_01_v3_plot, ncol = 2, labels = c("CHIRPSv2","CHIRPSv3"))

dev.off()




                