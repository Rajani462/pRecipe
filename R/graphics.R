library(data.table)
library(ggplot2)
library(rgdal)
library(viridis)
# Plot_time_series() ------------------------------------------------------


plot_timeseries <- function(data_table){
  
  mean_all <- data_table[, .(value = mean(value), name = factor("average")), by = c("x", "y", "Z")]
  data_table <- rbind(data_table, mean_all)
  data_table <- data_table[, reg_val := mean(value), by = .(month(Z), year(Z), name)][, ':='(reg_monmean = mean(reg_val), 
                                                                                             reg_monstd = sd(reg_val)), by = .(month(Z), name)]
  
  name_count <- length(unique(data_table$name)) - 1 
  
  ggplot(data_table, aes(Z, reg_val, color = name, linetype = name)) + 
    geom_line() + 
    geom_point() + 
    scale_linetype_manual(values = c(rep("dashed", name_count), "solid")) + 
    labs(x = "Month", y = "Precipitation (mm)") + 
    theme_bw()
  
}
# ---------------------------------------------------------------------------


# plot_seasonal_bar_plot() ------------------------------------------------

plot_seasonality <- function(data_table){
  
  data_table <- data_table[, reg_val := mean(value), 
                           by = .(month(Z), year(Z), name)][, ':='(reg_monmean = mean(reg_val),
                                                                   reg_monstd = sd(reg_val)), by = .(month(Z), name)]
  
  ggplot(data_table, aes(factor(month(Z)), reg_monmean, fill = name)) + 
    geom_bar(stat="identity", position=position_dodge(), alpha=0.5) + 
    geom_errorbar(aes(ymin = reg_monmean - reg_monstd, 
                      ymax = reg_monmean + reg_monstd), width = .4, 
                  position=position_dodge(width=0.90)) + 
    labs(x = "Month", y = "Precipitation (mm/month)") + 
    theme_bw()

}

# ---------------------------------------------------------------------------

# plot_raster() -----------------------------------------------------------


plot_annmean <- function(data_table, shapefile_path){
  
  data_table <- data_table[, year_val := sum(value), by = .(year(Z), x, y, name)][, ':='(anl_mean = mean(year_val), 
                                                                     anl_std = sd(year_val)),  by = .(x, y, name)]
  
  name_shp <- readOGR(shapefile_path)
  
  ggplot(data_table) +
    geom_raster(aes(x, y, fill = anl_mean)) + 
    coord_fixed(ratio = 1) + 
    scale_fill_viridis(direction = -1) + 
    labs(x = "Longitude", y = "Latitude", fill = "mm/yr") + 
    facet_wrap(~name, ncol = 2) + 
    ggtitle("Annual total mean precipitation (mm/yr)") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
  geom_path(data = name_shp, 
            aes(x = long, y = lat, group = group))
  
}

# ---------------------------------------------------------------------------


plot_annstd <- function(data_table, shapefile_path){
  
  data_table <- data_table[, year_val := sum(value), by = .(year(Z), x, y, name)][, ':='(anl_mean = mean(year_val), 
                                                                                         anl_std = sd(year_val)),  by = .(x, y, name)]
  name_shp <- readOGR(shapefile_path)
  
  ggplot(data_table) +
    geom_raster(aes(x, y, fill = anl_std)) + 
    coord_fixed(ratio = 1) + 
    scale_fill_viridis(direction = -1) + 
    labs(x = "Longitude", y = "Latitude", fill = "mm/yr") + 
    facet_wrap(~name, ncol = 2) + 
    ggtitle("Annual precipitation standard deviation (mm/yr)") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_path(data = name_shp, 
              aes(x = long, y = lat, group = group))
  
}

# ---------------------------------------------------------------------------

# test_functions ----------------------------------------------------------

combi <- readRDS(database <- "C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database/subse_preci_datble1.RDS")
comb_data <- rbindlist(combi)

path_shp <- "C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/shapefiles/SPH_KRAJ.shp"


plot_timeseries(comb_data)
plot_seasonality(comb_data)
plot_annmean(comb_data, path_shp)
plot_annstd(comb_data, path_shp)
