crop_data <- function(folder_path, name, start_year, end_year, shapefile_path, name_shp){
  if (!is.character(folder_path)) stop ("folder_path should be a character string.")
  folder_path <- paste0(folder_path, "/", name, ".Rds")
  name <- readRDS(folder_path)
  shapefile_path <- paste0(shapefile_path, "/", name_shp, ".shp")
  name_shp <- readOGR(shapefile_path)
  bound <- st_bbox(name_shp)
  name_sub <- name[between(x, ((bound[1])-1), ((bound[3])+1)) & between(y, ((bound[2])-1), ((bound[4])+1))]
  name_sub <- name_sub[year(Z) >= start_year & year(Z) <= end_year]
  coordinates(name_sub) <- ~ x + y
  proj4string(name_sub) <- proj4string(name_shp)
  name_sub <- name_sub[!is.na(over(name_sub, as(name_shp, "SpatialPolygons"))), ]
  name_sub <- as.data.table(name_sub)
  saveRDS(name_sub, paste0(folder_path, "/../../database/subset_data.Rds"))
  
  #spatial_plots_mean_sd
  name_sub <- name_sub[, ':='(mon = month(Z), year = year(Z))]
  name_sub <- name_sub[, year_val := sum(value), by = .(year, x, y)]#annual sum for each grid and year
  name_sub <- name_sub[, ':='(anl_mean = mean(year_val), anl_std = sd(year_val)),  by = .(x, y)] #annual mean precipitation and sd for each grid
  shp <- fortify(name_shp)
  ggplot() +
    geom_raster(data = name_sub, aes(x, y, fill = anl_mean)) + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = -1) + 
    labs(x = "Longitude", y = "Latitude") + 
    ggtitle("Annual mean precipitation (mm/yr)") + 
    theme_small + 
    geom_path(data = shp, 
              aes(x = long, y = lat, group = group))
  
  ggsave(paste0(folder_path, "/../../database/Ann_mean_plot.png"), width = 7.2, height = 5.3, units = "in", dpi = 600)
  
  ggplot() +
    geom_raster(data = name_sub, aes(x, y, fill = anl_std)) + 
    coord_fixed(ratio = 1) +
    scale_fill_viridis(direction = -1) + 
    labs(x = "Longitude", y = "Latitude") + 
    theme_small + 
    geom_path(data = shp, 
              aes(x = long, y = lat, group = group))
  
  ggsave(paste0(folder_path, "/../../database/Stnd_dev.png"), width = 7.2, height = 5.3, units = "in", dpi = 600)
  
  #temporal_plots(regional average)
  name_sub <- name_sub[, reg_val := mean(value), by = .(mon, year)] #mean of all grids for each month & year wise
  name_sub <- name_sub[, ':='(reg_monmean = mean(reg_val), reg_monstd = sd(reg_val)), by = mon] #mean monthly regional precip and std
  saveRDS(name_sub, paste0(folder_path, "/../../database/subset_plot.Rds"))
  
  ggplot(name_sub, aes(Z, reg_val)) + 
    geom_line() + 
    geom_point() + 
    labs(x = "Month", y = "Precipitation (mm)") + 
    theme_generic
  ggsave(paste0(folder_path, "/../../database/Monthly_line_plot.png"), width = 7.2, height = 5.3, units = "in", dpi = 600)
  
  name_sub$mon <- factor(name_sub$mon)
  
  box_plot <- ggplot(name_sub, aes(mon, reg_val)) + 
    geom_boxplot() + 
    labs(x = "Month", y = "Precipitation (mm)") + 
    theme_generic
  ggsave(paste0(folder_path, "/../../database/box_plot.png"), width = 7.2, height = 5.3, units = "in", dpi = 600)
  
  ggplot(name_sub, aes(factor(mon), reg_monmean)) + 
    geom_bar(stat="identity", position=position_dodge(), alpha=0.5) + 
    geom_errorbar(aes(ymin = reg_monmean - reg_monstd, 
                      ymax = reg_monmean + reg_monstd), width = .4, 
                  position=position_dodge(width=0.90)) + 
    labs(x = "Month", y = "Precipitation (mm/month)") + 
    theme_generic
  ggsave(paste0(folder_path, "/../../database/bar_plot.png"), width = 7.2, height = 5.3, units = "in", dpi = 600)
  
  
  ggplot(name_sub, aes(month(Z), reg_val)) + 
    geom_line() + 
    geom_point() + 
    facet_wrap(~year, scales = 'free') + 
    scale_x_discrete(limits = factor(c(1:12))) + 
    theme_bw()
  ggsave(paste0(folder_path, "/../../database/facet_plot.png"), width = 7.2, height = 5.3, units = "in", dpi = 600)
  
}

#####################################################

# test --------------------------------------------------------------------
database <- "C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database/"
shp_path <- "C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/shapefiles/"

crop_data(database, "cmap", 2001, 2010, shp_path, "SPH_KRAJ")

crop_data(database, "precl", 2001, 2010, shp_path, "India_state")

#####################################################






cma <- readRDS("C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database/gpm_imergm.RDS")
pre <- readRDS("C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database/precl.RDS")

shapefile_path <- "C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/shapefiles/Czech/SPH_KRAJ.shp"

dsn <- "C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/shapefiles/Czech/SPH_KRAJ.shp"

wa.map <- readOGR(dsn)

shp_path <- "C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/shapefiles/Czech/"

database2 <- "C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/cz_pilot"

############################

files = list.files(path = database, pattern = ".Rds")
dat_list =   lapply(files, function (x) data.table(readRDS(x)))



files = list.files(path = database2, pattern = ".Rds")
folder_path <- paste0(database, "/", files)
name <- readRDS(folder_path)

select_data2 <- function(folder_path, name){
  if (!Reduce("&", is.element(name, c("20cr", "all", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "gpm_imergm", "ncep_ncar", "ncep_doe", "precl", "trmm_3b43", "udel")))){
    stop("Error: Data set not supported. Select from 20cr, cmap, cpc, cru_ts, ghcn, gpcc, gpcp, gpm_imergm, ncep_ncar, ncep_doe, precl, trmm_3b43, udel")
  }
  if (name == "all") name <- c("cmap", "gpcc", "gpcp", "precl")
  name <- paste0(folder_path, "/", name, ".Rds") %>% as.list() 
  
}

daat2 <- select_data2("datbase", "cmap")


folder_path <- paste0(database, "/", daat)
dat_list2 <- lapply(folder_path, readRDS)



files = list.files(path = database2, pattern = ".Rds")
folder_path <- paste0(database, "/", files)
dat_list2 <- lapply(folder_path, readRDS)

########################

df <- list.files("database", pattern = ".RDS")

dat_list2 <- list(cma, pre)

shapefile_path <- paste0(shapefile_path, "/", name_shp, ".shp")
name_shp <- readOGR(shapefile_path)
bound <- st_bbox(name_shp)

reed <- lapply(dat_list2, function(i) setDT(i)
               [between(x, ((bound[1])-1), ((bound[3])+1)) & 
                                                 between(y, ((bound[2])-1), ((bound[4])+1))]
               [year(Z) >= 2002 & year(Z) <= 2004])
               
precipe <- lapply(reed, function(i) {
  sp::coordinates(i) <- ~ x + y 
  proj4string(i) <- proj4string(wa.map)
  i
})

subse_preci <-  lapply(precipe, function(i) i[!is.na(over(i, as(wa.map, "SpatialPolygons"))), ])   

subse_preci_datble <- lapply(subse_preci, as.data.table)

#data_preparation for plots

subse_preci_datble2 <- lapply(subse_preci_datble, function(i) setDT(i)
                              [, ':='(mon = month(Z), year = year(Z))]
                              [, year_val := sum(value), by = .(year, x, y)]
                              [, ':='(anl_mean = mean(year_val), anl_std = sd(year_val)),  by = .(x, y)]
                              [, reg_meanmon := mean(value), by = .(mon, year)])

comb_table <- rbindlist(subse_preci_datble2)

#############################################################################

#plots
data_comb <- readRDS("C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database/combi_precip.RDS")
subset <- readRDS("C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database/subset_plot.RDS")

subse_preci_datble <- readRDS("C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database/subse_preci_datble.RDS")
combi_precip_list <- readRDS("C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database/combi_precip_list.RDS")

subse_preci_datble <- readRDS("C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database/subse_preci_datble2.RDS")

mean_all <- subse_preci_datble[, ':='(value = mean(value), name = factor('average')), by = .(x,  y, Z)]

mean_all2 <- subse_preci_datble[, .(value = mean(value), name = factor("avearge")), by = c("x", "y", "Z")]
subse_preci_datble <- rbindlist(list(subse_preci_datble, mean_all))



subse_preci_datble2 <- readRDS("C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database/subse_preci_datble.RDS")

subse_preci_datble2

subse_preci_datble2 <- rbindlist(subse_preci_datble2)

mean_all <- subse_preci_datble2[, .(value = mean(value), name = factor("avearge")), by = c("x", "y", "Z")]
subse_preci_datble <- rbindlist(list(subse_preci_datble2, mean_all))

subse_preci_datble <- split(subse_preci_datble, subse_preci_datble$name)

#try <- rbindlist(subse_preci_datble)

subse_preci_datble2 <- lapply(subse_preci_datble, function(i) setDT(i)
                              [, ':='(mon = month(Z), year = year(Z))]
                              [, year_val := sum(value), by = .(year, x, y)]
                              [, ':='(anl_mean = mean(year_val), anl_std = sd(year_val)),  by = .(x, y)]
                              [, reg_val := mean(value), by = .(mon, year)]
                              [, ':='(reg_monmean = mean(reg_val), reg_monstd = sd(reg_val)), by = mon])




combi_precip <- rbindlist(subse_preci_datble2)



data_comb

data_comb1 <- data_comb[, .(x, y, Z, value, name)]
data_comb1

mean_all <- data_comb[, .(value = mean(value), name = factor("avearge")), by = c("x", "y", "Z")]

unique(data_comb, by = c("x", "y", "Z"))


comb <- rbindlist(list(data_comb1, mean_all))

str(comb)


unique(subset$reg_monmean)

precl <- data_comb[name == "precl"]

precl2 <- precl[, ':='(reg_meanmon2 = mean(reg_meanmon), reg_meanmon_std2 = sd(reg_meanmon)), by = mon]



data_comb2 <- data_comb[, mean:= mean(reg_val), by = mon]
data_comb3 <- data_comb[, mean:= mean(value), by = .(name, Z)]


library(data.table)
iris <- as.data.table(iris)

iris.num[, colSums(iris.num)]

iris[, mean(Petal.Width), (Species = factor('new'))]


ggplot(data_comb2, aes(x = Z)) + 
  geom_line(aes(y = reg_val, color = name), size = 0.8) + 
  geom_line(aes(y = mean), col = "red", size = 0.8) + 
  theme_classic()

ggplot(comb, aes(x = Z)) + 
  geom_line(aes(y = value, color = name, linetype = name), size = 0.8) + 
  scale_linetype_manual(values = c(rep("dashed", 4), "solid")) + 
  #scale_color_manual(values = c("red", "green", "black", "orange", "pink")) + 
  theme_generic

ggplot(comb, aes(x = Z, y = value, color = name, linetype = name), size = 0.8) + 
  geom_line() + 
  scale_linetype_manual(values = c(rep("dashed", 4), "solid")) + 
  #scale_color_manual(values = c("red", "green", "black", "orange", "pink")) + 
  theme_generic



unique(precl2$reg_meanmon_std2)

tri <- unique(data_comb[name == "precl"], by = c("reg_meanmon"))
write.csv(tri, "precl_reg_meanmon.csv")


write.csv(data_comb, "data.cmob.csv")
write.csv(subset, "dsubset.csv")


ggplot() +
  geom_raster(data = data_comb, aes(x, y, fill = anl_mean)) + 
  coord_fixed(ratio = 1) + 
  scale_fill_viridis(direction = -1) + 
  labs(x = "Longitude", y = "Latitude") + 
  facet_wrap(~name, ncol = 2) + 
  ggtitle("Annual mean precipitation (mm/yr)") + 
  theme_small + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_path(data = wa.map, 
            aes(x = long, y = lat, group = group))
ggsave(paste0(database, "/Boxplot_comb.png"), width = 7.2, height = 5.3, units = "in", dpi = 600)


database <- "C:/Users/rkpra/OneDrive/Documents/R_projects/pRecipe/data/database"

ggplot(data_comb, aes(Z, reg_meanmon, color = name)) + 
  geom_line() + 
  geom_point() + 
  labs(x = "Month", y = "Precipitation (mm)") + 
  theme_generic

#bar_plot with error bar and standard deviation
ggplot(data_comb, aes(factor(mon), reg_monmean, fill = name)) + 
  geom_bar(stat="identity", position=position_dodge(), alpha=0.5) + 
geom_errorbar(aes(ymin = reg_monmean - reg_monstd, 
                  ymax = reg_monmean + reg_monstd), width=.4, 
              position=position_dodge(width=0.90)) + 
  theme_generic



write.csv(data_comb[name == "precl"], "data_comb.csv")


#box_plot
ggplot(data_comb, aes(factor(mon), reg_meanmon, fill = name)) + 
  geom_boxplot() + 
  labs(x = "Month", y = "Precipitation (mm)") + 
  theme_generic


ggsave(paste0(folder_path, "/../../database/box_plot.png"), width = 7.2, height = 5.3, units = "in", dpi = 600)


ggplot(data = data_comb[name == "precl"], aes(month(Z), reg_meanmon)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~year, scales = 'free') + 
  scale_x_discrete(limits = factor(c(1:12))) + 
  theme_bw()
ggsave(paste0(folder_path, "/../../database/facet_plot.png"), width = 7.2, height = 5.3, units = "in", dpi = 600)


















name_sub <- as.data.table(name_sub)


namee <- c("20cr", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "udel") %>% as.list()

name <- paste0(database, "/", "gpcp", ".Rds") %>% as.list()
name

dummie_raster <- readRDS(name)
#########################
select_data <- function(folder_path, name){
  if (!Reduce("&", is.element(name, c("20cr", "all", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "gpm_imergm", "ncep_ncar", "ncep_doe", "precl", "trmm_3b43", "udel")))){
    stop("Error: Data set not supported. Select from 20cr, cmap, cpc, cru_ts, ghcn, gpcc, gpcp, gpm_imergm, ncep_ncar, ncep_doe, precl, trmm_3b43, udel")
  }
  if (name == "all") name <- c("20cr", "cmap", "cpc", "cru_ts", "ghcn", "gpcc", "gpcp", "gpm_imergm", "ncep_ncar", "ncep_doe", "precl", "trmm_3b43", "udel")
  name <- paste0(folder_path, "/", name, ".Rds") %>% as.list() 
  return(name)
  
  {
    saveRDS(name, "list.rds")
  }
  
}


