# Basic libraries needed - don't modify this for a library just used in one/two scripts

if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'DescTools',
               'ggplot2', 'ggalt', 'ggExtra',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales')

library(data.table)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)
library("rnaturalearth")
library("rnaturalearthdata")
library(getPass)
library(stringr)
library(ncdf4)
library(raster)
library(sf)
library(sp)
library(rgdal)
library(viridis)
library(lubridate)
library(parallel)
library(pryr)