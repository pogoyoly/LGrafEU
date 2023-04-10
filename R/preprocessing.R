##################################
#   PRE PROCESSING
##################################
library(raster)
library(dplyr)
library(terra)

#all maps should be normalized to a resolution of 100 x 100

################################################################################
#processing for lc data
#landcover data
landcov <- raster("data/lc_ls.tif", crs = "EPSG:25832")

#reclassify landcover data:
landcov_df <- as.data.frame(landcov, xy = TRUE)

landcov_df <- landcov_df %>%                               # Replacing values
  mutate(lc_ls = replace(lc_ls, lc_ls >=0 & lc_ls <=10, 1), # 1 not relevant
         lc_ls = replace(lc_ls, lc_ls >=13 & lc_ls <=16, 1),
         lc_ls = replace(lc_ls, lc_ls >=29 & lc_ls <=44, 1),
         lc_ls = replace(lc_ls, lc_ls >=11 & lc_ls <=12, 2), # arable land
         lc_ls = replace(lc_ls, lc_ls == 17, 3),
         lc_ls = replace(lc_ls, lc_ls >=25 & lc_ls <=28, 3), # pasture grass woodland shrub more and heathland
         lc_ls = replace(lc_ls, lc_ls == 18, 4),             # annual permanent crops
         lc_ls = replace(lc_ls, lc_ls >=19 & lc_ls <=24, 5)) # agro forestry forest mixed forest coniferous and broadleave forst


landcov_df

landcov1 <- rasterFromXYZ(landcov_df)
plot(landcov1, main = "landcover (lower saxony)")




################################################################################
#processing for slope data
#slope data
dem <- raster("data/dem_ls.tif", crs = "EPSG:25832")
slope = terrain(dem,'slope', unit = 'degrees', neighbors = 8, filename = 'slope.tif', overwrite = T)
plot(slope)
slope <- reclassify(slope, c(0,10,1, 10,90,2), include.lowest=F)
slope[slope == 0 ] <- NA
slope <- aggregate(slope, fact=4, fun=max)
origin(slope)<-c(0,0)

################################################################################
#processing for soil data

#soil texture data
texture <- raster("data/texture_ls.tif", crs = "EPSG:25832")

reclass_table <- matrix(c(1,5,2,4,3,4,4,2,5,2,6,4,7,3,8,2,9,2,10,1,11,1,12,1), ncol = 2, byrow = TRUE)
texture <- reclassify(texture, reclass_table)
texture <- disaggregate(texture, fact= 5)
plot(texture)

################################################################################
#processing for aglim data
aglim <- raster("data/aglim_ls.tif", crs = "EPSG:25832")
aglim[aglim == 0] <- 13
aglim <- disaggregate(aglim, fact=10)
aglim <- crop(aglim,slope)





