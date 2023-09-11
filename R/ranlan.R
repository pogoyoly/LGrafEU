library(NLMR)
library(ambient)
library(raster)


################################################
#generate aglim map
aglim_gen <- NLMR::nlm_randomcluster(nrow = 200,
                                          ncol = 200,
                                          p    = 0.5,
                                          ai   = c(0.9, 0.1, 0, 0),
                                          rescale = FALSE)

plot(aglim_gen)
res(aglim_gen)



#################################################
#generate soil map
soil_gen <- NLMR::nlm_randomcluster(nrow = 200,
                                     ncol = 200,
                                     p    = 0.5,
                                     ai   = c(.5,.5),
                                     rescale = FALSE)

plot(soil_gen)






####################################################################
#generate slope map


# Set the parameters
map_width <- 200  # Width of the Perlin noise map
map_height <- 200  # Height of the Perlin noise map
cell_size <- 1  # Cell size (distance between neighboring pixels)

# Generate Perlin noise map
perlin_map<-noise_perlin(
  dim = c(map_width, map_height),
  frequency = .01,
  octaves = 3,
  lacunarity = 1,
)
perlin_map <-normalise(perlin_map, from = range(perlin_map), to = c(0, 300))

# Function to calculate the slope
calculate_slope <- function(map, x, y, cell_size) {
  # Get neighboring pixel values
  top <- map[x, y - cell_size]
  bottom <- map[x, y + cell_size]
  left <- map[x - cell_size, y]
  right <- map[x + cell_size, y]

  # Calculate height differences
  dx <- (right - left) / cell_size
  dy <- (bottom - top) / cell_size

  # Calculate slope
  slope <- sqrt(dx^2 + dy^2)

  return(slope)
}

# Create a matrix to store slope values
slope_map <- matrix(NA, nrow = map_width, ncol = map_height)

# Calculate slope for each pixel
for (i in 2:(map_width-1)) {
  for (j in 2:(map_height-1)) {
    slope_map[i, j] <- calculate_slope(perlin_map, i, j, cell_size)
  }
}


# Set the cell size
cell_size_x <- 1  # Cell size in the x direction
cell_size_y <- 1  # Cell size in the y direction

# Create the raster
raster_data <- raster(slope_map, xmn = 0, xmx = cell_size_x * ncol(slope_map),
                      ymn = 0, ymx = cell_size_y * nrow(slope_map))

# Set the cell size
res(raster_data) <- c(cell_size_x, cell_size_y)
plot(raster_data)
slope_gen <- reclassify(raster_data, c(0,5,1, 5,10,2, 10,90,3), include.lowest=F)
plot(slope_gen)

#writeRaster(slope_gen, "C://Users//Eyal//Documents//PhD//antonia//slope2.tif", overwrite = TRUE)
