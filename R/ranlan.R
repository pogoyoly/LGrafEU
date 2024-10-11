

####################################################################
#generate perlin noise map

#' A perlin noise based potential space map generator
#'
#' @param width width of output raster
#' @param height height of output raster
#' @param cellSize cellsize of output raster
#' @param frequency perlin frequency
#' @param octaves perlin octave
#' @param lacunarity parlin lacunarity
#' @param categorized TRUE/FLASE tells you if the slope raster returns categorized or smooth
#' @param cat_method either by slope_lim or land_percentage
#' @param lim the slope degree cutoff for portential space where 1 will be potential space and 2 non potential space
#' @param percetange
#'
#' @return
#' @export
#' @import checkmate ambient raster
#'
#' @examples
#' test<-generate_perlin_noise(200,200,1,2,3,0.01,TRUE, "land_percentage", percetange = 75)
#' raster::plot(test)
#'
#'
#'
#'
#'
generate_perlin_noise<-function(width, height,cellSize, frequency, octaves, lacunarity, categorized,cat_method, lim = 0, percetange = 0){

#check function arguments
checkmate::assert_count(width, positive = TRUE)
checkmate::assert_count(height, positive = TRUE)
checkmate::assert_numeric(cellSize)
checkmate::assert_numeric(frequency)
checkmate::assert_numeric(octaves)
checkmate::assert_numeric(lacunarity)
checkmate::assert_logical(categorized)

  # Set the parameters
map_width <- width  # Width of the Perlin noise map
map_height <- height  # Height of the Perlin noise map
cell_size <- cellSize  # Cell size (distance between neighboring pixels)

# Generate Perlin noise map
perlin_map<-ambient::noise_perlin(
  dim = c(map_width, map_height),
  frequency = frequency,
  octaves = octaves,
  lacunarity = lacunarity,
)
perlin_map <-ambient::normalise(perlin_map, from = range(perlin_map), to = c(0, 300))

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
raster_data <- raster::raster(slope_map, xmn = 0, xmx = cell_size_x * ncol(slope_map),
                      ymn = 0, ymx = cell_size_y * nrow(slope_map))

# Set the cell size
raster::res(raster_data) <- c(cell_size_x, cell_size_y)
raster_data[is.na(raster_data[])] <- 1

if(cat_method == "slope_lim"){
slope_gen <- raster::reclassify(raster_data, c(0,lim,1, lim,90,2), include.lowest=F)
}
if(cat_method == "land_percentage"){


    # Flatten the slope map to a vector
    slope_values <- raster::values(raster_data)

    # Remove NA values if any
    slope_values <- slope_values[!is.na(slope_values)]

    # Sort the slope values in ascending order
    sorted_slope_values <- sort(slope_values)

    # Calculate the cutoff index based on the desired percentage
    cutoff_index <- ceiling((percetange / 100) * length(sorted_slope_values))

    # Determine the cutoff slope value
    slope_cutoff <- sorted_slope_values[cutoff_index]

    slope_gen <- raster::reclassify(raster_data, c(0,slope_cutoff,1, slope_cutoff,90,2), include.lowest=F)
}

if(categorized == TRUE){
  return(slope_gen)

}
if(categorized == FALSE){
  return(raster_data)
}
}

#test<-LGrafEU::generate_slope(200,200,1,0.01,2,5, TRUE)
#raster::plot(test)
