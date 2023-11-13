
#library(NLMR)
#library(ambient)
#library(raster)


################################################
#generate aglim map

#' Natural soil cluster generator for artificial aglim and soil texture maps
#'
#' @param rows
#' @param cols
#' @param p
#' @param ai
#' @param rescale
#'
#' @details
#' This is a wramper around the nlmr random cluster function to create naturalist soil raster
#'
#'
#' @return
#' @export
#'
#'
generate_soil <- function(rows,cols,p,ai,rescale){

                                          NLMR::nlm_randomcluster(nrow = rows,
                                          ncol = cols,
                                          p    = p, #0.5
                                          ai   = ai, #c(0.9, 0.1, 0, 0),
                                          rescale = rescale) # FALSE)
}

#plot(aglim_gen)
#res(aglim_gen)


#' A natural agriculture cluster map for creating artificial potential agricultural landscape
#'
#' @param rows
#' @param cols
#' @param p f
#' @param ai percentage of each category of landscape
#' @param rescale
#'
#' @description
#' This is a wramper around the nlmr random cluster function to create naturalist potential agriculture space raster
#'
#'
#' @return
#' @export
#'
#' @examples
generate_potential_landscape <- function(rows,cols,p,ai,rescale){

  NLMR::nlm_randomcluster(nrow = rows,
                          ncol = cols,
                          p    = p, #0.5
                          ai   = ai, #c(0.9, 0.1, 0, 0),
                          rescale = rescale) # FALSE)
}





####################################################################
#generate slope map

#' A perlin noise based slope generator
#'
#' @param width
#' @param height
#' @param cellSize
#' @param frequency
#' @param octaves
#' @param lacunarity
#' @param categorized TRUE/FLASE tells you if the slope raster returns categorized or smooth
#'
#' @return
#' @export
#'
#'
#'
generate_slope<-function(width, height,cellSize, frequency, octaves, lacunarity, categorized){

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
raster_data <- raster(slope_map, xmn = 0, xmx = cell_size_x * ncol(slope_map),
                      ymn = 0, ymx = cell_size_y * nrow(slope_map))

# Set the cell size
res(raster_data) <- c(cell_size_x, cell_size_y)
slope_gen <- reclassify(raster_data, c(0,5,1, 5,10,2, 10,90,3), include.lowest=F)

if(categorized == TRUE){
  return(slope_gen)

}
if(categorized == FALSE){
  return(raster_data)
}
}

#test<-LGrafEU::generate_slope(200,200,1,0.01,2,5, TRUE)
#raster::plot(test)
