

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
#' @param percetange if cat_method is land_percentage then the percentage of potential space is defined by this arg
#'
#' @export
#' @import checkmate raster
#' @importFrom ambient noise_perlin normalise
#'
#' @examples
#' test<-generate_perlin_noise(200,200,1,2,3,0.01,TRUE, "land_percentage", percetange = 75)
#' raster::plot(test)
#'
#'
#'
#'
#'
generate_perlin_noise <- function(width, height, cellSize, frequency, octaves, lacunarity, categorized, cat_method, lim = 0, percetange = 0) {

  # Check function arguments
  checkmate::assert_count(width, positive = TRUE)
  checkmate::assert_count(height, positive = TRUE)
  checkmate::assert_numeric(cellSize)
  checkmate::assert_numeric(frequency)
  checkmate::assert_numeric(octaves)
  checkmate::assert_numeric(lacunarity)
  checkmate::assert_logical(categorized)

  # Set the parameters
  map_width <- width
  map_height <- height
  cell_size <- cellSize

  # Generate Perlin noise map
  perlin_map <- ambient::noise_perlin(
    dim = c(map_width, map_height),
    frequency = frequency,
    octaves = octaves,
    lacunarity = lacunarity
  )
  perlin_map <- ambient::normalise(perlin_map, from = range(perlin_map), to = c(0, 300))

  # Function to calculate the slope
  calculate_slope <- function(map, x, y, cell_size) {
    top <- map[x, y - cell_size]
    bottom <- map[x, y + cell_size]
    left <- map[x - cell_size, y]
    right <- map[x + cell_size, y]

    dx <- (right - left) / cell_size
    dy <- (bottom - top) / cell_size
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

  # Create the raster from the slope matrix
  rast_data <- terra::rast(slope_map)
  plot(rast_data)
  # Create an extent object
  ext <- terra::ext(0, cell_size * ncol(slope_map), 0, cell_size * nrow(slope_map))

  # Assign the extent to the raster
  terra::ext(rast_data) <- ext

  # Set resolution (cell size)
  #terra::res(rast_data) <- cell_size
  #rast_data <- terra::subst(rast_data, NA, 1)
  rast_data <- terra::ifel(is.na(rast_data), 1, rast_data)


  # Reclassification based on slope limit
  if (cat_method == "slope_lim") {
    m <- c(0, lim, 1,
           lim, 90, 2)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)

    slope_gen <- terra::classify(rast_data, rcl = rclmat, include.lowest = TRUE)
  }

  # Reclassification based on land percentage
  if (cat_method == "land_percentage") {
    slope_values <- values(rast_data, na.rm = TRUE)
    sorted_slope_values <- sort(slope_values)
    cutoff_index <- ceiling((percetange / 100) * length(sorted_slope_values))
    slope_cutoff <- sorted_slope_values[cutoff_index]

    m <- c(0, slope_cutoff, 1,
           slope_cutoff, 90, 2)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)


    slope_gen <- terra::classify(rast_data, rcl = rclmat, include.lowest = FALSE)
  }

  # Return categorized or raw slope raster
  if (categorized == TRUE) {
    return(slope_gen)
  } else {
    return(rast_data)
  }
}
