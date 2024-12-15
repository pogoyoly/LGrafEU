


#########################################################
#' Inward facing function
#'
#' @param rst a raster of either soil aglim or slope
#' @param landcover a categorized landcover map
#'
#' @importFrom dplyr group_by summarise mutate n arrange
#'
#' @noRd
#'
confuslr <- function(rst, landcover) {
  # Calculate the number of random points
  trf_val <- ncol(rst) * nrow(rst) / 10
  points <- terra::spatSample(rst, size = trf_val, method = "random", as.points = TRUE)

  # Extract soil and landcover values at the sampled points
  soil_points <- terra::extract(rst, points)
  soil_points <- soil_points[2]
  lc_points <- terra::extract(landcover, points)
  lc_points <- lc_points[2]

  # Combine extracted points into a data frame and remove any rows with NA values
  all_points <- data.frame(soil_points = soil_points, lc_points = lc_points)

  all_points <-na.omit(all_points)

  colnames(all_points) <- c("soil_points", "lc_points")

  #set empty global variables
  cnt <- freq <- NULL

  # Group by soil_points and lc_points
  grouped_points <- dplyr::group_by(all_points, soil_points, lc_points)

  # Summarize to get the count
  summarized_points <- dplyr::summarise(grouped_points, cnt = dplyr::n(), .groups = "drop")

  # Mutate to calculate the frequency
  mutated_points <- dplyr::mutate(summarized_points,
                                  freq = round(cnt / sum(cnt), 3))

  # Arrange by descending frequency
  arranged_points <- dplyr::arrange(mutated_points, dplyr::desc(freq))

  # Mutate to convert lc_points to factor
  confusion_matrix <- dplyr::mutate(arranged_points, lc_points = as.factor(lc_points))


  # results
  confusion_matrix$lc_points <- factor(confusion_matrix$lc_points, levels = c( 0, 1, 2, 3, 4, 5))
  # Prepare the transition matrix
  transition2 <- matrix(0, nrow = 14, ncol = 6, dimnames = list(0:13, 0:5))

  # Create the main confusion matrix (matrix `a`)
  # Filter out NA values and get unique sorted levels for soil_points and lc_points
  soil_levels <- sort(unique(confusion_matrix$soil_points))
  lc_levels <- sort(unique(confusion_matrix$lc_points))

  # Create the matrix with correct dimensions and dimnames
  a <- matrix(0,
              nrow = length(soil_levels),
              ncol = length(lc_levels),
              dimnames = list(soil_levels, lc_levels))

  # Assign frequency values to matrix `a`
  soil_indices <- na.omit(as.numeric(factor(confusion_matrix$soil_points, levels = rownames(a))))
  lc_indices <- na.omit(as.numeric(factor(confusion_matrix$lc_points, levels = colnames(a))))
  a[cbind(soil_indices, lc_indices)] <- confusion_matrix$freq

  # Map `a` values into `transition2` based on matching rows and columns
  cols <- colnames(transition2)[colnames(transition2) %in% colnames(a)]
  rows <- rownames(transition2)[rownames(transition2) %in% rownames(a)]
  transition2[rows, cols] <- a[rows, cols]

  scale_rows_to_one <- function(mat) {
    if (!is.matrix(mat)) {
      stop("Input must be a matrix.")
    }

    # Compute row sums
    row_sums <- rowSums(mat)

    # Avoid division by zero
    row_sums[row_sums == 0] <- 1

    # Scale rows to sum to 1
    scaled_mat <- mat / row_sums

    return(scaled_mat)
  }

  transition2 <- scale_rows_to_one(transition2)

  return(transition2)
}


#' Inward facing function
#'
#' @param transition the transition matrix from the confus function
#' @param rst the raster from which the transition occurs
#' @param arabel_val value of arable fields
#'
#'
#' @noRd
#'
#'
translr <- function(transition, rst, arabel_val) {
  tran <- transition
  transform <- function(x) {
    # Initialize result vector
    result <- numeric(nrow(x))

    for (j in 1:nrow(x)) {
      cell_value <- x[j, 1]
      z <- 0
      tranz <- 0

      if (!is.na(cell_value)) {
        dice <- runif(1, min = 0, max = 1)
        for (i in 1:ncol(tran)) {
          if (isTRUE(dice >= tranz)) {
            tranz <- tranz + tran[cell_value + 1, i]
          }
          if (isTRUE(dice < tranz)) {
            val <- i - 1
            if(val == arabel_val){
              z <- 1

            }
            else{
              z <- 0
            }

            tranz <- - 1
            # <- 1
            break
          }
        }
      }

      # Store the result for this cell
      result[j] <- z
    }

    return(result)
  }

  # Apply the transform function, which now handles blocks
  transformed_reclassify <- terra::app(rst, fun = transform)

  return(transformed_reclassify)
}


#' A one layer transformation that allows the expansion of the potential space to
#' areas with submoptimal landscapes based on a co-variant probability
#'
#' @param rast A type feature raster
#' @param landcover A landcover raster
#' @param aggregation A number that defines how aggregated he potential space will be
#' @param arabel_val the value of arable fields in your landcover raster
#'
#' @export
#'
#'@examples
#'set.seed(123)
#'original_potential_space<-generate_perlin_noise(200,200,1,2,3,0.002,TRUE,
#'                                                "land_percentage", percetange = 50)
#'corresponding_fields<-establish_by_place_conquer(potential_space= original_potential_space,
#'                                                 cell_size=1,
#'                                                 includsion_value = 1,
#'                                                 mean_field_size = 200,
#'                                                 sd_field_size = 100,
#'                                                 distribution = "norm",
#'                                                 mean_shape_index = 3,
#'                                                 sd_shape_index = 0.3,
#'                                                 percent = 95,
#'                                                 assign_farmers = TRUE,
#'                                                 assign_mode = 2,
#'                                                 mean_fields_per_farm = 3,
#'                                                 sd_fields_per_farm = 3)
#'
#'map<-return_by_arable_land(corresponding_fields, method =2)
#'set.seed(123)
#'modified_potential_space<-generate_perlin_noise(200,200,1,2,3,0.002,TRUE,
#'                                                "land_percentage", percetange = 48)
#'
#'result<-LGrafEU::trans_1lr(modified_potential_space,map,2, arabel_val = 1)
#'par(mfrow=c(2,1))
#'terra::plot(original_potential_space)
#'terra::plot(modified_potential_space)
#'
#'
#'
#'
trans_1lr <- function(rast, landcover, aggregation, arabel_val = 1) {


  target_value <- arabel_val

  classified_landcover <- terra::app(landcover, function(x) ifelse(x == target_value, 1, 0))


  # Calculate the confusion matrix (assuming `confus` is a function defined elsewhere)
  con_mat <- confuslr(rast, classified_landcover)

  # Apply the transition function (assuming `trans` is defined elsewhere)
  trans_rast <- translr(con_mat, rast, arabel_val)

  # Aggregate the raster with modal function in terra
  #trans_rast <- terra::aggregate(trans_rast, fact = aggregation, fun = "max", na.rm = FALSE)

  # Disaggregate to return to original resolution
  #trans_rast <- terra::disagg(trans_rast, fact = aggregation)

  # Ensure extents match
  terra::ext(trans_rast) <- terra::ext(rast)

  majority_rule <- function(values) {
    # Exclude NA values and get the most frequent value
    if (all(is.na(values))) return(NA)
    return(as.numeric(names(which.max(table(values, useNA = "no")))))
  }

  is_odd <- aggregation %% 2 == 1
  if(is_odd == FALSE){
    aggregation <- aggregation + 1
  }

  terra::plot(trans_rast)
  fac2 <- aggregation * 2 -1
  # Apply the majority rule using a 3x3 moving window
  r_majority <- terra::focal(trans_rast, w = matrix(1, fac2, fac2), fun = majority_rule)
  r_majority <- terra::focal(r_majority, w = matrix(1, aggregation, aggregation), fun = max)

  return(r_majority)
}




