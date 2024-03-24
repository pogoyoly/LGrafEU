

#' Heuristic field establishment based on simple geometries
#'
#' @param potential_space a map containing potential space for field establishment
#' @param mean_field_size mean field size
#' @param sd_field_size sd field size
#' @param mean_shape_index mean shape index
#' @param sd_shape_index sd shape index
#' @param percent percent of the potential arabale land that will be filled ranging from 0-100
#' @param assign_farmers TRUE/FALSE to assign fields to random farmers
#' @param mean_fields_per_farm mean number of fields owned by each farmer
#'
#' @return
#' @export
#' @import raster dplyr
#'
#' @examples
establish_by_space<-function(potential_space,
                             road_raster,
                             use_road_raster,
                             mean_field_size,
                             sd_field_size,
                             mean_shape_index,
                             sd_shape_index,
                             percent,
                             assign_farmers,
                             mean_fields_per_farm
){

  achieved_percent <- 0
  potential_space[is.na(potential_space[])] <- 0
  land = matrix(0, nrow(potential_space), ncol(potential_space))
  land_raster<-raster::raster(land)
  field_num <- 1
  field_list <- list()
  if(use_road_raster == TRUE){
    road_raster[is.na(road_raster[])] <- 0
    road_inside<-road_raster
  }
  if(use_road_raster == FALSE){
    road_inside = matrix(0, nrow(potential_space), ncol(potential_space))
    road_inside<-raster::raster(road_inside)

  }

  while(achieved_percent < percent){
    field_placed <- FALSE
    while(field_placed == FALSE){
      #set field size and shape index
      field_size <- max(1, round(rnorm(1, mean=mean_field_size, sd=sd_field_size)))
      shape_index <- rnorm(1, mean=mean_shape_index, sd=sd_shape_index)
      if(shape_index <= 0){
        shape_index<-0.1
      }
      if(field_size <= 0){
        field_size<-1
      }
      field_row_size <- ceiling(shape_index * sqrt(field_size))
      field_col_size <- ceiling(field_size / field_row_size)

      #create empty matrix for fields

      check<-0

      while(check != 2){

        #choose random start location and check if it can be used
        start_row <- sample(1:ncol(potential_space), 1)
        start_col <- sample(1:nrow(potential_space), 1)
        if(is.na(potential_space[start_row,start_col]) == FALSE){
          check<-potential_space[start_row,start_col]
        }
      }


      directions <- c("a", "b", "c", "d")
      direction<-sample(directions,1)

      switch(direction,
             "a" = {
               row_range <- seq(from = start_row, to = start_row - field_row_size)
               col_range <- seq(from = start_col, to = start_col - field_col_size)
             },
             "b" = {
               row_range <- seq(from = start_row, to = start_row - field_row_size)
               col_range <- seq(from = start_col, to = start_col + field_col_size)
             },
             "c" = {
               row_range <- seq(from = start_row, to = start_row + field_row_size)
               col_range <- seq(from = start_col, to = start_col - field_col_size)
             },
             "d" = {
               row_range <- seq(from = start_row, to = start_row + field_row_size)
               col_range <- seq(from = start_col, to = start_col + field_col_size)
             },
      )


      #make sure that the col and row are inside the bound
      if(dplyr::first(row_range) <= 0 | dplyr::first(row_range) > nrow(potential_space) |
         dplyr::last(row_range) <= 0 | dplyr::last(row_range) > nrow(potential_space) |
         dplyr::first(col_range) <= 0 | dplyr::first(col_range) > ncol(potential_space) |
         dplyr::last(col_range) <= 0 | dplyr::last(col_range) > ncol(potential_space)){
        next

      }
      #expand field to one direction and check if can be established
      df = expand.grid(a = row_range, b = col_range)

      #if not try next direction


      #
      #check if any place sits on another farm
      coverlap<-function(a,b){
        if(land[a,b] != 0){
          return("overlap")
        }
      }

      coverlap_raods<-function(a,b){
        if(is.na(road_inside[a,b]) == TRUE || road_inside[a,b] != 0){
          return("overlap")
        }
      }


      val<- potential_space[round(mean(row_range)),round(mean(col_range))]
      qq <- mapply(coverlap, df$a, df$b, SIMPLIFY = TRUE)
      qr <-mapply(coverlap_raods, df$a, df$b, SIMPLIFY = TRUE)
      if ("overlap" %in% qq || "overlap" %in% qr || is.na(val) == TRUE || val != 2) {
        direction<-sample(directions,1)
        #print("repeat")
      } else {
        for(i in 1:length(row_range)){
          for(j in 1:length(col_range)){
            land[row_range[i], col_range[j]] <- 1
          }
        }

        #print("no repeat")
        field_placed <- TRUE
      }




    }
    land_raster<-raster::raster(land)
    #plot(land_raster)


    #write the information of the field in a field object and add it to the field list
    field_obj <- new("Field", number = field_num, location = list(row_range,col_range), farmer = 1)
    field_list<-c(field_list,field_obj)
    field_num <- field_num + 1

    #calculate how much of the space has been filled
    part_potential <- length(which(values(potential_space) %in% c (2)))
    part_filled <- length(which(values(land_raster) %in% c (1)))
    achieved_percent <- (part_filled / part_potential) * 100
    print(achieved_percent)


  }

  #distribute fields between farmers
  if(assign_farmers == TRUE){
    num_fields<-length(field_list)
    num_farmers<-round(num_fields/mean_fields_per_farm)
    for(i in 1:num_fields){
      farmer_num<-sample(1:num_farmers, 1)
      field_list[[i]]@farmer <- farmer_num
    }
  }

  result<-list(map = land_raster, field_list = field_list)

  return(result)
}

