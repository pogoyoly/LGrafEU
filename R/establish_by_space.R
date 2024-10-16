

#' Heuristic field establishment based on simple geometries
#'

#'
#' @param potential_space a raster including a potential space category for field placement
#' @param cell_size cell size for output
#' @param includsion_value inclusion value for the potential space raster
#' @param additional_lim additional road/river/exclusion raster for field exclusion
#' @param mean_field_size mean field size counted by number of cells from a normal distribution
#' @param sd_field_size sd field size counted by number of cells from a normal distribution
#' @param mean_shape_index mean shape index calculated by a relation between width/length of placement
#' @param sd_shape_index sd shape index calculated by a relation between width/length of placement
#' @param percent percent of the potential space to be filled with fields
#' @param assign_farmers TRUE/FALSE for farmer assignment in model
#' @param assign_mode 1 = random assignment 2 = spatially structured assignment
#' @param mean_fields_per_farm mean fields per farmer from a long normal distribution
#' @param sd_fields_per_farm sd fields per farmer from a long normal distribution
#'
#' @return A LGrafEU output object
#' @export
#' @import raster
#'
#' @examples
#' r<-raster::raster(matrix(1, nrow=50, ncol=50))
#' raster::extent(r)<-c(0,100,0,100)
#' test<-establish_by_space(potential_space= r,
#'                          cell_size=2,
#'                          includsion_value = 1,
#'                          mean_field_size = 50,
#'                          sd_field_size = 25,
#'                          mean_shape_index = 1,
#'                          sd_shape_index = 0.5,
#'                          percent = 65,
#'                          assign_farmers = TRUE,
#'                          assign_mode = 2,
#'                          mean_fields_per_farm = 5,
#'                          sd_fields_per_farm = 5)
#' raster::plot(test$map)


establish_by_space<-function(potential_space,
                             cell_size,
                             includsion_value,
                             additional_lim = NA,
                             mean_field_size,
                             sd_field_size,
                             mean_shape_index,
                             sd_shape_index,
                             percent,
                             assign_farmers,
                             assign_mode,
                             mean_fields_per_farm,
                             sd_fields_per_farm
){


  checkmate::assert_numeric(cell_size)
  checkmate::assert_numeric(includsion_value)
  checkmate::assert_numeric(mean_field_size)
  checkmate::assert_numeric(sd_field_size)
  checkmate::assert_numeric(mean_shape_index)
  checkmate::assert_numeric(sd_shape_index)
  checkmate::assert_numeric(percent)
  checkmate::assert_logical(assign_farmers)
  checkmate::assert_true(assign_mode == 1 || assign_mode == 2)
  checkmate::assert_numeric(mean_fields_per_farm)
  checkmate::assert_numeric(sd_fields_per_farm)


  #set land raster on which to save the fields
  potential_space[is.na(potential_space[])] <- 0
  land = matrix(0, nrow(potential_space), ncol(potential_space))
  land_raster<-raster::raster(land)

  #setup place holders
  achieved_percent <- 0
  field_num <- 1
  field_list <- list()


  #add any additional limitations to a seperate raster
  if(is.na(additional_lim) == FALSE){
    additional_lim[is.na(additional_lim[])] <- 0
    road_inside<-additional_lim
  }
  if(is.na(additional_lim) == TRUE){
    road_inside = matrix(0, nrow(potential_space), ncol(potential_space))
    road_inside<-raster::raster(road_inside)

  }


  #main placement loop
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


      #choose random start location and check if it can be used
      check<-0
      while(check != includsion_value){
        start_row <- sample(1:ncol(potential_space), 1)
        start_col <- sample(1:nrow(potential_space), 1)
        if(is.na(potential_space[start_row,start_col]) == FALSE){
          check<-potential_space[start_row,start_col]
        }
      }



      #choose direction for starting placment
      directions <- c("one", "two", "three", "four")
      direction<-sample(directions,1)
      switch(direction,
             "one" = {
               row_range <- seq(from = start_row, to = start_row - field_row_size)
               col_range <- seq(from = start_col, to = start_col - field_col_size)
             },
             "two" = {
               row_range <- seq(from = start_row, to = start_row - field_row_size)
               col_range <- seq(from = start_col, to = start_col + field_col_size)
             },
             "three" = {
               row_range <- seq(from = start_row, to = start_row + field_row_size)
               col_range <- seq(from = start_col, to = start_col - field_col_size)
             },
             "four" = {
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



      #
      #check if any place sits on another farm, or roads, or outside of potential space
      coverlap<-function(a,b){
        if(land[a,b] != 0){
          return("overlap")
        }
      }
      coverlap_roads<-function(a,b){
        if(is.na(road_inside[a,b]) == TRUE || road_inside[a,b] != 0){
          return("overlap")
        }
      }
      coverlap_space<-function(a,b){
        if(is.na(potential_space[a,b]) == TRUE || potential_space[a,b] != includsion_value){
          return("overlap")
        }
      }
      val<- potential_space[round(mean(row_range)),round(mean(col_range))]
      qq <- mapply(coverlap, df$a, df$b, SIMPLIFY = TRUE)
      qr <-mapply(coverlap_roads, df$a, df$b, SIMPLIFY = TRUE)
      qs <-mapply(coverlap_space, df$a, df$b, SIMPLIFY = TRUE)


      #confirm there is no overlap and then place the field
      if ("overlap" %in% qq || "overlap" %in% qr || "overlap" %in% qs|| is.na(val) == TRUE || val != includsion_value) {
        direction<-sample(directions,1)
        #print("repeat")
      } else {
        for(i in 1:length(row_range)){
          for(j in 1:length(col_range)){
            land[row_range[i], col_range[j]] <- 1
          }
        }
        field_placed <- TRUE
      }
    }


    #convert format back to raster
    land_raster<-raster::raster(land)

    #write the information of the field in a field object and add it to the field list
    d_temp <- expand.grid(x = row_range, y = col_range)
    field_obj <- new("Field", number = field_num, location = list(d_temp$x,d_temp$y), farmer = 1)
    field_list<-c(field_list,field_obj)
    field_num <- field_num + 1

    #calculate how much of the space has been filled
    part_potential <- length(which(raster::values(potential_space) %in% c (includsion_value)))
    part_filled <- length(which(raster::values(land_raster) %in% c (1)))
    achieved_percent <- (part_filled / part_potential) * 100
  }



  #distribute fields between farmers
  if(assign_farmers == TRUE){

    #mode 1 random distribution
    if(assign_mode == 1){
      num_fields<-length(field_list)
      k <- 1
      farmer_num <- 1
      while(k <= num_fields){
        ran_fields<-rlnorm(1, meanlog = log(mean_fields_per_farm), sdlog = log(sd_fields_per_farm))
        ran_fields<-ceiling(ran_fields)
        for(q in 1:ran_fields){
          if(k > num_fields){
            break
          }
          field_list[[k]]@farmer <- farmer_num
          q <- q + 1
          k <- k + 1
        }
        farmer_num <- farmer_num + 1
      }
    }

    #mode 2 structured distribution
    if(assign_mode == 2){
      sd_fields_per_farm <- sd_fields_per_farm
      num_fields<-length(field_list)
      field_touple = data.frame(matrix(vector(), 0, 2,
                                       dimnames=list(c(), c("Field", "Size"))),
                                stringsAsFactors=F)
      #have a vector that tells me distance between every field and 0,0
      for(i in 1:num_fields){
        temp_field<-field_list[[i]]
        temp_x<-min(temp_field@location[[1]])
        temp_y<-min(temp_field@location[[2]])
        dist<-sqrt(temp_x^2 + temp_y^2)
        temp_obj<-c(i,dist)
        field_touple<-rbind(field_touple,temp_obj)
      }
      #order fields by distance from raster origin
      field_touple <- field_touple[order(field_touple[,2]),]
      k <- 1
      farmer_num <- 1
      while(k <= nrow(field_touple)){
        ran_fields<-rlnorm(1, meanlog = log(mean_fields_per_farm), sdlog = log(sd_fields_per_farm))
        ran_fields<-ceiling(ran_fields)
        for(q in 1:ran_fields){
          loc<-field_touple[k,1]
          if(is.na(loc)==TRUE){
            break
          }
          field_list[[loc]]@farmer <- farmer_num
          q <- q + 1
          k <- k + 1
        }
        farmer_num <- farmer_num + 1
      }



    }

  }

  #make sure the output extent is defined by cell size and has origin in 00
  raster::extent(land_raster)<-c(0, cell_size*ncol(land_raster), 0, cell_size*nrow(land_raster))

  #unify resutls in list
  result<-list(map = land_raster, field_list = field_list)

  return(result)
}

setClass("Field", slots=list(number="numeric",location="list",farmer="numeric", crop = "numeric"))
