

#' Establish landscape by place and conquer
#'
#' @param potential_space a raster including a potential space category for field placement
#' @param cell_size cell size for output
#' @param includsion_value inclusion value for the potential space raster
#' @param additional_lim for use in case of inclusion of a raod raster
#' @param mean_field_size mean field size counted by number of cells from a normal distribution
#' @param sd_field_size sd field size counted by number of cells from a normal distribution
#' @param distribution
#' @param sd_shape_index sd shape index calculated by a relation between width/length of placement
#' @param percent percent of the potential space to be filled with fields
#' @param assign_farmers TRUE/FALSE for farmer assignment in model
#' @param assign_mode 1 = random assignment 2 = spatially structured assignment
#' @param mean_fields_per_farm mean fields per farmer from a long normal distribution
#' @param sd_fields_per_farm fields per farmer from a long normal distribution
#'
#' @return LGraf object
#' @export
#'
#' @examples
#' r<-generate_perlin_noise(100,100,1,2,3,0.01,TRUE, 10)
#' test<-establish_by_place_conquer(potential_space= r,
#'                          cell_size=1,
#'                          includsion_value = 1,
#'                          mean_field_size = 100,
#'                          sd_field_size = 25,
#'                          distribution = "norm",
#'                          mean_shape_index = 1,
#'                          sd_shape_index = 0.3,
#'                          percent = 70,
#'                          assign_farmers = TRUE,
#'                          assign_mode = 2,
#'                          mean_fields_per_farm = 3,
#'                          sd_fields_per_farm = 3)
#'
#'
#' raster::plot(test$map)


establish_by_place_conquer<-function(potential_space,
                                     cell_size,
                                     includsion_value,
                                     additional_lim = NA,
                                     mean_field_size,
                                     sd_field_size,
                                     distribution = "norm",
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

  field_num <- 1

  #main placement loop
  while(achieved_percent < percent){


    field_placed <- FALSE
    while(field_placed == FALSE){

      #set field size and shape index
      if(distribution == "norm"){
      field_size <- max(1, round(rnorm(1, mean=mean_field_size, sd=sd_field_size)))
      }
      if(distribution == "lnorm"){
        mu_N <- log(mean_field_size^2 / sqrt(sd_field_size^2 + mean_field_size^2))
        sigma_N <- sqrt(log(1 + (sd_field_size^2 / mean_field_size^2)))

        field_size <- max(1, round(rlnorm(1, meanlog = mu_N, sdlog = sigma_N)))

      }
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
      check2 <- 1
      while(check != includsion_value && check2 != 0){
        start_row <- sample(1:ncol(potential_space), 1)
        start_col <- sample(1:nrow(potential_space), 1)
        if(is.na(potential_space[start_row,start_col]) == FALSE){
          check<-potential_space[start_row,start_col]
          check2<-land[start_row,start_col]
        }
      }



      #choose direction for starting placment
      directions <- c("one", "two")
      direction<-sample(directions,1)
      switch(direction,
             "one" = {
               dir<- 1
             },
             "two" = {
               dir<- - 1
             },
      )


      #define max size
      max_size<- field_row_size * field_col_size
      changes <- 0
      cur_col <- start_col
      cur_row <- start_row
      placed_cells <- 0
      #start expansion according to direction

      while(placed_cells < max_size && changes != 2){
        suppressWarnings({

        #create block vectors
        vec1<- seq(start_row, start_row + field_row_size - 1, by=1)
        vec2 <- rep(cur_col, field_col_size)

        #create function to check if there is overlap
        coverlap<-function(a,b){
          if(is.na(potential_space[a,b]) == TRUE || land[a,b] != 0 ||  potential_space[a,b] != includsion_value){
            return("overlap")
          }
        }

        #count how many overlaps
        co_vec <- mapply(coverlap, vec1, cur_col, SIMPLIFY = TRUE)
        num_co <- sum(co_vec == "overlap")

        #if no overlaps place
        if(num_co == 0){
          for(i in 1:length(vec1)){
            land[vec1[i], cur_col] <- field_num

          }

          placed_cells <- placed_cells + length(vec1)
        }
        if(num_co > 0){
          #if there are overlaps we try to move the block left and right to see if we can minimize (try with mapply)

          #fist we create a matrix of the possible shift
          shift_vec<-seq( - field_row_size, field_row_size, by = 1)
          shift_func <- function(vec, shift) {
            vec<- vec + shift
          }
          shifted_vectors <- mapply(function(shift) shift_func(vec1, shift), shift_vec, SIMPLIFY = FALSE)
          result_matrix <- do.call(rbind, shifted_vectors)


          #count how many overlaps for each possible shifted vector
          coverlap_test<-function(a){
            if(is.na(potential_space[a,cur_col]) == TRUE || land[a,cur_col] != 0 ||  potential_space[a,cur_col] != includsion_value){
              return(1)
            }
            else{
              return(0)
            }
          }
          co_vec <- rowSums(apply(FUN = coverlap_test, MARGIN = c(1,2), result_matrix))

          if(sum(co_vec)== 0){
            dir <- dir * -1
            cur_col <- start_col + dir
          }
          else{
            #choose the one with the lowest value of co_vec without contributing to the shift
            find_threshold_with_sapply <- function(n_vector, m_vector) {
              # Ensure that n_vector and m_vector are sorted in the same order based on the absolute value of n_vector
              sorted_indices <- order(abs(n_vector))
              n_vector <- n_vector[sorted_indices]
              m_vector <- m_vector[sorted_indices]

              # Find the index where the minimum m value is found
              min_m_index <- which.min(m_vector)

              # If there are multiple minimum values, find the last occurrence
              if(length(min_m_index) > 1) {
                min_m_index <- max(min_m_index)
              }

              # Return the n value at the threshold point
              return(n_vector[min_m_index])
            }
            #This is the shift that is now choosen
            threshold_n <- find_threshold_with_sapply(shift_vec, co_vec)

            #shift by the choosen
            right_vec <- shift_func(vec1,threshold_n)
            for(i in 1:length(right_vec)){
              if(right_vec[i] > 0 && right_vec[i] < nrow(land) && cur_col > 0 && cur_col < ncol(land) && potential_space[right_vec[i], cur_col] == includsion_value){
                land[right_vec[i], cur_col] <- field_num
              }

            }

            placed_cells <- placed_cells +length(vec1)

            cur_col <- cur_col + dir
            if(cur_col >= ncol(land) || cur_col <= 1){
              dir <- dir * -1
              cur_col <- start_col + dir
              changes <- changes + 1

            }




          }




          #otherwise place field with current shift

        }

        #now add to cur_col and start again


      })

      }


      #check block one direction

      #count how many you cannot place

      #expand block to other direction

      #add to land raster with field number indexed

      #repeat till max size




      field_placed <- TRUE
      field_num <- field_num + 1


    }

    #calculate how much of the space has been filled
    part_potential <- length(which(raster::values(potential_space) %in% c (includsion_value)))
    part_filled <- length(land[land > 0])

    #part_filled <- length(which(raster::values(land_raster) %in% c (1)))
    achieved_percent <- (part_filled / part_potential) * 100

  }

  land_raster <- raster::raster(land)
  patched_raster <- landscapemetrics::get_patches(land_raster)[[1]]


  for(i in 2:length(patched_raster)){
    mati <- raster::as.matrix(patched_raster[[i]])
    dimnames(mati) <- list(x = 1:nrow(mati), y = 1:ncol(mati))
    mydf <- reshape2::melt(mati)
    names(mydf) <- c("x", "y", "Z")
    newdata <- mydf[which(is.na(mydf$Z) == FALSE),]
    field_obj <- new("Field", number = (i-1), location = list(newdata$x,newdata$y), farmer = 1)
    field_list<-c(field_list,field_obj)

    # Give names to each row and column as well as names of each dimension of the matrix itself.

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
