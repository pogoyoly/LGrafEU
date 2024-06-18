# Function to generate dead leaves texture
#' Field establishment based on a dead leave algorithm
#'
#' @param potential_space
#' @param cell_size
#' @param includsion_value
#' @param mean_field_size
#' @param sd_field_size
#' @param mean_shape_index
#' @param sd_shape_index
#' @param percent
#' @param assign_farmers
#' @param assign_mode
#' @param mean_fields_per_farm
#' @param sd_fields_per_farm
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' r<-raster::raster(matrix(1, nrow=50, ncol=50))
#' raster::extent(r)<-c(0,100,0,100)
#' dead_leaves_texture <- establish_by_dead_leaves(potential_space = r,
#'                                                 cell_size = 1,
#'                                                 includsion_value = 1,
#'                                                 mean_field_size = 50,
#'                                                 sd_field_size = 25,
#'                                                 mean_shape_index = .5,
#'                                                 sd_shape_index = .1,
#'                                                 percent = 0.75,
#'                                                 assign_farmers = TRUE,
#'                                                 assign_mode = 2,
#'                                                 mean_fields_per_farm = 4,
#'                                                 sd_fields_per_farm = 4)
#' raster::plot(dead_leaves_texture$map)


establish_by_dead_leaves <- function(potential_space,
                                 cell_size,
                                 includsion_value,
                                 mean_field_size,
                                 sd_field_size,
                                 mean_shape_index,
                                 sd_shape_index,
                                 percent,
                                 assign_farmers,
                                 assign_mode,
                                 mean_fields_per_farm,
                                 sd_fields_per_farm
) {
  # Initialize empty canvas

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



  #setup matrix to be filled
  canvas <- matrix(0, nrow = nrow(potential_space), ncol = ncol(potential_space))
  potential_space_matrix<-raster::as.matrix(potential_space)
  potential_space_matrix[potential_space_matrix !=includsion_value] <- 0
  potential_space_matrix[potential_space_matrix==includsion_value] <- 1

  #creat empty field list
  field_list <- list()

  #calculate size of potential space and set realized/to be filled
  num_potential_patches<-length(potential_space_matrix[potential_space_matrix==1])
  realized_patches <- 0
  to_be_filled <- num_potential_patches

  #set tracker for field numbers
  i<- 1

  # Generate patches
  while(realized_patches < to_be_filled) {

    #choose patch size
    field_size <- max(1, round(rnorm(1, mean=mean_field_size, sd=sd_field_size)))
    shape_index <- rnorm(1, mean=mean_shape_index, sd=sd_shape_index)
    if(shape_index <= 0){
      shape_index<-0.1
    }
    if(field_size <= 0){
      field_size<-1
    }


    #calcualte how much is realized
    realized_patches <- length(canvas[canvas > 0])


    #decide on row size and col size
    field_row_size <- ceiling(shape_index * sqrt(field_size))
    field_col_size <- ceiling(field_size / field_row_size)
    if(field_row_size > nrow(potential_space) | field_col_size > ncol(potential_space)){
      next
    }

    #choose start location
    patch_size <- field_size
    x <- sample(1:(ncol(potential_space) - field_row_size + 1), 1)  # Ensure patch stays within canvas bounds
    y <- sample(1:(nrow(potential_space) - field_col_size + 1), 1) # Ensure patch stays within canvas bounds

    # Add patch to canvas
    canvas[y:(y + field_col_size - 1), x:(x + field_row_size - 1)] <- i

    #update field number
    i <- i + 1

    #remove all sections outside of potential space
    canvas<-matrixcalc::hadamard.prod(canvas, potential_space_matrix)


  }

  #final removel of all sections outside of potential space
  output<-matrixcalc::hadamard.prod(canvas, potential_space_matrix)



  #convert to raster format to be used with landscape metrics tools
  dead_leves_rast<-raster::raster(output)


  #remove small patches till you reach the percentage that needs to be filled
  patched_raster <- landscapemetrics::get_patches(dead_leves_rast)[[1]]
  patchedf<-landscapemetrics::lsm_p_area(patched_raster, directions = 8)
  # patchedf<- patchedf %>% arrange(value)
  patchedf<-patchedf[order(patchedf$value), ]

  unique_list<-raster::unique(dead_leves_rast)
  realized_patches <- length(dead_leves_rast[dead_leves_rast > 0])
  to_be_filled <- realized_patches * percent

  #remove the smallest patch
  while(realized_patches > to_be_filled) {
    val<-patchedf[1,1]
    val<-as.numeric(val)
    dead_leves_rast[dead_leves_rast == unique_list[val]] <- 0
    realized_patches <- length(dead_leves_rast[dead_leves_rast > 0])
    patchedf <- patchedf[-1,]
  }

  #reindex
  #sapply(unique_list, function(unique_list) dead_leves_rast[dead_leves_rast == unique_list] <- unique_list)
  for(j in 1:length(unique_list)){
    dead_leves_rast[dead_leves_rast == unique_list[j]] <- j

  }


  #get patched raster for saving field locations in field list
  patched_raster <- landscapemetrics::get_patches(dead_leves_rast)[[1]]

  for(i in 2:length(patched_raster)){

    mati <- raster::as.matrix(patched_raster[[i]])
    dimnames(mati) <- list(x = 1:nrow(mati), y = 1:ncol(mati))
    mydf <- reshape2::melt(mati)
    names(mydf) <- c("x", "y", "Z")  # Rename the columns as per your expected output
    newdata <- mydf[which(is.na(mydf$Z) == FALSE),]
    field_obj <- new("Field", number = (i-1), location = list(newdata$x,newdata$y), farmer = 1)
    field_list<-c(field_list,field_obj)

    # Give names to each row and column as well as names of each dimension of the matrix itself.

  }

  if(assign_farmers == TRUE){

    #mode 1 is randomly assign formers
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

    #mode 2 is ordered assignment
    if(assign_mode == 2){
      sd_fields_per_farm <- sd_fields_per_farm
      num_fields<-length(field_list)

      field_touple = data.frame(matrix(vector(), 0, 2,
                                       dimnames=list(c(), c("Field", "Size"))),
                                stringsAsFactors=F)

      #creamte a vector that tells me distance between every field and 0,0
      for(i in 1:num_fields){
        temp_field<-field_list[[i]]
        temp_x<-min(temp_field@location[[1]])
        temp_y<-min(temp_field@location[[2]])
        dist<-sqrt(temp_x^2 + temp_y^2)
        temp_obj<-c(i,dist)
        field_touple<-rbind(field_touple,temp_obj)

      }

      #order by distance to 0,0
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



      #devide number of fields by field per farmer
      #start iterating over the list + field per farmer and devide it up
    }

  }

  #set extent by input raster
  raster::extent(dead_leves_rast)<-c(0, cell_size*ncol(dead_leves_rast), 0, cell_size*nrow(dead_leves_rast))

  #finalize result into single object
  result<-list(map = dead_leves_rast, field_list = field_list)

  return(result)
}



setClass("Field", slots=list(number="numeric",location="list",farmer="numeric"))

