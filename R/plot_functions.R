#' Plot by farmer
#'
#' @param output_obj and output object of one of the establish functions
#'
#' @return
#' @export
#'
#' @examples
plot_by_farmer<-function(output_obj){
  land = matrix(0, nrow(output_obj$map), ncol(output_obj$map))

  for(i in 1:length(output_obj$field_list)){


    obj<-output_obj$field_list[[i]]
    row_range<-obj@location[[1]]
    col_range<-obj@location[[2]]
    farmer<-obj@farmer

    for(k in 1:length(col_range)){
      land[row_range[k], col_range[k]] <- farmer
    }




  }

  land_raster<-raster::raster(land)
  extent(land_raster)<-extent(output_obj$map)

  ##test
  unique_values <- unique(values(land_raster))

  random_colors <- sample(colors(), length(unique_values))
  random_colors[1]<-"grey100"
  color_table <- setNames(random_colors, unique_values)


  assign_colors <- function(val) {
    color_table[as.character(val)]
  }

  #plot(land_raster, col=assign_colors(values(land_raster)), legend=FALSE)
  #legend("topright", legend=sort(unique_values), fill=random_colors, title="Values")

  #end test
  plot(land_raster)

}








#' Plot by arable land
#'
#' @param output_obj an output object of one of the establish functions
#'
#' @return
#' @export
#'
#' @examples
plot_by_arable_land<-function(output_obj){
  land = matrix(0, nrow(output_obj$map), ncol(output_obj$map))

  for(i in 1:length(output_obj$field_list)){


    obj<-output_obj$field_list[[i]]
    row_range<-obj@location[[1]]
    col_range<-obj@location[[2]]

    for(k in 1:length(col_range)){
      land[row_range[k], col_range[k]] <- 1
    }




  }

  land_raster<-raster::raster(land)
  extent(land_raster)<-extent(output_obj$map)

  plot(land_raster)

}




#' Plot by field number
#'
#' @param output_obj an output object of one of the establish functions
#'
#' @return
#' @export
#'
#' @examples
plot_by_field<-function(output_obj){
  land = matrix(0, nrow(output_obj$map), ncol(output_obj$map))

  for(i in 1:length(output_obj$field_list)){


    obj<-output_obj$field_list[[i]]
    row_range<-obj@location[[1]]
    col_range<-obj@location[[2]]

    for(k in 1:length(col_range)){
      land[row_range[k], col_range[k]] <- i
    }




  }

  land_raster<-raster::raster(land)
  extent(land_raster)<-extent(output_obj$map)

  plot(land_raster)

}
