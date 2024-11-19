#' return by farmer
#'
#' @param output_obj and output object of one of the establish functions
#' @param method 1 is plot directly 2 returns a raster of crop type
#'
#' @export
#' @importFrom grDevices colors
#'
return_by_farmer<-function(output_obj, method = 1){
  map<-output_obj$map
  obj_main<-output_obj$field_list
  land = matrix(0, nrow(map), ncol(map))

  for(i in 1:length(obj_main)){


    obj<-obj_main[[i]]
    row_range<-obj@location[[1]]
    col_range<-obj@location[[2]]
    farmer<-obj@farmer

    for(k in 1:length(col_range)){
      land[row_range[k], col_range[k]] <- farmer
    }




  }

  land_raster<-terra::rast(land)
  terra::ext(land_raster)<-terra::ext(map)

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
  if(method == 1){
    polygons <- terra::as.polygons(land_raster, n=8,fun=function(x){x > 0}, na.rm=TRUE, digits=12, dissolve=TRUE)
    terra::plot(land_raster, legend = FALSE)
    terra::plot(polygons, add = TRUE, border = "black", lwd = 1)

  }
  if(method == 2){
    return(land_raster)
  }

}








#' return by arable land
#'
#' @param output_obj an output object of one of the establish functions
#' @param method 1 is plot directly 2 returns a raster of crop type
#'
#' @export
#' @importFrom grDevices colors
#'
return_by_arable_land<-function(output_obj, method = 1){
  land = matrix(0, nrow(output_obj$map), ncol(output_obj$map))

  for(i in 1:length(output_obj$field_list)){


    obj<-output_obj$field_list[[i]]
    row_range<-obj@location[[1]]
    col_range<-obj@location[[2]]

    for(k in 1:length(col_range)){
      land[row_range[k], col_range[k]] <- 1
    }




  }

  land_raster<-terra::rast(land)
  terra::ext(land_raster)<-terra::ext(output_obj$map)

  if(method == 1){
    polygons <- terra::as.polygons(land_raster, n=8,fun=function(x){x > 0}, na.rm=TRUE, digits=12, dissolve=TRUE)
    terra::plot(land_raster, legend = FALSE)
    terra::plot(polygons, add = TRUE, border = "black", lwd = 1)
  }
  if(method == 2){
    return(land_raster)
  }

}




#' Plot by field number
#'
#' @param output_obj an output object of one of the establish functions
#' @param method 1 is plot directly 2 returns a raster of crop type
#'
#' @export
#'
return_by_field<-function(output_obj, method = 1){
  land = matrix(0, nrow(output_obj$map), ncol(output_obj$map))

  for(i in 1:length(output_obj$field_list)){


    obj<-output_obj$field_list[[i]]
    row_range<-obj@location[[1]]
    col_range<-obj@location[[2]]

    for(k in 1:length(col_range)){
      land[row_range[k], col_range[k]] <- i
    }




  }

  land_raster<-terra::rast(land)
  terra::ext(land_raster)<-terra::ext(output_obj$map)

  if(method == 1){
    polygons <- terra::as.polygons(land_raster, n=8,fun=function(x){x > 0}, na.rm=TRUE, digits=12, dissolve=TRUE)
    terra::plot(land_raster, legend = FALSE)
    terra::plot(polygons, add = TRUE, border = "black", lwd = 1)
  }
  if(method == 2){
    return(land_raster)
  }

}







#' Plot by crop type
#'
#' @param output_obj an output object of one of the establish functions
#' @param method 1 is plot directly 2 returns a raster of crop type
#'
#' @return method = 1 plots a raster and method = 2 returns an LGrafEU output object
#' @export
#'
return_by_crop<-function(output_obj, method = 1){
  land = matrix(0, nrow(output_obj$map), ncol(output_obj$map))

  for(i in 1:length(output_obj$field_list)){


    obj<-output_obj$field_list[[i]]
    row_range<-obj@location[[1]]
    col_range<-obj@location[[2]]

    crop<-obj@crop

    for(k in 1:length(col_range)){
      land[row_range[k], col_range[k]] <- crop
    }




  }

  land_raster<-terra::rast(land)
  terra::ext(land_raster)<-terra::ext(output_obj$map)

  if(method == 1){
    polygons <- terra::as.polygons(land_raster, n=8,fun=function(x){x > 0}, na.rm=TRUE, digits=12, dissolve=TRUE)
    terra::plot(land_raster, legend = FALSE)
    terra::plot(polygons, add = TRUE, border = "black", lwd = 1)
  }
  if(method == 2){
    return(land_raster)
  }

}
