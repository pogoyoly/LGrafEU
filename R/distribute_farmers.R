

#'  A function to fields between farmers on a LGrafEU output object
#'
#' @param output_obj a LGrafEU output object
#' @param assign_mode 1 = unstructured distribution 2 = spatially strcutured distribution of fields
#' @param mean_fields_per_farm mean fields per farmer
#' @param sd_fields_per_farm sd fields per farmer
#' @param distribution either norm or lnorm
#'
#' @return A LGraf output object with fields assined to farmers
#' @export
#'
#' @examples
#' r<-terra::rast(matrix(1, nrow=50, ncol=50))
#' map<-establish_by_place_conquer(potential_space= r,
#'                                 cell_size=1,
#'                                 includsion_value = 1,
#'                                 mean_field_size = 100,
#'                                 sd_field_size = 25,
#'                                 distribution = "norm",
#'                                 mean_shape_index = 1,
#'                                 sd_shape_index = 0.3,
#'                                 percent = 75,
#'                                 assign_farmers = TRUE,
#'                                 assign_mode = 2,
#'                                 mean_fields_per_farm = 3,
#'                                 sd_fields_per_farm = 3)
#'
#'
#' outcome<-distrubution_by_farmer(map,assign_mode = 1, mean_fields_per_farm =10, sd_fields_per_farm = 5, distribution = "norm")
#'
#' return_by_farmer(outcome)
distrubution_by_farmer <- function(output_obj, assign_mode = 1, mean_fields_per_farm, sd_fields_per_farm, distribution
){

  obj <- output_obj

    #mode 1 random distribution
    if(assign_mode == 1){

      #extract the number of fields from the output object
      num_fields <- length(obj$field_list)

      k <- 1
      farmer_num <- 1

      #run loop until all fields have been distributed
      while(k <= num_fields){


        #set choose a number of fields to assign a farmer according to the distribution
        if(distribution == "norm"){
          ran_fields <- rnorm(1, mean=mean_fields_per_farm, sd=sd_fields_per_farm)
          ran_fields <- ceiling(ran_fields)
        }
        if(distribution == "lnorm"){
          mu_N <- log(mean_fields_per_farm^2 / sqrt(sd_fields_per_farm^2 + mean_fields_per_farm^2))
          sigma_N <- sqrt(log(1 + (sd_fields_per_farm^2 / mean_fields_per_farm^2)))
          ran_fields <- max(1, round(rlnorm(1, meanlog = mu_N, sdlog = sigma_N)))
        }



        for(q in 1:ran_fields){
          if(k > num_fields){
            break
          }
          obj$field_list[[k]]@farmer <- farmer_num
          q <- q + 1
          k <- k + 1
        }
        farmer_num <- farmer_num + 1
      }
    }

    #mode 2 structured distribution
    if(assign_mode == 2){


    }




  return(obj)


}
