


#########################################################
#' Inward facing function
#'
#' @param rst a raster of either soil aglim or slope
#' @param landcover a categorized landcover map
#'
#'
confus<-function(rst,landcover){
  #first order only aglim
  trf_val<-ncol(rst)*nrow(rst)
  points<-dismo::randomPoints(rst, trf_val, tryf=10)

  # Extract at test points the value of the soil map
  soil_points<-raster::extract(rst, points)
  soil_points<-unlist(soil_points)
  soil_points<-as.data.frame(soil_points)

  # using the same points on the landcover map
  lc_points<-raster::extract(landcover, points)
  lc_points<-unlist(lc_points)
  lc_points<-as.data.frame(lc_points)

  #combine both to a df
  all_points <- cbind(soil_points, lc_points)

  #aggregate to confusion matrix
  #confusion_matrix<-all_points %>%
  #dplyr::group_by(soil_points,lc_points) %>%
  #dplyr::summarise(cnt = n()) %>%
  #dplyr::mutate(freq = round(cnt / sum(cnt), 3)) %>%
  #dplyr::arrange(desc(freq)) %>%
  #dplyr::mutate(lc_points = as.factor(lc_points))

  # Group by soil_points and lc_points
  grouped_points <- dplyr::group_by(all_points, soil_points, lc_points)

  # Summarize to get the count
  summarized_points <- dplyr::summarise(grouped_points, cnt = dplyr::n())

  # Mutate to calculate the frequency
  mutated_points <- dplyr::mutate(summarized_points, freq = round(cnt / sum(cnt), 3))

  # Arrange by descending frequency
  arranged_points <- dplyr::arrange(mutated_points, desc(freq))

  # Mutate to convert lc_points to factor
  confusion_matrix <- dplyr::mutate(arranged_points, lc_points = as.factor(lc_points))




  # results
  confusion_matrix$lc_points <- factor(confusion_matrix$lc_points, levels = c(1, 2, 3, 4, 5))


  transition2 <- matrix(0, nrow = 13, ncol = 5, dimnames = list(1:13, 1:5))



  a <- matrix(0, nrow = length(na.omit(unique(confusion_matrix$soil_points))),
            ncol = length(na.omit(unique(confusion_matrix$lc_points))),
            dimnames = list(sort(unique(confusion_matrix$soil_points)),
                            sort(unique(confusion_matrix$lc_points))))

  a[cbind(as.numeric(factor(na.omit(confusion_matrix$soil_points))),
        as.numeric(factor(na.omit(confusion_matrix$lc_points))))] <- confusion_matrix$freq


  cols <- colnames(transition2)[colnames(transition2) %in% colnames(a)]
  rows <- rownames(transition2)[rownames(transition2) %in% rownames(a)]
  transition2[rows, cols] <- a[rows, cols]
  return(transition2)
}


#' Inward facing function
#'
#' @param transition the transition matrix from the confus function
#' @param rst the raster from which the transition occurs
#'
#'
#'
#'
trans<-function(transition,rst){
#transition function
tran <- transition
transform<-function(x){
  z<- 0
  tranz <- 0
  if(is.na(x) ==FALSE){
    dice <- runif(1, min=0, max=1)
    for(i in 1:ncol(tran)){
      if(isTRUE(dice >=tranz)){
        tranz <- tranz + tran[x,i]
      }
      if(isTRUE(dice < tranz)){
        z <- i
        tranz <- -1
      }
    }


  }

  return(z)
}


#run apply function
transformed <- raster::calc(rst, transform)
transformed_reclassify <- raster::reclassify(transformed, cbind(0, NA), right=FALSE)
return(transformed_reclassify)

}

#' A one layer transformation
#'
#' @param rast A type feature raster
#' @param landcover A landcover raster
#' @param aggregation A number that defines how aggregated he potential space will be
#'
#'
trans_1lr<-function(rast,landcover,aggregation){
  con_mat<-confus(rast,landcover)
  trans_rast<-trans(con_mat,rast)
  trans_rast <- raster::aggregate(trans_rast, fact = aggregation, fun = modal, na.rm = FALSE)
  trans_rast <- raster::disaggregate(trans_rast, fact=aggregation)
  raster::extent(trans_rast)<-raster::extent(rast)
  return(trans_rast)
}




#######################################




#' A three layer transformation
#'
#' @param texture A soil texture raster
#' @param slope A categorized slope raster
#' @param aglim A agricultural limitation raster
#' @param landcov A categorized landcover raster
#'
#'
trans_3lr<-function(texture,slope,aglim,landcov, aggregation){


#get stratified points
vals1<-raster::unique(slope)
vals2 <- raster::unique(texture) # Get all classes


res1<-raster::res(aglim)[1]
res2<-raster::res(aglim)[2]
er <- terra::rast(terra::ext(aglim), resolution=c(res1,res2), vals = 0)
er<-raster::raster(er)

for(vali in vals1){
for(valii in vals2){

  #print(vali)
  #print(valii)
  #transform to 0 anything that isnt val and transform val to 1
  myFun1<-function(x) {ifelse (x == vali,1,0)}
  nr1<-raster::calc(slope ,myFun1)

  myFun2<-function(x) {ifelse (x == valii,1,0)}
  nr2<-raster::calc(texture ,myFun2)
  #plot(nr1)

  #multiply new raster with aglim raster
  nsa <- aglim*nr1*nr2
  n <-freq(nsa, useNA = "no")
  #print(nrow(n))
  #print(n)


  if(nrow(n) > 1){
  #turn all 0 in aglim raster to value where no caluclation happens
  nsa[nsa == 0] <- NA

  #print(nsa)
  #do the transofmr matrix on raster
  con_nsa<-LGrafEU::confus(nsa,landcov)
  nsa_trans<-LGrafEU::trans(con_nsa,nsa)


  #merge with empty raster
  allrasters <- raster::stack(er, nsa_trans)
  er <- raster::calc(allrasters,fun=sum,na.rm=T)
  }


}
}
#plot(er)
#first aggregate based on majority rule
er_majority <- raster::aggregate(er, fact = aggregation, fun = modal, na.rm = FALSE)
er_majority <- raster::disaggregate(er_majority, fact=aggregation)
er_majority2 <- raster::reclassify(er_majority, cbind(-Inf, 0.5, NA), right=FALSE)
return(er_majority2)


}


