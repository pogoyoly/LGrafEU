
#' Generate using artificial rasters
#'
#' Convert artificial rasters into artificial landcover
#' @param texture_gen artificially generated texture raster
#' @param slope_gen artificially generated lope raster
#' @param aglim_gen artificially generated aglim raster
#' @param texture_real location texture raster
#' @param slope_real location lope raster
#' @param aglim_real location aglim raster
#' @param landcover location landcover raster
#' @return An artificially generated landcover file
#' @export
#' @import terra raster
#'
sl_txt_ag<-function(texture_gen,slope_gen,aglim_gen,aglim_real,slope_real,texture_real,landcover){

  texture_real <- raster::crop(texture_real,aglim_real)
  slope_real <- raster::crop(slope_real,aglim_real)

  #get stratified points
  vals1<-unique(slope_gen)
  vals2 <- unique(texture_gen) # Get all classes


  #create empty raster (make sure that res works with the clumping variable)
  er <- terra::rast(terra::ext(slope_gen), resolution=c(1,1), vals = 0)
  er<-raster::raster(er)

  for(vali in vals1){
    for(valii in vals2){

      #print(vali)
      #print(valii)
      #transform to 0 anything that isnt val and transform val to 1
      myFun1<-function(x) {ifelse (x == vali,1,0)}
      nr1<-raster::calc(slope_gen ,myFun1)

      myFun2<-function(x) {ifelse (x == valii,1,0)}
      nr2<-raster::calc(texture_gen ,myFun2)
      #plot(nr1)

      #multiply new raster with aglim raster
      nsa <- aglim_gen*nr1*nr2
      n <-freq(nsa, useNA = "no")
      plot(nsa)
      #print(nrow(n))
      #print(n)

      #same for real
      nrr1<-raster::calc(slope_real ,myFun1)
      nrr2<-raster::calc(texture_real ,myFun2)
      nsa_real <- aglim_real*nrr1*nrr2

      mat1<-LGrafEU::confus(nsa_real,landcover)





      if(nrow(n) > 1){
        #turn all 0 in aglim raster to value where no caluclation happens
        nsa[nsa == 0] <- NA


        #do the transofmr matrix on raster
        nsa_trans<-LGrafEU::trans(mat1,nsa)
        plot(nsa_trans)
        nsa_trans <- raster::disaggregate(nsa_trans, fact=2)
        #plot(nsa_trans)

        #merge with empty raster
        allrasters <- raster::stack(er, nsa_trans)
        er <- raster::calc(allrasters,fun=sum,na.rm=T)
      }

      plot(er)
      er_majority <- raster::aggregate(er, fact = 4, fun = modal, na.rm = FALSE) # fact 3
      er_majority2 <- raster::aggregate(er_majority, fact = 2, fun = max, na.rm = FALSE) # fact 3
      er_majority2 <- raster::reclassify(er_majority2, cbind(-Inf, 0.5, NA), right=FALSE)

      plot(er_majority2)
    }
  }
  return(er_majority2)
}

#test<-sl_txt_ag(soil_gen,slope_gen,aglim_gen,aglim,slope_real,texture,landcov1)
