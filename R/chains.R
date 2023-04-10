##################################
#   Transition functions
##################################
library(dismo)
library(raster)
library(dplyr)
library(sp)
library(terra)

#scenarios:
# 1. only aglim
# 2. only soil
# 3. aglim + soil
# 4. aglim + slope
# 5. soil + slope
# 6. aglim + soil + slope



#########################################################
aglim_trans<-function(ag,landcover){
#first order only aglim
points<-randomPoints(ag, 250)

# Extract at test points the value of the soil map
soil_points<-extract(ag, points)
soil_points<-unlist(soil_points)
soil_points<-as.data.frame(soil_points)

# using the same points on the landcover map
lc_points<-extract(landcov1, points)
lc_points<-unlist(lc_points)
lc_points<-as.data.frame(lc_points)

#combine both to a df
all_points <- cbind(soil_points, lc_points)

#aggregate to confusion matrix
confusion_matrix<-all_points %>%
  group_by(soil_points,lc_points) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>%
  arrange(desc(freq)) %>%
  mutate(lc_points = as.factor(lc_points))

# results
confusion_matrix$lc_points <- factor(confusion_matrix$lc_points, levels = c(1, 2, 3, 4, 5))


transition2 <- matrix(0, nrow = 13, ncol = 5, dimnames = list(1:13, 1:5))


a <- matrix(0, nrow = length(unique(confusion_matrix$soil_points)),
            ncol = length(unique(confusion_matrix$lc_points)),
            dimnames = list(sort(unique(confusion_matrix$soil_points)),
                            sort(unique(confusion_matrix$lc_points))))

a[cbind(as.numeric(factor(confusion_matrix$soil_points)),
        as.numeric(factor(confusion_matrix$lc_points)))] <- confusion_matrix$freq


cols <- colnames(transition2)[colnames(transition2) %in% colnames(a)]
rows <- rownames(transition2)[rownames(transition2) %in% rownames(a)]
transition2[rows, cols] <- a[rows, cols]
transition2


#extraction fonfusion matrix

#transition function
transform<-function(x){
  z<- 0
  tranz <- 0
  if(is.na(x) ==FALSE){
    dice <- runif(1, min=0, max=1)
    for(i in 1:ncol(transition2)){
      if(isTRUE(dice >=tranz)){
        tranz <- tranz + transition2[x,i]
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
transformed <- calc(ag, transform)
transformed_reclassify <- reclassify(transformed, cbind(0, NA), right=FALSE)
transformed_majority <- aggregate(transformed_reclassify, fact = 2, fun = modal, na.rm = FALSE) # fact 3
return(transformed_majority)

}


test<-aglim_trans(aglim,landcov1)


#########################################################
# only soil

texture_trans<-function(txt,landcover){
  points<-randomPoints(txt, 250)

  # Extract at test points the value of the soil map
  soil_points<-extract(txt, points)
  soil_points<-unlist(soil_points)
  soil_points<-as.data.frame(soil_points)

  # using the same points on the landcover map
  lc_points<-extract(landcover, points)
  lc_points<-unlist(lc_points)
  lc_points<-as.data.frame(lc_points)

  #combine both to a df
  all_points <- cbind(soil_points, lc_points)

  #aggregate to confusion matrix
  confusion_matrix<-all_points %>%
    group_by(soil_points,lc_points) %>%
    summarise(cnt = n()) %>%
    mutate(freq = round(cnt / sum(cnt), 3)) %>%
    arrange(desc(freq)) %>%
    mutate(lc_points = as.factor(lc_points))

  # results
  confusion_matrix$lc_points <- factor(confusion_matrix$lc_points, levels = c(1, 2, 3, 4, 5))


  transition2 <- matrix(0, nrow = 5, ncol = 5, dimnames = list(1:5, 1:5))


  a <- matrix(0, nrow = length(unique(confusion_matrix$soil_points)),
              ncol = length(unique(confusion_matrix$lc_points)),
              dimnames = list(sort(unique(confusion_matrix$soil_points)),
                              sort(unique(confusion_matrix$lc_points))))

  a[cbind(as.numeric(factor(confusion_matrix$soil_points)),
          as.numeric(factor(confusion_matrix$lc_points)))] <- confusion_matrix$freq


  cols <- colnames(transition2)[colnames(transition2) %in% colnames(a)]
  rows <- rownames(transition2)[rownames(transition2) %in% rownames(a)]
  transition2[rows, cols] <- a[rows, cols]
  transition2


  #extraction fonfusion matrix

  #transition function
  transform<-function(x){
    z<- 0
    tranz <- 0
    if(is.na(x) ==FALSE){
      dice <- runif(1, min=0, max=1)
      for(i in 1:ncol(transition2)){
        if(isTRUE(dice >=tranz)){
          tranz <- tranz + transition2[x,i]
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
  transformed <- calc(txt, transform)


  transformed_reclassify <- reclassify(transformed, cbind(0, NA), right=FALSE)
  plot(transformed_reclassify)

  transformed_majority <- aggregate(transformed_reclassify, fact = 3, fun = modal, na.rm = FALSE) # fact 3
  return(transformed_majority)

}


test<-texture_trans(texture,landcov1)


#########################################################
# slope -> aglim -> kc

#clut slope
slope <- crop(slope,aglim)

#get stratified points
vals <- unique(slope) # Get all classes


#create empty raster (make sure that res works with the clumping variable)
er <- rast(ext(aglim), resolution=c(200,200), vals = 0)
er<-raster(er)


for(val in vals){

  print(val)
  #transform to 0 anything that isnt val and transform val to 1
  myFun<-function(x) {ifelse (x == val,1,0)}
  nr<-calc(slope ,myFun)

  #multiply new raster with aglim raster
  nsa <- aglim*nr
  #turn all 0 in aglim raster to value where no caluclation happens
  nsa[nsa == 0] <- NA


  #do the transofmr matrix on raster
  nsa_trans<-aglim_trans(nsa,landcov1)
  plot(nsa_trans)

  #merge with empty raster
  allrasters <- stack(er, nsa_trans)
  er <- calc(allrasters,fun=sum,na.rm=T)


  plot(er)

}


#########################################################
# soil -> aglim -> lc


texture <- crop(texture,aglim)

#get stratified points
vals <- unique(texture) # Get all classes


#create empty raster (make sure that res works with the clumping variable)
er <- rast(ext(aglim), resolution=c(200,200), vals = 0)
er<-raster(er)


for(val in vals){

  print(val)
  #transform to 0 anything that isnt val and transform val to 1
  myFun<-function(x) {ifelse (x == val,1,0)}
  nr<-calc(texture ,myFun)

  #multiply new raster with aglim raster
  nsa <- aglim*nr
  #turn all 0 in aglim raster to value where no caluclation happens
  nsa[nsa == 0] <- NA


  #do the transofmr matrix on raster
  nsa_trans<-aglim_trans(nsa,landcov1)
  plot(nsa_trans)

  #merge with empty raster
  allrasters <- stack(er, nsa_trans)
  er <- calc(allrasters,fun=sum,na.rm=T)


  plot(er)

}


#########################################################
# aglim -> soil -> lc


aglim <- crop(aglim,texture)

#get stratified points
vals <- unique(aglim) # Get all classes


#create empty raster (make sure that res works with the clumping variable)
er <- rast(ext(aglim), resolution=c(200,200), vals = 0)
er<-raster(er)


for(val in vals){

  print(val)
  #transform to 0 anything that isnt val and transform val to 1
  myFun<-function(x) {ifelse (x == val,1,0)}
  nr<-calc(aglim ,myFun)

  #multiply new raster with aglim raster
  nsa <- texture*nr
  #turn all 0 in aglim raster to value where no caluclation happens
  nsa[nsa == 0] <- NA


  #do the transofmr matrix on raster
  nsa_trans<-texture_trans(nsa,landcov1)
  plot(nsa_trans)

  #merge with empty raster
  print("check")
  allrasters <- stack(er, nsa_trans)
  er <- calc(allrasters,fun=sum,na.rm=T)


  plot(er)

}


