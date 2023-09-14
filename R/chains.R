##################################
#   Transition functions
##################################
#library(dismo)
#library(raster)
#library(dplyr)
#library(sp)
#library(terra)
#library(lattice)
#scenarios:
# 1. only aglim
# 2. only soil
# 3. aglim + soil
# 4. aglim + slope
# 5. soil + slope
# 6. aglim + soil + slope



#########################################################
#' Title
#'
#' @param rst
#' @param landcover
#'
#'
#' @return
#'
#' @examples
confus<-function(rst,landcover){
#first order only aglim
points<-randomPoints(rst, 250)

# Extract at test points the value of the soil map
soil_points<-extract(rst, points)
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
return(transition2)
}


#' Title
#'
#' @param transition
#' @param rst
#'
#' @return
#'
#' @examples
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
transformed <- calc(rst, transform)
transformed_reclassify <- reclassify(transformed, cbind(0, NA), right=FALSE)
transformed_majority <- aggregate(transformed_reclassify, fact = 2, fun = modal, na.rm = FALSE) # fact 3
return(transformed_majority)

}

#' A one layer transformation
#'
#' @param rast A type feature raster
#' @param landcover A landcover raster
#' @param aggregation A number
#'
#' @return A raster transformed
#' @export
#'
#' @examples
trans_1lr<-function(rast,landcover){
  con_mat<-confus(rast,landcover)
  trans_rast<-trans(con_mat,rast)
  trans_rast <- aggregate(trans_rast, fact = aggregation, fun = modal, na.rm = FALSE) # fact 3
  return(trans_rast)
}

#test<-trans_1lr(aglim,landcov1,2)
#plot(test)



#######################################
# slope -> soil -> aglim -> lc




#' A three layer transformation
#'
#' @param texture A soil texture raster
#' @param slope A categorized slope raster
#' @param aglim A agricultural limitation raster
#' @param landcov A categorized landcover raster
#'
#' @return
#' @export
#'
#' @examples
trans_3lr<-function(texture,slope,aglim,landcov){

texture <- crop(texture,aglim)
slope <- crop(slope,aglim)

#get stratified points
vals1<-unique(slope)
vals2 <- unique(texture) # Get all classes


er <- rast(ext(aglim), resolution=c(200,200), vals = 0)
er<-raster(er)

for(vali in vals1){
for(valii in vals2){

  #print(vali)
  #print(valii)
  #transform to 0 anything that isnt val and transform val to 1
  myFun1<-function(x) {ifelse (x == vali,1,0)}
  nr1<-calc(slope ,myFun1)

  myFun2<-function(x) {ifelse (x == valii,1,0)}
  nr2<-calc(texture ,myFun2)
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
  con_nsa<-confus(nsa,landcov)
  nsa_trans<-trans(con_nsa,nsa)


  #merge with empty raster
  allrasters <- stack(er, nsa_trans)
  er <- calc(allrasters,fun=sum,na.rm=T)
  }

  plot(er)
  er_majority <- aggregate(er, fact = 4, fun = modal, na.rm = FALSE) # fact 3
  er_majority2 <- aggregate(er_majority, fact = 2, fun = max, na.rm = FALSE) # fact 3
  er_majority2 <- reclassify(er_majority2, cbind(-Inf, 0.5, NA), right=FALSE)

  plot(er_majority2)
}
}
}

#test<-trans2(texture,slope_real,aglim)
#plot(test)

