df1 <- load("/Users/marcovinciguerra/Github/AgrimoniaProject/R/Weather/Daily Land 2016_2021 Lombardy.RData")

df2 <- load("/Users/marcovinciguerra/Github/AgrimoniaProject/R/Weather/Daily Single Levels 2016_2021 Lombardy.RData")

## start here ##

c_lon <- 1 # indicate the column of longitude
c_lat <- 2 # indicate the column of latitude
knn <- 4 # indicate the number of neighbours

Land[,c(which(lapply(Land, class)=="character"))] <- as.factor(Land[,c(which(lapply(Land, class)=="character"))])
SingleLevels[,c(which(lapply(SingleLevels, class)=="character"))] <- as.factor(SingleLevels[,c(which(lapply(SingleLevels, class)=="character"))])

# assign a unique value for each localisations
Land$id <- rep(1:nrow(unique(Land[,c(c_lon,c_lat)])),length(unique(Land$time)))
SingleLevels$id <- rep(1:nrow(unique(SingleLevels[,c(c_lon,c_lat)])),length(unique(SingleLevels$time)))

# 
Land_s <- Land[1:length(unique(Land$id)),c(c_lon,c_lat,ncol(Land))]
library(sp)
coordinates(Land_s)<-c("Lon","Lat")

SingleLevels_s <- SingleLevels[1:length(unique(SingleLevels$id)),c(c_lon,c_lat,ncol(SingleLevels))]
coordinates(SingleLevels_s)<-c("Lon","Lat")

#check
plot(SingleLevels_s,col="blue",)
text(SingleLevels_s,pos=4,col="blue")
plot(Land_s,add=T,col="red")
text(Land_s,pos=1,col="red")
#ok

dist <- spDists(Land_s,SingleLevels_s)
k_list <- list()
for (i in 1:nrow(dist)) {
  k <- order(dist[i,],decreasing = F)[1:knn]
  d <- dist[i,k]
  if(min(d)==0){
    k <- rep(k[1],4)
    d <- c(1,1,1,1)}
  d <- 1/d
  D <- sum(d)
  dp <- d/D
  k_list[[i]]<-cbind(as.data.frame(k),as.data.frame(dp))
}

# columns we don't need to repeat
no_col<-which(colnames(SingleLevels) %in% c("Lon","Lat","time","id"))

#numerical variables
"%notin%"<-Negate("%in%")
num_var <- which(lapply(SingleLevels, class)=="numeric")
cat_var <- which(lapply(SingleLevels, class)=="factor")

# prepare Land for enetring values
Land<-cbind(Land,as.data.frame(
  matrix(NA,nrow=nrow(Land),ncol=ncol(SingleLevels[,-no_col]),
         dimnames = list(NULL,names(SingleLevels)[-no_col]))))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (day in unique(Land$time)) {
  for (id1 in unique(Land$id)) {
    for (nvar in num_var[num_var %notin% no_col]) {value<-c()
    for (kn in 1:knn) {
      value[kn] <- k_list[[id1]]$dp[kn]*SingleLevels[SingleLevels$time==day & SingleLevels$id == k_list[[id1]]$k[kn],nvar]
    }
    Land[Land$time==day&Land$id==id1,which(names(Land)==names(SingleLevels)[nvar])] <- sum(value)
    }
    for (nvar in cat_var[cat_var %notin% no_col]) {
      value <- getmode(SingleLevels[SingleLevels$time==day&SingleLevels$id %in% k_list[[id1]]$k,nvar])
      Land[Land$time==day&Land$id==id1,which(names(Land)==names(SingleLevels)[nvar])] <- levels(value)[value]
    }
  }
}
