### EXAMPLE ###

# creating datasets
coord1a <-seq(45,46,0.1) #supposed to be ERA5 Land
coord1b <-seq(10,11,0.1)
coord1 <- matrix(c(rep(coord1a,length(coord1b)),
                   rep(coord1b,each=length(coord1a))),ncol = 2)

coord2a <-seq(45,46,0.25) #supposed to be ERA5 Single Level
coord2b <-seq(10,11,0.25)
coord2 <- matrix(c(rep(coord2a,length(coord2b)),
                   rep(coord2b,each=length(coord2a))),ncol = 2)

var1a <- 1:121+rnorm(121)
var2a <- sample(c("A","B","C"),121,replace = T)

var1b <- 101:125+rnorm(25)
var2b <- sample(c("D","E","F"),25,replace = T)

df1 <- cbind(as.data.frame(coord1),var1a,var2a)
df2 <- cbind(as.data.frame(coord2),var1b,var2b)

df1 <- rbind(df1,df1,df1,df1,df1)
df2 <- rbind(df2,df2,df2,df2,df2)

df1$time <- rep(seq.Date(as.Date("2021/01/01"),as.Date("2021/01/05"),by="days"),each=121)
df2$time <- rep(seq.Date(as.Date("2021/01/01"),as.Date("2021/01/05"),by="days"),each=25)

names(df1)[1:2]<-c("Lat","Lon")
names(df2)[1:2]<-c("Lat","Lon")

## start here ##

c_lon <- 2 # indicate the column of longitude
c_lat <- 1 # indicate the column of latitude
knn <- 4 # indicate the number of neighbours

df1[,c(which(lapply(df1, class)=="character"))] <- as.factor(df1[,c(which(lapply(df1, class)=="character"))])
df2[,c(which(lapply(df2, class)=="character"))] <- as.factor(df2[,c(which(lapply(df2, class)=="character"))])

# assign a unique value for each localisations
df1$id <- rep(1:nrow(unique(df1[,c(c_lon,c_lat)])),length(unique(df1$time)))
df2$id <- rep(1:nrow(unique(df2[,c(c_lon,c_lat)])),length(unique(df2$time)))

# 
df1_s <- df1[1:length(unique(df1$id)),c(c_lon,c_lat,ncol(df1))]
library(sp)
coordinates(df1_s)<-c("Lon","Lat")

df2_s <- df2[1:length(unique(df2$id)),c(c_lon,c_lat,ncol(df2))]
coordinates(df2_s)<-c("Lon","Lat")

#check
plot(df2_s,col="blue",)
text(df2_s,pos=4,col="blue")
plot(df1_s,add=T,col="red")
text(df1_s,pos=1,col="red")
#ok

dist <- spDists(df1_s,df2_s)
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
no_col<-which(colnames(df2) %in% c("Lon","Lat","time","id"))

#numerical variables
"%notin%"<-Negate("%in%")
num_var <- which(lapply(df2, class)=="numeric")
cat_var <- which(lapply(df2, class)=="factor")

# prepare df1 for enetring values
df1<-cbind(df1,as.data.frame(
  matrix(NA,nrow=nrow(df1),ncol=ncol(df2[,-no_col]),
         dimnames = list(NULL,names(df2)[-no_col]))))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (day in unique(df1$time)) {
  for (id1 in unique(df1$id)) {
    for (nvar in num_var[num_var %notin% no_col]) {value<-c()
      for (kn in 1:knn) {
        value[kn] <- k_list[[id1]]$dp[kn]*df2[df2$time==day & df2$id == k_list[[id1]]$k[kn],nvar]
      }
      df1[df1$time==day&df1$id==id1,which(names(df1)==names(df2)[nvar])] <- sum(value)
    }
    for (nvar in cat_var[cat_var %notin% no_col]) {
      value <- getmode(df2[df2$time==day&df2$id %in% k_list[[id1]]$k,nvar])
      df1[df1$time==day&df1$id==id1,which(names(df1)==names(df2)[nvar])] <- levels(value)[value]
    }
  }
}
