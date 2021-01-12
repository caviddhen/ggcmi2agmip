#' @title aggregateMap
#' @description read landuse files
#' @return writes csv and returns a dataframe
#' @param infile file to convert
#' @author David Chen
#' @importFrom  raster rastercellStats
#' @importFrom utils read.table
#' @export

aggregateMap <- function(infile){
  # reading the row and column setting
  header <- read.table(infile,nrows=6,sep=" ")
  nc <- header$V2[1]+1
  nr <- header$V2[2]
  xl <- header$V2[3]
  yl <- header$V2[4]
  size <- header$V2[5]
  nd <- header$V2[6]
  data <- matrix(scan(infile,skip=6,sep=" "),ncol=nc,byrow=T)
  data[data==nd | !is.finite(data)] <- 0
  cat(infile,range(data),"dims are",nc,nr,xl,yl,"\n")

  # row and column setting for full global grid
  NROWS <- 2160
  NCOLS <-4320
  NODATA <- -9999
  ULXMAP <- -179.95833333333333
  ULYMAP <- 89.9583333333333333
  # filling in blanks
  misleft <- as.integer(abs((ULXMAP - xl)/size)+.5)
  misright <- NCOLS-nc-misleft
  fill1 <- array(0,dim=c(nr,misleft))
  fill2 <- array(0,dim=c(nr,misright))
  cat(misleft,misright,dim(fill1),dim(fill2),"\n")
  data <- cbind(fill1,data,fill2)
  misbot <- as.integer(abs((-ULYMAP - yl)/size)+.5)
  mistop <- NROWS-nr-misbot
  fill1 <- array(0,dim=c(mistop,NCOLS))
  fill2 <- array(0,dim=c(misbot,NCOLS))
  cat(misbot,mistop,dim(fill1),dim(fill2),"\n")
  data <- rbind(fill1,data,fill2)
  rm(fill1,fill2)
  # converting to raster type for easy aggregation
  data2 <- raster(data,ULXMAP,-ULXMAP,-ULYMAP,ULYMAP)
  # aggregating by summing up hectares, converting back to simple matrix
  data3 <- matrix(aggregate(data2,fact=6,fun=sum)[,,1],ncol=720,byrow=T)
  #x11()
  #image.plot(data3,zlim=c(1,max(data3)),main=infile,cex=0.3)
  cat(range(data3),"dims are",dim(data3),"\n")
  data3
}
