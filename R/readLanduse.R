#' @title readLanduse
#' @description read landuse files
#' @return writes csv and returns a dataframe
#' @param path convert
#' @param fn
#' @param band
#' @param check TRUE aggregates to global for each year and variable to compare against raw data (rasters)
#' @author David Chen
#' @export


readLanduse <- function(path,fn,band){
  # first read coordinates
  npix <- 67420
  ff <- file(paste0(path,"grid.bin"),"rb")
  seek(ff,where=43,origin="start")
  x <- readBin(ff,integer(),size=2,n=npix*2)/100
  lon <- x[(1:npix)*2-1]
  lat <- x[(1:npix)*2]
  # ilon/ilat are row and column numbers, starting at 179.75W and 89.75N as 1/1
  ilon <- as.integer((lon+180)/0.5 + 1.01)
  ilat <- as.integer((-lat+90)/0.5 + 1.01)
  area <- (111e3*0.5)*(111e3*0.5)*cos(lat/180*pi)/10000 #ha
  close(ff)
  grid <- data.frame(lon,lat,ilon,ilat,area)
  # then read landuse
  ff <- file(paste0(path,fn),"rb")
  seek(ff,where=43+32*npix*(2000-1700)*2,origin="start")
  lu <- matrix(readBin(ff,integer(),n=32*npix,size=2),nrow=npix,byrow=T)[,c(band,band+16)]/1000
  close(ff)
  data <- array(NA,dim=c(360,720,2))
  for(p in 1:npix){
    data[grid$ilat[p],grid$ilon[p],] <- lu[p,]*grid$area[p]
  }
  data
}
