#' @title readMap
#' @description read landuse files
#' @return writes csv and returns a dataframe
#' @param file file to convert
#' @param nvar TRUE aggregates to global for each year and variable to compare against raw data (rasters)
#' @author David Chen
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @export


readMap <- function(filename,nvar=1){
  nc <- nc_open(filename)
  buf <-ncvar_get(nc,names(nc$var)[nvar])
  nc_close(nc)
  buf
}
