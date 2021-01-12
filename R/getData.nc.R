#' @title getData.nc
#' @description read landuse files
#' @return writes csv and returns a dataframe
#' @param filename1 first part of path
#' @param
#' @param check TRUE aggregates to global for each year and variable to compare against raw data (rasters)
#' @author David Chen
#' @export
#'
# function to read data from NC files
getData.nc <- function(filename1,rcps,crop,filename2,co,
                       sy,ny,rf,rl,spam,ir,mo,gc,avg,en){
  yield <- array(0,dim=c(360,720))
  # variables just for checking
  yield2 <- array(0,dim=c(360,720))
  counter <- 0
  buf.all <- NULL
  #cat("doing",sy,"with",ny,"firsts",rf,"lasts",rl,"\n")
  for(i in 1:length(sy)){
    # reading file
    fn <- paste(filename1,ifelse(sy[i]<2002,rcps[1],rcps[2]),
                paste0("/",crop),
                filename2, "w5e5_", ifelse(sy[i]<2002,rcps[1],rcps[2]),
                "_2015soc_",
                ifelse(sy[i]<2002,"default",co),
                "_yield-", paste(crop,"-",sep=""),
                ir,"_global_annual_",
                sy[i],"_",sy[i]+ny[i]-1,".nc",sep="")
    #if(!is.na(zipfile)) zip(zipfile,fn)
    nc <- nc_open(fn,verbose=F)
    buf <-ncvar_get(nc,names(nc$var)[1])
    # x11()
    # image.plot(buf[,,83])
    cat(fn,"\nvar name",names(nc$var),"and",dim(buf),"dimensions, range is",range(buf,na.rm=T),"(",range(buf[buf<1e10],na.rm=T),")\n")
    #cat(fn,"\n")
    ## ignoring everything with the NC-file fill value 1e20
    buf[buf>=1e10] <- NA
    # ignoring everything below 0.01t/ha
    buf[buf<=0.01] <- 0
    # plot first time slice to png for checking
    #     if(i==1){
    #    png(paste(output.path,"yields/",ir,"_yield_",mo,"_",gc,"_",co,"_",crop,"2_for_ERL.png",sep=""))
    #      image.plot(buf[,,1],main=paste(ir,"yield",crop,sy[i]))
    #        dev.off()
    #     }
    # assinging annual fields to yield array, using specified readfirst and readlast years for NC-year packages
    for(rr in rf[i]:rl[i]){
      bif <- buf[,,rr]
      yield <- yield + t(bif)/avg*spam
      yield2 <-yield2 + t(bif)/avg
      counter <- counter + 1
      buf[,,rr] <- buf[,,rr]*t(spam)*en
    }
    rm(bif)
    nc_close(nc)
    buf.all <- abind(buf.all,buf[,,c(rf[i]:rl[i])],along=3)
  }
  #cat("added",counter,max(yield/spam,na.rm=T),"\n")
  dimnames(buf.all) <- NULL
  # plotting per-pixel yield for checking
  #png(paste(isimip.lpj,"yields/yield_shifter_econ_models/",ir,"_yield_",mo,"_",gc,"_",co,"_",crop,"2.png",sep=""))
  #image.plot(yield2,zlim=c(0.0001,max(yield2,na.rm=T)),main="mean yield2s")
  #dev.off()
  #return total global production in kcal
  ret <- list(av=yield*en,all=aperm(buf.all,c(2,1,3)))
  ret
}
