#' @title processIsimip
#' @description does the processing of the isimip3b ggcmi crop yields into deisred output
#' @return writes csv and returns a dataframe
#' @param path convert
#' @param rcpso rcpso
#' @param gcms gcms
#' @param models models
#' @param time time scale of interest
#' @param co2_scen convert
#' @author David Chen, Christoph Mueller
#' @export



processIsimip <- function(rcpso,models, gcms, co2_scen, irr_scen = c("noirr","firr"), time,
                          general.path, lpj.spam.path, output.path, adm.path, rice.path,wheat.path){

  # crops to process ---------------------

  crops1 <- c("mai","ri1","ri2","soy","swh","wwh","ric","whe")
  crops3 <- c("MAIZ","RICE","RICE","SOYB","WHEA","WHEA")
  # ENERG and fresh matter ratio for c("mai", "ri1","ri2","soy","swh","wwh")
  ENERG <- c(3560,2800,2800,3350,3340,3340)
  FRESHMATTER <- 100 / c(88,87,87,91,88,88)
  ENERG_DM <- ENERG*FRESHMATTER

  # Year selections ----------------------------------

  # past centred around 1985-2014
  syear1.e<- syear2.e <- c(1850)
  nyear1.e <- nyear2.e <- c(165)
  readf1.e <- readf2.e <- c(135)
  readl1.e <- readl2.e <- c(164)
  # future centered around 2030 (2016-2045)
  syear3a.e <- c(2015)
  nyear3a.e <- c(86)
  readf3a.e <- c(1)
  readl3a.e <- c(30)
  # future centered around 2050 (2036-2065)
  syear3b.e <- c(2015)
  nyear3b.e <- c(86)
  readf3b.e <- c(22)
  readl3b.e <- c(51)
  # future centered around 2085 (2070-2099)
  syear3c.e <- c(2015)
  nyear3c.e <- c(86)
  readf3c.e <- c(55)
  readl3c.e <- c(86)
  # future full from 2015 to 2100
  syear3d.e <- c(2015)
  nyear3d.e <- c(86)
  readf3d.e <- c(1)
  readl3d.e <- c(86)

  ts <- c("2030s","2050s","2085s")
  grs <- c(30,50,85)

  #read in Country name data --------------

  gadm0 <- read.table(paste0(adm.path,"gadm0.meta.csv"),sep=",",quote="\"",header=F)
  countrylist <- gadm0[,2]
  # replace comma and odd letters
  countrylist <- sub(","," -",countrylist)
  countrylist <- sub("\303\247","c",countrylist)
  countrylist <- sub("\303\264","o",countrylist)
  ncountry <- length(countrylist)
  ISO.code <- gadm0[,3]
  country.code <- gadm0[,1]
  countrymap <- t(readMap(paste0(adm.path,"gadm0.mask.nc4"),1))
  countries <- unique(as.vector(countrymap[!is.na(countrymap)]))
  ncases <- length(ts)*length(models)*length(gcms)*(length(crops1)+1)*2
  ncases1 <- length(crops1)+1
  nat.prod.ref <- array(NA,dim=c(ncases,length(country.code),3)) #rf,ir,all
  nat.prod.fut <- array(NA,dim=c(ncases,length(country.code),3))
  nat.prod.ref1 <- array(NA,dim=c(ncases1,length(country.code),3))
  nat.prod.fut1 <- array(NA,dim=c(ncases1,length(country.code),3))
  nat.prod.fut1.annual <- array(NA,dim=c(ncases1,length(country.code),3,30))


  ## declare tables to be filled by loop------------------

  spam.r <- spam.i <- array(NA,dim=c(360,720,length(crops3)))
  #aggspam.r <- aggspam.i <- array(NA,dim=c(289,length(crops3)))
  aggspam.r <- aggspam.i <- array(NA,dim=c(ncountry,length(crops3)))
  read.spam <- rep(F,length(crops3))
  # declaration
  global.sum.all <- global.sum.wwh  <- global.sum.swh <- global.sum.mai <- global.sum.ri1  <- global.sum.ri2<- global.sum.soy <- array(NA,dim=c(length(models),length(rcpso)*length(co2_scen)*length(gcms),length(ts)))
  coln <- c(rep(co2_scen[1],dim(global.sum.all)[2]/2),rep(co2_scen[2],dim(global.sum.all)[2]/2))
  coln2 <- as.vector(outer(gcms,rcpso,paste,sep="_"))
  coln <- paste(c(coln2,coln2),coln,sep="_")
  dimnames(global.sum.all) <- list(models,coln,ts)
  dimnames(global.sum.wwh) <- list(models,coln,ts)
  dimnames(global.sum.swh) <- list(models,coln,ts)
  dimnames(global.sum.soy) <- list(models,coln,ts)
  dimnames(global.sum.ri1) <- list(models,coln,ts)
  dimnames(global.sum.ri2) <- list(models,coln,ts)
  dimnames(global.sum.mai) <- list(models,coln,ts)


  # Loop through ---------------------------

for(rcp in 2:length(rcpso)){
  output <- NULL
  cnames <- NULL
  rcps <- c(rcpso[1],rcpso[rcp])
  nat.prod.ref[] <- NA
  nat.prod.fut[] <- NA
  for(gc in gcms){ #only gfdl for now
    for(t in time){
      #for(co in co2a[1]){
      for(co in co2_scen){
        pastureref.p <- pasturefut.p <- pasturefut.annual <- NA
        for(mo in models){   #only EPIC IIASA
          #for(mo in models[1]){
          cnames2 <- NULL
          output2 <- NULL
          area.weights <- NULL
          prod.weights <- NULL
          nat.prod.ref1[] <- NA
          nat.prod.fut1[] <- NA
          nat.prod.fut1.annual[] <- NA
          crops.of.interest <- c(1:6)

          syear1 <- syear1.e; nyear1 <- nyear1.e; readf1 <- readf1.e; readl1 <- readl1.e
          syear2 <- syear2.e; nyear2 <- nyear2.e; readf2 <- readf2.e; readl2 <- readl2.e
          if(t==ts[1])
          {
            syear3 <- syear3a.e; nyear3 <- nyear3a.e; readf3 <- readf3a.e; readl3 <- readl3a.e
          } else if(t==ts[2])
          {
            syear3 <- syear3b.e; nyear3 <- nyear3b.e; readf3 <- readf3b.e; readl3 <- readl3b.e
          } else
          {
            syear3 <- syear3c.e; nyear3 <- nyear3c.e; readf3 <- readf3c.e; readl3 <- readl3c.e
          }

          syeara <- syear1
          nyeara <- nyear1
          readfa <- readf1
          readla <- readl1
          #co2a <- co2_scen

          prod.total.ref <- 0 # total calorie production for all crops
          prod.total.fut <- 0 # total calorie production for all crops
          for(cr in crops.of.interest){
            #for(cr in crops.of.interest[1]){
            #for(cr in 1:2){
            # check if SPAM data has been read already, if not, supply
            if(!read.spam[cr]){

              spam.r[,,cr] <- aggregateMap(paste(lpj.spam.path,crops3[cr],".R_physical_area",sep=""))
              spam.i[,,cr] <- aggregateMap(paste(lpj.spam.path,crops3[cr],".I_physical_area",sep=""))

              if(cr==2 | cr==3){
                rice.mask.r <- t(readMap(paste(rice.path,crops1[cr],"_rf_ggcmi_crop_calendar_phase3_v1.01.nc4",sep=""),5))
                rice.mask.i <- t(readMap(paste(rice.path,crops1[cr],"_ir_ggcmi_crop_calendar_phase3_v1.01.nc4",sep=""),5))
                spam.r[,,cr] <- spam.r[,,cr]*rice.mask.r
                spam.i[,,cr] <- spam.i[,,cr]*rice.mask.i
              }

              if(cr==5 | cr ==6){
                wh <- nc_open(paste(wheat.path,"winter_and_spring_wheat_areas_phase3.nc4", sep=""))
                wh.r <- ncvar_get(wh, paste(crops1[cr],"_area_rf",sep=""))
                wh.i <- ncvar_get(wh, paste(crops1[cr],"_area_ir",sep=""))
                nc_close(wh)
                spam.r[,,cr] <- t(wh.r)
                spam.i[,,cr] <- t(wh.i)

              }

              cat(crops3[cr],dim(spam.r[,,cr]),range(spam.r[,,cr],na.rm=T),"\n")
              #if(!is.na(zipfile)) zip(zipfile_b,paste(isimip.lpj,"yields/yield_shifter_econ_models/some_spam/",crops3[cr],".R_physical_area",sep=""))
              #if(!is.na(zipfile)) zip(zipfile_b,paste(isimip.lpj,"yields/yield_shifter_econ_models/some_spam/",crops3[cr],".I_physical_area",sep=""))
              read.spam[cr] <- T
            }
            #checks if file exists, otherwise move to next file
            skip <- FALSE
            fn <- paste0(general.path,mo, "/phase3b/", gc,"/",rcps[2],"/",
                         crops1[cr],"/",tolower(mo),"_",tolower(gc),"_",
                         "w5e5", "_", rcps[2],"_",  "2015soc", "_", co,
                         "_yield-",crops1[cr], "-",irr_scen[1],"_global_annual_","2015_2100",".nc")
            if(!file.exists(fn)) {
              cat("skipping",fn,"\n")
              skip <- TRUE
            } else{
              cat("doing",fn,"\n")
            }
            if(!skip){ # if file exists or crop files exist but no EPIC mgr file
              #rainfed production
              # only multi-annual average for reference period
              # if(!skip){ # only non-mgr
              yieldref.p.rf <- getData.nc(paste(general.path,mo, "/phase3b/",gc,"/",sep=""),
                                          rcps, crops1[cr],
                                          paste("/",tolower(mo),"_",tolower(gc),"_",sep=""),
                                          "historical",
                                          syeara,nyeara,readfa,readla,
                                          spam.r[,,cr],irr_scen[1],
                                          mo,gc,sum(readla-readfa+1),ENERG_DM[cr])$av
              yieldref.p <- yieldref.p.rf
              #cat(range(yieldref.p,na.rm=T),range(spam.r[,,cr],na.rm=T),"\n")
              #yieldref <- sum(yieldref.p,na.rm=T)
              fut <- getData.nc(paste(general.path,mo, "/phase3b/",gc,"/",sep=""),
                                rcps, crops1[cr],
                                paste("/",tolower(mo),"_",tolower(gc),"_",sep=""),
                                co,
                                syear3,nyear3,readf3,readl3,
                                spam.r[,,cr],irr_scen[1],
                                mo,gc,sum(readla-readfa+1),ENERG_DM[cr])
              yieldfut.p <- fut$av
              yieldfut.p.rf <- fut$av
              yieldfut.annual <- fut$all
              yieldfut.annual.rf <- fut$all
              rm(fut)

              yieldref.p.ir <- getData.nc(paste(general.path,mo, "/phase3b/",gc,"/",sep=""),
                                          rcps, crops1[cr],
                                          paste("/",tolower(mo),"_",tolower(gc),"_",sep=""),
                                          "historical",
                                          syeara,nyeara,readfa,readla,
                                          spam.i[,,cr],irr_scen[2],
                                          mo,gc,sum(readla-readfa+1),ENERG_DM[cr])$av
              yieldref.p <- yieldref.p + yieldref.p.ir
              #cat(range(yieldref.p,na.rm=T),range(spam.i[,,cr],na.rm=T),"\n")
              fut <- getData.nc(paste(general.path,mo, "/phase3b/",gc,"/",sep=""),
                                rcps, crops1[cr],
                                paste("/",tolower(mo),"_",tolower(gc),"_",sep=""),
                                co,
                                syear3,nyear3,readf3,readl3,
                                spam.i[,,cr],irr_scen[2],
                                mo,gc,sum(readla-readfa+1),ENERG_DM[cr])
              yieldfut.p <- yieldfut.p + fut$av
              yieldfut.p.ir <- fut$av
              yieldfut.annual <- yieldfut.annual + fut$all
              yieldfut.annual.ir <- fut$all
              rm(fut)

              yieldfut <- sum(yieldfut.p,na.rm=T)
              yieldref <- sum(yieldref.p,na.rm=T)
              yieldfut.rf <- sum(yieldfut.p.rf,na.rm=T)
              yieldref.rf <- sum(yieldref.p.rf,na.rm=T)
              yieldfut.ir <- sum(yieldfut.p.ir,na.rm=T)
              yieldref.ir <- sum(yieldref.p.ir,na.rm=T)

              # adding row to output table
              output2 <- rbind(output2,c(yieldref,yieldfut))
              if(t==ts[1]) {
                output <- rbind(output,c(yieldref,yieldfut))
              }else {
                output <- rbind(output,yieldfut)
              }


              for(ff in country.code){
                nat.prod.ref1[length(cnames2)+1,which(country.code==ff),1] <- sum(yieldref.p.rf[countrymap==ff],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,which(country.code==ff),1] <- sum(yieldfut.p.rf[countrymap==ff],na.rm=T)
                nat.prod.ref1[length(cnames2)+1,which(country.code==ff),2] <- sum(yieldref.p.ir[countrymap==ff],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,which(country.code==ff),2] <- sum(yieldfut.p.ir[countrymap==ff],na.rm=T)
                nat.prod.ref1[length(cnames2)+1,which(country.code==ff),3] <- sum(yieldref.p[countrymap==ff],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,which(country.code==ff),3] <- sum(yieldfut.p[countrymap==ff],na.rm=T)

                for(i in 1:dim(nat.prod.fut1.annual)[4]){
                  buf <- yieldfut.annual.rf[,,i]
                  nat.prod.fut1.annual[length(cnames2)+1,which(country.code==ff),1,i] <- sum(buf[countrymap==ff],na.rm=T)
                  buf <- yieldfut.annual.ir[,,i]
                  nat.prod.fut1.annual[length(cnames2)+1,which(country.code==ff),2,i] <- sum(buf[countrymap==ff],na.rm=T)
                  buf <- yieldfut.annual[,,i]
                  nat.prod.fut1.annual[length(cnames2)+1,which(country.code==ff),3,i] <- sum(buf[countrymap==ff],na.rm=T)

                }

              }
              nat.prod.ref[length(cnames)+1,,] <- nat.prod.ref1[length(cnames2)+1,,]
              nat.prod.fut[length(cnames)+1,,] <- nat.prod.fut1[length(cnames2)+1,,]
              prod.total.ref <- prod.total.ref + yieldref
              prod.total.fut <- prod.total.fut + yieldfut
              # adding column name to column names
              cnames <- c(cnames,paste(tolower(mo),"_",tolower(gc),"_",rcps[2],"_",co,"_",crops1[cr],sep=""))
              cnames2 <- c(cnames2,paste(tolower(mo),"_",tolower(gc),"_",rcps[2],"_",co,"_",crops1[cr],sep=""))
              #whe, soy, ric, mai
              index.ref <- length(rcpso)*length(gcms)*(which(co2_scen==co)-1)+length(gcms)*(which(rcpso=="historical")-1)+which(gcms==gc)
              index.fut <- length(rcpso)*length(gcms)*(which(co2_scen==co)-1)+length(gcms)*(which(rcpso==rcps[2])-1)+which(gcms==gc)
              if(cr==1){
                global.sum.mai[which(models==mo),index.ref,which(ts==t)] <- yieldref
                global.sum.mai[which(models==mo),index.fut,which(ts==t)] <- yieldfut
              } else if(cr==2){ #ri1 ri2 soy swh wwh
                global.sum.ri1[which(models==mo),index.ref,which(ts==t)] <- yieldref
                global.sum.ri1[which(models==mo),index.fut,which(ts==t)] <- yieldfut
              } else if(cr==3){
                global.sum.ri2[which(models==mo),index.ref,which(ts==t)] <- yieldref
                global.sum.ri2[which(models==mo),index.fut,which(ts==t)] <- yieldfut
              } else if(cr==4){
                global.sum.soy[which(models==mo),index.ref,which(ts==t)] <- yieldref
                global.sum.soy[which(models==mo),index.fut,which(ts==t)] <- yieldfut
              } else if(cr==5){
                global.sum.swh[which(models==mo),index.ref,which(ts==t)] <- yieldref
                global.sum.swh[which(models==mo),index.fut,which(ts==t)] <- yieldfut
              } else if(cr==6){
                global.sum.wwh[which(models==mo),index.ref,which(ts==t)] <- yieldref
                global.sum.wwh[which(models==mo),index.fut,which(ts==t)] <- yieldfut
              }

              if(cr==6){ # sum total crops and rice and wheats
                output2 <- rbind(output2,c(prod.total.ref,prod.total.fut))

                if(t==ts[1]) {
                  output <- rbind(output,c(prod.total.ref,prod.total.fut))
                }else {
                  output <- rbind(output,prod.total.fut)
                }

                # add up all columns (all crops but not pasture) to compute total crop change
                nat.prod.ref1[length(cnames2)+1,,1] <- colSums(nat.prod.ref1[(length(cnames2)-5):(length(cnames2)),,1],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,,1] <- colSums(nat.prod.fut1[(length(cnames2)-5):(length(cnames2)),,1],na.rm=T)
                nat.prod.ref1[length(cnames2)+1,,2] <- colSums(nat.prod.ref1[(length(cnames2)-5):(length(cnames2)),,2],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,,2] <- colSums(nat.prod.fut1[(length(cnames2)-5):(length(cnames2)),,2],na.rm=T)
                nat.prod.ref1[length(cnames2)+1,,3] <- colSums(nat.prod.ref1[(length(cnames2)-5):(length(cnames2)),,3],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,,3] <- colSums(nat.prod.fut1[(length(cnames2)-5):(length(cnames2)),,3],na.rm=T)
                for(i in 1:dim(nat.prod.fut1.annual)[4]){
                  nat.prod.fut1.annual[length(cnames2)+1,,1,i] <- colSums(nat.prod.fut1.annual[(length(cnames2)-5):(length(cnames2)),,1,i],na.rm=T)
                  nat.prod.fut1.annual[length(cnames2)+1,,2,i] <- colSums(nat.prod.fut1.annual[(length(cnames2)-5):(length(cnames2)),,2,i],na.rm=T)
                  nat.prod.fut1.annual[length(cnames2)+1,,3,i] <- colSums(nat.prod.fut1.annual[(length(cnames2)-5):(length(cnames2)),,3,i],na.rm=T)
                }
                nat.prod.ref[length(cnames)+1,,] <- nat.prod.ref1[length(cnames2)+1,,]
                nat.prod.fut[length(cnames)+1,,] <- nat.prod.fut1[length(cnames2)+1,,]

                cnames <- c(cnames,paste(tolower(mo),"_",tolower(gc),"_",rcps[2],"_",co,"_total",sep=""))
                cnames2 <- c(cnames2,paste(tolower(mo),"_",tolower(gc),"_",rcps[2],"_",co,"_total",sep=""))

                nat.prod.ref1[length(cnames2)+1,,1] <- colSums(nat.prod.ref1[(2:3),,1],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,,1] <- colSums(nat.prod.fut1[(2:3),,1],na.rm=T)
                nat.prod.ref1[length(cnames2)+1,,2] <- colSums(nat.prod.ref1[(2:3),,2],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,,2] <- colSums(nat.prod.fut1[(2:3),,2],na.rm=T)
                nat.prod.ref1[length(cnames2)+1,,3] <- colSums(nat.prod.ref1[(2:3),,3],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,,3] <- colSums(nat.prod.fut1[(2:3),,3],na.rm=T)
                for(i in 1:dim(nat.prod.fut1.annual)[4]){
                  nat.prod.fut1.annual[length(cnames2)+1,,1,i] <- colSums(nat.prod.fut1.annual[(2:3),,1,i],na.rm=T)
                  nat.prod.fut1.annual[length(cnames2)+1,,2,i] <- colSums(nat.prod.fut1.annual[(2:3),,2,i],na.rm=T)
                  nat.prod.fut1.annual[length(cnames2)+1,,3,i] <- colSums(nat.prod.fut1.annual[(2:3),,3,i],na.rm=T)
                }
                nat.prod.ref[length(cnames)+1,,] <- nat.prod.ref1[length(cnames2)+1,,]
                nat.prod.fut[length(cnames)+1,,] <- nat.prod.fut1[length(cnames2)+1,,]

                cnames <- c(cnames,paste(tolower(mo),"_",tolower(gc),"_",rcps[2],"_",co,"_ric",sep=""))
                cnames2 <- c(cnames2,paste(tolower(mo),"_",tolower(gc),"_",rcps[2],"_",co,"_ric",sep=""))


                nat.prod.ref1[length(cnames2)+1,,1] <- colSums(nat.prod.ref1[(5:6),,1],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,,1] <- colSums(nat.prod.fut1[(5:6),,1],na.rm=T)
                nat.prod.ref1[length(cnames2)+1,,2] <- colSums(nat.prod.ref1[(5:6),,2],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,,2] <- colSums(nat.prod.fut1[(5:6),,2],na.rm=T)
                nat.prod.ref1[length(cnames2)+1,,3] <- colSums(nat.prod.ref1[(5:6),,3],na.rm=T)
                nat.prod.fut1[length(cnames2)+1,,3] <- colSums(nat.prod.fut1[(5:6),,3],na.rm=T)
                for(i in 1:dim(nat.prod.fut1.annual)[4]){
                  nat.prod.fut1.annual[length(cnames2)+1,,1,i] <- colSums(nat.prod.fut1.annual[(5:6),,1,i],na.rm=T)
                  nat.prod.fut1.annual[length(cnames2)+1,,2,i] <- colSums(nat.prod.fut1.annual[(5:6),,2,i],na.rm=T)
                  nat.prod.fut1.annual[length(cnames2)+1,,3,i] <- colSums(nat.prod.fut1.annual[(5:6),,3,i],na.rm=T)
                }
                nat.prod.ref[length(cnames)+1,,] <- nat.prod.ref1[length(cnames2)+1,,]
                nat.prod.fut[length(cnames)+1,,] <- nat.prod.fut1[length(cnames2)+1,,]

                cnames <- c(cnames,paste(tolower(mo),"_",tolower(gc),"_",rcps[2],"_",co,"_whe",sep=""))
                cnames2 <- c(cnames2,paste(tolower(mo),"_",tolower(gc),"_",rcps[2],"_",co,"_whe",sep=""))


                global.sum.all[which(models==mo),index.ref,which(ts==t)] <- prod.total.ref
                global.sum.all[which(models==mo),index.fut,which(ts==t)] <- prod.total.fut
              }
            } #!skip
          } # for cr
          grf <- grs[which(time==t)]
          growth.rates1 <- (nat.prod.fut1/nat.prod.ref1)^(1/grf)-1
          growth.rates1[!is.finite(growth.rates1)] <- NA
          percent.change.annual <- array(NA,dim=dim(nat.prod.fut1.annual))
          percent.shock.annual <- array(NA,dim=dim(nat.prod.fut1.annual))
          for(i in 1:dim(nat.prod.fut1.annual)[4]){
            percent.change.annual[,,,i] <- (nat.prod.fut1.annual[,,,i]/nat.prod.ref1-1)*100
            percent.shock.annual[,,,i] <- (nat.prod.fut1.annual[,,,i]/nat.prod.fut1-1)*100
          }
          percent.change1 <- (nat.prod.fut1/nat.prod.ref1-1)*100
          percent.change1[!is.finite(percent.change1)] <- NA
          percent.change.annual[!is.finite(percent.change.annual)] <- NA
          percent.shock.annual[!is.finite(percent.shock.annual)] <- NA

          if(!is.null(output2)){
            rownames(output2) <- c("mai","ri1","ri2","soy","swh","wwh","total")
            # rownames should be cnames2 FIX
            colnames(output2) <- c("2000s",t)
            write.csv(format(output2,scientific=F),paste(output.path,"/ISI-MIP3_production_changes_",t,"_vs_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_isimip3b_national_all_crops.csv",sep=""),quote=F)
          }
          colnames(growth.rates1) <- ISO.code #countrylist
          rownames(growth.rates1) <- cnames2
          if(!all(is.na(growth.rates1))) write.csv(format(growth.rates1[,,1],scientific=F),paste(output.path,"/ISI-MIP3_growth_rates_30-year_average_",t,"_vs_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_rf_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
          if(!all(is.na(growth.rates1))) write.csv(format(growth.rates1[,,2],scientific=F),paste(output.path,"/ISI-MIP3_growth_rates_30-year_average_",t,"_vs_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_ir_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
          if(!all(is.na(growth.rates1))) write.csv(format(growth.rates1[,,3],scientific=F),paste(output.path,"/ISI-MIP3_growth_rates_30-year_average_",t,"_vs_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
          colnames(nat.prod.ref1) <- ISO.code #countrylist
          rownames(nat.prod.ref1) <- cnames2
          if(!all(is.na(nat.prod.ref1))) write.csv(format(nat.prod.ref1[,,1],scientific=F),paste(output.path,"/ISI-MIP3_production_reference_30-year_average_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_rf_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
          if(!all(is.na(nat.prod.ref1))) write.csv(format(nat.prod.ref1[,,2],scientific=F),paste(output.path,"/ISI-MIP3_production_reference_30-year_average_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_ir_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
          if(!all(is.na(nat.prod.ref1))) write.csv(format(nat.prod.ref1[,,3],scientific=F),paste(output.path,"/ISI-MIP3_production_reference_30-year_average_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
          colnames(percent.change1) <- ISO.code #countrylist
          rownames(percent.change1) <- cnames2
          if(!all(is.na(percent.change1))) write.csv(format(percent.change1[,,1],scientific=F),paste(output.path,"/ISI-MIP3_percent_changes_30-year_average_",t,"_vs_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_rf_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
          if(!all(is.na(percent.change1))) write.csv(format(percent.change1[,,2],scientific=F),paste(output.path,"/ISI-MIP3_percent_changes_30-year_average_",t,"_vs_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_ir_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
          if(!all(is.na(percent.change1))) write.csv(format(percent.change1[,,3],scientific=F),paste(output.path,"/ISI-MIP3_percent_changes_30-year_average_",t,"_vs_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
          pca1 <- psa1 <- NULL
          pca1.ir <- psa1.ir <- NULL
          pca1.rf <- psa1.rf <- NULL
          if(!all(is.na(percent.change.annual))){
            for(tt in 1:dim(percent.change.annual)[1])
            {
              pca1 <- cbind(pca1,percent.change.annual[tt,,3,])
              psa1 <- cbind(psa1,percent.shock.annual[tt,,3,])
              pca1.ir <- cbind(pca1.ir,percent.change.annual[tt,,2,])
              psa1.ir <- cbind(psa1.ir,percent.shock.annual[tt,,2,])
              pca1.rf <- cbind(pca1.rf,percent.change.annual[tt,,1,])
              psa1.rf <- cbind(psa1.rf,percent.shock.annual[tt,,1,])
            }
            rownames(pca1) <- ISO.code #countrylist
            rownames(psa1) <- ISO.code #countrylist
            colnames(pca1) <- as.vector(t(outer(cnames2,c(1:dim(percent.change.annual)[4]),paste,sep="_")))
            colnames(psa1) <- as.vector(t(outer(cnames2,c(1:dim(percent.change.annual)[4]),paste,sep="_")))
            rownames(pca1.rf) <- ISO.code #countrylist
            rownames(psa1.rf) <- ISO.code #countrylist
            colnames(pca1.rf) <- as.vector(t(outer(cnames2,c(1:dim(percent.change.annual)[4]),paste,sep="_")))
            colnames(psa1.rf) <- as.vector(t(outer(cnames2,c(1:dim(percent.change.annual)[4]),paste,sep="_")))
            rownames(pca1.ir) <- ISO.code #countrylist
            rownames(psa1.ir) <- ISO.code #countrylist
            colnames(pca1.ir) <- as.vector(t(outer(cnames2,c(1:dim(percent.change.annual)[4]),paste,sep="_")))
            colnames(psa1.ir) <- as.vector(t(outer(cnames2,c(1:dim(percent.change.annual)[4]),paste,sep="_")))
            write.csv(format(t(pca1.rf),scientific=F),paste(output.path,"/ISI-MIP3_percent_changes_30-year_annual_",t,"_vs_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_rf_isimip3b_national_all_crops.csv",sep=""),quote=F)
            write.csv(format(t(psa1.rf),scientific=F),paste(output.path,"/ISI-MIP3_percent_shocks_30-year_annual_",t,"_vs_average",t,"_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_rf_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
            write.csv(format(t(pca1.ir),scientific=F),paste(output.path,"/ISI-MIP3_percent_changes_30-year_annual_",t,"_vs_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_ir_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
            write.csv(format(t(psa1.ir),scientific=F),paste(output.path,"/ISI-MIP3_percent_shocks_30-year_annual_",t,"_vs_average",t,"_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_ir_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
            write.csv(format(t(pca1),scientific=F),paste(output.path,"/ISI-MIP3_percent_changes_30-year_annual_",t,"_vs_2000s_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
            write.csv(format(t(psa1),scientific=F),paste(output.path,"/ISI-MIP3_percent_shocks_30-year_annual_",t,"_vs_average",t,"_",mo,"_",gc,"_",rcpso[rcp],"_",co,"_for_isimip3b_national_all_crops.csv",sep=""),quote=F)
          }
          cat("end of",mo,"\n")
        } # for mo
        cat("end of",co,"\n")
      } # for co
    } # for sp
    cat("end of",gc,"\n")
  } # for gc

} # for rcp
save(global.sum.all,global.sum.wwh, global.sum.swh,global.sum.soy,global.sum.ri1,global.sum.ri2,global.sum.mai,file=paste0(output.path,"/ISI-MIP3_global_average_production_",t,"_rfir.Rdata"))
#save(output,prod.fut.p,prod.ref.p,fpu.prod.ref,fpu.prod.fut,spam.r,spam.i,file=paste(isimip.lpj,"yields/ISI-MIP3_production_changes_rcp2p6_and_8p5_with_and_without_co2_for_ERL.Rdata",sep=""))
#load(paste(isimip.lpj,"yields/jrc_x12_d3/ISI-MIP3_production_changes_rcp2p6_and_8p5_with_and_without_co2_for_JRC_X12_D3.Rdata",sep=""))


#save(list = ls(all = TRUE), file = paste(output.path,"/ISI-MIP3_production_changes_rcp2p6_and_8p5_with_and_without_co2_for_JRC_X12_D3_final_national_all_crops.Rdata",sep=""))
}
