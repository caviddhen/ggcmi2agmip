#' @title runProcessing
#' @description does the processing of the isimip3b ggcmi crop yields into deisred output
#' @return writes csv and returns a dataframe
#' @param path convert
#' @param rcpso convert
#' @param gcms gcms
#' @param models models
#' @param time time scale of interest
#' @param co2_scen convert
#' @param output_format convert into desired format
#' @author David Chen, Christoph Mueller
#' @export
#'

runProcessing <- function(general.path = "/p/projects/macmit/data/GGCMI/AgMIP.output/",
                          lpj.spam.path = "/p/projects/macmit/data/GGCMI/fast-track/",
                          output.path = "/p/projects/landuse/users/davidch/AgMIP_impacts/ISIMIP3b",
                          pasture.path = "/p/projects/lpjml/input/historical/input_VERSION2/",
                          rice.path = "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar/",
                          wheat.path = "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/landuse/winter_spring_wheat_separation/",
                         adm.path= "/p/projects/macmit/data/GGCMI/AgMIP.output/processed/masks/aggr/"
                          ){


models <- c("LPJmL","EPIC-IIASA","pDSSAT","GEPIC","LPJ-GUESS")
rcpso <- c("historical","picontrol","ssp126","ssp585")
gcms <- c("gfdl-esm4","mpi-esm1-2-hr","mri-esm2-0","ukesm1-0-ll")
co2s <- c("2015co2","default")
irrs <- c("noirr","firr")


time <- c("2030s","2050s","2085s", "2100")




# DO THE THING
#for(rcp in 2:length(rcpso)){

}
