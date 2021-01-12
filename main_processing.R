"C:/Users/mychen/Dropbox/AgMIP_project"


runProcessing <- function(general.path = "/p/projects/macmit/data/GGCMI/AgMIP.output/",
                          lpj.spam.path = "/p/projects/macmit/data/GGCMI/fast-track/yields/yield_shifter_econ_models/some_spam/",
                          output.path = "/p/projects/landuse/users/davidch/AgMIP_impacts/ISIMIP3b",
                          pasture.path = "/p/projects/lpjml/input/historical/input_VERSION2/",
                          rice.path = "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar/",
                          wheat.path = "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/landuse/winter_spring_wheat_separation/",
                          adm.path= "/p/projects/macmit/data/GGCMI/AgMIP.output/processed/masks/aggr/",
                          models <- c("LPJmL","EPIC-IIASA","pDSSAT","GEPIC","LPJ-GUESS"),
                          rcpso <- c("historical","picontrol","ssp126","ssp585"),
                          gcms <- c("gfdl-esm4","mpi-esm1-2-hr","mri-esm2-0","ukesm1-0-ll")
                          co2s <- c("2015co2","default"),
                          irrs <- c("noirr","firr"),
                          time <- c("2030s","2050s","2085s", "2100"))

#
# runProcessing <- processIsimip(general.path = "C:/Users/mychen/Dropbox/AgMIP_project/data/",
#                           lpj.spam.path = "C:/Users/mychen/Dropbox/AgMIP_project/data/",
#                           output.path = "C:/Users/mychen/Dropbox/AgMIP_project/data/output/",
#                           rice.path = "C:/Users/mychen/Dropbox/AgMIP_project/data/",
#                           wheat.path = "C:/Users/mychen/Dropbox/AgMIP_project/data/",
#                           adm.path= "C:/Users/mychen/Dropbox/AgMIP_project/data/",
#                           models = c("EPIC-IIASA"),
#                           rcpso = c("historical","ssp126","ssp585"),
#                           gcms = c("ukesm1-0-ll"),
#                           co2_scen = c("2015co2","default"),
#                           irr_scen = c("noirr","firr"),
#                           time = c("2030s","2050s","2085s", "2100"))
