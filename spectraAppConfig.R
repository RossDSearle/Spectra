

dropBoxPath <- 'SpectraFiles/Ross'
currentUser <- 'Ross'

spectraAPIServer <- 'http://esoil.io/APIDev'

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  localUploadDir <- '/srv/shiny-server/Spectra'
}else{
  localUploadDir<- 'C:/temp/specl'
}