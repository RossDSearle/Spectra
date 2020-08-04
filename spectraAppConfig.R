

dropBoxPath <- 'SpectraFiles/Ross'
currentUser <- 'Ross'

spectraAPIServer <- 'http://esoil.io/APIDev'
startLat <- -36.33387
startLon <- 141.5368 

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  localUploadDir <- '/srv/shiny-server/Spectra'
}else{
  localUploadDir<- 'C:/temp/specl'
}


spectralDevices <- c('ASD', 'Some Other Device', 'And Another Device')