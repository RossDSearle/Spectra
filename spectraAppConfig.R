

dropBoxPath <- 'SpectraFiles/Ross'
currentUser <- 'DemoUser'

spectraAPIServer <- 'http://esoil.io/APIDev'
startLat <- -36.33387
startLon <- 141.5368 

machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  localUploadDir <- '/mnt/data/Spectra/tmpUploads'
}else{
  localUploadDir<- 'C:/temp/specl'
}


spectralDevices <- c('ASD', 'Some Other Device', 'And Another Device')