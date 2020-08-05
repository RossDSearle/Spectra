library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(dygraphs)
library(httr)
library(jsonlite)
library(xts)
library(shinybusy)
library(shinyjs)
library(rhandsontable)
library(rdrop2)
library(RCurl)

debugMode <- F

defWidth = 380
loaderTime = 1
DEF_User = 'DemoUser'


machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  rootDir <- '/srv/shiny-server/Spectra'
}else{
  rootDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/Spectra'
}

source(paste0(rootDir, '/spectraAppConfig.R'))

spectraServer <- 'http://esoil.io/APIDev'



token <- readRDS("droptoken.rds")
#print(drop_acc(dtoken = token))
fls <- drop_dir(dropBoxPath, dtoken = token)  %>% data.frame()
spectraFiles <- fls$name

depthVals <- seq(0, 200, 5)

shiny::shinyApp(
  ui = f7Page(
    title = "SpectraCloud",
    init = f7Init(skin = "auto", theme = "light", filled = T, color = 'lightblue'),
    tags$head(tags$link( rel="icon", type="image/png", href="icons8-area-chart-64.png", sizes="32x32" ),
              tags$link( rel="apple-touch-icon", href="apple-touch-icon.png" )
              ),
    
    
    tags$script('
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }

   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude);
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("lat", coords.latitude);
          Shiny.onInputChange("long", coords.longitude);
      }, 1100)
  }
  });
'),
    
    
    useShinyjs(),
    
    #add_busy_bar(color = "#FF0000", centered = FALSE, height = "18px"),
    #add_busy_spinner(spin = "fading-circle"),
  #  add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
    
    #title = NULL,
    preloader = F,
    loading_duration = loaderTime,
    f7TabLayout(
     panels = tagList(
     f7Panel(title = "About", side = "right", theme = "light", effect = "cover",

             HTML('<H1> About</H1>This is a demo App to explore some possible use cases and workflows for the Soil Spectral Infrastructure project,<br><br><br>'),
 
             f7Block(
               strong = TRUE, inset = T,
               f7BlockHeader(text = "Login"),
               
               f7Text( inputId = 'wgtLoginUser', label = 'Username', placeholder='Demo'),
               f7Password(inputId = 'wgtLoginPwd', label = 'Password', placeholder='Demo'),
               f7Button(inputId = 'wgtLoginButton', label = 'Login', src = NULL, color = 'blue', fill = TRUE, outline = F, shadow = T, rounded = T)
             )

             # f7Link(label = "About BARS", src = "https://www.csiro.au/en/Research/AF/Areas/Boorowa-Agricultural-Research-Station", external = TRUE),
             # f7Link(label = "About CSIRO Ag & Food", src = "https://www.csiro.au/en/Research/AF", external = TRUE),
             # f7Link(label = "About CSIRO", src = "https://www.csiro.au", external = TRUE),
             # f7Link(label = "BoM Boowora", src = "http://www.bom.gov.au/places/nsw/boorowa/", external = TRUE),

             )
     #f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
     ),
 
##################################  NAVIGATION BAR   ##################################      
      navbar = f7Navbar(
        title = tags$div(
          tags$div(style="float: left;", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px", align='left'), HTML('&nbsp&nbsp&nbsp'),
          tags$div(style="font-size: 30px; vertical-align:middle!important; text-align:left!important; display:inline-block;", "SpectraCloud") 
                         ),
         ),
        hairline = T,
        shadow = T,
        left_panel = F,
        right_panel = T
      ),


##################################  UI - Submit Spectra  ##################################         
      
      f7Tabs(
        animated = T,
        id = "tabsAll",
        #swipeable = TRUE,
        f7Tab(
          tabName = "Submit",
          icon = f7Icon("cloud_upload"),
          active = TRUE,

          f7Float( f7Shadow(
            intensity = 24,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth, 'px'),
                      f7Card(
                        id = 'crdMap',
                        
                        fluidRow(f7Select(inputId = 'wgtselectSpectra', label = 'Choose a spectra file to upload',choices= spectraFiles)),
                        fluidRow(leafletOutput("wgtlocationMap", height = defWidth-50, width = defWidth-30)),
                        fluidRow( tags$div( style=paste0("width: 150px"),f7Text(inputId = 'wgtLonVal', label = 'Longitude')),
                                  tags$div( style=paste0("width: 150px"),f7Text(inputId = 'wgtLatVal', label = 'Latitude'))
                        ),
                        fluidRow( tags$div( style=paste0("width: 150px"),f7Select(inputId = 'wgtFromDepth', label = 'Upper Depth (cm)',choices= depthVals)),
                                  tags$div( style=paste0("width: 150px"),f7Select(inputId = 'wgtToDepth', label = 'Lower Depth (cm)',choices= depthVals))
                        ),
                        fluidRow( tags$div( style=paste0("width: 150px"),f7Select(inputId = 'wgtSpecType', label = 'Spectra Type',choices=spectralDevices))
                        ),          
                       
                        fluidRow(f7Button(inputId = 'wgtSubmitSample', label = 'Submit Spectra', src = NULL, color = 'green', fill = TRUE, outline = F, shadow = T, rounded = T)
                        )
                                  
                        
                      )
            )
          ),
          side = "left" )
          
          
        ),
        
        
        ##################################  UI - My Spectra   ##################################          
        f7Tab(
          tabName = "My Spectra",
          icon = f7Icon("lock_fill"),
          active = FALSE,
          f7Float(  
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              
              
              
              tags$div( style=paste0("width: ", defWidth, 'px'),
                        
                        f7Card(
                          
                          title = 'Choose a spectra to display from the map above of the list below',
                          fluidRow(leafletOutput("wgtMySpectraMap", height = defWidth-50, width = defWidth-30)),
                          
                          fluidRow( f7Picker(inputId = 'wgtMySpectraIDs', label='Choose a spectra to display from the map above of the list below',  choices = c('') )),
                        #  f7SmartSelect( inputId = 'wgtMySpectraSS' ,  label='Choose a spectra to display',  choices = c(''))
                         # f7DatePicker( inputId = 'wgtStartDate', label='Start Date')
                          
                        ))), side = "left"), 
          
          f7Float(  
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              
              tags$div( style=paste0("width: ", defWidth),           
                        f7Card(
                          
                          fluidRow(htmlOutput("wgtSpecInfoTxt")),
                          fluidRow(HTML('<BR>')),
                           fluidRow(f7Button(inputId = 'wgtButgetSpectraInfo', label = 'Get Spectra Data', src = NULL, color = 'green', fill = TRUE, outline = F, shadow = T, rounded = T))
                                   
                          
                        )
                        
              ))), side = "left"),
        
        
################################## UI - Spectra RESULTS   ##################################           
        f7Tab(
          tabName = "Spectra Results",
          icon = f7Icon("cloud_download"),
          active = FALSE,

          f7Float( 
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            div( style=paste0("width: ", defWidth ,"px; align='left'; vertical-align: middle;"),
                 f7Card(
                   title = 'Soil Analysis',
                   rHandsontableOutput(outputId = 'wgtSoilPropTable'),
                   HTML('<BR>')
                 )
            )
          ),
          
        ),
        f7Float( 
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            div( style=paste0("width: ", defWidth ,"px; align='left'; vertical-align: middle;"),
                 f7Card(
                   title = 'Raw Spectra',
                   plotOutput(outputId =  'wgtSpectraPlot'),
                   HTML('<BR>')
                 )
            )
          ),
          
        )
        ,
        f7Float( 
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            div( style=paste0("width: ", defWidth ,"px; align='left'; vertical-align: middle;"),
                 f7Card(
                   title = 'Metadata',
                   rHandsontableOutput(outputId = 'wgtMetadataTable'),
                   HTML('<BR>')
                 )
            )
          ),
          
        )
        ,side = "left"),
        
        
            
            
##################################  UI - SOIL DATA MAP   ##################################             
        
        f7Tab(
          tabName = "Preferences",
          icon = f7Icon("gear"),
          active = FALSE,
          f7Float(
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),  
            f7Card(
              title = NULL,
              fluidRow( f7Select('SoilPropList', "Select soil attribute", choices=c('clay', 'ecec', 'phc', 'soc')),  f7Select('SoilDepthList', "Select depth (cm)", choices=c('d1', 'd2', 'd3', 'd4'))),
              #f7Select('SoilPropList', "Select soil attribute", choices=c('clay', 'ecec', 'phc', 'soc')),
              HTML('<BR>'),
              leafletOutput("soilMap", height = 400),
              rHandsontableOutput('soilDataTable' )
              #tableOutput('soilDataTable' )
            )
            )
          )
          , side = "left")
        )
        
      )
    )
  ),


##################################  SERVER  ##################################   
  server = function(input, output, session) {
    
    session$allowReconnect(TRUE)
    
    RV <- reactiveValues()
    RV$currentUser=NULL
    RV$AllowGeoLocation=NULL
    RV$currentLoc=NULL
    RV$currentSpectraResults=NULL
    RV$AvailSepctra=NULL
    RV$currentSelectedSpectraID=NULL

    ##################################  SERVER - GLOBAL PROCESSING   ##################################
    
    acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=8, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
    
    
    output$wgtSpecInfoTxt <- renderUI({
    req(RV$currentSelectedSpectraID)
      
    rec <- RV$AvailSepctra[RV$AvailSepctra$SpectraID == RV$currentSelectedSpectraID, ]
    asrisID <- paste0(rec$agency_code, '_', rec$proj_code, '_',rec$s_id, '_',rec$o_id, '_',rec$h_no, '_',rec$samp_no)
    HTML(paste0('
    <H3>Metadata For ', rec$SpectraID, ' </H3>
    <table><tr><td><b>ASRIS ID : </b></td><td>',asrisID  ,'</td></tr></table>
    <table><tr><td><b>ASpecta Type : </b></td><td>',rec$Type  ,'</td></tr></table>
     <table><tr><td><b>AUser Name : </b></td><td>',rec$Username  ,'</td></tr></table>
      <table><tr><td><b>ATime Submitted Type : </b></td><td>',rec$SubmitTime  ,'</td></tr></table>
       <table><tr><td><b>ASpecta Latitude : </b></td><td>',rec$Lattitude  ,'</td></tr></table>
        <table><tr><td><b>ASpecta Longitude : </b></td><td>',rec$Longitude  ,'</td></tr></table>
         <table><tr><td><b>ASpecta Upper Depth : </b></td><td>',rec$UpperDepth  ,'</td></tr></table>
          <table><tr><td><b>ASpecta LowerDepth : </b></td><td>',rec$LowerDepth  ,'</td></tr></table>
           <table><tr><td><b>ASpecta Origina lName : </b></td><td>',rec$OriginalName  ,'</td></tr></table>
    ')) })
    
    observe({
      
      req(RV$AvailSepctra)
      clickm <-input$wgtMySpectraMap_marker_click
      if(is.null(clickm))
        return()
      
      sid <- clickm$id
      print(sid)
      RV$currentSelectedSpectraID <- sid
     
    })
    
    output$wgtMySpectraMap <- renderLeaflet({
      
      req(RV$AvailSepctra)
          print(RV$AvailSepctra)
      lats <- RV$AvailSepctra$Lattitude
      lons <- RV$AvailSepctra$Longitude
      specIds <- RV$AvailSepctra$SpectraID
      
      labs <- lapply(seq(nrow(RV$AvailSepctra)), function(i) {
        paste0( '<li>SpectraID : ', RV$AvailSepctra[i, "SpectraID"], '</li>
                <li>File Name : ', RV$AvailSepctra[i, "OriginalName"], '</li>
                <li>Upper Depth : ', RV$AvailSepctra[i, "UpperDepth"], '</li>
                <li>Lower Depth : ', RV$AvailSepctra[i, "LowerDepth"], '</li>'
                )
        
      })
      
      leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
        clearMarkers%>%addMarkers(lng = lons, lat = lats, label =  lapply(labs, HTML), layerId=specIds)%>%
        # setView(lng = input$long, lat = input$lat, zoom = 18) %>% #### this sets view to your current location
        
        setView(lng = startLon, lat = startLat, zoom = 14) %>%
        addControlGPS() %>%
        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
          overlayGroups = c( "My Spectra"),
          options = layersControlOptions(collapsed = T)
        )
    })
    
    observe({
      
     req(RV$currentUser)
      
      url <- paste0('http://esoil.io/APIDev/SoilSpectra/availableSpectra?username=DemoUser')
      response <-  GET(paste0(url))
      
      stream <- content(response, as="text", encoding	='UTF-8')
      dc <- fromJSON(stream)
      RV$AvailSepctra <- dc
      items <- RV$AvailSepctra$SpectraID
      
      updateF7Picker(session=session, inputId = 'wgtMySpectraIDs',
                     value = items[1],
                     choices = items
                     )
      
      # updateF7Picker(session=session, inputId = 'wgtMySpectraIDs',
      #                value = items[1],
      #                choices = items,
      #                openIn = "auto",
      #                toolbarCloseText = "Done",
      #                toolbar = TRUE,
      #                closeByOutsideClick = FALSE,
      #                sheetSwipeToClose = FALSE
      # )

     })
    
  
    observeEvent(input$wgtLoginButton, {
      
      f7Dialog(
        title = "Its only a demo",
        text = "Authentication is not enabled as yet but could be if needs be.",
        session = session
      )
      
    })
    
    output$wgtSpectraPlot = renderPlot({
      req(RV$currentSpectraResults$Spectrum)
        plot(RV$currentSpectraResults$Spectrum, type='l', col='red')
    })
    
    
    output$wgtMetadataTable = renderRHandsontable({
      req(RV$currentSpectraResults)
      if(nrow(RV$currentSpectraResults$Metadata) > 0){
        rhandsontable(RV$currentSpectraResults$Metadata, manualColumnResize = T, readOnly = TRUE, rowHeaders = F)%>%
          hot_table(highlightCol = F, highlightRow = F) 
      }else{
        return(NULL)
      }
    })
    
    output$wgtSoilPropTable = renderRHandsontable({
      req(RV$currentSpectraResults)
      if(nrow(RV$currentSpectraResults$SoilValues) > 0){
        rhandsontable(RV$currentSpectraResults$SoilValues,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F)%>%
          hot_table(highlightCol = F, highlightRow = F)
      }else{
        return(NULL)
      }
    })
    
    
    observeEvent( RV$currentLoc, {
      req(RV$currentLoc)
      
      #print(RV$currentLoc)
      # updateF7Text("wgtLonVal", value = formatC(RV$currentLoc$lng, digits = 5, format = "f") )
      # updateF7Text("wgtLatVal", value = formatC(RV$currentLoc$lat, digits = 5, format = "f"))
    })
    
    observe({
      RV$AllowGeoLocation= input$geolocation
     
    })

    
    
    output$wgtlocationMap <- renderLeaflet({
      
      RV$currentUser = DEF_User  # need this here to fire existing spectra list update
      
      leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%

       # setView(lng = input$long, lat = input$lat, zoom = 18) %>% #### this sets view to your current location
        
        setView(lng = startLon, lat = startLat, zoom = 14) %>%
        addControlGPS() %>%
        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
         # overlayGroups = c( "SW Probes"),
          options = layersControlOptions(collapsed = T)
        )
    })
    
    observeEvent(input$wgtlocationMap_click, {
      click = input$wgtlocationMap_click
      
      leafletProxy('wgtlocationMap')%>%clearMarkers%>%addMarkers(lng = click$lng, lat = click$lat)
      RV$currentLoc$lng = click$lng 
      RV$currentLoc$lat = click$lat
       updateF7Text(session = session, inputId="wgtLonVal", value = formatC(click$lng, digits = 5, format = "f") )
       updateF7Text(session = session, inputId="wgtLatVal", value = formatC(click$lat, digits = 5, format = "f"))
       
    })
    
    
    observeEvent(input$wgtSubmitSample, {
      
      req(input$wgtSubmitSample)
      tmpFile <- tempfile(pattern = 'upSpec_', tmpdir = localUploadDir, fileext = '.dat')
print(tmpFile)
      if(!debugMode){
          drop_download(path='spectrafiles/ross/archive_20392.asd', local_path = tmpFile, dtoken = token  )
    
          response <-  POST(paste0(spectraAPIServer, '/SoilSpectra/Upload'), body = list(fileinfo = upload_file(tmpFile),
                                                                             
                                                                             longitude=RV$currentLoc$lng ,
                                                                             latitude=RV$currentLoc$lat ,
                                                                             upperDepth=input$wgtFromDepth,
                                                                             lowerDepth=input$wgtToDepth,
                                                                             userName=currentUser,
                                                                             specType=input$wgtSpecType,
                                                                             format='json'
                                                                             ))
          stream <- content(response, as="text", encoding	='UTF-8')
          RV$currentSpectraResults <- fromJSON(stream)
      
      }else{
        RV$currentSpectraResults <- readRDS('c:/temp/payload.rds')
      }
         
      updateF7Tabs(session, id = "tabsAll", selected = "Spectra Results")
    
      
      
        
      
    })
    
    
  }
)









