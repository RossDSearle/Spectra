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



defWidth = '380px'
loaderTime = 1



machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  rootDir <- '/srv/shiny-server/Spectra'
}else{
  rootDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/Spectra'
}

source(paste0(rootDir, '/spectraAppConfig.R'))

spectraServer <- 'http://esoil.io/APIDev'



token <- readRDS("droptoken.rds")
print(drop_acc(dtoken = token))
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
     # panels = tagList(
        # f7Panel(title = "About", side = "left", theme = "dark", effect = "cover",
        #         
        #         # f7Link(label = "About BARS", src = "https://www.csiro.au/en/Research/AF/Areas/Boorowa-Agricultural-Research-Station", external = TRUE),
        #         # f7Link(label = "About CSIRO Ag & Food", src = "https://www.csiro.au/en/Research/AF", external = TRUE),
        #         # f7Link(label = "About CSIRO", src = "https://www.csiro.au", external = TRUE),
        #         # f7Link(label = "BoM Boowora", src = "http://www.bom.gov.au/places/nsw/boorowa/", external = TRUE),
        #         
        #         f7PanelItem(title = 'test', tabName = 'Weather'),
        #         f7Link(label = "BoM Boowora", src = "Weather", external = F)
        #         
        #         
        #         
        #         )
        #f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
     # ),
 
##################################  NAVIGATION BAR   ##################################      
      navbar = f7Navbar(
       # title = shiny::tags$div(style="background-image: url('Logos/HdrBkGrdImage.PNG');", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px"), "Boowora Agricultutral Research Station "),
        title = tags$div( tags$div(style="vertical-align:middle!important; text-align:left!important; display:inline-block;", "SpectraCloud"), HTML('&nbsp&nbsp&nbsp'), tags$div(style="float: right;", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px", align='right'))),
        hairline = F,
        shadow = T,
        left_panel = F,
        right_panel = F
      ),


##################################  UI - SOIL MOISTURE PROBE MAP  ##################################         
      
      f7Tabs(
        animated = T,
        #swipeable = TRUE,
        f7Tab(
          tabName = "Submit",
          icon = f7Icon("cloud_upload", old = TRUE),
          active = TRUE,
          f7Float( f7Shadow(
            intensity = 24,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = NULL,
                        footer = NULL,
                        outline = FALSE,
                        height = NULL,
                        id = 'crdMap',
                        fluidRow(f7Select(inputId = 'wgtselectSpectra', label = 'Choose a spectra file',choices= spectraFiles)),
                        fluidRow(leafletOutput("wgtlocationMap", height = 400 )),
                        fluidRow( tags$div( style=paste0("width: 150px"),f7Text(inputId = 'wgtLonVal', label = 'Longitude')),
                                  tags$div( style=paste0("width: 150px"),f7Text(inputId = 'wgtLatVal', label = 'Latitude'))
                                ),
                        fluidRow( tags$div( style=paste0("width: 150px"),f7Select(inputId = 'wgtFromDepth', label = 'Upper Depth (cm)',choices= depthVals)),
                                  tags$div( style=paste0("width: 150px"),f7Select(inputId = 'wgtToDepth', label = 'Lower Depth (cm)',choices= depthVals))
                        ),
                        fluidRow(f7Button(inputId = 'wgtSubmitSample', label = 'Submit Spectra', src = NULL, color = 'green', fill = TRUE, outline = F, shadow = T, rounded = T, size = NULL)
                        )
                                  
                        
                      )
            )
          ),
          side = "left" ),
          
          
          f7Float(  f7Shadow(
            intensity = 100,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = "NotSureYet",
                        id = 'crdMap',
                        
                        
                      ))), side = "left" )
          
        ),

        
################################## UI - SOIL MOISTURE MAPS   ##################################           
        f7Tab(
          tabName = "Manage Sensors",
          icon = f7Icon("plus_square_fill_on_square_fill", old = F ),
          active = FALSE,
          f7Float( 
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            div( style=paste0("width: ", defWidth ,"; align='left'; vertical-align: middle;"),
                 f7Card(
                   title = NULL,
                   #f7DatePicker( "SMmapDate", label='Select Map Date', value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd" ),
                  # f7Select(inputId = 'SMDepthList', label = "Soil Depth",  choices =  soilDepthsDF$sdLabels),
                   HTML('<BR>'),
                   #div( style=paste0("width: 100px"),
                   f7Button(inputId = 'drawSMmapbtn', label = "Draw Soil Moisture Map", src = NULL, color = 'green', fill = TRUE, outline = F, shadow = T, rounded = T, size = NULL),
                   #),
                   HTML('<BR>'),
                   f7Progress(id = "pg1", value = 0, color = "blue"),
                   
                   
                   leafletOutput("moistureMap2", height = 400 )
                   
                 )
            )
          )
        ), side = "left"),
        
        
##################################  UI - WEATHER   ##################################          
        f7Tab(
          tabName = "Login",
          icon = f7Icon("lock_fill", old = F),
          active = FALSE,
          f7Float(  
            f7Shadow(
            intensity = 10,
            hover = TRUE,
            
            tags$div( style=paste0("width: ", defWidth),
                      
                       f7Card(
                        title = paste0("Todays Weather (", format(Sys.Date(), format="%B %d %Y"), ')' ),
                        
                        verbatimTextOutput("todaysRainfall"),
                        verbatimTextOutput("todaysMaxRainfall"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentTemperature"),
                        verbatimTextOutput("todaysMinTemperature"),
                        verbatimTextOutput("todaysMaxTemperature"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentHumidity"),
                        verbatimTextOutput("todaysMinHumidity"),
                        verbatimTextOutput("todaysMaxHumidity"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentWindspeed"),
                        verbatimTextOutput("todaysMinWindspeed"),
                        verbatimTextOutput("todaysMaxWindspeed"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentWindDirection")
                        # verbatimTextOutput("todaysMinHumidity"),
                        # verbatimTextOutput("todaysMaxHumidity")
                      ))), side = "left"), 
                      
          f7Float(  
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              
              tags$div( style=paste0("width: ", defWidth),           
            f7Card(
              title = "Weather History",
              prettyRadioButtons(
                
                inputId = "WeatherHistoryButtons",
                label = "Variable:",
                
                c("Rainfall" = "Rainfall",
                  "Temperature" = "Temperature",
                  "Humidity" = "Humidity",
                  "Windspeed" = "Wind-Speed"),
                inline = TRUE,
                status = "success",
                animation = "pulse",
                bigger = T
              ),
              dygraphOutput("WeatherHistoryChart", height = "300px")
            )))), side = "left"),
            
            
##################################  UI - SOIL DATA MAP   ##################################             
        
        f7Tab(
          tabName = "Preferences",
          icon = f7Icon("gear", old = F),
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
    RV$AllowGeoLocation=NULL
    RV$currentLoc=NULL
    
    
    
    ##################################  SERVER - GLOBAL PROCESSING   ##################################
    
    acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=8, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
    
    observeEvent( RV$currentLoc, {
      updateF7Text("wgtLonVal", value = formatC(RV$currentLoc$lng, digits = 5, format = "f") )
      updateF7Text("wgtLatVal", value = formatC(RV$currentLoc$lat, digits = 5, format = "f"))
    })
    observe({
      RV$AllowGeoLocation= input$geolocation
    })

    
    output$wgtlocationMap <- renderLeaflet({
      leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%

        setView(lng = input$long, lat = input$lat, zoom = 18) %>%
        addControlGPS() %>%
        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
          overlayGroups = c( "SW Probes"),
          options = layersControlOptions(collapsed = T)
        )
    })
    
    observeEvent(input$wgtlocationMap_click, {
      click = input$locationMap_click
      leafletProxy('locationMap')%>%clearMarkers%>%addMarkers(lng = click$lng, lat = click$lat)
      RV$currentLoc$lng = click$lng 
      RV$currentLoc$lat = click$lat
      print(RV$currentLoc$lng)
      
    })
    
    
    observeEvent(input$wgtSubmitSample, {
      
      req(input$wgtSubmitSample)
      tmpFile <- tempfile(pattern = 'upSpec_', tmpdir = localUploadDir, fileext = '.dat')
      
      drop_download(path='spectrafiles/ross/archive_20392.asd', local_path = tmpFile, dtoken = token  )
      
      response <-  POST(paste0(spectraAPIServer, '/SoilSpectra/Upload'), body = list(fileinfo = upload_file(tmpFile), 
                                                                         attribute='TEST',  
                                                                         longitude='151.2345', 
                                                                         latitude='-25.7777', 
                                                                         upperDepth='0.0', 
                                                                         lowerDepth='.25',
                                                                         userName='BOBsBrother',
                                                                         specType='ASD',
                                                                         format='json'
                                                                         ))

         stream <- content(response, as="text", encoding	='UTF-8')
        print(stream)
        
        
      
    })
    
    
  }
)









