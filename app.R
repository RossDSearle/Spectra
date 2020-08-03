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



defWidth = '380px'
loaderTime = 1


machineName <- as.character(Sys.info()['nodename'])
if(machineName=='soils-discovery'){
  rootDir <- '/srv/shiny-server/Spectra'
  
  
}else{
  rootDir <- 'C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/Spectra'
  
}

spectraServer <- 'http://esoil.io/APIDev'



shiny::shinyApp(
  ui = f7Page(
    title = "miSensors",
    init = f7Init(skin = "auto", theme = "light", filled = T, color = 'lightblue'),
    tags$head(tags$link( rel="icon", type="image/png", href="wheat.png", sizes="32x32" ),
              tags$link( rel="apple-touch-icon", href="apple-touch-icon.png" )
              #tags$title("BCG AgDataShop"),
              #tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
              
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
        title = tags$div( tags$div(style="vertical-align:middle!important; text-align:left!important; display:inline-block;", "miSensors"), HTML('&nbsp&nbsp&nbsp'), tags$div(style="float: right;", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px", align='right'))),
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
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = "Location",
                        id = 'crdMap',
                        leafletOutput("locationMap", height = 400 ),
                        
                        fluidRow(column(width = 2,
                                        verbatimTextOutput("lat"),
                                        verbatimTextOutput("long"),
                                        verbatimTextOutput("geolocation")))
                        
                      )
            )
          ), side = "left" ),
          
          
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
    
    
    
    ##################################  SERVER - GLOBAL PROCESSING   ##################################
    
    acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=8, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
    
    output$lat <- renderPrint({
      input$lat
    })
    
    output$long <- renderPrint({
      input$long
    })
    
    output$geolocation <- renderPrint({
      input$geolocation
    })
    
    output$locationMap <- renderLeaflet({
      leaflet() %>%
        clearMarkers() %>%
        addTiles(group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
        setView(lng = input$long, lat = input$lat, zoom = 10) %>%
        
        # addControlGPS() %>%
        
        addLayersControl(
          baseGroups = c("Satelite Image", "Map"),
          overlayGroups = c( "SW Probes"),
          options = layersControlOptions(collapsed = T)
        )
    })
    
    
  }
)









