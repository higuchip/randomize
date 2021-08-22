library(shiny)
library(shinydashboard)
library(sf)
library(leaflet)

# Define UI 
ui <- fluidPage(

    # Application title
    HTML("<h1>Randomize! 🎲</h1>"),
    HTML("<h4>Aleatorize unidades amostrais dentro da área de um polígono.</h4>"),
    HTML("<p>Desenvolvido por <a href='https://www.linkedin.com/in/pedro-higuchi-4085a81b/'>Pedro Higuchi</a>.</p>"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = "poligono",
                      label = "Adicionar o polígono (.kml)",
                      multiple = TRUE,
                      accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.kml')),
            sliderInput("bins",
                        "Número de unidades Amostrais:",
                        min = 1,
                        max = 100,
                        value = 10),
            downloadButton("downloadData", "Download")
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(leafletOutput("mymap")
        )
    )
)

# Define server logic 
server <- function(input, output) {
    datasetInput = function() {
        inFile <- input$poligono
        
        if (is.null(inFile))
            return(NULL)
        
        poligono<-st_read(inFile$datapath)
        ua <- st_sample(st_geometry(poligono), 
                        input$bins)
        ua
        
    }
    


    output$mymap <- renderLeaflet({
        inFile <- input$poligono
        
        if (is.null(inFile))
            return(NULL)
        
        poligono<-st_read(inFile$datapath)
        ua<<-datasetInput()
        inters_pt <- ua %>%
            st_cast("MULTIPOINT") %>%
            st_cast("POINT")
        
        pontos<-st_coordinates(inters_pt) 
        pontos<-as.data.frame(pontos)
     
        
        leaflet() %>%
            addTiles(group = "Open Street")%>% 
            addPolylines(data = st_zm(poligono), group = "Pipeline",  color = "blue", opacity = 1)%>%
            addCircleMarkers(lng = pontos$X, lat=pontos$Y,
                             stroke = FALSE, fillOpacity = 0.5) %>% 
            addLayersControl(
                baseGroups = c("Open Street", "World Imagery")
            )
        
        
    })
    # Arquivo para download
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("coords_parcelas",".kml", sep = "")
        },
        content = function(file) {
            #ua<-datasetInput()
            st_write(ua, file, layer_options = "GEOMETRY=AS_XY")
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
