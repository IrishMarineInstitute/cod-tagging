# load necessary packages
library(shiny)
library(leaflet)
library(mapview)
library(dplyr)
library(plotly)
library(htmlwidgets)
#library(webshot2)
library(shinyscreenshot)

#library(MASS)
#webshot::install_phantomjs() # needed for map download function

recaptures <- read.csv("Data/Recaptures.csv")
recaps <- filter(recaptures, Distance_nm < 250)
recaps$Recap_Lat_jit <- jitter(recaps$Recap_Lat, factor=0.001)
recaps$Recap_Long_jit <- jitter(recaps$Recap_Long, factor=0.001)
recaps <- mutate(recaps, Tag_sea=ifelse(recaps$Release_lat >54.25, "North Channel",
                                        ifelse(recaps$Release_lat<52.0, "Celtic Sea", "Irish Sea")))
recaps <- mutate(recaps, distance_cat=as.factor(ifelse(recaps$Distance_nm <50, "Under 50nm",
                                                       ifelse(recaps$Distance_nm>50 & recaps$Distance_nm<80, "50-80nm",
                                                              ifelse(recaps$Distance_nm>80 & Distance_nm<140, "80-140nm", "Migrators >140nm")))))
recaps$distance_cat <- ordered(recaps$distance_cat, levels = c("Under 50nm", "50-80nm","80-140nm", "Migrators >140nm"))
tagging_events <- read.csv("Data/tagging_events.csv")
LF <- read.csv("Data/Combined data_LF.csv")

ui <- navbarPage("Tagging", 
                 id="Main",
                 tabPanel(title="Cod",
                          tags$head(
                            includeCSS("styles2.css"), includeScript("google-analytics.js")),# Include the customised CSS
                          div(class="outer",
                              tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; padding: 0}"), 
                              leafletOutput(outputId = "map", width = "100%", height = "100%"),
                              absolutePanel(id = "controls", class = "panel panel-default", 
                                            style = 'overflow-y:scroll;max-height:575px;',
                                            fixed = FALSE, draggable = FALSE, 
                                            top = 15, left = "auto", right = 10, bottom = "auto",
                                            width = 400,
                                            h3("Data explorer"),
                                            selectInput(inputId="layer_choice", label= "Select map layer", 
                                                        choices=c("Recaptures","Tagging Events","Gear of tagging vessel"),
                                                        selected="Recaptures"),
                              #absolutePanel(id = "controls", class = "panel panel-default", 
                                            #style = 'overflow-y:scroll;max-height:550px;', #creates scroll bar on y axis for panel where height of content exceets specified height
                                            #fixed = FALSE, draggable = FALSE, 
                                            #top = 190, left = "auto", right = 10, bottom = "auto",
                                           # width = 600,
                                           # br(),
                                            conditionalPanel(condition="input.layer_choice == 'Recaptures'",
                                                             selectInput(inputId="x_variable", label= "Select X variable for plot", 
                                                                         choices=c("Distance travelled","Days at Liberty","Growth (cm) at liberty", "Length (cm) at Release"),
                                                                         selected="Days at Liberty"),
                                                             selectInput(inputId="y_variable", label= "Select Y variable for plot", 
                                                                         choices=c("Distance travelled","Days at Liberty","Growth (cm) at liberty", "Length (cm) at Release"),
                                                                         selected="Distance travelled"),
                                                             selectInput(inputId="color_variable", label= "Select parameter to color points by", 
                                                                         choices=c("None", "Distance travelled","Days at Liberty","Growth",# "Gear",
                                                                                   "Sex", "Maturity", "Migratory Category", "Origin"),
                                                                         selected="None"),
                                                             actionButton("modal1", "Show plot")),
                                            conditionalPanel(condition="input.layer_choice == 'Gear of tagging vessel'",
                                                             htmlOutput("anglertextp1"),
                                                             imageOutput("img1", width = "auto", height = 300),
                                                             htmlOutput("img1txt"),
                                                             htmlOutput("anglertextp2"),
                                                             leafletOutput("angler_map_inset", width = "auto", height = 300),
                                                             br(),
                                                             htmlOutput("maptext1"),
                                                             htmlOutput("anglertextp3")),
                                            conditionalPanel(condition="input.layer_choice == 'Tagging Events'",
                                                             br(),
                                                             htmlOutput("LF_title"),
                                                             plotlyOutput("LF_hist")),
                              br(),
                              screenshotButton( 
                                selector = "body", 
                                filename = paste0("CodTagging_",Sys.Date()), 
                                id = "", 
                                scale = 1, 
                                timer = 0, 
                                download = TRUE),
                                            img(src="Logos/split_logos.png", width =350, style="display: block;margin-top:2em")
                              )
                          )
                 ), tabPanel(title="About",
                             htmlOutput("abouttext")) 
) # end of ui

server <- function(input, output, session) {
  # Create foundational leaflet map
  # and store it as a reactive expression
  foundational.map <- reactive({

    pal1 <- colorFactor("RdYlGn", domain=recaps$distance_cat, reverse = TRUE)
    col <-  colorNumeric("viridis",tagging_events$numbercodtagged, n=5)
    pal <-  colorFactor("Dark2",tagging_events$gear)
    
    if(input$layer_choice == "Recaptures"){
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        setView(lng = -3.5, lat = 53.8, zoom = 6)  %>%
        addCircleMarkers(lng=recaps$Recap_Long_jit, lat=recaps$Recap_Lat_jit, fillOpacity = 0.8, 
                         color = pal1(recaps$distance_cat), stroke = FALSE, 
                         popup=paste("<b>Long:</b> ",round(recaps$Recap_Long,4), 
                                     "<br />", "<b>Lat:</b> ",round(recaps$Recap_Lat,4), 
                                     "<br />", "<b>Release Date:</b>", recaps$Release_Date,
                                     "<br />", "<b>Recapture Date:</b>", recaps$Recap_Date,
                                     "<br />", "<b>Length at tagging:</b>", recaps$Release_Length_cm,
                                     "<br />", "<b>Length at recapture:</b>", recaps$Recap_Length_cm,
                                     "<br />", "<b>Growth:</b>", recaps$Growth,
                                     "<br />", "<b>Origin:</b>", recaps$Tag_sea)) %>%
        addLegend("bottomleft", pal=pal1, values = recaps$distance_cat,
                  title = "Migratory Category (from distance travelled)")
    
    }else if(input$layer_choice == "Tagging Events"){
        leaflet() %>%
          addProviderTiles(providers$Esri.OceanBasemap) %>%
          setView(lng = -3.5, lat = 53.8, zoom = 6)  %>%
          addCircleMarkers(lng=tagging_events$longitude, lat=tagging_events$latitude, 
                           radius = tagging_events$numbercodtagged/3, 
                           color = col(tagging_events$numbercodtagged),
                           stroke = FALSE, fillOpacity = 0.9,
                           popup=paste("<b>Long:</b> ",round(tagging_events$longitude,4),
                                       "<br />", "<b>Lat</b>: ",round(tagging_events$latitude,4), 
                                       "<br />", "<b>Year</b>", tagging_events$year,
                                       "<br />", "<b>No. fish tagged</b>", tagging_events$numbercodtagged,
                                       "<br />", "<b>Survey</b>", tagging_events$survey_ID)) %>%     
          addLegend("bottomleft", pal=col, values = tagging_events$numbercodtagged,
                    title = "Number of Cod tagged")
        
    }else if(input$layer_choice == "Gear of tagging vessel"){
      leaflet() %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        setView(lng = -3.5, lat = 53.8, zoom = 6)  %>%
        addCircles(lng=tagging_events$longitude, lat=tagging_events$latitude, 
                   radius=tagging_events$numbercodtagged, 
                   color = pal(tagging_events$gear), #fill = TRUE, #makes underlying layers clickable
                   group ='Gear Type',
                   popup=paste("<b>Long:</b> ",round(tagging_events$longitude,4), "<br />", 
                               "<b>Lat</b>: ",round(tagging_events$latitude,4), 
                               "<br />", "<b>Year</b>", tagging_events$year,
                               "<br />", "<b>Survey</b>", tagging_events$survey_ID)) %>%
        addLegend("bottomleft", pal=pal, values = tagging_events$gear,
                  title = "Gear types")
    }
  }) # end of foundational.map()

  angler_caught <- filter(tagging_events, gear == "Rod")
  output$angler_map_inset <-  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      setView(lng = -6.968, lat = 52.24, zoom = 10)  %>%
      addCircles(lng=angler_caught$longitude, lat=angler_caught$latitude, radius=50, #group="Angler caught & tagged",
                 color = "green" ,
                 popup=paste("<b>Long:</b> ",round(angler_caught$longitude,4), "<br />", "<b>Lat</b>: ",round(angler_caught$latitude,4), 
                             "<br />", "<b>Year</b>", angler_caught$year,
                             "<br />", "<b>Survey</b>", angler_caught$survey_ID))
  })

  # render foundational leaflet map
  output$map <- leaflet::renderLeaflet({
    
    # call reactive map
    foundational.map()
    
  }) # end of render leaflet
  
  # store the current user-created version
  # of the Leaflet map for download in 
  # a reactive expression
  user.created.map <- reactive({
    
    # call the foundational Leaflet map
    foundational.map() %>%
      
      # store the view based on UI
      setView( lng = input$map_center$lng
               ,  lat = input$map_center$lat
               , zoom = input$map_zoom
      )
    
  }) # end of creating user.created.map()
  
  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a PDF
  # output$dl <- downloadHandler(
  #   filename = paste0( Sys.Date()
  #                      , "_customLeafletmap"
  #                      , ".pdf"
  #   )
  #   
  #   , content = function(file) {
  #     mapshot2( x = user.created.map()
  #               , file = file
  #               , cliprect = "viewport"
  #               #the clipping rectangle matches the height & width from the viewing port
  #               , selfcontained = FALSE
  #               #when this was not specified, the function for produced a PDF of two pages:
  #               #one of the leaflet map, the other a blank page.
  #     )
  #   } # end of content() function
  # ) # end of downloadHandler() function
  

  ###############################################
  ############## Gear of tagging vessel text ####
  ###############################################
  AnglerText <- read.csv('Data/tagging_program.csv', header=TRUE)
  output$anglertextp1 <- renderText(
    paste0(AnglerText[1, which(colnames(AnglerText)=="para1")])
  )
  output$anglertextp2 <- renderText(
    paste0(AnglerText[1, which(colnames(AnglerText)=="para2")])
  ) 
  output$anglertextp3 <- renderText(
    paste0(AnglerText[1, which(colnames(AnglerText)=="para3")])
  ) 
  output$img1 <- renderImage({
    img_file <-"www/tagging_kit.jpg"
    return(list(src = img_file, filetype = "image/jpg", width = 300))
}, deleteFile = FALSE)
  output$img1txt <- renderText(
    paste0(AnglerText[1, which(colnames(AnglerText)=="img1")])
  )
  output$maptext1 <- renderText(
    paste0(AnglerText[1, which(colnames(AnglerText)=="map1")])
  )
  
  output$abouttext <- renderText(
    paste0(AnglerText[1, which(colnames(AnglerText)=="about")])
  )
  
  ###############################################
  ############## Recapture plots ################
  ###############################################
  dataX <- reactive({
    switch(input$x_variable,
           "Distance travelled" = recaps$Distance_nm,
           "Days at Liberty" = recaps$Days_at_liberty,
           "Growth (cm) at liberty" = recaps$Growth,
           "Length (cm) at Release" = recaps$Release_Length_cm)
  })
  
  dataY <- reactive({
    switch(input$y_variable,
           "Distance travelled" = recaps$Distance_nm,
           "Days at Liberty" = recaps$Days_at_liberty,
           "Growth (cm) at liberty" = recaps$Growth,
           "Length (cm) at Release" = recaps$Release_Length_cm)
  })
  
  dataColor <- reactive({
    switch(input$color_variable,
           "None" = "blue",#rep("1", len=dim(recaps)[1]),
           "Distance travelled" = recaps$Distance_nm,
           "Days at Liberty" = recaps$Days_at_liberty,
           "Growth" = recaps$Growth,
           #"Gear" = recaps$Gear,
           "Sex" = recaps$Sex,
           "Maturity" = recaps$Maturity,
           "Migratory Category" = recaps$distance_cat,
           "Origin" = recaps$Tag_sea)
  })
  
  output$days_dist = renderPlotly({
    plot_ly(x = dataX(), y = dataY(), color = dataColor(),
            text=~paste(input$x_variable,":",round(dataX(),0),"<br>",input$y_variable,":",round(dataY(),0)),
            hoverinfo = 'text', type = 'scatter', mode = 'markers') %>%
      layout(xaxis = list(title = input$x_variable),
             yaxis = list(title = input$y_variable))
  })
  
  ###############################################
  ############## other plots ####################
  ###############################################
  output$LF_title <- renderText(paste0("<p><i><b>Figure 1: Length frequency distribution of tagged Cod</b></i></p>")) #<U></U>
  output$LF_hist = renderPlotly({
    plot_ly(x = LF$Length, y = LF$Frequency, type = 'bar') %>%
      layout(xaxis = list(title = "Length"),
             yaxis = list(title = "Frequency"))
  })
  
  ###############################################
  ############## Modal pop ups ##################
  ###############################################  
  
  observeEvent(input$modal1,{
    showModal(modalDialog(
      plotlyOutput("days_dist"),
      #title= "Introduction to the Irish Groundfish Survey",
      easyClose = TRUE,
      footer = NULL))
  })
  
} # end of server

# run the Shiny app
shinyApp(ui = ui, server = server)

# end of script #