library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(lubridate)
library(fontawesome)
library(leaflet)
library(dashboardthemes)
library(ggplot2)


load("ufo_my_project1.RData")


ui <- dashboardPage( 
  
  dashboardHeader(title = "UFO Sightings - USA",
                  tags$li(class = "dropdown",
                          tags$a(href="https://www.linkedin.com/in/viktoriia-poliakova/", 
                                 target="_blank", 
                                 icon("linkedin", lib = "font-awesome") 
                          )
                  )
                  
  ), 
  
  dashboardSidebar(                                            
    sliderInput("years", h4("Years"),                        
                min = 1910, max = 2014,                      
                value = c(1910, 2014), step = 5, sep = ""),                                           
    selectInput("state", h4("State"),
                choices = c("All states", as.character(unique(ufo$state)) %>% sort()),
                selected = "All states", multiple = TRUE)
  ),
  
  
  dashboardBody(
    tags$style(".small-box.bg-olive { background-color: #85bda2 !important; color: #4c4c67 !important; }"),
    shinyDashboardThemes(
      theme = "grey_dark"),
    fluidPage(
      tabBox(width = 50,
             tabPanel('Welcome', valueBoxOutput('box1'), valueBoxOutput('box2'), valueBoxOutput('box3'),
                      fluidRow(align='center', h4('Welcome to this Shiny app exploring the occurences of Unidentified Flying Objects in the USA.
                                               The data comes from the National UFO Reporting Center (a US-based organization that investigates UFO sightings and reports of contact with UFO).')),
                      
                      fluidRow(align='center', h4('UFOs have always been subject of an immense interest to humans. Are we alone in this Universe? Are there other living creatures observing us from above and plotting a plan to take over us? These and many other questions are
                                           explored in literature, media, pop culture, creating countless theories, beliefs and disbeliefs.
                                           With this app, I wanted to take a glimpse into the reports of people who claim to have seen UFOs, in order to understand what these people have experienced and what exactly might be out there in the Universe.')),
                      
                      fluidRow(align='center', h4('The app was created purely for entertainment purposes and does not represent the author`s beliefs about supernatural activities :) ')),
                      
                      fluidRow(align='center', h4(tags$a(href="https://www.kaggle.com/NUFORC/ufo-sightings", "Data Source"))),
                      fluidRow(align='center', h4('')),
                      fluidRow(align='center', h4('')),
                      fluidRow(
                        align = "center",
                        img(src = 'https://cdn.pixabay.com/photo/2020/04/02/16/22/ufo-4995753_1280.png', 
                            align = "center", height="60%", width="60%")
                      ),
                      fluidRow(align='center', h4('')),
                      fluidRow(align='center', h6('Created by Viktoriia Poliakova')),
                      fluidRow(align='center', h6('MSc in Digital Marketing & Data Science @ emlyon business school'))
             ),
             
             
             tabPanel('States Analysis',
                      fluidRow(align='center', h4('Let`s have a look at UFO occurences by location. Seems like California is the all-time favorite place for UFOs to visit - perhaps they enjoy sunny weather?')),
                      fluidRow(align='center', h4('')),
                      fluidRow(align='center', h4('')),
                      plotOutput('plot1', height = "500px"),
                      fluidRow(align='center', h4('Play around with the State and Year filters on the left, and see which cities UFOs favorited the most.')),
                      fluidRow(align='center', h4('')),
                      fluidRow(align='center', h4('')),
                      plotOutput('plot2', height = "510px")),
             
             tabPanel('Shapes Exploration',
                      fluidRow(align='center', h4('Although ambiguous, "Light" is the most common UFO shape people report seeing. "Triangle" and "Circle" are runner-ups!')),
                      fluidRow(align='center', h4('')),
                      fluidRow(align='center', h4('')),
                      plotOutput('plot3')),
             tabPanel('Map of Occurences',
                      fluidRow(align='center', h4('Interactive map of UFO occurences across the country. Zoom in and explore all the different locations where UFOs were spotted!')),
                      fluidRow(align='center', h4('')),
                      fluidRow(align='center', h4('')),
                      leafletOutput('plot4')),
             tabPanel('Duration Analysis',
                      fluidRow(align='center', h4('Play around with the filters to see how the mean duration of UFO sightings change across years and states.')),
                      fluidRow(align='center', h4('')),
                      fluidRow(align='center', h4('')),
                      fluidRow(width=12, valueBoxOutput('box4')),
                      plotOutput('plot5')),
             tabPanel('Data',
                      fluidRow(DT::dataTableOutput("table_1"))
             )
      )
    )
  )
)


server <- function(input, output){ 
  data <- reactive({  
    if(input$state == "All states"){
      ufo %>%                  
        filter(year(date) >= input$years[1],
               year(date) <= input$years[2])
    } else {
      ufo %>%                  
        filter(year(date) >= input$years[1],
               year(date) <= input$years[2],
               state %in% input$state)
    }
  })
  
  
  
  
  output$table_1  <- DT::renderDataTable({data()}, 
                                         options = list(   
                                           lengthMenu = list(c(10, 12, 15), c('10 lines', '12 rows', '15 items')),   
                                           pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  
  output$box1 <- renderValueBox({
    ufo <- data()
    valueBox(
      value = nrow(ufo),
      subtitle =  "Total UFO Sightings", 
      icon = icon("satellite", lib = "font-awesome"),
      color = "olive"
    )
  })
  
  output$box2 <- renderValueBox({
    ufo <- data()
    valueBox(
      value = n_distinct(ufo$ufo_shape),
      subtitle =  "Different UFO Shapes Observed", 
      icon = icon("shapes", lib = "font-awesome"),
      color = "olive"
    )
  })
  
  output$box3 <- renderValueBox({
    ufo <- data()
    valueBox(
      value = n_distinct(ufo$city),
      subtitle =  "US Cities Where UFOs Were Seen", 
      icon = icon("city", lib = "font-awesome"),
      color = "olive"
    )
  })
  
  output$plot1 <- renderPlot(
    data() %>%
      group_by(state) %>%
      na.omit()%>%
      summarize(nb_occurences=n()) %>%
      ggplot(aes(nb_occurences, reorder(state, nb_occurences), fill=nb_occurences)) + geom_col(stat ='identity') + scale_fill_gradient(low = "#85bda2", high = "#4c4c67")
    
  )
  
  
  output$plot2 <- renderPlot(
    data() %>%
      group_by(city) %>%
      na.omit()%>%
      summarize(nb_occurences=n()) %>%
      top_n(20, nb_occurences) %>%
      ggplot(aes(nb_occurences, reorder(city, nb_occurences), fill=nb_occurences)) + geom_col(stat ='identity') + scale_fill_gradient(low = "#85bda2", high = "#4c4c67")
    
  )
  
  
  output$plot3 <- renderPlot(
    data() %>%
      group_by(ufo_shape) %>%
      na.omit()%>%
      summarize(nb_occurences=n()) %>%
      ggplot(aes(x=ufo_shape, y=nb_occurences)) + 
      geom_point(size=3, color = "black", fill=alpha('orange', 0.3), alpha=0.7, shape=21, stroke=2) + 
      geom_segment(aes(x=ufo_shape, 
                       xend=ufo_shape, 
                       y=0, 
                       yend=nb_occurences), color="darkgreen") + 
      labs(title="Most Frequent UFO Shapes",
           caption="") + 
      theme(axis.text.x = element_text(angle=30, vjust=0.6)))
  
  pal <- colorFactor(palette = "Spectral", domain = ufo$duration_minutes)
  
  
  output$plot4 <- renderLeaflet(
    leaflet(ufo) %>%
      addTiles() %>%
      addProviderTiles(providers$Stamen.Toner,options = providerTileOptions(opacity = 0.85)) %>%
      addCircleMarkers(~longitude, ~latitude,
                       popup = ~state, label = ~city,
                       clusterOptions = markerClusterOptions()))
  
  
  
  
  output$box4 <- renderValueBox({
    ufo <- data()
    valueBox(
      value = round(mean(ufo$duration_minutes),5),
      subtitle =  "Mean UFO Occurence Duration (min)", 
      icon = icon("city", lib = "font-awesome"),
      color = "olive")})
  
  
  output$plot5 <- renderPlot(
    data() %>%
      ggplot(aes(x = duration_minutes, y = state, fill = duration_minutes)) + 
      stat_summary(fun = "mean", geom = "bar", color='#6e9598') 
  )
  
  
}


# Run the app ----
shinyApp(ui = ui, server = server)