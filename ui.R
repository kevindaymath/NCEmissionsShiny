options(warn=-1)
library(shiny)
library(leaflet)
library(DT)
load("data\\data.RData")


# Define UI for dataset viewer application
fluidPage(
  
  # Application title
  titlePanel("EPA Emissions Data (in tons)"),
  
  
  # sidebarLayout(
  #   sidebarPanel(
  #     helpText("Select a county below"),
  #     selectInput("county",label="Choose a county",
  #                 choices=NCCountyList$County.Name, selected="Alamance"),
  #     width = 3,
  #     br()
  #   ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", selectInput("variable",label="Choose a varsiable",
                                                      choices=var, selected=var[1]),
                 htmlOutput("County"),leafletOutput("CountyMap"),
                 htmlOutput("Region"),leafletOutput("RegionMap")),
        tabPanel("Table", sidebarPanel(
          
          radioButtons("Level",label="Choose a level",
                       choices=list("County"=1,"Region"=2),selected =1), width = 3),
          checkboxGroupInput("show_vars","Display",unique(categories$Category),selected = "Agriculture",inline=TRUE),
          DT::dataTableOutput("tableCounty")
                 
                 
                 )
        
              
     

    
  )
)

)