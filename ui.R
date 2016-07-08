# title: "SWFWMD Hydrologic App"
# author: "Nathan Johnson"
# date: "2016-02-09"
# ui.R

library(xtable)
library(reshape2)
library(htmltools)
library(maptools)
library(maps)
library(mapproj)
library(ROracle)
library(shiny)
library(dygraphs)
library(xts)
library(leaflet)
library(rmarkdown)
# library(shinyjs)

# source("L:/Hydro Eval/Staff/Nathan/R/scripts/functions/packageLoad.R")
# packageLoad(c("shiny", "dygraphs", "xts", "leaflet")) # usdm for vif
# initialStationSelect <- data.frame("25282")
# names(initialStationSelect) = c("25282 - LAKE STARR")

options(shiny.reactlog=TRUE) 
options(shiny.error=browser)

shinyUI(fluidPage(
  
  div(tags$header(
    tags$img(src="colorSealTransparent.png", height=70, width=70, style ="display:inline-block"),
    tags$h2("SWFWMD Hydrologic App", style ="display:inline-block"),
    tags$a(href = "https://www.youtube.com/watch?v=UVuB4zHpH6E")#, "Tutorial")
  )),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      # uiOutput("hydrologicType"),
      # checkboxGroupInput("hydrologicType",
      #                    label = "Hydrologic Type",
      #                    choices = c("Well", "Lake", "Wetland", "River/Stream","Estuary", "Canal", "Retention Pond", "Borrow Pit", "Sinkhole", "Reservoir","Spring not in Vent", "Lake Outflow"),
      #                    selected = c("Well","River/Stream","Estuary", "Canal")
      #                    ), # did not use this because the dbquery() function converts to all caps. Oracle stores these in sentence case. Not sure how to get around this other than to change
      
      radioButtons('datum', 'Hydrologic Variable', c('Level (NGVD29)', 'Level (NAVD88)', 'Flow'),
                   inline = TRUE),
      uiOutput("stationList"), # use the uiOutput function because tableDT() cannot be passed to the UI
      uiOutput("inCheckboxGroup"), # use the uiOutput function because tableDT() cannot be passed to the UI
      # Set the label, choices, and selected item but don't need this
      # uiOutput("checkedStations"),
      
      # checkboxGroupInput("inCheckboxGroup",
      #                    label = "Displayed Stations",
      #                    choices = "", # choices = initialStationSelect, #"25282 - LAKE STARR",
      #                    selected = ""),# selected = "25282"), #as.numeric(initialStationSelect)), 
      
      
      # div(style="display:inline-block",submitButton("Submit")),
      # actionButton("submitButton","Display Data", style = "display:inline-block; color: #fff; background-color: #337ab7; border-color: #2e6da4"), # creates button in HTML
      br(),
      br(),
      br(),
      br(),
      h3("Downloads"),
      
      dateRangeInput("dates", 
                     label = NULL,
                     start = "1920-01-01", 
                     end = as.character(Sys.Date())),
      # div(style="display:inline-block",submitButton("Submit Report Settings")),
      div(style="display:inline-block",downloadButton('downloadData', 'Data')),
      # radioButtons('format', NULL, c('PDF','Word'), #, 'HTML', ),
                   # inline = TRUE),
      div(style="display:inline-block; float: right; margin: 3px 0 10px 10px", radioButtons('format', NULL, c('PDF','Word'), inline = TRUE)), #, 'HTML', ), # creates button in HTML
      div(style="display:inline-block; float: right", downloadButton('downloadReport', 'Report')),
      br()
    ),
    
    mainPanel(
      tabsetPanel(id = "tabsetId",
        #         tabPanel('Hydrograph', plotOutput("plot")), #,dataTableOutput("dt")
        tabPanel('Map', leafletOutput('mymap', height = "700px"), dataTableOutput("site")),
        tabPanel('Interactive Graph', br(), dygraphOutput("dygraph"),
                 dataTableOutput("dt")),
        tabPanel('Search Stations', dataTableOutput('fulldt'))
        # tabPanel('Tutorial', HTML('<iframe width="700" height="500" src="https://www.youtube.com/embed/UVuB4zHpH6E" frameborder="0" allowfullscreen></iframe>'))
        # tabPanel('ScatterPlot', plotOutput("scatter", height = "400px"), dataTableOutput("corMatrix"))
      )
    )
  )
)
)
