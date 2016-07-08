# title: "SWFWMD Hydrologic App"
# author: "Nathan Johnson"
# date: "2016-02-09"
# server.R


# Colleen Endres: 
#   SELECT DISTINCT s.site_id, s.site_primary_type_desc, s.uid_site_name, s.county_name, a.start_dt, a.end_dt, se.latitude, se.longitude 
# FROM site_dim s 
# SELECT DISTINCT s.site_id, s.site_primary_type_desc, s.uid_site_name, s.county_name,  se.latitude, se.longitude 
# FROM site_dim s 
# LEFT JOIN sites_edit se ON  s.site_id = se.site_id ; 
# This will work
# You will still need to join to sites_edit which is a GIS table that has the latitude and long info.
## here are some more changes

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

# source("L:/Hydro Eval/Staff/Nathan/R/scripts/functions/packageLoad.R") #packageLoad function
# packageLoad(c("xtable", "reshape2", "htmltools", "maptools", "maps", "mapproj", "ROracle", "rmarkdown")) # usdm for vif

## SET DRIVER AND DATABASE CONNECTION
drv <- dbDriver("Oracle")
con <- dbConnect(drv, username = "RESDATA_VIEW", password = "prod_resdata_view", 
                 dbname = "QUERY_RPTWP") # develop connection to oracle
# dbListTables(con, schema = "RESDATA")

#### Use different input lists for debugging
# input = list(stationName = paste(21439,"" , sep = ",")) # for

# input = list(stationName = paste(768709, 21439, 25370, 23558, 282059, 296842, 25282, 25291, sep = ","))
####
# selStationNames <- paste(input, collapse = ",")
# selectStationName <- paste(as.list(sort(unlist(strsplit(input$stationName,",")))), collapse = ",") # need to create list that is sorted to match the names right

shinyServer(function(input, output, session) {
  # Only the renderUI function works to update correctly. Tried the updateselectinput function but I couldn't pass the tableDT function to it
  
  output$stationList <- renderUI({
    dt <- as.list(tableDT()$SID)
    names(dt) <- paste(tableDT()$SID, tableDT()$Name, sep = " - ")
    # if(input$datum == "Flow"){
    selectizeInput("stationName", choices = dt,  multiple = TRUE, label = "SID - Station(s)", options = list(placeholder = 'Enter Station - SID (Begin Typing)'))#, selected = "25282")
    # } else {selectizeInput("stationName", choices = dt, multiple = TRUE, label = "SID - Station", options = list(placeholder = 'Type Station - SID'),selected = "25282")}
  })
  
  
  ## CREATE CHECKBOXES TO TURN STATIONS ON AND OFF NEED TO OBSERVE INPUT$STATIONNAME AND UPDATE CHECKBOX LIST.
  # output$checkedStations <- renderUI({
  #   selectStationName <- as.list(sort(unlist(strsplit(input$stationName,",")))) # need to create list that is sorted to match the names right
  #   names(selectStationName) <- paste(inventoryAll()$SITE_ID, inventoryAll()$SITE_NAME, sep = " - ")
  #   checkboxGroupInput("inCheckboxGroup",
  #                      label = "Displayed Stations",
  #                      choices = selectStationName,
  #                      selected = selectStationName
  #   )
  # })
  # output$hydrologicType <- renderUI({ 
  #   hyType <- as.list(unique(tableDT()$Type))
  #   checkboxGroupInput("hydrologicType", choices = hyType, label = "Hydrologic Type", inline = TRUE)
  # })
  
  output$inCheckboxGroup <- renderUI({
    validate(
      need(input$stationName != "", "")
    )
    
    selectStationName <- as.list(sort(unlist(strsplit(input$stationName,",")))) # need to create list that is sorted to match the names right
    names(selectStationName) <- paste(inventoryAll()$SITE_ID, inventoryAll()$SITE_NAME, sep = " - ")
    checkboxGroupInput(inputId = "inCheckboxGroup",
                       label = "Displayed Stations",
                       choices = selectStationName,
                       selected = selectStationName
    )
  })
  
  # datumChangeSelStationNames <- reactive({
  #   datumChangeSelStationNames <- datum()selectionstationName
  # })
  
  ##############  
  # observe({
  #   validate(
  #     need(input$stationName != "", "Please Enter Station - SID")
  #   )
  #   # selectStationName <- as.list(sort(unlist(input$stationName,","))) # need to create list that is sorted to match the names right
  #   selectStationName <- as.list(sort(unlist(strsplit(input$stationName,",")))) # need to create list that is sorted to match the names right
  #   names(selectStationName) <- paste(inventoryAll()$SITE_ID, inventoryAll()$SITE_NAME, sep = " - ")
  #   updateCheckboxGroupInput(session, "inCheckboxGroup",
  #                            label = "Displayed Stations",
  #                            choices = selectStationName,
  #                            selected = selectStationName
  #   )
  # })
  
  ###################  
  
  # 
  # observe({
  #   hydrologicType <- as.list(input$hydrologicType)
  #   updateCheckboxGroupInput(session, "hydrologicType", 
  #                            label = "Hydrologic Type", 
  #                            choices = hydrologicType, 
  #                            selected = hydrologicType)
  # })
  
  ## CONVERT CHECKBOX LIST INTO STRING TO BE READ BY ORACLE
  # selStationNames <- eventReactive(input$updateData, {
  # selStationNames <- eventReactive(input$submitButton, {
  selStationNames <- reactive({
    validate(
      need(input$inCheckboxGroup != "","Please Enter Station - SID")
    )
    selStationNames <- paste(as.list(unlist(strsplit(input$inCheckboxGroup,","))), collapse = ",")
  })
  
  ## BASIC INVENTORY METADATA
  inventory <- reactive({
    # if(is.null(input$stationName))
    #   return(tableDT()[1:1000,])
    # validate(
    #   need(input$stationName != "", "Please Enter Station - SID")
    # )   
    withProgress(message = 'Extracting station data from Oracle', value = 0,{
      inventory = dbGetQuery(con, paste("SELECT site_id, site_name, latitude_nbr,
                                      longitude_nbr, county_name FROM 
                                      ods_permit.site WHERE site_id IN (",
                                        selStationNames(), ")", sep = ""))
      # inventory = dbGetQuery(con, paste("SELECT * FROM ods_permit.site WHERE site_id IN (", selStationNames, ")", sep = "")) # use this for all table variables
    })  
  })
  
  inventoryAll <- reactive({
    # validate(
    #   need(input$stationName != "", "Please Enter Station - SID")
    # )
    withProgress(message = 'Extracting data from Oracle', value = 0,{
      inventoryAll = dbGetQuery(con, paste("SELECT site_id, site_name FROM
                                         ods_permit.site WHERE site_id IN (",
                                           paste(as.list(sort(unlist(strsplit(input$stationName,",")))), collapse = ","), ")", sep = ""))
    })  
  })
  
  
  ## LAT AND LONG CONVERTED TO DECIMAL DEGREES FROM WEIRD FORMAT
  inven <- reactive({
    # if(is.null(input$stationName))
    #   return(tableDT()[1:10,])
    
    # validate(
    #   need(input$stationName != "", "Please select SID - Station(s)")
    # )   
    inven <- within(inventory(), {
      dms = do.call(rbind, strsplit(as.character(inventory()$LATITUDE_NBR), " "))
      Lat = as.numeric(dms[,1])+(as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60
      rm(dms)
    })
    
    inven <- within(inven, {
      dms = do.call(rbind, strsplit(as.character(inven$LONGITUDE_NBR), " "))
      Long = -(as.numeric(dms[,1])+(as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60)
      rm(dms)
    })
  })
  
  ## SELECT A DATUM FROM UI 44 IS NAVD88 AND 2 IS NGVD29
  datum <- reactive({
    if(input$datum == "Level (NAVD88)") {
      datum = 44
    } else if (input$datum == "Level (NGVD29)"){
      datum = 2
    } else {
      datum = 3
    }
  })
  
  ## TIMES SERIES SIMPLY FORMATTED SITEID, DATE, AND LEVEL
  dataForm = reactive({ # eventReactive(input$submitButton, { #
    # data <- dbGetQuery(con, paste("SELECT site_dim_id, date_dim_id, recorded_val 
    # FROM resdata.hydrologic_data_daily_agg WHERE site_dim_id IN (",selStationNames(),") AND resource_parameter_dim_id IN (",datum(),")", sep = "")) 
    # input$submitButton
    # isolate({
    withProgress(message = 'Extracting daily data', value = 0,{
      stackData <- dbGetQuery(con, paste("SELECT site_dim_id, date_dim_id, recorded_val, MEASUREMENT_STATUS_CD, QUALITY_CODE_LIST_TXT FROM resdata.hydrologic_data_daily_agg WHERE site_dim_id IN (",selStationNames(),") AND resource_parameter_dim_id IN (",datum(),")", sep = ""))
      # stackData <- dbGetQuery(con, paste("SELECT site_dim_id, date_dim_id, recorded_val, MEASUREMENT_STATUS_CD, QUALITY_CODE_LIST_TXT FROM resdata.hydrologic_data_daily_agg WHERE site_dim_id IN (",selStationNames,") AND resource_parameter_dim_id IN (",spdf = split(dataForm(), dataForm()$siteID),")", sep = ""))
      # resource_parameter_dim_id = 2 is NGVD29, resource_parameter_dim_id = 44 NAVD88
      # resdata.df_site_param_mv is summary on all data to determine which parameters are avaialable for each site and the number of records site_dim_id
      
      dataForm = data.frame(siteID = stackData$SITE_DIM_ID, 
                            date = as.Date(as.character(stackData$DATE_DIM_ID), format = "%Y%m%d"), 
                            level = stackData$RECORDED_VAL) # Daily stacked values
    })
    # })
  })
  
  
  ## INDEX MAP FOR PRINTING
  output$map <- renderPlot({
    map.ylim = c(min(inven()$Lat)-0.5, max(inven()$Lat)+0.5)
    map.xlim = c(min(inven()$Long)-0.5, max(inven()$Long)+0.5)
    map("county", "florida", col =c("gray90"), 
        xlim = map.xlim, ylim = map.ylim, fill = FALSE)
    map("state", xlim = map.xlim, ylim = map.ylim , 
        col ="black", fill = FALSE, add = TRUE)
    for(i in 1:nrow(inven())){
      points(x = as.vector(inven()$Long[i]), 
             y = as.vector(inven()$Lat[i]), 
             cex = 0.75, pch = i, col = i)
    }
    text(x = inven()$Long, y = inven()$Lat, 
         labels = inven()$SITE_ID, col = "black", pos = 4, cex = 0.75)
    map.axes()
  })
  
  
  # ## TIME SERIES OF SITES FOR PRINT
  # output$plot <- renderPlot({
  #   par(mar = c(2,4,1,0.5))
  #   plot(dataForm()$date, dataForm()$level, 
  #        xlim = c(input$dates[1], input$dates[2]), type = "n", 
  #        ylim = c(min(dataForm()$level, na.rm = TRUE), (max(dataForm()$level, na.rm = TRUE))),  
  #        xlab = "Date", ylab = paste0("Level (",input$datum, "ft)"), pch = 2)
  #   for(i in 1:length(unique(dataForm()$siteID))){
  #     d = dataForm()[dataForm()$siteID == sort(unique(dataForm()$siteID))[i],]
  #     points(d$date, d$level, pch = i, col = i, cex = 0.5)
  #   }
  #   abline(v=pretty(dataForm()$date),col = "lightgray", lty = "dotted", lwd = par("lwd"))
  #   abline(h=pretty(dataForm()$level),col = "lightgray", lty = "dotted", lwd = par("lwd"))
  #   legend("topleft", 
  #          legend = paste(
  #            sort(as.numeric(unlist(strsplit(as.character(selStationNames()), ",")))), 
  #            inventory()$SITE_NAME, sep = ", "), 
  #          col = seq(1:length(unlist(strsplit(as.character(selStationNames()), ",")))), 
  #          pch = seq(1:length(unlist(strsplit(as.character(selStationNames()), ",")))), 
  #          merge = FALSE, bty = "n", cex = 0.75)
  # })
  
  ## DATA TABLE OF BASIC STATISTICS
  # createLink <- function(val) {
  # sprintf('<a href="http://bkvvmwmis03p/ResData/SiteMaintenance/SiteMaintenance.aspx?site=%s&ResView=1&Origin=ResSearch" target="_blank" class="btn btn-primary">WMIS</a>', val)
  #   )
  # }
  
  stationStatSummary <- reactive({
    # dataForm <- subset(dataForm(), dataForm()$date > input$dates[1] | dataForm()$date < input$dates[2])
    spdf <- split(dataForm(), dataForm()$siteID)
    spdf <- lapply(spdf, function(x) x[order(x["date"]),])
    beginDate <- as.matrix(as.data.frame(lapply(spdf, function(x) min(x$date, na.rm = TRUE))))[1,]
    endDate <- as.matrix(as.data.frame(lapply(spdf, function(x) max(x$date, na.rm = TRUE))))[1,]
    min <- sapply(spdf, function(x) min(x$level, na.rm = TRUE))
    max <- sapply(spdf, function(x) max(x$level, na.rm = TRUE))
    obs <- sapply(spdf, function(x) nrow(x))
    maxGap <- sapply(spdf, function(x) max(diff(x$date)))
    p10 <- sapply(spdf, function(x) quantile(x$level, 0.1, na.rm = TRUE))
    p50 <- sapply(spdf, function(x) quantile(x$level, 0.5, na.rm = TRUE))
    p90 <- sapply(spdf, function(x) quantile(x$level, 0.9, na.rm = TRUE))
    summary <- data.frame(name = names(max), beginDate, endDate, obs, min, p10,p50,p90,max, maxGap)
    inventory1 <- inventory()[c("SITE_ID", "SITE_NAME")]
    stationStatSummary <- merge(inventory1, summary, by.x = "SITE_ID", by.y = "name")
    names(stationStatSummary) <- c("SID", "name", "beginDate", "endDate", 
                                   "obs", "min", "p10","p50","p90","max", "maxGap")
    stationStatSummary$SID <- as.integer(stationStatSummary$SID)
    stationStatSummary$Link <- sprintf('<a href="http://www18.swfwmd.state.fl.us/ResData/SiteMaintenance/ExtSiteMaintenance.aspx?site=%s"> WMIS</a>', stationStatSummary$SID)
    # http://bkvvmwmis03p/ResData/SiteMaintenance/SiteMaintenance.aspx?site=25339&ResView=1&Origin=ResSearch 
    # stationStatSummary$Link <- sprintf('<a href="http://bkvvmwmis03p/ResData/SiteMaintenance/SiteMaintenance.aspx?site=%s&ResView=1&Origin=ResSearch"> WMIS</a>', stationStatSummary$SID) ## for internal WMIS
    stationStatSummary
  })
  
  output$dt <- renderDataTable({
    validate(
      # need(input$stationName != "", "Please select SID - Station(s)"),
      # need(selStationNames() != "", "Please review displayed stations"),
      need(strsplit(selStationNames(), ",")[[1]] %in% tableDT()$SID, "Please enter SID - Station")
    )
    return(stationStatSummary())
  }, options = list(paging = FALSE, searching = FALSE, pageLength = 10), escape = FALSE)
  
  ## ALL STATION DATATABLE 
  tableDT <- reactive({
    # validate(
    # need(input$stationName != "", "Please Enter Station - SID")
    # )   
    tableDT <- dbGetQuery(con, paste0("SELECT DISTINCT s.site_id, s.site_primary_type_desc, s.uid_site_name, s.county_name, a.start_dt, a.end_dt, se.latitude, se.longitude FROM site_dim s JOIN df_site_hdf_mv a ON s.site_dim_id = a.site_dim_id LEFT JOIN sites_edit se ON  s.site_id = se.site_id WHERE param_id IN (",datum(),")")) #AND s.site_primary_type_desc IN (", paste(input$hydrologicType, collapse = ","),")")) # receieved the SQL query from Colleen Endres to pull sites that only have water level data
    
    # tableDT <- dbGetQuery(con, paste0("SELECT DISTINCT s.site_id, s.site_primary_type_desc, s.uid_site_name, s.county_name, a.start_dt, a.end_dt, s.uid_type_cd, se.latitude, se.longitude FROM site_dim s JOIN df_site_hdf_mv a ON s.site_dim_id = a.site_dim_id LEFT JOIN sites_edit se ON  s.site_id = se.site_id"))# WHERE param_id IN (",c(44),")"))# AND a.UID_TYPE_CD IN (",c("WEL"),")"))
    
    # tableDT <- dbGetQuery(con, paste0("SELECT DISTINCT s.site_id, s.site_primary_type_desc, s.uid_site_name, s.county_name, a.start_dt, a.end_dt, s.uid_type_cd, se.latitude, se.longitude, a.param_id FROM site_dim s JOIN df_site_hdf_mv a ON s.site_dim_id = a.site_dim_id LEFT JOIN sites_edit se ON  s.site_id = se.site_id"))# WHERE param_id IN (",c(44)
    # testDT <- dbGetQuery(con, paste0("select * from resource_parameter_dim where lower(parameter_desc) like '%flow%'"))
    # testDT <- dbGetQuery(con, paste0("select * from resource_source_dim where lower(resource_source_desc) like '%15%'"))
    # flowDataTest <- dbGetQuery(con, paste0("select * from hydrologic_data_fact where resource_parameter_dim_id in (29,30,31)"))
    # 
    # tableDT = dbGetQuery(con, paste0("SELECT * FROM site_dim")) s JOIN df_site_hdf_mv a ON s.site_dim_id = a.site_dim_id LEFT JOIN sites_edit se ON s.site_id = se.site_id WHERE param_id in (2,44)")) # use this to explore what other fields are in teh df_site_hdf_mv table
    
    names(tableDT) = c("SID", "Type", "Name", "County", "Start", "End", "Lat", "Long")
    
    ## CONVERT FROM DMS TO DD LATITUDE AND LONGITUDE
    tableDT <- within(tableDT, {
      dms = do.call(rbind, strsplit(as.character(tableDT$Lat), " "))
      Lat = as.numeric(dms[,1])+(as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60
      rm(dms)
    })
    tableDT <- within(tableDT, {
      dms = do.call(rbind, strsplit(as.character(tableDT$Long), " "))
      Long = -(as.numeric(dms[,1])+(as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60)
      rm(dms)
    })
    tableDT <- within(tableDT, {
      Start = as.Date(tableDT$Start, format = "%Y-%m-%d")
      End = as.Date(tableDT$End, format = "%Y-%m-%d")
    })
  })
  
  ## USE REACTIVE DATATABLE FOR DT JAVASCRIPT
  output$fulldt <- renderDataTable({
    tableDT()
  }, options = list(pageLength = 10))
  
  # output$tableSID <- renderUI({
  # selectInput(inputId = "stationName", label = "SID(s)", choices = c("25282") , selected = "25282", multiple = FALSE, selectize = TRUE)
  # selectInput(inputId = "stationName", label = "SID(s)", choices = list(tableDT()$SID) , selected = "25282", multiple = FALSE, selectize = TRUE)
  # })
  
  ## DOWNLOAD DATA BUTTON
  output$downloadData = downloadHandler(
    filename = function() {paste("stationData.csv")},
    content = function(file){
      # stackData <- dbGetQuery(con, paste("SELECT * FROM resdata.hydrologic_data_daily_agg WHERE site_dim_id IN ('726932') AND resource_parameter_dim_id IN ('2')", sep = ""))
      
      stackData <- dbGetQuery(con, paste("SELECT site_dim_id, date_dim_id, recorded_val, MEASUREMENT_STATUS_CD, QUALITY_CODE_LIST_TXT FROM resdata.hydrologic_data_daily_agg WHERE site_dim_id IN (",selStationNames(),") AND resource_parameter_dim_id IN (",datum(),")", sep = ""))
      stackData <- data.frame(SITE_DIM_ID = stackData$SITE_DIM_ID,
                              DATE_DIM_ID = as.Date(as.character(stackData$DATE_DIM_ID), format = "%Y%m%d"), 
                              RECORDED_VAL = stackData$RECORDED_VAL,
                              MEASUREMENT_STATUS_CD = stackData$MEASUREMENT_STATUS_CD,
                              QUALITY_CODE_LIST_TXT = stackData$QUALITY_CODE_LIST_TXT) # Daily stacked values
      stackData <- subset(stackData, stackData$DATE_DIM_ID >= input$dates[1] & stackData$DATE_DIM_ID <= input$dates[2])
      stackData <- stackData[order(stackData$SITE_DIM_ID, stackData$DATE_DIM_ID),]
      
      # crossTab <- dcast(dataForm(), date~siteID, value.var = "level", mean)
      # colnames(crossTab) <- c("date", inventory()[inventory()$SITE_ID %in% colnames(crossTab),]$SITE_NAME) # for crossTab data format
      write.csv(stackData, file, row.names = FALSE)
    })
  
  ## LEAFLET MAP THAT CONTAINS THE PROXY LEAFLET MAP. THE SHINY SERVER DOES NOT DO PROXY MAPS WITH OUR CURRENT SETUP
  # output$myMap <- renderLeaflet({
  # leaflet(inven()) %>% 
  # addPopups(~Long, ~Lat, popup = ~paste(as.character(SITE_ID), 
  # as.character(SITE_NAME), 
  # sep = ", ")) %>%
  # addCircles(color = "black", popup = ~paste(as.character(SID), 
  # as.character(Name), sep = ", ")) %>%
  # addProviderTiles("Esri.WorldStreetMap") # ESRI's road basemap
  # })   
  
  #   dataMapBounds <- reactive({
  #     bounds = input$myMap_bounds
  #     latRng <- range(bounds$north, bounds$south)
  #     lngRng <- range(bounds$east, bounds$west)
  #     subset(tableDT(),
  #            Lat >= latRng[1] & Lat <= latRng[2]&
  #              Long >= lngRng[1] & Long <= lngRng[2])
  #   }) # TO DETERMINE THE BOUNDS TO SELECT ALL STATIONS TO BE DISPLAYED IN LEAFLET. D
  
  allData <- reactive({
    subset(tableDT(), 
           Lat >=0)
  }) # for some reason the tableDT does not pupulate all the values, so this is a workaround
  
  # dataMapBounds <- reactive({
  #   # if(is.null(selStationNames())){
  #   #   tableDT()[1:1000,]
  #   # } else {
  #   # validate(
  #   #   need(input$stationName != "", "Please Enter Station - SID")
  #   # ) 
  #   latRng <- range(max(inven()$Lat)+5,min(inven()$Lat)-5)
  #   lngRng <- range(min(inven()$Long)-5,max(inven()$Long)+5 )
  #   subset(tableDT(),
  #          Lat >= latRng[1] & Lat <= latRng[2]&
  #            Long >= lngRng[1] & Long <= lngRng[2])
  #   # }
  #   # tableDT()
  # })  
  
  
  # dataMapBoundsAll <- reactive({
  #   # validate(
  #   #   need(input$stationName != "", "Please Enter Station - SID")
  #   # ) 
  #   latRng <- range(max(tableDT()$Lat)+5,min(tableDT()$Lat)-5)
  #   lngRng <- range(min(tableDT()$Long)-5,max(tableDT()$Long)+5 )
  #   subset(tableDT(),
  #          Lat >= latRng[1] & Lat <= latRng[2]&
  #            Long >= lngRng[1] & Long <= lngRng[2])
  # })
  
  # DECIDED TO DISPLAY ALL STATIONS SINCE THE SERVER CANNOT HOST PROXY FUNCTIONS 
  
  #   
  
  # output$mapAdjacentStations <- renderLeaflet({
  #   leaflet(dataMapBounds()) %>%
  #     addPopups(inven()$Long, inven()$Lat, popup = ~paste(as.character(inven()$SITE_ID), 
  #                                                         as.character(inven()$SITE_NAME), sep = ", ")) %>%
  #     addCircleMarkers(color = "black", popup = ~paste(as.character(SID), 
  #                                                as.character(Name), sep = ", "), clusterOptions = markerClusterOptions()) %>%
  #     addProviderTiles("Esri.WorldTopoMap") # ESRIs topographic basemap
  # })
  
  output$mymap <- renderLeaflet({
    map <- leaflet(allData()) %>%
      # fitBounds(lng1 = ~min(Long) - 0.01, lng2 = ~max(Long) + 0.01,
      # lat1 = ~max(Lat) + 0.01 , lat2 = ~min(Lat) -0.01) %>%
      # if(input$stationName == "20878")
      # fitBounds(lng1 = min(inven()$Long) - 0.01, lng2 = max(inven()$Long) + 0.01,
      # lat1 = max(inven()$Lat) + 0.01 , lat2 = min(inven()$Lat) -0.01) %>%
      # fitBounds(lng1 = min(tableDT()$Long) - 0.01, lng2 = max(tableDT()$Long) + 0.01,
      # lat1 = max(tableDT()$Lat) + 0.01 , lat2 = min(tableDT()$Lat) -0.01) %>%
      # addTiles(group = "OpenStreetMap") %>%
      # addTiles(group = "OSM") %>% 
      # addPopups(lng = mouseOver()$lng, lat = mouseOver()$lat, popup = "1") %>%
      # addPopups(lng = dataLeaflet$lng, lat = dataLeaflet$lat, popup = dataLeaflet$id ) %>%
    addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
      
      addCircleMarkers(data = subset(allData(), allData()$Type %in% c("Spring not in Vent", "Spring at Vent")), color = "white", weight = 2, radius = 15, fillColor = "purple", fillOpacity = 0.2,
                       # popup = ~paste(as.character(SID),as.character(Name), sep = " - "),
                       stroke = TRUE,
                       layerId = ~paste(as.character(SID),as.character(Name), sep = " - "),
                       group = "Spring",
                       options = markerOptions(draggable = TRUE, riseOnHover = TRUE),
                       clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, zoomToBoundsOnClick = TRUE)
      ) %>%
      addCircleMarkers(data = subset(allData(), allData()$Type %in% c("River/Stream", "Canal", "Lake", "Lake Outflow", "Reservoir","Retention Pond","Pond", "Estuary", "Bay/Harbor", "Mine/Mine Discharge","Sinkhole", "Borrow Pit", "Ocean")), color = "white", weight = 2, radius = 15, fillColor = "black", fillOpacity = 0.2,
                       # popup = ~paste(as.character(SID),as.character(Name), sep = " - "),
                       stroke = TRUE,
                       layerId = ~paste(as.character(SID),as.character(Name), sep = " - "),
                       group = "Surface Water",
                       clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, zoomToBoundsOnClick = TRUE)
      ) %>%
      addCircleMarkers(data = subset(allData(), allData()$Type %in% c("Well")), color = "white", weight = 2, radius = 15, fillColor = "red", fillOpacity = 0.2,
                       # popup = ~paste(as.character(SID),as.character(Name), sep = " - "),
                       stroke = TRUE,
                       layerId = ~paste(as.character(SID),as.character(Name), sep = " - "), # to access by clicking on marker in map
                       group = "Groundwater",
                       # options = markerOptions(draggable = TRUE),
                       clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, zoomToBoundsOnClick = TRUE)
      ) %>%  
      addLayersControl(
        baseGroups = c("Topo", "Imagery"),
        if(datum() == 3){overlayGroups = c("Surface Water","Spring")} else {
          overlayGroups = c("Surface Water", "Groundwater","Spring", "Wetland")}, #, "Atmospheric"
        options = layersControlOptions(collapsed = FALSE))
    if(datum() != "Flow (cfs)"){
      map <- map %>% addCircleMarkers(data = subset(allData(), allData()$Type %in% "Wetland"), color = "white", weight = 2, radius = 15, fillColor = "darkgreen", fillOpacity = 0.2,
                                      # popup = ~paste(as.character(SID),as.character(Name), sep = " - "),
                                      stroke = TRUE,
                                      layerId = ~paste(as.character(SID),as.character(Name), sep = " - "),
                                      group = "Wetland",
                                      clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, zoomToBoundsOnClick = TRUE)
      )
    }
    map
  })   
  
  
  #!!! output$mymap <- renderLeaflet({
  # 
  # if(!exists("input$stationName")){
  #   map = leaflet(tableDT()) %>%
  #     fitBounds(lng1 = min(tableDT()$Long, na.rm = TRUE) - 0.01, lng2 = max(tableDT()$Long,na.rm = TRUE) + 0.01,
  #               lat1 = max(tableDT()$Lat, na.rm = TRUE) + 0.01 , lat2 = min(tableDT()$Lat, na.rm = TRUE) -0.01) %>%
  #     # addTiles(group = "OSM") %>%
  #     addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  #     # addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
  #     addCircleMarkers(color = "black", radius = 15,
  #                      popup = ~paste(as.character(SID),as.character(Name), sep = " - "),
  #                      stroke = FALSE,
  #                      layerId = ~as.character(SID),
  #                      # layerId = 1,
  #                      # clusterId= 2,
  #                      # options = markerOptions(draggable = TRUE),
  #                      clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, zoomToBoundsOnClick = TRUE)
  #     )
  #   # addLayersControl(
  #   #   baseGroups = c("OSM","Topo", "Imagery"),
  #   #   # overlayGroups = c("Quakes", "Outline"),
  #   #   options = layersControlOptions(collapsed = FALSE)
  #   # )
  # 
  # } else {
  #!!!    validate(
  # need(input$stationName != "", "Please Enter Station - SID")
  # need(input$inCheckboxGroup != "", "Please review displayed stations")
  # need(selStationNames() != "", "Please 'Submit'")
  # need(strsplit(selStationNames(), ",")[[1]] %in% tableDT()$SID, "Please Enter Station 'SID - Station'")
  #!!!      need(strsplit(selStationNames(), ",")[[1]] %in% tableDT()$SID, "Please enter SID - Station")
  #!!!   )
  #!!!   map <- leaflet(dataMapBounds()) %>%
  # fitBounds(lng1 = ~min(Long) - 0.01, lng2 = ~max(Long) + 0.01,
  # lat1 = ~max(Lat) + 0.01 , lat2 = ~min(Lat) -0.01) %>%
  # if(input$stationName == "20878")
  #!!!     fitBounds(lng1 = min(inven()$Long) - 0.01, lng2 = max(inven()$Long) + 0.01,
  #!!!             lat1 = max(inven()$Lat) + 0.01 , lat2 = min(inven()$Lat) -0.01) %>%
  # fitBounds(lng1 = min(tableDT()$Long) - 0.01, lng2 = max(tableDT()$Long) + 0.01,
  # lat1 = max(tableDT()$Lat) + 0.01 , lat2 = min(tableDT()$Lat) -0.01) %>%
  # addTiles(group = "OpenStreetMap") %>%
  # addTiles(group = "OSM") %>% 
  # addPopups(lng = mouseOver()$lng, lat = mouseOver()$lat, popup = "1") %>%
  # addPopups(lng = dataLeaflet$lng, lat = dataLeaflet$lat, popup = dataLeaflet$id ) %>%
  #!!!    addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  #   addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
  #   addPopups(inven()$Long, inven()$Lat,
  #             popup = ~paste(as.character(inven()$SITE_ID),
  #                            as.character(inven()$SITE_NAME), sep = " - ")) %>%
  #   addCircleMarkers(data = subset(dataMapBounds(), dataMapBounds()$Type %in% c("Spring not in Vent", "Spring at Vent")), color = "white", weight = 2, radius = 15, fillColor = "purple", fillOpacity = 0.2,
  #                    popup = ~paste(as.character(SID),as.character(Name), sep = " - "),
  #                    stroke = TRUE,
  #                    layerId = ~as.character(SID),
  #                    group = "Spring",
  #                    options = markerOptions(draggable = TRUE, riseOnHover = TRUE),
  #                    clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, zoomToBoundsOnClick = TRUE)
  #   ) %>%
  #   addCircleMarkers(data = subset(dataMapBounds(), dataMapBounds()$Type %in% c("River/Stream", "Canal", "Lake", "Lake Outflow", "Reservoir","Retention Pond","Pond", "Estuary", "Bay/Harbor", "Mine/Mine Discharge","Sinkhole", "Borrow Pit", "Ocean")), color = "white", weight = 2, radius = 15, fillColor = "black", fillOpacity = 0.2,
  #                    popup = ~paste(as.character(SID),as.character(Name), sep = " - "),
  #                    stroke = TRUE,
  #                    layerId = ~as.character(SID),
  #                    group = "Surface Water",
  #                    clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, zoomToBoundsOnClick = TRUE)
  #   ) %>%
  #   addCircleMarkers(data = subset(dataMapBounds(), dataMapBounds()$Type %in% c("Well")), color = "white", weight = 2, radius = 15, fillColor = "red", fillOpacity = 0.2,
  #                    popup = ~paste(as.character(SID),as.character(Name), sep = " - "),
  #                    stroke = TRUE,
  #                    layerId = ~as.character(SID), # to access by clicking on marker in map
  #                    group = "Groundwater",
  #                    # options = markerOptions(draggable = TRUE),
  #                    clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, zoomToBoundsOnClick = TRUE)
  #   ) %>%
  #   addCircleMarkers(data = subset(dataMapBounds(), dataMapBounds()$Type %in% "Wetland"), color = "white", weight = 2, radius = 15, fillColor = "darkgreen", fillOpacity = 0.2,
  #                    popup = ~paste(as.character(SID),as.character(Name), sep = " - "),
  #                    stroke = TRUE,
  #                    layerId = ~as.character(SID),
  #                    group = "Wetland",
  #                    clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, zoomToBoundsOnClick = TRUE)
  #   ) %>%
  #   addLayersControl(
  #     baseGroups = c("Topo", "Imagery"),
  #     overlayGroups = c("Surface Water", "Groundwater","Spring", "Wetland", "Atmospheric"),
  #     options = layersControlOptions(collapsed = FALSE)
  #   )
  # # }
  # map
  #!!!  })   
  
  # icon <- file.path("L:/Hydro Eval/Staff/Nathan/R/scripts/waterleveldashboard - app/app/circle.png")
  # addMarkers(
  #   popup = ~paste(as.character(SID),as.character(Name), sep = ", "),
  #   options = markerOptions(draggable = TRUE, opacity = 0.4),
  #   clusterOptions = markerClusterOptions(maxClusterRadius = 0.01)
  # )
  # 
  # clusterOptions = markerClusterOptions(maxClusterRadius = 0.01, iconWidth = 5, iconHeight = 5))
  # addProviderTiles("Esri.WorldTopoMap") # ESRIs topographic basemap
  
  # addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') # ESRIs topographic basemap
  
  
  
  # data <- reactiveValues(clickedMarker=NULL)
  # selected_site = eventReactive(input$mymap_marker_click,{
  #   event <- input$mymap_marker_click
  #   return(tableDT()[tableDT()$Long == event$lng,]) # & tableDT()$Lat == event$lat
  # })
  # dataLeaflet <- reactiveValues(mouseOver=list(id = "801582", nonce = 0.08816478,  lat = 28.53724, lng = -82.433))
  # dataLeaflet <- reactiveValues(id = input$mymap_marker_mouseover["id"], input$mymap_marker_mouseover["nonce"],  input$mymap_marker_mouseover["lat"], input$mymap_marker_mouseover["lng"])
  
  # mouseOver <- eventReactive(input$mymap_marker_mouseover,{
  #   dataLeaflet$mouseOver <- input$mymap_marker_mouseover
  # })
  
  #!!!!!!!!!!!!!!!!!!!
  
  observeEvent(input$mymap_marker_mouseover, {
    offset = isolate((input$mymap_bounds$north - input$mymap_bounds$south) / (20-input$mymap_zoom)^2)
    leafletProxy("mymap") %>%
      addPopups(lat = input$mymap_marker_mouseover$lat + offset, lng = input$mymap_marker_mouseover$lng, paste0(as.character(input$mymap_marker_mouseover$id), "<br/>", "(Click Circle to add)"), layerId = input$mymap_marker_mouseover$id)
  })
  
  h <- NULL
  observeEvent(input$mymap_marker_mouseout, {
    h <- c(input$mymap_marker_mouseout$id, h)
    leafletProxy("mymap", session) %>%
      # clearPopups()
      removePopup(layerId = h)
    
  })
  
  
  observeEvent(selStationNames(),{
    proxy <- leafletProxy("mymap", session)
    proxy %>% clearPopups()
    proxy %>% addPopups(inven()$Long, inven()$Lat,
                        popup = paste(as.character(inven()$SITE_ID),
                                      as.character(inven()$SITE_NAME), sep = " - "))
    proxy %>% fitBounds(lng1 = min(inven()$Long) - 0.01, lng2 = max(inven()$Long) + 0.01,
                        lat1 = max(inven()$Lat) + 0.01 , lat2 = min(inven()$Lat) -0.01)
  })
    
    #!!!!!!!!!!!!!!!!!!  
    
    # observeEvent(input$mymap_marker_mouseover$id, {
    #   mouseOver <- input$mymap_marker_mouseover$id
    #   leafletProxy("mymap", session) %>%
    #     addPopups(lng = mouseOver$lng, lat = mouseOver$lat, popup = mouseOver$id)
    #     # addPopups(lng = dataLeaflet$lng, lat = dataLeaflet$lat, popup = dataLeaflet$id)
    # })
    # 
    
    # dataLeaflet <- reactiveValues(id = "801582", nonce = 0.08816478,  lat = 28.53724, lng = -82.433)
    # observeEvent(input$mymap_marker_mouseover,{
    # dataLeaflet <- input$mymap_marker_mouseover
    # print(dataLeaflet)
    # })
    
    # observeEvent(input$submitButton,{
    #   updateTabsetPanel(session, "tabsetId", selected = "Time Series")
    # })
    
    observeEvent(input$mymap_marker_click,{
      # data$clickedMarker <- input$mymap_marker_click
      dt <- as.list(tableDT()$SID)
      names(dt) <- paste(tableDT()$SID, tableDT()$Name, sep = " - ")
      click <- input$mymap_marker_click
      updateSelectizeInput(session, "stationName", choices = dt, selected = c(input$stationName, strsplit(click$id, " ")[[1]][1]), label = "SID - Station(s)", options = list(placeholder = 'Enter Station - SID (Begin Typing)'))
      # leafletProxy("mymap", session) %>%
      # clearPopups() %>%
      # addPopups(click$lng, click$lat,
      # popup = click$id)
      # fitBounds(lng1 = min(inven()$Long) - 0.01, lng2 = max(inven()$Long) + 0.01,
      # lat1 = max(inven()$Lat) + 0.01 , lat2 = min(inven()$Lat) -0.01)
      # })
      # renderDataTable(tableDT()[tableDT()$SID == click$id,])}
      # output$site <- renderDataTable(data.frame(Long = round(click$lng,5), Lat = round(click$lat,5)))}#, ID = click$id))}
    })
    # observeEvent(input$mymap_click,{
    #   data$clickedMarker <- NULL
    #   output$site <- renderDataTable(data$clickedMarker)}
    #   )
    
    # 
    # site <- eventReactive(input$mymap_marker_click,{
    #   date <- input$mymap_marker_click
    # })
    # site <- renderDataTable(as.data.frame(site))
    # observeEvent(input$map_click,{
    #   data$clickedMarker <- NULL
    #   print(data$clickedMarker)})
    
    ## FIGURE OF SCATTERPLOT MATRIX
    # output$scatter <- renderPlot({
    #   cr = dcast(dataForm(), date~siteID, value.var = "level", mean)
    #   dataset <- cr[,-1] 
    #   colnames(dataset) <- inventory()[inventory()$SITE_ID %in% colnames(dataset),]$SITE_NAME
    #   pairs(dataset[1:ncol(dataset)])  
    # })
    
    ## CORRELATION MATRIX TABLE
    # output$corMatrix <- renderDataTable({
    #   cr <- dcast(dataForm(), date~siteID, value.var = "level", mean)
    #   dataset <- cr[,-1] 
    #   colnames(dataset) <- inventory()[inventory()$SITE_ID %in% colnames(dataset),]$SITE_NAME
    #   cor <- round(cor(dataset, use = "pairwise.complete.obs"),2)
    #   cor <- data.frame(Name = row.names(cor), cor)
    # }, options = list(searching = FALSE, paging = FALSE))
    # 
    ## INTERACTIVE TIME SERIES FROM JAVASCRIPT DYGRAPH PACKAGE
    # data <- reactiveValues(clickedMarker=NULL)
    # 
    # selected_site = eventReactive(input$mymap_marker_click,{
    #   event <- input$mymap_marker_click
    #   return(tableDT()[tableDT()$Long == event$lng,]) # & tableDT()$Lat == event$lat
    # })
    # 
    # site <- renderDataTable(selected_site)
    yAxisLabel <- reactive ({
      if(input$datum == "Level (NGVD29)"){
        yAxisLabel = "Level (NGVD29 ft)"
      } else if(input$datum == "Level (NAVD88)"){
        yAxisLabel = "Level (NAVD88 ft)"
      } else {
        yAxisLabel = "Flow (cfs)"
      }
    })
    
    output$dygraph = renderDygraph({
      validate(
        # need(input$stationName != "", "Please select SID - Station(s)"),
        # need(input$inCheckboxGroup != "", "Please review displayed stations"),
        # need(strsplit(selStationNames(), ",") %in% tableDT()$SID, "check this"),
        need(selStationNames() != "", "Please 'Submit'"),
        need(strsplit(selStationNames(), ",")[[1]] %in% tableDT()$SID, "Please enter SID - Station")
      )
      minLevel <- min(dataForm()$level, na.rm = TRUE)
      maxLevel <- max(dataForm()$level, na.rm = TRUE)
      crossTab = dcast(dataForm(), date~siteID, value.var = "level", mean)
      colnames(crossTab) <- c("date", inventory()[inventory()$SITE_ID %in% 
                                                    colnames(crossTab),]$SITE_NAME)
      row.names(crossTab) = crossTab$date
      xtsDataForm = as.xts(crossTab)
      dygraph(xtsDataForm) %>% 
        dyRangeSelector(fillColor = "", strokeColor = "") %>%
        dyAxis("y", label = yAxisLabel(), valueRange = c(minLevel, maxLevel)) %>%
        # dyAxis("y", label = paste0("Level (",input$datum, "ft)"), valueRange = c(minLevel, maxLevel)) %>% 
        dyLegend(show = "auto", width= 600)
      
    })
    
    ## DOWNLOAD BUTTON
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        src <- normalizePath('stationReport.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'stationReport.Rmd')
        
        out <- render('stationReport.Rmd', switch(
          input$format,
          PDF = pdf_document(fig_caption = TRUE, fig_width = 7, fig_height = 3.5), 
          HTML = html_document(), 
          Word = word_document()
        ))
        file.rename(out, file)
      }
    )
})