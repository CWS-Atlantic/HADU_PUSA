# Shiny app to interactively explore HARD and PUSA Survey data

library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(mapedit)
library(mapview)
library(sf)
library(stringr)  #comment in for data manips
library(viridis)
require(shiny)
require(leafpop)
require(readxl) #comment in for data manips
require(miniUI)

#setwd("C:/Users/englishm/Documents/Harlequins/For Publication/Data/")

#############################################
##  read in data and do some basic manips  ##
#############################################
# 
# 
hard.surveys <- read_xlsx("V:/Sackville/Wildlife/Databases/HADU_PUSA/Data/HADU_PUSA_Compilation_2024-10-04.xlsx", 1)

hard.obs <- read_xlsx("V:/Sackville/Wildlife/Databases/HADU_PUSA/Data/HADU_PUSA_Compilation_2024-10-04.xlsx", 2)


# pusa.surveys <- read_xlsx("C:/Users/englishm/Documents/Harlequins/PUSA Compilation_20231116.xlsx", 2)

# pusa.obs <- read_xlsx("C:/Users/englishm/Documents/Harlequins/PUSA Compilation_20231116.xlsx", 1)
#

#####################################
##   Filter data for publication   ##
#####################################

hard.pub <- select(hard.obs,
                   survey_id,
                   year,
                   month,
                   day,
                   #time, bugged at the moment
                   #waypoint_id,
                   latitude,
                   longitude,
                   province,
                   region,
                   location,
                   #notes,
                   HADU_M,
                   HADU_F,
                   HADU_unkn,
                   HADU_total,
                   PUSA)


hard.surveys.pub <- select(hard.surveys,
                           survey_id,
                           track_id,
                           target.species.1,
                           target.species.2,
                           #target.species.3,     #unused
                           #incidental.sightings, #not needed for pub
                           survey.platform,
                           agency,
                           year,
                           month,
                           day,
                           #region,         #not needed for pub
                           Province,
                           #start.time,     #times are bugged
                           start.location,
                           start.lat,
                           start.lon,
                           #end.time,       #times are bugged
                           end.location,
                           end.lat,
                           end.lon,
                           #platform,       #not needed for pub
                           #operator,       #not needed for pub
                           #pilot,          #privacy
                           #observer1,      #privacy
                           #observer2,      #privacy
                           #methods.notes,  #not needed for pub
                           #weather.code,   #not needed for pub
                           #glare,          #unused
                           wind.speed.kt,
                           wind_direction_code)#,
                           #seastate,        #not needed for pub
                           #swell.height.m,  #not needed for pub
                           #weather,         #not needed for pub
                           #data.source,     #not needed for pub
                           #notes)           #not needed for pub


# # # Check survey IDs are present and the same on both sheets: THIS IS GOOD FOR THE 2024-10-04 VERSION
# #
# # length(unique(hard.pub$survey_id))
# # 
# # obs.ids <- unique(hard.pub$survey_id)
# #
# # 
# # length(unique(hard.surveys.pub$survey_id))
# # 
# # survey.ids <- (unique(hard.surveys.pub$survey_id))
# # 
# # survey.ids <- as.data.frame(survey.ids)
# # 
# # #survey.ids <- rbind(survey.ids, "x")
# # 
# # 
# # obs.ids <- as.data.frame(obs.ids)
# # 
# # #obs.ids <- rbind(obs.ids, "x")
# # 
# # all.ids <- cbind(obs.ids, survey.ids)
# # 
# # all.ids$obs.ids <- sort(all.ids$obs.ids)
# # 
# # all.ids$survey.ids <- sort(all.ids$survey.ids)
# # 
# # 
# # #look for mismatches, in theory they should all line up
# # all.ids$match <- ifelse(all.ids$obs.ids == all.ids$survey.ids, "MATCH", "BIG ERROR LOL")
# 
# 
# ###############################################################
# ##  Work on incorporating the PUSA survey data compilation   ##  ~~THIS IS DONE!
# ###############################################################
# 
# # pusa.survey.id <- as.data.frame(unique(pusa.surveys$survey_id))
# #
# # pusa.survey.id <- as.data.frame(pusa.survey.id[order(pusa.survey.id$`unique(pusa.surveys$survey_id)`),])
# #
# # names(pusa.survey.id) <- "pusa.survey.id"
# #
# #
# # pusa.obs.id <- as.data.frame(unique(pusa.obs$survey_id))
# #
# # pusa.obs.id <- as.data.frame(pusa.obs.id[order(pusa.obs.id$`unique(pusa.obs$survey_id)`),])
# #
# # names(pusa.obs.id) <- "pusa.obs.id"
# #
# # #pusa.obs.id <- rbind(pusa.obs.id, "x")
# #
# #
# # all.ids <- cbind(pusa.obs.id, pusa.survey.id)
# #
# # names(all.ids) <- c("pusa.obs.id",
# #                     "pusa.survey.id")
# #
# # #all.ids <-  all.ids[order(all.ids$pusa.obs.id),]
# #
# #
# # #look for mismatches, in theory they should all line up
# # all.ids$match <- ifelse(all.ids$pusa.obs.id == all.ids$pusa.survey.id, "MATCH", "BIG ERROR LOL")
# #
# # subset <- pusa.obs.id[!pusa.obs.id$pusa.obs.id %in% obs.ids$obs.ids,]
# 
##########################$###
##  Deal with Observations  ##
#########################$####


### check lat lons

range(as.numeric(hard.pub$latitude), na.rm =T)

range(as.numeric(hard.pub$longitude), na.rm =T)

hard.pub <- filter(hard.pub, latitude > 0)

hard.pub <- filter(hard.pub, longitude < 0)

# #filter out the zeroes #unsure of this step, the zeroes are still important
# hard.pub <- filter(hard.pub,
#                    HADU_total > 0 | PUSA > 0)

#deal with NULLS: HADU

hard.pub[] <- lapply(hard.pub, gsub, pattern = "<NULL>", replacement = NA)

hard.pub[] <- lapply(hard.pub, gsub, pattern = "<Null>", replacement = NA)


#convert values to numeric
hard.pub$HADU_total <- as.numeric(hard.pub$HADU_total)

hard.pub$PUSA <- ifelse(is.na(hard.pub$PUSA), NA, hard.pub$PUSA)

hard.pub$PUSA <- as.numeric(hard.pub$PUSA)


#order by year (makes the datatable more intuitive)
hard.pub <- hard.pub[order(hard.pub$year),]

#assign obs to the breeding or non-breeding season
hard.pub$period <- ifelse(hard.pub$month %in% c(5,6,7), "Breeding", "Non-Breeding")

#work on fixing NAs and 0s based on target species.

species.matrix <- as.data.frame(cbind(hard.surveys.pub$survey_id, hard.surveys.pub$target.species.1, hard.surveys.pub$target.species.2))

names(species.matrix) <- c("survey_id",
                           "target_species_1",
                           "target_species_2")

#join the species matrix to the observations file

hard.pub <- left_join(hard.pub, species.matrix, by = "survey_id")

hard.pub$target_species_bind <- paste(hard.pub$target_species_1, hard.pub$target_species_2, sep="_")


#correct HADU counts based on target species
hard.pub$HADU_total_correct <- ifelse(str_detect(hard.pub$target_species_bind, "HADU"),
                                                  hard.pub$HADU_total,
                                                  NA)
#correct PUSA counts based on target species
hard.pub$PUSA_total_correct <- ifelse(str_detect(hard.pub$target_species_bind, "PUSA"),
                                                  hard.pub$PUSA,
                                                  NA)

hard.pub <- select(hard.pub,
                   survey_id,
                   year,
                   month,
                   day,
                   period,
                   province,
                   latitude,
                   longitude,
                   HADU_M,
                   HADU_F,
                   HADU_unkn,
                   HADU_total_correct,
                   PUSA_total_correct)


names(hard.pub) <- c("SurveyID_EnqueteID",
                     "Year_Annee",
                     "Month_Mois",
                     "Day_Jour",
                     "Periode",
                     "Province",
                     "Latitude",
                     "Longitude",
                     "HADU_M",
                     "HADU_F",
                     "HADU_U",
                     "HADU_Total",
                     "PUSA"
                     )


#abbreviate the provinces
hard.pub$Province <- gsub("NL", "TNL_NL", hard.pub$Province)
hard.pub$Province <- gsub("NS", "NE_NS", hard.pub$Province)
hard.pub$Province <- gsub("NB", "NB", hard.pub$Province)

#make periode numeric
hard.pub$Periode <- gsub("Non-Breeding", 1, hard.pub$Periode)
hard.pub$Periode <- gsub("Breeding", 2, hard.pub$Periode)


###########################
##   Deal with surveys   ##
###########################

str(hard.surveys.pub)

hard.surveys.pub[] <- lapply(hard.surveys.pub, gsub, pattern = "NA", replacement = NA)
hard.surveys.pub[] <- lapply(hard.surveys.pub[], gsub, pattern = "<NULL>", replacement = NA)
hard.surveys.pub[] <- lapply(hard.surveys.pub[], gsub, pattern = "<Null>", replacement = NA)

#fix provinces
hard.surveys.pub$Province <- gsub("Nova Scotia", "NE_NS", hard.surveys.pub$Province)
hard.surveys.pub$Province <- gsub("Newfoundland", "TNL_NL", hard.surveys.pub$Province)
hard.surveys.pub$Province <- gsub("New Brunswick", "NB", hard.surveys.pub$Province)
hard.surveys.pub$Province <- gsub("Prince Edward Island", "PE", hard.surveys.pub$Province)

#code out the survey platform
hard.surveys.pub$survey.platform <- gsub("Land", 1, hard.surveys.pub$survey.platform)
hard.surveys.pub$survey.platform <- gsub("Boat", 2, hard.surveys.pub$survey.platform)
hard.surveys.pub$survey.platform <- gsub("Helicopter", 3, hard.surveys.pub$survey.platform)
hard.surveys.pub$survey.platform <- gsub("Plane", 4, hard.surveys.pub$survey.platform)

range(as.numeric(hard.surveys.pub$start.lat), na.rm=T)
range(as.numeric(hard.surveys.pub$end.lat), na.rm=T)

range(as.numeric(hard.surveys.pub$start.lon), na.rm=T)
range(as.numeric(hard.surveys.pub$end.lon), na.rm=T)

names(hard.surveys.pub) <- c("SurveyID_EnqueteID",
                             "TrackID_PisteID",
                             "TargetSpecies1_EspeceCible1",
                             "TargetSpecies2_EspeceCible2",
                             "Platforme",
                             "Agency_Agence",
                             "Year_Annee",
                             "Month_Mois",
                             "Day_Jour",
                             "Province",
                             "StartLocation_LocationDebut",
                             "StartLatitude_LatitudeDebut",
                             "StartLongitude_LongitudeDebut",
                             "EndLocation_LocationFin",
                             "EndLatitude_LatitudeFin",
                             "EndLongitude_LongitudeFin",
                             #"Weather_Temps",
                             "WindSpeed_VitesseVent_kt",
                             "WindDirection_DirectionVent"
                             )


#fix some place names
unique(hard.surveys.pub$StartLocation_LocationDebut)

unique(hard.surveys.pub$EndLocation_LocationFin)

hard.surveys.pub[] <- lapply(hard.surveys.pub, gsub, pattern = "Corner Brooke", replacement = "Corner Brook")
hard.surveys.pub[] <- lapply(hard.surveys.pub, gsub, pattern = "St. Albans", replacement = "St. Alban's")
hard.surveys.pub[] <- lapply(hard.surveys.pub, gsub, pattern = "J.T. Cheeseman PP", replacement = "J.T. Cheeseman Provincial Park")
hard.surveys.pub[] <- lapply(hard.surveys.pub, gsub, pattern = "Penninsula", replacement = "Peninsula")


##################################################
##   write CSVs and GDB for ECCC Publication    ##
##################################################

#add Track_ID to the dataframe

hard.pub <- merge(x = hard.pub, 
                  y = hard.surveys.pub[, c("SurveyID_EnqueteID", "TrackID_PisteID")], 
                  by = "SurveyID_EnqueteID", 
                  all.x = TRUE)

###  TO DO:
## - remove IEMR data and surveys
iemr <- filter(hard.surveys.pub,
               Agency_Agence == "CWS/IEMR")

iemr.surveys <- iemr$SurveyID_EnqueteID
iemr.tracks <- iemr$TrackID_PisteID

#kick out IEMR data:

hard.pub <- hard.pub[!hard.pub$SurveyID_EnqueteID %in% iemr.surveys,]

hard.surveys.pub <- hard.surveys.pub[!hard.surveys.pub$SurveyID_EnqueteID %in% iemr.surveys,]


# setwd("C:/users/englishm/Documents/Harlequins/For Publication/Data/")
# getwd()
# 
# # write CSV
# write.csv(hard.pub, "CWS_Atlantic_HADU_PUSA_Observations_1966-2024_EN_FR.csv", row.names = F)
# 
# write.csv(hard.surveys.pub, "CWS_Atlantic_HADU_PUSA_Conditions_1966_2024_EN_FR.csv", row.names = F)
# #write GDB
# 
#convert to a SF object
hard.sf <- st_as_sf(hard.pub,
                    coords = c("Longitude", "Latitude"),
                    crs = 4326,
                    agr = "constant",
                    remove = FALSE,
                    na.fail = F)


st_write(hard.sf,
         layer = "Observations",
         dsn = "CWS_Atlantic_HADU_PUSA_Observations_1966_2024_EN_FR.gdb",
         driver = "OpenFileGDB",
         append = F)

##################################
##  Add rivers / survey effort  ##
##################################

#st_layers("C:/Users/EnglishM/Documents/Harlequins/For Publication/ATLR_HARD_PUSA_Tracklines_MasterDB.gdb")

survey.lines <- st_read("V:/Sackville/Wildlife/Databases/HADU_PUSA/Data/Survey track lines/Compilation/HARD_PUSA_Tracklines_Compilation.gdb")

survey.lines <- st_transform(survey.lines, 4326)

survey.lines <- st_zm(survey.lines, drop = T)

survey.lines <- select(survey.lines,
                       Track_ID,
                       Province,
                       Survey_Platform,
                       Shape_Length,
                       Shape)

#code out the survey platform
survey.lines$Survey_Platform <- gsub("Land", 1, survey.lines$Survey_Platform)
survey.lines$Survey_Platform <- gsub("Boat", 2, survey.lines$Survey_Platform)
survey.lines$Survey_Platform <- gsub("Helicopter", 3, survey.lines$Survey_Platform)
survey.lines$Survey_Platform <- gsub("Plane", 4, survey.lines$Survey_Platform)

names(survey.lines) <- c("TrackID_PisteID",
                         "Province",
                         "Platforme",
                         "Length_Longueur_m",
                         "Shape")

#kick out IEMR tracks
survey.lines <- survey.lines[!survey.lines$TrackID_PisteID %in% iemr.tracks,]


#abbreviate the provinces
survey.lines$Province <- gsub("Newfoundland and Labrador", "TNL_NL", survey.lines$Province)
survey.lines$Province <- gsub("Newfoundland", "TNL_NL", survey.lines$Province)
survey.lines$Province <- gsub("Nova Scotia", "NE_NS", survey.lines$Province)
survey.lines$Province <- gsub("New Brunswick", "NB", survey.lines$Province)

st_write(survey.lines,
         layer = "SurveyEffort",
         dsn = "CWS_Atlantic_HADU_PUSA_SurveyEffort_1966_2024_EN_FR.gdb",
         driver = "OpenFileGDB",
         append = F)

#################
##   RUN APP   ##
#################

#!# Comment out everything before this line!!

hard.pub <- read.csv("C:/Users/englishm/Documents/Harlequins/For Publication/Data/CWS_Atlantic_HADU_PUSA_Observations_1966_2024_EN_FR.csv")

#convert to a SF object
hard.sf <- st_as_sf(hard.pub,
                     coords = c("Longitude", "Latitude"),
                     crs = 4326,
                     agr = "constant",
                     remove = FALSE,
                     na.fail = F)

#create a "total birds" column just for plotting

hard.pub <- 
  hard.pub %>% 
  rowwise() %>% 
  mutate(total_birds = sum(HADU_Total, PUSA, na.rm = TRUE))

str(hard.pub)
###############################
##   Create user interface   ##
###############################

ui <- miniPage(
  
  #this supresses warning messages in the app. Needed because of how the plot is initially rendered
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  gadgetTitleBar("Harlequin Duck and Purple Sandpiper Surveys", left = NULL, right = NULL),
  
  miniTabstripPanel(
    miniTabPanel("ReadMe", icon = icon("file-alt"),
                 miniContentPanel(htmlOutput("text"))),
    
    miniTabPanel("Map", icon = icon("map-o"),
                 miniContentPanel(
                   style = "height: 90vh; overflow-y: auto;",  ## ADD A SCROLLBAR
                   editModUI("mymap", height = "100%", width = "100%"),
                   
                   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                                 draggable = TRUE, top = 120, left = "auto", right = 20, bottom = "auto", 
                                 width = 380, height = "auto",
                                 style = "z-index: 1000; opacity: 0.925",
                                 HTML('<button data-toggle="collapse" data-target="#demo">-</button>'),
                                 tags$div(id = 'demo',  class="collapse-in",
                                          
                                          h4("Summary data"),
                                          
                                          plotOutput("plot"),
                                          
                                          DTOutput("table"),
                                          
                                          uiOutput("downloadDataButton")
                                          )
                                 )
                 )
                 )
  ))

server <- function(input, output) {
  
  ###################################
  ##   Set up the colour palette   ##
  ###################################
  
  pal <- colorFactor(palette = 'plasma', #this comes from the viridis package
                     domain = hard.pub$Year)
  
  ######################
  ##   Draw the map   ##
  ###################### 
  
  #setup namespace for the leaflet map
  ns <- NS("mymap")
  
  lf <- leaflet(options = leafletOptions(minZoom = 5, maxZoom = 14)) %>%  
    addProviderTiles("Esri.OceanBasemap",group="OceanBasemap")  %>%       
    addProviderTiles("Esri.WorldImagery",group="WorldImagery")  %>%
    addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
    setView(-62.654, 49.373, zoom = 5) %>%
    
    addLayersControl(baseGroups = c("OceanBasemap","WorldImagery","TopoMap"),
                     overlayGroups = hard.pub$Year, 
                     position = "topleft",
                     options = layersControlOptions(collapsed = FALSE)) %>%
    
    addPolylines(data = survey.lines,
                 color = "red",
                 group = "Tracks",
                 weight = 2,
                 popup = popupTable(survey.lines, zcol = c("Track_ID", "Province", "Survey_Platform"), row.numbers = F, feature.id = F)) %>%

    addCircleMarkers(data = hard.pub,
                     radius = log(hard.pub$total_birds) + 1,
                     lng = as.numeric(hard.pub$Longitude),
                     lat = as.numeric(hard.pub$Latitude),
                     fillOpacity = 0.6,
                     fillColor = ~pal(Year), #maybe you dont want the years colour coded, but the species?
                     color = "black",
                     weight = 1,
                     group = as.character(hard.pub$Year),
                     popup = popupTable(hard.pub, zcol = c("Year", "Month", "Day", "HADU_Total", "PUSA", "Period", "Track_ID", "Survey_ID"), row.numbers = F, feature.id = F)) %>%
    
    addDrawToolbar(targetGroup='Selected',
                   polylineOptions=FALSE,
                   markerOptions = FALSE,
                   circleOptions = FALSE,
                   circleMarkerOptions = FALSE,
                   polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.2,
                                                                                     color = 'blue',
                                                                                     weight = 3)),
                   rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0.2,
                                                                                         color = 'blue',
                                                                                         weight = 3)),
                   editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  
  edits <- callModule(editMod, "mymap", leafmap = lf)
  
  #query the zoom level and store it as a reactive object
  mapzoom <- reactive({
    input[[ns("map_zoom")]]
  })
  
  #query the map bounds and store it as a reactive object
  mapbounds <- reactive({
    input[[ns("map_bounds")]]
  })
  
  ###################################
  ##   create a reactive dataset   ## 
  ###################################
  
  #setup a reactive dataset that only looks at the data in the drawn polygon
  selectedLocations <- reactive({
    req(edits()$finished)
    df <- as.data.frame(st_intersection(edits()$finished, hard.pub))
    df <- dplyr::select(df, year, HADU_total_correct, PUSA_total_correct)
    df <- df %>%
       group_by(year) %>%
       summarise(HADU_Total = sum(HADU_total_correct, na.rm = T),
                 PUSA_Total = sum(PUSA_total_correct, na.rm = T))
    
    
    df <- as.data.frame(df)
  })
  
  ##########################
  ##   Render Datatable   ##
  ##########################
  
  output$table <- renderDT({
    if(sum(selectedLocations()$HADU_Total)==0) {
      print("No data selected, please draw a shape over points.") #this used to work but now it doesnt. not sure why
    }
    else{
      datatable(as.data.frame(select(selectedLocations(), year, HADU_Total, PUSA_Total)), 
                options = list(pageLength = 5,
                               searching = F))
    }
  })
  
  ##################### 
  ##   Render plot   ##               ~~IT DOESNT DEAL WITH NAs IN THE WAY I WANT IT TO!!
  #####################
  
  #removed for now, but you can edit this back in if you like.
  
  output$plot <- renderPlot({
    if(sum(selectedLocations()$HADU_Total & sum(selectedLocations()$PUSA_Total))==0) {
      print("No data selected, please draw a shape over points.") #this used to work but now it doesnt. not sure why
    }
    else{
      ggplot(data = selectedLocations()) +
        geom_point(aes(x = year, y = HADU_Total, colour = "black"), size = 5) +
        geom_line(aes(x = year, y = HADU_Total, colour = "HADU"), size = 2) +
        
        geom_point(aes(x = year, y = PUSA_Total, colour = "black"), size = 5) +
        geom_line(aes(x = year, y = PUSA_Total, colour = "PUSA"), size = 2) +
        
        theme(
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          text = element_text(size=15)) +
        scale_x_continuous() +
        scale_y_continuous(limits = c(0, max(selectedLocations()$HADU_Total + selectedLocations()$PUSA_Total))) +
        
        scale_color_manual(values = c(#"Raw Data" = "black",
          "HADU" = "black",
          "PUSA" = "purple")) +
        
        xlab("Survey Year") +
        ylab("Total Birds")
    }
  })
  
  # Make the download button only appear when data is selected.
  output$downloadDataButton <- renderUI({
    if(!is.null(selectedLocations())) {
      downloadButton("downloadData", "Download.csv")
    }
  })
  
  # Downloadable csv of selected reactive dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("HARD_PUSA_data", Sys.Date(),".csv",sep="")
    },
    content = function(file) {
      write.csv(selectedLocations(), file, row.names = FALSE)
    })
  
  ########################################
  ##   HTML panel to describe the app   ##
  ########################################
  
  #Write whatever you want here. the HTML commands can be found here: https://shiny.rstudio.com/articles/tag-glossary.html
  
  output$text <- renderUI({
    HTML(paste("<h4> This is a Shiny application to interactively explore Harlequin Duck and Purple Sandpiper survey data in Eastern Canada. </h4> <hr>", 
               "You can draw either a rectagle or a polygon across an area containing points to receive a summary plot and table of the total number of birds seen per year. <br/>", 
               "You may draw multiple shapes across areas and the summary data will update automatically. To remove the shapes, click the <b> Delete </b> button (the garbage can), then click on an individual shape and click <b> Save </b> after clicking an individual shape. <br/>",
               "In cases where the map data is not available, try toggling between the 'OceanBaseMap', 'WorldImagery', and 'TopoMap' tabs on the left panel <br/>",
               "You may download the summary table in a .csv by clicking the <b> Download .csv </b> button. <br/>",
               "Click the <b> Map </b> tab at the bottom to get started.",
               sep = "<br/>"))
  })
}

shinyApp(ui, server)

##############
