##install.packages("leaflet")
##install.packages("sp")
##install.packages("rgdal")
##install.packages("RColorBrewer")
##install.packages("leaflet.extras")
##install.packages("leaflet.minicharts")
##install.packages("htmlwidgets")
##install.packages("raster")
##install.packages("mapview")
##install.packages("leafem")

## Call the libraries
library(leaflet)
library(sp)
library(rgdal)
library(RColorBrewer)
library(leaflet.extras)
library(leaflet.minicharts)
library(htmlwidgets)
library(raster)
library(mapview)
library(leafem)

## Read the shapefile for the concentration map: regions and cities
## 1. Regions
regioni <- readOGR('shp/regioni/regioni.shp')

par(mar = c(5,5,0,0), cex = 10)
hist(regioni$vittime, breaks = 20, main = "")

pal_regioni <- colorNumeric(
  palette = "YlOrRd",
  domain = regioni$vittime)

## 2. Cities
citta <- readOGR('shp/citta/citta.shp')

par(mar = c(5,5,0,0), cex = 10)
hist(citta$vittime, breaks = 20, main = "")

pal_citta <- colorNumeric(
  palette = "YlOrRd",
  domain = regioni$vittime)

## Read the csv file of the repressive events and prepare the popup and icons
totale <- read.csv("csv/cronologica.csv")

content_popup <- paste(sep = "<br/>",
                       paste0("<div class='leaflet-popup-scrolled' style='max-width:200px;max-height:200px'>"),
                       paste0("Event type:"),
                       paste0("<b>", totale$tipo,"</b>", " from the database ", "<b>", totale$origine_dati,"</b>"),
                       paste0(" "),
                       paste0("Name:"),
                       paste0("<b>", totale$nome, "</b>"),
                       paste0(" "),
                       paste0("Date:"),
                       paste0("<b>", totale$data_intera, "</b>"),
                       paste0(" "),
                       paste0("Place:"),
                       paste0("<b>", totale$luogo,"</b>"),
                       paste0(" "),
                       paste0("Number of victims:"),
                       paste0("<b>", totale$vittime,"</b>"),
                       paste0(" "),
                       paste0("Authors:"),
                       paste0("<b>", totale$tipo, " ucciso/i dai, ",totale$autori_evento, "</b>"),
                       paste0(" "),
                       paste0("Event summary:"),
                       paste0("<small>", totale$descrizione,"</small>"),
                       paste0("</div>"))

icons <- makeAwesomeIcon(icon= ~icona, library='fa', markerColor = ~colori, iconColor = ~colori_icona)

html_legend <- 
"<i class='fa fa-user' aria-hidden='true'></i>Political and racial arrests<br/> 
  <i class='fa fa-users' aria-hidden='true'></i>Massacres<br/> 
<i class='fa fa-envelope' aria-hidden='true'></i>Last Letters"

## Read the csv with the three dataset of the corpus
fascisti <- read.csv("csv/01_fascisti.csv")
nazisti <- read.csv("csv/02_nazisti.csv")
nazifascisti <- read.csv("csv/03_nazifascisti.csv")

## Read the shapefile of the historical borders of Italy during WWII
italia_inizio <- readOGR('shp/italia_1/italia_1.shp')
italia_gustav <- readOGR('shp/italia_2/italia_2.shp')
italia_hitler <- readOGR('shp/italia_3/italia_3.shp')
italia_gotica <- readOGR('shp/italia_4/italia_4.shp')
italia_fine <- readOGR('shp/italia_5/italia_5.shp')

## Read the raster with the historical map.
r <- raster("raster/mappa.tif")
palraster <- colorNumeric(c("#000000", "#666666", "#FFFFFF"), 
                          values(r), 
                          na.color = "transparent")

## Read the geojson datasets for the search tool
## 1. Massacres
atlante <- readr::read_file('geojson/atlante.geojson')
atlante_icon <- makeAwesomeIcon(icon='paintbrush', 
                           library='ion',
                           markerColor = 'red',
                           iconColor = 'black')

icon_atlante <- makeIcon(
  iconUrl = "https://img.icons8.com/ios-glyphs/30/000000/conference-call.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94)

## 2. Arrests
cdec <- readr::read_file('geojson/cdec.geojson')
cdec_icon <- makeAwesomeIcon(icon = "home", library = "glyphicon",
                             markerColor = "blue", iconColor = "white", spin = FALSE,
                             extraClasses = NULL, squareMarker = FALSE, iconRotate = 0,
                             fontFamily = "monospace", text = NULL)

icon_cdec <- makeIcon(
  iconUrl = "https://img.icons8.com/ios-glyphs/30/000000/name.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94)

## Last Letters
lastletters <- readr::read_file('geojson/lastletters.geojson')
lastletters_icon <- makeAwesomeIcon(icon='cash', 
                                library='ion',
                                markerColor = 'green', 
                                iconColor = 'black')

icon_lastletters <- makeIcon(
  iconUrl = "https://img.icons8.com/ios-glyphs/30/000000/secured-letter.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94)


## Beginning of the map

m <- leaflet(regioni) %>% 
  
  ## Basemap
  addTiles() %>% 
  
  ## Add a coordinates reader
  leafem::addMouseCoordinates() %>% 
  
  ## Basemap
  addProviderTiles(providers$Thunderforest.OpenCycleMap)  %>% 
  ## Add a zoom reset button
  addResetMapButton() %>%
  ## Add a Minimap to better navigate the map
  addMiniMap() %>%

  ## Map center and zoom adjustment
  setView(lng = 10.883447, 
          lat = 45.938287, 
          zoom = 4 ) %>%
  
  ## Add the concentration maps according to the number of repressive events divided by region.
  addPolygons(data = regioni,
              stroke = TRUE,
              opacity = 0.5,
              fillOpacity = 0.5,
              weight = 0.5, 
              group = "Repressive events by region",
              smoothFactor = 0.5,
              fillColor = ~pal_regioni(vittime),
              color = "black",
              label = ~paste(regione, ": ", vittime, " victims", sep = "")) %>%
  
  ## Add a legend for the concentration map
  addLegend("bottomleft", pal = pal_regioni, values = ~vittime,
            title = "Repressive events spread over the territory (cities or regions)",
            labFormat = labelFormat(suffix = " victims"),
            opacity = 1) %>%
  
  ## Add the concentration maps according to the number of repressive events divided by city.
  addPolygons(data = citta,
              stroke = TRUE,
              opacity = 0.5,
              fillOpacity = 0.8,
              weight = 0.5, 
              group = "Repressive events by city",
              smoothFactor = 0.5,
              fillColor = ~pal_citta(vittime),
              color = "black",
              label = ~paste(citta, ": ", vittime, " vittime", sep = "")) %>%

  ## Add the layer with all the reppresive events and its legend
  addAwesomeMarkers(data = totale, 
             lng = ~lng, 
             lat = ~lat,
             group = "Total list of repressive events",
             icon = icons,
             popup = c(content_popup),
             clusterOptions = markerClusterOptions()) %>%
  
  addControl(html = html_legend, position = "bottomleft") %>%
  
  addLegend("bottomleft", 
            
            colors = c("red", 
                       "black",
                       "orange",
                       "white"),
            
            labels=c("Nazis", 
                     "Fascists", 
                     "Nazi-Fascist collaboration", 
                     "Unknown authors"),
            
            title="Legend of colours and symbols") %>%
  
  ## Add Heatmap to compare the three dataset
  ## 1. Fascists
  addHeatmap(fascisti$lng,
             fascisti$lat, 
             group = "Repressive acts by fascists (Heatmap)",
             blur = 7, 
             max = 0.5, 
             radius = 10,
             gradient = 'Black') %>%
  
  ## 2. Nazis
  addHeatmap(nazisti$lng,
             nazisti$lat, 
             group = "Repressive acts by the Nazis (Heatmap)",
             blur = 7, 
             max = 0.5, 
             radius = 10,
             gradient = 'Red') %>%
  
  ## 3. Nazis-Fascists
  addHeatmap(nazifascisti$lng,
             nazifascisti$lat, 
             group = "Repressive acts conducted by both (Heatmap)",
             blur = 7, 
             max = 0.5,
             radius = 10,
             gradient = 'yellow') %>%
  
  ## Add the shapfiles with Italian borders during World War Two
  ## Untill  11 July 1943
  addPolygons(data = italia_inizio, 
              color = "red",
              opacity = 1,
              stroke = TRUE,
              fillOpacity = 0,
              weight = 2, 
              smoothFactor = 0.5,
              group = "Level 1 - Italy until the arrival of the Allies 10 July 1943",
              label = ~paste("Italy until the arrival of the Allies 10 July 1943")) %>%
  
  ## Untill 18 May 1944
  addPolygons(data = italia_gustav, 
              color = "blue",
              opacity = 1,
              stroke = TRUE,
              fillOpacity = 0,
              weight = 2.5, 
              smoothFactor = 0.5,
              group = "Level 2 - Gustav Line - 10 July 1943 to 18 May 1944",
              label = ~paste("Gustav Line - 10 July 1943 to 18 May 1944")) %>%
  
  ## Untill 24 May 1944
  addPolygons(data = italia_hitler, 
              color = "blue",
              opacity = 1,
              stroke = TRUE,
              fillOpacity = 0,
              weight = 2.5, 
              smoothFactor = 0.5,
              group = "Level 3 - Hitler Line - 18 May 1944 to 24 May 1944",
              label = ~paste("Hitler Line - 18 May 1944 to 24 May 1944")) %>%
  
  ## Untill 21 April 1945
  addPolygons(data = italia_gotica, 
              color = "blue",
              opacity = 1,
              stroke = TRUE,
              fillOpacity = 0,
              weight = 2.5, 
              smoothFactor = 0.5,
              group = "Level 4 - Gothic Line - 24 May 1944 to 21 April 1945",
              label = ~paste("Gothic Line - 24 May 1944 to 21 April 1945")) %>%
  
  ## Untill the end of the war
  addPolygons(data = italia_fine, 
              color = "green",
              opacity = 1,
              stroke = TRUE,
              fillOpacity = 0,
              weight = 2.5, 
              smoothFactor = 0.5,
              group = "Level 5 - Italy at the end of the war - 25 April 1945",
              label = ~paste("Italy at the end of the war - 25 April 1945")) %>%
  
  # Add the raster with the thematic map
  addRasterImage(r,
                 opacity = 1,
                 colors = palraster,  
                 maxBytes = 123123123,
                 group = "Level 6 - Advance of the Allies (thanks to ANPI Milano and Memorieincammino.it) ") %>%
  
  addBootstrapDependency() %>%
  
  ## Add the geojson for each datasets for the search box
  ## 1. Massacres
  addGeoJSONv2(
    atlante,
    labelProperty='NAME',
    popupProperty=propstoHTMLTable(
      table.attrs = list(class='table table-striped table-bordered'), drop.na = T),
    labelOptions = labelOptions(textsize ='12px', direction = 'auto' ),
    markerIcons = icon_atlante,
    markerOptions = markerOptions(riseOnHover = TRUE, opacity = 1),
    clusterOptions = markerClusterOptions(), group = "List of massacres (http://www.straginazifasciste.it/)") %>%
  
  ## 2. Arrests
  addGeoJSONv2(
    cdec,
    labelProperty='NAME',
    popupProperty=propstoHTMLTable(
      table.attrs = list(class='table table-striped table-bordered'), drop.na = T),
    labelOptions = labelOptions(textsize ='12px', direction = 'auto' ),
    markerIcons = icon_cdec,
    markerOptions = markerOptions(riseOnHover = TRUE, opacity = 1),
    clusterOptions = markerClusterOptions(), group = "List of political and racial arrests (http://dati.cdec.it/)") %>%
  
  ## 3. Last Letters
  addGeoJSONv2(
    lastletters,
    labelProperty='NAME',
    popupProperty=propstoHTMLTable(
      table.attrs = list(class='table table-striped table-bordered'), drop.na = T),
    labelOptions = labelOptions(textsize ='12px', direction = 'auto' ),
    markerIcons = icon_lastletters,
    markerOptions = markerOptions(riseOnHover = TRUE, opacity = 1),
    clusterOptions = markerClusterOptions(), group = "Places where last letters were written (http://www.ultimelettere.it/)") %>%
  
  ## Add the Search Tool
  addSearchFeatures(
    
    targetGroups =  c('List of massacres (http://www.straginazifasciste.it/)',
                      'List of political and racial arrests (http://dati.cdec.it/)', 
                      'Luoghi di scrittura delle ultime lettere (http://www.ultimelettere.it/)'),
    
    options = searchFeaturesOptions(
      propertyName='NAME', zoom=18, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE, position = "topleft" )) %>%
  
  addControl("<P><B>Search</B> for a person's name or an event.</P>",
             position='topleft') %>%
  
  ##addCircleMarkers(data=totale, lat = ~lat, lng = ~lng,
    ##               color = ~colori, popup = ~nome, radius = ~percentuale, group = "Totale")%>%

  ## Add the legends 
  addLegend("topright", 
            
            colors = c("trasparent", "trasparent", "trasparent"),
            
            labels=c("Giovanni Pietro Vitali - University College Cork", "Marie Skłodowska Curie Action European Fellow", "giovannipietrovitali@gmail.com"),
            
            title="Atlas of Nazi-Fascist Repression") %>%
  
  addLayersControl(baseGroups = c("Level 1 - Italy until the arrival of the Allies 10 July 1943",
                                  "Level 2 - Gustav Line - 10 July 1943 to 18 May 1944",
                                  "Level 3 - Hitler Line - 18 May 1944 to 24 May 1944",
                                  "Level 4 - Gothic Line - 24 May 1944 to 21 April 1945",
                                  "Level 5 - Italy at the end of the war - 25 April 1945",
                                  "Level 6 - Advance of the Allies (thanks to ANPI Milano and Memorieincammino.it) ",
                                  "Empty Map"),
                   
                   overlayGroups = c("Repressive events by region", 
                                     "Repressive events by city", 
                                     "Total list of repressive events", 
                                     "List of massacres (http://www.straginazifasciste.it/)",
                                     "List of political and racial arrests (http://dati.cdec.it/)",
                                     "Places where last letters were written (http://www.ultimelettere.it/)",
                                     "Repressive acts by the Nazis (Heatmap)",
                                     "Repressive acts by fascists (Heatmap)",
                                     "Repressive acts conducted by both (Heatmap)"),
                   
                   options = layersControlOptions(collapsed = TRUE)) %>%
  
  hideGroup(c("Empty Map",
              "List of massacres (http://www.straginazifasciste.it/)",
              "List of political and racial arrests (http://dati.cdec.it/)",
              "Places where last letters were written (http://www.ultimelettere.it/)",
              "Repressive events by region",
              "Repressive events by city",
              "Repressive acts by the Nazis (Heatmap)",
              "Repressive acts by fascists (Heatmap)",
              "Repressive acts conducted by both (Heatmap)",
              "Level 2 - Gustav Line - 10 July 1943 to 18 May 1944",
              "Level 3 - Hitler Line - 18 May 1944 to 24 May 1944",
              "Level 4 - Gothic Line - 24 May 1944 to 21 April 1945",
              "Level 5 - Italy at the end of the war - 25 April 1945",
              "Level 6 - Advance of the Allies (thanks to ANPI Milano and Memorieincammino.it) "))
  
  m