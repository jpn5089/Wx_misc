library(leaflet)
library(stringi)
library(htmltools)
library(RColorBrewer)
library(dplyr)

storm <- readLines("http://weather.unisys.com/hurricane/atlantic/2017/FRANKLIN/track.dat")

storm_dat <- read.table(textConnection(gsub("TROPICAL ", "TROPICAL_", storm[3:length(storm)])), 
                        header=TRUE, stringsAsFactors=FALSE) %>%
  mutate(STAT = gsub("_", " ", STAT))

# make column names prettier
colnames(storm_dat) <- c("advisory", "lat", "lon", "time", "wind_speed", "pressure", "status")

forecast <- data.frame(advisory = c("+12", "+24", "+36", "+48", "+72"),
                       lat = c(17.4,18.5,19.5,20.2,20.9),
                       lon = c(-84.9, -87.1, -89.3, -91.3, -95.2),
                       time = c("08/07/12Z","08/08/00Z","08/08/12Z","08/09/00Z","08/10/00Z"),
                       wind_speed = c(40, 50, 35, 40, 60),
                       pressure = c("NA","NA","NA","NA","NA"),
                       status = c("TROPICAL STORM", "TROPICAL STORM", "TROPICAL STORM", "TROPICAL STORM", "TROPICAL STORM"))

all <- rbind(storm_dat, forecast)

all$color <- as.character(factor(all$status, 
                                       levels=c("Tropical Depression", "Tropical Storm",
                                                "Hurricane-1", "Hurricane-2", "Hurricane-3",
                                                "Hurricane-4", "Hurricane-5"),
                                       labels=rev(brewer.pal(7, "YlOrBr"))))

last_advisory <- tail(which(grepl("^[[:digit:]]+$", all$advisory)), 1)

# draw the map
leaflet() %>% 
  addTiles() %>% 
  addPolylines(data=all[1:last_advisory,], ~lon, ~lat, color=~color) -> tmp_map

if (last_advisory < nrow(all)) {
  
  tmp_map <- tmp_map %>% 
    addCircles(data=all[last_advisory:nrow(all),], ~lon, ~lat, color=~color, fill=~color, radius=25000,
               popup=~sprintf("<b>Advisory forecast for +%sh (%s)</b><hr noshade size='1'/>
                              Position: %3.2f, %3.2f<br/>
                              Expected strength: <span style='color:%s'><strong>%s</strong></span><br/>
                              Forecast wind: %s (knots)<br/>Forecast pressure: %s",
                              htmlEscape(advisory), htmlEscape(time), htmlEscape(lon),
                              htmlEscape(lat), htmlEscape(color), htmlEscape(status), 
                              htmlEscape(wind_speed), htmlEscape(pressure)))
}

html_print(tmp_map)

###################################

# Forked from https://rud.is/b/2015/08/20/track-hurricane-danny-with-r-leaflet/
# Requires devtools::install_github('rstudio/leaflet')

library(xml2)
#library(plyr)
library(leaflet)
library(stringi)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(rvest)

html <- read_html('http://weather.unisys.com/hurricane/atlantic/2016/index.php')
links <- html_attr(html_nodes(html, "a"), "href")
links <- links[grep('track.dat', links)]

# get track data
track <- select.list(links, title="Select storm:", graphics = FALSE)
url <- paste("http://weather.unisys.com/hurricane/atlantic/2016", track, sep="/")

storm <- readLines(url)
storm <- read.table(textConnection(gsub("TROPICAL ", "TROPICAL_", storm[3:length(storm)])), header=TRUE, stringsAsFactors=FALSE)

# make storm type names prettier
storm$STAT <- stri_trans_totitle(gsub("_", " ", storm$STAT))

# make column names prettier
colnames(storm) <- c("advisory", "lat", "lon", "time", "wind_speed", "pressure", "status")

# Storm scale
ss <-  c("Tropical Depression", "Tropical Storm", "Hurricane-1", "Hurricane-2", "Hurricane-3", "Hurricane-4", "Hurricane-5")
pal <- colorRampPalette(c("blue", "green", "yellow", "orange", "red", "darkred", "black"))(length(ss))

storm$color <- as.character(factor(storm$status, levels = ss, labels = pal))

# lighten past position colors
storm$opacity <- 0.5
storm$opacity[strptime(storm$time, format="%m/%d/%H") <= Sys.time()] <- 0.1

# make windspeeds useful for point sizes
storm$wind_speed[storm$wind_speed == "-"] <- 0
storm$wind_speed <- as.integer(storm$wind_speed)
storm$wind_speed[is.na(storm$wind_speed)] <- 0

storm$time <- gsub("Z", "", storm$time)
storm$date <- strftime(strptime(storm$time, format="%m/%d/%H"), '%m/%d %Hh')

# separate complete and intermediate advisories (assuming they come in pairs - TODO)
#storm$adv <- gsub("A", "", storm$advisory)
#storm <- ddply(storm, "adv", head, 1)
#storm <- storm[order(storm$date),]

storm$time <- as.POSIXct(storm$time, format='%m/%d/%H', tz="UTC")
storm$localtime <- as.POSIXct(format(storm$time, tz=Sys.timezone(), usetz = TRUE))
storm$day <- paste(weekdays(storm$localtime, abbreviate = TRUE), format(storm$localtime, "%H:%M"), " ")

m <- # Create leaflet map.
  leaflet(data=storm, width=1024, height=768) %>%
  addTiles() %>%
  addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  ) %>%
  addPolylines(~lon, ~lat, color = 'grey', weight=3) %>%
  addCircleMarkers(~lon, ~lat, radius = ~wind_speed / 3, stroke = TRUE, color = 'grey',
                   opacity = 1, weight = 2, fillColor = ~color,
                   fillOpacity = ~opacity,
                   popup = ~sprintf("<b>Advisory forecast %s (%s)</b><hr noshade size='1'/>
                                    Local time: %s<br/>
                                    Position: %3.2f, %3.2f<br/>
                                    Strength: <strong>%s</strong><br/>
                                    Wind: %s (knots)<br/>Pressure: %s",
                                    htmlEscape(advisory), htmlEscape(date), htmlEscape(format(localtime, "%b %d %H:%M")),
                                    htmlEscape(lon), htmlEscape(lat),
                                    htmlEscape(status), htmlEscape(wind_speed), htmlEscape(pressure))
  ) %>%
  addLegend("bottomright", colors = pal, labels = ss)

html_print(m)

#saveWidget(m, '/tmp/trackr.html', selfcontained = FALSE)