############ srvTabMap.R ############

#global variable for leaflet ma
map <- NULL

antennae_cones<-reactive({
  if (is.null(antennae_data())) return(NULL)
  cones=NULL
  
  for(a in 1:nrow(antennae_data())){
    antennae<-antennae_data()[a,]
    x<-antennae$Long
    y<-antennae$Lat
    direction<-antennae$Direction
    bw<-antennae$Beamwidth
    len<-100*antennae$Gain
    wgs<-kegelcreation(x,y,direction,len,bw) # Many warnings...
    label_kegel <- ""
    cones[[antennae$Receiver]]<-list(x=c(wgs$X,x), y=c(wgs$Y,y), label=label_kegel)
    # cones<-list(cones, list(c(wgs$X,x), c(wgs$y,y), label_kegel))
  # leafletProxy("logger_map") %>% addPolygons(lng=c(wgs$X,x),lat=c(wgs$Y,y),fillOpacity=0.0, opacity = 1.0, color="#ff0000",stroke=FALSE)
  }
  cones
})

observeEvent(input$show_antennae_outline, {
  if (isTRUE(input$show_antennae_outline))   
    for(name in names(antennae_cones())) {
    # print(antennae_cones()[[name]]$x)
    leafletProxy("logger_map") %>% addPolygons(group="antennae_outline", lng=antennae_cones()[[name]]$x, lat=antennae_cones()[[name]]$y, fill=FALSE, opacity=0.5, stroke=TRUE, weight=1)
    }
  else {leafletProxy("logger_map") %>% clearGroup("antennae_outline")}
  updateActionButton(session,"dl","Foo")
})

color_palette <- reactive({
  mean_strength<-mean(filtered_data()$strength)
  print(mean_strength)
  std_strength<-sd(filtered_data()$strength)
  print(std_strength)
  range_strength<-abs(c(mean_strength-1.2*std_strength,mean_strength+1.2*std_strength))
  pal <- colorNumeric(
    palette = "Blues",
    domain = range_strength,
    reverse =TRUE)
  pal
})

observe({
  validate(
    need(antennae_data(), "Please provide file with antennae specifications.")#,
  )
  validate(
    need(logger_data(), "Please have a look at the filter settings.")#,
  )
  
  leafletProxy("logger_map") %>% clearGroup("bats") %>% clearPopups() %>% clearMarkers() %>% addCircles(lng=antennae_data()$Long,lat = antennae_data()$Lat)
  if(input$activate_single_data){
    data<-subset(filtered_data(),timestamp==timestamp[input$choose_single_data_set])
    for(p in 1:nrow(data)){
      meta <-subset(antennae_data(),Receiver==data$receiver[p])
      x<-meta$Long
      y<-meta$Lat
      direction<-meta$Direction
      bw<-meta$Beamwidth
      len<-100*meta$Gain
      wgs<-kegelcreation(x,y,direction,len,bw) # Many warnings...
      label_kegel <- paste0("Signal Properties:",br(),
                            "Receiver: ",data$receiver[p], br(),
                            "Date and Time: ", data$time[p],br(),
                            "Strength: ", data$strength[p],br(),
                            "Length: ", data$duration[p],br(),
                            "Bandwidth: ", data$bw[p],br(),
                            "Frequency: ",data$freq[p]
                            )
      leafletProxy("logger_map") %>% addPolygons(lng=c(wgs$X,x),lat=c(wgs$Y,y),fillColor = color_palette()(abs(data$strength[p])),fillOpacity=0.4,stroke=FALSE,popup=label_kegel, group="bats")
      }
    }
})

output$logger_map <- renderLeaflet({
  validate(
    need(antennae_data(), "Please provide file with antennae specifications.")
  )
  map<-leaflet() %>% addTiles() %>% addCircles( lng=antennae_data()$Long,lat = antennae_data()$Lat)
  for(name in names(antennae_cones())) {
   map<-map %>% addPolygons(group="antennae_outline", lng=antennae_cones()[[name]]$x, lat=antennae_cones()[[name]]$y, fill=FALSE, opacity=0.5, stroke=TRUE, weight=1)
  }
  map
})

output$singal_select_prop<-renderText(
  if(input$activate_single_data){
    paste0("Date and Time: ", filtered_data()$timestamp[input$choose_single_data_set])
  }
)

# calculates corner coordinates of antenna reception area
kegelcreation<-function(x,y,dir,length,deg){
  # first convert to utm
  utm<-wgstoutm(x,y)
  
  # then calc new coords
  
  kr <- (dir-deg/2)/180*pi
  kl <- (dir+deg/2)/180*pi
  
  utm_x1 <- utm$X + sin(kr) * length
  utm_y1 <- utm$Y + cos(kr) * length
  utm_zone1 <- utm$zone # if the zones switch, this setting needs to be changed 
  
  utm_x2 <- utm$X + sin(kl) * length
  utm_y2 <- utm$Y + cos(kl) * length
  utm_zone2 <- utm$zone # if the zones switch, this setting needs to be changed 
  
  # convert back to wgs
  wgs<-utmtowgs(c(utm_x1,utm_x2),c(utm_y1,utm_y2),c(utm_zone1,utm_zone2))
  return(wgs)
}

# WGS to UTM conversion
wgstoutm<-function(x,y){
  zone<-(floor((x + 180)/6) %% 60) + 1
  #xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  xy <- data.frame(cbind("X"=x,"Y"=y))
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(cbind.data.frame(X=res$X,Y=res$Y,zone))
}

# UTM to WGS conversion
utmtowgs<-function(x,y,zone){
  #xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  xy <- data.frame(cbind("X"=x,"Y"=y))
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS(paste0("+proj=utm +zone=",zone," +datum=WGS84"))  ## for example
  res <- spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
  return(as.data.frame(res))
}