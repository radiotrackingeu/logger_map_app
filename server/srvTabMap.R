############ srvTabMap.R ############

observe({
  if (!(is.null(antennae_data())||
        is.null(filtered_data())))
    js$enableTab("Map")
  else
    js$disableTab("Map")
})

# render map
output$logger_map <- renderLeaflet({
  validate(
    need(antennae_data(), "Please provide file with antennae specifications.")
  )
  map() %>% addAntennaePositions() %>% 
    addAntennaeCones() %>% 
    addLegend(position="topleft",   
              pal=color_palette(),
              values=filtered_data()$max_signal,
              title="SNR"
              )
})



# render data info text 
output$singal_select_prop<-renderText(
  if(input$activate_single_data){
    paste0("Date and Time: ", filtered_data()$timestamp[input$choose_single_data_set])
  }
)

miniplot_base<-reactive({
  if(input$activate_single_data){
    ggplot(filtered_data())+geom_point(aes(timestamp,max_signal),color="blue")
  }
})

output$miniplot<-renderPlot({
  if(input$activate_single_data){
    miniplot_base() + geom_point(aes(timestamp[input$choose_single_data_set],max_signal[input$choose_single_data_set]),color="red")
    }
})


observeEvent(input$show_antennae_outline, {
  if(input$show_antennae_outline)
    leafletProxy("logger_map") %>% showGroup("antennae_cones")
  else 
    leafletProxy("logger_map") %>% hideGroup("antennae_cones")
})

color_palette <- reactive({
  pal <- colorNumeric(
    palette = "Reds",
    domain = filtered_data()$max_signal,
    reverse = FALSE)
  pal
})

observe({
  validate(
    need(antennae_data(), "Please provide file with antennae specifications."),
    need(logger_data(), "Please have a look at the filter settings.")
  )

  leafletProxy("logger_map") %>% clearGroup("bats") %>% clearPopups() %>% clearMarkers()
  if(input$activate_single_data){
    leafletProxy("logger_map") %>% addDetectionCones()
  }
})

# creates basic map
map <- reactive({
  leaflet() %>% addProviderTiles(providers[[input$choose_map]]) %>% addMeasure(position = "bottomleft", 
                                                                               primaryLengthUnit = "meters",  
                                                                               primaryAreaUnit = "sqmeters",
                                                                               activeColor = "blue",
                                                                               completedColor = "red") %>% addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% addScaleBar(position="bottomright")
})

# adds circles at antennae positions on given map.
addAntennaePositions<-function(m) {
  m %>% addCircles(group="antennae_positions", lng=antennae_data()$pos_x,lat = antennae_data()$pos_y)
}

# adds the cone outline to antennae on given map.
addAntennaeCones<- function(m) {
  for(name in names(antennae_cones())) {
   m<-m %>% addPolygons(group="antennae_cones", lng=antennae_cones()[[name]]$x, lat=antennae_cones()[[name]]$y, fill=FALSE, opacity=0.5, stroke=TRUE, weight=1)
  }
  return(m)
}

addDetectionCones<-function(m) {
  data<-subset(filtered_data(),timestamp==timestamp[input$choose_single_data_set])
  if(nrow(data)==0) return(NULL)
  validate(
    need(data, "Please have a look at the filter settings.")
  )
  for(p in 1:nrow(data)){
    if (!(data$receiver[p]%in%antennae_data()[,1])) {
      next
    }
    a<-antennae_cones()[[data$receiver[p]]]
    label_kegel <- paste0("Signal Properties:",br(),
                          "Receiver: ",data$receiver[p], br(),
                          "Date and Time: ", data$time[p],br(),
                          "Strength: ", data$max_signal[p],br(),
                          "Length: ", data$duration[p],br(),
                          "Bandwidth: ", data$signal_bw[p],br(),
                          "Frequency: ",data$signal_freq[p]
    )
    m<-m %>% addPolygons(lng=a$x,lat=a$y,fillColor = color_palette()(data$max_signal[p]),fillOpacity=0.8,stroke=FALSE,popup=label_kegel, group="bats")
  }
  return(m)
}


# calculates cone shapes
antennae_cones<-reactive({
  cones=NULL
  if (!is.null(antennae_data())) {
    for(a in 1:nrow(antennae_data())){
      antennae<-antennae_data()[a,]
      x<-antennae$pos_x
      y<-antennae$pos_y
      direction<-antennae$orientation
      bw<-antennae$beam_width
      len<-100*antennae$gain
      wgs<-kegelcreation(x,y,direction,len,bw) # Many warnings...
      cones[[antennae$receiver]]<-list(x=c(wgs$X,x), y=c(wgs$Y,y))
    }
  }
  cones
})

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