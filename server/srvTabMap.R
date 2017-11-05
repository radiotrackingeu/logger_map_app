library(leaflet)
############ srvTabMap.R ############
#print(getwd())
#antennae_data<-read.csv2("R/LoggerMapApp/data/meta.csv", dec=".", stringsAsFactors = FALSE, row.names = NULL)

# creates leaflet map and adds polygons according to given meta data

create_logger_map<-function(data,meta,order_select=FALSE){
  m<- leaflet() %>% addTiles() %>% addCircles(lng=meta$Long,lat = meta$Lat)
  if(order_select){
    data<-subset(data,timestamp==timestamp[input$choose_single_data_set])
  }
  for(p in 1:dim(meta)[1]){
    if(any(data$receiver==meta$Receiver[p])){
      x<-meta$Long[p]
      y<-meta$Lat[p]
      if(order_select){
        m <- m %>% addLabelOnlyMarkers(lng=x,lat=y,label=as.character(data$timestamp),labelOptions = labelOptions(noHide = T, textOnly = T))
      }
      direction<-meta$Direction[p]
      bw<-meta$Beamwidth[p]
      len<-100*meta$Gain[p]
      wgs<-kegelcreation(x,y,direction,len,bw) # Many warnings...
      m<- m %>% addPolygons(lng=c(wgs$X,x),lat=c(wgs$Y,y),fillOpacity=0.4,stroke=FALSE,popup=paste0("test",br(),"test") )
    }
  }
  m
}

# creates leaflet map and adds polygons according to given meta data
create_antenna_map<-function(meta){
  m<- leaflet() %>% addTiles() %>% addCircles(lng=meta$Long,lat = meta$Lat)
  for(p in 1:dim(meta)[1]){
    x<-meta$Long[p]
    y<-meta$Lat[p]
    direction<-meta$Direction[p]
    wgs<-kegelcreation(x,y,direction,1000,60)
    m<- m %>% addPolygons(lng=c(wgs$X,x),lat=c(wgs$Y,y),fillOpacity=0.4,stroke=FALSE )
  }
  m
}

#create_antenna_map(antennae_data)
output$map_all_antenna <- renderLeaflet({
  validate(
    need(antennae_data(), "Please provide file with antennae specifications.")
  )
  create_antenna_map(antennae_data())
  })

output$logger_map <- renderLeaflet({
  validate(
    need(antennae_data(), "Please provide file with antennae specifications.")
  )
  create_logger_map(filtered_data(),antennae_data(),input$activate_single_data)
})

output$singal_select_prop<-renderText(
  if(input$activate_single_data){
    paste0("Signal Properties:",br(),
                "Date and Time: ", filtered_data()$timestamp[input$choose_single_data_set],br(),
                "Strength: ", filtered_data()$strength[input$choose_single_data_set],br(),
                "Length: ", filtered_data()$duration[input$choose_single_data_set],br(),
                "Bandwidth: ", filtered_data()$bw[input$choose_single_data_set],br(),
                "Frequency: ",filtered_data()$freq[input$choose_single_data_set]
                )
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