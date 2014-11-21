###### GIS functions #####

# This functino takes data and plots and open street maps (stamen toner lite)
# map







plot_enroll_map <- function(.data, zoom_level=12, center) {
  # .data = data frame with lon, lat, and popover data
  # zoom_level = zoom level for map
  # center = data.frame with lat and long
  require(rCharts)
  
  # put student data into list format
  withProgress(message="Data", value=0.1, {
    incProgress(.2, detail = "Filtering")
    incProgress(.4, detail = "Reformatting")
    stus_list<-apply(.data, 1, as.list)
 
    #build map
    incProgress(.5, detail = "Building base map")
    enroll_map<-Leaflet$new()
    enroll_map$setView(c(center$lat, center$lon), zoom=zoom_level)
    enroll_map$tileLayer(provider = 'Stamen.TonerLite')
    
    incProgress(.5, detail = "Adding map layers")
    enroll_map$geoJson(
      toGeoJSON(stus_list, lat='lat', lon='lon'),
      onEachFeature = '#! function(feature, layer){
           layer.bindPopup(feature.properties.popup)
           } !#',
      pointToLayer =  "#! function(feature, latlng){
           return L.circleMarker(latlng, {
           radius: 5,
           fillColor: feature.properties.fillColor || 'red', 
           color: '#000',
           weight: 1,
           fillOpacity: 0.7
           })
           } !#"           
    )
    incProgress(.7, detail = "Setting map attributes")
    enroll_map$set(width = "100%", height = 900)
    enroll_map$fullScreen(TRUE)
    enroll_map$enablePopover(TRUE)
    enroll_map$set(dom = "map_container")
    incProgress(.7, detail = "Rending map")
    #enroll_map$show('inline', include_assets = TRUE)
    enroll_map
    
    
  })
  
}