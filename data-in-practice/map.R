#where I come from
#url = "https://raw.githubusercontent.com/codeforgermany/click_that_hood/main/public/data/brazil-states.geojson"

library(leaflet)

get_geojson_map = function(data){
  
  #geojson = geojsonio::geojson_read("data-in-practice/brazil_states.geojson")
  
  customIcon = makeIcon(
    iconUrl = "palm-trees.png",
    iconWidth = 100, iconHeight = 100,
    iconAnchorX = 22, iconAnchorY = 20
  )
  
  
  m = leaflet(data) %>% 
  addTiles() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addGeoJSON(data, weight = 1, color = "purple") %>%
  setView(lng = -51.92528, lat =  -14.235004, zoom = 3) %>% 
  addMarkers(-43.196388,  -22.908333, popup= "My Hometown")
  
  
  return(m)
}

