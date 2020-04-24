

# leaflet -----------------------------------------------------------------



cases_pulse <- makePulseIcon(
  color = colv, 
  iconSize = 100,
  animate = T,
  heartbeat = 0.05
)
pulse_options <- markerOptions(opacity=0.5, riseOnHover = F,interactive = T)

addPulseMarkers(
  lng=lonlat_matrix[1:3,1], lat=lonlat_matrix[1:3,2],
  icon = cases_pulse,
  label = cv_country[1:3],
  popup = popup_cases[1:3],
  labelOptions = text_label_opt,
  group = layer1,
  options = pulse_options)



# text output -------------------------------------------------------------
output$cases <- renderText(
  {input$cases} 
)
output$deaths <- renderText(
  {input$deaths}
)
output$cases_last_15_days <- renderText(
  {input$cases_last_15_days}
)


