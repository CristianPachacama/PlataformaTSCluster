#BUSQUEDA de Series (MAPA) -----------------
# PANEL DE FILTROS ---------
#Descarga de Series----
output$descargar_series_map <- downloadHandler(
  filename = function() {
    paste(input$nombre_archivo_map, ".csv", sep = "")
  },
  content = function(file) {
    ind_nom<-as.character(lista_series[['Nombre_SerieT']][input$tabla_busq_map_rows_all])
    ind_fch<-(as.character(BDD_unificada$Fecha)>=input$rng_fech_map[1])&(as.character(BDD_unificada$Fecha)<=input$rng_fech_map[2])
    write.csv(BDD_unificada[ind_fch,c('Fecha',ind_nom)], file, row.names = FALSE)
  },
  contentType = "text/csv"
)


# MAPA DE BÚSQUEDA --------
output$mapa_busq <- renderLeaflet({
  
  leaflet() %>% addTiles() %>%
    
    addAwesomeMarkers(lng=clima_dat$Longitud,lat=clima_dat$Latitud,group ='Clima',icon=icons_clim,popup=burbuja_clima)%>%
    addAwesomeMarkers(lng=vazoes_dat$Longitud,lat=vazoes_dat$Latitud,group ='Vazoes',icon=icons_vaz,popup=burbuja_vazoe)%>%
    
    #Circulos (Usinas): Rojo
    addCircles(lng = usinas_dat$Longitud, lat =usinas_dat$Latitud,group ='Usinas Principales',weight = 1,color=pal2(usinas_dat$Capacidad),
               radius =5*(usinas_dat$Capacidad)^1.2 , popup =burbuja_usina 
    ) %>%
    
    
    # RADIO al rededor de Estación Encontrado 
    addCircles(lng = usinas_dat$Longitud, lat =usinas_dat$Latitud, group = 'Radio',weight = 1, color = pal2(usinas_dat$Capacidad),
               radius = 5*(usinas_dat$Capacidad)^1.2 , popup = burbuja_usina 
    ) %>%
    
    #Agregar Controles
    addLayersControl(
      overlayGroups = c("Clima", "Vazoes",'Usinas Principales'),
      options = layersControlOptions(collapsed = FALSE)
    )%>%
    addLegend(position='bottomleft',values=usinas_dat$Capacidad,
              pal=pal2,title='Capacidad Usina:')%>%
    addMiniMap(position='bottomright',toggleDisplay = TRUE)
  
})


# TABLA DINÁMICA ---------
output$tabla_busq_map<-renderDataTable(server = FALSE,
                                       datatable(filter = 'top',
                                                 options = list(pageLength = 4, searchHighlight = TRUE),
                                                 {
                                                   #lista_series<-NA_resum(BDD_unificada)
                                                   lista_series[,c(-3,-4)]
                                                 })
)

#GRAFICO DE LA SERIE -----
serie_mini_map<-eventReactive(input$mini_graf_map_boton,{
  nombres<-input$tabla_busq_map_rows_selected+1
  aux<-xts(BDD_unificada[,nombres],order.by =as.Date(BDD_unificada[,1]))
  return(aux)
  
})

output$mini_graf_map <- renderDygraph({
  aux<-serie_mini_map()
  dygraph(aux)%>% 
    dyRangeSelector(dateWindow = c('2000-01-01','2017-06-25'))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})


