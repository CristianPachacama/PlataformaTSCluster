#GENERAR ARCHIVO con SERIES -----------------------------
output$tabla_busqueda<-renderDataTable(server = FALSE,
                                       datatable(filter = 'top',
                                                 options = list(pageLength=9,searchHighlight = TRUE),
                                                 {
                                                   #lista_series<-NA_resum(BDD_unificada)
                                                   lista_series[,c(-3,-4)]
                                                 }))
#Descarga de Series----

output$descargar_series <- downloadHandler(
  filename = function() {
    paste(input$nombre_archivo, ".csv", sep = "")
  },
  content = function(file) {
    ind_nom<-as.character(lista_series[['Nombre_SerieT']][input$tabla_busqueda_rows_all])
    ind_fch<-(as.character(BDD_unificada$Fecha)>=input$rng_fech_generar[1])&(as.character(BDD_unificada$Fecha)<=input$rng_fech_generar[2])
    write.csv(BDD_unificada[ind_fch,c('Fecha',ind_nom)], file, row.names = FALSE)
  },
  contentType = "text/csv"
)

#GRAFICO DE LA SERIE----
serie_mini<-eventReactive(input$mini_graf_boton,{
  nombres<-input$tabla_busqueda_rows_selected+1
  aux<-xts(BDD_unificada[,nombres],order.by =as.Date(BDD_unificada[,1]))
  return(aux)
  
})

output$mini_graf <- renderDygraph({
  aux<-serie_mini()
  dygraph(aux)%>% 
    dyRangeSelector(dateWindow = c('2000-01-01','2017-06-25'))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})

