#SERIE ESPECIFICADA POR NOMBRE  -------
serie_nombre<-eventReactive(input$graf_seri_boton,{
  nombre<-input$nomb_serie
  aux<-xts(BDD_unificada[,nombre],order.by =as.Date(BDD_unificada[,1]))
  return(aux)
})
output$grafico_serie <- renderDygraph({
  aux<-serie_nombre()
  dygraph(aux, main = "Serie Especifica",group = 'serie')%>% 
    dyRangeSelector(dateWindow = isolate(input$rango_fecha_serie))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})


