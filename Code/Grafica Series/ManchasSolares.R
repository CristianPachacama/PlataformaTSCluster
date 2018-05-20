#MANCHAS SOLARES  -------
serie_manchas<-eventReactive(input$graf_manchas_boton,{
  nombre<-input$nomb_manchas
  aux<-xts(BDD_unificada[,nombre],order.by =as.Date(BDD_unificada[,1]))
  return(aux)
})
output$grafico_manchas <- renderDygraph({
  aux<-serie_manchas()
  dygraph(aux, main = "Serie Especifica",group = 'serie')%>% 
    dyRangeSelector(dateWindow = isolate(input$rango_fecha_serie))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})


