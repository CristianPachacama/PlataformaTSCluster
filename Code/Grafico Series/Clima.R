#CLIMA  -------------
serie_clim<-eventReactive(input$graf_clim_boton,{
  estac<-input$clim_estacion
  estac<-substr(estac,1,nchar(estac)-7)
  nombre<-paste0('CLIMA(',estac,')-',input$clim_variable)
  aux<-xts(BDD_unificada[,nombre],order.by =as.Date(BDD_unificada[,1]))
  return(aux)
})
output$grafico_clima <- renderDygraph({
  aux<-serie_clim()
  dygraph(aux, main = "Clima",group = 'clima')%>% 
    dySeries("V1", label = input$clim_variable) %>%
    dyRangeSelector(dateWindow = isolate(input$rango_fecha_clim))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})



