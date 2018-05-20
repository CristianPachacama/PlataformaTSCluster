#VAZOES -------------
serie_vaz<-eventReactive(input$graf_vaz_boton,{
  nombre<-paste0('VAZOES(',as.character(input$vaz_estacion),')')
  aux<-xts(BDD_unificada[,nombre],order.by =as.Date(BDD_unificada[,1]))
  return(aux)
})
# output$texto_vaz<-renderText({
#   nombre<-paste0('VAZOES(',input$vaz_estacion,')')
#   nombre
# })
output$grafico_vazoes <- renderDygraph({
  aux<-serie_vaz()
  dygraph(aux, main = paste0("Vazoes:",input$vaz_estacion),group = 'vazoes')%>%
    dySeries("V1", label = 'Vazoes') %>%
    dyAxis('y',label='Flujo (metro3/s)')%>%
    dyRangeSelector(dateWindow = isolate(input$rango_fecha_vaz))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})


