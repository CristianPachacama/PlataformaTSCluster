#INDICES  -----------
serie_indice<-eventReactive(input$graf_ind_boton,{
  nombre<-paste0('INDICE-',input$nombre_indice)
  aux<-xts(BDD_unificada[,nombre],order.by =as.Date(BDD_unificada[,1]))
  return(aux)
})
serie_indice_grp<-eventReactive(input$graf_ind_boton,{
  nombres<-paste0('INDICE-',input$grupo_indices )
  aux<-xts(BDD_unificada[,nombres],order.by =as.Date(BDD_unificada[,1]))
  return(aux)
  
})

output$grafico_indice <- renderDygraph({
  aux<-serie_indice()
  dygraph(aux, main = paste0("Indice: ",input$nombre_indice),group = 'indice')%>% 
    dyRangeSelector(dateWindow = isolate(input$rango_fecha_indi))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})
output$grafico_grupo_indices <- renderDygraph({
  aux<-serie_indice_grp()
  dygraph(aux, main = "Grupo de Indices",group = 'indice')%>% 
    dyRangeSelector(dateWindow = isolate(input$rango_fecha_indi))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})


