#CONTAMINACION ------
#Dinamizar Inputs: Filtrar Estacion segun Variable ==========
observe({
  #Input que condiciona: Variable de Contaminacion
  var_contam<-input$contam_variable
  
  #Actualizar Inputs: Seleccion de Estacion (Disponible)
  updateSelectInput(session,inputId = 'contam_estacion',
                    choices = switch(var_contam,
                                     'MP10'={
                                       ind<-contam_dat[['Variable']]=='MP10'
                                       as.character(contam_dat[['Estacion']][ind])
                                     },
                                     'MP2.5'={
                                       ind<-contam_dat[['Variable']]=='MP2.5'
                                       as.character(contam_dat[['Estacion']][ind])
                                     },
                                     'NO2'={
                                       ind<-contam_dat[['Variable']]=='NO2'
                                       as.character(contam_dat[['Estacion']][ind])
                                     }
                                     
                    )
                    
  )
  
})

#Generar Outputs -----
serie_contam<-eventReactive(input$graf_contam_boton,{
  nombre<-paste0('CONTAM(',input$contam_estacion,')-',input$contam_variable,'(Valor_Diario)')
  aux<-xts(BDD_unificada[,nombre],order.by =as.Date(BDD_unificada[,1]))
  return(aux)
})

output$grafico_contam <- renderDygraph({
  aux<-serie_contam()
  dygraph(aux, main = paste0("Contaminacion:",input$contam_estacion),group = 'contam')%>%
    dySeries("V1", label = input$contam_variable) %>%
    dyRangeSelector(dateWindow = isolate(input$rango_fecha_contam))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})

