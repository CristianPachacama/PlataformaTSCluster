#REPORTE DATOS FALTANTES   ------------------------------------------
#Dinamizar los Inputs  ===========================
observe({
  
  #Input que condiciona el valor del resto
  tipo_sr<-input$tipo
  
  #Actualiza resto de inputs
  updateSelectInput(session,inputId = 'variable',
                    label = 'Variable',
                    selected = switch(tipo_sr,
                                      Clima='Insolacao',
                                      Contaminacion='MP10(Valor_Diario)',
                                      Vazoes='Vazoes',
                                      Indices='Indices',
                                      Manchas_Solares='Manchas_Solares'
                    ),
                    
                    choices=switch(tipo_sr,
                                   Clima={
                                     c('Evaporacao_Piche','Insolacao','Precipitacao_12H',
                                       'Temp_Comp_Media','TempMaxima','TempMinima_12H',
                                       'Umidade_Relativa_Media','Velocidade_do_Vento_Media')
                                   },
                                   Contaminacion={
                                     c('MP10(Valor_Diario)','MP2.5(Valor_Diario)','NO2(Valor_Diario)')
                                   },
                                   Vazoes={
                                     c('Vazoes')
                                   },
                                   Indices={
                                     list('Índices'='Indices')
                                   },
                                   Manchas_Solares={
                                     list('Manchas Solares'='Manchas_Solares')
                                   }
                    )
                    
  )
  
})

#Función que genera datos al pulsar boton
datos <- eventReactive(input$reporte_boton,{
  #load('ReporteNAs.RData')
  dat<-Informe_NAs(BDD_unificada,Tipo=input$tipo,
                   Variable = input$variable,
                   fecha_inicio = input$rango_fch_reprt[1],
                   fecha_fin =  input$rango_fch_reprt[2])
  return(dat)
})

#Generar Outputs -------
#Tabla del Reporte -----
output$tabla_reprt <- renderDataTable(server = FALSE,
                                      datatable(filter = 'top',
                                                options = list(pageLength=9,searchHighlight = TRUE),{
                                                  aux<-datos()
                                                  aux
                                                }))
#Minigrafico de NA's -----
output$graf_obs<- renderPlotly({
  aux<-datos()
  aux<-data.frame(x=aux[['Observaciones']],y=aux[['Porcentaje']],nomb=aux[['Nombre_SerieT']])
  p<-plot_ly(aux)%>%
    add_trace(x = ~x, y = ~y, type = 'scatter',text = ~nomb,mode='markers') %>%
    layout(xaxis = list(title = "Número de Observaciones"),
           yaxis = list(title = 'Porcentaje de NAs'))%>% 
    config(displayModeBar = F)
  p$elementId <- NULL
  p
})
#Histograma de NA's -----
output$hist <- renderPlotly({
  aux<-datos()
  aux<-data.frame(x=aux[['Porcentaje']])
  p <- plot_ly(aux)%>%
    add_trace(x = ~x, type = 'histogram',opacity=0.6 ) %>%
    layout(title = 'Histograma: Porcentaje de Datos Faltantes',
           xaxis = list(title = ' '),
           yaxis = list(title ="Frecuencia" ))%>% 
    config(displayModeBar = F) 
  p$elementId <- NULL
  p
  
})
#Diagrama de Caja NA's -----
output$box <- renderPlotly({
  aux<-datos()
  aux<-aux<-data.frame(x=aux[['Porcentaje']])
  p <- plot_ly(aux)%>%
    add_trace(x = ~x, type = 'box') %>%
    layout(xaxis = list(title = 'Porcentaje de NAs')#,yaxis = list(title = ' ')
    )%>% 
    config(displayModeBar = F) 
  p$elementId <- NULL
  p
})
#Descarga de Series Filtradas NAs----

output$descargar_reprt <- downloadHandler(
  filename = function() {
    paste(input$nombre_reprt, ".csv", sep = "")
  },
  content = function(file) {
    ind_nom<-as.character(lista_series[['Nombre_SerieT']][input$tabla_reprt_rows_all])
    datos_aux<-BDD_unificada[,c('Fecha',ind_nom)]
    ind_fch<-(as.character(datos_aux$Fecha)>=input$rango_fch_reprt[1])&(as.character(datos_aux$Fecha)<=input$rango_fch_reprt[2])
    datos_aux<-datos_aux[ind_fch,]
    write.csv(datos_aux, file, row.names = FALSE)
  },
  contentType = "text/csv"
)

