#Vazoes --------
vaz_dsst_dat<-eventReactive(input$boton_vaz_dsst,{
  nombre<-paste0('VAZOES(',as.character(input$dsst_vaz_estac),')')
  fecha_inicio<-as.character(lista_series[lista_series$Nombre_SerieT==nombre,'Fecha_inicio'])
  fecha_fin<-as.character(lista_series[lista_series$Nombre_SerieT==nombre,'Fecha_fin'])
  #restric<-as.character(BDD_unificada[,1])>=fecha_inicio
  restric<-(as.character(BDD_unificada[,1])>=fecha_inicio)&(as.character(BDD_unificada[,1])<=fecha_fin)
  
  datos<-BDD_unificada[restric,nombre]
  #Descomposicion
  fit<-stlplus(datos, n.p = 365, s.window = 365,s.degree = 2,
               l.window = 200, t.window =input$sldr_vaz, t.degree = 2 )
  
  aux<-fit$data
  
  datos_aux<-rep(NA,31588)
  estc_org<-rep(NA,31588)
  tnd_org<-rep(NA,31588)
  ruido<-rep(NA,31588)
  
  datos_aux[restric]<-datos;remove(datos)
  estc_org[restric]<-aux$seasonal
  tnd_org[restric]<-aux$trend
  ruido[restric]<-aux$remainder
  
  estc_fit<-rep(NA,31588)
  tnd_fit<-rep(NA,31588)
  #ruido_fit<-rep(NA,31588)
  
  estc_fit[c(restric[restric==F],is.na(aux$raw))]<-estc_org[c(restric[restric==F],is.na(aux$raw))]
  tnd_fit[c(restric[restric==F],is.na(aux$raw))]<-tnd_org[c(restric[restric==F],is.na(aux$raw))]
  #ruido_fit[c(restric[restric==F],is.na(aux$raw))]<-ruido_org[c(restric[restric==F],is.na(aux$raw))]
  
  estc_org[c(restric[restric==F],is.na(aux$raw))]<-NA
  tnd_org[c(restric[restric==F],is.na(aux$raw))]<-NA
  #ruido_org[c(restric[restric==F],is.na(aux$raw))]<-NA
  
  remove(aux)
  
  datos<-data.frame(Serie_Original=datos_aux,Estacionalidad=estc_org,
                    Estacionalidad_Ajustada=estc_fit,Tendencia=tnd_org,
                    Tendencia_Ajustada=tnd_fit,Ruido=ruido)
  
  datos<-xts(datos,order.by = as.Date(BDD_unificada[,1]))
  remove(datos_aux,estc_fit,estc_org,tnd_org,tnd_fit,ruido)
  return(datos)
})
#Serie  Original  ------------- 
output$vaz_dsst_org <- renderDygraph({
  aux<-vaz_dsst_dat()
  aux<-aux$Serie_Original
  dygraph(aux, main = "Serie Original",group = 'descomp')%>% 
    dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})
#TENDENCIA ----------
output$vaz_dsst_tnd <- renderDygraph({
  aux<-vaz_dsst_dat()
  aux<-aux[,c('Tendencia','Tendencia_Ajustada')]
  dygraph(aux, main = "Tendencia",group = 'descomp')%>% 
    dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})
#ESTACIONALIDAD ----------
output$vaz_dsst_stc <- renderDygraph({
  aux<-vaz_dsst_dat()
  aux<-aux[,c('Estacionalidad','Estacionalidad_Ajustada')]
  dygraph(aux, main = "Estacionalidad",group = 'descomp')%>% 
    dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})
#RUIDO ----------
output$vaz_dsst_ruid <- renderDygraph({
  aux<-vaz_dsst_dat()
  aux<-aux$Ruido
  dygraph(aux, main = "Ruido",group = 'descomp')%>% 
    dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})
#Analisis de Valores Extremos ------
#Actualizar Inputs EVA =======
observe({
  #estacion_selc<-input$dsst_vaz_estac
  updateSelectInput(session,inputId = 'ext_vaz_estac',
                    selected = input$dsst_vaz_estac)
  
  updateSliderInput(session,inputId = 'sldr_vaz_ext',
                    value = input$sldr_vaz)
  
  updateDateRangeInput(session,inputId = 'rngo_fcha_ext',
                       start = input$rngo_fcha_dsst[1],
                       end = input$rngo_fcha_dsst[2])
  updateNumericInput(session,inputId = 'num_period_vaz_ext',
                     value=input$num_period_vaz)
})

#Funciones para generar datos ------
vaz_dsst_dat2<-eventReactive(input$boton_vaz_ext,{
  nombre<-paste0('VAZOES(',as.character(input$ext_vaz_estac),')')
  fecha_inicio<-as.character(lista_series[lista_series$Nombre_SerieT==nombre,'Fecha_inicio'])
  fecha_fin<-as.character(lista_series[lista_series$Nombre_SerieT==nombre,'Fecha_fin'])
  
  restric<-(as.character(BDD_unificada[,1])>=fecha_inicio)&(as.character(BDD_unificada[,1])<=fecha_fin)
  #restric<-as.character(BDD_unificada[,1])>=fecha_inicio
  
  datos<-BDD_unificada[restric,nombre]
  #Descomposicion
  fit<-stlplus(datos, n.p = 365, s.window = 365,s.degree = 2,
               l.window = 200, t.window =input$sldr_vaz_ext, t.degree = 2 )
  
  aux<-fit$data
  
  # datos_aux<-rep(NA,31588)
  # estc_org<-rep(NA,31588)
  # tnd_org<-rep(NA,31588)
  ruido<-rep(NA,31588)
  
  #datos_aux[restric]<-datos;
  remove(datos)
  # estc_org[restric]<-aux$seasonal
  # tnd_org[restric]<-aux$trend
  ruido[restric]<-aux$remainder
  
  # estc_fit<-rep(NA,31588)
  # tnd_fit<-rep(NA,31588)
  
  
  # estc_fit[c(restric[restric==F],is.na(aux$raw))]<-estc_org[c(restric[restric==F],is.na(aux$raw))]
  # tnd_fit[c(restric[restric==F],is.na(aux$raw))]<-tnd_org[c(restric[restric==F],is.na(aux$raw))]
  
  # estc_org[c(restric[restric==F],is.na(aux$raw))]<-NA
  # tnd_org[c(restric[restric==F],is.na(aux$raw))]<-NA
  
  remove(aux)
  
  datos<-data.frame(Ruido=ruido)
  
  datos<-xts(datos,order.by = as.Date(BDD_unificada[,1]))
  remove(ruido)
  return(datos)
})
#Ruido ------
output$vaz_ext_ruid <- renderDygraph({
  aux<-vaz_dsst_dat2()
  aux<-aux$Ruido
  dygraph(aux, main = "Ruido",group = 'descomp')%>% 
    dyRangeSelector(dateWindow = isolate(input$rngo_fcha_ext))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})
#EVAs Fevd() 
vaz_fevd_dat<-eventReactive(input$boton_vaz_ext,{
  nombre<-paste0('VAZOES(',as.character(input$dsst_vaz_estac),')')
  
  fecha_inicio<-as.character(lista_series[lista_series$Nombre_SerieT==nombre,'Fecha_inicio'])
  fecha_fin<-as.character(lista_series[lista_series$Nombre_SerieT==nombre,'Fecha_fin'])
  
  restric<-(as.character(BDD_unificada[,1])>=fecha_inicio)&(as.character(BDD_unificada[,1])<=fecha_fin)
  
  datos<-BDD_unificada[restric,nombre]
  #Descomposicion
  fit<-stlplus(datos, n.p = 365, s.window = 365,s.degree = 2,
               l.window = 200, t.window =input$sldr_vaz, t.degree = 2 )
  
  aux<-fit$data
  ruido<-aux$remainder
  
  remove(aux,fit)
  
  aux<-data.frame(Fecha=BDD_unificada[restric,1],Ruido=ruido)
  #Restringir rango de fechas para facilitar analisis
  rest_fch<-(as.character(aux$Fecha)>=isolate(input$rngo_fcha_ext[1]))&(as.character(aux$Fecha)<=isolate(input$rngo_fcha_ext[2]))
  aux<-aux[rest_fch,]
  fit1<-fevd(Ruido, aux,method = isolate(input$ext_vaz_metod),type = isolate(input$ext_vaz_tipo))
  
  return(fit1)
})
#Histograma Ruido -----
output$vaz_ext_histgrm<-renderPlotly({
  aux<-vaz_dsst_dat2()
  aux<-data.frame(x=as.numeric(aux$Ruido))
  p <- plot_ly(aux)%>%
    add_trace(x = ~x, type = 'histogram',opacity=0.6 ) %>%
    layout(title = 'Histograma del Ruido',
           xaxis = list(title = ' '),
           yaxis = list(title ="Frecuencia" ))%>% 
    config(displayModeBar = F) 
  p$elementId <- NULL
  p
  
})
#Grafico Resumen EVA Fevd ------
output$vaz_ext_fevd<-renderPlot({
  fit1<-vaz_fevd_dat()
  plot(fit1)
  mtext('Extreme Values Analysis(EVA) del Ruido', outer = TRUE, cex = 1.5,line=0.5)
})
