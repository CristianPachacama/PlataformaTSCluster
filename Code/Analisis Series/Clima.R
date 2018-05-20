#Clima ------------------------------
clm_dsst_dat<-eventReactive(input$boton_clm_dsst,{
  estac<-input$dsst_clm_estac
  estac<-substr(estac,1,nchar(estac)-7)
  nombre<-paste0('CLIMA(',estac,')-',input$dsst_clm_var)
  #nombre<-paste0('CLIMA(',as.character(input$dsst_clm_estac),')')
  fecha_inicio<-as.character(lista_series[lista_series$Nombre_SerieT==nombre,'Fecha_inicio'])
  restric<-as.character(BDD_unificada[,1])>=fecha_inicio
  
  datos<-BDD_unificada[restric,nombre]
  #Descomposicion
  fit<-stlplus(datos, n.p = 365, s.window = 365,s.degree = 2,
               l.window = 200, t.window = input$sldr_clm, t.degree = 2 )
  
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
#Serie Original -----
output$clm_dsst_org <- renderDygraph({
  aux<-clm_dsst_dat()
  aux<-aux$Serie_Original
  dygraph(aux, main = "Serie Original",group = 'descomp')%>% 
    dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})

#TENDENCIA -------
output$clm_dsst_tnd <- renderDygraph({
  aux<-clm_dsst_dat()
  aux<-aux[,c('Tendencia','Tendencia_Ajustada')]
  dygraph(aux, main = "Tendencia",group = 'descomp')%>% 
    dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})

#ESTACIONALIDAD -------
output$clm_dsst_stc <- renderDygraph({
  aux<-clm_dsst_dat()
  aux<-aux[,c('Estacionalidad','Estacionalidad_Ajustada')]
  dygraph(aux, main = "Estacionalidad",group = 'descomp')%>% 
    dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})

#RUIDO ---------
output$clm_dsst_ruid <- renderDygraph({
  aux<-clm_dsst_dat()
  aux<-aux$Ruido
  dygraph(aux, main = "Ruido",group = 'descomp')%>% 
    dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(width = 400)
})

