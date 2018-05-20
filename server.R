#Descarga de Paquetes ================================
# (Solo compilar esta sección la primera vez)
# install.packages('shinythemes',dependencies = T)
# install.packages('shinydashboard',dependencies = T)
# install.packages('markdown',dependencies = T)
# install.packages('leaflet',dependencies = T)
# install.packages('htmltools',dependencies = T)
# install.packages('rgdal',dependencies = T)
# install.packages('DT',dependencies = T)
# install.packages('plotly',dependencies = T)
# install.packages('ggplot2',dependencies = T)
# install.packages('dygraphs',dependencies = T)
# install.packages('seasonal',dependencies = T)
# install.packages('stlplus',dependencies = T)

#### install.packages('xts',dependencies = T) ####
#Correccion de Version en paquete 'xts' ===============
# install.packages('devtools', dependencies = T)
# require(devtools)
# install_version("xts", version = "0.9-7", repos = "http://cran.us.r-project.org")

#Carga de Paquetes ===================================

library(shiny)
#Mapas
library(leaflet)
library(htmltools)
library(rgdal)
#Datos Faltantes
library(DT)
#Grafica Series
library(plotly)
library(ggplot2)
library(dygraphs)
library(xts)
#Descomposicion
library(seasonal)
library(stlplus) #Missing Values
#Valores Extremos
library(extRemes)
#Mutivariante
library(smacof)
library(cluster)
#Datos Extras
load('datos_interfaz.RData')
load('Vazoes_Cluster.RData')
# Actualización de Datos Oct 2017----------------------------------
# save(burbuja_clima,burbuja_vazoe,burbuja_usina,
#      BDD_unificada,clima_dat,
#      contam_dat,lista_series,resum_NAs,usinas_dat,
#      vazoes_dat,icons_clim,icons_vaz,getColor,
#      Informe_NAs,NA_resum,pal,pal2,server,ui,interfaz,
#      file='datos_interfaz.RData')

# Define server ====================================================
shinyServer(function(input, output, session) {

  #DATOS  ---------------------------------------------------
    #DESCRIPCIÓN DE DATOS --------------
  output$tabla_muestra <- renderDataTable(datatable(options = list(pageLength=9),{
    series<-c(1,2,152,165,2300,2383)
    restric<-as.character(BDD_unificada[,1])>='2000-01-01'
    aux<-BDD_unificada[restric,series]
    aux
  }))
    # MAPAS ---------------------------------------------------------------------
    # load('Mapa.RData')
  output$mymap <- renderLeaflet({
    
    leaflet() %>% addTiles() %>%
      
      #Marcadores: Clima=Azul,Vazoes=Naranja
      # addAwesomeMarkers(lng=clima_dat$Longitud,lat=clima_dat$Latitud,group ='Clima',icon=icons_clim,label=clima_dat$Estacion,popup=htmlEscape(clima_dat$Estacion))%>%
      # addAwesomeMarkers(lng=vazoes_dat$Longitud,lat=vazoes_dat$Latitud,group ='Vazoes',icon=icons_vaz,label=vazoes_dat$Estacion,popup=htmlEscape(vazoes_dat$Estacion))%>%
      
      addAwesomeMarkers(lng=clima_dat$Longitud,lat=clima_dat$Latitud,group ='Clima',icon=icons_clim,popup=burbuja_clima)%>%
      addAwesomeMarkers(lng=vazoes_dat$Longitud,lat=vazoes_dat$Latitud,group ='Vazoes',icon=icons_vaz,popup=burbuja_vazoe)%>%
      
      #Circulos (Usinas): Rojo
      addCircles(lng = usinas_dat$Longitud, lat =usinas_dat$Latitud,group ='Usinas Principales',weight = 1,color=pal2(usinas_dat$Capacidad),
                 radius =5*(usinas_dat$Capacidad)^1.2 , popup =burbuja_usina 
      ) %>%
      #Agregar Controles
      addLayersControl(
        overlayGroups = c("Clima", "Vazoes",'Usinas Principales'),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      addLegend(position='bottomleft',values=usinas_dat$Capacidad,
                pal=pal2,title='Capacidad Usina:')%>%
      addMiniMap(position='bottomright',toggleDisplay = TRUE)
    
  })
    #BUSQUEDA de Series (MAPA) -----------------
      # PANEL DE FILTROS ---------
            #Descarga de Series----
  output$descargar_series_map <- downloadHandler(
    filename = function() {
      paste(input$nombre_archivo_map, ".csv", sep = "")
    },
    content = function(file) {
      ind_nom<-as.character(lista_series[['Nombre_SerieT']][input$tabla_busq_map_rows_all])
      ind_fch<-(as.character(BDD_unificada$Fecha)>=input$rng_fech_map[1])&(as.character(BDD_unificada$Fecha)<=input$rng_fech_map[2])
      write.csv(BDD_unificada[ind_fch,c('Fecha',ind_nom)], file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
      
      # MAPA DE BÚSQUEDA --------
  output$mapa_busq <- renderLeaflet({
    
    leaflet() %>% addTiles() %>%
      
      addAwesomeMarkers(lng=clima_dat$Longitud,lat=clima_dat$Latitud,group ='Clima',icon=icons_clim,popup=burbuja_clima)%>%
      addAwesomeMarkers(lng=vazoes_dat$Longitud,lat=vazoes_dat$Latitud,group ='Vazoes',icon=icons_vaz,popup=burbuja_vazoe)%>%
       
      #Circulos (Usinas): Rojo
      addCircles(lng = usinas_dat$Longitud, lat =usinas_dat$Latitud,group ='Usinas Principales',weight = 1,color=pal2(usinas_dat$Capacidad),
                 radius =5*(usinas_dat$Capacidad)^1.2 , popup =burbuja_usina 
      ) %>%
      
      
      # RADIO al rededor de Estación Encontrado 
      addCircles(lng = usinas_dat$Longitud, lat =usinas_dat$Latitud, group = 'Radio',weight = 1, color = pal2(usinas_dat$Capacidad),
                 radius = 5*(usinas_dat$Capacidad)^1.2 , popup = burbuja_usina 
      ) %>%
      
      #Agregar Controles
      addLayersControl(
        overlayGroups = c("Clima", "Vazoes",'Usinas Principales'),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      addLegend(position='bottomleft',values=usinas_dat$Capacidad,
                pal=pal2,title='Capacidad Usina:')%>%
      addMiniMap(position='bottomright',toggleDisplay = TRUE)
    
  })
  
  
      # TABLA DINÁMICA ---------
  output$tabla_busq_map<-renderDataTable(server = FALSE,
                                         datatable(filter = 'top',
                                                   options = list(pageLength = 4, searchHighlight = TRUE),
                                                   {
                                                     #lista_series<-NA_resum(BDD_unificada)
                                                     lista_series[,c(-3,-4)]
                                                   })
                                         )
 
      #GRAFICO DE LA SERIE -----
  serie_mini_map<-eventReactive(input$mini_graf_map_boton,{
    nombres<-input$tabla_busq_map_rows_selected+1
    aux<-xts(BDD_unificada[,nombres],order.by =as.Date(BDD_unificada[,1]))
    return(aux)
    
  })
  
  output$mini_graf_map <- renderDygraph({
    aux<-serie_mini_map()
    dygraph(aux)%>% 
      dyRangeSelector(dateWindow = c('2000-01-01','2017-06-25'))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
  

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
  
  #GRAFICO DE SERIES --------------------------------------------------
  
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
  
  
  #ANALISIS DE SERIES DE TIEMPO -----------------------------------------------
   #Descomposición STL -------------
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
  
  
    #Contaminacion -------------------
  cnt_dsst_dat<-eventReactive(input$boton_cnt_dsst,{
    nombre<-paste0('CONTAM(',input$dsst_cnt_estac,')-',input$dsst_cnt_var,'(Valor_Diario)')
    
    fecha_inicio<-as.character(lista_series[lista_series$Nombre_SerieT==nombre,'Fecha_inicio'])
    restric<-as.character(BDD_unificada[,1])>=fecha_inicio
    
    datos<-BDD_unificada[restric,nombre]
    #Descomposicion
    fit<-stlplus(datos, n.p = 365, s.window = 365,s.degree = 2,
                 l.window = 200, t.window = input$sldr_cnt, t.degree = 2 )
    
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
  
        #Dinamizar Inputs: Filtrar Estacion segun Variable ==========
  observe({
    #Input que condiciona: Variable de Contaminacion
    var_contam<-input$dsst_cnt_var
    
    #Actualizar Inputs: Seleccion de Estacion (Disponible)
    updateSelectInput(session,inputId = 'dsst_cnt_estac',
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
        #Serie Original --------
  output$cnt_dsst_org <- renderDygraph({
    aux<-cnt_dsst_dat()
    aux<-aux$Serie_Original
    dygraph(aux, main = "Serie Original",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
  
        #TENDENCIA --------
  output$cnt_dsst_tnd <- renderDygraph({
    aux<-cnt_dsst_dat()
    aux<-aux[,c('Tendencia','Tendencia_Ajustada')]
    dygraph(aux, main = "Tendencia",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
  
        #ESTACIONALIDAD -------
  output$cnt_dsst_stc <- renderDygraph({
    aux<-cnt_dsst_dat()
    aux<-aux[,c('Estacionalidad','Estacionalidad_Ajustada')]
    dygraph(aux, main = "Estacionalidad",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
  
        #RUIDO --------
  output$cnt_dsst_ruid <- renderDygraph({
    aux<-cnt_dsst_dat()
    aux<-aux$Ruido
    dygraph(aux, main = "Ruido",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
    
    #Indices -------------------------
  ind_dsst_dat<-eventReactive(input$boton_ind_dsst,{
    nombre<-paste0('INDICE-',input$dsst_ind_var)
    
    fecha_inicio<-as.character(lista_series[lista_series$Nombre_SerieT==nombre,'Fecha_inicio'])
    restric<-as.character(BDD_unificada[,1])>=fecha_inicio
    
    datos<-BDD_unificada[restric,nombre]
    #Descomposicion
    fit<-stlplus(datos, n.p = input$num_period_ind, s.window = 365,s.degree = 2,
                 l.window = 200, t.window = input$sldr_ind, t.degree = 2 )
    
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
  output$ind_dsst_org <- renderDygraph({
    aux<-ind_dsst_dat()
    aux<-aux$Serie_Original
    dygraph(aux, main = "Serie Original",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
  
        #TENDENCIA ------
  output$ind_dsst_tnd <- renderDygraph({
    aux<-ind_dsst_dat()
    aux<-aux[,c('Tendencia','Tendencia_Ajustada')]
    dygraph(aux, main = "Tendencia",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
  
        #ESTACIONALIDAD ------
  output$ind_dsst_stc <- renderDygraph({
    aux<-ind_dsst_dat()
    aux<-aux[,c('Estacionalidad','Estacionalidad_Ajustada')]
    dygraph(aux, main = "Estacionalidad",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
  
  
        #RUIDO --------
  output$ind_dsst_ruid <- renderDygraph({
    aux<-ind_dsst_dat()
    aux<-aux$Ruido
    dygraph(aux, main = "Ruido",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
  
  
    #Manchas Solares ------------------
  mch_dsst_dat<-eventReactive(input$boton_manchas_dsst,{
    nombre<-'Manchas_Solares'
    
    fecha_inicio<-as.character(lista_series[lista_series$Nombre_SerieT==nombre,'Fecha_inicio'])
    restric<-as.character(BDD_unificada[,1])>=fecha_inicio
    
    datos<-BDD_unificada[restric,nombre]
    #Descomposicion
    fit<-stlplus(datos, n.p = input$num_period_mch , s.window = 365 ,s.degree = 2,
                 l.window = 200, t.window = input$sldr_mch, t.degree = 2 )
    
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
  
        #Serie Original ------
  output$mch_dsst_org <- renderDygraph({
    aux<-mch_dsst_dat()
    aux<-aux$Serie_Original
    dygraph(aux, main = "Serie Original",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
        #TENDENCIA ------
  output$mch_dsst_tnd <- renderDygraph({
    aux<-mch_dsst_dat()
    aux<-aux[,c('Tendencia','Tendencia_Ajustada')]
    dygraph(aux, main = "Tendencia",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
        #ESTACIONALIDAD ------
  output$mch_dsst_stc <- renderDygraph({
    aux<-mch_dsst_dat()
    aux<-aux[,c('Estacionalidad','Estacionalidad_Ajustada')]
    dygraph(aux, main = "Estacionalidad",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
        #RUIDO --------
  output$mch_dsst_ruid <- renderDygraph({
    aux<-mch_dsst_dat()
    aux<-aux$Ruido
    dygraph(aux, main = "Ruido",group = 'descomp')%>% 
      dyRangeSelector(dateWindow = isolate(input$rngo_fcha_dsst))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
  
  # ANALISIS CLUSTERIZACION (MAPA) ======================
  
  clus_dat<-eventReactive(input$vaz_clus_boton,{
    D_aux <- switch(input$vaz_clus_metric,
                    'D_ccor'=D_ccor,
                    'D_cor'=D_cor,
                    'D_cort'=D_cort,
                    'D_acf'=D_acf,
                    'D_euc'=D_euc,
                    'D_fourier'=D_fourier,
                    'D_ifnrm'=D_ifnrm,
                    'D_manh'=D_manh,
                    'D_mink'=D_mink,
                    'D_pacf'=D_pacf,
                    'D_per'=D_per
                    )
    
    aux_cluster <- cluster_geografico(D=D_aux,k=as.numeric(input$vaz_clus_k),
                                      tipo=input$vaz_clus_metod,
                                      vazoes_code)
    
    return(aux_cluster$BDD_cluster)
  })
  
    # Mapa Cluster ----------
  output$mapa_cluster <- renderLeaflet({
    
    BDD_cluster <- clus_dat()
    # Etiquetas de Estaciones
    burbuja_clust <- paste(sep = "<br/>",
                           "<b><a href='-'>Tipo: </a></b>Vazoes",
                           paste0("<b><a href='-'>Estación: </a></b>", BDD_cluster$Estacion),
                           paste0("<b><a href='-'>Longitud: </a></b>", BDD_cluster$Longitud),
                           paste0("<b><a href='-'>Latitud: </a></b>" ,BDD_cluster$Latitud),
                           paste0("<b><a href='-'>Cluster: </a></b>" , BDD_cluster$Cluster)
    )
    
    #Modificar colorde Marcadores para estaciones
    getColor_clus <- function(BDD_cluster) {  
      #La funcion crea un vector con colores basado en una condicion
      sapply(BDD_cluster$Cluster, function(Cluster) {
        if(Cluster == 1) {
          "blue"
        } else if(Cluster == 2) {
          "orange"
        } else if(Cluster == 3) {
          "green"
        } else if(Cluster == 4) {
          'purple'
        } else if(Cluster == 5) {
          "gray"
        } else if(Cluster == 6) {
          "red"
        } else if(Cluster == 7) {
          "lightgray"
        } else if(Cluster == 8) {
          "darkblue"
        } else{
          NA
        }
        
      })
    }
    
    icons_clus <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor_clus(BDD_cluster) 
    )
    
    #Generacion de Mapa (Leaflet)
    
    leaflet() %>% addTiles() %>%
      
      #Marcadores: 
      addAwesomeMarkers(lng=BDD_cluster$Longitud,lat=BDD_cluster$Latitud,#group ='Clima',
                        icon=icons_clus,popup=burbuja_clust)%>%
      #Añadir Minimapa
      addMiniMap(position='bottomright',toggleDisplay = TRUE)
    
  })
  
    #Tabla de Estaciones por Cluster -----
  output$tabla_cluster <- renderDataTable(server = FALSE,
                                        datatable(filter = 'top',
                                                  options = list(pageLength=7,searchHighlight = TRUE),{
                                                    aux<-clus_dat()
                                                    aux[,c(-1,-3)]
                                                  })
                                        )
    #Grafico de Series (CLUSTER) ------
  serie_cluster <- eventReactive(input$vaz_clu_grf_boton,{
    est_selec<-input$tabla_cluster_rows_selected
    # aux<-xts(BDD_unificada[,nombres],order.by =as.Date(BDD_unificada[,1]))
    aux<-vazoes_cluster_ts[,est_selec]
    return(aux)
    
  })
  
  output$vaz_clu_grf <- renderDygraph({
    aux<-serie_cluster()
    dygraph(aux)%>% 
      dyRangeSelector(dateWindow = c('2010-01-01','2015-12-31'))%>%
      dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
      dyLegend(width = 400)
  })
})
