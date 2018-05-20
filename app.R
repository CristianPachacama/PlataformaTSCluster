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

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#-------------------   APP: TIME SERIES ANALYSIS V5   ---------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(shiny)
#library(shinythemes)
#library(shinydashboard)
#library(markdown)
#Mapas
library(leaflet)
library(htmltools)
library(rgdal)
#Datos Faltantes
library(DT)
#Grafica Series
library(plotly)
#library(ggplot2)
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
#>> Carga de Datos
load('Data/datos_interfaz.RData')
load('Data/Vazoes_Cluster.RData')
# PARAMETROS INICIALES -----------------------------------------
source("Code/ParametrosIniciales.R",local = TRUE)

# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!    USER INTERFACE   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
ui <- navbarPage(title = "Modelo Predictivo DGIP",
                 header = tags$h2("Header-Plataforma",tags$head(tags$link(rel='shortcut icon', 
                                                                          href='epn.ico', 
                                                                          type='image/x-icon'))),
                 position = "fixed-top",#theme=shinytheme('flatly'),#theme = 'estilo.css',
                 footer = fluidRow(column(12,img(src='epn_logo.png',width='30px',align='center'),
                                          tags$b('Proyecto: '),' "Extreme low Levels of setreamflow in Hydropower Plants".' ,
                                          '-',tags$a('Departamento de Matemática - EPN (2018)',href='http://www.epn.edu.ec'),
                                          tags$b('  ||  '),tags$b('Desarrollado por: '),
                                          tags$a('Cristian Pachacama',href='http://www.linkedin.com/in/cristian-david-pachacama')
                 )
                 ),
                 
                 #INTRODUCCION E INFORMACION DEL PROYECTO ---------------------------
                 tabPanel('Introducción',icon=icon('home'),
                          
                          fluidRow(
                            
                            sidebarPanel(img(src='epn_logo2.png',width='90%',align='center' ),
                                         fluidRow(' '),
                                         hr(),
                                         fluidRow(
                                           column(3,tags$b('Proyecto:')),column(1),
                                           column(8,'Forecast and Impact of extreme low levels of streamflow in hydropower plants.')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Código:')),column(1),
                                           column(8,'PIS-16-14')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Linea de Investigación:')),column(1),
                                           column(8,'Modeloamiento Estadístico')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Departamento:')),column(1),
                                           column(8,'Matemática')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Directora:')),column(1),
                                           column(8,'PhD. Adriana Uquillas')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Researcher:')),column(1),
                                           column(8,'PhD. Meitner Cadena')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Assistant:')),column(1),
                                           column(8,'Cristian Pachacama')
                                         )
                                         
                                         
                            ),
                            
                            mainPanel(
                              h3('Forecast and Impact of extreme low levels of streamflow in hydropower plants.'),
                              hr(),h4('Resume:'),
                              fluidRow(' '),
                              p('The development of a methodology to valuate hydrologic critical situations 
                                is critical given that the availability of water, profoundly affects the service 
                                conditions of the consumer energy market, mainly in the countries where the 
                                predominance energy sector is hydraulic.'),
                              p('In this project, we will propose a new approach for modeling extreme low levels 
                                of streamflow in hydropower plants. The Brazilian electricity system has the 
                                peculiarity that most of its energy is generated using renewable sources, 
                                mainly by hydroelectric plants (as also in Ecuador). 
                                Moreover, due to the continental dimensions of Brazil and the influences of many 
                                different climatological patterns, there is the possibility to develop a deep 
                                search in operation planning of hydroelectric plants.For this reason, it provides a representative sample of the difficulties for 
                                operating a predominantly renewable basis energy matrix. To deal with that kind 
                                of problems of Brazilian case we have the support of PhD Ildo Sauer, research 
                                professor of the Sao Paulo University, who is an expert in energy planning, demand 
                                models, resources and energy supply, regulation, control and energy policies. 
                                In this project, we will benefit of him experience in order to replicate these 
                                studies to the Ecuadorian case in next researches.'),
                              p('In the theoretical aspect, the modeling of future inflows will be made via Extreme 
                                Value Theory, because in the situation of climate changes the deviation of the 
                                mean-variance models increase significantly, resulting in periods with risk of deeper 
                                droughts. This, may, eventually, lead to situations in which the supply capacity is 
                                less than the demand, leading even rationing (Brazil had a rationing in 2002). 
                                It is then clear that the ability to accurately predict extreme low levels of flow 
                                rates and to make early warnings of these events is an important tool for the 
                                operation of the electric sectors with   predominance of hydraulic energy.'),
                              p('There are evidences that exist a causal relationship between the streamflow and the 
                                large-scale phenomena such as El Niño where the climate information is incorporated 
                                in a systematic way for decision making in water resources. However, this evidences 
                                and techniques used in its analysis do not consider extreme events caused by critical 
                                climate changes that are the objective of this study, where we propose the study of 
                                extreme low water levels in hydropower plants incorporating temporal, spatial and 
                                weather couplings that directly affect the operational planning.')
                              
                              
                            )
                            
                            
                            
                          ),hr()
                          
                          
                 ),
                 
                 #INFORMACIÓN DE LA BASE DE DATOS ------------------------------
                 navbarMenu("Datos",
                            #DESCRIPCIÓN DE DATOS --------------
                            tabPanel('Descripción',
                                     fluidRow(
                                       
                                       navlistPanel('Menú',
                                                    # Descripción ----
                                                    tabPanel('Información',
                                                             
                                                             mainPanel(width=12,
                                                                       
                                                                       h3('Base de Datos: Proyecto Semilla'),h4('Descripción de Datos'),hr(),
                                                                       p('Se cuenta con una base de datos de 31588 observaciones diarias de 2383 
                                                                         variables, las variables se encuentran clasificadas en 5 Tipos:'), 
                                                                       tags$ol(
                                                                         
                                                                         tags$li('Primero las variables correspondientes a Flujos (Vazoes), mismas que
                                                                                 corresponden a series de tiempo de 150 estaciones georeferenciadas.'),
                                                                         tags$li('Además, tenemos variables referentes a Clima. Contamos con 
                                                                                 observaciones de 8 variables en 260 estaciones. Las variables son las siguientes:',
                                                                                 tags$ul(
                                                                                   tags$li('Evaporacao_Piche'),
                                                                                   tags$li('Insolacao'),
                                                                                   tags$li('Precipitacao_12H'),
                                                                                   tags$li('Temp_Comp_Media'),
                                                                                   tags$li('TempMaxima'),
                                                                                   tags$li('TempMinima_12H'),
                                                                                   tags$li('Umidade_Relativa_Media'),
                                                                                   tags$li('Velocidade_do_Vento_Media')
                                                                                 )
                                                                         ),
                                                                         tags$li('Contamos además con las variables globales que corresponden a 13 índices,
                                                                                 que miden fenómenos meteorológicos y climatológicos a nivel mundial',
                                                                                 tags$ul(
                                                                                   tags$li('AAO: ',tags$a(href='http://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/cpc_aao_daily&STATION=AAO&TYPE=i&id=someone@somewhere&NPERYEAR=366','Antartic Oscillation Index')),
                                                                                   tags$li('AO: ', tags$a(href='http://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/cpc_ao_daily&STATION=AO&TYPE=i&id=someone@somewhere&NPERYEAR=366','Artic Oscillation Index')),
                                                                                   tags$li('MJO-RMM1: ',tags$a(href='http://climexp.knmi.nl/getindices.cgi?WMO=BMRCData/rmm1&STATION=RMM1&TYPE=i&id=someone@somewhere&NPERYEAR=366','Oscilacao Madden-Jullian RMM1')),
                                                                                   tags$li('MJO-RMM2: ',tags$a(href='http://climexp.knmi.nl/getindices.cgi?WMO=BMRCData/rmm2&STATION=RMM2&TYPE=i&id=someone@somewhere&NPERYEAR=366','Oscilacao Madden-Jullian RMM2')),
                                                                                   tags$li('NAO: ',tags$a(href='http://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/cpc_nao_daily&STATION=NAO&TYPE=i&id=someone@somewhere&NPERYEAR=366','North Atlantic Oscillation Index')),
                                                                                   tags$li('Nino3: ',tags$a(href='http://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/nino3_daily&STATION=NINO3&TYPE=i&id=someone@somewhere&NPERYEAR=366','El Niño 3')),
                                                                                   tags$li('Nino4: ',tags$a(href='http://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/nino4_daily&STATION=NINO4&TYPE=i&id=someone@somewhere&NPERYEAR=366','El Niño 4')),
                                                                                   tags$li('Nino12: ',tags$a(href='http://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/nino12_daily&STATION=NINO12&TYPE=i&id=someone@somewhere&NPERYEAR=366','El Niño 1+2')),
                                                                                   tags$li('Nino34: ',tags$a(href='http://climexp.knmi.nl/getindices.cgi?WMO=NCEPData/nino34_daily&STATION=NINO3.4&TYPE=i&id=someone@somewhere&NPERYEAR=366','El Niño 3.4')),
                                                                                   tags$li('SOI: ',tags$a(href='https://www.longpaddock.qld.gov.au/seasonalclimateoutlook/southernoscillationindex/soidatafiles/index.php','Southern Oscillation Index')),
                                                                                   tags$li('SOI_DAR: ',tags$a(href='https://www.longpaddock.qld.gov.au/seasonalclimateoutlook/southernoscillationindex/soidatafiles/index.php','Southern Oscillation Index')),
                                                                                   tags$li('SOI_TAH: ',tags$a(href='https://www.longpaddock.qld.gov.au/seasonalclimateoutlook/southernoscillationindex/soidatafiles/index.php','Southern Oscillation Index')),
                                                                                   tags$li('TSI: ',tags$a(href='http://climexp.knmi.nl/getindices.cgi?WMO=PMODData/solarconstant_daily&STATION=measured_solar_constant&TYPE=i&id=someone@somewhere&NPERYEAR=366','Total solar irradiance'))
                                                                                 )
                                                                         ),
                                                                         tags$li('Contamos además con 3 variables de Contaminación, en alrededor de 32 estaciones 
                                                                                 (dependiendo de la variable), dentro de Sao Paulo . Las detallamos a continuación:',
                                                                                 tags$ul(
                                                                                   tags$li('MP10'),
                                                                                   tags$li('MP2.5'),
                                                                                   tags$li('NO2')  
                                                                                 ),
                                                                                 tags$li('Finalmente, tenemos la variable ',tags$i('Sunspot'),'(Variable Global), que en la base se encuantra con el nombre',
                                                                                         tags$ul(
                                                                                           tags$li('Manchas_Solares: (EISN, por sus siglas en inglés) es un valor diario obtenido por un promedio 
                                                                                                   simple sobre los recuentos de manchas solares disponibles de las estaciones de alerta en la red SILSO.
                                                                                                   Los valores brutos de cada estación se escalan usando su coeficiente personal medio k anual durante el 
                                                                                                   último año transcurrido. Por lo tanto, en comparación con el número mensual internacional de manchas 
                                                                                                   solares (producido el primer día de cada mes), la precisión de la EISN es menor porque el cálculo se 
                                                                                                   basa en un número menor de estaciones y el coeficiente de escala k es solo una aproximación de la verdadera k coeficiente del mes')
                                                                                         )
                                                                                 )
                                                                                 
                                                                         )
                                                                         
                                                                       )#Fin de items Enumerados
                                                                       
                                                             )
                                                             
                                                    ),
                                                    #Diccionario -------
                                                    tabPanel('Diccionario',
                                                             
                                                             mainPanel(width=12,
                                                                       
                                                                       h3('Base de Datos: Diccionario'),h4('Codificación de Variables'),hr(),
                                                                       p('Como tenemos 2382 variables que se encuentran clasificadas en 4 Tipos,
                                                                         sus nombres se codificaron siguiendo las siguientes reglas:',
                                                                         tags$ol(
                                                                           tags$li('El nombre de la serie esta compuesto en primer lugar 
                                                                                   por el tipo de variable a la que corresponde, escrito en mayúsculas.
                                                                                   Contamos con 5 tipos de variables:',
                                                                                   tags$ul(
                                                                                     tags$li('VAZOES'),
                                                                                     tags$li('CLIMA'),
                                                                                     tags$li('CONTAM'),
                                                                                     tags$li('INDICE'),
                                                                                     tags$li('Manchas_Solares')
                                                                                   )
                                                                           ),
                                                                           tags$li('Seguido del tipo de variable, se escribe entre paréntesis 
                                                                                   el nombre de la estación a la que corresoponde la serie (excepto en 
                                                                                   variables tipo INDICE, ya que son globales).'),
                                                                           tags$li('Luego, seguido de un guión se escribe el nombre específico de la variable.')
                                                                         )
                                                                       ),
                                                                       h4('Ejemplo'),
                                                                       p('Por ejemplo, para la serie correspondiente a la variable ', tags$code('Insolacao'),
                                                                         '(de clase CLIMA), de la estación', tags$code('AGUA BRANCA'),'se codifica con el nombre:'),
                                                                       tags$li(tags$code('CLIMA(AGUA BRANCA)-Insolacao'))
                                                                       
                                                             )
                                                             
                                                             
                                                    ),
                                                    #Links Descarga ----------
                                                    tabPanel('Links de Descarga',
                                                             
                                                             mainPanel(width=12,
                                                                       
                                                                       h3('Enlaces a Datos'),hr(),
                                                                       p('Las variables con las que contamos, se clasificaron en los 5 tipos ya mencionados,
                                                                         precisamente porque fueron obtenidas de 5 distintas fuentes, mismas que mostramos a continuación.'), 
                                                                       tags$ol(
                                                                         
                                                                         tags$li('Primero las variables correspondientes a Flujos (Vazoes), fueron obtenidas 
                                                                                 de la página web de',tags$a(href='http://www.ons.org.br','Operador Nacional do Sistema Elétrico (ONS)'),'. A continuación el link de descarga de
                                                                                 los datos de esta variable.',
                                                                                 p(a(href='http://www.ons.org.br/download/operacao/hidrologia/Vaz%C3%B5es_Di%C3%A1rias_1931_2015.xls','http://www.ons.org.br/download/operacao/hidrologia/Vaz%C3%B5es_Di%C3%A1rias_1931_2015.xls'))
                                                                         ),
                                                                         tags$li('Además, tenemos variables referentes a Clima obtenidos de la página web del',
                                                                                 a(href='http://www.inmet.gov.br/portal/','Instituto Nacional de Meteorologia (INMET)'),
                                                                                 '. El link de descarga es el siguiente.',
                                                                                 p(a(href='http://www.inmet.gov.br/portal/index.php?r=bdmep/bdmep','http://www.inmet.gov.br/portal/index.php?r=bdmep/bdmep'))
                                                                         ),
                                                                         tags$li('Contamos además con las variables globales que corresponden a 13 índices,
                                                                                 descargadas de la página de ',a(href='http://climexp.knmi.nl/start.cgi?id=someone@somewhere','Climate Explorer'),
                                                                                 tags$ul(
                                                                                   tags$li('AAO: ',tags$a(href='http://climexp.knmi.nl/data/icpc_aao_daily.dat','http://climexp.knmi.nl/data/icpc_aao_daily.dat')),
                                                                                   tags$li('AO: ',tags$a(href='http://climexp.knmi.nl/data/icpc_ao_daily.dat','http://climexp.knmi.nl/data/icpc_ao_daily.dat')),
                                                                                   tags$li('MJO-RMM1: ',tags$a(href='http://climexp.knmi.nl/data/irmm1.dat','http://climexp.knmi.nl/data/irmm1.dat')),
                                                                                   tags$li('MJO-RMM2: ',tags$a(href='http://climexp.knmi.nl/data/irmm2.dat','http://climexp.knmi.nl/data/irmm2.dat')),
                                                                                   tags$li('NAO: ',tags$a(href='http://climexp.knmi.nl/data/icpc_nao_daily.dat','http://climexp.knmi.nl/data/icpc_nao_daily.dat')),
                                                                                   tags$li('Nino3: ',tags$a(href='http://climexp.knmi.nl/data/inino3_daily.dat','http://climexp.knmi.nl/data/inino3_daily.dat')),
                                                                                   tags$li('Nino4: ',tags$a(href='http://climexp.knmi.nl/data/inino4_daily.dat','http://climexp.knmi.nl/data/inino4_daily.dat')),
                                                                                   tags$li('Nino12: ',tags$a(href='http://climexp.knmi.nl/data/inino12_daily.dat','http://climexp.knmi.nl/data/inino12_daily.dat')),
                                                                                   tags$li('Nino34: ',tags$a(href='http://climexp.knmi.nl/data/inino34_daily.dat','http://climexp.knmi.nl/data/inino34_daily.dat')),
                                                                                   tags$li('SOI: ',tags$a(href='https://www.longpaddock.qld.gov.au/seasonalclimateoutlook/southernoscillationindex/soidatafiles/DailySOI1887-1989Base.txt','https://www.longpaddock.qld.gov.au/seasonalclimateoutlook/southernoscillationindex/soidatafiles/DailySOI1887-1989Base.txt')),
                                                                                   tags$li('SOI_DAR: ',tags$a(href='https://www.longpaddock.qld.gov.au/seasonalclimateoutlook/southernoscillationindex/soidatafiles/DailySOI1887-1989Base.txt','https://www.longpaddock.qld.gov.au/seasonalclimateoutlook/southernoscillationindex/soidatafiles/DailySOI1887-1989Base.txt')),
                                                                                   tags$li('SOI_TAH: ',tags$a(href='https://www.longpaddock.qld.gov.au/seasonalclimateoutlook/southernoscillationindex/soidatafiles/DailySOI1887-1989Base.txt','https://www.longpaddock.qld.gov.au/seasonalclimateoutlook/southernoscillationindex/soidatafiles/DailySOI1887-1989Base.txt')),
                                                                                   tags$li('TSI: ',tags$a(href='http://climexp.knmi.nl/data/isolarconstant_daily.dat','http://climexp.knmi.nl/data/isolarconstant_daily.dat'))
                                                                                 )
                                                                         ),
                                                                         tags$li('Contamos además con 3 variables de Contaminación. Estos datos fueron obtenidos
                                                                                 de la página del ',a(href='http://www.saopaulo.sp.gov.br','Gobierno de Sao Paulo'),
                                                                                 p(a(href='http://qualar.cetesb.sp.gov.br/qualar/relValoresDiarios.do?method=filtrarUgrhisEstacoesPorParametro'
                                                                                     ,'http://qualar.cetesb.sp.gov.br/qualar/relValoresDiarios.do?method=filtrarUgrhisEstacoesPorParametro')
                                                                                 )
                                                                                 
                                                                         ),
                                                                         tags$li('Finalmente, contamos con la variable ',tags$a(href='http://www.sidc.be/silso/eisninfo', 'Sunspot'),', obtenida en la página web de ',
                                                                                 tags$a(href='http://www.sidc.be/index.php', 'Solar Influences Data Analysis Center (SIDC)'),
                                                                                 tags$ul(
                                                                                   tags$li('Manchas_Solares:',tags$a(href='http://www.sidc.be/silso/DATA/SN_d_tot_V2.0.txt','http://www.sidc.be/silso/DATA/SN_d_tot_V2.0.txt'))
                                                                                 )
                                                                         )
                                                                         
                                                                       )#Fin de items Enumerados
                                                                       
                                                             )
                                                             
                                                    ),
                                                    #Muestra de Datos ------
                                                    tabPanel('Muestra de Datos',
                                                             fluidRow(
                                                               mainPanel(width=12,
                                                                         h3('Muestra de la Base de Datos'),hr(),
                                                                         p('A continuación mostramos una tabla que contiene observaciones de
                                                                           de 5 variables, una de cada uno de los Tipos de variables.'),
                                                                         fluidRow(dataTableOutput("tabla_muestra",width = "100%"))
                                                                         
                                                               )
                                                             )
                                                             ,hr()
                                                             
                                                    )
                                       )
                                       
                                     ),hr()
                                     
                            ),
                            #MAPA ESTACIONES -----------------------------------
                            tabPanel("Mapa Estaciones",
                                     #titlePanel("Mapa de Estaciones Existentes"),
                                     fluidRow(
                                       mainPanel(width = 11,
                                                 
                                                 h3('Mapa de Estaciones'),hr(),
                                                 p('A continuación se presenta un mapa donde se muestra 
                                        la ubicación de las estaciones de monitoreo donde 
                                        fueron recolectados los datos de Clima, y Vazoes. 
                                        Además se muestra la localización de las principales
                                        centrales Hidroeléctricas del Brasil (gráficadas en 
                                        círculos, cuyo tamaño es proporcional a la capacidad
                                        de cada central Hidroeléctrica).'),
                                                 leafletOutput("mymap",width = "100%",height = "450px")
                                                 
                                       )
                                     ),hr()
                            ),
                            #BUSQUEDA de Series (MAPA) -----------------
                            tabPanel('Buscar Estaciones (Mapa)',
                                     
                                     fluidRow(
                                       
                                       #titlePanel("Exploración de Datos Existentes"),
                                       sidebarPanel(
                                         h3('Búsqueda'),
                                         p('Para hallar una estación puedes filtrarla usando los siguientes parámetros.'),
                                         selectInput('serie_tipo_mapa', label= 'Selecciona una Clase ',list('Clima'='clima','Vazoes'='vazoes')),
                                         selectInput('serie_estac_mapa', label= 'Selecciona Estación',as.character(vazoes_dat[['Estacion']])),
                                         p('Si deseas puedes restringir las fechas de Inicio y Fin de las Series de Tiempo que se generaran.'),
                                         dateRangeInput('rng_fech_map',label = 'Elige Fechas',
                                                        start = '1931-01-01',end = '2017-06-25',
                                                        min = '1931-01-01', max = '2017-06-25',
                                                        language = 'es',separator = 'a' ),
                                         actionButton('busq_map_boton','Buscar',icon = icon('fas fa-search')),
                                         
                                         
                                         h3('Gráfica'),
                                         p('Para graficar las series primero seleccionelas de la tabla que se encuentra bajo el mapa y 
                                luego presione el botón ',tags$i('Graficar.')),
                                         actionButton('mini_graf_map_boton','Graficar',icon = icon('line-chart')),
                                         
                                         
                                         h3('Generar Archivo'),
                                         p('Para generar un Archivo con todas las Series de Tiempo de la Tabla, filtra los datos 
                                de la tabla, luego elige un Nombre para el archivo, y presiona el botón',tags$i('Generar Archivo.csv'),',  y espera que se descargue.'),
                                         textInput('nombre_archivo_map','Elije un Nombre para el Archivo', value ='Series de Tiempo'),
                                         
                                         downloadButton("descargar_series_map", "Generar Archivo.csv")
                                         
                                       ),
                                       
                                       mainPanel(
                                         h3("Búsqueda de Estaciones"),hr(),
                                         p('En el panel de la izquierda puede ajustar los parámetros de la búsqueda, a continuación 
                                se muestra una tabla con información de las series asociadas a las estaciones que busca 
                                . Además se muestran las series de las estaciones',tags$i('Vecinas.')),
                                         leafletOutput("mapa_busq",width = "100%",height = "400px"),
                                         fluidRow(dataTableOutput("tabla_busq_map",width = "60%")),hr(),
                                         dygraphOutput('mini_graf_map',height = "320px",width = '100%')
                                       )
                                       
                                     ),hr()
                                     
                            ),
                            
                            #GENERAR ARCHIVOS con SERIES ----------------
                            tabPanel('Generar Series de Tiempo',
                                    
                                     fluidRow(
                                       
                                       sidebarPanel(
                                         h3('Gráficos'),
                                         p('Selecciona Series de la Tabla para graficarlas'),
                                         actionButton('mini_graf_boton','Graficar',icon = icon('line-chart')),
                                         # dygraphOutput('mini_graf',height = "270px"),hr(),
                                         h3('Generar Archivo'),
                                         p('Para generar un Archivo con todas las Series de Tiempo de la Tabla, filtra los datos 
                                de la tabla, luego elige un Nombre para el archivo, y presiona el botón',tags$i('Generar Archivo.csv'),',  y espera que se descargue.'),
                                         textInput('nombre_archivo','Elije un Nombre para el Archivo', value ='Series de Tiempo'),
                                         p('Si deseas puedes restringir las fechas de Inicio y Fin de las Series de Tiempo que se generaran'),
                                         dateRangeInput('rng_fech_generar',label = 'Elige Fechas',
                                                        start = '1931-01-01',end = '2017-06-25',
                                                        min = '1931-01-01', max = '2017-06-25',
                                                        language = 'es',separator = 'a' ),
                                         downloadButton("descargar_series", "Generar Archivo.csv")
                                         
                                       ),
                                       mainPanel(
                                         h3("Exploración de Datos Existentes"),hr(),
                                         fluidRow(dataTableOutput("tabla_busqueda",width = "60%")),hr(),
                                         dygraphOutput('mini_graf',height = "290px",width = '80%')
                                       )
                                       
                                     ),hr()
                                     
                            ),
                            
                            #REPORTE Datos Faltantes ---------------------------
                            tabPanel("Reporte Datos Faltantes",
                                     #titlePanel("Reporte de Datos Faltantes"),
                                     fluidRow(
                                       
                                       sidebarPanel(
                                         p('Para generar un reporte de los datos faltantes de las series,
                                primero elija un Tipo, si es necesario especifique una variable
                                (en el caso de Clima y Contaminación). Luego elija el periodo
                                de tiempo que desea analizar y presione el botón', tags$i('Generar Reporte')),
                                         selectInput('tipo', 'Tipo', selected = 'Clima',
                                                     choices =list(Vazoes='Vazoes',Clima='Clima','Índices'= 'Indices',
                                                                   'Contaminación'='Contaminacion','Manchas Solares'='Manchas_Solares')),
                                         selectInput('variable', 'Variable', choices = 'Selecciona Variable'),
                                         
                                         dateRangeInput('rango_fch_reprt',language = "es", separator = " a ",
                                                        label = 'Periodo de Tiempo:',
                                                        min = '1931-01-01', max = '2017-06-25',
                                                        start = '1991-01-01', end = '2017-06-25'
                                         ),
                                         actionButton('reporte_boton','Generar Reporte',icon = icon('newspaper-o')),hr(),
                                         plotlyOutput('graf_obs',height = '260px'),hr(),
                                         h4('Generar Archivo'),
                                         p('Primero filtra la tabla de datos (usando los cajones ubicados en la parte superior de cada columna), de modo que la tabla contega a las series de tiempo que 
                                cumplan las condiciones que consideres (ej. porcentaje de valores perdidos, número de observaciones, etc.), 
                                luego elige un nombre para el archivo, y presiona el bontón',tags$i('Generar Archivo.csv'),',  y espera que se descargue.'),
                                         textInput('nombre_reprt','Elije un Nombre para el Archivo', value ='Series de Tiempo'),
                                         downloadButton("descargar_reprt", "Generar Archivo.csv")
                                       ),
                                       
                                       mainPanel(
                                         h3('Reporte de Datos Faltantes'),hr(),
                                         fluidRow(dataTableOutput("tabla_reprt",width = "50%")),hr(),
                                         plotlyOutput('hist'),
                                         plotlyOutput('box')
                                       )
                                       
                                     ),hr()
                                     
                            )
                            
                 ),
                 
                 
                 #GRAFICA DE SERIES ----------------------------------------
                 navbarMenu("Gráfica de Series",
                            #Grafica Vazoes --------
                            tabPanel("Vazoes",
                                     
                                     fluidRow(
                                       
                                       sidebarPanel(
                                         selectInput('vaz_estacion', label= 'Selecciona Estación',as.character(vazoes_dat[['Estacion']])),
                                         
                                         dateRangeInput('rango_fecha_vaz',language = "es", separator = " a ",
                                                        label = 'Periodo de Tiempo:',
                                                        min = '1931-01-01', max = '2017-06-25',
                                                        start = '1931-01-01', end = '2017-06-25'
                                         ),
                                         actionButton('graf_vaz_boton',label='Graficar Serie',icon = icon('line-chart'))
                                         
                                       ),
                                       mainPanel(
                                         h3('Gráfico de la Serie'),hr(),
                                         fluidRow(dygraphOutput("grafico_vazoes"))
                                       )
                                       
                                     ),hr()
                                     
                            ),
                            #Grafica Clima  --------
                            tabPanel("Clima",
                                     
                                     fluidRow(
                                       
                                       sidebarPanel(
                                         selectInput('clim_variable', label= 'Selecciona Variable',c('Insolacao','Evaporacao_Piche',
                                                                                                     'Precipitacao_12H','Temp_Comp_Media',
                                                                                                     'TempMaxima','TempMinima_12H',
                                                                                                     'Umidade_Relativa_Media','Velocidade_do_Vento_Media')),
                                         selectInput('clim_estacion', label= 'Selecciona Estación',clima_dat[['Estacion']]),
                                         
                                         dateRangeInput('rango_fecha_clim',language = "es", separator = " a ",
                                                        label = 'Periodo de Tiempo:',
                                                        min = '1931-01-01', max = '2017-06-25',
                                                        start = '2000-01-01', end = '2017-06-25'
                                         ),
                                         actionButton('graf_clim_boton',label='Graficar Serie',icon = icon('line-chart'))
                                         
                                       ),
                                       mainPanel(
                                         h3('Gráfico de la Serie'),hr(),
                                         dygraphOutput("grafico_clima")
                                       )
                                       
                                     ),hr()
                                     
                            ),
                            #Grafica Indices -------------
                            tabPanel("Índices",
                                     
                                     fluidRow(
                                       
                                       sidebarPanel(
                                         selectInput('nombre_indice', label= 'Selecciona un Índice',c("AAO", "AO" , "MJO_RMM1",
                                                                                                      "MJO_RMM2","NAO",'Nino12','Nino3',
                                                                                                      'Nino34','Nino4','SOI','SOI_DAR','SOI_TAH','TSI')),
                                         
                                         dateRangeInput('rango_fecha_indi',language = "es", separator = " a ",
                                                        label = 'Periodo de Tiempo:',
                                                        min = '1931-01-01', max = '2017-06-25',
                                                        start = '2005-01-01', end = '2017-06-25'
                                         ),
                                         checkboxGroupInput("grupo_indices", label = h4("O Elije un Grupo de Indices"), 
                                                            choices = c("AAO", "AO" , "MJO_RMM1",
                                                                        "MJO_RMM2","NAO",'Nino12','Nino3',
                                                                        'Nino34','Nino4','SOI','SOI_DAR','SOI_TAH','TSI'),
                                                            selected = c('AAO','AO')),
                                         actionButton('graf_ind_boton',label='Graficar Serie',icon = icon('line-chart'))
                                         
                                       ),
                                       mainPanel(
                                         h3('Gráfico de Series'),hr(),
                                         dygraphOutput("grafico_indice"),
                                         dygraphOutput("grafico_grupo_indices")
                                       )
                                       
                                     ),hr()
                                     
                            ),
                            #Grafica Contaminacion  ---------------
                            tabPanel("Contaminación",
                                     
                                     fluidRow(
                                       
                                       sidebarPanel(
                                         selectInput('contam_variable', label= 'Selecciona Variable',c('MP10','MP2.5','NO2')),
                                         selectInput('contam_estacion', label= 'Selecciona Estación',contam_dat[['Estacion']]),
                                         
                                         dateRangeInput('rango_fecha_contam',language = "es", separator = " a ",
                                                        label = 'Periodo de Tiempo:',
                                                        min = '1931-01-01', max = '2017-06-25',
                                                        start = '2010-01-01', end = '2017-06-25'
                                         ),
                                         actionButton('graf_contam_boton',label='Graficar Serie',icon = icon('line-chart'))
                                         
                                       ),
                                       mainPanel(
                                         h3('Gráfico de la Serie'),hr(),
                                         dygraphOutput("grafico_contam")
                                       )
                                       
                                     ),hr()
                                     
                            ),
                            #Grafica Manchas Solares ------------
                            tabPanel("Manchas Solares",
                                     
                                     fluidRow(
                                       
                                       sidebarPanel(
                                         selectInput('nomb_manchas', label='Serie de Tiempo',choices=list('Manchas Solares'='Manchas_Solares')),
                                         
                                         dateRangeInput('rng_fch_manchas',language = "es", separator = " a ",
                                                        label = 'Periodo de Tiempo:',
                                                        min = '1931-01-01', max = '2017-06-25',
                                                        start = '1980-01-01', end = '2017-06-25'
                                         ),
                                         actionButton('graf_manchas_boton','Graficar Serie',icon = icon('line-chart'))
                                       ),
                                       mainPanel(
                                         h3('Gráfico de la Serie'),hr(),
                                         dygraphOutput("grafico_manchas")
                                       )
                                       
                                     ),hr()
                                     
                            ),
                            
                            #Grafica de serie por nombre  ----------
                            tabPanel("Por Nombre",
                                     
                                     fluidRow(
                                       
                                       sidebarPanel(
                                         textInput('nomb_serie', label='Escribe Nombre de la Serie de Tiempo',value='CLIMA(AGUA BRANCA)-Insolacao'),
                                         
                                         dateRangeInput('rango_fecha_serie',language = "es", separator = " a ",
                                                        label = 'Periodo de Tiempo:',
                                                        min = '1931-01-01', max = '2017-06-25',
                                                        start = '1980-01-01', end = '2017-06-25'
                                         ),
                                         actionButton('graf_seri_boton','Graficar Serie',icon = icon('line-chart'))
                                       ),
                                       mainPanel(
                                         h3('Gráfico de la Serie'),hr(),
                                         dygraphOutput("grafico_serie")
                                       )
                                       
                                     ),hr()
                                     
                            )
                            
                 ),
                 
                 #DESESTACIONALIZACION DE SERIES --------------------------
                 navbarMenu('Análisis de Series',
                            # Vazoes ---------------------
                            tabPanel("Vazoes",
                                     navlistPanel('Tipo de Análisis',widths = c(2, 10),
                                                  # Vazoes Descomposición STL ----
                                                  tabPanel('1. Descomposición STL',
                                                           fluidRow(
                                                             
                                                             sidebarPanel(h3('Descomposición STL-Loess'),
                                                                          selectInput('dsst_vaz_estac', label= 'Selecciona Estacion',
                                                                                      as.character(vazoes_dat[['Estacion']])
                                                                          ),
                                                                          numericInput('num_period_vaz','Periodicidad (en días)',value = 365),
                                                                          sliderInput('sldr_vaz',label = 'Lags para Tendencia',
                                                                                      min=100,max=5000,step = 100,value = 700),
                                                                          
                                                                          dateRangeInput('rngo_fcha_dsst',language = "es", separator = " a ",
                                                                                         label = 'Periodo de Tiempo:',
                                                                                         min = '1931-01-01', max = '2017-06-25',
                                                                                         start = '2001-01-01', end = '2017-06-25'
                                                                          ),
                                                                          actionButton('boton_vaz_dsst',label='Descomponer Serie',icon=icon('line-chart'))
                                                             ),
                                                             mainPanel(
                                                               #Graficos de la Descomposicion
                                                               h3('Descomposición de Series de Vazoes'),hr(),
                                                               dygraphOutput("vaz_dsst_org",height = "240px"),
                                                               dygraphOutput("vaz_dsst_tnd",height = "240px"),
                                                               dygraphOutput("vaz_dsst_stc",height = "240px"),
                                                               dygraphOutput("vaz_dsst_ruid",height = "240px")
                                                             )
                                                             
                                                           )
                                                  ),
                                                  # Vazoes Valores Extremos ----
                                                  tabPanel('2. Valores Extremos',
                                                           fluidRow(
                                                             sidebarPanel(h3('Descomposición STL-Loess'),
                                                                          selectInput('ext_vaz_estac', label= 'Selecciona Estacion',
                                                                                      as.character(vazoes_dat[['Estacion']])
                                                                          ),
                                                                          numericInput('num_period_vaz_ext','Periodicidad (en días)',value = 365),
                                                                          sliderInput('sldr_vaz_ext',label = 'Lags para Tendencia',
                                                                                      min=100,max=5000,step = 100,value = 700),
                                                                          
                                                                          h3('Valores Extremos'),
                                                                          p('Elige el periodo de tiempo en el que se va a considerar los datos del ',tags$i('Ruido'),' a los que se les realizará el Análisis de Valores Extremos.'),
                                                                          dateRangeInput('rngo_fcha_ext',language = "es", separator = " a ",
                                                                                         label = 'Periodo de Tiempo:',
                                                                                         min = '1931-01-01', max = '2017-06-25',
                                                                                         start = '2001-01-01', end = '2017-06-25'
                                                                          ),
                                                                          
                                                                          selectInput('ext_vaz_metod',label='Elige un Método',choices = c("MLE", "GMLE", "Bayesian", "Lmoments"),selected = 'Lmoments'),
                                                                          selectInput('ext_vaz_tipo',label='Elige el Tipo ajuste (distribución)',choices = c("GEV", "GP", "PP", "Gumbel", "Exponential"),selected = 'GEV'),
                                                                          actionButton('boton_vaz_ext',label='Analizar',icon=icon('line-chart'))
                                                             ),
                                                             mainPanel(#width=12,
                                                               h3('Análisis de Valores Extremos'),hr(),
                                                               p('Una vez realizada la descomposición STL se puede realizar este análisis. Para ello consideramos al ruido obtenido de la 
                                                                 descomposición STL, que debería corresponder a un procesos estacionario. A partir de esta serie se obtiene lo siguiente.'),
                                                               
                                                               dygraphOutput("vaz_ext_ruid",height = "240px"),
                                                               plotlyOutput("vaz_ext_histgrm",height = '240px'),
                                                               plotOutput("vaz_ext_fevd",height = "800px")
                                                               
                                                             )
                                                             
                                                           )
                                                  )
                                     ),hr()
                                     
                            ),
                            
                            #Descomposicion de Clima ---------------------
                            tabPanel("Clima",
                                     fluidRow(
                                       
                                       sidebarPanel(h3('Descomposición STL-Loess'),
                                                    selectInput('dsst_clm_var', label= 'Selecciona una Variable',
                                                                c('Insolacao','Evaporacao_Piche',
                                                                  'Precipitacao_12H','Temp_Comp_Media',
                                                                  'TempMaxima','TempMinima_12H',
                                                                  'Umidade_Relativa_Media','Velocidade_do_Vento_Media')
                                                    ),
                                                    selectInput('dsst_clm_estac', label= 'Selecciona Estacion',
                                                                as.character(clima_dat[['Estacion']])
                                                    ),
                                                    sliderInput('sldr_clm',label = 'Lags para Tendencia',
                                                                min=100,max=5000,step = 100,value = 700),
                                                    dateRangeInput('rngo_fcha_dsst',language = "es", separator = " a ",
                                                                   label = 'Periodo de Tiempo:',
                                                                   min = '1931-01-01', max = '2017-06-25',
                                                                   start = '2001-01-01', end = '2017-06-25'
                                                    ),
                                                    actionButton('boton_clm_dsst',label='Descomponer Serie',icon=icon('line-chart'))
                                       ),
                                       mainPanel(
                                         #Graficos de la Descomposicion
                                         h3('Descomposición de Series de Clima'),hr(),
                                         dygraphOutput("clm_dsst_org",height = "240px"),
                                         dygraphOutput("clm_dsst_tnd",height = "240px"),
                                         dygraphOutput("clm_dsst_stc",height = "240px"),
                                         dygraphOutput("clm_dsst_ruid",height = "240px")
                                       )
                                       
                                     ),hr()
                            ),
                            #Descomposición Contaminacion -------
                            tabPanel("Contaminación",
                                     fluidRow(
                                       
                                       sidebarPanel(h3('Descomposición STL-Loess'),
                                                    selectInput('dsst_cnt_var', label= 'Selecciona una Variable',
                                                                c('MP10','MP2.5','NO2')
                                                    ),
                                                    selectInput('dsst_cnt_estac', label= 'Selecciona Estacion',
                                                                as.character(contam_dat[['Estacion']])
                                                    ),
                                                    sliderInput('sldr_cnt',label = 'Lags para Tendencia',
                                                                min=100,max=4000,step = 100,value = 700),
                                                    dateRangeInput('rngo_fcha_dsst',language = "es", separator = " a ",
                                                                   label = 'Periodo de Tiempo:',
                                                                   min = '1931-01-01', max = '2017-06-25',
                                                                   start = '2001-01-01', end = '2017-06-25'
                                                    ),
                                                    actionButton('boton_cnt_dsst',label='Descomponer Serie',icon=icon('line-chart'))
                                       ),
                                       mainPanel(
                                         #Graficos de la Descomposicion
                                         h3('Descomposición de Series de Contaminación'),hr(),
                                         dygraphOutput("cnt_dsst_org",height = "240px"),
                                         dygraphOutput("cnt_dsst_tnd",height = "240px"),
                                         dygraphOutput("cnt_dsst_stc",height = "240px"),
                                         dygraphOutput("cnt_dsst_ruid",height = "240px")
                                       )
                                       
                                     ),hr()
                            ),
                            #Descomposición Indices -------
                            tabPanel("Índice",
                                     fluidRow(
                                       
                                       sidebarPanel(h3('Descomposición STL-Loess'),
                                                    selectInput('dsst_ind_var', label= 'Selecciona un Índice',
                                                                c("AAO", "AO" , "MJO_RMM1","MJO_RMM2"
                                                                  ,"NAO",'Nino12','Nino3','Nino34',
                                                                  'Nino4','SOI','SOI_DAR','SOI_TAH','TSI')
                                                    ),
                                                    numericInput('num_period_ind','Periodicidad',value = 30),
                                                    sliderInput('sldr_ind',label = 'Lags para Tendencia',
                                                                min=100,max=2500,step = 100,value = 360),
                                                    dateRangeInput('rngo_fcha_dsst',language = "es", separator = " a ",
                                                                   label = 'Periodo de Tiempo:',
                                                                   min = '1931-01-01', max = '2017-06-25',
                                                                   start = '2001-01-01', end = '2017-06-25'
                                                    ),
                                                    actionButton('boton_ind_dsst',label='Descomponer Serie',icon=icon('line-chart'))
                                       ),
                                       mainPanel(
                                         #Graficos de la Descomposicion
                                         h3('Descomposición de Índice'),hr(),
                                         dygraphOutput("ind_dsst_org",height = "240px"),
                                         dygraphOutput("ind_dsst_tnd",height = "240px"),
                                         dygraphOutput("ind_dsst_stc",height = "240px"),
                                         dygraphOutput("ind_dsst_ruid",height = "240px")
                                       )
                                       
                                     ),hr()
                            ),
                            
                            #Descomposición Manchas Solares -------
                            tabPanel("Manchas Solares",
                                     fluidRow(
                                       
                                       sidebarPanel(h3('Descomposición STL-Loess'),
                                                    selectInput('dsst_manchas', label= 'Selecciona Serie de Tiempo',
                                                                list('Manchas Solares'='Manchas_Solares')
                                                    ),
                                                    numericInput('num_period_mch','Periodicidad',value = 4200),
                                                    sliderInput('sldr_mch',label = 'Lags para Tendencia',
                                                                min=0,max=10000,step = 100,value = 5000),
                                                    dateRangeInput('rngo_fcha_dsst',language = "es", separator = " a ",
                                                                   label = 'Periodo de Tiempo:',
                                                                   min = '1931-01-01', max = '2017-06-25',
                                                                   start = '2001-01-01', end = '2017-06-25'
                                                    ),
                                                    actionButton('boton_manchas_dsst',label='Descomponer Serie',icon=icon('line-chart'))
                                       ),
                                       mainPanel(
                                         #Graficos de la Descomposicion
                                         h3('Descomposición - Manchas Solares'),hr(),
                                         dygraphOutput("mch_dsst_org",height = "240px"),
                                         dygraphOutput("mch_dsst_tnd",height = "240px"),
                                         dygraphOutput("mch_dsst_stc",height = "240px"),
                                         dygraphOutput("mch_dsst_ruid",height = "240px")
                                       )
                                       
                                     ),hr()
                            ),
                            
                            #Descripcion LOESS ----------------
                            tabPanel('Detalles STL-Loess',
                                     
                                     fluidRow(
                                       
                                       mainPanel(width = 12,h3('STL: A Seasonal-Trend Decomposition Procedure Based on Loess'),
                                                 h5('Robert B. Cleveland, William S. Cleveland, Jean E. McRae, and  Irma Terpenning'),hr(),
                                                 h4('Abstract'),
                                                 p('STL is a filtering procedure for decomposing a time series into trend seasonal,
                                                   and remainder components. STL has a simple design that consist of a sequence of
                                                   applications of the loess smoother; the simplicity allows analysis of the properties
                                                   of the proceure and allows fast computation, even for very long time series and
                                                   large amounts of trend and seasonal smoothing. Other features of STL are 
                                                   specification of amounts of seasonal and trend smoothing that range, in a nearly
                                                   continuous way, from a very small amount of smoothing to a very large amount;
                                                   robust estimates of the trend and seasonal components that are not distorted
                                                   by aberrant behavior in the data; specification of the period of seasonal 
                                                   component to any integer multiple of the time sampling interval greater than one;
                                                   and the ability to decompose time series with missing values.'),
                                                 h4('Más'),
                                                 p('Para mirar a detalle la descomposición',tags$a(href='https://search.proquest.com/docview/1266805989?pq-origsite=gscholar','STL-Loess') , 'puede acceder al paper en el siguiente enlace:',
                                                   tags$a(href='https://search.proquest.com/docview/1266805989?pq-origsite=gscholar','https://search.proquest.com/docview/1266805989?pq-origsite=gscholar'))
                                       )
                                     ),hr()
                                     
                                     
                            )
                            
                            
                            
                            
                 ),
                 # ANALISIS MULTIVARIANTE DE SERIES ============================
                 tabPanel('Multivariante',
                          
                          fluidRow(
                            
                            sidebarPanel(
                              h4('Cluster de Series de Tiempo'),
                              p('Primero selecciona una de las Métricas definidas para series de tiempo.'),
                              selectInput('vaz_clus_metric', label= 'Selecciona Métrica',selected = 'D_ccor',
                                          list('Correlación Cruzada'='D_ccor',
                                               'Autocorrelación'='D_acf',
                                               'Correlación de Pearson'='D_cor',
                                               'Correlación Temporal'='D_cort',
                                               'Métrica Euclidea'='D_euc',
                                               'Métrica de Fourier'='D_fourier',
                                               'Métrica Infinito'='D_ifnrm',
                                               'Métrica Manhatan'='D_manh',
                                               'Métrica de Minkwoski'='D_mink',
                                               'Autocorrelación Parcial'='D_pacf',
                                               'Periodograma'='D_per')),
                              p('Luego elige un método de clusterización (agrupamiento).'),
                              selectInput('vaz_clus_metod', label= 'Selecciona Método',selected = 'clara',
                                          list('K-Medias'='kmedias','K-Medoid (CLARA)'='clara','Cluster Gerárquico'='gerarquico')),
                              p('Finalmente elige el número de clusters que quieres que se formen.'),
                              selectInput('vaz_clus_k', label= 'Número de Clusters',c(2:8),selected = 4),
                              actionButton('vaz_clus_boton',label='Clusterizar',icon = icon('braille')),hr(),
                              h4('Gráfico de Series'),
                              p('Para graficar una o varias series, primero clusteriza las estaciones, luego
                                  seleccione los nombres de las estaciones correspondientes en la Tabla que 
                                  se encuentra en la parte inferior derecha'),
                              actionButton('vaz_clu_grf_boton',label='Graficar',icon = icon('line-chart'))
                            ),
                            mainPanel(
                              h3('Mapa de Estaciones Clusterizadas: Vazoes '),hr(),
                              leafletOutput("mapa_cluster",width = "100%",height = "450px"),hr(),
                              h4('Tabla de Estaciones por Cluster'),
                              fluidRow(dataTableOutput("tabla_cluster",width = "50%")),hr(),
                              dygraphOutput('vaz_clu_grf')
                            )
                          ),hr()
                          
                 )
                 
)



# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
server <- function(input, output,session) {

  # DATOS ================================================================
  
  # >> MUESTRA DE DATOS    --------------------------------------
  source("Code/Descripcion Datos/MuestraDatos.R",local = TRUE)
  
  # >> MAPAS Estaciones -----------------------------------------
  source("Code/Descripcion Datos/Mapas.R",local = TRUE)
  
  # >> BUSQUEDA de Estaciones (MAPA) ----------------------------
  source("Code/Descripcion Datos/BusquedaMapa.R",local = TRUE)
  
  # >> Generar Series de Tiempo  --------------------------------
  source("Code/Descripcion Datos/GenerarSerie.R",local = TRUE)

  # >> REPORTE DATOS FALTANTES ----------------------------------
  source("Code/Descripcion Datos/ReporteFaltantes.R",local = TRUE)
  
  # GRAFICO DE SERIES ====================================================
  source("Code/Descripcion Datos/Grafico Series/Vazoes.R",local = TRUE)
  source("Code/Descripcion Datos/Grafico Series/Clima.R",local = TRUE)
  source("Code/Descripcion Datos/Grafico Series/Indices.R",local = TRUE)
  source("Code/Descripcion Datos/Grafico Series/Contaminacion.R",local = TRUE)
  source("Code/Descripcion Datos/Grafico Series/ManchasSolares.R",local = TRUE)
  source("Code/Descripcion Datos/Grafico Series/SeriexNombre.R",local = TRUE)
  
  # ANALISIS DE SERIES ===================================================
  # >> Vazoes  --------------------------------------------------
  source("Code/Descripcion Datos/Analisis Series/Vazoes.R",local = TRUE)

  # >> Clima  ---------------------------------------------------
  source("Code/Descripcion Datos/Analisis Series/Clima.R",local = TRUE)

  # >> Contaminacion  -------------------------------------------
  source("Code/Descripcion Datos/Analisis Series/Contaminacion.R",local = TRUE)

  # >> Indice  --------------------------------------------------
  source("Code/Descripcion Datos/Analisis Series/Indices.R",local = TRUE)

  # >> Manchas Solares  -----------------------------------------
  source("Code/Descripcion Datos/Analisis Series/ManchasSolares.R",local = TRUE)

  # MULTIVARIANTE   ======================================================
  source("Code/Descripcion Datos/Multivariante/TSCluster.R",local = TRUE)
}


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     RUN APP      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
shinyApp(ui = ui, server = server)





