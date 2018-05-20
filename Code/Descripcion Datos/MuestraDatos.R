#MUESTRA DE DATOS --------------
output$tabla_muestra <- renderDataTable(datatable(options = list(pageLength=9),{
  series<-c(1,2,152,165,2300,2383)
  restric<-as.character(BDD_unificada[,1])>='2000-01-01'
  aux<-BDD_unificada[restric,series]
  aux
}))