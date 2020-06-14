#Autor: Hugo Gallardo
#Extrae  los cambios de moneda de API https://mindicador.cl/api/ con consulta de todos los cambios anuales. 
#De estos, se extrae el primer cambio de moneda por mes y se agrupan todos en un dataframe.
#La funcion toma el año inicial y final, generando un rango entre esos anios.
#Se utiliza API https://mindicador.cl/api/.

#A considerar:
#UF tiene datos todos los días.
#UTM está a nivel mensual con el primer día de cada mes
#Dolar contiene solo algunos datos (248 del anio)
#Euro está solo algunos datos (248 del anio)
#En caso de necesitar solo un año, ingresar ambos anios iguales

library(jsonlite)
library(httr)
library(dplyr)
library(lubridate)

IndicadorMensual <- function(AnioInicioAPI, AnioFinalAPI){
  
  ListaMonedas<- c("uf","utm","euro","dolar") 
  #Generar rango de anios
  ListAnio <- AnioInicioAPI:AnioFinalAPI
  
  for( i in 1: length(ListaMonedas)){
    
    #Primer caso: UF
    if(i ==1){
      for(k in  1:length(ListAnio)) {
        if(k == 1){
          #Extraigo el json anual
          urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
          GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
          MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
          #Itero sobre json para transformar a DF
          for(j in 1:length(MonedaJsonAnio$serie)){
            if(j==1){
              DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
            }else{
              DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
              DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
            }
          }
          #Llevo cambio de moneda diaria a mensual filtrando por el primer día de cada mes:
          DFMonedaAnioMes <- DFMonedaAnioDia %>% filter(., day(fecha) ==01)
        } else{
          
          urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
          GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
          MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
          #Itero sobre json para transformar a DF
          for(j in 1:length(MonedaJsonAnio$serie)){
            if(j==1){
              DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
            }else{
              DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
              DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
            }
          }
          #Llevo cambio de moneda diaria a mensual:
          DFMonedaAnioMesAux <- DFMonedaAnioDia %>% filter(., day(fecha) ==01)
          DFMonedaAnioMes<- bind_rows(DFMonedaAnioMes,DFMonedaAnioMesAux)
          
        }
        
      }
      #Caso UTM
    }else if(i==2){
      
      for(k in  1:length(ListAnio)) {
        if(k == 1){
          #Extraigo el json anual
          urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
          GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
          MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
          #Itero sobre json para transformar a DF
          for(j in 1:length(MonedaJsonAnio$serie)){
            if(j==1){
              DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
            }else{
              DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
              DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
            }
            
          }
          #Llevo cambio de moneda diaria a mensual filtrando por el primer día de cada mes:
          DFMonedaAnioMesAux <- DFMonedaAnioDia
          DFMonedaAnioMes<- bind_rows(DFMonedaAnioMes,DFMonedaAnioMesAux)
          
        } else{
          
          urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
          GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
          MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
          #Itero sobre json para transformar a DF
          for(j in 1:length(MonedaJsonAnio$serie)){
            if(j==1){
              DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
            }else{
              DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
              DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
            }
          }
          #Llevo cambio de moneda diaria a mensual:
          DFMonedaAnioMesAux <- DFMonedaAnioDia
          DFMonedaAnioMes<- bind_rows(DFMonedaAnioMes,DFMonedaAnioMesAux)
          
        }
        
      }
      
    }else if(i >=3){
      for(k in  1:length(ListAnio)) {
        if(k == 1){
          #Extraigo el json anual
          urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
          GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
          MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
          #Itero sobre json para transformar a DF
          for(j in 1:length(MonedaJsonAnio$serie)){
            if(j==1){
              DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
            }else{
              DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
              DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
            }
          }
          #Llevo cambio de moneda diaria a mensual filtrando por el primer día de cada mes:
          DFMonedaAnioDia <- DFMonedaAnioDia %>% mutate(., anio = year(fecha)) %>% mutate(., mes = month(fecha))%>%
            arrange(fecha)
          DFMonedaAnioDia <- DFMonedaAnioDia[!duplicated(DFMonedaAnioDia[,c('anio', 'mes')]),]
          DFMonedaAnioDia$anio <- NULL
          DFMonedaAnioDia$mes <- NULL
          
          DFMonedaAnioMesAux <- DFMonedaAnioDia
          DFMonedaAnioMes<- bind_rows(DFMonedaAnioMes,DFMonedaAnioMesAux)
        } else{
          
          # for(j in 1: length(MonedaJsonAnio$serie) ){
          urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',ListaMonedas[i],'/',ListAnio[k])
          GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
          MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
          #Itero sobre json para transformar a DF
          for(j in 1:length(MonedaJsonAnio$serie)){
            if(j==1){
              DFMonedaAnioDia <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDia <-setNames(DFMonedaAnioDia,  c("moneda", "cambioclp", "fecha"))
            }else{
              DFMonedaAnioDiaAux <- data.frame(MonedaJsonAnio$codigo, MonedaJsonAnio$serie[[j]]$valor, MonedaJsonAnio$serie[[j]]$fecha , stringsAsFactors = FALSE)
              DFMonedaAnioDiaAux <-setNames(DFMonedaAnioDiaAux,  c("moneda", "cambioclp", "fecha"))
              DFMonedaAnioDia<- bind_rows(DFMonedaAnioDia,DFMonedaAnioDiaAux)
            }
          }
          #Llevo cambio de moneda diaria a mensual:
          DFMonedaAnioDia <- DFMonedaAnioDia %>% mutate(., anio = year(fecha)) %>% mutate(., mes = month(fecha))%>%
            arrange(fecha)
          DFMonedaAnioDia <- DFMonedaAnioDia[!duplicated(DFMonedaAnioDia[,c('anio', 'mes')]),]
          DFMonedaAnioDia$anio <- NULL
          DFMonedaAnioDia$mes <- NULL
          DFMonedaAnioMesAux <- DFMonedaAnioDia
          #Se agrega a DF final
          DFMonedaAnioMes<- bind_rows(DFMonedaAnioMes,DFMonedaAnioMesAux)
        }

      }
      
    }
    
  }
  DFMonedaAnioMes
}


