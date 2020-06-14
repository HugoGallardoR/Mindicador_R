library(jsonlite)
library(httr)

#Agregar a una lista una moneda en un anio en particular
#Monedas se deben escribir con comillas: "uf","utm","euro","dolar", etc
#Anios: 2010 en adelante
#Ejemplo de uso: ufAnual<- IndicadorAnual('uf', 2019)

IndicadorAnual <- function(moneda, anio){
  urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',moneda,'/',anio)
  GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
  MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
  MonedaJsonAnio
}

