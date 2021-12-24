library(tidyverse)
library(rvest)


rm(list = ls())

scrapear <- function(pagina,css){
  output <- pagina %>% 
    html_nodes(xpath=css) %>%
    html_text()
  return(output)
}

descargar_leer <- function(url){
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE,method = "wininet")
  pagina_web <- read_html("scrapedpage.html")
  return(pagina_web)
}

formatear_letra <- function(letra){
  return(chartr("аимсздкожэя", "AEIOUAEIOUN", toupper(letra)))
}

setwd("C:/Users/Usuario/Desktop/BCU/scrapping/alquileres")

base_ML_alquileres <- read_csv("base_ML_alquileres.csv")


url <- "https://listado.mercadolibre.com.uy/inmuebles/alquiler/"
pagina_web <- read_html(url)

raw <- pagina_web %>% html_nodes('div.ui-search-result__content-wrapper') %>% html_text()
precio <- pagina_web %>% html_nodes('div.ui-search-item__group.ui-search-item__group--price') %>% html_text()
ubicacion <- pagina_web %>% html_nodes('div.ui-search-item__group.ui-search-item__group--location') %>% html_text()
atributos <- pagina_web %>% html_nodes('div.ui-search-item__group.ui-search-item__group--attributes') %>% html_text()
titulo <- pagina_web %>% html_nodes('div.ui-search-item__group.ui-search-item__group--title') %>% html_text()
selector <- pagina_web %>% html_nodes('div.ui-search-pagination') %>% html_text()

i <- 0

while(str_detect(toupper(selector),"SIGUIENTE")){
  i <- i + 1
  url <- paste0("https://listado.mercadolibre.com.uy/inmuebles/alquiler/_Desde_",i*48+1)
  pagina_web <- read_html(url)
  
  raw <- c(raw,pagina_web %>% html_nodes('div.ui-search-result__content-wrapper') %>% html_text())
  precio <- c(precio,pagina_web %>% html_nodes('div.ui-search-item__group.ui-search-item__group--price') %>% html_text())
  ubicacion <- c(ubicacion,pagina_web %>% html_nodes('div.ui-search-item__group.ui-search-item__group--location') %>% html_text())
  atributos <- c(atributos,pagina_web %>% html_nodes('div.ui-search-item__group.ui-search-item__group--attributes') %>% html_text())
  titulo <- c(titulo,pagina_web %>% html_nodes('div.ui-search-item__group.ui-search-item__group--title') %>% html_text())
  selector <- pagina_web %>% html_nodes('div.ui-search-pagination') %>% html_text()
}

base_ML_alquileres_nueva <- data.frame(Fecha = rep(Sys.Date(),length(raw)),raw = formatear_letra(raw), precio = formatear_letra(precio), ubicacion = formatear_letra(ubicacion),atributos = formatear_letra(atributos), titulo = formatear_letra(titulo))

# base_ML_alquileres <- base_ML_alquileres_nueva
base_ML_alquileres <- rbind(base_ML_alquileres,base_ML_alquileres_nueva)

write.csv(base_ML_alquileres,"base_ML_alquileres.csv",row.names = F,fileEncoding = "UTF-8")


# ################### completa --------------------
# 
# url <- "https://apartamento.mercadolibre.com.uy/MLU-480450607-apartamento-alquiler-nostrum-bay-montevideo-imasuy-r-_JM#position=1&search_layout=grid&type=item&tracking_id=db3f34d8-a84e-4345-ba9c-f3b9a1f7f6b5"
# pagina_web <- read_html(url)
# pagina_web %>% html_nodes('div.ui-pdp-container__row.ui-pdp-container__row--technical-specifications') %>% html_attrs()
# 




