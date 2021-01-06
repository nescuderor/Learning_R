######################################################################################################################
################################################ Data scraping con R #################################################
######################################################################################################################

## Paquetes necesarios
install.packages("rvest")
install.packages("dplyr")
install.packages("tidyverse")

## Extracción de información específica a partir de tablas
# Librerías
library("rvest")
library("dplyr")
library("tidyverse")

##Tablas en formato plano o estático
#Selección de item
tabla_01 <- "https://www.patriotsoftware.com/blog/accounting/average-cost-living-by-state/"
recurso_tabla01 <- read_html(tabla_01)

#Obtención de tabla
tabla01_R <- recurso_tabla01 %>% html_nodes("table")%>%
  html_table() %>% .[[1]]
View(tabla01_R)

##Tablas con formato de filtro
#Selección de item
tabla_02 <- "https://es.wikipedia.org/wiki/Anexo:Videojuegos_m%C3%A1s_vendidos_de_la_Nintendo_Switch"
recurso_tabla02 <- read_html(tabla_02)
     
#Obtención de tabla
tabla02_R <- recurso_tabla02 %>% html_nodes("table") %>% .[1] %>%
  html_table() %>% .[[1]]
