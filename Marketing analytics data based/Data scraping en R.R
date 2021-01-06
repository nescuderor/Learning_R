######################################################################################################################
################################################ Data scraping con R #################################################
######################################################################################################################

## Paquetes necesarios
install.packages("rvest")
install.packages("dplyr")
install.packages("tidyverse")

## Extracción de información específica
#librerías
library("rvest")
library("dplyr")
library("tidyverse")

# Almacenar y guardar la Data
url <- "https://www.imdb.com/search/title/?title_type=movie&genres=horror&sort=user_rating,desc&explore=title_type,genres"
pagina <- read_html(url)

# Obtención de variables
name <- pagina %>% html_nodes(".lister-item-header a") %>% html_text
year <- pagina %>% html_nodes(".text-muted.unbold") %>% html_text
rating <- pagina %>% html_nodes(".ratings-imdb-rating strong") %>% html_text
synopsis <- pagina %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text

#Transmutación a data frame
df_01 <- data.frame(name, year, rating, synopsis, stringsAsFactors = FALSE)
###################################################################################################
# Para desarrollar un data frame es imperativo que las columnas tengan las mismas dimensiones.    #
# En un data farame se podrán añadir series como datos anexados o columnas individuales agregadas.#
###################################################################################################


##Navegación por redirecciones
#Selección y obtención de links validos
movie_link <- pagina %>% html_nodes(".lister-item-header a") %>% 
  html_attr("href") %>% paste("https://www.imdb.com/", ., sep="")
df_02 <- df_01 %>% data.frame(movie_link,stringsAsFactors = FALSE)

#Obtención del reparto principal
#Creación de función de busqueda
get_cast <- function(movie_link){
  movie_page <- read_html(movie_link)
  movie_cast <- movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = "-")
  return(movie_cast)
}

#Obtención de columna de cast
cast <- sapply(movie_link, FUN = get_cast)
df_03 <- df_02 %>% data.frame(cast, stringsAsFactors = FALSE)
View(df_03)

