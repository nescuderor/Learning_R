######################################################################################################################
################################################ Data scraping con R #################################################
######################################################################################################################

## Paquetes necesarios
install.packages("rvest")
install.packages("dplyr")
install.packages("tidyverse")

## Extracción de información específica
# Librerías
library("rvest")
library("dplyr")
library("tidyverse")

##Iteración/loops a través de multiples páginas de resultados
get_cast <- function(movie_link){
  movie_page <- read_html(movie_link)
  movie_cast <- movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = "-")
  return(movie_cast)
}

movies <- data.frame()

for(page_result in seq(from = 1, to = 101, by = 50)){
  link = paste("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
               page_result, "ref_=adv_nxt", sep="")
  
  pagina <- read_html(link)
  
  name <- pagina %>% html_nodes(".lister-item-header a") %>% html_text
  year <- pagina %>% html_nodes(".text-muted.unbold") %>% html_text
  rating <- pagina %>% html_nodes(".ratings-imdb-rating strong") %>% html_text
  synopsis <- pagina %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text
  movie_link <- pagina %>% html_nodes(".lister-item-header a") %>% 
    html_attr("href") %>% paste("https://www.imdb.com/", ., sep="")
  cast <- sapply(movie_link, FUN = get_cast)
  
  movies <- rbind(movies, data.frame(name,year,rating,synopsis,
                             movie_link,cast,stringsAsFactors = FALSE))
  print(paste("Begining page tittle number:", page_result))
}


  