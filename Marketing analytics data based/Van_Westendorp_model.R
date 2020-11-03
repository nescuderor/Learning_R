### Cargar la base de datos
library(haven)
PRICING <- read_sav("C:/Users/nicol/OneDrive/Escritorio/PRICING.sav")
View(PRICING)

#Instalación de paquetes
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("labelled")
install.packages("magrittr")

## Modelamiento
#Exploración de  la base de datos
library(tidyverse)

PRICING %>% glimpse
PRICING %>% str
PRICING %>% View
PRICING %>% names

#Exploración de etiquetas
library(labelled)

var_label(PRICING)
var_label(PRICING$VAR00013)

#Renombrando variables
data_fr <- PRICING %>% 
  transmute(
    Too_cheap = as_factor(VAR00024),
    Cheap = as_factor(VAR00025),
    Expensive = as_factor(VAR00026),
    Too_expensive = as_factor(VAR00027),
  )

View(data_fr)
data.class(data_fr)

#Tratamientos de NAs
Too_cheap <- data_fr$Too_cheap[!is.na(data_fr$Too_cheap)]
Cheap <- data_fr$Cheap[!is.na(data_fr$Cheap)]
Expensive <- data_fr$Expensive[!is.na(data_fr$Expensive)]
Too_expensive <- data_fr$Too_expensive[!is.na(data_fr$Too_expensive)]

new_datafr <- data.frame(Too_cheap, Cheap, Expensive, Too_expensive)
View(new_datafr)

#Conversión a numeros
llave <- c(
  "1 MILLÓN" = 1000000,
  "2 MILLONES" = 2000000,
  "3 MILLONES" = 3000000,
  "4 MILLONES" = 4000000,
  "5 MILLONES" = 5000000,
  "6 MILLONES" = 6000000,
  "7 MILLONES" = 7000000,
  "8 MILLONES" = 8000000,
  "9 MILLONES" = 9000000,
  "10 MILLONES" = 10000000,
  "11 MILLONES" = 11000000,
  "12 MILLONES" = 12000000,
  "13 MILLONES" = 13000000,
  "14 MILLONES" = 14000000,
  "15 MILLONES" = 15000000
)

New_pricing.df <- new_datafr %>%
  transmute(
    Too_cheap = llave[as.character(Too_cheap)],
    Cheap = llave[as.character(Cheap)],
    Expensive = llave[as.character(Expensive)],
    Too_expensive = llave[as.character(Too_expensive)],
  )
View(New_pricing.df)

#Frecuencias <- Establecimiento de las frecuencias acumuladas
Rangos <-  seq(0, 15000000, 1000000)

library(magrittr)
Pricing_Model <- New_pricing.df %$%
  tibble(
    Rangos = Rangos,
    Too_cheap = outer(Rangos, Too_cheap, "<=") %>% rowMeans(),
    Cheap = outer(Rangos, Cheap, "<=") %>% rowMeans(),
    Expensive = outer(Rangos, Expensive, ">=") %>% rowMeans(),
    Too_expensive = outer(Rangos, Too_expensive, ">=") %>% rowMeans(),
  )
write.table(Pricing_Model, file = "Van_Westendorp.csv", row.names = FALSE, col.names = TRUE,
            dec = ",")

#Desarrollo final del modelo
Van_Westendorp_model <- Pricing_Model %>%
  gather(key = "Variable",value = "Valor", Too_cheap:Too_expensive)

#Proyección final
Van_Westendorp_graph <- Van_Westendorp_model %>%
  ggplot + aes(x = Rangos, y = Valor, colour = Variable) + geom_line() + 
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent) + 
  scale_x_continuous(breaks = seq(0,15000000,1000000), labels = scales::comma) +  
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

Van_Westendorp_graph