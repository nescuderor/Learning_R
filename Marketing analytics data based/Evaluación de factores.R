### Cargar la base de datos
library(haven)
PRICING <- read_sav("C:/Users/nicol/OneDrive/Escritorio/PRICING.sav")
View(PRICING)

#Instalación de paquetes
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("labelled")
install.packages("magrittr")
install.packages("reshape")

### Modelamiento
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
    Alta_Calidad_U1 = as.double(VAR00010),
    Alta_Calidad_U2 = as.double(VAR00011),
    Alta_Calidad_U3 = as.double(VAR00012),
    P.Valor_U1 = as.double(VAR00013),
    P.Valor_U2 = as.double(VAR00014),
    P.Valor_U3 = as.double(VAR00015),
  )
new_dt <- data.frame(data_fr)

##Tratamiento de NAs y transformaciones iniciales
tail(new_dt)
new_dt <- new_dt[1:109,]

#Indexación por medio del remplazamiento por medias ante la existencia de NAs y NaNs
NAT1 <- mean(new_dt$Alta_Calidad_U1[!is.na(new_dt$Alta_Calidad_U1)]) #Tratamiento de NAs
NAT2 <- mean(new_dt$Alta_Calidad_U2[!is.na(new_dt$Alta_Calidad_U2)]) #Tratamiento de NAs
NAT3 <- mean(new_dt$Alta_Calidad_U3[!is.na(new_dt$Alta_Calidad_U3)]) #Tratamiento de NAs
NAT4 <- mean(new_dt$P.Valor_U1[!is.na(new_dt$P.Valor_U1)]) #Tratamiento de NAs
NAT5 <- mean(new_dt$P.Valor_U2[!is.na(new_dt$P.Valor_U2)]) #Tratamiento de NAs
NAT6 <- mean(new_dt$P.Valor_U3[!is.na(new_dt$P.Valor_U3)]) #Tratamiento de NAs

new_dt$Alta_Calidad_U1[is.na(new_dt$Alta_Calidad_U1)] <- NAT1
new_dt$Alta_Calidad_U2[is.na(new_dt$Alta_Calidad_U2)] <- NAT2
new_dt$Alta_Calidad_U3[is.na(new_dt$Alta_Calidad_U3)] <- NAT3
new_dt$P.Valor_U1[is.na(new_dt$P.Valor_U1)] <- NAT4
new_dt$P.Valor_U2[is.na(new_dt$P.Valor_U2)] <- NAT5
new_dt$P.Valor_U3[is.na(new_dt$P.Valor_U3)] <- NAT6


#Determinación de medias
AC_U1 <- mean(new_dt$Alta_Calidad_U1)
AC_U2 <- mean(new_dt$Alta_Calidad_U2)
AC_U3 <- mean(new_dt$Alta_Calidad_U3)
PV_U1 <- mean(new_dt$P.Valor_U1)
PV_U2 <- mean(new_dt$P.Valor_U2)
PV_U3 <- mean(new_dt$P.Valor_U3)

##Modelamiento y graficación
library(reshape)
final_dt <- data.frame(c(AC_U1, AC_U2, AC_U3), c(PV_U1, PV_U2, PV_U3), 
                       row.names = c("Universidad 1", "Universidad 2", "Universidad 3"))
final_dt <- rename(final_dt, c(c.AC_U1..AC_U2..AC_U3.="Alta_Calidad", 
                               c.PV_U1..PV_U2..PV_U3.="P.Valor"))
final_dt
#Cálculo del GAP
GAP <- final_dt$Alta_Calidad - final_dt$P.Valor
GAP_Percent <- GAP * 10
GAP_table <- data.frame(GAP, GAP_Percent, row.names = c("Universidad 1", "Universidad 2", "Universidad 3"))
GAP_table

GAP

#Graficación
library(ggplot2)
Dispersion <- ggplot(final_dt, aes(x = P.Valor, y = Alta_Calidad)) +
  geom_point() + geom_point(aes(colour = row.names(final_dt))) +
  scale_x_continuous(limits = c(0,10),
                     expand = c(0,0),
                     breaks = seq(0, 10, 1)) +
  scale_y_continuous(limits = c(0,10),
                     expand = c(0,0),
                     breaks = seq(0, 10, 1)) +
  geom_segment(aes(x = 0, 
                   xend = 10,
                   y = 0,
                   yend = 10)) +
  geom_text(aes(label=row.names(final_dt)),
            cex = 2, nudge_x = -0.4, nudge_y = 0.1 )
  
Dispersion
