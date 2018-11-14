
#### PREPROCES SCRIPT
library(dplyr)
library(stringr)
library(ggmap)
library(leaflet)
source("functions/Media_Tramos.R")
source("functions/Tramo_a_Hora.R")


key <- read.delim("google_key.txt")
goog_key(key)

data=read.csv("data/AccidentesBicicletas_2018.csv", sep=";", header = T, skip = 2) 
names(bikeData)[6] <- "NUMERO"

data <-  data %>% 
    mutate(DIRECCION.COMPLETA=paste(str_trim(LUGAR.ACCIDENTE), str_trim(NUMERO), "MADRID, SPAIN", sep=", ") %>% 
               str_replace("NA, ", "") %>% 
               str_replace(" - ", " CON "))

data <- data %>% mutate_geocode(location = DIRECCION.COMPLETA)

data <- MediaTramos(data) 
data <- Tramo_a_Hora(data)

data <- data %>% 
    mutate(nuevosTramosEdad = ifelse(mediaTramos<30,"15-29",
                                     ifelse(between(mediaTramos,30,50),"30-49",
                                            ifelse(mediaTramos>50,"50+","DESCONOCIDO"))))


data %>% saveRDS(file="data/bikes.RDS")
