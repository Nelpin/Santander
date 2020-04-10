library(readxl)
library(tidyverse)

data <- read_excel("bd_call_center_anonimized.xls")
#Del Shape del Dane de municipios obtengo los datos del municipio
#Paso el Shape a un dataframe simple
st_geometry(df_colombia)<-NULL
#Paso el código a numérico para cruzar los datos
df_colombia$MPIO_CCNCT<-as.numeric(df_colombia$MPIO_CCNCT)
#Hago el cruce entre las dos tablas
data_mun<-left_join(data,df_colombia, by=c("cod_mun"="MPIO_CCNCT"))

data_select<-data_mun%>%select(DPTO_CCDGO,cod_mun, MPIO_CNMBR, DPTO_CNMBR, direccion,fecha_call)

save(data_select, file = "data_municipios.Rdata")
load("data_municipios.Rdata")


df<-data_select%>%filter(DPTO_CCDGO<70)
write_csv(df, path="datos_call.csv")

#--------------------------------------------------------#
#                                                        #
#       CARGA DE BASE DE DATOS PROCESADA EN PHP          #
#                                                        #
#--------------------------------------------------------#

data_procesada <- read_excel("datos_call.xlsx")
data_procesada$latitud[data_procesada$latitud == "NULL"] <- NA
data_procesada$longitud[data_procesada$longitud == "NULL"] <- NA

summary(data_procesada)
data_procesada$latitud<-as.numeric(data_procesada$latitud)
data_procesada$longitud<-as.numeric(data_procesada$longitud)

data_procesada$Fecha<-as.Date(data_procesada$Fecha)

summary(data_procesada)

#Quito los que no están geolocalizados
data_limpia<-na.omit(data_procesada)
data_limpia<-data_limpia[,c(2:4,6,7,11)]


#Quito los duplicados
data_limpia <- data_limpia[!duplicated(data_limpia), ]

data_limpia<-data_limpia%>%filter(Departamento=="SANTANDER")

#Mapa
library(ggplot2)
library(gganimate)
library(magrittr)
library(sf)
library(pryr)

colombia<-read_sf("../../Shape/WGS84_MGN2019_00_COLOMBIA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")
santander<-colombia%>%filter(DPTO_CCDGO==68)
#Reduzco el shape para que el mapa no quede muy pesado y se demore la carga
santander <- st_simplify(santander, preserveTopology = TRUE, dTolerance = .02)


p<-ggplot(data = santander) +
  geom_sf() +
  geom_point(data = data_limpia, aes(x = longitud, y = latitud), color = "red") +
  coord_sf(xlim = c(-75, -72), ylim = c(5, 9), expand = FALSE)+
  labs(title = 'Fecha: {frame_time}') +
  transition_time(Fecha) +
  shadow_mark(alpha = 0.3, size = 0.5)

p

# Save at gif:
anim_save("mapa_santander.gif")

