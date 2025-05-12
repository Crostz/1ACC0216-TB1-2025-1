rm(list=ls(all=TRUE));
graphics.off()
cat("\014")

library(ggplot2)
library(cowplot)
library(patchwork)
library(ggpubr)
library(dplyr)

setwd("C:/Users/Mauricio/Documents/QUINTO CICLO/Data Science")
datos<-read.csv('hotel_bookings.csv', header = TRUE, stringsAsFactors = FALSE,sep=',', dec='.', na.strings = "NULL")

# Inspeccionar datos
str(datos)
summary(datos)

colSums(is.na(datos))

# Eliminar variable company
datos$company<-NULL

# Calcular la moda de la variable country
moda_country<-names(sort(table(datos$country), decreasing=TRUE))[1]
# Reemplazamos los datos faltantes en la variable country por la moda
datos$country[is.na(datos$country)]<-moda_country

# Calculamos la media de la variable agent sin contar los datos faltantes
media_agent<-mean(datos$agent, na.rm = TRUE)
# Reemplazar los datos faltantes por la media
datos$agent[is.na(datos$agent)]<-media_agent

# Comprobar si aún hay datos faltantes
colSums(is.na(datos))


# Outliers
# Usaremos gráfico de cajas para los Outliers
#lead_time

# Gráfico de cajas para ver los datos atípicos
g_at<-ggplot(datos, aes(x=lead_time)) + geom_boxplot(fill="steelblue") +
  labs(title="Boxplot",) + theme_classic()
g_at

# Para la variable adr
g_at_adr<-ggplot(datos, aes(x=adr)) + geom_boxplot(fill="steelblue") +
  labs(title="Outliers de lead_time",) + theme_classic()
g_at_adr


# Para la variable stays_in_week_nights
g_at_siwn<-ggplot(datos, aes(x=stays_in_week_nights)) + 
  geom_boxplot(fill="steelblue") + labs(title="Outliers de stays_in_weeks_nights",)+
  theme_classic()
g_at_siwn


# Tratamiento de Outliers
# Se reemplazarán los datos atípicos por la mediana de cada variable tratada
# Variable lead_time
Q1<-quantile(datos$lead_time, 0.25)
Q3<-quantile(datos$lead_time, 0.75)

IQR<-Q3-Q1
lower_bound<-Q1 - 1.5 * IQR
upper_bound<-Q3 + 1.5 * IQR

lead_time_median<-median(datos$lead_time)

# Reemplazamos en nuestra variable lead_time
datos$lead_time_imputed<-ifelse(datos$lead_time < lower_bound | datos$lead_time > upper_bound,
                                lead_time_median, datos$lead_time)

# Mismo proceso con variable adr
Q1_adr<-quantile(datos$adr, 0.25)
Q3_adr<-quantile(datos$adr, 0.75)

IQR_adr<-Q3_adr - Q1_adr
lower_bound_adr<-Q1_adr - 1.5 * IQR_adr
upper_bound_adr<-Q3_adr + 1.5 * IQR_adr

adr_median<-median(datos$adr)
# Reemplazamos en una nueva variable
datos$adr_imputed<-ifelse(datos$adr < lower_bound_adr | datos$adr > upper_bound_adr,
                          adr_median,
                          datos$adr)


# Último proceso: variable stays_in_week_nights
Q1_stays_in_week_nights<-quantile(datos$stays_in_week_nights, 0.25)
Q3_stays_in_week_nights<-quantile(datos$stays_in_week_nights, 0.75)

IQR_stays_in_week_nights<-Q3_stays_in_week_nights - Q1_stays_in_week_nights

lower_bound_stays_in_week_nights<-Q1_stays_in_week_nights - 1.5 * IQR_stays_in_week_nights
upper_bound_stays_in_week_nights<-Q3_stays_in_week_nights + 1.5 * IQR_stays_in_week_nights

stays_in_week_nights_median<-median(datos$stays_in_week_nights)

# Reemplazamos en una nueva variable
datos$stays_in_week_nights_imputed<-ifelse(datos$stays_in_week_nights < lower_bound_stays_in_week_nights |
                                             datos$stays_in_week_nights > upper_bound_stays_in_week_nights,
                                           stays_in_week_nights_median, datos$stays_in_week_nights)

df$arrival_date_month<-as.factor(df$arrival_date_month)
df$meal<-as.factor(df$meal)
df$hotel<-as.factor(df$hotel)
df$country<-as.factor(df$country)
df$market_segment<-as.factor(df$market_segment)
df$distribution_channel<-as.factor(df$distribution_channel)
df$reserved_room_type<-as.factor(df$reserved_room_type)
df$assigned_room_type<-as.factor(df$assigned_room_type)
df$deposit_type<-as.factor(df$deposit_type)
df$agent<-as.factor(df$agent)
df$customer_type<-as.factor(df$customer_type)
df$reservation_status<-as.factor(df$reservation_status)
datos$reservation_status_date<-as.Date(datos$reservation_status_date)

col(datos)
row(datos)

#Con na.strings=c("NULL","", " ")
#sum(is.na(df)) # Suma de valores NULL o nulos en el dataset
#df[!complete.cases(df),]
resumen<-sapply(df, function(col){
  sum(col == "NULL" | col == "" | col == " ")
})
resumen

lapply(df, function(col) table(col, useNA="ifany"))

# 1. ¿Cuántas reservas se realizan por tipo de hotel?
# ¿Qué tipo de hotel prefiere la gente?
table(datos$hotel)
datos_reservado<-datos[datos$is_canceled == 0 & datos$reservation_status == "Check-Out",] # Tomar en cuenta exclusivamente las filas con reserva no cancelada
datos_reservado

ggplot(datos_reservado, aes(x=hotel, fill=hotel)) + geom_bar() + theme_classic() + 
  labs(title="Gráfico de barras: reservas por tipo de hotel", x = "Tipo de hotel",
       y = "Cantidad de reservas") + geom_text(stat="count",
                                               aes(label =..count..), vjust=-0.5)
# Tipo de hotal preferido: City Hotel (46 228 reservas)

# 2. ¿Está aumentando la demanda con el tiempo?
reservas_por_fecha<-as.data.frame(table(datos_reservado$reservation_status_date))
colnames(reservas_por_fecha)<-c("fecha", "count")
reservas_por_fecha<-reservas_por_fecha[order(reservas_por_fecha$fecha),]
reservas_por_fecha$fecha<-as.Date(reservas_por_fecha$fecha) # Convertir fecha a variable tipo date

# group = 1: todos los puntos pertenecen a un mismo grupo
ggplot(reservas_por_fecha, aes(x=fecha, y = count, group = 1)) + theme_minimal() + 
  labs(title="Evolución de reservas a través del tiempo", x ="Mes y año", y="Número de reservas válidas") +
  geom_line(color="steelblue") + geom_smooth(method="loess", se =FALSE, color="darkred",linetype="dashed") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# La demanda alcanza niveles constantes a finales de 2017, pero en los últimos meses (septiembre-octubre), cae estrepitosamente.


# 3. ¿Cuáles son las temporadas de reservas (alta, media, baja)?
datos$arrival_date_month <- factor(
  datos$arrival_date_month,
  levels = c("January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December"))

ggplot(datos, aes(x=arrival_date_month)) + theme_classic() + geom_bar(fill="pink") +
  labs(title="Gráfica: Reservas por meses (temporada)", x = "Mes", y = "Cantidad de reservas")

# 4. ¿Cuál es la duración promedio de las estancias por tipo de hotel?
datos$duracion_estancia<-datos$stays_in_week_nights_imputed + datos$stays_in_weekend_nights
estancia_city<-datos[datos$hotel == "City Hotel",]
estancia_resort<-datos[datos$hotel == "Resort Hotel",]
#promedio_estancia<-mean(datos$duracion_estancia, na.rm=TRUE)
promedio_estancia_city<-mean(estancia_city$duracion_estancia)
promedio_estancia_resort<-mean(estancia_resort$duracion_estancia)
round(promedio_estancia_city)
round(promedio_estancia_resort)

# 5. ¿Cuántas reservas incluyen niños y/o bebés?
table(datos$children)
datos$children<-as.numeric(datos$children)
data_infantes<-datos[datos$children > 0 | datos$babies > 0,]
data_infantes$baby_child<-data_infantes$children + data_infantes$babies
#data_infantes$baby_child<-as.factor(data_infantes$baby_child)
data_infantes<-data_infantes[!is.na(data_infantes$baby_child),]
nrow(data_infantes)

# Gráfico de barras
ggplot(data_infantes[!is.na(data_infantes$baby_child),], aes(x=baby_child)) + geom_bar(fill = "lightgreen") + theme_minimal() +
  labs(title="Reservas: Niños y/o Bebés", x="Número de niños y bebés", y = "Cantidad de reservas") +
  geom_text(stat="count", aes(label=..count..), vjust = -0.5)

# 6. ¿Es importante contar con espacios de estacionamiento?
#data_parking<-datos[datos$required_car_parking_spaces > 0,]
ggplot(datos, aes(x=required_car_parking_spaces)) + geom_bar(fill="red") +
  theme_minimal() + labs(title="Gráfico: Estacionamientos requeridos por reserva",
                         x = "Número de estacionamientos requeridos", y = "Cantidad de reservas") +
  geom_text(stat="count", aes(label=..count..), vjust= -0.5) +
  scale_x_continuous(breaks=seq(min(datos$required_car_parking_spaces), 
                                max(datos$required_car_parking_spaces), by = 1))
table(datos$required_car_parking_spaces)

# 7. ¿En qué meses del año se producen más cancelaciones de reservas?
data_cancelada<-datos[datos$is_canceled == 1,]
data_cancelada$arrival_date_month <- factor(
  data_cancelada$arrival_date_month,
  levels = c("January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December")
)

ggplot(data_cancelada, aes(x = arrival_date_month)) + theme_minimal() +
  geom_bar(fill="orange") + labs(title="Gráfico: Número de cancelaciones por mes",
                                 x="Mes", y="Número de cancelaciones")

# 8. Plantear una pregunta del equipo
# ¿Qué canal de distribución genera en promedio mayores ingresos por habitación (ADR)?
datos$distribution_channel<-as.factor(datos$distribution_channel)
datos<-datos[datos$distribution_channel != "Undefined",]
table(datos$distribution_channel)

ggplot(datos, aes(x = distribution_channel, y = adr, fill = distribution_channel)) + theme_minimal()+
  labs(title="Ingresos por habitación (ADR) por canal de distribución", x = "Canal de distribución",
       y = "Ingreso por habitación promedio (ADR)") + geom_bar(stat = "summary", fun = mean)
