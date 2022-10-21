


#Paquetes -----

#install.packages("gghighlight")
#install.packages("ggtext")
#install.packages("ggthemes")
#install.packages("showtext")
#install.packages("ggimage")
#install.packages("remotes")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("janitor")
#install.packages("here")
#install.packages("skimr")
#install.packages("broom")


library(readxl)
library(readr)
library(dplyr)        #un paquete para manejar datos
library(tidyr)        #paquete para ordenar datos 
library(stringr) 
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggtext)  #para editar anotaciones y etiquetas 
library(gghighlight)  #para destacar valores en un gráfico 
library(showtext)
library(ggthemes)

#Importación y limpieza de datos ----

datos <- read_csv("Datos/chess_games.csv", col_select = c(2:6, 8, 10))

datos <- datos |> 
  mutate(rating_diff = datos$white_rating - datos$black_rating) |> 
  filter(datos$time_increment == "10+0")


#Visualización de datos ----

sort(table(datos$time_increment), decreasing = T)

table(datos$turns)
table(datos$winner)

##Relación entre la cantidad de turnos y el ganador de la partida. ----

datos |> 
  filter(winner %in% c("Black","White")) |> 
  ggplot(aes(x = turns, y = winner)) +
  labs(title="Titulo", subtitle = "sexo", x = "Cantidad de turnos", y = "Ganador") + 
  geom_boxplot(fill = c("black","white"), outlier.colour="black", outlier.shape=16,
               outlier.size=1.5, notch=FALSE ) +
  scale_x_continuous(breaks = seq(0, 350, by = 50))

datos |> 
  ggplot(aes(x = turns, y = winner)) +
  labs(title="Titulo") + 
  geom_boxplot(fill = c("black", "gray", "white"), outlier.colour="black", outlier.shape=16,
               outlier.size=1.5, notch=FALSE )


##Diferencia de ELO y efectividad del emparejamiento entre contrincantes. ----

table(datos$rating_diff)
head(sort(datos$white_rating, decreasing = T))
head(sort(datos$black_rating, decreasing = T))


datos |> 
  ggplot(aes(x = white_rating, y = rating_diff)) +
  geom_point(shape = 1)

datos |> 
  ggplot(aes(x = black_rating, y = rating_diff)) +
  geom_point(shape = 1)

datos |> 
  ggplot(aes(x = white_rating, y = black_rating)) +
  geom_point(shape = 1)


##Relación del ELO con el ganador de la partida. ----

datos |> 
  ggplot(aes(x = white_rating, y = winner)) +
  labs(title="Titulo") + 
  geom_boxplot(fill = c("black", "gray", "white"), outlier.colour="black", outlier.shape=16,
               outlier.size=1.5, notch=FALSE )

datos |> 
  ggplot(aes(x = black_rating, y = winner)) +
  labs(title="Titulo") + 
  geom_boxplot(fill = c("black", "gray", "white"), outlier.colour="black", outlier.shape=16,
               outlier.size=1.5, notch=FALSE )


datos |> 
  ggplot(aes(x = rating_diff, y = rated)) +
  labs(title="Titulo") + 
  geom_boxplot(fill = c("darkslategray2", "darkolivegreen2"), outlier.colour="black", outlier.shape=1,
               outlier.size=1.5, notch=FALSE )


##Relación entre la cantidad de turnos y el resultado obtenido en la partida. ----

datos |> 
  ggplot(aes(x = turns, y = victory_status)) +
  labs(title="Titulo") + 
  geom_boxplot( outlier.colour="black", outlier.shape=1,
               outlier.size=1.5, notch=FALSE )

##Relación entre la diferencia de ELO y el resultado obtenido en la partida. ----

datos |> 
  ggplot(aes(x = rating_diff, y = victory_status)) +
  labs(title="Titulo") + 
  geom_boxplot( outlier.colour="black", outlier.shape=1,
                outlier.size=1.5, notch=FALSE )







