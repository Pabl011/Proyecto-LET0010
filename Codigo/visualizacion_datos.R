


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

datos <- read_csv("Datos/chess_games.csv", col_select = c(3:5, 8, 10))

datos <- datos |> 
  mutate(rating_diff = datos$white_rating - datos$black_rating) |> 
  mutate(mean_rating = (datos$white_rating + datos$black_rating)/2)
#  rename(c("Turnos"="turns", "Estado de victoria"="victory_status", "Ganador" = "winner", "Rango Blanco"="white_rating", "Rango Negro"="black_rating", "Diferencia de rangos"="rating_diff"))


datos <- datos |> 
  filter(datos$time_increment == "10+0")


datos |> 
  ggplot(aes(x = turns, fill=mean_rating, colour = mean_rating)) +
  geom_density(alpha= 0.6)+
  scale_color_manual(values=c("black", "black", "black")) +
  scale_fill_manual(values=c("black", "slategray3", "white")) 

datos |> 
  ggplot(aes(x = turns, y = mean_rating)) +
  geom_point(shape = 1)

datos |> 
  ggplot(aes(x = turns, y = mean_rating)) +
  geom_point(shape = 1)

hist(datos$mean_rating, breaks=100, xlim = c(816, 2476), ylim = c(0, 1000))
abline(v= 1500, h = 740)
mean(datos$mean_rating)

datos |> 
  ggplot(aes(x = mean_rating, fill=winner, colour = winner)) +
  geom_histogram() +
  xlim(816, 2476) +
  scale_color_manual(values=c("black", "black", "black")) +
  scale_fill_manual(values=c("black", "slategray3", "white")) 

head(sort(table(datos$mean_rating), decreasing = T))

#Visualización de datos ----

sort(table(datos$time_increment), decreasing = T)

table(datos$turns)
table(datos$winner)
table(datos$rated)
##Relación entre la cantidad de turnos y el ganador de la partida. ----

datos |> 
  filter(winner %in% c("Black","White")) |> 
  ggplot(aes(x = turns, y = winner)) +
  labs(title="Titulo", subtitle = "sexo", x = "Cantidad de turnos", y = "Ganador") + 
  geom_boxplot(fill = c("black","white"), outlier.colour="black", outlier.shape=16,
               outlier.size=1.5, notch=FALSE ) +
  scale_x_continuous(breaks = seq(0, 350, by = 50))

datos |> #INTERESA: Saber si Influye el color del equipo en una partida de larga duración
  ggplot(aes(x = turns, y = winner)) +
  labs(title="Titulo") + 
  geom_boxplot(fill = c("black", "gray", "white"), outlier.colour="black", outlier.shape=16,
               outlier.size=1.5, notch=FALSE ) +
  scale_x_continuous(breaks = seq(0, 350, by = 50))


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

datos |> #INTERESA: Determinar si es el ELO un sistema eficaz para medir el nivel de competencia de un jugador y la efectividad del emparejamiento.
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

datos |> #INTERESA: Determinar si este juego es realmente equilibrado o se encuentra sesgado en favor de uno de los bandos.
  ggplot(aes(x = rating_diff, y = winner)) +
  labs(title="Titulo") + 
  geom_boxplot(fill = c("black", "gray", "white"), outlier.colour="black", outlier.shape=16,
               outlier.size=1.5, notch=FALSE ) #INTERESA: Conocer si Influye la diferencia de ELO de los jugadores en el ganador de la partida.


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
  ggplot(aes(x = white_rating, y = victory_status)) +
  labs(title="Titulo") + 
  geom_boxplot( outlier.colour="black", outlier.shape=0,
                outlier.size=1.5, notch=FALSE )

datos |> 
  ggplot(aes(x = black_rating, y = victory_status)) +
  labs(title="Titulo") + 
  geom_boxplot( outlier.colour="black", outlier.shape=0,
                outlier.size=1.5, notch=FALSE )

datos |> 
  ggplot(aes(x = rating_diff, y = victory_status)) +
  labs(title="Titulo") + 
  geom_boxplot( outlier.colour="black", outlier.shape=0,
                outlier.size=1.5, notch=FALSE )

table(datos$victory_status)


##Relación entre la cantidad de turnos y la diferencia de ELO. ----

datos |> 
  ggplot(aes(x = turns, y = white_rating)) +
  geom_point(shape = 1)

datos |> 
  ggplot(aes(x = turns, y = black_rating)) +
  geom_point(shape = 1)

datos |> #INTERESA
  ggplot(aes(x = turns, y = rating_diff)) +
  geom_point(shape = 1)

##Relación entre el ganador y el resultado obtenido en la partida. ----

#No se puede realizar. Son dos nominales.




#TABLA ----

#install.packages("datos")
#install.packages("gt")
#install.packages("gtExtras")
#install.packages("gtsummary")
#install.packages("reactable")
#install.packages("kableExtra")
#install.packages("broom")
#install.packages("broom.mixed")
library(datos)
library(gt)
library(gtsummary)
library(broom)
library(dplyr)
library(tidyr)

data.frame(Variable = names(datos),
           Tipo = c("Cualitativa dicotómica", "Cuantitativa discreta", "Cualitativa nominal", "Cualitativa nominal",
                    "Cualitativa nominal", "Cuantitativa discreta", "Cuantitativa discreta", "Cuantitativa discreta"),
           Descripción = c(
             "Indica si la partida es clasificatoria o no.",
             "Corresponde a la cantidad de turnos que duró la partida.",
             "Corresponde al resultado de la partida, independientemente del ganador. Puede ser empate, rendición, falta de tiempo o jaque mate.",
             "Corresponde al equipo ganador de la partida. En caso de haber un empate, no existe ganador.",
             "Corresponde a la modalidad de juego elegida para la partida. La modalidad refiere a la cantidad de tiempo en minutos que tiene un jugador para ejecutar su turno y la adición de tiempo en segundos conforme se ejecuta un turno.",
             "Puntos de clasificación ELO del contrincante del equipo blanco. Representa el nivel competitivo del jugador.",
             "Puntos de clasificación ELO del contrincante del equipo negro. Representa el nivel competitivo del jugador.",
             "Diferencia de ELO entre el jugador blanco y negro. Si su valor es positivo, indica que el jugador del equipo blanco tiene mayor ELO, y viceversa." 
           )) |> 
  gt() |> 
  tab_header(title = "Base de datos de Partidas de ajedrez en línea",
             subtitle = "Provenientes de la plataforma Lichess") |> 
  tab_source_note(source_note = "Fuente: Kaggle.com")



