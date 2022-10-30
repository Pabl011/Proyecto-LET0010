


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

data.frame(Variable = c("Turnos", "Ganador", "Elo Blancas", "Elo Negras", "Diferencia de Elo", "Elo promedio"),
           Tipo = c("Cuantitativa discreta", "Cualitativa nominal", "Cuantitativa discreta", "Cuantitativa discreta", "Cuantitativa discreta", "Cuantitativa continua"),
           Descripción = c(
             "Corresponde a la cantidad de turnos que duró la partida.",
             "Corresponde al equipo ganador de la partida, ya sea blancas o negras. En caso de haber un empate, no existe ganador, por lo que queda clasificado como 'empate'.",
             "Puntos de clasificación Elo del contrincante del equipo blanco. Representa el nivel competitivo del jugador dentro de la plataforma.",
             "Puntos de clasificación Elo del contrincante del equipo negro. Representa el nivel competitivo del jugador dentro de la plataforma.",
             "Diferencia de Elo entre los contrincantes. Si su valor es positivo, indica que el jugador del equipo blanco tiene mayor Elo, y viceversa.",
             "Corresponde al promedio del Elo entre los dos jugadores."
           )) |> 
  gt() |> 
  tab_header(title = "Base de datos de Partidas de ajedrez en línea",
             subtitle = "Provenientes de la plataforma Lichess") |> 
  tab_source_note(source_note = "Fuente: Kaggle.com")
#FIGURA 1

datos |> #INTERESA: Determinar si es el ELO un sistema eficaz para medir el nivel de competencia de un jugador y la efectividad del emparejamiento.
  ggplot(aes(x = white_rating, y = black_rating)) +
  geom_point(shape = 16, size=0.8) +
  geom_abline(aes(intercept = mean(black_rating) - mean(white_rating),
                  slope = 1),
              linetype = 2, color = "red") +
  geom_point(aes(x= 1500,y= 1500), colour="red") +
  scale_x_continuous(breaks = seq(0, 3000, by = 250)) +
  labs(title = "Figura 1. Relación entre el Elo de dos jugadores en una partida.",
       x = "Elo Blancas", y = "Elo Negras")

#FIGURA 2

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

datos |> 
  ggplot(aes(x = mean_rating)) +
  geom_histogram(aes(y=..density..), color="black", fill="slategray3", breaks= seq(800, 2500, 25)) +
  #  xlim(800, 2500) +
  scale_x_continuous(breaks = seq(750, 2500, by = 250)) +
  geom_density(color = "black", fill = "cadetblue2", alpha = 0.15) +
  geom_vline(aes(xintercept=getmode(mean_rating), color="Moda"),
             linetype="dashed", size=0.8) +
  geom_vline(aes(xintercept=mean(mean_rating), color="Media"),
             linetype="dashed", size=0.8) +
  geom_vline(aes(xintercept=median(mean_rating), color="Mediana"),
             linetype="dashed", size=0.8) +
  scale_color_manual(name = "", values = c(Moda = "red", Media = "blue", Mediana = "green")) +
  labs(title = "Figura 2. Distribución del promedio de Elo entre ambos jugadores.",
       subtitle = "Cada punto representa una partida observada.",
       x = "Elo promedio", y = "Densidad")

#FIGURA 3

datos |> 
  ggplot(aes(x = rating_diff, fill=winner)) +
  geom_density(alpha= 0.6) +
  xlim(-1000, 1000) +
  #  scale_x_continuous(breaks = seq(-1500, 1500, by = 250)) +
  scale_color_manual(values=c("black", "black", "black")) +
  scale_fill_manual(name="Ganador", values=c("black", "slategray3", "white"),
                    labels = c("Negras", "Empate", "Blancas")) +
  labs(title = "Figura 3. Relación entre la diferencia de Elo y el equipo ganador.",
       subtitle = "Distribución de la diferencia de Elo respecto a cada resultado.",
       x = "Diferencia de Elo", y = "Densidad")

#FIGURA 4

datos |> 
  ggplot(aes(x = turns, fill=winner)) +
  geom_density(alpha= 0.6) +
  #  xlim(0, 350) +
  scale_color_manual(values=c("black", "black", "black")) +
  scale_fill_manual(name = "Ganador", values=c("black", "slategray3", "white"),
                    labels = c("Negras", "Empate", "Blancas")) +
  scale_x_continuous(breaks = seq(0, 350, by = 50)) +
  labs(title = "Figura 4. Relación entre el número de turnos y el equipo ganador.",
       subtitle = "Distribución del número de turnos respecto a cada resultado.",
       x = "Turnos", y = "Densidad")


#FIGURA 5

datos |> #INTERESA
  ggplot(aes(x = turns, y = rating_diff)) +
  geom_point(shape = 1) +
  geom_hline(yintercept=0, col="red3", linetype = 1) +
  scale_x_continuous(breaks = seq(0, 350, by = 50)) +
  labs(title = "Figura 5. Relación entre el número de turnos y la diferencia de Elo.",
       subtitle = "Cada punto representa una partida observada.",
       x = "Turnos", y = "Diferencia de Elo")

