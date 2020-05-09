library(tidyverse)
library(ggplot2)

Base_de_Datos_FCQ_Cuanti_SDAKDUK %>% ggplot(aes(Edad, ..count..)) + geom_density(fill = "cadetblue1") + theme_economist()

#EJERCICIO 2 CON FACTORIALES

media1 <- Base_al_dia  %>% summarize(rate = sum(Estado_Animo_Factorial)/540) %>% .$rate
media2 <- Base_al_dia  %>% summarize(rate = sum(Salud_contenidonatu_factorial)/540) %>% .$rate
media3 <- Base_al_dia  %>% summarize(rate = sum(etica_contenidonatu_factorial)/540) %>% .$rate
media4 <- Base_al_dia  %>% summarize(rate = sum(etica_familiaridad_factorial)/540) %>% .$rate
media5 <- Base_al_dia  %>% summarize(rate = sum(precio_conveniencia_factorial)/540) %>% .$rate
media6 <- Base_al_dia  %>% summarize(rate = sum(conveniencia_factorial)/540) %>% .$rate
media7 <- Base_al_dia  %>% summarize(rate = sum(atractivosenso_factorial)/540) %>% .$rate
media8 <- Base_al_dia  %>% summarize(rate = sum(control_peso_factorial)/540) %>% .$rate
media9 <- Base_al_dia  %>% summarize(rate = sum(fam_conveniencia_factorial)/540) %>% .$rate


data <- data.frame(Categoria=c("Estado de Animo","Salud y Contenido Natural","Etica y Contenido Natural","Etica y Familiaridad",
                               "Precio y Conveniencia", "Conveniencia", "Atractivo Sensorial", "Control de Peso", 
                               "Familiaridad y Conveniencia"),
                   media=c(media1,media2,media3,media4,media5,media6,media7, media8, media9))
data$Categoria <- factor(data$Categoria, levels = data$Categoria[order(data$media)])
data %>% ggplot(aes(x=Categoria, y=media, fill=Categoria)) + 
  geom_bar(position = "dodge", stat="identity", aes(fill = Categoria)) + 
  geom_text(aes(label=round(media,digits = 2)), position=position_dodge(width=0.1), vjust=-0.5) +
  geom_hline(yintercept=4, linetype="dashed", color = "red") +
  scale_y_continuous(name="Valor medio de las categorías", limits=c(0,7), breaks = c(2,4,5,7),
                     label = c("2-Bajo", "4", "5-Medio", "7-Alto")) +
  theme_economist() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + guides(fill=FALSE) +
  scale_fill_viridis(discrete = TRUE, option = "D")
library(ggthemes)
install.packages("viridis")
library(viridis)

#EJERCICIO 2.1 CON FACTORIALES
library(haven)
Base_al_dia <- read_sav("Base al dia.sav")

rango11 <- length(which(Base_al_dia$Estado_Animo_Factorial_rango==1))
rango12 <- length(which(Base_al_dia$Estado_Animo_Factorial_rango==2))
rango13 <- length(which(Base_al_dia$Estado_Animo_Factorial_rango==3))
Estado_Animo <- c(rango11, rango12,rango13)

m <- 3
Estado_Animo <- vector(length=m)
for (i in 1:m){
  compute_s_n <- function(i){
    length(which(Base_al_dia$Estado_Animo_Factorial_rango==i))
  }
  Estado_Animo[i] <- compute_s_n(i)
}
Salud_Contenido_Natural <- vector(length=m)
for (i in 1:m){
  compute_s_n <- function(i){
  length(which(Base_al_dia$Salud_contenidonatu_factorial_rango==i))
  }
  Salud_Contenido_Natural[i] <- compute_s_n(i)
}
Etica_Contenido_Natural <- vector(length=m)
for (i in 1:m){
  compute_s_n <- function(i){
    length(which(Base_al_dia$etica_contenidonatu_factorial_rango==i))
  }
  Etica_Contenido_Natural[i] <- compute_s_n(i)
}
Etica_Familiaridad <- vector(length=m)
for (i in 1:m){
  compute_s_n <- function(i){
    length(which(Base_al_dia$etica_familiaridad_factorial_rango==i))
  }
  Etica_Familiaridad[i] <- compute_s_n(i)
}
Precio_Conveniencia <- vector(length=m)
for (i in 1:m){
  compute_s_n <- function(i){
    length(which(Base_al_dia$precio_conveniencia_factorial_rango==i))
  }
  Precio_Conveniencia[i] <- compute_s_n(i)
}
Conveniencia <- vector(length=m)
for (i in 1:m){
  compute_s_n <- function(i){
    length(which(Base_al_dia$conveniencia_factorial_rango==i))
  }
  Conveniencia[i] <- compute_s_n(i)
}
Atractivo_Sensorial <- vector(length=m)
for (i in 1:m){
  compute_s_n <- function(i){
    length(which(Base_al_dia$atractivosenso_factorial_rango==i))
  }
  Atractivo_Sensorial[i] <- compute_s_n(i)
}
Control_Peso <- vector(length=m)
for (i in 1:m){
  compute_s_n <- function(i){
    length(which(Base_al_dia$control_peso_factorial_rango==i))
  }
  Control_Peso[i] <- compute_s_n(i)
}
Familiaridad_Conveniencia <- vector(length=m)
for (i in 1:m){
  compute_s_n <- function(i){
    length(which(Base_al_dia$fam_conveniencia_factorial_rango==i))
  }
  Familiaridad_Conveniencia[i] <- compute_s_n(i)
}
##GRAFICO###
Categoria <- c(rep("Estado de Animo", 3),rep("Salud y Contenido Natural",3),rep("Etica y Contenido Natural",3)
               ,rep("Etica y Familiaridad", 3), rep("Precio y Conveniencia",3), rep("Conveniencia",3)
               ,rep("Atractivo Sensorial",3), rep("Control de Peso", 3),rep("Familiaridad y Conveniencia",3))
Tipo <- as.factor(rep(c("bajo" , "medio" , "alto") , 9))
Valor <- (c(Estado_Animo, Salud_Contenido_Natural, Etica_Contenido_Natural, Etica_Familiaridad
           ,Precio_Conveniencia,Conveniencia, Atractivo_Sensorial, Control_Peso, Familiaridad_Conveniencia))
data3 <- data.frame(Categoria, Tipo, Valor)
data3$Tipo <- factor(data3$Tipo, levels = c("alto", "medio", "bajo"))
data3$Categoria <- factor(data3$Categoria, levels = c("Atractivo Sensorial", "Precio y Conveniencia", 
                                                      "Familiaridad y Conveniencia", "Salud y Contenido Natural",
                                                      "Control de Peso", "Etica y Contenido Natural",
                                                      "Conveniencia", "Estado de Animo", "Etica y Familiaridad"))
data3 %>% ggplot(aes(Categoria, Valor, fill = Tipo)) + geom_bar(stat = "identity",color='black',width=0.9) + theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(breaks = c(0,200,400),  label = c("0%", "38%", "74%")) +
  scale_fill_viridis(discrete = TRUE, option = "D") + ylab("Porcentaje") + 
  geom_text(aes(label = paste0(round(Valor/540*100, 2),"%")),  position = position_stack(vjust = 0.5), size = 4, color = "brown") 
  


data4 <- data.frame(Tipo = c("bajo" , "medio" , "alto") , Estado_Animo, Salud_Contenido_Natural, Etica_Contenido_Natural, Etica_Familiaridad,
                    Precio_Conveniencia, Conveniencia, Atractivo_Sensorial, Control_Peso, Familiaridad_Conveniencia)
data4$Tipo <- factor(data4$Tipo, levels = c("alto", "medio", "bajo"))
 









