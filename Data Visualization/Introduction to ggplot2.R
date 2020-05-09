library(tidyverse)
library(dslabs)
data(murders)
p <- ggplot(data=murders)

##SCATTERPLOT
murders %>% ggplot() + 
  geom_point(aes(population/10^6, total))

p + geom_point(aes(population,total))

#geom_label y geom_text añaden los labels a la gráfica. 

p + geom_point(aes(population/10^6,total)) + geom_text(aes(population/10^6, total, label = abb))
p + geom_point(aes(population/10^6,total)) + geom_label(aes(population/10^6, total, label = abb))

#change the size of the points 
p + geom_point(aes(population/10^6,total), size = 3) + geom_text(aes(population/10^6, total, label = abb))

#corriendo el label para que se pueda ver bien 
p + geom_point(aes(population/10^6,total), size = 3) + geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)

#simplificando el código 
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
  #se pueden ir agregando capas así 
p + geom_point(size=3) + geom_text(nudge_x = 1.5) 
  #se puede agregar texto así 
p + geom_point(size=3) + geom_text(aes(10,800,label="Hello there"), nudge_x = 1.5) 

#cambiando la escala 
p + geom_point(size=3) + geom_text(nudge_x = 0.05) + scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")
#mejor más pegado el label al punto 
p + geom_point(size=3) + geom_text(nudge_x = 0.025) + scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")
#es más facil usar 
p + geom_point(size=3) + geom_text(nudge_x = 0.025) + scale_x_log10() + scale_y_log10() 
#añadiendo etiquetas a los ejes y título al gráfico
p + geom_point(size=3) + geom_text(nudge_x = 0.025) + scale_x_log10() + scale_y_log10() +
  xlab("Population in millions (log scale)") + ylab("total murders (log scale)") + ggtitle("US Gun Murders")

#para ir simplificando 
p <- murders %>% ggplot(aes(population/10^6, total, label = abb), size = 3) + geom_text(nudge_x = 0.05) + 
  scale_x_log10() + scale_y_log10() +xlab("Population in millions (log scale)") + ylab("total murders (log scale)") + 
  ggtitle("US Gun Murders")
#añadir colores a los puntos 
p + geom_point(size=3, color="blue")
#añadir colores a los puntos según la región 
p + geom_point(aes(col=region), size = 3) 
#añadir una linea que muestre el promedio de homicidios del país 
r <- murders %>% summarize(rate = sum(total) / sum(population) * 10^6) %>% pull(rate) #de esta manera se pueden calcular cosas
#lo que verdaderamente se calcula es un r para utilizarse en y=r*x
p + geom_point(aes(col=region), size = 3) + geom_abline(intercept = log10(r))
p + geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") + geom_point(aes(col = region), size = 3)
p <- p + scale_color_discrete(name = "Region")    # capitalize legend title
murders <- murders %>% mutate(sum(total) / sum(population) * 10^6)

##ggthems and ggrepel 
ds_theme_set()
library(ggthemes)
p + theme_economist()
p + theme_fivethirtyeight()

##todo el procedimiento 
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate
# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) + geom_text(nudge_x = 0.05) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)  +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

##HISTOGRAM 
library(dslabs)
data(heights)
heights %>% filter(sex=="Male")
q <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
q + geom_histogram()
#da un mensaje de error entonces podemos decidir el binwidth 
q + geom_histogram(binwidth = 1)
#ya se puede personalizar 
#fill para el color de las columnas
#col para el contorno de las columnas
q + geom_histogram(binwidth = 1, fill="blue", col="black") + xlab("Male heights in inches") +
  ggtitle("Histogram")

#para hacer un gráfico de densidad 
w <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
w + geom_density(fill="blue")

#para hacer un QQ plot. El QQ plot realmente compara los z's que deben ser metiendole los parámetros media y sigma; contra..
##los z's que realmente están en la base
t <- heights %>% filter(sex == "Male") %>% ggplot(aes(sample=height))
t + geom_qq()
params <- heights %>% filter(sex=="Male") %>% summarize(mean=mean(height), sd = sd(height))
t + geom_qq(dparams = params) 
#se necesita agregar la linea para mirar qué tanto se aproxima a la normal 
t + geom_qq(dparams = params) + geom_abline()

#para hacer un plot con varios plots se hace así 
# define plots p1, p2, p3
p_0 <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p_0 + geom_histogram(binwidth = 1, fill="blue", col="black")
p2 <- p_0 + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p_0 + geom_histogram(binwidth = 3, fill = "blue", col = "black")

# arrange plots next to each other in 1 row, 3 columns
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)

#para sobreponer 2 densities en 1 por sexo 
heights %>% 
  ggplot(aes(height, group = sex)) + 
  geom_density()
#por color 
heights %>% 
  ggplot(aes(height, color = sex)) + geom_density()
#por fill 
heights %>% 
  ggplot(aes(height, fill = sex)) + 
  geom_density(alpha=0.2)










