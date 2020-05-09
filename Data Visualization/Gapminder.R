library(dslabs)
data(gapminder)
head(gapminder)
library(dplyr)
#para ver qué pais tiene una tasa de mortalidad infantil más alta, entre Sri Lanka y Turquía 
gapminder %>% filter(year==2015 & country %in% c("Sri Lanka", "Turkey")) %>% select(country, infant_mortality)
library(ggplot2)
#gráfico para identificar si es europa vs el resto del mundo
ds_theme_set()
filter(gapminder, year == 1962) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point()

#ahora para comparar entre 1962 y 2012
filter(gapminder, year %in% c(1962, 2012)) %>% ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point() + 
  facet_grid(.~ year)

#para ver el proceso a través de los años
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>% filter(year %in% years & continent %in% continents) %>% ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() + facet_wrap(~ year)

##TIME SERIES PLOTS ##

gapminder %>% filter(country == "United States") %>% ggplot(aes(year, fertility)) + geom_line()

countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>% ggplot(aes(year, fertility, group = country)) + geom_line()
#para hacer que se vea mejor 
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>% ggplot(aes(year, fertility, col = country)) + geom_line()

#para agregar labels 
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>% ggplot(aes(year, life_expectancy, col = country)) + geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5 ) + theme(legend.position = "none")

## TRANSFORMATIONS ##

gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
#hacer un hist 
past_year <- 1970
gapminder %>% filter(year==past_year & !is.na(gdp)) %>% ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1, color = "black")
#transformación logarítmica
gapminder %>% filter(year==past_year & !is.na(gdp)) %>% ggplot(aes(log2(dollars_per_day))) + 
  geom_histogram(binwidth = 1, color = "black")
#transformación añadiendo una capa 
gapminder %>% filter(year==past_year & !is.na(gdp)) %>% ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1, color = "black") + scale_x_continuous(trans="log2")

## STRATIFY AND BOXPLOTS ##

#mirar los dollars_per_day estratificando por regiones
p <- gapminder %>% filter(year == past_year & !is.na(gdp)) %>% ggplot(aes(region, dollars_per_day))
p + geom_boxplot() 
#quedó super mal, entonces usar theme 
p + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#función reorder
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)

value <- c(10,11,12,6,4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)

p <- gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(trans="log2")
# a veces se usa mejor fill que color para que se vea mejor por grupos   

## COMPARING DISTRIBUTIONS 
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder %>% filter(year == past_year & !is.na(gdp)) %>% mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + scale_x_continuous(trans = "log2") + 
  facet_grid(.~ group)
#ahora se compara con el presente 
present_year <- 2010 
gapminder %>% filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>% 
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ group)
#es bueno tener la consistencia entre los paises del 70 a los de 2010. deben ser los mismos para medir. 
country_list_1 <- gapminder %>% filter(year == past_year & !is.na(dollars_per_day)) %>% .$country 
country_list_2 <- gapminder %>% filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)
# .$country sirve para sacar el vector (no entiendo bien aún)
#nuevo plot con los mismos países 
gapminder %>% filter(year %in% c(past_year, present_year) & !is.na(gdp) & country %in% country_list) %>% 
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ group)

#para ver las regiones que más han evolucionado 
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() + facet_grid(year~.)
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

  p <- gapminder %>%
    filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
    ggplot(aes(region, dollars_per_day, fill = continent)) +
    geom_boxplot(aes(region, dollars_per_day, fill = factor(year))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") + scale_y_continuous(trans = "log2")

## DENSITY PLOTS ##
# así puede verse la cantidad de paises en cada grupo
gapminder %>% filter(year == past_year & !is.na(gdp) & country %in% country_list) %>% 
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n=n()) %>% knitr::kable()
# así puede hacerse el density teniendo en cuenta la cantidad de paises 
#el argumento ..count.. permite esto 
#para que la densidad se más suave puede hacerse con bw en geom_desnsity
aes(x=dollars_per_day, y = ..count.. )
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(x=dollars_per_day, y = ..count.. , fill = group)) +
  scale_x_continuous(trans = "log2") + geom_density(alpha=0.2, bw= 0.75) + facet_grid(year~.)

#para mejorar la gráfica se usa case_when 
#se está haciendo una agrupación más específica por regiones
gapminder <- gapminder %>% mutate(group = case_when(.$region %in% west ~ "West", .$region %in% c(
  "Eastern Asia", "South-Eastern Asia") ~ "East Asia", .$region %in% c("Caribbean", "Central America", "South America")
  ~ "Latin America", .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa", T ~ "Others"))
#esta  variable de grupos es mejor pasarla a un factor para controlar el orden 
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(x=dollars_per_day, y = ..count.. , fill = group)) +
  scale_x_continuous(trans = "log2") + geom_density(alpha=0.2, bw= 0.75, position = "stack") + facet_grid(year~.)
# con el argumento weight es importante ponderar por la cantidad de población 
p <- gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(weight = population/sum(population)) %>% 
  ggplot(aes(x=dollars_per_day, y = ..count.. , fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") + geom_density(alpha=0.2, bw= 0.75, position = "stack") + facet_grid(year~.)

## ECOLOGICAL FALLACY ##
#importante primero hacer más casos 
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands", 
    T ~ "Others"))
#computar la variable de interés 
surv_income <- gapminder %>% filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>% summarize(income = sum(gdp)/sum(population)/365, 
                                infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
#para ver de menor a mayor 
surv_income %>% arrange(income)
#para graficar 
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, col = group)) + 
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) + 
  scale_y_continuous(trans = "log2", limit = c(0.875, 0.9981), breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 

## EJERCICIOS ##
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>% filter(year == 2012 & continent == "Africa") %>%
  ggplot(aes(fertility, life_expectancy, color = region)) + 
  geom_point()

#2# 
df <- gapminder %>% 
  filter(continent=="Africa" & year == 2012 & fertility <=3 & life_expectancy>=70) %>%
  select(country, region)

#3#
countries <- c("Vietnam", "United States")
tab <- gapminder %>% filter(country %in% countries & year %in% (1960:2010)) %>% 
  ggplot(aes(year, life_expectancy, col = country)) + geom_line()

#4# 
daydollars <- gapminder %>% mutate(dollars_per_day = gdp/population/365)%>% filter(continent == "Africa" & year == 2010 & !is.na(gdp))
daydollars %>% ggplot(aes(dollars_per_day, country)) + scale_x_continuous(trans = "log2") + geom_density() 

#5# 
gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day, country)) + scale_x_continuous(trans = "log2") + facet_grid(year~.) + geom_density()

#6# 
daydollars <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(gdp)) 
daydollars %>%
  ggplot(aes(dollars_per_day, year, fill = region)) + 
  geom_density(bw=0.5, position = "stack") + scale_x_continuous(trans = "log2") + facet_grid(year~.)

#7# 
#siempre mirar el missing value de las variables a graficar 
gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(dollars_per_day) & !is.na(infant_mortality)) %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text() + 
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)

  
  
  
  
  
  
  
  
  
  




