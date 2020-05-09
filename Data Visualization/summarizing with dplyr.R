##summarize and group_by 
library(tidyverse)
library(dslabs)
data(heights)
s <- heights %>% filter(sex=="Male") %>% summarize(average = mean(height), standard_deviation = sd(height))
#así puede accederse a esos valores calculados 
s$average
s$standard_deviation
heights %>% filter(sex=="Male") %>% summarize(median = median(height), min = min(height), max = max(height))
#summarize solo puede usarse para funciones que devuelven un solor valor. 

data(murders)
murders <- murders %>% mutate(murder_rate= total/population*100000) 
summarize(murders, mean(murder_rate)) #esto da un resultado que no es el real, hay que tener en cuenta los estados grandes 

#la manera correcta es 
us_murder_rate <- murders %>% summarize(rate = sum(total)/sum(population) *100000)
us_murder_rate %>% .$rate
us_murder_rate <- murders %>% summarize(rate = sum(total)/sum(population) *100000) %>% .$rate


##group the summarize 
heights %>% group_by(sex)
heights %>% group_by(sex) %>% summarize(mean = mean(height), sd = sd(height))
##también puede agruparse por region, en el caso de murders 

##funcion arrange 
murders %>% arrange(population) %>% head()

##para organizar el data frame de mayor a menor 
murders %>% arrange(desc(murder_rate)) %>% head()

##para ordenar segun region pero también según murder rate 
murders %>% arrange(region, desc(murder_rate)) %>% head()

##para ver el top_n de ciudades con murder_rate 
murders %>% top_n(10, murder_rate)
murders %>% arrange(desc(murder_rate)) %>% top_n(10)

##ASI SUMMARIZE CUANDO HAY NAs
library(dplyr)
library(NHANES)
data(NHANES)
## complete the line
NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>% summarize(minbp = min(BPSysAve, na.rm =T), maxbp = max(BPSysAve, na.rm=T))




