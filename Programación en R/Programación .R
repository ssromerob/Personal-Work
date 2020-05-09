##BASIC CONDITIONALS##

library(dslabs)
data(murders)
#así se crean variables 
murder_rate <- murders$total/murders$population*100000
ind <- which.min(murder_rate)
if (murder_rate[ind] < 0.5 ){
  print(murders$state[ind])
} else {
  print ("no state has murder rate that low")
}

#ifelse
a<-0
ifelse(a>0, 1/a, NA)
a<-c(0,1,2,-4,5)
result <- ifelse(a>0, 1/a, NA)

data("na_example")
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example),0,na_example)

#any 
#esta función detecta si hay al menos un TRUE
z <- c(T, T,F)
any(z)

#all 
#esta función detecta si todos son TRUE 
z <- c(T, T,F)
all(z)

########### BASIC FUNCTIONS ###############

avg <- function(x){
  s <- sum (x)
  n <- length(x)
  s/n
}

x<-1:1000
avg(x)

#identical 
identical(mean(x),avg(x))

avg <- function(x, arithmetic=T){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

########## FOR LOOPS ###################
compute_s_n <- function(n){
  x <- 1:n 
  sum(x)
}

compute_s_n(100)

for(i in 1:5){
  print(i)
}

m <- 25 
#1. create an empty vector 
s_n <- vector(length=m)
for (i in 1:m){
  s_n[i] <- compute_s_n(i)
}
#haciendo un plot
n <- 1:m 
plot(n, s_n)
lines(n, n*(n+1)/2)
  

######### OTHER FUNCTIONS ##################

#nchar() dice cuantos caracteres tiene una expresion 
  
  
  
  
  
  
  






