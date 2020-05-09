data(murders)
head(murders)

library(dslabs)
data(heights)
head(heights)

#obtener proporciones
prop.table(table(heights$sex))

#así se hace un CDF
a <- seq(min(heights), max(heights))    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(heights <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

#DISTRIBUCION NORMAL 
index <- heights$sex=="Male"
x <- heights$height[index]
avg_1 <- sum(x)/length(x)
avg_2 <- mean(x)
SD_1 <- sqrt(sum((x - avg_2)^2)/length(x))
SD_2 <- sd(x)
c(average=avg_2, SD=SD_2)

#para distribucion normal estandar 
z <- scale(x)
mean(abs(z)<2)

#una proporción de cuantas personas están en un intervalo 
x <- heights$height[heights$sex == "Male"]
mean(x > 69 & x <= 72)

#####################QUANTILES #######################
summary(heights$height)
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height,p)
print(percentiles)
percentiles[names(percentiles)=="25%"]

#qnorm() gives the theoretical value of quantile with prob. p given a normal distribution
#qnorm(p, mu, sigma)
#pnorm y qnorm son inversas. pnorm con el valor estándar da la probabilidad. qnorm con la prob. da el z 
p_11 <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p_11, 69, 3)
observed_quantiles <- quantile(z, p_11)
theoretical_quantiles <- qnorm(p_11)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

############## BOX PLOT ##############
hist(murder_rate)
z <- scale(murder_rate)
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

########### EJERCICIOS ##############
library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
p <- seq(0.1,0.9,0.2)
percentiles_female <- quantile(female,p)
female_percentiles <- c("10th"=percentiles_female[names(percentiles_female)=="10%"], "30th"=percentiles_female[names(percentiles_female)=="30%"],"50th"=percentiles_female[names(percentiles_female)=="50%"],"70th"=percentiles_female[names(percentiles_female)=="70%"],"90th"=percentiles_female[names(percentiles_female)=="90%"])
percentiles_male <- quantile(male,p)
male_percentiles <- c("10th"=percentiles_male[names(percentiles_male)=="10%"], "30th"=percentiles_male[names(percentiles_male)=="30%"],"50th"=percentiles_male[names(percentiles_male)=="50%"],"70th"=percentiles_male[names(percentiles_male)=="70%"],"90th"=percentiles_male[names(percentiles_male)=="90%"])
df <- data.frame(female = female_percentiles, male=male_percentiles)

