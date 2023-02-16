# R---Studio-project
Prediction Analysis of Wind Energy Consumption for next 30 years
The Code used for the project is as follows:

library(tidyverse)
library(fitdistrplus)
library(ggthemes)

#-----------------------------------------------------------------------------------------------------------------
# setting working directory
setwd("C:/Users/Parth Malpure/Desktop/P&S Project")

# importing data
world1 <- read_csv("World_Energy_Consumption.csv")

#----------------------------------------------------------------------------------------------------------------
# filtering the data
# calculating new column for gdp per capita
# converting gdp to $ trillion
world <- data.frame(world1) %>% 
  mutate(gdp_per_capita = (world1$gdp/world1$population)) %>%
  mutate(gdp = (world1$gdp/1000000000000)) %>%
  mutate(population = (world1$population/1000000)) %>%
  dplyr::select('country', 'year','population', 'gdp', 'gdp_per_capita', 
                'primary_energy_consumption', 'energy_per_capita',
                'renewables_consumption','renewables_share_energy','renewables_energy_per_capita',
                'biofuel_share_energy','solar_share_energy',
                'wind_consumption','wind_share_energy','wind_energy_per_capita',) %>% 
  filter(year > 1995 & year < 2015, 
         country %in% c('United States', 'China', 'Canada', 'Germany', 'United Kingdom',
                        'India', 'Japan', 'France', 'Brazil', 'Itlay', 'Russia', 'South Korea',
                        'Australia', 'Spain', 'Mexcio', 'Indonesia', 'Turkey','Netherlans',
                        'Saudi Arabia', 'Switzerland'))

#selecting four countries for comparison 
selected <- world %>% 
  filter(country %in% c('United States', 'China', 'Germany', 'United Kingdom'))

germany <- world %>% 
  filter(country %in% c('Germany'))

world2 <- world %>% 
  filter(country != 'Germany')
#--------------------------------------------------------------------------------------------------------------
# Part 1
# Year VS energy_consumption
world %>% 
  filter(country %in% c('United States', 'China', 'Germany', 'United Kingdom', 'India')) %>% 
  ggplot(data = ., mapping = aes( x = year, y = primary_energy_consumption, color = country)) +
  geom_line(size = 1.1) +
  labs(title = "Year vs Energy Consumption in TWH", x="Year", y="Energy Consumption in TWH", subtitle = " ") +
  theme_bw() + 
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12) )

# Year vs % Renewable Energy Share
ggplot(selected, aes( x = year, y = renewables_share_energy)) +
  geom_point( color = "blue", size = 1, alpha = 0.6) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~country)+
  labs(title = "Year vs % Renewable Energy Share", x="Year", y="% Renewable Energy Share", subtitle = " ") +
  theme_gdocs()+ 
  theme(axis.title = element_text(size = 14, colour = "black"),
        plot.title = element_text(size = 16, face = "bold", colour = "black", hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black") )


# Year vs Renewables_share_energy
ggplot(germany) +
  geom_line(aes( x = year, y = wind_share_energy), color = "steelblue", size = 1.2) +
  geom_line(aes( x = year, y = solar_share_energy), color = "darkgrey", size = 1, alpha = 0.7) +
  geom_line(aes( x = year, y = biofuel_share_energy), color = "grey", size = 0.8, alpha = 0.3) +
  labs(title = "Year vs % Renewables Energy Share ", x="Year", y="% Renewables Energy Share", subtitle = " ") +
  annotate("text", x = 2014, y = 4.070, label = "Wind", vjust = -0.5) +
  annotate("text", x = 2014, y = 2.509, label = "Solar", vjust = -0.5) +
  annotate("text", x = 2014, y = 0.912, label = "Biofuel",  vjust = -0.5) +
  theme_clean() + 
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12) )

# Year VS GDP
world %>% 
  filter(country %in% c('United States', 'China', 'Germany', 'United Kingdom', 'India')) %>%   
  ggplot(data = ., mapping = aes( x = year, y = gdp, color = country)) +
  geom_line(size = 1)  +
  labs(title = "Year vs GDP", x="Year", y="GDP in $ trillion", subtitle = " ") +
  theme_bw()+
  theme(legend.position="right") + 
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12) )

# Coorelation graph
# Energy VS GDP
ggplot(germany,  aes( x = wind_share_energy, y = gdp)) +
  geom_point( size = 1.7, color = "black", alpha = 0.8) +
  geom_smooth(method = "lm", size = 1.1, color = "blue") + 
  labs(title = "% Wind Energy Share vs GDP ", x= "% Wind Energy Share", y="GDP in $ trillion", subtitle = " ") +
  theme_bw()+ 
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12) )

cor(germany$wind_share_energy, germany$gdp, method = c("pearson"))
# pearson correlation coefficient = 0.9829859
cor(germany$wind_share_energy, germany$gdp, method = c("kendall"))
# kendall correlation coefficient = 0.9501507

#---------------------------------------------------------------------------------------------------------------

#1. Finding probabilities for a given range of % Wind Energy Share
#Assuming that the world's % wind energy share is normally distributed with mean = 0.5541424,sd = 1.24989

# X = R.V. of % wind Energy Share
# X ~ N(x; mean = 0.5541424,sd = 1.24989)

#(a) more than 2% wind energy share
# [P(X >= 2)]
1- pnorm(q = 2,mean = mean(world$wind_share_energy),sd = sd(world$wind_share_energy))

#(b) less than 1% wind energy share
# X ~ N(x; mean = 0.5541424,sd = 1.24989)
# [P(X<=1)]
pnorm(q = 1,mean = mean(world$wind_share_energy),sd = sd(world$wind_share_energy))

#(c) less than 0.5% wind energy share
# X ~ N(x; mean = 0.5541424,sd = 1.24989)
# [P(X<=0.5)]
pnorm(q = 0.5,mean = mean(world$wind_share_energy),sd = sd(world$wind_share_energy))

#------------------------------------------------------------------------------------------------------------------------------------
#2 - Test to check whether the proportion of %Wind energy share of Germany and Rest of the World are equal

#Two sample proportion test

#X1 = R.V. of proportion of Germany's wind-energy_share > 1.5
#X2 = R.V. of proportion of Rest of the World's wind-energy_share > 1.5

# H0: Pop_prop1-Pop_prop2 = 0 (There is no significant difference) 
# H1: Pop_prop1-Pop_prop2!= 0 (There is a significant difference)

# Step 1-a: no. of rows in germany
n1 <-length(which(germany$wind_share_energy >=0))

# Step 1-b: no. of rows where wind energy share > 1 for germany 
x1 <- germany %>% filter(wind_share_energy > 1) %>% nrow()

# Step 1-c: no. of rows in rest of the world
n2 <-length(which(world2$wind_share_energy >=0))

# Step 1-d: no. of rows where wind energy share > 1 for rest of the world
x2 <- world2 %>% filter(wind_share_energy > 1)%>% nrow()

# Step 2: Execute the test
prop.test(x=c(x1, x2),n=c(n1,n2))

# Step 3: Conclusion from the test using the p-value approach
#As p_value < 0.05, we reject the null hypothesis and conclude that there is a 
#significant difference between Germany's %wind energy share > 1.5% and World's %wind energy share > 1.5%.
#proportion of Germany's wind-energy_share > 1.5 is greater than  proportion of the Rest of the World's wind-energy_share > 1.5.

ggplot(selected, aes(x=(country), y=wind_share_energy, fill = country)) + 
  geom_boxplot(varwidth = TRUE, alpha=0.8) +
  geom_point(color="black", size=1.1, alpha=0.6) +
  labs(title = "Country vs Mean of % Wind Energy Share", x="Country", y="% Wind Energy Share", subtitle = " ") +
  theme_bw() +
  theme(legend.position="none") + 
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12) )

#---------------------------------------------------------------------------------------------------------------------
#3: Test if the mean %wind energy share of Germany and Rest of the World are equal or not.

#Two sample t-test - used to test the difference between two population means when the population
# variances are unknown,and assumped equal or unequal with large or small samples.

#X1 = R.V. of %wind energy share of Germany
#X2 = R.V. of %wind energy share of World

# H0: Pop_mean1-Pop_mean2 = 0 (There is no significant difference between the mean %wind energy share of Germany and Rest of the world)
# H1: Pop_mean1-Pop_mean2!= 0 (There is a significant difference between the mean %wind energy share of Germany and Rest of the world)

#### Step 1: Create two samples

#to return the same data each time we sample it, we need to set the seed to a numerical value
set.seed(100)

#sample 12 rows from Germany
cen_1_sample <- sample_n(germany, 12)

#sample 100 rows from World
cen_2_sample <- sample_n(world2, 100) 

# selecting the % Wind energy share column from Germany
sample1<-cen_1_sample$wind_share_energy

# selecting the % Wind energy share column from Rest of the World
sample2<-cen_2_sample$wind_share_energy

#### Step 2: Run the t-test
t.test(x=sample1,y=sample2)

#### Step 3: Conclusion from the Test using the p-value approach

#As p_value < 0.05, we reject the null hypothesis and 
#conclude that there significant difference between the mean of %wind energy share of Germany 
#and %wind energy share of Rest of the World.
# mean of %wind energy share of Germany is greater than Rest of the World %wind energy share.

selected1 <- selected %>% 
  group_by(country) %>% 
  summarise(Mean = mean(wind_share_energy)) %>% 
  mutate(Mean = round(Mean, 3))

ggplot(selected1, mapping = aes( x = country, y = Mean, fill = country)) +
  geom_bar(stat = "summary", fun = mean, alpha=0.8) +
  geom_text(aes(label = Mean), vjust = 1) +
  geom_hline(yintercept = mean(world$wind_share_energy, na.rm = T), size = 1.1) +
  annotate("text", x = 1.2, y = 0.6, label = "Mean of % Wind Energy Share of") +
  annotate("text", x = 1, y = 0.5, label = "the World = 0.554") +
  labs(title = "Country vs Mean of % Wind Energy Share", x="Country", y="% Wind Energy Share", subtitle = " ") +
  theme_bw()+ 
  theme(axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),legend.position="none")
#----------------------------------------------------------------------------------------------------------------------------
# Part 3
# curve fitting for Germany

# year Vs % Wind Energy  
ggplot(germany, aes(wind_share_energy)) +
  geom_histogram(bins = 4, color = 'black', fill = "steelblue") +
  labs(title = "Histogram of % Wind Energy Share", x= "% Wind Energy Share", y="Count", subtitle = " ") +
  theme_bw()+ 
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12) )

par(mfrow=c(1,1))
descdist(germany$wind_share_energy)

#for uniform
fit_uni <- fitdist(germany$wind_share_energy, "unif")
summary(fit_uni)

#for normal
fit_n <- fitdist(germany$wind_share_energy, "norm")
summary(fit_n)

# goodness-of-fit plots
par(mfrow=c(2,2))
plot.legend <- c("uniform", "norm")
denscomp(list(fit_uni, fit_n), legendtext = plot.legend, xlab = '% Wind Share')
cdfcomp (list(fit_uni, fit_n), legendtext = plot.legend, xlab = '% Wind Share')
qqcomp  (list(fit_uni, fit_n), legendtext = plot.legend, xlab = '% Wind Share')
ppcomp  (list(fit_uni, fit_n), legendtext = plot.legend, xlab = '% Wind Share')

gofstat(list(fit_uni, fit_n))

# AS AIC and BIC value is lower for Uniform distribution curve, therefore % wind energy share column follows Uniform Distribution.


par(mfrow=c(1,1))
#------------------------------------------------------------------------------------------------------------------------------------------------

# linear Regression model
# Creating a linear regression model to predict % Wind energy Share for Germany for the years 2021-2050, using the existing data.

wind_model <- lm(wind_share_energy~year, data = germany)
summary(wind_model)

par(mfrow=c(2,2)) 
plot(wind_model)
#The equation for the wind model is
# Y = -455.655 + (0.228*X)

#Plotting the Model with a best fitting line
par(mfrow=c(1,1)) 
ggplot(wind_model, mapping = aes(x = year, y=wind_share_energy)) +
  geom_point(size = 2, alpha = 0.7, color = "blue") +
  geom_smooth(method = "lm", colour = "black", size = 1.3, se = F) +
  geom_segment(aes(xend = year, yend = .fitted), color = "red", size = 0.8) +
  annotate("text", x = 1997.6, y = 3.9, label = "Y = -455.655 + (0.228*X)") +
  annotate("text", x = 1997, y = 3.6, label = "R-squared: 0.976") +
  annotate("text", x = 1997, y = 3.3, label = "  p = 2.97 x 10^(-15)") +
  labs(title = "% Wind Energy Share Model", x= "Year", y= "% Wind Energy Share", subtitle = " ") +
  theme_bw()+ 
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12) )

# Predicting % Wind Energy Share for using the model
Year <- c(2021:2050)
next_year <- data.frame(year = Year)
wind_predictions <- predict(wind_model, next_year) %>% round(2)
wind_energy_share <- (wind_predictions)

# Creating a data frame of predicted wind energy share for upcoming years
predictions <- data.frame(Year, wind_energy_share)
View(predictions)
#----------------------------------------------------------------------------------------------------------------------
write_xlsx(predictions, "C:/Users/suraj/Desktop/Prob Stat Project/World Energy/predictions.xlsx")

