install.packages(c("forecast","tseries","dplyr","ggplot2","readr"))
install.packages("car")
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)
library(readr)
library(car)
data=read.csv(file.choose())
head(d)
str(data)
as.numeric(data$TNO.Killed)
#Total no of acc in india
india_year <- data %>%
  group_by(Year) %>%
  summarise(
    TotalAccidents = sum(Total.No.of.RA),
    TotalInjuries  = sum(TNO.Injuried),
    TotalDeaths    = sum(TNO.Killed)
  )

print(india_year)

#Plot national trend
plot(india_year$Year, india_year$TotalAccidents,
     type="l", col="blue",
     main="India Total Road Accidents Trend",
     xlab="Year", ylab="Total Accidents")

#Top 10 Accident-Prone States (Hotspots)
top_states <- data %>%
  group_by(State) %>%
  summarise(TotalAccidents = sum(Total.No.of.RA)) %>%
  arrange(desc(TotalAccidents))

head(top_states, 10)

#fatality rate Per year
india_year <- india_year %>%
  mutate(FatalityRate = (TotalAccidents / TotalDeaths) * 100)

fatal_rank <- india_year %>%
  group_by(Year) %>%
  summarise(FatalityRate =(TotalAccidents / TotalDeaths) * 100,na.rm=TRUE) %>%
  arrange(desc(FatalityRate))

head(fatal_rank, 10)

#Correlation
cor.test(data$Total.No.of.RA, data$TNO.Injuried)
#Holt,s exponential
holt_model <- holt(ts_india, h = 5)
plot(holt_model)
summary(holt_model)
adf.test(ts_india)

#t-test
guj <- subset(data, State == "Gujarat")$Total.No.of.RA
mah <- subset(data, State == "Maharashtra")$Total.No.of.RA
tn=subset(data, State == "Tamil Nadu")$Total.No.of.RA
ktk=subset(data, State == "Karnataka")$Total.No.of.RA

length(guj)
length(mah)
length(tn)
length(ktk)
guj
mah

shapiro.test(guj)
shapiro.test(mah)
shapiro.test(tn)
shapiro.test(ktk)
hist(guj)
hist(tn)
hist(ktk)

leven=leveneTest(
  c(guj,tn),
  factor(rep(c("Gujarat", "Tamil Nadu"),
             times = c(length(guj), length(tn))))
)
t.test(guj, tn, var.equal = TRUE)
# Wilcoxon Rank sum test non parametric
wilcox.test(guj, ktk)

#Box Plot
library(ggplot2)
library(dplyr)

# Create dataframe
df <- data.frame(
  State = rep(c("Gujarat", "Tamil Nadu"), 
              times = c(length(guj), length(tn))),
  Accidents = c(guj, tn)
)

# Calculate summary statistics
summary_stats <- df %>%
  group_by(State) %>%
  summarise(
    min = min(Accidents),
    Q1 = quantile(Accidents, 0.25),
    median = median(Accidents),
    Q3 = quantile(Accidents, 0.75),
    max = max(Accidents)
  )

# Plot
ggplot(df, aes(x = State, y = Accidents, fill = State)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  
  # State name on median line
  geom_text(data = summary_stats,
            aes(x = State, y = median, label = State),
            color = "black", fontface = "bold", size = 4) +
  
  # Min
  geom_text(data = summary_stats,
            aes(x = State, y = min, label = paste0("Min=", round(min))),
            vjust = 1.5, size = 3) +
  
  # Q1
  geom_text(data = summary_stats,
            aes(x = State, y = Q1, label = paste0("Q1=", round(Q1))),
            vjust = -0.5, size = 3) +
  
  # Median value
  geom_text(data = summary_stats,
            aes(x = State, y = median, label = paste0("Med=", round(median))),
            vjust = -1.5, size = 3, color = "blue") +
  
  # Q3
  geom_text(data = summary_stats,
            aes(x = State, y = Q3, label = paste0("Q3=", round(Q3))),
            vjust = -0.5, size = 3) +
  
  # Max
  geom_text(data = summary_stats,
            aes(x = State, y = max, label = paste0("Max=", round(max))),
            vjust = -0.5, size = 3) +
  
  labs(
    title = "Comparison of Road Accidents: Gujarat vs Tamil Nadu",
    y = "Number of Accidents",
    x = ""
  ) +
  
  theme_minimal() +
  theme(legend.position = "none")

library(ggplot2)
library(dplyr)

# Create dataframe
df <- data.frame(
  State = rep(c("Gujarat", "Karnataka"), 
              times = c(length(guj), length(ktk))),
  Accidents = c(guj, ktk)
)

# Summary statistics
summary_stats <- df %>%
  group_by(State) %>%
  summarise(
    min = min(Accidents),
    Q1 = quantile(Accidents, 0.25),
    median = median(Accidents),
    Q3 = quantile(Accidents, 0.75),
    max = max(Accidents)
  )

# Plot
ggplot(df, aes(x = State, y = Accidents, fill = State)) +
  geom_boxplot(alpha = 0.6, width = 0.5) +
  
  # Labels with clear identification
  geom_text(data = summary_stats,
            aes(x = State, y = min, label = paste("Min:", round(min))),
            vjust = 1.5, size = 3, color = "black") +
  
  geom_text(data = summary_stats,
            aes(x = State, y = Q1, label = paste("Q1:", round(Q1))),
            vjust = -0.8, size = 3, color = "black") +
  
  geom_text(data = summary_stats,
            aes(x = State, y = median, label = paste("Median:", round(median))),
            vjust = -1.5, size = 3.2, color = "blue", fontface = "bold") +
  
  geom_text(data = summary_stats,
            aes(x = State, y = Q3, label = paste("Q3:", round(Q3))),
            vjust = -0.8, size = 3, color = "black") +
  
  geom_text(data = summary_stats,
            aes(x = State, y = max, label = paste("Max:", round(max))),
            vjust = -0.5, size = 3, color = "black") +
  
  # Optional: State name on median line
  geom_text(data = summary_stats,
            aes(x = State, y = median, label = State),
            vjust = 1.8, size = 4, fontface = "bold", color = "darkred") +
  
  labs(
    title = "Comparison of Road Accidents: Gujarat vs Karnataka",
    y = "Number of Accidents",
    x = ""
  ) +
  
  theme_minimal() +
  theme(legend.position = "none")

#Models
Year = c(2020, 2021, 2022, 2023),
Straight = c(237943, 278218, 309247, 322005),
Curved = c(47772, 49581, 54593, 58626),
Bridge = c(12836, 12709, 14111, 15528),
Culvert = c(6724, 6663, 7384, 10308),
Potholes = c(3564, 3625, 4446, 5840),
Steep = c(3967, 3967, 4475, 5094),
Construction = c(9075, 9075, 9211, 9425),
Others = c(44257, 48594, 57845, 53757),
Total = c(366138, 412432, 461312, 480583)
)
data
library(tidyr)
library(MASS)
#Poisson Model
poisson_model <- glm(Accidents ~ RoadType,
                     family = poisson(link = "log"),
                     data = long_data)

summary(poisson_model)

#Negative Binomial Model with reference category taken as Bridge
#relevel for straight rode
long_data$RoadType <- as.factor(long_data$RoadType)
long_data$RoadType <- relevel(long_data$RoadType, ref ="Bridge")

nb_model1<- glm.nb(Accidents ~ RoadType, data = long_data)
summary(nb_model1)

#Negative Binomial Model with reference category taken as Straight road
long_data$RoadType <- relevel(long_data$RoadType, ref ="Straight")

nb_model2 <- glm.nb(Accidents ~ RoadType, data = long_data)
summary(nb_model2)

AIC(poisson_model,nb_model1,nb_model2)

# Overspeeding
# Create dataset
data <- data.frame(
  Year = c(2018, 2019, 2020, 2021, 2022, 2023),
  Total_Accidents = c(467044, 449002, 366138, 412432, 461312, 480583),
  Overspeeding = c(303000, 305000, 258271, 303954, 328907, 336756)
)

# View data
print(data)
# Poisson Model
poisson_model <- glm(Overspeeding ~ Year,
                     family = poisson(link = "log"),
                     data = data)

summary(poisson_model)
# Overdispersion test
dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / 
  poisson_model$df.residual

dispersion
# Install if not installed
install.packages("MASS")

# Load package
library(MASS)

# NB Model
nb_model <- glm.nb(Overspeeding ~ Year, data = data)

summary(nb_model)

# NB model with offset (recommended)
nb_model2 <- glm.nb(Overspeeding ~ Year + offset(log(Total_Accidents)),
                    data = data)

summary(nb_model2)
AIC(poisson_model, nb_model, nb_model2)
# IRR for NB model
exp(coef(nb_model2))

# Predicted values
pred_nb <- predict(nb_model, type = "response")

# RMSE
rmse_nb <- sqrt(mean((data$Overspeeding - pred_nb)^2))

rmse_nb

# Predicted values
pred_nb_offset <- predict(nb_model2, type = "response")

# Predicted values
pred_nb <- predict(nb_model, type = "response")

# RMSE
rmse_nb <- sqrt(mean((data$Overspeeding - pred_nb)^2))

rmse_nb

# RMSE
rmse_nb_offset <- sqrt(mean((data$Overspeeding - pred_nb_offset)^2))

rmse_nb_offset

data.frame(
  Model = c("Poisson", "NB", "NB_with_offset"),
  RMSE = c(rmse_pois, rmse_nb, rmse_nb_offset)
)

