# Loading packages
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}

if (!requireNamespace("scales", quietly = TRUE)) {
  install.packages("scales")
}

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}

if (!requireNamespace("foreign", quietly = TRUE)) {
  install.packages("foreign")
}

if (!requireNamespace("glue", quietly = TRUE)) {
  install.packages("glue")
}

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}

if (!requireNamespace("forecast", quietly = TRUE)) {
  install.packages("forecast")
}

if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

# Loading libraries
library(tidyverse)
library(scales)
library(lubridate)
library(foreign)
library(glue)
library(remotes)
library(usethis)
library(forecast)
library(randomForest)

#Load of dataset from the repository
url <- "https://github.com/millamonfort/BirthBA/archive/refs/heads/main.zip"
download.file(url, destfile = "BirthsBABR.zip", mode = "wb")
unzip("BirthsBABR.zip", exdir = "BirthBA-main")

# Now, list the files in the extracted directory
files <- list.files("BirthBA-main/BirthBA-main/dataBithsBA", pattern = ".csv", full.names = TRUE)

# Read the CSV files into a list
BirthsBABR <- lapply(files, read.csv)

# Combine the list of data frames into a single data frame
BirthsBABR <- bind_rows(BirthsBABR)

# Delete unnecessary variable
BirthsBABR$X <- NULL

# Overview of the dataset
glimpse(BirthsBABR)

# Transform PROC_REA in character
BirthsBABR$PROC_REA <- as.character(BirthsBABR$PROC_REA)

# Create a new column 'Name_PROC' in the 'BirthsBA' dataset based on the 'PROC_REA' values.
BirthsBABR <- BirthsBABR %>%
  mutate(name_PROC = case_when(
    PROC_REA == 310010039 ~ "Vaginal delivery",
    PROC_REA == 310010047 ~ "High-risk vaginal delivery",
    PROC_REA == 310010055 ~ "Vaginal delivery in a Normal Delivery Center",
    PROC_REA == 411010026 ~ "High-risk cesarean section",
    PROC_REA == 411010034 ~ "Cesarean section",
    PROC_REA == 411010042 ~ "Cesarean section with tubal ligation",
    TRUE ~ NA_character_
  ))

## CREATION OF DATE FORMAT VARIABLE - Create a variable in date format
BirthsBABR$DT_ADMISSION <- ymd(BirthsBABR$DT_INTER)
BirthsBABR$DT_EXIT <- ymd(BirthsBABR$DT_SAIDA)

### Creation of year of admission
BirthsBABR$admYear <- year(BirthsBABR$DT_ADMISSION)

### Creation of month/year admission
BirthsBABR$admMonth <- as.Date(cut(BirthsBABR$DT_ADMISSION, breaks = 'month'))

# Overview of the dataset
glimpse(BirthsBABR)

### Graph 1 - Delivery Admissions Over Time in Bahia, Brazil (2008-2021)
BirthsBABR %>%
  group_by(admMonth, name_PROC) %>%
  summarize(qtInt = n()) %>%
  select(admMonth, name_PROC, qtInt) %>%
  filter(admMonth >="2011-01-01" & admMonth <=" 2021-12-01") %>%
  ggplot(aes(admMonth, qtInt, color = name_PROC)) +
  geom_line() +
  theme_bw() +
  scale_x_date(labels = date_format("%m-%Y"),
               breaks = seq(as.Date("2011-01-01"), as.Date("2021-12-01"), by = "3 months"),
               limits = as.Date(c('2011-01-01', '2021-12-31')),
               expand = c(0.01, 0.01)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
        legend.position = "bottom") +
  labs(x = "Month/year", y = "Number of Admissions",
       color = NULL)

### Graph 2 - Linear Regression of Vaginal Delivery and Cesarean Section in Bahia (2011-2021)
BirthsBABR %>%
  filter(name_PROC == "Vaginal delivery" | name_PROC == "Cesarean section") %>%
  group_by(admMonth, name_PROC) %>%
  summarize(qtInt = n()) %>%
  select(admMonth, name_PROC, qtInt) %>%
  filter(admMonth >= "2011-01-01" & admMonth <= "2021-12-01") %>%
  ggplot(aes(admMonth, qtInt, color = name_PROC)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, aes(group = name_PROC), color = "gray") +  # Add linear regression line
  theme_bw() +
  scale_x_date(labels = scales::date_format("%m-%Y"),
               breaks = scales::breaks_pretty(n = 10)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7),
        legend.position = "bottom") +
  labs(x = "Month/year", y = "Number of Admissions",
       color = NULL)

### Graph 3 - Monthly Count of Normal Delivery Hospitalizations. Bahia, from 2008 to 2021
BirthsBABR %>%
  filter(name_PROC == "Vaginal delivery") %>%
  group_by(admMonth) %>%
  summarize(qtInt = n()) %>%
  select(admMonth, qtInt) %>%
  mutate(ano = year(admMonth),
         mes = month(admMonth)) %>%
  filter(admMonth >="2011-01-01" & admMonth <=" 2021-12-01") %>%
  mutate(new_date = ym(glue("2020-{mes}"))) %>%
  ggplot(aes(new_date, qtInt, color = as.factor(ano))) +
  geom_line() +
  theme_bw() +
  scale_x_date(labels = date_format("%b"),
               breaks = "1 month") +
  scale_y_continuous(breaks = seq(0, 13000, by = 500)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10)) +
  labs(x = "Month", y = "Number of Admissions for vaginal delivery", color = "Year" )

### Graph 4 - Monthly Count of Cesarean Section Hospitalizations. Bahia, from 2008 to 2021
BirthsBABR %>%
  filter(name_PROC == "Cesarean section") %>%
  group_by(admMonth) %>%
  summarize(qtInt = n()) %>%
  select(admMonth, qtInt) %>%
  mutate(ano = year(admMonth),
         mes = month(admMonth)) %>%
  filter(admMonth >="2011-01-01" & admMonth <=" 2021-12-01") %>%
  mutate(new_date = ym(glue("2020-{mes}"))) %>%
  ggplot(aes(new_date, qtInt, color = as.factor(ano))) +
  geom_line() +
  theme_bw() +
  scale_x_date(labels = date_format("%b"),
               breaks = "1 month") +
  scale_y_continuous(breaks = seq(0, 5000, by = 200)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10)) +
  labs(x = "Month", y = "Number of Admissions for Cesarean section", color = "Year" )

### Graph 5 - Predictive Model for Birth Trends in Bahia, Brazil for the Next 10 Years
# FORECAST TOTAL BIRTHS
# Create a time series object
ts_data <- BirthsBABR %>%
  group_by(admMonth) %>%
  summarise(Number_of_Births = n()) %>%  # Count the number of rows
  pull(Number_of_Births) %>%
  ts(frequency = 12)

# Fit an ARIMA model
arima_model <- auto.arima(ts_data)

# Make predictions for the next 120 months (10 years)
future_preds <- forecast(arima_model, h = 120)

# Plot the results
plot(future_preds, main = "Forecast of Births for the Next 10 Years")

### Graph 6 - Predictive Model for Vaginal Delivery and Cesarean Section Trends in Bahia, Brazil for the Next 20 Years
# FORECAST BIRTHS PER PROCEDURES
# Filter data for Vaginal Delivery
vaginal_data <- BirthsBABR %>%
  filter(name_PROC == "Vaginal delivery") %>%
  group_by(admMonth) %>%
  summarise(Number_of_Births = n()) %>%
  pull(Number_of_Births) %>%
  ts(frequency = 24)

# Fit an ARIMA model for Vaginal Delivery
arima_model_vaginal <- auto.arima(vaginal_data)

# Make predictions for Vaginal Delivery
future_preds_vaginal <- forecast(arima_model_vaginal, h = 240)

# Filter data for Cesarean Section
cesarean_data <- BirthsBABR %>%
  filter(name_PROC == "Cesarean section") %>%
  group_by(admMonth) %>%
  summarise(Number_of_Births = n()) %>%
  pull(Number_of_Births) %>%
  ts(frequency = 24)

# Fit an ARIMA model for Cesarean Section
arima_model_cesarean <- auto.arima(cesarean_data)

# Make predictions for Cesarean Section
future_preds_cesarean <- forecast(arima_model_cesarean, h = 240)

# Plot the results for both Vaginal Delivery and Cesarean Section WITH MONTH/YEAR
autoplot(future_preds_vaginal, series = "Vaginal Delivery") +
  autolayer(future_preds_cesarean, series = "Cesarean Section", col = "red") +
  labs(title = "Forecast of Births for Vaginal Delivery and Cesarean Section",
       y = "Number of Births") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 20, by = 1))

### Graph 7 - Predictive Model with random forest and linear regression for Birth Trends in Bahia, Brazil for the Next 20 Years
# Select relevant columns for modeling
birth_forest_model <- BirthsBABR %>%
  group_by(admMonth) %>%
  summarise(Number_of_Births = n()) %>%
  ungroup() %>%
  mutate(Year = as.numeric(format(admMonth, "%Y"))) %>%
  select(Year, Number_of_Births)

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
split_index <- sample(1:nrow(birth_forest_model), 0.8 * nrow(birth_forest_model))
train_data <- birth_forest_model[split_index, ]
test_data <- birth_forest_model[-split_index, ]

# Add a quadratic term to capture non-linear trends
train_data$Year2 <- train_data$Year^2

# Create future data with quadratic terms
future_years <- seq(max(birth_forest_model$Year) + 1, max(birth_forest_model$Year) + 20)
future_data <- data.frame(Year = future_years, Year2 = future_years^2)

# Build a Random Forest regression model with a quadratic term
births_model <- randomForest(Number_of_Births ~ Year + Year2, data = train_data, ntree = 100)

# Make predictions for the next 20 years using Random Forest model
future_predictions_rf <- predict(births_model, newdata = future_data)

# Create a linear regression model with a quadratic term
linear_model <- lm(Number_of_Births ~ poly(Year, 2), data = train_data)

# Make predictions for the next 20 years using Linear model
future_predictions_linear <- predict(linear_model, newdata = future_data)

# Create predictions dataframe for linear model
predictions_df_linear <- data.frame(Year = future_years, Predicted_Births = future_predictions_linear)

# Visualize the predictions and historical data for linear model
ggplot() +
  geom_smooth(data = birth_forest_model, method = "lm", se = FALSE, aes(x = Year, y = as.numeric(Number_of_Births)), color = "blue") +
  geom_line(data = predictions_df_linear, aes(x = Year, y = Predicted_Births), color = "red", linetype = "dashed", size = 1.5) +
  labs(title = "Predicted Births in Bahia for the Next 20 Years",
       x = "Year",
       y = "Number of Births") +
  theme_minimal()
