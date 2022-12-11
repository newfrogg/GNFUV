## Get data from remote
# Extract the .zip file from https://archive.ics.uci.edu/ml/machine-learning-databases/00452/GNFUV%20USV%20Dataset.zip
## Setup
# Import needed library
library(stringr)
library(ggplot2)
library(corrplot)
library(rstudioapi)
library(lubridate)
# Change working directory to current script directory
setwd( dirname(getSourceEditorContext()$path) )
# Clear env variable
rm(list = ls())
# Clear R console
cat("\014")
## Coding
# Loading data

files <- list.files(path = getwd(), pattern = 'csv', full.names = TRUE, recursive = TRUE)
col_name <- c("device", "humidity", "temperature", "experiment", "time")

Df <- data.frame()
for(i in 1:4) {
    Df <- rbind(Df, read.csv(files[i], header = FALSE, col.names = col_name))
}

# Cleaning data
Df$device <- str_replace_all(Df$device, pattern = "'", "")
Df$device <- str_replace_all(Df$device, pattern = "\\{device: ", "")
Df$device <- str_replace_all(Df$device, pattern = "gnfuv-temp-exp1-55d487b85b-", "")

Df$device <- str_replace(Df$device, pattern = "2bl8b", "mst_asv")
Df$device <- str_replace(Df$device, pattern = "5g2xh", "plady_fleet1")
Df$device <- str_replace(Df$device, pattern = "5ztk8", "plady_fleet3")
Df$device <- str_replace(Df$device, pattern = "xcl97", "plady_fleet2")
Df$device <- factor(Df$device, levels=c("mst_asv","plady_fleet1","plady_fleet2","plady_fleet3" ))

Df$humidity <- str_replace(Df$humidity, pattern = "'humidity': ","")
Df$humidity <- suppressWarnings(as.numeric(Df$humidity))

Df$temperature <- str_replace(Df$temperature, pattern = "'temperature': ","")
Df$temperature <- suppressWarnings(as.numeric(Df$temperature))

Df$experiment <- str_replace(Df$experiment, pattern = "'experiment': ","")

Df$humidity <- str_replace(Df$humidity, pattern = "None","")

Df$time <- str_replace(Df$time, pattern = "'time': ","")
Df$time <- str_replace(Df$time, pattern = "\\}","")
Df$time <- as.numeric(Df$time)

date_time <- data.frame(Df$device,as_datetime(Df$time))

na_loc <- apply(is.na(Df), 2, which)
na_loc

Df <- Df [ -bitwAnd(c(na_loc$humidity), c(na_loc$temperature)) , ]

message("\nAfter cleaning data, we can know that missing humidity and temperature appear mostly in pi5 dataset 
caused by missing value in the 4th Df.")
message("\nClearly, 4 datasets were sampled in experiment 1 so the experiment attribute can and should be ignored")
Df <- Df[, -4]
Df$humidity <- suppressWarnings(as.numeric(Df$humidity))
# Data visualization
print(summary(Df))

# Device
stat_device <- ggplot(Df, aes(x = device)) + 
    stat_count(color="#3b4ef1", fill="#a4adf9") +
    labs(title="Device Histogram Plot", x="Device", y="Count") +
    coord_flip() +
    theme_grey()
stat_device

# Humidity
hist_humid <- ggplot(Df, aes(x = humidity)) + 
    geom_histogram(color="#3b4ef1", fill="#a4adf9", binwidth = 3) +
    labs(title="Humidity Histogram Plot", x="Humid", y="Count") +
    theme_grey()
hist_humid

box_humid <- ggplot(Df, aes(x = humidity, y = device, color = device)) + 
    geom_boxplot(outlier.size = 3) +
    labs(title="Humidity Box Plot", x="Humid", y="Device") +
    theme_grey()
box_humid

qq_humid_by_device <- ggplot(Df, aes(sample = humidity, shape = device, color = device)) +
    stat_qq() +
    stat_qq_line(size = 1.25) +
    labs(title="Humidity QQ Plot grouped by devices",x="Theorectical", y="Sample") +
    scale_color_manual(values = c("#1ff976", "#E69F00", "#56B4E9","#212123")) +
    scale_shape_manual(values = c(22,23,24,25)) +
    theme(legend.position = "bottom") 
qq_humid_by_device

qq_humid <- ggplot(Df, aes(sample = humidity)) +
    stat_qq(color="#3b4ef1") + 
    stat_qq_line(size = 1.25) + 
    labs(title="Humidity QQ Plot ",x="Theorectical", y="Sample") + 
    theme_grey()
qq_humid

## Temperature
hist_temp <- ggplot(Df, aes(x = temperature)) +
    geom_histogram(color="#3b4ef1", fill="#a4adf9", binwidth = 3) +
    labs(title="Temperature Histogram Plot", x="Temperature", y="Count") +
    theme_grey()
hist_temp

box_temp <- ggplot(Df, aes(x = temperature, y = device, color = device)) + 
    geom_boxplot(outlier.size = 3) +
    labs(title="Temperature Box Plot", x="Temperature", y="Device") +
    theme_grey()
box_temp

qq_temp_by_device <- ggplot(Df, aes(sample = temperature, shape = device, color = device)) +
    stat_qq() +
    stat_qq_line(size = 1) +
    labs(title="Temperature QQ Plot grouped by devices",x="Theorectical", y="Sample") +
    scale_color_manual(values = c("#1ff976", "#E69F00", "#56B4E9","#212123")) +
    scale_shape_manual(values = c(22,23,24,25)) +
    theme(legend.position = "bottom") 
qq_temp_by_device

qq_temp <- ggplot(Df, aes(sample = temperature)) +
    stat_qq(color="#3b4ef1") + 
    stat_qq_line(size = 1.25) + 
    labs(title="Temperature QQ Plot ",x="Theorectical", y="Sample") + 
    theme_grey()
qq_temp

corr_matrix <- corrplot(cor(Df[,c(2,3,4)]), method = "number", type = "lower")
# Inferential statistics


# confidence interval & 1 sample test
pairs(temperature ~ humidity+device+time, data = Df)

# t-test humid vs temperature
t.test(Df$humidity, Df$temperature, paired = T, mu = 0, alternative = "greater")

## Linear regression model

model1 <- lm(temperature ~ humidity+time+device, data = Df)

# Model explanations
summary(model1)

# Model visualization
plot(model1)

# ANOVA 
anova(model1)

# Open a new separate plot window (for easier watching)
## windows()




