## Install packages for data transformation and visualization
install.packages("tidyverse")
install.packages("stargazer")

## Load packages
library(tidyverse)
library(stargazer) # Gives neat tables as output for various data frames

## Set project path - YOU NEED TO CHANGE THIS PATH
setwd("C:/Users/Matthias/Desktop/Google_Data_Analytics_Capstone_Project_Container")
getwd()

## Import and first look at data
df <- read.csv2("core_data_v3.csv")
summary(df)
stargazer(df[c("time_sec",
                 "team_size",
                 "item_weight_kg",
                 "item_volume_cc",
                 "palette_quantity",
                 "palette_number",
                 "item_time_sec")],
          type = "latex",
          title = "Descriptive Statistics",
          out = "table1.tex",
          covariate.labels = c("Time per Palette (seconds)",
                               "Team Size",
                               "Item Weight (kilograms)",
                               "Item Volume (cubic centimeters)",
                               "Item Quantity per Palette",
                               "Number of Palettes",
                               "Time per Item (seconds)"))

## First visualization for the variable of interest "time_sec"
histo_df1 <- df %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = time_sec, fill = "red")) +
  guides(fill="none") +
  labs(x = "Time per Palette \n(seconds)",
       title = "How long does it take to fully stack a palette?",
       subtitle = "distribution of palette completion times via histogramm",
       caption = "source: own data")
        
histo_df1
ggsave("histogram_palette_time_team.png", plot = histo_df1)

### "time_sec" and "team_size"
## Variables "time_sec" and "team_size" in relation as box plot, sample sizes annotated
# Define a function that returns sample sizes which will be annotated to the visualizations 
sample_size_finder <-  function(variable, value){ 
  df %>% 
    filter(.data[[variable]] == value) %>%
    count()
}

# Build the box plot
str_0<- "n ="
ss_team_a = sample_size_finder("team_size", 3)
ss_team_b = sample_size_finder("team_size", 4)
ss_team_c = sample_size_finder("team_size", 5)
ss_team_d = sample_size_finder("team_size", 6)

box_df1 <- df %>% 
  ggplot() +
  geom_boxplot(mapping = aes(group = team_size, x = team_size, y = time_sec)) +
  scale_x_discrete(limits = c(3, 4, 5, 6)) +
  labs(x = "Team Size", y = "Time per Palette \n(seconds)", title = "How does the team size impact palette completion time?",
       subtitle = "comparison via box plot \nn = sample size",
       caption = "source: own data") +
  annotate("text", x = 3, y = 75, label = paste(str_0, ss_team_a)) +
  annotate("text", x = 4, y = 50, label = paste(str_0, ss_team_b)) +
  annotate("text", x = 5, y = 25, label = paste(str_0, ss_team_c)) +
  annotate("text", x = 6, y = 1, label = paste(str_0, ss_team_d))

box_df1
ggsave("boxplot_palette_time_team_size.png", plot = box_df1)

## Show means for team_size
# Define a function that returns number of observations for a variable's given value
add_observations_time_sec <- function(variable, value){
df %>% 
  filter(.data[[variable]] == value) %>% 
  select(time_sec) %>%
  sum()
}
# Gather the number of observations for given values for "team_size"
time_sec_team_size_3 = add_observations_time_sec("team_size", 3)
time_sec_team_size_4 = add_observations_time_sec("team_size", 4)
time_sec_team_size_5 = add_observations_time_sec("team_size", 5)
time_sec_team_size_6 = add_observations_time_sec("team_size", 6)

# Calculate the mean of "time_sec" for the individual "team_size"
mean_team_3 = time_sec_team_size_3/ss_team_a
mean_team_4 = time_sec_team_size_4/ss_team_b
mean_team_5 = time_sec_team_size_5/ss_team_c
mean_team_6 = time_sec_team_size_6/ss_team_d

mean_team <- c(mean_team_3, mean_team_4, mean_team_5, mean_team_6)

help_df <- data.frame(team_size = c(3:6),
                      mean_time_sec = as.double(mean_team))

helpf_df

## Transform "team_size" data and export into csv file for further visualization
# Define a function that returns number of observations for all observations of a variable
sample_size_finder_2 <-  function(variable){
  df %>% 
    select(.data[[variable]]) %>%
    count()
}

# Gather the number of observations for "team_size"
ss_team_size <- sample_size_finder_2("team_size")

team_size_stats <- c(ss_team_size, ss_team_size_a, ss_team_size_b, ss_team_size_c, ss_team_size_d)
team_size_stats

# Export team_size_stats as csv file for visualization in MS-Excel
write.csv2(team_size_stats, "pie_chart_team_size.csv")

## Variables "time_sec" and "item_volume_cc" in relation as box plot, sample sizes annotated
# Gather the number of observations for given values of "item_volume_cc"
ss_volume_a <- sample_size_finder("item_volume_cc", 30723.00)
ss_volume_b <- sample_size_finder("item_volume_cc", 41538.00)
ss_volume_c <- sample_size_finder("item_volume_cc", 43987.50)
ss_volume_d <- sample_size_finder("item_volume_cc", 64275.75)
ss_volume_e <- sample_size_finder("item_volume_cc", 85800.00)

# Plot as box plot
box_df2 <- df %>% 
  ggplot() +
  geom_boxplot(mapping = aes(group = item_volume_cc, x = item_volume_cc, y = time_sec)) +
  labs(title = "How does the volume of an item impact palette completion time?",
       subtitle = "comparison via box plot \nn = sample size",
       caption = "source: own data",
       x = "Item Volume \n(cubic centimeter)", 
       y = "Time per Palette \n(seconds)") +
  annotate("text", x = 30723.00, y = 150, label = paste(str_0, ss_volume_a)) +
  annotate("text", x = 37500.00, y = 120, label = paste(str_0, ss_volume_b)) +
  annotate("text", x = 47500.00, y = 75, label = paste(str_0, ss_volume_c)) +
  annotate("text", x = 64275.75, y = 50, label = paste(str_0, ss_volume_d)) +
  annotate("text", x = 85800.00, y = 20, label = paste(str_0, ss_volume_e))

box_df2
ggsave("boxplot_palette_time_item_volume_cc.png", plot = box_df2)

## Multiple linear regression, regular and Z-standardized 
# Regular multiple linear regression
mlr_df <- lm(time_sec ~ team_size + item_weight_kg + item_volume_cc + palette_quantity + palette_number, data = df)

summary(mlr_df)
stargazer(mlr_df,
          type = "latex",
          title = "Multiple Linear Regression",
          out = "tablex.txt",
          covariate.labels = c("Team Size",
                               "Item Weight (kilograms)",
                               "Item Volume (cubic centimeters)",
                               "Item Quantity per Palette",
                               "Number of Palette"))

# Data transformation for z-standardized multiple linear regression
z_time_sec <- scale(df$time_sec)
z_team_size <- scale(df$team_size)
z_item_weight_kg <- scale(df$item_weight_kg)
z_item_volume_cc <- scale(df$item_volume_cc)
z_palette_quantity <- scale(df$palette_quantity)
z_palette_number <- scale(df$palette_number)

dfz <- data.frame(z_time_sec,
                  z_team_size,
                  z_item_weight_kg,
                  z_item_volume_cc,
                  z_palette_quantity,
                  z_palette_number)

# Z-standardized multiple linear regression
Z_mlr_df <- lm(z_time_sec ~ z_team_size + z_item_weight_kg + z_item_volume_cc + z_palette_quantity + z_palette_number, data = dfz)

summary(Z_mlr_df)
stargazer(Z_mlr_df, type = "latex",
          title = "Z-Standardized Multiple Linear Regression",
          out = "tablez.txt",
          covariate.labels = c("Team Size",
                               "Item Weight (kilograms)",
                               "Item Volume (cubic centimeters)",
                               "Item Quantity per Palette",
                               "Number of Palette"))
