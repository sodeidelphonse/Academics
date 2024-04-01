
# Main libraries
library(dplyr)
library(ggplot2)

# Import the CSV file
df <- read.csv("Salary.csv")

# Data manipulation using SQL-like commands:

# Man salary by city for adults
df %>% 
  filter(age > 18) %>% 
  group_by(city) %>%
  summarise(mean_salry = mean(salary))

# Salary by selected country and maturity level
df %>%
  select(country_id, age, city, salary) %>%
  mutate(adult = age >= 18)  %>% 
  filter(country_id %in% c("BEN", "NIG")) %>% 
  group_by(country_id, adult) %>% 
  summarise(mean_salry = mean(salary, na.rm =T))


## Data visualization using ggplot2

# Scatter plot


# Histogram


# Boxplot 


