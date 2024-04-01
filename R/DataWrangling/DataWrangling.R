
#------------------------------------------------------------------------------
#  Data Wrangling with R Software
#  Author: Akoeugnigan Idelphonse SODE
#  LinkedIn: https://www.linkedin.com/in/idelphonse-akoeugnigan-sode-05015672/
#-------------------------------------------------------------------------------


# Main libraries
library(dplyr)
library(ggplot2)

# Import the CSV file
df <- read.csv("Salary.csv")


#--------------------------------------------
# Data manipulation using SQL-like commands
#--------------------------------------------


# Mean salary by city for adults
df_mean <- df %>% 
  filter(age > 18) %>% 
  group_by(country_id) %>%
  summarise(mean_salary = mean(salary, na.rm =T))
print(df_mean)

# Salary by selected country and age group
df %>%
  select(country_id, age, sex, city, salary) %>%
  mutate(adult = age >= 18)  %>% 
  filter(city %in% c("Cotonou", "Lagos", "Niamey")) %>% 
  group_by(city, adult) %>% 
  summarise(mean_salry = mean(salary, na.rm =T))

# Salary brackets using quantile as break points
brkpts <- quantile(df$salary, na.rm = TRUE)
df <- df %>% 
  mutate(salary_cl = cut(salary, breaks = brkpts, include.lowest = T)) 

# Mean of salary by salary bracket
df_cl <- df %>% 
  group_by(salary_cl) %>% 
  summarise(mean_salary = mean(salary, na.rm =T)) %>%
  na.omit(.)
df_cl


#----------------------------
#  Data visualization in R
#----------------------------


## a) Plot of quantitative variable

# Generic barplot of mean salary 
barplot(mean_salary ~ country_id, data = df_mean, 
        xlab = "Country", ylab = "Mean salary (thousands unit)")

# Barplot of mean salary by salary bracket
ggplot(data = df_cl, aes(x = salary_cl, y = mean_salary)) + 
  geom_bar(stat = "identity", width = 0.6, fill = "steelblue") +
  labs(x = "Salary brackets", y = "Mean salary (thousands unit)") +
  geom_text(aes(label = mean_salary), vjust = 1.6, color = "white") 

# Customize the bar colors and legend position
ggplot(data = df_cl, aes(x = salary_cl, y = mean_salary, fill = salary_cl)) + 
  geom_bar(stat = "identity", width = 0.6) +
  labs(x = "Salary brackets", y = "Mean salary (thousands unit)") +
  geom_text(aes(label = mean_salary), vjust = 1.6, color = "white") +
  theme(legend.position = "top") +
  theme_bw() 


## b) Plot of categories frequency

# Frequency of participants by country
ggplot(data = df, aes(x = country_id)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(x = "Country", 
       y = "Frequency", 
       title = "Participants by country") 
  
# Distribution of salary
ggplot(data = df, aes(x = factor(salary))) +
  geom_bar(fill = "cornflowerblue", color = "black") +
  labs(x = "Salary (thousands unit)", 
       y = "Frequency") 


## c) Plot of the raw data points:

# Scatter plot of salary vs age by sex
ggplot(data = df, aes(x = age, y = salary, color = sex)) +
  geom_point(na.rm = TRUE) +
  labs(x = "Age", 
       y = "salary (thousands unit)", 
       title = "Slary by age")

# Boxplot of salaries by country
ggplot(data = df, aes(x = country_id, y = salary)) +
  geom_boxplot(aes(fill = country_id), na.rm = TRUE) +
  labs(x = "Country", y = "Salary (thousands unit)") +
  stat_summary(fun = mean, geom ="point", size =2, color="red", na.rm = TRUE,
             position = position_dodge(width = .75))
  
# Boxplot of salaries by country and sex
ggplot(data = df, aes(x = country_id, y = salary)) +
  geom_boxplot(aes(fill = country_id), na.rm = TRUE) +
  labs(x = "Country", y = "Salary (thousands unit)") +
  stat_summary(fun = mean, geom ="point", size =2, color="red", na.rm = TRUE, 
               position = position_dodge(width = .75)) +
  facet_wrap(~ sex)
  
# Violin plot of salaries by country
ggplot(data = df, aes(x = country_id, y = salary)) +
  geom_violin(aes(fill = country_id), na.rm = TRUE) +
  labs(x = "Country", y = "Salary (thousands unit)") +
  stat_summary(fun = median, geom ="point", size = 2, color ="red", na.rm = TRUE, 
                 position = position_dodge(width = .75))
