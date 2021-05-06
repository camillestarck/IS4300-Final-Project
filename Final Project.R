# Camille Starck
# IS 4300 Final Project 
# Script used to create the R Markdown file


library(tidyverse)

urlfile <- "https://vincentarelbundock.github.io/Rdatasets/csv/AER/CASchools.csv"
data <- read_csv(url(urlfile))
# Link about the dataframe
# https://vincentarelbundock.github.io/Rdatasets/doc/AER/CASchools.html
data <- as.data.frame(data)
str(data)


# ------------------------------------------------------------------------------
# Do schools that have more English learners have lower scores?
hist(data$english)
# We want to use the median instead of the mean because this is skewed data
m <- median(data$english)

# If the percent of English learners is higher than the median, then label the row as 'more'. Otherwise 'less'
# Also, calculate the score as the average math and reading score. Then summarize the data by averaging the scores
# of the two groups.
data3 <- data %>% mutate(p_eng = ifelse(english > m, 'more', 'less')) %>% mutate(score = (read+math)/2)
scores <- data3 %>% group_by(p_eng) %>% summarize(mean=mean(score))
scores
# Store the scores of each group in vectors for the t test
more_score <- data3 %>% filter(p_eng == 'more') %>% select(score)
less_score <- data3 %>% filter(p_eng == 'less') %>% select(score)
# Perform the t test
t.test(more_read, less_read, alternative = 'less')
# Null: The schools with more English learners have the same or higher test scores. 
# Alternative: The schools with more English learners have lower test scores. 
# Conclusion: Reject the null.

# Visualization of t test
d <- data3 %>% ggplot()
d + geom_density(aes(score, fill=english_perc), alpha = .5) + 
  labs(title='Test Scores', subtitle='Figure 2') +
  scale_fill_manual(name="English Learners", values = c('lightskyblue','darkblue'))

# ------------------------------------------------------------------------------
# Simple linear regression
# Look at correlation coefficients
num_data <- data3 %>% select(students, teachers, calworks, lunch, expenditure, income, computer, english, score)
round(cor(num_data),2)

lm1 <- lm(data3$score ~ data3$lunch)
summary(lm1) # RMSE = 9.447. 

# Visualization of simple linear regression
p <- data3 %>% ggplot()
p + geom_point(aes(x = lunch, y = score), col = 'lightskyblue') +
  geom_abline(slope = -.61029, intercept = 681.43952, col = 'darkblue') +
  labs(title = "Scores Predicted by Reduced Lunch Percentage", x = 'Reduced Lunch Students (%)',
       y = 'Average Math & Reading Score') + 
  geom_text(x= 50, y=690, label='RMSE = 9.447', size=3) +
  geom_text(x = 50, y = 695, label = 'y = -.61x + 681.44', size = 4)
# ------------------------------------------------------------------------------
# Multiple Linear Regression
# Try many models and find one that minimizes the RMSE value but doesn't overfit

lm2 <- lm(data3$score ~ data$english + data$students + data$teachers + data$calworks +
            data$lunch + data$expenditure + data$income + data$computer)
summary(lm2) # RMSE = 8.419 but not all included variables are significant

lm3 <-lm(data3$score ~ data$english + data$lunch + data$expenditure + data$income)
summary(lm3) # RMSE = 8.424
plot(residuals(lm3))

lm4 <- lm(data3$score ~ data$english + data$lunch + data$income)
summary(lm4) # RMSE = 8.498

# lm3 has a good balance between significant variables and relatively small RMSE value


# Visualization of multiple linear regression residuals

length(residuals(lm1)) # 420
max(residuals(lm3)) # around 28.3
min(residuals(lm3)) # around -30.7

# Make a tidy data set containing residuals for models 1 and 4
ReComp <- data.frame(x = c(1:420,1:420),residuals = c(residuals(lm1),residuals(lm3)),
                     Model = c(rep("Model1",420),rep("Model3",420)))
errorPlot <- ReComp %>% ggplot()
errorPlot + geom_point(aes(x,residuals,col=Model),size=2)+
  labs(title="Residual Error Comparison")+
  geom_abline(intercept = 28.3,slope = 0,linetype = 2)+
  geom_abline(intercept = -30.7,slope = 0,linetype = 2) + 
  scale_color_manual(values = c('lightskyblue', 'mediumblue'), 
                     labels = c('Model1' = "Simple", 'Model3' ="Multiple"))

# This visual shows that the residuals of Model 3 are slightly better than Model 1, but only by a couple of points. 
# We can conclude that Model 1 or Model 3 could be used effectively. We include both in the .RMD file 