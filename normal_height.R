library("tidyverse")
library(readxl)

#all heights in cm
cite <- "https://vincentarelbundock.github.io/Rdatasets/doc/carData/Davis.html"
height <- read_csv("/Users/marty/Dropbox (Personal)/Coding Projects/DataSets/height_weight.csv")

height <- height %>% select(-1)

height_quants <- height %>% summarise(q5 = quantile(height, .05),
                                      q95 = quantile(height, .95),
                                      avg = mean(height),
                                      median = median(height))

x.dens <- density(height$height)
df_height_den <- data.frame(x = x.dens$x, y = x.dens$y)

bad_height <- height %>% summarize("Average Height" = mean(height),
                     "Median Height" = median(height),
                     "Shortest" = min(height),
                     "Tallest" = max(height),
                     "Standard Deviation, in Centimeters" = sd(height))


total_height <- height %>% 
  ggplot(aes(x = height)) + 
  geom_density(fill = "#a5d3c1") + 
  labs(x = "Height in Centimeters", y = "Probability Distribution")+
  theme(axis.text = element_text(family = "Nobile", size = 18 ),
        axis.title = element_text(family = "Nobile", size = 17 ))


bad_proof <- total_height + 
  geom_area(data = subset(df_height_den, x >= height_quants$q5 & x <= height_quants$q95), 
            aes(x = x, y = y), fill = "#9b9292") + 
  geom_vline(aes(xintercept = height_quants$median, color = "Median"), size = 1.5) +
  geom_vline(aes(xintercept = mean(height), color = "Mean"), size = 1.5) +
    scale_color_manual(name = "Descriptive Statistics", values = c(Median = "red", Mean = "#eebf48")) +
  theme(axis.text = element_text(family = "Nobile", size = 18 ),
        axis.title = element_text(family = "Nobile", size = 17 ))
    


total_height_bar <- height %>% group_by(sex) %>% 
  ggplot(aes(x = height)) + geom_bar(stat = "count", fill = "#a5d3c1", col = "black") +
  labs(x = "Height in Centimeters", y = "Total at that Height")+
  theme(axis.text = element_text(family = "Nobile", size = 18 ),
        axis.title = element_text(family = "Nobile", size = 17 ))


total_height_chart <- height %>% group_by(sex) %>%
  summarize("Average Height" = mean(height),
            "Median Height" = median(height),
            "Shortest" = min(height),
            "Tallest" = max(height),
            "Count" = n(),
            "Standard Deviation" = sd(height))

total_height_sex_histogram <- height %>% 
  group_by(sex) %>% 
  ggplot(aes(x = height, fill = sex)) + geom_histogram( aes(y = ..density..), col = "black", position = "dodge") +
  labs(x = "Height in Centimeters", y = "Total at that Height", caption = paste("Data from: " , cite))+
  theme(axis.text = element_text(family = "Nobile", size = 18 ),
        axis.title = element_text(family = "Nobile", size = 17 ))

height_men <- height %>% filter(sex == "M")
height_women <- height %>% filter(sex == "F")

height_men_hist <- height_men %>%
  ggplot(aes(x = height)) + geom_histogram(aes(y = ..density..), fill = "blue", col = "black") +
  labs(x = "Height in Centimeters", y = "Total at that Height", caption = paste("Data from: " , cite))+
  theme(axis.text = element_text(family = "Nobile", size = 18 ),
        axis.title = element_text(family = "Nobile", size = 17 ))


height_men_hist + stat_function(fun = dnorm,
                               args = list(mean = mean(height_men$height), sd(height_men$height)),
                               col = "blue",
                               lwd = 1)

height_women_hist <- height_women %>%
  ggplot(aes(x = height, fill = sex)) + geom_histogram(aes(y = ..density..), fill = "#990000", col = "black") + 
  labs(x = "Height in Centimeters", y = "Total at that Height", caption = paste("Data from: " , cite))+
  theme(axis.text = element_text(family = "Nobile", size = 18 ),
        axis.title = element_text(family = "Nobile", size = 17 ))


height_women_hist + stat_function(fun = dnorm,
                                   args = list(mean = mean(height_women$height), sd(height_women$height)),
                                   col = "black",
                                   lwd = 1) + 
  labs(y = "Probability Distribution")

prob_57 <- pnorm(57, mean = mean(height_women$height), sd = sd(height_women$height))
prob_164 <- pnorm(164, mean = mean(height_women$height), sd = sd(height_women$height))
                                                        