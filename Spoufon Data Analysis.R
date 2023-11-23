library(tidyverse)

#Analysis 1
#Read file
data <- read_rds("Lab_2_data-1.rds")
view(data)

#Mutate and Group data
data1 <- data %>% group_by(dealID, Category) %>% 
  summarize(number_of_purchases = n(),  day_until_purchased = (max(OrDoW) - min(DlDow)))

#Visualize the pattern
#Trying to find the pattern of how long to keep the deals in each category active by plotting bargraph
bargraph <- ggplot(data1, aes(x=factor(day_until_purchased), y=number_of_purchases)) +
  geom_bar(stat='identity', aes(fill = Category), position='dodge', width=0.7) +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~Category) +
  labs(x='Days Till Purchased', y='Number of Purchases', title='Time Interval Between Deal Offering and Purchase Across Categories') +
  theme_bw() +
  theme(strip.text = element_text(size = 9, face = "bold"))
bargraph

#Analysis 2
#Mutate data to analyze
day <- data %>% group_by(OrderTime,Category) %>% summarize(qty = sum(qty),price = mean(price))
day$OrderTime <- as.character(day$OrderTime)
day$hour <- as.numeric(sapply(strsplit(day$OrderTime, ":"), function(x) x[1]))
day$minute <-  as.numeric(sapply(strsplit(day$OrderTime, ":"), function(x) x[2]))
day$hours_passed <- round(day$hour + (day$minute / 60),digits = 2)
day$rounded <- round(day$hours_passed)
test <- day %>% group_by(hours_passed,Category) %>% summarize(qty = sum(qty))
test <- day %>% group_by(rounded,Category) %>% summarize(qty = sum(qty))

#Visualize the pattern of when customers are purchasing the most deals throughout the days
test_day <- ggplot(test,aes(x = rounded,y = qty,color = Category)) + 
  theme_classic() + 
  labs(title = "Deals Purchased During Average Day",x = "Elapsed Hours",y = "Deals Purchased") + 
  scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
  scale_color_manual(values = c("red","orange","blue","yellow","gray","orange","black"))+
  geom_line()  + 
  scale_x_continuous(breaks = seq(0, 24, by = 2)) + 
  theme(axis.title = element_text(size = 14),axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),legend.text = element_text(size = 12),plot.title = element_text(hjust=0.5,size = 15))
test_day


#Analysis 3
#Visualize the pattern of price points and quantities that result in the highest revenue for the different product categories
ggplot(data, aes(x = qty, y = total_revenue, color = price)) +
  geom_point(shape = 16, alpha = 0.7, size = 2) +
  facet_wrap(~ Category, scales = "free_y") +
  scale_color_gradient(low = "cyan", high = "red", trans = "sqrt") +
  labs(title = "Relationship Between Quantity and Total Revenue by Category",
       x = "Quantity",
       y = "Total Revenue") +
  theme_minimal()

