library(tidyverse)

#Read the data
tickets = read.csv("Lab_1_Data_Ticket-1.csv")

#Adding Upcharge columns to tickets data
tickets <- mutate(tickets,UpCharge = tickets$Price - tickets$Face)

#Group by Game column and output the average price for the game
test <- tickets %>% group_by(Game) %>% summarize(Price = mean(Price))

#Group by days and reseller and output the average upcharge prices of tickets
b <- tickets %>% group_by(Days,Reseller) %>% summarize(UpCharge= mean(Price-Face))

#Visualize with line graph to see the pattern of how does the upcharge price of tickets fluctuate according to day of baseball tickets
vis <-ggplot(data = b, aes(x = Days,y = UpCharge,color = Reseller )) + 
  geom_line() + geom_point() + scale_x_reverse(breaks = seq(0, 100, by = 10)) + 
  scale_y_continuous(breaks = seq(0, 100, by = 10))+ 
  labs(title = "Upcharge As Event Day Approaches", y = "Ticket Upcharge($)") + 
  theme_classic() + 
  theme(axis.title = element_text(size = 14),axis.text = element_text(size = 12),legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),plot.title = element_text(hjust=0.5,size = 15)) + 
  scale_color_manual(values = c("red","orange","blue","black"))

#First Visualization
vis

#Change the data type of Opdraw and create new column in the dataframe
tickets <- mutate(tickets,Capacity = as.factor(Opdraw))

#Group by Opponent and Capacity column and then output the average price difference between reseller prices and tickets actual price
tkprice <- tickets %>% group_by(Opponent,Capacity) %>% summarise(PriceDifference = mean(Price - Face))

#Change the data type of Capacity to numeric data type
tkprice <- mutate(tkprice,Capacity = as.numeric(as.character(Capacity)))

#Visualize the data with bar graph to see the relationship between the upcharge prices with Opponent and stadium capacity
plot <- ggplot(tkprice, aes(x = reorder(Opponent,desc(PriceDifference)), y = PriceDifference)) + 
  geom_bar(stat = "summary",fun = 'mean', aes(fill = Capacity)) + 
  scale_fill_gradient(low = 'green', high = 'red',name="Stadium Capacity",labels = scales::percent) + 
  labs(title = "Ticket Upcharge based on Opponent", x = "Opponent", y = "Average Ticket Upcharge($)") + 
  theme_classic() + 
  theme(axis.title = element_text(size = 14),axis.text = element_text(size = 12),legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),plot.title = element_text(hjust = .5,size = 15))

#Second Visualization
plot

#Filter data for Premium and Regular game
premium_data <- tickets[tickets$Game == "Premium", ]
regular_data <- tickets[tickets$Game == "Regular", ]


premium_data <- remove_outliers(premium_data, "Price")
regular_data <- remove_outliers(regular_data, "Price")

#Visualize the data with density plot for premium and regular game to see the relationship average ticket prices for each resellers
ggplot() +
  geom_density(data = premium_data, aes(x = Price, fill = Reseller), alpha = 0.5) +
  geom_vline(data = premium_data, aes(xintercept = mean(Price)), color = "black", linetype = "dashed") +
  geom_text(data = premium_data, aes(x = mean(Price), y = 0, label = "Avg Face Value"), vjust = -1) +
  geom_density(data = regular_data, aes(x = Price, fill = Reseller), alpha = 0.5) +
  geom_vline(data = regular_data, aes(xintercept = mean(Price)), color = "black", linetype = "dashed") +
  geom_text(data = regular_data, aes(x = mean(Price), y = 0, label = "Avg Face Value"), vjust = -1) +
  facet_wrap(~Game, ncol = 2, scales = "free_y") +
  labs(title = "Distribution of Prices for Premium and Regular Games (Outliers Removed)",
       x = "Price",
       y = "Density") +
  scale_fill_manual(values = c("blue", "red", "green", "orange")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"))  

