###############################
##### Created by Team 15 
##### MBAN2 Hult 2021
##### Air France case 
##### Date : 10.21.2021
##### Version 0.4
###############################

library(readxl)
DC <- read_excel("C:/Users/elyou/Desktop/R/Air france/Air France Case Spreadsheet Supplement.xls", 
                 sheet = "DoubleClick")
View(Air_France_Case_Spreadsheet_Supplement)


library(readxl)
kayak0 <- read_excel("C:/Users/elyou/Desktop/R/Air france/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "Kayak")
View(Air_France_Case_Spreadsheet_Supplement)

#cleaning kayak data 


kayak1 <- na.omit(kayak0) 

###### We can not convert character to numeric 
# we will create kayak1 from the crutch with numeric Data

Search.Engine <- c("kayak","kayak")
Clicks<- c(2839,2839)
Media.Cost<- c(3567.1334999999999,3567.1334999999999)
Total.Bookings<- c(208,208)
Avg.Ticket<- c(1123.5288461538421,1123.5288461538421)
Total.Revenue<- c(233693.99999999916,233693.99999999916)
Net.Revenue<- c(230126.86649999916,230126.86649999916)

kayak2 <- data.frame(Search.Engine,Clicks,Media.Cost,Total.Bookings,Avg.Ticket,
                       Total.Revenue,Net.Revenue)
my_kayak <- kayak2[1,]

#Creating a new data frame 

my_df_DC <- data.frame(DC$`Publisher Name`,DC$Clicks,DC$`Click Charges`,
                       DC$Amount, DC$`Total Cost`,DC$`Total Volume of Bookings`)

#Aggregation of the rows 

my_df_DC_sum <-  rowsum(my_df_DC[,2:6],my_df_DC$DC..Publisher.Name.)


# To compare with Kayak we need same template
# Calculation in my_df_sum Average Tickets/ total revenue/ Net revenue 

##Average Tickets 
my_df_DC_sum$Avg_Ticket <- my_df_DC_sum$DC.Amount/my_df_DC_sum$DC..Total.Volume.of.Bookings.

##Total revenue
my_df_DC_sum$Total_Revenue <- my_df_DC_sum$DC.Amount

## Net Revenue 

my_df_DC_sum$Net_Revenue <- my_df_DC_sum$Total_Revenue- my_df_DC_sum$DC..Total.Cost.

## Creating new data frame with Double click on the same structure of Kayak

Publisher_Names <- data.frame(c("Google_Global","Google_US","MSN_Global","MSN_US","Overture_Global",
                     "Overture_US", "Yahoo_US"))

my_DC_better <- data.frame(Publisher_Names,my_df_DC_sum$DC.Clicks,my_df_DC_sum$DC..Total.Cost,
                           my_df_DC_sum$DC..Total.Volume.of.Bookings.,
                           my_df_DC_sum$Avg_Ticket,my_df_DC_sum$Total_Revenue,
                           my_df_DC_sum$Net_Revenue)

#Renaming the variable in Dome.of.Bookings.,
  
install.packages('dplyr')
library(dplyr)

DC_final<- rename(my_DC_better, Search.Engine = c..Google_Global....Google_US....MSN_Global....MSN_US....Overture_Global...,
                  Clicks=my_df_DC_sum.DC.Clicks,Media.Cost=my_df_DC_sum.DC..Total.Cost,
                  Total.Bookings= my_df_DC_sum.DC..Total.Volume.of.Bookings.,
                  Avg.Ticket = my_df_DC_sum.Avg_Ticket, Total.Revenue= my_df_DC_sum.Total_Revenue,
                  Net.Revenue= my_df_DC_sum.Net_Revenue)

#Combine the Double Click and Kayak Data 
my_df <- rbind(DC_final,my_kayak)

# calculate Bookings per Dollar and Net Revenue per booking

my_df$Booking.per.Dollar <- my_df$Total.Bookings/my_df$Media.Cost

my_df$Net.Revnue.per.Booking <- my_df$Net.Revenue/my_df$Total.Bookings

library(ggplot2)


my_scatter <- ggplot(my_df, aes(x=    Booking.per.Dollar, y=Net.Revnue.per.Booking, 
                      label=Search.Engine))+
                       geom_point(color = "red", size = 2)+
                        geom_text(aes(label= Search.Engine),size=4,
                                  hjust=0, vjust=0)

library(plotly)
ggplotly(my_scatter)

### Bar chart for Net revenue search Engine 

ggplot(my_df, aes(x = reorder(interaction(Search.Engine),-Net.Revenue),
                  y = Net.Revenue)) +
  labs(y="Net.Revenue",x="Search.Engine")+
  
  geom_bar(stat ="identity", fill= "#002157")+theme_light()+
  
  ylim(0,1500000)+
  
  geom_text(aes(label =paste(round((my_df$Net.Revenue/sum(my_df$Net.Revenue))*100), "%", sep="") 
                             , vjust = -0.2),col="#F71d25")


### Bar chart for Total cost search Engine 

ggplot(my_df, aes(x = reorder(interaction(Search.Engine),-Media.Cost),
                  y = Media.Cost)) +
  labs(y="Media.Cost",x="Search.Engine")+
  
  geom_bar(stat ="identity", fill= "#F71d25")+theme_light()+
  
  ylim(0,400000)+
  
  geom_text(aes(label =paste(round((my_df$Media.Cost/sum(my_df$Media.Cost))*100), "%", sep="") 
                , vjust = -0.2),col="#002157")

### Creating pie chart for the % share market stratigy 

# 3D Exploded Pie Chart
install.packages("plotrix")
library(plotrix)

install.packages("RODBC")
library(RODBC)


slices <- my_df$Total.Bookings
lbls <- my_df$Search.Engine
pct <- round(slices/sum(slices)*100)    
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie3D(slices,labels = lbls,explode=0.1,main="Booking per Search Engine", 
      labelcex = 0.8)

#optimization 

# Multiplication of Booking per dollar and Net revenue per Booking 
Kayak <- 0.058310125*1106.3792
Yahoo_US <- 0.014329679*1262.9775
MSN_US <- 0.008696469*1181.7951
Google_US <- 0.004382981*897.9621

#Calculating the optimal combination weighted for Engine research 

Total <- Kayak+Yahoo_US+MSN_US+Google_US


w_kayak <- Kayak/Total
print(w_kayak)

w_Yahoo_US <- Yahoo_US/Total
print(w_Yahoo_US)

w_MSN_US <- MSN_US/Total
print(w_MSN_US)

w_Google_US <- Google_US/Total
print(w_Google_US)

# Optimal combination is : 
# 0.66*kayak + 0.19*Yahoo_US +0.11*MSN_US +0.04*Google_US 

# Comparing between current Model and our Model;
# creating New data farm with wight of each Engine research 

New_df <- rbind(my_df[2,],my_df[4,],my_df[7,],my_df[8,])

New_df$current_model <- c(0.47,0.02,0.06,0.001)

New_df$our_model <- c(0.04,0.11,0.19,0.66)

# Calculating the sum of Net revenue per Booking for each Model 

net_revenue_per_booking_current_model  <- sum(New_df$current_model*New_df$Net.Revnue.per.Booking)
print(net_revenue_per_booking_current_model)

#522 dollars 

net_revenue_per_booking_our_model  <- sum(New_df$our_model*New_df$Net.Revnue.per.Booking)
print(net_revenue_per_booking_our_model)
#1136 dollars 

#with our Model we double the net revenue per booking 

