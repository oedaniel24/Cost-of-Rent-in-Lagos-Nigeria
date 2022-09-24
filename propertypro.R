#setting up R

library(tidyverse)
library(rvest)
library(robotstxt)
library(xml2)
library(ggplot2)

#SCRAPPING DATA FROM PROPERTYPRO.NG

#check if site can be scrapped
paths_allowed(paths=c("https://www.propertypro.ng/property-for-rent/in/lagos"))


#scrapping data from multiple pages, storing them into vectors and joining them
#in a dataframe
site_url<- "https://www.propertypro.ng/property-for-rent/in/lagos?page="

property_df <- data.frame()

for(page_no in  seq(from = 0, to = 6525, by = 1)){
  page_url <- paste0(site_url,page_no)
  page <- read_html(page_url)
  description <- page %>% html_nodes(".listings-property-title2") %>% html_text()
  location <- page %>% html_nodes("a+ h4") %>% html_text()
  price <- page %>%  html_nodes(".n50 span:nth-child(2)") %>% html_text()
  bed <- page %>%  html_nodes(".fur-areea span:nth-child(1)") %>% html_text()
  baths <- page %>%  html_nodes(".fur-areea span:nth-child(2)") %>% html_text()
  toilet <- page %>%  html_nodes("span~ span+ span") %>% html_text()
  
  
  property_df <- rbind(property_df, data.frame(description,location,price,bed,
                                               baths,toilet))
  
  
  print(paste("page_no:",page_no))
}


#CLEANING SCRAPPED DATA

#creating duplicate of scrapped data for cleaning
property_df_cleaned <- property_df


#filtering out apartments whose payment is per month or per day or per sqm
property_df_cleaned <- property_df_cleaned %>% filter(!grepl("/month|/day|/sqm",
                                                             price))

#filtering out apartments with price less than 20,000 and greater than 50,000,000
property_df_cleaned <- property_df_cleaned %>% filter(price > 20000) %>% 
  filter(price < 50000000)


#cleaning the price column
property_df_cleaned$price <- property_df_cleaned$price %>% str_remove("/year") %>% 
                              str_remove_all( ",") %>%  as.numeric() 

#cleaning the bed column
property_df_cleaned$bed <- property_df_cleaned$bed %>% str_remove(" beds") %>%
                           as.numeric() %>% as.factor() 

#cleaning the baths column
property_df_cleaned$baths <- property_df_cleaned$baths %>% str_remove("baths") %>%
                              str_trim(side = c("both")) %>% as.numeric() %>%

#cleaning the toilet column
property_df_cleaned$toilet <- property_df_cleaned$toilet %>% str_remove("Toilets") %>%
                                str_trim(side = c("both")) %>% as.numeric() %>%

#dropping rows with missing values from bed, baths and toilet
property_df_cleaned <- property_df_cleaned %>% filter(!if_any(c(bed, baths, 
                                                                toilet), is.na)) 


#creating "city" column by extracting second to last word from the "location" column
property_df_cleaned <- property_df_cleaned %>% mutate(city=word(location,-2))


#extracting second to last word from "location" column to create "city" column 
#removed "Victoria" from "Victoria Island". Adding it back
property_df_cleaned$city <- ifelse(property_df_cleaned$city == "Island", 
                              paste("Victoria", property_df_cleaned$city),
                              property_df_cleaned$city)

#extracting second to last word from "location" column to create "city" column 
#removed "Anthony" from "Anthony Village". Adding it back
property_df_cleaned$city <- ifelse(property_df_cleaned$city == "Village", 
                                   paste("Anthony", property_df_cleaned$city),
                                   property_df_cleaned$city)

#removing houses in cities not located in lagos state
property_df_cleaned <- property_df_cleaned %>%
  filter(!city %in% c("Camp", "Guzape", "Gwarinpa")) 

#removing rows where bed, baths and toilet = 0
property_df_cleaned <- property_df_cleaned %>% filter(bed != 0 & baths != 0 & 
                                                        toilet != 0) 


#removing any residual rows that still contained data on non-residential spaces
property_df_cleaned <- property_df_cleaned %>% filter(!grepl("Office", description)) %>% 
  filter(!grepl("Shop", description)) %>% filter(!grepl("Hotel", description)) %>% 
  filter(!grepl("Commercial", description)) %>%  filter(!grepl("Warehouse", description)) %>% 
  filter(!grepl("Truck", description)) %>% filter(!grepl("Seminar", description)) %>% 
  filter(!grepl("Musical", description)) %>% filter(!grepl("Car", description)) 


#creating "house_type" column from the description column
property_df_cleaned <- property_df_cleaned %>% mutate(house_type =   
  ifelse(grepl("[Ff]lat|[Aa]partment|brf", description), "Flats & Apartments",
  ifelse(grepl("Duplex|Terrace|Detached", description), "Duplex",
  ifelse(grepl("Maison", description), "Maisonette",
  ifelse(grepl("Mansion", description), "Mansion",
  ifelse(grepl("Self Contain", description), "Self Contain",
  ifelse(grepl("Bungalow", description), "Bungalow",
  ifelse(grepl("Penthouse", description), "Penthouse", "Others"))))))))
#"others" include houses with vague and uncommon descriptions eg room and parlour,
#town house etc


#creating "new" column to specify newly built or renovated houses 
property_df_cleaned <- property_df_cleaned %>% mutate(new =  
  ifelse(grepl("[Nn]ew", description), "New", "Old")) 
                                                        

#making "house_type" and "new" columns categorical variables
property_df_cleaned$accomodation_type <- property_df_cleaned$accomodation_type %>% 
  as.factor()
property_df_cleaned$new <- property_df_cleaned$new %>% 
  as.factor()

#dropping rows with missing values 
property_df_cleaned <- property_df_cleaned %>% drop_na()


#EXPLORING THE DATASET

#Exploring the price column
summary(property_df_cleaned$price)
#Min: 22000
#1st Qu.: 650000
#Median: 14000000
#Mean: 3426688
#3rd Qu.: 3500000
#Max: 48000000

#creating histogram to show distribution of price column
property_df_cleaned %>% ggplot(aes(x=price)) + scale_x_log10() + geom_histogram(bins = 10) +
  labs(title = "Distribution of Prices")
#distribution appears skewed
#comparing mean and median
#mean > median indicating positive skewness

#Investigating Categorical Variable
#city 
property_df_cleaned %>% ggplot(aes(city,price)) + scale_y_log10() +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Distribution of Prices per City")
#house_type
property_df_cleaned %>% ggplot(aes(house_type,price)) + scale_y_log10() +
  geom_boxplot() + labs(title = "Distribution of Prices per House Type")
#bed
property_df_cleaned %>% ggplot(aes(as.factor(bed),price)) + scale_y_log10() +
  geom_boxplot() + labs(title = "Distribution of Prices per Number of Bedrooms")
#new
property_df_cleaned %>% ggplot(aes(new,price)) + scale_y_log10() +
  geom_boxplot() + labs(title = "Distribution of Prices For New and Old Houses")


#checking if there is a significant difference between the average price per city
#Alpha = 0.05
kruskal.test(price ~ city, data = property_df_cleaned)
#p-value < 2.2e-16


#checking if there is an significant difference between the average price for
#the different house types
#Alpha  = 0.05
kruskal.test(price ~ house_type, data = property_df_cleaned)
#p-value < 2.2e-16


#checking if there is an significant difference between the average price per
#number of beds
#Alpha  = 0.05
kruskal.test(price ~ as.factor(bed), data = property_df_cleaned)
#p-value < 2.2e-16

#checking if there is an significant difference between the average prices for
#new or old buildings
#Alpha  = 0.05
kruskal.test(price ~ new, data = property_df_cleaned)
#p-value < 2.2e-16


#ANALYSING THE DATA

#number of bedrooms
property_df_cleaned %>% group_by(bed) %>% summarise(mean_price = mean(price)) %>% 
 ggplot(aes(bed, mean_price)) + scale_y_log10() + geom_col() +
  labs(title = "Average Price Per Number of Bedroom")
#prices increases with the number of bedrooms 

#new or old building
property_df_cleaned %>% group_by(new) %>% summarise(mean=mean(price)) %>% 
  ggplot(aes(new, mean)) + scale_y_log10() + geom_col() + 
  labs(title = "Average Prices for New and Old Buildings")
#new houses have a higher average price compared to the old houses

#comparing the prices of different house types and the city it's located in
table <- table(property_df_cleaned$city, property_df_cleaned$house_type)

#investigating the distribution of house types per city
(prop.table(table, 1)) * 100

#investigating the distribution of of house types
(prop.table(table, 2)) * 100

#the prices of different house types and city it's located in
property_df_cleaned %>% group_by(city, house_type) %>% summarise(mean_price
                                                                 =mean(price)) %>%
  ggplot(aes(house_type,city, fill = log(mean_price))) + geom_tile() 

#exporting scrapped data
write.csv(property_df, "scrapped_propertypro_lagos.csv")
#exporting the cleaned data
write.csv(property_df_cleaned, "propertypro_lagos.csv")

#creating dataframe for heatmap visualisation
heatmap <- property_df_cleaned %>% select(city, house_type, price)
heatmap$price <- heatmap$price %>% log()
#exporting data
write.csv(heatmap, "heatmap.csv")


