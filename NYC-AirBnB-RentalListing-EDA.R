###Exploratory Data Analysis
### New York City AirBnB data

#Load libraries
library(readr)
library(ggplot2)
library(dplyr)
library(viridis)
library(reshape2)
library(RColorBrewer)
library(leaflet)
library('car')

#Load data
nyc_airbnb <- read.csv("AB_NYC_2019.csv")
nyc_airbnb_copy <- nyc_airbnb

#Preview of data
head(nyc_airbnb)
str(nyc_airbnb)

#replacing missing data
nyc_airbnb <- replace(nyc_airbnb, is.na(nyc_airbnb),0)

#dropping columns not relevant
nyc_airbnb_st <- nyc_airbnb[ ,-c(4,13)]

#Listings by neighbourhood group & room type
Listing1 <- as.data.frame(table(nyc_airbnb_st$neighbourhood_group, nyc_airbnb_st$room_type))
names(Listing1)[1] <- "Neighbourhood_group"
names(Listing1)[2] <- "room_type"
names(Listing1)[3] <- "Listings"
Listing1

#Preliminary Plots
##Barplot of listings by neighbourhood group, type
ListPlot <- ggplot(Listing1, aes(x = reorder(Neighbourhood_group, -Listings), y = Listings, fill = room_type)) + geom_bar(stat = "identity", position = "dodge") + labs (x = element_blank(), y = "Listings") + scale_fill_viridis_d() + theme_classic()
ListPlot

##Density plot of availability by neighborhood group
avail_plot <- ggplot(data = nyc_airbnb_st, aes(x = availability_365, fill = neighbourhood_group)) + geom_density(adjust=2, alpha=.4, show.legend = TRUE) + theme_classic()
avail_plot

##Violin Plot of price distribution by neighbourhood group
nyc_price1 <- subset(nyc_airbnb_st, price < 500)
nyc_price2 <- subset(nyc_airbnb_st, price > 500)
vplot <- ggplot(nyc_price1, aes(x= neighbourhood_group, y = price, fill = neighbourhood_group)) + geom_violin(alpha = 0.5) + labs (x = element_blank()) + geom_boxplot(width = 0.1, outlier.size = 0.25, fill = "white") + scale_fill_brewer(palette = "Dark2") + theme_classic() + theme(legend.position = "none")
vplot

##Scatter plots of price vs minimum number of nights
###Price vs min number of nights
price_scatter1 <- ggplot(nyc_airbnb_st, aes(minimum_nights, price, color = room_type)) + geom_point(alpha = 0.5, size = 2) + labs (x = "minimum nights", y = "price") + theme_classic()
price_scatter1

###price vs min nights for price < 500
price_scatter2 <- ggplot(nyc_price1, aes(minimum_nights, price, color = room_type)) + geom_point(alpha = 0.5, size = 2) + labs (x = "minimum nights", y = "price") + theme_classic()
price_scatter2

###price vs min nights for min nights < 90 days
nyc_short_term1 <- subset(nyc_airbnb_st, minimum_nights < 90)
price_scatter4 <- ggplot(nyc_short_term1, aes(minimum_nights, price, color = room_type)) + geom_point(alpha = 0.5, size = 2) + labs (x = "minimum nights", y = "price") + theme_classic()
price_scatter4

###price vs min nights for price <500 and min nights < 90 days
nyc_short_term2 <- subset(nyc_price1, minimum_nights < 30)
price_scatter5 <- ggplot(nyc_short_term2, aes(minimum_nights, price, color = room_type)) + geom_point(alpha = 0.7, size = 2) + labs (x = "minimum nights", y = "price") + theme_classic()
price_scatter5

##Map of price variation & reviews variation by location
nyc_private <- subset(nyc_airbnb_st, room_type == 'Private room')
nyc_shared <- subset(nyc_airbnb_st, room_type == 'Shared room')
nyc_entire <- subset(nyc_airbnb_st, room_type == 'Entire home/apt')

#Price variation
##Shared apt
bins_shared <- seq(0,1800, by=200)
palette_1 <- mypalette <- colorBin( palette="YlOrBr", domain=nyc_shared$price, na.color="transparent", bins = bins_shared)
leaflet(nyc_shared) %>% addTiles() %>% setView(-74.00, 40.71, zoom = 12) %>% addProviderTiles("CartoDB.Positron") %>% addCircleMarkers(~longitude, ~latitude, fillColor = ~palette_1(price), fillOpacity = 1, color = "black", radius = 6, stroke = FALSE) %>% addLegend( pal=palette_1, values=~price, opacity=0.9, title = "Shared Room - Price", position = "bottomright" )

##private apt
palette_2 <- mypalette <- colorBin( palette="YlOrBr", domain=nyc_private$price, na.color="transparent", bins = bins_shared)
leaflet(nyc_private) %>% addTiles() %>% setView(-74.00, 40.71, zoom = 12) %>% addProviderTiles("CartoDB.Positron") %>% addCircleMarkers(~longitude, ~latitude, fillColor = ~palette_2(price), fillOpacity = 1, color = "black", radius = 6, stroke = FALSE) %>% addLegend( pal=palette_2, values=~price, opacity=0.9, title = "Private Room - Price", position = "bottomright")

##entire apt
palette_3 <- mypalette <- colorBin( palette="YlOrBr", domain=nyc_entire$price, na.color="transparent", bins = bins_shared)
leaflet(nyc_entire) %>% addTiles() %>% setView(-74.00, 40.71, zoom = 12) %>% addProviderTiles("CartoDB.Positron") %>% addCircleMarkers(~longitude, ~latitude, fillColor = ~palette_3(price), fillOpacity = 1, color = "black", radius = 6, stroke = FALSE) %>% addLegend( pal=palette_3, values=~price, opacity=0.9, title = "Entire home/apt - Price", position = "bottomright" )

#Review variation
##shared apt
bins_shared <- seq(10,400, by=50)
palette_4 <- mypalette <- colorBin( palette="YlGnBu", domain=nyc_shared$number_of_reviews, na.color="transparent", bins = bins_shared)
leaflet(nyc_shared) %>% addTiles() %>% setView(-74.00, 40.71, zoom = 12) %>% addProviderTiles("CartoDB.Positron") %>% addCircleMarkers(~longitude, ~latitude, fillColor = ~palette_4(number_of_reviews), fillOpacity = 1, color = "black", radius = 6, stroke = FALSE) %>% addLegend( pal=palette_4, values=~price, opacity=0.9, title = "Shared Room - Total Reviews", position = "bottomright" )

##Private apt
palette_5 <- mypalette <- colorBin( palette="YlGnBu", domain=nyc_private$number_of_reviews, na.color="transparent", bins = bins_shared)
leaflet(nyc_private) %>% addTiles() %>% setView(-74.00, 40.71, zoom = 12) %>% addProviderTiles("CartoDB.Positron") %>% addCircleMarkers(~longitude, ~latitude, fillColor = ~palette_5(number_of_reviews), fillOpacity = 1, color = "black", radius = 6, stroke = FALSE) %>% addLegend( pal=palette_5, values=~price, opacity=0.9, title = "Private Room - Total Reviews", position = "bottomright")

##Entire apt
palette_6 <- mypalette <- colorBin( palette="YlGnBu", domain=nyc_entire$number_of_reviews, na.color="transparent", bins = bins_shared)
leaflet(nyc_entire) %>% addTiles() %>% setView(-74.00, 40.71, zoom = 12) %>% addProviderTiles("CartoDB.Positron") %>% addCircleMarkers(~longitude, ~latitude, fillColor = ~palette_6(number_of_reviews), fillOpacity = 1, color = "black", radius = 6, stroke = FALSE) %>% addLegend( pal=palette_6, values=~price, opacity=0.9, title = "Entire home/apt - Total Reviews", position = "bottomright")

#Create a subset of data & preprocess for regression model
newAirbnbData <- airbnbData[,c(5,9,10,11,12,15,16)]
ssAirbnbData <- subset(newAirbnbData, price > 0)
ssAirbnbData$neighbourhood_group <- factor(ssAirbnbData$neighbourhood_group)
ssAirbnbData$price <- log(ssAirbnbData$price)

#Build & test multiple linear regression models
model <- lm(price ~ neighbourhood_group + room_type + minimum_nights + number_of_reviews + calculated_host_listings_count + availability_365 , data = ssAirbnbData)
summary(model)
BIC(model)
AIC(model)
vif(model)

model1 <- lm(price ~ neighbourhood_group + room_type + minimum_nights + number_of_reviews + availability_365 , data = ssAirbnbData)
summary(model1)

model2 <- lm(price ~ neighbourhood_group + room_type + minimum_nights + number_of_reviews, data = ssAirbnbData)
summary(model2)

model3 <- lm(price ~ neighbourhood_group + room_type + minimum_nights + availability_365, data = ssAirbnbData)
summary(model3)

model4 <- lm(price ~ neighbourhood_group + room_type + number_of_reviews + availability_365, data = ssAirbnbData)
summary(model4)

model5 <- lm(price ~ neighbourhood_group + room_type + minimum_nights, data = ssAirbnbData)
summary(model5)

model6 <- lm(price ~ neighbourhood_group + room_type + availability_365, data = ssAirbnbData)
summary(model6)

##Use ANOVA to pick best model b/w model 1  & model 4
anova(model1, model4)

#Screen the data for further influential values
library('base')
cooksd <- cooks.distance(model1)
influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(ssAirbnbData)))])
airbnbNoOutliers <- ssAirbnbData[-influential, ]
str(airbnbNoOutliers)

#Re-run the model on the screened data
model1_new <- lm(price ~ neighbourhood_group + room_type + minimum_nights + number_of_reviews + availability_365 , data = airbnbNoOutliers)
summary(model1_new)
BIC(model1_new)
AIC(model1_new)
vif(model1_new)

predicted_values <- model1_new$fitted.values
airbnbNoOutliers$predicted_values <- predicted_values
head(airbnbNoOutliers)

qqplot(predicted_values, model1_new$residuals)
qqline(predicted_values, model1_new$residuals)

###########################################################################






