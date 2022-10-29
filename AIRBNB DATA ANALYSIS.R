 
# AirBnB NYC Data Price Prediction Using SVR

# Loading Library
library(tidyverse)
library(data.table)
library(DT)
library(ggthemes)
library(leaflet)
library(ggmap)
library(plotly)
library(lubridate)
library(e1071)
library(corrplot)

# Loading the Dataset
abnb = fread("C:\\Users\\kapad\\Desktop\\CDSP\\MODULE 4 R\\Airbnb_NYC.csv")
head(abnb) %>% 
  datatable(options = list(scrollX = TRUE))

# Showing the Summary Table
summary(abnb)
# ANALYSIS OF MISSING VALUES
sapply(abnb, function(x) sum(is.na(x)))
# Because we do not have any id to track these data from airbnb so we can drop these data
abnb = na.omit(abnb)

# ------------------ EDA -------------------------
# Neighbourhood Distribution
abnb[, .(freq = .N), by =  Boroughs] %>%
  .[, perc := round(freq/sum(freq) *100, 2)] %>%
  ggplot(aes(Boroughs, perc))+
  geom_bar(stat = "identity", width = 0.5, fill ="slategray2"  ) +
  geom_text(aes(Boroughs, perc, label = paste0(perc, "%")),
            position = position_dodge(width = 0.5),
            vjust = 0.07)
# Maximum Distribution is in Brooklyn & Manhattan Covering around 80% of it.

# Popular room types in neighbourhoods
abnb[, .(freq =.N), by = .(Boroughs, Prop_Type)] %>%
  .[, perc := round(freq/sum(freq) *100, 2), by = Boroughs] %>%
  ggplot(aes(Boroughs, perc, fill = Prop_Type))+
  geom_bar(stat = "identity", width = 0.5 ) +
  geom_text(aes(Boroughs, perc, label = paste0(perc, "%")),
            position = position_stack(vjust = 0.5),
            vjust = 0.07)+
  scale_fill_viridis_d(name = "") 
# Shared Rooms are less in all the neighbourhood compared to others

# Stats for Price Based on Location
abnb[, Price := as.double(Price)]
abnb[!is.na(Price)&Price !=0, 
           .(Mean = round(mean(Price), 2), 
             Median = median(Price),
             First_quartile = quantile(Price, .25),
             Third_quartile = quantile(Price, .75),
             Min = min(Price),
             Max = max(Price)),
           by = Boroughs]

# Stats for Price based on room_type
abnb[!is.na(Price)&Price !=0, 
           .(Mean = round(mean(Price), 2), 
             Median = median(Price),
             First_quartile = quantile(Price, .25),
             Third_quartile = quantile(Price, .75),
             Min = min(Price),
             Max = max(Price)),
           by = Prop_Type]


# Getting Map Using ggmap
newyork_m <- get_map(c(left = min(abnb$Longitude) - .0001,
                         bottom = min(abnb$Latitude) - .0001,
                         right = max(abnb$Longitude) + .0001,
                         top = max(abnb$Latitude) + .0001),
                       maptype = "watercolor", source = "osm")
# Mapping function
map_plot <- function(df, color_col, continues_color_col = TRUE) {
  if (continues_color_col) {
    scale_fill <- scale_color_viridis_c()
  } else{
    scale_fill <- scale_color_viridis_d()
  }
  ggmap(newyork_m) +
    geom_point(
      data = df,
      aes_string("Longitude", "Latitude",
                 color = color_col),
      size = 1
    ) +
    theme(legend.position = "bottom") +
    scale_fill
}

# Price Map
per95 <- abnb[, quantile(Price, 0.95,na.rm=T)]
map_plot(df = abnb[Price <=per95  ], 
         color_col = "Price") 

# Reviews Per Month
per95_rev <- abnb[,quantile(Reviews30d, 0.95)]
map_plot(df = abnb[Reviews30d < per95_rev ], 
         color_col = "Reviews30d")

# Calculating correlation with price
pricing_corr <- abnb %>%
  select(Price, Min_Nights, Review_Cnt, Reviews30d, Host_Listing_Cnt)

pricing_corrcalc <- cor(pricing_corr, use = "complete.obs")
pricing_corrcalc <- round(pricing_corrcalc, 2)
# Observing correlation with price
datatable(pricing_corrcalc, 
          colnames = c("Price", "Minimum nights", "No. of reviews", "Reviews per month", "Count listings owned by host"),
          rownames = c("Price", "Minimum nights", "No. of reviews", "Reviews per month", "Count listings owned by host"))
# Plotting the Correlation
corrplot(pricing_corrcalc, tl.col = "black")

# Creating SVR Model for Price Prediction
str(abnb)
# We have to convert the char columns into a Factor Datatype
abnb$Boroughs = as.factor(abnb$Boroughs)
abnb$Prop_Type = as.factor(abnb$Prop_Type)

# Checking the data again
str(abnb)

# Creating Training & Testing Data
set.seed(222)
ind=sample(2,nrow(abnb),replace = T,prob = c(0.8,0.2))
train=abnb[ind==1,]
test=abnb[ind==2,]

# SVR Model

#polynomial kernel
svm1=svm(Price~., data =train , type="nu-regression",kernel="polynomial", shrinking = T )
pred1=predict(svm1, newdata = test)
#MSE for polynomial SVM
mean((pred1 -test$Price)^2)

#linear kernel
svm2=svm(Price~., data =train , type="nu-regression",kernel="linear", shrinking = T )
pred2=predict(svm2, newdata = test)
#MSE for linear SVM
mean((pred2 -test$Price)^2)

summary(svm1)
summary(svm2)

# Done