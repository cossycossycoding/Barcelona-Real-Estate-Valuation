if (!require(readxl)){
  install.packages("readxl")
}
if (!require(tidyverse)){
  install.packages("tidyverse")
}
library(readxl)
library(tidyverse)

# Price analysis for 413 properties
# Import raw data
BarcelonaRE.Data <- read_xlsx("C:\\Users\\ruans\\Desktop\\MQM\\Applied Probability and Statistics\\Class 12 Team Assignment\\BarcelonaRE_Data.xlsx", sheet = 2, range = "A1:M414", col_names = TRUE)

# Construct regression model with all variables
names(BarcelonaRE.Data) <- c("1","Price","City Zone","m2","Rooms","Bathrooms","Elevator","Atico","Terrasse","Parking","Kitchen","Type","Yard")
names(BarcelonaRE.Data)
model1 <- lm(
  formula = Price ~ m2 + Rooms + Bathrooms + Elevator + Atico + Terrasse + Parking + Kitchen + Type + Yard, 
  data = BarcelonaRE.Data
)
summary(model1)

# Remove independent variable Kitchen and Type
model2 <- lm(Price ~ m2 + Rooms + Bathrooms + Elevator + Atico + 
               Terrasse + Parking + Yard, 
             data = BarcelonaRE.Data)
summary(model2)

# Check and calculate Multi-Collinearity
install.packages("car")
library(car)

vif(model2)

# Create interaction term m2 * Rooms
model3 <- lm(Price ~ m2 * Rooms + Bathrooms + Elevator + Atico + 
               Terrasse + Parking + Yard, 
             data = BarcelonaRE.Data)
summary(model3)

# Take logarithm for Price and Space
model4 <- lm(log(Price) ~ log(m2) + Rooms + I(Rooms^2) + Bathrooms + 
               Elevator + Atico + Parking + Yard,
             data = BarcelonaRE.Data)
summary(model4)

# Merge Yard and Terrasse into the 'Outdoor Space' variable
BarcelonaRE.Data$Outdoor <- ifelse(BarcelonaRE.Data$Yard | BarcelonaRE.Data$Terrasse, 1, 0)

model5 <- lm(log(Price) ~ log(m2) + Rooms + I(Rooms^2) + Bathrooms + 
                    Elevator + Atico + Parking + Outdoor,
                  data = BarcelonaRE.Data)
summary(model5)

# Remove Atico variable
model6 <- lm(log(Price) ~ log(m2) + Rooms + I(Rooms^2) + Bathrooms + 
                       Elevator + Parking + Outdoor, 
                     data = BarcelonaRE.Data)
summary(model6)

# Regional fixed effect model
model7 <- lm(log(Price) ~ log(m2) + Rooms + I(Rooms^2) + Bathrooms + 
                     Elevator + Parking + Outdoor + factor(`City Zone`),
                   data = BarcelonaRE.Data)
summary(model7)

# Remove insignificant variables
final_simple <- lm(log(Price) ~ log(m2) + Bathrooms + Elevator + 
                     Parking + Outdoor + factor(`City Zone`),
                   data = BarcelonaRE.Data)
summary(final_simple)


# Price prediction for 200 properties
# Import raw data
price.prediction <- read_xlsx("C:\\Users\\ruans\\Desktop\\MQM\\Applied Probability and Statistics\\Class 12 Team Assignment\\PriceSubmission.xlsx", sheet = 1, range = "A1:M201", col_names = TRUE)

# Rename columns
names(price.prediction) <- c("ID", "Price", "City Zone", "m2", "Rooms", "Bathrooms", 
                     "Elevator", "Atico", "Terrasse", "Parking", "Kitchen", "Type", "Yard")

# Create Outdoor variable 
price.prediction$Outdoor <- ifelse(price.prediction$Yard | price.prediction$Terrasse, 1, 0)

# Attain model sigma value
model_sigma <- summary(final_simple)$sigma

# Conduct price prediction using final model
log_predictions <- predict(final_simple, newdata = price.prediction)
price.prediction$Predicted_Price <- exp(log_predictions + 0.5 * model_sigma^2) %>% round(0)

# Preserve prediction results
write.csv(price.prediction, "barcelona_price_predictions.csv", row.names = FALSE)


