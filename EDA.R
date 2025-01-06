install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("naniar")
library(ggplot2)
library(dplyr)
library(tidyr)
library(naniar)

#setting up working directory
setwd("C:/Users/ASUS/Desktop/Projects/Education Inequality")

#reading data
data <- read.csv("mydata.csv")

#Viewing the data in a spreadsheet format
#View(data)

#Removing unnecessary columns (e.g. series code + country code)
data$Country.Code <- NULL
data$Series.Code <- NULL

#Statistical Summary of data
summary(data)
#data["Series"] <- as.factor(list(data["Series"]))
colnames(data) <- c("CountryName","Indicator",2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

data$Indicator <- factor(data$Indicator)
data$CountryName <- factor(data$CountryName)

cols_to_change <- which(names(data) %in% c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020))
data[cols_to_change] <- lapply(data[cols_to_change], as.double)

# Calculate the percentage of NA values in each column
na_percentage <- sapply(data, function(col) mean(is.na(col)) * 100)
na_percentage_named <- setNames(na_percentage, names(data))
na_percentage_named <- as.data.frame(na_percentage_named)
na_percentage_named$Years <- rownames(na_percentage_named)
na_percentage_named <- na_percentage_named[-c(1, 2), ]
colnames(na_percentage_named) <- c("Missing_Value_Percentage","year")


NA_BY_YEAR <- plot(na_percentage_named$year,na_percentage_named$Missing_Value_Percentage)




# Calculate the number of NAs for each factor level in CountryName
ByCountry_NA_Values <- data %>%
  group_by(CountryName) %>%
  summarise_all(~ sum(is.na(.)))

#-------------------
# Calculate the sum of all year values (2000 to 2020) for each country
ByCountry_NA_Values$YearSum <- rowSums(ByCountry_NA_Values[, 3:ncol(ByCountry_NA_Values)], na.rm = TRUE)
ByCountry_NA_Values <- ByCountry_NA_Values[, c("CountryName", "YearSum")]
# View the result
View(ByCountry_NA_Values)

summary(ByCountry_NA_Values) #Median of Missing values for each country is 219

#deleting countries that have more than 219 missing values
threshold <- median(ByCountry_NA_Values$YearSum)
# Filter CountryName where YearSum is less than the threshold
countries_below_threshold <- ByCountry_NA_Values$CountryName[ByCountry_NA_Values$YearSum < threshold]
selected_countries <- as.vector(countries_below_threshold)

# Filter rows where CountryName is in selected_countries
filtered_data <- data[data$CountryName %in% selected_countries, ]


#dropping Years that contain more than 30% missing values
filtered_data <- filtered_data[, !names(filtered_data) %in% c("2020", "2019","2000","2001","2002","2003","2004","2005")]
# Calculate the percentage of NA values in each column
filtered_na_percentage <- sapply(filtered_data, function(col) mean(is.na(col)) * 100)
filtered_na_percentage_named <- setNames(filtered_na_percentage, names(filtered_data))
filtered_na_percentage_named <- as.data.frame(filtered_na_percentage_named)
filtered_na_percentage_named$Years <- rownames(filtered_na_percentage_named)
filtered_na_percentage_named <- filtered_na_percentage_named[-c(1, 2), ]
colnames(filtered_na_percentage_named) <- c("Missing_Value_Percentage","year")


plot(filtered_na_percentage_named$year,filtered_na_percentage_named$Missing_Value_Percentage)


# Columns to use for calculating the row mean
selected_columns <- c("2006", "2007", "2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")

# Compute row-wise means using selected columns, excluding NA
row_means <- rowMeans(filtered_data[selected_columns], na.rm = TRUE)

# Replace NA values in the selected columns with the row means
data_imputed <- filtered_data # Create a copy to impute
for (i in seq_len(nrow(filtered_data))) {
  for (col in selected_columns) {
    if (is.na(filtered_data[i, col])) {
      data_imputed[i, col] <- row_means[i]
    }
  }
}

# View the imputed data
data_new <- as.data.frame(data_imputed)
View(data_new)

summary(data_new)
# Remove rows with any missing values
data_no_missing <- na.omit(data_new)

# Check the result
print("Data after removing rows with missing values:")
data_new <- as.data.frame(data_no_missing)
View(data_new)
summary(data_new)
write.csv(data_no_missing, file = "data.csv", row.names = FALSE)

#reading data
data <- read.csv("data.csv")

# Summary statistics for the entire dataset
summary(data)

# Histogram with a Normal distribution curve overlaid
hist(data[["X2006"]], prob = TRUE, main = "Distribution of 2006", 
     xlab = "2006 Values", col = "lightblue", border = "white")
curve(dnorm(x, mean = mean(data[["X2006"]]), sd = sd(data[["X2006"]])), 
      add = TRUE, col = "red", lwd = 2)

# As we see, all columns have a positive skew
# To treat this, we can transform them using Logarithm
# Loop through all columns except the first two
logdata <- data
for (i in 3:ncol(data)) {
  logdata[[i]] <- log(data[[i]] + 1)  # Apply log transformation (adding 1 to avoid log(0))
}

# Check the transformed data
summary(logdata)

# Histogram with a Normal distribution curve overlaid of the logdata
hist(logdata[["X2006"]], prob = TRUE, main = "Distribution of 2006", 
     xlab = "2006 Values", col = "lightblue", border = "white")
curve(dnorm(x, mean = mean(logdata[["X2006"]]), sd = sd(logdata[["X2006"]])), 
      add = TRUE, col = "red", lwd = 2)
#as we can see, the data became more symmetric
#now we continue with the analysis
# Q-Q plot for X2006 in the log-transformed data
qqnorm(logdata[["X2006"]], main = "Q-Q Plot for Log(X2006)")
qqline(logdata[["X2006"]], col = "red", lwd = 2)

# Correlation matrix for the log-transformed data
cor_matrix <- cor(logdata, use = "complete.obs")
print(cor_matrix)

