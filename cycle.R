# Install and load the packages and libraries
#=====================================================

install.packages("tidyverse")
install.packages("dplyr")
install.packages("outliers")
install.packages("readxl")
install.packages("ggplot2")
install.packages("readr")
install.packages("gridExtra")
install.packages("reshape2")
install.packages("circlize")
install.packages("plotly")
install.packages("fmsb")
install.packages("rgl")
install.packages("GGally")

library(tidyverse)
library(dplyr)
library(outliers)
library(readxl)
library(ggplot2)
library(readr)
library(gridExtra)
library(reshape2)
library(circlize)
library(plotly)
library(rgl)
library(fmsb)
library(GGally)

#-------------------------------------------------------------------------------

# Specify the file path
file_path <- "D:/DC_mini/Sprocket Central Pty Ltd Data.xlsx"

# Read the Excel file with all sheets into a list of data frames
all_data <- excel_sheets(file_path) %>%
  set_names() %>%
  map(~ read_excel(file_path, sheet = .x))

# Assign each data frame to a variable with its respective name
transactions <- all_data$Transactions
new_customer_list <- all_data$NewCustomerList
customer_demographic <- all_data$CustomerDemographic
customer_address <- all_data$CustomerAddress


class(transactions)
class(new_customer_list)
class(customer_demographic)
class(customer_address)

#-------------------------------------------------------------------------------

# Check the Dimension, Column names, data type and Structure of the data set
#============================================================================

glimpse(transactions)
glimpse(new_customer_list)
glimpse(customer_demographic)
glimpse(customer_address)

#-------------------------------------------------------------------------------

# Removing unnecessary columns
#=================================

# Remove specific columns from the 'transactions' data frame
transactions <- transactions %>%
  select(-product_first_sold_date)  

# Remove specific columns from the 'new_customer_list' data frame

new_customer_list <- new_customer_list[, -c(17, 18, 19, 20, 21)]

# Remove specific columns from the 'customer_demographic' data frame
customer_demographic <- customer_demographic %>%
  select(-default)

#-------------------------------------------------------------------------------

# Change the name of a specific column
#=========================================

# Change the name of past_3_years_bike_related_purchases of new_customer_list
names(new_customer_list)[names(new_customer_list) == 
    "past_3_years_bike_related_purchases"] <- "bike_related_purchases"

# Change the name of past_3_years_bike_related_purchases of customer_demographic
names(customer_demographic)[names(customer_demographic) == 
    "past_3_years_bike_related_purchases"] <- "bike_related_purchases"

# Change the name of a specific column
names(customer_address)[names(customer_address) == "postcode"] <- "postal_code"


#-------------------------------------------------------------------------------

#'new_customer_list' and 'customer_demographic' are the data frame and 'DOB' 
# is the column containing numeric date values and automatically assign 
# appropriate data types to columns
#============================================================================

new_customer_list$DOB <- as.POSIXct(new_customer_list$DOB, format = "%Y-%m-%d")

wrong_dob<- customer_demographic$DOB
wrong_dob

customer_demographic$DOB <- as.Date("1900-01-01") + 
  as.numeric(customer_demographic$DOB) - 1
customer_demographic$DOB

transactions <- type_convert(transactions)
new_customer_list <- type_convert(new_customer_list)
customer_demographic <- type_convert(customer_demographic)
customer_address <- type_convert(customer_address)

#-------------------------------------------------------------------------------

# Check the Dimension, Column names, data type and Structure of the data set
#==============================================================================

glimpse(transactions)
glimpse(new_customer_list)
glimpse(customer_demographic)
glimpse(customer_address)

#-------------------------------------------------------------------------------

# Check for duplicate rows in the data frame
#==============================================

has_duplicates <- any(duplicated(transactions))
has_duplicates

has_duplicates <- any(duplicated(new_customer_list))
has_duplicates

has_duplicates <- any(duplicated(customer_demographic))
has_duplicates

has_duplicates <- any(duplicated(customer_address))
has_duplicates

#-------------------------------------------------------------------------------

# Define a function to check consistency in categorical columns
#================================================================

check_consistency <- function(data, ignore_columns = NULL) {
  cat("Inconsistent columns:\n")
  
  # Select only character and factor columns
  char_columns <- data %>%
    select_if(is.character) %>%
    names()
  
  # Exclude columns specified in 'ignore_columns'
  if (!is.null(ignore_columns)) {
    char_columns <- setdiff(char_columns, ignore_columns)
  }
  
  for (col in char_columns) {
    cat_count <- data %>%
      group_by({{ col }}) %>%
      summarise(n = n()) %>%
      ungroup()
    
    if (nrow(cat_count) <= 1) {
      cat("  - ", col, "\n")
    }
  }
  
  cat("Other columns are consistent.\n")
}

# check for each data frame
cat("\n For 'transactions' data frame: \n")
check_consistency(transactions, ignore_columns = c("transaction_date"))

cat("\n For 'new_customer_list' data frame: \n")
check_consistency(new_customer_list, ignore_columns = c("DOB"))

cat("\n For 'customer_demographic' data frame: \n")
check_consistency(customer_demographic, ignore_columns = c("DOB"))

cat("\n For 'customer_address' data frame: \n")
check_consistency(customer_address)


#-------------------------------------------------------------------------------

# Null values count
#====================

# Count null values for all columns in the 'transactions' data frame
null_counts <- colSums(is.na(transactions))
print(null_counts)

# Count null values for all columns in the 'new_customer_list' data frame
null_counts <- colSums(is.na(new_customer_list))
print(null_counts)

# Count null values for all columns in the 'customer_demographic' data frame
null_counts <- colSums(is.na(customer_demographic))
print(null_counts)

# Count null values for all columns in the 'customer_address' data frame
null_counts <- colSums(is.na(customer_address))
print(null_counts)


#-------------------------------------------------------------------------------

# Handle missing values in each data frame
#===========================================

# Remove rows with any NA values from the data frame 'transactions'
transactions <- na.omit(transactions)

# Remove rows with any NA values from the data frame 'new_customer_list'
new_customer_list <- na.omit(new_customer_list)

# Remove rows with any NA values from the data frame 'customer_demographic'
customer_demographic <- na.omit(customer_demographic)

# Remove rows with any NA values from the data frame 'customer_address'
customer_address <- na.omit(customer_address)


#-------------------------------------------------------------------------------

# Null values count
#=====================

# Count null values for all columns in the 'transactions' data frame
null_counts <- colSums(is.na(transactions))
print(null_counts)
# Count null values for all columns in the 'new_customer_list' data frame
null_counts <- colSums(is.na(new_customer_list))
print(null_counts)
# Count null values for all columns in the 'customer_demographic' data frame
null_counts <- colSums(is.na(customer_demographic))
print(null_counts)
# Count null values for all columns in the 'customer_address' data frame
null_counts <- colSums(is.na(customer_address))
print(null_counts)


#-------------------------------------------------------------------------------

# Check for Unique values 
#==========================

# 'transactions' is the data frame
for (col in names(transactions)) {
  unique_values <- unique(transactions[[col]])
  num_unique <- length(unique_values)
  cat("Column:", col, "has", num_unique, "unique values.\n")
}

# Define a vector of column names to exclude from displaying unique values
columns_to_exclude <- c("transaction_id", "product_id", "customer_id",
                        "transaction_date", "list_price", "standard_cost")  


for (col in names(transactions)) {
  # Check if the column is not in the 'columns_to_exclude' list
  if (!(col %in% columns_to_exclude)) {
    unique_values <- unique(transactions[[col]])
    cat("Unique values in column '", col, "':\n")
    print(unique_values)
    cat("\n")
  }
}

#------------

# 'new_customer_list' is the data frame
for (col in names(new_customer_list)) {
  unique_values <- unique(new_customer_list[[col]])
  num_unique <- length(unique_values)
  cat("Column:", col, "has", num_unique, "unique values.\n")
}


# Define a vector of column names to include in displaying unique values
columns_to_include <- c("gender", "wealth_segment", "job_title",
                        "job_industry_category", "deceased_indicator",
                        "owns_car", "state", "country", "property_valuation")  

for (col in columns_to_include) {
  if (col %in% names(new_customer_list)) {
    unique_values <- unique(new_customer_list[[col]])
    cat("Unique values in column '", col, "':\n")
    print(unique_values)
    cat("\n")
  }
}

# Replacing Undefined property_valuation
new_customer_list <- new_customer_list %>%
  mutate(property_valuation = ifelse(property_valuation %in% c("11","12"), "10",
                                     property_valuation ))

# Remove blank job_industry_category values
new_customer_list <- new_customer_list[new_customer_list$job_industry_category
                                       != "n/a", ]

# Rechecking unique values
for (col in columns_to_include) {
  if (col %in% names(new_customer_list)) {
    unique_values <- unique(new_customer_list[[col]])
    cat("Unique values in column '", col, "':\n")
    print(unique_values)
    cat("\n")
  }
}


#--------------

# 'customer_demographic' is the data frame
for (col in names(customer_demographic)) {
  unique_values <- unique(customer_demographic[[col]])
  num_unique <- length(unique_values)
  cat("Column:", col, "has", num_unique, "unique values.\n")
}

# Define a vector of column names to include in displaying unique values
columns_to_include <- c("gender", "job_title", "job_industry_category",
                        "wealth_segment", "deceased_indicator", "owns_car",
                        "tenure")  

for (col in columns_to_include) {
  if (col %in% names(customer_demographic)) {
    unique_values <- unique(customer_demographic[[col]])
    cat("Unique values in column '", col, "':\n")
    print(unique_values)
    cat("\n")
  }
}


# Replacing Misspellings in gender of 'customer_demographic' data frame
customer_demographic <- customer_demographic %>%
  mutate(gender = ifelse(gender %in% c("Femal", "F"), "Female", gender))

customer_demographic <- customer_demographic %>%
  mutate(gender = ifelse(gender %in% c("M"), "Male", gender))

# # Remove Undefined gender
# customer_demographic <- customer_demographic[customer_demographic$gender
#                                              != "U", ]

# Remove blank job_industry_category values
customer_demographic <- customer_demographic[customer_demographic$
                                               job_industry_category != "n/a", ]

# Remove Undefined deceased_indicator
customer_demographic <- customer_demographic[customer_demographic
                                             $deceased_indicator != "Y", ]

# Define a vector of column names to include in displaying unique values
columns_to_include <- c("gender", "job_industry_category", "deceased_indicator")  

# Rechecking unique values
for (col in columns_to_include) {
  if (col %in% names(customer_demographic)) {
    unique_values <- unique(customer_demographic[[col]])
    cat("Unique values in column '", col, "':\n")
    print(unique_values)
    cat("\n")
  }
}

#----------------------

# 'customer_address' is the data frame
for (col in names(customer_address)) {
  unique_values <- unique(customer_address[[col]])
  num_unique <- length(unique_values)
  cat("Column:", col, "has", num_unique, "unique values.\n")
}

# Define a vector of column names to include in displaying unique values
columns_to_include <- c("state", "country", "property_valuation")  

for (col in columns_to_include) {
  if (col %in% names(customer_address)) {
    unique_values <- unique(customer_address[[col]])
    cat("Unique values in column '", col, "':\n")
    print(unique_values)
    cat("\n")
  }
}

# Replacing Full forms of State in 'customer_demographic' data frame
customer_address <- customer_address %>%
  mutate(state = ifelse(state %in% c("New South Wales"), "NSW", state))

customer_address <- customer_address %>%
  mutate(state = ifelse(state %in% c("Victoria"), "VIC", state))

# Replacing Undefined property_valuation
customer_address <- customer_address %>%
  mutate(property_valuation = ifelse(property_valuation %in% c("11","12"), "10",
                                     property_valuation ))

# Rechecking unique values
for (col in c("state","property_valuation")) {
  if (col %in% names(customer_address)) {
    unique_values <- unique(customer_address[[col]])
    cat("Unique values in column '", col, "':\n")
    print(unique_values)
    cat("\n")
  }
}


#-------------------------------------------------------------------------------

# Recheck the Column names of the data set and Reassigning data types
#====================================================================

print(colnames(transactions))
print(colnames(new_customer_list))
print(colnames(customer_demographic))
print(colnames(customer_address))

transactions <- type_convert(transactions)
new_customer_list <- type_convert(new_customer_list)
customer_demographic <- type_convert(customer_demographic)
customer_address <- type_convert(customer_address)

#-------------------------------------------------------------------------------

# Recheck the Dimension, Column names, data type and Structure of the data set
#============================================================================

glimpse(transactions)
glimpse(new_customer_list)
glimpse(customer_demographic)
glimpse(customer_address)


#-------------------------------------------------------------------------------

# Define a function to handle outliers using the IQR method
#=============================================================

#######   TRANSACTIONS DATA FRAME   ###########

# 'transactions' is the data frame and 'transaction_date' is  date values
transactions$transaction_date <- as.Date(transactions$transaction_date)

# Calculate z-scores for the date column
z_scores <- (transactions$transaction_date - mean(transactions$
            transaction_date)) / sd(transactions$transaction_date)

# Define a threshold for considering values as outliers (e.g., z-score > 3)
outliers <- transactions[z_scores > 3, ]

# Print the outliers
print(outliers)


# Define a function to detect outliers using the IQR method for multiple columns
detect_outliers <- function(data, column_names) {
  # Initialize an empty data frame to store outliers
  all_outliers <- data.frame()
  
  for (col in column_names) {
    # Check if the specified column contains missing values
    if (any(is.na(data[[col]]))) {
      cat("Column", col, "contains missing values.Outlier detection skipped.\n")
    } else {
      # Calculate the first quartile (Q1) and third quartile (Q3)
      q1 <- quantile(data[[col]], 0.25)
      q3 <- quantile(data[[col]], 0.75)
      
      # Calculate the IQR (Interquartile Range)
      iqr <- q3 - q1
      
      # Calculate the lower and upper bounds for outliers
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      
      # Identify outliers using the bounds for the current column
      outliers <- data %>%
        filter(data[[col]] < lower_bound | data[[col]] > upper_bound)
      
      # Add the outliers for the current column to the result
      all_outliers <- bind_rows(all_outliers, outliers)
    }
  }
  
  return(all_outliers)
}

# select the columns which we want to detect outlier
outliers_data <- detect_outliers(transactions, c('list_price', 'standard_cost'))

# Print the outliers
if (nrow(outliers_data) > 0) {
  print(outliers_data)
}

#-----------------------

#######   NEW CUSTOMER LIST DATA FRAME   ###########

# 'new_customer_list' is your data frame and 'BOD' is date values
new_customer_list$DOB <- as.Date(new_customer_list$DOB)

# Calculate z-scores for the date column
z_scores <- (new_customer_list$DOB - mean(new_customer_list$DOB)) /
  sd(new_customer_list$DOB)

# Define a threshold for considering values as outliers (e.g., z-score > 3)
outliers <- new_customer_list[z_scores > 3, ]

# Print the outliers
print(outliers)

# select the columns which we want to detect outlier
outliers_data <- detect_outliers(new_customer_list, c('bike_related_purchases',
                  'tenure', 'postcode', 'property_valuation', 'Rank', 'Value'))



# Print the outliers
if (nrow(outliers_data) > 0) {
  print(outliers_data)
}

#---------------------

#######   CUSTOMER DEMOGRAPHIC DATA FRAME   ###########

# 'customer_demographic' is your data frame and 'DOB' is date values
customer_demographic$DOB <- as.Date(customer_demographic$DOB)

# Calculate z-scores for the date column
z_scores <- (customer_demographic$DOB - mean(customer_demographic$DOB)) / 
  sd(customer_demographic$DOB)

# Define a threshold for considering values as outliers (e.g., z-score > 3)
outliers <- customer_demographic[z_scores > 3, ]

# Print the outliers
print(outliers)

# select the columns which we want to detect outlier
outliers_data <- detect_outliers(customer_demographic, 
                                 c('bike_related_purchases', 'tenure'))

# Print the outliers
if (nrow(outliers_data) > 0) {
  print(outliers_data)
}

#------------------------------

#######   CUSTOMER ADDRESS DATA FRAME   ###########

outliers_data <- detect_outliers(customer_address, c('postal_code',
                                                     'property_valuation'))

# Print the outliers
if (nrow(outliers_data) > 0) {
  print(outliers_data)
}


#===============================================================================
#===============================================================================


# Create a box plot for "Distribution of standard_cost"
#========================================================

ggplot(transactions, aes(y = standard_cost)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of standard_cost",
    x = NULL,  
    y = "standard_cost",  
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels

#---------------

# Removing outliers and reploting the box plot
#===============================================

# Calculate the quartiles and IQR
q1 <- quantile(transactions$standard_cost, 0.25)
q3 <- quantile(transactions$standard_cost, 0.75)
iqr <- q3 - q1

# Calculate the lower and upper bounds for outliers
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Identify outliers
outliers <- transactions$standard_cost < lower_bound | transactions$
  standard_cost > upper_bound

# Create a new data frame without outliers
transactions <- transactions[!outliers, ]

# Create a boxplot without outliers
ggplot(transactions, aes(y = standard_cost)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Distribution of standard_cost (Outliers Removed)",
    x = NULL,
    y = "standard_cost",
    caption = "Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::comma)

#-----------------

# Create a box plot for "Distribution of List_price"
#=======================================================

ggplot(transactions, aes(y = list_price)) +
  geom_boxplot(fill = "orange", color = "red", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of list_price",
    x = NULL,  
    y = "list_price",  
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels

#--------------------

# Create a Line plot for "Transaction Count Over Time"
#-=======================================================

# Convert 'transaction_date' to Date format
transactions$transaction_date <- as.Date(transactions$transaction_date)

# Extract the year from 'transaction_date'
transactions$month_year <- format(transactions$transaction_date, "%Y-%m")

# Create a summary data frame with transaction counts per year
transaction_counts <- transactions %>%
  group_by(month_year) %>%
  summarize(transaction_count = n())


# Create a line chart with data labels, enhanced styling, and a smoothed line
ggplot(transaction_counts, aes(x = month_year, y = transaction_count, 
                               group = 1)) +
  geom_line(color = "steelblue", size = 1.5) +
  geom_point(color = "yellow", size = 4) +
  geom_text(aes(label = transaction_count), vjust = -0.5, hjust = 0.5, 
            size = 5, color = "red") +  # Add a smoothed line (loess method)
  geom_smooth(method = "loess", color = "lawngreen", se = FALSE) + 
  labs(
    title = "Transaction Count Over Time",
    x = "Month-Year",
    y = "Transaction Count",
    caption = "Source: Gunjan"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f0f0f0"),
    plot.background = element_rect(fill = "#ffffff")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.caption = element_text(size = 10, color = "#999999"))

#---------------------------#-------------------------#-------------------------
#------------#------------------------#-----------------------#-----------------


# Create a boxplot for "bike_related_purchases"
#================================================

ggplot(new_customer_list, aes(y = bike_related_purchases)) +
  geom_boxplot(fill = "yellow", color = "salmon", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of bike_related_purchases",
    x = NULL, 
    y = "bike_related_purchases",  
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels

#----------------

# Create a box plot for "tenure"
#================================

ggplot(new_customer_list, aes(y = tenure)) +
  geom_boxplot(fill = "lawngreen", color = "darkblue", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of tenure",
    x = NULL,  
    y = "tenure",  
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels

#--------------------

# Create a box plot for "Distribution of postcode"
#==================================================

ggplot(new_customer_list, aes(y = postcode)) +
  geom_boxplot(fill = "lawngreen", color = "maroon", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of postcode",
    x = NULL,  
    y = "postcode",  
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels


#----------

# Create a box plot for "property_valuation"
#=============================================

ggplot(new_customer_list, aes(y = property_valuation)) +
  geom_boxplot(fill = "cyan", color = "tomato", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of property_valuation",
    x = NULL,  
    y = "property_valuation",  
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels


#---------------

# Removing outliers and reploting the box plot
#===============================================

# Calculate the quartiles and IQR
q1 <- quantile(new_customer_list$property_valuation, 0.25)
q3 <- quantile(new_customer_list$property_valuation, 0.75)
iqr <- q3 - q1

# Calculate the lower and upper bounds for outliers
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Identify outliers
outliers <- new_customer_list$property_valuation < 
  lower_bound | new_customer_list$property_valuation > upper_bound

# replace data frame without outliers
new_customer_list <- new_customer_list[!outliers, ]

# Create a boxplot without outliers
ggplot(new_customer_list, aes(y = property_valuation)) +
  geom_boxplot(fill = "cyan", color = "tomato", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Distribution of property_valuation (Outliers Removed)",
    x = NULL,
    y = "property_valuation",
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::comma)


#---------------

# Create a box plot for "Rank"
#===============================

ggplot(new_customer_list, aes(y = Rank)) +
  geom_boxplot(fill = "olivedrab1", color = "orangered", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of Rank",
    x = NULL,  
    y = "Rank",  
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels

#--------------------

# Create a box plot for "Value"
#===============================

ggplot(new_customer_list, aes(y = Value)) +
  geom_boxplot(fill = "yellow", color = "darkblue", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of Value",
    x = NULL,  
    y = "Value",  
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels

#---------------

# Removing outliers and reploting the box plot
#===============================================

# Calculate the quartiles and IQR
q1 <- quantile(new_customer_list$Value, 0.25)
q3 <- quantile(new_customer_list$Value, 0.75)
iqr <- q3 - q1

# Calculate the lower and upper bounds for outliers
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Identify outliers
outliers <- new_customer_list$Value < lower_bound | new_customer_list$
  Value > upper_bound

# replace data frame without outliers
new_customer_list <- new_customer_list[!outliers, ]

# Create a boxplot without outliers
ggplot(new_customer_list, aes(y = Value)) +
  geom_boxplot(fill = "yellow", color = "darkblue", size = 1.2) +
  theme_minimal() +
  labs(
    title = "Distribution of Value (Outliers Removed)",
    x = NULL,
    y = "Value",
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::comma)


#-----------------------

# Create a Scatter plot for "Customer Count by Year"
#===================================================

# Convert 'DOB' to Date format
new_customer_list$DOB <- as.Date(new_customer_list$DOB)

# Extract the year from 'DOB'
new_customer_list$year <- format(new_customer_list$DOB, "%Y")

# Create a summary data frame with counts of customers per year
customer_counts <- new_customer_list %>%
  group_by(year) %>%
  summarize(customer_count = n())

# Create an attractive scatter plot with data labels
ggplot(customer_counts, aes(x = year, y = customer_count)) +
  geom_point(color = "greenyellow", size = 4, shape = 16, fill = "gold") + 
  geom_text(aes(label = customer_count), vjust = -0.5, hjust = 0.5, size = 5,
            color = "darkred") +  # Enhanced data labels
  labs(
    title = "Customer Count by Year",
    x = "Year",
    y = "Customer Count",
    caption = "Source: Gunjan"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f0f0f0"),
    plot.background = element_rect(fill = "#ffffff")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.caption = element_text(size = 10, color = "#999999"))


#---------------------------#-------------------------#-------------------------
#------------#------------------------#-----------------------#-----------------


# Create a Histogram for "Customer Count by 10-Year Groups"
#===========================================================

# Create bins with 10-year intervals
bins <- seq(min(as.numeric(new_customer_list$year)), max(as.numeric(
  new_customer_list$year)) + 10, by = 10)

# Calculate counts of customers in each 10-year group
customer_counts <- cut(as.numeric(new_customer_list$year), breaks = bins, 
                       right = FALSE)
count_table <- table(customer_counts)

# Create a more attractive bar chart
ggplot(data = as.data.frame(count_table), aes(x = customer_counts, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue4", color = "cyan") + 
  labs(
    title = "Customer Count by 10-Year Groups",
    x = "Year Group",
    y = "Customer Count",
    caption = "Source: Gunjan"
  ) +
  scale_x_discrete(labels = bins, breaks = seq_along(bins) - 0.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f0f0f0"),
    plot.background = element_rect(fill = "#ffffff")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.caption = element_text(size = 10, color = "#999999"))


#---------------------------#-------------------------#-------------------------
#------------#------------------------#-----------------------#-----------------


# Create a box plot for "bike_related_purchases"
#===================================================

ggplot(customer_demographic, aes(y = bike_related_purchases)) +
  geom_boxplot(fill = "yellow", color = "deeppink", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of bike_related_purchases",
    x = NULL, 
    y = "bike_related_purchases",  
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels

#-------------------------------

# Create a box plot for "Tenure"
#================================

ggplot(customer_demographic, aes(y = tenure)) +
  geom_boxplot(fill = "orchid", color = "darkblue", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of tenure",
    x = NULL, 
    y = "tenure", 
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels

#----------------

# Create a Histogram for "Customer Count by Age"
#==================================================

# Convert 'DOB' to Date format
customer_demographic$DOB <- as.Date(customer_demographic$DOB)

# Calculate the age from 'DOB'
today <- Sys.Date()
customer_demographic$age <- as.integer(difftime(today, customer_demographic$DOB,
                          units = "weeks") / 52.143)  # Calculate age in years

# Create a summary data frame with counts of customers per age
age_counts <- customer_demographic %>%
  group_by(age) %>%
  summarize(customer_count = n())

# Create a scatter plot with data labels
ggplot(age_counts, aes(x = age, y = customer_count)) +
  geom_point(color = "olivedrab1", size = 4, shape = 16, fill = "maroon") +  
  geom_text(aes(label = customer_count), vjust = -0.5, hjust = 0.5, size = 5,
            color = "midnightblue") +  # Enhanced data labels
  labs(
    title = "Customer Count by Age",
    x = "Age",
    y = "Customer Count",
    caption = "Source: Gunjan"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f0f0f0"),
    plot.background = element_rect(fill = "#ffffff")
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.caption = element_text(size = 10, color = "#999999"))


#---------------------------#-------------------------#-------------------------
#------------#------------------------#-----------------------#-----------------

# Create a box plot for "Distribution of postal_code"
#--------------------------------------------------------------------------

ggplot(customer_address, aes(y = postal_code)) +
  geom_boxplot(fill = "deepskyblue", color = "tan1", size = 1.2) +
  theme_minimal() +  # Use a minimalistic theme
  labs(
    title = "Distribution of postal_code",
    x = NULL,  
    y = "postal_code", 
    caption = "Source: Gunjan"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_blank(),  # Hide x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    legend.position = "none"  # Remove legend if not needed
  ) +
  scale_y_continuous(labels = scales::comma)  # Format y-axis labels


#===============================================================================
################################################################################
#-------------------------------------------------------------------------------
                                                                                      
#  EEEEEEEEEEEEEEEEEEEE     DDDDDDDDDDDD                     AAA               
#  E::::::::::::::::::E     D:::::::::::DDD                 A:::A              
#  E::::::::::::::::::E     D::::::::::::::DD              A:::::A             
#  EE:::::EEEEEEEE::::E     DDD::::DDDDD:::::D            A:::::::A            
#    E::::E      EEEEEE      D::::D    D:::::D           A:::::::::A           
#    E::::E                  D::::D     D:::::D         A:::::A:::::A          
#    E:::::EEEEEEEEE         D::::D     D:::::D        A:::::A A:::::A         
#    E:::::::::::::E         D::::D     D:::::D       A:::::A   A:::::A        
#    E:::::::::::::E         D::::D     D:::::D      A:::::A     A:::::A       
#    E:::::EEEEEEEEE         D::::D     D:::::D     A:::::AAAAAAAAA:::::A      
#    E::::E                  D::::D     D:::::D    A:::::::::::::::::::::A     
#    E::::E      EEEEEE      D::::D    D:::::D    A:::::AAAAAAAAAAAAA:::::A    
#  EE:::::EEEEEEE:::::E     DDD::::DDDDD:::::D   A:::::A             A:::::A   
#  E::::::::::::::::::E     D::::::::::::::DD   A:::::A               A:::::A  
#  E::::::::::::::::::E     D:::::::::::DDD    A:::::A                 A:::::A 
#  EEEEEEEEEEEEEEEEEEEE     DDDDDDDDDDDD      AAAAAAA                   AAAAAAA
                                                                                      
#-------------------------------------------------------------------------------
################################################################################                                                                                      
#===============================================================================


# Calculate the count and percentage of each order status
#==========================================================

order_status_summary <- transactions %>%
  group_by(order_status) %>%
  summarize(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Create a pie chart
ggplot(order_status_summary, aes(x = "", y = count, fill = order_status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste(order_status, "(", round(percentage, 1), "%)")),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Pie Chart of Order Status",
    x = NULL,
    y = NULL,
    fill = "Order Status"
  ) +
  theme_void() +
  theme(legend.position = "right")

#-------------------------------------------------------------------------------

# Pie charts for 'brand' and 'product_line' Combined
#=====================================================

brand_pie <- ggplot(data = transactions, aes(x = 1, fill = brand)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +  # Convert to pie chart
  labs(
    title = "Pie Chart of brand",
    fill = "Brand"
  ) +
  theme_void()  # Remove axis and labels


product_line_pie <- ggplot(data = transactions, aes(x = 1, fill = product_line)
                           )+geom_bar(width = 1) +coord_polar(theta = "y") +  
  # Convert to pie chart
  labs(
    title = "Pie Chart of product_line",
    fill = "Product Line"
  ) +
  theme_void()  # Remove axis and labels


# Display the pie charts side by side
grid.arrange(brand_pie, product_line_pie, ncol = 2)

# Calculate percentages within each facet
abcd <- transactions %>%
  group_by(brand, product_line) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Create pie chart for 'brand' with 'product_line' facets
brand_product_pie <- ggplot(data = abcd, aes(x = 1, fill = product_line, y =
                                               percentage)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Convert to pie chart
  labs(
    title = "Pie Chart of brand with product_line",
    fill = "Product Line",
    y = NULL
  ) +
  theme_void() +  # Remove axis and labels
  facet_wrap(~ brand) +  # Facet by 'brand'
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5),
            show.legend = FALSE)  # Hide the legend

# Display the pie charts with facets
print(brand_product_pie)


#-------------------------------------------------------------------------------

# Scatter plot for 'list_price' vs. 'standard_cost'
#=====================================================

ggplot(data = transactions, aes(x = list_price, y = standard_cost)) +
  geom_point(color = "chartreuse") +
  labs(
    title = "Scatter Plot of List Price vs. Standard Cost",
    x = "List Price",
    y = "Standard Cost"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------

# Calculate summary statistics for numeric columns
#=====================================================

summary(new_customer_list[c("bike_related_purchases", "tenure", 
                            "property_valuation", "Rank", "Value")])

#-------------------------------------------------------------------------------

# Calculate the count and percentage of each gender
#======================================================

order_status_summary <- new_customer_list %>%
  group_by(gender) %>%
  summarize(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)



# Create a pie chart
#=====================

ggplot(order_status_summary, aes(x = "", y = count, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste(gender, "(", round(percentage, 1), "%)")), 
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Pie Chart of gender",
    x = NULL,
    y = NULL,
    fill = "gender"
  ) +
  theme_void() +
  theme(legend.position = "right")

#-------------------------------------------------------------------------------

# Histogram for 'bike_related_purchases'
#==========================================

ggplot(data = new_customer_list, aes(x = bike_related_purchases)) +
  geom_histogram(binwidth = 5, fill = "cyan", color = "maroon") +
  labs(
    title = "Histogram of Bike Related Purchases",
    x = "Bike Related Purchases",
    y = "Frequency"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------

# Histogram for 'tenure'
#=========================

ggplot(data = new_customer_list, aes(x =tenure)) +
  geom_histogram(binwidth = 1, fill = "gold", color = "red") +
  labs(
    title = "Histogram of tenure",
    x = "tenure",
    y = "Frequency"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------

# Histogram for 'Property Valuation'
#====================================

ggplot(data = new_customer_list, aes(x = property_valuation)) +
  geom_histogram(binwidth = 1, fill = "greenyellow", color = "steelblue") +
  labs(
    title = "Histogram of Property Valuation",
    x = "Property Valuation",
    y = "Frequency"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------

# Histogram for 'DOB' year
#==========================

ggplot(data = new_customer_list, aes(x = as.Date(DOB), y = ..count..)) +
  geom_histogram(binwidth = 365, fill = "aquamarine", color = "blue") +
  labs(
    title = "Distribution of Birthdates",
    x = "Date of Birth",
    y = "Frequency"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------

# Bar plot for 'job_industry_category'
#=======================================

ggplot(data = new_customer_list, aes(x = job_industry_category)) +
  geom_bar(fill = "violetred") +
  labs(
    title = "Distribution of job_industry_category",
    x = "job_industry_category",
    y = "Count"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------

# Bar plot for 'wealth_segment'
#=================================

ggplot(data = new_customer_list, aes(x = wealth_segment)) +
  geom_bar(fill = "forestgreen") +
  labs(
    title = "Distribution of wealth_segment",
    x = "wealth_segment",
    y = "Count"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------

# Bar plot for 'owns_car'
#==========================

# Create a data frame with counts of 'owns_car' values
owns_car_counts <- table(new_customer_list$owns_car)
print(owns_car_counts)

# Create a pie chart
ggplot(NULL, aes(x = "", y = owns_car_counts, fill = names(owns_car_counts))) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(
    title = "Pie Chart of owns_car",
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = c("coral", "lightblue")) +
  theme_void()

#-------------------------------------------------------------------------------

# Calculate the count and percentage of each deceased_indicator
#===============================================================

order_status_summary <- customer_address %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)



# Create a pie chart
#=======================

ggplot(order_status_summary, aes(x = "", y = count, fill = state)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste(state, "(", round(percentage, 1), "%)")),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Pie Chart of state",
    x = NULL,
    y = NULL,
    fill = "state"
  ) +
  theme_void() +
  theme(legend.position = "right")



#===============================================================================
################################################################################
#-------------------------------------------------------------------------------

 
#             .___  ___.  _______ .______        _______ ________ 
#            |   \/   | |   ____||   _  \      /  _____||   ____|
#           |  \  /  | |  |__   |  |_)  |    |  |  __  |  |__   
#          |  |\/|  | |   __|  |      /     |  | |_ | |   __|  
#         |  |  |  | |  |____ |  |\  \----.|  |__| | |  |____ 
#        |__|  |__| |_______|| _| `._____| \______| |_______|
  
  
#-------------------------------------------------------------------------------
################################################################################                                                                                      
#===============================================================================


#checking column names
#======================

print(colnames(transactions))
print(colnames(new_customer_list))
print(colnames(customer_demographic))
print(colnames(customer_address))

# Merge transactions and customer_demographic and customer_address data sets 
# based on 'customer_id'
#============================================================================
merged_data <- merge(transactions, customer_demographic, by = "customer_id")
merged_data <- merge(merged_data, customer_address, by = "customer_id")


glimpse(merged_data)

has_duplicates <- any(duplicated(merged_data))
has_duplicates

# Count null values 
print(colSums(is.na(transactions)))

# Remove rows with any NA values 
transactions <- na.omit(transactions)

print(colnames(merged_data))

#-------------------------------------------------------------------------------

# Scatter Plot of Age vs. Tenure
#================================

ggplot(merged_data, aes(x = age, y = tenure, color = gender)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Age vs. Tenure",
    x = "Age",
    y = "Tenure"
  ) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "pink"))

#-------------------------------------------------------------------------------

# Stacked Bar Chart of Car Ownership and Gender
#================================================

ggplot(merged_data, aes(x = gender, fill = owns_car)) +
  geom_bar(position = "stack") +
  labs(
    title = "Car Ownership by Gender",
    x = "Gender",
    y = "Count"
  ) +
  scale_fill_manual(values = c("No" = "cyan", "Yes" = "forestgreen"))

#-------------------------------------------------------------------------------

# Stacked Bar Chart of Wealth Segment and Car Ownership
#========================================================

ggplot(merged_data, aes(x = wealth_segment, fill = owns_car)) +
  geom_bar(position = "stack") +
  labs(
    title = "Car Ownership by Wealth Segment",
    x = "Wealth Segment",
    y = "Count"
  ) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"))

#-------------------------------------------------------------------------------

# Stacked Bar Chart of Job Industry and Gender
#===============================================

ggplot(merged_data, aes(x = job_industry_category, fill = gender)) +
  geom_bar(position = "stack") +
  labs(
    title = "Job Industry Distribution by Gender",
    x = "Job Industry",
    y = "Count"
  ) +
  scale_fill_manual(values = c("Male" = "cyan", "Female" = "pink", 
                               "Other" = "purple"))

#-------------------------------------------------------------------------------

# Box Plot of Tenure by Wealth Segment
#========================================

ggplot(merged_data, aes(x = wealth_segment, y = tenure, fill = 
                          wealth_segment)) + geom_boxplot() +
  labs(
    title = "Tenure Distribution by Wealth Segment",
    x = "Wealth Segment",
    y = "Tenure"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------

# Heatmap of Correlations:
#=========================

correlations <- cor(merged_data[, c("age", "tenure", "property_valuation")])
melt_corr <- melt(correlations)

ggplot(melt_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(
    title = "Correlation Heatmap",
    x = "",
    y = ""
  ) +
  scale_fill_gradient(low = "green", high = "yellow") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------

# Chord Diagram for Customer Interactions
#========================================

# Create a matrix of interactions (e.g., customer IDs) with counts
interaction_matrix <- table(merged_data$gender, merged_data$brand)

chordDiagram(interaction_matrix, transparency = 0.5)

#-------------------------------------------------------------------------------

# Brand Distribution by Age and Gender
#=======================================

data_counts <- merged_data %>%
  group_by(age, gender, brand) %>%
  summarise(Count = n())  # Calculate the count of customers

# Create the grouped bar chart
ggplot(data_counts, aes(x = age, y = Count, fill = brand)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ gender) +
  labs(
    title = "Brand Distribution by Age and Gender",
    x = "Age Group",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#-------------------------------------------------------------------------------

# Online Order Distribution by Brand and State
#===============================================
  
# Create the stacked bar chart
ggplot(merged_data, aes(x = state, fill = online_order)) +
  geom_bar(position = "fill") +
  facet_wrap(~ brand) +
  labs(
    title = "Online Order Distribution by Brand and State",
    x = "State",
    y = "Proportion of Online Orders"
  ) +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "green"), labels = 
                      c("No", "Yes")) +
  theme_minimal()

#-------------------------------------------------------------------------------

# Impact of Wealth Segment on Brand
#=====================================

# Create a bar chart
ggplot(merged_data, aes(x = brand, fill = wealth_segment)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Impact of Wealth Segment on Brand",
    x = "Brand",
    y = "Count"
  ) +
  scale_fill_manual(values = c("High Net Worth" = "blue", "Mass Customer" = 
                                 "red", "Affluent Customer" = "green")) +
  theme_minimal() +
  theme(legend.title = element_blank())  # Remove legend title


#-------------------------------------------------------------------------------
################################################################################
#===============================================================================


#                     88888888888  888b      88  88888888ba,    
#                    88           8888b     88  88      `"8b   
#                   88           88 `8b    88  88        `8b  
#                  88aaaaa      88  `8b   88  88         88  
#                 88"""""      88   `8b  88  88         88  
#                88           88    `8b 88  88         8P  
#               88           88     `8888  88      .a8P   
#              88888888888  88      `888  88888888Y"'    


#===============================================================================
################################################################################
#-------------------------------------------------------------------------------