## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library('tidyverse')

# Import the data set.
turtle_sales <- read.csv(file.choose(), header=T)

# Print the data frame.
turtle_sales

# Create a new data frame from a subset of the sales data frame.
# Remove redundant columns (Ranking, Year, Genre, Publisher)
turtle_sales2 <- select(turtle_sales, -Ranking, -Year, -Genre, - Publisher)

# View the data frame.
head(turtle_sales2)

# View the descriptive statistics.
summary(turtle_sales2)

################################################################################
# 2. Prepare the data

# Convert 'Product' to factor (categorical variable).
turtle_sales3 <- mutate( turtle_sales2,
                 Product = as.factor(Product))
# View as a tibble.
as_tibble(turtle_sales3)

# View summary.
summary(turtle_sales3)

# Check for missing data in the entire dataframe
missing_data <- sum(is.na(turtle_sales3))

# Print the results
print(paste("Total missing data:", missing_data))

##################################################################################
# 3.  Explore the data set
# Install the psych package if not already installed
#install.packages("e1071")

# Load necessary packages
library(e1071)      # for skewness and kurtosis
library(dplyr)

# Return a frequency table for the 'Platform' column.
table(turtle_sales3$Platform)

# Remove rows where Platform is equal to "2600"
turtle_sales3 <- turtle_sales3 %>%
  filter(Platform != "2600")

# Confirm delete.
table(turtle_sales3$Platform)

# Return a frequency table for the 'Product' column.
table(turtle_sales3$Product)

# Create new column 'Regional_Sales' by adding EU_Sales and NA_Sales
turtle_sales_clean <- turtle_sales3 %>%
  mutate(Regional_Sales = EU_Sales + NA_Sales)

# Print DataFrame
print(turtle_sales_clean)

# Save the cleaned turtle_sales dataframe as a CSV file
write.csv(turtle_sales_clean, file = "turtle_sales_clean.csv", row.names = FALSE)


# Calculate the sum of Regional_Sales
total_regional_sales <- sum(turtle_sales_clean$Regional_Sales)

# Print the total_regional_sales.
total_regional_sales

# Calculate the sum of Global_Sales
total_global_sales <- sum(turtle_sales_clean$Global_Sales)

# Print the total_global_sales
total_global_sales

# Calculate the percentage of Global_Sales represented by Regional_Sales
percentage_regional_sales <- (total_regional_sales / total_global_sales) * 100

# Print the percentage_regional_sales
percentage_regional_sales

# Calculate Total Sales per Product Regionally
product_sales <- turtle_sales_clean %>%
  group_by(Product) %>%
  summarise(Total_Sales = sum(Regional_Sales)) %>%
  arrange(desc(Total_Sales))

# Identify Products with Higher Sales
# Identify the top 10 products
top_products <- head(product_sales, 10)  

# Print the top_products
top_products

# Identify Products with Lower Sales
# Identify the bottom 10 products
bottom_products <- tail(product_sales, 10)

# Print the bottom_products
bottom_products

# Group the data by platform and calculate total sales for EU_Sales 
# and NA_Sales
sales_per_platform <- turtle_sales_clean %>%
  group_by(Platform) %>%
  summarise(Total_EU_Sales = sum(EU_Sales),
            Total_NA_Sales = sum(NA_Sales))

# Print the table
print(sales_per_platform)



# Calculate frequency of each Platform
platform_frequency <- turtle_sales_clean %>%
  count(Platform) %>%
  arrange(desc(n))

# Print the table
print(platform_frequency)


#  Assess Skewness for Regional Sales
skewness_global <- skewness(turtle_sales_clean$Regional_Sales)

# Print Skewness
print(paste("Skewness of Global Sales:", skewness_global))

# Assess Kurtosis for Regional Sales
kurtosis_global <- kurtosis(turtle_sales_clean$Regional_Sales)

# Print Kurtosis
print(paste("Kurtosis of Global Sales:", kurtosis_global))

# Filter rows with NA_Sales above 15, then arrange in descending order by sales
products_above_15_na <- turtle_sales_clean %>%
  filter(NA_Sales > 15) %>%
  arrange(desc(NA_Sales))

# Print the resulting dataframe
print(products_above_15_na)

# Filter rows with EU_Sales above 20, then arrange in descending order by sales
products_above_20_eu <- turtle_sales_clean %>%
  filter(EU_Sales > 20) %>%
  arrange(desc(EU_Sales))

# Print the resulting dataframe
print(products_above_20_eu)

# Calculate the correlation coefficient between NA Sales and EU Sales
correlation <- cor(turtle_sales_clean$NA_Sales, turtle_sales_clean$EU_Sales)

# Print the correlation coefficient
print(correlation)
################################################################################
# Load necessary packages
library(ggplot2)  # for data visualization

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Scatterplot of NA Sales vs EU Sales with trend line
ggplot(turtle_sales_clean, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point(color = "#52B2BF") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear trend line
  labs(title = "Scatterplot of NA Sales vs EU Sales with Trend Line",
       x = "NA Sales (millions)",
       y = "EU Sales (millions)")


# Create a scatter plot to show the relationship between platform frequency 
# and total sales
# Combine total sales from both regions for each platform
sales_per_platform$total_sales <- sales_per_platform$Total_EU_Sales + 
  sales_per_platform$Total_NA_Sales

# Merge platform frequency with total sales
platform_data <- merge(
  platform_frequency, sales_per_platform, by = "Platform", all = TRUE)

# scatter plot
ggplot(platform_data, aes(x = n, y = total_sales)) +
  geom_point(color = "#52B2BF") +
  labs(title = "Relationship between Platform Frequency and Total Sales",
       x = "Platform Frequency",
       y = "Total Sales (millions)") +
  theme_minimal()


## 2b) Histograms
# Distribution of Global Sales.
ggplot(turtle_sales_clean, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 7, fill = "#52B2BF", color = "black") +
  labs(title = "Distribution of Global Sales",
       x = "Global Sales (millions)",
       y = "Frequency") +
  theme_minimal()

## 2c) Boxplots
# Boxplot of NA_Sales and EU_Sales
sales_boxplot <- gather(
  turtle_sales_clean, key = "Region", 
  value = "Sales", 
  NA_Sales, EU_Sales)

ggplot(sales_boxplot, aes(x = Region, y = Sales, fill = Region)) +
  geom_boxplot() +
  labs(title = "Boxplot of Sales by Region",
       x = "Region",
       y = "Sales (millions)",
       fill = "Region") +
  scale_fill_manual(values = c("NA_Sales" = "#52B2BF", "EU_Sales" = "#bf5f52"))

## 2d) Bar chart

# Create a bar chart for platform frequency
ggplot(platform_frequency, aes(x = reorder(Platform, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#52B2BF") +
  labs(title = "Platform Frequency",
       x = "Platform",
       y = "Frequency") +
  theme_minimal()




# Create a bar chart to show the total sales for each platform

# Combine total sales from both regions for each platform
sales_per_platform$total_sales <- 
  sales_per_platform$Total_EU_Sales + sales_per_platform$Total_NA_Sales
ggplot(sales_per_platform, aes(x = reorder(Platform, -total_sales), 
                               y = total_sales, fill = Platform)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Total Sales by Platform",
       x = "Platform",
       y = "Total Sales",
       fill = "Platform") +
  scale_fill_viridis_d() +  # Use the Viridis color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


 
###############################################################################

# 3. Observations and insights
# Glabal_sales includes sales from other regions not included in the CSV file.
# Platform named 2600 was observed and deleted.
# Total sales per product: The products with higher sales are 107,515, 123,254,
# 195,326, 948,876, 231, and 979. While the products with lower sales are 
# 447,1459, 6770, 7573, 7533, 6721, 5510, 7143, 5726, and 6471.
# Scatter Plot: there is a  positive correlation, sales increase in one 
# region, they also tend to increase in the other region.
# Outliers were identified in points that deviate significantly from the main
# trend line. These are sales above 15 Million in NA Sales and 20 Million in EU
# Sales. This can highlight specific products or platforms that perform 
# exceptionally well in one region compared to the other. 
# The correlation coefficient between NA Sales and EU Sales is 
# approximately 0.7088. A moderately strong positive correlation
# but it's not a perfect correlation. Adjustments may need to be made in 
# strategies or marketing efforts to leverage or mitigate the correlation 
# between these regions based on specific business goals and market conditions.
# There is also a positive relationship between total sales and frequency of 
# platform

# Histograms: The histogram shows that sales is skewed to the right with
# evidence of outliers.
# The most common or average sales value is around 4 and 10 million which is
# located at the peak of the distribution.

# Boxplot: Most of the sales in EU are concentrated below 8 Million while NA sales
# seem to have a fair spread. Outliers are clearly seen here.
# The median line seems centered within the box in NA and a little off in EU. Hence
# a symmetric distribution is indicated. While skewness is evident in EU data.

# Kurtosis and Skewness:
# The positive skewness value suggests that there is a significant proportion of 
# Global Sales values concentrated at the lower end, with a long tail towards 
# higher values.
# The high kurtosis value indicates that the distribution of Global Sales has 
# heavy tails and a sharp peak, implying the presence of extreme values and 
# higher variability compared to a normal distribution.
# 
# Together, the skewness and kurtosis values indicate that the distribution of 
# Global Sales is not symmetric, with a tendency towards higher values and a 
# higher likelihood of extreme observations.

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# Load and explore the data
turtle_sales_clean <- read.csv(file.choose(), header=TRUE) 

# View data frame created in Week 4.
View(turtle_sales_clean)

# View data as tibble
as_tibble(turtle_sales_clean)

# View the dimensions of the data set i.e. the number of rows and columns.
dim(turtle_sales_clean)


# Calculate the thresholds for outliers in NA_Sales, EU_Sales and Global_Sales columns
# Calculate q1 and q2
na_sales_q <- quantile(turtle_sales_clean$NA_Sales, probs = c(0.25, 0.75))
eu_sales_q <- quantile(turtle_sales_clean$EU_Sales, probs = c(0.25, 0.75))
global_sales_q <- quantile(turtle_sales_clean$Global_Sales, probs = c(0.25, 0.75))

# Calculate iqr
na_sales_iqr <- na_sales_q[2] - na_sales_q[1]
eu_sales_iqr <- eu_sales_q[2] - eu_sales_q[1]
global_sales_iqr <- global_sales_q[2] - global_sales_q[1]

# Calculate Upper and lower band limits
na_sales_lower_bound <- na_sales_q[1] - 1.5 * na_sales_iqr
na_sales_upper_bound <- na_sales_q[2] + 1.5 * na_sales_iqr

eu_sales_lower_bound <- eu_sales_q[1] - 1.5 * eu_sales_iqr
eu_sales_upper_bound <- eu_sales_q[2] + 1.5 * eu_sales_iqr

global_sales_lower_bound <- global_sales_q[1] - 1.5 * global_sales_iqr
global_sales_upper_bound <- global_sales_q[2] + 1.5 * global_sales_iqr

# Remove rows with outliers in NA_Sales, EU_Sales and Global_Sales columns
turtle_sales_clean_no_outliers <- turtle_sales_clean %>%
  filter(NA_Sales >= na_sales_lower_bound & NA_Sales <= na_sales_upper_bound &
           EU_Sales >= eu_sales_lower_bound & EU_Sales <= eu_sales_upper_bound &
           Global_Sales >= global_sales_lower_bound & Global_Sales <= global_sales_upper_bound )


# Check output: Determine the min, max, and mean values.

# View the descriptive statistics.
summary(turtle_sales_clean_no_outliers)

# Create a data profile report.
DataExplorer::create_report(turtle_sales_clean_no_outliers)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sum_per_product <- turtle_sales_clean_no_outliers %>%
  group_by(Product) %>%
  summarise(
    Total_NA_Sales = sum(NA_Sales),
    Total_EU_Sales = sum(EU_Sales),
    Total_Regional_Sales = sum(Regional_Sales),
    Total_Global_Sales = sum(Global_Sales)
  )

# View the resulting dataframe
print(sum_per_product)


# View the data frame.
View (sum_per_product)

# Save thesum_per_product dataframe as a CSV file
write.csv(sum_per_product, file = "sum_per_product.csv", row.names = FALSE)

# Explore the data frame.
head(sum_per_product) # View the first few rows
summary(sum_per_product)  # Summary statistics for numerical columns

# Bar plot of total sales per product
ggplot(sum_per_product, aes(x = Product, y = Total_Global_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Global Sales per Product",
       x = "Product",
       y = "Total Global Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Top 5 products by total global sales
top_products <- head(sum_per_product[order(sum_per_product$Total_Global_Sales, decreasing = TRUE), ], 5)
print(top_products)

# Bottom 5 products by total global sales
bottom_products <- head(sum_per_product[order(sum_per_product$Total_Global_Sales), ], 5)
print(bottom_products)

# Calculate correlations between total sales
cor_matrix <- cor(sum_per_product[, c("Total_NA_Sales", "Total_EU_Sales", "Total_Global_Sales")])
print(cor_matrix)

## 2b) Determine which plot is the best to compare game sales.

# Create scatterplots.

# Scatterplot comparing NA_Sales and EU_Sales
ggplot(sum_per_product, aes(x = Total_NA_Sales, y = Total_EU_Sales)) +
  geom_point(color = "#52B2BF", alpha = 0.6, size = 3) +  # Adjust point transparency and size
  geom_smooth(method = "lm", se = FALSE, color = "#bf5f52") +  # Add a linear regression line
  labs(title = "Game Sales Comparison (NA vs. EU)",
       x = "NA Total Sales",
       y = "EU Total Sales") +
  theme_minimal() +  # Apply a minimalistic theme
  theme(plot.title = element_text(hjust = 0.5),  # Center plot title
        axis.text = element_text(size = 10),  # Adjust axis text size
        axis.title = element_text(size = 12, face = "bold"))  # Adjust axis title size and font


# Create a histogram to compare game sales
histogram <- ggplot(sum_per_product, aes(x = Total_Global_Sales)) +
  geom_histogram(binwidth = 2, fill = "#52B2BF", color = "#bf5f52") +
  labs(title = "Distribution of Total Global Sales per Product",
       x = "Total Global Sales (millions)",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  )

# Print the histogram
print(histogram)


# Create density plot of Total Global Sales
density_plot <- ggplot(sum_per_product, aes(x = Total_Global_Sales)) +
  geom_density(fill = "#52B2BF", color = "#bf5f52", alpha = 0.7) +
  labs(title = "Density of Total Global Sales per Product",
       x = "Total Global Sales (millions)",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.text = element_text(size = 10),     # Adjust text size
        axis.title = element_text(size = 12))    # Adjust axis label size

# Print the density plot
print(density_plot)

# Create boxplots.
# Boxplot comparing game sales
# Create a boxplot comparing game sales
boxplot <- ggplot(sum_per_product, aes(x = "", y = Total_Global_Sales)) +
  geom_boxplot(fill = "#52B2BF", color = "#bf5f52", alpha = 0.7) +
  labs(title = "Comparison of Game Sales",
       y = "Total Global Sales",
       x = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

# Print the boxplot
print(boxplot)


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Create Q-Q plot for NA Sales
qqnorm(sum_per_product$Total_NA_Sales, main = "Q-Q Plot for NA Sales")
qqline(sum_per_product$Total_NA_Sales)

# Create Q-Q plot for EU Sales
qqnorm(sum_per_product$Total_EU_Sales, main = "Q-Q Plot for EU Sales")
qqline(sum_per_product$Total_EU_Sales)

# Create Q-Q plot for Regional Sales
qqnorm(sum_per_product$Total_Regional_Sales, main = "Q-Q Plot for Regional Sales")
qqline(sum_per_product$Total_Regional_Sales)

# Create Q-Q plot for Global Sales
qqnorm(sum_per_product$Total_Global_Sales, main = "Q-Q Plot for Global Sales")
qqline(sum_per_product$Total_Global_Sales)


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.

# Perform Shapiro-Wilk test for NA Sales
shapiro_test_na <- shapiro.test(sum_per_product$Total_NA_Sales)
print("Shapiro-Wilk test for NA Sales:")
print(shapiro_test_na)

# Perform Shapiro-Wilk test for EU Sales
shapiro_test_eu <- shapiro.test(sum_per_product$Total_EU_Sales)
print("Shapiro-Wilk test for EU Sales:")
print(shapiro_test_eu)

# Perform Shapiro-Wilk test for Regionl Sales
shapiro_test_regional <- shapiro.test(sum_per_product$Total_Regional_Sales)
print("Shapiro-Wilk test for Regional Sales:")
print(shapiro_test_regional)

# Perform Shapiro-Wilk test for Global Sales
shapiro_test_global <- shapiro.test(sum_per_product$Total_Global_Sales)
print("Shapiro-Wilk test for Global Sales:")
print(shapiro_test_global)


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

# Install and load the e1071 package if not already installed
  'if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}'
# library(e1071)

# Calculate skewness for NA Sales
skewness_na <- skewness(sum_per_product$Total_NA_Sales)
print("Skewness of NA Sales:")
print(skewness_na)

# Calculate skewness for EU Sales
skewness_eu <- skewness(sum_per_product$Total_EU_Sales)
print("Skewness of EU Sales:")
print(skewness_eu)

# Calculate skewness for Regional Sales
skewness_regional <- skewness(sum_per_product$Total_Regional_Sales)
print("Skewness of Regional Sales:")
print(skewness_regional)

# Calculate skewness for Global Sales
skewness_global <- skewness(sum_per_product$Total_Global_Sales)
print("Skewness of Global Sales:")
print(skewness_global)

# Calculate kurtosis for NA Sales
kurtosis_na <- kurtosis(sum_per_product$Total_NA_Sales)
print("Kurtosis of NA Sales:")
print(kurtosis_na)

# Calculate kurtosis for EU Sales
kurtosis_eu <- kurtosis(sum_per_product$Total_EU_Sales)
print("Kurtosis of EU Sales:")
print(kurtosis_eu)

# Calculate kurtosis for Regional Sales
kurtosis_regional <- kurtosis(sum_per_product$Total_Regional_Sales)
print("Kurtosis of Regional Sales:")
print(kurtosis_regional)

# Calculate kurtosis for Global Sales
kurtosis_global <- kurtosis(sum_per_product$Total_Global_Sales)
print("Kurtosis of Global Sales:")
print(kurtosis_global)

## 3d) Determine correlation
# Calculate the correlation matrix for sales data columns
sales_correlation <- cor(sum_per_product[, c("Total_NA_Sales", "Total_EU_Sales",
                                             "Total_Regional_Sales", "Total_Global_Sales")])
print("Correlation Matrix:")
print(sales_correlation)


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Filter products with total global sales above 7.5 million
filtered_sum_per_product <- sum_per_product[sum_per_product$Total_Global_Sales > 17.5,]
# Bar Plot of Total Global Sales per Product with custom theme
bar_plot <- ggplot(filtered_sum_per_product, aes(x = reorder(
  Product, -Total_Global_Sales), y = Total_Global_Sales)) +
  geom_bar(stat = "identity", fill = "#52B2BF") +
  labs(title = "Total Global Sales per Product (Sales > £17.5M)",
       x = "Product",
       y = "Total Global Sales (millions)") +
  theme_minimal()

# Print the plot
print(bar_plot)

# Define the product codes
productg17.5M <- c("978", "999", "1307", "1501", "1945", "2285", "2877")
# Specify the ggplot function for plotting product sales distribution.
grouped_sales_plot <- ggplot(filtered_sum_per_product, aes(x = factor(
  Product, levels = productg17.5M))) +  
  # Specify the geom_bar function.
  # Add position.
  geom_bar(aes(y = Total_NA_Sales, fill = "Total NA Sales"), 
           stat = "identity", position = "dodge") +
  geom_bar(aes(y = Total_EU_Sales, fill = "Total EU Sales"), 
           stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("Total NA Sales" = "#52B2BF", 
                               "Total EU Sales" = "#bf5f52")) +
  theme_minimal() +
  # Add a label to y.
  labs(title = "NA vs. EU Total Sales (Sales > £17.5M) by Product",
       x = "Product",
       y = "Total Sales (millions)")

# Print the plot
print(grouped_sales_plot)




# Filter products with total global sales between 5 and 7 million
filtered_sum_per_product2 <- sum_per_product[
  sum_per_product$Total_Global_Sales >= 5 & sum_per_product$Total_Global_Sales <= 7, ]

# Limit the number of product names displayed to 10
filtered_sum_per_product2 <- filtered_sum_per_product2[1:10, ]

# Bar chart to display products with total global sales between 5 and 7 million
sales_range_bar_plot <- ggplot(filtered_sum_per_product2, aes(x = reorder(
  Product, Total_Global_Sales), y = Total_Global_Sales)) +
  geom_bar(stat = "identity", fill = "#52B2BF") +
  labs(title = "Top 10 Products with Total Global Sales Between £5M and £7M",
       x = "Product",
       y = "Total Global Sales (millions)") +
  theme_minimal()

# Print the plot
print(sales_range_bar_plot)


# Define the product codes
products_5n7M <- c("2371", "2387", "2457", "2495", "2521", "2793", 
                   "2807","2811","2814", "2870")
# Specify the ggplot function for plotting product sales distribution.
grouped_sales_plot2 <- ggplot(filtered_sum_per_product2, aes(
  x = factor(Product, levels = products_5n7M))) +  
  # Specify the geom_bar function.
  # Add position.
  geom_bar(aes(y = Total_NA_Sales, fill = "Total NA Sales"), stat = "identity", 
           position = "dodge") +
  geom_bar(aes(y = Total_EU_Sales, fill = "Total EU Sales"), stat = "identity", 
           position = "dodge") + 
  scale_fill_manual(values = c(
    "Total NA Sales" = "#52B2BF", "Total EU Sales" = "#bf5f52")) +
  theme_minimal() +
  # Add a label to y.
  labs(title = "NA vs. EU Total Sales (Sales Between £5M and £7M) by Product",
       x = "Product",
       y = "Total Sales (millions)")

# Print the plot
print(grouped_sales_plot2)

# High sales figures Product Distribution by Platform
# Filter 'turtle_sales_clean' by 'productg17.5M'
filtered_sales <- turtle_sales_clean %>%
  filter(Product %in% productg17.5M)

# Group the filtered result by 'Platform' and count the number of products per platform
platform_counts <- filtered_sales %>%
  group_by(Platform) %>%
  summarise(Product_Count = n()) %>%
  arrange(desc(Product_Count)) # Arrange in descending order by product count

# Plot the product distribution by platform
platform_distribution_plot <- ggplot(platform_counts, aes(
  x = reorder(Platform, -Product_Count), y = Product_Count)) +
  geom_bar(stat = "identity", fill = "#52B2BF") +
  labs(title = "High sales figures Product Distribution by Platform",
       x = "Platform",
       y = "Product Count") +
  theme_minimal()

# Print the plot
print(platform_distribution_plot)

#Top 10 Products with Total Global Sales Between £5M and £7M by platfrom
# Filter 'turtle_sales_clean' by 'pproducts_5n7M'
filtered_sales2 <- turtle_sales_clean %>%
  filter(Product %in% products_5n7M)

# Group the filtered result by 'Platform' and count the number of products per platform
platform_counts <- filtered_sales2 %>%
  group_by(Platform) %>%
  summarise(Product_Count = n()) %>%
  arrange(desc(Product_Count)) # Arrange in descending order by product count

# Plot the product distribution by platform
platform_distribution_plot2 <- ggplot(platform_counts, aes(
  x = reorder(Platform, -Product_Count), y = Product_Count)) +
  geom_bar(stat = "identity", fill = "#bf5f52") +
  labs(title = "Top 10 Volume Product Distribution by Platform",
       x = "Platform",
       y = "Product Count") +
  theme_minimal()

# Print the plot
print(platform_distribution_plot2)

###############################################################################

# 5. Observations and insights
# Scatter plot
"There is a correlation between Sales in Europe and North America"

# Histogram
"When the product frequency peaked, it dropped and sales increased. 
The popular product doesn’t generate the highest income figure."

# Density plot
"Fewer Products record the highest sales number."

# Box Plot
"About seven products sold above 17.5M"

# Q-Q plot
"The Q-Q plot for global sales indicates a positive correlation, and data points
are right skewed"

# Shapiro-Wilk test
"For all sales types (NA Sales, EU Sales, Regional Sales, and Global Sales), 
the p-values are extremely low (close to 0), indicating strong evidence against 
the null hypothesis of normality"

# Skewness
"All sales types exhibit positive skewness. Positive skewness indicates that the
distribution is right-skewed, meaning there is a longer tail on the right side 
of the distribution.
The positive skewness suggests that there is an asymmetry in the distribution of
sales data, with more extreme values occurring on the higher end. Based on 
previous analysis, This skewness is due to a few products with exceptionally 
high sales figures, contributing to the longer tail on the right side of the 
distribution.Visualizing the data and identifying high-leverage points can 
provide further insights into the distributional characteristics and potential 
biases in the data."

# Kurtosis
"All sales types exhibit positive kurtosis. Positive kurtosis indicates that the
distribution has heavier tails and a sharper peak compared to a normal 
distribution. This suggests that the data have more extreme values and higher 
variability than a normal distribution. The sharper peak and heavier tails 
suggest that there may be products with exceptionally high sales figures 
contributing to the variability in the data."

# Correlation Matrix
"Regional Sales (NA and EU Sales) has a very high impact on Global Sales"




###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
sum_per_product <- read.csv(file.choose(), header=TRUE) 

# View data frame created in Week 5.
 print(sum_per_product)

# Remove Product and Total_Regional_Sales from the DataFrame
sum_per_product2 <- select(sum_per_product, -c(Total_Regional_Sales, Product))

# Determine a summary of the data frame.
summary(sum_per_product2)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
# Determine the correlation between the sales columns
sales_correlation <- cor(select(sum_per_product2, Total_EU_Sales, 
                                Total_NA_Sales, Total_Global_Sales))
# View the correlation output
print(sales_correlation)

# Import the psych package.
library(psych)

# Use the corPlot() function.
# Specify the data frame (sum_per_product2) and set 
# character size (cex=2).
corPlot(sum_per_product2, cex=2, main = "Sales_correlation")

## 2b) Create a plot (simple linear regression)
# Step 2: Create plots to view the linear regression
# Plot Total_EU_Sales vs Total_NA_Sales
plot(sum_per_product2$Total_NA_Sales, sum_per_product2$Total_EU_Sales,
     main = "Total EU Sales vs Total NA Sales",
     xlab = "Total NA Sales",
     ylab = "Total EU Sales",
     col = "#52B2BF")

# Fit linear regression model
eu_na_model <- lm(Total_EU_Sales ~ Total_NA_Sales, data = sum_per_product2)

# Add regression line to the plot
abline(eu_na_model, col = "#bf5f52")

# Plot Total_EU_Sales vs Total_Global_Sales
plot(sum_per_product2$Total_Global_Sales, sum_per_product2$Total_EU_Sales,
     main = "Total EU Sales vs Total Global Sales",
     xlab = "Total Global Sales",
     ylab = "Total EU Sales",
     col = "#52B2BF")

# Fit linear regression model
eu_global_model <- lm(Total_EU_Sales ~ Total_Global_Sales, 
                      data = sum_per_product2)

# Add regression line to the plot
abline(eu_global_model, col = "#bf5f52")

###############################################################################

# 3. Create a multiple linear regression model
# Create a new object and specify the lm function and the variables.
modela = lm(Total_Global_Sales~Total_EU_Sales+Total_NA_Sales, 
            data=sum_per_product2)

# Print the summary statistics.
summary(modela)

###############################################################################

# 4. Predictions based on given values
# Provided values of NA_Sales_sum and EU_Sales_sum
NA_Sales_sum <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales_sum <- c(23.80, 1.56, 0.65, 0.97, 0.52)

# Create a dataframe with the provided values
test_data <- data.frame(Total_NA_Sales = NA_Sales_sum, Total_EU_Sales = 
                         EU_Sales_sum)

#Predict global sales using the linear regression model
predicted_global_sales <- predict(modela, newdata = test_data)

# Print the predicted global sales
cat("Predicted Global Sales:\n", predicted_global_sales, "\n")

# Observed global sales
observed_global_sales <- NA_Sales_sum + EU_Sales_sum

# Print the observed global sales
cat("Observed Global Sales:\n", observed_global_sales, "\n")


# Compare with observed values for a number of records.
comparison <- data.frame(NA_Sales_sum, EU_Sales_sum, Predicted_Global_Sales = 
                           predicted_global_sales, Observed_Global_Sales = 
                           observed_global_sales)
print(comparison)

###############################################################################

# 5. Observations and insights
# Your observations and insights here...

observation <- " Total_EU_Sales: The coefficient of approximately 1.17244 
suggests that, holding other variables constant, a one-unit increase in 
European sales is associated with an increase of approximately 1.17244 units in 
global sales.

Total_NA_Sales: Similarly, the coefficient of approximately 0.96746 indicates 
that, holding other variables constant, a one-unit increase in North American 
sales is associated with an increase of approximately 0.96746 units in global 

The p-values associated with the coefficients of Total_EU_Sales and 
Total_NA_Sales are both less than 0.001 (indicated by the '***' symbol). 
This suggests strong evidence against the null hypothesis that these 
coefficients are equal to zero, indicating that both predictors are 
significantly related to global sales.

The Multiple R-squared value of approximately 0.9157 indicates that 
about 91.57% of the variability in global sales is explained by the model. 
The Adjusted R-squared value of approximately 0.9147 adjusts the R-squared value
for the number of predictors in the model.

There is indeed a relationship between sales in North America (NA), Europe (EU),
and global sales

Higher values of NA_Sales_sum and EU_Sales_sum generally result 
in higher predicted global sales.

The linear regression model tends to overestimate the global sales based on the
provided values of NA_Sales_sum and EU_Sales_sum

The discrepancies between predicted and observed global sales could be 
influenced by various factors not captured by the provided predictor variables 
(NA_Sales_sum and EU_Sales_sum) which includes sales in other region or 
continent.

The linear regression model's performance may be satisfactory in providing 
rough estimates of global sales based on regional sales data.

Further evaluation, refinement, and potentially incorporating additional 
predictor variables may improve the accuracy of global sales predictions."

Conclusion < - "The analysis reveals several key insights crucial for business 
decision-making. Firstly, there exists a strong positive correlation between 
sales in Europe (EU) and North America (NA) with global sales, indicating the 
significance of these regions in driving overall sales performance. The multiple
linear regression model further confirms this relationship, with both EU and NA 
sales significantly contributing to global sales. However, the model tends to 
overestimate global sales, suggesting potential factors not captured by the 
provided predictors. Further exploration could involve incorporating additional 
variables such as marketing expenditure, economic indicators, or product 
features to enhance prediction accuracy. Additionally, investigating the 
discrepancies between predicted and observed global sales can provide 
valuable insights into regional market dynamics and potential areas for 
optimization. Future actions may include refining the model, conducting 
market segmentation analysis, and devising targeted marketing strategies to 
capitalize on regional sales opportunities and mitigate forecasting 
inaccuracies."

###############################################################################
###############################################################################




