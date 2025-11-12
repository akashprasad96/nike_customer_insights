
#'#Environment reset (run first)
rm(list = ls())            # clear objects
graphics.off()             # close plots
cat("\014")                # clear console

#'#Load Libraries
library(tidyverse)
library(janitor)
library(ggplot2)
library(forcats)
library(scales)
library(dplyr)
library(lubridate)


#'#########################################
#'# Part I - Exploring & Data Cleaning
#'#########################################

#'# Step 1: Read the product file
product <- read_csv("Nike_Sales_Uncleaned.csv")
nrow(product)
clean_names(product)
colnames(product)

#'# Understanding numeric data spread and identifying irrelevant columns
summary(product)
length(unique(product$Discount_Applied))  # counts how many distinct values
view(product$Size)

#'# Identifying character columns for conversion
view(product$Order_Date)
table(product$Sales_Channel) #'# Ditinct values with thier counts
glimpse(select(product, where(is.character))) 

table(product$Region) # Ditinct values with thier counts


#'#Irrelevant = Revenue, profit, Discount_Applied
#'#identifier = order_id,
#'#variables = gender_catg, product_line, Product_name, sales_channel, region
#'#conversion:-
#'#   units_sold = negative to 0
#'#   MRP =  Currency conversion to Dollars and storing in new column Price_USD
#'#   Sales = Units x Price_USD
#'#   Region =  Standardizing column values for "Hyderabad" and "Bangalore" entries
#'#   size = Creating new column by categorizing into footwear or apparel
#'#   order_date = convert to year


#'# Step 1 - Removing unwanted character fields
product <- select(product, -Profit, -Revenue, -Discount_Applied)

#'# Step 2 - Cleaning Units sold
#'# Replacing negative and NA with 0
product$Units_Sold <- ifelse((product$Units_Sold < 0) | is.na(product$Units_Sold),
                              0, product$Units_Sold)

#'# Step 3 - Currency conversion
#'# Current approximate rate: 1 USD ≈ 83 INR
product <- product %>%
  mutate(Price_USD = round(MRP / 88, 2 ))

#'# Step 4 - Create Sales column
product <- product %>%
  mutate(Sales = round(Units_Sold * Price_USD, 2 ))

#'# Step 5 - Standardizing the Region field
product <- product %>%
  mutate(Region = case_when(
    grepl("^beng|^bang", Region, ignore.case = TRUE) ~ "Bengaluru",
    grepl("^hyd", Region, ignore.case = TRUE) ~ "Hyderabad",
    TRUE ~ Region
  ))

#'# Step 6 - Creating a column to identify the product category as footwear or apparel
product <- product %>%
  mutate(Product_Category = case_when(
    is.na(Size) ~ NA_character_,
    grepl("^[0-9]+\\.?[0-9]*$", Size) ~ "Footwear",  # Pure numeric (7, 8.5, 12, etc)
    grepl("(?i)[smlx]", Size) ~ "Apparel",           # S/M/L/XL/XXL (case insensitive)
  ))


#'# Step 7 - Date field conversion
#'# This handles both formats automatically
product$Order_Date <- parse_date_time(product$Order_Date, 
                                       orders = c("ymd", "dmy"))

#'# Step 8 - Drop NA rows
product <- drop_na(product)

#'# Final Dataset for 
nrow(product)
colnames(product)
view(product)


#'#########################################
#'# PHASE I
#'#########################################

#'# Phase 1 color palette (Blues)
phase1_colors <- c("#0066CC", "#00A9E0", "#00BCD4", "#4FC3F7", "#81D4FA", 
                   "#0277BD", "#0288D1", "#039BE5", "#03A9F4")


#'# Visual 1: Product Line Performance
#'# Aggregate sales by product line
product_line_sales <- product %>%
  group_by(Product_Line) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

#'# Create the bar chart
viz1 <- ggplot(product_line_sales, aes(x = reorder(Product_Line, Total_Sales), 
                                       y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "#00A9E0") +
  coord_flip() +
  labs(
    title = "Product Line Performance",
    subtitle = "Which product lines drive our sales? | Phase 1: Market Performance",
    x = "Product Line",
    y = "Total Sales (USD)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#0066CC"),
    plot.subtitle = element_text(size = 12, color = "#555555", face = "italic"),
    axis.text = element_text(size = 11, color = "#333333"),
    axis.title = element_text(size = 12, face = "bold", color = "#0066CC"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

#'# Display
print(viz1)

#'# Visual 2: Product Category Mix

#'# Aggregate sales by product category
category_sales <- product %>%
  group_by(Product_Category) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  mutate(Percentage = Total_Sales / sum(Total_Sales) * 100)

#'# Create bar chart
viz2 <- ggplot(category_sales, aes(x = reorder(Product_Category, Total_Sales), 
                                   y = Total_Sales,
                                   fill = Product_Category)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(aes(label = paste0("$", scales::comma(round(Total_Sales)), "\n", 
                               round(Percentage, 1), "%")),
            hjust = -0.1, size = 4.5, fontface = "bold", color = "#333333") +
  coord_flip() +
  labs(
    title = "Product Category Mix",
    subtitle = "Footwear vs. Apparel: Understanding our portfolio balance | Phase 1",
    x = "Product Category",
    y = "Total Sales (USD)",
    fill = "Category"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(prefix = "$"), 
                     limits = c(0, max(category_sales$Total_Sales) * 1.15)) +
  scale_fill_manual(values = c("Footwear" = "#0066CC", "Apparel" = "#00BCD4")) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#0066CC"),
    plot.subtitle = element_text(size = 12, color = "#555555", face = "italic"),
    axis.text = element_text(size = 11, color = "#333333"),
    axis.title = element_text(size = 12, face = "bold", color = "#0066CC"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz2)

#'# Visual 3: Regional Performance

#'# Aggregate sales by region
regional_sales <- product %>%
  group_by(Region) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

#'# Create bar chart
viz3 <- ggplot(regional_sales, aes(x = reorder(Region, Total_Sales), 
                                   y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "#00A9E0") +
  geom_text(aes(label = paste0("$", scales::comma(round(Total_Sales)))),
            hjust = -0.1, size = 4, fontface = "bold", color = "#333333") +
  coord_flip() +
  labs(
    title = "Regional Performance",
    subtitle = "Where are we winning geographically? | Phase 1: Market Performance",
    x = "Region",
    y = "Total Sales (USD)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     limits = c(0, max(regional_sales$Total_Sales) * 1.15)) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#0066CC"),
    plot.subtitle = element_text(size = 12, color = "#555555", face = "italic"),
    axis.text = element_text(size = 11, color = "#333333"),
    axis.title = element_text(size = 12, face = "bold", color = "#0066CC"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz3)

#'# Visual 4: Customer Demographics (Gender Category)

#'# Aggregate sales by gender category
gender_sales <- product %>%
  group_by(Gender_Category) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

#'# Create bar chart
viz4 <- ggplot(gender_sales, aes(x = reorder(Gender_Category, Total_Sales), 
                                 y = Total_Sales,
                                 fill = Gender_Category)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(aes(label = paste0("$", scales::comma(round(Total_Sales)))),
            hjust = -0.1, size = 4.5, fontface = "bold", color = "#333333") +
  coord_flip() +
  labs(
    title = "Customer Demographics",
    subtitle = "Understanding who's buying: Men's, Women's, Kids | Phase 1",
    x = "Gender Category",
    y = "Total Sales (USD)",
    fill = "Segment"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     limits = c(0, max(gender_sales$Total_Sales) * 1.15)) +
  scale_fill_manual(values = c("Men" = "#0066CC", 
                               "Women" = "#4FC3F7", 
                               "Kids" = "#81D4FA")) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#0066CC"),
    plot.subtitle = element_text(size = 12, color = "#555555", face = "italic"),
    axis.text = element_text(size = 11, color = "#333333"),
    axis.title = element_text(size = 12, face = "bold", color = "#0066CC"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz4)

#'# Visual 5: Channel Effectiveness
#'# Aggregate sales by sales channel
channel_sales <- product %>%
  group_by(Sales_Channel) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

#'# Create bar chart
viz5 <- ggplot(channel_sales, aes(x = reorder(Sales_Channel, Total_Sales), 
                                  y = Total_Sales,
                                  fill = Sales_Channel)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  geom_text(aes(label = paste0("$", scales::comma(round(Total_Sales)))),
            hjust = -0.1, size = 4.5, fontface = "bold", color = "#333333") +
  coord_flip() +
  labs(
    title = "Channel Effectiveness",
    subtitle = "How are customers buying: Online vs. Retail? | Phase 1",
    x = "Sales Channel",
    y = "Total Sales (USD)",
    fill = "Channel"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     limits = c(0, max(channel_sales$Total_Sales) * 1.15)) +
  scale_fill_manual(values = c("Online" = "#00A9E0", "Retail" = "#0066CC")) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#0066CC"),
    plot.subtitle = element_text(size = 12, color = "#555555", face = "italic"),
    axis.text = element_text(size = 11, color = "#333333"),
    axis.title = element_text(size = 12, face = "bold", color = "#0066CC"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz5)

#'#########################################
#'# PHASE II
#'#########################################

#'# Visual 6: Channel vs Product Category (FLIPPED)
library(ggplot2)
library(dplyr)
library(scales)

#'# Aggregate units sold by channel and product category
channel_category <- product %>%
  group_by(Sales_Channel, Product_Category) %>%
  summarise(Total_Units = sum(Units_Sold, na.rm = TRUE), .groups = 'drop')

#'# Create grouped bar chart
viz6 <- ggplot(channel_category, aes(x = Sales_Channel, 
                                     y = Total_Units,
                                     fill = Product_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::comma(Total_Units)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 4, fontface = "bold", color = "#333333") +
  labs(
    title = "Channel-Product Fit",
    subtitle = "Which channels work best for which products? | Phase 2: Channel Strategy",
    x = "Sales Channel",
    y = "Units Sold",
    fill = "Product Category"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma_format(),
                     limits = c(0, max(channel_category$Total_Units) * 1.15)) +
  scale_fill_manual(values = c("Footwear" = "#4A148C", "Apparel" = "#9C27B0")) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#4A148C"),
    plot.subtitle = element_text(size = 12, color = "#555555", face = "italic"),
    axis.text = element_text(size = 11, color = "#333333"),
    axis.title = element_text(size = 12, face = "bold", color = "#4A148C"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz6)


#'# Visual 7: Channel Performance Over Time + Total Sales
#'# Visual 7: Channel Units + Total Sales (Dual Axis)

#'# Visual 7: Channel Performance with Grey Sales Bars


#'# Create monthly trends by channel
channel_trends <- product %>%
  mutate(Month_Year = floor_date(Order_Date, "month")) %>%
  group_by(Month_Year, Sales_Channel) %>%
  summarise(Units = sum(Units_Sold, na.rm = TRUE), .groups = 'drop')

#'# Create total sales (revenue)
total_sales <- product %>%
  mutate(Month_Year = floor_date(Order_Date, "month")) %>%
  group_by(Month_Year) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE), .groups = 'drop')

#'# Find max values for scaling
max_units <- max(channel_trends$Units)
max_sales <- max(total_sales$Total_Sales)

#'# Scale factor: adjust this to control bar height relative to lines
scale_factor <- max_units * 1.5 / max_sales

#'# Create the plot
viz7 <- ggplot() +
  #'# Sales bars (background, grey shaded)
  geom_col(data = total_sales,
           aes(x = Month_Year, y = Total_Sales * scale_factor),
           fill = "#BDBDBD", alpha = 0.6, width = 25) +
  #'# Channel units (foreground lines)
  geom_line(data = channel_trends, 
            aes(x = Month_Year, y = Units, color = Sales_Channel, group = Sales_Channel),
            size = 1.3) +
  geom_point(data = channel_trends,
             aes(x = Month_Year, y = Units, color = Sales_Channel),
             size = 3) +
  scale_y_continuous(
    name = "Units Sold",
    labels = comma_format(),
    sec.axis = sec_axis(~ . / scale_factor, 
                        name = "Total Sales (USD)",
                        labels = dollar_format(prefix = "$"))
  ) +
  labs(
    title = "Channel Performance & Total Sales Over Time",
    subtitle = "Channel units (lines) + Total sales revenue (bars) | Phase 2",
    x = "Month",
    color = "Sales Channel"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  scale_color_manual(values = c("Online" = "#9C27B0", "Retail" = "#4A148C")) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#4A148C"),
    plot.subtitle = element_text(size = 11, color = "#555555", face = "italic"),
    axis.text = element_text(size = 10, color = "#333333"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.left = element_text(size = 11, face = "bold", color = "#4A148C"),
    axis.title.y.right = element_text(size = 11, face = "bold", color = "#111111"),
    axis.title.x = element_text(size = 11, face = "bold", color = "#4A148C"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz7)

#'#########################################
#'# PHASE III
#'#########################################

#'# Visual 8: Seasonal Patterns by Product Category (PINK LINES FIXED)

#'# Visual 8: Seasonal Patterns by Product Category (CORRECT CATEGORIES)

#'# Extract month and aggregate across all years
seasonal_data <- product %>%
  mutate(Month = month(Order_Date, label = TRUE, abbr = TRUE)) %>%
  group_by(Month, Product_Category) %>%
  summarise(Total_Units = sum(Units_Sold, na.rm = TRUE), .groups = 'drop')

#'# Create the plot
viz8 <- ggplot(seasonal_data, aes(x = Month, y = Total_Units, 
                                  color = Product_Category, 
                                  group = Product_Category)) +
  geom_line(size = 1.3) +
  geom_point(size = 3) +
  labs(
    title = "Seasonal Patterns by Product Category",
    subtitle = "Which categories peak in which months? (All years combined) | Phase 3",
    x = "Month",
    y = "Total Units Sold",
    color = "Product Category"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma_format()) +
  scale_color_manual(values = c(
    "Apparel" = "#E91E63",      # Hot pink
    "Footwear" = "#AD1457"      # Deep pink
  )) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#AD1457"),
    plot.subtitle = element_text(size = 11, color = "#555555", face = "italic"),
    axis.text = element_text(size = 10, color = "#333333"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(size = 11, face = "bold", color = "#AD1457"),
    axis.title.x = element_text(size = 11, face = "bold", color = "#AD1457"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz8)


#'# Visual 9: Price Bins vs Units Sold by Product Category
#'# Visual 9A: Price Bins vs Units Sold - APPAREL ONLY
library(ggplot2)
library(dplyr)
library(scales)

#'# Create price bins for Apparel
apparel_binned <- product %>%
  filter(Product_Category == "Apparel") %>%
  mutate(Price_Bin = cut(Price_USD, 
                         breaks = c(0, 50, 100, 150, 200, 250, 300, Inf),
                         labels = c("$0-50", "$50-100", "$100-150", 
                                    "$150-200", "$200-250", "$250-300", "$300+"),
                         include.lowest = TRUE))

#'# Aggregate by price bin
apparel_analysis <- apparel_binned %>%
  group_by(Price_Bin) %>%
  summarise(Total_Units = sum(Units_Sold, na.rm = TRUE), .groups = 'drop')

#'# Create scatter plot for Apparel
viz9a <- ggplot(apparel_analysis, aes(x = Price_Bin, y = Total_Units)) +
  geom_point(size = 6, color = "#E91E63", alpha = 0.8) +
  labs(
    title = "Price Range vs Units Sold - Apparel",
    subtitle = "Does price affect apparel sales volume? | Phase 3",
    x = "Price Range (USD)",
    y = "Total Units Sold"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma_format()) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#E91E63"),
    plot.subtitle = element_text(size = 11, color = "#555555", face = "italic"),
    axis.text = element_text(size = 10, color = "#333333"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 11, face = "bold", color = "#E91E63"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz9a)

#'# Visual 9B: Price Bins vs Units Sold - FOOTWEAR ONLY
library(ggplot2)
library(dplyr)
library(scales)

#'# Create price bins for Footwear
footwear_binned <- product %>%
  filter(Product_Category == "Footwear") %>%
  mutate(Price_Bin = cut(Price_USD, 
                         breaks = c(0, 50, 100, 150, 200, 250, 300, Inf),
                         labels = c("$0-50", "$50-100", "$100-150", 
                                    "$150-200", "$200-250", "$250-300", "$300+"),
                         include.lowest = TRUE))

#'# Aggregate by price bin
footwear_analysis <- footwear_binned %>%
  group_by(Price_Bin) %>%
  summarise(Total_Units = sum(Units_Sold, na.rm = TRUE), .groups = 'drop')

#'# Create scatter plot for Footwear
viz9b <- ggplot(footwear_analysis, aes(x = Price_Bin, y = Total_Units)) +
  geom_point(size = 6, color = "#AD1457", alpha = 0.8) +
  labs(
    title = "Price Range vs Units Sold - Footwear",
    subtitle = "Does price affect footwear sales volume? | Phase 3",
    x = "Price Range (USD)",
    y = "Total Units Sold"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma_format()) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#AD1457"),
    plot.subtitle = element_text(size = 11, color = "#555555", face = "italic"),
    axis.text = element_text(size = 10, color = "#333333"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 11, face = "bold", color = "#AD1457"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz9b)

#'#########################################
#'# PHASE IV
#'#########################################
#'

#'# Visual 10: Gender Category vs Product Category
#'# Aggregate by Gender_Category and Product_Category
gender_category <- product %>%
  group_by(Gender_Category, Product_Category) %>%
  summarise(Total_Units = sum(Units_Sold, na.rm = TRUE), .groups = 'drop')

#'# Create grouped bar chart
viz10 <- ggplot(gender_category, aes(x = Product_Category, y = Total_Units, 
                                     fill = Gender_Category)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Gender Performance by Product Category",
    subtitle = "Which gender buys what? | Phase 4",
    x = "Product Category",
    y = "Total Units Sold",
    fill = "Gender Category"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = comma_format()) +
  scale_fill_manual(values = c(
    "Men" = "#BF360C",         # Dark red-orange
    "Women" = "#FF7043",       # Coral
    "Kids" = "#FFAB91"         # Light coral
  )) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#BF360C"),
    plot.subtitle = element_text(size = 11, color = "#555555", face = "italic"),
    axis.text = element_text(size = 10, color = "#333333"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11, face = "bold", color = "#BF360C"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz10)

#'# Visual 11: Regional Market Positioning - Bubble Chart with Gradient
#'# Aggregate by Region
regional_bubble <- product %>%
  group_by(Region) %>%
  summarise(
    Avg_Price = mean(Price_USD, na.rm = TRUE),
    Total_Sales = sum(Sales, na.rm = TRUE),
    Total_Units = sum(Units_Sold, na.rm = TRUE),
    .groups = 'drop'
  )

#'# Create bubble chart with colored legends
viz11 <- ggplot(regional_bubble, aes(x = Avg_Price, y = Total_Sales, 
                                     size = Total_Units, color = Total_Units,
                                     label = Region)) +
  geom_point(alpha = 0.8) +
  geom_text(size = 4, fontface = "bold", color = "#8B0000", vjust = -1.8) +
  labs(
    title = "Regional Market Positioning",
    subtitle = "Premium vs Value Markets: Price × Sales × Volume | Phase 4",
    x = "Average Price (USD)",
    y = "Total Sales (USD)",
    size = "Total Units Sold",
    color = "Total Units Sold"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  scale_size_continuous(range = c(8, 30), labels = comma_format()) +
  scale_color_gradientn(
    colors = c("#FFF3E0", "#FFB74D", "#FF7043", "#E64A19", "#BF360C"),
    labels = comma_format(),
    guide = guide_colorbar(barwidth = 1.5, barheight = 10, order = 1)
  ) +
  guides(
    size = guide_legend(override.aes = list(color = "#FF7043"), order = 2)
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "#BF360C"),
    plot.subtitle = element_text(size = 11, color = "#555555", face = "italic"),
    axis.text = element_text(size = 10, color = "#333333"),
    axis.title = element_text(size = 11, face = "bold", color = "#BF360C"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E0E0E0"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA)
  )

print(viz11)