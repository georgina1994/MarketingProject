#################################
# Air France Internet Marketing #
#################################
# Team 9 - Business Case Presentation
# R analysis of Air France online revenue in June, 2007 - Hult International Business School

#Authors:
#  - Thiago Amaral De Almeida Marcondes
#  - Georgina Canela Ferre
#  - Punyisa Kraisang
#  - Mable Nalugya
#  - Bleson Shaji Mathew



# SETUP ##################################################################################################################
# Import libraries
library(readxl)
library(dplyr)
library(caTools)
library (ggrepel)



# IMPORT DATA & INITIAL VIEW #############################################################################################
# Import Air France's DoubleClick dataframe
file_path <- "Air France Case Spreadsheet Supplement.xls"
dbc_df    <- read_xls(file_path, sheet = "DoubleClick")

# Examine DoubleClick data
View(dbc_df)
summary(dbc_df)
dim(dbc_df)
cat("Numbers of NA values:", sum(is.na(dbc_df)))

# Change columns name for more convinient when processing data
names(dbc_df) <- c("publisher_id", "publisher_name",    "keyword_id",  "keyword",       "match_type", 
                   "campaign",     "keyword_group",     "category",    "bid_strategy",  "keyword_type",
                   "status",       "search_engine_bid", "clicks",      "click_charges", "avg_cost_per_click",
                   "impressions",  "engine_click_thru", "avg_pos",     "trans_conv",    "total_cost_per_trans",
                   "amount",       "total_cost",        "total_volumn_of_bookings")



# CLEANING ###############################################################################################################
# Column "Keyword Type" is unassigned for entire column, consider remove it.
dbc_df <- subset(dbc_df, select = -keyword_type)



# FINDING THESHOLD #######################################################################################################
## Theshold for the ratio of cost per revenue ####
# The number comes from investigating the histogram of the ratio.
# From the histogram, we decided to cut the good/bad ratio at 70%.
hist(dbc_df$total_cost/dbc_df$amount*100, 
     main = "Histogram of Cost per Revenue", 
     xlab = "Cost per revenue (%)",
     breaks = 48)
abline(v = 70, col="red")
THESHOLD_COST = 0.7


## Theshold for the conversion in % ####
# The number comes from investigating the histogram of the conversions
# The histogram is too skewed, we decided to cut the good/bad percetnage at 0%.
hist(dbc_df$trans_conv, 
     main = "Histogram of Conversion", 
     xlab = "Conversion (%)", 
     breaks = 64)
THESHOLD_CONV = 0

max(dbc_df$trans_conv)

## Theshold for the revenue in USD ####
# The revenue of a keyword is considered to be good if it is more than 0.
THESHOLD_REV  = 0


## Theshold for the average price of the ticket sold in USD ####
# The number comes from investigating the share cost (i.e. cost per revenue) in each range of average price.
# Select only working columns
avg_ticket_df <- subset(dbc_df, select = c(amount, total_volumn_of_bookings, total_cost))

# Calculate average price per ticket sold
avg_ticket_df$avg_ticket <- avg_ticket_df$amount/avg_ticket_df$total_volumn_of_bookings

# Remove the missing values column due to 0 numbe rof bookings
avg_ticket_df <- avg_ticket_df[-which(is.na(avg_ticket_df$avg_ticket)),]

# Add a group for average ticket column
for (i in 1:length(avg_ticket_df$avg_ticket)){
  if(avg_ticket_df$avg_ticket[i]<500){avg_ticket_df$avg_ticket_group[i]<-"0-500"}
  else if(avg_ticket_df$avg_ticket[i]<1000){avg_ticket_df$avg_ticket_group[i]<-"500-1k"}
  else if(avg_ticket_df$avg_ticket[i]<1500){avg_ticket_df$avg_ticket_group[i]<-"1k-1.5k"}
  else if(avg_ticket_df$avg_ticket[i]<2000){avg_ticket_df$avg_ticket_group[i]<-"1.5k-2k"}
  else (avg_ticket_df$avg_ticket_group[i]<-">2k")
}

# Summarizing values by average_ticket groups
avg_ticket_df <- avg_ticket_df %>% group_by(avg_ticket_group)
avg_ticket_summary <- avg_ticket_df %>% summarise(
  media_cost = round(sum(total_cost)/1000,digits=0), 
  revenue = round(sum(amount)/1000,digits=0),
  share_cost = round((media_cost/revenue)*100,digits=2))
avg_ticket_summary

# From the share cost of each average price, we decided to cut the good/bad average price at 500 USD.
THESHOLD_AVG_TICKET  = 500



# FUNCTIONS ##############################################################################################################
#' Function to evaluate the ratio of cost per revenue to decide if the ratio is good or bad
#' @param cost (numeric) The cost (in USD) of considering keyword
#' @param revenue (numeric) The revenue (in USD) of considering keyword
#' @return 1 if the cost is good, else 0
evaluate_cost <- function(cost, revenue) {
  
  # If the revenue is 0. The ratio cannot be computed. The divider cannot be 0.
  # Hence, we assume it is bad.
  if (revenue == 0){
    return(0)
  }
  
  # Compute the ratio
  ratio <- cost/revenue
  
  if (ratio <= THESHOLD_COST) {
    return(1)
  }
  else {
    return(0)
  }
}

#' Function to evaluate the conversion to decide if it is good or bad
#' @param conversion (numeric) The conversion (in %) of considering keyword
#' @return 1 if the conversion is good, else 0
evaluate_conversion <- function(conversion) {
  if (conversion <= THESHOLD_CONV) {
    return(1)
  }
  else {
    return(0)
  }
}

#' Function to evaluate the revenue (in USD) to decide if the revenue is good or bad
#' @param revenue (numeric) The revenue (in USD) of considering keyword
#' @return 1 if the revenue is good, else 0
evaluate_revenue <- function(revenue) {
  if (revenue > THESHOLD_REV) {
    return(1)
  }
  else {
    return(0)
  }
}

#' Function to evaluate the average ticket price (in USD) to decide if the average ticket price is good or bad
#' @param revenue (numeric) The revenue (in USD) of considering keyword
#' @param booking_num (numeric) The number of the ticket sold of considering keyword
#' @return 1 if the average price is good, else 0
evaluate_avg_price <- function(revenue, booking_num) {
  
  # If the number of tocket is 0. The ratio cannot be computed. The divider cannot be 0.
  # Hence, we assume it is bad.
  if (booking_num == 0){
    return(0)
  }
  
  # Compute the average price
  avg_price <- revenue/booking_num
  
  if (avg_price > THESHOLD_AVG_TICKET) {
    return(1)
  }
  else {
    return(0)
  }
}



# APPLY THESHOLDS ########################################################################################################
# Evaluate which combination of data for each keyword is good to keep or bad to drop
# In this analysis, the "Total Cost", "Amount", and "Total Volume of Bookings" columns are focused.
for (i in 1:nrow(dbc_df)) {
  
  # Create temporary vector to hold the result of each evaluation
  good_bad <- c()
  
  # Evaluate the cost based on the theshold
  cost <- dbc_df$total_cost[i]
  revenue <- dbc_df$amount[i]
  good_bad <- c(good_bad, evaluate_cost(cost, revenue))
  
  
  # Evaluate conversion based on the theshold
  conversion <- dbc_df$trans_conv[i]
  good_bad <- c(good_bad, evaluate_conversion(conversion))
  
  
  # Evaluate amount (revenue) based on the theshold
  good_bad <- c(good_bad, evaluate_revenue(revenue))
  
  
  # Evaluate average ticket
  booking_num <- dbc_df$total_volumn_of_bookings[i]
  good_bad <- c(good_bad, evaluate_avg_price(revenue, booking_num))
  
  
  # Compute the result of every evaluation result and assign to new column
  dbc_df$good_bad[i] <- round(mean(good_bad))
}

# From the specified criteria, we got:
cat("Of all the", length(dbc_df$good_bad), 
    "keywords, only", 
    sum(dbc_df$good_bad),
    "keywords is good,\n")
cat("which is considered as", 
    round(mean(dbc_df$good_bad) * 100, 2), 
    "% of all keywords.\n")



# APPLY LOGISTIC REGRESSION MODEL ########################################################################################
# Separate dataset to train ans test datasets by stratified sampling
train_index <- sample.split(dbc_df$good_bad, SplitRatio = 0.6)
train_df <- dbc_df[train_index, ]
test_df <- dbc_df[!train_index, ]

# Create logistic regression model
dbc_model <- glm(good_bad ~ amount + total_cost + total_volumn_of_bookings + trans_conv,
                 data = dbc_df,
                 family = "binomial")

# Investigate the model
cat("The built logistic regression model is:\n")
summary(dbc_model)

# Convert coffeicient to probability
coef <- round((exp(dbc_model$coefficients) - 1)*100, 2)
coef_names <- names(coef)
cat("Base on the coefficient of the model, \n")
for (i in 2:length(coef)) {
  cat("  Every 1 increase in", coef_names[i], ", the odd ratio changes by", -coef[i], "%\n")
}

# Test the model with test datset
test_df$response <- predict(dbc_model, test_df, type="response")

# Evaluate the tets result
test_df$correct <- ifelse(test_df$good_bad == round(test_df$response), 1, 0)

# Show the percentage of test correctness
cat("The model correctly predict the good and bad keywords by", round(mean(test_df$correct)*100, 2), "%.\n")



# ANSWER QUESTIONS #######################################################################################################
# Q1: Should Media Contacts recommend a uniform strategy for Air France across search engine publishers? 
#     Or would it be more effective to tailor each publisher strategy to maximize return on investment? 
##########################################################################################################
# Select only keywords that is good
good_df <- subset(dbc_df, good_bad == 1)

# Count the good keywords for each publisher and keyword group
good_group_summary <- good_df %>% 
  group_by(publisher_name, keyword_group) %>% 
  count(keyword_group)
View(good_group_summary)

# Plot graph of Goolgle/MSN Global
gg_msn_global <- subset(good_group_summary, 
                        publisher_name == "Google - Global" | publisher_name == "MSN - Global")
gg_msn_plot <- ggplot(gg_msn_global, 
                      aes(x=keyword_group, y=n, fill=publisher_name))
gg_msn_plot <- 
  gg_msn_plot + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Keywords Group", 
       y = "Number of keywords", 
       title = "Good keywords by keyword group of Google Global & MSN Global")
gg_msn_plot

# From the above table and plot, the good keywords of each publisher are different.
# Hence, we conclude that the Air France should tailor each publisher strategy instead of applying
# uniform strategy.


# Q2: How can campaigns be improved to increase overall value gained from investment with a search engine 
#     publisher? Should keywords be added or dropped from the campaign? Should campaign tactics or copy 
#     be adjusted to improve campaign performance?
##########################################################################################################
# From the APPLY THESHOLDS section, we could improve the overall gained from investment by reducing the
# cost on all bad keywords. We suggest dropping all the bad keywords in order to reduce the cost so that 
# the company would have an additional budget to invest.


# Q3: What are the most important KPIs, and what impact will campaign changes have on these KPIs?
##########################################################################################################
# From the regression model, the important KPI are revenue (i.e. amount column), cost, and booking number.
# Surprisingly, the conversion doesn't has a high significant in the model, meaning that it doesn't 
# have a significant relationship with how good or bad keyword is.
# When some keywords is dropped, it would reduce the overall cost and also slightly reduce the 
# overall revenue.

# Select only keywords that is bad
bad_df <- subset(dbc_df, good_bad == 0)

# Sum the reduced cost
cat("Cost before change:", sum(dbc_df$total_cost), "\n")
cat("Cost after change :", sum(good_df$total_cost), "\n")
cat("Reduced cost      :", sum(bad_df$total_cost), "\n")

# Sum the reduced revenue
cat("Revenue before change:", sum(dbc_df$amount), "\n")
cat("Revenue after change :", sum(good_df$amount), "\n")
cat("Reduced revenue      :", sum(bad_df$amount), "\n")

# Net different
cat("Net difference:", sum(bad_df$total_cost) - sum(bad_df$amount), "\n")


# Q4: How should future SEM campaigns be structured? In the past, Media Contacts had concentrated on 
#     Google, Microsoft, and Yahoo; was there now an opportunity to optimize search advertising with 
#     metasearch companies such as Kayak?
##########################################################################################################
# Try to aggregating Kayak's results into Double Click's dataset
# Import kayak data
kayak_df <- read_excel(file_path, sheet = "Kayak")

# Clean dataframe
kayak_col_names <- kayak_df[2, ]
kayak_df <- kayak_df[3, ]
names(kayak_df) <- c("name", "clicks", "media_cost", "total_booking", "avg_ticket", "total_rev", "net_rev")

# Combine Double Click data with Kayak
dbc_ky <- rbind(dbc_df, 
                c("", kayak_df$name, "","","","","","","","","",
                  kayak_df$clicks,0,0,0,0,0,0,0, kayak_df$total_rev, 
                  kayak_df$media_cost, kayak_df$total_booking, 1))

# Convert number to numeric
dbc_ky$clicks <- as.numeric(dbc_ky$clicks)
dbc_ky$total_cost <- as.numeric(dbc_ky$total_cost)
dbc_ky$total_volumn_of_bookings <- as.numeric(dbc_ky$total_volumn_of_bookings)
dbc_ky$amount <- as.numeric(dbc_ky$amount)

# Sum the data for each publisher 
summary <- dbc_ky %>% 
  group_by(publisher_name) %>% 
  summarise(click = sum(clicks), 
            booking = sum(total_volumn_of_bookings), 
            media_cost = round(sum(total_cost)/1000, digits=0),
            revenue = round(sum(amount)/1000, digits=0),
            share_cost = round((media_cost/revenue)*100, digits=2),
            conversion = round((booking/click)*100, digits=2),
            roa = round((revenue/media_cost), digits=2))

# Plot the bubble plot for ROA and Conversion. The size is revenue.
summary <- summary %>% arrange(revenue)
summary$publisher_name <- as.vector(summary$publisher_name)
summary$publisher_name <- factor(summary$publisher_name, summary$publisher_name)
bubble_plot <- 
  ggplot(summary, aes(x = roa, y = conversion, size = revenue)) + 
  geom_point(alpha=0.75, color="red") +  
  geom_text_repel(aes(label = summary$publisher_name), size = 4, direction = "both") + 
  labs(x = "Return on Assets (USD)", 
       y = "Conversions (% of bookings per click)", 
       title = "Conversion and Revenue on Search Engine (June, 2007)", 
       size = "Revenue (K USD)")
bubble_plot 

# Base on the bubble graph above, Kayak has a highest ROA and highest conversion comparing to the other 
# search engine. Even though its revenue is lower comparing to the Google and Yahoo, we could say that 
# if we invest more on Kayak, it has a potential to give the company more revenue. 



# ADDITIONAL PLOT ########################################################################################
# Renevue plot per search engine + media cost
revenue_plot <- ggplot(summary, 
                       aes(fill=summary$share_cost, x=summary$publisher_name, y=summary$revenue))
revenue_plot <- revenue_plot + 
  scale_fill_gradient(low = "green", high = "red") +
  geom_bar(position="stack", stat="identity")+
  labs(x = "", 
       y = "Revenue (K USD)", 
       fill ="Media Cost %",
       title = "Total revenue by search engine considering media cost (%)")
revenue_plot <- revenue_plot + coord_flip()
revenue_plot

# This plot is good to mix revenue and media cost (%). Here we can visualize the more expensive 
# publishers and its importance for total revenue.
# In Google's case, considering its importance for total revenue, our recommendation is to review keywords, 
# dropping those with higher media cost (%).
# For Overture, on the other hand, we recommend a reduction on the overrall investment, saving some 
# important money that can be reallocated on other publishers.
