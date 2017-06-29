# ---------------------------------- Opex Analytics Case Study ------------------------------------
#
# Submitted by: Arjun Mishra
# Submitted on: 

# -------------------------------- Preliminary Data Analysis -------------------------------------

rm(list = ls())
setwd("G:/Data Science/R/Opex_Analytics_Test")
# Reading in required packages
lapply(c("ggplot2", "lubridate", "caret", "dplyr", 
         "data.table", "dummies", "rpart", "xgboost"), require, character.only = TRUE)

# Reading in the datasets
product_detail = fread("product_details.csv")
  
inventory = fread("eod_inventory.csv")

product_move = fread("product_movement.csv")

# Taking a look at the datasets
summary(product_detail)
str(product_detail)

length(unique(product_detail$subsegment))
# There are only 2 subsegments

length(unique(product_detail$group))
# There are 13 unique groups

length(unique(product_detail$material))

# This dataset just contains the details of the products and needs to be joined
# to the other datasets.

summary(inventory)
str(inventory)

# Date needs to be converted to a date variable. The variation of the stock units
# is huge. While the median is 5118, the max value if 10623168 - could be bad data.

summary(product_move)
str(product_move)

length(unique(product_move$move_type))
# There are only 3 different move types.

# The total units in this dataset also has the same case as the stock units. The move
# type is an important variable.

# There are no missing values in the data.


# Joining the product detail data to the other two individually
inventory = left_join(inventory, product_detail, by = c("location", "material"))

product_move = left_join(product_move, product_detail, by = c("location", "material"))


# It is given that the data contains only one plant location - thus variable is not needed
inventory$location = NULL
product_move$location = NULL

# Now, making the conversions needed
inventory$material = as.character(inventory$material)

inventory$date = as.Date(inventory$date, "%m/%d/%Y")
str(inventory)


product_move$material = as.character(product_move$material)

product_move$date = as.Date(product_move$date, "%m/%d/%Y")
str(product_move)

# Creating some time variables to use in exploratory analysis and as features in models
inventory$weekday = weekdays(inventory$date) # Sunday is 1
inventory$month = month(inventory$date)
inventory$monthday = mday(inventory$date)
inventory$short_date = format(inventory$date, "%b-%Y") # Will use for plotting

# Making the week number variable
inventory$week[inventory$monthday < 9] = 1 # Giving 8 days to first week and other extra days to last
inventory$week[inventory$monthday > 8 & inventory$monthday < 16] = 2
inventory$week[inventory$monthday > 15 & inventory$monthday < 23] = 3
inventory$week[inventory$monthday > 22] = 4

#Making the season variable - if this is CPG, seasonality is bound to be present
inventory$season[inventory$month %in% c(12, 1, 2)] = "Winter"
inventory$season[inventory$month %in% c(3, 4, 5)] = "Spring"
inventory$season[inventory$month %in% c(6, 7, 8)] = "Summer"
inventory$season[inventory$month %in% c(9, 10, 11)] = "Fall"


# Same for product movement
product_move$weekday = weekdays(product_move$date) # Sunday is 1
product_move$month = month(product_move$date)
product_move$monthday = mday(product_move$date)
product_move$short_date = format(product_move$date, "%b-%Y") # Will use for plotting

product_move$week[product_move$monthday < 9] = 1 # Giving 8 days to first week and other extra days to last
product_move$week[product_move$monthday > 8 & product_move$monthday < 16] = 2
product_move$week[product_move$monthday > 15 & product_move$monthday < 23] = 3
product_move$week[product_move$monthday > 22] = 4

product_move$season[product_move$month %in% c(12, 1, 2)] = "Winter"
product_move$season[product_move$month %in% c(3, 4, 5)] = "Spring"
product_move$season[product_move$month %in% c(6, 7, 8)] = "Summer"
product_move$season[product_move$month %in% c(9, 10, 11)] = "Fall"


# To save some space, we can create a dummy variable from the 2 value subsegment
inventory$writing_utensils[inventory$subsegment == "Writing Utensils"] = 1
inventory$writing_utensils[is.na(inventory$writing_utensils)] = 0 # Fancy Writing Utensils
inventory$subsegment = NULL

product_move$writing_utensils[product_move$subsegment == "Writing Utensils"] = 1
product_move$writing_utensils[is.na(product_move$writing_utensils)] = 0 # Fancy Writing Utensils
product_move$subsegment = NULL

# Creating the stockout boolean variable
inventory$stockout = 0
inventory$stockout[inventory$stock_units == 0] = 1


# Finally we can also create a merged dataset of these two datasets

# As the stockout variable is in the inventory dataset, we need to take inventory
# as our main dataset

# Also, we can check the number of dates of either dataset present in the other.
nrow(inventory) - sum(inventory$date %in% product_move$date) # 643 number of rows with no match

nrow(product_move) - sum(product_move$date %in% inventory$date) # 398 number of rows with no match

# Thus, this also gives us that there are less number of product movement dates
# that are not in the inventory dataset, rather than the other way around

final_data = left_join(inventory, product_move[ , c(1, 2, 3, 4, 5)], 
                       by = c("material", "date", "group"))

# There are some move_type and total_units fields with NAs. Means on those dates
# there was no movement of product but inventory was recorded.

final_data$move_type[is.na(final_data$move_type)] = "No Movement"

final_data$total_units[is.na(final_data$total_units)] = 0

summary(final_data) # There are no NA's anymore.


# ---------------------------- Exploratory Analysis ----------------------------------------

# Starting with the product detail graph to get a look at the number of materials in
# the groups and subsegments

# Arranging for bar plot to come in decreasing order
product_detail$group = factor(product_detail$group,
                              levels=names(sort(table(product_detail$group))))

ggplot(product_detail, aes(x = group, fill = subsegment)) + 
  geom_bar() + coord_flip() + labs(title = "Materials count in each Group")

# We can clearly see that there are very few materials in the Fancy Writing Utensils subsegment.
# Also, most of the materials are concentrated in Groups 6, 13 and 9.
# But this does not mean that Writing Utensils are the most sold. The Fancy Writing
# Utensils could have more sale. 

# There is no need of the product detail dataset now.
rm(product_detail)


# Inventory dataset

# In the dataset, the dates where stock units are not listed means there was no
# change in their inventory. Thus, the number of occurences of each material in
# the dataset can be used as a proxy for their popularity.

# Plotting Stock units is not advisable. If we group by material or group etc. and add
# the number of stock units, it would give a wrong idea - if the number of stock units
# change by 1 unit a lot of days and the in stock number is high, the analysis would be wrong.

# As the number of materials are too many, this graph will just give us a shape.
ggplot(data.frame(sort(table(inventory$material), decreasing = TRUE)), 
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") + coord_flip() + 
        labs(x = "Material", y = "Count", title = "Materials by popularity")

# We can also evaluate if there are some popular materials with stockout proportions
# much more than others
mat_prop = inventory %>%
              group_by(material) %>%
              summarize(n = n(), stockout_sum = sum(stockout)) %>%
              mutate(stockout_prop = stockout_sum/n) %>%
              arrange(desc(stockout_prop))

ggplot(mat_prop[mat_prop$n > 500, ][1:30, ], 
       aes(x = reorder(material, stockout_prop), y = stockout_prop)) +
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "Material", y = "Proportion", title = "Materials stockout proportion")
  
# We can see that for the materials with high frequency of stock changes, the 
# maximum stockout proportion is 0.03. It also shows that there is a contantly
# decreasing trend in the proportion.

# Plotting the same graphs in terms of the groups but with stockouts info

inventory$group = factor(inventory$group,
                              levels=names(sort(table(inventory$group))))

ggplot(inventory, 
       aes(x = group, fill = factor(stockout))) + 
  geom_bar() + coord_flip() + 
  labs(x = "Group", y = "Count", title = "Groups by popularity", fill = "Stockout")

# Following this graph, we can plot the proportion of stockouts in each group
group_prop = inventory %>%
              group_by(group, writing_utensils = factor(writing_utensils)) %>%
              summarise(n = n(), stockout_sum = sum(stockout)) %>%
              mutate(stockout_prop = stockout_sum/n) 

ggplot(group_prop, 
       aes(x = reorder(group, stockout_prop), y = stockout_prop, fill = writing_utensils)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "Group", y = "Proportion", title = "Groups stockout proportion")

# We can see that out of the most popular groups, Group 6 has the maximum proportion
# of stockouts.
# Also, while the number of Fancy Writing Utensils cases is small, the three groups
# do have on an average higher proportion of stockout.

# We can also look at the proportion of data points in the Writing utensils and Fancy
# writing utensils groups
# Thus we can confirm whether the distribution in details dataset holds here too.

table(inventory$writing_utensils)/nrow(inventory)
# Writing utensils consist of 92 % of the data


# We can now start plotting with the time variables

range(inventory$date)
# Extends approximately from December 2011 to November 2014

# Starting with an overall view of the number of stockouts over time
# As there are three years, we will plot line trend per year
overall_out = inventory %>%
                mutate(year = year(date)) %>%
                group_by(short_date, month, year) %>%
                summarise(n = sum(stockout))

ggplot(overall_out[overall_out$year == 2012, ], aes(x = month, y = n)) + geom_line() +
  geom_point() + scale_x_continuous(breaks = 1:12) + 
  labs(x = "Month", y = "Count", title = "Number of stockouts per month for 2012")

ggplot(overall_out[overall_out$year == 2013, ], aes(x = month, y = n)) + geom_line() +
  geom_point() + scale_x_continuous(breaks = 1:12) + 
  labs(x = "Month", y = "Count", title = "Number of stockouts per month for 2013")

# We will not include months 11 and 12 for 2014 as data might not be complete
ggplot(overall_out[overall_out$year == 2014 & !overall_out$month %in% c(11, 12), ], 
       aes(x = month, y = n)) + geom_line() +
  geom_point() + scale_x_continuous(breaks = 1:12) + 
  labs(x = "Month", y = "Count", title = "Number of stockouts per month for 2014")

# From these graphs, we can see that the number of stock outs are usually higher in
# the initial months compared to the later part of the year.
# Also, there was a steady decrease in the number of stockouts in 2012. It might be
# of some merit to see what happened there.

# We can confirm the first part by making a season histogram
ggplot(inventory %>% group_by(season) %>% summarise(n = sum(stockout)), 
       aes(x = season, y = n)) + geom_bar(stat = "identity") + 
  labs(x = "Season", y = "Count", title = "Number of Stockouts per season")


# Now we will look at stockouts for other time variables:
ggplot(inventory %>% group_by(weekday) %>% summarise(n = sum(stockout)), 
       aes(x = reorder(weekday, n), y = n)) + geom_bar(stat = "identity") + 
  labs(x = "Weekday", y = "Count", title = "Number of Stockouts by Day of week")

# Ignoring Saturday and Sunday numbers as they are rare, we can see a steady increase
# in the number of stockouts as we progress through the week.

ggplot(inventory %>% group_by(week) %>% summarise(n = sum(stockout)), 
       aes(x = week, y = n)) + geom_bar(stat = "identity") + 
  labs(x = "Week", y = "Count", title = "Number of Stockouts by week of month")

# From this graph, we cannot necessarily say that there is a difference between
# Weeks 1, 2 & 3, but the number of stockouts surely increase in the last week of
# the month.

# At a granular level: 
ggplot(inventory %>% group_by(monthday) %>% summarise(n = sum(stockout)), 
       aes(x = monthday, y = n)) + geom_bar(stat = "identity") + 
  labs(x = "Monthday", y = "Count", title = "Number of Stockouts by Day of month")

# There are peaks at around the 10th and 20th day of the month.


# Product Movement Dataset

# For the product movement dataset, we can look at the sum of the total units
# as this gives us the units moved directly. There is no residual from previous days
# like stock units.

# Looking at the number of total units for each kind of product movement
units_flow = product_move %>% group_by(move_type) %>% summarise(n = sum(total_units))

ggplot(units_flow, 
       aes(x = move_type, y = n)) + geom_bar(stat = "identity") + 
  labs(x = "Move Type", y = "Count", title = "Number of units moved by type of movement")

sum(units_flow$n[2:3]) - units_flow$n[1] # Difference of 177926678

# As is apparent, most of the movement of goods takes place "Internally.
# The number of goods flowing in is not equal to the number of goods flowing out however.

# For the rest of the descriptive analysis, we will divide up the movement type streams.


ggplot(product_move[product_move$move_type == "Goods Receipt", ], 
       aes(x = weekday, y = sum(total_units))) + geom_bar(stat = "identity") + 
  labs(x = "Weekday", y = "Total Units moved", title = "Goods Receipt movement by day of week")

ggplot(product_move[product_move$move_type == "Stock Transfer", ], 
       aes(x = weekday, y = sum(total_units))) + geom_bar(stat = "identity") + 
  labs(x = "Weekday", y = "Total Units moved", title = "Stock Transfer movement by day of week")

ggplot(product_move[product_move$move_type == "Ship to Customer", ], 
       aes(x = weekday, y = sum(total_units))) + geom_bar(stat = "identity") + 
  labs(x = "Weekday", y = "Total Units moved", title = "Ship to Customer movement by day of week")

# From these graphs, we can see that the Goods receipt is high during the early part
# of the week. The stock transfer and ship to customer, however, are higher during the
# the later part of the week. 
# This makes sense as the stocking up takes place first and then the movement takes place.

# By week of month
ggplot(product_move[product_move$move_type == "Goods Receipt", ], 
       aes(x = week, y = sum(total_units))) + geom_bar(stat = "identity") + 
  labs(x = "Week", y = "Total Units moved", title = "Goods Receipt movement by week of month")

ggplot(product_move[product_move$move_type == "Stock Transfer", ], 
       aes(x = week, y = sum(total_units))) + geom_bar(stat = "identity") + 
  labs(x = "Week", y = "Total Units moved", title = "Stock Transfer movement by week of month")

ggplot(product_move[product_move$move_type == "Ship to Customer", ], 
       aes(x = week, y = sum(total_units))) + geom_bar(stat = "identity") + 
  labs(x = "Week", y = "Total Units moved", title = "Ship to Customer movement by week of month")

# The goods receipt and stock transfer have the same trend. However, there is a very
# different trend for units shipped to customer. The units shipped suddenly spike during
# the end of the month in week 4. 

# Finally, we will look at the movement by months
ggplot(product_move[product_move$move_type == "Goods Receipt", ], 
       aes(x = month, y = sum(total_units))) + geom_bar(stat = "identity") + 
  labs(x = "Month", y = "Total Units moved", title = "Goods Receipt movement by Month") + 
  scale_x_continuous(breaks = 1:12)

ggplot(product_move[product_move$move_type == "Stock Transfer", ], 
       aes(x = month, y = sum(total_units))) + geom_bar(stat = "identity") + 
  labs(x = "Month", y = "Total Units moved", title = "Stock Transfer movement by Month") + 
  scale_x_continuous(breaks = 1:12)

ggplot(product_move[product_move$move_type == "Ship to Customer", ], 
       aes(x = month, y = sum(total_units))) + geom_bar(stat = "identity") + 
  labs(x = "Month", y = "Total Units moved", title = "Ship to Customer movement by Month") + 
  scale_x_continuous(breaks = 1:12)

# From these three graphs, we can see that all three movements start the year with a
# climb and peak towards the middle of the year.


# ----------------------- Predictive Analysis ---------------------------------------

# predictins over week, scope reduction, R-shiny, hypothesis test for difference in weeks
# Feature - material movement in last month, popularity in last month

# As we saw from our exploratory analysis, different materials have different
# characteristics in terms of the their popularity, stockout frequency etc. Thus, 
# the material will definitely be an important variable in our prediction.

# However, there are ~1800 different materials. Modeling such a categorical variable
# with so many categories is very hard.

# Thus, we will choose representative groups in which we will make the predictions.
# The group variable also could have been an important variable in our model
# but we need predictions on a material level so we can consider only a few groups.
# We can also consider just those groups in which the stockout proportion is high
# because at the end of the day, that is what we want to predict and reduce.

# Also, we need to group our data on a weekly level as we don't want to make
# predictions on a daily level. That will not be very useful in planning and it will
# also be imprecise.

# Looking at our exploratory analysis, we also saw that the trend for year 2012 was
# very different compared to the other years. It is also a very old time period and
# a lot would have changed since then. If there are enough data points, we can
# remove this year.

# From our exploratory analysis, we see that Group 6 has one of the highest popularity
# , the highest number of materials in it and a high stockout proportion.
# For the Fancy Writing Utensils, Group 2 has the highest popularity and a high
# stockout proportion.
# Group 3 is also a good candidate for Writing Utensils subsegment - it has high
# number of materials, high popularity and high stockout rate.

# ----------------------------- Hypothesis Test ------------------------------

# We are going to model our data to make predictions on a week by week level.
# Thus to see if there is even a difference between the weeks of the month in the
# stockout levels, we should first perform a hypothesis test. It will give us an
# idea about if we should use that as a predictor variable in our analysis or not.

hypo = final_data %>% group_by(week, month, year(date)) %>% 
  summarise(stockout_sum = sum(stockout))

# First plotting a box plot
boxplot(hypo$stockout_sum ~ as.factor(hypo$week), 
        xlab = "Week", ylab = "Stockout count")
title(main = "Stockout Boxplot of the 4  weeks")

# From the box plot, we can see there might be a difference in the weeks

# To validate this, we will perform the ANOVA test
anova_model = aov(hypo$stockout_sum ~ as.factor(hypo$week))
summary(anova_model)

# The p-value is 0.0112. At a 5% significance level, we can say this is significant - 
# not a lot of difference but there is difference.

# To see where the difference lies in the 4 weeks, we can perform the Tukey's pairwise comparison.
pair_comp = TukeyHSD(anova_model, which = 'as.factor(hypo$week)', conf.level = 0.95)
pair_comp

# As suspected, the highest difference comes in week 4-2 and 4-3.


#--------------------------------- Data Model ----------------------------------------

# First we will trim down the dataset and get it in the form we need for modeling
model_data = final_data

# As we are predicting on the weeks, we don't need the dates, weekday or monthday.
model_data$year = year(model_data$date)
model_data = dplyr::select(model_data, -date, -short_date, -weekday, -monthday)

############################# Feature Engineering ================================

# My idea is to predict the availability for the next week so that the manager can
# run the model on the last day of the previous week and see if there will be a 
# stockout in the next week.

# First, we will allocate signs to the total_untis based on move_type.
model_data$total_units[model_data$move_type %in% 
                         c("Stock Transfer", "Ship to Customer")] = 
  (-1) * model_data$total_units[model_data$move_type %in% 
                                  c("Stock Transfer", "Ship to Customer")]

# As I want to see how many of a particular kind of movement took place in a week, I
# will create dummies for the move_type variable

move_type_dummies = data.frame(dummy(model_data$move_type))

dummy_name = c("Goods Receipt", "No Movement", "Ship to Customer", "Stock Transfer")
colnames(move_type_dummies) = dummy_name

# Insert into main dataset
model_data = cbind(model_data, move_type_dummies)

model_data$move_type = NULL

# Also, we will create variables which give us how much movement of each kind took place.
move_agg = model_data$total_units * move_type_dummies
colnames(move_agg) = c("goods_receipt", "no_movement", "ship_to_customer", "stock_transfer")

# Insert into main dataset without the no movement numbers as its always 0
model_data = cbind(model_data, move_agg[, c(1, 3, 4)])

# Another metric we could use as information is if any of the units are greater
# than the average for that season
avg_data = model_data %>% group_by(material, year, season) %>%
  summarise(av_stock = mean(stock_units), av_total = mean(total_units), 
            av_goods_rec = mean(goods_receipt), av_ship = mean(ship_to_customer),
            av_transfer = mean(stock_transfer))

model_data = left_join(model_data, avg_data, by = c("material", "year", "season"))

# Aggregating the data
model_data1 = model_data %>% 
  group_by(material, group, month, week, season, writing_utensils, year,
           av_stock, av_total, av_goods_rec, av_ship, av_transfer) %>%
  summarise(last_stock = last(stock_units), stockout_sum = sum(stockout), 
            total_unit_movement = sum(total_units), goods_receipt_count = sum(`Goods Receipt`),
            no_movement_count = sum(`No Movement`), ship_to_customer_count = sum(`Ship to Customer`),
            stock_transfer_count = sum(`Stock Transfer`), goods_receipt_sum = sum(goods_receipt),
            ship_to_customer_sum = sum(ship_to_customer), stock_transfer_sum = sum(stock_transfer))

# Using the average to make variables
model_data1 = model_data1 %>%
  mutate(gr_av_stock = (last_stock > av_stock), 
         gr_av_total = (abs(total_unit_movement) > abs(av_total)),
         gr_av_goods = (goods_receipt_sum > av_goods_rec),
         gr_av_ship = (abs(ship_to_customer_sum) > abs(av_ship)),
         gr_av_transfer = (abs(stock_transfer_sum) > abs(av_transfer)))


# Now as we said, we will predict the stockout for the next week. Thus, in our data
# rows, we need the data for the previous week so that we can make the prediction for
# that week.
# This is sort of like using time series but we will have a lot of features for our
# machine learning model to work and we will also have features for that week like
# season, week number, month etc.

# We could make more features like inventory_increase, ratio_of_inventory_increase etc.
# for which we will need to compute 2 week lag data. This is for future. 


model_data2 = model_data1 %>% arrange(material, year, month, week)
model_data2 = model_data2[ , c(1:7, 13:27)]

# Variables starting with lw mean last week
lag_data = as.data.frame(sapply(model_data2[, c(8:22)], shift, n = 1L, type = "lag"))
colnames(lag_data) = paste("lw_", colnames(lag_data), sep = "")

model_data2 = cbind(model_data2, lag_data)

# Now, the first row of every material will have information from the previous
# material so we have to remove those rows
require("plyr") # Do not load this at the top as it has conflicts with dplyr
del_rows = ddply(model_data2, "material", head, 1)
model_data2 = dplyr::setdiff(model_data2, del_rows)

# Trimming down dataset to required one for prediction
model_data2 = model_data2[c(1:7, 9, 23:37)]

# Making the final dependent variable
model_data2$stockout[model_data2$stockout_sum > 0] = 1
model_data2$stockout = as.factor(model_data2$stockout)
model_data2$stockout_sum = NULL


# Dividing up dataset into test and train - we are only taking the last 10 weeks as
# our test

# Now we are going to reduce the scope to a few groups and only materials with a 
# minimum number of weeks information
scope_check = model_data2 %>% group_by(group, material) %>% 
  dplyr::summarize(n = n()) %>% mutate(greater_100 = (n >100))

# Looking at which groups have how many materials with weeks > 100
group_check = scope_check %>% group_by(group) %>% 
  dplyr::summarise(n = sum(greater_100))

greater_100 = scope_check$material[scope_check$greater_100 == TRUE]

# Groups 2, 3, 4 and 6 have a lot of materials with above 100 weeks. These are also
# representative of writing utensils and fancy writing utensils

# Taking the subset
model_data2 = model_data2[model_data2$group %in% c("Group 2", "Group 3", "Group 4", "Group 6"), ]
model_data2 = model_data2[model_data2$material %in% greater_100, ]

model_data2 = model_data2 %>% arrange(material, year, month, week)
test = ddply(model_data2, "material", tail, 5)

train = dplyr::setdiff(model_data2, test)

# We will build a simple regression tree for this problem. 
# It does a good job of segregating the set on variable boundaries
# Time-allowing, we will move on to more complex models.

# -------------------------- Modeling -------------------------------

tree_model = rpart(stockout ~ . , data = train, method = "class")
require(rpart.plot)
prp(tree_model) # Very bad tree considering the number of options

predict_test = predict(tree_model, newdata = test)
predictions_table = data.table(actual = test$stockout, predicted = predict_test)

# trying with different thresholds, 0.18 is where the accuracy becomes constant.
# Reducing it does not have any further effect:
table(test$stockout, predictions_table$predicted.1 > 0.18)

var_imp = tree_model$variable.importance
var_imp_set = data.frame("Variables" = names(var_imp), "Values" = as.vector(var_imp))

ggplot(var_imp_set, aes(x = reorder(Variables, Values), y = Values)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "Variable", y = "Importance", title = "Variable Importance for tree model")

# With such a biased threshold, we are able to get an accuracy of 0.9287.
# The accuracy is very bad, considering if we predicted all to be no_stockout (0),
# the baseline accuracy would have been 0.959.
# Also the model is not able to predict the True stockout cases very well i.e.
# the precision and sensitivity, both are low.

# ---------------------------------- End -------------------------------------------