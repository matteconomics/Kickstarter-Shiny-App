#*****************************************************
# Advanced R Shiny Project 
#   Cleaning, Exploring, and Modeling Data
#
#     By: Matthew Lopez
# 
#*****************************************************


#Importing Libraries  --------------------------------------------------------------------------------
library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyselect)
library(countrycode)
library(rworldmap)
library(gganimate)
library(plotly) # plotly may interfere with dplyr, only enable once time to plot
library(leaflet)
library(maps)
library(ggmap)
library(tigris)
library(sf)
library(geojsonio)

# turning off scientific notation 
options(scipen = 5)

# Reading in Data
ks_2018 <- read_csv("Data/ks-projects-201801.csv")


# Basic stats to analyze data
head(ks_2018)
str(ks_2018)
colnames(ks_2018)
table(ks_2018$category)
table(ks_2018$main_category)
table(ks_2018$currency)
table(ks_2018$country)
table(ks_2018$state)
# May want to remove undefined and live kickstarters

# Looking at the odd countries that did not have data
odd_countries <- ks_2018[ks_2018$country == 'N,0"',]
head(odd_countries)

# Currencies appear to be standard across data, look at the currencies we have in odd countries
table(odd_countries$currency)
#table(odd_countries$year)

# Using the currency data we have, can add the country. Creating a country key for currencies stored:
odd_currencies <- data.frame(Currency = c("AUD", "CAD", "CHF", "DKK", "EUR", "GBP", "NOK", "NZD", "SEK", "USD"),
                             Country_2 = c("Australia", "Canada", "Liechtenstein/Switzerland", "Denmark", "EURO", "United Kingdom", "Norway", "New Zealand", "Sweden", "United States")
                             )
odd_currencies$Country_2 <- as.character(odd_currencies$Country_2)

# Merge odd currencies with original data to fill in invalid country
ks_merge <- merge(ks_2018,odd_currencies,by.x = "currency", by.y= "Currency", all = TRUE)

# Looking at a few cases where the counry is missing
which(ks_merge$country == 'N,0"')
# Looking at rows that have EURO in the added column
which(ks_merge$country == 'N,0"' & ks_merge$Country_2 == "EURO")
euro_ex <- 24843

ks_merge[138,]

# Loop to replace each missing country with the country associated with it's currency
for(i in 1:nrow(ks_merge)){
  if(ks_merge$country[i] == 'N,0"'){
    ks_merge$country[i] <- ks_merge$Country_2[i]
  }
  
}

# Fill in Country Abbreviation with Country Name
ks_merge$country <- ifelse(ks_merge$country != "EURO",
                           countrycode(ks_merge$country, "iso2c", "country.name"),
                           ks_merge$country)

# Comparing the converted contries to original abbreviations
table(ks_merge$country)
table(ks_2018$country)

# Comparing the number of different country categories, the number should be the same
length(table(ks_2018$country))
length(table(ks_merge$country))


# Convert launched variable to date format
ks_merge$launched <- as.Date(ks_merge$launched)
# Calculate the number of days difference from beginning to deadline for kickstarter
ks_merge$days <- as.numeric(ks_merge$deadline - ks_merge$launched)
# Extract the year variable
ks_merge$year <- as.numeric(substr(ks_merge$deadline,1,4))

# Creating dollar per backer variable
ks_merge$Dollar_per_backer <- ks_merge$usd_pledged_real / ks_merge$backers

missing <- ks_merge[is.na(ks_merge$Dollar_per_backer),]

# Creating pledge ratio
ks_merge$pledge_ratio <- ks_merge$usd_pledged_real / ks_merge$usd_goal_real


# Fixing undefined state ----------------------------------------------------------------
# Miscallaneous Kickstarters "undefined" state
undefined_kicks <- ks_merge[ks_merge$state == "undefined",]
range(undefined_kicks$pledge_ratio)
summary(undefined_kicks$pledge_ratio)

# number of undefined kickstarters that raised enough to be funded
sum(undefined_kicks$pledge_ratio > 1) #number is 1582 kickstarters
#Almost half of undefined kickstarters reached pledge goal


# Visualizations for identifying outliers----------------------------------------------------------------------------------
hist(ks_merge$goal, breaks = 10,000)
hist(ks_merge$usd_goal_real)
hist(ks_merge$usd_goal_real,breaks = 10000, xlim = c(0,500000))

# Check real goal data, appears to be outliers based on historgram
summary(ks_merge$usd_goal_real)

# Determine where most of the data lies, identify outliers
quantile(ks_merge$usd_goal_real,
         probs = c(0.0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))

# Visualizing data without accounting for outliers, only for vizualization purposes 
hist(ks_merge$usd_goal_real[ks_merge$usd_goal_real < 100000],
     breaks = 200)

# Analyze the outliers, Looking at only the top 1% of kickstarters goals
hist(ks_2018$usd_goal_real[ks_2018$usd_goal_real > 100000],
     breaks= 200)

# Will address these extreme values later, continue analyzing other variables for outliers

# Distribution of days between launch and deadline date
hist(ks_merge$days)

# Again there appears to be some outliers, look at the distribution more closely
quantile(ks_merge$days,
         probs = c(0.0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))

# Graphing under 100 days, should represent more than 99% of the data
hist(ks_merge$days[ks_merge$days < 100],
     breaks = 50)

# Looking at the top 1% of day values in isolation
greater_70_days <- ks_merge[ks_merge$days > 70,]

# Can see that there was an error with a few of the dates. Some launch dates are missing. Correct the improper dates
old_kicks <- which(as.numeric(substr(ks_merge$launched,1,4)) < 2009) # storing the rows where dates are incorrect in a variable
# Replacing the incorrect days variable with the median value of the days for all kickstarters
ks_merge$days[old_kicks] <- median(ks_merge$days[-old_kicks])
# Ensuring proper replacement in the old kicks rows
ks_merge$days[old_kicks]
# Plotting actual distribution of days with corrected values
hist(ks_merge$days) # Better representation of the days variables



# Copying Merged Data --------------------------------------------------
ks_copy <- ks_merge #copying data

# Summary of all final variables in the merged data
summary(ks_copy)

# Dollar per backer has some infinite values. Correct these before moving on
inf_rows <- is.infinite(ks_copy$Dollar_per_backer) # identifying infinit rows
ks_with_inf <- ks_copy[inf_rows,] # subsetting only the infinite rows
# Based on the subset, infinite values are caused by missing backer values

# Isolate the data without infinite rows
excluding_inf_rows <- ks_copy[is.finite(ks_copy$Dollar_per_backer),]
# Calculate mean donor number and replace previous zeros with mean donor
mean_donor <- mean(excluding_inf_rows$backers)
mean_donor
median_donor <- median(excluding_inf_rows$backers)
median_donor

ks_copy$Dollar_per_backer[inf_rows] <- median_donor
summary(ks_copy$Dollar_per_backer)

# Infinite values were fixed, NA's represent kickstarters where there were 0 donors
head(ks_copy[is.na(ks_copy$Dollar_per_backer),],10)


# Analyzing the dollar per backer variable
hist(ks_copy$Dollar_per_backer)

# Calculate number of kickstarters with NA's and 0 backers
#
#
#

# Addressing dollar goal outliers -----------------------------------------------
# Boxplot of dollar goal
boxplot(ks_copy$usd_goal_real)


# Some observations have high dollar goals. Take a closer look at full data, subsetting
high_kicks <- ks_copy[ks_copy$usd_goal_real > 100000,]
summary(high_kicks$usd_goal_real)
quantile(high_kicks$usd_goal_real,
         probs = c(0.25,0.5,0.75,0.9))
sum(high_kicks$usd_goal_real > 400000)

# There are only about 3000 kickstarters with a goal higher than $400,000
# It's possible that these are not important
# However, might want to keep a few that had backers and raised some amount of money.
hist(high_kicks$usd_goal_real)

high_kicks_sample <-
  high_kicks %>%
  arrange(desc(usd_goal_real)) %>%
  head(.,20)

# Looking at a few observations and more summary statistics, most of the observations above $100,000 goal
# are still below $400,000. The larger goals appear to be joke kickstarters or intentionally high goals without true intent to raise money.
# For analysis purposes we will focus on kickstarters that are below $400,000 and also select certain kickstarters to keep


# Saving cleaned data and creating a new copy for analysis-------------------------------------------------------------
# Saving cleaned dataset to repository
write_csv(ks_copy,"Output_Data/Kicks_Cleaned.csv")
hist(ks_copy$days)

# Number of Kickstarters Ever
nrow(ks_copy)

# ks_copy is mostly cleaned, however there are a few things we do not need
# Will delete data from 2018 as the kickstarters were not completed at time data was gethered
# also, delete and live and undefined kickstarters. These are very few but cause some issue with visualizaitons later

# Counting how many of these observations there are
nrow(ks_copy[ks_copy$year == 2018,])
nrow(ks_copy[ks_copy$state == "live",])
nrow(ks_copy[ks_copy$state == "undefined",])
# there are 9633 by adding each of these, some may overlap. Just for reference

# Filtering Data
Kicks <- ks_copy %>%
  filter(year != 2018 & state != "live" & state != "undefined")

# Creating Summaries --------------------------------------------------------
# Number of kickstarters per year
kickstarters_per_year <-
Kicks %>%
  group_by(year) %>%
  summarise(n = n())

# Number of successful kickstarters all years
kickstarter_status_all_years <-
  Kicks %>%
  group_by(state) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Summary of different kickstarter statuses per year
kickstarter_status_per_year <- 
  Kicks %>%
  group_by(year,state) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# Success Rate Per year
kickstarter_success_per_year <- kickstarter_status_per_year %>%
  mutate(state = replace(state, state != "successful", "not_successful")) %>%
  group_by(year, state) %>%
  summarize(across(.cols = c(n, freq),  sum)) %>%
  mutate(label_ypos = cumsum(n))

# aggregate(n ~ state + year, data = kickstarter_status_per_year, FUN = sum) %>%
#   mutate(label_ypos = cumsum(n),
#          freq = n / sum(n))

# Creating some Visualizations ---------------------------------
# Barplot for number of successful and total kickstarters per year
a <- ggplot(kickstarter_success_per_year,
            aes(x= year, y = n, fill = state))
a_chart <- 
  ggplotly(a + geom_bar(stat = "identity") + 
  geom_text(aes(label = n), position = "stack") +
  scale_fill_discrete(name = "Outcome", labels = c("Not Successful" , "Successful")) +
  theme_classic() +
  ggtitle("Number of Kickstarters Per Year") + 
  xlab("Year") +
  ylab("# of Kickstarters"))

a_chart[['x']][['data']][[1]][['name']] <- 'Not Successful'
a_chart[['x']][['data']][[2]][['name']] <- 'Successful'
a_chart

# Creating line plot for visualizing data from 2009 to 2017 by state

b <- ggplot(kickstarter_status_per_year, aes(x = year, y = freq, colour = state))

b_chart <- ggplotly(
  b + 
  geom_line() +
  theme_classic() + 
  scale_fill_brewer(palette = "Set1") + 
  scale_colour_discrete(name = "Outcome", labels = c("Canceled", "Failed", "Live" , "Successful", "Suspended", "Undefined")) + 
  ggtitle("Comparing Different Kickstarter Outcomes") +
  xlab("Year") +
  ylab("Percentage of total")
)

b_chart[['x']][['data']][[1]][['name']] <- 'Canceled'
b_chart[['x']][['data']][[2]][['name']] <- 'Failed'
b_chart[['x']][['data']][[3]][['name']] <- 'Sucessful'
b_chart[['x']][['data']][[4]][['name']] <- 'Suspended'

b_chart


# Testing plotly graph
plot_ly(x = kickstarter_status_per_year$year,
        y = kickstarter_status_per_year$freq,
        color = kickstarter_status_per_year$state,
        type = 'scatter',
        mode = 'lines')

# More summaries ---------------------------------
# Plots for number of backers
total_backers <-
Kicks %>%
  group_by(year) %>%
  summarize(num_backers = n())
sum(total_backers$num_backers)

plot(total_backers$year, total_backers$num_backers)

# Summary of category backers, Total
category_backers <- 
  Kicks %>%
  group_by(main_category) %>%
  summarize(num_backers = n())

# Summary of category backers, Annually
category_backers_year <- 
  Kicks %>%
  group_by(main_category, year) %>%
  summarize(num_backers = n())

ggplotly(
ggplot(data = category_backers_year, mapping = aes(x = year, y = num_backers, color = main_category)) +
  geom_line() + 
  #geom_line(data = total_backers, mapping = aes(x = year, y = num_backers, color = "Black")) +
  theme_classic() + 
  scale_x_continuous(breaks = seq(min(category_backers_year$year), max(category_backers_year$year), by = 1)) +
  ggtitle(label = "Number of Backers Each Year") +
  xlab("Year") + 
  ylab("# of Backers") #+
  # sec_axis(dup_axis(
  #   breaks = category_backers_year$num_backers,
  #   labels = category_backers_year$main_category,
  #   name = NULL
  #   )
  # )
)


# Total Money Raised Each Year, only successful kickstarters recieve funding
money_raised <- 
  Kicks %>%
  filter(state == "successful") %>%
  group_by(year) %>%
  summarize(money_raised = sum(usd_pledged_real))

money_raised %>%
  ggplot(mapping = aes(x = year, y = money_raised)) + 
  geom_line()


# Success rate by categories
category_success <-
  ks_copy %>%
  group_by(state,main_category) %>%
  summarise(n = n(),
            average_goal = mean(usd_goal_real),
            median_goal = median(usd_goal_real)) %>%
  mutate(freq = n / sum(n))

category_success2 <-
  category_success %>%
  group_by(main_category) %>%
  dplyr::summarise(total_category = sum(n))

category_success2 <- merge(category_success,category_success2, by = "main_category")
category_success2$percent_of_category <- category_success2$n / category_success2$total_category
#category_success2[category_success2$state == "successful",]
category_success <- category_success[category_success$state == "successful",]
category_success <- category_success[order(category_success$freq, decreasing = TRUE),]

# Plotting success rate by each category
barplot(category_success$freq,
        names.arg = category_success$main_category,
        horiz = TRUE,
        xlab = "category",
        ylab = "Success Rate")


c <- ggplot(category_success, aes(x = reorder(main_category,freq), y = freq, fill = main_category))
c + 
  geom_bar(stat = "identity") +
  theme_classic() +
  coord_flip() +
  ggtitle("Success Rate by Category") +
  xlab("Category") +
  ylab("% of Successful Kickstarters") 

d <- ggplot(category_success2[category_success2$state == "successful",], aes(x = reorder(main_category,-percent_of_category), y = percent_of_category, fill = main_category))
d + 
  geom_bar(stat = "identity") +
  ggtitle("Percent of Successful Kickstarters in each Category") +
  xlab("Main Category") +
  ylab("% successful out of total") + 
  theme_classic()

# Analyzing relationship between number of kickstarters in a category and success rate ------------------
state_number_categories <-
  ks_copy %>%
  dplyr::select(state,main_category) %>%
  dplyr::group_by(main_category, state) %>%
  summarize(number = n())

number_categories <- 
  ks_copy %>%
  dplyr::group_by(main_category) %>%
  summarize(total = n())

state_number_categories <- merge(state_number_categories, number_categories)
state_number_categories$percent <- state_number_categories$number / state_number_categories$total
state_number_categories <- state_number_categories[state_number_categories$state == "successful",]

plot(state_number_categories$number, state_number_categories$percent)

# Basic correlations 
cor(log(state_number_categories$percent), (state_number_categories$number))
ols <-lm(percent ~ number , data = state_number_categories)
summary(ols)

par(mfrow=c(2,2))
plot(ols)

plot(x = state_number_categories$number, y = state_number_categories$percent)
abline(ols, col = "red")

# There's a few outliers, could be worth identifying them and rerunning model

# Final Summaries -------------------------
# Total money raised each year
total_raised <- 
  ks_copy %>%
  group_by(year,state) %>%
  summarise(total = sum(usd_pledged_real))

total_raised_succ <- total_raised[total_raised$state == "successful",]
total_raised_succ_ever <- sum(total_raised_succ$total)
total_raised_succ_ever

# Summary about pledge ratio and number of backers
pledge_and_backer <- 
  ks_copy %>%
  group_by(state) %>%
  summarise(med_pledge = median(pledge_ratio, na.rm = TRUE),
            mean_pledge = mean(pledge_ratio, na.rm = TRUE),
            med_backers = median(backers, na.rm = TRUE),
            mean_backers = mean(backers, na.rm = TRUE),
            mean_dollar_per_backer = mean(Dollar_per_backer, na.rm = TRUE),
            median_dollar_per_backer = median(Dollar_per_backer, na.rm = TRUE)
            )

# Range of dollar goals by category
kick_goal_summaries <-
ks_copy %>%
  group_by(main_category) %>%
  summarise(min_goal = min(usd_goal_real),
            max_goal = max(usd_goal_real),
            average_goal = mean(usd_goal_real),
            median_goal = median(usd_goal_real)
            )

e <- ggplot(kick_goal_summaries, aes(x = main_category, y = median_goal))
e +
  geom_bar(stat = "identity")

# Basic Plotting of data
# ggplot(ks_2018, aes(x = backers, y = usd_pledged_real)) + 
#   geom_point(shape = ".")





# Creating Map of Data -----------------------------------------------------------

country_coordinates <- read_csv("Data/world_country_and_usa_states_latitude_and_longitude_values.csv")
country_coordinates <- country_coordinates[,2:4]

worldcountry <- geojson_read("Data/custom.geo.json", what = "sp")

country_summary <-
  ks_copy %>%
  group_by(country) %>%
  summarise(number_countries = n())

country_summary <- country_summary[-24,]
table(country_summary$country)
country_summary$country[country_summary$country == "Hong Kong SAR China"] <- "China"

kickstarter_countries <- sp::merge(worldcountry,country_summary, by.x = "name", by.y = "country",all = F)

bins <- c(0,100,500,1000,10000,100000,Inf)

pal <- colorBin(
  palette = "Blues",
  domain = kickstarter_countries$number_countries,
  bins = bins
)



leaflet(kickstarter_countries) %>%
  addTiles() %>%
  addPolygons(
    stroke = FALSE,
    smoothFactor = 0.3, 
    fillOpacity = 1,
    fillColor = ~pal(number_countries))%>%
  addLegend("bottomright", pal= pal, values = ~number_countries,
            title = "Number of Total Kickstarters",
            opacity = 1)





#Testing Different Models --------------------------------
Kicks$successful <- ifelse(Kicks$state == "successful", 1, 0)
head(Kicks)
Kicks$country <- as.factor(Kicks$country)
Kicks$main_category <- as.factor(Kicks$main_category)
Kicks2 <- na.omit(Kicks)
smp_size <- floor(0.8 * nrow(Kicks2))

set.seed(123)
train_ind <- sample(seq_len(nrow(Kicks2)), size = smp_size)
train <- Kicks2[train_ind,]
test <- Kicks2[-train_ind,]

# Logistic Regression
model <- glm(successful ~ backers + main_category + usd_goal_real + country, family=binomial(link='logit'),data=train)
summary(model)
anova(model, test = "Chisq")
test.predicted <- predict(model, newdata = test, type= "response")
table(test$successful, test.predicted > 0.5) #confusion matrix

# Logistic regression too simple. Most likely needs some form of regularization


#Ridge and Lasso Models
library(glmnet)
library(caret)

# Preparing data for ridge/lasso
x_train <- model.matrix(successful ~ backers + main_category + usd_goal_real + country, train)[,-1]
head(x_train)
y_train <- train$successful
x_test <- model.matrix(successful ~ backers + main_category + usd_goal_real + country, test)[,-1]
y_test <- test$successful

# variable to use for value of lambda
grid <- 10^seq(10, -2, length = 100)

# Ridge------------------------------------------------------------------------------------------------------
fit.ridge <- glmnet(x = x_train, y = y_train, family = "binomial", alpha = 0, lambda = grid)
cv.out.ridge <- cv.glmnet(x_train, y_train, alpha=0)
plot(cv.out.ridge)
bestlam.ridge <- cv.out$lambda.min
ridge.pred <- predict(fit.ridge ,s=bestlam.ridge, newx = x_test, type = "response")
table(test$successful, ridge.pred > 0.5)


#Lasso-----------------------------------------------------------------------------------------------------
fit.lasso <- glmnet(x = x_train, y = y_train, family = "binomial", alpha = 1, lambda = grid)
plot(fit.lasso)

# cross-validation will take a few minutes. 3 folds are selected for speed
cv.out.lasso <- cv.glmnet(x_train, y_train, alpha=1, family = "binomial", type.measure = "class", nfolds = 3)
plot(cv.out.lasso)
bestlam.lasso <- cv.out.lasso$lambda.min
lasso.coef <- predict(cv.out.lasso, type = "coefficients", s = bestlam.lasso)[1:20,]
lasso.coef
lasso.pred <- predict(fit.lasso ,s=bestlam.lasso, newx = x_test, type = "response")
table(test$successful, lasso.pred > 0.4)

# Roc curve of lasso
plot(roc.glmnet(cv.out.lasso, newx = x_test, newy=y_test))


####################### Testing code that will allow user input in Shiny App #########################-----------------------------------------------------------------------

user_choice <- x_test[1,, drop = FALSE] #selecting one row to be used for user input, simply need to change values as given

# Variables that store user choices
text_cat <- "Comics"
text_country <- "Australia"
backer_number <- 100
usd_number <- 1000

# Changing variables based on user input, matrix can thene be used for prediction.
category_index <- grepl(paste0(text_cat), colnames(user_choice))
country_index <- grepl(paste0(text_country), colnames(user_choice))
user_choice[category_index] <- 1
user_choice[country_index] <- 1
user_choice[,"backers"] <- backer_number
user_choice[,"usd_goal_real"] <- usd_number

# Prediction to make based on user input
choice.predict <- predict(fit.lasso, s = bestlam, newx = user_choice, type = "response")
choice.predict


# Decision tree
library(rpart)
library(rpart.plot)

tree <- rpart(successful ~ backers + main_category + usd_goal_real + country, data = train, method = 'class')
rpart.plot(tree, extra = 106)

tree_predict <- predict(tree, test, type = "class")

table(test$successful, tree_predict)
########### End of work for Shiny App so far ################################################ --------------------------------------------------

# Testing visualizing data by taking samples ---------------------------------------------------
barplot(table(ks_2018$main_category))

# Correlation Matrix
ks_2018_copy %>%
  select(where(is.numeric)) %>%
  #as.matrix() %>%
  cor() %>%
  corrplot(type= "upper")


# Take a small sample of data for easier visualization
set.seed(1)
small_sample <- sample.int(n = nrow(ks_2018), size = 1000, replace = F)
train <- ks_2018_copy[small_sample,]


ggplot(data = train,
       mapping = aes(x = usd_goal_real)) +
  geom_histogram(alpha = 0.5, bins = 10)


# Scatterplot of numeric variables
train %>%
  select(where(is.numeric)) %>%
  pairs()

# Density plot of 10,000 sample from data
ggplot(data = train[train$usd_pledged_real < 100000,],
       aes(x = backers, y = usd_pledged_real)) + 
  geom_point(alpha = 0.01) +
  geom_density2d() + 
  theme_bw()










