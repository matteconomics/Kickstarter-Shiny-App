#*****************************************************
# Advanced R Shiny Project 
#   Shiny App
#
#     By: Matthew Lopez
# 
#*****************************************************

# Importing libraries------------------------------------------------------------------------------------------------------
library(readr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(geojsonio)
library(leaflet)
library(glmnet)

options(scipen = 5) # turning off scientific notation 

# Preparing Data created in Analysis File for use in App -----------------------------------------------------------------------------------
# Reading in Data
Kicks <- read.csv("Output_Data/Kicks_Cleaned.csv") %>%
  filter(year != 2018 & state != "live" & state != "undefined") #excluding certain observations from data

# Converting variables to character
Kicks$country <- as.character(Kicks$country)
Kicks$state <- as.character(Kicks$state)

# Percent of successful kickstarters
sum(Kicks$state == "successful") / nrow(Kicks)


# Creating datasets to be called later---------------------------------------------------------------------------------------------------------
kickstarter_status_per_year <- 
  Kicks %>%
  group_by(year,state) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

total_raised <- 
  Kicks %>%
  group_by(year,state) %>%
  summarise(total = sum(usd_pledged_real))

total_raised_succ <- total_raised[total_raised$state == "successful",]
total_raised_succ_ever <- sum(total_raised_succ$total)

# Creating summaries of category data. Creating 2 summary datasets and then merging them
category_success <-
  Kicks %>%
  group_by(state,main_category) %>%
  summarise(n = n(),
            average_goal = mean(usd_goal_real),
            median_goal = median(usd_goal_real)) %>%
  mutate(freq = n / sum(n))

category_success2 <-
  category_success %>%
  group_by(main_category) %>%
  dplyr::summarise(total_category = sum(n))

# Merging the two summaries
category_success2 <- merge(category_success,category_success2, by = "main_category")
category_success2$percent_of_category <- category_success2$n / category_success2$total_category
category_success <- category_success[category_success$state == "successful",]
category_success <- category_success[order(category_success$freq, decreasing = TRUE),]


# Preparing Data for Leaflet Map --------------------------------------------------------------------------------------------------------
country_coordinates <- read_csv("Data/world_country_and_usa_states_latitude_and_longitude_values.csv") #reading in latitude and longitude for merging with data 
country_coordinates <- country_coordinates[,2:4] 

# Reading in map file
worldcountry <- geojson_read("Data/custom.geo.json", what = "sp")

# Creating summary data for map
country_summary <-
  Kicks %>%
  group_by(country) %>%
  summarise(number_countries = n())

country_summary <- country_summary[-24,]
table(country_summary$country) # verifying country variables.
country_summary$country[country_summary$country == "Hong Kong SAR China"] <- "China" #renaming China

# Merging map with data about country. Storing in a Spatial dataframe
kickstarter_countries <- sp::merge(worldcountry,country_summary, by.x = "name", by.y = "country",all = F)
bins <- c(0,100,500,1000,10000,100000,Inf) # assigning bins for map
# Creating color palette for map
pal <- colorBin(
  palette = "Greens",
  domain = kickstarter_countries$number_countries,
  bins = bins
)

# Preparing Data for running Logistic Regression model in App----------------------------------------------------------------
# Creating binary outcome variable
Kicks$successful <- ifelse(Kicks$state == "successful", 1, 0)
# Storing country and category lists in variable. Can be used for user selection in model
country_list <- unique(as.character(Kicks$country))
category_list <- unique(as.character(Kicks$main_category))
Kicks$country <- as.factor(Kicks$country)
Kicks$main_category <- as.factor(Kicks$main_category)
Kicks2 <- na.omit(Kicks)

# Setting sample size and creating training and testing data
smp_size <- floor(0.8 * nrow(Kicks2))
set.seed(123) # make results reproducible
train_ind <- sample(seq_len(nrow(Kicks2)), size = smp_size)
train <- Kicks2[train_ind,]
test <- Kicks2[-train_ind,]

# Splitting X and Y variables for use in Glmnet
x_train <- model.matrix(successful ~ backers + main_category + usd_goal_real + country, train)[,-1]
y_train <- train$successful
x_test <- model.matrix(successful ~ backers + main_category + usd_goal_real + country, test)[,-1]
y_test <- test$successful

# creating container for lambda values to search
grid <- 10^seq(10, -2, length = 100)

# Lasso model
fit.lasso <- glmnet(x = x_train, y = y_train, family = "binomial", alpha = 1, lambda = grid)
cv.out.lasso <- cv.glmnet(x_train, y_train, alpha=1)
bestlam.lasso <- cv.out.lasso$lambda.min
lasso.coef <- predict(cv.out.lasso, type = "coefficients", s = bestlam.lasso)[1:20,]
lasso.pred <- predict(fit.lasso, s=bestlam.lasso, newx = x_test, type = "response")
table(test$successful, lasso.pred > 0.5) # confusion matrix

# Creating container for user selections
user_choice <- x_test[1,, drop = FALSE] #selecting one row to be used for user input, simply need to change values as given

# Define UI------------------------------------------------------------------------------------------------------------------------------------
ui <- navbarPage("Kickstarter Analysis",
                 
                 ###### Inserting shinydashboard dependencies ######
                 header = tagList(
                   useShinydashboard()
                 ),
                 #######################################################
                 
##################################### Panel 1 #########################################

                 tabPanel("About",
                          # mainPanel(
                          # Title of Page
                          h1(strong("Analyzing and Predicting the Success of Kickstarters"), align = "center"),
                          
                          # Section Heading 1
                          h4(strong("Introduction")),
                          
                              p("Raising money in order to create products has been made easier with the use of crowdfunding. It is no longer necessary to raise capital 
                              from large or wealthier investors. Products can be created by directly raising money from individuals with an interest in the product. Kickstarter is one example
                              of using crowdfunding for this goal."),
                          
                              p("This Shiny App explores the success of crowdfunding by analyzing Kickstarter projects specifically.
                                On this page, you will find a brief description for each tab in this project that will help to make the most of the app. 
                                Each section of the project is dedicated to exploring certain aspects and topics, presenting analysis and summary of Kickstarter data 
                                downloaded from Kaggle."),
                          
                          # Title 
                          h2(strong("Project Section Explanations"), align = "center"),
                          
                          # Section Heading 2  
                          h4(strong("Success of Kickstarters")),
                          
                              p("This contains a short summary of Kickstarter since it's founding in 2009. It explores the growth of kickstarters over time and the success rate of projects. 
                                  Important summary highlights such as total money raised are found here."),
                          
                          # Section Heading 3 
                            h4(strong("Kickstarter Categories")),
                          
                              p("This section explores successful kickstarters in more detail by category. As you will see, some kickstarter categories perform better than others.
                                 Some related analyses are presented and further analysis can be done in 'Explore Data' and 'Predicting Success'. "),
                          
                          # Section Heading 4
                          h4(strong("Map of Kickstarters")),
                          
                              p("This provides an interactive map to see where most kickstarters are based."),
                          
                          # Section Heading 5
                            h4(strong("Explore Data / Interactive Summary")),
                          
                              p("This section is dedicated to allowing the user to explore the data in more detail. You will be able to build on the previous analysis of successful kickstarter projects.
                                 It is possible to create tables summarizing various key variables and explore the data in more detail before utilizing the prediction tool.
                                 The tables are created with user input selecting variables to summarize."),
                          
                          # Section Heading 6
                            h4(strong("Predicting Success")),
                          
                              p("Finally, you can evaluate the chance of success of a kickstarter with certain characteristics. This will provide a chance to tangibly see the difference in sucess across categories.
                                This could be useful to set backer and dollar goals.")
                          ),
                 
############################### Panel 2 ######################################

                 tabPanel("Success of Kickstarters",
                          # Title of Page
                          fluidRow(h2(strong("Brief History from 2009 to 2017"))),
                          
                          # Row Containing Summary Boxes
                          fluidRow(
                            box(
                              title = "Total Amount Raised (US Dollars)", width = 4, background = "green",
                              h4(paste0(dollar(total_raised_succ_ever)), align = "center")
                            ),
                            box(
                              title = "Percentage of Kickstarters that Succeed", width = 4, background = "green",
                              h4(paste0("35%"), align = "center")
                            ),
                            box(
                              title = "Total Number of Kickstarters", width = 4, background = "green",
                              h4(paste0(format(nrow(Kicks), big.mark = ",")), align = "center")
                            )
                          ),
                          
                          # Printing out Visualizations
                          fluidRow(
                            # splitLayout(cellWidths = c("50%", "50%"),
                            plotlyOutput("Total_Hist"),
                            plotlyOutput("Total_Line")
                            # )
                            # plotlyOutput("Backers_Line")
                          )
                 ),
                 
######################################## Panel 3 ##############################                 
                 tabPanel("Kickstarter Categories",
                          # Title of Page
                          h2(strong("Comparing Success Across Categories")),
                          
                          # Visualizations
                          fluidRow(
                            plotlyOutput("Success_Category"),
                            plotlyOutput("Most_Success_Category"),
                            plotlyOutput("Backers_Line")
                          )
                          
                 ),

############################### Panel 4# #################################
                 tabPanel("Map of Kickstarters",
                          # Title
                          h2(strong("Where are all the Kickstarters?")),
                          
                          # Map
                          leafletOutput("KickMap")
                 ),
                 
############################### Panel 5 ################################
                 tabPanel("Explore Data / Interactive Summary",
                          # Title
                          h2(strong("Creating Summary Data")), 
                          
                          # Paragraph instructions
                          p("Instructions: The Group variable organizes the data before calculating summaries. Not selecting a group will calculate the summary across the entire dataset.
                            A few sample choices are pre-selected to provide guidance. Multiple choices can be selected for each input."),
                          sidebarLayout(
                            sidebarPanel(
                              
                              # Dropdown Choices for Summary
                              selectInput(inputId = "group_var",
                                          label = "Group:",
                                          choices = c("main_category", "state", "country", "year"),
                                          multiple = TRUE,
                                          selected = "year"
                                          ),
                              
                              selectInput(inputId = "summary_var",
                                         label = "Variable to Summarize:",
                                         choices = c("backers", "usd_pledged_real", "usd_goal_real"),
                                         multiple = TRUE,
                                         selected = "backers"
                                         ),
                              
                              selectInput(inputId = "summary_stat",
                                          label = "Statistic to Calculate:",
                                          choices = c("mean", "median", "min" , "max"),
                                          multiple = TRUE,
                                          selected = "mean"
                                         )
                              ),
                            
                            # Output of Summary Calculation
                           mainPanel(
                             dataTableOutput("Group_Stats") 
                           ) 
                          )
                          ),

################################ Panel 6 ##################################
                 tabPanel("Predicting Success",
                          # Title
                          h2(strong("Predicting Success of Custom Kickstarters")),
                          
                          # Explanation
                          p("Here you can see what the chances are of a kickstarter succeeding. This would be most helpful for determining the number of backers and goal to reach.
                          The probability output here is calculated using the model developed in R. A probability of 0.2 means that the probability of being successful is 20%. 
                          In practice, a probability above 0.4 is enough to predict that a kickstarter will be sucessful based on caculations and evaluation of the model in R.
                               "),
                          # Container for Variable Selection
                            box(
                              width = 12, title = "Variables for Model",
                              splitLayout(
                                
                                selectizeInput(inputId = "model_country",
                                               label = "Country",
                                               choices = NULL),
                                
                                selectizeInput(inputId = "model_category",
                                            label = "Category",
                                            choices = NULL),
                                
                                textInput(inputId = "model_backers",
                                            label = "Backer #",
                                            value = "100"),
                                
                                textInput(inputId = "model_goal",
                                            label = "Goal in US Dollars",
                                            value = "5000"),
                                # Allowing for dropdown from box to be visible past box
                                tags$head(tags$style(HTML("
                                                            .shiny-split-layout > div {
                                                              overflow: visible;
                                                            }
                                                            ")))
                              )
                            ),
                          
                          # Button for running Calculations of user input
                          actionButton("run_model", "Calculate"),
                          
                          # Outputting model results
                          h3(textOutput("model_result")) 
                          
                          )
                 
                 
)





# Define Server -------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
############ Map output #############
  output$KickMap <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Kickstarters",
      kickstarter_countries$name, kickstarter_countries$number_countries
    ) %>% lapply(htmltools::HTML)  
    
    # Creating Map with data
    leaflet(data = kickstarter_countries) %>%
      addTiles() %>%
      addPolygons(
        dashArray = "3",
        stroke = FALSE,
        fillOpacity = 1,
        fillColor = ~pal(number_countries),
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))%>%
      addLegend("bottomright", pal= pal, values = ~number_countries,
                title = "Number of Total Kickstarters",
                opacity = 1)
  })

##### Barplot for number of successful and total kickstarters per year #####
  output$Total_Hist <- renderPlotly({
    
    # Creating Summary of Data
    kickstarter_success_per_year <- kickstarter_status_per_year %>%
      mutate(state = replace(state, state != "successful", "not_successful")) %>%
      group_by(year, state) %>%
      summarize(across(.cols = c(n, freq),  sum)) %>%
      mutate(label_ypos = cumsum(n))
    
    # Storing basic ggplot
    a <- ggplot(kickstarter_success_per_year,
                aes(x= year, y = n, fill = state))
    
    # Creating and storing ggplot in plotly variable
    a_chart <-
      ggplotly(a + geom_bar(stat = "identity") + 
               #geom_text(aes(label = n), vjust = 1.6, size = 3.5) +
               scale_fill_discrete(name = "Outcome" , labels = c("Not Successful", "Successful")) +
               theme_classic() +
               scale_x_continuous(breaks = seq(min(kickstarter_success_per_year$year), max(kickstarter_success_per_year$year), by = 1)) +
               ggtitle("Number of Kickstarters Per Year") + 
               xlab("Year") +
               ylab("# of Kickstarters") +
               ylim(0,80000))
    
    # Renaming Plotly Legend Variables manually
    a_chart[['x']][['data']][[1]][['name']] <- 'Not Successful'
    a_chart[['x']][['data']][[2]][['name']] <- 'Successful'
    
    # Outputting chart
    a_chart
    
  })
  
############ Line graph comparing trend in outcomes ##################
  output$Total_Line <- renderPlotly({
    
    b <- ggplot(kickstarter_status_per_year, aes(x = year, y = freq, colour = state))
    
    b_chart <- ggplotly(
      b + 
        geom_line() +
        theme_classic() + 
        scale_fill_brewer(palette = "Set1") + 
        scale_colour_discrete(name = "Outcome", labels = c("Canceled", "Failed", "Live" , "Successful", "Suspended", "Undefined")) + 
        scale_x_continuous(breaks = seq(min(kickstarter_status_per_year$year), max(kickstarter_status_per_year$year), by = 1)) +
        ggtitle("Comparing Different Kickstarter Outcomes") +
        xlab("Year") +
        ylab("Percentage of total")
    )
    
    b_chart[['x']][['data']][[1]][['name']] <- 'Canceled'
    b_chart[['x']][['data']][[2]][['name']] <- 'Failed'
    b_chart[['x']][['data']][[3]][['name']] <- 'Sucessful'
    b_chart[['x']][['data']][[4]][['name']] <- 'Suspended'    
    
    b_chart
    
  })
  
##### Line graph for Number of Backers by category #####
  output$Backers_Line <- renderPlotly({
    
    category_backers_year <- 
      Kicks %>%
      group_by(main_category, year) %>%
      summarize(num_backers = n())
    
    c_graph <- 
      ggplotly(
      ggplot(data = category_backers_year, mapping = aes(x = year, y = num_backers, color = main_category)) +
        geom_line() + 
        #geom_line(data = total_backers, mapping = aes(x = year, y = num_backers, color = "Black")) +
        theme_classic() + 
        scale_x_continuous(breaks = seq(min(category_backers_year$year), max(category_backers_year$year), by = 1)) +
        ggtitle(label = "Number of Backers in Each Category") +
        xlab("Year") + 
        ylab("# of Backers")
    )
    # Output graph
    c_graph
    
  })
  
##### Barplot Showing analyzing the distribution of successful kickstarters #####
  output$Success_Category <- renderPlotly({
    
    c <- ggplot(category_success, aes(x = reorder(main_category,freq), y = freq, fill = main_category))
    
    plot_3 <- 
      c + 
      geom_bar(stat = "identity") +
      theme_classic() +
      coord_flip() +
      ggtitle("Most Successful Categories as Percent of Total") +
      xlab("Category") +
      ylab("% of Successful Kickstarters")
    
    ggplotly(plot_3)
    
  })
  
##### Barplot showing the most successful categories#####
  output$Most_Success_Category <- renderPlotly({
    
    d <- ggplot(category_success2[category_success2$state == "successful",], aes(x = reorder(main_category,-percent_of_category), y = percent_of_category, fill = main_category))
    plot_4 <-
      d + 
      geom_bar(stat = "identity") +
      ggtitle("Category Success Rate") +
      xlab("Main Category") +
      ylab("% successful in category") + 
      theme_classic()
    
    ggplotly(plot_4)
  })
  
  
##### Outputting summary table based on user selected variables #####
  output$Group_Stats <- renderDataTable({
    
    #storing user choices in variable
    vars_to_summarize <- input$summary_var 
    # Storing user choice for calculation
    funcs <- input$summary_stat
    # Obtaining functions from character string inputs chosed by user, storing functions in variable
    funcs2 <- lapply(funcs, get)
    names(funcs2) <- funcs # Renaming function names in the list created
    
    # Summarizing data based on user chocies
    Category_Sum <-
      Kicks %>%
        group_by_at(.vars = input$group_var) %>%
      dplyr::summarise(across(.cols = !!vars_to_summarize, .fns = funcs2))
    
    #Outputting data
    Category_Sum
  })
  
########### Storing calculation run in model after user input #############
  Success_Result <- eventReactive(input$run_model,{
    
    # Variables that store user choices
    text_cat <- input$model_category
    text_country <- input$model_country
    backer_number <- as.numeric(input$model_backers)
    usd_number <- as.numeric(input$model_goal)
    
    ######## Changing variables based on user input #########
    # Seacrhing column name for  selections that were made and storing index
    category_index <- grepl(paste0(text_cat), colnames(user_choice))
    country_index <- grepl(paste0(text_country), colnames(user_choice))
    
    # Changing Binary Variables of indexes stored
    user_choice[category_index] <- 1 
    user_choice[country_index] <- 1 
    
    # Changing numeric variables based on user input
    user_choice[,"backers"] <- backer_number
    user_choice[,"usd_goal_real"] <- usd_number
    
    # Prediction to make based on user input, Prediction uses Model created before creating UI
    choice.predict <- predict(fit.lasso, s = bestlam.lasso, newx = user_choice, type = "response")
    
    # Store value
    return(round(choice.predict, 2))
    
  })
  
########### Printing text with model calculation result #############
  output$model_result <- renderText({
    
      paste0("The probability of success is:", " ", Success_Result())
    
    })
    
######### Storing model choices for country and category on server side ##########
    updateSelectizeInput(session, "model_country", choices = country_list, server = TRUE)
    updateSelectizeInput(session, "model_category", choices = category_list, server = TRUE)
  
}



# Run the application ---------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
