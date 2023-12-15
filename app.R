###################################################################################
# This R script should contain all relevant codes you have used to generate the Shiny app.
# Codes should be well-commented.
# Do NOT include codes that you have experimented with, or is not connected to the app.
###################################################################################
# install and load required packages here
library(shiny)
library(usmap)
library(ggplot2)
library(plotly)
library(DT) 
library(tidyverse)
library(dplyr)

# install.packages("package_name")   # if required


################################################################################
# write R code here (such as, load dataset, any code prior to the app, etc.)
#Load Dataset:
monster <- read_csv("monster_com-job_sample.csv")

#dropping the columns:
monster <- monster[,c(4,8,9,13,14)]
monster <- monster %>%
  filter(has_expired == "No") #keeping only the jobs that have not expired

# separating the job type to only full time or part time:
monster <- monster %>%
  mutate(job_type = str_extract(tolower(job_type), "full time|part time"))


# separate the location to only state abbreviation:
monster <- monster %>%
  separate(location, into = c("city","state"), sep = " ", convert = TRUE)

monster$state <- str_extract(monster$state, "\\b[A-Z]{2}\\b")
#Dropping the city column:
Monster <- monster[,c(1,2,4,5,6)]


#separating sector into basics:
Monster <- Monster %>%
  separate(sector, into = c("sector","drop"), sep = "\\(", convert = TRUE)%>%
  separate(sector, into = c("sector","drop2"), sep = "/", convert = TRUE)%>%
  select(has_expired, job_type, sector, state, uniq_id)

#seeing how many sectors are there and how many are the same/similar basics:
x <- Monster %>% group_by(sector) %>% summarise(count = n())

#exporting the file to excel to make a new data set with similar categories
library(openxlsx)

write.xlsx(Monster, "Monster.xlsx", rowNames = FALSE)


#importing the file with basic categories
Categories <- read_csv("Categories.csv")

#joining the 50+States data set as map needs full form of states:
state_names <- read_csv("50+States.csv")
categories <- Categories %>%
  left_join(state_names, by = c("state" = "Abbr"))

#arranging data in a which which counts number of jobs for each sector separately:
table_data <- categories %>%
  group_by(State, Category, job_type, uniq_id) %>%
  summarise(count = n())%>%
  arrange(desc(count))

#Removing NA and renaming for graph:
table_data <- na.omit(table_data)%>%
  rename("state"="State") #doing this cause using State gave me error
table_data <- table_data %>%
  rename("Number_of_Jobs" = "count")






################################################################################
# shiny app code

# define UI 
ui <- fluidPage(
  titlePanel("US Map and Table for number of Jobs in each State"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Category of Job:", choices = unique(table_data$Category)),
      selectInput("job_type", "Select Job Type:", choices = unique(table_data$job_type))
    ),
    mainPanel(
      plotlyOutput("us_map"), #plotly for interactive US map
      DT::dataTableOutput("job_table")  #For interactive table
    )
  )
  
)



# define server logic 
server <- function(input, output) {
  output$us_map <- renderPlotly({
    
    t1 <- table_data %>%
      group_by(Category, state, job_type) %>%
      summarize(Total_Jobs = n())
    
    t1 <- t1 %>%
      filter(Category == input$category) %>%
      filter(job_type == input$job_type)
    
    #code for generating the map that shows variation in number of jobs through colors: 
    map <- plot_usmap(data = t1, values = "Total_Jobs", color = "red") + 
      scale_fill_continuous(
        low = "white", high = "red", name = "Number of Jobs", label = scales::comma
      ) + theme(legend.position = "right")
    #white indicates the lowest value, red indicates the highest value. the intensity of color increases as values increase. 
    ggplotly(map)
    
    
  })
  #code for generating table that shows number of jobs of each sector in each state
  output$job_table <- DT::renderDataTable({
    map_data <- table_data %>%
      filter(Category == input$category) %>%
      filter(job_type == input$job_type) %>%
      group_by(state) %>%
      summarise(Number_of_Jobs = sum(Number_of_Jobs))
    
    datatable(map_data, options = list(pageLength = 10))  #users can choose the number of data they want to view
  })
  
}



# run the app
shinyApp(ui = ui, server = server)


################################################################################
