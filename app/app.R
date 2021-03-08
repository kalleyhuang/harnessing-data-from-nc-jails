#load libraries
library(shiny)
library(rsconnect)
library(tidyverse)
library(lubridate)

#load data
population <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/app/population.csv")
population$date <- as.Date(population$date, "%Y-%m-%d")
length_of_stay <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/app/length_of_stay.csv")
length_of_stay$date <- as.Date(length_of_stay$date, "%Y-%m-%d")
ratio <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/app/ratio.csv")
ratio$date <- as.Date(ratio$date, "%Y-%m-%d")
county_list <- population$county

#define UI for application
ui <- fluidPage(
    titlePanel("California Jails"), # application title
    sidebarLayout(
        sidebarPanel(
            p("These datasets are scraped from the California Board of State and Community Corrections'
              Jail Profile Survey by Jacob Kaplan from the University of Pennsylvania."),
            p("These data were collected from 57 counties from October 1995 to March 2020. Note that 
              California has 58 counties, but its least populous county, Alpine County, does not have 
              a jail and contracts with Calaveras County and El Dorado County."), #background
            selectInput(inputId = "county_chosen", label = "County:", choices = county_list,
                        selected = county_list[0]), #select county
            sliderInput(inputId = "range", label = "Range:", 
                        min = as.Date("1995-01-01", "%Y-%m-%d"), max = as.Date("2020-03-01"), 
                        value = c(as.Date("1995-01-01", "%Y-%m-%d"), as.Date("2020-03-01"))), #select years
            verbatimTextOutput("pop_info"), #hover for population at time
            verbatimTextOutput("length_info"), #hover for length of stay at time
            verbatimTextOutput("ratio_info") #hover for ratio at time
        ),
        mainPanel(
           plotOutput("popPlot", hover = hoverOpts(id = "pop_hover")),
           plotOutput("lengthPlot", hover = hoverOpts(id = "length_hover")),
           plotOutput("ratioPlot", hover = hoverOpts(id = "ratio_hover")),
        )
    )
)

# define server logic required to draw plots
server <- function(input, output) {

    output$popPlot <- renderPlot({
        population %>%
            filter(county == input$county_chosen) %>%
            filter(date >= min(input$range) & date <= max(input$range)) %>% 
            ggplot(data = ., mapping = aes(x = date, group = 1)) +
            geom_line(mapping = aes(y = sen_male, color = "male", linetype = "solid")) +
            geom_line(mapping = aes(y = sen_female, color = "female", linetype = "solid")) +
            geom_line(mapping = aes(y = unsen_male, color = "male", linetype = "dotted")) +
            geom_line(mapping = aes(y = unsen_female, color = "female", linetype = "dotted")) +
            labs(title = "Average Daily Populations Over Time", 
                 x = "Year", y = "Population") +
            scale_color_discrete(name = "Gender", breaks = c("male", "female"), labels = c("Male", "Female")) +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            scale_linetype_manual(name = "Sentence Type",
                                  values = c("solid", "dotted"), labels = c("Sentenced", "Unsentenced")) +
            theme_classic()
    })
    
    output$pop_info <- renderText({
        paste0("Date: ", as.Date(input$pop_hover$x, origin = origin), 
               "\nPopulation: ", input$pop_hover$y)
    })
    
    output$lengthPlot <- renderPlot({
        length_of_stay %>%
            filter(county == input$county_chosen) %>%
            filter(date >= min(input$range) & date <= max(input$range)) %>% 
            ggplot(data = ., mapping = aes(x = date, group = 1)) +
            geom_line(mapping = aes(y = pretrial_release, linetype = "solid")) +
            geom_line(mapping = aes(y = sentenced_release, linetype = "dotted")) +
            labs(title = "Average Quarterly Lengths of Stay Over Time", 
                 x = "Year", y = "Length of Stay (in days)") +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            scale_linetype_manual(name = "Sentence Type",
                                  values = c("solid", "dotted"), labels = c("Sentenced", "Pretrial")) +
            theme_classic()
    })
    
    output$length_info <- renderText({
        paste0("Date: ", as.Date(input$length_hover$x, origin = origin), 
               "\nLength of Stay: ", input$length_hover$y)
    })
    
    output$ratioPlot <- renderPlot({
        ratio %>%
            filter(county == input$county_chosen) %>%
            filter(date >= min(input$range) & date <= max(input$range)) %>% 
            ggplot(data = ., mapping = aes(x = date, y = ratio)) +
            geom_line() +
            labs(title = "Unsentenced to Sentenced Ratio Over Time", x = "Year", y = "Ratio") +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            theme_classic()
    })
    
    output$ratio_info <- renderText({
        paste0("Date: ", as.Date(input$ratio_hover$x, origin = origin), 
               "\nRatio: ", input$ratio_hover$y)
    })
    
}

# run application 
shinyApp(ui = ui, server = server)