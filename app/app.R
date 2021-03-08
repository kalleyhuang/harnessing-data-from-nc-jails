# load libraries
library(shiny)
library(rsconnect)
library(tidyverse)

# load data
population <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/app/population.csv")
length_of_stay <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/app/length_of_stay.csv")
ratio <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/app/ratio.csv")
county_list <- population$county

# define UI for application
ui <- fluidPage(
    titlePanel("California Jails"), # application title
    sidebarLayout(
        sidebarPanel( # dropdown to select county of interest
            selectInput("county_chosen", "County:", county_list)
        ),
        mainPanel(
           plotOutput("popPlot"),
           plotOutput("lengthPlot"),
           plotOutput("ratioPlot"),
        )
    )
)

# define server logic required to draw plots
server <- function(input, output) {

    output$popPlot <- renderPlot({
        population %>%
            filter(county == input$county_chosen) %>%
            ggplot(data = ., mapping = aes(x = date, group = 1)) +
            geom_line(mapping = aes(y = sen_male, color = "male", linetype = "solid")) +
            geom_line(mapping = aes(y = sen_female, color = "female", linetype = "solid")) +
            geom_line(mapping = aes(y = unsen_male, color = "male", linetype = "dotted")) +
            geom_line(mapping = aes(y = unsen_female, color = "female", linetype = "dotted")) +
            labs(title = "Average Daily Populations Over Time", 
                 x = "Year", y = "Population") +
            scale_color_discrete(name = "Gender", breaks = c("male", "female"), labels = c("Male", "Female")) +
            scale_y_continuous(breaks = c(1995, 2000, 2005, 2010, 2005, 2020)) +
            scale_linetype_manual(name = "Sentence Type",
                                  values = c("solid", "dotted"), labels = c("Sentenced", "Unsentenced")) +
            theme_classic()
    })
    
    output$lengthPlot <- renderPlot({
        length_of_stay %>%
            filter(county == input$county_chosen) %>%
            ggplot(data = ., mapping = aes(x = date, group = 1)) +
            geom_line(mapping = aes(y = pretrial_release, linetype = "solid")) +
            geom_line(mapping = aes(y = sentenced_release, linetype = "dotted")) +
            labs(title = "Average Quarterly Lengths of Stay Over Time", 
                 x = "Year", y = "Length of Stay (in days)") +
            scale_linetype_manual(name = "Sentence Type",
                                  values = c("solid", "dotted"), labels = c("Sentenced", "Pretrial")) +
            theme_classic()
    })
    
    output$ratioPlot <- renderPlot({
        ratio %>%
            filter(county == input$county_chosen) %>%
            ggplot(data = ., mapping = aes(x = date, y = ratio)) +
            geom_line() +
            labs(title = "Unsentenced to Sentenced Ratio Over Time", x = "Year", y = "Ratio") +
            theme_classic()
    })
    
}

# run application 
shinyApp(ui = ui, server = server)