#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
library(here)
library(tidyverse)

# Data pre-processing
county_m <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/data/california_jail_county_monthly_1995_2020.csv")
county_m$date <- as.Date(county_m$date, format = "%Y-%m-%d")

county_q <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/data/california_jail_county_quarterly_1995_2020.csv")

population <- county_m %>%
    select(.data$census_county_name, .data$date, 
           .data$avg_daily_pop_unsentenced_male, .data$avg_daily_pop_unsentenced_female, 
           .data$avg_daily_pop_sentenced_male, .data$avg_daily_pop_sentenced_female) %>% 
    group_by(.data$census_county_name, .data$date) %>%
    mutate(.data$avg_daily_pop_unsentenced_male = sum(.data$avg_daily_pop_unsentenced_male), 
           .data$avg_daily_pop_unsentenced_female = sum(.data$avg_daily_pop_unsentenced_female),
           .data$avg_daily_pop_sentenced_male = sum(.data$avg_daily_pop_sentenced_male), 
           .data$avg_daily_pop_sentenced_female = sum(.data$avg_daily_pop_sentenced_female)) %>%
    unique() %>%
    group_by(.data$census_county_name) %>%
    arrange(.data$date, .by_group = TRUE)

# ratio <- county_m %>%
#     select(county, date, unsen_male, unsen_female, sen_male, sen_female) %>%
#     group_by(county, date) %>%
#     mutate(unsen_male = sum(unsen_male), unsen_female = sum(unsen_female),
#            sen_male = sum(sen_male), sen_female = sum(sen_female),
#            unsen = unsen_male + unsen_female, sen = sen_male + sen_female, ratio = unsen / sen) %>%
#     unique() %>%
#     select(date, county, ratio) %>%
#     arrange(date)

# length_of_stay <- county_q %>%
#     filter(year > 2001) %>%
#     select(county, date, pretrial_release, sentenced_release) %>%
#     na.omit() %>%
#     arrange(date)

county_list <- county_m$census_county_name

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("California Jails"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #dropdown where you can select counties
            selectInput("county", "County:", county_list)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("popPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$popPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        population %>%
            filter(.data$census_county_name == .env$county) %>%
            ggplot(data = ., mapping = aes(x = date, group = 1)) +
            geom_line(mapping = aes(y = avg_daily_pop_sentenced_male, 
                                    color = "male", linetype = "solid")) +
            geom_line(mapping = aes(y = avg_daily_pop_sentenced_female, 
                                    color = "female", linetype = "solid")) +
            geom_line(mapping = aes(y = avg_daily_pop_unsentenced_male, 
                                    color = "male", linetype = "dotted")) +
            geom_line(mapping = aes(y = unsen_femaleavg_daily_pop_unsentenced_female, 
                                    color = "female", linetype = "dotted"))
            # geom_vline(xintercept = as.Date("2011-01-01"), linetype = "dotted") +
            # geom_vline(xintercept = as.Date("2012-01-01"), linetype = "dotted") +
            # geom_vline(xintercept = as.Date("2014-01-01"), linetype = "dotted") +
            # geom_vline(xintercept = as.Date("2016-01-01"), linetype = "dotted") +
            # labs(x = "Year", y = "Average Daily Populations") +
            # scale_color_discrete(name = "Gender", breaks = c("male", "female"), labels = c("Male", "Female")) +
            # scale_linetype_manual(name = "Sentence Type",
            #                       values = c("solid", "dotted"), labels = c("Sentenced", "Unsentenced")) +
            # theme_classic() +
            # theme(text = element_text(size = 9), legend.position = "none")
    })
}

# length_of_stay %>%
#   filter(county == "San Joaquin County") %>%
#   ggplot(data = ., mapping = aes(x = date, group = 1)) +
#   geom_line(mapping = aes(y = pretrial_release, linetype = "solid")) +
#   geom_line(mapping = aes(y = sentenced_release, linetype = "dotted")) +
#   geom_vline(xintercept = as.Date("2011-01-01"), linetype = "dotted") +
#   geom_vline(xintercept = as.Date("2012-01-01"), linetype = "dotted") +
#   geom_vline(xintercept = as.Date("2014-01-01"), linetype = "dotted") +
#   geom_vline(xintercept = as.Date("2016-01-01"), linetype = "dotted") +
#   labs(x = "Year", y = "Average Lengths of Stay") +
#   scale_linetype_manual(name = "Sentence Type",
#                         values = c("solid", "dotted"), labels = c("Sentenced", "Pretrial")) +
#   theme_classic() +
#   theme(text = element_text(size = 9), legend.position = "none")

# ratio %>%
#   filter(county == "") %>%
#   ggplot(data = ., mapping = aes(x = date, y = ratio)) +
#   geom_line() +
#   geom_vline(xintercept = as.Date("2011-01-01"), linetype = "dotted") +
#   geom_vline(xintercept = as.Date("2012-01-01"), linetype = "dotted") +
#   geom_vline(xintercept = as.Date("2014-01-01"), linetype = "dotted") +
#   geom_vline(xintercept = as.Date("2016-01-01"), linetype = "dotted") +
#   labs(x = "Year", y = "Ratio") +
#   theme_classic() +
#   theme(text = element_text(size = 9))

# Run the application 
shinyApp(ui = ui, server = server)