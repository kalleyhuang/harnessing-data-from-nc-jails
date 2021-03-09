#load libraries
library(shiny)
library(rsconnect)
library(tidyverse)
library(lubridate)
library(highcharter)

#load data
population <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/app/population.csv")
population$date <- as.Date(population$date, "%Y-%m-%d")
length_of_stay <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/app/length_of_stay.csv")
length_of_stay$date <- as.Date(length_of_stay$date, "%Y-%m-%d")
ratio <- read.csv("https://raw.githubusercontent.com/kalleyhuang/harnessing-data-from-nc-jails/master/app/ratio.csv")
ratio$date <- as.Date(ratio$date, "%Y-%m-%d")
county_list <- population$county %>% 
    unique()
county_list <- county_list[c(length(county_list), 1:length(county_list)-1)]

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
            p("Select county and years of interest:"),
            selectInput(inputId = "county_chosen", label = "County:", choices = county_list,
                        selected = county_list[0]), #select county
            sliderInput(inputId = "range", label = "Range:", 
                        min = as.Date("1995-01-01", "%Y-%m-%d"), max = as.Date("2020-03-01"), 
                        value = c(as.Date("1995-01-01", "%Y-%m-%d"), as.Date("2020-03-01"))), #select years
            #p("Hover for values:"),
            #verbatimTextOutput("pop_info"), #hover for population at time
            #verbatimTextOutput("length_info"), #hover for length of stay at time
            #verbatimTextOutput("ratio_info") #hover for ratio at time
        ),
        mainPanel(
           #plotOutput("popPlot", hover = hoverOpts(id = "pop_hover", nullOutside = F)),
           #plotOutput("lengthPlot", hover = hoverOpts(id = "length_hover", nullOutside = F)),
           #plotOutput("ratioPlot", hover = hoverOpts(id = "ratio_hover", nullOutside = F)),
            highchartOutput("popHighchart"),
            highchartOutput("lengthHighchart"),
            highchartOutput("ratioHighchart")
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
            geom_line(mapping = aes(y = unsen_male, color = "male", linetype = "unsentenced")) +
            geom_line(mapping = aes(y = unsen_female, color = "female", linetype = "unsentenced")) +
            geom_line(mapping = aes(y = sen_male, color = "male", linetype = "sentenced")) +
            geom_line(mapping = aes(y = sen_female, color = "female", linetype = "sentenced")) +
            labs(title = "Average Daily Populations Over Time", 
                 x = "Year", y = "Population") +
            scale_color_discrete(name = "Gender", breaks = c("male", "female"), labels = c("Male", "Female")) +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            scale_linetype(name = "Sentence Type", breaks = c("unsentenced", "sentenced"), 
                           labels = c("Unsentenced", "Sentenced")) +
            theme_classic()
    })
    
    output$pop_info <- renderText({
        paste0("Date: ", as.Date(input$pop_hover$x, origin = origin), 
               "\nPopulation: ", input$pop_hover$y)
    })
    
    output$popHighchart <- renderHighchart({
        pop <- population %>% 
            filter(county == input$county_chosen) %>%
            filter(date >= min(input$range) & date <= max(input$range))
        
        highchart() %>% 
            hc_chart(type = "line") %>% 
            hc_title(text = "Average Daily Populations Over Time") %>% 
            hc_series(list(name = "Unsentenced males", data = pop$unsen_male, color = "blue", dashStyle = "shortDot"),
                      list(name = "Unsentenced females", data = pop$unsen_female, color = "red", dashStyle = "shortDot"),
                      list(name = "Sentenced males", data = pop$sen_male, color = "blue"),
                      list(name = "Sentenced females", data = pop$sen_female, color = "red")) %>% 
            hc_xAxis(title = list(text = "Year"), categories = year(pop$date)) %>% 
            hc_yAxis(title = list(text = "Population"), labels = list(format = "{value}")) %>% 
            hc_tooltip(table = T, sort = T, 
                       pointFormat = paste0("<br><span style='color:{point.color}'>\u25CF</span>",
                                            " {series.name}: {point.y}")) %>% 
            hc_plotOptions(series = list(marker = list(enabled = F)))
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
    
    output$lengthHighchart <- renderHighchart({
        length <- length_of_stay %>% 
            filter(county == input$county_chosen) %>%
            filter(date >= min(input$range) & date <= max(input$range))
        
        highchart() %>% 
            hc_chart(type = "line") %>% 
            hc_title(text = "Average Quarterly Lengths of Stay Over Time") %>% 
            hc_series(list(name = "Sentenced", data = length$sentenced_release, 
                           color = "black"),
                      list(name = "Pretrial", data = length$pretrial_release, 
                           color = "black", dashStyle = "shortDot")) %>% 
            hc_xAxis(title = list(text = "Year"), categories = year(length$date)) %>% 
            hc_yAxis(title = list(text = "Length of Stay (in days)"), labels = list(format = "{value}")) %>% 
            hc_tooltip(table = T, sort = T, 
                       pointFormat = paste0("<br><span style='color:{point.color}'>\u25CF</span>",
                                            " {series.name}: {point.y}")) %>% 
            hc_plotOptions(series = list(marker = list(enabled = F)))
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
    
    output$ratioHighchart <- renderHighchart({
        rat <- ratio %>% 
            filter(county == input$county_chosen) %>%
            filter(date >= min(input$range) & date <= max(input$range))
        
        highchart() %>% 
            hc_chart(type = "line") %>% 
            hc_title(text = "Unsentenced to Sentenced Ratio Over Time") %>% 
            hc_series(list(data = rat$ratio, color = "black")) %>% 
            hc_xAxis(title = list(text = "Year"), categories = year(rat$date)) %>% 
            hc_yAxis(title = list(text = "Ratio"), labels = list(format = "{value}")) %>% 
            hc_tooltip(table = T, sort = T, 
                       pointFormat = paste0("<br><span style='color:{point.color}'>\u25CF</span>",
                                            " {point.y}")) %>% 
            hc_plotOptions(series = list(marker = list(enabled = F))) %>% 
            hc_legend(enabled = F)
    })
    
}

# run application 
shinyApp(ui = ui, server = server)