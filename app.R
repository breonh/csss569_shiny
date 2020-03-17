
library(shiny)

if(!require(ggrepel)){
    install.packages("ggrepel")
    library(ggrepel)
}

if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Crime in Atlanta, GA 2017"),
    tabsetPanel(
        
        tabPanel("About", fluid = TRUE,
                     mainPanel(
                         br(),
                         p("The visualizations provided here show data from the Atlanta Police Department for the year 2017. Click through the tabs to check them out."),
                         p("The data used in this application can be found on",
                           a("the Atlanta PD website",
                             href = "https://www.atlantapd.org/i-want-to/crime-data-downloads")),
                         p("The visualizations are based around ATL PD's patrol zones, colloquially referred to as 'Zones'. See picture below."),
                         br(),
                         div(id = "picture",
                             img(src = "zones.jpg", height = 500, width = 500))
                         
                 )),
        
        tabPanel("Neighborhoods", fluid = TRUE,
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         h4("Population"),
                         sliderInput("pop_slider",
                                            "",
                                            min = 0,
                                            max = 17000,
                                            value = 17000),
                         
                         checkboxGroupInput("pop_items",
                                      "",
                                      choices = list("Natural Log of Population" = "1", 
                                                     "Separate Zones" = "2"), 
                                      selected = "1"),
                         
                         sliderInput("pop_slider_labels",
                                     h5("Factor Change from Average ATL Crime Rate"),
                                     min = 1.5,
                                     max = 5,
                                     value = 3)
                     ),
                     # Population Graph goes here
                     mainPanel(
                         plotOutput(outputId = "pop_rate")
                     )
                     )),
        
        tabPanel("Calendar", fluid = TRUE,
                 br(), 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("calendar_zones", h3("Zones"), 
                                     choices = list("Zone 1: Bankhead" = 1, 
                                                    "Zone 2: Buckhead" = 2,
                                                    "Zone 3: Moreland Ave" = 3,
                                                    "Zone 4: West End" = 4,
                                                    "Zone 5: Centennial Olympic" = 5,
                                                    "Zone 6: Old Fourth Ward" = 6,
                                                    "All Zones" = 7), selected = 7)
                                  ),
                     mainPanel(
                         plotOutput(outputId = "calendar_fig")
                     )
                     ))
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Loading the Data
    sp_input <- read_csv("sp_input.csv")
    hm_input <- read_csv("hm_input.csv")
    full_input <- read_csv("full_input.csv")
    
    # The Population Scatter Plot
    
    output$pop_rate <- renderPlot({
        pop_input <- sp_input %>%
            filter(population <= input$pop_slider) %>%
            mutate(outlier_flag = if_else(rate >= input$pop_slider_labels * mean(rate), 1, 0)) %>%
            mutate(zone = factor(zone, levels = c(1:6)))
        
        pop_input$intercept <- mean(sp_input$rate)
        
        p <- ggplot() +
            geom_jitter(data = pop_input, aes(x = population, y = rate, color = zone), size = 2) +
            geom_text_repel(data = pop_input %>% filter(outlier_flag == 1), aes(x = population, y = rate, label = neighborhood)) +
            geom_hline(yintercept = pop_input$intercept, linetype = "dashed") +
            scale_color_manual(values = c("1" = "#beaed4",
                                          "2" = "#7fc97f",
                                          "3" = "#bf5b17",
                                          "4" = "#d0c700",
                                          "5" = "#e18800",
                                          "6" = "#386cb0")) +
            xlab("Residents") +
            ylab("Crimes per 1k Residents") +
            labs(caption = "Crime per 1k Residents in Each Atlanta Neighborhood by Population.\n Dashed line is average crime rate in 2017.") +
            theme_bw() +
            theme(legend.position = "bottom",
                  axis.ticks = element_blank(),
                  text = element_text(size = 18, color = "black")) 
        
        validate(
            need(input$pop_items != "", "Please check a box. \n My advice: You want to see the population logged.")
        )
        
        if(input$pop_items == 1){p <- p + scale_x_log10()} else {p}
        
        if(input$pop_items == 2){p <- p + facet_wrap(~ zone) + theme(text = element_text(size = 16),
                                                                     strip.background = element_rect(fill = "white"))} else {p}
        
        if(all(c(1,2) %in% input$pop_items)){p <- p + facet_wrap(~ zone) + scale_x_log10() + theme(text = element_text(size = 16),
                                                                                                   strip.background = element_rect(fill = "white"))} else {p}
        
        p
    
    })
    
    output$calendar_fig <- renderPlot({
        
        if(input$calendar_zones == 1){zone_selections <- c(1)}
        if(input$calendar_zones == 2){zone_selections <- c(2)}
        if(input$calendar_zones == 3){zone_selections <- c(3)}
        if(input$calendar_zones == 4){zone_selections <- c(4)}
        if(input$calendar_zones == 5){zone_selections <- c(5)}
        if(input$calendar_zones == 6){zone_selections <- c(6)}
        if(input$calendar_zones == 7){zone_selections <- c(1:6)}
        
        calendar_input <- full_input %>%
            filter(zone %in% zone_selections) %>%
            group_by(months, monthweek, week_day, days) %>%
            tally() %>%
            ungroup() %>%
            mutate(month_week = if_else(monthweek == 52, 1, monthweek)) %>%
            mutate(days = as.factor(days)) %>%
            mutate(week_day = case_when(week_day == "Sun" ~ "Su",
                                        week_day == "Mon" ~ "M",
                                        week_day == "Tue" ~ "T",
                                        week_day == "Wed" ~ "W",
                                        week_day == "Thu" ~ "R",
                                        week_day == "Fri" ~ "F",
                                        week_day == "Sat" ~ "Sa")) %>%
            mutate(week_day = factor(week_day, levels = c("Su", "M", "T", "W", "R", "F", "Sa"))) %>%
            mutate(months = factor(months, levels = c("Jan", "Feb", "Mar",
                                                      "Apr", "May", "Jun",
                                                      "Jul", "Aug", "Sep",
                                                      "Oct", "Nov", "Dec")))
        
        p2 <- ggplot(data = calendar_input) +
            geom_tile(aes(x = week_day, y = month_week, fill = n), color = "white") +
            scale_fill_gradient(low = "#3288bd", high = "#ea1919") +
            scale_y_reverse(expand = c(0,0)) +
            scale_x_discrete(expand = c(0,0)) +
            xlab("")+
            ylab("") + 
            theme_bw() +
            labs(caption = "Crime Per 1,000 Residents Each Day in Atlanta 2017") +
            theme(axis.ticks = element_blank(),
                  axis.text.y = element_blank(),
                  panel.grid = element_blank(),
                  legend.title = element_blank(),
                  text = element_text(size = 20),
                  axis.text.x = element_text(color = "black", size = 10),
                  strip.background = element_rect(fill = "#ffdc9b")) +
            facet_wrap(~ months) 
        
        p2
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
