library(shiny)
library(gapminder)
#library(tidyverse)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)

# Defne data 
df <- gapminder
df <- df %>% as.data.frame()
df.countries <- df %>% select(country) %>% unique()

# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title="Gapminder Data"),
    dashboardSidebar(
        varSelectizeInput(inputId='variable',
                          label='Variable to plot:',
                          data=df[,4:6],
                          options=list(
                              placeholder='Select variable',
                              onInitialize = I('function() { this.setValue(""); }'))),
        selectizeInput(inputId='country1',
                       label='Country 1 (red line):',
                       choices=df.countries,
                       options=list(
                           placeholder='Select or type country',
                           onInitialize = I('function() { this.setValue(""); }'))
        ),
        selectizeInput(inputId='country2',
                       label='Country 2 (blue line):',
                       choices=df.countries,
                       options=list(
                           placeholder='Select or type country',
                           onInitialize = I('function() { this.setValue("United States"); }'))
                       )),
    dashboardBody(
        box(title = "Country Comparison",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                plotlyOutput('plot1'))
        )
)

# Define server logic required to plot data
server <- function(input, output) {
    # Subset data based on Country 1 selection
    df.sub1 <- reactive({
        req(input$country1)
        df %>% filter(country==input$country1) %>% select(year, !!input$variable)
    })
    # Subset data based on Country 2 selection
    df.sub2 <- reactive({
        req(input$country2)
        df %>% filter(country==input$country2) %>% select(year, !!input$variable)
    })
    
    output$plot1 <- renderPlotly({
        df1   <- df.sub1()
        df2   <- df.sub2()
        plot1 <- ggplot() + 
            geom_point(data=df1, color="darkred",
                       aes_string(x=names(df1)[1], y=names(df1)[2])) +
            geom_line(data=df1, color="darkred",
                      aes_string(x=names(df1)[1], y=names(df1)[2])) +
            geom_point(data=df2, color="darkblue",
                       aes_string(x=names(df2)[1], y=names(df2)[2])) +
            geom_line(data=df2, color="darkblue",
                      aes_string(x=names(df2)[1], y=names(df2)[2])) +
            labs(x = "Year", y = names(df1)[2]) +
            scale_y_log10() +
            theme(panel.background=element_rect(color="black",fill="lightyellow",size=0.25),
                  panel.grid=element_blank(),
                  plot.title=element_text(size=9, color="black"),
                  strip.background=element_rect(color="black", size=0.25),
                  axis.ticks=element_line(size=0.25),
                  axis.text=element_text(size=7, color="black"),
                  axis.title=element_text(size=8, color="black"),
                  legend.title=element_text(size=8, color="black"),
                  legend.text=element_text(size=7, color="black"),
                  legend.justification="top",
                  legend.position="bottom",
                  legend.key=element_rect(color="black", fill="lightyellow", size=0.1))
        
        ggplotly(plot1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
