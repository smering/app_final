# Loading necessary libraries ------
library(tidyverse)
library(shiny)
library(plotly)
library(ggplot2)
library(rsconnect)
library(scales)
library(DT)
library(bslib)

options <- c("Midwest", "Northeast", "South", "West")
theme <- bs_theme(
  bg = "#0b3d91", fg = "white", primary = "#FCC780",
  base_font = font_google("Fira Sans"),
  code_font = font_google("Fira Sans")
) #https://shiny.rstudio.com/articles/themes.html

ui <- fixedPage(theme = theme,
                hr(),
  titlePanel("Substance Use Disorder & Treatment Accessability"),
  hr(),
  fixedRow(
    column(2, 
    selectInput(inputId = "region", 
                         label = "Select a US Region",
                         choices = options)),
    column(10,
           fixedRow(
             column(width = 6,
                    plotlyOutput("payment")),
             column(width = 6,
                    plotlyOutput("days")),
           ),
  )),
  hr(),
  fixedRow(column(10, offset = 2,
                  DT::dataTableOutput("substances")
)))

server <- function(input, output) {
  path <- "/Users/sarahmering/Documents/GitHub/final-project-af_sm"
  day_wait <- read_csv(file.path(path, "shiny_day_wait.csv"))
  day_wait_us <- read_csv(file.path(path, "day_wait_US.csv"))
  pmt_source <- read_csv(file.path(path, "shiny_pmt.csv"))
  pmt_us <- read_csv(file.path(path, "pmt_US.csv"))
  substance_summary <- read_csv(file.path(path, "substance_summary.csv"))

   payment <- reactive({
     req(input$region)
     pmt_source %>% 
       filter(region %in% input$region) 
   })
  
   days <- reactive({
     req(input$region)
     day_wait %>% 
       filter(region %in% input$region) 
   })

  output$payment <- renderPlotly({
    ggplot() +
      geom_col(data = payment(), aes(x = state, y = proportion, fill = pmt_source), position="fill") +
      geom_col(data = pmt_us, aes(x = state, y = proportion, fill = pmt_source), position="fill") +
      labs(title = "SUD Payment Type Utilized by State", fill = "Payment \nSources") + 
      coord_flip() +
      theme(plot.title = element_text(family = "Sans"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(family = "Sans"),
            axis.text.y = element_text(family = "Sans"),
            legend.text = element_text(family = "Sans"))
  })
  
  output$days <- renderPlotly({ 
    ggplot() +
      geom_col(data = days(), aes(x = state, y = proportion, fill = trt_day_wait), position="fill") +
      geom_col(data = day_wait_us, aes(x = state, y = proportion, fill = trt_day_wait), position="fill") +
      labs(title = "Days on SUD Treatment Waitlist by State", fill = "Days") + 
      coord_flip() +
      theme(plot.title = element_text(family = "Sans"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(family = "Sans"),
            axis.text.y = element_text(family = "Sans"),
            legend.text = element_text(family = "Sans"))
  })
  
  output$substances <- DT::renderDataTable({
    DT::datatable(
    regionFilter <- subset(substance_summary, substance_summary$Region == input$region) %>% select(-Region),
    options = list(lengthMenu = c(5,10,15), pageLength = 7)
  )
  })
  
}
  
shinyApp(ui = ui, server = server)

