library(shiny)

ui <- fluidPage(
  headerPanel("Mag Quad Magnolia Data"),
  fluidRow(
    column(width = 1),
    column(width = 6,
           h4("Click on a the image below to \"measure\" a leaf. The code on the right will indicate the length and width of the leaf closest to where you clicked."),
    actionButton(inputId = "but", label = NULL, style = "width: 100%; height: 500px;
background: url('https://prod.wp.cdn.aws.wfu.edu/sites/16/2012/06/mag-quad.jpg');  background-size: cover; background-position: center;"),
    ),
    column(width = 5,
           h4("Copy the code below in R to read your leaf data in."),
           br(),
    verbatimTextOutput("text", placeholder = TRUE)
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- reactiveValues(
    length = NULL,
    width = NULL
  )
  observeEvent(input$but, {
    d <- MASS::mvrnorm(1, 
                       mu = c(15, 8), 
                       Sigma = matrix(c(5.4^2, 5.4 * 4 * .42, 5.4 * 4 * .42, 4^2), 2) )
    while (any(d < 2)) {
      d <- MASS::mvrnorm(1, 
                         mu = c(15, 8), 
                         Sigma = matrix(c(5.4^2, 5.4 * 4 * .42, 5.4 * 4 * .42, 4^2), 2) )
    }
    data$length <- c(data$length, d[1])
    data$width <- c(data$width, d[2])
  })
  
  output$text <- renderText({
    req(input$but)
    glue::glue("library(tidyverse)\n",
               "magnolia_data <- tibble(\n",
               "  observation = c({glue::glue_collapse(1:length(data$length), sep = ', ')}), \n",
               "  leaf_length = c({glue::glue_collapse(round(data$length, 2), sep = ', ')}), \n",
               "  leaf_width = c({glue::glue_collapse(round(data$width,2), sep = ', ')})\n)",
               .trim = FALSE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
