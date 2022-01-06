#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Decision Tree Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("datafile", "Data upload", accept = ".csv"),
            checkboxInput("header", "Header", TRUE),
            selectInput("m.target", "Select Target", choices = NULL), # no choices before uploading 
            selectInput("m.inputs", "Select Inputs", choices = NULL, multiple = TRUE), # no choices before uploading 
            sliderInput("cp",
                        "complexity:",
                        min = 1,
                        max = 50,
                        value = 30)
        
            ,
            width=3
            ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("plot", plotOutput("distPlot")),
                tabPanel("dataset", tableOutput("contents"))
                )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    

    output$contents <- renderTable({
        file <- input$datafile
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need(ext == "csv", "Upload dataset"))

        df <- read.csv(file$datapath, header = input$header)
        
        vars <- names(df)
        # Update select input immediately after clicking on the action button. 
        updateSelectInput(session, "m.target","Select target", choices = vars)
        updateSelectInput(session, "m.inputs","Select inputs", choices = vars)
        
        df
    })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$cp + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
