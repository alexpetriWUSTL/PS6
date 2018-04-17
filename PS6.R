## Shiny Lecture
library("shiny")

### Let's start with an example
runExample("01_hello") # a histogram

### We can make our own example by modifying this.


#### More examples
runExample("02_text") # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg") # global variables
runExample("05_sliders") # slider bars
runExample("06_tabsets") # tabbed panels
runExample("07_widgets") # help text and submit buttons
runExample("08_html") # Shiny app built from HTML
runExample("09_upload") # file upload wizard
runExample("10_download") # file download wizard
runExample("11_timer") # an automated timer

## Let's build our own (This is due next Tuesday as a problem set)

#1 ) 
#### As our first step, we are going to make a UI that does nothing.  We are going to say:

# Presidential Forecasts

# Here are the results of presidential forecasts from 1952-2008
# (this shoudl be in a lower font)

#2)
## As our second step, we are going to follow example 2 above and have it show the last X elections (as selectd by the user)

#3) Now we are going to have it plot the election results 

# https://shiny.rstudio.com/reference/shiny/1.0.2/plotOutput.html

# 4) Now we are going to add a line to add a dropdown window to add a specific forecast to the plot

# 5) Now we are going to make it so it prints out the data points when clicked on

# https://shiny.rstudio.com/articles/plot-interaction.html

ui <- fluidPage(
  
  # App title ----
  titlePanel("Presidential Forecasts"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Include clarifying text ----
      helpText("Here are the results from presidential forecasts from 1952-2008"),

        
      # Input: Selector for choosing dataset, including choices for each forecast ---
      selectInput(inputId = "forecastModel",
                  label = "Choose a forecast model:",
                  choices = c("Campbell", "Lewis-Beck", "EWT2C2",
                                "Fair", "Hibbs", "Abramowitz", "Actual"),
                  selected = "Actual"),
        
        
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Past Elections:",
                   value = 1,
                   max = 15,
                   step = 1)
    ),

            # Main panel for displaying outputs, including plot, click, and table ----
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("plot", click = "plot_click"), verbatimTextOutput("info")),
                    tabPanel("Table", tableOutput("view")))
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  library(EBMAforecast)
  data("presidentialForecast")
  
  # a function that switches between each option for the forecast 
  datasetInput <- reactive({
   switch(input$forecastModel,
         "Campbell" = presidentialForecast$Campbell, "Lewis-Beck" = presidentialForecast$`Lewis-Beck`, "EWT2C2" = presidentialForecast$EWT2C2,
          "Fair" = presidentialForecast$Fair, "Hibbs" = presidentialForecast$Hibbs, "Abramowitz" = presidentialForecast$Abramowitz, "Actual" = presidentialForecast$Actual)
  })
  # add to the output a plot, resetting the x-axis so it shows the year and not the observation number, having ACTUAL as the default and allowing you to add/overlay a new plot
  output$plot <- renderPlot({
    presidentialForecast$year <- rownames(presidentialForecast)
    plot(x = 1:input$obs, y = presidentialForecast$Actual[1:input$obs], xlab = "Year", 
    ylab = "Percent Vote Share in Selected Model", main = "Selected Model Vote Share by Year", 
    xaxt = "n", type = "l")
    axis(1, at = seq(1, 15, 1), labels = seq(1952, 2008, 4), las = 2)
    lines(x = 1:input$obs, y = datasetInput()[1:input$obs], col = "green", type = "l")
  })
  #show the tail of the label so it shows the most recent election result(s) from the desired number of previous elections to see
  output$view <- renderTable({
    tail(presidentialForecast, n = input$obs)
    }, rownames = TRUE) 
  #have the clicker render the output coordinates of wherever you click on the plot
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
}


# Create Shiny app ----
shinyApp(ui, server)






