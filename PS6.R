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
install.packages("EBMAforecast")
library(EBMAforecast)
data(presidentialForecast)



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
      helpText("Here are the results from presidential forecasts from 1952-2008")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot"), click = "plot_click"),
                  tabPanel("Table", tableOutput("table")))
    )
  )
)



# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  library(EBMAforecast)
  data("presidentialForecast")
  
  
  output$plot <- renderPlot({
    plot(presidentialForecast)
  })
  
  output$table <- renderTable({
    presidentialForecast
  }) 
  
?plot
  
  
}

# Create Shiny app ----
shinyApp(ui, server)
