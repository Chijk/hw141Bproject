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
    titlePanel("Food finder"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("Location",
                      "Enter your Location",
                      "Davis"),
            selectInput("Sort", "Sort method",
                        list("rating", "price", "review_count", "distance"),selected = "rating"),
            textInput("Name",
                      "Enter The Restaurant Name For Graph",
                      "Burgers and Brew"
                      ),
            textInput("Restaurant1",
                      "Enter The Restaurant1 Name For Comparision",
                      "Burgers and Brew"
            ),
            textInput("Restaurant2",
                      "Enter The Restaurant2 Name For Graph",
                      "In-N-Out Burger"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel( textOutput("Resturant Result"),
            tableOutput("table"),
            plotOutput("radarPlot"),
            plotOutput("boxplot"),
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(jsonlite)
    library(lubridate)
    library(httr)
    library(tidyverse)
    library(fmsb)
    readRenviron(".Renviron")
    Temp_input=reactive({input$Location})
    r= reactive({r= GET(
        "https://api.yelp.com/v3/businesses/search",
        add_headers(Authorization = paste("Bearer", Sys.getenv("key"))),
        query = list(
            location = Temp_input()
        )
    )})
    temp_sort=reactive({input$Sort})


    json= reactive({json <- content(r(), as = "text")})
    a=reactive({fromJSON(json(), flatten = TRUE)$busines%>%select(name,rating,price,review_count,distance,phone,url)})
    address=reactive({fromJSON(json(), flatten = TRUE)$busines$location.address1})
    ab=reactive({cbind(a(),address())})

    s_ab <- reactive({if (input$Sort=='rating')
    {ab()[order(ab()$rating,decreasing = TRUE),]}
        else if(input$Sort=='price')
        {ab()[order(ab()$price),]}
        else if(input$Sort=='review_count')
        {ab()[order(ab()$review_count,decreasing = TRUE),]}
        else if(input$Sort=='distance')
        {ab()[order(ab()$distance),]}
        })

    max_radar=c(5,10000,10000)
    min_radar=c(3,1,1)
    g_data=reactive({g_data=s_ab()%>%filter(name==input$Name)%>%select(rating,review_count,distance)})
    plot_data=reactive({rbind(max_radar,min_radar,g_data())})

    h1_data=reactive({h_data=s_ab()%>%filter(name==input$Restaurant1)%>%select(review_count,distance)})
    h2_data=reactive({h_data=s_ab()%>%filter(name==input$Restaurant2)%>%select(review_count,distance)})
    h_cb_data=reactive({rbind(h1_data(),h2_data())})
    h_cb=reactive({unlist(h_cb_data())})

    output$boxplot=renderPlot({barplot(height=h_cb(),beside=TRUE,main="Resturant Feedback Comparing")})



    output$radarPlot = renderPlot({
        req(!is.null(input$Name))
        radarchart(plot_data(),title="Visualized Resturant Information",plwd=3,plty=1)})
    output$table = renderTable(s_ab())

}

# Run the application
shinyApp(ui = ui, server = server)
