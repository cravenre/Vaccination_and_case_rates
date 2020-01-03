library(shiny)

# Define the scatter plot to show on shiny app
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Measles Vaccinations and Cases"),
    
    tabsetPanel(
        tabPanel("US States",
                 # Sidebar with a slider input for number of bins
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("Year", label=h3("Select year"), choices=state_year),
                         selectInput("State", label=h3("Select state"), choices=state_list)
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         fluidRow(
                             column(6, "Vaccination Rates", plotOutput("state_vaxrate")),
                             column(6, "Case Count", plotOutput("state_cases"))
                         )))
                 ),
        
        tabPanel("Globally",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("Year", label=h3("Select year"), choices=global_year),
                         selectInput("Country", label=h3("Select country"), choices=global_list)
                     ),
                     
                     mainPanel(
                         fluidRow(
                             column(6, "Vaccination Rates", plotOutput("global_vaxrate")),
                             column(6, "Case Count", plotOutput("global_cases"))
                         )
                     )
                 )))
    
    # # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         selectInput("Year", label=h3("Select year"), choices=state_year),
    #         selectInput("State", label=h3("Select state"), choices=state_list)
    #     ),
    #     
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         fluidRow(
    #             column(6, "Vaccination Rates", plotOutput("state_vaxrate")),
    #             column(6, "Case Count", plotOutput("state_cases"))
    #         )
    # 
    #     )
    # )
))