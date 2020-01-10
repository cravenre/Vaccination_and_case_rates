library(shiny)

# Define the scatter plot to show on shiny app
shinyUI(
     navbarPage("Measles Vaccinations and Cases", theme=shinytheme("flatly"),
                tabPanel("National", icon = icon("flag"),
                         fluidRow(
                             sidebarPanel(
                                 selectInput("Year", label=h3("Select Year (Map)"), choices=state_year),
                                 selectInput("statemap_vars", label=h4("Color map by:"), choices=state_map_vars),
                                 h5("Data Source: Centers for Disease Control")
                             ),
                             mainPanel("State Vaccination Rates", plotlyOutput("state_map"))
                         ),
                         
                         fluidRow(
                             sidebarPanel(
                                 selectInput("State", label=h3("Select state"), choices=state_list),
                                 selectInput("statexvar", label=h5("Variable 1"), choices=state_vars,
                                             selected=state_vars[1]),
                                 selectInput("stateyvar", label=h5("Variable 2"), choices=state_vars,
                                             selected=state_vars[2]),
                            ),
                            mainPanel(
                                fluidRow(
                                    column(9, plotOutput("state_plot"))
                                    )
                                )
                            )
                         ),

                tabPanel("Global", icon = icon("globe"),
                         fluidRow(
                             sidebarPanel(
                                 selectInput("year", label=h3("Select Year (Map)"), choices=global_year[1:19]),
                                 selectInput("globalmap_vars", label=h3("Color map by:", choices=state_map_vars)),
                                 h5("Data Source: World Health Organization")
                             ),
                             mainPanel("Global Vaccination Rates", plotlyOutput("global_map"))
                         ),
                         fluidRow(
                             sidebarPanel(
                                 selectInput("Country", label=h3("Select Country"), choices=global_list)
                             ),
                             mainPanel(
                                 fluidRow(column(6, "Vaccination Rates", plotOutput("global_vaxrate")),
                                          column(6, "Case Count", plotOutput("global_cases"))
                                )
                             )
                         )
                         )
                )
)