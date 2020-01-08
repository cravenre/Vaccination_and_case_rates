library(shiny)

# Define the scatter plot to show on shiny app
shinyUI(
     navbarPage("Measles Vaccinations and Cases", theme=shinytheme("flatly"),
                tabPanel("National",
                         fluidRow(
                             sidebarPanel(
                                 selectInput("Year", label=h3("Select Year (Map)"), choices=state_year),
                                 h5("Data Source: Centers for Disease Control")
                             ),
                             mainPanel("State Vaccination Rates", plotlyOutput("state_map"))
                         ),
                         
                         fluidRow(
                             sidebarPanel(
                                 selectInput("State", label=h3("Select state"), choices=state_list)
                            ),
                            mainPanel(
                                fluidRow(
                                    column(7, "Vaccination Rates", plotOutput("state_vaxrate")),
                                    column(5, "Case Count", plotOutput("state_cases"))
                                    )
                                )
                            )
                         ),

                tabPanel("Global",
                         fluidRow(
                             sidebarPanel(
                                 selectInput("year", label=h3("Select Year (Map)"), choices=global_year[1:19]),
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
# tabPanel("Global",
#          # fixedRow(
#          #     column(12,
#          #            sidebarLayout(
#          #                sidebarPanel(
#          #                    selectInput("Year", label=h3("Select Year"), choices=global_year)
#          #                )
#          #            mainPanel(
#          #                fluidrow(column(9, "Vaccination Rates by Country", plotlyOutput("global_map") ))
#          #            )
#          #                )
#          #            ))
#          # )
#          sidebarLayout(
#              sidebarPanel(
#                  selectInput("Year", label=h3("Select year"), choices=global_year),
#                  selectInput("Country", label=h3("Select country"), choices=global_list)
#              ),
# 
#              mainPanel(
#                  fluidRow(
#                      column(6, "Vaccination Rates", plotOutput("global_vaxrate")),
#                      column(6, "Case Count", plotOutput("global_cases"))
#                  )
#              )
#          )),
#         
#         tabPanel("Globally",
#                  sidebarLayout(
#                      sidebarPanel(
#                          selectInput("Year", label=h3("Select year"), choices=global_year),
#                          selectInput("Country", label=h3("Select country"), choices=global_list)
#                      ),
#                      
#                      mainPanel(
#                          fluidRow(
#                              column(6, "Vaccination Rates", plotOutput("global_vaxrate")),
#                              column(6, "Case Count", plotOutput("global_cases"))
#                          )
#                      )
#                  )))
#     
# ))