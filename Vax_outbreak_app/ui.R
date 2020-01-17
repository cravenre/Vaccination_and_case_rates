library(shiny)

# Define the scatter plot to show on shiny app
shinyUI(
     navbarPage("Measles: A Look at Vaccination Rates", theme=shinytheme("superhero"),
                tabPanel("Introduction", icon = icon("book-open"),
                         fluidRow(
                             mainPanel(
                                 fluidRow(
                                     column(3),
                                     column(9,
                                            h2("Measles: A Look at Vaccination Rates"),
                                            h4("Measles is a highly contagious viral disease that can be prevented by the effective use of
                                                vaccination. Two doses of measles vaccine is roughly 97% effective while
                                                a single dose is still 93% effective at preventing measles. Prior to the
                                                introduction of the vaccine in 1963, there were estimated to be over 3 million
                                                cases of measles and over 400 deaths annually. With widespread vaccination and
                                                public health policies, measles was declared elimnated from the US population
                                                in 2000. Since then the virus has caused outbreaks within small populations."),
                                            br(),
                                            h2("Vaccinations, Cases, and Expenditures"),
                                            h4("To understand the importance of measles vaccination on case counts,
                                                the number of measles cases, vaccination rates, and health care
                                               expenditures have been compiled. The data are shown at the national level,
                                               looking state by state, as well as the global level, looking at the data for
                                               each country individually. US vaccination rates are those reported for children
                                               entering kindergarten of the listed year who have received the full two-dose
                                               vaccination. Global vaccine data is as reported by the World Health Organization
                                               and includes data for both the first and second doses of vaccine. All health 
                                               expenditure data is presented as per capita expenses in current US dollars."),
                                            br(),
                                            h4("Sources: CDC, WHO, OECD"))
                                 )
 
                             )
                         )),
                
                tabPanel("National", icon = icon("flag"),
                         fluidRow(
                             sidebarPanel(
                                 selectInput("statemap_vars", label=h3("Color map by:"), choices=state_map_vars),
                                 uiOutput("state_map_years")
                             ),
                             mainPanel( 
                                       h4(textOutput("state_map_text")),
                                       
                                       plotlyOutput("state_map")),
                             style='margin-bottom:30px;border:1px solid; padding: 10px;'
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
                                    column(9,
                                           h4(textOutput("state_plot_text")),
                                           plotOutput("state_plot"))
                                    )
                                )
                            )
                         ),

                tabPanel("Global", icon = icon("globe"),
                         fluidRow(
                             sidebarPanel(
                                 selectInput("globalmap_vars", label=h3("Color map by:"), choices=global_map_vars),
                                 uiOutput("global_map_years")
                             ),
                             mainPanel(
                                 h4(textOutput("global_map_text")),
                                 plotlyOutput("global_map")),
                             style='margin-bottom:30px;border:1px solid; padding: 10px;'
                         ),
                         fluidRow(
                             sidebarPanel(
                                 selectInput("Country", label=h3("Select Country"), choices=global_list),
                                 selectInput("globalxvar", label=h5("Variable 1"), choices=global_vars,
                                             selected=global_vars[1]),
                                 selectInput("globalyvar", label=h5("Variable 2"), choices=global_vars,
                                             selected=global_vars[3])
                             ),
                             mainPanel(
                                 fluidRow(
                                     column(9,
                                            h4(textOutput("global_plot_text")),
                                            plotOutput("global_plot"))
                                )
                             )
                         )
                         )
                )
)