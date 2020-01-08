#Creating the shiny app for vax data
shinyServer(
    function(input, output) {
        
        output$state_vaxrate <- renderPlot({
            state_merged %>% 
                filter(state == input$State) %>% 
                ggplot(aes(x=year, y=vax_rate)) +
                geom_point()
        })

        output$state_cases <- renderPlot({
            state_merged %>%
                filter(state == input$State) %>%
                ggplot(aes(x=year, y=measles_cases)) +
                geom_point()
        })
        
        output$global_vaxrate <- renderPlot({
            global_merged %>% 
                filter(country_name == input$Country) %>% 
                ggplot(aes(x=year, y=vax_rate)) +
                geom_point()
        })
        
        output$global_cases <- renderPlot({
            global_merged %>% 
                filter(country_name == input$Country) %>% 
                ggplot(aes(x=year, y=case_total)) +
                geom_point()
        })
        
        output$state_map <- renderPlotly({
            state_mapdata <- state_merged %>% 
                filter(year == input$Year)
            
            g1 <- list(
                scope = 'usa',
                projection = list(type = 'albers usa'),
                showlakes = FALSE,
                lakecolor = toRGB('white')
            )
            
            plot_geo(state_mapdata, locationmode='USA-states') %>%
                add_trace(z=~vax_rate, locations = ~abbrev, color= ~vax_rate, colors='RdBu') %>%
                layout(
                    title = "MMR Vaccination Rates",
                    geo =g1)
        })
        
        output$global_map <- renderPlotly({
            global_mapdata <- global_merged %>% 
                filter(year == input$year)
            
            g2 <- list(
                projection = list(type = 'Mercator')
            )
            
            plot_geo(global_mapdata) %>%
                add_trace(z=~vax_rate, locations = ~iso3, color = ~vax_rate, colors='RdBu') %>%
                layout(
                    title = "Global Vaccination Rate",
                    geo =g2)
        })

    }
)
