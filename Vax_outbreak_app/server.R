#Creating the shiny app for vax data
shinyServer(
    function(input, output) {

        output$state_map_years <- renderUI({
            statemapyears <- state_merged[,c("year", as.character(input$statemap_vars))] %>% 
                drop_na()
            selectInput("state_years", label=h4("Select Year"),
                        choices = sort(unique(statemapyears$year), decreasing=TRUE))
        })
        
        output$state_map_text <- renderText({
            print(names(state_map_vars[which(state_map_vars == input$statemap_vars)]))
        })
        
        output$state_plot <- renderPlot({
            state_merged[,c("state",as.character(input$statexvar),as.character(input$stateyvar))] %>% 
                drop_na() %>% 
                filter(state == input$State) %>% 
                ggplot(aes_string(x=input$statexvar, y=input$stateyvar)) +
                geom_point(size=3) +
                labs(x=names(state_vars[which(state_vars==input$statexvar)]),
                     y=names(state_vars[which(state_vars==input$stateyvar)])) +
                theme(axis.text.x=element_text(angle=60, size=12),
                      axis.text.y=element_text(size=12),
                      axis.title.x=element_text(size=16),
                      axis.title.y=element_text(size=16))
        })
        
        output$state_plot_text <- renderText({
            print(paste(names(state_vars[which(state_vars == input$statexvar)]),"and",
                  names(state_vars[which(state_vars == input$stateyvar)]), sep=" "))
        })
        
        output$global_map_years <- renderUI({
            globalmapyears <- global_merged[,c("year", as.character(input$globalmap_vars))] %>% 
                drop_na()
            selectInput("global_years", label=h4("Select Year"),
                        choices = sort(unique(globalmapyears$year), decreasing=TRUE))
        })
        
        output$global_plot <- renderPlot({
            global_merged[,c("country_name",as.character(input$globalxvar),
                             as.character(input$globalyvar))] %>% 
                drop_na() %>% 
                filter(country_name ==input$Country) %>% 
                ggplot(aes_string(x=input$globalxvar, y=input$globalyvar)) +
                geom_point(size=3) +
                labs(x=names(global_vars[which(global_vars==input$globalxvar)]),
                     y=names(global_vars[which(global_vars==input$globalyvar)])) +
                theme(axis.text.x=element_text(angle=60, size=12),
                      axis.text.y=element_text(size=12),
                      axis.title.x=element_text(size=16),
                      axis.title.y=element_text(size=16))
        })
        
        output$global_plot_text <- renderText({
            print(paste(names(global_vars[which(global_vars == input$globalxvar)]),"and",
                        names(global_vars[which(global_vars == input$globalyvar)]), sep=" "))
        })
        
        output$global_map_text <- renderText({
            print(names(global_map_vars[which(global_map_vars == input$globalmap_vars)]))
        })
        
        output$state_map <- renderPlotly({
            state_mapdata <- state_merged %>% 
                filter(year == input$state_years)
            
            g1 <- list(
                scope = 'usa',
                projection = list(type = 'albers usa'),
                showlakes = FALSE,
                lakecolor = toRGB('white')
            )
            
            plot_geo(state_mapdata, locationmode='USA-states') %>%
                add_trace(z=~get(input$statemap_vars),hoverinfo="text", text=~hover, locations = ~abbrev,
                          color= ~get(input$statemap_vars), colors='YlGnBu') %>% 
                colorbar(title="") %>%
                layout(
                    geo =g1)
        })
        
        output$global_map <- renderPlotly({
            global_mapdata <- global_merged %>% 
                filter(year == input$global_years)
            
            g2 <- list(
                projection = list(type = 'Mercator')
            )
            
            plot_geo(global_mapdata) %>%
                add_trace(z=~get(input$globalmap_vars),hoverinfo="text", text=~hover, locations = ~iso3,
                          color = ~get(input$globalmap_vars), colors='YlGnBu') %>% 
                colorbar(title="") %>%
                layout(
                    geo =g2)
        })

    }
)
