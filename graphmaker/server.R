#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
    
    main_table1 <- reactive({read_csv(input$iso_file$datapath)})
    
    output$main_table_names <- renderUI({
        choices <- unique(names(main_table1()))
        selectInput("colnames_select",
                    "Select iso colums from 206, 207, 208 and ID in that order",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE)
    })

    
    main_table2 <- reactive({
        main_table1() %>% 
            select(input$colnames_select)
    })
    

    

# Base Table --------------------------------------------------------------


    base_table1 <- reactive({data_base %>% 
            select(input$base_group, `Presence of Pb`, `Presence of Cu`, `206Pb/204Pb`, `207Pb/204Pb`, `208Pb/204Pb`)})
    
    
    output$pb_filter <- renderUI({
        choices <- unique(base_table1()[[2]])
        selectInput("pb_select",
                    "Select Pb Values",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE)
    })
    
    output$cu_filter <- renderUI({
        choices <- unique(base_table1()[[3]])
        selectInput("cu_select",
                    "Select Cu Values",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE)
    })
    
    

# Euclidian distance table ------------------------------------------------

    
    distance_table <- reactive({
        req(main_table2(), base_table1(), input$pb_select, input$cu_select)
        
        base_table2 <- base_table1() %>% 
            filter(.[[2]] %in% input$pb_select & .[[3]] %in% input$cu_select)
        
        cols_main <- c("pb64", "pb74", "pb84")
        cols_base <- c("206Pb/204Pb", "207Pb/204Pb", "208Pb/204Pb")
        
        results <- apply(main_table2()[, cols_main[1:3]], 1, function(row1) {
            apply(base_table2[, cols_base], 1, function(row2) {
                sqrt(sum((row1 - row2)^2))
            })
        })
        
        colnames(results) <- main_table2()[[4]] # Id Colum
        
        distances <- as_tibble(results)
        distances$regions <- base_table2[[1]] # regions colum
        
        distances %>% 
            pivot_longer(cols = -regions, names_to = "Sample", values_to = "Distances")
        
    })
    
    distance_table2 <- reactive({
        req(distance_table(), input$numbuer_of_distances)
        
        distance_table() %>%
            group_by(Sample) %>% 
            slice_min(order_by = Distances, n = input$numbuer_of_distances, with_ties = FALSE) %>% 
            ungroup()
    })
    

    output$group_selector <- renderUI({
        choices <- unique(base_table1()[[1]])
        choices_select <- unique(distance_table2()[[1]])
        selectInput("group_selector",
                    "Select Sample Groups",
                    choices = choices,
                    selected = choices_select,
                    multiple = TRUE)
    })
    
    distance_table3 <- reactive({
        req(distance_table())
        distance_table() %>%
            filter(regions %in% input$group_selector) %>% 
            group_by(Sample) %>% 
            slice_min(order_by = Distances, n = input$numbuer_of_distances, with_ties = FALSE) %>% 
            ungroup()
            
        
    })
    
    output$main_table2 <- renderDataTable({
        distance_table3()
    })
    
    output$downloadData <- downloadHandler(
        filename = paste("data-", Sys.Date(), ".csv", sep=""),
        content = function(file) {
            write.csv(distance_table3(), file, row.names = FALSE)
        }
    )
    
    

# Plot output -------------------------------------------------------------

    output$plot <- renderPlot({
        req(main_table2)
        req(base_table1())
        base_table3 <- base_table1() %>% 
            filter(
                .[[1]] %in% input$group_selector &
                    .[[2]] %in% input$pb_select &
                    .[[3]] %in% input$cu_select)
        graph(df = main_table2(),
              isotope_ratios = input$colnames_select,
              df2 = base_table3,
              col2 = as.factor(base_table3[[1]]),
              legend = unique(as.factor(base_table3[[1]])),
              legend_col = unique(as.numeric(as.factor(base_table3[[1]]))),
              title = input$title,
              pch = input$main_shape,
              cex = input$main_size,
              col = input$main_color,
              pch2 = input$base_shape,
              cex2 = input$base_size,
              iso_lines = input$iso_lines,
              t_color = input$main_iso_color,
              iso_color = input$second_iso_color,
              legend_ncol = input$legend_ncol)}, 
        width = function(){3 * req(input$image_sclae)}, 
        height = function(){4 * req(input$image_sclae)}
        )
    
    # output$table <- renderTable({base_table2()})

}
