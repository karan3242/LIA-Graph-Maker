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
    
    main_table1 <- reactive({
        if(input$input_type == 'Single'){
            tibble("pb64" = input$pb64,
                   "pb74" = input$pb74,
                   "pb84" = input$pb84,
                   "id" = "sample")
        } else {
            read_csv(input$iso_file$datapath)
        }
        
        })
    
    output$main_table_names <- renderUI({
        choices <- unique(names(main_table1()))
        selectInput("colnames_select",
                    "Select iso colums from 206, 207, 208 and ID in that order",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE)
    })

    
    main_table2 <- reactive({
      req(input$colnames_select)
      
        main_table1() %>% 
            select(all_of(head(input$colnames_select, 4)))
    })
    

    

# Base Table --------------------------------------------------------------


    base_table1 <- reactive({data_base %>% 
            select(input$base_group, `Presence of Pb`, `Presence of Cu`, `206Pb/204Pb`, `207Pb/204Pb`, `208Pb/204Pb`, everything())})
    
    
    output$pb_filter <- renderUI({
        choices <- unique(base_table1()$`Presence of Pb`)
        selectInput("pb_select",
                    "Select Pb Values",
                    choices = choices,
                    selected = choices,
                    multiple = TRUE)
    })
    
    output$cu_filter <- renderUI({
        choices <- unique(base_table1()$`Presence of Cu`)
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
            filter(base_table1()$`Presence of Pb` %in% input$pb_select & 
                     base_table1()$`Presence of Cu` %in% input$cu_select)
        
        cols_main <- c("pb64", "pb74", "pb84")
        cols_base <- c("206Pb/204Pb", "207Pb/204Pb", "208Pb/204Pb")
        
        indices <- expand.grid(
          main_idx = seq_len(nrow(main_table2())),
          base_idx = seq_len(nrow(base_table2))
        )
        
        m1 <- as.matrix(main_table2()[indices$main_idx, cols_main])
        m2 <- as.matrix(base_table2[indices$base_idx, cols_base])
        
        distances <- sqrt(rowSums((m1 - m2)^2))
        
        long_results <- data.frame(
          main_row = indices$main_idx,
          base_row = indices$base_idx,
          distance = distances
        )
        
        bind_cols("Distance"= long_results$distance, main_table2()[long_results$main_row, ], base_table2[long_results$base_row, ])
    })
    
    distance_table2 <- reactive({
        req(distance_table(), input$numbuer_of_distances)
        
        distance_table() %>%
            group_by(pick(5)) %>% 
            slice_min(order_by = Distance, 
                      n = input$numbuer_of_distances, 
                      with_ties = FALSE) %>% 
            ungroup()
    })
    

    output$group_selector <- renderUI({
        choices <- unique(base_table1()[[1]])
        choices_select <- unique(distance_table2()[[6]])
        selectInput("group_selector",
                    "Select Sample Groups",
                    choices = choices,
                    selected = choices_select,
                    multiple = TRUE)
    })
    
    distance_table3 <- reactive({
        req(distance_table())
      
        table <- distance_table() %>%
          filter(distance_table()[[6]] %in% input$group_selector) %>% 
          group_by(pick(5)) %>% 
          slice_min(order_by = Distance, 
                    n = input$numbuer_of_distances, 
                    with_ties = FALSE) %>% 
          ungroup()
        
        table
       
       
      
    })
    
    output$main_table2 <- renderDataTable({
      distance_table3()

    })
    
    summary_table <- reactive({
      # Ensure inputs are available before proceeding
      req(distance_table3(), input$base_group)

      group_cols <- switch(input$base_group,
                           "Mine"             = c("Country", "corrected Region", "Mine"),
                           "corrected Region" = c("Country", "corrected Region"),
                           "Country"          = "Country",
                           "Country"          # Default fallback
      )

      distance_table3() %>% 
         select(all_of(group_cols)) %>%
         group_by(across(all_of(group_cols))) %>%
         summarize(
          Count = n(),
           .groups = "drop"
          )
    })

    output$summary_table <- renderDataTable({
      summary_table()
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
        
        if(input$input_type == "Single") {
            xlim <- c(input$pb64 - 0.2, input$pb64 + 0.2)
            ylim74 <- c(input$pb74 - 0.2, input$pb74 + 0.2)
            ylim84 <- c(input$pb84 - 0.2, input$pb84 + 0.2)
        } else {
            xlim <- NULL
            ylim74 <- NULL
            ylim84 <- NULL
        }
        
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
              legend_ncol = input$legend_ncol,
              xlim = xlim,
              ylim74 = ylim74,
              ylim84 = ylim84,
              show_label = input$point_lables)}, 
        width = function(){3 * req(input$image_sclae)}, 
        height = function(){4 * req(input$image_sclae)}
        )
    
    # output$table <- renderTable({base_table2()})

}
