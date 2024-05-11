#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

function(input, output, session) {
  
  ######################## General
  require(shiny)
  require(googlesheets4)
  
  googlesheets4::gs4_deauth()
  googlesheets4::gs4_auth()
  
  table_data <- reactive({
    test_data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1YEn75uxp6UyXC8mwW1F1f89S0uI_KBdTu-XAD4_riW0/edit?usp=sharing")
  })
  
  #####################3### Input data page
  observe({
    shinyjs::toggleState("submit", !is.null(input$fruit) && input$fruit != "") # Prevents submit button from being pressed if 'fruit' is empty
  }) 
  
  global <- reactiveValues(response = FALSE) # Initiate response for confirmation button
  
  output$datatable <- renderDT({
    DT::datatable(table_data(), options = list(pageLength = -1))
  })
  
  observeEvent(input$submit, {
    shinyalert(
      title = "Confirm Submission",
      text = paste0("Are you sure you want to submit the rating of <b>",input$rating, "</b> for <b>", input$fruit, "</b>?"),
      html = TRUE,
      type = "warning",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      showCancelButton = TRUE,
      showConfirmButton = TRUE,
      confirmButtonText = "Confirm",
      cancelButtonText = "Cancel",
      animation = FALSE,
      callbackR = function(x) {
        global$response <- x
        if (global$response == TRUE) {shinyalert(title = "Saved", type = "success")}
        if (global$response == FALSE) {shinyalert(title = "Cancelled", type = "error")}
      }
    )
  }) # finish confirmation button
  
  response_data <- reactive({
    response_data <- tibble::tibble(Date = as.Date(Sys.Date()), Fruit = input$fruit, Rating = as.numeric(input$rating)) # New input entry
    print(response_data)
  }) # Write new row
  
  observeEvent(input$shinyalert, {
    req(input$shinyalert) 
    print(global$response)
    if (global$response != FALSE) {
      
      if (nrow(table_data()) == 0) {
        sheet_write(data = as.data.frame(response_data()),
                    ss = "https://docs.google.com/spreadsheets/d/1YEn75uxp6UyXC8mwW1F1f89S0uI_KBdTu-XAD4_riW0/edit?usp=sharing",
                    sheet = "Data")
        
        global$response <- FALSE # Resets the confirmation button
      } else {
        sheet_append(data = as.data.frame(response_data()),
                     ss = "https://docs.google.com/spreadsheets/d/1YEn75uxp6UyXC8mwW1F1f89S0uI_KBdTu-XAD4_riW0/edit?usp=sharing",
                     sheet = "Data")
        print(global$response)
        global$response <- FALSE # Resets the confirmation button
      }
      
      print(response_data())
      
      table_data_new <- reactive({
        test_data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1YEn75uxp6UyXC8mwW1F1f89S0uI_KBdTu-XAD4_riW0/edit?usp=sharing")
      }) # Reread updated datatable
    }
    
    output$datatable <- renderDT({
      DT::datatable(table_data_new(), options = list(pageLength = -1))
    })
  })

  ######################## Overview Tab
  if (exists("table_data_new()")) { # Printing an updated one if the user has inputted new data
      output$overview_rating <- renderPlot({
        #boxplot(as.numeric(overview_plotting_df()$Rating) ~ overview_plotting_df()$Fruit)  
        ggplot(data = table_data_new(), mapping = aes(x=table_data()$Fruit, y=as.numeric(table_data()$Rating))) +
          geom_boxplot() +
          theme_minimal() +
          labs(y="Rating", x="Fruit") +
          scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 1))
      }) # Renders the plot of all data
    print("print updated")
    
   } else { # Else, printing old one if the user has inputted new data
      output$overview_rating <- renderPlot({
        #boxplot(as.numeric(overview_plotting_df()$Rating) ~ overview_plotting_df()$Fruit)  
        ggplot(data = table_data(), mapping = aes(x=table_data()$Fruit, y=as.numeric(table_data()$Rating))) +
          geom_boxplot() +
          theme_minimal() +
          labs(y="Rating", x="Fruit") +
          scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 1))
      }) # Renders the plot of all data
    print("print updated")
  }

  
  ######################## Filter TaB
  
  output$datatable_filt <- renderDT(
    table_data() %>% 
      mutate(Date = as.Date(Date)),
    rownames = FALSE,
    filter = 'top',
    caption = 'Filtered data entries') # Renders filtered table as a dataframe (filtered by user actively)
  
  output$filtered_rating <- renderPlot({
    filtered_data_ind <- input$datatable_filt_rows_all
    if (length(filtered_data_ind) > 0 && length(filtered_data_ind) < nrow(table_data())) {
      filtered_data <- table_data()[filtered_data_ind, ]
      ggplot(data = filtered_data, mapping = aes(x=Fruit, y=as.numeric(Rating))) +
        geom_boxplot() +
        theme_minimal() +
        labs(y="Rating", x="Fruit") +
        scale_y_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 1))
    } # Renders plot of filtered data
  })
  }
