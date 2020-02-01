library(shiny)
library(ggplot2)
library(eeptools)

outputDir <- "responses"
tankslist2 <- read.delim("tanklist.txt", stringsAsFactors = FALSE)
age_months<- age_calc(as.Date(tankslist2$DOB, format="%d/%m/%Y"), 
               enddate = as.Date(Sys.Date()),
               units = "months", precise = FALSE)
tankslist1 <-cbind(tankslist2,age_months)


# Define the fields we want to save from the form
fields <- c("Tank_number", "Comments", "Setup_by")

saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()
  
  # Create a unique file name
  fileName <- sprintf(
    "%s_%s.rds", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
  
  # Write the file to the local system
  saveRDS(
    object = data,
    file = file.path(outputDir, fileName)
  )
}

loadData <- function() {
  # read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    field_list <- c(fields, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list
  } else {
    data <- lapply(files, function(x) readRDS(x)) 
    
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }
  
  data
}



resetForm <- function(session) {
  # reset values
  updateTextInput(session, "Tank_number", value = "")
  updateCheckboxInput(session, "Comments", value = "")
  updateSliderInput(session, "Setup_by", value = "")
}

ui <- fluidPage(
  
  # App title ----
  titlePanel("Appquarium"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h4("FISH SETUP"),
      textInput("Tank_number", "Tank number", ""),
      textInput("Comments", "Comments", ""),
      selectInput("Setup_by", "Setup by:",
                  c("", "Andy", "Athena", "Talhah", "Vicky")
      ),
      actionButton("submit", "Submit"),
      actionButton("clear", "Clear Form"),
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Setups",  dataTableOutput("responses")),
        tabPanel("Tanks list", dataTableOutput("tankslist")))
    )
  )
)

server = function(input, output, session) {

 

  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(input)
    resetForm(session)
    showNotification("Thank you! Your response has been recorded.",
                     type="message")
  })
  
  observeEvent(input$clear, {
    resetForm(session)
  })
  
  # When the Delete button is clicked, delete all of the saved data files
  observeEvent(input$delete, {
    deleteData()
  })
  
 
  
  # Show the previous responses in a reactive table ----
  output$responses <- renderDataTable({
    # update with current response when Submit or Delete are clicked
    input$submit 
    input$delete
    
    loadData()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
    }
  )
  
  output$tankslist <- renderDataTable({
    tankslist1  
    })
}

shinyApp(ui, server)
