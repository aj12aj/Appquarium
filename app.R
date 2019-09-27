library(shiny)
library(ggplot2)
library(eeptools)

outputDir <- "responses"
outputDir2 <- "responses2"
tankslist2 <- read.delim("tanklist.txt", stringsAsFactors = FALSE)
age<- age_calc(as.Date(tankslist1$Date.of.birth, format="%d/%m/%Y"), 
               enddate = as.Date(Sys.Date()),
               units = "months", precise = FALSE)
tankslist1 <-cbind(tankslist2,age)
lastone <-last(tankslist1$Tank.number)+1


# Define the fields we want to save from the form
fields <- c("Tank_number", "Comments", "Setup_by")
fields2 <- c("Line", "DOB", "addedby")
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


#################add tank
saveData2 <- function(input) {
  # put variables in a data frame
  data2 <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fields2) {
    var2 <- input[[x]]
    if (length(var2) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data2[[x]] <- list(var2)
    } else {
      # all other data types
      data2[[x]] <- var2
    }
  }

  # Create a unique file name
  fileName <- sprintf(
    "%s_%s.rds", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
  
  # Write the file to the local system
  saveRDS(
    object = data2,
    file = file.path(outputDir2, fileName)
  )
}

loadData2 <- function() {
  # read all the files into a list
  files2 <- list.files(outputDir2, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    field_list <- c("lastone",fields2)
    data2 <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data2) <- field_list
  } else {
    data2 <- lapply(files, function(x) readRDS(x)) 
    
    # Concatenate all data together into one data.frame
    data2 <- do.call(rbind, data2)
  }
  
  data2
}

#################add tank
#deleteData <- function() {
  # Read all the files into a list
 # files <- list.files(outputDir, full.names = TRUE)
  
  #lapply(files, file.remove)
#}

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
      br(),
      br(),
      h4("ADD TANK"),
      h5("Tank number:", lastone),
      selectInput("Line","Line",c("",tankslist1$Line)),
      dateInput("DOB",
                label = 'DOB: yyyy-mm-dd',
                value = Sys.Date()),
      selectInput("Location", "Location",
                  c("", "PeterMac", "WEHI")),
      selectInput("addedby", "Added by:",
                  c("", "Andy", "Athena", "Talhah", "Vicky")
      ),
      
      downloadButton("downloadData", "Download"),
      actionButton("delete", "Delete All Data")
      
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
  
  #Add tank
  observeEvent(input$addtank, {
    saveData2(input)
    showNotification("Thank you! Your response has been recorded.",
                     type="message")
  })
  #Add tank
  
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
    input$addtank
    loadData2()
  })
}

shinyApp(ui, server)