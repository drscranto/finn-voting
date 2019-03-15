
library(shiny)

apt.columns <- c("Ward","Division","House.Number","Street.Name","Address.Line.2")

big.apts <- read.csv("data/possible_apts.csv", stringsAsFactors=FALSE)

ui <- fluidPage(
  
  titlePanel("Voter Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("ward", "Ward: ",
                  choices=c("All",sort(unique(big.apts[["Ward"]]))),
                  selected= "All"
      ),
      selectInput("division", "Division: ",
                  choices=c("All",sort(unique(big.apts[["Division"]]))),
                  selected= "All"
      ),
      numericInput("aptCapacity", "How many registered voters in an apt building?",
                   value=10, min=0, max=100, step=1
      ),
      downloadButton("downloadApts", "Download list of apartments")
    ),
    
    mainPanel(
      h3("Possible apartment buildings:"),
      tableOutput("apartments")
    )
  )
)

server <- function(input, output) {
  
  subsetApartments <- reactive({
    sub.apts <- big.apts
    if (input$ward != "All"){sub.apts <- sub.apts[sub.apts[,"Ward"] == input$ward,]}
    if (input$division != "All"){sub.apts <- sub.apts[sub.apts[,"Division"] == input$division,]}
    sub.apts <- sub.apts[sub.apts$Number.People > input$aptCapacity,]
    return(sub.apts)
  })

  displayApartments <- reactive({
    sub.apts <- subsetApartments()
    max.apts <- min(10,nrow(sub.apts))
    return(sub.apts[1:max.apts,])
  })
  
  output$apartments <- renderTable(displayApartments())

  output$downloadApts <- downloadHandler(
    filename = function(){
      paste0("apts_",input$ward,"_",input$division,".csv")
    },
    content = function(file){
      printList <- subsetApartments()
      printList <- printList[,apt.columns]
      write.csv(printList,file,row.names=FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

