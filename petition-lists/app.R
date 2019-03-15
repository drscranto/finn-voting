
library(shiny)

big.data <- read.csv("data/BigWalkList20190217.csv", stringsAsFactors=FALSE)

display.columns1 <- c("First.Name","Last.Name","Ward","Division","Age","Party.Code","Last.Vote.Date","Vote.Percent","House.Number","Street.Name")
print.columns <- c("Ward","Division","First.Name","Last.Name","Gender","Age","Last.Vote.Date","House.Number","Street.Name","Apartment.Number")


ui <- fluidPage(
   
   titlePanel("Voter Data"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("ward", "Ward: ",
                     choices=c("All",sort(unique(big.data[["Ward"]]))),
                    selected= "All"
        ),
        selectInput("division", "Division: ",
                    choices=c("All",sort(unique(big.data[["Division"]]))),
                    selected= "All"
        ),
        selectInput("selectParty","Party Affiliation:",
                    choices=c("All", "Democrats",sort(unique(big.data[["Party.Code"]]))),
                    selected= "Democrats"
        ),
        numericInput("superVoter", "Percent of recent elections voted in:",
                     value=20, min=0, max=100, step=1
        ),
        textInput("streetname","Street Name:",
                  value=""
        ),
        textInput("housenumber", "House number:",
                     value=""
        ),
        downloadButton("downloadList", "Download List")
      ),
      
      mainPanel(
        h3("Data Summary:"),
        tableOutput("summary"),
        h3("Data Preview:"),
        tableOutput("voterList1")
      )
   )
)

server <- function(input, output) {
  
   subsetData <- reactive({
     sub.data <- big.data
     if (input$ward != "All"){sub.data <- sub.data[sub.data[,"Ward"] == input$ward,]}
     if (input$division != "All"){sub.data <- sub.data[sub.data[,"Division"] == input$division,]}
     if (input$selectParty != "All"){
       if (input$selectParty == "Democrats"){
         sub.data <- sub.data[(sub.data[,"Party.Code"] == "D")|(sub.data[,"Party.Code"] == "DEM"),]
       }else{
       sub.data <- sub.data[sub.data[,"Party.Code"] == input$selectParty,]
       }
     }
     subsetVote <- (sub.data$Vote.Percent >= input$superVoter/100)|(is.na(sub.data$Vote.Percent))
     sub.data <- sub.data[subsetVote, ]
     if (input$streetname != ""){sub.data <- sub.data[sub.data$Street.Name == input$streetname,]}
     if (input$housenumber != ""){sub.data <- sub.data[sub.data$House.Number == input$housenumber,]}
     return(sub.data)
   })
   
   summarizeData <- reactive({
     sub.data <- subsetData()
     summ.names <- c("TotalPeople","TotalDems")
     summ.data <- data.frame(matrix(0,1,length(summ.names)))
     names(summ.data) <- summ.names
     summ.data$TotalPeople <- nrow(sub.data)
     summ.data$TotalDems <- sum((sub.data$Party.Code=="D")|(sub.data$Party.Code=="DEM"), na.rm=TRUE)
     return(summ.data)
   })
   
   displayData1 <- reactive({
     sub.data <- subsetData()
     sub.data <- sub.data[,display.columns1]
     sub.data <- sub.data[1:10,]
     return(sub.data)
   })
   
   output$summary <- renderTable(summarizeData())
   output$voterList1 <- renderTable(displayData1())
   output$downloadList <- downloadHandler(
     filename = function(){
       paste0("data_",input$ward,"_",input$division,".csv")
     },
     content = function(file){
       print.list <- subsetData()
       print.list <- print.list[,print.columns]
       print.list <- print.list[order(print.list$Street.Name, print.list$House.Number),]
       extra.cols <- data.frame(matrix("",nrow(print.list),6))
       names(extra.cols) <- c("Signed", "Not Home", "Refused", "Moved", "Inaccessible", "Language")
       print.list <- cbind(print.list,extra.cols)
       write.csv(print.list,file,row.names=FALSE)
     }
   )
}

shinyApp(ui = ui, server = server)

