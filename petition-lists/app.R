
# when a volunteer is out on the street let them enter in data in real time maybe to a google form and read from google drive

library(shiny)

big.data <- read.csv("data/BigWalkList20190217.csv", stringsAsFactors=FALSE)

#big.data <- read.csv("data/SmallWalkList20190217.csv", stringsAsFactors=FALSE)
#big.data[,c("Signed", "Refused", "Not.Home", "Moved", "Inaccessible", "Language", "Notes")] <- ""
#write.csv(big.data,file="data/BigWalkList20190217.csv",row.names=FALSE)

#small.bool <- big.data$Ward==5 | big.data$Ward==7
#small.data <- big.data[small.bool,]
#write.csv(small.data,file="data/SmallWalkList20190217.csv",row.names=FALSE)

display.columns1 <- c("ID.Number","First.Name","Last.Name","Ward","Division","Age","Party.Code","Last.Vote.Date","Vote.Percent","House.Number","Street.Name")
print.columns <- c("ID.Number","Ward","Division","ID.Number","First.Name","Last.Name","Gender","Age","Last.Vote.Date","House.Number","Street.Name","Apartment.Number","Home.Phone")
display.columns2 <- c("ID.Number","First.Name","Last.Name","Ward","Division","House.Number","Street.Name")
input.columns <- c("Signed", "Refused", "Not.Home", "Moved", "Inaccessible", "Language", "Notes")

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
        textInput("streetname","Street Name:",
                  value=""
        ),
        textInput("housenumber", "House number:",
                     value=""
        ),
        br(),
        downloadButton("downloadWalkList", "Download Walklist"),
        br(),
        h4("Input voter information:"),
        actionButton("saveinput", "Update Voter Input"),
        textOutput("voterWarning"),
        textOutput("totalChanges"),
        br(),
        actionButton("writeinput", "Save All Changes to File")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Walklists",
                   br(),
                   div(style="display:inline-block",
                      selectInput("selectParty","Party Affiliation:",
                               choices=c("All", "Democrats",sort(unique(big.data[["Party.Code"]]))),
                               selected= "Democrats")
                   ),
                   div(style="display:inline-block",
                       numericInput("superVoter", "Percent of recent elections voted in:",
                                value=20, min=0, max=100, step=1)
                   ),
                   h3("Data Summary:"),
                   tableOutput("summary"),
                   h3("Data Preview:"),
                   tableOutput("voterList1")
          ),
          tabPanel("Input",
                   h3("Find voter:"),
                   div(style="display:inline-block",
                       textInput("idnumber", "ID number:",
                             value="")
                   ),
                   div(style="display:inline-block",
                       textInput("firstname","First Name:",
                             value="")
                   ),
                   div(style="display:inline-block",
                       textInput("lastname","Last Name:",
                             value="")
                   ),
                   tableOutput("voterList2"),
                   # TODO ensure only one voter is used
                   h3("Enter data:"),
                   radioButtons("allinputs", NULL,inline=TRUE,
                                     choiceNames = c("Signed petition","Refused","Not Home","Moved",
                                                 "Otherwise Inaccessible","Not an English speaker"),
                                choiceValues = 1:6
                   ),
                   textInput("inputnotes","Notes:",value = "", width='600px'),
                   tableOutput("voterList3")
          )
        )
      )
   )
)

server <- function(input, output) {
  
   subsetDataWalk <- reactive({
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
     
     sname.ind <- grep( input$streetname, sub.data$Street.Name, ignore.case=TRUE )
     sub.data <- sub.data[sname.ind,]
     
     hnum.ind <- grep( input$housenumber, sub.data$House.Number, ignore.case=TRUE )
     sub.data <- sub.data[hnum.ind,]

     return(sub.data)
   })
   
   subsetDataInput <- reactive({
     sub.data <- big.data
     if (input$ward != "All"){sub.data <- sub.data[sub.data[,"Ward"] == input$ward,]}
     if (input$division != "All"){sub.data <- sub.data[sub.data[,"Division"] == input$division,]}

     sname.ind <- grep( input$streetname, sub.data$Street.Name, ignore.case=TRUE )
     sub.data <- sub.data[sname.ind,]

     hnum.ind <- grep( input$housenumber, sub.data$House.Number, ignore.case=TRUE )
     sub.data <- sub.data[hnum.ind,]

     idnum.ind <- grep( input$idnumber, sub.data$ID.Number, ignore.case=TRUE )
     sub.data <- sub.data[idnum.ind,]

     # TODO option to fuzzy match names
     fname.ind <- grep( input$firstname, sub.data$First.Name, ignore.case=TRUE )
     sub.data <- sub.data[fname.ind,]

     lname.ind <- grep( input$lastname, sub.data$Last.Name, ignore.case=TRUE )
     sub.data <- sub.data[lname.ind,]

     return(sub.data)
   })
   
   summarizeData <- reactive({
     sub.data <- subsetDataWalk()
     summ.names <- c("TotalPeople","TotalDems")
     summ.data <- data.frame(matrix(0,1,length(summ.names)))
     names(summ.data) <- summ.names
     summ.data$TotalPeople <- nrow(sub.data)
     summ.data$TotalDems <- sum((sub.data$Party.Code=="D")|(sub.data$Party.Code=="DEM"), na.rm=TRUE)
     return(summ.data)
   })
   
   displayData1 <- reactive({
     sub.data <- subsetDataWalk()
     sub.data <- sub.data[,display.columns1]
     sub.data <- sub.data[1:min(10,nrow(sub.data)),]
     return(sub.data)
   })
   
   displayData2 <- reactive({
     sub.data <- subsetDataInput()
     sub.data <- sub.data[,display.columns2]
     if (nrow(sub.data)>1){
       sub.data <- sub.data[1:min(10,nrow(sub.data)),]
     }
     return(sub.data)
   })
   
   new.data <- reactiveValues(big.inputs = big.data[,c("ID.Number",input.columns)], num.changes = 0)
   
   displayData3 <- reactive({
     sub.data <- subsetDataInput()
     sub.inputs <- new.data$big.inputs[new.data$big.inputs$ID.Number == sub.data[1,"ID.Number"],input.columns]
     return(sub.inputs)
   })

   one.voter <- reactiveValues(warning.text = NULL)
   
   observeEvent(input$saveinput,{
     sub.data <- subsetDataInput()
     if (nrow(sub.data)==1){
       new.data$big.inputs[new.data$big.inputs$ID.Number == sub.data[1,"ID.Number"],
                           as.character(input.columns[as.numeric(input$allinputs)])] <- "TRUE"
       new.data$big.inputs[new.data$big.inputs$ID.Number == sub.data[1,"ID.Number"],input.columns[7]] <- input$inputnotes
         
       new.data$num.changes <- new.data$num.changes+1
       one.voter$warning.text <- paste(sub.data[1,"ID.Number"], "saved!")
     }else{
       one.voter$warning.text = "Warning: Select only one voter to input data"
     }
   })

   observeEvent(input$writeinput,{
     big.data[,input.columns] <- new.data$big.inputs[,input.columns]
     write.csv(big.data,file="data/BigWalkList20190217.csv",row.names=FALSE)
   })
   
   output$summary <- renderTable(summarizeData())
   output$voterList1 <- renderTable(displayData1())
   output$voterList2 <- renderTable(displayData2())
   output$voterList3 <- renderTable(displayData3())
   output$downloadWalkList <- downloadHandler(
     filename = function(){
       paste0("data_",input$ward,"_",input$division,".csv")
     },
     content = function(file){
       print.list <- subsetDataWalk()
       print.list <- print.list[,print.columns]
       print.list <- print.list[order(print.list$Street.Name, print.list$House.Number),]
       write.csv(print.list,file,row.names=FALSE)
     }
   )
   output$voterWarning <- renderText(one.voter$warning.text)
   output$totalChanges <- renderText(paste(new.data$num.changes,"total changes made"))
}

shinyApp(ui = ui, server = server)

