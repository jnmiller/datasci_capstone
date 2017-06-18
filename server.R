require(shiny)
require(data.table)
require(quanteda)
source("kneser_ney.R")

ptbls <- list()
for (i in seq(1,4)) {
    ptbls[[i]] <- fread(paste0("probtables",i,".bin"))
}
shinyServer(function(input, output) {
    
    output$wordPredictions <- renderTable({
        if(trimws(input$inputText) != "") {
            predictWordKN(input$inputText, NULL, ptbls)
        } else {
            "type to get predictions"
        }
    })
})