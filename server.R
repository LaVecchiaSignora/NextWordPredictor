#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
suppressWarnings(library(shiny))
suppressWarnings(library(tm))
suppressWarnings(library(stringr))
load("./2gram.RData")
load("./3gram.RData")
load("./4gram.RData")
inform <<- ""



Predict <- function(x) {
   
    x1 <- removeNumbers(removePunctuation(tolower(x)))
    x2 <- strsplit(x1, " ")[[1]]
    
    
    if (length(x2)>= 3) {
        x2 <- tail(x2,3)
        if (identical(character(0),head(quadgram[quadgram$unigram == x2[1] & quadgram$bigram == x2[2] & quadgram$trigram == x2[3], 4],1))){
            Predict(paste(x2[2],x2[3],sep=" "))
        }
        else {inform <<- " quadro-gram is used."; head(quadgram[quadgram$unigram == x2[1] & quadgram$bigram == x2[2] & quadgram$trigram == x2[3], 4],1)}
    }
    else if (length(x2) == 2){
        x2 <- tail(x2,2)
        if (identical(character(0),head(trigram[trigram$unigram == x2[1] & trigram$bigram == x2[2], 3],1))) {
            Predict(x2[2])
        }
        else {inform<<- " tri-gram is used"; head(trigram[trigram$unigram == x2[1] & trigram$bigram == x2[2], 3],1)}
    }
    else if (length(x2) == 1){
        x2 <- tail(x2,1)
        if (identical(character(0),head(bigram[bigram$unigram == x2[1], 2],1))) {
            inform<<-"No match."
            }
        else {inform <<- "bi-gram is used."; head(bigram[bigram$unigram == x2[1],2],1)}
    }
}
    
    
shinyServer(function(input, output) {
    output$prediction <- renderPrint({
        result <- Predict(input$inputString)
        output$text2 <- renderText({inform})
        result
    });
        

    
    output$text1 <- renderText({
        input$inputString});
}
)

