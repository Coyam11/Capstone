library(dplyr)
library(stringr)
library(data.table)
library(shiny)


freqs1 <- read.csv("onegram.csv")
freqs2 <- read.csv("twogram.csv")
freqs3 <- read.csv("trigram.csv")
freqs4 <- read.csv("fourgram.csv")
freqs5 <- read.csv("fivegram.csv")


freqs1 <- data.table(freqs1[,2:3]) 
freqs2 <- data.table(freqs2[,2:3]) 
freqs3 <- data.table(freqs3[,2:3]) 
freqs4 <- data.table(freqs4[,2:3])
freqs5 <- data.table(freqs5[,2:3]) 

freqs1$name <- as.character(freqs1$name)
freqs2$name <- as.character(freqs2$name)
freqs3$name <- as.character(freqs3$name)
freqs4$name <- as.character(freqs4$name)
freqs5$name <- as.character(freqs5$name)

# Function to order ngrams

first_last <- function(variablenombres, cantgram) {
    lista <- list()
    first_words <- c(rep("", length(variablenombres)))
    last_word <- c(rep("", length(variablenombres)))
    for (i in 1:length(variablenombres)){
        lista[i] <- strsplit(variablenombres[i], " ")
        if(cantgram==2){
            first_words[i] <- lista[[i]][1]
        }else if(cantgram==3){
            first_words[i] <- paste(lista[[i]][1], lista[[i]][2], sep = " ")
        }else if(cantgram==4){
            first_words[i] <- paste(lista[[i]][1], lista[[i]][2], lista[[i]][3], sep = " ")       
        }else if(cantgram==5){
            first_words[i] <- paste(lista[[i]][1], lista[[i]][2], lista[[i]][3], lista[[i]][4], sep = " ")
        } 
        last_word[i] <- lista[[i]][cantgram] 
    }
    return(cbind(variablenombres, first_words, last_word))
}

freq1nueva <- freqs1
names(freq1nueva) <- c("last_word", "count")

freq2nueva <- first_last(freqs2$name, 2)
freq2nueva <- merge(freq2nueva, freqs2, by.x = "variablenombres", by.y = "name")
names(freq2nueva)[1]<-"ngram"

freq3nueva <- first_last(freqs3$name, 3)
freq3nueva <- merge(freq3nueva, freqs3, by.x = "variablenombres", by.y = "name")
names(freq3nueva)[1]<-"ngram"

freq4nueva <- first_last(freqs4$name, 4)
freq4nueva <- merge(freq4nueva, freqs4, by.x = "variablenombres", by.y = "name")
names(freq4nueva)[1]<-"ngram"

freq5nueva <- first_last(freqs5$name, 5)
freq5nueva <- merge(freq5nueva, freqs5, by.x = "variablenombres", by.y = "name")
names(freq5nueva)[1]<-"ngram"


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    

    
    
    kn1bi <- function(words, d=0.5){
        
        n <- length(str_split(words, " ")[[1]])
        
        
        if(n >= 4){
            base <- freq5nueva
            words <- str_split(words, " ")[[1]]
            words <- paste(words[c(length(words)-3, length(words)-2, length(words)-1, length(words))], collapse= " ")        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 3                 
                
                
            }} 
        if(n == 3){
            base <- freq4nueva 
            words <- str_split(words, " ")[[1]]
            words <- paste(words[c(length(words)-2, length(words)-1, length(words))], collapse = " ")        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 2                 
                
                
            }} 
        if ( n == 2 ) {
            base <- freq3nueva
            words <- str_split(words, " ")[[1]]
            words <- paste(words[c(length(words)-1, length(words))], collapse=" ")        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 1                 
                
            }} 
        if (n ==1 ) {
            base <- freq2nueva
            words <- str_split(words, " ")[[1]]
            words <- words[c(length(words))]        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 0                 
                
            }} 
        if (n ==0) {
            base <- freq1nueva
            base$num <- c(rep(0, length(base$ngram)))
            base$num <- base$count
            den <- sum(base$count)
            base$prob <- base$num / den
            return(base[,c(1,4)])
        } 
        
        if (n >= 1){
            
            base$num <- c(rep(0, length(base$ngram)))
            base$num[base$first_words==words]<-base$count[base$first_words==words]-d
            den <- sum(base$count[base$first_words==words])
            base$prob <- base$num/den
            return(base[,c(1,3,6)])
        }
    }
    
    lambda <- function(words, d=0.5) {
        n <- length(str_split(words, " ")[[1]])
        
        if(n >= 4){
            base <- freq5nueva
            words <- str_split(words, " ")[[1]]
            words <- paste(words[c(length(words)-3, length(words)-2, length(words)-1, length(words))], collapse= " ")        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 3                 
                
                
            }} 
        if(n == 3){
            base <- freq4nueva 
            words <- str_split(words, " ")[[1]]
            words <- paste(words[c(length(words)-2, length(words)-1, length(words))], collapse = " ")        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 2                 
                
                
            }} 
        if ( n == 2 ) {
            base <- freq3nueva
            words <- str_split(words, " ")[[1]]
            words <- paste(words[c(length(words)-1, length(words))], collapse=" ")        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 1                 
                
            }} 
        if (n ==1 ) {
            base <- freq2nueva
            words <- str_split(words, " ")[[1]]
            words <- words[c(length(words))]        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 0                 
                
            }} 
        
        if (n ==0) {
            base <- freq1nueva
            num <- d*length(base$count)
            den <- sum(base$count)
            lambda <- num / den
            return(lambda)
        } 
        
        if (n >= 1){
            
            num <- d * length(base$first_words[base$first_words==words])
            den <- sum(base$count[base$first_words==words])
            lambda <- num/den
            return(lambda)
        }
    } 
    
    
    pcont <- function(words){
        n <- length(str_split(words, " ")[[1]])
        
        if(n >= 4){
            base <- freq5nueva
            words <- str_split(words, " ")[[1]]
            words <- paste(words[c(length(words)-3, length(words)-2, length(words)-1, length(words))], collapse= " ")        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 3                 
                
                
            }} 
        if(n == 3){
            base <- freq4nueva 
            words <- str_split(words, " ")[[1]]
            words <- paste(words[c(length(words)-2, length(words)-1, length(words))], collapse = " ")        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 2                 
                
                
            }} 
        if ( n == 2 ) {
            base <- freq3nueva
            words <- str_split(words, " ")[[1]]
            words <- paste(words[c(length(words)-1, length(words))], collapse=" ")        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 1                 
                
            }} 
        if (n ==1 ) {
            base <- freq2nueva
            words <- str_split(words, " ")[[1]]
            words <- words[c(length(words))]        
            
            
            if(length(base$count[base$first_words==words])==0){
                n <- 0                 
                
            }} 
        
        if (n ==0) {
            base <- freq1nueva
            pcont_num <- base %>%
                group_by(last_word)%>%
                summarise(count=n())
            pcont_den <- length(base$last_word)
            pcont <- pcont_num$count/pcont_den
            pcont<- data.frame(w=as.character(pcont_num$last_word), prob_cont=pcont)
            return(pcont)
            
        } 
        
        if (n >= 1){
            
            pcont_num <- base %>%
                group_by(last_word)%>%
                summarise(count=n())
            pcont_den <- length(base$ngram)
            pcont <- pcont_num$count/pcont_den
            pcont<- data.frame(w=as.character(pcont_num$last_word), prob_cont=pcont)
            return(pcont)
        }
    }
    
    
    predict <- function(words, number){
        firstpart<-kn1bi(words)
        lambda <- lambda(words)
        pcont <- pcont(words)
        prediction <- merge(firstpart, pcont, by.x='last_word', by.y='w') %>%
            group_by(last_word) %>%
            summarise(prediction = (max(prob)+lambda*max(prob_cont))*100)
        names(prediction)<-c("PredictedWord", "Probability")
        return(prediction[order(-prediction$Probability)[1:number],])
    }
    
    
    sliderValues <- reactive({
        
        predict(input$word, input$`Number of predicted words`)
            
        
    })
    
    # Show the values in an HTML table ----
    output$words <- renderTable({
        sliderValues()
    })    
})

