rm(list=ls())

library(dplyr)
library(stringr)

# Kneser-ney

# Bigram

# 1 Primera parte de la funcion.

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

kn1bi('a lot')

# Segunda parte de la ecuación. Lambda

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

lambda('test')

# Tercera parte de a ecuación. Pcontinuation


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

pcont('test')

# Predicción

predict <- function(words, number){
        firstpart<-kn1bi(words)
        lambda <- lambda(words)
        pcont <- pcont(words)
        prediction <- merge(firstpart, pcont, by.x='last_word', by.y='w') %>%
                group_by(last_word) %>%
                summarise(prediction = max(prob)+lambda*max(prob_cont))
        names(prediction)<-c("PredictedWord", "Probability")
                return(prediction[order(-prediction$Probability)[1:number],])
}

a<-predict('then you must be',20)
class(a)
