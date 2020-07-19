library(dplyr)
library(stringr)
library(data.table)
library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Next word prediction"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textAreaInput("word", "Enter words"),
            sliderInput("Number of predicted words", label = h3("Number of predicted words"), min = 1, max = 10, value = 5),
            submitButton("Predict Next word")),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Prediction", tableOutput("words")),
                        tabPanel("How it works?",
                                 h3("How it works?"),
                                 h6("This app tries to predict a the next word from a text input from the user.
                                    For this objective the app works with a model based on Kneser-Ney.
                                    The full process is completely explained in the 'Methology' tab"),
                                 h6("1. First, enter the words in the white box of the sidebar panel."), 
                                 h6("2. Then, select the number of options for the predicted word"),
                                 h6("3. Click bottom 'Predict Next Word'"),
                                 h6("In the table in the main menu we can see the output. The predict word and the correspondent probability")
                        ),
                        tabPanel("Methodology",
                        h3("Data Science Capston"),
                        h4("Background"),
                        h6("This project is the work of the Data Science Capstone"),
                        h6("The goal of the works is: From three dataset from twitter, news and blogs, generate a model that could predict the next word"),
                        h4("First steps"),
                        h6("The first steps consists in download the data, take a sample and makes some a arrenge in the data.
                           Make all word lower case, remove punctuation, take out profanity words, steam the document, remove stop words."),
                        h4("N-Gram tables"),
                        h6("Next, we have to create diferent ngram tables. We make ngram tablas from 1, 2, 3, 4 and 5 ngrams."),
                        h4("Prediction model"),
                        h6("The first aprroach was to make a simpler predictive model base on the maximum likelihood estimation. This is, 
                           if we consider for example the word 'i' and want to predict the next word, we take the 2gram table look 
                           all the times that the first word is 'i'. The sum of counts that contains 'i' as a first word is the denominator.
                           The numerator is the count for each posible second word"),
                        h6("This approach works when we have situations of observed words. But if i enter a word or group of words that 
                        is not observed in the corpues, we have no probability."),
                        h4("Kneser-Ney"),
                        h6("For the reasons prevously descripted, we start looking different model that take account of this NOT observed ngrams situations."),
                        h6("The Kneser-Ney model is a recoursive method that also takes account of the enviroment of the word to predict."),
                        h6("Basically the method takes out some probability of the observed ngrams and distributes that probability in the unseen ngrams.
                           In the cases that there are not any seen ngram, the model downs 1 grams and works the same way, until the unigram in case of necesity")
                        
                        
                        
                        )
            
        )
    )
)))
