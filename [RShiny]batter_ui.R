library(shiny)
library(datasets)
library(bslib)
library(png)
library(shinydashboard)
# Load Data
tra = read.csv("trajectory.csv", head = T)
tra$events = ifelse(tra$events == "grounded_into_double_play", "double_play(G)", 
                    ifelse(tra$events == "sac_fly_double_play", "double_play", tra$events))
inplay = tra[tra$events == "single" | tra$events == "double" | 
                 tra$events == "triple" | tra$events == "home_run", ]


# UI
shinyUI(fixedPage(
    
        tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:white}')),
        
        titlePanel(tagList(
            span("Portforlio - Batter")), 
            windowTitle = "Portforlio - Batter"
            ),
        
        hr(),
        
        sidebarPanel(width = 4,
                selectInput("Player", "Select a Player:", 
                            choices = c(choose = "", rownames(table(inplay$name))), selected = TRUE),
                radioButtons("Inplay", "Inplay", choices = c("All" = "all", "Hit" = "hit"), 
                             inline = TRUE),
                radioButtons("Event", "Event", choices = c("All" = "all",
                                                           "Single" = "single",
                                                           "Double" = "double",
                                                           "Triple" = "triple",
                                                           "Homerun" = "home_run"), selected = "all")
                ),
        br(),
        
        mainPanel(width = 8,
            tabsetPanel(
                tabPanel("Spray Chart", plotOutput("spray")))
            )
    
    ) #fixedPage
    ) #shinyUI


