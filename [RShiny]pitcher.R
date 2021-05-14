library(shiny)
library(datasets)
library(ggplot2)
library(dplyr)

# Load Data
pbp = read.csv("plate.csv", head = T)
pbp_L = pbp[pbp$stand == "L", ]
pbp_R = pbp[pbp$stand == "R", ]

# UI
ui = fixedPage(
    
    tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:white}')),
    
    titlePanel(tagList(
        span("Portforlio - Pitcher")), 
        windowTitle = "Portforlio - Pitcher"
    ),
    
    hr(),
    
    sidebarPanel(width = 4,
                 selectInput("Player", "Select a Player:", choices = c(choose = '', rownames(table(pbp$name))), selectize = TRUE),
                 radioButtons("Stand", "Batter Stand", choices = c("ALL", rownames(table(pbp$stand))), inline = TRUE, selected = "ALL"),
                 radioButtons(inputId = "Pitch_Type", label = "Pitch Type",
                              choices = c("All" = "all", 
                                          "Change Up (CH)" = "CH", 
                                          "Curve ball (CU)" = "CU", 
                                          "Cutter (FC)" = "FC",
                                          "Four-Seam Fastball (FF)" = "FF",
                                          "Forkball (FO)" = "FO",
                                          "Splitter (FS)" = "FS",
                                          "Two-Seam Fastball (FT)" = "FT",
                                          "Knuckle-curve (KC)" = "KC",
                                          "Knuckleball (KN)" = "KN",
                                          "Sinker (SI)" = "SI",
                                          "Slider (SL)" = "SL"), selected = "all")
    ),
    
    br(),
    
    mainPanel(width = 8,
              tabsetPanel(
                  tabPanel("Pitch Type", plotOutput("myPlot"))
              )
    )
)


server = function(input, output){
    
    output$myPlot = renderPlot({
        # Strike Zone
        Top = 3.5
        Bot = 1.6
        Left = -0.95
        Right = 0.95
        StrikeZone = data.frame(
            x=c(Left, Left, Right, Right, Left),
            y=c(Bot, Top, Top, Bot, Bot))
        
        # Batter Stand
        player = pbp[pbp$name == input$Player, ]
        pitchtype = player[player$pitch_type == input$Pitch_Type, ]
        
        player_L = pbp_L[pbp_L$name == input$Player, ]
        pitchtype_L = player_L[player_L$pitch_type == input$Pitch_Type, ]
        
        player_R = pbp_R[pbp_R$name == input$Player, ]
        pitchtype_R = player_R[player_R$pitch_type == input$Pitch_Type, ]
        
        # Plot Color
        pt_color = c("CH" = '#1DBE3A', "CU" = '#00D1ED', "FC" = '#933F2C', 
                     "FF" = '#D22D49', "FO" = '#55CCAB', "FS" = '#888888', 
                     "FT" = '#DE6A04', "KC" = '#6236CD', "KN" = '#888888', 
                     "SI" = '#FE9D00', "SL" = '#EEE716')
        
        # Plot function
        plot_normal = function(PLAYER){
            ggplot(PLAYER, aes(x = -plate_x,y = plate_z, color = pitch_type))+
                geom_point(data = PLAYER, size = 5) +
                geom_path(aes(x, y),data = StrikeZone, lwd = 1, col = "black") +
                xlim(-5,5) + ylim(-2,6)+
                scale_color_manual(values = pt_color)+
                ggtitle(input$Player)+
                theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_rect(fill = NA), 
                      panel.border = element_rect(color = "black", fill = NA, size = 1))+
                theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                      axis.text.x = element_blank(), axis.text.y = element_blank(),
                      axis.ticks = element_blank())+
                theme(legend.position = c(0.91,0.80), legend.title = element_blank(), 
                      legend.text = element_text(size = 15), legend.key.size = unit(1, "cm"), 
                      legend.key = element_blank(), legend.box.background = element_rect())
        }
        
        
        plot_pitchtype = function(PLAYER, PITCHTYPE){
            ggplot(PLAYER, aes(-plate_x,plate_z, color = pitch_type))+
                geom_point(data = PLAYER, size = 5,alpha = 0.1)+
                geom_point(data = PITCHTYPE, size = 5)+
                geom_path(aes(x, y),data = StrikeZone, lwd = 1, col = "black")+
                xlim(-5,5) + ylim(-2,6)+
                scale_color_manual(values = pt_color)+
                ggtitle(input$Player)+
                theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), 
                      panel.border = element_rect(color = "black", fill = NA, size = 1))+
                theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                      axis.text.x = element_blank(), axis.text.y = element_blank(),
                      axis.ticks = element_blank())+
                theme(legend.position = c(0.90,0.80), legend.title = element_blank(), 
                      legend.text = element_text(size = 15), legend.key.size = unit(1, "cm"),
                      legend.key = element_blank(), legend.box.background = element_rect())
        }
        
        # Stand: ALL
        if(input$Stand == "ALL"){
            if(input$Pitch_Type == "all"){plot_normal(player)}
            else{plot_pitchtype(player, pitchtype)}}
        # Stand: L
        else if(input$Stand == "L"){
            if(input$Pitch_Type == "all"){plot_normal(player_L)}
            else{plot_pitchtype(player_L, pitchtype_L)}}
        #Stand: R 
        else if(input$Stand == "R"){
            if(input$Pitch_Type == "all"){plot_normal(player_R)}
            else{plot_pitchtype(player_R, pitchtype_R)}}
        
    }, height = 650, width = 500)
    
    output
}

# Run the application 
shinyApp(ui = ui, server = server)
