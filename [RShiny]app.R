library(shiny)
library(datasets)
library(plotly)
library(ggplot2)
library(rgl)

visual = read.csv("visualization.csv", head = TRUE)

ui = (fixedPage(
    
    
    tags$style(HTML('body {font-family:"Bahnschrift",Georgia,Serif; background-color:white}')),
    
    titlePanel(tagList(
        img(src = "baseball.png", height = 30, width = 30),
        span("Pitcher Visualization Report")), 
        windowTitle = "Pitcher Visualization Report"
    ),
    
    hr(style = "border-color: #399098"),
    
    
    fluidRow(
        sidebarPanel(
            selectInput("Player", "Select a Player:", 
                        choices = c(rownames(table(visual$name))), selectize = TRUE),
            radioButtons("Stand", "Batter Stand", 
                         choices = c("ALL", "L", "R"), inline = TRUE, selected = "ALL")
        ),
        mainPanel(
            tabsetPanel(tabPanel("Pitch Chart", plotlyOutput("pitch_chart"), 
                                 br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                 tableOutput("table1")),
                        tabPanel("Release Point 2d", br(),plotlyOutput("relpos2d"),
                                 br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()),
                        tabPanel("Release Point 3d", rglwidgetOutput(("relpos3d")))
            ),
            
        ))
    ,
    
    hr(),
    p('All of the data used to generate this app were obtained from baseballsavant.com')
    
    
)
)

server = function(input, output) {
    
    # 1. Pitch Chart
    output$pitch_chart <- renderPlotly({
        player = visual[visual$name == input$Player, ]
        player_L = visual[visual$name == input$Player & visual$batter_stand == "L", ]
        player_R = visual[visual$name == input$Player & visual$batter_stand == "R", ]
        
        # Strike Zone
        Top = 3.5; Bot = 1.6; Left = -0.95; Right = 0.95
        StrikeZone = data.frame(
            x = c(Left, Left, Right, Right, Left),
            y = c(Bot, Top, Top, Bot, Bot))
        
        # Home Plate
        Left2 = -0.95; Right2 = 0.95; Mid2 = 0; Top2 = 0; Bot2 = -0.7; Bot3 = -1.2
        HomePlate = data.frame(
            x = c(Left2, Right2, Right2, Mid2, Left2, Left2),
            y = c(Top2, Top2, Bot2, Bot3, Bot2, Top2)
        )
        
        # Customize Color
        pt_color = c("CHANGEUP" = '#1DBE3A', "CURVEBALL" = '#00D1ED', "CUTTER" = '#933F2C', 
                     "4-SEAM FASTBALL" = '#D22D49', "FORKBALL" = '#55CCAB', "SPLITTER" = '#888888', 
                     "2-SEAM FASTBALL" = '#DE6A04', "KNUCKLE CURVE" = '#6236CD', "KNUCKLEBALL" = '#888888', 
                     "SINKER" = '#FE9D00', "SLIDER" = '#EEE716')
        
        
        plot1_normal = function(PLAYER){
            p = ggplot(data = PLAYER) +
                geom_point(data = PLAYER, size = 4, aes(plate_x, plate_z, fill = pitch_name,
                                                        text = paste(pitch_name, 
                                                                     "<br>Speed: ", speed, "km/h",
                                                                     "<br>Result: ", description)),
                           pch = 21, color = "black", stroke = 0.3) +
                geom_path(data = StrikeZone, aes(x, y), lwd = 0.5, col = "black") +
                geom_path(data = HomePlate, aes(x, y), lwd = 0.5, col = "gray") + 
                xlim(-4,4) + ylim(-2,5) +
                scale_fill_manual(values = pt_color) + 
                theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold")) + 
                ggtitle(paste(input$Player, "(", rownames(table(player$p_throws)), ")")) + 
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_rect(fill = NA), 
                      panel.border = element_blank())+
                theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                      axis.text.x = element_blank(), axis.text.y = element_blank(),
                      axis.ticks = element_blank())+
                theme(legend.title = element_blank(), 
                      legend.text = element_text(size = 10), legend.key.size = unit(1, "cm"), 
                      legend.key = element_blank(), legend.box.background = element_rect())
            
            ggplotly(p, tooltip = "text", width = 600, height = 600)
        }
        
        # Plot
        if(input$Stand == "ALL"){
            plot1_normal(player)
        }else if(input$Stand == "L"){
            plot1_normal(player_L)
        }else{
            plot1_normal(player_R)
        }
        
        
    })
    
    # Pitch Chart Table 
    output$table1 = renderTable({
        player = visual[visual$name == input$Player, ]
        player_L = visual[visual$name == input$Player & visual$batter_stand == "L", ]
        player_R = visual[visual$name == input$Player & visual$batter_stand == "R", ]
        
        table1_normal = function(PLAYER){
            c1 = sort(table(PLAYER$pitch_name), decreasing = TRUE)
            c2 = rep("%", length(c1))
            table1 = data.frame(round(c1/nrow(PLAYER)*100, 2), c2)
            table1$Perc = paste(table1[,2], table1[,3])
            data.frame(table1$Var1, table1$Perc)
        }
        
        if(input$Stand == "ALL"){
            table1_normal(player)
        }else if(input$Stand == "L"){
            table1_normal(player_L)
        }else if(input$Stand == "R"){
            table1_normal(player_R)
        }
        
        
        
    }, colnames = FALSE)
    
    # 2. Release Point 2d
    output$relpos2d = renderPlotly({
        
        player = visual[visual$name == input$Player, ]
        player_L = visual[visual$name == input$Player & visual$batter_stand == "L", ]
        player_R = visual[visual$name == input$Player & visual$batter_stand == "R", ]
        
        pt_color = c("CHANGEUP" = '#1DBE3A', "CURVEBALL" = '#00D1ED', "CUTTER" = '#933F2C', 
                     "4-SEAM FASTBALL" = '#D22D49', "FORKBALL" = '#55CCAB', "SPLITTER" = '#888888', 
                     "2-SEAM FASTBALL" = '#DE6A04', "KNUCKLE CURVE" = '#6236CD', "KNUCKLEBALL" = '#888888', 
                     "SINKER" = '#FE9D00', "SLIDER" = '#EEE716')
        
        total_mean_z = mean(player$release_pos_z)
        total_mean_x = mean(player$release_pos_x)
        line = data.frame(x = c(total_mean_x, total_mean_x, total_mean_x - 1), y = c(0, total_mean_z, total_mean_z))
        
        plot2_normal = function(PLAYER2){
            p = ggplot(data = PLAYER2) +
                geom_path(data = line, aes(x, y), lwd = 0.1, color = "red") + 
                geom_point(data = PLAYER2, size = 2, aes(release_pos_x, release_pos_z, fill = pitch_name,
                                                        text = paste("Count: ", balls, "-", strikes)), stroke = 0.1) +
                xlim(-6, 6) + ylim(0, 8) +
                annotate("text", x = total_mean_x - 2, y = total_mean_z, label = paste(round(total_mean_z, 1), "ft"), color = "red") + 
                scale_fill_manual(values = pt_color) + 
                theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold")) + 
                ggtitle(paste(input$Player, "(", rownames(table(PLAYER2$p_throws)), ")")) + 
                theme_linedraw() + 
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
                theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                      axis.text.x = element_blank(), axis.text.y = element_blank(),
                      axis.ticks = element_blank())+
                theme(legend.title = element_blank(), 
                      legend.text = element_text(size = 10), legend.key.size = unit(1, "cm"), 
                      legend.key = element_blank(), legend.box.background = element_blank())
            
            ggplotly(p, tooltip = "text", width = 600, height = 600)
        }
        
        if(input$Stand == "ALL"){
            plot2_normal(player)
        }else if(input$Stand == "L"){
            plot2_normal(player_L)
        }else if(input$Stand == "R"){
            plot2_normal(player_R)
        }
        
    })
    
    # 3. Release Point 3d
    output$relpos3d = renderRglwidget({
        visual$pitch_color = ifelse(visual$pitch_name == "CHANGEUP", '#1DBE3A',
                                    ifelse(visual$pitch_name == "CURVEBALL", '#00D1ED',
                                           ifelse(visual$pitch_name == "CUTTER", '#933F2C',
                                                  ifelse(visual$pitch_name == "4-SEAM FASTBALL", '#D22D49',
                                                         ifelse(visual$pitch_name == "FORKBALL", '#55CCAB',
                                                                ifelse(visual$pitch_name == "SPLITTER", '#888888',
                                                                       ifelse(visual$pitch_name == "2-SEAM FASTBALL", '#DE6A04',
                                                                              ifelse(visual$pitch_name == "KNUCKLE CURVE", '#6236CD',
                                                                                     ifelse(visual$pitch_name == "KNUCKLEBALL", '#888888',
                                                                                            ifelse(visual$pitch_name == "SINKER", '#FE9D00',
                                                                                                   '#EEE716'))))))))))
        player = visual[visual$name == input$Player, ]
        player_L = visual[visual$name == input$Player & visual$batter_stand == "L", ]
        player_R = visual[visual$name == input$Player & visual$batter_stand == "R", ]
        
        plot3_normal = function(PLAYER3){
            plot3d(x = PLAYER3$release_pos_x, y = PLAYER3$release_pos_y, z = PLAYER3$release_pos_z,
                   col = PLAYER3$pitch_color,
                   type = 's',
                   radius = .03,
                   xlab = "", ylab = "", zlab = "height")
            par3d(windowRect = c(0,0,512,512))
            legend3d("topright", legend = c(rownames(sort(table(PLAYER3$pitch_name)))), col = rownames(sort(table(PLAYER3$pitch_color))), pch = 16)
            rglwidget()
        }
        
        if(input$Stand == "ALL"){
            plot3_normal(player)
        }else if(input$Stand == "L"){
            plot3_normal(player_L)
        }else if(input$Stand == "R"){
            plot3_normal(player_R)
        }
       
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
