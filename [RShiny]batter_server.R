library(shiny)
library(datasets)
library(ggplot2)
library(dplyr)

# Load Data
tra = read.csv("trajectory.csv", head = T)
tra$events = ifelse(tra$events == "grounded_into_double_play", "GIDP", 
                    ifelse(tra$events == "sac_fly_double_play", "double_play", tra$events))
tra$events = factor(tra$events, levels = c("single", "double", "triple", "home_run",  
                                           "field_out", "force_out", "double_play", "GDIP", "triple_play",
                                           "fielders_choice_out", "field_error", "fielders_choice",
                                           "sac_bunt", "sac_fly"))
tra = na.omit(tra)
inplay = tra[tra$events == "single" | tra$events == "double" | 
                 tra$events == "triple" | tra$events == "home_run", ]



# Server
shinyServer(function(input, output) {
    
    # Spray Chart
    output$spray = renderPlot({
        
        all_events = tra[tra$name == input$Player, ]
        player = inplay[inplay$name == input$Player, ]
        
        hit1 = all_events[all_events$events == input$Event, ]
        hit2 = player[player$events == input$Event, ]
        
        # Plot
        park = data.frame(
            x = c(-40, 125, 290),
            y = c(75, 200, 75))
        
        
        
        
        
        pt_color = c("single" = '#FE6100', "double" = '#785EF0', 
                     "triple" = '#FFB000', "home_run" = '#D02478',
                     "double_play" = '#C2C2C2', "field_out" = '#C2C2C2', "fielders_choice_out" = '#C2C2C2',
                     "force_out" = '#C2C2C2', "GIDP" = '#C2C2C2', "triple_play" = '#C2C2C2',
                     "field_error" = '#F0F0F0', "fielders_choice" = '#F0F0F0', 
                     "sac_bunt" = '#F0F0F0', "sac_fly" = '#F0F0F0')
        
        # Plot function
        spray_all = function(PLAYER){
            ggplot(PLAYER, aes(hc_x, hc_y, color = events)) +
                geom_point(size = 5) +
                xlim(-100, 350) + ylim(250, -50) +
                scale_color_manual(values = pt_color) + 
                geom_path(aes(x, y), data = park, lwd = 0.5, col = "black", alpha = 0.8)+
                ggtitle(input$Player)+
                theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_rect(fill = NA), 
                      panel.border = element_rect(color = "black", fill = NA, size = 1))+
                theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                      axis.text.x = element_blank(), axis.text.y = element_blank(),
                      axis.ticks = element_blank())+
                theme(legend.position = c(0.85,0.83), legend.title = element_blank(), 
                      legend.text = element_text(size = 15), legend.key.size = unit(0.5, "cm"), 
                      legend.key = element_blank())
        }
        
        spray_hit = function(PLAYER, HIT){
            ggplot(PLAYER, aes(hc_x, hc_y, color = events)) +
                geom_point(size = 5, alpha = 0.1) +
                geom_point(data = HIT, size = 5) + 
                xlim(-100, 350) + ylim(250, -50) +
                scale_color_manual(values = pt_color) + 
                geom_path(aes(x, y), data = park, lwd = 0.5, col = "black", alpha = 0.8)+
                ggtitle(input$Player)+
                theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"))+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_rect(fill = NA), 
                      panel.border = element_rect(color = "black", fill = NA, size = 1))+
                theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
                      axis.text.x = element_blank(), axis.text.y = element_blank(),
                      axis.ticks = element_blank())+
                theme(legend.position = c(0.85,0.83), legend.title = element_blank(), 
                      legend.text = element_text(size = 15), legend.key.size = unit(0.5, "cm"), 
                      legend.key = element_blank())
        }
        
        # ggplot
        if(input$Inplay == "all"){
            if(input$Event == "all"){spray_all(all_events)}
            else{spray_hit(all_events, hit1)}}
        else if(input$Inplay == "hit"){
            if(input$Event == "all"){spray_all(player)}
            else{spray_hit(player, hit2)}}
        

    }, height = 600, width = 600)

})
