library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(viridis)

for(n in seq(30, 90, 1)){
    
    #n = 20
    
    message(n)
    
    set.seed(5)
    g <-
        erdos.renyi.game(n = n,
                         p.or.m = n * 9,
                         type = "gnm")
    
    
    set.seed(25)
    g_g <-
        g %>%
        ggraph(layout = 'dh') + 
        # geom_edge_link() + 
        geom_node_point(aes(), size = 3)
    
    g_g_df <-
        g_g$data
    
    
    dfs <- nrow(g_g_df)
    
    rep_x <- c()
    rep_y <- c()
    
    vert_mov <- c()
    lat_mov <- c()
    
    direction <- c()
    
    push_base_x <-
        (max(g_g_df$x) - min(g_g_df$x)) / 35
    
    push_base_y <-
        (max(g_g_df$y) - min(g_g_df$y)) / 35
    
    
    for(i in 1:(dfs - 1)){
        
        push <- FALSE
        
        ### Get points
        
        from_x <-
            g_g_df$x[i]
        
        from_y <-
            g_g_df$y[i]
        
        to_x <-
            g_g_df$x[i + 1]
        
        to_y <-
            g_g_df$y[i + 1]
        
        
        
        ### Add origin
        
        rep_x <-
            append(rep_x, from_x)
        
        rep_y <-
            append(rep_y, from_y)
        
        
        #####################
        # Movement
        #####################
        
        ### Which movement is bigger
        
        move_x <-
            abs(from_x - to_x)
        
        move_y <-
            abs(from_y - to_y)
        
        
        ### Assign real movement
        
        vert_mov[i] <-
            ifelse(from_y < to_y, 'up', 'down')
        
        lat_mov[i] <-
            ifelse(from_x < to_x, 'left', 'right')
        
        
        
        ### Movement logic
        
        ################### Start ###################
        
        if(i == 1){
            
            movement <-
                ifelse(move_x >= move_x, 'vert', 'lat')
            
            # message(i, 'start')
            
        }
        
        
        ################### Same and opposites ###################
        
        # If repeat direction, alternate
        else if( vert_mov[i - 1] == vert_mov[i] & lat_mov[i - 1] == lat_mov[i] ){
            
            movement <-
                ifelse(direction[length(direction)] == 'lat', 'vert', 'lat')
            
            # message(i, 'repeat',
            #         'v', vert_mov[i - 1], vert_mov[i],
            #         'l', lat_mov[i - 1], lat_mov[i],
            #         'past', direction[length(direction)])
            
        }
        
        # If opposite direction (Up and Left -> Down and Right), repeat
        else if( vert_mov[i - 1] != vert_mov[i] & lat_mov[i - 1] != lat_mov[i] ){
            
            movement <-
                ifelse(direction[length(direction)] == 'lat', 'lat', 'vert')
            
            # message(i, 'opposite',
            #         'v', vert_mov[i - 1], vert_mov[i],
            #         'l', lat_mov[i - 1], lat_mov[i],
            #         'past', direction[length(direction)])
            
        }
        
        
        ################### Same and opposites ###################
        
        # If past was lateral and consistent vertical movement (up and up, down and down)
        else if( direction[i - 1] == 'lat' & vert_mov[i - 1] == vert_mov[i] ){
            
            movement <- 'vert'
            
            # message(i, 'lateral and same vertical',
            #         'v', vert_mov[i - 1], vert_mov[i],
            #         'l', lat_mov[i - 1], lat_mov[i],
            #         'past', direction[length(direction)])
            
        }
        
        # If past was vertical and consistent lateral movement (left and left, right and right)
        else if( direction[i - 1] == 'ver' & lat_mov[i - 1] == lat_mov[i] ){
            
            movement <- 'lat'
            
            # message(i, 'vertical and same lateral',
            #         'v', vert_mov[i - 1], vert_mov[i],
            #         'l', lat_mov[i - 1], lat_mov[i],
            #         'past', direction[length(direction)])
            
        }
        
        
        
        # If past was lateral and opposite vertical movement (up and down, down and up)
        else if( direction[i - 1] == 'lat' & vert_mov[i - 1] != vert_mov[i] ){
            
            movement <- 'lat'
            
            # message(i, 'lateral and dif vertical',
            #         'v', vert_mov[i - 1], vert_mov[i],
            #         'l', lat_mov[i - 1], lat_mov[i],
            #         'past', direction[length(direction)])
            
            if(i %% 2 == 0){
                
                push <- TRUE
                # message('PUSH')
                
            }
            
        }
        
        # If past was vertical and opposite lateral movement (left and right, right and left)
        else if( direction[i - 1] == 'ver' & lat_mov[i - 1] != lat_mov[i] ){
            
            movement <- 'vert'
            
            # message(i, 'vertical and dif lateral',
            #         'v', vert_mov[i - 1], vert_mov[i],
            #         'l', lat_mov[i - 1], lat_mov[i],
            #         'past', direction[length(direction)])
            
            if(i %% 2 == 0){
                
                push <- TRUE
                # message('PUSH')
                
            }
            
        }
        
        
        
        # Alternate
        else{
            
            movement <-
                ifelse(direction[length(direction)] == 'lat', 'lat', 'vert')
            
            # message(i, 'alternating',
            #         'v', vert_mov[i - 1], vert_mov[i],
            #         'l', lat_mov[i - 1], lat_mov[i],
            #         'past', direction[length(direction)])
            
            if(i %% 2 == 0){
                
                push <- TRUE
                # message('PUSH')
                
            }
            
        }
        
        
        ### Assign results
        
        direction <-
            append(direction, movement)
        
        ################ Edge push #############
        
        if(i != 1){
            
            if(push){
                
                ### First point
                
                if(movement == 'lat'){
                    
                    ### First point
                    
                    append_x1 <- from_x
                    
                    ###
                    temp_dif_y1 <-
                        abs(g_g_df$y[i - 1] - g_g_df$y[i])
                    
                    temp_dif_y2 <-
                        abs(g_g_df$y[i] - g_g_df$y[i + 1])
                    
                    
                    if( vert_mov[i - 1] == vert_mov[i] ){
                        append_y1 <-
                            (g_g_df$y[i] + g_g_df$y[i + 1]) / 2
                    }
                    
                    else if(vert_mov[i] == 'up'){
                        append_y1 <-
                            #g_g_df$y[i] - min(temp_dif_y1, temp_dif_y2)
                            g_g_df$y[i] - push_base_y
                    }
                    
                    else if(vert_mov[i] == 'down'){
                        append_y1 <-
                            #g_g_df$y[i] + min(temp_dif_y1, temp_dif_y2)
                            g_g_df$y[i] + push_base_y
                    }
                    
                    
                    
                    ### Second point
                    
                    append_x2 <- to_x
                    
                    append_y2 <- append_y1
                    
                }
                
                
                if(movement == 'vert'){
                    
                    
                    ### First point
                    
                    append_y1 <- from_y
                    
                    ###
                    temp_dif_x1 <-
                        abs(g_g_df$x[i - 1] - g_g_df$x[i])
                    
                    temp_dif_x2 <-
                        abs(g_g_df$x[i] - g_g_df$x[i + 1])
                    
                    
                    if(lat_mov[i - 1] == lat_mov[i]){
                        append_x1 <-
                            (g_g_df$x[i] + g_g_df$x[i + 1]) / 2
                    }
                    
                    else if(lat_mov[i] == 'right'){
                        append_x1 <-
                            #g_g_df$x[i] + min(temp_dif_x1, temp_dif_x2)
                            g_g_df$x[i] + push_base_x
                    }
                    
                    else if(lat_mov[i] == 'left'){
                        append_x1 <-
                            #g_g_df$x[i] - min(temp_dif_x1, temp_dif_x2)
                            g_g_df$x[i] - push_base_x
                    }
                    
                    
                    
                    ### Second point
                    
                    append_y2 <- to_y
                    
                    append_x2 <- append_x1
                    
                }
                
                
                ### Assign
                
                rep_x <-
                    append(rep_x, c(append_x1, append_x2))
                
                rep_y <-
                    append(rep_y, c(append_y1, append_y2))
                
                
            }
            
        }
        
        
        ### Add regular point
        
        if( !push ){
            
            ### Get and add intermediate points
            
            inter_x <-
                ifelse(movement == 'lat', to_x, from_x)
            
            inter_y <-
                ifelse(movement == 'vert', to_y, from_y)
            
            rep_x <-
                append(rep_x, inter_x)
            
            rep_y <-
                append(rep_y, inter_y)
            
        }
        
        
        
        ### Add last point
        #
        #if( i == ( dfs - 1) ){
        #    
        #    rep_x <-
        #        append(rep_x, g_g_df$x[dfs])
        #    
        #    rep_y <-
        #        append(rep_y, g_g_df$y[dfs])
        #    
        #}
        
    }
    
    
    flow_df <-
        data_frame(x = rep_x,
                   y = rep_y)
    
    flow_df <- flow_df[1:nrow(flow_df) - 1, ]
    
    final_data <-
        g_g$data
    
    final_data <- final_data[1:nrow(final_data) - 1, ]
    
    temp_graph <-
        ggplot() +
        geom_path(data = flow_df,
                  aes(x = x, y = y),
                  size = 1.2) +
        geom_point(data = final_data,
                   aes(x = x, y = y,
                       col = ggraph.index),
                   size = 10) +
        theme_void() +
        theme(legend.position="none") +
        # scale_color_gradient(low = '#A8CF37', high = '#71CDE5')
        scale_color_viridis()
    
    png(str_c('all_network_visualizations/', n, '.png'),
        width = 2000,
        height = 2000)
    
    # tiff(str_c('exports/', n, '.tiff'),
    #      width = 800,
    #      height = 800,
    #      res = 300)
    
    # tiff(str_c('exports/', n, '.tiff'),
    #      units = 'in',
    #      width = 5,
    #      height = 5,
    #      res = 300)
    
    print(temp_graph)
    
    dev.off()
}



