server <- function(input, output) {
  
  tab_Collateddata_1 <-  reactive({
    
    Collateddata%>%
      dplyr::filter(Name == input$Name) %>% 
      dplyr::filter(Competition == input$Competition)

  })  
  
  output$select_Name <-  renderUI({
    selectizeInput('Name', 'Select Name', choices = c("select" = "", unique(Collateddata$Name)))  
  }) 
  
  output$select_Competition <-  renderUI({
    inputName = as.character(input$Name)
    choice_Competition <- reactive({
      Collateddata %>% 
        dplyr::filter(Name == inputName) %>% 
        pull(Competition) %>% 
        as.character()
      
      
    })
    
    
    if (input$Report_Type == "Single Comp"){
    selectizeInput('Competition', 'Select Competition', choices = c("select" = "", choice_Competition()))  }
  })        
  
  
  
  
  
  
  Distance_filter_1 <-  reactive({
    if (input$checkbox1 == TRUE){
      a <- Collateddata%>%
        dplyr::filter(Name == input$Name) %>%
        dplyr::filter(!Distance %in% NA) %>%
        dplyr::filter(Distance >70)
    }else{
      a=NULL
    }
    
    if (input$checkbox2 == TRUE){
      b <- Collateddata%>%
        dplyr::filter(Name == input$Name) %>%
        dplyr::filter(!Distance %in% NA) %>%
        dplyr::filter(Distance >=68&Distance<70)
    }else{
      b=NULL
    } 
    if (input$checkbox3 == TRUE){
      c <- Collateddata%>%
        dplyr::filter(Name == input$Name) %>%
        dplyr::filter(!Distance %in% NA) %>%
        dplyr::filter(Distance >=66&Distance<68)
    }else{
      c=NULL
    } 
    if (input$checkbox4 == TRUE){
      d <- Collateddata%>%
        dplyr::filter(Name == input$Name) %>%
        dplyr::filter(!Distance %in% NA) %>%
        dplyr::filter(Distance >=64&Distance<66) 
    }else{
      d=NULL
    }
    
    combined = bind_rows(a,b,c,d)
    return(combined)
  })
  
  
  # Distance_filter_1 <-  reactive({
  #   if (input$Distance1 == ">70"){
  #     Collateddata%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(Distance >=70)
  #     
  #   }else if (input$Distance1 == "68-70"){
  #     Collateddata%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(Distance >=68&Distance<70)
  #     
  #   }else if (input$Distance1 == "66-68"){
  #     Collateddata%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(Distance >=66&Distance<68)
  #     
  #   }else if (input$Distance1 == "64-66"){
  #     Collateddata%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(Distance >=64&Distance<66) 
  #   }
  # })
  # 
  # Distance_filter_2 <-  reactive({
  #   if (input$Distance2 == ">70"){
  #     Collateddata%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(Distance >=70)
  #   }else if (input$Distance2 == "68-70"){
  #     Collateddata%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(Distance >=68&Distance<70)
  #     
  #   }else if (input$Distance2 == "66-68"){
  #     Collateddata%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(Distance >=66&Distance<68)
  #     
  #   }else if (input$Distance2 == "64-66"){
  #     Collateddata%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(Distance >=64&Distance<66) 
  #   }
  # 
  # })
  

  
  ##### Tables ########
  output$datatable1 <-  DT::renderDataTable({
    
    if (input$Report_Type == "Distance Comparison"){
      datatable1 <- Distance_filter_1() %>% dplyr::select(Name, Competition, Round, Distance, AngleAvg, HVelAvg, RVelAvg, `Turn 1 Single`, `Turn 1 Double`, 
                                                                   `Turn 2 Single`, `Turn 2 Double`,  `Turn 3 Single`, `Turn 3 Double`,
                                                                   `Turn 4 Single`, `Turn 4 Double`)
      # datatable_Distance2 <- Distance_filter_2() %>% dplyr::select(Name, Competition, Round, Distance, AngleAvg, HVelAvg, RVelAvg, `Turn 1 Single`, `Turn 1 Double`, 
      #                                                              `Turn 2 Single`, `Turn 2 Double`,  `Turn 3 Single`, `Turn 3 Double`,
      #                                                              `Turn 4 Single`, `Turn 4 Double`)
      
      # datatable1 = bind_rows(datatable_Distance1,datatable_Distance2)
      ({datatable(datatable1, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "Release Angle (deg)", "Hor Release Angle (deg)", "Release Vel (m/s)", "Turn 1 Single (s)",
                                         "Turn 1 Double (s)", "Turn 2 Single (s)", "Turn 2 Double (s)", "Turn 3 Single (s)", "Turn 3 Double (s)", "Turn 4 Single (s)", "Double to Release (s)"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),  order=list(list(3, 'desc')), scrollX = TRUE))})
    } else {
      
      datatable1 <- tab_Collateddata_1() %>% dplyr::select(Name, Competition, Round, Distance, AngleAvg, HVelAvg, RVelAvg, `Turn 1 Single`, `Turn 1 Double`, 
                                                           `Turn 2 Single`, `Turn 2 Double`,  `Turn 3 Single`, `Turn 3 Double`,
                                                           `Turn 4 Single`, `Turn 4 Double`)
      datatable1[is.na(datatable1)] <- "X"
      ({datatable(datatable1, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "Release Angle (deg)", "Hor Release Angle (deg)", "Release Vel (m/s)", "Turn 1 Single (s)",
                                         "Turn 1 Double (s)", "Turn 2 Single (s)", "Turn 2 Double (s)", "Turn 3 Single (s)", "Turn 3 Double (s)", "Turn 4 Single (s)", "Double to Release (s)"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),  dom='t',ordering=F, scrollX = TRUE))})
    }
  })
  
  
  output$datatable2 <-  DT::renderDataTable({
    
    if (input$Report_Type == "Distance Comparison"){
      datatable2 <- Distance_filter_1() %>% dplyr::select(Name, Competition, Round, Distance, `Turn 1`, `Turn 2`, `Turn 3`, `Turn 4`, TotalTime)
      # datatable_Distance2 <- Distance_filter_2() %>% dplyr::select(Name, Competition, Round, Distance, `Turn 1`, `Turn 2`, `Turn 3`, `Turn 4`, TotalTime)
      
      # datatable2 = bind_rows(datatable_Distance1,datatable_Distance2)
      ({datatable(datatable2, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "Turn 1 (s)", "Turn 2 (s)", "Turn 3 (s)", "Turn 4 (s)", "Total (s)"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),  order=list(list(3, 'desc')), scrollX = TRUE))})
      
      
    } else {
    datatable2 <- tab_Collateddata_1() %>% dplyr::select(Name, Competition, Round, Distance, `Turn 1`, `Turn 2`, `Turn 3`, `Turn 4`, TotalTime)
    
    datatable2[is.na(datatable2)] <- "X"
    ({datatable(datatable2, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "Turn 1 (s)", "Turn 2 (s)", "Turn 3 (s)", "Turn 4 (s)", "Total (s)"), 
                options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),   dom='t',ordering=F, scrollX = TRUE))})
    }
  })
  
  
  
  
  output$datatable3 <-  DT::renderDataTable({
    if (input$Report_Type == "Distance Comparison"){
    
      datatable3 <- Distance_filter_1() %>% dplyr::select(Name, Competition, Round, Distance, DUTurn1, DUTurn2, DUTurn3, DUTurn4)
      # datatable_Distance2 <- Distance_filter_2() %>% dplyr::select(Name, Competition, Round, Distance, DUTurn1, DUTurn2, DUTurn3, DUTurn4)
      # 
      # datatable3 = bind_rows(datatable_Distance1,datatable_Distance2)
      
      
     ({datatable(datatable3, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "Turn 1 (s)", "Turn 2 (s)", "Turn 3 (s)", "Turn 4 (s)"), 
                 options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")), order=list(list(3, 'desc')), scrollX = TRUE))})
      
    }else{
    datatable3 <- tab_Collateddata_1() %>% dplyr::select(Name, Competition, Round, Distance, DUTurn1, DUTurn2, DUTurn3, DUTurn4)
      
    
    
    datatable3[is.na(datatable3)] <- "X"
    ({datatable(datatable3, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "Turn 1 (s)", "Turn 2 (s)", "Turn 3 (s)", "Turn 4 (s)"), 
                options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),  dom='t',ordering=F, scrollX = TRUE))})
    }
    })
  
  


  
  
  ##### GGPlots #####
  
  output$ggplot1 <-  renderPlot({
    if (input$Report_Type == "Distance Comparison"){
      
      Plot_Distance1 <-  Distance_filter_1() %>% dplyr::select(`Distance`, `Round&Distance`, `Distance`, `Turn1Single`, `Turn1Double`, 
                                                       `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
                                                       `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Distance", "Round&Distance"))
      # Plot_Distance2 <-  Distance_filter_2() %>% dplyr::select(`Distance`, `Round&Distance`,`Distance`, `Turn1Single`, `Turn1Double`, 
      #                                                 `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
      #                                                 `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Distance", "Round&Distance"))
      
      Plot1 <- bind_rows(Plot_Distance1)

      nb.cols <- n_distinct(Plot1$`Round&Distance`)
      mycolors <- colorRampPalette(brewer.pal(8, "Greens"))(nb.cols)
      
      ggplot(data=Plot1, aes(x=variable, y=value, fill=reorder(`Round&Distance`, `Distance`))) + geom_bar(stat="identity",position=position_dodge()) + 
        scale_fill_manual(values = mycolors) + scale_x_discrete(labels = c("Turn 1 Single", "Turn 1 Double", "Turn 2 Single", "Turn 2 Double","Turn 3 Single", "Turn 3 Double","Turn 4 Single", "Turn 4 Double")) +
        scale_y_continuous("time (s)") + theme(axis.title.x = element_blank())+ labs(fill = "Round & Distance (m)")
  }else {
      
      Plot1 <-  tab_Collateddata_1() %>% dplyr::select(`Round&Distance`, `Turn1Single`, `Turn1Double`, 
                                    `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
                                    `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Round&Distance"))
  
  ggplot(data=Plot1, aes(x=variable, y=value, fill=`Round&Distance`)) + geom_bar(stat="identity",position=position_dodge()) + scale_fill_brewer(palette = "Greens") +
    scale_y_continuous("time (s)") + scale_x_discrete(labels = c("Turn 1 Single", "Turn 1 Double", "Turn 2 Single", "Turn 2 Double","Turn 3 Single", "Turn 3 Double","Turn 4 Single", "Turn 4 Double")) + 
    theme(axis.title.x = element_blank())+ labs(fill = "Round & Distance (m)")
  }
  })
  
  output$ggplot2 <-  renderPlot({
    if (input$Report_Type == "Distance Comparison"){
      Plot_Distance1 <-  Distance_filter_1() %>% dplyr::select(`Distance`, `Round&Distance`, `Turn1Single`, `Turn1Double`, 
                                                               `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
                                                               `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Distance", "Round&Distance"))
        
      # Plot_Distance2 <-  Distance_filter_2() %>% dplyr::select(`Distance`, `Round&Distance`, `Turn1Single`, `Turn1Double`, 
      #                                                          `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
      #                                                          `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Distance", "Round&Distance"))

      
      Plot2 <- bind_rows(Plot_Distance1) 

      
      ggplot(data=Plot2, aes(x=reorder(`Round&Distance`,-`Distance`), y=value, fill=`variable`)) + geom_bar(stat="identity")  + 
        scale_fill_manual(name = "Turn", labels = c("Turn 1 Single", "Turn 1 Double", "Turn 2 Single", "Turn 2 Double","Turn 3 Single", "Turn 3 Double","Turn 4 Single", "Turn 4 Double"),values=c("orange","green4", "orange","green4", "orange", "green4", "orange", "green4")) +
        scale_x_discrete("Round & Distance (m)") +
        scale_y_continuous("Time (s)", breaks = seq(0, 2.4, 0.1)) + coord_flip()
      
      
    }else {
      
      Plot2 <-tab_Collateddata_1() %>% dplyr::select(`Round`, `Turn1Single`, `Turn1Double`, 
                                                     `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
                                                     `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Round"))
      
      ggplot(data=Plot2, aes(x=`Round`, y=value, fill=`variable`)) + geom_bar(stat="identity")  + 
        scale_fill_manual(name = "Turn", labels = c("Turn 1 Single", "Turn 1 Double", "Turn 2 Single", "Turn 2 Double","Turn 3 Single", "Turn 3 Double","Turn 4 Single", "Turn 4 Double"),values=c("orange","green4", "orange","green4", "orange", "green4", "orange", "green4")) +
        scale_x_continuous(breaks = c(1:6)) + scale_y_continuous("Time (s)", breaks = seq(0, 2.4, 0.1)) + coord_flip()
      
    }
  })
  
  output$ggplot3 <-  renderPlot({
    if (input$Report_Type == "Distance Comparison"){
      Plot_Distance1 <-  Distance_filter_1() %>% dplyr::select(`Distance`, `Round&Distance`, `DUTurn1`, `DUTurn2`, `DUTurn3`, `DUTurn4`) %>% 
        reshape2::melt(id = c("Distance","Round&Distance"))
      # Plot_Distance2 <-  Distance_filter_2() %>% dplyr::select(`Distance`, `Round&Distance`, `DUTurn1`, `DUTurn2`, `DUTurn3`, `DUTurn4`) %>% 
      #   reshape2::melt(id = c("Distance","Round&Distance"))
      
      Plot3 <- bind_rows(Plot_Distance1)
      nb.cols <- n_distinct(Plot3$`Round&Distance`)
      mycolors <- colorRampPalette(brewer.pal(8, "Greens"))(nb.cols)
      
      ggplot(data=Plot3, aes(x=variable, y=value, fill=reorder(`Round&Distance`, `Distance`))) + geom_bar(stat="identity",position=position_dodge()) + 
        scale_fill_manual(values = mycolors) + scale_x_discrete("Round", labels = c("Turn 1", "Turn 2", "Turn 3", "Turn 4")) +
        scale_y_continuous("time (s)")+ labs(fill = "Round & Distance (m)")
      
    }else {
      
    
     
  Plot3 <- tab_Collateddata_1() %>% dplyr::select(`Round&Distance`, `DUTurn1`, `DUTurn2`, `DUTurn3`, `DUTurn4`) %>% reshape2::melt(id = c("Round&Distance"))
  ggplot(data=Plot3, aes(x=variable, y=value, fill=`Round&Distance`)) + geom_bar(stat="identity",position=position_dodge()) + scale_fill_brewer(palette = "Greens") + 
    scale_x_discrete("Round", labels = c("Turn 1", "Turn 2", "Turn 3", "Turn 4")) +
    scale_y_continuous("time (s)")+ labs(fill = "Round & Distance (m)")
    }
  })
  
  
  
  # Table2 <- tab_Collateddata() %>% dplyr::select(Name, Competition, Round, Distance, Turn1, Turn2, Turn3, Turn4, TotalTime)
  # 
  # 
  # 
  # Table3 <- tab_Collateddata() %>% dplyr::select(Name, Competition, Round, Distance, DUTurn1, DUTurn2, DUTurn3, DUTurn4)
  # 
  # 
  # 
  # 
  # 
  # 
  # Table1 <-  t(Table1)
  # Table1[is.na(Table1)] <- "X"
  # 
  # datatable(Table1)
  # 
  # 
  # Table2 <-  t(Table2)
  # Table2[is.na(Table2)] <- "X"
  # datatable(Table2)
  # 
  # Table3 <-  t(Table3)
  # Table3[is.na(Table3)] <- "X"
  # 
  # datatable(Table3)
  # 
  ##### GGPLOTS ######
  
}
