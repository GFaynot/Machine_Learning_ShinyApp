# https://github.com/gadenbuie/tweet-conf-dash/blob/main/server.R
options(encoding="UTF-8")
function(session, input, output) {
  

  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  # ---------------------- Filters --------------------------
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  observeEvent(input$annee,{
    updateSelectInput(session,
                      inputId = "mois",
                      label = "Mois",
                      choices = c("All",as.character(sort(factor(unique(data$Month[year(data$date_invoice)==input$annee]), levels = month.name)))),
                      selected = "All")
    updateSelectInput(session,
                      inputId = "comparaison_indicateurs",
                      label = "Compared to ",
                      choices = sort(unique(year(data$date_invoice[year(data$date_invoice)<input$annee])), decreasing = TRUE),
                      selected = as.integer(input$annee)-1)
  })

  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  # ---------------- ValueBox : indicators --------------------
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  output$Indicateur_chiffre_affaires <- renderValueBox({
    if(input$mois=="All"){
      valueBox(subtitle = paste("Chiffre d'affaires en ",input$annee), value = paste(format(round(sum(data$net_sales[year(data$date_invoice)==input$annee]), 0), nsmall=0, big.mark=","),"€"), icon = icon("fa-solid fa-euro-sign"), color = "yellow")
    }else{
      valueBox(subtitle = paste("Chiffre d'affaires en ",input$annee), value = paste(format(round(sum(data$net_sales[year(data$date_invoice)==input$annee & data$Month==input$mois]), 0), nsmall=0, big.mark=","),"€"), icon = icon("fa-solid fa-euro-sign"),color = "yellow")
    }
  })
  output$Indicateur_marge <- renderValueBox({
    if(input$mois=="All"){
      valueBox(subtitle = paste("Marge brute en ",input$annee),value = paste(round((sum(data$Marge[year(data$date_invoice)==input$annee])/sum(data$net_sales[year(data$date_invoice)==input$annee]))*100, 0),"%"), icon = icon("fa-solid fa-euro-sign"),color = "orange")
    }else{
      valueBox(subtitle = paste("Marge brute en ",input$annee),value = paste(round((sum(data$Marge[year(data$date_invoice)==input$annee & data$Month==input$mois])/sum(data$net_sales[year(data$date_invoice)==input$annee & data$Month==input$mois]))*100, 0),"%"), icon = icon("fa-solid fa-euro-sign"),color = "orange")
    }

  })
  output$Indicateur_couts <- shinydashboard::renderValueBox({
    if(input$mois=="All"){
      shinydashboard::valueBox(subtitle = paste("Coût de revient en ",input$annee),value =  paste(format(round(sum(data$cost_sales[year(data$date_invoice)==input$annee]), 0), nsmall=0, big.mark=","),"€"), icon = icon("fa-solid fa-euro-sign"),color = "orange")
    }else{
      shinydashboard::valueBox(subtitle = paste("Coût de revient en ",input$annee),value =  paste(format(round(sum(data$cost_sales[year(data$date_invoice)==input$annee & data$Month==input$mois]), 0), nsmall=0, big.mark=","),"€"), icon = icon("fa-solid fa-euro-sign"),color = "orange")
    }
  })
  output$Indicateur_commandes <- shinydashboard::renderValueBox({
    
    df_ca_moyen <- aggregate(list(CA.moyen.commande = data[year(data$date_invoice)==input$annee,"net_sales"]),
                             by=list(Date = as.Date(data$date_invoice_month[year(data$date_invoice)==input$annee]), Commande = data$N.invoice[year(data$date_invoice)==input$annee]),sum)

    df <-data.frame(aggregate(list(CA.moyen.commande = df_ca_moyen$CA.moyen.commande),
                           by=list(Date = df_ca_moyen$Date),mean) %>% 
                   mutate(Month = factor(month.name[month(Date)],levels=month.name)))
    
    if(input$mois=="All"){
      shinydashboard::valueBox(subtitle = paste("Chiffre d'affaires moyen par commande en ",input$annee),value = paste(format(round(mean(df$CA.moyen.commande), 0), nsmall=0, big.mark=","),"€"), icon = icon("fal fa-box-open"),color = "orange")
    }else{
      shinydashboard::valueBox(subtitle = paste("Chiffre d'affaires moyen par commande en ",input$annee),value = paste(format(round(df$CA.moyen.commande[df$Month==input$mois], 0), nsmall=0, big.mark=","),"€") , icon = icon("fal fa-box-open"),color = "orange")
    }
  })


  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  # ----------------- InfoBox : evolution ---------------------
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  output$Evolution_chiffre_affaires <- renderInfoBox({
    if(input$mois=="All"){
      if (input$annee==year(Sys.Date())){
        ind <- paste(format(round((sum(data$net_sales[year(data$date_invoice)==input$annee])-sum(data$net_sales[year(data$date_invoice)==input$comparaison_indicateurs & month(data$date_invoice)<=month(Sys.Date())]))/sum(data$net_sales[year(data$date_invoice)==input$comparaison_indicateurs & month(data$date_invoice)<=month(Sys.Date())])*100,1), nsmall=0, big.mark=","),"%")
      }else{
        ind <- paste(format(round((sum(data$net_sales[year(data$date_invoice)==input$annee])-sum(data$net_sales[year(data$date_invoice)==input$comparaison_indicateurs]))/sum(data$net_sales[year(data$date_invoice)==input$comparaison_indicateurs])*100,1), nsmall=0, big.mark=","),"%")
      }
    }else{
      ind <- paste(format(round((sum(data$net_sales[year(data$date_invoice)==input$annee & data$Month==input$mois])-sum(data$net_sales[year(data$date_invoice)==input$comparaison_indicateurs & data$Month==input$mois]))/sum(data$net_sales[year(data$date_invoice)==input$comparaison_indicateurs & data$Month==input$mois])*100,1), nsmall=0, big.mark=","),"%")
    }
    if(ind<0){
      col <- "red"
      img <- "far fa-arrow-circle-down"
    }else{
      col <- "green"
      img <- "far fa-arrow-circle-up"
    }
    infoBox(title = "Evolution du CA",
            value = ind,
            icon = icon(img,verify_fa = FALSE),
            color = col
    )
  })

  output$Evolution_marge <- renderInfoBox({
    if(input$mois=="All"){
      if (input$annee==year(Sys.Date())){
        ind <- paste(format(round((sum(data$Marge[year(data$date_invoice)==input$annee])/sum(data$net_sales[year(data$date_invoice)==input$annee])-sum(data$Marge[year(data$date_invoice)==input$comparaison_indicateurs & month(data$date_invoice)<=month(Sys.Date())])/sum(data$net_sales[year(data$date_invoice)==input$comparaison_indicateurs & month(data$date_invoice)<=month(Sys.Date())]))*100,1), nsmall=0, big.mark=","),"%")
      }else{
        ind <- paste(format(round((sum(data$Marge[year(data$date_invoice)==input$annee])/sum(data$net_sales[year(data$date_invoice)==input$annee])-sum(data$Marge[year(data$date_invoice)==input$comparaison_indicateurs])/sum(data$net_sales[year(data$date_invoice)==input$comparaison_indicateurs]))*100,1), nsmall=0, big.mark=","),"%")
      }

    }else{
      ind <- paste(format(round((sum(data$Marge[year(data$date_invoice)==input$annee & data$Month==input$mois])/sum(data$net_sales[year(data$date_invoice)==input$annee & data$Month==input$mois])-sum(data$Marge[year(data$date_invoice)==input$comparaison_indicateurs & data$Month==input$mois])/sum(data$net_sales[year(data$date_invoice)==input$comparaison_indicateurs & data$Month==input$mois]))*100,1), nsmall=0, big.mark=","),"%")
      
    }

    if(ind<0){
      col <- "red"
      img <- "far fa-arrow-circle-down"
    }else{
      col <- "green"
      img <- "far fa-arrow-circle-up"
    }
    infoBox(title = "Evolution de la marge",
            value = ind,
            icon = icon(img,verify_fa = FALSE),
            color = col
    )
  })
  output$Evolution_couts <- renderInfoBox({
    if(input$mois=="All"){
      if (input$annee==year(Sys.Date())){
        ind <- paste(format(round((sum(data$cost_sales[year(data$date_invoice)==input$annee])-sum(data$cost_sales[year(data$date_invoice)==input$comparaison_indicateurs & month(data$date_invoice)<=month(Sys.Date())]))/sum(data$cost_sales[year(data$date_invoice)==input$comparaison_indicateurs & month(data$date_invoice)<=month(Sys.Date())])*100,1), nsmall=0, big.mark=","),"%")
      }else{
        ind <- paste(format(round((sum(data$cost_sales[year(data$date_invoice)==input$annee])-sum(data$cost_sales[year(data$date_invoice)==input$comparaison_indicateurs]))/sum(data$cost_sales[year(data$date_invoice)==input$comparaison_indicateurs])*100,1), nsmall=0, big.mark=","),"%")
      }

    }else{
      ind <- paste(format(round((sum(data$cost_sales[year(data$date_invoice)==input$annee & data$Month==input$mois])-sum(data$cost_sales[year(data$date_invoice)==input$comparaison_indicateurs & data$Month==input$mois]))/sum(data$cost_sales[year(data$date_invoice)==input$comparaison_indicateurs & data$Month==input$mois])*100,1), nsmall=0, big.mark=","),"%")

    }
    if(ind<0){
      col <- "red"
      img <- "far fa-arrow-circle-down"
    }else{
      col <- "green"
      img <- "far fa-arrow-circle-up"
    }
    infoBox(title = "Evolution des coûts",
            value = ind,
            icon = icon(img,verify_fa = FALSE),
            color = col
    )
  })
  output$Evolution_commandes <- renderInfoBox({
    df_ca_moyen <- aggregate(list(CA.moyen.commande = data[year(data$date_invoice)==input$annee,"net_sales"]),
                             by=list(Date = as.Date(data$date_invoice_month[year(data$date_invoice)==input$annee]), Commande = data$N.invoice[year(data$date_invoice)==input$annee]),sum)
    df_ca_moyen_compare <- aggregate(list(CA.moyen.commande = data[year(data$date_invoice)==input$comparaison_indicateurs,"net_sales"]),
                                     by=list(Date = as.Date(data$date_invoice_month[year(data$date_invoice)==input$comparaison_indicateurs]), Commande = data$N.invoice[year(data$date_invoice)==input$comparaison_indicateurs]),sum)
    
    df <- left_join(
      data.frame(aggregate(list(CA.moyen.commande = df_ca_moyen$CA.moyen.commande),
                           by=list(Date = df_ca_moyen$Date),mean) %>% 
                   mutate(Month = factor(month.name[month(Date)],levels=month.name))),
      data.frame(aggregate(list(CA.moyen.commande.compare = df_ca_moyen_compare$CA.moyen.commande),
                           by=list(Date.compare = df_ca_moyen_compare$Date),mean) %>% 
                   mutate(Month = factor(month.name[month(Date.compare)],levels=month.name))),
      by=c("Month"="Month"))
    
    if(input$mois=="All"){
      ind <- paste(round(((mean(df$CA.moyen.commande)-mean(df$CA.moyen.commande.compare))/mean(df$CA.moyen.commande.compare))*100,1),"%")
    }else{
      ind <- paste(round(((df$CA.moyen.commande[df$Month==input$mois]-df$CA.moyen.commande.compare[df$Month==input$mois])/df$CA.moyen.commande.compare[df$Month==input$mois])*100,1),"%")
    }
    if(ind<0){
      col <- "red"
      img <- "far fa-arrow-circle-down"
    }else{
      col <- "green"
      img <- "far fa-arrow-circle-up"
    }
    infoBox(title = "Evolution des CA moyens par commande",
            value = ind,
            icon = icon(img,verify_fa = FALSE),
            color = col
    )
  
  })

  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  # ----------------------- Graphiques ------------------------
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  output$net_sales_plot <- renderPlotly({

    if(input$Expand_filters==FALSE){
      df <- left_join(
        data.frame(aggregate(list(CA = data[year(data$date_invoice)==input$annee,input$Choix_Variables]),by=list(Date = as.Date(data$date_invoice_month[year(data$date_invoice)==input$annee])),sum) %>%
                     mutate(CAcumul = cumsum(CA)) %>%
                     mutate(Month = factor(month.name[month(Date)],levels=month.name))),

        data.frame(aggregate(list(CA.compare = data[year(data$date_invoice)==input$comparaison_indicateurs,input$Choix_Variables]),by=list(Date.compare = as.Date(data$date_invoice_month[year(data$date_invoice)==input$comparaison_indicateurs])),sum) %>%
                     mutate(CAcumul.compare = cumsum(CA.compare)) %>%
                     mutate(Month = factor(month.name[month(Date.compare)],levels=month.name))),
        by=c("Month"="Month"))
      if(input$option_cumul_net_sales_plot==TRUE){
        df %>%
          plot_ly(x = ~Month, y = ~CAcumul,type = 'bar',marker = list(color = "#394670"), name = input$annee) %>%
            add_trace(x = ~Month, y = ~CAcumul.compare,type = 'scatter',mode = "lines+marker",marker = list(color = "black"), name = input$comparaison_indicateurs) %>%
            layout(showlegend = T,
                   xaxis = list(title = ""),
                   yaxis = list (title = ""),
                   plot_bgcolor  = "rgba(0, 0, 0, 0)",
                   paper_bgcolor = "rgba(0, 0, 0, 0)")
      }else{
        df %>%
          plot_ly(x = ~Month, y = ~CA,type = 'bar',marker = list(color = "#394670"), name = input$annee) %>%
          add_trace(x = ~Month, y = ~CA.compare,type = 'scatter',mode = "lines+marker",marker = list(color = "black"), name = input$comparaison_indicateurs) %>%
          layout(showlegend = T,
                 xaxis = list(title = ""),
                 yaxis = list (title = ""),
                 plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)")
      }
    }else{
      data.frame(aggregate(list(CA = data[year(data$date_invoice)==input$annee,input$Choix_Variables]),by=list(Date = as.Date(data$date_invoice_month[year(data$date_invoice)==input$annee]), Filtre = data[year(data$date_invoice)==input$annee,input$Variable_selection_net_sales_plot_pie]),sum)) %>%
          plot_ly(x = ~Date, y = ~CA,type = 'bar',color = ~Filtre, colors = "Paired") %>%
          layout(showlegend = T,barmode = 'stack',
                 xaxis = list(title = ""),
                 yaxis = list (title = ""),
                 plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)")
    }

  })



  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  # ---------------- Statistiques Descriptives ----------------
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

  data_r <- reactiveValues(data = data, name = "data")

  observe({
    data_r$data <- data.stat.desc
    data_r$name <- "data"
  })

  results <- esquisse_server(
    id = "esquisse",
    data_rv = data_r
  )



  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  # -------- Approche Prédictive : Filters TEST ---------------
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  
  data.reactive <- reactive({
    get("data")
  })
  
  
  vars <- reactive({
    setNames(as.list(names(data[,c("date_invoice","market","channel","territory","activity")])), c(
      "Date",
      "Marché de la vente",
      "Canal de vente",
      "Continent de vente",
      "Activité du produit"
    ))
  })
  
  
  res_filter <- filter_data_server(
    id = "filtering",
    data = data.reactive,
    name = reactive("data"),
    vars = vars,
    widget_num = "slider",
    widget_date = "slider",
    label_na = "Missing"
    )
  
  observeEvent(res_filter$filtered(), {
    updateProgressBar(
      session = session, id = "pbar",
      value = nrow(res_filter$filtered()), total = nrow(data.reactive())
    )
  })
  
  
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
  # ---------------- Approche Prédictive ----------------------
  #&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  
  Function_Param1 <- eventReactive(input$action_ML,{
    input$nombre_donnees_predites
  })

  
  InputDataset_ML <- eventReactive(input$action_ML,{
    if(input$Custom_Data==1){
      machinelearning_complet(data = data.machinelearning,NbMoisPred = Function_Param1())
    }else{
      machinelearning_personnalise(data = data.frame(res_filter$filtered()),NbMoisPred = Function_Param1())
      
    }
    
  })


  
  output$prediction_data <- renderTable(
    
    if(input$prediction_cumulees==FALSE){
      InputDataset_ML()[[1]]
    }else{
      InputDataset_ML()[[4]]
    }
  )
  
  output$prediction_plot <- renderPlotly(

    if(input$prediction_cumulees==FALSE){
      InputDataset_ML()[[2]]
    }else{
      InputDataset_ML()[[3]]
    }
  )



  output$download <- downloadHandler(
    filename = function() {
      paste('Predictions-', Sys.time(), '.csv', sep='')
    },
    content = function(con) {
      if(input$prediction_cumulees==FALSE){
        write.csv(InputDataset_ML()[[1]], con, row.names=FALSE)
      }else{
        write.csv(InputDataset_ML()[[4]], con, row.names=FALSE)
      }
    }
  )



  


}
