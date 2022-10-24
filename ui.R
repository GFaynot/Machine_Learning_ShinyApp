# Define UI
fluidPage(
  
  #Navbar structure for UI
  navbarPage(title = img(src="digigram.png"),
             # theme = "bootstrap.min.css",
             theme = shinytheme("lumen"),
             
             header = tagList(
               useShinydashboard()
             ),
            

             tabPanel("Accueil", fluid = TRUE, icon = icon("house-user"),
                      fluidRow(
                        column(
                          width=1,offset = 9,
                            selectInput(inputId = "annee",
                                        label = "Year",
                                        choices = sort(unique(year(data$date_invoice)), decreasing = TRUE),
                                        selected = year(Sys.Date()),
                                        width = '100%')
                          ),
                        column(
                          width=1,offset = 0,
                          selectInput(inputId = "mois",
                                      label = "Month",
                                      choices = c("All",as.character(sort(factor(unique(data$Month), levels = month.name)))),
                                      width = '100%')
                        ),
                        
                        column(
                          width=1,offset = 0,
                          selectInput(inputId = "comparaison_indicateurs",
                                      label = "Compared to ",
                                      choices = sort(unique(year(data$date_invoice)), decreasing = TRUE),
                                      selected = year(Sys.Date())-1)
                          
                        )
                      ),
                      tags$style(".small-box { background-color: #FAD5B5 !important; color: #000000 !important; }"),
                      fluidRow(
                        valueBoxOutput("Indicateur_chiffre_affaires", width = 3),
                        valueBoxOutput("Indicateur_marge", width = 3),
                        valueBoxOutput("Indicateur_couts", width = 3),
                        valueBoxOutput("Indicateur_commandes", width = 3)
                      ),
                      
                      tags$style(".small-box.bg-red { background-color: #FFA495 !important; color: #000000 !important; }"),
                      tags$style(".small-box.bg-green { background-color: #9AFCAC !important; color: #000000 !important; }"),
                      
                      fluidRow(
                        infoBoxOutput("Evolution_chiffre_affaires", width = 3),
                        infoBoxOutput("Evolution_marge", width = 3),
                        infoBoxOutput("Evolution_couts", width = 3),
                        infoBoxOutput("Evolution_commandes", width = 3)
                      ),
                      
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          h3("Paramètres"),
                          width=2,
                          br(),
                          selectInput(inputId = 'Choix_Variables',
                                      label = 'Choix de variables',
                                      choices = c("Chiffre d'affaires"="net_sales","Marge brute"="Marge","Coûts de revient"="cost_sales"),
                                      width = '100%'),
                          br(),
                          conditionalPanel(
                            condition = "input.Expand_filters == false",
                            checkboxInput("option_cumul_net_sales_plot", label = "Cumulé", value = FALSE)
                          ),
                            
                          br(),
                          materialSwitch("Expand_filters", label = "Analyse en détails", value = FALSE),
                          
                          br(),

                          conditionalPanel(
                            condition = "input.Expand_filters == true",
                            radioGroupButtons('Variable_selection_net_sales_plot_pie','Choix de variables',c("territory","activity","market","channel")),
                            
                          )
                          
                          
                        ),
                        
                        mainPanel(
                          width = 10,
                          plotlyOutput('net_sales_plot',width = "100%")
                        )
                      )

             ),
             
             tabPanel("Statistiques Descriptives", fluid = TRUE, icon = icon("chart-bar"),
                      titlePanel("Data Visualisation"),

                      tabsetPanel(
                        tabPanel(
                          title = "esquisse",
                          esquisse_ui(
                            id = "esquisse",
                            header = FALSE # dont display gadget title
                          )
                        )
                      )

             ),
             
             tabPanel("Approche prédictive ", fluid = TRUE, icon = icon("brain"),
                      
                      sidebarPanel(
                        titlePanel("Paramètres"),
                        width=3,
                        br(),
                        radioButtons("Custom_Data", label = "Choix des données",
                                     choices = list("Données complètes" = 1, "Données personnalisées" = 2), 
                                     selected = 1),
                        
                        conditionalPanel(
                          condition = "input.Custom_Data == 2",
                          
                          progressBar(
                          id = "pbar", value = 100,
                          total = 100, display_pct = TRUE),
                          
                          filter_data_ui("filtering", max_height = "500px")
                        ),

                        
                        br(),
                        checkboxInput("prediction_cumulees", label = "Prédictions Cumulée", value = FALSE),
                        br(),
                        
                        sliderInput(inputId = "nombre_donnees_predites",  
                                    label = "Nombre de mois à prédire", 
                                    min=1,
                                    max=12,
                                    value=1,
                                    step=1),
                        
                        
                        div(actionButton("action_ML", 
                                     label = "Action", 
                                     icon("paper-plane"), 
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            style="float:right")
                        
                      ),
                      
                      # Show the main display
                      mainPanel(
                        width=9,
                        tabsetPanel(
                          type = "tabs",
                          tabPanel("Graphique",
                                   # conditionalPanel(
                                   #   condition = "input.prediction_cumulees== 'FALSE'",
                                   #   box(withSpinner(plotlyOutput('prediction_plot',width = "100%")), width = 12)
                                   # ),
                                   # conditionalPanel(
                                   #   condition = "input.prediction_cumulees== 'TRUE'",
                                   #   box(withSpinner(plotlyOutput('prediction_cumul_plot',width = "100%")), width = 12)
                                   # )
                                   box(withSpinner(plotlyOutput('prediction_plot',width = "100%")), width = 12)
                          ),
                          tabPanel("Data",
                                   box(withSpinner(tableOutput('prediction_data')), width = 12),
                                   downloadButton("download")
                          )
                        )
                        
                      )
                      
             ),
             
             tabPanel("About", icon = icon("info-circle"),

             )
  )
)