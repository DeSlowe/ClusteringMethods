# app.R - Shiny app completa per confronto tra dati originali e sintetici
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(forcats)
library(gridExtra)
library(dendextend)
library(tidyverse)
library(patchwork)
library(plyr)
library(dendextend)
library(factoextra)
library(fmsb)
library(formattable)

# Dataset predefiniti
data_iris <- iris
data_mtcars <- mtcars
data_diamonds <- ggplot2::diamonds[sample(nrow(ggplot2::diamonds), 1000), ]

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analisi e Confronto Dati"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "dataset", icon = icon("database")),
      menuItem("Visualizzazione", tabName = "visualize", icon = icon("chart-line")),
      menuItem("Statistiche", tabName = "stats", icon = icon("calculator")),
      menuItem("Clustering", tabName = "clustering", icon = icon("random"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dataset tab
      tabItem(tabName = "dataset",
              fluidRow(
                box(
                  title = "Selezione Dataset",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  radioButtons("dataSource", "Sorgente dati:",
                               choices = c("Dataset predefinito" = "predefined",
                                           "Carica file CSV" = "upload"),
                               selected = "predefined"),
                  conditionalPanel(
                    condition = "input.dataSource == 'predefined'",
                    selectInput("dataset", "Seleziona dataset:",
                                choices = c("Iris" = "iris",
                                            "MTCars" = "mtcars",
                                            "Diamonds (sample)" = "diamonds",
                                            "CSV Esempio" = "sample"))
                  ),
                  conditionalPanel(
                    condition = "input.dataSource == 'upload'",
                    fileInput("fileUpload", "Carica file CSV",
                              accept = c("text/csv", 
                                         "text/comma-separated-values,text/plain", 
                                         ".csv")),
                    checkboxInput("header", "Il file ha l'intestazione", TRUE),
                    radioButtons("sep", "Separatore:",
                                 choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                 selected = ","),
                    radioButtons("dec", "Decimale:",
                                 choices = c(Punto = ".", Virgola = ","),
                                 selected = ".")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Anteprima Dati",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("dataTable")
                )
              ),
              fluidRow(
                box(
                  title = "Informazioni Dataset",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("dataInfo")
                ),
                box(
                  title = "Struttura delle Variabili",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("dataStructure")
                )
              )
      ),
      
      # Visualizzazione tab
      tabItem(tabName = "visualize",
              fluidRow(
                box(
                  title = "Impostazioni Grafico",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("plotType", "Tipo di grafico:",
                              choices = c("Scatter Plot" = "scatter",
                                          "Istogramma" = "histogram",
                                          "Boxplot" = "boxplot",
                                          "Barre" = "bar",
                                          "Densità" = "density")),
                  uiOutput("xAxisInput"),
                  uiOutput("yAxisInput"),
                  uiOutput("colorInput"),
                  uiOutput("facetInput"),
                  actionButton("updatePlot", "Aggiorna Grafico", 
                               class = "btn-success")
                ),
                box(
                  title = "Grafico",
                  status = "info",
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput("plot", height = "600px")
                )
              )
      ),
      
      # Statistiche tab
      tabItem(tabName = "stats",
              fluidRow(
                tabBox(
                  id = "statsTabBox",
                  width = 12,
                  tabPanel("Statistiche Numeriche", 
                           fluidRow(
                             column(3,
                                    selectInput("numericStatsColumn", "Seleziona colonna numerica:", choices = NULL),
                                    actionButton("calculateNumericStats", "Calcola statistiche", class = "btn-primary")
                             ),
                             column(9,
                                    verbatimTextOutput("numericStatsSummary"),
                                    plotOutput("numericStatsPlot", height = "300px")
                             )
                           )
                  ),
                  tabPanel("Statistiche Categoriali", 
                           fluidRow(
                             column(3,
                                    selectInput("categoricalStatsColumn", "Seleziona colonna categoriale:", choices = NULL),
                                    actionButton("calculateCatStats", "Calcola statistiche", class = "btn-primary")
                             ),
                             column(9,
                                    verbatimTextOutput("categoricalStatsSummary"),
                                    plotOutput("categoricalStatsPlot", height = "300px")
                             )
                           )
                  ),
                  tabPanel("Correlazione", 
                           plotOutput("corrPlot", height = "500px"))
                )
              )
      ),
      
      # Clustering tab
       tabItem(tabName = "clustering",
              fluidRow(
                box(title = "Opzioni Clustering", status = "primary", solidHeader = TRUE, width = 4,
                    selectInput("distance", "Metrica di distanza:",
                                choices = c("euclidean", "manhattan", "maximum", "minkowski", "canberra")),
                    selectInput("linkage", "Metodo di collegamento:",
                                choices = c("single", "complete", "average", "ward.D")),
                    selectInput("var_x", "Variabile X:", choices = NULL),
                    selectInput("var_y", "Variabile Y:", choices = NULL),
                    selectInput("optimal_methods", "Meteodo per il numero di cluster:", 
                                choices = c("silhouette", "wss", "gap_stat")),
                    numericInput("k", "Numero di cluster:", 3, min = 2, max = 10),
                    actionButton("run_clustering", "Esegui Clustering", class = "btn-success")
                ),
                box(title = "Dendrogramma", status = "info", solidHeader = TRUE, width = 8,
                    plotOutput("dendrogram"))
              ),
              fluidRow(
                box(title = "Scatter Plot Clustering", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("cluster_plot"))
              ),
              fluidRow(
                box(title = "Optimal Clusters", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("optimalmethods_plot"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Caricamento del dataset
  data <- reactive({
    if (input$dataSource == "predefined") {
      switch(input$dataset,
             "iris" = data_iris,
             "mtcars" = data_mtcars,
             "diamonds" = data_diamonds,
             "sample" = {
               # Carica il file sample.csv dalla directory di lavoro
               tryCatch({
                 df <- read.csv("sample.csv", header = TRUE, sep = ",", dec = ".")
                 # Converti le colonne appropriate in fattori
                 df <- transform_data_types(df)
                 df
               }, error = function(e) {
                 # Se il file non esiste, restituisci un dataframe vuoto con la struttura corretta
                 data.frame(
                   ID = integer(),
                   Age = integer(),
                   Gender = character(),
                   City = character(),
                   Income = numeric(),
                   Score = numeric(),
                   Last_Purchase_Date = character(),
                   stringsAsFactors = FALSE
                 )
               })
             }
      )
    } else {
      req(input$fileUpload)
      tryCatch({
        df <- read.csv(input$fileUpload$datapath,
                       header = input$header,
                       sep = input$sep,
                       dec = input$dec)
        # Converti le colonne appropriate in fattori
        df <- transform_data_types(df)
        df
      }, error = function(e) {
        showNotification("Errore nel caricamento del file", type = "error")
        return(NULL)
      })
    }
  })
  
  # Funzione per ottimizzare i tipi di dati
  transform_data_types <- function(df) {
    for (col_name in colnames(df)) {
      col <- df[[col_name]]
      
      # Se è un carattere, verifica se è categoriale
      if (is.character(col)) {
        unique_vals <- unique(col)
        # Se ha pochi valori unici rispetto alle righe, lo trasformiamo in fattore
        if (length(unique_vals) <= min(20, nrow(df) * 0.3)) {
          df[[col_name]] <- as.factor(col)
        }
      }
      
      # Converti potenziali date
      if (is.character(col) && any(grepl("^\\d{2,4}[/-]\\d{1,2}[/-]\\d{1,2}$", col))) {
        tryCatch({
          df[[col_name]] <- as.Date(col)
        }, error = function(e) {
          # Non è una data, lasciamo com'è
        })
      }
    }
    return(df)
  }
  
  # Anteprima dati
  output$dataTable <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Informazioni sul dataset
  output$dataInfo <- renderPrint({
    req(data())
    df <- data()
    
    cat("Numero di righe:", nrow(df), "\n")
    cat("Numero di colonne:", ncol(df), "\n\n")
    
    num_cols <- sum(sapply(df, is.numeric))
    cat_cols <- sum(sapply(df, is.factor) | sapply(df, is.character))
    date_cols <- sum(sapply(df, function(x) inherits(x, "Date")))
    other_cols <- ncol(df) - num_cols - cat_cols - date_cols
    
    cat("Tipi di colonne:\n")
    cat("- Numeriche:", num_cols, "\n")
    cat("- Categoriali:", cat_cols, "\n")
    cat("- Date:", date_cols, "\n")
    cat("- Altro:", other_cols, "\n\n")
    
    cat("Riassunto dataset:\n")
    summary(df)
  })
  
  # Struttura del dataset
  output$dataStructure <- renderPrint({
    req(data())
    str(data())
  })
  
  # Aggiungi colonna tipo per ogni variabile
  get_column_types <- reactive({
    req(data())
    df <- data()
    
    types <- sapply(df, function(x) {
      if (is.numeric(x)) {
        return("numeric")
      } else if (is.factor(x) || is.character(x)) {
        return("categorical")
      } else if (inherits(x, "Date")) {
        return("date")
      } else {
        return("other")
      }
    })
    
    return(types)
  })
  
  # Liste di colonne per tipo
  get_numeric_columns <- reactive({
    req(data())
    column_types <- get_column_types()
    names(column_types[column_types == "numeric"])
  })
  
  get_categorical_columns <- reactive({
    req(data())
    column_types <- get_column_types()
    names(column_types[column_types == "categorical"])
  })
  
  # UI dinamici per la visualizzazione
  output$xAxisInput <- renderUI({
    req(data())
    cols <- colnames(data())
    
    if (input$plotType %in% c("histogram", "density")) {
      numeric_cols <- get_numeric_columns()
      selectInput("xAxis", "Variabile X:", choices = numeric_cols)
    } else if (input$plotType == "boxplot") {
      numeric_cols <- get_numeric_columns()
      selectInput("xAxis", "Variabile X (gruppi):", choices = c(get_categorical_columns(), get_numeric_columns()))
    } else {
      selectInput("xAxis", "Variabile X:", choices = cols)
    }
  })
  
  output$yAxisInput <- renderUI({
    req(data())
    if (input$plotType %in% c("scatter", "boxplot")) {
      numeric_cols <- get_numeric_columns()
      selectInput("yAxis", "Variabile Y:", choices = numeric_cols)
    } else if (input$plotType == "bar") {
      selectInput("yAxis", "Variabile Y (conteggio):", choices = colnames(data()))
    } else {
      # Per istogramma e densità non serve y-axis
      return(NULL)
    }
  })
  
  output$colorInput <- renderUI({
    req(data())
    cols <- colnames(data())
    selectInput("colorVar", "Variabile colore (opzionale):", 
                choices = c("Nessuna" = "", cols), 
                selected = "")
  })
  
  output$facetInput <- renderUI({
    req(data())
    categorical_cols <- get_categorical_columns()
    selectInput("facetVar", "Variabile facet (opzionale):", 
                choices = c("Nessuna" = "", categorical_cols), 
                selected = "")
  })
  
  # Generazione del grafico
  output$plot <- renderPlotly({
    req(data(), input$xAxis)
    req(input$updatePlot)
    
    isolate({
      df <- data()
      p <- ggplot(df)
      
      if (input$plotType == "scatter") {
        req(input$yAxis)
        p <- p + geom_point(aes_string(x = input$xAxis, y = input$yAxis))
      } else if (input$plotType == "histogram") {
        p <- p + geom_histogram(aes_string(x = input$xAxis), bins = 30)
      } else if (input$plotType == "density") {
        p <- p + geom_density(aes_string(x = input$xAxis))
      } else if (input$plotType == "boxplot") {
        req(input$yAxis)
        p <- p + geom_boxplot(aes_string(x = input$xAxis, y = input$yAxis))
      } else if (input$plotType == "bar") {
        p <- p + geom_bar(aes_string(x = input$xAxis))
      }
      
      # Aggiungi colore se selezionato
      if (!is.null(input$colorVar) && input$colorVar != "") {
        p <- p + aes_string(color = input$colorVar, fill = input$colorVar)
      }
      
      # Aggiungi facet se selezionato
      if (!is.null(input$facetVar) && input$facetVar != "") {
        p <- p + facet_wrap(as.formula(paste("~", input$facetVar)))
      }
      
      # Aggiungi tema e titoli
      p <- p + theme_minimal() +
        labs(title = paste("Grafico:", input$plotType),
             x = input$xAxis,
             y = if(input$plotType %in% c("scatter", "boxplot")) input$yAxis else "Conteggio")
      
      ggplotly(p)
    })
  })
  
  # Aggiornamento delle liste di colonne per statistiche
  observe({
    req(data())
    num_cols <- get_numeric_columns()
    cat_cols <- get_categorical_columns()
    
    updateSelectInput(session, "numericStatsColumn", choices = num_cols)
    updateSelectInput(session, "categoricalStatsColumn", choices = cat_cols)
    updateSelectInput(session, "contingencyVar1", choices = colnames(data()))
    updateSelectInput(session, "contingencyVar2", choices = colnames(data()))
  })
  
  # Calcolo statistiche numeriche
  observeEvent(input$calculateNumericStats, {
    req(data(), input$numericStatsColumn)
    
    output$numericStatsSummary <- renderPrint({
      column_data <- data()[[input$numericStatsColumn]]
      
      cat("Statistiche per", input$numericStatsColumn, ":\n\n")
      cat("N. osservazioni:", length(column_data), "\n")
      cat("N. valori mancanti:", sum(is.na(column_data)), "\n\n")
      
      cat("Distribuzione:\n")
      cat("- Minimo:", min(column_data, na.rm = TRUE), "\n")
      cat("- 1° Quartile:", quantile(column_data, 0.25, na.rm = TRUE), "\n")
      cat("- Media:", mean(column_data, na.rm = TRUE), "\n")
      cat("- Mediana:", median(column_data, na.rm = TRUE), "\n")
      cat("- 3° Quartile:", quantile(column_data, 0.75, na.rm = TRUE), "\n")
      cat("- Massimo:", max(column_data, na.rm = TRUE), "\n\n")
      
      cat("Variabilità:\n")
      cat("- Deviazione standard:", sd(column_data, na.rm = TRUE), "\n")
      cat("- Varianza:", var(column_data, na.rm = TRUE), "\n")
      cat("- Range:", diff(range(column_data, na.rm = TRUE)), "\n")
      cat("- IQR:", IQR(column_data, na.rm = TRUE), "\n")
    })
    
    output$numericStatsPlot <- renderPlot({
      column_data <- data()[[input$numericStatsColumn]]
      
      p <- ggplot(data(), aes_string(x = input$numericStatsColumn)) +
        geom_histogram(aes(y = ..density..), fill = "steelblue", color = "black", bins = 30) +
        geom_density(alpha = 0.2, fill = "red") +
        geom_vline(xintercept = mean(column_data, na.rm = TRUE), 
                   color = "red", linetype = "dashed", size = 1) +
        geom_vline(xintercept = median(column_data, na.rm = TRUE), 
                   color = "green", linetype = "dashed", size = 1) +
        theme_minimal() +
        labs(title = paste("Distribuzione di", input$numericStatsColumn),
             subtitle = "Linea rossa = media, Linea verde = mediana",
             x = input$numericStatsColumn, y = "Densità")
      
      # Aggiungiamo un boxplot
      p2 <- ggplot(data(), aes_string(y = input$numericStatsColumn)) +
        geom_boxplot(fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Boxplot", x = "")
      
      # Combina i grafici
      gridExtra::grid.arrange(p, p2, ncol = 1, heights = c(2, 1))
    })
  })
  
  # Calcolo statistiche categoriali
  observeEvent(input$calculateCatStats, {
    req(data(), input$categoricalStatsColumn)
    
    output$categoricalStatsSummary <- renderPrint({
      column_data <- data()[[input$categoricalStatsColumn]]
      
      # Convertiamo in fattore se è una stringa
      if (is.character(column_data)) {
        column_data <- as.factor(column_data)
      }
      
      freq_table <- table(column_data, useNA = "ifany")
      prop_table <- prop.table(freq_table) * 100
      
      cat("Statistiche per", input$categoricalStatsColumn, ":\n\n")
      cat("N. osservazioni:", length(column_data), "\n")
      cat("N. valori mancanti:", sum(is.na(column_data)), "\n")
      cat("N. categorie uniche:", length(levels(column_data)), "\n\n")
      
      cat("Frequenze:\n")
      print(freq_table)
      cat("\n")
      
      cat("Percentuali:\n")
      print(round(prop_table, 2))
      cat("%\n\n")
      
      cat("Moda:", names(which.max(freq_table)), "\n")
    })
    
    output$categoricalStatsPlot <- renderPlot({
      column_data <- data()[[input$categoricalStatsColumn]]
      
      # Convertiamo in fattore se è una stringa
      if (is.character(column_data)) {
        column_data <- as.factor(column_data)
      }
      
      # Creiamo un dataframe per la visualizzazione
      plot_df <- data.frame(
        categoria = column_data
      )
      
      # Se ci sono troppi livelli, mostriamo solo i più frequenti
      if (length(levels(as.factor(column_data))) > 10) {
        plot_df$categoria <- forcats::fct_lump(plot_df$categoria, n = 10)
      }
      
      # Grafico a barre
      ggplot(plot_df, aes(x = categoria)) +
        geom_bar(aes(y = ..count.., fill = ..count..)) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Distribuzione di", input$categoricalStatsColumn),
             x = input$categoricalStatsColumn, y = "Frequenza",
             fill = "Frequenza")
    })
  })
  
  # Grafico di correlazione
  output$corrPlot <- renderPlot({
    req(data())
    
    # Seleziona solo le colonne numeriche
    num_data <- data()[, get_numeric_columns(), drop = FALSE]
    
    if (ncol(num_data) > 1) {
      corr_matrix <- cor(num_data, use = "pairwise.complete.obs")
      
      # Prepara il dataframe per ggplot
      corr_df <- as.data.frame(as.table(corr_matrix))
      names(corr_df) <- c("Var1", "Var2", "Correlation")
      
      # Crea il grafico di correlazione
      ggplot(corr_df, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1, 1)) +
        geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Matrice di Correlazione",
             x = "", y = "")
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Servono almeno due variabili numeriche per creare una matrice di correlazione") +
        theme_void()
    }
  })
  
  # Clustering
  observe({
    req(data())
  
    updateSelectInput(session, "var_x", choices = names(data()))
    updateSelectInput(session, "var_y", choices = names(data()))
    updateSelectInput(session, "optimal_methods", choices = input$optima_methods)
  })
  
  clustering_result <- eventReactive(input$run_clustering, {
    req(input$var_x, input$var_y)
    df <- data() %>% select(all_of(input$var_x), all_of(input$var_y)) %>% scale()
    dist_matrix <- dist(df, method = input$distance)
    hclust(dist_matrix, method = input$linkage)  %>% 
      as.dendrogram() %>% 
      set('labels', '') %>%  
      set('branches_k_color', k = input$k)
  })
  
  output$dendrogram <- renderPlot({
    req(clustering_result())
    plot(clustering_result(), main = "Dendrogramma", xlab = "", sub = "")
  })
  
  output$cluster_plot <- renderPlotly({
    req(clustering_result())
    clusters <- cutree(clustering_result(), k = input$k)
    df <- data() %>% select(all_of(input$var_x), all_of(input$var_y)) %>% mutate(cluster = as.factor(clusters))
    p <- ggplot(df, aes_string(x = input$var_x, y = input$var_y, color = "cluster")) +
      geom_point(size = 3) + theme_minimal() + labs(title = "Clustering Scatter Plot")
    ggplotly(p)
  })
  
  output$optimalmethods_plot <- renderPlotly({
    req(clustering_result())
    df <- data() %>% select(all_of(input$var_x), all_of(input$var_y)) %>% scale()
    p <- df %>% 
      fviz_nbclust(FUN = hcut, method=input$optimal_methods)
    ggplotly(p)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)  