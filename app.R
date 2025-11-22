library(shiny)
library(bs4Dash)
library(DT)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)

# -------------------------------
# Helper Functions
# -------------------------------
format_rupiah <- function(x) {
  paste0("Rp ", format(round(x, 0), big.mark = ".", decimal.mark = ","))
}

detect_col <- function(df, candidates) {
  nm <- names(df)
  idx <- which(tolower(nm) %in% tolower(candidates))
  if (!length(idx)) return(NA_character_)
  nm[idx[1]]
}

# -------------------------------
# UI
# -------------------------------
ui <- bs4DashPage(
  title = "Dashboard Penjualan OBSI B",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = "OBSI B",
    bs4SidebarMenu(
      bs4SidebarHeader("Dashboard"),
      bs4SidebarMenuItem("Dashboard Penjualan", tabName = "data", icon = icon("table")),
      bs4SidebarMenuItem("Analisis Penjualan", tabName = "analisis", icon = icon("chart-line"))
    )
  ),
  controlbar = bs4DashControlbar(
    checkboxInput("dark_mode", "Dark Mode", value = FALSE)
  ),
  body = bs4DashBody(
    tags$style(HTML("
      .dark-mode .dataTables_wrapper .dataTables_length select,
      .dark-mode .dataTables_wrapper .dataTables_filter input {
        background-color: #2b2b2b !important;
        color: white !important;
        border: 1px solid #444444 !important;
      }
      .dark-mode .dataTables_wrapper .dataTables_info,
      .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button,
      .dark-mode .dataTables_wrapper .dataTables_length label,
      .dark-mode .dataTables_wrapper .dataTables_filter label {
        color: white !important;
      }
      .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background-color: #444444 !important;
        color: white !important;
      }
      .dark-mode .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background-color: #06b6d4 !important;
        color: white !important;
      }
    ")),
    bs4TabItems(
      # Dashboard Penjualan
      bs4TabItem(
        tabName = "data",
        fluidRow(
          bs4Card(
            width = 12,
            title = "Upload File",
            status = "primary",
            solidHeader = TRUE,
            fileInput("file", "File CSV / Excel", accept = c(".csv",".xlsx"))
          )
        ),
        fluidRow(
          bs4Card(
            width = 12,
            title = "Data Penjualan",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("tabelData")
          )
        )
      ),
      # Analisis Penjualan
      bs4TabItem(
        tabName = "analisis",
        fluidRow(
          bs4Card(
            width = 12,
            title = "Grafik & Tabel Penjualan Berdasarkan Merk",
            status = "info",
            solidHeader = TRUE,
            plotOutput("grafikMerk", height = "400px"),
            DTOutput("tabelMerk")
          )
        ),
        fluidRow(
          bs4Card(
            width = 12,
            title = "Pilih Merk untuk Melihat Penjualan Berdasarkan Type",
            status = "warning",
            solidHeader = TRUE,
            selectInput("pilihMerk", "Pilih Merk:", choices = NULL),
            plotOutput("grafikType", height = "400px"),
            DTOutput("tabelType")
          )
        )
      )
    )
  )
)

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {
  
  # Reactive file
  dataFile <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- if(ext == "csv") read_csv(input$file$datapath) else read_excel(input$file$datapath)
    col_harga <- detect_col(df, c("harga","price","total"))
    df[[col_harga]] <- as.numeric(df[[col_harga]])
    df
  })
  
  dataAnalisis <- reactive({
    df <- dataFile()
    col_merk <- detect_col(df, c("merk"))
    col_type <- detect_col(df, c("type"))
    col_harga <- detect_col(df, c("harga","price","total"))
    
    df %>% rename(
      Merk = !!col_merk,
      Type = !!col_type,
      Harga = !!col_harga
    )
  })
  
  observe({
    updateSelectInput(session, "pilihMerk",
                      choices = unique(dataAnalisis()$Merk))
  })
  
  # -------------------------------
  # Card Status & DT Class
  # -------------------------------
  card_status <- reactive({
    if (input$dark_mode) "gray-dark" else "primary"
  })
  
  dt_class <- reactive({
    if (input$dark_mode) "cell-border stripe table-dark" else "stripe hover compact"
  })
  
  # Toggle dark mode class on body
  observe({
    if (input$dark_mode) {
      runjs("document.body.classList.add('dark-mode');")
    } else {
      runjs("document.body.classList.remove('dark-mode');")
    }
  })
  
  output$tabelData <- renderDT({
    datatable(dataAnalisis(), 
              options = list(pageLength = 10, scrollX = TRUE),
              style = 'bootstrap4',
              class = dt_class())
  })
  
  output$tabelMerk <- renderDT({
    df <- dataAnalisis()
    hasil <- df %>%
      group_by(Merk) %>%
      summarise(total_unit = n(), rata_rata_harga = mean(Harga, na.rm = TRUE)) %>%
      arrange(desc(total_unit))
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil, 
              options = list(pageLength = 10, scrollX = TRUE),
              style = 'bootstrap4',
              class = dt_class())
  })
  
  output$tabelType <- renderDT({
    req(input$pilihMerk)
    df <- dataAnalisis() %>% filter(Merk == input$pilihMerk)
    hasil <- df %>%
      group_by(Type) %>%
      summarise(total_unit = n(), rata_rata_harga = mean(Harga, na.rm = TRUE)) %>%
      arrange(desc(total_unit))
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil, 
              options = list(pageLength = 10, scrollX = TRUE),
              style = 'bootstrap4',
              class = dt_class())
  })
  
  # -------------------------------
  # Grafik Dynamic Theme
  # -------------------------------
  theme_dynamic <- reactive({
    if (input$dark_mode) {
      theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#2b2b2b"),
          panel.background = element_rect(fill = "#2b2b2b"),
          panel.grid = element_line(color = "#444444"),
          axis.text = element_text(color = "white"),
          axis.title = element_text(color = "white"),
          plot.title = element_text(color = "white")
        )
    } else {
      theme_minimal() +
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "#e0e0e0"),
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black"),
          plot.title = element_text(color = "black")
        )
    }
  })
  
  output$grafikMerk <- renderPlot({
    req(dataAnalisis())
    df <- dataAnalisis()
    summaryMerk <- df %>%
      group_by(Merk) %>%
      summarise(total_unit = n()) %>%
      arrange(desc(total_unit))
    
    text_color <- if (input$dark_mode) "white" else "black"
    
    ggplot(summaryMerk, aes(x = reorder(Merk, total_unit), y = total_unit, fill = total_unit)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = total_unit), vjust = -0.5, size = 4, color = text_color) +
      scale_fill_gradient(low = "#3b82f6", high = "#06b6d4") +
      labs(title = "Grafik Penjualan per Merk", x = "Merk", y = "Total Terjual") +
      theme_dynamic()
  })
  
  output$grafikType <- renderPlot({
    req(dataAnalisis(), input$pilihMerk)
    df <- dataAnalisis() %>% filter(Merk == input$pilihMerk)
    summaryType <- df %>%
      group_by(Type) %>%
      summarise(total_unit = n()) %>%
      arrange(desc(total_unit))
    
    text_color <- if (input$dark_mode) "white" else "black"
    
    ggplot(summaryType, aes(x = reorder(Type, total_unit), y = total_unit, fill = total_unit)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = total_unit), vjust = -0.5, size = 4, color = text_color) +
      scale_fill_gradient(low = "#f97316", high = "#facc15") +
      labs(title = paste("Grafik Penjualan Type untuk Merk:", input$pilihMerk),
           x = "Type", y = "Total Terjual") +
      theme_dynamic()
  })
}

# -------------------------------
# Run App
# -------------------------------
shinyApp(ui, server)