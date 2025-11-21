library(shiny)
library(shinydashboard)
library(readr)
library(readxl)
library(DT)
library(dplyr)

# Fungsi format rupiah
format_rupiah <- function(x) {
  paste0("Rp ", format(round(x,2), big.mark=".", decimal.mark=","))
}

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Penjualan"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tampilan Data", tabName = "data", icon = icon("table")),
      menuItem("Analisis Penjualan", tabName = "analisis", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data",
        h3("Upload File"),
        fileInput("file", "Pilih CSV atau Excel"),
        DTOutput("tabel")
      ),
      
      tabItem(
        tabName = "analisis",
        h3("Grafik & Tabel Penjualan Berdasarkan Merk"),
        plotOutput("grafikMerk"),
        DTOutput("tabelMerk"),
        hr(),
        
        h3("Pilih Merk untuk Melihat Penjualan Berdasarkan Type"),
        selectInput("pilihMerk", "Pilih Merk:", choices = NULL),
        plotOutput("grafikType"),
        DTOutput("tabelType")
      )
    )
  )
)

server <- function(input, output, session) {
  
  dataClean <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    df <- if(ext == "csv") read_csv(input$file$datapath) else read_excel(input$file$datapath)
    df$Harga <- as.numeric(df$Harga)
    
    for(i in seq_along(df)) {
      if(is.character(df[[i]])) {
        df[[i]][is.na(df[[i]]) | df[[i]]==""] <- "Tidak diketahui"
      }
    }
    
    df
  })
  
  observeEvent(dataClean(), {
    updateSelectInput(session, "pilihMerk", choices = unique(dataClean()$merk))
  })
  
  output$tabel <- renderDT({ datatable(dataClean()) })
  
  # Grafik Merk dengan total unit terlihat semua
  output$grafikMerk <- renderPlot({
    req(dataClean())
    df <- dataClean()
    
    summaryMerk <- df %>%
      group_by(merk) %>%
      summarise(total_unit = n()) %>%
      arrange(desc(total_unit))
    
    maxY <- max(summaryMerk$total_unit) * 1.1  # beri ruang 10% agar label terlihat
    bar <- barplot(
      summaryMerk$total_unit,
      names.arg = summaryMerk$merk,
      las = 2,
      ylab = "Total Terjual",
      main = "Grafik Penjualan per Merk",
      ylim = c(0, maxY)
    )
    
    text(x = bar, y = summaryMerk$total_unit, labels = summaryMerk$total_unit, pos = 3, cex = 0.9)
  })
  
  output$tabelMerk <- renderDT({
    df <- dataClean()
    hasil <- df %>%
      group_by(merk) %>%
      summarise(total_unit = n(), rata_rata_harga = mean(Harga, na.rm = TRUE)) %>%
      arrange(desc(rata_rata_harga))
    
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil)
  })
  
  output$grafikType <- renderPlot({
    req(dataClean(), input$pilihMerk)
    df <- dataClean() %>% filter(merk == input$pilihMerk)
    
    summaryType <- df %>%
      group_by(type) %>%
      summarise(total_unit = n()) %>%
      arrange(desc(total_unit))
    
    maxY <- max(summaryType$total_unit) * 1.1
    bar <- barplot(
      summaryType$total_unit,
      names.arg = summaryType$type,
      las = 2,
      ylab = "Total Terjual",
      main = paste("Grafik Penjualan Type untuk Merk:", input$pilihMerk),
      ylim = c(0, maxY)
    )
    
    text(x = bar, y = summaryType$total_unit, labels = summaryType$total_unit, pos = 3, cex = 0.9)
  })
  
  output$tabelType <- renderDT({
    req(input$pilihMerk)
    df <- dataClean() %>% filter(merk == input$pilihMerk)
    
    hasil <- df %>%
      group_by(type) %>%
      summarise(total_unit = n(), rata_rata_harga = mean(Harga, na.rm = TRUE)) %>%
      arrange(desc(rata_rata_harga))
    
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil)
  })
}

shinyApp(ui, server)
