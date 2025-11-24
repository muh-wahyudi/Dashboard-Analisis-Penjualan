library(shiny)
library(bs4Dash)
library(shinyjs)
library(fresh)
library(DT)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(plotly)
library(scales)

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

safe_rename <- function(df, old, new) {
  if (!is.na(old) && old %in% names(df)) {
    names(df)[names(df) == old] <- new
  }
  df
}

# -------------------------------
# Color palette
# -------------------------------
color_map <- list(
  primary = "#007bff", secondary = "#6c757d", info = "#17a2b8", success = "#28a745",
  warning = "#ffc107", danger = "#dc3545", dark = "#343a40", light = "#f8f9fa",
  purple = "#6f42c1", orange = "#fd7e14", teal = "#20c997", pink = "#e83e8c"
)

colorBoxInput <- function(id, label, colors) {
  tagList(
    tags$h5(label, style = "font-weight:600; margin-bottom:10px;"),
    div(style = "display:flex; flex-wrap:wrap; gap:10px; margin-bottom:16px;",
        lapply(names(colors), function(clr) {
          actionButton(
            inputId = paste0(id, "_", clr),
            label = NULL,
            style = paste0(
              "background-color:", colors[[clr]], ";",
              "border:2px solid transparent; width:32px; height:32px; border-radius:6px; cursor:pointer;",
              "transition: all 0.3s ease; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
            ),
            class = "color-box",
            title = clr
          )
        })
    )
  )
}

# -------------------------------
# UI
# -------------------------------
ui <- bs4DashPage(
  title = "Dashboard Penjualan Premium",
  header = bs4DashNavbar(
    title = tags$div(
      icon("chart-line"), 
      " Dashboard Penjualan", 
      style = "font-weight:700; font-size:20px;"
    )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    bs4SidebarMenu(
      bs4SidebarHeader("MENU UTAMA"),
      bs4SidebarMenuItem("Dashboard Penjualan", tabName = "data", icon = icon("table")),
      bs4SidebarMenuItem("Analisis Penjualan", tabName = "analisis", icon = icon("chart-line"))
    )
  ),
  controlbar = bs4DashControlbar(
    skin = "light",
    title = "Theme Customizer",
    bs4Card(
      title = tags$div(icon("palette"), " Customizer Tema"),
      solidHeader = TRUE,
      width = 12,
      status = "primary",
      colorBoxInput("navbar_color", "🎨 Warna Navbar", color_map),
      colorBoxInput("sidebar_color", "📌 Warna Sidebar", color_map),
      checkboxInput("sidebar_dark", "Mode Sidebar Gelap", FALSE),
      colorBoxInput("accent_color", "✨ Warna Aksen", color_map),
      tags$hr(style = "margin:20px 0; border-top:2px solid #e0e0e0;"),
      div(style = "padding:12px; background:#f8f9fa; border-radius:6px;",
          tags$p(style = "font-size:13px; color:#666; margin:0; line-height:1.6;",
                 icon("info-circle"), " Klik kotak warna untuk mengubah tema.",
                 tags$br(),
                 tags$small("💡 Tip: Coba kombinasi dengan Dark Mode!"))
      )
    )
  ),
  body = bs4DashBody(
    useShinyjs(),
    uiOutput("ui_fresh_theme"),
    tags$style(HTML("
      .color-box.selected { 
        border-color: #000 !important; 
        box-shadow: 0 0 12px rgba(0,0,0,0.4) !important;
        transform: scale(1.15) !important;
      }
      .color-box:hover {
        transform: scale(1.08);
        box-shadow: 0 4px 8px rgba(0,0,0,0.2) !important;
      }
      .info-box {
        transition: all 0.3s ease;
      }
      .info-box:hover {
        transform: translateY(-4px);
        box-shadow: 0 8px 16px rgba(0,0,0,0.15);
      }
      .card {
        transition: box-shadow 0.3s ease;
      }
      .card:hover {
        box-shadow: 0 6px 20px rgba(0,0,0,0.12);
      }
      .btn-download {
        margin: 8px 4px;
        transition: all 0.3s ease;
      }
      .btn-download:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      }
    ")),
    
    bs4TabItems(
      # DATA TAB
      bs4TabItem(
        tabName = "data",
        fluidRow(
          valueBoxOutput("vbox_total_records", width = 3),
          valueBoxOutput("vbox_total_merk", width = 3),
          valueBoxOutput("vbox_total_type", width = 3),
          valueBoxOutput("vbox_avg_harga", width = 3)
        ),
        fluidRow(
          bs4Card(
            width = 12, 
            title = tags$div(icon("upload"), " Upload File Data Penjualan"), 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = TRUE,
            fileInput("file", "Pilih File CSV atau Excel", 
                      accept = c(".csv",".xlsx",".xls"),
                      buttonLabel = "Browse...",
                      placeholder = "Belum ada file yang dipilih"),
            tags$div(style = "margin-top:12px;",
                     tags$p(style = "color:#666; font-size:13px;",
                            icon("info-circle"), " Format yang didukung: CSV, XLSX, XLS",
                            tags$br(),
                            "📊 Pastikan file memiliki kolom: Merk, Type, Harga"
                     )
            )
          )
        ),
        fluidRow(
          bs4Card(
            width = 12, 
            title = tags$div(icon("table"), " Preview Data"), 
            status = "info", 
            solidHeader = TRUE,
            collapsible = TRUE,
            div(style = "margin-bottom:12px;",
                downloadButton("download_data", "Download Data", class = "btn-primary btn-download", icon = icon("download")),
                actionButton("refresh_data", "Refresh", class = "btn-info btn-download", icon = icon("sync"))
            ),
            DTOutput("tabelData")
          )
        )
      ),
      
      # ANALISIS TAB
      bs4TabItem(
        tabName = "analisis",
        fluidRow(
          bs4Card(
            width = 12,
            title = tags$div(icon("chart-bar"), " Analisis Penjualan per Merk"),
            status = "success", 
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(6,
                     div(style = "padding:12px;",
                         tags$h5("📊 Grafik Interaktif", style = "font-weight:600; margin-bottom:16px;"),
                         div(style = "padding-bottom:60px;",  # <-- padding ekstra
                             plotlyOutput("grafikMerkInteractive", height = "450px")
                         ),
                         div(style = "margin-top:12px;",
                             downloadButton("download_merk_table", "Download Data", class = "btn-success btn-sm", icon = icon("download"))
                         )
                     )
              ),
              column(6,
                     div(style = "padding:12px;",
                         tags$h5("📋 Tabel Detail", style = "font-weight:600; margin-bottom:16px;"),
                         DTOutput("tabelMerk")
                     )
              )
            )
          )
        ),
        fluidRow(
          bs4Card(
            width = 12,
            title = tags$div(icon("filter"), " Analisis Detail per Merk dan Type"),
            status = "warning", 
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(12,
                     div(style = "background:#f8f9fa; padding:16px; border-radius:8px; margin-bottom:16px;",
                         selectInput("pilihMerk", 
                                     label = tags$div(icon("tag"), " Pilih Merk untuk Analisis:"), 
                                     choices = NULL,
                                     width = "100%")
                     )
              )
            ),
            fluidRow(
              column(6,
                     div(style = "padding:12px;",
                         tags$h5("📈 Grafik Type", style = "font-weight:600; margin-bottom:16px;"),
                         div(style = "padding-bottom:60px;",  # <-- padding ekstra
                             plotlyOutput("grafikTypeInteractive", height = "450px")
                         ),
                         div(style = "margin-top:12px;",
                             downloadButton("download_type_table", "Download Data", class = "btn-warning btn-sm", icon = icon("download"))
                         )
                     )
              ),
              column(6,
                     div(style = "padding:12px;",
                         tags$h5("📋 Tabel Detail Type", style = "font-weight:600; margin-bottom:16px;"),
                         DTOutput("tabelType")
                     )
              )
            )
          )
        ),
        fluidRow(
          bs4Card(
            width = 12,
            title = tags$div(icon("balance-scale"), " Perbandingan Top 5"),
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,  # pastikan card terbuka
            fluidRow(
              column(6, 
                     div(style = "padding:12px;",
                         tags$h5("🏆 Top 5 Merk", style = "font-weight:600; text-align:center;"),
                         div(style = "padding-bottom:60px;",  # <-- padding ekstra
                             plotlyOutput("comparisonMerk", height = "450px")
                         )
                     )
              ),
              column(6, 
                     div(style = "padding:12px;",
                         tags$h5("🏆 Top 5 Type", style = "font-weight:600; text-align:center;"),
                         div(style = "padding-bottom:60px;",
                             plotlyOutput("comparisonType", height = "350px")
                         )
                     )
              )
            )
          )
        )
      )
    )
  ),
  footer = bs4DashFooter(
    left = tags$div(
      "Dashboard Penjualan Premium © 2025",
      style = "color:#666;"
    ),
    right = tags$div(
      icon("code"), " Built with R Shiny & bs4Dash",
      style = "color:#666;"
    )
  )
)

# -------------------------------
# SERVER
# -------------------------------
server <- function(input, output, session) {
  sel <- reactiveValues(nav = "primary", side = "primary", accent = "primary", side_dark = FALSE)
  
  for (clr in names(color_map)) {
    local({
      color_key <- clr
      nav_btn <- paste0("navbar_color_", color_key)
      side_btn <- paste0("sidebar_color_", color_key)
      acc_btn <- paste0("accent_color_", color_key)
      
      observeEvent(input[[nav_btn]], { sel$nav <- color_key }, ignoreInit = TRUE)
      observeEvent(input[[side_btn]], { sel$side <- color_key }, ignoreInit = TRUE)
      observeEvent(input[[acc_btn]], { sel$accent <- color_key }, ignoreInit = TRUE)
    })
  }
  
  observeEvent(input$sidebar_dark, { sel$side_dark <- isTRUE(input$sidebar_dark) }, ignoreInit = TRUE)
  
  theme_current <- reactive({
    nav_hex <- color_map[[sel$nav]]
    side_hex <- color_map[[sel$side]]
    acc_hex <- color_map[[sel$accent]]
    
    if (isTRUE(sel$side_dark)) {
      create_theme(
        bs4dash_vars(bs4dash_accent = acc_hex, navbar_light_color = nav_hex),
        bs4dash_sidebar_dark(bg = side_hex, submenu_bg = side_hex, submenu_color = "#fff"),
        bs4dash_color(blue = acc_hex)
      )
    } else {
      create_theme(
        bs4dash_vars(bs4dash_accent = acc_hex, navbar_light_color = nav_hex),
        bs4dash_sidebar_light(bg = side_hex, submenu_bg = side_hex, submenu_color = "#000"),
        bs4dash_color(blue = acc_hex)
      )
    }
  })
  
  output$ui_fresh_theme <- renderUI({ use_theme(theme_current()) })
  
  observe({
    runjs("$('.color-box').removeClass('selected');")
    runjs(sprintf("$('#%s').addClass('selected');", paste0("navbar_color_", sel$nav)))
    runjs(sprintf("$('#%s').addClass('selected');", paste0("sidebar_color_", sel$side)))
    runjs(sprintf("$('#%s').addClass('selected');", paste0("accent_color_", sel$accent)))
  })
  
  # Value Boxes
  output$vbox_total_records <- renderValueBox({
    df <- dataAnalisis()
    val <- if(!is.null(df)) nrow(df) else 0
    valueBox(value = format(val, big.mark = ".", decimal.mark = ","), subtitle = "Total Records", icon = icon("database"), color = "primary")
  })
  
  output$vbox_total_merk <- renderValueBox({
    df <- dataAnalisis()
    val <- if(!is.null(df) && "Merk" %in% names(df)) length(unique(df$Merk)) else 0
    valueBox(value = val, subtitle = "Jumlah Merk", icon = icon("tags"), color = "info")
  })
  
  output$vbox_total_type <- renderValueBox({
    df <- dataAnalisis()
    val <- if(!is.null(df) && "Type" %in% names(df)) length(unique(df$Type)) else 0
    valueBox(value = val, subtitle = "Jumlah Type", icon = icon("list"), color = "success")
  })
  
  output$vbox_avg_harga <- renderValueBox({
    df <- dataAnalisis()
    val <- if(!is.null(df) && "Harga" %in% names(df)) mean(df$Harga, na.rm = TRUE) else 0
    valueBox(value = format_rupiah(val), subtitle = "Rata-rata Harga", icon = icon("money-bill-wave"), color = "warning")
  })
  
  # Data Processing
  dataFile <- reactive({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    df <- tryCatch({
      if (ext == "csv") read_csv(input$file$datapath, show_col_types = FALSE)
      else if (ext %in% c("xlsx", "xls")) read_excel(input$file$datapath)
      else stop("Format file tidak didukung")
    }, error = function(e) {
      showNotification(paste("Error membaca file:", e$message), type = "error")
      return(NULL)
    })
    df
  })
  
  dataAnalisis <- reactive({
    df <- dataFile()
    req(df)
    col_merk <- detect_col(df, c("merk","brand","merek"))
    col_type <- detect_col(df, c("type","tipe"))
    col_harga <- detect_col(df, c("harga","price","total","amount","nominal","jumlah"))
    df <- safe_rename(df, col_merk, "Merk")
    df <- safe_rename(df, col_type, "Type")
    df <- safe_rename(df, col_harga, "Harga")
    if ("Harga" %in% names(df)) df$Harga <- suppressWarnings(as.numeric(df$Harga))
    df
  })
  
  observe({
    df <- dataAnalisis()
    req(df)
    choices <- if ("Merk" %in% names(df)) sort(unique(na.omit(df$Merk))) else character(0)
    updateSelectInput(session, "pilihMerk", choices = choices, selected = if(length(choices)) choices[1] else NULL)
  })
  
  dt_class <- reactive({
    if (isTRUE(sel$side_dark)) "cell-border stripe table-dark" else "stripe hover compact"
  })
  
  # Tables
  output$tabelData <- renderDT({
    df <- dataAnalisis()
    req(df)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), style = 'bootstrap4', class = dt_class(), filter = 'top', rownames = FALSE)
  })
  
  observeEvent(input$refresh_data, {
    showNotification("Data refreshed!", type = "message", duration = 2)
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste0("data_penjualan_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(dataAnalisis(), file, row.names = FALSE) }
  )
  
  output$tabelMerk <- renderDT({
    df <- dataAnalisis()
    req(df)
    if (!"Merk" %in% names(df)) return(datatable(data.frame(Info = "Kolom 'Merk' tidak ditemukan"), options = list(dom = 't')))
    hasil <- df %>%
      group_by(Merk) %>%
      summarise(total_unit = n(), rata_rata_harga = mean(Harga, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_unit))
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil, options = list(pageLength = 10, scrollX = TRUE), class = dt_class(), rownames = FALSE)
  })
  
  output$grafikMerkInteractive <- renderPlotly({
    df <- dataAnalisis()
    req(df)
    if (!"Merk" %in% names(df)) return(NULL)
    data_plot <- df %>%
      group_by(Merk) %>%
      summarise(total_unit = n(), .groups = "drop") %>%
      arrange(desc(total_unit))
    
    p <- plot_ly(data_plot, x = ~Merk, y = ~total_unit, type = 'bar', marker = list(color = color_map$primary)) %>%
      layout(title = "Total Unit per Merk", xaxis = list(title = ""), yaxis = list(title = "Jumlah Unit"), margin = list(b = 80))
    
    config(p, displayModeBar = "hover", displaylogo = FALSE)
  })
  
  output$grafikTypeInteractive <- renderPlotly({
    df <- dataAnalisis()
    req(df)
    merk_sel <- input$pilihMerk
    req(merk_sel)
    df <- df %>% filter(Merk == merk_sel)
    if (!"Type" %in% names(df)) return(NULL)
    data_plot <- df %>%
      group_by(Type) %>%
      summarise(total_unit = n(), .groups = "drop") %>%
      arrange(desc(total_unit))
    p <- plot_ly(data_plot, x = ~Type, y = ~total_unit, type = 'bar', marker = list(color = color_map$warning)) %>%
      layout(title = paste("Total Unit per Type -", merk_sel), xaxis = list(title = ""), yaxis = list(title = "Jumlah Unit"), margin = list(b = 80))
    config(p, displayModeBar = "hover", displaylogo = FALSE)
  })
  
  output$tabelType <- renderDT({
    df <- dataAnalisis()
    req(df)
    merk_sel <- input$pilihMerk
    req(merk_sel)
    df <- df %>% filter(Merk == merk_sel)
    if (!"Type" %in% names(df)) return(datatable(data.frame(Info = "Kolom 'Type' tidak ditemukan"), options = list(dom = 't')))
    hasil <- df %>%
      group_by(Type) %>%
      summarise(total_unit = n(), rata_rata_harga = mean(Harga, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_unit))
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil, options = list(pageLength = 10, scrollX = TRUE), class = dt_class(), rownames = FALSE)
  })
  
  output$comparisonMerk <- renderPlotly({
    df <- dataAnalisis()
    req(df)
    data_plot <- df %>%
      group_by(Merk) %>%
      summarise(total_unit = n(), .groups = "drop") %>%
      arrange(desc(total_unit)) %>% head(5)
    p <- plot_ly(data_plot, x = ~Merk, y = ~total_unit, type = 'bar', marker = list(color = color_map$danger)) %>%
      layout(title = "Top 5 Merk Terlaris", margin = list(b = 80))
    config(p, displayModeBar = "hover", displaylogo = FALSE)
  })
  
  output$comparisonType <- renderPlotly({
    df <- dataAnalisis()
    req(df)
    merk_sel <- input$pilihMerk
    if (is.null(merk_sel)) return(NULL)
    data_plot <- df %>%
      filter(Merk == merk_sel) %>%
      group_by(Type) %>%
      summarise(total_unit = n(), .groups = "drop") %>%
      arrange(desc(total_unit)) %>% head(5)
    p <- plot_ly(data_plot, x = ~Type, y = ~total_unit, type = 'bar', marker = list(color = color_map$teal)) %>%
      layout(title = paste("Top 5 Type -", merk_sel), margin = list(b = 80))
    config(p, displayModeBar = "hover", displaylogo = FALSE)
  })
  
  output$download_merk_table <- downloadHandler(
    filename = function() { paste0("data_merk_", Sys.Date(), ".csv") },
    content = function(file) {
      df <- dataAnalisis()
      req(df)
      hasil <- df %>%
        group_by(Merk) %>%
        summarise(total_unit = n(), rata_rata_harga = mean(Harga, na.rm = TRUE), .groups = "drop")
      write.csv(hasil, file, row.names = FALSE)
    }
  )
  
  output$download_type_table <- downloadHandler(
    filename = function() { paste0("data_type_", Sys.Date(), ".csv") },
    content = function(file) {
      df <- dataAnalisis()
      req(df)
      merk_sel <- input$pilihMerk
      req(merk_sel)
      hasil <- df %>%
        filter(Merk == merk_sel) %>%
        group_by(Type) %>%
        summarise(total_unit = n(), rata_rata_harga = mean(Harga, na.rm = TRUE), .groups = "drop")
      write.csv(hasil, file, row.names = FALSE)
    }
  )
}

# -------------------------------
# RUN APP
# -------------------------------
shinyApp(ui, server)
