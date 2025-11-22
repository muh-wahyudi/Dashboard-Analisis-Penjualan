# app.R (Theme Customizer - Model A, full)
library(shiny)
library(bs4Dash)
library(shinyjs)
library(fresh)
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

safe_rename <- function(df, old, new) {
  if (!is.na(old) && old %in% names(df)) {
    names(df)[names(df) == old] <- new
  }
  df
}

# -------------------------------
# Color palette (named keys)
# -------------------------------
color_map <- list(
  primary = "#007bff", secondary = "#6c757d", info = "#17a2b8", success = "#28a745",
  warning = "#ffc107", danger = "#dc3545", dark = "#343a40", light = "#f8f9fa",
  purple = "#6f42c1", orange = "#fd7e14", teal = "#20c997", pink = "#e83e8c"
)

# Small helper to render clickable color boxes in UI
colorBoxInput <- function(id, label, colors) {
  tagList(
    tags$h5(label),
    div(style = "display:flex; flex-wrap:wrap; gap:8px; margin-bottom:8px;",
        lapply(names(colors), function(clr) {
          actionButton(
            inputId = paste0(id, "_", clr),
            label = NULL,
            style = paste0(
              "background-color:", colors[[clr]], ";",
              "border:2px solid transparent; width:28px; height:28px; border-radius:4px; cursor:pointer;"
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
  title = "Dashboard Penjualan - Themer (Model A)",
  header = bs4DashNavbar(),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    bs4SidebarMenu(
      bs4SidebarHeader("Dashboard"),
      bs4SidebarMenuItem("Dashboard Penjualan", tabName = "data", icon = icon("table")),
      bs4SidebarMenuItem("Analisis Penjualan", tabName = "analisis", icon = icon("chart-line"))
    )
  ),
  controlbar = bs4DashControlbar(
    bs4Card(
      title = "Theme Customizer",
      solidHeader = TRUE,
      width = 12,
      # Navbar themer
      colorBoxInput("navbar_color", "Navbar Themer", color_map),
      # Sidebar themer + dark toggle
      colorBoxInput("sidebar_color", "Sidebar Themer", color_map),
      checkboxInput("sidebar_dark", "Sidebar Dark Mode", FALSE),
      # Accent themer
      colorBoxInput("accent_color", "Accents Themer (Primary)", color_map),
      tags$hr(),
      p(style = "font-size:12px; color:#666;", "Click a color box to apply theme. Model A: immediate theme update.")
    )
  ),
  body = bs4DashBody(
    useShinyjs(),
    uiOutput("ui_fresh_theme"),    # inject theme dynamically
    tags$style(HTML("
      /* highlight selected color */
      .color-box.selected { border-color: #000 !important; box-shadow: 0 0 6px rgba(0,0,0,0.25); }
      /* spacing fix for controlbar */
      .controlbar .card { margin-bottom:12px; }
    ")),
    
    bs4TabItems(
      # data tab
      bs4TabItem(
        tabName = "data",
        fluidRow(
          bs4Card(
            width = 12, title = "Upload File", status = "primary", solidHeader = TRUE,
            fileInput("file", "File CSV / Excel", accept = c(".csv",".xlsx"))
          )
        ),
        fluidRow(
          bs4Card(
            width = 12, title = "Preview Data", status = "primary", solidHeader = TRUE,
            DTOutput("tabelData")
          )
        )
      ),
      # analysis tab
      bs4TabItem(
        tabName = "analisis",
        fluidRow(
          bs4Card(
            width = 12,
            title = "Grafik & Tabel Penjualan Berdasarkan Merk",
            status = "info", solidHeader = TRUE,
            plotOutput("grafikMerk", height = "450px"),
            DTOutput("tabelMerk")
          )
        ),
        fluidRow(
          bs4Card(
            width = 12,
            title = "Pilih Merk untuk Melihat Penjualan Berdasarkan Type",
            status = "warning", solidHeader = TRUE,
            selectInput("pilihMerk", "Pilih Merk:", choices = NULL),
            plotOutput("grafikType", height = "450px"),
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
  # reactive values to store selected keys (names from color_map)
  sel <- reactiveValues(nav = "primary", side = "primary", accent = "primary", side_dark = FALSE)
  
  # Create observer for each color box button to update reactive value (navbar, sidebar, accent)
  # We create observers dynamically to keep code concise
  for (clr in names(color_map)) {
    local({
      color_key <- clr
      nav_btn <- paste0("navbar_color_", color_key)
      side_btn <- paste0("sidebar_color_", color_key)
      acc_btn <- paste0("accent_color_", color_key)
      
      observeEvent(input[[nav_btn]], {
        sel$nav <- color_key
      }, ignoreInit = TRUE)
      
      observeEvent(input[[side_btn]], {
        sel$side <- color_key
      }, ignoreInit = TRUE)
      
      observeEvent(input[[acc_btn]], {
        sel$accent <- color_key
      }, ignoreInit = TRUE)
    })
  }
  
  # Sidebar dark mode toggle
  observeEvent(input$sidebar_dark, {
    sel$side_dark <- isTRUE(input$sidebar_dark)
  }, ignoreInit = TRUE)
  
  # Build reactive fresh theme based on selected colors
  theme_current <- reactive({
    # map keys to hex
    nav_hex <- color_map[[sel$nav]]
    side_hex <- color_map[[sel$side]]
    acc_hex <- color_map[[sel$accent]]
    
    if (isTRUE(sel$side_dark)) {
      create_theme(
        bs4dash_vars(
          bs4dash_accent = acc_hex,
          navbar_light_color = nav_hex,
          navbar_light_active_color = nav_hex,
          navbar_light_hover_color = nav_hex
        ),
        bs4dash_sidebar_dark(
          bg = side_hex,
          submenu_bg = side_hex,
          submenu_color = "#fff"
        ),
        bs4dash_color(
          blue = acc_hex
        )
      )
    } else {
      create_theme(
        bs4dash_vars(
          bs4dash_accent = acc_hex,
          navbar_light_color = nav_hex,
          navbar_light_active_color = nav_hex,
          navbar_light_hover_color = nav_hex
        ),
        bs4dash_sidebar_light(
          bg = side_hex,
          submenu_bg = side_hex,
          submenu_color = "#000"
        ),
        bs4dash_color(
          blue = acc_hex
        )
      )
    }
  })
  
  # Inject theme into UI reactively
  output$ui_fresh_theme <- renderUI({
    theme <- theme_current()
    use_theme(theme)
  })
  
  # Visual highlight selected boxes (add/remove class)
  observe({
    # remove selected style first
    runjs("$('.color-box').removeClass('selected');")
    # add selected to navbar
    runjs(sprintf("$('#%s').addClass('selected');", paste0("navbar_color_", sel$nav)))
    # add selected to sidebar
    runjs(sprintf("$('#%s').addClass('selected');", paste0("sidebar_color_", sel$side)))
    # add selected to accent
    runjs(sprintf("$('#%s').addClass('selected');", paste0("accent_color_", sel$accent)))
  })
  
  # -------------------------------
  # Application: data handling (unchanged behavior)
  # -------------------------------
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
  
  # update merk choices
  observe({
    df <- dataAnalisis()
    req(df)
    choices <- if ("Merk" %in% names(df)) sort(unique(na.omit(df$Merk))) else character(0)
    updateSelectInput(session, "pilihMerk", choices = choices, selected = if(length(choices)) choices[1] else NULL)
  })
  
  dt_class <- reactive({
    if (isTRUE(sel$side_dark)) "cell-border stripe table-dark" else "stripe hover compact"
  })
  
  output$tabelData <- renderDT({
    df <- dataAnalisis()
    req(df)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), style = 'bootstrap4', class = dt_class())
  })
  
  output$tabelMerk <- renderDT({
    df <- dataAnalisis()
    req(df)
    if (!"Merk" %in% names(df)) {
      return(datatable(data.frame(Info = "Kolom 'Merk' tidak ditemukan"), options = list(dom = 't')))
    }
    hasil <- df %>%
      group_by(Merk) %>%
      summarise(total_unit = n(), rata_rata_harga = mean(Harga, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_unit))
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil, options = list(pageLength = 10, scrollX = TRUE), style = 'bootstrap4', class = dt_class())
  })
  
  output$tabelType <- renderDT({
    req(input$pilihMerk)
    df <- dataAnalisis()
    req(df)
    if (!"Type" %in% names(df)) {
      return(datatable(data.frame(Info = "Kolom 'Type' tidak ditemukan"), options = list(dom = 't')))
    }
    df2 <- df %>% filter(Merk == input$pilihMerk)
    hasil <- df2 %>%
      group_by(Type) %>%
      summarise(total_unit = n(), rata_rata_harga = mean(Harga, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_unit))
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil, options = list(pageLength = 10, scrollX = TRUE), style = 'bootstrap4', class = dt_class())
  })
  
  # Graph theme (keeps base font larger)
  theme_dynamic <- reactive({
    if (isTRUE(sel$side_dark)) {
      theme_minimal(base_size = 16) +
        theme(
          plot.background = element_rect(fill = "#2b2b2b"),
          panel.background = element_rect(fill = "#2b2b2b"),
          panel.grid = element_line(color = "#444444"),
          axis.text = element_text(color = "white", size = 14),
          axis.title = element_text(color = "white", size = 16, face = "bold"),
          plot.title = element_text(color = "white", size = 18, face = "bold", hjust = 0.5)
        )
    } else {
      theme_minimal(base_size = 16) +
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(color = "#e0e0e0"),
          axis.text = element_text(color = "black", size = 14),
          axis.title = element_text(color = "black", size = 16, face = "bold"),
          plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5)
        )
    }
  })
  
  output$grafikMerk <- renderPlot({
    df <- dataAnalisis()
    req(df)
    if (!"Merk" %in% names(df)) { plot.new(); title("Kolom 'Merk' tidak ditemukan"); return() }
    summaryMerk <- df %>% group_by(Merk) %>% summarise(total_unit = n(), .groups = "drop") %>% arrange(desc(total_unit))
    summaryMerk$Merk_wrap <- sapply(summaryMerk$Merk, function(x) paste(strwrap(x, width = 16), collapse = "\n"))
    max_y <- max(summaryMerk$total_unit, na.rm = TRUE)
    ggplot(summaryMerk, aes(x = reorder(Merk_wrap, total_unit), y = total_unit, fill = total_unit)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = total_unit), vjust = -0.8, size = 6, fontface = "bold") +
      coord_cartesian(ylim = c(0, max_y * 1.25)) +
      scale_fill_gradient(low = "#3b82f6", high = "#06b6d4") +
      labs(title = "Grafik Penjualan per Merk", x = "Merk", y = "Total Terjual") +
      theme_dynamic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14), plot.margin = margin(18,12,50,12))
  })
  
  output$grafikType <- renderPlot({
    req(input$pilihMerk)
    df <- dataAnalisis() %>% filter(Merk == input$pilihMerk)
    req(nrow(df) > 0)
    if (!"Type" %in% names(df)) { plot.new(); title("Kolom 'Type' tidak ditemukan"); return() }
    summaryType <- df %>% group_by(Type) %>% summarise(total_unit = n(), .groups = "drop") %>% arrange(desc(total_unit))
    summaryType$Type_wrap <- sapply(summaryType$Type, function(x) paste(strwrap(x, width = 16), collapse = "\n"))
    max_y <- max(summaryType$total_unit, na.rm = TRUE)
    ggplot(summaryType, aes(x = reorder(Type_wrap, total_unit), y = total_unit, fill = total_unit)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = total_unit), vjust = -0.8, size = 5, fontface = "bold") +
      coord_cartesian(ylim = c(0, max_y * 1.2)) +
      scale_fill_gradient(low = "#f97316", high = "#facc15") +
      labs(title = paste("Grafik Type untuk Merk:", input$pilihMerk), x = "Type", y = "Total Terjual") +
      theme_dynamic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13), plot.margin = margin(12,12,40,12))
  })
}

# -------------------------------
# Run App
# -------------------------------
shinyApp(ui, server)
