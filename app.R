library(shiny)
library(bs4Dash)
library(DBI)
library(RSQLite)
library(sodium)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(shinyjs)
library(waiter)
library(plotly)
library(shinycssloaders)
library(fresh)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(rlang)

# -----------------------
# CONFIG
# -----------------------
DB_FILE <- "users.sqlite"
FREE_UPLOADS <- 0
APP_TITLE <- "KELOMPOK 2"

# -----------------------
# DATABASE INIT & HELPERS
# -----------------------

db_connect <- function() DBI::dbConnect(SQLite(), DB_FILE)

init_db <- function(db_file = DB_FILE) {
  tryCatch({
    con <- dbConnect(SQLite(), db_file)
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Password default untuk semua admin baru
    admin_pw <- password_store("admin123")
    
    # DAFTAR 4 ADMIN
    target_admins <- list(
      list(email = "FIRMAN@local", role = "admin", stars = 9999),
      list(email = "WAHYUDI@local", role = "admin", stars = 9999),
      list(email = "LUTFI@local", role = "admin", stars = 9999),
      list(email = "Zayyan@local", role = "admin", stars = 9999)
    )
    
    # AKUN YANG AKAN DIHAPUS
    emails_to_delete <- c("admin@local", "demo@local")
    
    if (!"users" %in% dbListTables(con)) {
      # INI ADALAH FUNGSI CREATE TABLE AWAL
      dbExecute(con, "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, email TEXT UNIQUE NOT NULL, password_hash TEXT NOT NULL, role TEXT NOT NULL DEFAULT 'user', stars INTEGER NOT NULL DEFAULT 3, uploads_used INTEGER NOT NULL DEFAULT 0, created_at TEXT DEFAULT CURRENT_TIMESTAMP, photo_path TEXT DEFAULT 'default_user.jpg');")
      
      # Masukkan 4 Admin Baru
      for (admin in target_admins) {
        dbExecute(con, "INSERT INTO users (email, password_hash, role, stars, uploads_used, photo_path) VALUES (?, ?, ?, ?, 0, 'default_user.jpg')", 
                  params = list(admin$email, admin_pw, admin$role, admin$stars))
      }
      
    } else {
      # PENAMBAHAN KOLOM photo_path JIKA TABEL SUDAH ADA
      existing_cols <- dbListFields(con, "users")
      if (!"photo_path" %in% existing_cols) {
        dbExecute(con, "ALTER TABLE users ADD COLUMN photo_path TEXT DEFAULT 'default_user.jpg'")
      }
      
      for (admin in target_admins) {
        existing <- dbGetQuery(con, "SELECT COUNT(*) FROM users WHERE email = ?", params = list(admin$email))$"COUNT(*)"
        if (existing == 0) {
          dbExecute(con, "INSERT INTO users (email, password_hash, role, stars, uploads_used, photo_path) VALUES (?, ?, ?, ?, 0, 'default_user.jpg')", 
                    params = list(admin$email, admin_pw, admin$role, admin$stars))
        } else {
          dbExecute(con, "UPDATE users SET role = 'admin', stars = 9999, password_hash = ? WHERE email = ?", 
                    params = list(admin_pw, admin$email))
        }
      }
    }
    
    # HAPUS AKUN YANG TIDAK DIINGINKAN (admin@local dan demo@local)
    for (email in emails_to_delete) {
      dbExecute(con, "DELETE FROM users WHERE email = ?", params = list(email))
    }
    
    TRUE
  }, error = function(e) {
    message("Error initializing DB: ", e$message); FALSE
  })
}
init_db()

# --- HELPER DB LAINNYA ---
get_user_by_email <- function(email) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  res <- dbGetQuery(con, "SELECT * FROM users WHERE email = ?", params = list(email))
  if (nrow(res) == 0) return(NULL)
  
  user_data <- res[1, ]
  user_data$password_hash <- NULL 
  
  return(user_data)
}

verify_credentials <- function(email, password) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  rec <- dbGetQuery(con, "SELECT * FROM users WHERE email = ?", params = list(email))
  if (nrow(rec) == 0) return(NULL)
  
  ok <- tryCatch(password_verify(rec$password_hash[1], password), error = function(e) FALSE)
  
  if (isTRUE(ok)) {
    user_data <- rec[1, ]
    user_data$password_hash <- NULL 
    return(user_data)
  } else {
    return(NULL)
  }
}
create_user <- function(email, password, initial_stars = 3, role = "user") {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  pw_hash <- password_store(password)
  tryCatch({
    dbExecute(con, "INSERT INTO users (email, password_hash, role, stars, uploads_used, photo_path) VALUES (?, ?, ?, ?, 0, 'default_user.jpg')", params = list(email, pw_hash, role, as.integer(initial_stars)))
    TRUE
  }, error = function(e) FALSE)
}

# --- FUNGSI BARU UNTUK UPDATE FOTO PROFIL ---
update_user_photo_path <- function(email, photo_path) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  tryCatch({
    dbExecute(con, "UPDATE users SET photo_path = ? WHERE email = ?", params = list(photo_path, email))
    TRUE
  }, error = function(e) FALSE)
}
# --- END FUNGSI BARU ---

fetch_all_users <- function() {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbGetQuery(con, "SELECT id, email, role, stars, uploads_used, created_at FROM users ORDER BY created_at DESC")
}
update_user_stars <- function(email, delta) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "BEGIN")
  user <- dbGetQuery(con, "SELECT stars FROM users WHERE email = ?", params = list(email))
  if (nrow(user) == 0) { dbExecute(con, "ROLLBACK"); return(FALSE) }
  new_stars <- as.integer(user$stars[1]) + as.integer(delta)
  if (new_stars < 0) { dbExecute(con, "ROLLBACK"); return(FALSE) }
  dbExecute(con, "UPDATE users SET stars = ? WHERE email = ?", params = list(new_stars, email))
  dbExecute(con, "COMMIT"); TRUE
}
increment_upload_used <- function(email) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "UPDATE users SET uploads_used = uploads_used + 1 WHERE email = ?", params = list(email)); TRUE
}
reset_uploads_used <- function(email) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "UPDATE users SET uploads_used = 0 WHERE email = ?", params = list(email)); TRUE
}
delete_user_by_email <- function(email) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  user_to_delete <- get_user_by_email(email)
  if (is.null(user_to_delete)) return("User not found")
  if (identical(user_to_delete$role, "admin")) { return("Cannot delete admin account") }
  tryCatch({ dbExecute(con, "DELETE FROM users WHERE email = ?", params = list(email)); return("Success") }, error = function(e) { message("DB Error deleting user: ", e$message); return("DB Error") })
}
# -------------------------------
# Helper Functions (General)
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
get_outliers <- function(data_vector) {
  if (length(data_vector) < 4) return(numeric(0))
  Q1 <- quantile(data_vector, 0.25, na.rm = TRUE)
  Q3 <- quantile(data_vector, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data_vector[data_vector < lower_bound | data_vector > upper_bound]
}
# -----------------------
# Helper: loading HTML (Waiter)
# -----------------------
loading_html <- function() {
  tagList(
    tags$div(style = "display:flex; flex-direction:column; align-items:center; justify-content:center;",
             tags$style(HTML(".cw-spinner { width:80px; height:80px; border:8px solid #f3f3f3; border-top:8px solid #3498db; border-radius:50%; animation: cw-spin 1s linear infinite; margin-bottom:12px; } @keyframes cw-spin { to { transform: rotate(360deg); } } .cw-loader-wrap { display:flex; flex-direction:column; align-items:center; justify-content:center; }")),
             tags$div(class = "cw-loader-wrap",
                      tags$div(class = "cw-spinner"),
                      tags$h4("Memuat aplikasi...")
             )
    )
  )
}
# -------------------------------
# Color palette (Theme Customizer)
# -------------------------------
color_map <- list(
  primary = "#ffc107", secondary = "#6c757d", info = "#17a2b8", success = "#28a745",
  warning = "#f8f9fa", danger = "#dc3545", dark = "#343a3e", light = "#007bff",
  purple = "#6f42c1", orange = "#fd7e14", teal = "#20c997", pink = "#e83e8c", terbaik ="#f5eded"
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
  title = APP_TITLE,
  
  header = bs4DashNavbar(
    title = tags$div(
      APP_TITLE,
      style = "font-weight:700; font-size:20px; display:flex; align-items:center; justify-content:center; padding-bottom:25px; border-bottom:3px solid #000000;"
    ),
    rightUi = uiOutput("auth_buttons_ui")
  ),
  
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    
    # --- MODIFIKASI UNTUK FITUR FOTO DINAMIS YANG DAPAT DIKLIK ---
    div(id = "user_panel_container", 
        uiOutput("dynamic_user_panel")
    ),
    
    
    # --- END MODIFIKASI FOTO DINAMIS ---
    
    div(
      style = "color:#555; font-size:13px; margin-left:15px; margin-top:-10px; margin-bottom:10px;",
      uiOutput("sidebar_user_uploads")
    ),
    
    bs4SidebarMenu(
      bs4SidebarHeader("MENU UTAMA"),
      bs4SidebarMenuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
      
      bs4SidebarHeader("ANALISIS"),
      bs4SidebarMenuItem("Grafik", tabName = "grafik", icon = icon("chart-area")),
      bs4SidebarMenuItem("Plot Kustom", tabName = "kustom", icon = icon("gears")),
      bs4SidebarMenuItem("Histori Data", tabName = "history", icon = icon("history")),
      
      bs4SidebarHeader("LAINNYA"),
      bs4SidebarMenuItem("Profile Team", tabName = "team", icon = icon("users")),
      
      uiOutput("admin_panel_menu_ui")
    )
  ),
  
  controlbar = bs4DashControlbar(
    skin = "light",
    title = "Theme Customizer",
    bs4Card(
      title = tags$div(icon("palette"), "COSTUM TEMA"),
      solidHeader = TRUE,
      width = 12,
      status = "primary",
      checkboxInput("sidebar_dark", "Mode Sidebar ", FALSE),
      colorBoxInput("navbar_color", "🎨 Warna Navbar", color_map),
      colorBoxInput("sidebar_color", "📌 Warna Sidebar", color_map),
      colorBoxInput("accent_color", "✨ Warna Aksen", color_map),
      tags$hr(style="margin:20px 0; border-top:2px solid #000000;"),
      div(style="padding:12px; background:#f8f9fa; border-radius:6px;",
          tags$p(style="font-size:13px; color:#666; margin:0; line-height:1.6;",
                 icon("info-circle"), " Klik kotak warna untuk mengubah tema.",
                 tags$br(),
                 tags$small("💡 Tip: Coba kombinasi dengan Dark Mode!")
          )
      )
    )
  ),
  
  body = bs4DashBody(
    useShinyjs(),
    use_waiter(),
    waiter_show_on_load(html = loading_html(), color = "#ffffff"),
    uiOutput("ui_fresh_theme"),
    
    tags$head(
      tags$link(rel = "icon", href = "favicon.png", type = "image/x-icon")
    ),
    
    tags$head(
      tags$style(HTML("
      /* Dark Mode DT styling */
      .dark-mode .dataTable, .dark-mode .dataTables_info, .dark-mode .dataTables_filter label, .dark-mode .dataTables_length label, .dark-mode .dataTables_paginate a { color: #ffffff !important; }
      .dark-mode table.dataTable tbody td { color: #ffffff !important; }
      .dark-mode table.dataTable thead th { color: #ffffff !important; }
      .dark-mode .dataTables_filter input, .dark-mode .dataTables_length select { background-color: #2f2f2f !important; color: #ffffff !important; border: 1px solid #555 !important; }
      .badge-star { background-color: #ffc107; color: #343a40; font-size: 80%; padding: 4px 8px; border-radius: 4px; font-weight: bold; margin-left: 5px; } 
      .progress-bar-container { height: 20px; background-color: #e9ecef; border-radius: 4px; overflow: hidden; margin-top: 10px; }
      .progress-bar { height: 100%; width: 0; background-color: #007bff; text-align: center; color: white; transition: width 0.4s ease; }
      .upload-status { margin-top: 10px; font-style: italic; color: #6c757d; }
      .card-text-light { color: #f8f9fa !important; }
      #user_panel_container img { cursor: pointer; } /* CSS Baru: Kursor pointer pada foto */
      "))
    ),
    
    bs4TabItems(
      
      # ================= TAB HOME (ACCOUNT/UPLOAD) =================
      bs4TabItem(
        tabName = "dashboard",
        fluidRow(
          bs4Card(width = 12, title = "Status Account", status = "primary", solidHeader = TRUE,
                  uiOutput("status_card_ui")
          )
        ),
        fluidRow(
          bs4Card(width = 6, title = "Upload Data Premium", status = "primary", solidHeader = TRUE,
                  uiOutput("upload_ui"),
                  downloadButton("download_data_premium", "Download Data (CSV)", class = "btn-secondary", icon = icon("download"))
          ),
          bs4Card(width = 6, title = "Account Summary", status = "primary", solidHeader = TRUE,
                  uiOutput("summary_ui"),
                  actionButton("btn_show_help", label = "Bantuan & Format Data", icon = icon("question-circle"), class="btn-info", style="margin-left:10px;")
          )
        ),
        fluidRow(
          # Insight otomatis
          box(
            width = 12,
            title = "Insight Otomatis",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("insightBox")
          )
        )
      ),
      
      # ================= TAB ANALISIS =================
      bs4TabItem(
        tabName = "grafik",
        fluidRow(
          valueBoxOutput("vbox_total_records", width = 3),
          valueBoxOutput("vbox_total_merk", width = 3),
          valueBoxOutput("vbox_total_type", width = 3),
          valueBoxOutput("vbox_total_sales", width = 3)
        ),
        fluidRow(
          valueBoxOutput("vbox_avg_harga", width = 3)
        ),
        
        fluidRow(
          bs4Card(
            width = 12,
            title = tags$div(icon("table"), " Preview Data"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            div(style="margin-bottom:12px;",
                downloadButton("download_data", "Download Data", class="btn-primary btn-download", icon=icon("download")),
                actionButton("refresh_data", "Refresh", class="btn-info btn-download", icon=icon("sync"))
            ),
            DTOutput("tabelData")
          )
        ),
        
        fluidRow(
          bs4Card(
            width = 12,
            title = tags$div(icon("chart-bar"), " Analisis Penjualan per Merk"),
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(6,
                     selectInput("pilih_grafik_merk", "Pilih Tipe Grafik:", choices = c("Bar"="bar","Line"="line","Pie"="pie"), selected="bar"),
                     plotlyOutput("grafikMerkInteractive", height="450px"),
                     downloadButton("download_merk_table", "Download Data", class="btn-success btn-sm", icon=icon("download"))
              ),
              column(6, DTOutput("tabelMerk"))
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
              column(6,
                     selectInput("pilihMerk", "Pilih Merk:", choices=NULL, width="100%"),
                     selectInput("pilih_grafik_type", "Pilih Tipe Grafik:", choices = c("Bar"="bar","Line"="line","Pie"="pie"), selected="bar")
              )
            ),
            fluidRow(
              column(6, plotlyOutput("grafikTypeInteractive", height="450px"),
                     downloadButton("download_type_table", "Download Data", class="btn-warning btn-sm", icon=icon("download"))),
              column(6, DTOutput("tabelType"))
            )
          )
        ),
        
        fluidRow(
          bs4Card(
            width = 6,
            title = tags$div(icon("chart-pie"), " Analisis Distribusi Harga (Box Plot)"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("boxPlotHarga", height="400px"),
            tags$small(style="color:#666; margin-top:10px; display:block;",
                       icon("info-circle"), " Grafik ini menunjukkan median (garis tengah), quartil, dan outlier (titik) harga jual setiap merek."
            )
          ),
          bs4Card(
            width = 6,
            title = tags$div(icon("exclamation-triangle"), " Data Harga Outlier (Ekstrem)"),
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("tabelOutliers"),
            tags$small(style="color:#666; margin-top:10px; display:block;",
                       icon("exclamation-circle"), " Harga yang berada di luar batas 1.5 * IQR."
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
            collapsed = TRUE,
            fluidRow(
              column(6,
                     selectInput("pilih_grafik_top5", "Pilih Tipe Grafik:", choices = c("Bar"="bar","Pie"="pie"), selected="bar"),
                     plotlyOutput("comparisonMerk", height="350px")
              ),
              column(6, plotlyOutput("comparisonType", height="350px"))
            )
          )
        )
      ),
      
      # ================= TAB PLOT KUSTOM =================
      bs4TabItem(
        tabName = "kustom",
        fluidRow(
          bs4Card(
            width = 12, title = tags$div(icon("gears"), " Pembuatan Plot Kustom"),
            status = "secondary", solidHeader = TRUE,
            fluidRow(
              column(4,
                     selectInput("kustom_x_axis", "Sumbu X (Kategorikal):", choices=NULL),
                     selectInput("kustom_y_axis", "Sumbu Y (Numerik):", choices=NULL)
              ),
              column(4,
                     selectInput("kustom_plot_type", "Tipe Grafik:", choices=c("Bar" = "bar", "Box Plot" = "box", "Histogram" = "hist"))
              ),
              column(4,
                     uiOutput("kustom_kategori_select")
              )
            ),
            plotlyOutput("kustomPlotOutput", height = "550px")
          )
        )
      ),
      
      # ================= TAB HISTORI DATA =================
      bs4TabItem(
        tabName = "history",
        fluidRow(
          bs4Card(
            width = 12, title = tags$div(icon("history"), " Histori Data Upload"),
            status = "teal", solidHeader = TRUE,
            uiOutput("history_ui"),
            tags$hr(),
            DTOutput("history_data_preview")
          )
        )
      ),
      
      # ================= TAB ADMIN =================
      bs4TabItem(
        tabName = "admin",
        fluidRow(
          bs4Card(width = 12, title = "Admin Panel - Manage Stars/Users", status = "danger", solidHeader = TRUE,
                  fluidRow(
                    column(width = 4,
                           textInput("admin_search_email", "Cari user (email)"),
                           actionButton("admin_search_btn", "Cari", class = "btn-primary", icon = icon("search"), style = "width:100%; margin-top:6px;")
                    ),
                    column(width = 8,
                           DTOutput("admin_users_dt")
                    )
                  ),
                  hr(),
                  fluidRow(
                    column(width = 4,
                           uiOutput("admin_selected_ui"),
                           numericInput("admin_star_delta", "Jumlah bintang (positif=Tambah, negatif=Kurangi)", value = 1, min = -1000, max = 1000),
                           actionButton("admin_apply_btn", "Apply", class = "btn-success", icon = icon("check"), style = "width:100%; margin-top:8px;"),
                           actionButton("admin_delete_btn", "Hapus User", class = "btn-danger", icon = icon("user-slash"), style = "width:100%; margin-top:8px;")
                    ),
                    column(width = 8,
                           verbatimTextOutput("admin_log")
                    )
                  )
          )
        )
      ),
      
      # ================= TAB TEAM =================
      bs4TabItem(
        tabName = "team",
        fluidRow(
          bs4Card(
            width = 12,
            title = tags$div(icon("users"), " Profile Team"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(4,
                     div(style="text-align:center; padding:20px;",
                         img(src="foto ke 1.jpg", width="90%", style="margin-bottom:15px;"),
                         tags$h4("FIRMAN SYAH"),
                         tags$p("Peran: Data Cleaning"),
                         tags$a(
                           href = "https://wa.me/6287726993572?text=Halo%20saya%20tertarik%20untuk%20membeli%20stars%20di%20Dashboard%20Penjualan?",
                           target = "_blank",
                           class = "btn btn-success btn-sm",
                           icon("whatsapp"), " Beli Stars (WhatsApp)"
                         )
                     )),
              column(4,
                     div(style="text-align:center; padding:20px;",
                         img(src="foto2.jpg", width="90%", style="margin-bottom:15px;"),
                         tags$h4("MUH.WAHYUDI"),
                         tags$p("Peran: Data Visualization"),
                         tags$a(
                           href =  "https://wa.me/6287846327517?text=Halo%20saya%20tertarik%20untuk%20membeli%20stars%20di%20Dashboard%20Penjualan?",
                           target = "_blank",
                           class = "btn btn-success btn-sm",
                           icon("whatsapp"), " Beli Stars (WhatsApp)"
                         )
                         
                     )),
              column(4,
                     div(style="text-align:center; padding:20px;",
                         img(src="foto ke 2 .jpg", width="90%", style="margin-bottom:15px;"),
                         tags$h4("LUTFI ZHAFRAN"),
                         tags$p("Peran: App Developer"),
                         tags$a(
                           href =  "https://wa.me/6287726993572?text=Halo%20saya%20tertarik%20untuk%20membeli%20stars%20di%20Dashboard%20Penjualan?",
                           target = "_blank",
                           class = "btn btn-success btn-sm",
                           icon("whatsapp"), " Beli Stars (WhatsApp)"
                         )
                         
                     )),
              column( width = 4,
                      offset = 4,
                      div(style="text-align:center; padding:20px;",
                          img(src="foto ke 3.jpg", width="90%", style="margin-bottom:15px;"),
                          tags$h4("Nur Badilla Zayyan"),
                          tags$p("Peran: App Developer"),
                          
                          tags$a(
                            href =  "https://wa.me/6287726993572?text=Halo%20saya%20tertarik%20untuk%20membeli%20stars%20di%20Dashboard%20Penjualan?",
                            target = "_blank",
                            class = "btn btn-success btn-sm",
                            icon("whatsapp"), " Beli Stars (WhatsApp)"
                          )
                      ))
            )
          )
        )
      )
    )
  ),
  
  footer = bs4DashFooter(
    left = "Dashboard Analisis Penjualan",
    right = tagList(icon("code"), "Kelompok 2")
  )
)

# -----------------------
# SERVER
# -----------------------
server <- function(input, output, session) {
  
  Sys.sleep(1)
  waiter_hide()
  
  user_session <- reactiveValues(logged_in = FALSE, email = NULL, role = NULL)
  rv <- reactiveValues(
    data = NULL,
    data_history = list(),
    admin_selected_email = NULL,
    user_update_trigger = 0,
    upload_progress = 0
  )
  
  # Logika untuk menampilkan menu Admin Panel
  output$admin_panel_menu_ui <- renderUI({
    if (isTRUE(user_session$logged_in) && identical(user_session$role, "admin")) {
      bs4SidebarMenuItem("Admin Panel", tabName = "admin", icon = icon("user-shield"))
    } else {
      NULL
    }
  })
  
  # === JS OBSERVER UNTUK MENGGANTIKAN TOMBOL GANTI FOTO ===
  observe({
    shinyjs::onclick("user_panel_container", {
      if (isTRUE(user_session$logged_in)) {
        # Kirim sinyal input baru (input$photo_click_trigger)
        # Nilai Math.random() memastikan input berubah setiap kali diklik
        runjs('Shiny.setInputValue("photo_click_trigger", Math.random());')
      }
    })
  })
  
  # === PANEL PENGGUNA DINAMIS (FOTO) ===
  user_info_reactive <- reactive({
    rv$user_update_trigger
    if (!user_session$logged_in) return(NULL)
    get_user_by_email(user_session$email)
  })
  
  # FUNGSI BARU: Merender Panel Pengguna secara dinamis
  output$dynamic_user_panel <- renderUI({
    user <- user_info_reactive()
    if (!user_session$logged_in || is.null(user)) {
      # Tampilan jika belum login
      return(bs4SidebarUserPanel(
        image = "default_user.jpg",
        name = "Guest User"
      ))
    }
    
    # Tampilan jika sudah login (mengambil path dari DB)
    photo_path <- user$photo_path %||% "default_user.jpg" # Jika null, pakai default
    
    bs4SidebarUserPanel(
      image = photo_path, # <--- Menggunakan path foto dari DB
      name = uiOutput("sidebar_user_name_with_stars") 
    )
  })
  # === END MODIFIKASI FOTO DINAMIS ===
  
  # Stars Count di Sidebar User Panel
  output$sidebar_user_name_with_stars <- renderUI({ 
    user <- user_info_reactive()
    req(user)
    tags$div(
      user_session$email,
      tags$span(class="badge badge-star", icon("star"), user$stars)
    )
  })
  
  # Uploads Used di bawah User Panel
  output$sidebar_user_uploads <- renderUI({
    user <- user_info_reactive()
    req(user)
    paste("Uploads used:", user$uploads_used)
  })
  # ====================================================================================
  
  # --- DATA PENJUALAN ---
  data_sales <- reactive({
    rv$data
  })
  
  # --- INSIGHT OTOMATIS + HISTOGRAM ---
  output$insight_bar <- renderPlotly({
    df <- data_sales(); req(df); req("Merk" %in% names(df))
    df_summary <- df %>% dplyr::count(Merk, name = "Jumlah") %>% arrange(desc(Jumlah))
    plot_ly(
      data = df_summary, x = ~Merk, y = ~Jumlah, type = "bar",
      hoverinfo = "text", text = ~paste("Merk:", Merk, "<br>Jumlah:", Jumlah)
    ) %>%
      layout(xaxis = list(title = "Merk"), yaxis = list(title = "Jumlah Terjual"), margin = list(l = 40, r = 20, b = 40, t = 10))
  })
  
  output$insight_hist <- renderPlotly({
    df <- data_sales(); req(df); req("Harga" %in% names(df))
    plot_ly(
      data = df, x = ~Harga, type = "histogram",
      marker = list(color = "#17a2b8")
    ) %>%
      layout(
        title = "Distribusi Harga",
        xaxis = list(title = "Harga", tickformat = ",.0f"),
        yaxis = list(title = "Frekuensi"),
        margin = list(l = 40, r = 20, b = 40, t = 10)
      )
  })
  
  output$insightBox <- renderUI({
    req(data_sales())
    
    df <- data_sales()
    total_penjualan <- sum(df$Harga, na.rm = TRUE)
    rata2_harga     <- mean(df$Harga, na.rm = TRUE)
    merk_terbanyak  <- names(sort(table(df$Merk), decreasing = TRUE))[1]
    type_termahal   <- if("Type" %in% names(df)) df$Type[which.max(df$Harga)] else "N/A"
    
    tagList(
      tags$h5("Ringkasan Statistik"),
      tags$ul(
        tags$li(paste("Total nilai penjualan:", format_rupiah(total_penjualan))),
        tags$li(paste("Rata-rata harga:", format_rupiah(rata2_harga))),
        tags$li(paste("Merk paling banyak terjual:", merk_terbanyak)),
        tags$li(paste("Type harga tertinggi:", type_termahal)),
        tags$li(paste("Jumlah data (observations):", nrow(df)))
      ),
      tags$h5("Distribusi Penjualan per Merk"),
      plotlyOutput("insight_bar", height = "400px"),
      tags$h5("Distribusi Harga"),
      plotlyOutput("insight_hist", height = "300px")
    )
  })
  
  # -------- Theme Selector (Tidak Berubah) --------
  sel <- reactiveValues(nav = "primary", side = "terbaik", accent = "primary", side_dark = FALSE)
  for(clr in names(color_map)){
    local({
      color_key <- clr
      observeEvent(input[[paste0("navbar_color_",color_key)]], { sel$nav <- color_key }, ignoreInit = TRUE)
      observeEvent(input[[paste0("sidebar_color_",color_key)]], { sel$side <- color_key }, ignoreInit = TRUE)
      observeEvent(input[[paste0("accent_color_",color_key)]], { sel$accent <- color_key }, ignoreInit = TRUE)
    })
  }
  observeEvent(input$sidebar_dark, { sel$side_dark <- isTRUE(input$sidebar_dark) }, ignoreInit = TRUE)
  theme_current <- reactive({
    nav_hex <- color_map[[sel$nav]]; side_hex <- color_map[[sel$side]]; acc_hex <- color_map[[sel$accent]]
    if(isTRUE(sel$side_dark)){
      create_theme(bs4dash_vars(bs4dash_accent = acc_hex, navbar_light_color = nav_hex), bs4dash_sidebar_dark(bg = side_hex, submenu_bg = side_hex, submenu_color = "#fff"), bs4dash_color(blue = acc_hex))
    } else {
      create_theme(bs4dash_vars(bs4dash_accent = acc_hex, navbar_light_color = nav_hex), bs4dash_sidebar_light(bg = side_hex, submenu_bg = side_hex, submenu_color = "#000"), bs4dash_color(blue = acc_hex))
    }
  })
  output$ui_fresh_theme <- renderUI({ use_theme(theme_current()) })
  # ------------------------------------------------
  
  # AUTH UI & LOGIC (Tidak Berubah)
  output$auth_buttons_ui <- renderUI({
    if (!user_session$logged_in) {
      tagList(actionButton("btn_show_login", "Login", class = "btn-primary"), actionButton("btn_show_register", "Register", class = "btn-default", style = "margin-left:8px;"))
    } else {
      tagList(actionButton("btn_logout", "Logout", class = "btn-danger", style = "margin-left:8px;"))
    }
  })
  observeEvent(input$btn_login, {
    req(input$login_email, input$login_password); removeModal()
    waiter_show(html = tagList(tags$h4("Memeriksa kredensial...")), color = "#ffffff"); on.exit(waiter_hide(), add = TRUE)
    user <- verify_credentials(input$login_email, input$login_password)
    if (is.null(user)) { shinyjs::alert("Login gagal: email atau password salah.")
    } else {
      user_session$logged_in <- TRUE; user_session$email <- user$email; user_session$role  <- user$role
      showNotification(paste("Berhasil login:", user$email), type = "message")
      rv$user_update_trigger <- rv$user_update_trigger + 1
    }
  })
  observeEvent(input$btn_logout, {
    user_session$logged_in <- FALSE; user_session$email <- NULL; user_session$role  <- NULL; rv$data <- NULL
    showNotification("Anda telah logout.", type = "message")
  })
  observeEvent(input$btn_show_login, {
    showModal(modalDialog(
      title = "Login",
      textInput("login_email", "Email", value=""),
      passwordInput("login_password", "Password", value=""),
      footer = tagList(modalButton("Batal"), actionButton("btn_login", "Login", class = "btn-primary")),
      easyClose = TRUE
    ))
  })
  observeEvent(input$btn_show_register, {
    showModal(modalDialog(title = "Register", textInput("reg_email", "Email"), passwordInput("reg_password", "Password"), footer = tagList(modalButton("Batal"), actionButton("btn_register", "Daftar", class = "btn-primary")), easyClose = TRUE))
  })
  observeEvent(input$btn_register, {
    req(input$reg_email, input$reg_password); removeModal()
    ok <- create_user(input$reg_email, input$reg_password, initial_stars = FREE_UPLOADS)
    if (isTRUE(ok)) { shinyjs::alert(paste0("Registrasi berhasil. Silakan login. Anda mendapatkan ", FREE_UPLOADS, " Stars gratis."))
    } else { shinyjs::alert("Registrasi gagal — mungkin email sudah terdaftar.") }
  })
  observeEvent(input$btn_demo_reset, {
    req(user_session$logged_in)
    if (user_session$role == "admin" || user_session$email == "demo@local") {
      reset_uploads_used(user_session$email)
      rv$user_update_trigger <- rv$user_update_trigger + 1
      showNotification("Uploads used direset (0).", type = "message")
    } else { showNotification("Aksi tidak diizinkan.", type = "error") }
  })
  
  # --- LOGIKA UPLOAD FOTO (DIPICU OLEH KLIK PADA FOTO) ---
  observeEvent(input$photo_click_trigger, { # <-- DIPICU OLEH KLIK FOTO
    req(user_session$logged_in)
    showModal(modalDialog(
      title = "Unggah Foto Profil Baru",
      fileInput(
        inputId = "profile_photo_upload",
        label = "Pilih File Foto (JPG/PNG)",
        accept = c("image/jpeg", "image/png"),
        width = "100%"
      ),
      footer = tagList(
        modalButton("Batal"),
        actionButton("confirm_upload", "Simpan Foto", class = "btn-success")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_upload, {
    req(user_session$logged_in)
    req(input$profile_photo_upload)
    
    isolate({
      file_info <- input$profile_photo_upload
      ext <- tolower(tools::file_ext(file_info$datapath))
      
      # 1. Pastikan ekstensi valid
      if (!ext %in% c("jpg", "jpeg", "png")) {
        showNotification("Format file harus JPG atau PNG.", type = "error")
        return()
      }
      
      # 2. Tentukan nama file unik dan path
      user_email_sanitized <- gsub("[^a-zA-Z0-9]", "_", user_session$email)
      new_filename <- paste0("profile_", user_email_sanitized, ".", ext)
      target_path_full <- paste0("www/", new_filename)
      
      # 3. Pindahkan file
      if (file.copy(file_info$datapath, target_path_full, overwrite = TRUE)) {
        
        # 4. Update Database
        if (update_user_photo_path(user_session$email, new_filename)) {
          showNotification("Foto profil berhasil diperbarui!", type = "message")
          removeModal()
          
          # 5. Trigger update UI
          rv$user_update_trigger <- rv$user_update_trigger + 1
        } else {
          showNotification("Gagal menyimpan path foto ke database.", type = "error")
        }
      } else {
        showNotification("Gagal memindahkan file foto ke server.", type = "error")
      }
    })
  })
  # --- END LOGIKA UPLOAD FOTO ---
  
  # Status Card & Summary UI (Tidak Berubah)
  status_card_ui_func <- reactive({
    rv$user_update_trigger
    if (!user_session$logged_in) return(tagList(tags$h4("Belum login"), tags$p("Silakan login atau daftar terlebih dahulu.")))
    user <- get_user_by_email(user_session$email); if (is.null(user)) return(tags$p("User tidak ditemukan."))
    tagList(
      tags$h4(user$email),
      tags$p(class = "muted", paste("Role:", user$role)),
      tags$div(class = "status-box",
               tags$h3("Stars: ", tags$span(style = "color:#d97706; font-weight:700;", user$stars)),
               tags$p("Uploads used: ", user$uploads_used),
               if (user$role == "admin") tags$p(tags$em("Anda login sebagai admin."))
      )
    )
  })
  summary_ui_func <- reactive({
    rv$user_update_trigger
    if (!user_session$logged_in) return(tags$p("Login untuk melihat ringkasan akun."))
    user <- get_user_by_email(user_session$email); if (is.null(user)) return(tags$p("User tidak ditemukan"))
    tagList(
      tags$h4("Ringkasan Cepat"),
      tags$p(strong("Email:"), user$email),
      tags$p(strong("Role:"), user$role),
      tags$p(strong("Stars:"), tags$span(style = "color:#d97706; font-weight:700;", user$stars)),
      tags$p(strong("Uploads used:"), user$uploads_used)
    )
  })
  output$status_card_ui <- renderUI({ status_card_ui_func() })
  output$summary_ui <- renderUI({ summary_ui_func() })
  
  # UPLOAD UI (Tidak Berubah)
  output$upload_ui <- renderUI({
    rv$user_update_trigger
    if (!user_session$logged_in) return(tags$p("Silakan login untuk mengakses fitur upload."))
    user <- get_user_by_email(user_session$email); if (is.null(user)) return(tags$p("User tidak ditemukan."))
    disable_upload <- FALSE
    cost_text <- ""
    if (user$role != "admin") {
      if (user$uploads_used < FREE_UPLOADS) {
        cost_text <- paste0("(GRATIS - Upload ke-", user$uploads_used + 1, " dari ", FREE_UPLOADS, ")")
      } else if (user$stars >= 1) {
        cost_text <- "(1 STAR akan dikonsumsi)"
      } else {
        disable_upload <- TRUE
      }
    }
    if (disable_upload) {
      tagList(
        tags$h4("Akses Upload Terkunci"),
        tags$p(paste0("Anda sudah menggunakan ", FREE_UPLOADS, " upload gratis dan saldo bintang Anda habis.")),
        tags$p("Silakan beli paket premium untuk melanjutkan.")
      )
    }
    else {
      tagList(
        fileInput("file_input_premium", "Pilih file (CSV / XLSX)", accept = c(".csv", ".xlsx", ".xls")),
        tags$p(cost_text, style="font-style:italic;"),
        actionButton("btn_do_upload", "Upload & Proses", class = "btn-primary", icon = icon("upload")),
        div(class = "progress-bar-container", id = "progress_container",
            div(class = "progress-bar", id = "progress_bar", "0%")
        ),
        div(class = "upload-status", id = "upload_status", "Status: Menunggu file."),
        tags$hr()
      )
    }
  })
  
  # handle upload action (Tidak Berubah)
  observeEvent(input$btn_do_upload, {
    req(user_session$logged_in); req(input$file_input_premium)
    rv$upload_progress <- 0
    update_progress <- function(detail, value) {
      session$sendCustomMessage("progressUpdate", list(value=value*100, detail=detail))
    }
    update_progress("Mengecek saldo dan izin...", 0.1)
    
    isolate({
      user <- get_user_by_email(user_session$email)
      if (is.null(user)) { showNotification("User tidak ditemukan.", type = "error"); return() }
      
      cost <- 0
      if (user$role != "admin") {
        if (user$uploads_used >= FREE_UPLOADS) cost <- 1
        if (cost > 0 && user$stars < cost) {
          showNotification("Saldo bintang tidak cukup. Hubungi admin.", type = "error"); return()
        }
      }
      
      update_progress("Memproses transaksi...", 0.2)
      if (cost == 1) {
        ok <- update_user_stars(user$email, -cost)
        if (!ok) { showNotification("Gagal mengurangi bintang.", type = "error"); return() }
      }
      increment_upload_used(user$email)
      showNotification(paste0("Upload diterima. ", ifelse(cost==0, "Gratis", paste0(cost, " bintang")) ," dikonsumsi."), type = "message")
      rv$user_update_trigger <- rv$user_update_trigger + 1
      
      update_progress("Membaca file...", 0.4)
      fp <- input$file_input_premium$datapath; nm <- input$file_input_premium$name; ext <- tools::file_ext(nm); df <- NULL
      tryCatch({
        if (tolower(ext) == "csv") df <- read_csv(fp, show_col_types = FALSE)
        else if (tolower(ext) %in% c("xlsx", "xls")) df <- read_excel(fp)
        else stop("Format tidak didukung")
      }, error = function(e) { showNotification(paste("Gagal membaca file:", e$message), type = "error") })
      if (is.null(df)) { rv$data <- NULL; return() }
      
      update_progress("Memvalidasi kolom...", 0.6)
      col_merk <- detect_col(df, c("merk","brand","merek")); col_type <- detect_col(df, c("type","tipe")); col_harga <- detect_col(df, c("harga","price","total","amount","nominal","jumlah"))
      df <- safe_rename(df, col_merk, "Merk"); df <- safe_rename(df, col_type, "Type"); df <- safe_rename(df, col_harga, "Harga")
      kolom_wajib_ada <- c("Merk", "Harga")
      if (!all(kolom_wajib_ada %in% names(df))) {
        missing_cols <- setdiff(kolom_wajib_ada, names(df)); rv$data <- NULL
        showNotification(paste("Gagal: Kolom wajib tidak ditemukan:", paste(missing_cols, collapse=", ")), type = "error", duration = 8); return()
      }
      
      update_progress("Membersihkan data...", 0.8)
      df$Harga <- suppressWarnings(as.numeric(df$Harga))
      df$Type <- df$Type %||% "N/A"
      df$Merk <- as.factor(df$Merk)
      df <- df %>% filter(!is.na(Harga))
      
      update_progress("Menyimpan data...", 0.9)
      rv$data <- df
      
      data_id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", user_session$email)
      rv$data_history[[data_id]] <- list(
        data = df,
        filename = input$file_input_premium$name,
        timestamp = Sys.time()
      )
      update_progress("Selesai!", 1.0)
      showNotification("Data berhasil di-upload dan diproses!", type = "message")
    })
  })
  
  # Logic untuk Progress Bar (Tidak Berubah)
  observe({
    shinyjs::runjs('Shiny.addCustomMessageHandler("progressUpdate", function(data) {
        $("#progress_bar").css("width", data.value + "%").text(data.detail);
        $("#upload_status").text("Status: " + data.detail);
        if (data.value === 100) { setTimeout(function() { $("#progress_bar").css("width", "0%").text("0%"); $("#upload_status").text("Status: Menunggu file."); }, 2000); }
    });')
  })
  
  # Modal Bantuan Dashboard (Tidak Berubah)
  observeEvent(input$btn_show_help, {
    showModal(modalDialog(
      title = tags$div(icon("info-circle"), " Bantuan & Format Data"),
      size = "l",
      tags$p("Untuk analisis yang optimal, data yang Anda unggah harus memiliki kolom berikut (case-insensitive):"),
      tags$ul(
        tags$li(
          tags$span(strong("Merk"), " atau ", strong("Brand"), " atau ", strong("Merek"), " (Wajib, Kategorikal)")
        ),
        tags$li(
          tags$span(strong("Harga"), " atau ", strong("Price"), " atau ", strong("Nominal"), " (Wajib, Numerik)")
        ),
        tags$li(
          tags$span(strong("Type"), " atau ", strong("Tipe"), " (Opsional, Kategorikal)")
        )
      ),
      tags$hr(),
      tags$h5("Tips: Saldo Bintang"),
      tags$p(paste0("Anda mendapatkan ", strong(FREE_UPLOADS), " upload gratis (tidak mengurangi bintang). Setelah habis, setiap upload premium akan mengkonsumsi ", strong("1 Bintang"), ".")),
      tags$p("Hubungi admin melalui tautan WhatsApp di tim profile untuk membeli Stars."),
      easyClose = TRUE
    ))
  })
  
  # Download Handler (Home tab) (Tidak Berubah)
  output$download_data_premium <- downloadHandler(
    filename = function() paste0("data_processed_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$data)
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
  
  # Reactive Data yang digunakan oleh semua Plot/Tabel (Analisis tab) (Tidak Berubah)
  dataAnalisis <- reactive({
    rv$data
  })
  
  observeEvent(input$refresh_data, {
    showNotification("Data di-refresh dari data terakhir yang dimuat/di-*upload*.", type="message")
    df <- rv$data 
  })
  
  observe({
    df <- dataAnalisis(); req(df)
    choices <- if("Merk" %in% names(df)) sort(unique(na.omit(df$Merk))) else character(0)
    updateSelectInput(session, "pilihMerk", choices = choices, selected = if(length(choices)) choices[1] else NULL)
    
    cols <- names(df)
    kategori_cols <- cols[sapply(df, function(x) is.character(x) || is.factor(x))]
    numerik_cols <- cols[sapply(df, is.numeric)]
    updateSelectInput(session, "kustom_x_axis", choices = kategori_cols, selected = if("Merk" %in% kategori_cols) "Merk" else kategori_cols[1])
    updateSelectInput(session, "kustom_y_axis", choices = numerik_cols, selected = if("Harga" %in% numerik_cols) "Harga" else numerik_cols[1])
  })
  
  # --- PLOT KUSTOM LOGIC (Tidak Berubah) ---
  output$kustom_kategori_select <- renderUI({
    df <- dataAnalisis(); req(df); req(input$kustom_x_axis)
    choices <- sort(unique(df[[input$kustom_x_axis]]))
    selectInput("kustom_kategori_filter", paste("Filter", input$kustom_x_axis), choices = c("Semua", choices), selected = "Semua")
  })
  
  output$kustomPlotOutput <- renderPlotly({
    df <- dataAnalisis(); req(df); req(input$kustom_x_axis); req(input$kustom_y_axis); req(input$kustom_plot_type)
    
    x_col <- input$kustom_x_axis
    y_col <- input$kustom_y_axis
    plot_type <- input$kustom_plot_type
    
    filter_value <- input$kustom_kategori_filter
    if (!is.null(filter_value) && filter_value != "Semua") {
      df <- df %>% filter(!!sym(x_col) == filter_value)
    }
    
    if (plot_type == "bar") {
      df_plot <- df %>% 
        group_by(!!sym(x_col)) %>% 
        summarise(RataRata = mean(!!sym(y_col), na.rm = TRUE), .groups = "drop")
      
      p <- plot_ly(df_plot, 
                   x = ~df_plot[[x_col]], 
                   y = ~RataRata, 
                   type = 'bar') %>%
        layout(title = paste("Rata-rata", y_col, "per", x_col), 
               yaxis = list(title = paste("Rata-rata", y_col), tickformat = ",.0f"), 
               xaxis = list(title = x_col))
      
    } else if (plot_type == "box") {
      p <- plot_ly(df, 
                   x = ~df[[x_col]], 
                   y = ~df[[y_col]], 
                   type = 'box') %>%
        layout(title = paste("Distribusi", y_col, "per", x_col), 
               yaxis = list(title = y_col, tickformat = ",.0f"), 
               xaxis = list(title = x_col), 
               showlegend=FALSE)
      
    } else if (plot_type == "hist") {
      p <- plot_ly(df, 
                   x = ~df[[y_col]], 
                   color = ~df[[x_col]], 
                   type = 'histogram') %>%
        layout(title = paste("Histogram", y_col, "dibagi oleh", x_col), 
               xaxis = list(title = y_col, tickformat = ",.0f"), 
               yaxis = list(title = "Frekuensi"))
      
    } else {
      p <- plot_ly()
    }
    
    p %>% config(displayModeBar="hover", displaylogo=FALSE)
  })
  
  # --- HISTORI DATA LOGIC (Tidak Berubah) ---
  output$history_ui <- renderUI({
    req(user_session$logged_in)
    history_list <- rv$data_history
    if (length(history_list) == 0) {
      return(tags$p("Belum ada data yang di-*upload* di sesi ini."))
    }
    choices <- rev(names(history_list))
    names(choices) <- rev(sapply(history_list, function(h) paste0(h$filename, " (", format(h$timestamp, "%Y-%m-%d %H:%M:%S"), ")")))
    
    tagList(
      selectInput("history_select_data", "Pilih Dataset untuk Preview/Load:", choices = choices, width = "100%"),
      actionButton("history_load_btn", "Load Data ke Analisis Utama", class = "btn-warning", icon = icon("sync")),
      actionButton("history_delete_btn", "Hapus Data dari Histori", class = "btn-danger", icon = icon("trash-alt"), style="margin-left:10px;")
    )
  })
  
  observeEvent(input$history_load_btn, {
    req(input$history_select_data)
    if (input$history_select_data %in% names(rv$data_history)) {
      rv$data <- rv$data_history[[input$history_select_data]]$data
      showNotification(paste("Dataset", rv$data_history[[input$history_select_data]]$filename, "berhasil dimuat ke Analisis Utama."), type="message")
    }
  })
  
  observeEvent(input$history_delete_btn, {
    req(input$history_select_data)
    showModal(modalDialog(
      title = "Konfirmasi Hapus Histori",
      paste("Apakah Anda yakin ingin menghapus data:", rv$data_history[[input$history_select_data]]$filename, "?"),
      footer = tagList(
        modalButton("Batal"),
        actionButton("confirm_delete_history", "Hapus", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_history, {
    removeModal()
    req(input$history_select_data)
    rv$data_history[[input$history_select_data]] <- NULL
    showNotification("Data histori berhasil dihapus.", type="warning")
  })
  
  output$history_data_preview <- renderDT({
    req(user_session$logged_in); req(input$history_select_data)
    df <- rv$data_history[[input$history_select_data]]$data
    datatable(df, options=list(pageLength=10, scrollX=TRUE), style='bootstrap4', class=dt_class(), rownames=FALSE)
  })
  # --- END HISTORI DATA LOGIC ---
  
  dt_class <- reactive({ if(isTRUE(sel$side_dark)) "cell-border stripe table-dark" else "stripe hover compact" })
  
  # -------- Value Boxes (Tidak Berubah) --------
  output$vbox_total_records <- renderValueBox({
    df <- dataAnalisis()
    val <- if(!is.null(df)) nrow(df) else 0
    valueBox(value=format(val, big.mark=".", decimal.mark=","), subtitle="Total Records", icon=icon("database"), color="primary")
  })
  
  output$vbox_total_merk <- renderValueBox({
    df <- dataAnalisis()
    val <- if(!is.null(df) && "Merk" %in% names(df)) length(unique(df$Merk)) else 0
    valueBox(value=val, subtitle="Jumlah Merk", icon=icon("tags"), color="info")
  })
  
  output$vbox_total_type <- renderValueBox({
    df <- dataAnalisis()
    val <- if(!is.null(df) && "Type" %in% names(df)) length(unique(df$Type)) else 0
    valueBox(value=val, subtitle="Jumlah Type", icon=icon("list"), color="success")
  })
  
  output$vbox_total_sales <- renderValueBox({
    df <- dataAnalisis()
    val <- if(!is.null(df) && "Harga" %in% names(df)) sum(df$Harga, na.rm=TRUE) else 0
    valueBox(value=format_rupiah(val), subtitle="Total Nilai Penjualan", icon=icon("hand-holding-usd"), color="danger")
  })
  
  output$vbox_avg_harga <- renderValueBox({
    df <- dataAnalisis()
    val <- if(!is.null(df) && "Harga" %in% names(df)) mean(df$Harga, na.rm=TRUE) else 0
    valueBox(value=format_rupiah(val), subtitle="Rata-rata Harga", icon=icon("money-bill-wave"), color="warning")
  })
  
  # -------- Tabel & Plot (Tidak Berubah) --------
  output$tabelData <- renderDT({
    df <- dataAnalisis()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Msg = "Login & Upload data untuk ditampilkan"), options = list(dom = 't')))
    }
    datatable(df, options=list(pageLength=10, scrollX=TRUE), style='bootstrap4', class=dt_class(), filter='top', rownames=FALSE)
  })
  
  output$tabelMerk <- renderDT({
    df <- dataAnalisis(); req(df)
    if(!"Merk" %in% names(df)) return(datatable(data.frame(Info="Kolom 'Merk' tidak ditemukan"), options=list(dom='t')))
    hasil <- df %>% group_by(Merk) %>% summarise(total_unit=n(), rata_rata_harga=mean(Harga, na.rm=TRUE), .groups="drop") %>% arrange(desc(total_unit))
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil, options=list(pageLength=10, scrollX=TRUE), class=dt_class(), rownames=FALSE)
  })
  
  output$tabelType <- renderDT({
    df <- dataAnalisis(); req(df)
    merk_sel <- input$pilihMerk; req(merk_sel)
    df <- df %>% filter(Merk==merk_sel)
    if(!"Type" %in% names(df)) return(datatable(data.frame(Info="Kolom 'Type' tidak ditemukan"), options=list(dom='t')))
    hasil <- df %>% group_by(Type) %>% summarise(total_unit=n(), rata_rata_harga=mean(Harga, na.rm=TRUE), .groups="drop") %>% arrange(desc(total_unit))
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil, options=list(pageLength=10, scrollX=TRUE), class=dt_class(), rownames=FALSE)
  })
  
  output$tabelOutliers <- renderDT({
    df <- dataAnalisis(); req(df); req("Harga" %in% names(df))
    outlier_prices <- get_outliers(df$Harga)
    if(length(outlier_prices) == 0) return(datatable(data.frame(Info="Tidak ditemukan data Outlier Harga"), options=list(dom='t')))
    
    df_outlier <- df %>%
      filter(Harga %in% outlier_prices) %>%
      select(Merk, Type, Harga) %>%
      mutate(Harga = format_rupiah(Harga))
    
    datatable(df_outlier, options=list(pageLength=5, scrollX=TRUE), class=dt_class(), rownames=FALSE)
  })
  
  
  # --- Plotly Generator Function (Tidak Berubah) ---
  renderPlotlyGrafik <- function(data_plot, x_col, y_col, graf_type, title) {
    n_cat <- nrow(data_plot)
    warna_types <- RColorBrewer::brewer.pal(max(3, min(8, n_cat)), "Set2")
    warna_types <- rep(warna_types, length.out=n_cat)
    
    if(graf_type=="bar"){
      p <- plot_ly(data_plot, x=as.formula(paste0("~", x_col)), y=as.formula(paste0("~", y_col)),
                   type='bar', marker=list(color=warna_types))
    } else if(graf_type=="line"){
      p <- plot_ly(data_plot, x=as.formula(paste0("~", x_col)), y=as.formula(paste0("~", y_col)),
                   type='scatter', mode='lines+markers', marker=list(color="#007bff"),
                   line=list(color="#007bff"))
    } else if(graf_type=="pie"){
      p <- plot_ly(data_plot, labels=as.formula(paste0("~", x_col)), values=as.formula(paste0("~", y_col)),
                   type='pie', marker=list(colors=warna_types))
    } else {
      p <- plot_ly()
    }
    p %>% layout(title=title, margin=list(b=80)) %>% config(displayModeBar="hover", displaylogo=FALSE)
  }
  
  output$grafikMerkInteractive <- renderPlotly({
    df <- dataAnalisis(); req(df)
    data_plot <- df %>% group_by(Merk) %>% summarise(total_unit=n(), .groups="drop") %>% arrange(desc(total_unit))
    renderPlotlyGrafik(data_plot, "Merk", "total_unit", input$pilih_grafik_merk, "Total Unit per Merk")
  })
  
  output$grafikTypeInteractive <- renderPlotly({
    df <- dataAnalisis(); req(df)
    merk_sel <- input$pilihMerk; req(merk_sel)
    df <- df %>% filter(Merk==merk_sel)
    if(!"Type" %in% names(df)) return(NULL)
    data_plot <- df %>% group_by(Type) %>% summarise(total_unit=n(), .groups="drop") %>% arrange(desc(total_unit))
    renderPlotlyGrafik(data_plot, "Type", "total_unit", input$pilih_grafik_type, paste("Total Unit per Type -", merk_sel))
  })
  
  output$boxPlotHarga <- renderPlotly({
    df <- dataAnalisis(); req(df); req(all(c("Merk", "Harga") %in% names(df)))
    df$Merk <- factor(df$Merk)
    plot_ly(
      data = df, y = ~Harga, color = ~Merk, type = "box", boxpoints = "outliers", name = ~Merk
    ) %>%
      layout(
        title = "Distribusi Harga Jual per Merk",
        yaxis = list(title = "Harga (Rupiah)", tickformat = ",.0f"),
        xaxis = list(title = "Merk"),
        showlegend = FALSE, margin = list(b = 100)
      ) %>%
      config(displayModeBar = "hover", displaylogo = FALSE)
  })
  
  output$comparisonMerk <- renderPlotly({
    df <- dataAnalisis(); req(df)
    data_plot <- df %>% group_by(Merk) %>% summarise(total_unit=n(), .groups="drop") %>% arrange(desc(total_unit)) %>% head(5)
    renderPlotlyGrafik(data_plot, "Merk", "total_unit", input$pilih_grafik_top5, "Top 5 Merk Terlaris")
  })
  
  output$comparisonType <- renderPlotly({
    df <- dataAnalisis(); req(df)
    merk_sel <- input$pilihMerk; req(merk_sel)
    data_plot <- df %>% filter(Merk==merk_sel) %>% group_by(Type) %>% summarise(total_unit=n(), .groups="drop") %>% arrange(desc(total_unit)) %>% head(5)
    renderPlotlyGrafik(data_plot, "Type", "total_unit", input$pilih_grafik_top5, paste("Top 5 Type -", merk_sel))
  })
  
  # -----------------------
  # ADMIN PANEL (Tidak Berubah)
  # -----------------------
  output$admin_users_dt <- renderDT({ datatable(data.frame(Msg = "Hanya admin dapat melihat tabel ini"), options = list(dom = 't')) })
  
  observe({
    rv$user_update_trigger
    if (user_session$logged_in && identical(user_session$role, "admin")) {
      output$admin_users_dt <- renderDT({ datatable(fetch_all_users(), selection = "single", options = list(pageLength = 10, scrollX = TRUE)) })
    }
  })
  
  observeEvent(input$admin_search_btn, {
    req(user_session$logged_in); if (!identical(user_session$role, "admin")) { showNotification("Hanya admin dapat melakukan aksi ini.", type = "error"); return() }
    q <- trimws(input$admin_search_email %||% "")
    if (q == "") { showNotification("Masukkan email untuk mencari.", type = "warning"); return() }
    user <- get_user_by_email(q)
    if (is.null(user)) {
      showNotification("User tidak ditemukan.", type = "error"); output$admin_selected_ui <- renderUI({ tags$p("User tidak ditemukan") }); rv$admin_selected_email <- NULL
    } else {
      rv$admin_selected_email <- user$email
      output$admin_selected_ui <- renderUI({
        current_user <- get_user_by_email(user$email)
        tagList(tags$h5("Selected user"), tags$p(strong("Email: "), current_user$email), tags$p(strong("Role: "), current_user$role), tags$p(strong("Stars: "), tags$span(style = "color:#d97706; font-weight:700;", current_user$stars)), tags$p(strong("Uploads used: "), current_user$uploads_used))
      })
      showNotification(paste("User ditemukan:", user$email), type = "message")
    }
  })
  
  observeEvent(input$admin_users_dt_rows_selected, {
    sel_row <- input$admin_users_dt_rows_selected; if (length(sel_row) == 0) return()
    df <- fetch_all_users(); row <- df[sel_row[1], , drop = FALSE]; rv$admin_selected_email <- row$email
    output$admin_selected_ui <- renderUI({
      tagList(tags$h5("Selected user"), tags$p(strong("Email: "), row$email), tags$p(strong("Role: "), row$role), tags$p(strong("Stars: "), tags$span(style = "color:#d97706; font-weight:700;", row$stars)), tags$p(strong("Uploads used: "), row$uploads_used))
    })
  })
  
  observeEvent(input$admin_apply_btn, {
    req(user_session$logged_in); if (!identical(user_session$role, "admin")) { showNotification("Hanya admin.", type = "error"); return() }
    target <- rv$admin_selected_email %||% trimws(input$admin_search_email %||% ""); if (is.null(target) || target == "") { showNotification("Pilih atau cari user terlebih dahulu.", type = "warning"); return() }
    delta <- as.integer(input$admin_star_delta); if (is.na(delta) || delta == 0) { showNotification("Masukkan jumlah bintang (bukan 0).", type = "warning"); return() }
    ok <- update_user_stars(target, delta)
    if (ok) {
      logmsg <- paste0(Sys.time(), " | Admin ", user_session$email, " applied ", delta, " stars to ", target)
      output$admin_log <- renderText({ paste0(logmsg, "\n\nOperation successful.") })
      showNotification("Perubahan bintang berhasil.", type = "message")
      rv$user_update_trigger <- rv$user_update_trigger + 1
      current_user <- get_user_by_email(target)
      output$admin_selected_ui <- renderUI({
        req(current_user)
        tagList(tags$h5("Selected user"), tags$p(strong("Email: "), current_user$email), tags$p(strong("Role: "), current_user$role), tags$p(strong("Stars: "), tags$span(style = "color:#d97706; font-weight:700;", current_user$stars)), tags$p(strong("Uploads used: "), current_user$uploads_used))
      })
    } else {
      showNotification("Gagal mengubah bintang (mungkin akan menjadi negatif).", type = "error")
    }
  })
  
  observeEvent(input$admin_delete_btn, {
    req(user_session$logged_in); if (!identical(user_session$role, "admin")) { showNotification("Hanya admin dapat melakukan aksi ini.", type = "error"); return() }
    target <- rv$admin_selected_email %||% trimws(input$admin_search_email %||% ""); if (is.null(target) || target == "") { showNotification("Pilih atau cari user terlebih dahulu.", type = "warning"); return() }
    showModal(modalDialog(title = "Konfirmasi Penghapusan", paste("Apakah Anda yakin ingin menghapus user:", target, "? Aksi ini tidak dapat dibatalkan."), footer = tagList(modalButton("Batal"), actionButton("confirm_delete_user", "Hapus Sekarang", class = "btn-danger"))))
  })
  
  observeEvent(input$confirm_delete_user, {
    removeModal(); target <- rv$admin_selected_email %||% trimws(input$admin_search_email %||% "")
    if (is.null(target) || target == "") { showNotification("Target tidak valid.", type = "error"); return() }
    result <- delete_user_by_email(target)
    if (result == "Success") {
      logmsg <- paste0(Sys.time(), " | Admin ", user_session$email, " deleted user ", target)
      output$admin_log <- renderText({ paste0(logmsg, "\n\nOperation successful.") })
      showNotification(paste("User", target, "berhasil dihapus."), type = "message")
      rv$admin_selected_email <- NULL; output$admin_selected_ui <- renderUI({ tags$p("User berhasil dihapus.") })
      rv$user_update_trigger <- rv$user_update_trigger + 1
    } else if (result == "Cannot delete admin account") {
      showNotification("Gagal: Admin tidak dapat menghapus sesama akun admin.", type = "error")
    } else {
      showNotification(paste("Gagal menghapus user:", result), type = "error")
    }
  })
}

# -----------------------
# RUN APP
# -----------------------
shinyApp(ui, server)