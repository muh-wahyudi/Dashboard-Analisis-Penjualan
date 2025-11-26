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
library(rlang) # PENTING: Menambahkan rlang untuk operator %||%

# -----------------------
# CONFIG
# -----------------------
DB_FILE <- "users.sqlite" 
FREE_UPLOADS <- 3 
APP_TITLE <- "Dashboard Penjualan - Premium Berfitur"

# -----------------------
# DATABASE INIT & HELPERS
# -----------------------
init_db <- function(db_file = DB_FILE) {
  tryCatch({
    con <- dbConnect(SQLite(), db_file)
    on.exit(dbDisconnect(con), add = TRUE)
    if (!"users" %in% dbListTables(con)) {
      dbExecute(con, "
      CREATE TABLE users (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        email TEXT UNIQUE NOT NULL,
        password_hash TEXT NOT NULL,
        role TEXT NOT NULL DEFAULT 'user',
        stars INTEGER NOT NULL DEFAULT 3,
        uploads_used INTEGER NOT NULL DEFAULT 0,
        created_at TEXT DEFAULT CURRENT_TIMESTAMP
      );
    ")
      admin_pw <- password_store("admin123")
      demo_pw  <- password_store("user123")
      dbExecute(con, "INSERT INTO users (email, password_hash, role, stars, uploads_used) VALUES (?, ?, 'admin', 9999, 0)",
                params = list("admin@local", admin_pw))
      dbExecute(con, "INSERT INTO users (email, password_hash, role, stars, uploads_used) VALUES (?, ?, 'user', 3, 0)",
                params = list("demo@local", demo_pw))
    }
    TRUE
  }, error = function(e) {
    message("Error initializing DB: ", e$message)
    FALSE
  })
}
init_db()

db_connect <- function() DBI::dbConnect(SQLite(), DB_FILE)

get_user_by_email <- function(email) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  res <- dbGetQuery(con, "SELECT * FROM users WHERE email = ?", params = list(email))
  if (nrow(res) == 0) return(NULL)
  res[1, , drop = FALSE]
}

verify_credentials <- function(email, password) {
  rec <- get_user_by_email(email)
  if (is.null(rec)) return(NULL)
  ok <- tryCatch(password_verify(rec$password_hash, password), error = function(e) FALSE)
  if (isTRUE(ok)) {
    rec$password_hash <- NULL
    rec
  } else NULL
}

create_user <- function(email, password, initial_stars = 3, role = "user") {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  pw_hash <- password_store(password)
  tryCatch({
    dbExecute(con, "INSERT INTO users (email, password_hash, role, stars, uploads_used) VALUES (?, ?, ?, ?, 0)",
              params = list(email, pw_hash, role, as.integer(initial_stars)))
    TRUE
  }, error = function(e) FALSE)
}

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
  dbExecute(con, "COMMIT")
  TRUE
}

increment_upload_used <- function(email) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "UPDATE users SET uploads_used = uploads_used + 1 WHERE email = ?", params = list(email))
  TRUE
}

reset_uploads_used <- function(email) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  dbExecute(con, "UPDATE users SET uploads_used = 0 WHERE email = ?", params = list(email))
  TRUE
}

delete_user_by_email <- function(email) {
  con <- db_connect(); on.exit(dbDisconnect(con), add = TRUE)
  
  # 1. Cek peran user yang akan dihapus
  user_to_delete <- get_user_by_email(email)
  if (is.null(user_to_delete)) return("User not found")
  
  # 2. Safety Check: Mencegah penghapusan akun admin
  if (identical(user_to_delete$role, "admin")) {
    return("Cannot delete admin account")
  }
  
  # 3. Eksekusi penghapusan
  tryCatch({
    dbExecute(con, "DELETE FROM users WHERE email = ?", params = list(email))
    return("Success")
  }, error = function(e) {
    message("DB Error deleting user: ", e$message)
    return("DB Error")
  })
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

# -----------------------
# Helper: loading HTML (Waiter)
# -----------------------
loading_html <- function() {
  if (file.exists("www/loading.gif")) {
    tagList(
      tags$div(style = "display:flex; flex-direction:column; align-items:center; justify-content:center;",
               tags$img(src = "loading.gif", height = "100px", alt = "loading"),
               tags$h4("Memuat aplikasi...")
      )
    )
  } else {
    tagList(
      tags$style(HTML("
        .cw-spinner { width:80px; height:80px; border:8px solid #f3f3f3; border-top:8px solid #3498db; border-radius:50%; animation: cw-spin 1s linear infinite; margin-bottom:12px; }
        @keyframes cw-spin { to { transform: rotate(360deg); } }
        .cw-loader-wrap { display:flex; flex-direction:column; align-items:center; justify-content:center; }
      ")),
      tags$div(class = "cw-loader-wrap",
               tags$div(class = "cw-spinner"),
               tags$h4("Memuat aplikasi...")
      )
    )
  }
}

# -------------------------------
# Color palette (Theme Customizer)
# -------------------------------
color_map <- list(
  primary = "#ffc107", secondary = "#6c757d", info = "#17a2b8", success = "#28a745",
  warning = "#f8f9fa", danger = "#dc3545", dark = "#343a40", light = "#007bff",
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
    bs4SidebarMenu(
      bs4SidebarHeader("MENU UTAMA"),
      bs4SidebarMenuItem("Account/Upload", tabName = "home", icon = icon("user")),
      bs4SidebarMenuItem("Analisis Penjualan", tabName = "analisis", icon = icon("chart-line")),
      bs4SidebarMenuItem("Profile team", tabName = "team", icon = icon("users")),
      uiOutput("admin_panel_menu_ui") # Diubah menjadi uiOutput
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
      tags$style(HTML("
        .top-actions { text-align: right; margin-bottom: 10px; }
        .status-box h3 { margin: 0; font-size: 1.1rem; }
        .muted { color:#6b7280; font-size:13px; }
        .admin-input { margin-bottom: 8px; }
        .btn-download { margin-right: 8px; }
      "))
    ),
    
    bs4TabItems(
      
      # ================= TAB HOME (ACCOUNT/UPLOAD) =================
      bs4TabItem(
        tabName = "home",
        fluidRow(
          bs4Card(width = 12, title = "Status Account", status = "info", solidHeader = TRUE,
                  uiOutput("status_card_ui")
          )
        ),
        fluidRow(
          bs4Card(width = 6, title = "Upload Data Premium", status = "primary", solidHeader = TRUE,
                  uiOutput("upload_ui")
          ),
          bs4Card(width = 6, title = "Account Summary", status = "success", solidHeader = TRUE,
                  uiOutput("summary_ui")
          )
        )
      ),
      
      # ================= TAB ANALISIS =================
      bs4TabItem(
        tabName = "analisis",
        fluidRow(
          valueBoxOutput("vbox_total_records", width = 3),
          valueBoxOutput("vbox_total_merk", width = 3),
          valueBoxOutput("vbox_total_type", width = 3),
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
                     downloadButton("download_merk_table", "Download Data", class="btn-success btn-sm")
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
                     downloadButton("download_type_table", "Download Data", class="btn-warning btn-sm")),
              column(6, DTOutput("tabelType"))
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
      
      # ================= TAB ADMIN =================
      bs4TabItem(
        tabName = "admin",
        fluidRow(
          bs4Card(width = 12, title = "Admin Panel - Manage Stars/Users", status = "danger", solidHeader = TRUE,
                  fluidRow(
                    column(width = 4,
                           textInput("admin_search_email", "Cari user (email)"),
                           actionButton("admin_search_btn", "Cari", class = "btn-primary", style = "width:100%; margin-top:6px;")
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
                           actionButton("admin_apply_btn", "Apply", class = "btn-success", style = "width:100%; margin-top:8px;"),
                           actionButton("admin_delete_btn", "Hapus User", class = "btn-danger", style = "width:100%; margin-top:8px;")
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
                         img(src="foto1.jpg", width="90%", style="margin-bottom:15px;"),
                         tags$h4("FIRMAN SYAH"), tags$p("Peran: Data Cleaning")
                     )),
              column(4,
                     div(style="text-align:center; padding:20px;",
                         img(src="foto2.jpg", width="90%", style="margin-bottom:15px;"),
                         tags$h4("MUH.WAHYUDI"), tags$p("Peran: Data Visualization")
                     )),
              column(4,
                     div(style="text-align:center; padding:20px;",
                         img(src="foto3.jpg", width="90%", style="margin-bottom:15px;"),
                         tags$h4("LUTFI ZHAFRAN"), tags$p("Peran: App Developer")
                     ))
            )
          )
        )
      )
    )
  ),
  
  footer = bs4DashFooter(
    left = "Dashboard Penjualan Motor Second",
    right = tagList(icon("code"), "Kelompok 2")
  )
)

# -----------------------
# SERVER
# -----------------------
server <- function(input, output, session) {
  
  Sys.sleep(3)  
  waiter_hide()  
  
  user_session <- reactiveValues(logged_in = FALSE, email = NULL, role = NULL)
  rv <- reactiveValues(data = NULL, admin_selected_email = NULL)
  
  # Logika untuk menampilkan menu Admin Panel (BARU)
  output$admin_panel_menu_ui <- renderUI({
    if (isTRUE(user_session$logged_in) && identical(user_session$role, "admin")) {
      bs4SidebarMenuItem("Admin Panel", tabName = "admin", icon = icon("user-shield"))
    } else {
      NULL
    }
  })
  
  # -------- Theme Selector --------
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
    nav_hex <- color_map[[sel$nav]]
    side_hex <- color_map[[sel$side]]
    acc_hex <- color_map[[sel$accent]]
    if(isTRUE(sel$side_dark)){
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
  
  # AUTH UI (top-right)
  output$auth_buttons_ui <- renderUI({
    if (!user_session$logged_in) {
      tagList(
        actionButton("btn_show_login", "Login", class = "btn-primary"),
        actionButton("btn_show_register", "Register", class = "btn-default", style = "margin-left:8px;")
      )
    } else {
      tagList(
        span(icon("user"), tags$b(" ", user_session$email)),
        actionButton("btn_logout", "Logout", class = "btn-danger", style = "margin-left:8px;")
      )
    }
  })
  
  # Login modal
  observeEvent(input$btn_show_login, {
    showModal(modalDialog(
      title = "Login",
      textInput("login_email", "Email"),
      passwordInput("login_password", "Password"),
      footer = tagList(modalButton("Batal"), actionButton("btn_login", "Login", class = "btn-primary")),
      easyClose = TRUE
    ))
  })
  
  # Register modal
  observeEvent(input$btn_show_register, {
    showModal(modalDialog(
      title = "Register",
      textInput("reg_email", "Email"),
      passwordInput("reg_password", "Password"),
      footer = tagList(modalButton("Batal"), actionButton("btn_register", "Daftar", class = "btn-primary")),
      easyClose = TRUE
    ))
  })
  
  # perform register
  observeEvent(input$btn_register, {
    req(input$reg_email, input$reg_password)
    removeModal()
    
    # Otomatis memberikan 3 Bintang
    ok <- create_user(input$reg_email, input$reg_password, initial_stars = 3) 
    
    if (isTRUE(ok)) {
      shinyjs::alert("Registrasi berhasil. Silakan login. Anda mendapatkan 3 Stars gratis.")
    } else {
      shinyjs::alert("Registrasi gagal — mungkin email sudah terdaftar.")
    }
  })
  
  # perform login
  observeEvent(input$btn_login, {
    req(input$login_email, input$login_password)
    removeModal()
    waiter_show(html = tagList(tags$h4("Memeriksa kredensial...")), color = "#ffffff")
    on.exit(waiter_hide(), add = TRUE)
    user <- verify_credentials(input$login_email, input$login_password)
    if (is.null(user)) {
      shinyjs::alert("Login gagal: email atau password salah.")
    } else {
      user_session$logged_in <- TRUE
      user_session$email <- user$email
      user_session$role  <- user$role
      showNotification(paste("Berhasil login:", user$email), type = "message")
      
      output$admin_users_dt <- renderDT({ datatable(fetch_all_users(), selection = "single", options = list(pageLength = 10, scrollX = TRUE)) })
      output$status_card_ui <- renderUI({ status_card_ui_func() })
      output$summary_ui <- renderUI({ summary_ui_func() })
    }
  })
  
  # logout
  observeEvent(input$btn_logout, {
    user_session$logged_in <- FALSE
    user_session$email <- NULL
    user_session$role  <- NULL
    rv$data <- NULL # Data direset
    showNotification("Anda telah logout.", type = "message")
    
    output$admin_users_dt <- renderDT({ datatable(data.frame(Msg = "Hanya admin dapat melihat tabel ini"), options = list(dom = 't')) })
  })
  
  # Status Card UI builder
  status_card_ui_func <- function() {
    if (!user_session$logged_in) {
      return(tagList(tags$h4("Belum login"), tags$p("Silakan login atau daftar terlebih dahulu.")))
    }
    user <- get_user_by_email(user_session$email)
    if (is.null(user)) return(tags$p("User tidak ditemukan."))
    tagList(
      tags$h4(user$email),
      tags$p(class = "muted", paste("Role:", user$role)),
      tags$div(class = "status-box",
               tags$h3("Stars: ", tags$span(style = "color:#d97706; font-weight:700;", user$stars)),
               tags$p("Uploads used: ", user$uploads_used),
               if (user$role == "admin") tags$p(tags$em("Anda login sebagai admin."))
      )
    )
  }
  
  output$status_card_ui <- renderUI({ status_card_ui_func() })
  
  # Summary UI builder
  summary_ui_func <- function() {
    if (!user_session$logged_in) return(tags$p("Login untuk melihat ringkasan akun."))
    user <- get_user_by_email(user_session$email)
    if (is.null(user)) return(tags$p("User tidak ditemukan"))
    tagList(
      tags$h4("Ringkasan Cepat"),
      tags$p(strong("Email:"), user$email),
      tags$p(strong("Role:"), user$role),
      tags$p(strong("Stars:"), tags$span(style = "color:#d97706; font-weight:700;", user$stars)),
      tags$p(strong("Uploads used:"), user$uploads_used),
      actionButton("btn_demo_reset", "Reset Uploads (Demo only)", class = "btn-link")
    )
  }
  
  output$summary_ui <- renderUI({ summary_ui_func() })
  
  # demo reset
  observeEvent(input$btn_demo_reset, {
    req(user_session$logged_in)
    if (user_session$role == "admin" || user_session$email == "demo@local") {
      reset_uploads_used(user_session$email)
      showNotification("Uploads used direset (0).", type = "message")
      output$status_card_ui <- renderUI({ status_card_ui_func() })
      output$summary_ui <- renderUI({ summary_ui_func() })
    } else {
      showNotification("Aksi tidak diizinkan.", type = "error")
    }
  })
  
  # UPLOAD UI
  output$upload_ui <- renderUI({
    if (!user_session$logged_in) return(tags$p("Silakan login untuk mengakses fitur upload."))
    user <- get_user_by_email(user_session$email)
    if (is.null(user)) return(tags$p("User tidak ditemukan."))
    disable_upload <- FALSE
    if (user$role != "admin") {
      if (user$uploads_used >= FREE_UPLOADS && user$stars <= 0) disable_upload <- TRUE
    }
    if (disable_upload) {
      tagList(
        tags$p("Anda telah menggunakan 3 upload gratis dan saldo bintang habis."),
        actionButton("btn_contact_admin", "Hubungi Admin", class = "btn-warning")
      )
    } else {
      tagList(
        fileInput("file_input_premium", "Pilih file (CSV / XLSX)", accept = c(".csv", ".xlsx", ".xls")),
        actionButton("btn_do_upload", "Upload & Proses", class = "btn-primary"),
        tags$hr(),
        downloadButton("download_data_premium", "Download Data (CSV)", class = "btn-secondary")
      )
    }
  })
  
  observeEvent(input$btn_contact_admin, {
    showModal(modalDialog(title = "Hubungi Admin", "Hubungi admin lewat email: admin@local (demo).", easyClose = TRUE))
  })
  
  # handle upload action
  observeEvent(input$btn_do_upload, {
    req(user_session$logged_in)
    req(input$file_input_premium)
    
    waiter_show(html = tagList(tags$h4("Memproses file..."), tags$p("Mengecek saldo bintang...")), color = "#ffffff")
    on.exit(waiter_hide(), add = TRUE)
    
    isolate({
      user <- get_user_by_email(user_session$email)
      if (is.null(user)) { showNotification("User tidak ditemukan.", type = "error"); return() }
      
      # decide cost
      if (user$role != "admin") {
        if (user$uploads_used < FREE_UPLOADS) {
          increment_upload_used(user$email)
          showNotification("Upload diterima (gratis).", type = "message")
        } else {
          if (user$stars >= 1) {
            ok <- update_user_stars(user$email, -1)
            if (!ok) { showNotification("Gagal mengurangi bintang.", type = "error"); return() }
            increment_upload_used(user$email)
            showNotification("Upload diterima. 1 bintang dikonsumsi.", type = "message")
          } else {
            showNotification("Saldo bintang tidak cukup. Hubungi admin.", type = "error")
            return()
          }
        }
      } else {
        showNotification("Admin: upload diterima tanpa konsumsi.", type = "message")
      }
      
      # read file safely and validate
      fp <- input$file_input_premium$datapath
      nm <- input$file_input_premium$name
      ext <- tools::file_ext(nm)
      df <- NULL
      tryCatch({
        if (tolower(ext) == "csv") df <- read_csv(fp, show_col_types = FALSE)
        else if (tolower(ext) %in% c("xlsx", "xls")) df <- read_excel(fp)
        else stop("Format tidak didukung")
      }, error = function(e) {
        showNotification(paste("Gagal membaca file:", e$message), type = "error")
      })
      
      if (is.null(df)) {
        rv$data <- NULL
        return()
      }
      
      # Data processing (rename columns for analysis)
      col_merk <- detect_col(df, c("merk","brand","merek"))
      col_type <- detect_col(df, c("type","tipe"))
      col_harga <- detect_col(df, c("harga","price","total","amount","nominal","jumlah"))
      df <- safe_rename(df, col_merk, "Merk")
      df <- safe_rename(df, col_type, "Type")
      df <- safe_rename(df, col_harga, "Harga")
      
      # Tambahkan cek validitas data KRITIS setelah rename
      
      kolom_wajib_ada <- c("Merk", "Harga")
      if (!all(kolom_wajib_ada %in% names(df))) {
        # Jika kolom wajib tidak ditemukan, tampilkan error dan JANGAN simpan data
        missing_cols <- setdiff(kolom_wajib_ada, names(df))
        rv$data <- NULL # Pastikan data kosong
        showNotification(
          paste("Gagal: Kolom wajib tidak ditemukan:", paste(missing_cols, collapse=", ")),
          type = "error", 
          duration = 8
        )
        return() 
      }
      
      # Konversi Harga setelah memastikan kolom ada
      df$Harga <- suppressWarnings(as.numeric(df$Harga))
      
      rv$data <- df
      
      # refresh status UIs
      output$status_card_ui <- renderUI({ status_card_ui_func() })
      output$summary_ui <- renderUI({ summary_ui_func() })
      
      showNotification("Data berhasil di-*upload* dan diproses! Lihat tab Analisis Penjualan.", type = "message")
      
    })
  })
  
  # Download Handler (Home tab)
  output$download_data_premium <- downloadHandler(
    filename = function() paste0("data_processed_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$data)
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
  
  # Reactive Data yang digunakan oleh semua Plot/Tabel (Analisis tab)
  dataAnalisis <- reactive({
    rv$data
  })
  
  observe({
    df <- dataAnalisis()
    req(df)
    choices <- if("Merk" %in% names(df)) sort(unique(na.omit(df$Merk))) else character(0)
    updateSelectInput(session, "pilihMerk", choices = choices, selected = if(length(choices)) choices[1] else NULL)
  })
  
  dt_class <- reactive({ if(isTRUE(sel$side_dark)) "cell-border stripe table-dark" else "stripe hover compact" })
  
  # refresh data handler (hanya refresh tampilan, data dari rv$data)
  observeEvent(input$refresh_data, { 
    req(rv$data) 
    waiter_show(html = tagList(tags$h4("Memuat ulang data...")), color = "#ffffff")
    Sys.sleep(1) 
    showNotification("Data refreshed!", type="message", duration=2) 
    waiter_hide()
  })
  
  # Download handler dataAnalisis (Analisis tab)
  output$download_data <- downloadHandler(
    filename=function(){ paste0("data_penjualan_", Sys.Date(), ".csv") },
    content=function(file){ write.csv(dataAnalisis(), file, row.names=FALSE) }
  )
  
  # -------- Value Boxes --------
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
  
  output$vbox_avg_harga <- renderValueBox({
    df <- dataAnalisis()
    val <- if(!is.null(df) && "Harga" %in% names(df)) mean(df$Harga, na.rm=TRUE) else 0
    valueBox(value=format_rupiah(val), subtitle="Rata-rata Harga", icon=icon("money-bill-wave"), color="warning")
  })
  
  # -------- Tabel & Plot --------
  # PERBAIKAN UTAMA: Sentralisasi logic tampilan data
  output$tabelData <- renderDT({
    df <- dataAnalisis()
    
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Msg = "Login & Upload data untuk ditampilkan"), options = list(dom = 't')))
    }
    
    datatable(df, options=list(pageLength=10, scrollX=TRUE), style='bootstrap4', class=dt_class(), filter='top', rownames=FALSE)
  })
  
  output$tabelMerk <- renderDT({
    df <- dataAnalisis()
    req(df)
    if(!"Merk" %in% names(df)) return(datatable(data.frame(Info="Kolom 'Merk' tidak ditemukan"), options=list(dom='t')))
    hasil <- df %>% group_by(Merk) %>% summarise(total_unit=n(), rata_rata_harga=mean(Harga, na.rm=TRUE), .groups="drop") %>% arrange(desc(total_unit))
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil, options=list(pageLength=10, scrollX=TRUE), class=dt_class(), rownames=FALSE)
  })
  
  output$tabelType <- renderDT({
    df <- dataAnalisis()
    req(df)
    merk_sel <- input$pilihMerk
    req(merk_sel)
    df <- df %>% filter(Merk==merk_sel)
    if(!"Type" %in% names(df)) return(datatable(data.frame(Info="Kolom 'Type' tidak ditemukan"), options=list(dom='t')))
    hasil <- df %>% group_by(Type) %>% summarise(total_unit=n(), rata_rata_harga=mean(Harga, na.rm=TRUE), .groups="drop") %>% arrange(desc(total_unit))
    hasil$rata_rata_harga <- format_rupiah(hasil$rata_rata_harga)
    datatable(hasil, options=list(pageLength=10, scrollX=TRUE), class=dt_class(), rownames=FALSE)
  })
  
  output$download_merk_table <- downloadHandler(
    filename=function(){ paste0("data_merk_", Sys.Date(), ".csv") },
    content=function(file){
      df <- dataAnalisis(); req(df)
      hasil <- df %>% group_by(Merk) %>% summarise(total_unit=n(), rata_rata_harga=mean(Harga, na.rm=TRUE), .groups="drop")
      write.csv(hasil, file, row.names=FALSE)
    }
  )
  
  output$download_type_table <- downloadHandler(
    filename=function(){ paste0("data_type_", Sys.Date(), ".csv") },
    content=function(file){
      df <- dataAnalisis(); req(df)
      merk_sel <- input$pilihMerk; req(merk_sel)
      hasil <- df %>% filter(Merk==merk_sel) %>% group_by(Type) %>% summarise(total_unit=n(), rata_rata_harga=mean(Harga, na.rm=TRUE), .groups="drop")
      write.csv(hasil, file, row.names=FALSE)
    }
  )
  
  renderPlotlyGrafik <- function(data_plot, x_col, y_col, graf_type, title) {
    n_cat <- nrow(data_plot)
    warna_types <- RColorBrewer::brewer.pal(max(3, min(8, n_cat)), "Set2")
    warna_types <- rep(warna_types, length.out=n_cat)
    
    if(graf_type=="bar"){
      p <- plot_ly(data_plot, x=as.formula(paste0("~", x_col)), y=as.formula(paste0("~", y_col)),
                   type='bar', marker=list(color=warna_types))
    } else if(graf_type=="line"){
      p <- plot_ly(data_plot, x=as.formula(paste0("~", x_col)), y=as.formula(paste0("~", y_col)),
                   type='scatter', mode='lines+markers', marker=list(color=warna_types),
                   line=list(color="#007bff"))
    } else if(graf_type=="pie"){
      p <- plot_ly(data_plot, labels=as.formula(paste0("~", x_col)), values=as.formula(paste0("~", y_col)),
                   type='pie', marker=list(colors=warna_types))
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
  
  output$comparisonMerk <- renderPlotly({
    df <- dataAnalisis(); req(df)
    data_plot <- df %>% group_by(Merk) %>% summarise(total_unit=n(), .groups="drop") %>% arrange(desc(total_unit)) %>% head(5)
    renderPlotlyGrafik(data_plot, "Merk", "total_unit", input$pilih_grafik_top5, "Top 5 Merk Terlaris")
  })
  
  output$comparisonType <- renderPlotly({
    df <- dataAnalisis(); req(df)
    merk_sel <- input$pilihMerk; req(merk_sel)
    data_plot <- df %>% filter(Merk==merk_sel) %>% group_by(Type) %>% summarise(total_unit=n(), .groups="drop") %>% arrange(desc(total_unit)) %>% head(5)
    renderPlotlyGrafik(data_plot, "Type", "total_unit", "bar", paste("Top 5 Type -", merk_sel))
  })
  
  
  # -----------------------
  # ADMIN PANEL
  # -----------------------
  output$admin_users_dt <- renderDT({ datatable(data.frame(Msg = "Hanya admin dapat melihat tabel ini"), options = list(dom = 't')) })
  
  # if admin logged in, render full table
  observe({
    if (user_session$logged_in && identical(user_session$role, "admin")) {
      output$admin_users_dt <- renderDT({ datatable(fetch_all_users(), selection = "single", options = list(pageLength = 10, scrollX = TRUE)) })
    }
  })
  
  # search by email
  observeEvent(input$admin_search_btn, {
    req(user_session$logged_in)
    if (!identical(user_session$role, "admin")) { showNotification("Hanya admin dapat melakukan aksi ini.", type = "error"); return() }
    q <- trimws(input$admin_search_email %||% "")
    if (q == "") { showNotification("Masukkan email untuk mencari.", type = "warning"); return() }
    user <- get_user_by_email(q)
    if (is.null(user)) {
      showNotification("User tidak ditemukan.", type = "error")
      output$admin_selected_ui <- renderUI({ tags$p("User tidak ditemukan") })
      rv$admin_selected_email <- NULL
    } else {
      rv$admin_selected_email <- user$email
      output$admin_selected_ui <- renderUI({
        tagList(tags$h5("Selected user"), tags$p(strong("Email: "), user$email),
                tags$p(strong("Role: "), user$role),
                tags$p(strong("Stars: "), tags$span(style = "color:#d97706; font-weight:700;", user$stars)),
                tags$p(strong("Uploads used: "), user$uploads_used))
      })
      showNotification(paste("User ditemukan:", user$email), type = "message")
    }
  })
  
  # when admin selects row in table
  observeEvent(input$admin_users_dt_rows_selected, {
    sel_row <- input$admin_users_dt_rows_selected
    if (length(sel_row) == 0) return()
    df <- fetch_all_users()
    row <- df[sel_row[1], , drop = FALSE]
    rv$admin_selected_email <- row$email
    output$admin_selected_ui <- renderUI({
      tagList(tags$h5("Selected user"), tags$p(strong("Email: "), row$email),
              tags$p(strong("Role: "), row$role),
              tags$p(strong("Stars: "), tags$span(style = "color:#d97706; font-weight:700;", row$stars)),
              tags$p(strong("Uploads used: "), row$uploads_used))
    })
  })
  
  # admin apply stars
  observeEvent(input$admin_apply_btn, {
    req(user_session$logged_in)
    if (!identical(user_session$role, "admin")) { showNotification("Hanya admin.", type = "error"); return() }
    target <- rv$admin_selected_email %||% trimws(input$admin_search_email %||% "")
    if (is.null(target) || target == "") { showNotification("Pilih atau cari user terlebih dahulu.", type = "warning"); return() }
    delta <- as.integer(input$admin_star_delta)
    if (is.na(delta) || delta == 0) { showNotification("Masukkan jumlah bintang (bukan 0).", type = "warning"); return() }
    ok <- update_user_stars(target, delta)
    if (ok) {
      logmsg <- paste0(Sys.time(), " | Admin ", user_session$email, " applied ", delta, " stars to ", target)
      output$admin_log <- renderText({ paste0(logmsg, "\n\nOperation successful.") })
      showNotification("Perubahan bintang berhasil.", type = "message")
      
      output$admin_users_dt <- renderDT({ datatable(fetch_all_users(), selection = "single", options = list(pageLength = 10, scrollX = TRUE)) })
      user <- get_user_by_email(target)
      output$admin_selected_ui <- renderUI({
        tagList(tags$h5("Selected user"), tags$p(strong("Email: "), user$email),
                tags$p(strong("Role: "), user$role),
                tags$p(strong("Stars: "), tags$span(style = "color:#d97706; font-weight:700;", user$stars)),
                tags$p(strong("Uploads used: "), user$uploads_used))
      })
    } else {
      showNotification("Gagal mengubah bintang (mungkin akan menjadi negatif).", type = "error")
    }
  })
  
  # admin delete user
  observeEvent(input$admin_delete_btn, {
    req(user_session$logged_in)
    
    # 1. Cek Admin Role
    if (!identical(user_session$role, "admin")) { 
      showNotification("Hanya admin dapat melakukan aksi ini.", type = "error"); return() 
    }
    
    # 2. Tentukan target user
    target <- rv$admin_selected_email %||% trimws(input$admin_search_email %||% "")
    if (is.null(target) || target == "") { 
      showNotification("Pilih atau cari user terlebih dahulu.", type = "warning"); return() 
    }
    
    # 3. Konfirmasi sebelum hapus (opsional, tapi disarankan)
    showModal(modalDialog(
      title = "Konfirmasi Penghapusan",
      paste("Apakah Anda yakin ingin menghapus user:", target, "? Aksi ini tidak dapat dibatalkan."),
      footer = tagList(
        modalButton("Batal"),
        actionButton("confirm_delete_user", "Hapus Sekarang", class = "btn-danger")
      )
    ))
  })
  
  # Logika penghapusan setelah konfirmasi
  observeEvent(input$confirm_delete_user, {
    removeModal()
    target <- rv$admin_selected_email %||% trimws(input$admin_search_email %||% "")
    
    if (is.null(target) || target == "") {
      showNotification("Target tidak valid.", type = "error"); return()
    }
    
    # Panggil fungsi penghapusan dengan safety check
    result <- delete_user_by_email(target)
    
    if (result == "Success") {
      logmsg <- paste0(Sys.time(), " | Admin ", user_session$email, " deleted user ", target)
      output$admin_log <- renderText({ paste0(logmsg, "\n\nOperation successful.") })
      showNotification(paste("User", target, "berhasil dihapus."), type = "message")
      
      # Reset tampilan dan refresh tabel admin
      rv$admin_selected_email <- NULL
      output$admin_selected_ui <- renderUI({ tags$p("User berhasil dihapus.") })
      output$admin_users_dt <- renderDT({ datatable(fetch_all_users(), selection = "single", options = list(pageLength = 10, scrollX = TRUE)) })
      
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