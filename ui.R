## Library ####
library(shiny)
library(shinydashboard)
library(bslib)
library(leaflet)
library(pool) # untuk bekerja dengan banyak koneksi
library(glue) # untuk membuat query schema lebih dinamis
library(DBI)
library(RPostgres)
library(readxl)
library(dplyr)
library(sf)
library(mapview)
library(plotly)
library(tidyr)
library(jsonlite)
library(colorspace)
library(shinyjs) # agar bisa menggunakan onclick
library(remotes)
library(DT)
library(reactable)
library(htmltools)
library(purrr)
library(lubridate)
library(stringr)
library(stringi)
library(leafgl)
library(rsconnect)


ui <- page_navbar(
  
  # Header ####
  title = tagList(
    tags$img(
      src = "WCS_Logo_Acronym_Dark Background.png",
      height = "100px",
      style = "margin-right: 5px;"
    ),
    
    tags$div(
      style = "
        display: flex; flex-direction: 
        column; line-height: 1; 
        color: white; 
        font-weight: bold; 
        font-size: 30px; 
        letter-spacing: 1px; 
        text-transform: uppercase; 
        margin: 0; 
        text-align: center;",
      tags$span("Forest Verse", style = "display: block;"),
      #tags$span("Verse", style = "display: block;")
    ),
    div(
      style = "position: absolute; top: 40px; right: 20px; z-index: 9999;",
      popover(
        actionLink("info_btn", icon("info-circle")),
        title = "Tentang Aplikasi",
        placement = "right",  # ✅ Valid
        tags$div(
          tags$b("Informasi:"),
          p("Ini adalah versi pertama. Masih ada bug :)"),
          tags$b("Feedback:"),
          p("Hubungi elestari@wcs.org jika ada masukan.")
        )
      ))
  ),
  ## CSS ####
  tags$head(
    tags$style(HTML(" 
    
    .navbar {
        background-image: url('Sungai Canguk_WCS-IP_Praminto Moehayat.JPG');
        background-size: cover;
        background-repeat: no-repeat;
        background-position: center;
        color: white;
      }
      .navbar .navbar-brand, .navbar .nav-link {
        color: white !important;
      }
      .navbar::after {
        content: 'Foto: Praminto Moehayat / Way Canguk';
        position: absolute;
        bottom: 5px;
        right: 15px;
        font-size: 12px;
        color: white;
        background-color: rgba(0, 0, 0, 0.4);
        padding: 2px 6px;
        border-radius: 4px;
      }
    
      /* Background title bar */
     
      .navbar-brand {
      display: flex !important;
      align-items: center !important;
      gap: 12px;
      padding-top: 0 !important;
      padding-bottom: 0 !important;
      margin: 0 !important;
      line-height: 1 !important;
      }
      .navbar-brand img {
      display: block;
      margin: 0;
    }
      .navbar {
      margin-bottom: 0 !important;
      }
      .navbar .nav-link {
      color: white !important;
      font-weight: bold;
      }
    
      /* Ubah warna latar belakang popover */
      .popover {
        background-color: white !important;
        border: 1px solid #ccc;
      }

      .popover-body {
        color: black;
        background-color: white !important;
      }

      .popover-header {
        background-color: #e6f0ee !important;
        font-weight: bold;
      }
      .card-header-custom {
        background-color: #205b29;
        color: black;
        font-weight: bold;
      }
      
      .card-custom-bg {
        background-color: #e8f5e9;
      }
      
      .card img {
        width: 100%;
        height: 360px;
        object-fit: cover;
        border-radius: 12px 12px 0 0;
      }
      
      .sidebar-button {
        background-color: #f5faeb;
        border: none;
        padding: 12px 10px;
        margin-bottom: 10px;
        border-radius: 12px;
        font-weight: bold;
        font-size: 1rem;
        text-align: left;
        color: #000;
        display: flex;
        align-items: center;
        gap: 10px;
        width: 100%;
        box-shadow: none;
        border: none;
        transition: all 0.2s ease;
      }
      
      .sidebar-button img {
        background-color: transparent !important;
        border-radius: 0 !important;
        box-shadow: none !important;
        height: 40px;
      }
      
      .sidebar-button:hover {
        background-color: #dbe9ea;
      }

      .sidebar-button.active {
        background-color: #b3cfd0;
        
      }

      .sidebar-footer {
        font-size: 10px;
        color: #666;
        text-align: center;
        padding: 8px;
        margin-top: auto;
      }
      
      /* Button Kembali */
      .get-started-button {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 12px;
        height: 48px;
        width: 180px;
        background-color: #c3efc8;
        border-radius: 0.5rem;
        font-weight: bold;
        transition: background-color 0.2s ease-in-out;
        cursor: pointer;
      }
      .get-started-button:hover {
        background-color: #22732e;
      }
      .get-started-text {
        color: #22732e;
        transition: color 0.2s ease-in-out;
      }
      .get-started-button:hover .get-started-text {
        color: #c3efc8;
      }
      .arrow-container {
        position: relative;
        display: flex;
        align-items: center;
        justify-content: center;
        height: 28px;
        width: 28px;
        border-radius: 9999px;
        overflow: hidden;
        background-color: #c3efc8;
        transition: background-color 0.2s;
      }
      .get-started-button:hover .arrow-container {
        background-color: #c3efc8;
      }
      .arrow-icon {
        position: absolute;
        font-size: 18px;
        transition: opacity 0.2s ease-in-out;
      }
      .arrow-icon.first {
        opacity: 0;
        color: #22732e;
      }
      .arrow-icon.second {
        opacity: 1;
        color: #c3efc8;
      }
      .get-started-button:hover .arrow-icon.first {
        opacity: 1;
      }
      .get-started-button:hover .arrow-icon.second {
        opacity: 0;
      }
      
      /* Foto di Tab Peserta */
      .profile-card {
        background-color: #f9fafb;
        border-radius: 12px;
        padding: 24px;
        width: 100%;
        max-width: none;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        font-family: sans-serif;
        box-sizing: border-box;
      }
      .profile-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .profile-header-left {
        display: flex;
        align-items: center;
        gap: 16px;
      }
      .profile-photo {
        width: 60px;
        height: 60px;
        border-radius: 50%;
        object-fit: cover;
      }
      .profile-name {
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 4px;
        text-align: left;
      }
      .profile-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        margin-top: 24px;
        border-top: 1px solid #e5e7eb;
        padding-top: 16px;
        font-size: 14px;
        color: #4b5563;
      }
      .profile-grid div {
        text-align: center;
      }
      .profile-grid .label {
        font-weight: bold;
        color: #111827;
        margin-bottom: 4px;
      }
      
      /* Button Tab Aplikasi - Patroli */
      .btn-custom {
        background-color: #205b29;     /* warna hijau tua */
        color: white !important;       /* warna teks */
        border: none;
        padding: 12px 20px;
        font-size: 16px;
        font-weight: bold;
        border-radius: 8px;
        transition: background-color 0.3s ease;
        height: 48px;
        width: 180px;
      }
  
      .btn-custom:hover {
        background-color: #1a4720;     /* warna saat hover */
      }
      
      /* Button Kembali Tab Aplikasi - Patroli */
      .btn-custom-back {
        background-color: #8b0000;     /* warna hijau tua */
        color: white !important;       /* warna teks */
        border: none;
        padding: 12px 20px;
        font-size: 16px;
        font-weight: bold;
        border-radius: 8px;
        transition: background-color 0.3s ease;
        height: 48px;
        width: 180px;
      }
  
      .btn-custom-back:hover {
        background-color: #560000;     /* warna saat hover */
      }
      
      /* Responsive Adjustments */
      @media (max-width: 768px) {
      .sidebar-button {
        font-size: 1rem;  /* Lebih kecil untuk layar kecil */
        padding: 10px 8px;
      }
      
    ")),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=yes")
  ),
  
  theme = bs_theme(
    version = 5, 
    primary = "#00623c"),
  useShinyjs(),
  fillable = TRUE,
  
  ## Layout Sidebar ForestVerse ####
  layout_sidebar(
    border = FALSE,
    fillable = FALSE,
    sidebar = sidebar(
      height = "100vh",
      style = "
      overflow-y: hidden; 
      padding-bottom: 10px;
      ",
      
      
      ### Button Home ####
      actionButton(
        "btn_home", 
        label = tagList(
          tags$img(
            src = "home.png", 
            height = "3rem", 
            style = "margin-right: 8px;"), 
          "HOME"), 
        class = "w-100 sidebar-button"),
      
      ### Button Beneficiaries ####
      actionButton(
        "btn_kegiatan", 
        label = tagList(
          tags$img(src = "RESTORASI_transparent.png", height = "3rem", style = "margin-right: 8px;"), 
          "BENEFICIARIES"), 
        class = "w-100 sidebar-button"),
      
      ### Button Petani ####
      actionButton(
        "btn_petani",
        label = tagList(
          tags$img(src = "RESTORASI_transparent.png", height = "3rem", style = "margin-right: 8px;"), 
          "PETANI"), 
        class = "w-100 sidebar-button"),
      
      ### Button Biodiv ####
      actionButton(
        "btn_biodiv", 
        label = tagList(
          tags$img(src = "Biodiversity Monitoring_transparent.png", height = "3rem", style = "margin-right: 8px;"), 
          "BIODIVERSITAS"), 
        class = "w-100 sidebar-button"),
      
      ### Button Patroli ####
      actionButton(
        "btn_patrol", 
        label = tagList(
          tags$img(
            src = "PATROLI_transparent (1).png",
            height = "3rem", 
            style = "margin-right: 8px;"), 
          "PATROLI"
        ),
        class = "w-100 sidebar-button"),
      
      ### Button WRU ####
      actionButton(
        "btn_konflik", 
        label = tagList(
          tags$img(src = "HWCM_transparent.png", height = "3rem", style = "margin-right: 8px;"), 
          "KONFLIK MANUSIA - SATWA"), 
        class = "w-100 sidebar-button"),
      
      ### Button Stasiun Penelitian ####
      actionButton(
        "btn_stasiunpenelitian", 
        label = tagList(
          tags$img(src = "Biodiversity Monitoring_transparent.png", height = "3rem", style = "margin-right: 8px;"), 
          "STASIUN PENELITIAN"), 
        class = "w-100 sidebar-button"),
      
      ### Button Restoration ####
      actionButton(
        "btn_restoration", 
        label = tagList(
          tags$img(src = "RESTORASI_transparent.png", height = "3rem", style = "margin-right: 8px;"), 
          "RESTORASI"), 
        class = "w-100 sidebar-button"),
      
      div(style = "margin-top: auto; height: 10px;"),
      div(
        class = "sidebar-footer",
        HTML("&copy; CONSERVATION EVIDENCE UNIT<br>FOREST PROGRAM - WCSIP")
      )
    ),
    
    ### Main content ####
    uiOutput("main_tab")
  )
)
