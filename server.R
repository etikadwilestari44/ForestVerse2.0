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


server <- function(input, output, session) {
  
  # Main Panel - ReactiveVal ####
  active_section <- reactiveVal("HOME")
  current_page <- reactiveVal("BIODIV") 
  # Menyimpan layout internal untuk setiap section
  layout_state <- reactiveVal("PETANI")  # Default untuk PETANI
  
  # Main Panel - Button Sidebar ####
  observeEvent(input$btn_home,     { active_section("HOME") })
  observeEvent(input$btn_patrol,   { active_section("PATROLI") })
  observeEvent(input$btn_konflik, { active_section("KONFLIK") })
  observeEvent(input$btn_petani,   {
    active_section("PETANI")
    layout_state("PETANI")  # Reset ke layout awal saat pindah section
  })
  observeEvent(input$btn_restoration,   { active_section("RESTORASI") })
  observeEvent(input$btn_kegiatan,   { active_section("KEGIATAN") })
  observeEvent(input$btn_biodiv,   { active_section("BIODIV"); current_page("BIODIV") })
  
  # Biodiversitas - Card ####
  # Klik card menggunakan shinyjs::onclick untuk card biodiv
  observe({
    onclick("card-harimau", {
      current_page("page_harimau")
    })
    onclick("card-gajah", {
      current_page("page_gajah")
    })
    onclick("card-maleo", {
      current_page("page_maleo")
    })
    onclick("card-kurot", {
      current_page("page_kurot")
    })
    onclick("card-siamang-owa", {
      current_page("page_siamang_owa")
    })
    onclick("card-rangkong", {
      current_page("page_rangkong")
    })
  })
  
  # Patroli - Sidebar State ####
  # Tampilan sidebar state - filter Patroli ####
  sidebar_state <- reactiveVal("filterPatroli")  # default state
  ## Tampilan sidebar state - Summary Patroli ####
  ## Tampilan sidebar state - Analysisi Patroli ####
  
  # Patroli - Button Sidebar ####
  observeEvent(input$goBackToFilterPatroli, {
    sidebar_state("filterPatroli")
  })
  
  observeEvent(input$goToAnalysisPatroli, {
    sidebar_state("analysisPatroli")
  })
  
  observeEvent(input$goBackToSummaryPatroli, {
    sidebar_state("summaryPatroli")
  })
  
  
  # Petani - Layout main panel ####
  # Navigasi internal di dalam section PETANI
  observeEvent(input$go_to_data_petani, {
    layout_state("layout_data_kegiatan_training")
  })
  observeEvent(input$go_to_data_lahan, {
    layout_state("layout_data_lahan_petani")
  })
  observeEvent(input$goBackToPetani, {
    layout_state("PETANI")
  })
  
  mode <- reactiveVal("tabel")
  petani_terpilih <- reactiveVal(NULL)
  
  # User Interface - Tab ####
  output$main_tab <- renderUI({
    section <- active_section()
    layout <- layout_state()
    
    ## Home ####
    if (section == "HOME") { tagList(
      card(
        full_screen = FALSE,
        class = "p-4 mb-4 bg-light border rounded shadow-sm",
        
        card_header(h2("Selamat datang di Forest Verse!")),
        
        card_body(
          p("Forest Verse adalah platform untuk memantau progres, serta melakukan filter, summary dan visualisasi data Forest Program WCS-IP", class = "lead"),
          p("Panduan lengkap cara menggunakan Forest Verse juga tersedia di Perpustakaan Forest Space ya!")
        ),
        
        card_footer(
          tags$a(
            href = "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bird-survey-results-for-areas-in-the-city-of-melbourne-february-and-march-2018/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C",
            class = "btn btn-info",
            target = "_blank",
            "Download Panduan"
          )
        )
      )
      
    )
      
      ## UI::Patroli ####
    } else if (section == "PATROLI") { 
      navset_tab(
        ## UI:Patroli:Dashboard ####
        nav_panel("Dashboard",
                  br(),
                  layout_column_wrap(
                    gap = "1rem",
                    div(style = "margin-top: 1rem;",
                        card(
                          card_body(
                            layout_columns(
                              uiOutput("valuebox_adopsi_smart"),
                              div(
                                class = "info-card",
                                style = "padding-top: 3rem;",  # Atur jarak atas
                                p("Untuk melihat informasi adopsi SMART lebih lengkap, silakan klik tombol di bawah ini:"),
                                tags$a(
                                  href = "https://wcs1.sharepoint.com/:x:/r/sites/FORESTPROGRAM-WCSIP2/_layouts/15/Doc.aspx?sourcedoc=%7BDBACC5F5-268D-431B-86B4-0B09A6B1089C%7D&file=1.4%20SMART%20adoption.xlsx&action=default&mobileredirect=true",  # Replace with your link
                                  class = "btn btn-primary",
                                  target = "_blank",  # Open in a new tab
                                  "Lihat Selengkapnya"
                                )
                              )
                            )
                          )
                        ))
                  ),
                  #### UI:Patroli:Dashboard:KM Patroli ####
                  card(
                    card_body(
                      layout_sidebar(
                        border = FALSE,
                        fillable = FALSE,
                        
                        ##### UI:Patroli:Dashboard:Filter KM Patroli ####
                        sidebar = tagList(
                          h4("🔍 Filter Data"),
                          selectInput(
                            "landscape_gridpatroli", "Pilih Landscape",
                            choices = unique(data_gridpatroli$landscape),
                            selected = unique(data_gridpatroli$landscape)[1],
                            multiple = TRUE
                          )
                        ),
                        ##### UI:Patroli:Dashboard:Peta dan Grafik KM Patroli ####
                        div(
                          class = "d-flex justify-content-between gap-3",  # CSS flexbox untuk sejajar
                          div(style = "flex: 1;", plotlyOutput("grafik_gridprioritas", height = "500px")),
                          div(style = "flex: 1;", leafletOutput("peta_gridprioritas", height = "500px"))
                        ))
                    )
                  ),
                  #### UI:Patroli:Dashboard:Upaya Patroli ####
                  card(
                    card_body(
                      layout_sidebar(
                        border = FALSE,
                        fillable = FALSE,
                        
                        ##### UI:Patroli:Dashboard:Filter Upaya Patroli ####
                        sidebar = tagList(
                          h4("🔍 Filter Data"),
                          selectInput(
                            "landscape_temuan", 
                            "Pilih Landscape:", 
                            choices = unique(data_temuanpatroli$landscape),
                            selected = unique(data_temuanpatroli$landscape)[1],
                            multiple = TRUE),
                          radioButtons("jenis_temuan", "Jenis Temuan:", choices = c("Patroli", "Satwa")),
                          radioButtons("mode_cpue", "Hitung CPUE per:", choices = c("100km", "10km")),
                          radioButtons("mode", "Mode Tampilan:", choices = c("Tahunan", "Bulanan"), inline = TRUE),
                          uiOutput("tahun_ui"),
                          uiOutput("observation_ui"),
                          uiOutput("kategori_ui")
                        ),
                        
                        ##### UI:Patroli:Dashboard:Grafik Temuan Patroli ####
                        
                        div(
                          class = "d-flex justify-content-between gap-3",  # CSS flexbox untuk sejajar
                          div(style = "flex: 1;", plotlyOutput("plot_cpue", height = "500px"))
                        ))
                    )
                  )),
        ### UI:Patroli:Aplikasi ####
        nav_panel("Aplikasi", 
                  layout_sidebar(
                    border = FALSE,
                    fillable = FALSE,
                    sidebar = uiOutput("sidebar_aplikasi_patroli"),
                    uiOutput("main_panel")
                  )
        )
      )
      
      ## UI:WRU ####
    } else if (section == "KONFLIK") { navset_tab(
      ### UI:WRU - Dashboard ####
      nav_panel("Dashboard", "-- Konten KONFLIK --"),
      ### UI:WRU - Aplikasi ####
      nav_panel("Aplikasi", "-- Aplikasi KONFLIK --")
    )
      
      ## UI:Petani ####
    } else if (section == "PETANI") {
      switch(layout,
             "PETANI" = navset_tab(
               
               ### UI:Petani:Dashboard ####
               nav_panel(
                 "Dashboard",
                 br(),
                 layout_columns(
                   fill = FALSE,
                   
                   ## UI:Petani:Dashboard:ValueBox:Total Petani ####
                   value_box(
                     title = tags$div("Jumlah Total Petani", style = "font-weight: bold;"),
                     value = textOutput("totalPetani"),
                     showcase = icon("person", class = "fa-2x"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border",
                     tags$div(
                       style = "margin-top: 6px;",
                       
                       tags$div(style = "display: flex; justify-content: flex-start;",
                                tags$b("BBS :"), 
                                tags$span(style = "margin-left: 5px;", 
                                          textOutput("totalPetaniBBS"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Singkil :"),
                                tags$span(style = "margin-left: 5px;", 
                                          textOutput("totalPetaniSingkil"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Sulut :"),
                                tags$span(style = "margin-left: 5px;", 
                                          textOutput("totalPetaniSulut"))
                       )
                     )
                   ),
                   
                   ## UI:Petani:Dashboard:ValueBox:Petani Dampingan ####
                   value_box(
                     title = tags$div("Jumlah Petani Dampingan", style = "font-weight: bold;"),
                     value = textOutput("totalPetaniDampingan"),
                     showcase = icon("person", class = "fa-2x"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border",
                     tags$div(
                       style = "margin-top: 6px;",
                       
                       tags$div(style = "display: flex; justify-content: flex-start;",
                                tags$b("BBS :"), 
                                tags$span(style = "margin-left: 5px;", 
                                          textOutput("totalPetaniDampinganBBS"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Singkil :"),
                                tags$span(style = "margin-left: 5px;", 
                                          textOutput("totalPetaniDampinganSingkil"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Sulut :"),
                                tags$span(style = "margin-left: 5px;", 
                                          textOutput("totalPetaniDampinganSulut"))
                       )
                     )
                   ),
                   
                   ## UI:Petani:Dashboard:ValueBox:Lahan Petani Terpetakan ####
                   value_box(
                     title = tags$div("Jumlah Lahan Petani yang Dipetakan", style = "font-weight: bold;"),
                     value = textOutput("totalLahanPetaniTerpetakan"),
                     showcase = icon("person"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border",
                     tags$div(
                       style = "margin-top: 6px;",
                       
                       tags$div(style = "display: flex; justify-content: flex-start;",
                                tags$b("BBS :"), 
                                tags$span(style = "margin-left: 5px;", textOutput("totalPetaniTerpetakanBBS"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Singkil :"),
                                tags$span(style = "margin-left: 5px;", textOutput("totalPetaniTerpetakanSingkil"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Sulut :"),
                                tags$span(style = "margin-left: 5px;", textOutput("totalPetaniTerpetakanSulut"))
                       )
                     ),
                     actionLink(
                       "go_to_data_lahan",
                       "Lihat Detail Data",
                       style = "font-weight: bold; color: #fff;")
                   )),
                 
                 ## UI:Petani:Dashboard:ValueBox:Kegiatan Pelatihan GAP ####
                 layout_columns(
                   value_box(
                     title = tags$div("Jumlah Kegiatan Pelatihan GAP", style = "font-weight: bold;"),
                     value = textOutput("totalKegiatanTrainingGAP"),
                     showcase = icon("person"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border",
                     tags$div(
                       style = "margin-top: 6px;",
                       
                       tags$div(style = "display: flex; justify-content: flex-start;",
                                tags$b("BBS :"), 
                                tags$span(style = "margin-left: 5px;", textOutput("kegiatanTrainingBBS"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Singkil :"),
                                tags$span(style = "margin-left: 5px;", textOutput("kegiatanTrainingSingkil"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Sulut :"),
                                tags$span(style = "margin-left: 5px;", textOutput("kegiatanTrainingSulut"))
                       )
                     ),
                     actionLink(
                       "go_to_data_petani",
                       "Lihat Detail Data",
                       style = "font-weight: bold; color: #fff;")
                   ),
                   
                   ## UI:Petani:Dashboard:ValueBox:Petani Terlatih ####
                   value_box(
                     title = tags$div("Jumlah Petani Terlatih", style = "font-weight: bold;"),
                     value = textOutput("totalPetaniTrainingGAP"),
                     showcase = icon("person", class = "fa-2x"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border",
                     tags$div(
                       style = "margin-top: 6px;",
                       
                       tags$div(style = "display: flex; justify-content: flex-start;",
                                tags$b("BBS :"), 
                                tags$span(style = "margin-left: 5px;", textOutput("totalPetaniTerlatihBBS"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Singkil :"),
                                tags$span(style = "margin-left: 5px;", textOutput("totalPetaniTerlatihSingkil"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Sulut :"),
                                tags$span(style = "margin-left: 5px;", textOutput("totalPetaniTerlatihSulut"))
                       )
                     )
                   ),
                   
                   ## UI:Petani:Dashboard:ValueBox:Petani Adopsi ####
                   value_box(
                     title = tags$div("Jumlah Petani Adopsi", style = "font-weight: bold;"),
                     value = textOutput("totalPetaniAdopsi"),
                     showcase = icon("person"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border",
                     tags$div(
                       style = "margin-top: 6px;",
                       
                       tags$div(style = "display: flex; justify-content: flex-start;",
                                tags$b("BBS :"), 
                                tags$span(style = "margin-left: 5px;", textOutput("totalPetaniAdopsiBBS"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Singkil :"),
                                tags$span(style = "margin-left: 5px;", textOutput("totalPetaniAdopsiSingkil"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Sulut :"),
                                tags$span(style = "margin-left: 5px;", textOutput("totalPetaniAdopsiSulut"))
                       )
                     )
                   )
                 ),
                 ## UI:Petani:Dashboard:Grafik: Kumulatif Total Petani #### 
                 layout_columns(
                   card(
                     height = "700px", 
                     full_screen = TRUE, 
                     card_header("Jumlah Kumulatif Petani",
                                 class="card-header-custom"), 
                     plotlyOutput("barChartPetaniKumulatif", 
                                  height = "450px"))
                 ),
                 
                 ## UI:Petani:Dashboard:Peta Lahan Petani ####
                 layout_columns(
                   card(
                     height = "700px", 
                     full_screen = TRUE, 
                     card_header("Peta Lahan Petani",
                                 class="card-header-custom"), 
                     leafletOutput("mapLahanPetani", 
                                   height = "450px"))
                 )),
               
               ## UI:Petani:Profile Petani ####
               nav_panel(
                 "Profile Petani",
                 layout_sidebar(
                   border = FALSE,
                   fillable = FALSE,
                   
                   #### UI:Petani:Profile Petani:Sidebar ####
                   sidebar = tagList(
                     h4("🔍 Filter Data"),
                     
                     div(style = "margin-bottom: 1rem;",
                         selectInput("lokasi_petani", "📍 Pilih Landscape",
                                     choices = data_petani$landscape,
                                     selected = NULL,
                                     multiple = TRUE)
                     )
                   ),
                   
                   #### UI:Petani:Profile Petani:Main Content ####
                   layout_columns(
                     uiOutput("konten_profile_petani")
                   )
                   
                 )),
               ## UI:Petani:Kegiatan Pelatihan ####
               nav_panel(
                 "Kegiatan",
                 layout_sidebar(
                   border = FALSE,
                   fillable = FALSE,
                   
                   #### UI:Petani:Kegiatan Pelatihan:Sidebar ####
                   sidebar = tagList(
                     h4("🔍 Filter Data"),
                     div(style = "margin-bottom: 1rem;",
                         dateRangeInput("periode_kegiatanGAP", "📅 Pilih Periode",
                                        start = Sys.Date() - 30, 
                                        end = Sys.Date(), 
                                        format = "dd-mm-yyyy")
                     ),
                     div(style = "margin-bottom: 1rem;",
                         selectInput("lokasi_kegiatanGAP", "📍 Pilih Landscape",
                                     choices = data_kegiatan_training_gap$landscape,
                                     selected = NULL,
                                     multiple = TRUE)
                     )
                   ),
                   
                   #### UI:Petani:Kegiatan Pelatihan:Tabel Kegiatan GAP ####
                   layout_columns(
                     reactableOutput("tabel_kegiatan_gap")
                   )
                 ))
             )
      )
    } 
    
    ## UI:Restorasi ####
    else if (section == "Restorasi") { navset_tab(
      #### UI:Restorasi:Dashboard ####
      nav_panel("Dashboard", "-- Konten RESTORASI --"),
      
      ## UI:Restorasi:Aplikasi ####
      nav_panel("Aplikasi", "-- Aplikasi RESTORASI --")
    )
      
      # UI:Beneficiaries ####
    } else if (section == "KEGIATAN") { navset_tab(
      
      ## UI:Beneficiaries:Tab Kegiatan ####
      nav_panel("Kegiatan",
                
                # UI:Beneficiaries:Tab Kegiatan:Sidebar ----
                layout_sidebar(
                  border = FALSE,
                  fillable = FALSE,
                  collapsible = TRUE,
                  sidebar = sidebar(
                    height = "auto",
                    style = "overflow-y: auto; padding-bottom: 20px;",
                    h4("🔍 Filter Data"),
                    
                    ##### UI:Beneficiaries:Tab Kegiatan:Sidebar:Filter Landscape ####
                    selectizeInput(
                      "filter_landscape_beneficiaries", 
                      "Landscape", 
                      choices = c(unique(data_kegiatan$landscape)), 
                      multiple = TRUE),
                    
                    ##### UI:Beneficiaries:Tab Kegiatan:Sidebar:Filter Periode ####
                    dateRangeInput("filter_periode_beneficiaries", "Periode Kegiatan",
                                   start = min(data_kegiatan$tanggal_mulai_kegiatan, na.rm = TRUE),
                                   end = max(data_kegiatan$tanggal_akhir_kegiatan, na.rm = TRUE)),
                    
                    ##### UI:Beneficiaries:Tab Kegiatan:Sidebar:Filter Jenis Kegiatan ####
                    selectizeInput(
                      "filter_jenis_beneficiaries", 
                      "Jenis Kegiatan", 
                      choices = c(unique(data_kegiatan$jenis_kegiatan)), 
                      multiple = TRUE),
                    
                    ##### UI:Beneficiaries:Tab Kegiatan:Sidebar:Filter Unit ####
                    selectizeInput(
                      inputId = "filter_unit_beneficiaries",
                      label = "Program/Unit yang Terlibat",
                      choices = c(unit_terlibat_clean),
                      multiple = TRUE)
                  ),
                  
                  ## UI:Beneficiaries:Tab Kegiatan:Tabel Kegiatan Beneficiaries ----
                  layout_columns(
                    style = "overflow-x: auto;",
                    reactableOutput("tabel_beneficiaries")
                  )
                )),
      
      ## UI:Beneficiaries:Tab Peserta ####
      nav_panel("Peserta",
                
                ## UI:Beneficiaries:Tab Peserta:Sidebar ####
                layout_sidebar(
                  border = FALSE,
                  fillable = FALSE,
                  collapsible = TRUE,
                  sidebar = sidebar(
                    height = "100vh",
                    style = "overflow-y: auto; height: 100vh; padding-bottom: 20px;",
                    
                    h4("🔍 Filter Data"),
                    ##### UI:Beneficiaries:Tab Peserta:Sidebar:Filter Landscape ####
                    selectizeInput(
                      "filter_landscape_peserta", 
                      "Landscape", 
                      choices = c(unique(data_peserta_kegiatan$landscape)), 
                      multiple = TRUE),
                    
                    ##### UI:Beneficiaries:Tab Peserta:Sidebar:Filter Jenis Kelamin ####
                    selectizeInput(
                      "filter_gender_beneficiaries", 
                      "Jenis Kelamin", 
                      choices = c(unique(data_peserta$jenis_kelamin)), 
                      multiple = TRUE),
                    
                    ##### UI:Beneficiaries:Tab Peserta:Sidebar:Filter ASN/NON-ASN ####
                    selectizeInput(
                      "filter_asn_beneficiaries", 
                      "ASN/Non-ASN", 
                      choices = c(unique(data_peserta$asn_nonasn)), 
                      multiple = TRUE),
                    
                    ##### UI:Beneficiaries:Tab Peserta:Sidebar:Filter Unit ####
                    selectizeInput(
                      "filter_instansi_beneficiaries", 
                      "Asal Instansi", 
                      choices = c(unique(data_peserta$asal_instansi)), 
                      multiple = TRUE),
                    
                  ),
                  
                  ## UI:Beneficiaries:Tab Peserta:Tabel Peserta ----
                  layout_columns(
                    uiOutput("konten_ui_beneficiaries")
                  )
                ))
    )
      
      # UI:Biodiversitas ####
    } else if (section == "BIODIV") { 
      switch(current_page(),
             
             ## UI:Biodiversitas:Card ####
             "BIODIV" = layout_column_wrap(
               width = 1/4,  # artinya 4 kolom per baris (karena 1/4 * 4 = 1)
               fixed_width = TRUE,
               heights_equal = "row",  # tinggi tiap card seragam dalam 1 baris
               style = "padding: 1.5rem; gap: 1.5rem;",
               
               ### UI:Biodiversitas:Card:Harimau ####
               div(
                 id = "card-harimau",
                 style = "cursor:pointer;",
                 card(
                   style = "overflow: hidden; position: relative;",
                   card_image(
                     src = "CT_Leuser_2024.jpeg",
                     style = "height: 350px; object-fit: cover; width: 100%;"),
                   
                   card_footer(class = "text-center bg-light", tags$b("Harimau Sumatera"))
                 )
               ),
               
               ### UI:Biodiversitas:Card:Gajah ####
               div(
                 id = "card-gajah",
                 style = "cursor:pointer;",
                 card(
                   style = "overflow: hidden;",
                   card_image(
                     src = "Yongki_gajah sumatera_BBTNBBS_Vino Anggoro.JPG",
                     style = "height: 350px; object-fit: cover; width: 100%;"),
                   card_footer(class = "text-center bg-light", tags$b("Gajah Sumatera"))
                 )
               ),
               
               ### UI:Biodiversitas:Card:Rangkong ####
               div(
                 id = "card-rangkong",
                 style = "cursor:pointer;",
                 card(
                   style = "overflow: hidden;",
                   card_image(
                     src = "Rangkong.JPG",
                     style = "height: 350px; object-fit: cover; width: 100%;"),
                   card_footer(class = "text-center bg-light", tags$b("Rangkong"))
                 )
               ),
               
               ### UI:Biodiversitas:Card:Maleo ####
               div(
                 id = "card-maleo",
                 style = "cursor:pointer;",
                 card(
                   style = "overflow: hidden;",
                   card_image(
                     src = "IHU_Maleo_Festival 2015.JPG",
                     style = "height: 350px; object-fit: cover; width: 100%;"),
                   card_footer(class = "text-center bg-light", tags$b("Maleo"))
                 )
               ),
               
               ### UI:Biodiversitas:Card:Anoa-Babirusa ####
               div(
                 id = "card-anoa-babirusa",
                 style = "cursor:pointer;",
                 card(
                   style = "overflow: hidden;",
                   card_image(
                     src = "04_anoa.JPG",
                     style = "height: 350px; object-fit: cover; width: 100%;"),
                   card_footer(class = "text-center bg-light", tags$b("Anoa dan Babi Rusa"))
                 )
               ),
               
               ### UI:Biodiversitas:Card:Kurot ####
               div(
                 id = "card-kurot",
                 style = "cursor:pointer; overflow-y: hidden;",
                 card(
                   style = "overflow: hidden;",
                   card_image(
                     src = "Maslim As-singkily_WCSIP_2.JPG",
                     style = "height: 350px; object-fit: cover; width: 100%;"),
                   card_footer(class = "text-center bg-light", tags$b("Kura-Kura Rote"))
                 )
               ),
               
               ### UI:Biodiversitas:Card:Orang Utan ####
               div(
                 id = "card-orang-utan",
                 style = "cursor:pointer; overflow-y: hidden;",
                 card(
                   style = "overflow: hidden;",
                   card_image(
                     src = "Orangutan sumatra 2.2.JPG",
                     style = "height: 350px; object-fit: cover; width: 100%;"),
                   card_footer(class = "text-center bg-light", tags$b("Orang Utan"))
                 )
               ),
               
               ### UI:Biodiversitas:Card:Siamang - Owa ####
               div(
                 id = "card-siamang-owa",
                 style = "cursor:pointer; overflow-y: hidden;",
                 card(
                   style = "overflow: hidden;",
                   card_image(
                     src = "Siamang.jpeg",
                     style = "height: 350px; object-fit: cover; width: 100%;"),
                   card_footer(class = "text-center bg-light", tags$b("Siamang dan Owa"))
                 )
               )
             ),
             
             ### UI:Biodiversitas:Page Maleo ####
             "page_maleo" = tagList(
               layout_sidebar(
                 border = FALSE,
                 fillable = FALSE,
                 
                 ### UI:Biodiversitas:Page Maleo:Sidebar ####
                 sidebar = tagList(
                   h4("🔍 Filter Data"),
                   div(style = "margin-bottom: 1rem;",
                       dateRangeInput("periode", "📅 Pilih Periode",
                                      start = min(data_telur_maleo$date_collect),
                                      end = max(data_telur_maleo$date_collect))
                   ),
                   div(style = "margin-bottom: 1rem;",
                       selectInput("lokasi", "📍 Pilih Nesting Ground",
                                   choices = unique(data_telur_maleo$ng_name),
                                   selected = NULL)
                   )),
                 
                 layout_columns(
                   fill = FALSE,
                   
                   ### UI:Biodiversitas:Page Maleo:ValueBox:Jumlah Telur ####
                   value_box(
                     title = tags$div("Jumlah Telur", style = "font-weight: bold;"),
                     value = textOutput("totalTelurTextMaleo"),
                     showcase = icon("egg", class = "fa-2x"),  # gunakan class ukuran atau bs_icon()
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border"
                   ),
                   
                   ### UI:Biodiversitas:Page Maleo:ValueBox:Jumlah Telur yang Tidak Dipindahkan ####
                   value_box(
                     title = tags$div("Jumlah Telur yang Tidak Dipindahkan", style = "font-weight: bold;"),
                     value = textOutput("boxTelurGagal"),
                     showcase = icon("egg", class = "fa-2x"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border"
                   ),
                   
                   ### UI:Biodiversitas:Page Maleo:ValueBox:Jumlah Telur Menetas ####
                   value_box(
                     title = tags$div("Jumlah Telur Menetas", style = "font-weight: bold;"),
                     value = textOutput("boxTelurMenetasMaleo"),
                     showcase = icon("egg", class = "fa-2x"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border"
                   )),
                 layout_columns(
                   fill = FALSE,
                   
                   ### UI:Biodiversitas:Page Maleo:ValueBox:Chick Dilepasliarkan ####
                   value_box(
                     title = tags$div("Chick Dilepasliarkan", style = "font-weight: bold;"),
                     value = textOutput("boxChickReleased"),
                     showcase = icon("crow"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border"
                   ),
                   
                   ### UI:Biodiversitas:Page Maleo:ValueBox:Breeding Pair ####
                   value_box(
                     title = tags$div("Breeding Pair", style = "font-weight: bold;"),
                     value = textOutput("breedingPairMaleo"),
                     showcase = icon("crow"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border",
                     tags$div(
                       style = "margin-top: 6px;",
                       
                       tags$div(style = "display: flex; justify-content: flex-start;",
                                tags$b("Mean :"), 
                                tags$span(style = "margin-left: 5px;", 
                                          textOutput("breedingPairMeanMaleo"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Min :"),
                                tags$span(style = "margin-left: 5px;", 
                                          textOutput("breedingPairMinMaleo"))
                       ),
                       
                       tags$div(style = "display: flex;",
                                tags$b("Max :"),
                                tags$span(style = "margin-left: 5px;", 
                                          textOutput("breedingPairMaxMaleo"))
                       )
                     )
                   ),
                   
                   ### UI:Biodiversitas:Page Maleo:ValueBox:Succes Rate ####
                   value_box(
                     title = tags$div("Succes Rate", style = "font-weight: bold;"),
                     value = textOutput("successRateMaleo"),
                     showcase = icon("percent"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border"
                   )
                 ),
                 layout_columns(
                   
                   ### UI:Biodiversitas:Page Maleo:Peta Jumlah Telur per Grid Maleo ####
                   card(
                     height = "500px", 
                     full_screen = TRUE, 
                     card_header("Nesting Ground"), 
                     leafletOutput("mapTelurGridMaleo", height = "450px"))
                 ),
                 layout_columns(
                   
                   ### UI:Biodiversitas:Page Maleo:Chart Telur Maleo per Tahun ####
                   card(
                     height = "500px", 
                     full_screen = TRUE, 
                     card_header("Nesting Ground", class="card-header-custom"), 
                     plotlyOutput("barChartTelurMaleoNestingGround", height = "450px")),
                   
                   ### UI:Biodiversitas:Page Maleo:Chart Telur Berhasil VS Tidak Berhasil Menetas ####
                   card(
                     height = "500px", 
                     full_screen = TRUE, 
                     card_header("Telur", class="card-header-custom"), 
                     plotlyOutput("pieChartMaleoBerhasilTidakBerhasil", height = "450px")),
                   
                   ### UI:Biodiversitas:Page Maleo:Kondisi Telur Tidah Dipindahkan ####
                   card(
                     height = "500px", 
                     full_screen = TRUE, 
                     card_header("Kondisi Telur Tidak Dipindahkan", class="card-header-custom"), 
                     plotlyOutput("plotAlasanTelurMaleoTidakDipindahkan", height = "450px"))
                 ),
                 layout_columns(
                   
                   ### UI:Biodiversitas:Page Maleo:Grafik Telur Maleo Hatchery ####
                   card(
                     height = "500px", 
                     full_screen = TRUE, 
                     card_header("Hatchery", class="card-header-custom"), 
                     plotlyOutput("barChartTelurMaleoHatchery", height = "450px")),
                   
                   ### UI:Biodiversitas:Page Maleo:Hatchery Maleo ####
                   card(
                     height = "500px", 
                     full_screen = TRUE, 
                     card_header("Hatchery", class="card-header-custom"), 
                     plotlyOutput("barChartChickReleasedMaleo", height = "450px"))
                 ),
                 layout_columns(
                   
                   ### UI:Biodiversitas:Page Maleo:Grafik Curah Hujan di Nesting Ground ####
                   card(
                     height = "500px",  
                     full_screen = TRUE, 
                     card_header("Lingkungan", class="card-header-custom"), 
                     plotlyOutput("lineChartHujanMaleo", height = "450px")),
                   
                   ### UI:Biodiversitas:Page Maleo:Gangguan Nesting Ground ####
                   card(
                     height = "500px",  
                     full_screen = TRUE, 
                     card_header("Gangguan", class="card-header-custom"), 
                     plotlyOutput("barGangguanHorizontalMaleo", height = "450px"))
                 )
               )
             ),
             
             # UI:Biodiversitas:Page Kurot ####
             "page_kurot" = tagList(
               layout_sidebar(
                 border = FALSE,
                 fillable = FALSE,
                 
                 ## UI:Biodiversitas:Page Kurot:Sidebar ####
                 sidebar = tagList(
                   h4("🔍 Filter Data"),
                   div(style = "margin-bottom: 1rem;",
                       dateRangeInput("periode", "📅 Pilih Periode",
                                      start = min(data_telur_maleo$date_collect),
                                      end = max(data_telur_maleo$date_collect))
                   ),
                   div(style = "margin-bottom: 1rem;",
                       selectInput("lokasi_kurot", "📍 Pilih Lokasi",
                                   choices = c("IKH", "Danau Ledulu", "Danau Lendeoen", "Danau Peto"),
                                   selected = NULL)
                   )
                 ),
                 
                 ## UI:Biodiversitas:Page Kurot:ValueBox ####
                 layout_columns(
                   fill = FALSE,
                   
                   ### UI:Biodiversitas:Page Kurot:ValueBox:Jumlah Telur ####
                   value_box(
                     title = tags$div("Jumlah Telur", style = "font-weight: bold;"),
                     value = "0",
                     showcase = icon("egg", class = "fa-2x"),  # gunakan class ukuran atau bs_icon()
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border"
                   ),
                   
                   ### UI:Biodiversitas:Page Kurot:ValueBox:Jumlah Tukik ####
                   value_box(
                     title = tags$div("Jumlah Tukik", style = "font-weight: bold;"),
                     value = "0",
                     showcase = icon("egg", class = "fa-2x"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border"
                   ),
                   
                   ### UI:Biodiversitas:Page Kurot:ValueBox:Jumlah Tukik Dewasa ####
                   value_box(
                     title = tags$div("Jumlah Kurot Dewasa", style = "font-weight: bold;"),
                     value = "0",
                     showcase = icon("egg", class = "fa-2x"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border"
                   ),
                   
                   ### UI:Biodiversitas:Page Kurot:ValueBox:Survival Rate ####
                   value_box(
                     title = tags$div("Survival Rate", style = "font-weight: bold;"),
                     value = "0",
                     showcase = icon("crow"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border"
                   ),
                   
                   ### UI:Biodiversitas:Page Kurot:ValueBox:Succes Rate ####
                   value_box(
                     title = tags$div("Succes Rate", style = "font-weight: bold;"),
                     value = "$5,000",
                     showcase = icon("percent"),
                     theme = value_box_theme(bg = "#436d46", fg = "#fff"),
                     class = "border"
                   )),
                 
                 ## UI:Biodiversitas:Page Kurot:Map ####
                 layout_columns(
                   card(
                     height = "500px", 
                     full_screen = TRUE, 
                     card_header("Nesting Ground"), 
                     leafletOutput("peta_kurot", height = "450px"))
                 ),
                 ## UI:Biodiversitas:Page Kurot:Grafik ###
                 layout_columns(
                   card(height = "500px", full_screen = TRUE, card_header("Nesting Ground"), plotlyOutput("barChartTelurkurot", height = "450px")),
                   card(height = "500px", full_screen = TRUE, card_header("Telur", class="card-header-custom"), plotlyOutput("pieChartkurot", height = "450px")),
                   card(height = "500px", full_screen = TRUE, card_header("Kondisi Telur Tidak Dipindahkan", class="card-header-custom"), plotlyOutput("plotAlasankurot", height = "450px"))
                 )
               )
             ),
             
             # UI:Biodiversitas:Page Harimau ####
             "page_harimau" = tagList(
               layout_sidebar(
                 border = FALSE,
                 fillable = FALSE,
                 
                 sidebar = tagList(
                   h4("🔍 Filter Data"),
                   
                   div(style = "margin-bottom: 1rem;",
                       selectInput("pilih_landscape", "📍 Pilih Landscape",
                                   choices = c("Leuser", "BBS"),
                                   selected = NULL)
                   ),
                   div(style = "margin-bottom: 1rem;",
                       dateRangeInput("periode", "📅 Pilih Periode",
                                      start = min(data_telur_maleo$date_collect),
                                      end = max(data_telur_maleo$date_collect))
                   )
                 ),
                 p("Ini adalah konten spesifik untuk Harimau Sumatera.")
               )),
             
             # UI:Biodiversitas:Page Gajah ####
             "page_gajah" = tagList(
               layout_sidebar(
                 border = FALSE,
                 fillable = FALSE,
                 
                 sidebar = tagList(
                   h4("🔍 Filter Data"),
                   
                   div(style = "margin-bottom: 1rem;",
                       selectInput("pilih_landscape", "📍 Pilih Landscape",
                                   choices = c("Leuser", "BBS", "Way Kambas"),
                                   selected = NULL)
                   ),
                   div(style = "margin-bottom: 1rem;",
                       dateRangeInput("periode", "📅 Pilih Periode",
                                      start = min(data_telur_maleo$date_collect),
                                      end = max(data_telur_maleo$date_collect))
                   )
                 ),
                 p("Ini adalah konten spesifik untuk Gajah Sumatera.")
               )),
             
             # UI:Biodiversitas:Page Siamang - Owa ####
             "page_siamang_owa" = tagList(
               layout_sidebar(
                 border = FALSE,
                 fillable = FALSE,
                 
                 sidebar = tagList(
                   h4("🔍 Filter Data"),
                   
                   div(style = "margin-bottom: 1rem;",
                       selectInput("pilih_landscape", "📍 Pilih Landscape",
                                   choices = c("Leuser", "BBS", "Way Kambas"),
                                   selected = NULL)
                   ),
                   div(style = "margin-bottom: 1rem;",
                       dateRangeInput("periode", "📅 Pilih Periode",
                                      start = min(data_telur_maleo$date_collect),
                                      end = max(data_telur_maleo$date_collect))
                   )
                 ),
                 p("Ini adalah konten spesifik untuk Siamang.")
               )),
             
             # UI:Biodiversitas:Page Rangkong ####
             "page_rangkong" = tagList(
               layout_sidebar(
                 border = FALSE,
                 fillable = FALSE,
                 
                 sidebar = tagList(
                   h4("🔍 Filter Data"),
                   
                   div(style = "margin-bottom: 1rem;",
                       selectInput("pilih_landscape", "📍 Pilih Landscape",
                                   choices = c("Leuser", "BBS", "Way Kambas"),
                                   selected = NULL)
                   ),
                   div(style = "margin-bottom: 1rem;",
                       dateRangeInput("periode", "📅 Pilih Periode",
                                      start = min(data_telur_maleo$date_collect),
                                      end = max(data_telur_maleo$date_collect))
                   )
                 ),
                 p("Ini adalah konten spesifik untuk Rangkong.")
               ))
      )}
    
  })
  
  # Server:Biodiversitas:Tab Maleo ####
  ## Server:Biodiversitas:Tab Maleo:Load Data Telur ####
  data_FilteredMaleo <- reactive({
    data_telur_maleo %>%
      filter(
        ng_name == input$lokasi,
        date_collect >= input$periode[1],
        date_collect <= input$periode[2]
      )
  })
  
  ## Server:Biodiversitas:Tab Maleo: Set Periode ####
  is_lintas_tahun <- reactive({
    start_year <- format(input$periode[1], "%Y")
    end_year <- format(input$periode[2], "%Y")
    start_year != end_year
  })
  
  # output$sparkJumlahTelur <- renderPlotly({
  #   df <- data_FilteredMaleo()
  #   
  #   df_summary <- df %>%
  #     group_by(date_collect) %>%
  #     summarise(jumlah_telur = n_distinct(egg_id))
  #   
  #   plot_ly(df_summary) %>%
  #     add_lines(
  #       x = ~date_collect, y = ~jumlah_telur,
  #       color = I("white"), span = I(1),
  #       fill = 'tozeroy', alpha = 0.2
  #     ) %>%
  #     layout(
  #       xaxis = list(visible = F, showgrid = F, title = ""),
  #       yaxis = list(visible = F, showgrid = F, title = ""),
  #       hovermode = "x",
  #       margin = list(t = 0, r = 0, l = 0, b = 0),
  #       font = list(color = "white"),
  #       paper_bgcolor = "transparent",
  #       plot_bgcolor = "transparent"
  #     ) %>%
  #     config(displayModeBar = F) %>%
  #     htmlwidgets::onRender(
  #       "function(el) {
  #         el.closest('.bslib-value-box')
  #           .addEventListener('bslib.card', function(ev) {
  #             Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
  #           })
  #       }"
  #     )
  # })
  
  # output$sparkMenetas <- renderPlotly({
  #   df <- data_FilteredMaleo()
  #   
  #   df_summary <- df %>%
  #     filter(hatch == 1) %>%
  #     group_by(date_collect) %>%
  #     summarise(jumlah_menetas = n(), .groups = "drop")
  #   
  #   plot_ly(df_summary) %>%
  #     add_lines(
  #       x = ~date_collect,
  #       y = ~jumlah_menetas,
  #       line = list(color = "white"),
  #       fill = 'tozeroy',
  #       hoverinfo = "x+y"
  #     ) %>%
  #     layout(
  #       xaxis = list(visible = FALSE),
  #       yaxis = list(visible = FALSE),
  #       margin = list(t = 0, r = 0, b = 0, l = 0),
  #       paper_bgcolor = "transparent",
  #       plot_bgcolor = "transparent"
  #     ) %>%
  #     config(displayModeBar = FALSE) %>%
  #     htmlwidgets::onRender(
  #       "function(el) {
  #         el.closest('.bslib-value-box')
  #           .addEventListener('bslib.card', function(ev) {
  #             Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
  #           })
  #       }"
  #     )
  # })
  
  # output$sparkTelurGagal <- renderPlotly({
  #   alasan_data <- data_FilteredMaleo() %>%
  #     filter(collect == 0) %>%
  #     group_by(cond) %>%
  #     summarise(Jumlah = n()) %>%
  #     filter(!is.na(cond), cond != "")
  #   
  #   if (nrow(alasan_data) == 0) return(NULL)
  #   
  #   plot_ly(
  #     data = alasan_data,
  #     x = ~Jumlah,
  #     y = ~reorder(cond, Jumlah),
  #     type = "bar",
  #     orientation = "h",
  #     color = ~cond,                  # 🟢 Tambah ini
  #     colors = "Set2",                # 🟢 Palet warna (bisa ganti: "Dark2", "Set3", dll)
  #     hoverinfo = "x+y"
  #   ) %>%
  #     layout(
  #       xaxis = list(visible = FALSE),
  #       yaxis = list(visible = FALSE),
  #       margin = list(t = 0, r = 0, b = 0, l = 0),
  #       paper_bgcolor = "transparent",
  #       plot_bgcolor = "transparent",
  #       showlegend = FALSE            # 🔇 Sembunyikan legenda jika spark style
  #     ) %>%
  #     config(displayModeBar = FALSE)
  # })
  
  
  ## Server:Biodiversitas:Tab Maleo:ValueBox####
  ### Server:Biodiversitas:Tab Maleo:ValueBox: Telur Maleo Menetas ####
  output$boxTelurMenetasMaleo <- renderText({
    df <- data_FilteredMaleo()
    sum(df$hatch == 1, na.rm = TRUE)
  })
  
  ### Server:Biodiversitas:Tab Maleo:ValueBox: Jumlah Telur Maleo ####
  output$totalTelurTextMaleo <- renderText({
    df <- data_FilteredMaleo()
    formatC(n_distinct(df$egg_id), format = "d", big.mark = ",")
  })
  
  ### Server:Biodiversitas:Tab Maleo:ValueBox: Telur Tidak Dipindahkan ####
  output$boxTelurGagal <- renderText({
    df <- data_FilteredMaleo()
    sum(df$collect == 0, na.rm = TRUE)
  })
  
  
  ### Server:Biodiversitas:Tab Maleo:ValueBox: Data Chick Dilepasliarkan ####
  output$boxChickReleased <- renderText({
    df <- data_FilteredMaleo() %>%
      filter(hatch == 1, notes == "Released")
    
    formatC(nrow(df), format = "d", big.mark = ",")
  })
  
  ### Server:Biodiversitas:Tab Maleo:Load Data: Data BreedingPair ####
  breeding_pair_data <- reactive({
    req(data_FilteredMaleo())
    data <- data_FilteredMaleo() %>%
      filter(collect == 1)  # hanya telur yang dipindahkan
    
    data_bulanan <- data %>%
      mutate(
        tahun = lubridate::year(date_collect),
        bulan = lubridate::month(date_collect)
      ) %>%
      group_by(ng_name, tahun, bulan) %>%
      summarise(
        jumlah_telur = n(),
        total_hari = lubridate::days_in_month(first(date_collect)),
        rata_rata_per_hari = round(jumlah_telur / total_hari, 2),
        .groups = "drop"
      )
    
    data_tahunan <- data_bulanan %>%
      group_by(ng_name, tahun) %>%
      summarise(
        jumlah_telur_per_tahun = sum(jumlah_telur, na.rm = TRUE),
        total_hari_per_tahun = sum(total_hari, na.rm = TRUE),
        rata_rata_telur = jumlah_telur_per_tahun / total_hari_per_tahun,
        estimasi_telur_per_tahun = round(rata_rata_telur * 365, 0),
        rata_rata_telur = round(rata_rata_telur, 2),
        mean_pasangan = round(estimasi_telur_per_tahun / 10, 0),
        min_pasangan = round(estimasi_telur_per_tahun / 12, 0),
        max_pasangan = round(estimasi_telur_per_tahun / 8, 0),
        .groups = "drop"
      )
    
    data_tahunan
  })
  
  ### Server:Biodiversitas:Tab Maleo:ValueBox: Data Breeding Pair ####
  output$breedingPairMaleo <- renderText({
    df <- breeding_pair_data()
    
    # Jika kosong
    if (nrow(df) == 0) return("0")
    
    # Ambil data terbaru berdasarkan tahun
    df <- df %>% arrange(desc(tahun))
    mean_pair <- df$mean_pasangan[1]
    
    format(mean_pair, big.mark = ",")
  })
  
  ### Server:Biodiversitas:Tab Maleo:ValueBox: Text Breeding Pair Mean ####
  output$breedingPairMeanMaleo <- renderText({
    df <- breeding_pair_data()
    if (nrow(df) == 0) return("0")
    format(df$mean_pasangan[1], big.mark = ",")
  })
  
  ### Server:Biodiversitas:Tab Maleo:ValueBox: Text Breeding Pair Min ####
  output$breedingPairMinMaleo <- renderText({
    df <- breeding_pair_data()
    if (nrow(df) == 0) return("0")
    format(df$min_pasangan[1], big.mark = ",")
  })
  
  ### Server:Biodiversitas:Tab Maleo:ValueBox: Text Breeding Pair Max ####
  output$breedingPairMaxMaleo <- renderText({
    df <- breeding_pair_data()
    if (nrow(df) == 0) return("0")
    format(df$max_pasangan[1], big.mark = ",")
  })
  
  ### Server:Biodiversitas:Tab Maleo:ValueBox: Success Rate ####
  output$successRateMaleo <- renderText({
    df <- data_FilteredMaleo()
    
    total_chick <- sum(df$hatch == 1, na.rm = TRUE)
    total_telur <- nrow(df)
    
    if (total_telur == 0) return("0%")
    
    success_rate <- round((total_chick / total_telur) * 100, 1)
    paste0(success_rate, "%")
  })
  
  ## Server:Biodiversitas:Tab Maleo:Grafik ----
  ### Server:Biodiversitas:Tab Maleo:Grafik: Perbandingan Jumlah Telur Dipindahkan dan Jumlah Telur Tidak Dipindahkan ####
  output$pieChartMaleoBerhasilTidakBerhasil <- renderPlotly({
    summary_data <- data_FilteredMaleo() %>%
      summarise(
        `Telur Dipindahkan` = sum(collect == 1),
        `Telur Tidak Dipindahkan` = sum(collect == 0)
      ) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Status", values_to = "Jumlah")
    
    # Tentukan warna secara manual (urutan harus sama dengan levels di summary_data$Status)
    warna_manual <- c("Telur Dipindahkan" = "#1B4332",   # hijau tua
                      "Telur Tidak Dipindahkan" = "#9B2226")  # hijau muda
    
    # Pastikan urutan warna sesuai urutan label
    warna_digunakan <- warna_manual[summary_data$Status]
    
    plot_ly(
      data = summary_data,
      labels = ~Status,
      values = ~Jumlah,
      type = "pie",
      textposition = 'inside',
      textinfo='label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      marker = list(colors = warna_digunakan)
    ) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        margin = list(t = 0, r = 0, b = 0, l = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        showlegend = TRUE,
        legend = list(
          orientation = "h",    # horizontal
          x = 0.5,              # tengah horizontal
          y = -0.2,             # di bawah chart
          xanchor = "center"
        ))
  })
  
  ### Server:Biodiversitas:Tab Maleo:Grafik: Alasan Telur Tidak Dipindahkan ####
  output$plotAlasanTelurMaleoTidakDipindahkan <- renderPlotly({
    alasan_data <- data_FilteredMaleo() %>%
      filter(collect == 0) %>%
      group_by(cond) %>%
      summarise(Jumlah = n()) %>%
      filter(!is.na(cond), cond != "") %>%
      arrange(Jumlah)
    
    plot_ly(
      data = alasan_data,
      x = ~Jumlah,
      y = ~reorder(cond, Jumlah),
      type = "bar",
      orientation = "h",
      marker = list(color = "tomato"),
      text = ~cond,
      textposition = "outside"
    ) %>%
      layout(
        yaxis = list(title = "Alasan", showticklabels = FALSE),  # ⛔ jangan tampilkan label lagi
        xaxis = list(title = "Jumlah"),
        margin = list(l = 50, r = 100),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = FALSE)
    
  })
  
  ### Server:Biodiversitas:Tab Maleo:Grafik: Jumlah Telur per Hatchery ####
  output$barChartTelurMaleoHatchery <- renderPlotly({
    df <- data_FilteredMaleo()
    if (nrow(df) == 0) return(NULL)
    
    start_year <- lubridate::year(input$periode[1])
    end_year   <- lubridate::year(input$periode[2])
    is_lintas_tahun <- start_year != end_year
    
    # Siapkan data
    bar_data <- df %>%
      filter(!is.na(h), !is.na(hatch)) %>%
      mutate(
        status_menetas = if_else(hatch == 1, "Berhasil Menetas", "Gagal Menetas"),
        periode_label = if (is_lintas_tahun) as.character(lubridate::year(date_collect)) else format(date_collect, "%b")
      ) %>%
      group_by(h, periode_label, status_menetas) %>%
      summarise(jumlah = n(), .groups = "drop")
    
    # Urutkan bulan jika hanya 1 tahun
    if (!is_lintas_tahun) {
      bar_data$periode_label <- factor(bar_data$periode_label, levels = month.abb)
    }
    
    hatchery_list <- unique(bar_data$h)
    traces <- list()
    
    # Tambahkan trace per hatchery per status (Berhasil dan Gagal)
    for (h in hatchery_list) {
      for (status in c("Berhasil Menetas", "Gagal Menetas")) {
        subset_data <- bar_data %>% filter(h == !!h, status_menetas == !!status)
        
        traces[[length(traces) + 1]] <- list(
          data = subset_data,
          visible = if (h == hatchery_list[1]) TRUE else FALSE,  # hanya hatchery pertama yg tampil awal
          name = status,
          legendgroup = status,
          showlegend = if (h == hatchery_list[1]) TRUE else FALSE,
          marker = list(color = if (status == "Berhasil Menetas") "forestgreen" else "firebrick")
        )
      }
    }
    
    # Mulai dengan plot kosong
    p <- plot_ly()
    
    # Tambahkan semua trace
    for (tr in traces) {
      p <- add_trace(
        p,
        data = tr$data,
        x = ~periode_label,
        y = ~jumlah,
        type = "bar",
        name = tr$name,
        marker = tr$marker,
        legendgroup = tr$legendgroup,
        showlegend = tr$showlegend,
        visible = tr$visible
      )
    }
    
    # Dropdown menu untuk memilih hatchery
    updatemenus <- list(
      list(
        buttons = lapply(seq_along(hatchery_list), function(i) {
          vis_flags <- rep(FALSE, length(traces))
          vis_flags[c((2*i-1), (2*i))] <- TRUE  # 2 trace per hatchery: sukses & gagal
          
          list(
            method = "restyle",
            args = list("visible", vis_flags),
            label = hatchery_list[i]
          )
        }),
        direction = "right",
        x = 0.1, y = 1.15,
        showactive = TRUE
      )
    )
    
    # Layout akhir
    p %>% layout(
      barmode = "group",
      xaxis = list(title = if (is_lintas_tahun) "Tahun" else "Bulan"),
      yaxis = list(title = "Jumlah Telur"),
      legend = list(title = list(text = "<b>Status</b>")),
      updatemenus = updatemenus,
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    )
  })
  
  ### Server:Biodiversitas:Tab Maleo:Grafik: Jumlah Telur per Nesting Ground ####
  output$barChartTelurMaleoNestingGround <- renderPlotly({
    df <- data_FilteredMaleo()  # Sudah sesuai nesting ground & periode
    
    if (nrow(df) == 0) return(NULL)
    
    bar_data <- if (is_lintas_tahun()) {
      df %>%
        mutate(tahun = format(date_collect, "%Y")) %>%
        group_by(tahun) %>%
        summarise(jumlah_telur = n()) %>%
        rename(kategori = tahun)
    } else {
      df %>%
        mutate(bulan = format(date_collect, "%Y-%m")) %>%
        group_by(bulan) %>%
        summarise(jumlah_telur = n()) %>%
        rename(kategori = bulan)
    }
    
    plot_ly(
      data = bar_data,
      x = ~kategori,
      y = ~jumlah_telur,
      type = 'bar',
      marker = list(color = 'darkorange')
    ) %>%
      layout(
        xaxis = list(title = ifelse(is_lintas_tahun(), "Tahun", "Bulan")),
        yaxis = list(title = "Jumlah Telur"),
        margin = list(t = 0, r = 0, b = 0, l = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      )
  })
  
  ### Server:Biodiversitas:Tab Maleo:Grafik: Jumlah Chick Dilepasliarkan ####
  output$barChartChickReleasedMaleo <- renderPlotly({
    df <- data_FilteredMaleo() %>%
      filter(hatch == 1, notes == "Released")  # Hanya yang dilepasliarkan
    
    if (nrow(df) == 0) return(NULL)
    
    # Cek lintas tahun
    is_lintas <- lubridate::year(input$periode[1]) != lubridate::year(input$periode[2])
    
    # Agregasi data sesuai periode
    chick_data <- if (is_lintas) {
      df %>%
        mutate(tahun = format(date_collect, "%Y")) %>%
        count(tahun, name = "jumlah") %>%
        rename(kategori = tahun)
    } else {
      df %>%
        mutate(bulan = format(date_collect, "%b")) %>%
        count(bulan, name = "jumlah") %>%
        rename(kategori = bulan) %>%
        mutate(kategori = factor(kategori, levels = month.abb))
    }
    
    # Buat grafik
    plot_ly(
      data = chick_data,
      x = ~kategori,
      y = ~jumlah,
      type = "bar",
      marker = list(color = "seagreen")
    ) %>%
      layout(
        title = paste("Chick Dilepasliarkan -", input$lokasi),
        xaxis = list(title = if (is_lintas) "Tahun" else "Bulan"),
        yaxis = list(title = "Jumlah Chick"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent",
        margin = list(t = 30, r = 10, b = 40, l = 40)
      )
  })
  
  
  ## Server:Biodiversitas:Tab Maleo:Load Data: telur per grid ####
  filtered_grid_maleo <- reactive({
    data_telur_per_grid %>%
      filter(
        ng_name == input$lokasi,
        date_collect >= input$periode[1],
        date_collect <= input$periode[2]
      ) %>%
      group_by(field_grid, ng_name) %>%
      summarise(
        total_telur = sum(total_telur, na.rm = TRUE),
        geometry = st_union(st_as_sfc(geom)),
        .groups = "drop"
      ) %>%
      st_as_sf(sf_column_name = "geometry", crs = 4326)
  })
  
  ### Server:Biodiversitas:Tab Maleo:Map: telur per grid ####
  output$mapTelurGridMaleo <- renderLeaflet({
    df <- filtered_grid_maleo()
    if (nrow(df) == 0 || all(is.na(df$total_telur))) {
      return(leaflet() %>% addTiles())
    }
    
    # Buat kolom kategori manual
    df <- df %>%
      mutate(
        kategori_telur = case_when(
          total_telur <= 50 ~ "Rendah",
          total_telur <= 150 ~ "Sedang",
          total_telur > 150 ~ "Tinggi",
          TRUE ~ NA_character_
        )
      )
    
    # Tetapkan warna per kategori
    kategori_levels <- c("Rendah", "Sedang", "Tinggi")
    pal <- colorFactor(
      palette = c("lightyellow", "orange", "darkred"),
      domain = kategori_levels
    )
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(kategori_telur),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>Grid:</b> ", field_grid, "<br>",
          "<b>Total Telur:</b> ", total_telur, "<br>",
          "<b>Kategori:</b> ", kategori_telur
        )
      ) %>%
      addLegend(
        pal = pal,
        values = df$kategori_telur,
        title = "Kategori Jumlah Telur",
        position = "bottomright",
        opacity = 1
      )
  })
  
  ## Server:Biodiversitas:Tab Maleo:Load Data: curah hujan ####
  filtered_temp_rain_maleo <- reactive({
    data_temp_rain %>%
      filter(
        ng_name == input$lokasi,
        date >= input$periode[1],
        date <= input$periode[2]
      )
  })
  
  ### Server:Biodiversitas:Tab Maleo:Grafik: Curah Hujan ####
  output$lineChartHujanMaleo <- renderPlotly({
    df <- filtered_temp_rain_maleo()
    
    if (nrow(df) == 0) return(NULL)
    
    plot_ly(
      data = df,
      x = ~date,
      y = ~rainfall,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'deepskyblue')
    ) %>%
      layout(
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"),
              list(
                count = 1,
                label = "YTD",
                step = "year",
                stepmode = "todate"),
              list(step = "all"))),
          
          rangeslider = list(type = "date")),
        yaxis = list(title = "Curah Hujan (mm)"),
        margin = list(t = 0, r = 0, b = 0, l = 0),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      )
  })
  
  ## Server:Biodiversitas:Tab Maleo:Load Data: gangguan per nesting ground ####
  filtered_threat_maleo <- reactive({
    data_gangguan_maleo %>%
      filter(
        ng_name == input$lokasi,
        date >= input$periode[1],
        date <= input$periode[2]
      )
  })
  
  ### Server:Biodiversitas:Tab Maleo:Load Data:Summary Data Gangguan per Kategori ####
  gangguan_summary_maleo <- reactive({
    df <- filtered_threat_maleo()
    
    if (nrow(df) == 0) return(NULL)
    
    df %>%
      select(date, ng_name, biawak:sapi) %>%  # Ambil kolom gangguan
      pivot_longer(cols = -c(date, ng_name), names_to = "jenis_gangguan", values_to = "kejadian") %>%
      filter(kejadian == 1) %>%
      count(jenis_gangguan, name = "jumlah") %>%
      arrange(jumlah)
  })
  
  
  ### Server:Biodiversitas:Tab Maleo:Grafik: Gangguan dalam Nesting Ground ####
  output$barGangguanHorizontalMaleo <- renderPlotly({
    df <- gangguan_summary_maleo()
    if (is.null(df)) return(NULL)
    
    plot_ly(
      data = df,
      x = ~jumlah,
      y = ~reorder(jenis_gangguan, jumlah),
      type = 'bar',
      orientation = 'h',
      marker = list(color = 'indianred'),
      text = ~jenis_gangguan,
      textposition = "outside"
    ) %>%
      layout(
        xaxis = list(title = "Jumlah Kejadian"),
        yaxis = list(title = "Jenis Gangguan", showticklabels = FALSE),  # ⛔ sembunyikan label Y
        margin = list(t = 20, r = 100, b = 20, l = 50),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      )
  })
  
  # Server: Tab Beneficiaries ####
  
  ## Server:Beneficiaries:Tab Peserta:Status tampilan "tabel" atau "card" ####
  mode <- reactiveVal("tabel")
  peserta_terpilih <- reactiveVal(NULL)
  
  ## Server:Beneficiaries: Tab Kegiatan:Sidebar: Filter tab kegiatan ####
  filtered_beneficiaries <- reactive({
    req(input$filter_periode_beneficiaries)
    df <- data_kegiatan %>%
      filter(
        is.null(input$filter_landscape_beneficiaries) | landscape %in% input$filter_landscape_beneficiaries,
        is.null(input$filter_jenis_beneficiaries) | jenis_kegiatan %in% input$filter_jenis_beneficiaries,
        is.null(input$filter_unit_beneficiaries) | sapply(program_unit_terlibat, function(u) {
          any(grepl(paste(input$filter_unit_beneficiaries, collapse = "|"), u))
        }),
        tanggal_mulai_kegiatan >= input$filter_periode_beneficiaries[1],
        tanggal_akhir_kegiatan <= input$filter_periode_beneficiaries[2]
      )
    df
  })
  
  ## Server:Beneficiaries: Tab Kegiatan:Tabel: Tab Kegiatan ####
  output$tabel_beneficiaries <- renderReactable({
    reactable(filtered_beneficiaries(),
              defaultColDef = colDef(
                header = function(value) {
                  # Ganti "_" dengan spasi dan ubah ke Title Case (kapital di awal setiap kata)
                  value <- gsub("_", " ", value, fixed = TRUE)
                  s <- strsplit(value, " ")[[1]]
                  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
                },
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 100,
                headerStyle = list(background = "#e0f8e3")
              ),
              columns = list(
                keterangan = colDef(minWidth = 140, align = "left"),
                judul_kegiatan = colDef(minWidth = 140, align = "left"), # overrides the default
                program_unit_terlibat = colDef(name = "Program/Unit"),
                program_unit_terlibat = colDef(name = "Kabupaten/Kota")
              ),
              bordered = TRUE,
              highlight = TRUE,
              searchable = TRUE,
              showPageSizeOptions = TRUE,
              defaultPageSize = 5,
              paginationType = "jump")
  })
  
  ## Server:Beneficiaries: Tab Peserta: Sidebar: filter tab peserta ####
  filtered_peserta_beneficiaries <- reactive({
    data_peserta %>%
      filter(
        nomor_handphone %in% (
          data_peserta_kegiatan %>%
            filter(
              is.null(input$filter_landscape_peserta_beneficiaries) | landscape %in% input$filter_landscape_peserta_beneficiaries
            ) %>%
            pull(nomor_handphone)
        ),
        is.null(input$filter_gender_beneficiaries) | jenis_kelamin %in% input$filter_gender_beneficiaries,
        is.null(input$filter_asn_beneficiaries) | asn_nonasn %in% input$filter_asn_beneficiaries,
        is.null(input$filter_instansi_beneficiaries) | asal_instansi %in% input$filter_instansi_beneficiaries
      )
  })
  
  ## Server:Beneficiaries:Tab Peserta:Tabel: Tab Peserta ####
  output$konten_ui_beneficiaries <- renderUI({
    if (mode() == "tabel") {
      reactableOutput("tabel_peserta_beneficiaries")
    } else {
      row <- peserta_terpilih()
      kegiatan <- data_peserta_kegiatan[data_peserta_kegiatan$nomor_handphone == row$nomor_handphone, ]
      
      tagList(
        div(class = "profile-card",
            div(class = "profile-header",
                div(class = "profile-header-left",
                    tags$img(src = "user_foto.jpeg", class = "profile-photo")),
                div(class = "profile-name", row$nama_peserta),
                div(HTML("<span style='font-size: 20px;'>&#43;</span> 
                       &nbsp; 
                       <span>&#9993;</span> 
                       &nbsp; 
                       <span>&#9742;</span>"))
            ),
            
            div(class = "profile-grid",
                div(HTML("<div class='label'>Jenis Kelamin</div>",row$jenis_kelamin)),
                div(HTML("<div class='label'>ASN/Non-ASN</div>",row$asn_nonasn)),
                div(HTML("<div class='label'>Asal Instansi</div>",row$asal_instansi)),
                div(HTML("<div class='label'>Posisi/Jabatan</div>", row$posisi_jabatan))
            )),
        if (nrow(kegiatan) > 0) {
          reactable(kegiatan[, c("jenis_kegiatan","judul_kegiatan", "tanggal_mulai_kegiatan", "tanggal_akhir_kegiatan","tempat_kegiatan")],
                    defaultColDef = colDef(
                      header = function(value) {
                        # Ganti "_" dengan spasi dan ubah ke Title Case (kapital di awal setiap kata)
                        value <- gsub("_", " ", value, fixed = TRUE)
                        s <- strsplit(value, " ")[[1]]
                        paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
                      },
                      cell = function(value) format(value, nsmall = 1),
                      align = "center",
                      minWidth = 100,
                      headerStyle = list(background = "#e0f8e3")
                    ),
                    defaultPageSize = 15,
                    bordered = TRUE,
                    highlight = TRUE,
                    showPageSizeOptions = TRUE,
                    paginationType = "jump")
        } else {
          p("Tidak ada kegiatan tercatat untuk peserta ini.")
        },
        br(),
        tags$div(
          class = "get-started-button",
          onclick = "Shiny.setInputValue('kembali', Math.random())",
          tags$span("Kembali", class = "get-started-text"),
          tags$div(
            class = "arrow-container",
            tags$span("\u2190", class = "arrow-icon first"),
            tags$span("\u2190", class = "arrow-icon second")
          )
        )
      )
    }
  })
  
  ## Server:Beneficiaries:Tab Peserta:Tabel:Tabel Peserta ####
  output$tabel_peserta_beneficiaries <- renderReactable({
    reactable(
      filtered_peserta_beneficiaries(),
      selection = "single",
      highlight = TRUE,
      onClick = "select",
      defaultColDef = colDef(
        header = function(value) {
          # Ganti "_" dengan spasi dan ubah ke Title Case (kapital di awal setiap kata)
          value <- gsub("_", " ", value, fixed = TRUE)
          s <- strsplit(value, " ")[[1]]
          paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
        },
        cell = function(value) format(value, nsmall = 1),
        align = "center",
        minWidth = 100,
        headerStyle = list(background = "#e0f8e3")
      ),
      columns = list(
        nomor_handphone = colDef(show = FALSE),
        landscape = colDef(show = FALSE),
        asn_nonasn = colDef(name = "ASN/Non-ASN"),
        posisi_jabatan = colDef(name = "Posisi/Jabatan")
      ),
      defaultPageSize = 15,
      bordered = TRUE,
      searchable = TRUE,
      showPageSizeOptions = TRUE,
      paginationType = "jump"
    )
  })
  
  ### Server:Beneficiaries:Tab Peserta:Tabel: klik row di tabel peserta ####
  observeEvent(input$tabel_peserta__reactable__selected, {
    idx <- input$tabel_peserta__reactable__selected
    if (length(idx) > 0) {
      peserta_terpilih_beneficiaries(filtered_peserta_beneficiaries()[idx, ])
      mode("card")
    }
  })
  
  ### Server:Beneficiaries:Tab Peserta:Tabel: klik button "kembali" di tab peserta ####
  observeEvent(input$kembali, {
    mode("tabel")
  })
  
  # Server:Petani ####
  
  ## Server:Petani:Dashboard:ValueBox: Total Data Petani ####
  output$totalPetani <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(keaktifan == TRUE) %>%
      summarise(jml = n_distinct(id_petani)) %>%
      pull(jml)
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: BBS ####
  output$totalPetaniBBS <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(landscape == "BBS", keaktifan == TRUE) %>%
      distinct(id_petani) %>%
      nrow()
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Singkil ####
  output$totalPetaniSingkil <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(landscape == "Singkil", keaktifan == TRUE) %>%
      nrow()
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Sulut ####
  output$totalPetaniSulut <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(landscape == "Sulut", keaktifan == TRUE) %>%
      nrow()
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ## Server:Petani:Dashboard:ValueBox: Total Petani Dampingan ####
  output$totalPetaniDampingan <- renderText({
    jumlah_petaniDampinganbbs <- data_summary_total_petani %>%
      filter(landscape == "BBS", grepl("SL", id_petani), keaktifan == TRUE) %>%
      distinct(id_petani) %>%
      nrow()
    
    jumlah_petaniDampingansingkil <- data_summary_total_petani %>%
      filter(landscape == "Singkil") %>%
      distinct(id_petani) %>%
      nrow()
    
    jumlah_petaniDampingansulut <- data_summary_total_petani %>%
      filter(landscape == "Sulut") %>%
      distinct(id_petani) %>%
      nrow()
    
    total <- jumlah_petaniDampinganbbs + jumlah_petaniDampingansingkil + jumlah_petaniDampingansulut
    
    formatC(total, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: BBS ####
  output$totalPetaniDampinganBBS <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(landscape == "BBS", grepl("SL", id_petani), keaktifan == TRUE) %>%
      distinct(id_petani) %>%
      nrow()
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Singkil ####
  output$totalPetaniDampinganSingkil <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(landscape == "Singkil", keaktifan == TRUE) %>%
      nrow()
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Sulut ####
  output$totalPetaniDampinganSulut <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(landscape == "Sulut", keaktifan == TRUE) %>%
      nrow()
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ## Server:Petani:Dashboard:ValueBox:Total Lahan Petani Terpetakan ####
  output$totalLahanPetaniTerpetakan <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(terpetakan == TRUE) %>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: BBS ####
  output$totalPetaniTerpetakanBBS <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(landscape == "BBS", terpetakan == TRUE) %>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Singkil ####
  output$totalPetaniTerpetakanSingkil <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(landscape == "Singkil", terpetakan == TRUE) %>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Sulut ####
  output$totalPetaniTerpetakanSulut <- renderText({
    jumlah <- data_summary_total_petani %>%
      filter(landscape == "Sulut", terpetakan == TRUE) %>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ## Server:Petani:Dashboard:ValueBox: Total Kegiatan Pelatihan GAP ####
  output$totalKegiatanTrainingGAP <- renderText({
    jumlah <- data_kegiatan_training_gap %>%
      filter(
        (landscape == "BBS" & jenis_training %in% c("GAP", "Penguatan")) |
          (landscape == "Singkil" & jenis_training == "GAP") |
          (landscape == "Sulut" & jenis_training == "GAP")
      ) %>%
      distinct(id_kegiatan_training) %>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: BBS ####
  output$kegiatanTrainingBBS <- renderText({
    jumlah <- data_kegiatan_training_gap %>%
      filter(
        (landscape == "BBS" & jenis_training %in% c("GAP", "Penguatan"))
      ) %>%
      distinct(id_kegiatan_training) %>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Singkil ####
  output$kegiatanTrainingSingkil <- renderText({
    jumlah <- data_kegiatan_training_gap %>%
      filter(
        (landscape == "Singkil" & jenis_training == "GAP")
      ) %>%
      distinct(id_kegiatan_training) %>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Sulut ####
  output$kegiatanTrainingSulut <- renderText({
    jumlah <- data_kegiatan_training_gap %>%
      filter(
        (landscape == "Sulut" & jenis_training == "GAP")
      ) %>%
      distinct(id_kegiatan_training) %>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  
  ## Server:Petani:Dashboard:ValueBox: Total Peserta Pelatihan GAP ####
  output$totalPetaniTrainingGAP <- renderText({
    jumlah <- data_petani_terlatih %>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: BBS ####
  output$totalPetaniTerlatihBBS <- renderText({
    jumlah <- data_petani_terlatih %>%
      filter(landscape == 'BBS')%>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Singkil ####
  output$totalPetaniTerlatihSingkil <- renderText({
    jumlah <- data_petani_terlatih %>%
      filter(landscape == 'Singkil')%>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Sulut ####
  output$totalPetaniTerlatihSulut <- renderText({
    jumlah <- data_petani_terlatih %>%
      filter(landscape == 'Sulut')%>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ## Server:Petani:Dashboard:ValueBox: Total Petani Adopsi ####
  output$totalPetaniAdopsi <- renderText({
    jumlah <- data_petani_adopsigap %>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: BBS ####
  output$totalPetaniAdopsiBBS <- renderText({
    jumlah <- data_petani_adopsigap %>%
      filter(landscape == 'BBS')%>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Singkil ####
  output$totalPetaniAdopsiSingkil <- renderText({
    jumlah <- data_petani_adopsigap %>%
      filter(landscape == 'Singkil')%>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ### Server:Petani:Dashboard:renderText: Sulut ####
  output$totalPetaniAdopsiSulut <- renderText({
    jumlah <- data_petani_adopsigap %>%
      filter(landscape == 'Sulut')%>%
      nrow()
    
    formatC(jumlah, format = "d", big.mark = ",")
  })
  
  ## Server:Petani:Dashboard:Chart Kumulatif Petani ----
  output$barChartPetaniKumulatif <- renderPlotly({
    
    
    
    ## --- Data Petani Terlatih ---
    df_Petaniterlatih <- data_kumulatif_data_peserta_training %>%
      mutate(
        tahun = as.integer(tahun_pertama),
        jumlah = jumlah_petani_dampingan + jumlah_petani_nondampingan
      ) %>%
      group_by(tahun, landscape) %>%
      summarise(jumlah = sum(jumlah, na.rm = TRUE), .groups = "drop") %>%
      arrange(tahun) %>%
      group_by(landscape) %>%
      mutate(jumlah_kumulatif = cumsum(jumlah),
             kategori = "Terlatih") %>%
      ungroup()
    
    ## --- Data Petani Adopsi ---
    df_Petaniadopsi <- data_kumulatif_data_petani_adopsi %>%
      mutate(
        tahun = as.integer(tahun),
        jumlah = jumlah_petani_dampingan + jumlah_petani_non_dampingan
      ) %>%
      group_by(tahun, landscape) %>%
      summarise(jumlah = sum(jumlah, na.rm = TRUE), .groups = "drop") %>%
      arrange(tahun) %>%
      group_by(landscape) %>%
      mutate(jumlah_kumulatif = cumsum(jumlah),
             kategori = "Adopsi") %>%
      ungroup()
    
    ## --- Gabung kedua dataset ---
    df_all_petani <- bind_rows(df_Petaniterlatih, df_Petaniadopsi)
    
    ## --- Plot ---
    plot_ly(
      data = df_all_petani,
      x = ~tahun,
      y = ~jumlah_kumulatif,
      color = ~kategori,
      colors = c("Terlatih" = "#1f77b4", "Adopsi" = "#ff7f0e"),
      type = "bar",
      transforms = list(
        list(
          type = 'filter',
          target = ~landscape,
          operation = '=',
          value = unique(df_all_petani$landscape)[1]   # default landscape pertama
        )
      )
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Tahun"),
        yaxis = list(title = "Total Petani"),
        legend = list(title = list(text = "Kategori")),
        updatemenus = list(
          list(
            type = "dropdown",
            active = 0,
            buttons = lapply(unique(df_all_petani$landscape), function(ls) {
              list(
                method = "restyle",
                args = list("transforms[0].value", ls),
                label = ls
              )
            })
          )
        )
      )
  })
  
  
  ## Server:Petani:Dashboard:Map: Lahan Petani Terpetakan ####
  output$mapLahanPetani <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 104.3515884, lat = -5.4484732, zoom = 10) %>%
      
      # Lokasi administrasi
      # addGlPolygons(
      #   data = st_cast(lokasi_adm_sf, "POLYGON"),
      #   fillColor = "gray", weight = 1, fillOpacity = 0.3,
      #   group = "Wilayah Administrasi"
      # ) %>%
      
      # Batas resor Taman Nasional
      addGlPolygons(
        data = st_cast(batas_tamannasional_sf, "POLYGON"),
        fillColor = "#006d2c", weight = 1, fillOpacity = 0.8,
        group = "Batas Resor Taman Nasional"
      ) %>%
      
      # Batas IPZ
      addGlPolygons(
        data = st_cast(ipz_tnbbs_sf, "POLYGON"),
        fillColor = "#a50f15", weight = 2, fillOpacity = 0.4,
        group = "IPZ TNBBS"
      ) %>%
      
      # Polygon lahan petani
      addGlPolygons(
        data = st_cast(polygon_petani_bbs_sf, "POLYGON"),
        fillColor = "#253494", weight = 2, fillOpacity = 0.5,
        group = "Polygon Petani TNBBS"
      ) %>%
      
      # Titik lahan petani
      addGlPoints(
        data = sf_lahan_petani_wgs,
        fillColor = "#993404", radius = 4,
        group = "Titik Petani",
        popup = ~paste0(
          "<b> Nama Petani :</b>", nama_petani, "<br/>",
          "<b> Lahan ke : <b/>", hamparan_ke, "<br/>",
          "<b> Luas Lahan : <b/>", luas_lahan_ha, "<br/>",
          "<b> Tahun Penanaman : <b/>", tahun_penanaman, "<br/>",
          "<b> Site : <b/>", landscape, "<br/>",
          "<b> Lokasi : <b/>", desa, "<br/>"
        )
      ) %>%
      
      # Kontrol layer
      addLayersControl(
        overlayGroups = c(
          "Wilayah Administrasi",
          "IPZ TNBBS",
          "Polygon Petani TNBBS",
          "Titik Petani"
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  ## Server:Petani:Tab Profile Petani: Sidebar: Filter Data ####
  filtered_data_petani <- reactive({
    req(data_petani)
    
    if (!is.null(input$lokasi_petani) && input$lokasi_petani != "") {
      data_petani %>% filter(landscape == input$lokasi_petani)
    } else {
      data_petani
    }
  })
  
  ## Server:Petani:Tab Profile Petani: Konten Profile Petani ####
  output$konten_profile_petani <- renderUI({
    if (mode() == "tabel") {
      reactableOutput("tabel_petani")
    } else {
      row <- petani_terpilih()
      
      layout_columns(
        col_widths = c(12),
        class = "p-4 mb-4",
        layout_columns(
          col_widths = c(6,6),
          
          ### Profile Image and Contact ####
          div(
            style = "text-align: center;",
            tags$img(
              src = "user_foto.png",  # Ganti dengan foto profil
              class = "rounded-circle",
              style = "width: 120px; height: 120px;"
            ),
            h4(row$nama_petani),
            tags$hr(),
            tags$p(tags$b("ID Petani"), br(),row$id_petani),
            tags$p(tags$b("Jenis Kelamin"), br(), row$jenis_kelamin),
            tags$p(tags$b("Kelompok Tani"), br(), row$nama_kelompok_tani),
            tags$p(tags$b("Jabatan"), br(), row$jabatan)
          ),
          
          layout_columns(
            ### General Info ####
            card(
              card_header("Informasi Petani"),
              tags$p(tags$b("Jenis Produk:"), row$jenis_produk),
              tags$p(tags$b("Total Luas Lahan:"), row$total_luas_lahan_ha),
              tags$p(tags$b("Tanggal Bergabung:"), row$tanggal_kesepahaman)
            ),
            card(
              card_header("Informasi Administrasi"),
              tags$p(tags$b("Alamat Domisili:"), row$alamat_domisili_saat_ini),
              tags$p(tags$b("Landscape:"), row$landscape),
              tags$p(tags$b("Resort:"), row$resort),
              tags$p(tags$b("Provinsi:"), row$provinsi),
              tags$p(tags$b("Kabupaten:"), row$kabupaten),
              tags$p(tags$b("Kecamatan:"), row$kecamatan),
              tags$p(tags$b("Desa:"), row$desa)
            )
          )
        ),
        card(
          card_header("Informasi Lahan"),
          reactableOutput("LahanPetani")
        ),
        navset_tab(
          nav_panel(title = "Training", reactableOutput("tabelPetaniperModul")),
          nav_panel(title = "Monitoring Adopsi", reactableOutput("tabelAdopsiModul"))
        ),
        br(),
        tags$div(
          class = "get-started-button",
          onclick = "Shiny.setInputValue('kembali', Math.random())",
          tags$span("Kembali", class = "get-started-text"),
          tags$div(
            class = "arrow-container",
            tags$span("\u2190", class = "arrow-icon first"),
            tags$span("\u2190", class = "arrow-icon second")
          )
        )
      )
    }
  })
  
  ## Server:Petani:Tab Profile Petani: Tabel Petani ----
  output$tabel_petani <- renderReactable({
    reactable(filtered_data_petani(),
              defaultColDef = colDef(
                header = function(value) {
                  # Ganti "_" dengan spasi dan ubah ke Title Case (kapital di awal setiap kata)
                  value <- gsub("_", " ", value, fixed = TRUE)
                  s <- strsplit(value, " ")[[1]]
                  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
                },
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 100,
                headerStyle = list(background = "#e0f8e3")
              ),
              highlight = TRUE,
              defaultPageSize = 15,
              bordered = TRUE,
              selection = "single",
              onClick = "select",
              showPageSizeOptions = TRUE,
              paginationType = "jump",
              searchable = TRUE
    )
  })
  
  ### Server:Petani:Tab Profile Petani: klik row di tabel petani ####
  observeEvent(input$tabel_petani__reactable__selected, {
    idx <- input$tabel_petani__reactable__selected
    if (length(idx) > 0) {
      petani_terpilih(filtered_data_petani()[idx, ])  # Gunakan data hasil filter
      mode("card")
    }
  })
  
  ### Server:Petani:Tab Profile Petani:klik button "kembali" di tab petani ####
  observeEvent(input$kembali, {
    mode("tabel")
  })
  
  ### Server:Petani:Tab Profil Petani: Tabel Modul Training ####
  output$tabelPetaniperModul <- renderReactable({
    req(petani_terpilih())  # Pastikan ada petani yang dipilih
    
    id_terpilih <- petani_terpilih()$id_petani
    
    data_petani_training_per_modul %>%
      filter(id_petani == id_terpilih) %>%
      select(starts_with("modul_")) %>%  # hanya kolom modul_1 sampai modul_11
      pivot_longer(cols = everything(), names_to = "Modul", values_to = "Jumlah Pelatihan") %>%
      reactable(
        columns = list(
          Modul = colDef(name = "Modul"),
          `Jumlah Pelatihan` = colDef(name = "Jumlah Pelatihan")
        ),
        bordered = TRUE,
        highlight = TRUE,
        striped = TRUE,
        defaultColDef = colDef(
          header = function(value) {
            # Ganti "_" dengan spasi dan ubah ke Title Case (kapital di awal setiap kata)
            value <- gsub("_", " ", value, fixed = TRUE)
            s <- strsplit(value, " ")[[1]]
            paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
          },
          cell = function(value) format(value, nsmall = 1),
          align = "center",
          minWidth = 100,
          headerStyle = list(background = "#e0f8e3")
        )
      )
  })
  
  ### Server:Petani:Tab Profile Petani: Tabel Lahan Petani ####
  output$LahanPetani <- renderReactable({
    req(petani_terpilih())
    
    id_terpilih <- petani_terpilih()$id_petani
    
    data_petani_lahan <- data_lahan_petani %>%
      filter(id_petani == id_terpilih)
    
    reactable(
      data_petani_lahan %>%
        select(id_lahan, luas_lahan_ha, hamparan_ke, lokasi_lahan, desa),
      columns = list(
        id_lahan = colDef(name = "ID Lahan"),
        luas_lahan_ha = colDef(name = "Luas (ha)", format = colFormat(digits = 2)),
        hamparan_ke = colDef(name = "Lahan Ke-"),
        lokasi_lahan = colDef(name = "Lokasi Lahan"),
        desa = colDef(name = "Desa")
      ),
      striped = TRUE,
      highlight = TRUE,
      bordered = TRUE,
      defaultColDef = colDef(
        header = function(value) {
          # Ganti "_" dengan spasi dan ubah ke Title Case (kapital di awal setiap kata)
          value <- gsub("_", " ", value, fixed = TRUE)
          s <- strsplit(value, " ")[[1]]
          paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
        },
        cell = function(value) format(value, nsmall = 1),
        align = "center",
        minWidth = 100,
        headerStyle = list(background = "#e0f8e3")
      )
    )
  })
  
  ### Server:Petani:Tab Profile Petani: Tabel Modul Adopsi ----
  output$tabelAdopsiModul <- renderReactable({
    req(petani_terpilih())  # pastikan ada petani yang dipilih
    
    id_terpilih <- petani_terpilih()$id_petani
    
    data_petani_adopsi_per_modul %>%
      filter(id_petani == id_terpilih) %>%
      select(starts_with("modul_")) %>%  # ambil kolom modul_1 sampai modul_11
      pivot_longer(cols = everything(), names_to = "Modul", values_to = "Jumlah Adopsi") %>%
      reactable(
        columns = list(
          Modul = colDef(name = "Modul"),
          `Jumlah Adopsi` = colDef(name = "Jumlah Adopsi")
        ),
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE,
        defaultColDef = colDef(
          header = function(value) {
            # Ganti "_" dengan spasi dan ubah ke Title Case (kapital di awal setiap kata)
            value <- gsub("_", " ", value, fixed = TRUE)
            s <- strsplit(value, " ")[[1]]
            paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
          },
          cell = function(value) format(value, nsmall = 1),
          align = "center",
          minWidth = 100,
          headerStyle = list(background = "#e0f8e3")
        )
      )
  })
  
  ## Server:Petani:Tab Kegiatan:Sidebar: Tabel Kegiatan ####
  filtered_data_kegiatan_training_gap <- reactive({
    req(data_kegiatan_training_gap)
    
    data_kegiatan_training_gap %>%
      filter(
        if (!is.null(input$lokasi_kegiatanGAP) && input$lokasi_kegiatanGAP != "") landscape == input$lokasi_kegiatanGAP else TRUE,
        if (!is.null(input$periode_kegiatanGAP)) as.Date(tanggal_kegiatan) >= input$periode_kegiatanGAP[1] else TRUE,
        if (!is.null(input$periode_kegiatanGAP)) as.Date(tanggal_kegiatan) <= input$periode_kegiatanGAP[2] else TRUE
      )
  })
  
  ## Server:Petani:Tab Kegiatan: Tabel Kegiatan ----
  output$tabel_kegiatan_gap <- renderReactable({
    reactable(filtered_data_kegiatan_training_gap(),
              defaultColDef = colDef(
                header = function(value) {
                  # Ganti "_" dengan spasi dan ubah ke Title Case (kapital di awal setiap kata)
                  value <- gsub("_", " ", value, fixed = TRUE)
                  s <- strsplit(value, " ")[[1]]
                  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
                },
                cell = function(value) format(value, nsmall = 1),
                align = "center",
                minWidth = 100,
                headerStyle = list(background = "#e0f8e3")
              ),
              highlight = TRUE,
              selection = "single",
              onClick = "select",
              columns = list(
                id_kegiatan_training = colDef(show = FALSE)
              )
    )
  })
  
  
  ## Patroli:UI:Sidebar Aplikasi ####
  output$sidebar_aplikasi_patroli <- renderUI({
    if (sidebar_state() == "filterPatroli") {
      accordion(
        accordion_panel(
          "Filter Spatial",
          icon = bsicons::bs_icon("map-fill"),
          
          uiOutput("landscape_ui_AplikasiPatroli"),
          uiOutput("bptn_ui_AplikasiPatroli"),
          uiOutput("sptn_ui_AplikasiPatroli"),
          uiOutput("resor_ui_AplikasiPatroli")
        ),
        accordion_panel(
          "📅 Filter Temporal Data",
          dateRangeInput(
            "tanggal_AplikasiPatroli", "Filter Periode:",
            start = min(data_patroli$waypoint_date, na.rm = TRUE),
            end   = max(data_patroli$waypoint_date, na.rm = TRUE),
            format = "yyyy-mm-dd"
          )
        ),
        accordion_panel(
          "Filter Category Data",
          icon = bsicons::bs_icon("menu-app"),
          uiOutput("kolom_ui_AplikasiPatroli"),
          tags$hr(),
          strong("Catatan:"),
          p("Filter BPTN, SPTN & Resor hanya berdasarkan data batas TN (t_batas_tn). 
              Titik patroli ditampilkan semua (di dalam & di luar kawasan). 
              Biru = dalam kawasan, Merah = luar kawasan.")
        ),
        br(),
        actionButton("show_summary", "Tampilkan Summary"),
        uiOutput("back_btn_ui_AplikasiPatroli")   # <-- tombol back dinamis
      )
    } else if (sidebar_state() == "summaryPatroli") {
      tagList(
        h4("Ringkasan"),
        p("Isi ringkasan atau komponen lain di sini..."),
        br(),
        actionButton("goBackToFilterPatroli", label = tagList(icon("arrow-left"), "Kembali" ), class = "btn-custom-back"),
        actionButton("goToAnalysisPatroli", label = tagList("Analisis", icon("arrow-right")), class = "btn-custom")
      )
    } else if (sidebar_state() == "analysisPatroli") {
      tagList(
        h4("Analisis"),
        p("Isi Analisis atau komponen lain di sini..."),
        br(),
        actionButton("goBackToSummaryPatroli", label = tagList("Kembali", icon("arrow-left")), class = "btn-custom-back")
      )
    }
  })
  
  ### Patroli:UI:Main Panel Aplikasi ####
  output$main_panel <- renderUI({
    if (sidebar_state() == "filterPatroli") {
      # harus sama: main_content_AplikasiPatroli
      uiOutput("main_content_AplikasiPatroli")  
      
    } else if (sidebar_state() == "summaryPatroli") {
      tagList(
        reactableOutput("tbl_summary_AplikasiPatroli"),
        plotOutput("plot_summary")
      )
      
    } else if (sidebar_state() == "analysisPatroli") {
      tagList(
        h3("📈 Hasil Analisis"),
        plotOutput("grafik_analisis")
      )
    }
  })
  
  
  ## Patroli::Server - Aplikasi ####
  # flag apakah tombol summary sudah diklik
  summary_mode <- reactiveVal(FALSE)
  
  # simpan snapshot data patroli sebelum masuk summary
  snapshot_data <- reactiveVal(NULL)
  
  observeEvent(input$show_summary, {
    snapshot_data(patroli_filtered() %>% st_drop_geometry())
    # summary_mode(TRUE)   # HAPUS
    sidebar_state("summaryPatroli")   # GANTI ke state summary
  })
  
  ### --- Patroli::Server::Aplikasi:Filter landscape ----
  observe({
    
    all_landscape <- sort(unique(c(
      as.character(data_tamannasional$kawasan),
      as.character(data_patroli$landscape)
    )))
    
    if (length(all_landscape) > 0) {
      updateSelectInput(session, "landscape_AplikasiPatroli",
                        choices = all_landscape,
                        selected = all_landscape[1])
    }
  })
  
  output$landscape_ui_AplikasiPatroli <- renderUI({
    all_landscape <- sort(unique(c(
      as.character(data_tamannasional$kawasan),
      as.character(data_patroli$landscape)
    )))
    
    selectInput("landscape_AplikasiPatroli", "Landscape / Kawasan",
                choices = all_landscape,
                selected = all_landscape[1],
                multiple = TRUE)
  })
  
  
  ### --- Patroli::Server::Aplikasi:Filter BPTN ----
  batas_by_landscape <- reactive({
    req(input$landscape_AplikasiPatroli)
    data_tamannasional %>% filter(kawasan %in% input$landscape_AplikasiPatroli)
  })
  
  # --- Filter BPTN UI
  output$bptn_ui_AplikasiPatroli <- renderUI({
    bt <- batas_by_landscape()
    bt_bptn <- bt %>% filter(!is.na(bptn) & bptn != "")
    bptn_vals <- sort(unique(bt_bptn$bptn))
    if (length(bptn_vals) > 1) {
      selectInput("bptn_AplikasiPatroli", "BPTN", choices = bptn_vals,
                  selected = bptn_vals, multiple = TRUE)
    } else NULL
  })
  
  batas_after_bptn <- reactive({
    bt <- batas_by_landscape()
    if (!is.null(input$bptn_AplikasiPatroli) && length(input$bptn_AplikasiPatroli) > 0) {
      bt <- bt %>% filter(is.na(bptn) | bptn %in% input$bptn_AplikasiPatroli)
    }
    bt
  })
  
  ### --- Patroli::Server::Aplikasi:Filter SPTN ----
  output$sptn_ui_AplikasiPatroli <- renderUI({
    bt <- batas_after_bptn()
    bt_sptn <- bt %>% filter(!is.na(sptn) & sptn != "")
    sptn_vals <- sort(unique(bt_sptn$sptn))
    if (length(sptn_vals) > 1) {
      selectInput("sptn_AplikasiPatroli", "SPTN", choices = sptn_vals,
                  selected = sptn_vals, multiple = TRUE)
    } else NULL
  })
  
  batas_after_sptn <- reactive({
    bt <- batas_after_bptn()
    if (!is.null(input$sptn_AplikasiPatroli) && length(input$sptn_AplikasiPatroli) > 0) {
      bt <- bt %>% filter(is.na(sptn) | sptn %in% input$sptn_AplikasiPatroli)
    }
    bt
  })
  
  ### --- Patroli::Server::Aplikasi:Filter RESOR ----
  output$resor_ui_AplikasiPatroli <- renderUI({
    bt <- batas_after_sptn()
    resor_vals <- sort(unique(na.omit(bt$resor)))
    if (length(resor_vals) > 1) {
      selectInput("resor_AplikasiPatroli", "Resor", choices = resor_vals,
                  selected = resor_vals, multiple = TRUE)
    } else NULL
  })
  
  batas_filtered <- reactive({
    bt <- batas_after_sptn()
    if (!is.null(input$resor_AplikasiPatroli) && length(input$resor_AplikasiPatroli) > 0) {
      bt <- bt %>% filter(resor %in% input$resor_AplikasiPatroli)
    }
    bt
  })
  
  ### --- Patroli::Server::Aplikasi:Filter Data Patroli ----
  patroli_filtered <- reactive({
    req(input$landscape_AplikasiPatroli)
    pt <- data_patroli %>% filter(landscape %in% input$landscape_AplikasiPatroli)
    
    # filter tanggal
    if (!is.null(input$tanggal_AplikasiPatroli) && length(input$tanggal_AplikasiPatroli) == 2) {
      pt <- pt %>%
        mutate(waypoint_date = as.Date(waypoint_date)) %>%
        filter(
          waypoint_date >= as.Date(input$tanggal_AplikasiPatroli[1]) &
            waypoint_date <= as.Date(input$tanggal_AplikasiPatroli[2])
        ) 
    }
    
    bt <- batas_filtered()
    bt_union <- st_union(bt)
    
    if (nrow(bt) > 0 && nrow(pt) > 0) {
      pt_join <- st_join(
        pt, bt,
        join = st_within, left = TRUE,
        suffix = c("", "_bt")
      )
      
      for (nm in c("bptn_bt", "sptn_bt", "resor_bt")) {
        if (!nm %in% names(pt_join)) pt_join[[nm]] <- NA
      }
      
      inside_union <- lengths(st_within(pt, bt_union)) > 0
      
      pt_join <- pt_join %>%
        mutate(
          di_dalam_kawasan = inside_union,
          bptn  = ifelse(is.na(bptn),  "Luar Kawasan", bptn),
          sptn  = ifelse(is.na(sptn_bt),  "Luar Kawasan", sptn_bt),
          resor = ifelse(is.na(resor), "Luar Kawasan", resor),
          color_status = ifelse(inside_union, "blue", "red")
        )
      
      if ("Taman Nasional Bukit Barisan Selatan" %in% input$landscape_AplikasiPatroli) {
        inside_ipz <- lengths(st_intersects(pt_join, data_batas_area_ipz_tnbbs)) > 0
        pt_join <- pt_join %>%
          mutate(IPZ_status = ifelse(inside_ipz, "IPZ", "Non IPZ"))
      } else {
        pt_join$IPZ_status <- "Non IPZ"
      }
      
      pt <- pt_join
    } else {
      pt$di_dalam_kawasan <- FALSE
      pt$bptn  <- "Luar Kawasan"
      pt$sptn  <- "Luar Kawasan"
      pt$resor <- "Luar Kawasan"
      pt$IPZ_status <- "Non IPZ"
    }
    
    pt
  })
  
  ### --- Patroli::Server::Aplikasi:Filter Kolom yang dipilih ----
  output$kolom_ui_AplikasiPatroli <- renderUI({
    all_cols <- names(st_drop_geometry(data_patroli))
    default_cols <- c(
      "patrol_id",
      "landscape",
      "patrol_start_date",
      "patrol_end_date",
      "waypoint_id",
      "resor",
      "sptn",
      "bptn",
      "kawasan",
      "di_dalam_kawasan",
      "IPZ_status"
      ,"observation_category_0",
      "observation_category_1",
      "pelanggaran"
    )
    default_cols <- intersect(default_cols, all_cols)
    selectInput("kolom_patroli", 
                "Pilih Kolom Tabel Patroli:",
                choices = c(all_cols, "IPZ_status"),
                selected = default_cols,
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  ### --- Patroli::Server::Aplikasi:Tabel TN ----
  output$tbl_batas_AplikasiPatroli <- renderReactable({
    d <- batas_filtered() %>% 
      st_drop_geometry() %>%
      select(kawasan, bptn, sptn, resor, luas_ha)
    reactable(d, searchable = TRUE, striped = TRUE, defaultPageSize = 10)
  })
  
  ### --- Patroli::Server::Aplikasi:Tabel RAW Data Patroli ----
  output$tbl_patroli_AplikasiPatroli <- renderReactable({
    d <- patroli_filtered() %>% st_drop_geometry()
    
    # Selalu tambahkan row_key & details
    d$row_key <- seq_len(nrow(d))
    d$details <- "Show details"
    
    # Pastikan filter kolom tidak menghapus row_key/details
    if (!is.null(input$kolom_patroli)) {
      keep <- intersect(input$kolom_patroli, names(d))
      d <- d[c(keep, "row_key", "details")]
    }
    
    reactable(
      d,
      searchable = TRUE,
      striped = TRUE,
      outlined = TRUE,
      defaultPageSize = 10,
      bordered = TRUE,
      showPageSizeOptions = TRUE,
      paginationType = "jump",
      resizable = TRUE,
      theme = reactableTheme(
        stripedColor = "#f6f8fa",
        cellPadding = "8px 12px",
        style = list(fontFamily = "
                     -apple-system,
                     BlinkMacSystemFont, 
                     Segoe UI, 
                     Helvetica, 
                     Arial, 
                     sans-serif")
      ),
      defaultColDef = colDef(
        header = function(value) tools::toTitleCase(gsub("_", " ", value, fixed = TRUE)),
        align = "center",
        minWidth = 70,
        headerStyle = list(background = "#e0f8e3")
      ),
      columns = list(
        row_key = colDef(show = FALSE),
        patrol_id = colDef(name = "Patrol ID", sortable = TRUE),
        id_cell = colDef(name = "ID Cell"),
        id_grid = colDef(name = "ID Grid"),
        patrol_leg_id = colDef(name = "Patrol Leg ID"),
        landscape = colDef(filterable = TRUE),
        observation_category_1 = colDef(filterable = TRUE),
        observation_category_0 = colDef(filterable = TRUE),
        waypoint_id = colDef(name = "Waypoint ID", sortable = TRUE),
        pelanggaran = colDef(sortable = TRUE),
        patrol_start_date = colDef(sortable = TRUE),
        patrol_end_date = colDef(sortable = TRUE),
        details = colDef(
          name = "Details",
          sortable = FALSE,
          filterable = FALSE,
          cell = function() htmltools::tags$button("🔍 Show details")
        )
      ),
      onClick = JS("
      function(rowInfo, column) {
        if (column.id !== 'details') return
        if (window.Shiny) {
          Shiny.setInputValue('show_details', {
            row_key: rowInfo.values.row_key
          }, { priority: 'event' })
        }
      }
    ")
    )
  })
  
  #### --- Patroli::Server::Aplikasi:Button Show Detail ----
  observeEvent(input$show_details, {
    req(input$show_details$row_key)
    
    df <- patroli_filtered() %>% st_drop_geometry()
    df$row_key <- seq_len(nrow(df))   # harus sama dengan renderReactable
    
    baris_lengkap <- df[df$row_key == input$show_details$row_key, , drop = FALSE]
    
    if (nrow(baris_lengkap) == 0) {
      showModal(modalDialog(title = "Detail", "Baris tidak ditemukan.", easyClose = TRUE))
      return()
    }
    
    showModal(modalDialog(
      title = paste("Detail", baris_lengkap$patrol_id[1], "|", baris_lengkap$waypoint_id[1]),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Tutup"),
      verbatimTextOutput("detail_content")
    ))
    
    output$detail_content <- renderPrint({
      for (i in seq_len(nrow(baris_lengkap))) {
        for (col in names(baris_lengkap)) {
          val <- baris_lengkap[[col]][i]
          if (!is.na(val)) cat(sprintf("%-25s: %s\n", col, as.character(val)))
        }
        if (nrow(baris_lengkap) > 1 && i < nrow(baris_lengkap)) cat("----\n")
      }
    })
  })
  
  
  ### --- Patroli::Server::Aplikasi:Peta ####
  output$map_AplikasiPatroli <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addLayersControl(baseGroups = c("OSM", "Imagery"),
                       options = layersControlOptions(collapsed = TRUE))
  })
  
  observe({
    req(sidebar_state() == "filterPatroli")  # GANTI
    
    bt <- batas_filtered()
    pt <- patroli_filtered()
    
    pal_resor <- colorFactor("Set2", domain = bt$resor %>% as.character())
    
    m <- leafletProxy("map_AplikasiPatroli") %>%
      clearControls() %>%
      clearGroup("batas") %>%
      clearGroup("patroli") %>%
      clearGroup("ipz")
    
    if (nrow(bt) > 0) {
      bt_poly <- st_cast(bt, "POLYGON")
      m <- m %>% addGlPolygons(
        data = bt_poly,
        group = "batas",
        fillColor = pal_resor(bt_poly$resor),
        color = pal_resor(bt_poly$resor),
        weight = 1.5, opacity = 0.8, fillOpacity = 0.3
      )
    }
    
    if ("Taman Nasional Bukit Barisan Selatan" %in% input$landscape_AplikasiPatroli &&
        nrow(data_batas_area_ipz_tnbbs) > 0) {
      m <- m %>% addGlPolygons(
        data = st_cast(data_batas_area_ipz_tnbbs, "POLYGON"),
        group = "ipz",
        fillColor = "orange", color = "darkred",
        weight = 1, opacity = 0.8, fillOpacity = 0.4
      )
    }
    
    if (nrow(pt) > 0) {
      if (!"color_status" %in% names(pt)) {
        pt$color_status <- ifelse(pt$di_dalam_kawasan, "blue", "red")
      }
      m <- m %>% addGlPoints(
        data = pt, group = "patroli",
        fillColor = pt$color_status, radius = 6,
        popup = paste0(
          "<b>Waypoint:</b> ", pt$waypoint_id,
          "<br/><b>Tgl:</b> ", pt$waypoint_date,
          "<br/><b>Obs0:</b> ", pt$observation_category_0,
          "<br/><b>Obs1:</b> ", pt$observation_category_1,
          "<br/><b>Dalam Kawasan:</b> ", pt$di_dalam_kawasan,
          "<br/><b>IPZ:</b> ", pt$IPZ_status
        )
      )
    }
    
    # --- Fit bounds
    if (nrow(bt) > 0) {
      bb <- st_bbox(st_transform(bt, 4326))
      center <- c(mean(c(bb["xmin"], bb["xmax"])), mean(c(bb["ymin"], bb["ymax"])))
      m <- m %>% setView(lng = center[1], lat = center[2], zoom = 10)  # zoom bisa disesuaikan (8–12)
    } else if (nrow(pt) > 0) {
      bb <- st_bbox(st_transform(pt, 4326))
      center <- c(mean(c(bb["xmin"], bb["xmax"])), mean(c(bb["ymin"], bb["ymax"])))
      m <- m %>% setView(lng = center[1], lat = center[2], zoom = 10)
    }
    
    
    
    
    m <- m %>% addLegend(
      position = "bottomright",
      colors = c("blue", "red"),
      labels = c("Dalam Kawasan", "Luar Kawasan"),
      title = "Status Kawasan", opacity = 1
    )
  })
  
  ### --- Patroli::Server::Aplikasi:Query Summary Data ####
  summary_data <- eventReactive(input$show_summary, {
    d <- patroli_filtered() %>% st_drop_geometry()
    if (nrow(d) == 0) return(NULL)
    
    d <- d %>%
      mutate(
        tahun = lubridate::year(waypoint_date),
        quarter = lubridate::quarter(waypoint_date),
        bulan = lubridate::month(waypoint_date, label = TRUE, abbr = TRUE)
      )
    
    if ("Taman Nasional Bukit Barisan Selatan" %in% input$landscape_AplikasiPatroli) {
      d %>%
        group_by(tahun, quarter, bulan, bptn, sptn, resor, IPZ_status) %>%
        summarise(jumlah = n(), .groups = "drop")
    } else {
      d %>%
        group_by(tahun, quarter, bulan, bptn, sptn, resor) %>%
        summarise(jumlah = n(), .groups = "drop")
    }
  })
  
  ### --- Patroli::Server::Aplikasi:Render Main Panel Aplikasi ####
  output$main_content_AplikasiPatroli <- renderUI({
    if (sidebar_state() == "filterPatroli") {
      tabsetPanel(
        tabPanel("Peta", leafletOutput("map_AplikasiPatroli", height = 600)),
        tabPanel("Tabel Batas TN", reactableOutput("tbl_batas_AplikasiPatroli")),
        tabPanel("Tabel Patroli", reactableOutput("tbl_patroli_AplikasiPatroli"))
      )
    } else if (sidebar_state() == "summaryPatroli") {
      tabsetPanel(
        tabPanel("Tabel Summary", reactableOutput("tbl_summary_AplikasiPatroli")),
        tabPanel("Grafik Temuan", plotOutput("plot_summary", height = 500))
      )
    }
  })
  
  ### --- Patroli::Server::Aplikasi:Tabel Summary ----
  output$tbl_summary_AplikasiPatroli <- renderReactable({
    req(summary_data())
    reactable(
      summary_data(), 
      searchable = TRUE, 
      striped = TRUE, 
      defaultPageSize = 10)
  })
  
  ### --- Patroli::Server::Aplikasi:Btton back di Tabel Summary ----
  output$back_btn_ui_AplikasiPatroli <- renderUI({
    if (summary_mode()) {
      tagList(
        br(),
        actionButton("back_btn_AplikasiPatroli", "⬅ Kembali ke Halaman Sebelumnya")
      )
    }
  })
  
  observeEvent(input$back_btn_AplikasiPatroli, {
    # summary_mode(FALSE)   # HAPUS
    sidebar_state("filterPatroli")    # GANTI ke state filter
  })
  
  
  
  ## Patroli::Server - Dashboard ####
  ### Patroli::Server - Peta Grid Patroli ####
  # Warna berdasarkan landscape (sinkron grafik & peta)
  palette_landscape <- reactive({
    all_landscapes <- unique(data_gridpatroli$landscape)
    colorFactor("Dark2", domain = all_landscapes)
  })
  
  # Data filter berdasarkan input
  data_filtered_grid <- reactive({
    req(input$landscape_gridpatroli)
    
    list(
      data_grid = data_gridpatroli %>%
        filter(landscape %in% input$landscape_gridpatroli),
      
      batas_tn = batas_tamannasional_sf %>%
        filter(kawasan %in% input$landscape_gridpatroli)
    )
  })
  
  # Grafik Bar Plotly
  output$grafik_gridprioritas <- renderPlotly({
    df <- data_filtered_grid()$data_grid
    req(nrow(df) > 0)
    pal <- palette_landscape()
    df$tahun <- factor(df$tahun, levels = sort(unique(df$tahun)))
    
    
    plot_ly(
      df,
      x = ~tahun,
      y = ~persen_grid_prioritas_dipatroli,
      color = ~landscape,
      colors = pal(levels(factor(df$landscape))),
      type = 'bar',
      text = ~paste0(round(persen_grid_prioritas_dipatroli, 1), "%"),
      textposition = 'inside',
      textfont = list(color = "white"),
      marker = list(line = list(color = 'white', width = 1))
    ) %>%
      layout(
        barmode = 'stack',
        title = list(
          text = "<b>Grid Prioritas Dipatroli</b>",
          x = 0.5, xanchor = "center"
        ),
        xaxis = list(title = "Tahun"),
        yaxis = list(title = "Persentase Grid Prioritas Dipatroli (%)", ticksuffix = "%"),
        legend = list(
          x = 0,
          y = -0.2,  # letakkan di bawah grafik
          xanchor = "left",
          yanchor = "top",
          traceorder = "normal",
          font = list(size = 12),
          itemwidth = 50  # Lebar per item legend agar terlihat sejajar dan terbentuk dua kolom
        )
      )
  })
  
  # Peta Leaflet
  output$peta_gridprioritas <- renderLeaflet({
    df <- data_filtered_grid()
    batas_tn <- df$batas_tn
    pal <- palette_landscape()
    
    peta <- leaflet() %>%
      addTiles()
    
    # Tampilkan IPZ hanya jika BBS dipilih
    if ("Taman Nasional Bukit Barisan Selatan" %in% input$landscape_gridpatroli) {
      peta <- peta %>%
        addPolygons(data = ipz_tnbbs_sf,
                    color = "#a50f15", weight = 2,
                    group = "IPZ TNBBS")
    }
    
    # Tampilkan KPH Aceh hanya jika Leuser dipilih
    if ("Taman Nasional Gunung Leuser" %in% input$landscape_gridpatroli) {
      peta <- peta %>%
        addPolygons(data = batas_kph_aceh_sf,
                    color = "#969696", fillColor = "#7bccc4",
                    weight = 1, fillOpacity = 0.8,
                    group = "Batas KPH Aceh")
    }
    
    # Tambahkan batas taman nasional sesuai landscape
    kawasan_list <- unique(batas_tn$kawasan)
    for (kws in kawasan_list) {
      subset_sf <- batas_tn %>% filter(kawasan == kws)
      peta <- peta %>%
        addPolygons(
          data = subset_sf,
          color = pal(kws),
          fillColor = pal(kws),
          weight = 2,
          fillOpacity = 0.6,
          group = kws,
          label = ~kawasan
        )
    }
    
    # Layer control
    peta <- peta %>%
      addLayersControl(
        overlayGroups = c(
          if ("Taman Nasional Bukit Barisan Selatan" %in% input$landscape_gridpatroli) "IPZ TNBBS",
          if ("Taman Nasional Gunung Leuser" %in% input$landscape_gridpatroli) "Batas KPH Aceh",
          unique(batas_tn$kawasan)
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    peta
  })
  
  ### Patroli::Server - Value Box Adopsi SMART ####
  output$valuebox_adopsi_smart <- renderUI({
    
    total_upt <- data_smart_adopsi %>% 
      distinct(nama_upt) %>% 
      nrow()
    
    upt_adopsi <- data_smart_adopsi %>%
      filter(implementasi_smart == TRUE) %>%
      distinct(nama_upt) %>%
      nrow()
    
    persentase <- round((upt_adopsi / total_upt) * 100, 1)
    
    value_box(
      title = tags$div("UPT yang mengadopsi SMART", style = "font-weight: bold;"),
      value = paste0(persentase, "%"),
      showcase = tags$img(src = "SMART_LOGO.png",style = "width: 160px; height: auto; margin-bottom: 12px;"),
      class = "border",
      tags$div(
        style = "margin-top: 5px;",
        tags$div(tags$span(paste0("Total Adopsi ", upt_adopsi, " dari ", total_upt, " UPT")))
      ),
      style = "background-color: #3b561e; color: white; border-radius: 12px; padding: 1.5rem; box-shadow: 0 4px 16px rgba(0, 0, 0, 0.05);"
    )
    
  })
  
  ### Patroli::Server - Temuan Patroli ####
  # Warna berdasarkan landscape (sinkron grafik & peta)
  palette_landscape <- reactive({
    all_landscapes <- unique(data_gridpatroli$landscape)
    colorFactor("Dark2", domain = all_landscapes)
  })
  
  data_filtered <- reactive({
    req(input$landscape_temuan, input$jenis_temuan)
    df <- if (input$jenis_temuan == "Patroli") data_temuanpatroli else data_temuansatwa
    df <- df %>% filter(landscape %in% input$landscape_temuan)
    return(df)
  })
  
  output$tahun_ui <- renderUI({
    req(data_filtered())
    selectInput("tahun", "Pilih Tahun:", choices = sort(unique(data_filtered()$tahun)))
  })
  
  output$observation_ui <- renderUI({
    req(data_filtered(), input$jenis_temuan == "Satwa")
    selectInput("observation_category_1", "Upaya Patroli:", choices = unique(data_filtered()$observation_category_1),   multiple = TRUE)
  })
  
  output$kategori_ui <- renderUI({
    req(data_filtered(), input$jenis_temuan == "Satwa")
    selectInput("kategori_satwa", "Kategori Satwa:", choices = unique(data_filtered()$kategori_satwa))
  })
  
  output$plot_cpue <- renderPlotly({
    df <- data_filtered()
    req(nrow(df) > 0)
    
    if (input$jenis_temuan == "Patroli") {
      df <- df %>% mutate(factor_cpue = ifelse(input$mode_cpue == "100km", 100, 10))
      
      if (input$mode == "Tahunan") {
        df_sum <- df %>%
          group_by(tahun, landscape) %>%
          summarise(across(c(snares_total, illegal_encroachment_points, illegal_logging_points, foot, km_total), sum, na.rm = TRUE),
                    factor_cpue = first(factor_cpue), .groups = "drop") %>%
          mutate(denominator = ifelse(landscape == "Taman Nasional Bogani Nani Wartabone", km_total, foot),
                 snares_cpue = snares_total * factor_cpue / denominator,
                 encroach_cpue = illegal_encroachment_points * factor_cpue / denominator,
                 logging_cpue = illegal_logging_points * factor_cpue / denominator)
        
        tahun_levels <- seq(min(df_sum$tahun, na.rm = TRUE), 2025)
        df_sum$tahun <- factor(df_sum$tahun, levels = tahun_levels)
        
        plot_ly(df_sum, x = ~tahun) %>%
          add_lines(y = ~snares_cpue, name = "Snare CPUE") %>%
          add_lines(y = ~encroach_cpue, name = "Encroachment CPUE") %>%
          add_lines(y = ~logging_cpue, name = "Illegal Logging CPUE") %>%
          add_bars(y = ~foot, name = "Effort (Foot)", yaxis = "y2", opacity = 0.5) %>%
          layout(
            title = "CPUE Temuan Patroli per Tahun",
            xaxis = list(title = "Tahun", type = "category", categoryorder = "array", categoryarray = tahun_levels),
            yaxis = list(title = "CPUE", side = "left"),
            yaxis2 = list(title = "Foot", side = "right", overlaying = "y", showgrid = FALSE),
            barmode = "group"
          )
        
      } else {
        req(input$tahun)
        df_month <- df %>% filter(tahun == input$tahun) %>%
          group_by(bulan, landscape) %>%
          summarise(across(c(snares_total, illegal_encroachment_points, illegal_logging_points, foot, km_total), sum, na.rm = TRUE),
                    factor_cpue = first(factor_cpue), .groups = "drop") %>%
          mutate(denominator = ifelse(landscape == "Taman Nasional Bogani Nani Wartabone", km_total, foot),
                 snares_cpue = snares_total * factor_cpue / denominator,
                 encroach_cpue = illegal_encroachment_points * factor_cpue / denominator,
                 logging_cpue = illegal_logging_points * factor_cpue / denominator)
        
        df_month$bulan <- factor(df_month$bulan, levels = 1:12)
        
        plot_ly(df_month, x = ~bulan) %>%
          add_lines(y = ~snares_cpue, name = "Snare CPUE") %>%
          add_lines(y = ~encroach_cpue, name = "Encroachment CPUE") %>%
          add_lines(y = ~logging_cpue, name = "Illegal Logging CPUE") %>%
          add_bars(y = ~foot, name = "Effort (Foot)", yaxis = "y2", opacity = 0.5) %>%
          layout(
            title = paste("CPUE Temuan Patroli Bulanan -", input$tahun),
            xaxis = list(title = "Bulan", tickvals = 1:12, ticktext = month.name),
            yaxis = list(title = "CPUE", side = "left"),
            yaxis2 = list(title = "Foot", side = "right", overlaying = "y", showgrid = FALSE),
            barmode = "group"
          )
      }
      
    } else if (input$jenis_temuan == "Satwa") {
      req(input$observation_category_1, input$kategori_satwa)
      
      df <- df %>%
        filter(observation_category_1 %in% input$observation_category_1,
               kategori_satwa %in% input$kategori_satwa) %>%
        mutate(satuan_cpue = 100)
      
      if (input$mode == "Tahunan") {
        df_sum <- df %>%
          group_by(tahun, jenis_satwa, landscape) %>%
          summarise(jumlah_temuan = sum(jumlah_temuan, na.rm = TRUE),
                    foot = sum(foot, na.rm = TRUE),
                    km_total = sum(km_total, na.rm = TRUE),
                    satuan_cpue = first(satuan_cpue), .groups = "drop") %>%
          mutate(denominator = ifelse(landscape == "Taman Nasional Bogani Nani Wartabone", km_total, foot),
                 cpue = ifelse(denominator > 0, jumlah_temuan * satuan_cpue / denominator, NA_real_))
        
        df_effort <- df_sum %>% group_by(tahun) %>% summarise(foot = sum(foot, na.rm = TRUE), .groups = "drop")
        tahun_levels <- seq(min(df_sum$tahun, na.rm = TRUE), 2025)
        df_sum$tahun <- factor(df_sum$tahun, levels = tahun_levels)
        
        plot_ly(data = df_sum, x = ~tahun) %>%
          add_trace(y = ~cpue, split = ~jenis_satwa, type = "scatter", mode = "lines+markers") %>%
          add_trace(data = df_effort, x = ~tahun, y = ~foot, name = "Effort (Foot)", type = "bar", yaxis = "y2", opacity = 0.5) %>%
          layout(
            title = "CPUE Temuan Satwa Tahunan",
            xaxis = list(title = "Tahun"),
            yaxis = list(title = "CPUE", side = "left"),
            yaxis2 = list(title = "Foot", side = "right", overlaying = "y"),
            barmode = "group"
          )
        
      } else {
        req(input$tahun)
        df_month <- df %>% filter(tahun == input$tahun) %>%
          group_by(bulan, jenis_satwa, landscape) %>%
          summarise(jumlah_temuan = sum(jumlah_temuan, na.rm = TRUE),
                    foot = sum(foot, na.rm = TRUE),
                    km_total = sum(km_total, na.rm = TRUE),
                    satuan_cpue = first(satuan_cpue), .groups = "drop") %>%
          mutate(denominator = ifelse(landscape == "Taman Nasional Bogani Nani Wartabone", km_total, foot),
                 cpue = ifelse(denominator > 0, jumlah_temuan * satuan_cpue / denominator, NA_real_))
        
        df_effort <- df_month %>% group_by(bulan) %>% summarise(foot = sum(foot, na.rm = TRUE), .groups = "drop")
        df_month$bulan <- factor(df_month$bulan, levels = 1:12)
        
        plot_ly(data = df_month, x = ~bulan) %>%
          add_trace(y = ~cpue, split = ~jenis_satwa, type = "scatter", mode = "lines+markers") %>%
          add_trace(data = df_effort, x = ~bulan, y = ~foot, name = "Effort (Foot)", type = "bar", yaxis = "y2", opacity = 0.5) %>%
          layout(
            title = paste("CPUE Temuan Satwa Bulanan -", input$tahun),
            xaxis = list(title = "Bulan", tickvals = 1:12, ticktext = month.name),
            yaxis = list(title = "CPUE", side = "left"),
            yaxis2 = list(title = "Foot", side = "right", overlaying = "y"),
            barmode = "group"
          )
      }
    }
  })
  
  
}


