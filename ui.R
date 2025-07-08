# ui.R

library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)

fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styleDraf1.css"),
    tags$link(rel = "stylesheet", 
            href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
  
  # Bootstrap Grid System (jika belum ada)
  tags$link(rel = "stylesheet", 
            href = "https://cdnjs.cloudflare.com/ajax/libs/bootstrap/4.6.0/css/bootstrap-grid.min.css"),
  
  ),
  
  div(class = "custom-title-panel",
      h1("Dashboard Tingkat Kemiskinan di Indonesia",
         'data-text' = "Dashboard Tingkat Kemiskinan di Indonesia")
  ),
  
  navbarPage(
    title = "Navigasi",
    id = "main_tabs",
    
    tabPanel("Beranda",
             icon = icon("home"),
             div(class = "beranda-content",
                 h2("Selamat Datang di Dashboard Tingkat Kemiskinan"),
                 p("Dashboard ini menyediakan visualisasi dan analisis data tingkat kemiskinan di berbagai provinsi di Indonesia."),
                 p("Unggah data Anda dan pilih provinsi untuk memulai analisis deskriptif."),
                 br(),
                 img(src = "peta_indonesia.png", alt = "Peta Indonesia", style = "max-width: 80%; height: auto; display: block; margin: auto;"),
                 em("Gambar: Ilustrasi Peta Indonesia")
             )
    ),
    
    tabPanel("Input Data & Analisis Deskriptif",
             icon = icon("upload"),
             sidebarLayout(
               sidebarPanel(
                 h3("Unggah Data Kemiskinan"),
                 fileInput("file_upload", "Pilih File Data (CSV/Excel/SPSS)",
                           multiple = FALSE,
                           accept = c(".csv", ".xlsx", ".xls", ".sav")),
                 hr(),
                 uiOutput("provinsi_selector"),
                 actionButton("run_analisis", "Jalankan Analisis Deskriptif", class = "btn-primary"),
                 hr(),
                 # Legend Provinsi dipindahkan ke sini
                 h4("Provinsi Terpilih:"),
                 uiOutput("selected_provinsi_legend"),
                 hr(),
                 # Legend Nama Variabel dipindahkan ke sini
                 h4("Nama Variabel:"),
                 p("X1: Jumlah Penduduk Miskin"),
                 p("P0: Persentase Penduduk Miskin"),
                 p("P1: Indeks Kedalaman Kemiskinan"),
                 p("P2: Indeks Keparahan Kemiskinan"),
                 p("X2: Garis Kemiskinan"),
                 hr(),
                 # Section untuk Download Laporan PDF
                 h4("Download Laporan PDF"),
                 downloadButton("download_deskriptif_pdf", "Laporan Deskriptif", 
                                class = "btn-info", style = "width: 100%; margin-bottom: 5px;"),
                 conditionalPanel(
                   condition = "input.run_anova > 0",
                   downloadButton("download_anova_pdf", "Laporan ANOVA", 
                                  class = "btn-warning", style = "width: 100%; margin-bottom: 5px;")
                 ),
                 downloadButton("download_lengkap_pdf", "Laporan Lengkap", 
                                class = "btn-success", style = "width: 100%; margin-bottom: 5px;")
               ),
               mainPanel(
                 tabsetPanel(
                   # Sub-tab "Analisis Deskriptif" untuk Panduan
                   tabPanel("Analisis Deskriptif", icon = icon("book"),
                            box(
                              width = 20,
                              status = "info",
                              solidHeader = TRUE,
                              # uiOutput("informasi_analisis_deskriptif_r"), # Opsional: jika ini masih ada
                              # hr(),                                       # Opsional: jika ini masih ada
                              
                              # <<< TAMBAHKAN div DENGAN ID UNIK DI SINI >>>
                              div(id = "markdown_panduan_container", # ID ini akan kita gunakan di CSS
                                  includeMarkdown("www/panduan_analisis_deskriptif.Rmd") # Atau .md
                              )
                              # <<< AKHIR div >>>
                            )
                   ),
                   tabPanel("Pratinjau Data",
                            DT::dataTableOutput("tabel_data")),
                   # TAB BARU UNTUK RINGKASAN STATISTIK
                   tabPanel("Ringkasan Statistik",
                            # Ringkasan Statistik Per Provinsi & Per Kolom
                            h3("Ringkasan Statistik Per Provinsi"),
                            p("Menampilkan ringkasan statistik (min, max, mean, median, standar deviasi, jumlah observasi) untuk setiap kolom kemiskinan (X1, P0, P1, P2, X2) secara terpisah di setiap provinsi yang dipilih."),
                            hr(),
                            uiOutput("ringkasan_statistik_ui"), # Ini adalah ringkasan per kolom
                            
                            br(), # Spasi pemisah
                            
                            
                   ),
                   # TAB DISTRIBUSI (HANYA BOXPLOT GABUNGAN)
                   tabPanel("Distribusi",
                            uiOutput("boxplot_gabungan_distribusi_ui"))
                 )
               )
             )
    ),
    
    # Tab Analisis ANOVA
tabPanel("Analisis ANOVA",
         icon = icon("calculator"),
         
         fluidRow(
           # SIDEBAR: Pengaturan Analisis (Sticky)
           column(3,
                  div(class = "panel panel-info", style = "position: sticky; top: 20px;",
                      div(class = "panel-heading",
                          h4("Pengaturan Analisis ANOVA", style = "margin: 0; color: white;")
                      ),
                      div(class = "panel-body",
                          # Status provinsi terpilih
                          uiOutput("provinsi_ui_anova"),
                          
                          br(),
                          
                          # Pilihan variabel dependent
                          selectInput("var_anova", 
                                      label = strong("Variabel Dependent:"),
                                      choices = c("Jumlah Penduduk Miskin" = "Jumlah Penduduk Miskin",
                                                  "Persentase Penduduk Miskin" = "Persentase Penduduk Miskin",
                                                  "Indeks Kedalaman Kemiskinan" = "Indeks Kedalaman Kemiskinan",
                                                  "Indeks Keparahan Kemiskinan" = "Indeks Keparahan Kemiskinan",
                                                  "Garis Kemiskinan" = "Garis Kemiskinan"),
                                      selected = "Persentase Penduduk Miskin"),
                          
                          br(),
                          
                          # Tombol analisis
                          actionButton("run_anova", 
                                       "Jalankan Analisis ANOVA", 
                                       class = "btn btn-primary btn-block"),
                          
                          br(), br(),
                          
                          # Informasi ANOVA
                          div(class = "well well-sm",
                              h5(strong("Tentang ANOVA:")),
                              p("Analysis of Variance (ANOVA) digunakan untuk menguji apakah terdapat perbedaan rata-rata yang signifikan antar kelompok (provinsi).", 
                                style = "font-size: 12px; text-align: justify;"),
                              
                              h5(strong("Asumsi ANOVA:")),
                              tags$ul(
                                tags$li("Normalitas residual", style = "font-size: 12px;"),
                                tags$li("Homogenitas varians", style = "font-size: 12px;"),
                                tags$li("Independensi observasi", style = "font-size: 12px;")
                              ),
                              
                              h5(strong("Interpretasi P-value:")),
                              tags$ul(
                                tags$li("p < 0.05: Signifikan", style = "font-size: 12px;"),
                                tags$li("p ≥ 0.05: Tidak signifikan", style = "font-size: 12px;")
                              )
                          )
                      )
                  )
           ),
           
           # MAIN CONTENT: Hasil Analisis
           column(9,
                  # Tab Navigation untuk hasil
                  tabsetPanel(
                    id = "anova_results_tabs",
                    type = "tabs",
                    
                    # Tab 1: Uji Asumsi
                    tabPanel("Uji Asumsi",
                             value = "assumptions",
                             icon = icon("check-circle"),
                             
                             br(),
                             
                             # Uji Normalitas
                             div(class = "panel panel-info",
                                 div(class = "panel-heading",
                                     h4("Uji Asumsi Normalitas", style = "margin: 0;")
                                 ),
                                 div(class = "panel-body",
                                     p("Periksa apakah residual berdistribusi normal menggunakan uji Shapiro-Wilk.", 
                                       class = "text-muted"),
                                     
                                     # Hasil Uji Normalitas
                                     h5(strong("Hasil Uji Shapiro-Wilk:")),
                                     uiOutput("normality_test"),
                                     
                                     br(),
                                     
                                     # Plot Q-Q di bawah hasil uji
                                     h5(strong("Q-Q Plot untuk Visualisasi Normalitas:")),
                                     p("Titik-titik harus mengikuti garis diagonal merah untuk menunjukkan distribusi normal.", 
                                       class = "text-muted", style = "font-size: 12px;"),
                                     plotOutput("qq_plot", height = "400px")
                                 )
                             ),
                             
                             br(),
                             
                             # Uji Homogenitas
                             div(class = "panel panel-info",
                                 div(class = "panel-heading",
                                     h4("Uji Asumsi Homogenitas", style = "margin: 0;")
                                 ),
                                 div(class = "panel-body",
                                     p("Periksa apakah varians antar kelompok homogen menggunakan uji Levene.", 
                                       class = "text-muted"),
                                     
                                     # Hasil Uji Homogenitas
                                     h5(strong("Hasil Uji Levene:")),
                                     uiOutput("homogeneity_test"),
                                     
                                     br(),
                                     
                                     # Plot Residual di bawah hasil uji
                                     h5(strong("Plot Residual vs Fitted Values:")),
                                     p("Titik-titik harus tersebar acak di sekitar garis horizontal untuk menunjukkan varians yang homogen.", 
                                       class = "text-muted", style = "font-size: 12px;"),
                                     plotOutput("residuals_plot", height = "400px")
                                 )
                             )
                    ),
                    
                    # Tab 2: Hasil ANOVA
                    tabPanel("Hasil ANOVA",
                             value = "anova_results", 
                             icon = icon("chart-bar"),
                             
                             br(),
                             
                             # Hasil Utama ANOVA
                             div(class = "panel panel-info",
                                 div(class = "panel-heading",
                                     h4("Hasil Analisis ANOVA", style = "margin: 0;")
                                 ),
                                 div(class = "panel-body",
                                     uiOutput("anova_summary"),
                                     
                                     br(),
                                     
                                     h5(strong("Visualisasi Perbandingan Antar Provinsi:")),
                                     plotOutput("anova_plot", height = "450px")
                                 )
                             ),
                             
                             br(),
                             
                             # Tabel ANOVA Detail
                             div(class = "panel panel-info",
                                 div(class = "panel-heading",
                                     h4("Tabel ANOVA", style = "margin: 0;")
                                 ),
                                 div(class = "panel-body",
                                     p("Tabel detail yang menunjukkan dekomposisi varians.", 
                                       class = "text-muted"),
                                     
                                     fluidRow(
                                       column(8,
                                              tableOutput("anova_table")
                                       ),
                                       column(4,
                                              div(class = "well well-sm",
                                                  h5(strong("Keterangan:")),
                                                  tags$ul(
                                                    tags$li(strong("df:"), " Degrees of freedom", style = "font-size: 12px;"),
                                                    tags$li(strong("Sum Sq:"), " Sum of squares", style = "font-size: 12px;"),
                                                    tags$li(strong("Mean Sq:"), " Mean squares", style = "font-size: 12px;"),
                                                    tags$li(strong("F value:"), " F-statistic", style = "font-size: 12px;"),
                                                    tags$li(strong("Pr(>F):"), " P-value", style = "font-size: 12px;")
                                                  )
                                              )
                                       )
                                     )
                                 )
                             ),
                             
                             br(),
                             
                             # Ringkasan Model
                             div(class = "panel panel-info",
                                 div(class = "panel-heading",
                                     h4("Ringkasan Model", style = "margin: 0;")
                                 ),
                                 div(class = "panel-body",
                                     p("Informasi tentang kekuatan model dan variabilitas yang dijelaskan.", 
                                       class = "text-muted"),
                                     uiOutput("model_summary")
                                 )
                             )
                    ),
                    
                    # Tab 3: Post-Hoc Analysis
                    tabPanel("Post-Hoc: Tukey HSD",
                             value = "posthoc",
                             icon = icon("search-plus"),
                             
                             br(),
                             
                             conditionalPanel(
                               condition = "output.show_tukey",
                               
                               # Header untuk hasil signifikan
                               div(class = "alert alert-success",
                                   h4(icon("check-circle"), " ANOVA Signifikan - Analisis Post-Hoc Dilakukan"),
                                   p("Karena hasil ANOVA menunjukkan perbedaan yang signifikan (p < 0.05), analisis dilanjutkan dengan uji Tukey HSD untuk mencari provinsi yang berbeda secara signifikan.")
                               ),
                               
                               # Hasil Tukey HSD
                               div(class = "panel panel-info",
                                   div(class = "panel-heading",
                                       h4("Hasil Tukey HSD", style = "margin: 0;")
                                   ),
                                   div(class = "panel-body",
                                       uiOutput("tukey_results")
                                   )
                               ),
                               
                               br(),
                               
                               # Plot Tukey HSD
                               div(class = "panel panel-info",
                                   div(class = "panel-heading",
                                       h4("Visualisasi Tukey HSD", style = "margin: 0;")
                                   ),
                                   div(class = "panel-body",
                                       p("Interval kepercayaan yang tidak melewati garis 0 menunjukkan perbedaan signifikan.", 
                                         class = "text-muted"),
                                       plotOutput("tukey_plot", height = "500px")
                                   )
                               ),
                               
                               br(),
                               
                               # Interpretasi Lengkap
                               div(class = "panel panel-info",
                                   div(class = "panel-heading",
                                       h4("Interpretasi Lengkap", style = "margin: 0;")
                                   ),
                                   div(class = "panel-body",
                                       uiOutput("interpretation")
                                   )
                               )
                             ),
                             
                             conditionalPanel(
                               condition = "!output.show_tukey",
                               
                               # Pesan jika tidak signifikan
                               div(class = "alert alert-warning",
                                   h4(icon("exclamation-triangle"), " Uji Post-Hoc Tidak Diperlukan"),
                                   div(class = "well",
                                       h5(strong("Mengapa tidak ada analisis Tukey HSD?")),
                                       p("Uji Post-Hoc (Tukey HSD) hanya dilakukan jika hasil ANOVA menunjukkan signifikansi (p < 0.05). 
                                         Hasil ANOVA Anda menunjukkan tidak ada perbedaan signifikan antar provinsi."),
                                       
                                       h5(strong("Apa artinya?")),
                                       p("Meskipun ada variasi dalam data antar provinsi, perbedaan tersebut tidak cukup besar 
                                         untuk dianggap signifikan secara statistik."),
                                       
                                       h5(strong("Langkah selanjutnya:")),
                                       tags$ul(
                                         tags$li("Periksa ukuran sampel - mungkin perlu data lebih banyak"),
                                         tags$li("Pertimbangkan faktor lain yang mungkin mempengaruhi variabel"),
                                         tags$li("Evaluasi kembali pemilihan provinsi untuk analisis")
                                       )
                                   )
                               )
                             )
                    )
                  )
           )
         ),         
             # Footer dengan panduan interpretasi
             fluidRow(
               column(12,
                      div(class = "alert alert-info",
                          h5(icon("graduation-cap"), " Panduan Interpretasi"),
                          tags$ul(
                            tags$li("P-value < 0.05: Terdapat perbedaan signifikan antar kelompok"),
                            tags$li("F-statistic yang besar menunjukkan perbedaan antar kelompok lebih besar dari variasi dalam kelompok"),
                            tags$li("R-squared menunjukkan proporsi variabilitas yang dijelaskan oleh faktor provinsi"),
                            tags$li("Tukey HSD mengontrol error rate saat melakukan multiple comparison"),
                            tags$li("Asumsi normalitas dan homogenitas harus terpenuhi untuk validitas hasil")
                          )
                      )
               )
             )
    ),
# new
tabPanel("Peta",
         icon = icon("map"),
         fluidRow(
           box(
             width = 12,
             title = "Panduan Penggunaan Tab Peta",
             status = "info",
             solidHeader = TRUE,
             HTML(includeHTML("www/panduan_peta.html"))
           )
         ),
         fluidRow(
           box(width = 12, title = "Peta Kemiskinan Interaktif", status = "primary", solidHeader = TRUE,
               selectInput("indikator_peta", "Pilih Indikator untuk Peta:",
                           choices = c("Jumlah Penduduk Miskin (X1)" = "X1",
                                       "Garis Kemiskinan (X2)" = "X2",
                                       "Persentase Miskin (P0)" = "P0",
                                       "Indeks Kedalaman (P1)" = "P1",
                                       "Indeks Keparahan (P2)" = "P2"),
                           selected = "X2"),
               leafletOutput("peta_kemiskinan", height = 600)
           )
         ),
         fluidRow(
           box(
             width = 12,
             title = "Validasi Nama Wilayah",
             status = "info",
             solidHeader = TRUE,
             uiOutput("hasil_validasi_nama")
           )
         )
)
  )
)