# server.R

library(shiny)
library(readr)
library(readxl)
library(haven)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)
library(car)
library(multcomp)
library(sf)
library(stringr)
library(leaflet)
library(rmarkdown)  # Tambahan untuk render PDF
library(knitr)      # Tambahan untuk tabel di PDF
library(tinytex)    # Tambahan untuk LaTeX/PDF generation

# --- Definisi Fungsi Server Utama ---
server <- function(input, output, session) { # <<< SEMUA KODE SERVER HARUS DI DALAM KURUNG KURAWAL INI {}
  
  # --- BLOK PEMETAAN PROVINSI ---
  provinsi_mapping <- reactiveVal(NULL)
  observe({
    map_file_path_xlsx <- "www/provinsi.xlsx"
    
    mapping_df <- NULL
    
    if (file.exists(map_file_path_xlsx)) {
      tryCatch({
        mapping_df <- read_excel(map_file_path_xlsx)
      }, error = function(e) {
        showNotification(paste("Gagal membaca provinsi.xlsx:", e$message), type = "error")
        mapping_df <- NULL
      })
    } else {
      showNotification("File 'www/provinsi.xlsx' tidak ditemukan. Legend nama provinsi mungkin tidak berfungsi.", type = "warning")
    }
    
    if (!is.null(mapping_df)) {
      if (!all(c("ID_Provinsi", "Nama_Provinsi") %in% names(mapping_df))) {
        showNotification("File pemetaan provinsi harus memiliki kolom 'ID_Provinsi' dan 'Nama_Provinsi'.", type = "error")
        provinsi_mapping(NULL)
      } else {
        mapping_df$ID_Provinsi <- as.character(mapping_df$ID_Provinsi) # Pastikan ini karakter
        provinsi_mapping(setNames(mapping_df$Nama_Provinsi, mapping_df$ID_Provinsi))
      }
    } else {
      provinsi_mapping(NULL)
    }
  })
  
  # Reactive value untuk menyimpan data yang diunggah
  data_uploaded <- reactiveVal(NULL)
  
  # Observer untuk mengunggah file
  observeEvent(input$file_upload, {
    req(input$file_upload)
    file_path <- input$file_upload$datapath
    file_ext <- tolower(tools::file_ext(file_path))
    
    df <- NULL
    
    if (file_ext == "csv") {
      tryCatch({
        df <- read_csv(file_path, col_names = TRUE)
      }, error = function(e) {
        showNotification(paste("Gagal membaca CSV:", e$message), type = "error")
      })
    } else if (file_ext %in% c("xlsx", "xls")) {
      tryCatch({
        df <- read_excel(file_path)
      }, error = function(e) {
        showNotification(paste("Gagal membaca Excel:", e$message), type = "error")
      })
    } else if (file_ext == "sav") {
      tryCatch({
        df <- read_sav(file_path)
        df <- as.data.frame(lapply(df, function(x) if("haven_labelled" %in% class(x)) as_factor(x) else x))
        
      }, error = function(e) {
        showNotification(paste("Gagal membaca SPSS (.sav):", e$message), type = "error")
      })
    } else {
      showNotification("Format file tidak didukung. Harap unggah CSV, XLSX, XLS, atau SAV.", type = "warning")
    }
    
    if (!is.null(df)) {
      # Memastikan kolom 'Kab' dan 'Provinsi' ada dan P0 ada
      if (!"Kab" %in% names(df) || !"Provinsi" %in% names(df) || !"P0" %in% names(df)) {
        showNotification("Data harus mengandung kolom 'Kab', 'Provinsi', dan 'P0'.", type = "error")
        data_uploaded(NULL)
        return()
      }
      # Memastikan kolom-kolom yang akan dianalisis adalah numerik
      df$Provinsi <- as.numeric(df$Provinsi)
      df$P0 <- as.numeric(df$P0)
      df$X1 <- as.numeric(df$X1)
      df$P1 <- as.numeric(df$P1)
      df$P2 <- as.numeric(df$P2)
      df$X2 <- as.numeric(df$X2)
      
      data_uploaded(df)
      showNotification("Data berhasil diunggah! Sekarang pilih provinsi untuk analisis.", type = "message")
    } else {
      data_uploaded(NULL) # Reset data jika gagal
    }
  })
  
  # UI dinamis untuk memilih hingga 4 provinsi
  output$provinsi_selector <- renderUI({
    df <- data_uploaded()
    req(df)
    
    unique_provinsi_ids <- sort(unique(df$Provinsi))
    prov_map <- provinsi_mapping()
    
    choices_with_names <- if (!is.null(prov_map) && all(unique_provinsi_ids %in% names(prov_map))) {
      setNames(unique_provinsi_ids, paste0(prov_map[as.character(unique_provinsi_ids)], " (ID: ", unique_provinsi_ids, ")"))
    } else {
      unique_provinsi_ids # Fallback ke ID jika mapping tidak ada atau tidak lengkap
    }
    
    if (length(unique_provinsi_ids) > 0) {
      selectizeInput("selected_provinsi",
                     "Pilih Hingga 4 Provinsi untuk Analisis:",
                     choices = choices_with_names,
                     multiple = TRUE,
                     selected = NULL,
                     options = list(maxItems = 4)
      )
    } else {
      p("Tidak ada data provinsi yang ditemukan setelah unggah.")
    }
  })
  
  # Render legend provinsi terpilih di sidebarPanel
  output$selected_provinsi_legend <- renderUI({
    req(input$selected_provinsi)
    prov_map <- provinsi_mapping()
    
    if (is.null(input$selected_provinsi) || length(input$selected_provinsi) == 0) {
      return(p("Belum ada provinsi yang dipilih."))
    }
    
    tagList(
      lapply(input$selected_provinsi, function(prov_id) {
        prov_name <- if (!is.null(prov_map) && as.character(prov_id) %in% names(prov_map)) {
          prov_map[as.character(prov_id)]
        } else {
          paste("Nama tidak tersedia")
        }
        p(paste0("ID ", prov_id, ": ", prov_name))
      })
    )
  })
  
  
  # Reactive value untuk menyimpan data yang sudah difilter per provinsi
  filtered_data <- reactive({
    df <- data_uploaded()
    req(df, input$selected_provinsi)
    
    df_filtered <- df %>%
      filter(Provinsi %in% as.numeric(input$selected_provinsi))
    
    df_filtered
  })
  
  
  # Render tabel data
  output$tabel_data <- DT::renderDataTable({
    df_to_display <- filtered_data()
    
    if (is.null(df_to_display) || nrow(df_to_display) == 0) {
      return(DT::datatable(data.frame(Pesan = "Pilih data dan provinsi untuk pratinjau."), options = list(pageLength = 10)))
    }
    
    prov_map <- provinsi_mapping()
    if (!is.null(prov_map) && "Provinsi" %in% names(df_to_display)) {
      df_to_display$Nama_Provinsi <- prov_map[as.character(df_to_display$Provinsi)]
      df_to_display <- df_to_display %>%
        dplyr::select(Kab, Provinsi, Nama_Provinsi, everything())
    }
    
    DT::datatable(df_to_display, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  
  # Reactive value untuk menyimpan hasil analisis deskriptif per provinsi dan per kolom
  analisis_deskriptif_per_prov_col <- reactiveVal(NULL)
  # Reactive value untuk menyimpan hasil analisis deskriptif GABUNGAN per provinsi
  analisis_gabungan_per_prov <- reactiveVal(NULL)
  
  
  # Observer untuk tombol "Jalankan Analisis Deskriptif"
  observeEvent(input$run_analisis, {
    df_filtered <- filtered_data()
    req(df_filtered)
    
    if (nrow(df_filtered) == 0) {
      showNotification("Tidak ada data yang tersedia untuk provinsi yang dipilih.", type = "warning")
      analisis_deskriptif_per_prov_col(NULL)
      analisis_gabungan_per_prov(NULL)
      return()
    }
    
    results_per_col <- list()
    results_gabungan <- list()
    
    # Loop melalui setiap provinsi yang dipilih
    for (prov_id in input$selected_provinsi) {
      df_prov <- df_filtered %>% filter(Provinsi == prov_id)
      if (nrow(df_prov) == 0) {
        results_per_col[[as.character(prov_id)]] <- "Tidak ada data untuk provinsi ini."
        results_gabungan[[as.character(prov_id)]] <- "Tidak ada data untuk provinsi ini."
        next
      }
      
      # Analisis per kolom untuk Ringkasan Statistik Per Provinsi & Per Kolom
      prov_results_col <- list()
      for (col_name in c("X1", "P0", "P1", "P2", "X2")) {
        if (col_name %in% names(df_prov)) {
          data_col <- df_prov[[col_name]]
          data_col_cleaned <- na.omit(data_col)
          
          if (length(data_col_cleaned) > 0) {
            prov_results_col[[col_name]] <- list(
              ringkasan = summary(data_col_cleaned),
              mean = mean(data_col_cleaned),
              median = median(data_col_cleaned),
              sd = sd(data_col_cleaned),
              n = length(data_col_cleaned)
            )
          } else {
            prov_results_col[[col_name]] <- "Tidak ada data numerik valid untuk analisis kolom ini."
          }
        } else {
          prov_results_col[[col_name]] <- paste("Kolom '", col_name, "' tidak ditemukan untuk provinsi ini.", sep="")
        }
      }
      results_per_col[[as.character(prov_id)]] <- prov_results_col
      
      # Analisis gabungan untuk Ringkasan Statistik Gabungan Per Provinsi
      combined_data_prov <- unlist(df_prov %>% dplyr::select(any_of(c("X1", "P0", "P1", "P2", "X2"))))
      combined_data_prov_cleaned <- na.omit(combined_data_prov)
      
      if (length(combined_data_prov_cleaned) > 0) {
        results_gabungan[[as.character(prov_id)]] <- list(
          ringkasan = summary(combined_data_prov_cleaned),
          mean = mean(combined_data_prov_cleaned),
          median = median(combined_data_prov_cleaned),
          sd = sd(combined_data_prov_cleaned),
          n = length(combined_data_prov_cleaned)
        )
      } else {
        results_gabungan[[as.character(prov_id)]] <- "Tidak ada data numerik valid untuk analisis gabungan di provinsi ini."
      }
    }
    analisis_deskriptif_per_prov_col(results_per_col)
    analisis_gabungan_per_prov(results_gabungan)
    showNotification("Analisis deskriptif selesai!", type = "message")
  })
  
  
  # UI dinamis untuk menampilkan ringkasan statistik per provinsi, per kolom
  output$ringkasan_statistik_ui <- renderUI({
    req(analisis_deskriptif_per_prov_col())
    all_results <- analisis_deskriptif_per_prov_col()
    selected_provs <- input$selected_provinsi
    prov_map <- provinsi_mapping()
    
    if (is.null(selected_provs) || length(selected_provs) == 0) {
      return(p("Pilih provinsi dan jalankan analisis untuk melihat ringkasan statistik per kolom."))
    }
    
    tagList(
      lapply(selected_provs, function(prov_id) {
        prov_name <- if (!is.null(prov_map) && as.character(prov_id) %in% names(prov_map)) {
          prov_map[as.character(prov_id)]
        } else {
          paste("ID:", prov_id)
        }
        prov_data_results <- all_results[[as.character(prov_id)]]
        
        if (is.character(prov_data_results)) {
          return(tagList(
            h4(paste("Provinsi:", prov_name)),
            p(prov_data_results, style = "color: red;")
          ))
        }
        
        tagList(
          h4(paste("Ringkasan Statistik untuk Provinsi:", prov_name)),
          hr(),
          lapply(names(prov_data_results), function(col_name) {
            if(is.list(prov_data_results[[col_name]])) {
              tagList(
                h5(paste("Kolom:", col_name)),
                verbatimTextOutput(paste0("ringkasan_prov_", prov_id, "_col_", col_name))
              )
            } else {
              tagList(
                h5(paste("Kolom:", col_name)),
                p(prov_data_results[[col_name]], style = "color: orange;")
              )
            }
          })
        )
      })
    )
  })
  
  # Output renderPrint untuk setiap kombinasi provinsi dan kolom (per kolom)
  observe({
    req(input$selected_provinsi)
    all_results <- analisis_deskriptif_per_prov_col()
    
    if (is.null(all_results)) return()
    
    for (prov_id in input$selected_provinsi) {
      prov_data_results <- all_results[[as.character(prov_id)]]
      if (!is.list(prov_data_results)) next
      
      for (col_name in c("X1", "P0", "P1", "P2", "X2")) {
        local({
          current_prov_id <- prov_id
          current_col_name <- col_name
          output_id <- paste0("ringkasan_prov_", current_prov_id, "_col_", current_col_name)
          
          output[[output_id]] <- renderPrint({
            req(analisis_deskriptif_per_prov_col())
            results_to_print <- analisis_deskriptif_per_prov_col()[[as.character(current_prov_id)]][[current_col_name]]
            
            if (is.list(results_to_print)) {
              cat("--- Ringkasan Statistik ---\n")
              print(results_to_print$ringkasan)
              cat("\nMean              :", results_to_print$mean, "\n")
              cat("Median            :", results_to_print$median, "\n")
              cat("Standar Deviasi   :", results_to_print$sd, "\n")
              cat("Jumlah Observasi  :", results_to_print$n, "\n")
            } else {
              cat(results_to_print)
            }
          })
        })
      }
    }
  })
  
  
  # UI dinamis untuk menampilkan Boxplot Gabungan (per provinsi)
  output$boxplot_gabungan_distribusi_ui <- renderUI({
    req(filtered_data())
    selected_provs <- input$selected_provinsi
    prov_map <- provinsi_mapping()
    
    if (is.null(selected_provs) || length(selected_provs) == 0) {
      return(p("Pilih provinsi dan jalankan analisis untuk melihat plot distribusi."))
    }
    
    tagList(
      lapply(selected_provs, function(prov_id) {
        prov_name <- if (!is.null(prov_map) && as.character(prov_id) %in% names(prov_map)) {
          prov_map[as.character(prov_id)]
        } else {
          paste("ID:", prov_id)
        }
        tagList(
          h3(paste("Distribusi Kolom (X1, P0, P1, P2, X2) untuk Provinsi:", prov_name)),
          hr(),
          plotOutput(paste0("boxplot_faceted_prov_", prov_id), height = "600px") # Memberikan tinggi tetap
        )
      })
    )
  })
  
  # Output renderPlot untuk Boxplot Gabungan (faceted) per provinsi
  observe({
    req(input$selected_provinsi)
    df_filtered_all_provs <- filtered_data()
    
    if (is.null(df_filtered_all_provs) || nrow(df_filtered_all_provs) == 0) return()
    
    prov_map <- provinsi_mapping()
    
    for (prov_id in input$selected_provinsi) {
      local({
        current_prov_id <- prov_id
        current_prov_name <- if (!is.null(prov_map) && as.character(current_prov_id) %in% names(prov_map)) {
          prov_map[as.character(current_prov_id)]
        } else {
          paste("ID:", current_prov_id)
        }
        output_id <- paste0("boxplot_faceted_prov_", current_prov_id)
        
        output[[output_id]] <- renderPlot({
          df_prov_current <- df_filtered_all_provs %>% filter(Provinsi == current_prov_id) %>%
            dplyr::select(any_of(c("X1", "P0", "P1", "P2", "X2"))) # Pilih kolom yang relevan
          
          df_long <- df_prov_current %>%
            pivot_longer(everything(), names_to = "Variabel", values_to = "Nilai") %>%
            na.omit() # Hapus NA untuk plot
          
          if (nrow(df_long) > 1) { # Minimal 2 baris setelah pivot untuk plot bermakna
            ggplot(df_long, aes(x = 1, y = Nilai)) + # x=1 karena setiap facet hanya punya satu boxplot
              geom_boxplot(aes(fill = Variabel), alpha = 0.7, outlier.shape = 1) + # Fill berdasarkan Variabel
              facet_wrap(~ Variabel, scales = "free_y", ncol = 3) + # PENTING: Pisahkan plot per variabel dengan skala Y bebas
              labs(title = paste("Distribusi Variabel Kemiskinan di Provinsi", current_prov_name),
                   x = "", # Sumbu X dikosongkan karena sudah ada facet
                   y = "Nilai") +
              theme_minimal() +
              theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                    axis.title.y = element_text(face = "bold"), # Pastikan judul Y ada
                    axis.text.x = element_blank(), # Sembunyikan label sumbu X
                    axis.ticks.x = element_blank(), # Sembunyikan tanda sumbu X
                    panel.spacing.x = unit(1, "lines"), # Spasi antar facet
                    strip.text = element_text(face = "bold") # Tebalkan judul facet
              )
          } else {
            ggplot() +
              annotate("text", x = 0.5, y = 0.5, label = "Tidak cukup data untuk boxplot ini", size = 6, color = "red") +
              theme_void()
          }
        })
      })
    }
  })
  
  # UI untuk pengaturan ANOVA
  output$provinsi_ui_anova <- renderUI({
    req(input$selected_provinsi)
    prov_map <- provinsi_mapping()
    
    # Membuat label provinsi
    if (!is.null(prov_map)) {
      selected_labels <- paste0(prov_map[as.character(input$selected_provinsi)], 
                                " (", input$selected_provinsi, ")")
    } else {
      selected_labels <- as.character(input$selected_provinsi)
    }
    
    if(length(input$selected_provinsi) >= 2) {
      tagList(
        p("Provinsi yang dipilih untuk analisis ANOVA:", 
          style = "font-weight: bold;"),
        tags$ul(
          lapply(selected_labels, function(label) {
            tags$li(label)
          })
        )
      )
    } else {
      p("Pilih minimal 2 provinsi di tab 'Input Data & Analisis Deskriptif' terlebih dahulu.", 
        style = "color: red; font-weight: bold;")
    }
  })
  
  
  # Reactive ANOVA model - disesuaikan dengan struktur data yang ada
  anova_model <- eventReactive(input$run_anova, {
    validate(
      need(input$selected_provinsi, "Pilih provinsi terlebih dahulu di tab 'Input Data & Analisis Deskriptif'"),
      need(length(input$selected_provinsi) >= 2, "Pilih minimal 2 provinsi"),
      need(input$var_anova, "Pilih variabel dependent")
    )
    
    # Gunakan filtered_data() yang sudah ada di server.R
    df_filtered <- filtered_data()
    req(df_filtered)
    
    # Mapping nama variabel yang user-friendly ke nama kolom asli
    var_mapping <- c(
      "Jumlah Penduduk Miskin" = "X1",
      "Persentase Penduduk Miskin" = "P0",
      "Indeks Kedalaman Kemiskinan" = "P1", 
      "Indeks Keparahan Kemiskinan" = "P2",
      "Garis Kemiskinan" = "X2"
    )
    
    actual_var <- var_mapping[input$var_anova]
    
    df_for_anova <- df_filtered %>%
      filter(!is.na(.data[[actual_var]])) %>%
      mutate(Provinsi = factor(Provinsi))
    
    formula_str <- paste(actual_var, "~ Provinsi")
    model <- aov(as.formula(formula_str), data = df_for_anova)
    
    list(model = model, data = df_for_anova, variable = actual_var, 
         variable_label = input$var_anova)
  })
  

  output$anova_plot <- renderPlot({
    model_data <- anova_model()
    df <- model_data$data
    var_name <- model_data$variable
    var_label <- model_data$variable_label
    prov_map <- provinsi_mapping()
    
    # Tambahkan nama provinsi jika ada mapping
    if (!is.null(prov_map)) {
      df$Provinsi_Label <- paste0(prov_map[as.character(df$Provinsi)], " (", df$Provinsi, ")")
    } else {
      df$Provinsi_Label <- as.character(df$Provinsi)
    }
    
    ggplot(df, aes(x = Provinsi_Label, y = .data[[var_name]], fill = Provinsi_Label)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
      labs(title = paste("Perbandingan", var_label, "Antar Provinsi"),
           subtitle = "Boxplot menunjukkan distribusi data; titik merah = rata-rata",
           x = "Provinsi", y = var_label) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            plot.subtitle = element_text(size = 10, color = "gray60")) +
      scale_fill_brewer(type = "qual", palette = "Set3")
  })
  
  # Enhanced Normality Test with better formatting
  output$normality_test <- renderUI({
    model_data <- anova_model()
    residuals <- residuals(model_data$model)
    
    shapiro_test <- shapiro.test(residuals)
    p_value <- shapiro_test$p.value
    w_statistic <- shapiro_test$statistic
    
    if(p_value > 0.05) {
      conclusion_class <- "status-met"
      conclusion_text <- "ASUMSI TERPENUHI"
      interpretation <- "Residual berdistribusi normal"
      implication <- "Hasil ANOVA dapat dipercaya dan valid secara statistik."
      recommendation <- "Lanjutkan analisis tanpa transformasi data."
      status_icon <- "check-circle"
      status_color <- "#28a745"
    } else {
      conclusion_class <- "status-violated"
      conclusion_text <- "ASUMSI DILANGGAR"
      interpretation <- "Residual tidak berdistribusi normal"
      implication <- "Hasil ANOVA mungkin tidak akurat, terutama untuk sampel kecil."
      recommendation <- "Pertimbangkan transformasi data (log, sqrt) atau gunakan uji non-parametrik (Kruskal-Wallis)."
      status_icon <- "times-circle"
      status_color <- "#dc3545"
    }
    
    HTML(paste0(
      '<div class="assumption-result">',
      '<div style="text-align: center; margin-bottom: 15px;">',
      '</div>',
      
      '<div class="row">',
      '<div class="col-md-6">',
      '<div style="background: #f8f9fa; padding: 12px; border-radius: 6px; margin-bottom: 10px;">',
      '<strong>W-statistic:</strong> ', round(w_statistic, 4), '<br>',
      '<strong>P-value:</strong> ', format(p_value, scientific = TRUE, digits = 4),
      '</div>',
      '</div>',
      '<div class="col-md-6">',
      '<div style="text-align: center;">',
      '<span class="status-indicator ', conclusion_class, '">',
      '<i class="fas fa-', status_icon, '" style="margin-right: 5px;"></i>',
      conclusion_text,
      '</span>',
      '</div>',
      '</div>',
      '</div>',
      
      '<div style="margin-top: 15px; padding: 15px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid ', status_color, ';">',
      '<p style="margin: 5px 0;"><strong>Interpretasi:</strong> ', interpretation, '</p>',
      '<p style="margin: 5px 0;"><strong>Implikasi:</strong> ', implication, '</p>',
      '<p style="margin: 5px 0;"><strong>Rekomendasi:</strong> ', recommendation, '</p>',
      '</div>',
      '</div>'
    ))
  })
  
  # Enhanced Plot outputs with better styling
  output$qq_plot <- renderPlot({
    model_data <- anova_model()
    residuals <- residuals(model_data$model)
    
    # Create Q-Q plot with better styling
    par(bg = "white", mar = c(5, 4, 4, 2) + 0.1)
    qqnorm(residuals, 
           main = "Q-Q Plot: Uji Normalitas Residual",
           sub = "Titik-titik harus mengikuti garis merah untuk distribusi normal",
           xlab = "Theoretical Quantiles", 
           ylab = "Sample Quantiles",
           pch = 16, 
           col = "#667eea", 
           cex = 0.8)
    qqline(residuals, col = "#dc3545", lwd = 3)
    
    # Add grid for better readability
    grid(col = "lightgray", lty = "dotted")
  })
  
  output$residuals_plot <- renderPlot({
    model_data <- anova_model()
    fitted_vals <- fitted(model_data$model)
    residuals <- residuals(model_data$model)
    
    par(bg = "white", mar = c(5, 4, 4, 2) + 0.1)
    plot(fitted_vals, residuals,
         main = "Residuals vs Fitted Values",
         sub = "Titik harus tersebar acak di sekitar garis horizontal untuk homogenitas",
         xlab = "Fitted Values", 
         ylab = "Residuals",
         pch = 16, 
         col = "#667eea", 
         cex = 0.8)
    abline(h = 0, col = "#dc3545", lwd = 3)
    
    # Add smoothed line to detect patterns
    lines(lowess(fitted_vals, residuals), col = "#ffc107", lwd = 2)
    
    # Add grid
    grid(col = "lightgray", lty = "dotted")
    
    legend("topright", 
           legend = c("Residuals", "Zero line", "Trend line"), 
           col = c("#667eea", "#dc3545", "#ffc107"), 
           lty = c(NA, 1, 1), 
           pch = c(16, NA, NA),
           cex = 0.8,
           bg = "white")
  })
  
  # Enhanced main ANOVA plot
  output$anova_plot <- renderPlot({
    model_data <- anova_model()
    df <- model_data$data
    var_name <- model_data$variable
    var_label <- model_data$variable_label
    prov_map <- provinsi_mapping()
    
    # Add province names if mapping exists
    if (!is.null(prov_map)) {
      df$Provinsi_Label <- paste0(prov_map[as.character(df$Provinsi)], " (", df$Provinsi, ")")
    } else {
      df$Provinsi_Label <- as.character(df$Provinsi)
    }
    
    # Create enhanced ggplot
    p <- ggplot(df, aes(x = Provinsi_Label, y = .data[[var_name]], fill = Provinsi_Label)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2, outlier.alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "red", color = "darkred") +
      labs(title = paste("Perbandingan", var_label, "Antar Provinsi"),
           subtitle = "Boxplot menunjukkan distribusi data; titik merah = rata-rata; titik = outlier",
           x = "Provinsi", 
           y = var_label) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, color = "gray60", hjust = 0.5),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray90", size = 0.5),
        panel.grid.minor = element_line(color = "gray95", size = 0.3),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      scale_fill_brewer(type = "qual", palette = "Set3")
    
    print(p)
  })
  
  # Enhanced Tukey Plot
  output$tukey_plot <- renderPlot({
    req(input$run_anova)
    
    tryCatch({
      model_data <- anova_model()
      anova_result <- summary(model_data$model)
      p_value <- anova_result[[1]][1, "Pr(>F)"]
      
      if(!is.na(p_value) && p_value < 0.05) {
        tukey_result <- TukeyHSD(model_data$model, conf.level = 0.95)
        tukey_df <- as.data.frame(tukey_result$Provinsi)
        tukey_df$comparison <- rownames(tukey_df)
        prov_map <- provinsi_mapping()
        
        # Convert comparison names to province names
        if (!is.null(prov_map)) {
          tukey_df$comparison_labeled <- sapply(tukey_df$comparison, function(comp) {
            parts <- strsplit(comp, "-")[[1]]
            if (length(parts) == 2) {
              prov1_name <- prov_map[parts[1]]
              prov2_name <- prov_map[parts[2]]
              
              if (!is.na(prov1_name) && !is.na(prov2_name)) {
                return(paste(prov1_name, "vs", prov2_name))
              }
            }
            return(comp)
          })
        } else {
          tukey_df$comparison_labeled <- gsub("-", " vs ", tukey_df$comparison)
        }
        
        # Order by difference for better visualization
        tukey_df$comparison_labeled <- factor(tukey_df$comparison_labeled, 
                                              levels = tukey_df$comparison_labeled[order(tukey_df$diff)])
        
        p <- ggplot(tukey_df, aes(y = comparison_labeled)) +
          geom_point(aes(x = diff), color = "#2c5282", size = 3) +
          geom_errorbarh(aes(xmin = lwr, xmax = upr), 
                         color = "#2c5282", 
                         height = 0.3,
                         size = 1.2) +
          geom_vline(xintercept = 0, 
                     color = "#dc3545", 
                     linetype = "dashed", 
                     size = 1.2) +
          labs(title = "Tukey HSD: Interval Kepercayaan 95%",
               subtitle = "Interval yang tidak melewati garis 0 menunjukkan perbedaan signifikan",
               x = "Perbedaan Rata-rata",
               y = "Perbandingan Provinsi") +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 14, color = "#2c5282", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#6c757d"),
            axis.text.y = element_text(size = 10, color = "#4a5568"),
            axis.text.x = element_text(size = 10, color = "#4a5568"),
            axis.title = element_text(size = 12, color = "#2d3748", face = "bold"),
            panel.grid.major.y = element_line(color = "lightgray", linetype = "dotted"),
            panel.grid.major.x = element_line(color = "lightgray", linetype = "dotted"),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(20, 20, 20, 40)  # Extra margin for labels
          )
        
        print(p)
        
      } else {
        # ggplot version of no result message
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Tukey HSD tidak diperlukan\n(ANOVA tidak signifikan)",
                   size = 6, color = "#6c757d") +
          theme_void() +
          theme(plot.background = element_rect(fill = "white", color = NA))
      }
      
    }, error = function(e) {
      # ggplot error handling
      ggplot() + 
        annotate("text", x = 0.5, y = 0.7, 
                 label = "Error dalam membuat plot Tukey HSD",
                 size = 5, color = "#dc3545") +
        annotate("text", x = 0.5, y = 0.3, 
                 label = paste("Detail:", e$message),
                 size = 4, color = "#6c757d") +
        theme_void() +
        theme(plot.background = element_rect(fill = "white", color = NA))
    })
  })
  
  # Enhanced ANOVA Table with better formatting
  output$anova_table <- renderTable({
    model_data <- anova_model()
    anova_result <- summary(model_data$model)
    anova_table <- anova_result[[1]]
    
    # Clean up the table
    rownames(anova_table) <- c("Antar Provinsi", "Dalam Provinsi (Error)")
    colnames(anova_table) <- c("df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    
    # Format the table
    anova_table[, "Sum Sq"] <- round(anova_table[, "Sum Sq"], 4)
    anova_table[, "Mean Sq"] <- round(anova_table[, "Mean Sq"], 4)
    anova_table[, "F value"] <- round(anova_table[, "F value"], 4)
    anova_table[, "Pr(>F)"] <- format(anova_table[, "Pr(>F)"], scientific = TRUE, digits = 4)
    
    anova_table
  }, 
  rownames = TRUE, 
  digits = 4,
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  spacing = "s")
  
  # Enhanced Homogeneity Test dengan status box berwarna
  output$homogeneity_test <- renderUI({
    model_data <- anova_model()
    df <- model_data$data
    var_name <- model_data$variable
    
    formula_str <- paste(var_name, "~ Provinsi")
    levene_test <- car::leveneTest(as.formula(formula_str), data = df)
    p_value <- levene_test$`Pr(>F)`[1]
    f_statistic <- levene_test$`F value`[1]
    df1 <- levene_test$Df[1]
    df2 <- levene_test$Df[2]
    
    if(p_value > 0.05) {
      conclusion_class <- "status-met"
      conclusion_text <- "ASUMSI TERPENUHI"
      interpretation <- "Varians homogen antar grup"
      implication <- "ANOVA adalah uji yang tepat untuk data ini."
      recommendation <- "Lanjutkan dengan interpretasi hasil ANOVA."
      status_icon <- "check-circle"
      status_color <- "#28a745"
      status_bg_color <- "#d4edda"
      status_border_color <- "#c3e6cb"
      status_text_color <- "#155724"
    } else {
      conclusion_class <- "status-violated"
      conclusion_text <- "ASUMSI DILANGGAR"
      interpretation <- "Varians tidak homogen antar grup"
      implication <- "Hasil ANOVA mungkin bias, terutama jika ukuran sampel tidak seimbang."
      recommendation <- "Pertimbangkan transformasi data atau gunakan uji alternatif seperti Welch ANOVA."
      status_icon <- "times-circle"
      status_color <- "#dc3545"
      status_bg_color <- "#f8d7da"
      status_border_color <- "#f5c6cb"
      status_text_color <- "#721c24"
    }
    
    HTML(paste0(
      '<div class="assumption-result">',
      
      # Statistics and Status Section
      '<div class="row">',
      '<div class="col-md-6">',
      '<div style="background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 10px; border: 1px solid #dee2e6;">',
      '<div style="margin-bottom: 8px;">',
      '<strong style="color: #2c5282;">F-statistic:</strong> ',
      '<span style="font-family: monospace; color: #495057;">F(', df1, ', ', df2, ') = ', round(f_statistic, 4), '</span>',
      '</div>',
      '<div>',
      '<strong style="color: #2c5282;">P-value:</strong> ',
      '<span style="font-family: monospace; color: #495057;">', format(p_value, scientific = TRUE, digits = 4), '</span>',
      '</div>',
      '</div>',
      '</div>',
      
      # Status Box dengan warna sesuai kondisi
      '<div class="col-md-6">',
      '<div style="background: ', status_bg_color, '; padding: 15px; border-radius: 8px; margin-bottom: 10px; border: 2px solid ', status_border_color, '; text-align: center;">',
      '<div style="margin-bottom: 8px;">',
      '<i class="fas fa-', status_icon, '" style="color: ', status_color, '; font-size: 20px; margin-right: 8px;"></i>',
      '</div>',
      '<div style="font-weight: 700; font-size: 14px; color: ', status_text_color, '; letter-spacing: 0.5px;">',
      conclusion_text,
      '</div>',
      '</div>',
      '</div>',
      '</div>',
      
      # Interpretation Section
      '<div style="margin-top: 15px; padding: 20px; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 10px; border-left: 4px solid ', status_color, '; box-shadow: 0 2px 4px rgba(0,0,0,0.05);">',
      '<h6 style="color: #2c5282; margin-bottom: 15px; font-weight: 600;">',
      '<i class="fas fa-info-circle" style="margin-right: 8px;"></i>Interpretasi Hasil',
      '</h6>',
      '<div style="margin-bottom: 12px;">',
      '<strong style="color: #495057;">Interpretasi:</strong> ',
      '<span style="color: #6c757d;">', interpretation, '</span>',
      '</div>',
      '<div style="margin-bottom: 12px;">',
      '<strong style="color: #495057;">Implikasi:</strong> ',
      '<span style="color: #6c757d;">', implication, '</span>',
      '</div>',
      '<div>',
      '<strong style="color: #495057;">Rekomendasi:</strong> ',
      '<span style="color: #6c757d;">', recommendation, '</span>',
      '</div>',
      '</div>',
      '</div>'
    ))
  })
  
  # Enhanced Normality Test dengan format yang sama
  output$normality_test <- renderUI({
    model_data <- anova_model()
    residuals <- residuals(model_data$model)
    
    shapiro_test <- shapiro.test(residuals)
    p_value <- shapiro_test$p.value
    w_statistic <- shapiro_test$statistic
    
    if(p_value > 0.05) {
      conclusion_class <- "status-met"
      conclusion_text <- "ASUMSI TERPENUHI"
      interpretation <- "Residual berdistribusi normal"
      implication <- "Hasil ANOVA dapat dipercaya dan valid secara statistik."
      recommendation <- "Lanjutkan analisis tanpa transformasi data."
      status_icon <- "check-circle"
      status_color <- "#28a745"
      status_bg_color <- "#d4edda"
      status_border_color <- "#c3e6cb"
      status_text_color <- "#155724"
    } else {
      conclusion_class <- "status-violated"
      conclusion_text <- "ASUMSI DILANGGAR"
      interpretation <- "Residual tidak berdistribusi normal"
      implication <- "Hasil ANOVA mungkin tidak akurat, terutama untuk sampel kecil."
      recommendation <- "Pertimbangkan transformasi data (log, sqrt) atau gunakan uji non-parametrik (Kruskal-Wallis)."
      status_icon <- "times-circle"
      status_color <- "#dc3545"
      status_bg_color <- "#f8d7da"
      status_border_color <- "#f5c6cb"
      status_text_color <- "#721c24"
    }
    
    HTML(paste0(
      '<div class="assumption-result">',
      
      # Statistics and Status Section
      '<div class="row">',
      '<div class="col-md-6">',
      '<div style="background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 10px; border: 1px solid #dee2e6;">',
      '<div style="margin-bottom: 8px;">',
      '<strong style="color: #2c5282;">W-statistic:</strong> ',
      '<span style="font-family: monospace; color: #495057;">', round(w_statistic, 4), '</span>',
      '</div>',
      '<div>',
      '<strong style="color: #2c5282;">P-value:</strong> ',
      '<span style="font-family: monospace; color: #495057;">', format(p_value, scientific = TRUE, digits = 4), '</span>',
      '</div>',
      '</div>',
      '</div>',
      
      # Status Box dengan warna sesuai kondisi
      '<div class="col-md-6">',
      '<div style="background: ', status_bg_color, '; padding: 15px; border-radius: 8px; margin-bottom: 10px; border: 2px solid ', status_border_color, '; text-align: center;">',
      '<div style="margin-bottom: 8px;">',
      '<i class="fas fa-', status_icon, '" style="color: ', status_color, '; font-size: 20px; margin-right: 8px;"></i>',
      '</div>',
      '<div style="font-weight: 700; font-size: 14px; color: ', status_text_color, '; letter-spacing: 0.5px;">',
      conclusion_text,
      '</div>',
      '</div>',
      '</div>',
      '</div>',
      
      # Interpretation Section
      '<div style="margin-top: 15px; padding: 20px; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 10px; border-left: 4px solid ', status_color, '; box-shadow: 0 2px 4px rgba(0,0,0,0.05);">',
      '<h6 style="color: #2c5282; margin-bottom: 15px; font-weight: 600;">',
      '<i class="fas fa-info-circle" style="margin-right: 8px;"></i>Interpretasi Hasil',
      '</h6>',
      '<div style="margin-bottom: 12px;">',
      '<strong style="color: #495057;">Interpretasi:</strong> ',
      '<span style="color: #6c757d;">', interpretation, '</span>',
      '</div>',
      '<div style="margin-bottom: 12px;">',
      '<strong style="color: #495057;">Implikasi:</strong> ',
      '<span style="color: #6c757d;">', implication, '</span>',
      '</div>',
      '<div>',
      '<strong style="color: #495057;">Rekomendasi:</strong> ',
      '<span style="color: #6c757d;">', recommendation, '</span>',
      '</div>',
      '</div>',
      '</div>'
    ))
  })
  
  # Enhanced ANOVA Summary dengan tema biru netral
  output$anova_summary <- renderUI({
    model_data <- anova_model()
    anova_result <- summary(model_data$model)
    p_value <- anova_result[[1]][1, "Pr(>F)"]
    f_value <- anova_result[[1]][1, "F value"]
    df1 <- anova_result[[1]][1, "Df"]
    df2 <- anova_result[[1]][2, "Df"]
    
    # Determine significance level with blue theme
    if(p_value < 0.001) {
      sig_level <- "sangat signifikan (p < 0.001)"
      status_icon <- "exclamation-circle"
      status_color <- "#28a745"  # Red only for very significant results
      result_bg <- "background: linear-gradient(135deg, #e6ffe6 0%, #f0fff4 100%);"
    } else if(p_value < 0.01) {
      sig_level <- "sangat signifikan (p < 0.01)"
      status_icon <- "exclamation-circle"
      status_color <- "#28a745"  # Red only for very significant results
      result_bg <- "background: linear-gradient(135deg, #e6ffe6 0%, #f0fff4 100%);"
    } else if(p_value < 0.05) {
      sig_level <- "signifikan (p < 0.05)"
      status_icon <- "check-circle"
      status_color <- "#28a745"  # Green for significant results
      result_bg <- "background: linear-gradient(135deg, #e6ffe6 0%, #f0fff4 100%);"
    } else {
      sig_level <- "tidak signifikan (p ≥ 0.05)"
      status_icon <- "minus-circle"
      status_color <- "#6c757d"  # Gray for non-significant
      result_bg <- "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);"
    }
    
    # Effect size calculation (Eta squared) - neutral colors
    ss_between <- anova_result[[1]][1, "Sum Sq"]
    ss_total <- sum(anova_result[[1]][, "Sum Sq"])
    eta_squared <- ss_between / ss_total
    
    effect_size_interpretation <- if(eta_squared < 0.01) {
      list(text = "kecil (< 1%)", color = "#6c757d")
    } else if(eta_squared < 0.06) {
      list(text = "sedang (1-6%)", color = "#495057")
    } else if(eta_squared < 0.14) {
      list(text = "besar (6-14%)", color = "#343a40")
    } else {
      list(text = "sangat besar (> 14%)", color = "#212529")
    }
    
    if(p_value < 0.05) {
      result_text <- "SIGNIFIKAN"
      interpretation <- paste0("Terdapat perbedaan rata-rata ", model_data$variable_label, 
                               " yang signifikan antar provinsi yang dipilih")
      recommendation <- "Lanjutkan dengan uji Post-Hoc (Tukey HSD) untuk mengetahui provinsi mana yang berbeda secara spesifik."
    } else {
      result_text <- "TIDAK SIGNIFIKAN"
      interpretation <- paste0("Tidak terdapat perbedaan rata-rata ", model_data$variable_label, 
                               " yang signifikan antar provinsi yang dipilih")
      recommendation <- "Tidak perlu melakukan uji Post-Hoc. Provinsi-provinsi yang dipilih memiliki rata-rata yang relatif sama."
    }
    
    HTML(paste0(
      '<div class="anova-summary">',
      '<div style="text-align: center; margin-bottom: 20px;">',
      '<h4><i class="fas fa-calculator" style="color: #2c5282; margin-right: 10px;"></i>',
      'Hasil ANOVA untuk ', model_data$variable_label, '</h4>',
      '</div>',
      
      # Statistical Results in Cards - Blue theme
      '<div class="row" style="margin-bottom: 20px;">',
      '<div class="col-md-3">',
      '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 15px; border-radius: 10px; text-align: center; border-left: 4px solid #2c5282;">',
      '<h6 style="color: #2c5282; margin-bottom: 5px;">F-Statistic</h6>',
      '<strong style="font-size: 1.2em; color: #495057;">F(', df1, ', ', df2, ') = ', round(f_value, 3), '</strong>',
      '</div>',
      '</div>',
      '<div class="col-md-3">',
      '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 15px; border-radius: 10px; text-align: center; border-left: 4px solid #2c5282;">',
      '<h6 style="color: #2c5282; margin-bottom: 5px;">P-Value</h6>',
      '<strong style="font-size: 1.2em; color: #495057;">', format(p_value, scientific = FALSE, digits = 4), '</strong>',
      '</div>',
      '</div>',
      '<div class="col-md-3">',
      '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 15px; border-radius: 10px; text-align: center; border-left: 4px solid ', effect_size_interpretation$color, ';">',
      '<h6 style="color: ', effect_size_interpretation$color, '; margin-bottom: 5px;">Effect Size (η²)</h6>',
      '<strong style="font-size: 1.2em; color: #495057;">', round(eta_squared, 4), '</strong><br>',
      '</div>',
      '</div>',
      '<div class="col-md-3">',
      '<div style="', result_bg, ' padding: 15px; border-radius: 10px; text-align: center; border-left: 4px solid ', status_color, ';">',
      '<h6 style="color: ', status_color, '; margin-bottom: 5px;">Status</h6>',
      '<strong style="font-size: 1.1em; color: ', status_color, ';">',
      '<i class="fas fa-', status_icon, '" style="margin-right: 5px;"></i>',
      result_text, '</strong>',
      '</div>',
      '</div>',
      '</div>',
      
      # Interpretation Box - Blue theme
      '<div style="background: linear-gradient(135deg, #ebf8ff 0%, #f7fafc 100%); padding: 20px; border-radius: 10px; border-left: 4px solid #2c5282;">',
      '<h5 style="color: #2c5282; margin-bottom: 15px;">',
      '<i class="fas fa-lightbulb" style="margin-right: 8px;"></i>Interpretasi</h5>',
      '<p style="margin: 8px 0; font-size: 14px; color: #4a5568;"><strong>Tingkat Signifikansi:</strong> ', sig_level, '</p>',
      '<p style="margin: 8px 0; font-size: 14px; color: #4a5568;"><strong>Kesimpulan:</strong> ', interpretation, '</p>',
      '<p style="margin: 8px 0; font-size: 14px; color: #4a5568;"><strong>Rekomendasi:</strong> ', recommendation, '</p>',
      '</div>',
      '</div>'
    ))
  })
  
  # Enhanced Model Summary dengan tema netral biru
  output$model_summary <- renderUI({
    model_data <- anova_model()
    
    # Calculate statistics
    formula_str <- paste(model_data$variable, "~ Provinsi")
    lm_model <- lm(as.formula(formula_str), data = model_data$data)
    r_squared <- summary(lm_model)$r.squared
    adj_r_squared <- summary(lm_model)$adj.r.squared
    
    n_obs <- nrow(model_data$data)
    n_groups <- length(unique(model_data$data$Provinsi))
    
    # Interpret R-squared with neutral colors
    if(r_squared < 0.30) {
      r_interpretation <- list(text = "lemah (< 30%)", color = "#6c757d")
    } else if(r_squared < 0.70) {
      r_interpretation <- list(text = "sedang (30-70%)", color = "#495057")
    } else if(r_squared < 0.90) {
      r_interpretation <- list(text = "kuat (70-90%)", color = "#343a40")
    } else {
      r_interpretation <- list(text = "sangat kuat (> 90%)", color = "#212529")
    }
    
    HTML(paste0(
      '<div class="model-summary">',
      '<div style="text-align: center; margin-bottom: 20px;">',
      '</div>',
      
      # Model Info Cards - Blue theme
      '<div class="row" style="margin-bottom: 15px;">',
      '<div class="col-md-4">',
      '<div style="background: linear-gradient(135deg, #ebf8ff 0%, #f7fafc 100%); padding: 12px; border-radius: 8px; text-align: center; border-left: 4px solid #2c5282;">',
      '<h6 style="color: #2c5282; margin-bottom: 5px;">Observasi</h6>',
      '<strong style="font-size: 1.3em; color: #4a5568;">', n_obs, '</strong>',
      '</div>',
      '</div>',
      '<div class="col-md-4">',
      '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 12px; border-radius: 8px; text-align: center; border-left: 4px solid #4a5568;">',
      '<h6 style="color: #4a5568; margin-bottom: 5px;">Provinsi</h6>',
      '<strong style="font-size: 1.3em; color: #2d3748;">', n_groups, '</strong>',
      '</div>',
      '</div>',
      '<div class="col-md-4">',
      '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 12px; border-radius: 8px; text-align: center; border-left: 4px solid #2c5282;">',
      '<h6 style="color: #2c5282; margin-bottom: 5px;">Variabel</h6>',
      '<strong style="font-size: 1.1em; color: #4a5568;">', model_data$variable_label, '</strong>',
      '</div>',
      '</div>',
      '</div>',
      
      # Performance Metrics - Neutral colors
      '<div class="row">',
      '<div class="col-md-6">',
      '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 15px; border-radius: 8px; border-left: 4px solid ', r_interpretation$color, ';">',
      '<h6 style="color: ', r_interpretation$color, '; margin-bottom: 10px;">R-squared</h6>',
      '<div style="display: flex; align-items: center; justify-content: space-between;">',
      '<strong style="font-size: 1.5em; color: #495057;">', round(r_squared, 4), '</strong>',
      '<span style="color: ', r_interpretation$color, '; font-weight: bold;">', round(r_squared * 100, 1), '%</span>',
      '</div>',
      '<small style="color: ', r_interpretation$color, ';">Kekuatan: ', r_interpretation$text, '</small>',
      '</div>',
      '</div>',
      '<div class="col-md-6">',
      '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 15px; border-radius: 8px; border-left: 4px solid #6c757d;">',
      '<h6 style="color: #6c757d; margin-bottom: 10px;">Adjusted R-squared</h6>',
      '<div style="display: flex; align-items: center; justify-content: space-between;">',
      '<strong style="font-size: 1.5em; color: #495057;">', round(adj_r_squared, 4), '</strong>',
      '<span style="color: #6c757d; font-weight: bold;">', round(adj_r_squared * 100, 1), '%</span>',
      '</div>',
      '<small style="color: #6c757d;">Disesuaikan untuk jumlah variabel</small>',
      '</div>',
      '</div>',
      '</div>',
      
      # Interpretation - Blue theme
      '<div style="margin-top: 20px; padding: 15px; background: linear-gradient(135deg, #ebf8ff 0%, #f7fafc 100%); border-radius: 8px; border-left: 4px solid #2c5282;">',
      '<h6 style="color: #2c5282; margin-bottom: 10px;"><i class="fas fa-info-circle" style="margin-right: 5px;"></i>Interpretasi Model</h6>',
      '<p style="margin: 5px 0; font-size: 14px; color: #4a5568;">Model ini dapat menjelaskan <strong>', round(r_squared * 100, 1), 
      '%</strong> variabilitas dalam ', model_data$variable_label, ' berdasarkan perbedaan provinsi.</p>',
      '<p style="margin: 5px 0; font-size: 14px; color: #4a5568;">Sisa <strong>', round((1-r_squared) * 100, 1), 
      '%</strong> variabilitas dijelaskan oleh faktor lain yang tidak termasuk dalam model.</p>',
      '</div>',
      '</div>'
    ))
  })
  
  # Enhanced Tukey Results dengan tema netral biru
  output$tukey_results <- renderUI({
    req(input$run_anova)
    
    tryCatch({
      model_data <- anova_model()
      anova_result <- summary(model_data$model)
      p_value <- anova_result[[1]][1, "Pr(>F)"]
      
      if(!is.na(p_value) && p_value < 0.05) {
        tukey_result <- TukeyHSD(model_data$model, conf.level = 0.95)
        tukey_data <- tukey_result$Provinsi
        
        # Count significant comparisons
        significant_count <- sum(tukey_data[, "p adj"] < 0.05, na.rm = TRUE)
        total_comparisons <- nrow(tukey_data)
        
        # Create summary statistics - Blue theme
        results_html <- paste0(
          '<div style="text-align: center; margin-bottom: 20px;">',
          '<h5><i class="fas fa-search-plus" style="color: #2c5282; margin-right: 10px;"></i>Hasil Tukey HSD</h5>',
          '<p style="color: #6c757d; margin: 0;">Tingkat Kepercayaan: 95%</p>',
          '</div>',
          
          # Summary Cards - Neutral blue theme
          '<div class="row" style="margin-bottom: 20px;">',
          '<div class="col-md-4">',
          '<div style="background: linear-gradient(135deg, #ebf8ff 0%, #f7fafc 100%); padding: 15px; border-radius: 10px; text-align: center; border-left: 4px solid #2c5282;">',
          '<h6 style="color: #2c5282; margin-bottom: 5px;">Total Perbandingan</h6>',
          '<strong style="font-size: 1.5em; color: #4a5568;">', total_comparisons, '</strong>',
          '</div>',
          '</div>',
          '<div class="col-md-4">',
          '<div style="background: linear-gradient(135deg, #f0fff4 0%, #f7fafc 100%); padding: 15px; border-radius: 10px; text-align: center; border-left: 4px solid #28a745;">',
          '<h6 style="color: #28a745; margin-bottom: 5px;">Signifikan</h6>',
          '<strong style="font-size: 1.5em; color: #4a5568;">', significant_count, '</strong><br>',
          '</div>',
          '</div>',
          '<div class="col-md-4">',
          '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 15px; border-radius: 10px; text-align: center; border-left: 4px solid #6c757d;">',
          '<h6 style="color: #6c757d; margin-bottom: 5px;">Tidak Signifikan</h6>',
          '<strong style="font-size: 1.5em; color: #4a5568;">', total_comparisons - significant_count, '</strong><br>',
          '</div>',
          '</div>',
          '</div>'
        )
        
        # General interpretation - Neutral colors
        if(significant_count > 0) {
          if(significant_count == total_comparisons) {
            general_interpretation <- "Semua provinsi berbeda signifikan satu sama lain."
            interp_color <- "#4a5568"
            interp_bg <- "background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%);"
          } else if(significant_count > total_comparisons/2) {
            general_interpretation <- "Sebagian besar provinsi menunjukkan perbedaan yang signifikan."
            interp_color <- "#4a5568"
            interp_bg <- "background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%);"
          } else {
            general_interpretation <- "Hanya sebagian kecil provinsi yang menunjukkan perbedaan signifikan."
            interp_color <- "#4a5568"
            interp_bg <- "background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%);"
          }
        } else {
          general_interpretation <- "Tidak ada perbedaan berpasangan yang signifikan setelah koreksi multiple comparison."
          interp_color <- "#6c757d"
          interp_bg <- "background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%);"
        }
        
        results_html <- paste0(results_html,
                               '<div style="', interp_bg, ' padding: 15px; border-radius: 10px; border-left: 4px solid #2c5282;">',
                               '<h6 style="color: #2c5282; margin-bottom: 10px;">',
                               '<i class="fas fa-lightbulb" style="margin-right: 5px;"></i>Interpretasi Umum</h6>',
                               '<p style="margin: 0; font-size: 14px; color: ', interp_color, ';">', general_interpretation, '</p>',
                               '<p style="margin: 10px 0 0 0; font-size: 12px; color: #6c757d;">',
                               '<em>Catatan: Nilai p yang sudah disesuaikan (p adj) mengontrol Family-wise Error Rate.</em></p>',
                               '</div>')
        
        HTML(results_html)
      }
    }, error = function(e) {
      HTML(paste0('<div style="background: #fed7d7; color: #721c24; padding: 15px; border-radius: 10px; border-left: 4px solid #dc3545;">',
                  '<h6><i class="fas fa-exclamation-triangle" style="margin-right: 5px;"></i>Error</h6>',
                  '<p style="margin: 0;">Error dalam menjalankan Tukey HSD: ', e$message, '</p>',
                  '</div>'))
    })
  })
  
  output$show_tukey <- reactive({
    req(input$run_anova)
    tryCatch({
      model_data <- anova_model()
      anova_result <- summary(model_data$model)
      p_value <- anova_result[[1]][1, "Pr(>F)"]
      
      result <- !is.na(p_value) && p_value < 0.05
      print(paste("DEBUG show_tukey: p_value =", p_value, ", result =", result)) # Debug line
      return(result)
    }, error = function(e) {
      print(paste("ERROR in show_tukey:", e$message)) # Debug line
      return(FALSE)
    })
  })
  outputOptions(output, "show_tukey", suspendWhenHidden = FALSE)
  
  # 2. WAJIB: Interpretasi lengkap (yang hilang!)
  # Enhanced Interpretation dengan tema netral biru - CORRECTED
  output$interpretation <- renderUI({
    req(input$run_anova)
    
    tryCatch({
      model_data <- anova_model()
      anova_result <- summary(model_data$model)
      p_value <- anova_result[[1]][1, "Pr(>F)"]
      prov_map <- provinsi_mapping()
      
      if(!is.na(p_value) && p_value < 0.05) {
        tukey_result <- TukeyHSD(model_data$model, conf.level = 0.95)
        tukey_df <- as.data.frame(tukey_result$Provinsi)
        
        # Calculate group means for better interpretation
        group_means <- model_data$data %>%
          group_by(Provinsi) %>%
          summarise(mean_val = mean(.data[[model_data$variable]], na.rm = TRUE),
                    .groups = 'drop') %>%
          arrange(desc(mean_val))
        
        # Add province names if mapping exists
        if (!is.null(prov_map)) {
          group_means$Provinsi_Name <- prov_map[as.character(group_means$Provinsi)]
          group_means$Display_Name <- ifelse(is.na(group_means$Provinsi_Name), 
                                             as.character(group_means$Provinsi),
                                             group_means$Provinsi_Name)
        } else {
          group_means$Display_Name <- as.character(group_means$Provinsi)
        }
        
        # Identify highest and lowest performing provinces
        highest_prov <- group_means$Display_Name[1]
        lowest_prov <- group_means$Display_Name[nrow(group_means)]
        
        # Identify significant pairs
        significant_pairs <- rownames(tukey_df)[tukey_df$`p adj` < 0.05]
        non_significant_pairs <- rownames(tukey_df)[tukey_df$`p adj` >= 0.05]
        
        interpretation_html <- paste0(
          '<div style="background: linear-gradient(135deg, #ffffff 0%, #f7fafc 100%); border: 2px solid #2c5282; border-radius: 15px; padding: 30px; margin: 20px 0; box-shadow: 0 6px 12px rgba(44, 82, 130, 0.1);">',
          '<h5 style="color: #2c5282; text-align: center; margin-bottom: 20px;">',
          '<i class="fas fa-lightbulb" style="margin-right: 10px;"></i>INTERPRETASI KOMPREHENSIF HASIL TUKEY HSD</h5>',
          
          '<div style="background: linear-gradient(135deg, #ebf8ff 0%, #f7fafc 100%); padding: 15px; border-radius: 6px; margin: 15px 0; text-align: center; font-weight: bold; border-left: 4px solid #2c5282;">',
          '<p style="margin: 0; color: #2c5282;"><strong>Variabel yang Dianalisis:</strong> ', model_data$variable_label, '</p>',
          '</div>',
          
          '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #4a5568;">',
          '<h6 style="color: #4a5568;">Peringkat Provinsi:</h6>',
          '<p style="margin: 5px 0; color: #2d3748;"><strong>Tertinggi:</strong> ', highest_prov, ' (', round(group_means$mean_val[1], 2), ')</p>',
          '<p style="margin: 5px 0; color: #2d3748;"><strong>Terendah:</strong> ', lowest_prov, ' (', round(group_means$mean_val[nrow(group_means)], 2), ')</p>',
          '</div>'
        )
        
        if(length(significant_pairs) > 0) {
          # Detailed significant comparisons - minimal green
          significant_details <- sapply(significant_pairs, function(pair) {
            idx <- which(rownames(tukey_df) == pair)
            diff_value <- tukey_df[idx, "diff"]
            p_adj_value <- tukey_df[idx, "p adj"]
            lwr <- tukey_df[idx, "lwr"]
            upr <- tukey_df[idx, "upr"]
            
            direction <- ifelse(diff_value > 0, "lebih tinggi", "lebih rendah")
            
            # Convert province names if mapping exists
            if (!is.null(prov_map)) {
              pair_parts <- strsplit(pair, "-")[[1]]
              prov1_name <- prov_map[pair_parts[1]]
              prov2_name <- prov_map[pair_parts[2]]
              if (!is.na(prov1_name) && !is.na(prov2_name)) {
                pair_clean <- paste(prov1_name, "vs", prov2_name)
              } else {
                pair_clean <- gsub("-", " vs ", pair)
              }
            } else {
              pair_clean <- gsub("-", " vs ", pair)
            }
            
            paste0(
              '<div style="background: linear-gradient(135deg, #f0fff4 0%, #f7fafc 100%); padding: 12px; border-radius: 6px; border-left: 3px solid #28a745; margin: 8px 0;">',
              '<strong style="color: #2d3748;">', pair_clean, '</strong><br>',
              '<small style="color: #4a5568;">• Perbedaan rata-rata: <strong>', round(abs(diff_value), 4), '</strong> (', direction, ')<br>',
              '• Interval kepercayaan 95%: [', round(lwr, 4), ', ', round(upr, 4), ']<br>',
              '• P-value (disesuaikan): <strong>', format(p_adj_value, scientific = TRUE, digits = 3), '</strong><br>',
              '• <em style="color: #28a745;">Perbedaan signifikan secara statistik</em></small>',
              '</div>'
            )
          })
          
          interpretation_html <- paste0(
            interpretation_html,
            '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #2c5282;">',
            '<h6 style="color: #2c5282;"><i class="fas fa-check-circle" style="margin-right: 5px;"></i>Perbandingan yang SIGNIFIKAN (p < 0.05):</h6>',
            paste(significant_details, collapse = ''),
            '</div>'
          )
          
          # Practical significance assessment
          effect_sizes <- abs(tukey_df[significant_pairs, "diff"])
          avg_effect_size <- mean(effect_sizes)
          
          practical_interpretation <- if(avg_effect_size < sd(model_data$data[[model_data$variable]], na.rm = TRUE) * 0.2) {
            "kecil secara praktis"
          } else if(avg_effect_size < sd(model_data$data[[model_data$variable]], na.rm = TRUE) * 0.5) {
            "sedang secara praktis"
          } else {
            "besar secara praktis"
          }
          
          interpretation_html <- paste0(
            interpretation_html,
            '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #4a5568;">',
            '<h6 style="color: #4a5568;"><i class="fas fa-ruler" style="margin-right: 5px;"></i>Signifikansi Praktis:</h6>',
            '<p style="margin: 0; color: #4a5568;">Rata-rata perbedaan yang signifikan adalah <strong>', practical_interpretation, '</strong>.</p>',
            '</div>'
          )
        }
        
        if(length(non_significant_pairs) > 0) {
          interpretation_html <- paste0(
            interpretation_html,
            '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 15px; border-radius: 6px; margin: 15px 0; border-left: 4px solid #6c757d;">',
            '<h6 style="color: #6c757d;"><i class="fas fa-equals" style="margin-right: 5px;"></i>Perbandingan yang TIDAK Signifikan:</h6>',
            '<p style="margin: 0; color: #4a5568;"><strong>', length(non_significant_pairs), '</strong> pasangan provinsi tidak menunjukkan perbedaan yang signifikan.</p>',
            '<p style="margin: 5px 0 0 0; color: #6c757d;"><em>Provinsi-provinsi ini memiliki rata-rata yang secara statistik sama.</em></p>',
            '</div>'
          )
        }
        
        # Overall conclusion - Blue theme
        if(length(significant_pairs) > 0) {
          conclusion_text <- paste0(
            'Analisis menunjukkan bahwa terdapat perbedaan yang signifikan dalam ', 
            model_data$variable_label, ' antar provinsi. Dari ', 
            length(unique(model_data$data$Provinsi)), ' provinsi yang dianalisis, ',
            length(significant_pairs), ' pasangan menunjukkan perbedaan yang signifikan secara statistik.'
          )
          
          interpretation_html <- paste0(
            interpretation_html,
            '<div style="background: linear-gradient(135deg, #ebf8ff 0%, #f7fafc 100%); padding: 20px; border-radius: 8px; margin: 20px 0; border: 2px solid #2c5282;">',
            '<h6 style="color: #2c5282;"><i class="fas fa-clipboard-check" style="margin-right: 5px;"></i>Kesimpulan Keseluruhan:</h6>',
            '<p style="margin: 10px 0; color: #4a5568;">', conclusion_text, '</p>',
            '<h6 style="color: #2c5282; margin-top: 15px;"><i class="fas fa-lightbulb" style="margin-right: 5px;"></i>Rekomendasi:</h6>',
            '<ul style="color: #4a5568; margin: 10px 0;">',
            '<li>Fokuskan perhatian pada provinsi dengan nilai tertinggi dan terendah</li>',
            '<li>Investigasi faktor-faktor yang menyebabkan perbedaan signifikan</li>',
            '<li>Kembangkan strategi intervensi yang disesuaikan dengan karakteristik masing-masing provinsi</li>',
            '<li>Pertimbangkan best practices dari provinsi dengan performa terbaik</li>',
            '</ul>',
            '</div>'
          )
        } else {
          interpretation_html <- paste0(
            interpretation_html,
            '<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); padding: 20px; border-radius: 8px; margin: 20px 0; border: 2px solid #6c757d;">',
            '<h6 style="color: #6c757d;"><i class="fas fa-info-circle" style="margin-right: 5px;"></i>Kesimpulan:</h6>',
            '<p style="margin: 10px 0; color: #4a5568;">Meskipun ANOVA menunjukkan adanya perbedaan secara keseluruhan, ',
            'uji post-hoc tidak mengidentifikasi perbedaan spesifik antar pasangan provinsi. ',
            'Hal ini mungkin disebabkan oleh koreksi multiple comparison yang ketat.</p>',
            '<h6 style="color: #6c757d; margin-top: 15px;"><i class="fas fa-tools" style="margin-right: 5px;"></i>Rekomendasi:</h6>',
            '<ul style="color: #4a5568; margin: 10px 0;">',
            '<li>Pertimbangkan untuk meningkatkan ukuran sampel jika memungkinkan</li>',
            '<li>Evaluasi kembali kriteria pengelompokan provinsi</li>',
            '<li>Gunakan analisis tambahan seperti trend analysis</li>',
            '</ul>',
            '</div>'
          )
        }
        
        interpretation_html <- paste0(interpretation_html, '</div>')
        
        HTML(interpretation_html)
      } else {
        HTML('<div style="background: linear-gradient(135deg, #f7fafc 0%, #edf2f7 100%); color: #4a5568; padding: 15px; border-radius: 10px; border-left: 4px solid #6c757d;">
             <h6><i class="fas fa-info-circle" style="margin-right: 5px;"></i>Tidak ada interpretasi Tukey</h6>
             <p style="margin: 5px 0 0 0;">Uji Tukey HSD tidak diperlukan karena hasil ANOVA tidak signifikan.</p>
           </div>')
      }
    }, error = function(e) {
      HTML(paste0('<div style="background: #fed7d7; color: #721c24; padding: 15px; border-radius: 10px; border-left: 4px solid #dc3545;">',
                  '<h6><i class="fas fa-exclamation-triangle" style="margin-right: 5px;"></i> Error dalam interpretasi</h6>',
                  '<p style="margin: 0;">Error: ', e$message, '</p></div>'))
    })
  })
  
  
  # new Tab Spasial
  # Path dinamis file ZIP shapefile
  shp_zip_path <- file.path(getwd(), "Administrasi_Kabupaten3.zip")
  
  # 2. Load shapefile dari file ZIP
  data_shp <- reactive({
    req(file.exists(shp_zip_path))  # Pastikan file ZIP ada
    
    # Bersihkan folder 'temp_shp' sebelum ekstrak baru
    unlink("temp_shp", recursive = TRUE)
    dir.create("temp_shp")
    
    tryCatch({
      unzip(shp_zip_path, exdir = "temp_shp")
      shp_path <- list.files("temp_shp", pattern = "\\.shp$", full.names = TRUE)
      
      if (length(shp_path) == 0) {
        stop("File .shp tidak ditemukan dalam zip.")
      }
      
      st_read(shp_path[1])
    }, error = function(e) {
      showNotification(paste("Error loading shapefile:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # 3. Gabungkan shapefile dengan data kemiskinan
  shp_data <- reactive({
    df <- data_shp()
    kemiskinan <- data_uploaded()
    
    req(df, kemiskinan)  # Pastikan kedua data tersedia
    
    # Periksa nama kolom yang akan di-join
    if (!"nmkab" %in% names(df)) {
      showNotification("Kolom 'nmkab' tidak ditemukan di shapefile", type = "error")
      return(NULL)
    }
    
    if (!"Kab" %in% names(kemiskinan)) {
      showNotification("Kolom 'Kab' tidak ditemukan di data kemiskinan", type = "error")
      return(NULL)
    }
    
    tryCatch({
      result <- df %>%
        left_join(kemiskinan, by = c("nmkab" = "Kab")) %>%
        mutate(
          P0 = as.numeric(P0),
          X1 = as.numeric(X1),
          P1 = as.numeric(P1),
          P2 = as.numeric(P2),
          X2 = as.numeric(X2)
        )
      
      # Hapus baris dengan nilai NA pada X2 (untuk legend)
      result <- result %>% filter(!is.na(X2))
      
      return(result)
    }, error = function(e) {
      showNotification(paste("Error joining data:", e$message), type = "error")
      return(NULL)
    })
    
    # new
    observe({
      hasil <- cek_kecocokan_wilayah()
      
      if (length(hasil$shapefile_only) > 0) {
        showNotification(
          paste0("⚠️ ", length(hasil$shapefile_only), 
                 " wilayah hanya ditemukan di shapefile. Lihat daftar validasi."),
          type = "warning",
          duration = 8
        )
      }
      
      if (length(hasil$excel_only) > 0) {
        showNotification(
          paste0("⚠️ ", length(hasil$excel_only), 
                 " wilayah hanya ditemukan di Excel. Lihat daftar validasi."),
          type = "warning",
          duration = 8
        )
      }
    })
    
  })
  
  # new --- VALIDATOR NAMA WILAYAH ---
  cek_kecocokan_wilayah <- reactive({
    df_shp <- data_shp()
    df_excel <- data_uploaded()
    
    req(df_shp, df_excel)
    
    wilayah_shp <- unique(df_shp$nmkab)
    wilayah_excel <- unique(df_excel$Kab)
    
    hanya_di_shapefile <- setdiff(wilayah_shp, wilayah_excel)
    hanya_di_excel <- setdiff(wilayah_excel, wilayah_shp)
    
    list(
      shapefile_only = hanya_di_shapefile,
      excel_only = hanya_di_excel
    )
  })
  
  # new new 4. Render Peta
  output$peta_kemiskinan <- renderLeaflet({
    df <- shp_data()
    req(df)
    
    indikator <- input$indikator_peta
    req(indikator %in% names(df))
    
    nilai_indikator <- df[[indikator]]
    valid_vals <- nilai_indikator[!is.na(nilai_indikator)]
    
    if (length(valid_vals) == 0) {
      showNotification("Tidak ada data valid untuk indikator yang dipilih", type = "warning")
      return(leaflet() %>% addTiles())
    }
    
    # Gunakan pretty untuk rentang dinamis
    bins <- pretty(valid_vals, n = 5)
    
    # Ganti ke skema warna YlOrRd
    pal <- colorBin("YlOrRd", domain = valid_vals, bins = bins, na.color = "transparent")
    
    # Buat label dinamis
    judul <- switch(indikator,
                    X1 = "Jumlah Penduduk Miskin<br>(ribu orang)",
                    X2 = "Garis Kemiskinan<br>(Rp/kapita/bulan)",
                    P0 = "Persentase Penduduk Miskin",
                    P1 = "Indeks Kedalaman Kemiskinan",
                    P2 = "Indeks Keparahan Kemiskinan",
                    indikator)
    
    leaflet(df) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(nilai_indikator),
        color = "white",
        weight = 1,
        fillOpacity = 0.7,
        popup = ~paste0(
          "<strong>", nmkab, "</strong><br>",
          "Garis Kemiskinan: Rp ", format(X2, big.mark = ".", decimal.mark = ","), "<br>",
          "Jumlah Penduduk Miskin: ", X1, " ribu<br>",
          "Persentase Miskin (P0): ", P0, "%<br>",
          "Indeks Kedalaman (P1): ", P1, "<br>",
          "Indeks Keparahan (P2): ", P2
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~nilai_indikator,
        title = judul,
        labFormat = labelFormat(
          prefix = if (indikator == "X2") "Rp " else "",
          suffix = if (indikator == "X1") " ribu" else "",
          big.mark = "."
        ),
        opacity = 0.7,
        na.label = "Tidak ada data"
      )
  })
  
  # new
  output$hasil_validasi_nama <- renderUI({
    hasil <- cek_kecocokan_wilayah()
    req(hasil)
    
    HTML(paste0(
      '<div style="background-color:#ffffff; padding:20px; border-radius:10px; box-shadow:0 1px 3px rgba(0,0,0,0.1);">',
      '<h4 style="margin-top:0;">Validasi Nama Wilayah</h4>',
      
      if (length(hasil$shapefile_only) > 0) {
        paste0(
          "<p>Wilayah berikut ditemukan di shapefile tapi tidak ada di Excel:</p><ul>",
          paste0("<li>", hasil$shapefile_only, "</li>", collapse = ""),
          "</ul>"
        )
      } else "",
      
      if (length(hasil$excel_only) > 0) {
        paste0(
          "<p>Wilayah berikut ditemukan di Excel tapi tidak ada di shapefile:</p><ul>",
          paste0("<li>", hasil$excel_only, "</li>", collapse = ""),
          "</ul>"
        )
      } else "",
      
      if (length(hasil$shapefile_only) == 0 && length(hasil$excel_only) == 0) {
        '<p style="color:green;">Semua nama wilayah cocok antara shapefile dan Excel.</p>'
      } else "",
      
      "</div>"
    ))
  })
  
  # ==============================================================================
  # BAGIAN BARU: IMPLEMENTASI DOWNLOAD HANDLER UNTUK LAPORAN PDF
  # ==============================================================================
  
  # Function untuk membuat konten Rmd - VISUALISASI DIPERBAIKI
  create_deskriptif_rmd <- function() {
    # Get data dan hasil analisis
    data_filtered <- filtered_data()
    analisis_per_col <- analisis_deskriptif_per_prov_col()
    selected_provs <- input$selected_provinsi
    prov_map <- provinsi_mapping()
    
    # Header Rmd yang sederhana dan aman
    rmd_content <- paste0('---
title: "Laporan Analisis Deskriptif Kemiskinan"
author: "Dashboard Tingkat Kemiskinan Indonesia"
date: "', format(Sys.Date(), "%d %B %Y"), '"
output: 
    pdf_document:
    latex_engine: pdflatex
    toc: false
    number_sections: true
    extra_dependencies: ["geometry", "titling"]
fontsize: 11pt
geometry: "top=2.5cm, bottom=2cm, left=2cm, right=2cm"
header-includes:
  - \\usepackage{titling}
  - \\setlength{\\droptitle}{-1.5cm}
  - \\pretitle{\\begin{center}\\LARGE\\bfseries}
  - \\posttitle{\\end{center}}
  - \\preauthor{\\begin{center}\\large}
  - \\postauthor{\\end{center}}
  - \\predate{\\begin{center}}
  - \\postdate{\\end{center}}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  error = TRUE,
  fig.align = "center",
  fig.width = 12,
  fig.height = 8
)

# Load libraries dengan error handling
suppressPackageStartupMessages({
  library(knitr)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

# Try loading additional packages
tryCatch(library(kableExtra), error = function(e) NULL)
tryCatch(library(scales), error = function(e) NULL)

options(scipen = 999)
```

# Ringkasan Eksekutif

Laporan ini menyajikan analisis deskriptif data kemiskinan untuk **', length(selected_provs), ' provinsi** yang dipilih.

## Provinsi yang Dianalisis

')

# Tabel provinsi dengan pendekatan yang lebih aman
rmd_content <- paste0(rmd_content, '```{r provinsi-table}\n')
rmd_content <- paste0(rmd_content, 'prov_data <- data.frame(\n')
rmd_content <- paste0(rmd_content, '  No = 1:', length(selected_provs), ',\n')
rmd_content <- paste0(rmd_content, '  Provinsi = c(')

# Build province names safely
prov_names <- c()
for(prov_id in selected_provs) {
  prov_name <- if(!is.null(prov_map) && as.character(prov_id) %in% names(prov_map)) {
    prov_map[as.character(prov_id)]
  } else {
    paste("Provinsi", prov_id)
  }
  prov_names <- c(prov_names, prov_name)
}

rmd_content <- paste0(rmd_content, paste0('"', prov_names, '"', collapse = ', '), '),\n')
rmd_content <- paste0(rmd_content, '  Jumlah_Kabupaten = c(')

# Get kabupaten counts
kab_counts <- c()
for(prov_id in selected_provs) {
  count <- data_filtered %>% filter(Provinsi == prov_id) %>% nrow()
  kab_counts <- c(kab_counts, count)
}

rmd_content <- paste0(rmd_content, paste(kab_counts, collapse = ', '), ')\n')
rmd_content <- paste0(rmd_content, ')\n')
rmd_content <- paste0(rmd_content, 'kable(prov_data, col.names = c("No", "Provinsi", "Jumlah Kabupaten/Kota"))\n')
rmd_content <- paste0(rmd_content, '```\n\n')

# Variabel yang dianalisis
rmd_content <- paste0(rmd_content, '## Variabel yang Dianalisis

1. **X1**: Jumlah Penduduk Miskin (ribu jiwa)
2. **P0**: Persentase Penduduk Miskin (%)
3. **P1**: Indeks Kedalaman Kemiskinan
4. **P2**: Indeks Keparahan Kemiskinan
5. **X2**: Garis Kemiskinan (Rp/kapita/bulan)

\\newpage

# Analisis Deskriptif Per Provinsi

')

# Loop untuk setiap provinsi
for(i in seq_along(selected_provs)) {
  prov_id <- selected_provs[i]
  prov_name <- prov_names[i]  # Gunakan nama yang sudah dibuat
  
  rmd_content <- paste0(rmd_content, '## ', prov_name, '\n\n')
  
  # Statistik deskriptif
  if(!is.null(analisis_per_col) && !is.null(analisis_per_col[[as.character(prov_id)]])) {
    prov_results <- analisis_per_col[[as.character(prov_id)]]
    
    if(is.list(prov_results)) {
      rmd_content <- paste0(rmd_content, '### Ringkasan Statistik\n\n')
      
      # Tabel statistik
      rmd_content <- paste0(rmd_content, '```{r stats-', i, '}\n')
      rmd_content <- paste0(rmd_content, '# Ambil hasil analisis untuk provinsi ini\n')
      rmd_content <- paste0(rmd_content, 'prov_results <- analisis_per_col[["', prov_id, '"]]\n')
      rmd_content <- paste0(rmd_content, '\n')
      rmd_content <- paste0(rmd_content, 'if(!is.null(prov_results) && is.list(prov_results)) {\n')
      rmd_content <- paste0(rmd_content, '  # Buat tabel statistik\n')
      rmd_content <- paste0(rmd_content, '  stats_data <- data.frame(\n')
      rmd_content <- paste0(rmd_content, '    Variabel = character(0),\n')
      rmd_content <- paste0(rmd_content, '    Mean = numeric(0),\n')
      rmd_content <- paste0(rmd_content, '    Median = numeric(0),\n')
      rmd_content <- paste0(rmd_content, '    SD = numeric(0),\n')
      rmd_content <- paste0(rmd_content, '    N = integer(0)\n')
      rmd_content <- paste0(rmd_content, '  )\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  # Nama variabel\n')
      rmd_content <- paste0(rmd_content, '  var_labels <- c(\n')
      rmd_content <- paste0(rmd_content, '    "X1" = "Jumlah Penduduk Miskin",\n')
      rmd_content <- paste0(rmd_content, '    "P0" = "Persentase Penduduk Miskin",\n')
      rmd_content <- paste0(rmd_content, '    "P1" = "Indeks Kedalaman Kemiskinan",\n')
      rmd_content <- paste0(rmd_content, '    "P2" = "Indeks Keparahan Kemiskinan",\n')
      rmd_content <- paste0(rmd_content, '    "X2" = "Garis Kemiskinan"\n')
      rmd_content <- paste0(rmd_content, '  )\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  # Loop melalui variabel\n')
      rmd_content <- paste0(rmd_content, '  for(var_name in names(var_labels)) {\n')
      rmd_content <- paste0(rmd_content, '    if(var_name %in% names(prov_results)) {\n')
      rmd_content <- paste0(rmd_content, '      var_result <- prov_results[[var_name]]\n')
      rmd_content <- paste0(rmd_content, '      if(is.list(var_result) && !is.null(var_result$mean)) {\n')
      rmd_content <- paste0(rmd_content, '        stats_data <- rbind(stats_data, data.frame(\n')
      rmd_content <- paste0(rmd_content, '          Variabel = var_labels[var_name],\n')
      rmd_content <- paste0(rmd_content, '          Mean = round(var_result$mean, 2),\n')
      rmd_content <- paste0(rmd_content, '          Median = round(var_result$median, 2),\n')
      rmd_content <- paste0(rmd_content, '          SD = round(var_result$sd, 2),\n')
      rmd_content <- paste0(rmd_content, '          N = var_result$n\n')
      rmd_content <- paste0(rmd_content, '        ))\n')
      rmd_content <- paste0(rmd_content, '      }\n')
      rmd_content <- paste0(rmd_content, '    }\n')
      rmd_content <- paste0(rmd_content, '  }\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  # Tampilkan tabel\n')
      rmd_content <- paste0(rmd_content, '  if(nrow(stats_data) > 0) {\n')
      rmd_content <- paste0(rmd_content, '    kable(stats_data, caption = paste("Statistik Deskriptif -", "', prov_name, '"))\n')
      rmd_content <- paste0(rmd_content, '  } else {\n')
      rmd_content <- paste0(rmd_content, '    cat("Tidak ada data statistik yang tersedia.\\n")\n')
      rmd_content <- paste0(rmd_content, '  }\n')
      rmd_content <- paste0(rmd_content, '} else {\n')
      rmd_content <- paste0(rmd_content, '  cat("Data tidak tersedia untuk provinsi ini.\\n")\n')
      rmd_content <- paste0(rmd_content, '}\n')
      rmd_content <- paste0(rmd_content, '```\n\n')
      
      # Visualisasi yang diperbaiki
      rmd_content <- paste0(rmd_content, '### Visualisasi Distribusi\n\n')
      rmd_content <- paste0(rmd_content, '```{r plot-', i, ',fig.height=4, fig.width=6, fig.cap="Distribusi Variabel Kemiskinan"}\n')
      rmd_content <- paste0(rmd_content, '# Visualisasi dengan error handling yang lebih baik\n')
      rmd_content <- paste0(rmd_content, 'tryCatch({\n')
      rmd_content <- paste0(rmd_content, '  # Ambil data untuk provinsi ini\n')
      rmd_content <- paste0(rmd_content, '  df_prov <- data_filtered[data_filtered$Provinsi == ', prov_id, ', ]\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  if(nrow(df_prov) == 0) {\n')
      rmd_content <- paste0(rmd_content, '    plot.new()\n')
      rmd_content <- paste0(rmd_content, '    text(0.5, 0.5, "Tidak ada data untuk provinsi ini", cex = 1.5)\n')
      rmd_content <- paste0(rmd_content, '  } else {\n')
      rmd_content <- paste0(rmd_content, '    # Cek kolom yang tersedia\n')
      rmd_content <- paste0(rmd_content, '    available_cols <- c()\n')
      rmd_content <- paste0(rmd_content, '    target_cols <- c("X1", "P0", "P1", "P2", "X2")\n')
      rmd_content <- paste0(rmd_content, '    \n')
      rmd_content <- paste0(rmd_content, '    for(col in target_cols) {\n')
      rmd_content <- paste0(rmd_content, '      if(col %in% colnames(df_prov) && any(!is.na(df_prov[[col]]))) {\n')
      rmd_content <- paste0(rmd_content, '        available_cols <- c(available_cols, col)\n')
      rmd_content <- paste0(rmd_content, '      }\n')
      rmd_content <- paste0(rmd_content, '    }\n')
      rmd_content <- paste0(rmd_content, '    \n')
      rmd_content <- paste0(rmd_content, '    if(length(available_cols) == 0) {\n')
      rmd_content <- paste0(rmd_content, '      plot.new()\n')
      rmd_content <- paste0(rmd_content, '      text(0.5, 0.5, "Tidak ada kolom data yang valid", cex = 1.5)\n')
      rmd_content <- paste0(rmd_content, '    } else {\n')
      rmd_content <- paste0(rmd_content, '      # Siapkan data untuk plotting\n')
      rmd_content <- paste0(rmd_content, '      plot_data <- df_prov[, available_cols, drop = FALSE]\n')
      rmd_content <- paste0(rmd_content, '      \n')
      rmd_content <- paste0(rmd_content, '      # Convert ke long format dengan cara yang aman\n')
      rmd_content <- paste0(rmd_content, '      long_data <- data.frame(\n')
      rmd_content <- paste0(rmd_content, '        Variabel = character(0),\n')
      rmd_content <- paste0(rmd_content, '        Nilai = numeric(0)\n')
      rmd_content <- paste0(rmd_content, '      )\n')
      rmd_content <- paste0(rmd_content, '      \n')
      rmd_content <- paste0(rmd_content, '      for(col in available_cols) {\n')
      rmd_content <- paste0(rmd_content, '        values <- plot_data[[col]]\n')
      rmd_content <- paste0(rmd_content, '        valid_values <- values[!is.na(values) & is.numeric(values)]\n')
      rmd_content <- paste0(rmd_content, '        if(length(valid_values) > 0) {\n')
      rmd_content <- paste0(rmd_content, '          temp_df <- data.frame(\n')
      rmd_content <- paste0(rmd_content, '            Variabel = rep(col, length(valid_values)),\n')
      rmd_content <- paste0(rmd_content, '            Nilai = valid_values\n')
      rmd_content <- paste0(rmd_content, '          )\n')
      rmd_content <- paste0(rmd_content, '          long_data <- rbind(long_data, temp_df)\n')
      rmd_content <- paste0(rmd_content, '        }\n')
      rmd_content <- paste0(rmd_content, '      }\n')
      rmd_content <- paste0(rmd_content, '      \n')
      rmd_content <- paste0(rmd_content, '      if(nrow(long_data) == 0) {\n')
      rmd_content <- paste0(rmd_content, '        plot.new()\n')
      rmd_content <- paste0(rmd_content, '        text(0.5, 0.5, "Tidak ada data numerik yang valid", cex = 1.5)\n')
      rmd_content <- paste0(rmd_content, '      } else {\n')
      rmd_content <- paste0(rmd_content, '        # Buat plot\n')
      rmd_content <- paste0(rmd_content, '        p <- ggplot(long_data, aes(x = factor(1), y = Nilai)) +\n')
      rmd_content <- paste0(rmd_content, '          geom_boxplot(fill = "lightblue", alpha = 0.7) +\n')
      rmd_content <- paste0(rmd_content, '          geom_jitter(width = 0.2, alpha = 0.6, color = "darkblue") +\n')
      rmd_content <- paste0(rmd_content, '          facet_wrap(~ Variabel, scales = "free_y", ncol = 3) +\n')
      rmd_content <- paste0(rmd_content, '          labs(title = "Distribusi Variabel Kemiskinan", \n')
      rmd_content <- paste0(rmd_content, '               subtitle = "', prov_name, '",\n')
      rmd_content <- paste0(rmd_content, '               x = "", y = "Nilai") +\n')
      rmd_content <- paste0(rmd_content, '          theme_minimal() +\n')
      rmd_content <- paste0(rmd_content, '          theme(\n')
      rmd_content <- paste0(rmd_content, '            axis.text.x = element_blank(),\n')
      rmd_content <- paste0(rmd_content, '            axis.ticks.x = element_blank(),\n')
      rmd_content <- paste0(rmd_content, '            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),\n')
      rmd_content <- paste0(rmd_content, '            plot.subtitle = element_text(hjust = 0.5, size = 12),\n')
      rmd_content <- paste0(rmd_content, '            strip.text = element_text(face = "bold")\n')
      rmd_content <- paste0(rmd_content, '          )\n')
      rmd_content <- paste0(rmd_content, '        \n')
      rmd_content <- paste0(rmd_content, '        # Format Y axis labels\n')
      rmd_content <- paste0(rmd_content, '        if("scales" %in% rownames(installed.packages())) {\n')
      rmd_content <- paste0(rmd_content, '          p <- p + scale_y_continuous(labels = scales::comma)\n')
      rmd_content <- paste0(rmd_content, '        }\n')
      rmd_content <- paste0(rmd_content, '        \n')
      rmd_content <- paste0(rmd_content, '        print(p)\n')
      rmd_content <- paste0(rmd_content, '      }\n')
      rmd_content <- paste0(rmd_content, '    }\n')
      rmd_content <- paste0(rmd_content, '  }\n')
      rmd_content <- paste0(rmd_content, '}, error = function(e) {\n')
      rmd_content <- paste0(rmd_content, '  plot.new()\n')
      rmd_content <- paste0(rmd_content, '  text(0.5, 0.5, paste("Error dalam visualisasi:", e$message), cex = 1, col = "red")\n')
      rmd_content <- paste0(rmd_content, '})\n')
      rmd_content <- paste0(rmd_content, '```\n\n')
    }
  } else {
    rmd_content <- paste0(rmd_content, 'Data tidak tersedia untuk provinsi ini.\n\n')
  }
  
  # Page break
  if(i < length(selected_provs)) {
    rmd_content <- paste0(rmd_content, '\\newpage\n\n')
  }
}

# Perbandingan antar provinsi
if(length(selected_provs) > 1) {
  rmd_content <- paste0(rmd_content, '\n\\newpage\n\n')
  rmd_content <- paste0(rmd_content, '# Perbandingan Antar Provinsi\n\n')
  rmd_content <- paste0(rmd_content, '```{r comparison}\n')
  rmd_content <- paste0(rmd_content, '# Buat tabel perbandingan sederhana\n')
  rmd_content <- paste0(rmd_content, 'comp_data <- data.frame(\n')
  rmd_content <- paste0(rmd_content, '  Provinsi = character(0),\n')
  rmd_content <- paste0(rmd_content, '  Rata_P0 = numeric(0)\n')
  rmd_content <- paste0(rmd_content, ')\n')
  rmd_content <- paste0(rmd_content, '\n')
  
  for(j in seq_along(selected_provs)) {
    prov_id <- selected_provs[j]
    prov_name <- prov_names[j]
    
    rmd_content <- paste0(rmd_content, '# Data untuk ', prov_name, '\n')
    rmd_content <- paste0(rmd_content, 'prov_data_', j, ' <- data_filtered[data_filtered$Provinsi == ', prov_id, ', ]\n')
    rmd_content <- paste0(rmd_content, 'if(nrow(prov_data_', j, ') > 0 && "P0" %in% colnames(prov_data_', j, ')) {\n')
    rmd_content <- paste0(rmd_content, '  mean_p0 <- mean(prov_data_', j, '$P0, na.rm = TRUE)\n')
    rmd_content <- paste0(rmd_content, '  if(!is.na(mean_p0)) {\n')
    rmd_content <- paste0(rmd_content, '    comp_data <- rbind(comp_data, data.frame(\n')
    rmd_content <- paste0(rmd_content, '      Provinsi = "', prov_name, '",\n')
    rmd_content <- paste0(rmd_content, '      Rata_P0 = round(mean_p0, 2)\n')
    rmd_content <- paste0(rmd_content, '    ))\n')
    rmd_content <- paste0(rmd_content, '  }\n')
    rmd_content <- paste0(rmd_content, '}\n')
  }
  
  rmd_content <- paste0(rmd_content, '\n')
  rmd_content <- paste0(rmd_content, '# Tampilkan tabel perbandingan\n')
  rmd_content <- paste0(rmd_content, 'if(nrow(comp_data) > 0) {\n')
  rmd_content <- paste0(rmd_content, '  colnames(comp_data) <- c("Provinsi", "Rata-rata P0 (%)")\n')
  rmd_content <- paste0(rmd_content, '  kable(comp_data, caption = "Perbandingan Rata-rata Persentase Kemiskinan")\n')
  rmd_content <- paste0(rmd_content, '} else {\n')
  rmd_content <- paste0(rmd_content, '  cat("Tidak ada data untuk perbandingan.\\n")\n')
  rmd_content <- paste0(rmd_content, '}\n')
  rmd_content <- paste0(rmd_content, '```\n\n')
}

# Footer
rmd_content <- paste0(rmd_content, '\n---\n\n')
rmd_content <- paste0(rmd_content, '*Laporan dibuat pada ', format(Sys.time(), "%d %B %Y"), '*')

return(rmd_content)
  }
  
  # Function untuk membuat konten Rmd untuk Laporan ANOVA - VERSI AMAN DENGAN ERROR HANDLING
  create_anova_rmd <- function() {
    
    # Debug: Print ke console untuk troubleshooting
    cat("Starting create_anova_rmd function...\n")
    
    # Error handling untuk seluruh fungsi
    tryCatch({
      
      # Check if ANOVA has been run
      anova_exists <- !is.null(input$run_anova) && input$run_anova > 0
      
      if(!anova_exists) {
        cat("ANOVA not run, returning basic template...\n")
        return(paste0('---
title: "Laporan Analisis ANOVA Data Kemiskinan"
author: "Dashboard Tingkat Kemiskinan Indonesia"
date: "', format(Sys.Date(), "%d %B %Y"), '"
output: 
  pdf_document:
    latex_engine: pdflatex
    toc: false
    number_sections: true
fontsize: 12pt
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, error = TRUE)
```

# Informasi Analisis

Analisis ANOVA belum dijalankan. Silakan jalankan analisis ANOVA terlebih dahulu dari dashboard.

---

*Laporan dibuat pada ', format(Sys.time(), "%d %B %Y"), '*
'))
      }
      
      # Get ANOVA results dengan error handling
      model_data <- tryCatch({
        anova_model()
      }, error = function(e) {
        cat("Error getting ANOVA model:", e$message, "\n")
        return(NULL)
      })
      
      if(is.null(model_data)) {
        cat("Model data is NULL, returning error template...\n")
        return(paste0('---
title: "Laporan Analisis ANOVA Data Kemiskinan"
author: "Dashboard Tingkat Kemiskinan Indonesia"
date: "', format(Sys.Date(), "%d %B %Y"), '"
output: 
  pdf_document:
    latex_engine: pdflatex
    toc: false
    number_sections: true
    extra_dependencies: ["geometry", "titling"]
fontsize: 11pt
geometry: "top=2.5cm, bottom=2cm, left=2cm, right=2cm"
header-includes:
  - \\usepackage{titling}
  - \\setlength{\\droptitle}{-1.5cm}
  - \\pretitle{\\begin{center}\\LARGE\\bfseries}
  - \\posttitle{\\end{center}}
  - \\preauthor{\\begin{center}\\large}
  - \\postauthor{\\end{center}}
  - \\predate{\\begin{center}}
  - \\postdate{\\end{center}}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, error = TRUE)
```

# Error dalam Analisis

Error dalam mengambil hasil ANOVA. Silakan coba jalankan analisis kembali.

---

*Laporan dibuat pada ', format(Sys.time(), "%d %B %Y"), '*
'))
      }
      
      # Get necessary data dengan error handling
      selected_provs <- tryCatch(input$selected_provinsi, error = function(e) c())
      prov_map <- tryCatch(provinsi_mapping(), error = function(e) NULL)
      
      if(length(selected_provs) == 0) {
        cat("No provinces selected...\n")
        return("Error: Tidak ada provinsi yang dipilih untuk analisis.")
      }
      
      # Extract ANOVA results safely
      anova_result <- tryCatch(summary(model_data$model), error = function(e) NULL)
      if(is.null(anova_result)) {
        return("Error: Gagal mendapatkan hasil ANOVA.")
      }
      
      p_value <- tryCatch(anova_result[[1]][1, "Pr(>F)"], error = function(e) NA)
      f_value <- tryCatch(anova_result[[1]][1, "F value"], error = function(e) NA)
      df1 <- tryCatch(anova_result[[1]][1, "Df"], error = function(e) NA)
      df2 <- tryCatch(anova_result[[1]][2, "Df"], error = function(e) NA)
      ss_between <- tryCatch(anova_result[[1]][1, "Sum Sq"], error = function(e) NA)
      ss_within <- tryCatch(anova_result[[1]][2, "Sum Sq"], error = function(e) NA)
      ms_between <- tryCatch(anova_result[[1]][1, "Mean Sq"], error = function(e) NA)
      ms_within <- tryCatch(anova_result[[1]][2, "Mean Sq"], error = function(e) NA)
      
      # Calculate R-squared safely
      r_squared <- tryCatch({
        if(!is.na(ss_between) && !is.na(ss_within)) {
          ss_total <- ss_between + ss_within
          ss_between / ss_total
        } else {
          NA
        }
      }, error = function(e) NA)
      
      # Determine significance
      is_significant <- !is.na(p_value) && p_value < 0.05
      
      cat("Building RMD content...\n")
      
      # Build province names safely
      prov_names_safe <- c()
      obs_counts <- c()
      
      for(prov_id in selected_provs) {
        prov_name <- tryCatch({
          if(!is.null(prov_map) && as.character(prov_id) %in% names(prov_map)) {
            prov_map[as.character(prov_id)]
          } else {
            paste("Provinsi", prov_id)
          }
        }, error = function(e) paste("Provinsi", prov_id))
        
        # Safe name escaping
        prov_name_safe <- gsub('"', "", prov_name)  # Remove quotes instead of escaping
        prov_names_safe <- c(prov_names_safe, prov_name_safe)
        
        # Safe observation count
        count <- tryCatch({
          sum(model_data$data$Provinsi == prov_id, na.rm = TRUE)
        }, error = function(e) 0)
        obs_counts <- c(obs_counts, count)
      }
      
      # Hasil utama
      if(is_significant) {
        result_conclusion <- "SIGNIFIKAN - Terdapat perbedaan rata-rata yang signifikan antar provinsi"
        decision <- "Tolak H0, terima H1"
      } else {
        result_conclusion <- "TIDAK SIGNIFIKAN - Tidak terdapat perbedaan rata-rata yang signifikan antar provinsi"
        decision <- "Gagal menolak H0"
      }
      
      # Build RMD content dengan sintaks yang lebih aman
      rmd_content <- paste0('---
title: "Laporan Analisis ANOVA Data Kemiskinan"
author: "Dashboard Tingkat Kemiskinan Indonesia"
date: "', format(Sys.Date(), "%d %B %Y"), '"
output: 
  pdf_document:
    latex_engine: pdflatex
    toc: false
    number_sections: true
    extra_dependencies: ["geometry", "titling"]
fontsize: 11pt
geometry: "top=2.5cm, bottom=2cm, left=2cm, right=2cm"
header-includes:
  - \\usepackage{titling}
  - \\setlength{\\droptitle}{-1.5cm}
  - \\pretitle{\\begin{center}\\LARGE\\bfseries}
  - \\posttitle{\\end{center}}
  - \\preauthor{\\begin{center}\\large}
  - \\postauthor{\\end{center}}
  - \\predate{\\begin{center}}
  - \\postdate{\\end{center}}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  error = TRUE,
  fig.align = "center",
  fig.width = 10,
  fig.height = 6
)

library(knitr)
library(ggplot2)
library(dplyr)
suppressPackageStartupMessages({
  tryCatch(library(kableExtra), error = function(e) NULL)
  tryCatch(library(car), error = function(e) NULL)
})
options(scipen = 999)

# Store analysis variables for use in chunks
p_value <- ', ifelse(is.na(p_value), 'NA', format(p_value, scientific = FALSE)), '
f_value <- ', ifelse(is.na(f_value), 'NA', format(f_value, scientific = FALSE)), '
r_squared <- ', ifelse(is.na(r_squared), 'NA', format(r_squared, scientific = FALSE)), '
is_significant <- ', ifelse(is_significant, 'TRUE', 'FALSE'), '
```

# Ringkasan Eksekutif

  Laporan ini menyajikan hasil analisis ANOVA untuk menguji perbedaan rata-rata **', 
      model_data$variable_label, '** antar ', length(unique(model_data$data$Provinsi)), ' provinsi.

## Informasi Dasar Analisis

```{r info-basic}
info_table <- data.frame(
  Aspek = c("Variabel yang Diuji", "Jumlah Provinsi", "Total Observasi", "Metode Analisis"),
  Keterangan = c("', model_data$variable_label, '", "', 
      length(unique(model_data$data$Provinsi)), '", "', 
      nrow(model_data$data), '", "One-Way ANOVA")
)
kable(info_table, caption = "Informasi Analisis ANOVA")
```

## Provinsi yang Dibandingkan

```{r provinsi-list}
provinsi_info <- data.frame(
  No = 1:', length(selected_provs), ',
  Nama_Provinsi = c("', paste(prov_names_safe, collapse = '", "'), '"),
  Jumlah_Observasi = c(', paste(obs_counts, collapse = ', '), ')
)
kable(provinsi_info, caption = "Daftar Provinsi yang Dibandingkan")
```

\\newpage

# Uji Asumsi ANOVA

## Uji Normalitas Residual

```{r normality-test}
residuals_data <- residuals(model_data$model)
shapiro_result <- tryCatch(shapiro.test(residuals_data), 
                          error = function(e) list(statistic = NA, p.value = NA))

normality_table <- data.frame(
  Test = "Shapiro-Wilk",
  P_value = ifelse(is.na(shapiro_result$p.value), "NA", 
                   format(round(shapiro_result$p.value, 6), scientific = FALSE)),
  Interpretasi = ifelse(is.na(shapiro_result$p.value), "Tidak dapat dihitung",
                        ifelse(shapiro_result$p.value > 0.05, 
                               "Normal (asumsi terpenuhi)", 
                               "Tidak normal (asumsi dilanggar)"))
)
kable(normality_table, caption = "Hasil Uji Normalitas")
```

```{r qq-plot, fig.height=4, fig.width=6, fig.cap="Q-Q Plot"}
tryCatch({
  residuals_data <- residuals(model_data$model)
  qqnorm(residuals_data, main = "Q-Q Plot: Uji Normalitas Residual")
  qqline(residuals_data, col = "red", lwd = 2)
}, error = function(e) {
  plot.new()
  text(0.5, 0.5, "Error dalam Q-Q plot", cex = 1.2, col = "red")
})
```

## Interpretasi Uji Normalitas

```{r interpretasi-normalitas, results="asis"}
if(!is.na(shapiro_result$p.value)) {
  if(shapiro_result$p.value > 0.05) {
    cat("**Interpretasi Uji Normalitas:**\\n\\n")
    cat("Hasil uji Shapiro-Wilk menunjukkan p-value =", 
        format(round(shapiro_result$p.value, 6), scientific = FALSE), 
        "> 0.05, yang berarti kita gagal menolak hipotesis nul. ")
    cat("Hal ini mengindikasikan bahwa residual model ANOVA berdistribusi normal. ")
    cat("Asumsi normalitas untuk ANOVA **terpenuhi**, sehingga hasil analisis ANOVA dapat diandalkan.\\n\\n")
    cat("Q-Q plot juga mendukung kesimpulan ini, dimana titik-titik data mengikuti garis diagonal dengan baik.")
  } else {
    cat("**Interpretasi Uji Normalitas:**\\n\\n")
    cat("Hasil uji Shapiro-Wilk menunjukkan p-value =", 
        format(round(shapiro_result$p.value, 6), scientific = FALSE), 
        "< 0.05, yang berarti kita menolak hipotesis nul. ")
    cat("Hal ini mengindikasikan bahwa residual model ANOVA **tidak berdistribusi normal**. ")
    cat("Pelanggaran asumsi normalitas ini dapat mempengaruhi validitas hasil ANOVA, ")
    cat("terutama untuk sampel kecil. Namun, ANOVA cukup robust terhadap pelanggaran normalitas ")
    cat("untuk sampel yang cukup besar (n > 30 per grup).\\n\\n")
  }
} else {
  cat("**Interpretasi Uji Normalitas:**\\n\\n")
  cat("Uji normalitas tidak dapat dilakukan. Hal ini mungkin terjadi karena ukuran sampel yang terlalu kecil ")
  cat("atau terdapat masalah dalam data. Evaluasi normalitas dapat dilakukan secara visual melalui Q-Q plot.")
}
```

\\newpage

## Uji Homogenitas Varians

```{r homogeneity-test}
if("car" %in% rownames(installed.packages())) {
  formula_str <- paste(model_data$variable, "~ factor(Provinsi)")
  levene_result <- tryCatch(car::leveneTest(as.formula(formula_str), data = model_data$data),
                           error = function(e) data.frame("F value" = NA, "Pr(>F)" = NA))
  
  homogeneity_table <- data.frame(
    Test = "Levene Test",
    P_value = ifelse(is.na(levene_result[1, "Pr(>F)"]), "NA", 
                     format(round(levene_result[1, "Pr(>F)"], 6), scientific = FALSE)),
    Interpretasi = ifelse(is.na(levene_result[1, "Pr(>F)"]), "Tidak dapat dihitung",
                          ifelse(levene_result[1, "Pr(>F)"] > 0.05, 
                                 "Homogen (asumsi terpenuhi)", 
                                 "Tidak homogen (asumsi dilanggar)"))
  )
} else {
  homogeneity_table <- data.frame(
    Test = "Levene Test", P_value = "Package tidak tersedia", Interpretasi = "Tidak dapat diuji"
  )
}
kable(homogeneity_table, caption = "Hasil Uji Homogenitas")
```

```{r residual-plot, fig.height=4, fig.width=6, fig.cap="Residuals vs Fitted Values"}
tryCatch({
  fitted_vals <- fitted(model_data$model)
  residuals_vals <- residuals(model_data$model)
  plot(fitted_vals, residuals_vals, main = "Residuals vs Fitted Values",
       xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = "blue")
  abline(h = 0, col = "red", lwd = 2, lty = 2)
}, error = function(e) {
  plot.new()
  text(0.5, 0.5, "Error dalam residual plot", cex = 1.2, col = "red")
})
```

## Interpretasi Uji Homogenitas

```{r interpretasi-homogenitas, results="asis"}
if("car" %in% rownames(installed.packages()) && !is.na(levene_result[1, "Pr(>F)"])) {
  if(levene_result[1, "Pr(>F)"] > 0.05) {
    cat("**Interpretasi Uji Homogenitas:**\\n\\n")
    cat("Hasil uji Levene menunjukkan p-value =", 
        format(round(levene_result[1, "Pr(>F)"], 6), scientific = FALSE), 
        "> 0.05, yang berarti kita gagal menolak hipotesis nul. ")
    cat("Hal ini mengindikasikan bahwa varians antar grup adalah **homogen** (sama). ")
    cat("Asumsi homogenitas varians untuk ANOVA **terpenuhi**, sehingga hasil analisis ANOVA valid.\\n\\n")
    cat("Plot residuals vs fitted values juga mendukung kesimpulan ini, dimana sebaran residual ")
    cat("relatif merata di sekitar garis horizontal.")
  } else {
    cat("**Interpretasi Uji Homogenitas:**\\n\\n")
    cat("Hasil uji Levene menunjukkan p-value =", 
        format(round(levene_result[1, "Pr(>F)"], 6), scientific = FALSE), 
        "< 0.05, yang berarti kita menolak hipotesis nul. ")
    cat("Hal ini mengindikasikan bahwa varians antar grup **tidak homogen** (heteroskedastisitas). ")
    cat("Pelanggaran asumsi homogenitas dapat mempengaruhi tingkat kesalahan Tipe I dalam ANOVA.\\n\\n")
    cat("Disarankan untuk menggunakan alternatif seperti Welchs ANOVA yang lebih robust ")
    cat("terhadap pelanggaran asumsi homogenitas, atau melakukan transformasi data.")
  }
} else {
  cat("**Interpretasi Uji Homogenitas:**\\n\\n")
  cat("Uji homogenitas tidak dapat dilakukan karena package car tidak tersedia atau ")
  cat("terdapat masalah dalam data. Evaluasi homogenitas dapat dilakukan secara visual ")
  cat("melalui plot residuals vs fitted values. Sebaran residual yang merata mengindikasikan ")
  cat("homogenitas varians.")
}
```

\\newpage

# Hasil Utama ANOVA

```{r hasil-utama}
hasil_summary <- data.frame(
  Statistik = c("F-statistic", "P-value", "R-squared", "Kesimpulan", "Keputusan"),
  Nilai = c("', 
ifelse(is.na(f_value), "NA", round(f_value, 3)), '", "', 
ifelse(is.na(p_value), "NA", ifelse(p_value < 0.0001, "< 0.0001", format(round(p_value, 6), scientific = FALSE))), '", "', 
ifelse(is.na(r_squared), "NA", paste0(round(r_squared * 100, 2), "%")), '", "',
result_conclusion, '", "', decision, '")
)
kable(hasil_summary, caption = "Ringkasan Hasil ANOVA")
```

## Tabel ANOVA Lengkap

```{r anova-table}
anova_complete <- data.frame(
  Sumber_Variasi = c("Antar Provinsi", "Dalam Provinsi", "Total"),
  Df = c(', ifelse(is.na(df1), 'NA', df1), ', ', ifelse(is.na(df2), 'NA', df2), ', ', 
ifelse(is.na(df1) || is.na(df2), 'NA', df1 + df2), '),
  Sum_Sq = c(', ifelse(is.na(ss_between), 'NA', round(ss_between, 2)), ', ', 
ifelse(is.na(ss_within), 'NA', round(ss_within, 2)), ', ',
ifelse(is.na(ss_between) || is.na(ss_within), 'NA', round(ss_between + ss_within, 2)), '),
  Mean_Sq = c(', ifelse(is.na(ms_between), 'NA', round(ms_between, 2)), ', ', 
ifelse(is.na(ms_within), 'NA', round(ms_within, 2)), ', NA),
  F_value = c(', ifelse(is.na(f_value), 'NA', round(f_value, 3)), ', NA, NA),
  Pr_F = c("', ifelse(is.na(p_value), 'NA', ifelse(p_value < 0.0001, "< 0.0001", 
                                                   format(round(p_value, 6), scientific = FALSE))), '", "NA", "NA")
)
kable(anova_complete, caption = "Tabel Analisis Varians (ANOVA)")
```

## Visualisasi Perbandingan Antar Provinsi

```{r comparison-plot, fig.height=4, fig.width=6, fig.cap="Perbandingan Antar Provinsi"}
tryCatch({
  plot_data <- model_data$data
  var_name <- model_data$variable
  
  if(!is.null(prov_map)) {
    plot_data$Provinsi_Label <- factor(
      prov_map[as.character(plot_data$Provinsi)],
      levels = prov_map[as.character(sort(unique(plot_data$Provinsi)))]
    )
  } else {
    plot_data$Provinsi_Label <- factor(paste("Provinsi", plot_data$Provinsi))
  }
  
  p <- ggplot(plot_data, aes(x = Provinsi_Label, y = .data[[var_name]], fill = Provinsi_Label)) +
    geom_boxplot(alpha = 0.7, outlier.size = 2) +
    geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +
    labs(title = paste("Perbandingan", model_data$variable_label, "Antar Provinsi"),
         x = "Provinsi", y = model_data$variable_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    scale_fill_brewer(type = "qual", palette = "Set3")
  
  print(p)
}, error = function(e) {
  plot.new()
  text(0.5, 0.5, "Error dalam visualisasi", cex = 1.2, col = "red")
})
```
\\newpage

## Interpretasi Hasil ANOVA

```{r interpretasi-anova, results="asis"}

if(is_significant) {
  cat("Hasil analisis ANOVA menunjukkan **perbedaan yang signifikan** antar provinsi:\\n\\n")
  cat("- **F-statistic =", ifelse(is.na(f_value), "NA", round(f_value, 3)), ":** ")
  cat("Nilai F yang besar mengindikasikan bahwa variabilitas antar grup (provinsi) ")
  cat("lebih besar dibandingkan variabilitas dalam grup.\\n\\n")
  cat("- **P-value =", ifelse(is.na(p_value), "NA", 
      ifelse(p_value < 0.0001, "< 0.0001", format(round(p_value, 6), scientific = FALSE))), ":** ")
  cat("Nilai p < 0.05 menunjukkan bukti statistik yang kuat untuk menolak hipotesis nol. ")
  cat("Artinya, rata-rata ", model_data$variable_label, " berbeda signifikan antar provinsi.\\n\\n")
  cat("- **R-squared =", ifelse(is.na(r_squared), "NA", paste0(round(r_squared * 100, 2), "%")), ":** ")
  if(!is.na(r_squared)) {
    if(r_squared < 0.30) {
      cat("Efek kecil - faktor provinsi menjelaskan sebagian kecil variabilitas data.")
    } else if(r_squared < 0.70) {
      cat("Efek sedang - faktor provinsi menjelaskan sebagian besar variabilitas data.")
    } else {
      cat("Efek besar - faktor provinsi menjelaskan sebagian besar variabilitas data.")
    }
  }
  cat("\\n\\n**Kesimpulan:** Terdapat bukti statistik yang cukup untuk menyatakan bahwa ")
  cat("rata-rata ", model_data$variable_label, " berbeda antar provinsi yang dibandingkan. ")
  cat("Uji post-hoc diperlukan untuk mengetahui provinsi mana yang berbeda secara spesifik.")
} else {
  cat("Hasil analisis ANOVA menunjukkan **tidak ada perbedaan yang signifikan** antar provinsi:\\n\\n")
  cat("- **F-statistic =", ifelse(is.na(f_value), "NA", round(f_value, 3)), ":** ")
  cat("Nilai F yang relatif kecil mengindikasikan bahwa variabilitas antar grup (provinsi) ")
  cat("tidak lebih besar secara signifikan dibandingkan variabilitas dalam grup.\\n\\n")
  cat("- **P-value =", ifelse(is.na(p_value), "NA", 
      ifelse(p_value < 0.0001, "< 0.0001", format(round(p_value, 6), scientific = FALSE))), ":** ")
  cat("Nilai p >= 0.05 menunjukkan tidak ada bukti statistik yang cukup untuk menolak hipotesis nol. ")
  cat("Artinya, tidak ada perbedaan rata-rata ", model_data$variable_label, " yang signifikan antar provinsi.\\n\\n")
  cat("- **R-squared =", ifelse(is.na(r_squared), "NA", paste0(round(r_squared * 100, 2), "%")), ":** ")
  if(!is.na(r_squared)) {
    cat("Faktor provinsi hanya menjelaskan ", round(r_squared * 100, 2), "% variabilitas dalam data.")
  }
  cat("\\n\\n**Kesimpulan:** Tidak terdapat bukti statistik yang cukup untuk menyatakan bahwa ")
  cat("rata-rata ", model_data$variable_label, " berbeda antar provinsi yang dibandingkan. ")
  cat("Kondisi antar provinsi dapat dianggap relatif homogen untuk variabel ini.")
}
```
')

# Tambahkan bagian Tukey jika signifikan
if(is_significant) {
  rmd_content <- paste0(rmd_content, '

\\newpage

# Uji Post-Hoc (Tukey HSD)

## Hasil Uji Tukey HSD

```{r tukey-test}
tukey_result <- tryCatch(TukeyHSD(model_data$model, conf.level = 0.95),
                        error = function(e) NULL)

if(!is.null(tukey_result) && !is.null(tukey_result$Provinsi)) {
  tukey_df <- as.data.frame(tukey_result$Provinsi)
  tukey_df$Comparison <- rownames(tukey_df)
  
  # Convert to readable names
  if(!is.null(prov_map)) {
    tukey_df$Comparison_Label <- sapply(tukey_df$Comparison, function(comp) {
      parts <- strsplit(comp, "-")[[1]]
      if(length(parts) == 2) {
        prov1 <- ifelse(parts[1] %in% names(prov_map), prov_map[parts[1]], parts[1])
        prov2 <- ifelse(parts[2] %in% names(prov_map), prov_map[parts[2]], parts[2])
        return(paste(prov1, "vs", prov2))
      }
      return(comp)
    })
  } else {
    tukey_df$Comparison_Label <- tukey_df$Comparison
  }
  
  tukey_display <- data.frame(
    Perbandingan = tukey_df$Comparison_Label,
    Perbedaan = round(tukey_df$diff, 4),
    P_value = ifelse(tukey_df$"p adj" < 0.0001, "< 0.0001", 
                     format(round(tukey_df$"p adj", 6), scientific = FALSE))
  )
  
  kable(tukey_display, caption = "Hasil Uji Tukey HSD")
} else {
  cat("Uji Tukey HSD tidak dapat dilakukan.")
}
```

## Visualisasi Hasil Tukey HSD

```{r tukey-plot, fig.height=4, fig.width=6, fig.cap="Tukey HSD Plot"}
if(!is.null(tukey_result) && !is.null(tukey_result$Provinsi)) {
  tryCatch({
    plot(tukey_result, las = 1)
    abline(v = 0, col = "red", lty = 2, lwd = 2)
  }, error = function(e) {
    plot.new()
    text(0.5, 0.5, "Error dalam Tukey plot", cex = 1.2, col = "red")
  })
}
```

## Interpretasi Uji Tukey HSD

```{r interpretasi-tukey, results="asis"}
if(!is.null(tukey_result) && !is.null(tukey_result$Provinsi)) {
  cat("Uji Tukey HSD dilakukan untuk mengidentifikasi pasangan provinsi mana yang memiliki ")
  cat("perbedaan rata-rata yang signifikan setelah hasil ANOVA menunjukkan adanya perbedaan overall.\\n\\n")
  
  # Hitung jumlah perbandingan signifikan
  significant_comparisons <- sum(tukey_df$"p adj" < 0.05, na.rm = TRUE)
  total_comparisons <- nrow(tukey_df)
  
  cat("**Ringkasan Hasil:**\\n\\n")
  cat("- Total perbandingan: ", total_comparisons, " pasang provinsi\\n\\n")
  cat("- Perbandingan signifikan: ", significant_comparisons, " pasang provinsi\\n\\n")
  cat("- Persentase signifikan: ", round((significant_comparisons/total_comparisons)*100, 1), "%\\n\\n")
  
  if(significant_comparisons > 0) {
    cat("**Pasangan provinsi dengan perbedaan signifikan (p < 0.05):**\\n\\n")
    significant_pairs <- tukey_df[tukey_df$"p adj" < 0.05, ]
    for(i in 1:nrow(significant_pairs)) {
      cat("- ", significant_pairs$Comparison_Label[i], 
          " (perbedaan = ", round(significant_pairs$diff[i], 4), 
          ", p = ", format(round(significant_pairs$"p adj"[i], 6), scientific = FALSE), ")\\n\\n")
    }
    cat("**Interpretasi Praktis:**\\n")
    cat("Perbedaan yang signifikan ini menunjukkan bahwa provinsi-provinsi tersebut ")
    cat("memiliki karakteristik ", model_data$variable_label, " yang berbeda secara statistik. ")
    cat("Nilai perbedaan (diff) menunjukkan seberapa besar selisih rata-rata antar provinsi, ")
    cat("dimana nilai positif berarti provinsi pertama memiliki rata-rata lebih tinggi.")
  } else {
    cat("**Temuan Menarik:**\\n")
    cat("Meskipun ANOVA overall menunjukkan hasil signifikan, tidak ada pasangan provinsi ")
    cat("yang menunjukkan perbedaan signifikan dalam uji Tukey HSD. Hal ini dapat terjadi ")
    cat("karena uji Tukey lebih konservatif (menggunakan koreksi multiple comparisons) ")
    cat("atau karena perbedaan tersebar merata di banyak pasangan provinsi dengan efek kecil.")
  }
  
  cat("\\n\\n**Catatan Metodologi:**\\n")
  cat("Uji Tukey HSD menggunakan koreksi Bonferroni untuk mengendalikan tingkat kesalahan ")
  cat("familywise, sehingga lebih ketat dibandingkan uji t biasa. Confidence interval 95% ")
  cat("yang tidak mengandung nol mengindikasikan perbedaan yang signifikan.")
} else {
  cat("**Interpretasi Uji Tukey HSD:**\\n\\n")
  cat("Uji Tukey HSD tidak dapat dilakukan karena terdapat masalah dalam data atau model. ")
  cat("Hal ini mungkin terjadi jika jumlah grup terlalu sedikit atau terdapat grup dengan ")
  cat("observasi yang sangat sedikit.")
}
```
')
}

# Bagian kesimpulan
rmd_content <- paste0(rmd_content, '

\\newpage

# Kesimpulan dan Rekomendasi

## Ringkasan Hasil Analisis

```{r ringkasan-hasil, results="asis"}
cat("Analisis ANOVA telah dilakukan untuk menguji perbedaan rata-rata ", model_data$variable_label, 
    " antar ", length(unique(model_data$data$Provinsi)), " provinsi dengan total ", 
    nrow(model_data$data), " observasi.\\n\\n")

# Hasil uji asumsi
cat("**Evaluasi Asumsi ANOVA:**\\n\\n")
cat("1. Normalitas Residual: ")
if(!is.na(shapiro_result$p.value)) {
  if(shapiro_result$p.value > 0.05) {
    cat("Terpenuhi (p = ", format(round(shapiro_result$p.value, 6), scientific = FALSE), " > 0.05)")
  } else {
    cat("Dilanggar (p = ", format(round(shapiro_result$p.value, 6), scientific = FALSE), " < 0.05)")
  }
} else {
  cat("Tidak dapat dievaluasi")
}

cat("\\n\\n2. Homogenitas Varians: ")
if("car" %in% rownames(installed.packages()) && !is.na(levene_result[1, "Pr(>F)"])) {
  if(levene_result[1, "Pr(>F)"] > 0.05) {
    cat("Terpenuhi (p = ", format(round(levene_result[1, "Pr(>F)"], 6), scientific = FALSE), " > 0.05)")
  } else {
    cat("Dilanggar (p = ", format(round(levene_result[1, "Pr(>F)"], 6), scientific = FALSE), " < 0.05)")
  }
} else {
  cat("Tidak dapat dievaluasi")
}
cat("\\n\\n")
```

## Kesimpulan Utama

```{r kesimpulan-utama, results="asis"}
# Hasil ANOVA

if(is_significant) {
  cat("Hasil analisis menunjukkan terdapat **perbedaan yang signifikan secara statistik** ")
  cat("dalam rata-rata ", model_data$variable_label, " antar provinsi yang dianalisis ")
  cat("(F = ", round(f_value, 3), ", p ", ifelse(p_value < 0.0001, "< 0.0001", 
      paste("=", format(round(p_value, 6), scientific = FALSE))), ").\\n\\n")
  
  # Interpretasi R-squared
  cat("**Besaran Efek:**\\n")
  if(!is.na(r_squared)) {
    effect_size <- round(r_squared * 100, 2)
    cat("Faktor provinsi menjelaskan ", effect_size, "% dari total variabilitas data ", 
        model_data$variable_label, ". ")
    if(r_squared < 0.30) {
      cat("Ini menunjukkan efek kecil.")
    } else if(r_squared < 0.70) {
      cat("Ini menunjukkan efek sedang.")
    } else {
      cat("Ini menunjukkan efek besar.")
    }
  }
  cat("\\n\\n")
  
  # Hasil Post-hoc jika ada
  if(!is.null(tukey_result) && !is.null(tukey_result$Provinsi)) {
    significant_pairs <- sum(tukey_df$"p adj" < 0.05, na.rm = TRUE)
    total_pairs <- nrow(tukey_df)
    
    cat("**Hasil Uji Post-Hoc (Tukey HSD):**\\n")
    cat("Dari ", total_pairs, " perbandingan berpasangan, ", significant_pairs, 
        " pasangan (", round((significant_pairs/total_pairs)*100, 1), 
        "%) menunjukkan perbedaan yang signifikan.\\n\\n")
    
    if(significant_pairs > 0) {
      cat("Pasangan provinsi dengan perbedaan signifikan:\\n\\n")
      sig_pairs <- tukey_df[tukey_df$"p adj" < 0.05, ]
      for(i in 1:nrow(sig_pairs)) {
        direction <- ifelse(sig_pairs$diff[i] > 0, "lebih tinggi", "lebih rendah")
        cat("- ", sig_pairs$Comparison_Label[i], ": Selisih = ", 
            abs(round(sig_pairs$diff[i], 2)), " (", direction, ")\\n\\n")
      }
    }
    cat("\\n")
  }
  
} else {
  cat("**KESIMPULAN TIDAK SIGNIFIKAN:**\\n\\n")
  cat("Hasil analisis menunjukkan **tidak terdapat perbedaan yang signifikan secara statistik** ")
  cat("dalam rata-rata ", model_data$variable_label, " antar provinsi yang dianalisis ")
  cat("(F = ", round(f_value, 3), ", p = ", 
      format(round(p_value, 6), scientific = FALSE), " > 0.05).\\n\\n")
  
  cat("**Interpretasi:**\\n")
  cat("Meskipun terdapat variasi dalam data antar provinsi, perbedaan ini tidak cukup besar ")
  cat("untuk dianggap signifikan secara statistik. Kondisi ", model_data$variable_label, 
      " dapat dianggap relatif homogen antar provinsi yang dianalisis.\\n\\n")
      
  if(!is.na(r_squared)) {
    cat("Faktor provinsi hanya menjelaskan ", round(r_squared * 100, 2), 
        "% dari variabilitas data, menunjukkan bahwa faktor lain mungkin lebih berpengaruh.")
  }
  cat("\\n\\n")
}
```

## Rekomendasi Tindak Lanjut

```{r rekomendasi-detail, results="asis"}
if(is_significant) {
  cat("**1. Analisis Lanjutan:**\\n\\n")
  cat("- Investigasi mendalam terhadap faktor-faktor yang menyebabkan perbedaan antar provinsi\\n\\n")
  cat("- Analisis korelasi dengan variabel sosial-ekonomi lainnya\\n\\n")
  cat("- Studi longitudinal untuk memahami tren perubahan dari waktu ke waktu\\n\\n")
  
  cat("**2. Intervensi Kebijakan:**\\n\\n")
  if(!is.null(tukey_result) && !is.null(tukey_result$Provinsi)) {
    # Identifikasi provinsi dengan performance tertinggi dan terendah
    prov_means <- aggregate(model_data$data[, model_data$variable], 
                           by = list(model_data$data$Provinsi), FUN = mean, na.rm = TRUE)
    best_prov_id <- prov_means$Group.1[which.max(prov_means$x)]
    worst_prov_id <- prov_means$Group.1[which.min(prov_means$x)]
    
    best_prov_name <- ifelse(!is.null(prov_map) && as.character(best_prov_id) %in% names(prov_map),
                            prov_map[as.character(best_prov_id)], paste("Provinsi", best_prov_id))
    worst_prov_name <- ifelse(!is.null(prov_map) && as.character(worst_prov_id) %in% names(prov_map),
                             prov_map[as.character(worst_prov_id)], paste("Provinsi", worst_prov_id))
    
    cat("- Pelajari best practices dari ", best_prov_name, " (performance terbaik)\\n\\n")
    cat("- Prioritaskan intervensi di ", worst_prov_name, " (memerlukan perhatian khusus)\\n\\n")
  }
  cat("- Rancang program yang disesuaikan dengan karakteristik spesifik masing-masing provinsi\\n\\n")
  cat("- Alokasi sumber daya yang proporsional berdasarkan tingkat kebutuhan\\n\\n")
  
  cat("**3. Monitoring dan Evaluasi:**\\n\\n")
  cat("- Implementasi sistem monitoring berkala (quarterly/yearly)\\n\\n")
  cat("- Penetapan target perbaikan yang realistis untuk setiap provinsi\\n\\n")
  cat("- Evaluasi efektivitas intervensi melalui analisis before-after\\n\\n")
  
} else {
  cat("**Rekomendasi untuk Hasil Tidak Signifikan:**\\n\\n")
  
  cat("**1. Optimasi Strategi Nasional:**\\n\\n")
  cat("- Manfaatkan homogenitas kondisi untuk implementasi program nasional yang seragam\\n\\n")
  cat("- Fokus pada efisiensi dan standardisasi prosedur di semua provinsi\\n\\n")
  cat("- Sharing resources dan best practices antar provinsi\\n\\n")
  
  cat("**2. Eksplorasi Faktor Lain:**\\n\\n")
  cat("- Investigasi faktor-faktor selain provinsi yang mungkin mempengaruhi variabilitas\\n\\n")
  cat("- Analisis pada level yang lebih granular (kabupaten/kota)\\n\\n")
  cat("- Pertimbangkan variabel demografis, geografis, atau temporal\\n\\n")
  
  cat("**3. Peningkatan Kualitas Data:**\\n\\n")
  cat("- Evaluasi metode pengumpulan data untuk mengurangi noise\\n\\n")
  cat("- Pertimbangkan penambahan sampel jika ukuran sampel masih terbatas\\n\\n")
  cat("- Standardisasi instrumen pengukuran antar provinsi\\n\\n")
}

cat("**Rekomendasi Metodologis:**\\n\\n")
if(!is.na(shapiro_result$p.value) && shapiro_result$p.value <= 0.05) {
  cat("- Pertimbangkan transformasi data (log, square root) untuk mengatasi non-normalitas\\n\\n")
}
if("car" %in% rownames(installed.packages()) && !is.na(levene_result[1, "Pr(>F)"]) && levene_result[1, "Pr(>F)"] <= 0.05) {
  cat("- Gunakan Welch ANOVA sebagai alternatif untuk mengatasi heteroskedastisitas\\n\\n")
}
cat("- Pertimbangkan analisis non-parametrik (Kruskal-Wallis) sebagai validasi\\n\\n")
cat("- Lakukan power analysis untuk menentukan ukuran sampel optimal\\n\\n")
```

---

*Laporan dibuat pada ', format(Sys.time(), "%d %B %Y pukul %H:%M WIB"), '*
')

cat("RMD content built successfully!\n")
return(rmd_content)

    }, error = function(e) {
      cat("ERROR in create_anova_rmd:", e$message, "\n")
      # Return minimal working template jika ada error
      return(paste0('---
title: "Laporan Analisis ANOVA Data Kemiskinan"
author: "Dashboard Tingkat Kemiskinan Indonesia"
date: "', format(Sys.Date(), "%d %B %Y"), '"
output: pdf_document
---

# Error dalam Pembuatan Laporan

Terjadi error dalam pembuatan laporan ANOVA: ', e$message, '

Silakan coba lagi atau hubungi administrator.

---

*Laporan dibuat pada ', format(Sys.time(), "%d %B %Y"), '*
'))
    })
  }
  
# Function untuk membuat konten Rmd untuk Laporan Lengkap (Gabungan Deskriptif + ANOVA)
create_lengkap_rmd <- function() {
  
  cat("Starting create_lengkap_rmd function...\n")
  
  tryCatch({
    # Get data dan hasil analisis
    data_filtered <- filtered_data()
    analisis_per_col <- analisis_deskriptif_per_prov_col()
    selected_provs <- input$selected_provinsi
    prov_map <- provinsi_mapping()
    
    # Check ANOVA availability
    anova_exists <- !is.null(input$run_anova) && input$run_anova > 0
    model_data <- NULL
    if(anova_exists) {
      model_data <- tryCatch(anova_model(), error = function(e) NULL)
    }
    
    # Header untuk laporan lengkap
    rmd_content <- paste0('---
title: "Laporan Lengkap Analisis Data Kemiskinan"
subtitle: "Analisis Deskriptif dan ANOVA"
author: "Dashboard Tingkat Kemiskinan Indonesia"
date: "', format(Sys.Date(), "%d %B %Y"), '"
output: 
  pdf_document:
    latex_engine: pdflatex
    toc: true
    toc_depth: 3
    number_sections: true
    extra_dependencies: ["geometry", "titling"]
fontsize: 11pt
geometry: "top=2.5cm, bottom=2cm, left=2cm, right=2cm"
header-includes:
  - \\usepackage{titling}
  - \\setlength{\\droptitle}{-1.5cm}
  - \\pretitle{\\begin{center}\\LARGE\\bfseries}
  - \\posttitle{\\end{center}}
  - \\preauthor{\\begin{center}\\large}
  - \\postauthor{\\end{center}}
  - \\predate{\\begin{center}}
  - \\postdate{\\end{center}}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  error = TRUE,
  fig.align = "center",
  fig.width = 10,
  fig.height = 6
)

# Load libraries dengan error handling
suppressPackageStartupMessages({
  library(knitr)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  tryCatch(library(kableExtra), error = function(e) NULL)
  tryCatch(library(scales), error = function(e) NULL)
  tryCatch(library(car), error = function(e) NULL)
})

options(scipen = 999)
```
\\newpage

# Ringkasan Eksekutif

Laporan ini menyajikan analisis komprehensif data kemiskinan yang meliputi:

1. **Analisis Deskriptif**: Statistik deskriptif untuk **', length(selected_provs), ' provinsi** yang dipilih
2. **Analisis ANOVA**: ', ifelse(anova_exists && !is.null(model_data), 
                                 paste0('Uji perbedaan rata-rata **', model_data$variable_label, '** antar provinsi'), 
                                 'Belum dilakukan atau data tidak tersedia'), '

## Provinsi yang Dianalisis

```{r provinsi-overview}
# Build province data
prov_data <- data.frame(
  No = 1:', length(selected_provs), ',
  Provinsi = c(')
  
  # Build province names safely untuk laporan lengkap
  prov_names <- c()
  kab_counts <- c()
  
  for(prov_id in selected_provs) {
    prov_name <- if(!is.null(prov_map) && as.character(prov_id) %in% names(prov_map)) {
      prov_map[as.character(prov_id)]
    } else {
      paste("Provinsi", prov_id)
    }
    prov_names <- c(prov_names, prov_name)
    
    # Count kabupaten
    count <- data_filtered %>% filter(Provinsi == prov_id) %>% nrow()
    kab_counts <- c(kab_counts, count)
  }
  
  rmd_content <- paste0(rmd_content, paste0('"', prov_names, '"', collapse = ', '), '),
  Jumlah_Kabupaten = c(', paste(kab_counts, collapse = ', '), '),
  Status_ANOVA = c(', 
                        ifelse(anova_exists && !is.null(model_data), 
                               paste0(rep('"Tersedia"', length(selected_provs)), collapse = ', '),
                               paste0(rep('"Tidak Tersedia"', length(selected_provs)), collapse = ', ')), ')
)

kable(prov_data, col.names = c("No", "Provinsi", "Jumlah Kab/Kota", "Status ANOVA"), 
      caption = "Ringkasan Provinsi yang Dianalisis")
```

# BAGIAN I: ANALISIS DESKRIPTIF

## Variabel yang Dianalisis

Analisis deskriptif mencakup lima variabel utama kemiskinan:

1. **X1**: Jumlah Penduduk Miskin (ribu jiwa)
2. **P0**: Persentase Penduduk Miskin (%)
3. **P1**: Indeks Kedalaman Kemiskinan
4. **P2**: Indeks Keparahan Kemiskinan  
5. **X2**: Garis Kemiskinan (Rp/kapita/bulan)

\\newpage
')

# BAGIAN DESKRIPTIF - Loop untuk setiap provinsi
for(i in seq_along(selected_provs)) {
  prov_id <- selected_provs[i]
  prov_name <- prov_names[i]
  
  rmd_content <- paste0(rmd_content, '## Analisis Deskriptif: ', prov_name, '\n\n')
  
  # Statistik deskriptif per provinsi - PERBAIKAN UTAMA
  if(!is.null(analisis_per_col) && !is.null(analisis_per_col[[as.character(prov_id)]])) {
    rmd_content <- paste0(rmd_content, '### Ringkasan Statistik\n\n')
    
    # Tabel statistik - DIPERBAIKI
    rmd_content <- paste0(rmd_content, '```{r stats-desc-', i, '}\n')
    rmd_content <- paste0(rmd_content, 'prov_results <- analisis_per_col[["', prov_id, '"]]\n')
    rmd_content <- paste0(rmd_content, '\n')
    rmd_content <- paste0(rmd_content, 'if(!is.null(prov_results) && is.list(prov_results)) {\n')
    rmd_content <- paste0(rmd_content, '  # Inisialisasi data frame kosong\n')
    rmd_content <- paste0(rmd_content, '  stats_data <- data.frame(\n')
    rmd_content <- paste0(rmd_content, '    Variabel = character(0),\n')
    rmd_content <- paste0(rmd_content, '    Mean = character(0),\n')
    rmd_content <- paste0(rmd_content, '    Median = character(0),\n')
    rmd_content <- paste0(rmd_content, '    SD = character(0),\n')
    rmd_content <- paste0(rmd_content, '    Min = character(0),\n')
    rmd_content <- paste0(rmd_content, '    Max = character(0),\n')
    rmd_content <- paste0(rmd_content, '    N = character(0),\n')
    rmd_content <- paste0(rmd_content, '    stringsAsFactors = FALSE\n')
    rmd_content <- paste0(rmd_content, '  )\n')
    rmd_content <- paste0(rmd_content, '  \n')
    rmd_content <- paste0(rmd_content, '  var_labels <- c(\n')
    rmd_content <- paste0(rmd_content, '    "X1" = "Jumlah Penduduk Miskin (ribu jiwa)",\n')
    rmd_content <- paste0(rmd_content, '    "P0" = "Persentase Penduduk Miskin (%)",\n')
    rmd_content <- paste0(rmd_content, '    "P1" = "Indeks Kedalaman Kemiskinan",\n')
    rmd_content <- paste0(rmd_content, '    "P2" = "Indeks Keparahan Kemiskinan",\n')
    rmd_content <- paste0(rmd_content, '    "X2" = "Garis Kemiskinan (Rp/kapita/bulan)"\n')
    rmd_content <- paste0(rmd_content, '  )\n')
    rmd_content <- paste0(rmd_content, '  \n')
    rmd_content <- paste0(rmd_content, '  # Loop melalui variabel dengan penanganan error yang lebih baik\n')
    rmd_content <- paste0(rmd_content, '  for(var_name in names(var_labels)) {\n')
    rmd_content <- paste0(rmd_content, '    if(var_name %in% names(prov_results)) {\n')
    rmd_content <- paste0(rmd_content, '      var_result <- prov_results[[var_name]]\n')
    rmd_content <- paste0(rmd_content, '      if(is.list(var_result) && !is.null(var_result$mean)) {\n')
    rmd_content <- paste0(rmd_content, '        # Safe conversion dengan format yang konsisten\n')
    rmd_content <- paste0(rmd_content, '        safe_mean <- tryCatch({\n')
    rmd_content <- paste0(rmd_content, '          if(is.numeric(var_result$mean) && !is.na(var_result$mean)) {\n')
    rmd_content <- paste0(rmd_content, '            as.character(round(var_result$mean, 2))\n')
    rmd_content <- paste0(rmd_content, '          } else { "N/A" }\n')
    rmd_content <- paste0(rmd_content, '        }, error = function(e) "N/A")\n')
    rmd_content <- paste0(rmd_content, '        \n')
    rmd_content <- paste0(rmd_content, '        safe_median <- tryCatch({\n')
    rmd_content <- paste0(rmd_content, '          if(is.numeric(var_result$median) && !is.na(var_result$median)) {\n')
    rmd_content <- paste0(rmd_content, '            as.character(round(var_result$median, 2))\n')
    rmd_content <- paste0(rmd_content, '          } else { "N/A" }\n')
    rmd_content <- paste0(rmd_content, '        }, error = function(e) "N/A")\n')
    rmd_content <- paste0(rmd_content, '        \n')
    rmd_content <- paste0(rmd_content, '        safe_sd <- tryCatch({\n')
    rmd_content <- paste0(rmd_content, '          if(is.numeric(var_result$sd) && !is.na(var_result$sd)) {\n')
    rmd_content <- paste0(rmd_content, '            as.character(round(var_result$sd, 2))\n')
    rmd_content <- paste0(rmd_content, '          } else { "N/A" }\n')
    rmd_content <- paste0(rmd_content, '        }, error = function(e) "N/A")\n')
    rmd_content <- paste0(rmd_content, '        \n')
    rmd_content <- paste0(rmd_content, '        safe_min <- tryCatch({\n')
    rmd_content <- paste0(rmd_content, '          if(is.numeric(var_result$min) && !is.na(var_result$min)) {\n')
    rmd_content <- paste0(rmd_content, '            as.character(round(var_result$min, 2))\n')
    rmd_content <- paste0(rmd_content, '          } else { "N/A" }\n')
    rmd_content <- paste0(rmd_content, '        }, error = function(e) "N/A")\n')
    rmd_content <- paste0(rmd_content, '        \n')
    rmd_content <- paste0(rmd_content, '        safe_max <- tryCatch({\n')
    rmd_content <- paste0(rmd_content, '          if(is.numeric(var_result$max) && !is.na(var_result$max)) {\n')
    rmd_content <- paste0(rmd_content, '            as.character(round(var_result$max, 2))\n')
    rmd_content <- paste0(rmd_content, '          } else { "N/A" }\n')
    rmd_content <- paste0(rmd_content, '        }, error = function(e) "N/A")\n')
    rmd_content <- paste0(rmd_content, '        \n')
    rmd_content <- paste0(rmd_content, '        safe_n <- tryCatch({\n')
    rmd_content <- paste0(rmd_content, '          if(is.numeric(var_result$n) && !is.na(var_result$n)) {\n')
    rmd_content <- paste0(rmd_content, '            as.character(as.integer(var_result$n))\n')
    rmd_content <- paste0(rmd_content, '          } else { "0" }\n')
    rmd_content <- paste0(rmd_content, '        }, error = function(e) "0")\n')
    rmd_content <- paste0(rmd_content, '        \n')
    rmd_content <- paste0(rmd_content, '        # Tambahkan baris ke data frame\n')
    rmd_content <- paste0(rmd_content, '        new_row <- data.frame(\n')
    rmd_content <- paste0(rmd_content, '          Variabel = var_labels[var_name],\n')
    rmd_content <- paste0(rmd_content, '          Mean = safe_mean,\n')
    rmd_content <- paste0(rmd_content, '          Median = safe_median,\n')
    rmd_content <- paste0(rmd_content, '          SD = safe_sd,\n')
    rmd_content <- paste0(rmd_content, '          Min = safe_min,\n')
    rmd_content <- paste0(rmd_content, '          Max = safe_max,\n')
    rmd_content <- paste0(rmd_content, '          N = safe_n,\n')
    rmd_content <- paste0(rmd_content, '          stringsAsFactors = FALSE\n')
    rmd_content <- paste0(rmd_content, '        )\n')
    rmd_content <- paste0(rmd_content, '        stats_data <- rbind(stats_data, new_row)\n')
    rmd_content <- paste0(rmd_content, '      }\n')
    rmd_content <- paste0(rmd_content, '    }\n')
    rmd_content <- paste0(rmd_content, '  }\n')
    rmd_content <- paste0(rmd_content, '  \n')
    rmd_content <- paste0(rmd_content, '  if(nrow(stats_data) > 0) {\n')
    rmd_content <- paste0(rmd_content, '    kable(stats_data, caption = paste("Statistik Deskriptif -", "', prov_name, '"))\n')
    rmd_content <- paste0(rmd_content, '  } else {\n')
    rmd_content <- paste0(rmd_content, '    cat("Tidak ada data statistik yang tersedia.\\n")\n')
    rmd_content <- paste0(rmd_content, '  }\n')
    rmd_content <- paste0(rmd_content, '} else {\n')
    rmd_content <- paste0(rmd_content, '  cat("Data tidak tersedia untuk provinsi ini.\\n")\n')
    rmd_content <- paste0(rmd_content, '}\n')
    rmd_content <- paste0(rmd_content, '```\n\n')
    
    # Visualisasi - DIPERBAIKI POSISI DAN UKURAN
    rmd_content <- paste0(rmd_content, '### Distribusi Data\n\n')
    rmd_content <- paste0(rmd_content, '```{r plot-desc-', i, ', fig.cap="Distribusi Variabel Kemiskinan - ', prov_name, '"}\n')
    rmd_content <- paste0(rmd_content, 'tryCatch({\n')
    rmd_content <- paste0(rmd_content, '  df_prov <- data_filtered[data_filtered$Provinsi == ', prov_id, ', ]\n')
    rmd_content <- paste0(rmd_content, '  \n')
    rmd_content <- paste0(rmd_content, '  if(nrow(df_prov) > 0) {\n')
    rmd_content <- paste0(rmd_content, '    available_cols <- c()\n')
    rmd_content <- paste0(rmd_content, '    target_cols <- c("X1", "P0", "P1", "P2", "X2")\n')
    rmd_content <- paste0(rmd_content, '    \n')
    rmd_content <- paste0(rmd_content, '    for(col in target_cols) {\n')
    rmd_content <- paste0(rmd_content, '      if(col %in% colnames(df_prov) && any(!is.na(df_prov[[col]]))) {\n')
    rmd_content <- paste0(rmd_content, '        available_cols <- c(available_cols, col)\n')
    rmd_content <- paste0(rmd_content, '      }\n')
    rmd_content <- paste0(rmd_content, '    }\n')
    rmd_content <- paste0(rmd_content, '    \n')
    rmd_content <- paste0(rmd_content, '    if(length(available_cols) > 0) {\n')
    rmd_content <- paste0(rmd_content, '      long_data <- data.frame(\n')
    rmd_content <- paste0(rmd_content, '        Variabel = character(0),\n')
    rmd_content <- paste0(rmd_content, '        Nilai = numeric(0)\n')
    rmd_content <- paste0(rmd_content, '      )\n')
    rmd_content <- paste0(rmd_content, '      \n')
    rmd_content <- paste0(rmd_content, '      for(col in available_cols) {\n')
    rmd_content <- paste0(rmd_content, '        values <- df_prov[[col]]\n')
    rmd_content <- paste0(rmd_content, '        valid_values <- values[!is.na(values) & is.numeric(values)]\n')
    rmd_content <- paste0(rmd_content, '        if(length(valid_values) > 0) {\n')
    rmd_content <- paste0(rmd_content, '          temp_df <- data.frame(\n')
    rmd_content <- paste0(rmd_content, '            Variabel = rep(col, length(valid_values)),\n')
    rmd_content <- paste0(rmd_content, '            Nilai = valid_values\n')
    rmd_content <- paste0(rmd_content, '          )\n')
    rmd_content <- paste0(rmd_content, '          long_data <- rbind(long_data, temp_df)\n')
    rmd_content <- paste0(rmd_content, '        }\n')
    rmd_content <- paste0(rmd_content, '      }\n')
    rmd_content <- paste0(rmd_content, '      \n')
    rmd_content <- paste0(rmd_content, '      if(nrow(long_data) > 0) {\n')
    rmd_content <- paste0(rmd_content, '        # PERBAIKAN: Plot dengan spacing yang lebih baik\n')
    rmd_content <- paste0(rmd_content, '        p <- ggplot(long_data, aes(x = factor(1), y = Nilai)) +\n')
    rmd_content <- paste0(rmd_content, '          geom_boxplot(fill = "lightblue", alpha = 0.7, width = 0.6) +\n')
    rmd_content <- paste0(rmd_content, '          geom_jitter(width = 0.2, alpha = 0.6, color = "darkblue", size = 1) +\n')
    rmd_content <- paste0(rmd_content, '          facet_wrap(~ Variabel, scales = "free_y", ncol = 5) +\n')
    rmd_content <- paste0(rmd_content, '          labs(title = paste("Distribusi Variabel Kemiskinan -", "', prov_name, '"), \n')
    rmd_content <- paste0(rmd_content, '               x = "", y = "Nilai") +\n')
    rmd_content <- paste0(rmd_content, '          theme_minimal() +\n')
    rmd_content <- paste0(rmd_content, '          theme(\n')
    rmd_content <- paste0(rmd_content, '            axis.text.x = element_blank(),\n')
    rmd_content <- paste0(rmd_content, '            axis.ticks.x = element_blank(),\n')
    rmd_content <- paste0(rmd_content, '            plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),\n')
    rmd_content <- paste0(rmd_content, '            strip.text = element_text(face = "bold", size = 10),\n')
    rmd_content <- paste0(rmd_content, '            panel.spacing = unit(1, "lines")\n')
    rmd_content <- paste0(rmd_content, '          )\n')
    rmd_content <- paste0(rmd_content, '        \n')
    rmd_content <- paste0(rmd_content, '        # Format Y axis labels jika package tersedia\n')
    rmd_content <- paste0(rmd_content, '        if("scales" %in% rownames(installed.packages())) {\n')
    rmd_content <- paste0(rmd_content, '          p <- p + scale_y_continuous(labels = scales::comma)\n')
    rmd_content <- paste0(rmd_content, '        }\n')
    rmd_content <- paste0(rmd_content, '        \n')
    rmd_content <- paste0(rmd_content, '        print(p)\n')
    rmd_content <- paste0(rmd_content, '      } else {\n')
    rmd_content <- paste0(rmd_content, '        plot.new()\n')
    rmd_content <- paste0(rmd_content, '        text(0.5, 0.5, "Tidak ada data numerik yang valid", cex = 1.5)\n')
    rmd_content <- paste0(rmd_content, '      }\n')
    rmd_content <- paste0(rmd_content, '    } else {\n')
    rmd_content <- paste0(rmd_content, '      plot.new()\n')
    rmd_content <- paste0(rmd_content, '      text(0.5, 0.5, "Tidak ada kolom data yang valid", cex = 1.5)\n')
    rmd_content <- paste0(rmd_content, '    }\n')
    rmd_content <- paste0(rmd_content, '  } else {\n')
    rmd_content <- paste0(rmd_content, '    plot.new()\n')
    rmd_content <- paste0(rmd_content, '    text(0.5, 0.5, "Tidak ada data untuk provinsi ini", cex = 1.5)\n')
    rmd_content <- paste0(rmd_content, '  }\n')
    rmd_content <- paste0(rmd_content, '}, error = function(e) {\n')
    rmd_content <- paste0(rmd_content, '  plot.new()\n')
    rmd_content <- paste0(rmd_content, '  text(0.5, 0.5, paste("Error:", e$message), cex = 1, col = "red")\n')
    rmd_content <- paste0(rmd_content, '})\n')
    rmd_content <- paste0(rmd_content, '```\n\\newpage')
  }
  
  # Tambahkan page break kecuali untuk provinsi terakhir
  if(i < length(selected_provs)) {
    rmd_content <- paste0(rmd_content, '\\newpage\n\n')
  }
}

# BAGIAN ANOVA
rmd_content <- paste0(rmd_content, '\\newpage\n\n')
rmd_content <- paste0(rmd_content, '# BAGIAN II: ANALISIS ANOVA\n\n')

if(anova_exists && !is.null(model_data)) {
  # Extract ANOVA results
  anova_result <- tryCatch(summary(model_data$model), error = function(e) NULL)
  
  if(!is.null(anova_result)) {
    p_value <- tryCatch(anova_result[[1]][1, "Pr(>F)"], error = function(e) NA)
    f_value <- tryCatch(anova_result[[1]][1, "F value"], error = function(e) NA)
    df1 <- tryCatch(anova_result[[1]][1, "Df"], error = function(e) NA)
    df2 <- tryCatch(anova_result[[1]][2, "Df"], error = function(e) NA)
    ss_between <- tryCatch(anova_result[[1]][1, "Sum Sq"], error = function(e) NA)
    ss_within <- tryCatch(anova_result[[1]][2, "Sum Sq"], error = function(e) NA)
    ms_between <- tryCatch(anova_result[[1]][1, "Mean Sq"], error = function(e) NA)
    ms_within <- tryCatch(anova_result[[1]][2, "Mean Sq"], error = function(e) NA)
    
    # Calculate R-squared safely
    r_squared <- tryCatch({
      if(!is.na(ss_between) && !is.na(ss_within)) {
        ss_total <- ss_between + ss_within
        ss_between / ss_total
      } else {
        NA
      }
    }, error = function(e) NA)
    
    is_significant <- !is.na(p_value) && p_value < 0.05
    
    # Build safe province names for ANOVA
    prov_names_safe <- c()
    obs_counts <- c()
    
    for(prov_id in selected_provs) {
      prov_name <- tryCatch({
        if(!is.null(prov_map) && as.character(prov_id) %in% names(prov_map)) {
          prov_map[as.character(prov_id)]
        } else {
          paste("Provinsi", prov_id)
        }
      }, error = function(e) paste("Provinsi", prov_id))
      
      prov_name_safe <- gsub('"', "", prov_name)
      prov_names_safe <- c(prov_names_safe, prov_name_safe)
      
      count <- tryCatch({
        sum(model_data$data$Provinsi == prov_id, na.rm = TRUE)
      }, error = function(e) 0)
      obs_counts <- c(obs_counts, count)
    }
    
    # Add ANOVA setup
    rmd_content <- paste0(rmd_content, '```{r anova-setup, include=FALSE}\n')
    rmd_content <- paste0(rmd_content, '# Store analysis variables for use in ANOVA chunks\n')
    rmd_content <- paste0(rmd_content, 'p_value <- ', ifelse(is.na(p_value), 'NA', format(p_value, scientific = FALSE)), '\n')
    rmd_content <- paste0(rmd_content, 'f_value <- ', ifelse(is.na(f_value), 'NA', format(f_value, scientific = FALSE)), '\n')
    rmd_content <- paste0(rmd_content, 'r_squared <- ', ifelse(is.na(r_squared), 'NA', format(r_squared, scientific = FALSE)), '\n')
    rmd_content <- paste0(rmd_content, 'is_significant <- ', ifelse(is_significant, 'TRUE', 'FALSE'), '\n')
    rmd_content <- paste0(rmd_content, '```\n\n')
    
    # ANOVA content - reuse struktur dari create_anova_rmd dengan modifikasi
    rmd_content <- paste0(rmd_content, '## Informasi Analisis ANOVA\n\n')
    rmd_content <- paste0(rmd_content, 'Analisis ini menguji perbedaan rata-rata **', model_data$variable_label, '** antar ', length(unique(model_data$data$Provinsi)), ' provinsi.\n\n')
    
    # Info basic table
    rmd_content <- paste0(rmd_content, '```{r anova-info-basic}\n')
    rmd_content <- paste0(rmd_content, 'info_table <- data.frame(\n')
    rmd_content <- paste0(rmd_content, '  Aspek = c("Variabel yang Diuji", "Jumlah Provinsi", "Total Observasi", "Metode Analisis"),\n')
    rmd_content <- paste0(rmd_content, '  Keterangan = c("', model_data$variable_label, '", "', 
                          length(unique(model_data$data$Provinsi)), '", "', 
                          nrow(model_data$data), '", "One-Way ANOVA")\n')
    rmd_content <- paste0(rmd_content, ')\n')
    rmd_content <- paste0(rmd_content, 'kable(info_table, caption = "Informasi Analisis ANOVA")\n')
    rmd_content <- paste0(rmd_content, '```\n\n')
    
    # Hasil ANOVA utama
    if(is_significant) {
      result_conclusion <- "SIGNIFIKAN - Terdapat perbedaan rata-rata yang signifikan antar provinsi"
      decision <- "Tolak H0, terima H1"
    } else {
      result_conclusion <- "TIDAK SIGNIFIKAN - Tidak terdapat perbedaan rata-rata yang signifikan antar provinsi"
      decision <- "Gagal menolak H0"
    }
    
    rmd_content <- paste0(rmd_content, '## Hasil Utama ANOVA\n\n')
    rmd_content <- paste0(rmd_content, '```{r anova-hasil-utama}\n')
    rmd_content <- paste0(rmd_content, 'hasil_summary <- data.frame(\n')
    rmd_content <- paste0(rmd_content, '  Statistik = c("F-statistic", "P-value", "R-squared", "Kesimpulan", "Keputusan"),\n')
    rmd_content <- paste0(rmd_content, '  Nilai = c("', 
                          ifelse(is.na(f_value), "NA", round(f_value, 3)), '", "', 
                          ifelse(is.na(p_value), "NA", ifelse(p_value < 0.0001, "< 0.0001", format(round(p_value, 6), scientific = FALSE))), '", "', 
                          ifelse(is.na(r_squared), "NA", paste0(round(r_squared * 100, 2), "%")), '", "',
                          result_conclusion, '", "', decision, '")\n')
    rmd_content <- paste0(rmd_content, ')\n')
    rmd_content <- paste0(rmd_content, 'kable(hasil_summary, caption = "Ringkasan Hasil ANOVA")\n')
    rmd_content <- paste0(rmd_content, '```\n\n')
    
    # Interpretasi ANOVA
    rmd_content <- paste0(rmd_content, '## Interpretasi Hasil ANOVA\n\n')
    rmd_content <- paste0(rmd_content, '```{r anova-interpretasi, results="asis"}\n')
    rmd_content <- paste0(rmd_content, 'cat("**Interpretasi Hasil ANOVA:**\\n\\n")\n')
    rmd_content <- paste0(rmd_content, '\n')
    rmd_content <- paste0(rmd_content, 'if(is_significant) {\n')
    rmd_content <- paste0(rmd_content, '  cat("Hasil analisis ANOVA menunjukkan **perbedaan yang signifikan** antar provinsi:\\n\\n")\n')
    rmd_content <- paste0(rmd_content, '  cat("- **F-statistic =", ifelse(is.na(f_value), "NA", round(f_value, 3)), ":** ")\n')
    rmd_content <- paste0(rmd_content, '  cat("Nilai F yang besar mengindikasikan bahwa variabilitas antar grup (provinsi) ")\n')
    rmd_content <- paste0(rmd_content, '  cat("lebih besar dibandingkan variabilitas dalam grup.\\n\\n")\n')
    rmd_content <- paste0(rmd_content, '  cat("- **P-value =", ifelse(is.na(p_value), "NA", \n')
    rmd_content <- paste0(rmd_content, '      ifelse(p_value < 0.0001, "< 0.0001", format(round(p_value, 6), scientific = FALSE))), ":** ")\n')
    rmd_content <- paste0(rmd_content, '  cat("Nilai p < 0.05 menunjukkan bukti statistik yang kuat untuk menolak hipotesis nul. ")\n')
    rmd_content <- paste0(rmd_content, '  cat("Artinya, rata-rata ", model_data$variable_label, " berbeda signifikan antar provinsi.\\n\\n")\n')
    rmd_content <- paste0(rmd_content, '  cat("- **R-squared =", ifelse(is.na(r_squared), "NA", paste0(round(r_squared * 100, 2), "%")), ":** ")\n')
    rmd_content <- paste0(rmd_content, '  if(!is.na(r_squared)) {\n')
    rmd_content <- paste0(rmd_content, '    if(r_squared < 0.25) {\n')
    rmd_content <- paste0(rmd_content, '      cat("Efek kecil - faktor provinsi menjelaskan sebagian kecil variabilitas data.")\n')
    rmd_content <- paste0(rmd_content, '    } else if(r_squared < 0.50) {\n')
    rmd_content <- paste0(rmd_content, '      cat("Efek sedang - faktor provinsi menjelaskan sebagian besar variabilitas data.")\n')
    rmd_content <- paste0(rmd_content, '    } else {\n')
    rmd_content <- paste0(rmd_content, '      cat("Efek besar - faktor provinsi menjelaskan sebagian besar variabilitas data.")\n')
    rmd_content <- paste0(rmd_content, '    }\n')
    rmd_content <- paste0(rmd_content, '  }\n')
    rmd_content <- paste0(rmd_content, '} else {\n')
    rmd_content <- paste0(rmd_content, '  cat("Hasil analisis ANOVA menunjukkan **tidak ada perbedaan yang signifikan** antar provinsi:\\n\\n")\n')
    rmd_content <- paste0(rmd_content, '  cat("- **F-statistic =", ifelse(is.na(f_value), "NA", round(f_value, 3)), ":** ")\n')
    rmd_content <- paste0(rmd_content, '  cat("Nilai F yang relatif kecil mengindikasikan bahwa variabilitas antar grup (provinsi) ")\n')
    rmd_content <- paste0(rmd_content, '  cat("tidak lebih besar secara signifikan dibandingkan variabilitas dalam grup.\\n\\n")\n')
    rmd_content <- paste0(rmd_content, '  cat("- **P-value =", ifelse(is.na(p_value), "NA", \n')
    rmd_content <- paste0(rmd_content, '      ifelse(p_value < 0.0001, "< 0.0001", format(round(p_value, 6), scientific = FALSE))), ":** ")\n')
    rmd_content <- paste0(rmd_content, '  cat("Nilai p >= 0.05 menunjukkan tidak ada bukti statistik yang cukup untuk menolak hipotesis nul. ")\n')
    rmd_content <- paste0(rmd_content, '  cat("Artinya, tidak ada perbedaan rata-rata ", model_data$variable_label, " yang signifikan antar provinsi.\\n\\n")\n')
    rmd_content <- paste0(rmd_content, '  if(!is.na(r_squared)) {\n')
    rmd_content <- paste0(rmd_content, '    cat("Faktor provinsi hanya menjelaskan ", round(r_squared * 100, 2), "% variabilitas dalam data.")\n')
    rmd_content <- paste0(rmd_content, '  }\n')
    rmd_content <- paste0(rmd_content, '}\n')
    rmd_content <- paste0(rmd_content, '```\n\n')
    rmd_content <- paste0(rmd_content, '\\newpage')
    
    # Visualisasi ANOVA
    rmd_content <- paste0(rmd_content, '## Visualisasi Perbandingan Antar Provinsi\n\n')
    rmd_content <- paste0(rmd_content, '```{r anova-comparison-plot, fig.height=4, fig.width=8, fig.cap="Perbandingan Antar Provinsi"}\n')
    rmd_content <- paste0(rmd_content, 'tryCatch({\n')
    rmd_content <- paste0(rmd_content, '  plot_data <- model_data$data\n')
    rmd_content <- paste0(rmd_content, '  var_name <- model_data$variable\n')
    rmd_content <- paste0(rmd_content, '  \n')
    rmd_content <- paste0(rmd_content, '  if(!is.null(prov_map)) {\n')
    rmd_content <- paste0(rmd_content, '    plot_data$Provinsi_Label <- factor(\n')
    rmd_content <- paste0(rmd_content, '      prov_map[as.character(plot_data$Provinsi)],\n')
    rmd_content <- paste0(rmd_content, '      levels = prov_map[as.character(sort(unique(plot_data$Provinsi)))]\n')
    rmd_content <- paste0(rmd_content, '    )\n')
    rmd_content <- paste0(rmd_content, '  } else {\n')
    rmd_content <- paste0(rmd_content, '    plot_data$Provinsi_Label <- factor(paste("Provinsi", plot_data$Provinsi))\n')
    rmd_content <- paste0(rmd_content, '  }\n')
    rmd_content <- paste0(rmd_content, '  \n')
    rmd_content <- paste0(rmd_content, '  p <- ggplot(plot_data, aes(x = Provinsi_Label, y = .data[[var_name]], fill = Provinsi_Label)) +\n')
    rmd_content <- paste0(rmd_content, '    geom_boxplot(alpha = 0.7, outlier.size = 2) +\n')
    rmd_content <- paste0(rmd_content, '    geom_jitter(width = 0.2, alpha = 0.5, size = 1.5) +\n')
    rmd_content <- paste0(rmd_content, '    labs(title = paste("Perbandingan", model_data$variable_label, "Antar Provinsi"),\n')
    rmd_content <- paste0(rmd_content, '         x = "Provinsi", y = model_data$variable_label) +\n')
    rmd_content <- paste0(rmd_content, '    theme_minimal() +\n')
    rmd_content <- paste0(rmd_content, '    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +\n')
    rmd_content <- paste0(rmd_content, '    scale_fill_brewer(type = "qual", palette = "Set3")\n')
    rmd_content <- paste0(rmd_content, '  \n')
    rmd_content <- paste0(rmd_content, '  print(p)\n')
    rmd_content <- paste0(rmd_content, '}, error = function(e) {\n')
    rmd_content <- paste0(rmd_content, '  plot.new()\n')
    rmd_content <- paste0(rmd_content, '  text(0.5, 0.5, "Error dalam visualisasi", cex = 1.2, col = "red")\n')
    rmd_content <- paste0(rmd_content, '})\n')
    rmd_content <- paste0(rmd_content, '```\n\n')
    
    # Post-hoc test jika signifikan
    if(is_significant) {
      rmd_content <- paste0(rmd_content, '## Uji Post-Hoc (Tukey HSD)\n\n')
      rmd_content <- paste0(rmd_content, '### Hasil Tukey HSD\n\n')
      rmd_content <- paste0(rmd_content, '```{r anova-tukey-test}\n')
      rmd_content <- paste0(rmd_content, 'tukey_result <- tryCatch(TukeyHSD(model_data$model, conf.level = 0.95),\n')
      rmd_content <- paste0(rmd_content, '                        error = function(e) NULL)\n')
      rmd_content <- paste0(rmd_content, '\n')
      rmd_content <- paste0(rmd_content, 'if(!is.null(tukey_result) && !is.null(tukey_result$Provinsi)) {\n')
      rmd_content <- paste0(rmd_content, '  tukey_df <- as.data.frame(tukey_result$Provinsi)\n')
      rmd_content <- paste0(rmd_content, '  tukey_df$Comparison <- rownames(tukey_df)\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  # PERBAIKAN: Akses kolom dengan cara yang aman\n')
      rmd_content <- paste0(rmd_content, '  p_adj_col <- tukey_df[["p adj"]]\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  if(!is.null(prov_map)) {\n')
      rmd_content <- paste0(rmd_content, '    tukey_df$Comparison_Label <- sapply(tukey_df$Comparison, function(comp) {\n')
      rmd_content <- paste0(rmd_content, '      parts <- strsplit(comp, "-")[[1]]\n')
      rmd_content <- paste0(rmd_content, '      if(length(parts) == 2) {\n')
      rmd_content <- paste0(rmd_content, '        prov1 <- ifelse(parts[1] %in% names(prov_map), prov_map[parts[1]], parts[1])\n')
      rmd_content <- paste0(rmd_content, '        prov2 <- ifelse(parts[2] %in% names(prov_map), prov_map[parts[2]], parts[2])\n')
      rmd_content <- paste0(rmd_content, '        return(paste(prov1, "vs", prov2))\n')
      rmd_content <- paste0(rmd_content, '      }\n')
      rmd_content <- paste0(rmd_content, '      return(comp)\n')
      rmd_content <- paste0(rmd_content, '    })\n')
      rmd_content <- paste0(rmd_content, '  } else {\n')
      rmd_content <- paste0(rmd_content, '    tukey_df$Comparison_Label <- tukey_df$Comparison\n')
      rmd_content <- paste0(rmd_content, '  }\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  # PERBAIKAN: Buat display table dengan error handling\n')
      rmd_content <- paste0(rmd_content, '  tukey_display <- data.frame(\n')
      rmd_content <- paste0(rmd_content, '    Perbandingan = tukey_df$Comparison_Label,\n')
      rmd_content <- paste0(rmd_content, '    Perbedaan = round(tukey_df$diff, 4),\n')
      rmd_content <- paste0(rmd_content, '    P_value = ifelse(p_adj_col < 0.0001, "< 0.0001", \n')
      rmd_content <- paste0(rmd_content, '                     format(round(p_adj_col, 6), scientific = FALSE)),\n')
      rmd_content <- paste0(rmd_content, '    Signifikan = ifelse(p_adj_col < 0.05, "Ya", "Tidak")\n')
      rmd_content <- paste0(rmd_content, '  )\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  kable(tukey_display, caption = "Hasil Uji Tukey HSD")\n')
      rmd_content <- paste0(rmd_content, '} else {\n')
      rmd_content <- paste0(rmd_content, '  cat("Uji Tukey HSD tidak dapat dilakukan.\\n")\n')
      rmd_content <- paste0(rmd_content, '}\n')
      rmd_content <- paste0(rmd_content, '```\n\n')
      rmd_content <- paste0(rmd_content, '\\newpage \n')
      
      # VISUALISASI TUKEY HSD - DISEDERHANAKAN
      rmd_content <- paste0(rmd_content, '### Visualisasi Hasil Tukey HSD\n\n')
      rmd_content <- paste0(rmd_content, '```{r tukey-plot, fig.height=6, fig.width=8, fig.cap="Confidence Intervals Tukey HSD"}\n')
      rmd_content <- paste0(rmd_content, 'if(!is.null(tukey_result) && !is.null(tukey_result$Provinsi)) {\n')
      rmd_content <- paste0(rmd_content, '  tryCatch({\n')
      rmd_content <- paste0(rmd_content, '    # Set margin untuk label panjang\n')
      rmd_content <- paste0(rmd_content, '    old_par <- par(mar = c(5, 12, 4, 2))\n')
      rmd_content <- paste0(rmd_content, '    \n')
      rmd_content <- paste0(rmd_content, '    # Plot Tukey HSD sederhana\n')
      rmd_content <- paste0(rmd_content, '    plot(tukey_result, las = 1, cex.axis = 0.7, cex.lab = 0.9)\n')
      rmd_content <- paste0(rmd_content, '    \n')
      rmd_content <- paste0(rmd_content, '    # Garis referensi di 0\n')
      rmd_content <- paste0(rmd_content, '    abline(v = 0, col = "red", lty = 2, lwd = 2)\n')
      rmd_content <- paste0(rmd_content, '    \n')
      rmd_content <- paste0(rmd_content, '    # Judul\n')
      rmd_content <- paste0(rmd_content, '    title(\n')
      rmd_content <- paste0(rmd_content, '          sub = "Interval yang tidak melewati garis merah = signifikan",\n')
      rmd_content <- paste0(rmd_content, '          cex.main = 1.1, cex.sub = 0.8)\n')
      rmd_content <- paste0(rmd_content, '    \n')
      rmd_content <- paste0(rmd_content, '    # Restore par\n')
      rmd_content <- paste0(rmd_content, '    par(old_par)\n')
      rmd_content <- paste0(rmd_content, '    \n')
      rmd_content <- paste0(rmd_content, '  }, error = function(e) {\n')
      rmd_content <- paste0(rmd_content, '    plot.new()\n')
      rmd_content <- paste0(rmd_content, '    text(0.5, 0.5, "Error dalam plot Tukey HSD", cex = 1.2, col = "red")\n')
      rmd_content <- paste0(rmd_content, '  })\n')
      rmd_content <- paste0(rmd_content, '} else {\n')
      rmd_content <- paste0(rmd_content, '  plot.new()\n')
      rmd_content <- paste0(rmd_content, '  text(0.5, 0.5, "Data Tukey HSD tidak tersedia", cex = 1.2, col = "gray50")\n')
      rmd_content <- paste0(rmd_content, '}\n')
      rmd_content <- paste0(rmd_content, '```\n\n')
      
      # INTERPRETASI TUKEY - DIPERBAIKI
      rmd_content <- paste0(rmd_content, '### Interpretasi Uji Tukey HSD\n\n')
      rmd_content <- paste0(rmd_content, '```{r anova-tukey-interpretasi, results="asis"}\n')
      rmd_content <- paste0(rmd_content, 'if(!is.null(tukey_result) && !is.null(tukey_result$Provinsi)) {\n')
      rmd_content <- paste0(rmd_content, '  # PERBAIKAN: Akses kolom p adj dengan cara yang aman\n')
      rmd_content <- paste0(rmd_content, '  tukey_data <- as.data.frame(tukey_result$Provinsi)\n')
      rmd_content <- paste0(rmd_content, '  p_adj_values <- tukey_data[["p adj"]]\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  significant_comparisons <- sum(p_adj_values < 0.05, na.rm = TRUE)\n')
      rmd_content <- paste0(rmd_content, '  total_comparisons <- length(p_adj_values)\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  cat("**Ringkasan Post-Hoc:**\\n\\n")\n')
      rmd_content <- paste0(rmd_content, '  cat("- Total perbandingan: ", total_comparisons, " pasang provinsi\\n\\n")\n')
      rmd_content <- paste0(rmd_content, '  cat("- Perbandingan signifikan: ", significant_comparisons, " pasang provinsi\\n\\n")\n')
      rmd_content <- paste0(rmd_content, '  cat("- Persentase signifikan: ", round((significant_comparisons/total_comparisons)*100, 1), "%\\n\\n")\n')
      rmd_content <- paste0(rmd_content, '  \n')
      rmd_content <- paste0(rmd_content, '  if(significant_comparisons > 0) {\n')
      rmd_content <- paste0(rmd_content, '    cat("Hasil ini menunjukkan bahwa terdapat perbedaan yang signifikan antar provinsi tertentu, ")\n')
      rmd_content <- paste0(rmd_content, '    cat("yang mengkonfirmasi hasil ANOVA overall.\\n")\n')
      rmd_content <- paste0(rmd_content, '  } else {\n')
      rmd_content <- paste0(rmd_content, '    cat("Meskipun ANOVA overall signifikan, tidak ada pasangan provinsi yang berbeda signifikan ")\n')
      rmd_content <- paste0(rmd_content, '    cat("dalam uji post-hoc. Hal ini menunjukkan perbedaan yang terdistribusi merata.\\n")\n')
      rmd_content <- paste0(rmd_content, '  }\n')
      rmd_content <- paste0(rmd_content, '} else {\n')
      rmd_content <- paste0(rmd_content, '  cat("Data Tukey HSD tidak tersedia untuk interpretasi.\\n")\n')
      rmd_content <- paste0(rmd_content, '}\n')
      rmd_content <- paste0(rmd_content, '```\n\n')
    }
    
  } else {
    rmd_content <- paste0(rmd_content, 'Analisis ANOVA tidak dapat dilakukan karena terjadi error dalam memproses model.\n\n')
  }
} else {
  rmd_content <- paste0(rmd_content, 'Analisis ANOVA belum dilakukan atau data tidak tersedia.\n\n')
  rmd_content <- paste0(rmd_content, '**Untuk melakukan analisis ANOVA:**\n')
  rmd_content <- paste0(rmd_content, '1. Pilih variabel yang akan dianalisis di dashboard\n')
  rmd_content <- paste0(rmd_content, '2. Klik tombol "Jalankan ANOVA"\n')
  rmd_content <- paste0(rmd_content, '3. Hasil akan muncul di panel ANOVA dan dapat dimasukkan dalam laporan ini\n\n')
}

# BAGIAN III: KESIMPULAN - VERSI YANG DIPERBAIKI DAN SEDERHANA
# BAGIAN III: KESIMPULAN DAN REKOMENDASI
rmd_content <- paste0(rmd_content, '\\newpage\n\n')
rmd_content <- paste0(rmd_content, '# BAGIAN III: KESIMPULAN\n\n')

# Kesimpulan Utama
rmd_content <- paste0(rmd_content, '## Kesimpulan Utama\n\n')

# 1. Kesimpulan Deskriptif
rmd_content <- paste0(rmd_content, '### 1. Analisis Deskriptif\n\n')
rmd_content <- paste0(rmd_content, '```{r kesimpulan-deskriptif, results="asis"}\n')
rmd_content <- paste0(rmd_content, 'total_provinsi <- length(selected_provs)\n')
rmd_content <- paste0(rmd_content, 'total_kabupaten <- ifelse(!is.null(data_filtered), nrow(data_filtered), 0)\n')
rmd_content <- paste0(rmd_content, '\n')
rmd_content <- paste0(rmd_content, 'cat("**Temuan Analisis Deskriptif:**\\n\\n")\n')
rmd_content <- paste0(rmd_content, '\n')
rmd_content <- paste0(rmd_content, 'if(!is.null(analisis_per_col) && length(analisis_per_col) > 0) {\n')
rmd_content <- paste0(rmd_content, '  cat("Analisis deskriptif terhadap", total_provinsi, "provinsi menunjukkan variasi dalam indikator kemiskinan. ")\n')
rmd_content <- paste0(rmd_content, '  cat("Setiap provinsi memiliki karakteristik kemiskinan yang berbeda, terlihat dari distribusi ")\n')
rmd_content <- paste0(rmd_content, '  cat("lima variabel utama: jumlah penduduk miskin (X1), persentase penduduk miskin (P0), ")\n')
rmd_content <- paste0(rmd_content, '  cat("indeks kedalaman kemiskinan (P1), indeks keparahan kemiskinan (P2), dan garis kemiskinan (X2).\\n\\n")\n')
rmd_content <- paste0(rmd_content, '  \n')
rmd_content <- paste0(rmd_content, '  cat("Perbedaan kondisi kemiskinan antar provinsi mencerminkan keragaman sosio-ekonomi wilayah ")\n')
rmd_content <- paste0(rmd_content, '  cat("yang memerlukan pendekatan penanganan kemiskinan yang disesuaikan dengan karakteristik ")\n')
rmd_content <- paste0(rmd_content, '  cat("spesifik masing-masing provinsi.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '} else {\n')
rmd_content <- paste0(rmd_content, '  cat("Data deskriptif tidak tersedia untuk analisis mendalam. ")\n')
rmd_content <- paste0(rmd_content, '  cat("Diperlukan penyediaan data yang lebih lengkap untuk memperoleh insight yang komprehensif.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '}\n')
rmd_content <- paste0(rmd_content, '```\n\n')

# 2. Kesimpulan ANOVA
rmd_content <- paste0(rmd_content, '### 2. Analisis Varians (ANOVA)\n\n')
rmd_content <- paste0(rmd_content, '```{r kesimpulan-anova, results="asis"}\n')
rmd_content <- paste0(rmd_content, 'cat("**Temuan Analisis ANOVA:**\\n\\n")\n')
rmd_content <- paste0(rmd_content, '\n')
rmd_content <- paste0(rmd_content, '# Cek apakah ANOVA dilakukan dengan aman\n')
rmd_content <- paste0(rmd_content, 'anova_dilakukan <- FALSE\n')
rmd_content <- paste0(rmd_content, 'anova_signifikan <- FALSE\n')
rmd_content <- paste0(rmd_content, 'nama_variabel <- "variabel kemiskinan"\n')
rmd_content <- paste0(rmd_content, 'nilai_p <- NA\n')
rmd_content <- paste0(rmd_content, 'nilai_r_squared <- NA\n')
rmd_content <- paste0(rmd_content, '\n')
rmd_content <- paste0(rmd_content, 'tryCatch({\n')
rmd_content <- paste0(rmd_content, '  if(exists("model_data") && !is.null(model_data)) {\n')
rmd_content <- paste0(rmd_content, '    anova_dilakukan <- TRUE\n')
rmd_content <- paste0(rmd_content, '    nama_variabel <- model_data$variable_label\n')
rmd_content <- paste0(rmd_content, '    \n')
rmd_content <- paste0(rmd_content, '    # Ekstrak hasil ANOVA dengan aman\n')
rmd_content <- paste0(rmd_content, '    anova_result <- summary(model_data$model)\n')
rmd_content <- paste0(rmd_content, '    if(!is.null(anova_result) && length(anova_result) > 0) {\n')
rmd_content <- paste0(rmd_content, '      nilai_p <- anova_result[[1]][1, "Pr(>F)"]\n')
rmd_content <- paste0(rmd_content, '      anova_signifikan <- !is.na(nilai_p) && nilai_p < 0.05\n')
rmd_content <- paste0(rmd_content, '      \n')
rmd_content <- paste0(rmd_content, '      # Hitung R-squared\n')
rmd_content <- paste0(rmd_content, '      ss_between <- anova_result[[1]][1, "Sum Sq"]\n')
rmd_content <- paste0(rmd_content, '      ss_within <- anova_result[[1]][2, "Sum Sq"]\n')
rmd_content <- paste0(rmd_content, '      if(!is.na(ss_between) && !is.na(ss_within)) {\n')
rmd_content <- paste0(rmd_content, '        nilai_r_squared <- ss_between / (ss_between + ss_within)\n')
rmd_content <- paste0(rmd_content, '      }\n')
rmd_content <- paste0(rmd_content, '    }\n')
rmd_content <- paste0(rmd_content, '  }\n')
rmd_content <- paste0(rmd_content, '}, error = function(e) {\n')
rmd_content <- paste0(rmd_content, '  # Jika error, tetap gunakan default values\n')
rmd_content <- paste0(rmd_content, '})\n')
rmd_content <- paste0(rmd_content, '\n')
rmd_content <- paste0(rmd_content, 'if(anova_dilakukan) {\n')
rmd_content <- paste0(rmd_content, '  if(anova_signifikan) {\n')
rmd_content <- paste0(rmd_content, '    cat("Hasil uji ANOVA menunjukkan **perbedaan yang signifikan secara statistik** ")\n')
rmd_content <- paste0(rmd_content, '    cat("dalam rata-rata", nama_variabel, "antar provinsi. ")\n')
rmd_content <- paste0(rmd_content, '    cat("Temuan ini mengkonfirmasi bahwa faktor provinsi memiliki pengaruh nyata ")\n')
rmd_content <- paste0(rmd_content, '    cat("terhadap variabel kemiskinan yang dianalisis.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '    \n')
rmd_content <- paste0(rmd_content, '    if(!is.na(nilai_r_squared)) {\n')
rmd_content <- paste0(rmd_content, '      effect_size_pct <- round(nilai_r_squared * 100, 2)\n')
rmd_content <- paste0(rmd_content, '      cat("Faktor provinsi menjelaskan", effect_size_pct, "% dari total variabilitas data. ")\n')
rmd_content <- paste0(rmd_content, '      if(nilai_r_squared < 0.25) {\n')
rmd_content <- paste0(rmd_content, '        cat("Hal ini menunjukkan efek berukuran kecil hingga sedang.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '      } else if(nilai_r_squared < 0.50) {\n')
rmd_content <- paste0(rmd_content, '        cat("Hal ini menunjukkan efek berukuran sedang.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '      } else {\n')
rmd_content <- paste0(rmd_content, '        cat("Hal ini menunjukkan efek berukuran besar.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '      }\n')
rmd_content <- paste0(rmd_content, '    }\n')
rmd_content <- paste0(rmd_content, '    \n')
rmd_content <- paste0(rmd_content, '    cat("Implikasi dari temuan ini adalah perlunya kebijakan pengentasan kemiskinan ")\n')
rmd_content <- paste0(rmd_content, '    cat("yang mempertimbangkan karakteristik spesifik setiap provinsi.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '  } else {\n')
rmd_content <- paste0(rmd_content, '    cat("Hasil uji ANOVA menunjukkan **tidak ada perbedaan yang signifikan secara statistik** ")\n')
rmd_content <- paste0(rmd_content, '    cat("dalam rata-rata", nama_variabel, "antar provinsi. ")\n')
rmd_content <- paste0(rmd_content, '    cat("Temuan ini mengindikasikan bahwa rata-rata variabel kemiskinan relatif homogen ")\n')
rmd_content <- paste0(rmd_content, '    cat("antar provinsi yang dianalisis.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '    \n')
rmd_content <- paste0(rmd_content, '    cat("Implikasi dari temuan ini adalah kemungkinan penerapan pendekatan kebijakan ")\n')
rmd_content <- paste0(rmd_content, '    cat("yang relatif seragam untuk provinsi-provinsi yang dianalisis.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '  }\n')
rmd_content <- paste0(rmd_content, '} else {\n')
rmd_content <- paste0(rmd_content, '  cat("Analisis ANOVA tidak dilakukan dalam laporan ini. ")\n')
rmd_content <- paste0(rmd_content, '  cat("Untuk mendapatkan pemahaman yang lebih mendalam tentang perbedaan antar provinsi, ")\n')
rmd_content <- paste0(rmd_content, '  cat("disarankan untuk melakukan analisis ANOVA pada variabel kemiskinan utama.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '}\n')
rmd_content <- paste0(rmd_content, '```\n\n')

# 3. Kesimpulan Tukey
rmd_content <- paste0(rmd_content, '### 3. Analisis Post-Hoc (Tukey HSD)\n\n')
rmd_content <- paste0(rmd_content, '```{r kesimpulan-tukey, results="asis"}\n')
rmd_content <- paste0(rmd_content, 'cat("**Temuan Uji Post-Hoc Tukey HSD:**\\n\\n")\n')
rmd_content <- paste0(rmd_content, '\n')
rmd_content <- paste0(rmd_content, 'tukey_dilakukan <- FALSE\n')
rmd_content <- paste0(rmd_content, 'tukey_signifikan_pairs <- 0\n')
rmd_content <- paste0(rmd_content, 'tukey_total_pairs <- 0\n')
rmd_content <- paste0(rmd_content, '\n')
rmd_content <- paste0(rmd_content, 'tryCatch({\n')
rmd_content <- paste0(rmd_content, '  if(anova_dilakukan && anova_signifikan && !is.null(model_data)) {\n')
rmd_content <- paste0(rmd_content, '    tukey_result <- TukeyHSD(model_data$model, conf.level = 0.95)\n')
rmd_content <- paste0(rmd_content, '    if(!is.null(tukey_result) && !is.null(tukey_result$Provinsi)) {\n')
rmd_content <- paste0(rmd_content, '      tukey_dilakukan <- TRUE\n')
rmd_content <- paste0(rmd_content, '      tukey_data <- as.data.frame(tukey_result$Provinsi)\n')
rmd_content <- paste0(rmd_content, '      tukey_signifikan_pairs <- sum(tukey_data[["p adj"]] < 0.05, na.rm = TRUE)\n')
rmd_content <- paste0(rmd_content, '      tukey_total_pairs <- nrow(tukey_data)\n')
rmd_content <- paste0(rmd_content, '    }\n')
rmd_content <- paste0(rmd_content, '  }\n')
rmd_content <- paste0(rmd_content, '}, error = function(e) {\n')
rmd_content <- paste0(rmd_content, '  # Jika error, tetap gunakan default values\n')
rmd_content <- paste0(rmd_content, '})\n')
rmd_content <- paste0(rmd_content, '\n')
rmd_content <- paste0(rmd_content, 'if(tukey_dilakukan && tukey_total_pairs > 0) {\n')
rmd_content <- paste0(rmd_content, '  signif_percentage <- round((tukey_signifikan_pairs / tukey_total_pairs) * 100, 1)\n')
rmd_content <- paste0(rmd_content, '  \n')
rmd_content <- paste0(rmd_content, '  cat("Uji post-hoc Tukey HSD mengidentifikasi", tukey_signifikan_pairs, "dari", \n')
rmd_content <- paste0(rmd_content, '      tukey_total_pairs, "pasangan provinsi (", signif_percentage, "%) ")\n')
rmd_content <- paste0(rmd_content, '  cat("yang memiliki perbedaan rata-rata yang signifikan secara statistik.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '  \n')
rmd_content <- paste0(rmd_content, '  if(tukey_signifikan_pairs > 0) {\n')
rmd_content <- paste0(rmd_content, '    if(signif_percentage < 30) {\n')
rmd_content <- paste0(rmd_content, '      cat("Proporsi pasangan yang signifikan relatif rendah, menunjukkan bahwa ")\n')
rmd_content <- paste0(rmd_content, '      cat("meskipun terdapat perbedaan antar provinsi secara keseluruhan, ")\n')
rmd_content <- paste0(rmd_content, '      cat("sebagian besar provinsi memiliki tingkat kemiskinan yang tidak berbeda drastis. ")\n')
rmd_content <- paste0(rmd_content, '      cat("Hal ini mengindikasikan adanya kelompok provinsi dengan karakteristik serupa.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '    } else if(signif_percentage < 60) {\n')
rmd_content <- paste0(rmd_content, '      cat("Proporsi pasangan yang signifikan berada pada tingkat sedang, menunjukkan ")\n')
rmd_content <- paste0(rmd_content, '      cat("adanya pola pengelompokan provinsi berdasarkan tingkat kemiskinan yang berbeda.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '    } else {\n')
rmd_content <- paste0(rmd_content, '      cat("Proporsi pasangan yang signifikan relatif tinggi, menunjukkan ")\n')
rmd_content <- paste0(rmd_content, '      cat("heterogenitas yang tinggi dalam kondisi kemiskinan antar provinsi.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '    }\n')
rmd_content <- paste0(rmd_content, '    \n')
rmd_content <- paste0(rmd_content, '    cat("Temuan ini memberikan insight spesifik tentang provinsi mana yang memiliki ")\n')
rmd_content <- paste0(rmd_content, '    cat("perbedaan signifikan, memungkinkan identifikasi prioritas dalam program ")\n')
rmd_content <- paste0(rmd_content, '    cat("pengentasan kemiskinan.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '  } else {\n')
rmd_content <- paste0(rmd_content, '    cat("Meski ANOVA menunjukkan perbedaan signifikan secara keseluruhan, ")\n')
rmd_content <- paste0(rmd_content, '    cat("tidak ada pasangan provinsi individu yang berbeda signifikan dalam uji post-hoc. ")\n')
rmd_content <- paste0(rmd_content, '    cat("Hal ini dapat terjadi ketika perbedaan terdistribusi merata.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '  }\n')
rmd_content <- paste0(rmd_content, '} else if(anova_dilakukan && anova_signifikan) {\n')
rmd_content <- paste0(rmd_content, '  cat("Uji post-hoc Tukey HSD tidak dapat dilakukan atau tidak menghasilkan hasil yang valid. ")\n')
rmd_content <- paste0(rmd_content, '  cat("Hal ini mungkin disebabkan oleh keterbatasan data atau struktur data yang tidak memadai.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '} else {\n')
rmd_content <- paste0(rmd_content, '  cat("Uji post-hoc Tukey HSD tidak dilakukan karena hasil ANOVA tidak menunjukkan ")\n')
rmd_content <- paste0(rmd_content, '  cat("perbedaan yang signifikan antar provinsi. Uji post-hoc hanya relevan dilakukan ")\n')
rmd_content <- paste0(rmd_content, '  cat("ketika ANOVA menunjukkan adanya perbedaan signifikan.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '}\n')
rmd_content <- paste0(rmd_content, '```\n\n')

# Kesimpulan Integratif Sederhana
rmd_content <- paste0(rmd_content, '## Kesimpulan Integratif\n\n')
rmd_content <- paste0(rmd_content, '```{r kesimpulan-integratif, results="asis"}\n')
rmd_content <- paste0(rmd_content, 'cat("**Sintesis Temuan dan Implikasi Kebijakan:**\\n\\n")\n')
rmd_content <- paste0(rmd_content, '\n')
rmd_content <- paste0(rmd_content, 'if(anova_dilakukan) {\n')
rmd_content <- paste0(rmd_content, '  if(anova_signifikan) {\n')
rmd_content <- paste0(rmd_content, '    if(tukey_dilakukan && tukey_signifikan_pairs > 0) {\n')
rmd_content <- paste0(rmd_content, '      cat("Analisis komprehensif menunjukkan **pola kemiskinan yang heterogen** antar provinsi. ")\n')
rmd_content <- paste0(rmd_content, '      cat("Terdapat perbedaan fundamental yang memerlukan pendekatan kebijakan yang diferensiasi ")\n')
rmd_content <- paste0(rmd_content, '      cat("berdasarkan karakteristik spesifik setiap provinsi atau kelompok provinsi.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '    } else {\n')
rmd_content <- paste0(rmd_content, '      cat("Meskipun terdapat perbedaan signifikan secara keseluruhan, perbedaan tersebut ")\n')
rmd_content <- paste0(rmd_content, '      cat("terdistribusi secara merata. Hal ini menunjukkan perlunya pendekatan yang adaptif ")\n')
rmd_content <- paste0(rmd_content, '      cat("dengan mempertimbangkan nuansa lokal masing-masing provinsi.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '    }\n')
rmd_content <- paste0(rmd_content, '  } else {\n')
rmd_content <- paste0(rmd_content, '    cat("Kondisi kemiskinan yang relatif homogen antar provinsi memungkinkan penerapan ")\n')
rmd_content <- paste0(rmd_content, '    cat("strategi kebijakan yang relatif seragam, namun tetap perlu mempertimbangkan ")\n')
rmd_content <- paste0(rmd_content, '    cat("konteks dan kebutuhan spesifik setiap wilayah.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '  }\n')
rmd_content <- paste0(rmd_content, '} else {\n')
rmd_content <- paste0(rmd_content, '  cat("Berdasarkan analisis deskriptif, terlihat adanya variasi dalam indikator kemiskinan ")\n')
rmd_content <- paste0(rmd_content, '  cat("antar provinsi. Untuk mendapatkan pemahaman yang lebih komprehensif, disarankan ")\n')
rmd_content <- paste0(rmd_content, '  cat("melakukan analisis inferensial lanjutan.\\n\\n")\n')
rmd_content <- paste0(rmd_content, '}\n')
rmd_content <- paste0(rmd_content, '\n')
rmd_content <- paste0(rmd_content, 'cat("**Rekomendasi Utama:**\\n\\n")\n')
rmd_content <- paste0(rmd_content, 'cat("1. Mengembangkan strategi pengentasan kemiskinan yang disesuaikan dengan ")\n')
rmd_content <- paste0(rmd_content, 'cat("karakteristik dan tingkat kemiskinan masing-masing provinsi\\n\\n")\n')
rmd_content <- paste0(rmd_content, 'cat("2. Mengoptimalkan alokasi sumber daya berdasarkan prioritas dan kebutuhan spesifik wilayah\\n\\n")\n')
rmd_content <- paste0(rmd_content, 'cat("3. Mengembangkan sistem monitoring dan evaluasi untuk memantau efektivitas ")\n')
rmd_content <- paste0(rmd_content, 'cat("program pengentasan kemiskinan secara berkelanjutan\\n\\n")\n')
rmd_content <- paste0(rmd_content, '```\n\n')

rmd_content <- paste0(rmd_content, '---\n\n')

# Footer
rmd_content <- paste0(rmd_content, '---\n\n')
rmd_content <- paste0(rmd_content, '*Laporan Lengkap dibuat pada ', format(Sys.time(), "%d %B %Y pukul %H:%M WIB"), '*\n\n')
rmd_content <- paste0(rmd_content, '*Dashboard Tingkat Kemiskinan Indonesia*')

cat("RMD content for lengkap built successfully with clean and simple conclusion!\n")
return(rmd_content)

  }, error = function(e) {
    cat("ERROR in create_lengkap_rmd:", e$message, "\n")
    return(paste0('---
title: "Laporan Lengkap Analisis Data Kemiskinan"
author: "Dashboard Tingkat Kemiskinan Indonesia"
date: "', format(Sys.Date(), "%d %B %Y"), '"
output: pdf_document
---

# Error dalam Pembuatan Laporan

Terjadi error dalam pembuatan laporan lengkap: ', e$message, '

Silakan coba lagi atau hubungi administrator.

---

*Laporan dibuat pada ', format(Sys.time(), "%d %B %Y"), '*
'))
  })
}

  # Download handler dengan debugging yang lebih baik
  output$download_deskriptif_pdf <- downloadHandler(
    filename = function() {
      paste0("Laporan_Deskriptif_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Create temporary files
      temp_dir <- tempdir()
      temp_report <- file.path(temp_dir, paste0("report_", Sys.time() %>% as.numeric(), ".Rmd"))
      
      tryCatch({
        # Get RMD content
        rmd_content <- create_deskriptif_rmd()
        
        # Write content dengan encoding yang aman
        writeLines(rmd_content, temp_report, useBytes = FALSE)
        
        # Debug: Cek apakah file .Rmd berhasil dibuat
        if(!file.exists(temp_report)) {
          stop("File .Rmd tidak berhasil dibuat")
        }
        
        cat("File .Rmd berhasil dibuat di:", temp_report, "\n")
        
        # Prepare environment
        env <- new.env()
        env$data_filtered <- filtered_data()
        env$analisis_per_col <- analisis_deskriptif_per_prov_col()
        env$selected_provs <- input$selected_provinsi
        env$prov_map <- provinsi_mapping()
        
        # Show progress
        withProgress(message = 'Membuat laporan PDF...', value = 0, {
          incProgress(0.3, detail = "Memproses data...")
          
          # Try rendering with different engines
          success <- FALSE
          error_msg <- ""
          
          # Method 1: Try with pdflatex (lebih stabil)
          if(!success) {
            incProgress(0.5, detail = "Mencoba dengan pdflatex...")
            tryCatch({
              rmarkdown::render(
                input = temp_report,
                output_format = rmarkdown::pdf_document(
                  latex_engine = "pdflatex",
                  keep_tex = FALSE
                ),
                output_file = file,
                envir = env,
                quiet = TRUE  # Set ke TRUE untuk mengurangi output
              )
              success <- TRUE
              incProgress(1, detail = "Selesai dengan pdflatex!")
            }, error = function(e) {
              error_msg <- paste("pdflatex error:", e$message)
              cat("Error pdflatex:", e$message, "\n")
            })
          }
          
          # Method 2: Try with xelatex if pdflatex fails
          if(!success) {
            incProgress(0.7, detail = "Mencoba dengan xelatex...")
            tryCatch({
              rmarkdown::render(
                input = temp_report,
                output_format = rmarkdown::pdf_document(
                  latex_engine = "xelatex",
                  keep_tex = FALSE
                ),
                output_file = file,
                envir = env,
                quiet = TRUE
              )
              success <- TRUE
              incProgress(1, detail = "Selesai dengan xelatex!")
            }, error = function(e) {
              error_msg <- paste(error_msg, "; xelatex error:", e$message)
              cat("Error xelatex:", e$message, "\n")
            })
          }
          
          # Method 3: HTML fallback
          if(!success) {
            incProgress(0.9, detail = "Membuat HTML sebagai alternatif...")
            temp_html <- sub("\\.pdf$", ".html", file)
            
            tryCatch({
              rmarkdown::render(
                input = temp_report,
                output_format = rmarkdown::html_document(
                  toc = FALSE,
                  theme = "flatly"
                ),
                output_file = temp_html,
                envir = env,
                quiet = TRUE
              )
              
              if (file.exists(temp_html)) {
                file.rename(temp_html, file)
                showNotification(
                  "PDF gagal dibuat. File disimpan sebagai HTML. Silakan install tinytex untuk PDF: tinytex::install_tinytex()", 
                  type = "warning", 
                  duration = 15
                )
                success <- TRUE
              }
              incProgress(1, detail = "Selesai (HTML)")
            }, error = function(e) {
              error_msg <- paste(error_msg, "; HTML error:", e$message)
              cat("Error HTML:", e$message, "\n")
            })
          }
          
          if(!success) {
            showNotification(paste("Semua metode gagal:", error_msg), type = "error", duration = 10)
            
            # Buat file teks sebagai fallback terakhir
            writeLines(c("LAPORAN GAGAL DIBUAT", "", "Error:", error_msg, "", 
                         "Data yang tersedia:", 
                         paste("- Provinsi:", length(input$selected_provinsi)),
                         paste("- Data rows:", nrow(filtered_data()))), file)
          }
        })
        
      }, error = function(e) {
        showNotification(paste("Error utama:", e$message), type = "error")
        writeLines(c("ERROR:", e$message), file)
      }, finally = {
        # Cleanup
        if(file.exists(temp_report)) {
          unlink(temp_report, force = TRUE)
        }
      })
    }
  )
  
  # Download handler untuk Laporan ANOVA  
  output$download_anova_pdf <- downloadHandler(
    filename = function() {
      paste0("Laporan_ANOVA_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Similar implementation as above with ANOVA-specific content
      temp_dir <- tempdir()
      temp_report <- file.path(temp_dir, paste0("anova_", Sys.time() %>% as.numeric(), ".Rmd"))
      
      rmd_content <- create_anova_rmd()
      writeLines(rmd_content, temp_report, useBytes = TRUE)
      
      env <- new.env()
      model_data <- tryCatch(anova_model(), error = function(e) NULL)
      env$model_data <- model_data
      env$prov_map <- provinsi_mapping()
      
      withProgress(message = 'Membuat laporan ANOVA PDF...', value = 0, {
        incProgress(0.3, detail = "Memproses analisis...")
        
        success <- FALSE
        
        # Try PDF generation
        tryCatch({
          rmarkdown::render(
            input = temp_report,
            output_format = rmarkdown::pdf_document(
              latex_engine = "xelatex",
              toc = FALSE,
              toc_depth = 3,
              number_sections = TRUE
            ),
            output_file = file,
            envir = env,
            quiet = FALSE
          )
          success <- TRUE
          incProgress(1, detail = "Selesai!")
        }, error = function(e) {
          print(paste("PDF error:", e$message))
        })
        
        # HTML fallback
        if (!success) {
          incProgress(0.8, detail = "Membuat HTML...")
          temp_html <- sub("\\.pdf$", ".html", file)
          
          tryCatch({
            rmarkdown::render(
              input = temp_report,
              output_format = rmarkdown::html_document(
                toc = FALSE,
                toc_float = TRUE,
                theme = "flatly"
              ),
              output_file = temp_html,
              envir = env,
              quiet = FALSE
            )
            
            if (file.exists(temp_html)) {
              file.rename(temp_html, file)
              showNotification("File disimpan sebagai HTML.", type = "warning", duration = 10)
            }
            incProgress(1, detail = "Selesai (HTML)")
          }, error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
          })
        }
      })
      
      unlink(temp_report, force = TRUE)
    }
  )
  
  # Download handler untuk Laporan Lengkap
  output$download_lengkap_pdf <- downloadHandler(
    filename = function() {
      paste0("Laporan_Lengkap_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Similar implementation combining both reports
      temp_dir <- tempdir()
      temp_report <- file.path(temp_dir, paste0("lengkap_", Sys.time() %>% as.numeric(), ".Rmd"))
      
      rmd_content <- create_lengkap_rmd()
      writeLines(rmd_content, temp_report, useBytes = TRUE)
      
      env <- new.env()
      env$data_filtered <- filtered_data()
      env$analisis_per_col <- analisis_deskriptif_per_prov_col()
      env$selected_provs <- input$selected_provinsi
      env$prov_map <- provinsi_mapping()
      model_data <- tryCatch(anova_model(), error = function(e) NULL)
      env$model_data <- model_data
      
      withProgress(message = 'Membuat laporan lengkap PDF...', value = 0, {
        incProgress(0.2, detail = "Menyiapkan data...")
        
        success <- FALSE
        
        # Try PDF generation
        tryCatch({
          rmarkdown::render(
            input = temp_report,
            output_format = rmarkdown::pdf_document(
              latex_engine = "xelatex",
              toc = TRUE,
              toc_depth = 3,
              number_sections = TRUE
            ),
            output_file = file,
            envir = env,
            quiet = FALSE
          )
          success <- TRUE
          incProgress(1, detail = "Selesai!")
        }, error = function(e) {
          print(paste("PDF error:", e$message))
        })
        
        # HTML fallback
        if (!success) {
          incProgress(0.8, detail = "Membuat HTML...")
          temp_html <- sub("\\.pdf$", ".html", file)
          
          tryCatch({
            rmarkdown::render(
              input = temp_report,
              output_format = rmarkdown::html_document(
                toc = TRUE,
                toc_float = TRUE,
                theme = "flatly",
                code_folding = "hide"
              ),
              output_file = temp_html,
              envir = env,
              quiet = FALSE
            )
            
            if (file.exists(temp_html)) {
              file.rename(temp_html, file)
              showNotification("File disimpan sebagai HTML.", type = "warning", duration = 10)
            }
            incProgress(1, detail = "Selesai (HTML)")
          }, error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
          })
        }
      })
      
      unlink(temp_report, force = TRUE)
    }
  )
  
}  
# <<< AKHIR DARI FUNGSI SERVER UTAMA