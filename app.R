
# Import the R package
library(readxl)
library(ggplot2)
library(dplyr)
library(nnet)
library(shiny)
library(shinydashboard)


# Load the data
data2KS5 <- read_xlsx("data.xlsx")
df <- lapply(data2KS5[, -1], as.factor)
df <- data.frame(data2KS5[, 1], df)
df$Usia <- factor(df$Usia, levels = c("Kurang dari 18 years old", "18 - 24", "25 - 34", "35 - 44", "45 - 54", "Lebih dari 55 years old"))
df$Tingkat.Pendidikan <- factor(df$Tingkat.Pendidikan, levels = c("SMA ke bawah", "D1-D3", "D4/S1", "S2"))
df$Pengeluaran.Per.Bulan <- factor(df$Pengeluaran.Per.Bulan, levels = c("Kurang dari Rp. 500.000", "Rp. 500.000 - Rp. 999.000", "Rp. 1.000.000 - Rp. 1.999.000", "Rp. 2.000.000 - Rp. 2.999.000", "Rp. 3.000.000 - Rp. 3.999.000", "Rp. 4.000.000 - Rp. 4.999.000", "Lebih dari Rp. 5.000.000"))
for (x in 8:24) {
  levels(df[, x]) <- c("1", "2", "3", "4", "5", "6", "7")
}
# Model
model <- multinom(Tingkat.Kepuasan ~ Jenis.Kelamin + Usia + Pengeluaran.Per.Bulan + Tingkat.Pendidikan + Pekerjaan + Frekuensi.Kunjungan + Kenyamanan.kedai + Perlengkapan.kedai + Kapasitas.kedai + Pelayanan + Penampilan.karyawan + Ketepatan.janji.layanan + Simpati.ke.pelanggan + Kecepatan.layanan.karyawan + Karyawan.dapat.dipercaya + Transaksi.aman + Kesopanan.karyawan + Karyawan.tahu.kebutuhan.pelanggan + Suasana.kedai.nyaman + Paket.harga.terbaik + Harga.produk.masuk.akal + Kualitas.makanan, data = df)




ui <-
  dashboardPage(
    dashboardHeader(title = "Shoffee Dashboard", dropdownMenu(
      type = "messages",
      messageItem(
        from = "Dibuat oleh",
        message = "I Putu Agus Wahyu Dupayana."
      ),
      messageItem(
        from = "Dukungan",
        message = "Coffee Dashboard.",
        icon = icon("life-ring"),
        time = "2022"
      )
    )),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("Tabel", tabName = "tabel", icon = icon("table")),
        menuItem("Grafik", tabName = "grafik", icon = icon("chart-pie"), badgeLabel = "baru", badgeColor = "green"),
        menuItem("Regresi", tabName = "regresi", icon = icon("chart-line"), badgeLabel = "baru", badgeColor = "green"),
        menuItem("Materi", tabName = "materi", icon = icon("book"))
      )
    ),
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(
          tabName = "dashboard",
          h1("Dashboard Kedai Kopi", style = "margin-bottom: 20pt;"),
          fluidRow(
            infoBoxOutput("lakilaki"),
            infoBoxOutput("perempuan"),
            infoBoxOutput("total"),
          ),
          fluidRow(
            box(title = "Diagram Lingkaran Berdasarkan Jenis Kelamin", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot2")),
            box(title = "Diagram Batang Berdasarkan Jenis Kelamin", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot3")),
          ),
          fluidRow(
            box(title = "Diagram Lingkaran Berdasarkan Usia", status = "success", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot4")),
            box(title = "Diagram Batang Berdasarkan Usia", status = "success", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot5")),
          ),
          fluidRow(
            box(title = "Diagram Lingkaran Berdasarkan Pengeluaran Per Bulan", status = "info", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot6")),
            box(title = "Diagram Batang Berdasarkan Pengeluaran Per Bulan", status = "info", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot7")),
          ),
          fluidRow(
            box(title = "Diagram Lingkaran Berdasarkan Tingkat Pendidikan", status = "warning", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot8")),
            box(title = "Diagram Batang Berdasarkan Tingkat Pendidikan", status = "warning", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot9")),
          ),
          fluidRow(
            box(title = "Diagram Lingkaran Berdasarkan Pekerjaan", status = "danger", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot10")),
            box(title = "Diagram Batang Berdasarkan Pekerjaan", status = "danger", solidHeader = TRUE, collapsible = TRUE, plotOutput("plot11")),
          ),
        ),

        # Second tab content
        tabItem(
          tabName = "data",
          fluidRow(
            box(
              style = "width:12;overflow-x: scroll;height:540px;overflow-y: scroll;",
              title = "Raw Data", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
              h1("Data Kedai Kopi", align = "center"),
              div("", style = "margin:50px;"),
              column(4, numericInput("size_head", "Tentukan jumlah baris yang ingin ditampilkan : ", value = 5, min = 1, max = 100, step = 1), ),
              column(4, selectInput("orderby_head", "Urutkan data berdasarkan : ", choices = gsub("[.]", " ", names(df)[c(-1)]))),
              column(4, selectInput("order_head", "Urutkan data : ", choices = list("Menaik" = "asc", "Menurun" = "desc"))),
              tableOutput("table")
            ),
            box(
              style = "width:12;overflow-x: scroll;height:540px;overflow-y: scroll;",
              title = "Struktur Data", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
              verbatimTextOutput("strdata")
            ),
            box(
              style = "width:12;overflow-x: scroll;height:540px;overflow-y: scroll;",
              title = "Ringkasan Data", status = "info", solidHeader = TRUE, collapsible = TRUE, width = 12,
              verbatimTextOutput("summarydata")
            ),
          ),
        ),

        # Third tab content
        tabItem(
          tabName = "tabel",
          fluidRow(
            box(
              title = "Tabel Kontingensi", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
              column(6, selectInput("baris", "Pilih Baris:", choices = names(df)[c(2, 3, 4, 5, 6)])),
              column(6, selectInput("kolom", "Pilih Kolom:", choices = names(df)[c(2, 3, 4, 5, 6)])),
              verbatimTextOutput("tabelkontingensi"),
            ),
            box(
              title = "Hasil Uji Chi Square", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
              verbatimTextOutput("chisq"),
            ),
          ),
        ),

        # Forth tab content
        tabItem(
          tabName = "grafik",
          fluidRow(
            box(
              style = "width:12;overflow-x: scroll;height:520px;overflow-y: scroll;",
              title = "Visualisasi", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
              column(
                3, selectInput("visualisasi", label = "Pilih Visualisasi : ", choices = list("Diagram Lingkaran" = "piechart", "Diagram Batang" = "histogram")),
                selectInput("var_terpilih",
                  label = "Pilih Variabel : ",
                  choices = gsub("[.]", " ", names(df)[c(-1)]),
                ),
                tags$b("Ringkasan Data : "),
                verbatimTextOutput("summarydatapilih")
              ),
              column(9, plotOutput("plot1", brush = "plot_brush"))
            ),
            box(
              title = "Interpretasi", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
              textOutput("interpretasi")
            )
          ),
        ),

        # Fifth tab content
        tabItem(
          tabName = "regresi",
          fluidRow(box(
            style = "width:12;overflow-x: scroll;height:200px;overflow-y: scroll;",
            title = "Proses Analisis Regresi", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
            column(12, verbatimTextOutput("model")),
          )),
          fluidRow(box(
            title = "Hasil Uji Kesesuaian Model", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 12,
            column(
              6,
              h3("AIC (Akaike's Information Criterion)"),
              verbatimTextOutput("aic"),
              textOutput("interpretasiaic"),
            ),
            column(
              6, h3("BIC (Bayesian Information Criterion)"),
              verbatimTextOutput("bic"),
              textOutput("interpretasibic"),
            )
          )),
          fluidRow(
            box(
              title = "Hasil Prediksi", status = "info", solidHeader = TRUE, collapsible = TRUE, width = 12,
              column(
                12,
                h3("Pilih Variabel Regresi", align = "center"),
              ),
              column(6, selectInput("pil2",
                label = "Jenis Kelamin: ",
                choices = levels(df[, 2]),
              )),
              column(6, selectInput("pil3",
                label = "Usia: ",
                choices = levels(df[, 3]),
              )),
              column(6, selectInput("pil4",
                label = "Pengeluaran Per Bulan: ",
                choices = levels(df[, 4]),
              )),
              column(6, selectInput("pil5",
                label = "Tingkat Pendidikan: ",
                choices = levels(df[, 5]),
              )),
              column(6, selectInput("pil6",
                label = "Pekerjaan: ",
                choices = levels(df[, 6]),
              )),
              column(6, selectInput("pil7",
                label = "Frekuensi Kunjungan: ",
                choices = levels(df[, 7]),
              )),
              column(6, sliderInput("pil8", "Kenyamanan kedai:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil9", "Perlengkapan kedai:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil10", "Kapasitas kedai:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil11", "Pelayanan:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil12", "Penampilan karyawan:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil13", "Ketepatan janji layanan:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil14", "Simpati ke pelanggan:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil15", "Kecepatan layanan karyawan:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil16", "Karyawan dapat dipercaya:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil17", "Transaksi aman:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil18", "Kesopanan karyawan:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil19", "Karyawan tahu kebutuhan pelanggan:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil20", "Suasana kedai nyaman:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil21", "Paket harga terbaik:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil22", "Harga produk masuk akal:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(6, sliderInput("pil23", "Kualitas makanan:",
                min = 1, max = 7,
                value = 4, step = 1
              )),
              column(
                12,
                h3("Hasil Regresi", align = "center"),
                textOutput("regresi"),
              )
            )
          ),
        ),

        # Six tab content
        tabItem(
          tabName = "materi",
          fluidRow(
            box(
              title = "Analisi Regresi Logistik Multinomial", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
              column(
                12,
                h2("Regresi Logistik", align = "center"),
                p("Regresi logistik merupakan salah satu metode yang dapat digunakan untuk mencari hubungan variabel respon yang bersifat berskala nominal atau ordinal dengan dua kategori atau mempunyai skala nominal atau ordinal dengan lebih dari dua kategori dengan satu atau lebih variabel prediktor dan variabel respon yang bersifat kontinyu atau kategorik. Regresi logistik juga merupakan hubungan antara regresi logit dan regresi probit. Yang termasuk dalam regresi ini adalah regresi biner (dengan respon Y hanya dua kategori) (Tirta, 2009). Model dari regresi logistik ini, adalah:"),
                withMathJax(),
                helpText("$$logit(\\pi_i)=\\ln\\left(\\frac{\\pi_i}{1-\\pi_i}\\right)=\\alpha+\\beta_ix_{1,i}+...+\\beta_kx_{k,i}, i=1, ..., n$$"),
                h2("Regresi Logistik Multinomial", align = "center"),
                p("Menurut Yudisasanta A. dan Ratna M. (2012) regresi logistik multinomial merupakan regresi logistik yang digunakan saat variabel dependen mempunyai skala yang bersifat polichotomous atau multinomial. Skala multinomial adalah suatu pengukuran yang dikategorikan menjadi lebih dari dua kategori. Metode yang digunakan dalam penelitian ini adalah regresi logistik dengan variabel dependen berskala nominal dengan tiga kategori."),
                p("Mengacu pada regresi logistik trichotomous untuk model regresi dengan variabel dependen berskala nominal tiga kategori digunakan kategori variabel hasil Y dikodekan 0, 1, dan 2. Variabel Y diparameterkan menjadi dua fungsi logit. Sebelumnya perlu ditentukan kategori hasil mana yang digunakan untuk membandingkan. Pada umumnya digunakan Y = 0 sebagai pembanding. Untuk membentuk fungsi logit, akan dibandingan Y = 1 dan Y = 2, terhadap Y = 0. Bentuk model regresi logistik dengan p variabel prediktor seperti berikut"),
                helpText("$$\\pi(x)=\\frac{e^{(\\beta_0+\\beta_1x_1+...+\\beta_px_p)}}{1+e^{(\\beta_0+\\beta_1x_1+...+\\beta_px_p)}}$$"),
                helpText("dengan menggunakan transformasi logit akan didapatkan dua fungsi logit $$ g_1(x)=\\ln\\left(\\frac{P(Y=1|x)}{P(Y=0|x)}\\right) =\\beta_{10}+\\beta_{11}x_1+...+\\beta_{1p} x_p $$"),
                helpText("dan $$ g_2(x)=\\ln\\left(\\frac{P(Y=2|x)}{P(Y=0|x)}\\right) =\\beta_{20}+\\beta_{22}x_1+...+\\beta_{2p}x_p$$ "),
                helpText("Berdasarkan kedua fungsi logit tersebut maka didapatkan model regresi logistik trichotomous sebagai berikut : $$P(Y=0|x)=\\frac{1}{1+e^{g_1(x)}+e^{g_2(x)}}$$"),
                helpText("dan $$P(Y=2|x)=\\frac{e^{g_2(x)}}{1+e^{g_1(x)}+e^{g_2(x)}}$$"),
                helpText("Mengikuti aturan dari model logistik biner, maka akan dimisalkan \\(P(Y=j|x)=\\pi_j(x)\\) untuk j=0, 1, 2 untuk setiap fungsi dari vektor \\(2(p+1)\\) dengan parameter \\(\\beta^T=(\\beta^T_1\\beta^T_2)\\). Pernyataan umum untuk probabilitas bersyarat dalam model tiga kategori adalah: $$P(Y=j|x)=\\frac{e^{g_j(x)}}{{\\Sigma^2_{k=0}}e^{g_k(x)}} ,j=0,1,2 \\label{eq2}$$"),
                helpText("dengan vektor \\({\\beta_0}=0\\) sehingga \\(g_0 (x)=0\\) "),
                helpText("Model logistik untuk kategori variabel dependen lebih dari satu atau polichotomous, yaitu: $$\\ln\\left[\\frac{\\pi_j}{\\pi_q}\\right]={\\beta_0}^{(j)}+\\Sigma^k_{i=1}{\\beta_i}^{(j)}{x_i}$$"),
                p("pada persamaan (1) dapat dilihat bahwa salah satu dari kategori digunakan sebagai referensi dan disebut sebagai basis(baseline), yaitu kategori yang menjadi dasar pembanding pengaruh kategori lainnya."),
                h2("Estimasi Parameter", align = "center"),
                helpText("Dalam model regresi logistik, nilai harapan antar variabel respon tidak linier serta memiliki varian-varian yang tidak sama sehingga penduga parameter \\(\\beta\\) diperoleh melalui metode Maximum Likelihood (Hosmer dan Lemeshow, 2000). Untuk memecahkan masalah sistem persamaan nonlinier, solusi yang dilakukan adalah dengan mengestimasi \\(\\beta\\) melalui proses iterasi Newton Raphson. Karena variabel respon \\(y_j\\) diasumsikan saling bebas, maka diperoleh fungsi likelihood bersyarat untuk sampel sebanyak n observasi sebagai berikut: $$\\textit{l}(\\beta)=\\prod^n_{i=1}[\\pi_0{(x_i)^{y_{0i}}}\\pi_1(x_i)^{y_{1i}}\\pi_2(x_i)^{y_{2i}}]$$"),
                helpText("Secara matematis, akan lebih mudah untuk mendapatkan nilai yang akan memaksimalkan fungsi likelihood di atas melalui log dari fungsi tersebut yaitu log likelihood. Dengan demikian maka fungsi log likelihood-nya adalah: $$L(\\beta)=\\Sigma^n_{i=1}y_ig_1(x_i)+y_{2i}g_2(x_i)-\\ln(1+e^{g_1(x_i)}+e^{g_2(x_i)})$$"),
                helpText("Untuk mendapatkan nilai \\(\\beta\\) yang memaksimumkan \\(L(\\beta)\\) maka dilakukan diferensiasi terhadap \\(L(\\beta)\\), dengan syarat $$ \\frac{\\partial L}{\\partial\\beta}=0,\\frac{\\partial^2L}{\\partial^2\\beta}<0$$"),
                h2("Uji Kesesuaian Model(Goodness of Fit)", align = "center"),
                p("Untuk mengetahui apakah model dengan variabel dependen tersebut merupakan model yang sesuai, maka perlu dilakukan suatu uji kesesuaian model."),
                h3("AIC(Akaike's Information Criterion)"),
                helpText("Pemilihan model dari sebuah data set yang terbaik salah satunya adalah dengan menggunakan metode AIC (Akaike's Information Criterion). Menurut Tirta(2009) besarnya AIC dihitung melalui rumus berikut : $$AIC=-2l(\\hat{\\theta})+2q$$"),
                helpText("dengan \\(l(\\hat{\\theta})\\) adalah nilai likelihood dari model yang dihadapi dan q adalah banyaknya parameter dalam model. Pemilihan model terbaik dilihat dari nilai terkecil dari AIC. "),
                h3("BIC(Bayesian Information Criterion)"),
                helpText("adalah nilai likelihood dari model yang dihadapi dan q adalah banyaknya parameter dalam model. Pemilihan model terbaik dilihat dari nilai terkecil dari AIC. $$BIC=-2l(\\hat{\\theta})+q \\ln{N}$$"),
                helpText("dengan \\(-2l(\\hat{\\theta})\\) adalah model log likelihood, adalah q jumlah parameter pada model, dan N adalah jumlah objek pemgamatan. Model dengan nilai BIC lebih kecil dipilih sebagai model terbaik untuk data.  ")
              )
            ),
          ),
        )
      )
    )
  )

server <- function(input, output, session) {
  # First tab output
  output$lakilaki <- renderValueBox({
    infoBox(h4("Laki-laki"), summary(df$Jenis.Kelamin)[1], icon = icon("male"), fill = TRUE)
  })
  output$perempuan <- renderValueBox({
    infoBox(h4("Perempuan"), summary(df$Jenis.Kelamin)[2], icon = icon("female"), color = "yellow", fill = TRUE)
  })
  output$total <- renderValueBox({
    infoBox(h4("Total"), nrow(df), icon = icon("user-friends"), color = "green", fill = TRUE)
  })
  output$plot2 <- renderPlot({
    ggplot(df %>%
      group_by(Jenis.Kelamin) %>%
      summarise(Percent = n() / nrow(.) * 100), aes(x = "", y = Percent, fill = Jenis.Kelamin)) +
      geom_col() +
      geom_text(aes(label = paste0(round(Percent, 2), "%")), position = position_stack(vjust = 0.5), colour = "white") +
      coord_polar(theta = "y") +
      labs(fill = "Jenis Kelamin") +
      xlab("") +
      ylab("Persentase") +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
      )
  })
  output$plot3 <- renderPlot({
    ggplot(df, aes(x = Jenis.Kelamin, fill = Jenis.Kelamin)) +
      geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
      xlab("Jenis Kelamin") +
      ylab("Total") +
      labs(fill = "Jenis Kelamin") +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
      )
  })
  output$plot4 <- renderPlot({
    ggplot(df %>%
      group_by(Usia) %>%
      summarise(Percent = n() / nrow(.) * 100), aes(x = "", y = Percent, fill = Usia)) +
      geom_col() +
      geom_text(aes(label = paste0(round(Percent, 2), "%")), position = position_stack(vjust = 0.5), colour = "white") +
      coord_polar(theta = "y") +
      labs(fill = "Usia") +
      xlab("") +
      ylab("Persentase") +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
      )
  })
  output$plot5 <- renderPlot({
    ggplot(df, aes(x = Usia, fill = Usia)) +
      geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
      xlab("Usia") +
      ylab("Total") +
      labs(fill = "Usia") +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
      )
  })
  output$plot6 <- renderPlot({
    ggplot(df %>%
      group_by(Pengeluaran.Per.Bulan) %>%
      summarise(Percent = n() / nrow(.) * 100), aes(x = "", y = Percent, fill = Pengeluaran.Per.Bulan)) +
      geom_col() +
      geom_text(aes(label = paste0(round(Percent, 2), "%")), position = position_stack(vjust = 0.5), colour = "white") +
      coord_polar(theta = "y") +
      labs(fill = "Pengeluaran Per Bulan") +
      xlab("") +
      ylab("Persentase") +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
      )
  })
  output$plot7 <- renderPlot({
    ggplot(df, aes(x = Pengeluaran.Per.Bulan, fill = Pengeluaran.Per.Bulan)) +
      geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
      xlab("Pengeluaran Per Bulan") +
      ylab("Total") +
      labs(fill = "Pengeluaran Per Bulan") +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
      )
  })
  output$plot8 <- renderPlot({
    ggplot(df %>%
      group_by(Tingkat.Pendidikan) %>%
      summarise(Percent = n() / nrow(.) * 100), aes(x = "", y = Percent, fill = Tingkat.Pendidikan)) +
      geom_col() +
      geom_text(aes(label = paste0(round(Percent, 2), "%")), position = position_stack(vjust = 0.5), colour = "white") +
      coord_polar(theta = "y") +
      labs(fill = "Tingkat Pendidikan") +
      xlab("") +
      ylab("Persentase") +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
      )
  })
  output$plot9 <- renderPlot({
    ggplot(df, aes(x = Tingkat.Pendidikan, fill = Tingkat.Pendidikan)) +
      geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
      xlab("Tingkat Pendidikan") +
      ylab("Total") +
      labs(fill = "Tingkat Pendidikan") +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
      )
  })
  output$plot10 <- renderPlot({
    ggplot(df %>%
      group_by(Pekerjaan) %>%
      summarise(Percent = n() / nrow(.) * 100), aes(x = "", y = Percent, fill = Pekerjaan)) +
      geom_col() +
      geom_text(aes(label = paste0(round(Percent, 2), "%")), position = position_stack(vjust = 0.5), colour = "white") +
      coord_polar(theta = "y") +
      labs(fill = "Pekerjaan") +
      xlab("") +
      ylab("Persentase") +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
      )
  })
  output$plot11 <- renderPlot({
    ggplot(df, aes(x = Pekerjaan, fill = Pekerjaan)) +
      geom_bar() +
      geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
      xlab("Pekerjaan") +
      ylab("Total") +
      labs(fill = "Pekerjaan") +
      theme(
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
      )
  })
  # Third Tab output
  output$tabelkontingensi <- renderPrint({
    tabel <- xtabs(as.formula(paste0("~", input$baris, "+", input$kolom)), data = df)
    print(tabel)
  })
  output$chisq <- renderPrint({
    tabel <- xtabs(as.formula(paste0("~", input$baris, "+", input$kolom)), data = df)
    print(chisq.test(tabel))
  })

  # Second tab output
  output$table <- renderTable({
    head(df[order(df[gsub("[ ]", ".", input$orderby_head)], decreasing = (input$order_head == "desc")), ], n = input$size_head)[, -1]
  })
  output$summarydata <- renderPrint({
    summary(df)
  })
  output$strdata <- renderPrint({
    str(df)
  })
  # Fourth tab output
  output$plot1 <- renderPlot(
    {
      if (input$visualisasi == "piechart") {
        ggplot(df %>%
          group_by(df[gsub("[ ]", ".", input$var_terpilih)]) %>%
          summarise(Percent = n() / nrow(.) * 100), aes(x = "", y = Percent, fill = .data[[gsub("[ ]", ".", input$var_terpilih)]])) +
          geom_col() +
          geom_text(aes(label = paste0(round(Percent, 2), "%")), position = position_stack(vjust = 0.5), colour = "white") +
          coord_polar(theta = "y") +
          labs(fill = input$var_terpilih) +
          xlab("") +
          ylab("Persentase") +
          ggtitle(paste("Diagram Lingkaran Persentase\nJumlah Pengunjung\nberdasarkan", input$var_terpilih)) +
          theme(
            plot.title = element_text(hjust = 0.5),
            text = element_text(size = 12),
            legend.position = "bottom",
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent"),
          )
      } else if (input$visualisasi == "histogram") {
        ggplot(df, aes(x = .data[[gsub("[ ]", ".", input$var_terpilih)]], fill = .data[[gsub("[ ]", ".", input$var_terpilih)]])) +
          geom_bar() +
          geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
          xlab(input$var_terpilih) +
          ylab("Total") +
          labs(fill = input$var_terpilih) +
          ggtitle(paste("Diagram Batang Jumlah\nPengunjung berdasarkan\n", input$var_terpilih)) +
          theme(
            plot.title = element_text(hjust = 0.5),
            text = element_text(size = 12),
            legend.position = "bottom",
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.box.background = element_rect(fill = "transparent"),
          )
      }
    },
    width = "auto",
    height = 500,
  )
  output$summarydatapilih <- renderPrint({
    summary(df[[gsub("[ ]", ".", input$var_terpilih)]])
  })
  output$interpretasi <- renderText({
    interpretasi <- as.vector(NULL)
    for (i in 1:length(summary(df[[gsub("[ ]", ".", input$var_terpilih)]]))) {
      interpretasi[i] <- paste("kategori", names(summary(df[[gsub("[ ]", ".", input$var_terpilih)]])[i]), "sebanyak", as.numeric(summary(df[[gsub("[ ]", ".", input$var_terpilih)]])[i]), "pelanggan")
    }
    if (input$visualisasi == "piechart") {
      paste("Berdasarkan diagram lingkaran diatas, untuk pelanggan yang berdasarkan", input$var_terpilih, "dapat kita lihat bahwa", toString(interpretasi))
    } else if (input$visualisasi == "histogram") {
      paste("Berdasarkan diagram batang diatas, untuk pelanggan yang berdasarkan", input$var_terpilih, "dapat kita lihat bahwa", toString(interpretasi))
    }
  })
  # Fourth tab output
  output$regresi <- renderText({
    prediksidata <- data.frame(
      Jenis.Kelamin = toString(input$pil2),
      Usia = toString(input$pil3),
      Pengeluaran.Per.Bulan = toString(input$pil4),
      Tingkat.Pendidikan = toString(input$pil5),
      Pekerjaan = toString(input$pil6),
      Frekuensi.Kunjungan = toString(input$pil7),
      Kenyamanan.kedai = toString(input$pil8),
      Perlengkapan.kedai = toString(input$pil9),
      Kapasitas.kedai = toString(input$pil10),
      Pelayanan = toString(input$pil11),
      Penampilan.karyawan = toString(input$pil12),
      Ketepatan.janji.layanan = toString(input$pil13),
      Simpati.ke.pelanggan = toString(input$pil14),
      Kecepatan.layanan.karyawan = toString(input$pil15),
      Karyawan.dapat.dipercaya = toString(input$pil16),
      Transaksi.aman = toString(input$pil17),
      Kesopanan.karyawan = toString(input$pil18),
      Karyawan.tahu.kebutuhan.pelanggan = toString(input$pil19),
      Suasana.kedai.nyaman = toString(input$pil20),
      Paket.harga.terbaik = toString(input$pil21),
      Harga.produk.masuk.akal = toString(input$pil22),
      Kualitas.makanan = toString(input$pil23)
    )
    paste("Berdasarkan analisis regresi logistik multinomial yang telah dilakukan dengan memilih besaran tiap variabel, maka didapatkan hasil predikasi tingkat kepuasan pelanggan yaitu", predict(model, newdata = prediksidata), "poin")
  })

  output$aic <- renderPrint({
    AIC(model)
  })
  output$interpretasiaic <- renderText({
    paste("Didapatkan nilai AIC dari model terbaik sebesar", round(AIC(model), 2))
  })
  output$interpretasibic <- renderText({
    paste("Didapatkan nilai BIC dari model terbaik sebesar", round(BIC(model), 2))
  })
  output$bic <- renderPrint({
    BIC(model)
  })
  output$model <- renderPrint({
    summary(model)
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
