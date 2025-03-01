

#library(webr)


#library(plyr)
library(readxl)
#library(openxlsx)
#library(data.table)
library(shiny)
#library(shinyAce)
#source("chooser.R")

#library(lavaan)

#library(mnormt)
#library(curl)
#library(plspm)


########################################
########UI (User Interface)#############
########################################

modul_IDX_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
    
    
    
    tabsetPanel(
      
      tabPanel(title = tags$h5( tags$img(src = "dataset.png", width = "30px"), 'Dataset'),
               
               
    
    
      radioButtons(ns("pilih_sektor"),
                   label="Choose Sector:", 
                   choices = c("Healthcare", "Basic Materials", "Financials", "Transportation & Logistic", "Technology", "Consumer Non-Cyclicals", "Industrials", "Energy", "Consumer Cyclicals", "Infrastructures", "Properties & Real Estate"),
                   selected=c("Technology"), inline = TRUE),
      
      
      
 br(),
 
 uiOutput(ns("keterangan_sektor_teknologi")),  
 uiOutput(ns("buka_variabel_sektor_technology")),
 #uiOutput(ns("buka_data_sektor_technology")),
 DT::DTOutput(ns("buka_data_sektor_technology")),
 
               
               
               
               br()
 
 
 
 
 
 
 
 
      ),
 
 
 
 
 
 
 
 
 
 
 
 
 
 tabPanel(title = tags$h5( tags$img(src = "data analysis.png", width = "30px"), 'Data Analysis with IDX Data'),
          
          
          
          radioButtons(ns("pilih_metode"),
                       label="Choose Method:", 
                       choices = c("Regression", 
      "Panel Data Regression", 
      "Covariance Based Structural Equation Modeling (CB-SEM)", 
      "Path Analysis", 
      "Discriminant Analysis", 
      "Conjoint Analysis", 
      "Analysis of Covariance (ANCOVA)", 
      "Partial Least Squares Structural Equation Modeling (PLS-SEM)", 
      "Two State Least Squares (2SLS)", 
      "Logistic Regression", 
      "Autoregressive Integrated Moving Average (ARIMA)", 
      "Autoregressive Conditional Heteroscedastic (ARCH) & Autoregressive Generalized Conditional Heteroscedastic (GARCH)",
      "Vector Autoregressive (VAR) & Vector Error Correction Model (VECM)",
      "Independent t-Test & Mann-Whitney Test",
      "Analysis of Variance & Kruskal-Wallis Test",
      "Multivariate Analysis of Variance (MANOVA)",
      "Paired t-Test, Wilcoxon Test & Sign Test",
      "Pearson, Spearman & Kendall Correlation",
      "Friedman Test"),
                       selected=c("Partial Least Squares Structural Equation Modeling (PLS-SEM)"), inline = FALSE),    
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          br()
          
          
          
 ) #tabpanel Data Analysis with IDX Data
 
 
 
 
    ), #Akhir tabsetpanel
               
               
               
               
               
               
               
    
    
    br()
    
  ) #Akhir Fluidpage
  
  
} #Akhir dari modul_IDX_ui

#Akhir dari modul_IDX_ui
#Akhir dari modul_IDX_ui
#Akhir dari modul_IDX_ui
#Akhir dari modul_IDX_ui











































































########################################
################Server##################
########################################



modul_IDX_server <- function(input, output, session) {
  
  
  ###########Sektor Teknologi###########
  
  
  kirim_nama_kolom_sektor_teknologi <-function()
  {
    
    dat <- read_xlsx("www/data sektor technology.xlsx")
    
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat)
    
    return(nama)
    
    
  }
  
  
  
  output$buka_variabel_sektor_technology <- renderUI({
    
    
    cek <- input$pilih_sektor
    
    if(cek == "Technology")
    {
      # print("cek")
      #print(cek)
      #print(cek)
     # uiOutput(session$ns("buka_topik_psikologi"))
      
      
      
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_sektor_teknologi"), 
                         label="Select Variables:", choices = c( kirim_nama_kolom_sektor_teknologi()), selected=c("Sector", "Board", "Code", "Company", "Year","Listing Date (Year)","Source","Link Source", "Total Asset", "Total Liabilities", "Total Equity" ,"High", "Low", "Close", "Volume (th.share)"), inline = TRUE)
      
      
      
      
      
      
      
      
    }
    
    
    
  }) #buka_pilih_topik
  
  
  
  
  
  output$keterangan_sektor_teknologi <- renderUI({
    
    
    cek <- input$pilih_sektor
    
    if(cek == "Technology")
    {
    
    h2("Sector of Technology", style="
         font-family: 'cursive';
         color: blue;
         text-align:center
         ")
      
    }
    
    
  })
  
  
  
  #############Seleksi Data Sektor Teknologi
  
  
  data_seleksi_sektor_teknologi <- function()
  {
    
    dat <- read_xlsx("www/data sektor technology.xlsx")
    
    dat <- as.data.frame(dat)
    
    nama <- colnames(dat) 
    
    
    
    terpilih_variabel <- input$terpilih_variabel_sektor_teknologi
    
    dat_kirim = dat[c(terpilih_variabel)]
    
    return(dat_kirim)
    
    
  }
  
  ############
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########
  
  
  
  
  
  
  output$buka_data_sektor_technology <- DT::renderDT({
    
    cek <- input$pilih_sektor
    
    if(cek == "Technology")
    {
    dat <- data_seleksi_sektor_teknologi()
    print(dat)
    
    
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} #akhir dari modul_IDX_server

#akhir dari modul_IDX_server
#akhir dari modul_IDX_server
#akhir dari modul_IDX_server

















































































ui <- fluidPage(
  
  
  includeHTML("intro_home.html"),
  
  
  uiOutput("modul_IDX"),
  
  
  br()
  
) #Akhir dari UI











server <- function(input, output) {
  
  
  
  
  
  output$modul_IDX <- renderUI({
    
    
    
    #source("module//modul_IDX.R")
    callModule(module = modul_IDX_server, id = "modul_IDX")
    modul_IDX_ui(id = "modul_IDX")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server










shinyApp(ui, server)














