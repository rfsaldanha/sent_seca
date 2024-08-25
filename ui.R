library(leaflet)
library(shiny)
library(shinybusy)
#source("global.R");
#setwd("/dados/htdocs/shiny.icict.fiocruz.br/sent_seca/")
load("dados_sent_seca.RData");

navbarPage(title = div(img(src='selo_obs_h.png',
                           style="margin-top: -14px;
                               padding-right:0px;
                               padding-bottom:10px",
                           height = 60)), id="nav",
  tags$body(
    includeHTML("libras.html"),
    tags$script(src = "https://vlibras.gov.br/app/vlibras-plugin.js"),
    includeScript("libras.js")
  ),
  tabPanel("Mapa",
    # Spinner for map load
    add_busy_spinner(spin = "semipolar", margins = c(20,30)),
    
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 630, height = "auto",
        h2("Precipitação e Cobertura Vegetal no Semiárido"),
        sidebarPanel(
#          selectInput(
#            'cod_mes', 'Mês', choices = mes$desc,
#            selectize = T
#          ),
#          selectInput(
#            'cod_ano', 'Ano', choices = c(2001:2015),
#            selectize = T
#          )

            fluidRow(
              column(
                width = 4,
                selectInput("cod_map", label = "Índice da seca", choices = c("Precipitação")),
              ),
              column(
                width = 4,
                selectInput("cod_ano", label = "Ano",
                            choices = c(2001:2015), selected = TRUE, multiple = FALSE), 
              ),
              column(
                width = 4,
                selectInput(inputId="cod_mes", label="Mês", choices = c(1:12), selected = TRUE, multiple = FALSE
                ), 
              )
            ),
          
          width = '100%'
          
          ), 

          # AI generated text  
          uiOutput("map_descr_ia"),

          # h5("Município selecionado:", verbatimTextOutput("munic_sel")),
          # selectInput('munic_sel', '', 
          #             c("",paste(geo$cod6,as.character(geo$NOME_MUNIC),'/', geo$SIGLA)), 
          #             selectize=T),
          # 
          # Plots
          plotOutput("histCentile", height = 200),
          plotOutput("scatterCollegeIncome", height = 250),
          helpText("Nota: Selecione um município para visualização na aba de gráfico."),
      ),

      tags$div(id="cite",
        'Painel desenvolvido por Observatório Clima e Saúde e PCDaS', tags$em('(Lis/Icict/Fiocruz)'), ''
      )
    )
  ),
  tabPanel("Indicadores por Município",
         # Spinner for map load
         add_busy_spinner(spin = "semipolar", margins = c(20,30)),
         # App title ----
         titlePanel("Saúde no Semiárido"),
         
         # Sidebar layout with input and output definitions ----
         sidebarLayout(
           
           # Sidebar panel for inputs ----
           sidebarPanel(
             # Input: Select a dataset ----
             selectInput("dataset", "Selecione um indicador:",
                         choices = c("Internação Diarréia e Gastroenterite Origem Infecção Presumível", 
                                     "Taxa de internação por asma",
                                     "Taxa de internação por dengue",
                                     "Incidência de dengue clássico"),
                         selected = TRUE, multiple = FALSE),
             
             radioButtons("cod_idade", "Faixa etária",
                          c("00 - 04 anos"=4, 
                            "05 - 09 anos"=509,
                            "10 - 19 anos"=1019,
                            "20 - 64 anos"=2064,
                            "65 - 99 anos"=6500)),
             
             selectInput('munic_sel_2', 'Consulte outro município', 
                         c("",paste(geo$cod6,as.character(geo$NOME_MUNIC),'/', geo$SIGLA)), 
                         selectize=T, selected = ""),
             
             # Input: Specify the number of observations to view ----
             #numericInput("obs", "Número de observações para visualização em tabela:", 10),
             
             
             # Include clarifying text ----
             helpText("Nota: A seleção temporal do gráfico é realizada na",
                      "aba 'Mapas'",
                      ""),
             # Input: actionButton() to defer the rendering of output ----
             # until the user explicitly clicks the button (rather than
             # doing it immediately when inputs change). This is useful if
             # the computations required to render output are inordinately
             # time-consuming.
             #actionButton("update", "Atualizar filtro")
           ),
           
           # Main panel for displaying outputs ----
           mainPanel(
             h4("Município selecionado:", verbatimTextOutput("munic_sel_2")),#
             uiOutput("mun_selected"),
             
             dygraphs::dygraphOutput("plot_g1"),
             #verbatimTextOutput("info"),
             
             # Output: Header + summary of distribution ----
             HTML("<BR>"),
             h5("Resumo:"),
             uiOutput("plot_g1_descr_ia"),
             HTML("<BR>"),
             # Output: Header + table of distribution ----
             h5("Dados:"),
             downloadButton('downloadData', 'Download'),
             tableOutput("view")
           )
         )
      ),
  tabPanel("Séries e Tendências",
     # Spinner for map load
     add_busy_spinner(spin = "semipolar", margins = c(20,30)),
     h4("Município selecionado:"),
     uiOutput("mun_selected_2"),
     HTML("<BR>"),
     h4("Precipitação média acumulada"),
     dygraphs::dygraphOutput("plot_g2.1"), 
     # AI generated text
     HTML("<BR>"),
     h5("Resumo:"),
     uiOutput("plot_g2.1_descr_ia"),
     HTML("<BR><BR>"),
     h4("Condição da vegetação (NDVI)"),
     dygraphs::dygraphOutput("plot_g2.2"),
     # AI generated text
     HTML("<BR>"), 
     h5("Resumo:"),
     uiOutput("plot_g2.2_descr_ia"),
     HTML("<BR><BR>"),
     h4("Indicador de saúde",  verbatimTextOutput("dataset")),
     dygraphs::dygraphOutput("plot_g2.3"),
     # AI generated text
     HTML("<BR>"),  
     h5("Resumo:"),
     uiOutput("plot_g2.3_descr_ia"),
     HTML("<BR><BR>"),
   )
)


