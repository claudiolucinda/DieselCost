#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("BDT_Fcns.R")
source("Yield_Curve.R")
source("Petro_Losses.R")

load("Add_Data.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Quanto vai custar o desconto do Diesel?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("desconto",
                     "Dias de Desconto:",
                     min = 1,
                     max = 218,
                     value = 60, step=1),
         
         sliderInput("valor",
                     "Valor do Desconto:",
                     min=.01,
                     max = 2,
                     value =.46
         ),
         actionButton("do", "Calcular")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h1("Custo para a Sociedade: "),
        p("Vai aparecer aqui embaixo. Aguarde:"),
        tags$hr(),
        h3(uiOutput("valor")),
        tags$hr(),
        h2("Quanto Vamos pagar?"),
        p("Usei informações de mercado e do que observamos por aí para calcular 
          quais são os custos para a sociedade desta greve sem fim dos caminhoneiros. Já que vai ser a gente mesmo quem vai pagar a conta, é melhor saber o tamanho dela."),
        p("Note-se que os parâmetros são os de 28 de Maio de 2018."),
        p("Calculado como o valor de uma série de contratos a termo -- um para cada dia -- com valor igual ao preço de hoje menos o desconto. Usei um lattice de 10 períodos para cada dia."),
        p("Informações Técnicas: "),
        tags$ol(
          tags$li("Volatilidade: Volatilidade do Ativo Subjacente (USGulf Diesel), em BRL mais Custo de Internação"),
          tags$li("Taxa de Juros: Calculada como uma Lattice calibrada pelo modelo BDT e curva de juros dada pela equação da ANBIMA"),
          tags$li("Quantidade: Volume vendido em Abril de 2018")
        ),
        p("Para mais informações, contatar: claudiolucinda@usp.br")
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    out01<- eventReactive(input$do, {
     prejuizos_daily<-sapply(1:input$desconto, function(.x) Petro_Losses(.Big_T=.x,.Nr_Period = 10,.Coefs_YC = Coefs_ANBIMA,.Vol_dia = Vol_dia,.desconto =input$valor,.S0=2.1016))
     venda_dia<-venda_mensal_Diesel/30
     prejuizo_total<-sum(venda_dia*prejuizos_daily)
     prejuizo_pc<-prejuizo_total/209059554
     output1<-cat(paste0("Custo Total para a Sociedade em R$: ",
                         prettyNum(prejuizo_total,big.mark = ".",decimal.mark = ","), " e "))
     output2<-cat(paste0("Custo Per Capita em R$: ",
                         prettyNum(round(prejuizo_pc,2),big.mark = ".",decimal.mark = ",")))
     
   }, ignoreNULL = FALSE)
output$valor<- renderPrint({out01()})
}

# Run the application 
shinyApp(ui = ui, server = server)

