

library(dplyr)
library(RColorBrewer)
library(stringi)
library(markdown)
library(ggplot2)
library(shiny)
library(plotly)
library(sidrar)
library(shinythemes)
library(highcharter)
library(stringr)

options(OutDec= ",") 

#Tabela 261

dados261<-get_sidra(api="/t/261/n1/all/n2/all/n3/all/v/allxp/p/last%2011/c1/allxt/c2/allxt/c58/1141,1142,1144,1145,1146,1147,1148,1149,1150,1151,1152,1153,2792,2793,3242,3243,3244/d/v93%203")


#Renomeando as colunas
colnames(dados261)<-c("nt_cod","nt","regiao_cod","regiao","va_cod","va","ano_cod","ano","dom_cod","domicilio","sexo_cod","sexo","id_cod","idade","um_cod","um","valor")

#transformando as variaveis que estao como "char" em "integer"
names1 <- c("nt_cod","regiao_cod","va_cod","ano_cod","dom_cod","sexo_cod","id_cod","um_cod")
dados261[names1] <- sapply(dados261[names1],as.numeric)

dados261$idade<-factor(dados261$idade, levels = c("70 anos ou mais","65 a 69 anos","60 a 64 anos","55 a 59 anos","50 a 54 anos",
                                                  "45 a 49 anos","40 a 44 anos","35 a 39 anos","30 a 34 anos","25 a 29 anos","20 a 24 anos",
                                                  "18 a 19 anos","15 a 17 anos","10 a 14 anos","5 a 9 anos","1 a 4 anos","Menos de 1 ano"))

a<-sort(unique(dados261$ano))  
b<-sort(unique(dados261$regiao))
gr<-sort(unique(dados261$regiao[which(dados261$nt_cod==2)]))
ufs<-sort(unique(dados261$regiao[which(dados261$nt_cod==3)]))

cores<-c("Set3", "Set2","Set1", "Pastel2","Pastel1","Paired","Dark2","Accent","YlOrRd","YlOrBr","YlGnBu","YlGn","Reds","Purples","PuRd","PuBuGn","PuBu","OrRd","Oranges","Greys","Greens","GnBu","BuPu","BuGn","Blues","Spectral","RdYlGn","RdYlBu","RdGy","RdBu","PuOr","PRGn","PiYG","BrBG")


#Tabela 262

dados262<-get_sidra(api="/t/262/n2/all/n3/all/v/allxp/p/last%2011/c86/allxt/c1/allxt/c2/allxt/d/v93%203")

#Renomeando as colunas
colnames(dados262)<-c("nt_cod","nt","regiao_cod","regiao","va_cod","va","ano_cod","ano", "cor_cod1","Raca","dom_cod","domicilio","sexo_cod","sexo","um_cod","um","valor")


#transformando as variaveis que estao como "char" em "integer"
names2 <- c("nt_cod","regiao_cod","va_cod","ano_cod","dom_cod","sexo_cod","cor_cod1","um_cod")
dados262[names2] <- sapply(dados262[names2],as.numeric)

dados262$Raca<- factor(dados262$Raca, levels = c("Branca","Parda", "Preta","Amarela","Indígena","Sem declaração"))
#c("Sem declaração","Preta","Parda","Indígena","Branca","Amarela" ))



################################################################################################################################

ui <- navbarPage(theme = shinytheme("cerulean"),"NeaR",
                 tabPanel("Sobre o projeto",
                          fluidPage(
                            tags$h3("VISUALIZAÇÃO DE DADOS INTERATIVOS NO R: CONSTRUINDO UMA APLICAÇÃO SHINY UTILIZANDO O SIDRA/IBGE"),
                            tags$p("Beatriz Rodrigues Pinna, Larissa de Carvalho Alves e Gustavo Henrique Mitraud Assis Rocha"),
                            tags$hr(),
                            h4("Resumo"),
                            br(),
                            tags$p("Dado o aumento de complexidade dos dados e das análises, torna-se essencial o emprego de novas ferramentas de visualização
                                   para auxiliar a comunicação entre especialistas e leigos. O objetivo deste estudo é apresentar algumas destas novas formas de
                                   visualização de dados, em especial as que permitem um maior controle do usuário final para investigar e filtrar as suas 
                                   variáveis de interesse de modo interativo. Para isto, produzimos uma demonstração empregando o pacote shiny para o R, 
                                   utilizando os dados do SIDRA (IBGE) como caso a ser relatado." ),
                            tags$p("A escolha do R se dá principalmente por ser um software livre e oferecer um conjunto de funções e bibliotecas estatísticas para a elaboração de técnicas gráficas simples e sofisticadas dentro dessa gramática dos gráficos. Desta forma, para a criação de uma aplicação web se faz necessário o estudo de alguns pacotes para a visualização que ajudem a revelar informações sobre os dados."),
                            tags$p("Para este trabalho foi necessário estudar os principais pacotes para confecção de gráficos interativos para visualizar as características importantes de um conjunto de dados. Os principais pacotes utilizados para representações gráficas são: ggplot2, plotly e highcharter. Apresentamos várias formas de gráficos interativos nesta aplicação produzida e indicamos como elas podem ser realizadas na forma de manuais introdutórios aos principais pacotes 
                                   de visualização disponibilizados em uma conta no RPubs.", a(href="http://rpubs.com/near_ence", "Clique aqui!"))
                            
                            )
                          ), #close tabPanel projeto
                 navbarMenu("Tipos de Gráficos",
                            tabPanel("Barras",
                                     sidebarLayout(position = "left",
                                                   sidebarPanel(h4("Crie a visualização"),
                                                                selectInput('color', 'Selecione a variável de interesse', choices = c("Grupos de idade" = "idade","Sexo" = "sexo","Situação do domicílio" = "domicilio", "Raça ou cor" = "Raca"), selected = "idade"),
                                                                selectInput('x', 'Selecione a variável do eixo x', choices = c("Região" = "regiao","Sexo" = "sexo","Situação do domicílio" = "domicilio"), selected = "regiao"),
                                                                #selectInput('y', 'Eixo y', choices = c("População Residente" ="valor"), selected = "valor"),
                                                                selectInput('ano', 'Selecione o ano', choices = a, selected = "2004"),       
                                                                selectInput('reg', 'Selecione o nível territorial', choices = c("Grande Região", "Unidade da Federação"), selected = "Unidade da Federação"),
                                                                conditionalPanel(
                                                                  condition = "input.reg == 'Unidade da Federação'",
                                                                  selectInput(
                                                                    "reg1","Selecione a(s) unidade(s)", choices = ufs, selected = "Acre", multiple=TRUE)),
                                                                conditionalPanel(
                                                                  condition = "input.reg == 'Grande Região'",
                                                                  selectInput(
                                                                    "reg2","Selecione a(s) região(ões)", choices = gr, selected = "Norte" , multiple=TRUE))
                                                   ), #close sidebarpanel barras
                                                   mainPanel(plotlyOutput('plot', width = "100%",height = "500px"),
                                                             sidebarPanel(h4("Personalize a aparência"),width = 7,
                                                                          tabsetPanel(
                                                                            tabPanel("Gráfico",
                                                                                     selectInput('pos', 'Selecione o tipo do gráfico de barras', choices = c("Empilhado"="stack","Lado a lado"="group"), selected = "stack"),
                                                                                     selectInput('cor2', 'Escolha uma paleta de cores', choices = cores, selected = "Dark2")
                                                                            ),
                                                                            tabPanel("Texto",
                                                                                     textInput('tt', 'Altere o título', value = "Gráfico de Barras"),
                                                                                     sliderInput('ang', 'Rotacione o nome da(s) categoria(s) da variável do eixo X', min=-90,max=90, step=45, value = -90 )
                                                                            )
                                                                          ) #close tabsetPanel
                                                             )#close sidebarPanel
                                                   )#close mainPanel barras
                                     ) #close siderlayout barras
                            ), #close tabPanel barras
                            tabPanel("Linhas",
                                     sidebarLayout(position = "left",
                                                   sidebarPanel(h4("Crie a visualização"),
                                                                selectInput('color1', 'Selecione a variável de interesse', choices = c("Região" = "regiao","Grupos de idade" = "idade","Sexo" = "sexo","Situação do domicílio" = "domicilio","Raça ou cor" = "Raca"), selected = "idade"),
                                                                #selectInput('x1', 'Selecione a variável do eixo x', choices = c("Ano" = "ano"), selected = "ano"),
                                                                #selectInput('y1', 'Eixo y', choices = c("População Residente" ="valor"), selected = "valor"),
                                                                selectInput('regs', 'Selecione o nível territorial', choices = c("Grande Região", "Unidade da Federação"), selected = "Unidade da Federação"),
                                                                conditionalPanel(
                                                                  condition = "input.regs == 'Unidade da Federação'",
                                                                  selectInput(
                                                                    "reg3","Selecione a unidade de federação", choices = ufs, selected = "Acre", multiple=FALSE)),
                                                                conditionalPanel(
                                                                  condition = "input.regs == 'Grande Região'",
                                                                  selectInput(
                                                                    "reg4","Selecione a região", choices = gr, selected = "Norte" , multiple=FALSE))
                                                   ), #close siderPanel série
                                                   mainPanel(plotlyOutput('plot2', width = "100%",height = "500px"),
                                                             sidebarPanel(h4("Personalize a aparência"),width = 7,
                                                                          tabsetPanel(
                                                                            tabPanel("Gráfico",
                                                                                     selectInput('cor3', 'Escolha uma paleta de cores', choices = cores, selected = "Dark2")
                                                                            ),
                                                                            tabPanel("Texto",
                                                                                     textInput('tt1', 'Altere o título', value = "Série"),
                                                                                     sliderInput('ang1', 'Rotacione o nome das categorias da variável do eixo X', min=-90,max=90, step=45, value = -90 )
                                                                                     
                                                                            )
                                                                          ) #close tabsetPanel
                                                             )#close sidebarPanel
                                                   )#close mainPanel série         
                                     ) #close siderlayout série
                                     
                            ), #close tabPanel série
                            tabPanel("Setores",
                                     sidebarLayout(position = "left",
                                                   sidebarPanel(h4("Crie a visualização"),
                                                                selectInput('xk', 'Selecione a variável de interesse', choices = c("Grupos de idade" = "idade","Sexo" = "sexo","Situação do domicílio" = "domicilio","Raça ou cor" = "Raca"), selected = "sexo"),
                                                                selectInput('anok', 'Selecione o ano', choices = a, selected = "2004"),       
                                                                selectInput('regsk', 'Selecione o nível territorial', choices = c("Grande Região", "Unidade da Federação"), selected = "Unidade da Federação"),
                                                                conditionalPanel(
                                                                  condition = "input.regsk == 'Unidade da Federação'",
                                                                  selectInput(
                                                                    "reg3k","Selecione a unidade de federação", choices = ufs, selected = "Acre", multiple=FALSE)),
                                                                conditionalPanel(
                                                                  condition = "input.regsk == 'Grande Região'",
                                                                  selectInput(
                                                                    "reg4k","Selecione a região", choices = gr, selected = "Norte" , multiple=FALSE))
                                                   ), #close siderPanel série
                                                   mainPanel(plotlyOutput('plot3', width = "100%",height = "500px"),
                                                             sidebarPanel(h4("Personalize a aparência"),width = 7,
                                                                          tabsetPanel(
                                                                            tabPanel("Gráfico",
                                                                                     selectInput('cor3k', 'Escolha uma paleta de cores', choices = cores, selected = "Dark2")
                                                                            ),
                                                                            tabPanel("Texto",
                                                                                     textInput('tt1k', 'Altere o título', value = "Setores")
                                                                            )
                                                                          ) #close tabsetPanel
                                                             )#close sidebarPanel
                                                   )#close mainPanel série         
                                     ) #close siderlayout série
                                     
                            ), #close tabPanel série
                            tabPanel("Boxplot",
                                     sidebarLayout(position = "left",
                                                   sidebarPanel(h4("Crie a visualização"),
                                                                selectInput('eixobox', 'Selecione a variável de interesse', choices = c("Grupos de idade" = "idade","Sexo" = "sexo","Situação do domicílio" = "domicilio","Raça ou cor" = "Raca"), selected = "sexo")
                                                   ), #close siderPanel série
                                                   mainPanel(plotlyOutput('plot4', width = "100%",height = "500px"),
                                                             sidebarPanel(h4("Personalize a aparência"),width = 7,
                                                                          tabsetPanel(
                                                                            tabPanel("Gráfico",
                                                                                     selectInput('corbox', 'Escolha uma paleta de cores', choices = cores, selected = "Dark2")
                                                                            ),
                                                                            tabPanel("Texto",
                                                                                     textInput('ttbox', 'Altere o título', value = "Boxplot"),
                                                                                     sliderInput('ang2', 'Rotacione o nome das categorias da variável do eixo X', min=-90,max=90, step=45, value = -90 )
                                                                            )
                                                                          ) #close tabsetPanel
                                                             )#close sidebarPanel
                                                   )#close mainPanel série         
                                     ) #close siderlayout série
                                     
                            ), #close tabPanel box
                            tabPanel("Mapas",
                                     sidebarLayout(position = "left",
                                                   sidebarPanel(h4("Crie a visualização"),
                                                                selectInput('anomap', 'Selecione o ano', choices = a, selected = "2004" )
                                                   ), #close siderPanel série
                                                   mainPanel(highchartOutput('plot5', width = "100%",height = "500px")
                                                             ,sidebarPanel(h4("Personalize a aparência"),width = 7,
                                                                           tabsetPanel(tabPanel("Texto",
                                                                                                textInput('ttmap', 'Altere o título', value = "População Residente")
                                                                                                
                                                                           )
                                                                           ) #close tabsetPanel
                                                             )#close sidebarPanel
                                                   )#close mainPanel série         
                                     ) #close siderlayout série
                                     
                            ) #close tabPanel box
                 ) #close navbar Menu gráficos
                          )# close navbarPage




fc<-function(data, x, y, ...){
  data %>% filter(nt == x) %>% filter(ano == y) %>% group_by_(...) %>%
    summarise(valor2 = sum(valor, na.rm = TRUE)) %>%
    mutate(valor = valor2 / sum(valor2)) -> data
  return(data)}

fc1<-function(data, ...) {
  data %>% group_by_(...) %>%
    summarise(valor = sum(valor, na.rm = TRUE)) -> data
  return(data)}



fc2<-function(data,z, ...) {
  data %>% filter(ano == z)  %>% group_by_(...) %>%
    summarise(valor3 = sum(valor, na.rm = TRUE)) %>%
    mutate(valor4 = (valor3 / sum(valor3))*100) -> data
  return(data)}



fc3<-function(data, ...){
  data %>% filter(nt_cod == 3)  %>% group_by_(...) %>%
    summarise(valor5 = sum(valor, na.rm = TRUE)/1000) -> data
  return(data)}


fc4<-function(x) {
  substr(x, start = 0, stop = str_locate(x, ",")-1)
}


fc5<-function(x) {
  stri_trans_general(x, "Latin-ASCII")}


fc6<-function(data,z, ...) {
  data %>% filter(nt_cod==3) %>% filter(ano == z)  %>% group_by_(...) %>%
    summarise(valor6 = (sum(valor, na.rm = TRUE))/1000, digits = 0) -> data
  return(data)}




server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    
    if(input$color=="Raca"){
      data1<- reactive(fc(dados262, input$reg,input$ano, input$x, input$color)) 
      
      data3<-reactive(
        if (input$x == "regiao" & input$reg == "Unidade da Federação")
        {data1() %>% filter(regiao %in% input$reg1)}
        else if (input$x == "regiao" & input$reg == "Grande Região")
        {data1() %>% filter(regiao %in% input$reg2)}
        else 
        {data1()}
      ) #close reactive data3
      
      plot_ly(data = data3(), x = ~get(input$x), y = ~valor, color = ~get(input$color), 
              type = "bar",  colors = input$cor2)%>%
        layout(title = input$tt,
               xaxis = list(title = " ", tickangle = input$ang),
               yaxis = list(title = "Proporção"),
               margin = list(b = 100),
               barmode = input$pos,
               separators = ",.")
      
    }else{
      data1<- reactive(fc(dados261, input$reg,input$ano, input$x, input$color)) 
      
      data3<-reactive(
        if (input$x == "regiao" & input$reg == "Unidade da Federação")
        {data1() %>% filter(regiao %in% input$reg1)}
        else if (input$x == "regiao" & input$reg == "Grande Região")
        {data1() %>% filter(regiao %in% input$reg2)}
        else 
        {data1()}
      ) #close reactive data3
      
      plot_ly(data = data3(), x = ~get(input$x), y = ~valor, color = ~get(input$color), 
              type = "bar",  colors = input$cor2)%>%
        layout(title = input$tt,
               xaxis = list(title = " ", tickangle = input$ang),
               yaxis = list(title = "Proporção"),
               margin = list(b = 100),
               barmode = input$pos,
               separators = ",.")}
    
    
  }) #close output$plot
  
  output$plot2 <- renderPlotly({
    
    if(input$color1=="Raca"){
      data2<- reactive(
        fc1(dados262,input$color1, "ano", "regiao"))
      
      data4<- reactive(
        if (input$regs == "Grande Região"){
          data2() %>% filter(regiao == input$reg4)
        }else if (input$regs == "Unidade da Federação"){
          data2() %>% filter(regiao == input$reg3) 
        } )
      
      plot_ly(data = data4(), x = ~ano, y = ~valor,type="scatter", color = ~get(input$color1) ,split=~get(input$color1), mode = 'markers',colors = input$cor3, marker = list(fill = brewer.pal(18, input$cor3)),line=list(fill = brewer.pal(18, input$cor3)))%>% 
        layout(title = input$tt1,
               xaxis = list(title = "Anos", tickangle = input$ang1 ),
               yaxis = list(title = "Mil pessoas", exponentformat = "none"),
               margin = list(b = 100),
               barmode = "stack",
               exponentformat= "none", 
               separators = ",.")
      
    } else{
      data2<- reactive(
        fc1(dados261,input$color1, "ano", "regiao"))
      
      data4<- reactive(
        if (input$regs == "Grande Região"){
          data2() %>% filter(regiao == input$reg4)
        }else if (input$regs == "Unidade da Federação"){
          data2() %>% filter(regiao == input$reg3) 
        } )
      
      plot_ly(data = data4(), x = ~ano, y = ~valor,type="scatter", color = ~get(input$color1) ,split=~get(input$color1), mode = 'markers',colors = input$cor3, marker = list(fill = brewer.pal(18, input$cor3)),line=list(fill = brewer.pal(18, input$cor3)))%>% 
        layout(title = input$tt1,
               xaxis = list(title = "Anos", tickangle = input$ang1),
               yaxis = list(title = "Mil pessoas", exponentformat = "none"),
               margin = list(b = 100),
               barmode = "stack",
               
               separators = ",.")}
    
    
  }) #close output$plot2
  
  
  output$plot3 <- renderPlotly({
    
    if(input$xk=="Raca"){
      data5<- reactive(fc2(dados262, input$anok, input$xk, "regiao")) 
      
      data6<-reactive(
        if (input$regsk == "Unidade da Federação")
        {data5() %>% filter(regiao == input$reg3k)}
        else
        {data5() %>% filter(regiao == input$reg4k)}
      ) #close reactive data6
      plot_ly(data = data6(), labels = ~get(input$xk), values = ~valor3, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#000000'),
              #hoverinfo = c(input$xk, 'valor3'),
              # text = ~paste(~get(input$xk), valor3, 'por mil pessoas'),
              marker = list(colors = colorRampPalette(brewer.pal(12, input$cor3k))(17) ),
              showlegend = FALSE) %>%
        layout(title = input$tt1k,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               separators = ",.")  
    } else {
      data5<- reactive(fc2(dados261, input$anok, input$xk, "regiao")) 
      
      data6<-reactive(
        if (input$regsk == "Unidade da Federação")
        {data5() %>% filter(regiao == input$reg3k)}
        else
        {data5() %>% filter(regiao == input$reg4k)}
      ) #close reactive data6
      
      
      plot_ly(data = data6(), labels = ~get(input$xk), values = ~valor3, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#000000'),
              hoverinfo = "label+percent",
              # text = ~paste(~get(input$xk), valor3, 'por mil pessoas'),
              marker = list(colors = colorRampPalette(brewer.pal(12, input$cor3k))(17) ),
              showlegend = FALSE) %>%
        layout(title = input$tt1k,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               separators = ",.") }
    
  }) #close output$plot3
  
  
  
  output$plot4 <- renderPlotly({
    
    
    if(input$eixobox=="Raca"){
      data6<- reactive(fc3(dados262, "regiao", "ano", input$eixobox)) 
      
      plot_ly(data = data6(), x = ~ano, y = ~valor5,  color = ~get(input$eixobox), type="box",
              colors = input$corbox, marker = list(fill = brewer.pal(18, input$corbox))
              #marker = list(colors = colorRampPalette(brewer.pal(12, input$corbox))(17))
      )%>% 
        layout(boxmode = "group",
               title = input$ttbox,
               xaxis = list(title = " ", tickangle = input$ang2),
               yaxis = list(title = "Mil de pessoas", exponentformat = "none"),
               margin = list(b = 100),
               separators = ",.")
      
    }else {
      
      data6<- reactive(fc3(dados261, "regiao", "ano", input$eixobox)) 
      
      plot_ly(data = data6(), x = ~ano, y = ~valor5,  color = ~get(input$eixobox), type="box",
              colors = input$corbox, marker = list(fill = brewer.pal(18, input$corbox))
              #marker = list(colors = colorRampPalette(brewer.pal(12, input$corbox))(17))
      )%>% 
        layout(boxmode = "group",
               title = input$ttbox,
               xaxis = list(title = "Anos", tickangle = input$ang2),
               yaxis = list(title = "Milhões de pessoas", exponentformat = "none"),
               margin = list(b = 100),
               separators = ",.")}
    
    
    
    
    
  }) #close output$plot4
  
  
  
  output$plot5 <- renderHighchart({
    
    mapdata <- get_data_from_map(download_map_data("countries/br/br-all"))
    
    mapdata = mapdata %>%
      mutate(regiao2 = fc4(mapdata$`woe-label`))
    
    dados261$regiao2<-dados261$regiao
    
    dados261$regiao2<-fc5(dados261$regiao2)
    
    data7<- reactive(fc6(dados261, input$anomap, "regiao","regiao2","ano")) 
    
    
    data8<-reactive(merge(x = data7(), y = mapdata, by = "regiao2", all = TRUE))
    
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    hcmap("countries/br/br-all", data = data8(), value = "valor6",
          joinBy = c("hc-a2", "hc-a2"), name= " ", color=unique,
          tooltip = list(valueDecimals = 6, valuePrefix = "")) %>%
      hc_title(text = input$ttmap)  %>%
      hc_tooltip(pointFormat = "Estado: {point.regiao} <br> População Residente: {point.valor6: .,6f} milhões ")
    
    
    
  })
  
} #close server



shinyApp(ui, server)


