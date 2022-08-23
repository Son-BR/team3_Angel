library(CGPfunctions)
library(gganimate)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(ggplot2)
library(gifski)
library(GGally)
library(plotly)
library(shiny)
library(shiny)
library(dplyr)
library(tidyr)
library(plyr)

data <- read.csv('result.csv', header=1)

port <- read.csv('result.csv', header=1)

port <- rename(port,
               c('조회년도'='year',
                 '조회월'='month',
                 '항만명'='portName',
                 '입출항구분'='ADCategory',
                 '합계'='total'))

data <- rename(data,
               c('조회년도'='year',
                 '조회월'='month',
                 '항만명'='portName',
                 '입출항구분'='ADCategory',
                 '합계'='total'))

port <- port[port$portName=='인천',]
port <- port|>
  gather(key='By_item', 
         value='ton',colnames(port)[7:37])
data <- data[data$portName=='인천',1:37]
data$total <- rowSums(data[,7:37])

# 품목별 대분류 설정
food <-  c("육.류", "어패류.갑각류.등", "양.곡", "제분공업.생산품", "기타동.식물성.생산품", "동.식물성.유지류", "당.류", "조제식품.음료.주류등")
material <-  c("시멘트", "모.래", "무연탄", "유연탄", "철광석", "기타광석.및.생산품", "원유.역청유...석유", "석유.정제품", "석유가스.및.기타가스", "비.료")
light <-  c("플라스틱.고무및.제품", "피혁류.및.그제품", "원.목", "목재.목탄.코르크.등", "방직용섬유.및.그제품")
heavy <-  c("화학공업.생산품", "고.철", "철강.및.그제품", "비철금속.및.그제품", "기계류.및.그부품", "전기기기.및.그부품", "차량.및.그부품", "항공기.선박.및.부품", "기.타")

allitem <-  c("육.류", "어패류.갑각류.등", "양.곡", "제분공업.생산품", "기타동.식물성.생산품", "동.식물성.유지류", "당.류", "조제식품.음료.주류등", 
              "시멘트", "모.래", "무연탄", "유연탄", "철광석", "기타광석.및.생산품", "원유.역청유...석유", "석유.정제품", "석유가스.및.기타가스", "비.료", 
              "플라스틱.고무및.제품", "피혁류.및.그제품", "원.목", "목재.목탄.코르크.등", "방직용섬유.및.그제품", 
              "화학공업.생산품", "고.철", "철강.및.그제품", "비철금속.및.그제품", "기계류.및.그부품", "전기기기.및.그부품", "차량.및.그부품", "항공기.선박.및.부품")

port$category <- port$By_item

for (i in food){
  port$category <- gsub(i, 'consumer goods', port$category)
}
for (i in material){
  port$category <- gsub(i, 'material', port$category)
}
for (i in light){
  port$category <- gsub(i, 'light industry goods', port$category)
}
for (i in heavy){
  port$category <- gsub(i, 'heavy & chem industry goods', port$category)
}


# 수출입계, 총계 행 제거
port <- port[!(port$Measures=="수출입계"),]
port <- port[!(port$Measures=="총계"),]
table(port$Measures)

data <- data[!(data$Measures=="수출입계"),]
data <- data[!(data$Measures=="총계"),]

# 데이터 타입 변환
port$year <- as.factor(port$year)
port$month <- as.factor(port$month)
port$ADCategory <- as.factor(port$ADCategory)
port$Measures <- as.factor(port$Measures)
port$category <- as.factor(port$category)

data$year <- as.factor(data$year)
data$month <- as.factor(data$month)
data$ADCategory <- as.factor(data$ADCategory)
data$Measures <- as.factor(data$Measures)


#---------------------------------------------------------------------------------
# 연도별 입출항 현황 다중 막대 그래프
port.a <- aggregate(port$total,list(port$year,port$ADCategory),sum)
colnames(port.a) <- c('year', 'ADCategory', 'total')
port.a$total <- round(port.a$total/10000,0)

ui <- fluidPage(
  tags$h1('연도별 수출입 현황'),
  tags$h3('단위 : 만 톤 (ton)'),
  plotOutput('plot')
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    ggplot(port, aes(x=year, y=total, fill=ADCategory))+
      geom_col(position='dodge')+theme_bw()

  })
  
}

shinyApp(ui, server)
#----------------------------------------------------------------------------------
data.a <- data[data$ADCategory=='입항',]
data.a <- aggregate(data.a[,6], by=list(data.a$year,data.a$Measures), FUN=sum)
colnames(data.a) <- c('year', 'Measures', 'total')
data.a$total <- round(data.a$total/10000,0)

data.p <- data[data$ADCategory=='출항',]
data.p <- aggregate(data.p[,6], by=list(data.p$year,data.p$Measures), FUN=sum)
colnames(data.p) <- c('year', 'Measures', 'total')
data.p$total <- round(data.p$total/10000,0)


ui <- fluidPage(
  headerPanel(h1('연도별 선박 현황'), 
              h3('기간 : 2017 ~ 2021년')),
  mainPanel(
    tabsetPanel(
      tabPanel('입항',
               plotOutput('plot1')),
      tabPanel('출항',
               plotOutput('plot2'))
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    newggslopegraph(data.a,year,total,Measures,
                    Title=' ',
                    SubTitle=' ',
                    Caption='단위 (만 톤(ton))',
                    XTextSize = 20,    # Size of the times
                    YTextSize = 7,     # Size of the groups
                    TitleTextSize = 20,
                    SubTitleTextSize = 15,
                    CaptionTextSize = 17,
                    TitleJustify = "left",
                    SubTitleJustify = "left",
                    CaptionJustify = "right",
                    DataTextSize = 5,
                    LineColor = c('brown', 'blue', 'red', 'orange'),
                    LineThickness = 2)
  })
  output$plot2 <- renderPlot({
    newggslopegraph(data.p,year,total,Measures,
                    Title=' ',
                    SubTitle=' ',
                    Caption='단위 (만 톤(ton))',
                    XTextSize = 20,    # Size of the times
                    YTextSize = 7,     # Size of the groups
                    TitleTextSize = 20,
                    SubTitleTextSize = 15,
                    CaptionTextSize = 17,
                    TitleJustify = "left",
                    SubTitleJustify = "left",
                    CaptionJustify = "right",
                    DataTextSize = 5,
                    LineColor = c('brown', 'blue', 'red', 'orange'),
                    LineThickness = 2)
  })
  
}


shinyApp(ui, server)

# ----------------------------------------------------------------
ui <- fluidPage(
  tags$h1('연도별 수출입 현황'),
  tags$h3('중분류 기준'),
  tags$img(src="https://media0.giphy.com/media/1vrAR4gCDmLnPpBbMq/giphy.gif?cid=790b76117743789543fb5334664c78d350358f65fce24c76&rid=giphy.gif&ct=g"))

server <- function(input,output){
  
}

shinyApp(ui, server)
# ----------------------------------------------------------------
ui <- fluidPage(
  tags$h1('연도별 수출입 현황'),
  tags$h3('중분류 기준'),
  tags$img(src="https://media1.giphy.com/media/8piqHXMAy3ueIcdPtb/giphy.gif?cid=790b7611c6b5e346f9c1168de81509e4e8cdea478ab5eb56&rid=giphy.gif&ct=g"))

server <- function(input,output){
  
}

shinyApp(ui, server)