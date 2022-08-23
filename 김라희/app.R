#
# This is a Shiny web application. 
# You can run the application by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


# Load library
library(bslib)
library(shiny)
library(ggplot2)
library(hrbrthemes)
library(tidyverse)

hrbrthemes::import_roboto_condensed()

# Load dataset
harbor <- read.csv('harbor.csv')

# Lollipop Graph
total <- subset(harbor, Measures=='총계', select=-합계)

# 2017~2021년 품목별 거래량 중앙값
y2017 <- summary(total[1:4, 5:36])[3,]
y2018 <- summary(total[5:8, 5:36])[3,]
y2019 <- summary(total[9:12, 5:36])[3,]
y2020 <- summary(total[13:16, 5:36])[3,]
y2021 <- summary(total[17:20, 5:36])[3,]

# 텍스트에서 중앙값 숫자만 가져오기
value <- gsub('\\D', '', y2017)
value <- as.numeric(value)

value2 <- gsub('\\D', '', y2018)
value2 <- as.numeric(value2)

value3 <- gsub('\\D', '', y2019)
value3 <- as.numeric(value3)

value4 <- gsub('\\D', '', y2020)
value4 <- as.numeric(value4)

value5 <- gsub('\\D', '', y2021)
value5 <- as.numeric(value5)

# 품목명 리스트
column <- names(harbor)[6:37]

# 년도별 품목별 중앙값 데이터프레임 생성
items <- data.frame(column, value, value2, value3, value4, value5)
names(items) <- c('품목명', '2017', '2018', '2019', '2020', '2021')

items2 <- items %>%
  arrange(items[, -1]) %>%
  mutate(x=factor(품목명, 품목명))


# Bar Chart
# 조회년도, 항만명, 입출항구분, Measures, 합계 추출
harbor2 <- harbor[c(1:5)]

# 국적선, 외국선, 연안선 합계 추출
condition <- (harbor2$Measures=='국적선' | harbor2$Measures=='외국선' | harbor2$Measures=='연안선')
harbor2 <- harbor2[condition,]

# # 년도별 입항, 출항, 입항환적, 출항환적 합계(톤) 변화
# ggplot(harbor2) +
#   aes(x=입출항구분, fill=Measures, weight=합계) +
#   geom_bar(position='dodge') +
#   scale_fill_viridis_d(option='viridis', direction = 1) +
#   labs(title='2017~2021년 광양항 입출항환적 거래량') +
#   theme_light() +
#   theme(legend.position='left',
#         plot.title=element_text(size=24L, hjust=0.5))


# Define UI
ui <- fluidPage(
    
    # application theme
    theme = bs_theme(version=4, bootswatch='minty'),

    # application title
    titlePanel("광양항 품목별 입출항환적 거래량"),
    
    # sidebar layout
    sidebarLayout(
      sidebarPanel(
        # select type of year plot
        radioButtons('Year', '', unique(harbor2$조회년도))
      ),
      mainPanel(
        # output bar plot and reference
        plotOutput(outputId='barplot', height='700px'),
        tags$a(href='https://new.portmis.go.kr/portmis/websquare/websquare.jsp?w2xPath=/portmis/w2/main/intro.xml',
                 '출처: 해운항만 물류정보시스템', target='_blank')
      )
    )
)


# Define server logic
server <- function(input, output) {
  
  # bar plot 
  output$barplot <- renderPlot({
    ggplot(items2, aes(x=x, y=`2017`)) +
    geom_segment(aes(x=x, xend=x, y=0, yend=`2017`)) +
    geom_point(size=4, pch=21, bg=4, col=1) +
    coord_flip(clip='off', expand=TRUE) +
    scale_y_continuous(labels=scales::comma) +
    
    # theme settings
    theme_ipsum(base_family='D2Coding') +
    xlab('') +
    ylab('[단위: 톤]')
  })
}  
    
  # gganimate
  # transition_states(states=items[, -1],
  #                   transition_length=4, state_length=1) +
  # ease_aes('cubic-in-out')
  
  #rendering the animation for gif
  # final_animation <- animate(plt,
  #                            100,
  #                            fps = 20,
  #                            duration = 30,
  #                            width = 950,
  #                            height = 750,
  #                            renderer = gifski_renderer())


# run the application
shinyApp(ui, server)
