# test

# ======================================================================================

# bar

df <- read.csv("result.csv")
df <- df[df$항만명 == "부산", ]
df.test <- df[df$입출항구분=="출항"&df$Measures=="총계",c(1,2,7:37)]

# 연월 합쳐서 새컬럼
df.test$time <- c(paste(df.test$조회년도, df.test$조회월, sep = "_"))
df.test <- df.test[,c(-1,-2)]

value <- c(unlist(t(df.test[,-32])))
data <- data.frame(subject = rep(c(names(df.test)[1:31]),times=60), values=value,time=df.test$time)

# 상위 10 품목
data.name <- names(head(sort(tapply(data$value, data$subject,mean),decreasing = T),10))
data10 <- data[data$subject %in% data.name,]

# ======================================================================================

# map
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)
library(mapproj)

# 지도, 위도경도 데이터
ko.map <- map_data("world") %>% filter(region=="South Korea")
data <- world.cities %>% filter(country.etc=="Korea South")
data <- data[data$name %in% c("Pusan","Inchon","Kwangyang","Ulsan"),]
data <- data[,c(1,4:5)]
data$name <- c("인천", "광양", "부산", "울산")

df.all <- read.csv("result.csv")
df.all <- df.all[df.all$입출항구분 %in% c("입항", "출항") & df.all$Measures == "수출입계", c(1:4,6)]

# 데이터에 위도 경도 추가
df.all$lat <- rep(c(data[3,2],data[1,2],data[2,2],data[4,2]),time=12,each=2)
df.all$long <- rep(c(data[3,3],data[1,3],data[2,3],data[4,3]),time=12,each=2)
df.in <- df.all[df.all$입출항구분=="입항",]
df.out <- df.all[df.all$입출항구분=="출항",]

# ======================================================================================

library(shiny)


ui <- pageWithSidebar(
  headerPanel(h1("제목을 입력하는 칸")),
  
  sidebarPanel(
    
    h3("지도그래프"),
    selectInput("inout", "입항 or 출항",
                c("입항","출항")),
    
    selectInput("year", "연도를 선택하세요",
                c(2017:2021)),
    
    h3("막대그래프"),
    selectInput("time", "연, 월을 선택하세요",
                unique(df.test$time))
    
    ),
  
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Angel", h1("Visual Project Team 3"), h2("주제: 항만 수출입 데이터를 이용한 시각화"), 
               h2("　"),
               h2("구성원 및 역할"),
               h3("조장"), h4("손병립: 부산항"),
               h3("조원"), h4("김라희: 광양항"), h4("김재열: 울산항"), h4("이재상: 인천항") ),
      tabPanel("map", plotOutput("mapPlot", click = "plot_click"),
               tableOutput("mapdata")),
      tabPanel("subject10", plotOutput("barPlot")),
      tabPanel("gif",
               img(src="https://media2.giphy.com/media/hnZGRJJh5LDXXpfheq/giphy.gif?cid=790b76116d1849c79dd9ae72639b5dbce25dadfd3fbcc545&rid=giphy.gif&ct=g", align="center", height="400px", width='1000px'),
               img(src="https://media2.giphy.com/media/eVF0ffUwAJwh80B4BF/giphy.gif?cid=790b7611c5cdd8ae4e910a54611b8cacf06bc09824431f86&rid=giphy.gif&ct=g", align="center", height="400px", width='1000px'),
               img(src="https://media2.giphy.com/media/rnVuU5R3M9khNirnRQ/giphy.gif?cid=790b76114798274a6ef9310b3ae309ab68b6bb49ab5dc4e7&rid=giphy.gif&ct=g", align="center", height="400px", width='1000px'),
               img(src="https://media1.giphy.com/media/YCfhOsoBuRtD5Jn4jq/giphy.gif?cid=790b7611d5261d95667260798d44f9de69f80f5c6201820a&rid=giphy.gif&ct=g", align="center", height="400px", width='1000px')
               )
    )
  )
)

server <- function(input, output, session) {
  
  output$barPlot <- renderPlot({
    ggplot(data=subset(data10, time == input$time),
           aes(x=subject, y=values, fill=subject)) + 
      geom_bar(stat = 'identity') +
      theme_bw() +
      scale_x_discrete(limits = c(data.name))
  })
  
  output$mapPlot <- renderPlot({
    ggplot() +
        geom_polygon(data = ko.map, aes(x=long, y = lat, group = group), fill="grey", alpha=1) +
        theme_void() + coord_map() + ylim(34,38.5) + xlim(126, 129.6) +
        geom_point(data=df.all, aes(x=long, y = lat, size=3), col="tomato")+
        theme(legend.position="none")
  })
  
  output$mapdata <- renderTable({
    req(input$plot_click)
    nearPoints(subset(df.all, 입출항구분 == input$inout & 조회년도==input$year), input$plot_click)
  })
}
df.all


shinyApp(ui, server)


# ===============================


