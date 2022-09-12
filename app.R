#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
# prepare
library("xts")
version="0.1"
#load from commutag
#info <- read.csv(file = './info.csv')
url_str="https://commutag.agawork.tw/dataset/list-image?all=1&dataset=630573b110be1564a1c8d123"
con <- url(url_str)
txt <- readLines(con)
commlist<-jsonlite::fromJSON(txt)
info=commlist$data
# add cnt=1 column
row_cnt <- nrow(info)
cnt<-seq(from=1,to=1,length.out=row_cnt)
info$cnt = cnt

# convert string to numeric format
info$createdAt1 = as.POSIXct(info$createdAt)




# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(paste("2022地方選舉看板 群眾標註",version)),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        selectInput(inputId = "party",
                  label = "選個政黨:",
                  choices = c("全部","民進黨", "國民黨", "民眾黨","時代力量","基進黨","其他","無黨")),
                  
        selectInput(inputId = "county",
                    label = "選個縣市:",
                    #choices = c("全部")),
                   choices = c("全部","桃園市","臺南市","臺中市","新北市","高雄市","臺北市","嘉義市","新竹市","基隆市","澎湖縣","花蓮縣","臺東縣","屏東縣","嘉義縣","雲林縣","南投縣","彰化縣","苗栗縣","新竹縣","宜蘭縣","金門縣","連江縣")),
      ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("info"),
           plotOutput("distPlot"),
           plotOutput("partyPlot"),
           plotOutput("countyPlot"),
           tableOutput("view")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$info <- renderPrint({
    time_str<-strftime(Sys.time(), format = "%Y/%m/%d %H:%M:%S", tz = "Asia/Taipei")
    cat(paste("本儀表板主要提供觀察目前公民科學上傳的看板統計資訊。",
              "由於自願者自由意志上傳，所以上傳者在哪，就只會呈現那部份的資料。",
              "警語：請勿過度詮釋。提醒：分享時請注意提供資料時間",
              paste("目前資料 Taipei 時間：",time_str),sep="\n"))
      })
    output$distPlot <- renderPlot({
      info_f1 <- info
      if(input$party!="全部") {
        info_f1 <- filter(info, formReply$ee70rmv5t0h == input$party)
      } 
      info_f <- info_f1
      if(input$county!="全部") {
        info_f <- filter(info_f1, formReply$"58jkyyodprf" == input$county)
      } 
      
      
      #str(info_f)
      if(nrow(info_f)>=2){
        
        # setup time series 
        
        info_ts <- xts(info_f$cnt,info_f$createdAt1)
        
        #group by day
        sum_ts <- apply.daily(info_ts,sum)
        if(nrow(sum_ts)>=2){
          plot(sum_ts,main="ElectionSign2022 Daily Records Count",sub="")
        }
      }
    })
    
    output$partyPlot <- renderPlot({
      info_f1 <- info
      if(input$party!="全部") {
        info_f1 <- filter(info, formReply$ee70rmv5t0h == input$party)
      } 
      info_f <- info_f1
      if(input$county!="全部") {
        info_f <- filter(info_f1, formReply$"58jkyyodprf" == input$county)
      } 
      if(nrow(info_f)>=2){
        pie_data <- info_f %>% group_by(formReply$ee70rmv5t0h) %>% summarise(cnt = sum(cnt))
        pie_data$label <- paste(pie_data$value," " , sprintf("%.2f",  pie_data$cnt/sum(pie_data$cnt)*100),"%")
        #pie(pie_data$cnt,labels=pie_data$label,family="黑體-繁 中黑")
        pie(pie_data$cnt,labels=pie_data$label,family="SimSun") #
      }
    })
    output$countyPlot <- renderPlot({
      info_f1 <- info
      if(input$party!="全部") {
        info_f1 <- filter(info, formReply$ee70rmv5t0h == input$party)
      } 
      info_f <- info_f1
      if(input$county!="全部") {
        info_f <- filter(info_f1, formReply$"58jkyyodprf" == input$county)
      } 
      if(nrow(info_f)>=2){
        pie_data <- info_f %>% group_by(formReply$"58jkyyodprf") %>% summarise(cnt = sum(cnt))
        pie_data$label <- paste(pie_data$value," " , sprintf("%.2f",  pie_data$cnt/sum(pie_data$cnt)*100),"%")
        #pie(pie_data$cnt,labels=pie_data$label,family="黑體-繁 中黑")
        pie(pie_data$cnt,labels=pie_data$label,family="SimSun") #
      }
    })
    output$view <- renderTable({
      info_f1 <- info
      if(input$party!="全部") {
        info_f1 <- filter(info, formReply$ee70rmv5t0h == input$party)
      } 
      info_f <- info_f1
      if(input$county!="全部") {
        info_f <- filter(info_f1, formReply$"58jkyyodprf" == input$county)
      } 
      if(nrow(info_f)>=2){
        tbl_data <- info_f %>% group_by(formReply$lfran2q0o) %>% summarise(cnt = sum(cnt))
        names(tbl_data)[names(tbl_data) == 'value'] <- '候選人'
        head(tbl_data[order(tbl_data$cnt,decreasing=T),], n = 100)
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
