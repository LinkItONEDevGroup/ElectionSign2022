#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# prepare
library("xts")
version="0.0.4"
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
        selectInput(inputId = "party",
                  label = "選個政黨:",
                  choices = c("全部","民進黨", "國民黨", "民眾黨","時代力量","基進黨","其他","無黨")),
                  #choices = c("全部")),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("partyPlot"),
           tableOutput("view")
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
      print(input$party)
      if(input$party!="全部") {
        info_f <- filter(info, formReply$ee70rmv5t0h == input$party)
      } else {
        info_f <- info
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
      pie_data <- info %>% group_by(formReply$ee70rmv5t0h) %>% summarise(cnt = sum(cnt))
      pie_data$label <- paste(pie_data$value," " , sprintf("%.2f",  pie_data$cnt/sum(pie_data$cnt)*100),"%")
      pie(pie_data$cnt,labels=pie_data$label,family="黑體-繁 中黑")
      
    })
    output$view <- renderTable({
      tbl_data <- info %>% group_by(formReply$lfran2q0o) %>% summarise(cnt = sum(cnt))
      names(tbl_data)[names(tbl_data) == 'value'] <- '候選人'
      head(tbl_data[order(tbl_data$cnt,decreasing=T),], n = 5)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
