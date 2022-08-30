
# 
#install.packages("xts")                      
library("xts")
version="0.0.4"
#load from commutag
info <- read.csv(file = '/Users/wuulong/github/ElectionSign2022/info.csv')

# add cnt=1 column
row_cnt <- nrow(info)
cnt<-seq(from=1,to=1,length.out=row_cnt)
info$cnt = cnt

# convert string to numeric format
info$createdAt1 = as.POSIXct(info$createdAt)

# setup time series 
info_ts <- xts(info$cnt,info$createdAt1)

#group by day
sum_ts <- apply.daily(info_ts,sum)

#plot
plot(sum_ts)

url_str="https://commutag.agawork.tw/dataset/list-image?all=1&dataset=630573b110be1564a1c8d123"
con <- url(url_str)
txt <- readLines(con)
commlist<-jsonlite::fromJSON(txt)
info=commlist$data
nrow(info)


pie_data <- info %>% group_by(formReply$"ee70rmv5t0h") %>% summarise(cnt = sum(cnt))
pie_data$label <- paste(pie_data$value," " , sprintf("%.2f",  pie_data$cnt/sum(pie_data$cnt)*100),"%")
pie(pie_data$cnt,labels=pie_data$label,family="宋體-繁") 
#黑體-繁 中黑, Times 粗斜體,新細明體 Regular,標楷體 標準體, 宋體-繁 細體
# testing
f2=filter(info, formReply$ee70rmv5t0h =='國民黨')

s = "民進黨"
grep(s,info$formReply$ee70rmv5t0h,fixed=TRUE,useBytes=TRUE)

pie_data$label <- paste(pie_data$value," " ,  pie_data$cnt/sum(pie_data$cnt)*100," %")
pie_data$label <- paste(pie_data$value," " , sprintf("%.2f",  pie_data$cnt/sum(pie_data$cnt)*100),"%")

tbl_data <- info %>% group_by(formReply$lfran2q0o) %>% summarise(cnt = sum(cnt))
names(tbl_data)[names(tbl_data) == 'value'] <- '候選人'
head(tbl_data[order(tbl_data$cnt,decreasing=T),], n = 5)

# group_by for 圓餅圖
tbl_data <- info %>% group_by(大選區,子選區, 政黨) %>% summarise(cnt = sum(cnt))
tbl_data$選區全名<-paste(tbl_data$大選區,"-" , tbl_data$子選區,sep="")
pivot_data<-xtabs(cnt ~ 選區全名 + 政黨, tbl_data)
write.csv(pivot_data,paste(getwd(),"/info_sum.csv",sep=""))

#setwd("/Users/wuulong/github/ElectionSign2022")

