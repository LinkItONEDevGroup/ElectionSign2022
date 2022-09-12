#install.packages("xts") 
# README: 紀錄開發相關技巧，一般為測試中
setwd("/Users/wuulong/github/ElectionSign2022")
info <- read.csv(file =  '/Users/wuulong/github/ElectionSign2022/info.csv')

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
info_url=commlist$data
#nrow(info)

##### 轉出相容 CSV
if(0){
  info_sim<- data.frame(
    "imageName" = info_url$"_id",               
    "lat" = info_url$lat,                     
    "lng" = info_url$lng,                    
    "dataTime"= info_url$dataTime,                 
    "remark"  =info_url$remark,                 
    "上傳者"= info_url$uploader,                   
    "標註者" = info_url$annotation$user, 
    "createdAt"= info_url$createdAt,                
    "updatedAt" =  info_url$updatedAt,               
    "候選人姓名（不想寫請填NA）"=info_url$formReply$lfran2q0o,
    "大選區"  = info_url$formReply$"58jkyyodprf",                 
    "政黨"  = info_url$formReply$ee70rmv5t0h,                   
    "子選區"   = info_url$formReply$"2wu96il4btl",               
    "候選人類別（同框選小的）"= info_url$formReply$gpblqqqneri,   
    "多看板中的哪一個" = info_url$formReply$ffa7d7flp3b,        
    "看板類別" = info_url$formReply$qm7irp36j0j,               
    "思源地圖類別" =info_url$formReply$nakaut06nes,            
    "思源地圖名稱" =info_url$formReply$"8xkviczc0zk",            
    "是否有政黨標誌" = info_url$formReply$e5bu0xiz3u6,         
    "主標語" =info_url$formReply$hc7isa5a0oq,                  
    "位置描述資訊" = info_url$formReply$zzphcj2dao,            
    "看板費用類別" = info_url$formReply$"8ejvuuxkqnw"
  ) 
  
  colnames(info_sim) <- c(
    "imageName" ,               
    "lat" ,                     
    "lng" ,                    
    "dataTime",                 
    "remark"  ,                 
    "上傳者",                   
    "標註者" , 
    "createdAt",                
    "updatedAt" ,               
    "候選人姓名（不想寫請填NA）",
    "大選區"  ,                 
    "政黨"  ,                   
    "子選區" ,               
    "候選人類別（同框選小的）",   
    "多看板中的哪一個" ,        
    "看板類別" ,               
    "思源地圖類別" ,            
    "思源地圖名稱" ,            
    "是否有政黨標誌" ,         
    "主標語" ,                  
    "位置描述資訊" ,            
    "看板費用類別" 
  )
  
  write.csv(info_sim,file=paste(getwd(),"/info_sim.csv",sep=""))
}
#####

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

names(info)

# dataset 基本資料
url_str="https://commutag.agawork.tw/dataset/view-dataset?id=630573b110be1564a1c8d123"
con <- url(url_str)
txt <- readLines(con)
commlist<-jsonlite::fromJSON(txt)
info_base=commlist$data

df <- data.frame(quest=info_base$form$itemArr$quest,id=info_base$form$itemArr$id)
df[is.element(df$id, "ee70rmv5t0h"),2]

names(info_url)[names(info_url) == 'verification'] <- '驗證者'
names(info_url)
df <- data.frame("標註者"=info_url$annotation$user,"標籤"=info_url$annotation$annotation)

info_url$formReply[1:3,2:4]
info_url$formReply[[1]]





# 目前上傳者輸出只有 ID, 沒有 name
# 漏了以下
"驗證者" =info_url$verification,
"標籤"  = info_url$annotation$annotation,                  
  "imageUrl"                 
  "cnt" 

  "看板猜測費用總價" 

               思源地圖類別 
               思源地圖名稱 
               是否有政黨標誌
               主標語 
               位置描述資訊 
              看板費用類別
              ,
              看板猜測費用總價 =info_url$formReply$szycu3e5bmk
               )


https://commutag.agawork.tw/user/list-name?id=5f16b3799da4ec1711d623b4

library(readODS)
# 大選區還沒資料清單
county_cur<-unique(info_sim$大選區)
#county_all<-c("桃園市","臺南市","臺中市","新北市","高雄市","臺北市","嘉義市","新竹市","基隆市","澎湖縣","花蓮縣","臺東縣","屏東縣","嘉義縣","雲林縣","南投縣","彰化縣","苗栗縣","新竹縣","宜蘭縣","金門縣","連江縣")
county<-read_ods("2022地方選舉看板.ods",3)
county_need<-setdiff(county$縣市,county_cur)


# 子選區全名還沒資料清單
ref<-read_ods("2022地方選舉看板.ods",5)
ref$子選區全名<-paste(ref$大選區,ref$子選區,sep="-")
info_sim$子選區全名<-paste(info_sim$大選區,info_sim$子選區,sep="-")
#ref_f<-filter(ref, !is.na(ref$"誰（暱稱，可多人）"))
target<-setdiff(unique(ref$子選區全名),unique(info_sim$子選區全名))

sprintf("C:縣市還差 %.2f %%, D:鄉鎮還差 %.2f %%", length(county_need)/nrow(county)*100,length(target)/nrow(ref)*100)

getname <- function(id) {
  url_str=paste("https://commutag.agawork.tw/user/list-name?id=",id,sep="")
  #paste("url:",url,sep="")
  con <- url(url_str)
  txt <- readLines(con)
  commlist<-jsonlite::fromJSON(txt)
  info_name<-commlist$data
}
cat(getname("5f16b3799da4ec1711d623b4")$name)

uploader<-unique(info_sim$上傳者)
uname<-sapply(uploader,getname)

unique(info_sim$上傳者)

village <- read.csv(file = paste(getwd(),"/village_cnt.csv",sep=""))
county1=filter(village, village$COUNTYNAME =='新竹市' )
town1=filter(county1, county1$TOWNNAME=='東區' )
town1_have=filter(town1, town1$NUMPOINTS>0 )


links <- read.csv(file =  '/Users/wuulong/github/ElectionSign2022/links.csv')
s='張文興'
filter(links,candidate==s)
links[grep(s,links$candidate),]


human <- read.csv(file =  '/Users/wuulong/Downloads/舊港島人口 - 整理過.csv')
hts<-ts(human$人數, frequency=1, start=c(0,1))
library("TTR")



#
int_result2 <- int_raw %>%  group_by('townfull'=paste(COUNTYNAME,TOWNNAME,sep="")) %>%  count()
head(st_data(int_result2[order(int_result2$n,decreasing = T),]),n=20)

#simplify geometry, 有用 可以從 510->96
village_simple<- st_simplify(village_4326,0.01,dTolerance=100)
system.time({int_raw2 <- st_intersection(x = village_simple, y = info_sim_sf_4326)})

village_pop<-sp::merge(village_simple,population_keeps_nospace,by.x='fullname',by.y='fullname',all.x=T)

