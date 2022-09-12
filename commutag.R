library("xts")
library(dplyr)
version="0.1.1"
#load from commutag
setwd("/Users/wuulong/github/ElectionSign2022")
if(1){ # 線上json取得，轉換成類似 CSV 格式，轉出 info_sim.csv
  url_str="https://commutag.agawork.tw/dataset/list-image?all=1&dataset=630573b110be1564a1c8d123"
  con <- url(url_str)
  txt <- readLines(con)
  commlist<-jsonlite::fromJSON(txt)
  info_url=commlist$data
  info = info_url

##### 轉出相容 CSV

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
    "看板估計編碼" = info_url$formReply$"wxqdc8fm5k"
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
}else{
  info <- read.csv(file = paste(getwd(),"/info.csv",sep=""))
}
# add cnt=1 column
row_cnt <- nrow(info)
cnt<-seq(from=1,to=1,length.out=row_cnt)
info$cnt = cnt


if(1){ # group_by for 圓餅圖
  tbl_data <- info %>% group_by(大選區,子選區, 政黨) %>% summarise(cnt = sum(cnt))
  tbl_data$選區全名<-paste(tbl_data$大選區,"-" , tbl_data$子選區,sep="")
  pivot_data<-xtabs(cnt ~ 選區全名 + 政黨, tbl_data)
  write.csv(pivot_data,paste(getwd(),"/info_sum.csv",sep=""))
}

if(1){ #村里內有多少看板，人口
  #人口
  population <- read.csv(file =  paste(getwd(),'村里戶數人口數量_10409.csv',sep="/"))
  #"統計年月"      "區域別"        "村里"          "戶數"          "人口數" 
  keeps=c("統計年月","區域別","村里","戶數","人口數")
  population_keeps<-population[keeps]
  population_keeps_nospace<-population_keeps %>% mutate_if(is.character, function(x)gsub('\\s+', '',x)) 
  population_keeps_nospace$fullname <- paste(population_keeps_nospace$區域別 , population_keeps_nospace$村里,sep="")
  
  
  #village 長出全名再join
  library(sf)
  filename="/Users/wuulong/github/ElectionSign2022/2022地方選舉看板/7438-村里界圖(TWD97經緯度)/VILLAGE_MOI_1100415.shp"
  nc <- st_read(filename)
  village<-st_make_valid(nc) 
  village$VILLNAME[is.na(village$VILLNAME)]<-""
  village$fullname <-paste(village$COUNTYNAME,village$TOWNNAME,village$VILLNAME,sep="")
  village$area<-st_area(village)
  #village_pop<-sp::merge(village,population_keeps_nospace,by.x='fullname',by.y='fullname',all.x=T)
  ##village_pop[c('fullname','人口數')]
  ##village_pop[is.na(village_pop$人口數),]
  #village_pop$density<- village_pop$人口數/village_pop$area
  ##village_pop[order(village_pop$density,decreasing = T),]
  
  #library(stringr)
  
  #將 info_sim 讀成地理資料
  filename1=paste(getwd(),'info_sim.csv',sep="/")
  info_sim <- read.csv(file =  filename1)
  info_sim_sf<-st_read(filename1, options=c("X_POSSIBLE_NAMES=lng","Y_POSSIBLE_NAMES=lat"))
  
  #地理join 出數量
  village_4326<-st_transform(village,crs=4326)
  info_sim_sf_4326<-st_set_crs(info_sim_sf,4326)
  village_simple<- st_simplify(village_4326,0.01,dTolerance=0) #100: 快，但數量不精準。0: 慢但精準 ，第二個參數 0.01：我不知道在寫什麼
  system.time({int_raw <- st_intersection(x = village_simple, y = info_sim_sf_4326)}) #0: 要跑約 1 hr, 100: 約 4 分鐘
  write.csv(int_raw,file=paste(getwd(),"/int_raw.csv",sep=""))
  #int_raw2<-read.csv(file=paste(getwd(),"/int_raw.csv",sep=""),row.names = 0) # 讀回來會壞掉
  int_result <- int_raw %>%  group_by(fullname) %>%  count()
  #int_result[order(int_result$n,decreasing = T),c('fullname','n')]
  
  
  # 輸出乾淨的 function
  st_data <- function(SF) { SF[, colnames(SF) != attr(SF, "sf_column"), drop = TRUE]}
  #int_data<-st_data(int_result)
  #setdiff(village$fullname,population_keeps_nospace$fullname)
  
  # 全縣市完成比例
  p_county<-length(unique(int_raw$COUNTYNAME))/length(unique(village_simple$COUNTYNAME))*100
  # 全鄉鎮完成比例
  p_town<-length(unique(paste(int_raw$COUNTYNAME,int_raw$TOWNNAME,sep=""))) / length(unique(paste(village_simple$COUNTYNAME,village_simple$TOWNNAME,sep=""))) *100
  # 全村里完成比例
  p_village<-length(unique(paste(int_raw$COUNTYNAME,int_raw$TOWNNAME,int_raw$VILLNAME,sep="")))/length(unique(paste(village_simple$COUNTYNAME,village_simple$TOWNNAME,village_simple$VILLNAME,sep=""))) *100
  sprintf("縣市完成比例=%.2f%%,鄉鎮完成比例=%.2f%%村里完成比例=%.2f%%,",p_county,p_town,p_village)
  # 村里最高 20 筆
  st_data(head(int_result[order(int_result$n,decreasing = T),],n=20))
  int_area<-int_result[grep('.*臺北市.*',int_result$fullname),]
  st_data(head(int_area[order(int_area$n,decreasing = T),],n=20))
  
  #面人數最低
  int_pop<-sp::merge(int_result,population_keeps_nospace,by.x='fullname',by.y='fullname',all.x=T)
  int_pop$面人數<-int_pop$人口數/int_pop$n
  head(st_data(int_pop[order(int_pop$面人數),]),n=20)
  #int_pop$area<-st_area(int_pop)
  #int_pop$density<- int_pop$人口數/int_pop$area
  #int_pop$面密度<-int_pop$density*int_pop$n
  
  #新竹市東區村里比例
  int_area<-int_result[grep('.*新竹市東區.*',int_result$fullname),]
  village_f<-filter(village,COUNTYNAME=='新竹市',TOWNNAME=='東區')
  length(int_area$fullname)/length(unique(village_f$fullname))*100
}

