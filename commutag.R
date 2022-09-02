library("xts")
library(dplyr)
version="0.1"
#load from commutag

if(1){
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

# group_by for 圓餅圖
tbl_data <- info %>% group_by(大選區,子選區, 政黨) %>% summarise(cnt = sum(cnt))
tbl_data$選區全名<-paste(tbl_data$大選區,"-" , tbl_data$子選區,sep="")
pivot_data<-xtabs(cnt ~ 選區全名 + 政黨, tbl_data)
write.csv(pivot_data,paste(getwd(),"/info_sum.csv",sep=""))


