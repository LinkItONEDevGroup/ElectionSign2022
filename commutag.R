library("xts")
version="0.0.4"
#load from commutag
info <- read.csv(file = paste(getwd(),"/info.csv",sep=""))

# add cnt=1 column
row_cnt <- nrow(info)
cnt<-seq(from=1,to=1,length.out=row_cnt)
info$cnt = cnt

# group_by for 圓餅圖
tbl_data <- info %>% group_by(大選區,子選區, 政黨) %>% summarise(cnt = sum(cnt))
tbl_data$選區全名<-paste(tbl_data$大選區,"-" , tbl_data$子選區,sep="")
pivot_data<-xtabs(cnt ~ 選區全名 + 政黨, tbl_data)
write.csv(pivot_data,paste(getwd(),"/info_sum.csv",sep=""))


