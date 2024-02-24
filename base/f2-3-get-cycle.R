#Скрипт для отримання та очищення статистичних даних Офісу Герального Прокурора
## Третя таблиця форми 2





library (xlsx)
library (stringr)
library(readxl)
library (curl)
library (tidyverse)


chapter_ord<-c("I","II","III","IV","V","VI","VII","VIII","IX","X",
               "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX")   
ymin<-2016
ymax<-2023


special_pgo<-data.frame (
  Year=ymin:ymax,
  
  Trange=c(
    "A21:Q93",
    "A23:Q102",
    "A25:Q112",
    "A25:Q112",
    "A44:Q353",
    "A44:Q353",
    "A44:Q364",
    "A44:Q366"),
  Trange2=c(
    "A5:Q130",
    "A5:Q135",
    "A5:Q147",
    "A5:Q147",
    "----",
    "----",
    "----",
    "----"),

  TNames=c(
    
    "TX1 TX2 TX3 FS23X1 FS23X2 FS23X3 FS23X4 FS23X5 FS23X6 FS23X7 FS23X8 FS23X9",
    "TX1 TX2 TX3 FS23X1 FS23X2 FS23X3 FS23X4 FS23X5 FS23X6 FS23X7 FS23X8 FS23X9",
    "TX1 TX2 TX3 FS23X1 FS23X2 FS23X3 FS23X4 FS23X5 FS23X6 FS23X7 FS23X8 FS23X9",
    "TX1 TX2 TX3 FS23X1 FS23X2 FS23X3 FS23X4 FS23X5 FS23X6 FS23X7 FS23X8 FS23X9",
    "TX1 TX2 TX3 FS23X1 FS23X2 FS23X3 FS23X4 FS23X5 FS23X6 FS23X7 FS23X8 FS23X9",
    "TX1 TX2 TX3 FS23X1 FS23X2 FS23X3 FS23X4 FS23X5 FS23X6 FS23X7 FS23X8 FS23X9",
    "TX1 TX2 TX3 FS23X1 FS23X2 FS23X3 FS23X4 FS23X5 FS23X6 FS23X7 FS23X8 FS23X9",
    "TX1 TX2 TX3 FS23X1 FS23X2 FS23X3 FS23X4 FS23X5 FS23X6 FS23X7 FS23X8 FS23X9"),
  
 
  
  colTypes=c(
    
    
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric",    
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric",    
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric",    
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric")
 )


#Завантаження звітів з офіційного сайту



listf2<-list.files(path="RAWF2/")
for (ij in 1:length (listf2))
  
{ 
  
  
  colT<-unlist(str_split(special_pgo$colTypes[ij],pattern = " "))
  namesc<-unlist(str_split(special_pgo$TNames[ij], pattern = " "))
  
  fileload<-paste0("RAWF2/",listf2[ij])
  
  if (ij<5)
  {raw_f22016_1 <- read_excel(fileload, 
                              sheet = "3.1", 
                              range = special_pgo$Trange[ij], 
                              col_names = namesc, 
                              col_types = colT)
  
  raw_f22016_2 <- read_excel(fileload, 
                             sheet = "3.2", 
                             range = special_pgo$Trange2[ij], 
                             col_names = namesc, 
                             col_types = colT)
  
  raw_f2_2016<-rbind(raw_f22016_1,raw_f22016_2)

  } 
  if (ij %in% 5:8)
  {
   
    raw_f2_2016 <- read_excel(fileload, 
                               sheet = "3", 
                               range = special_pgo$Trange[ij], 
                               col_names = namesc, 
                               col_types = colT)
    
  
  }
  
  
  
  
  
  
  raw_f2_2016$Year<-2015+ij
  raw_f2_2016$Chapter<-"-"
  raw_f2_2016$IsChp<-FALSE
  raw_f2_2016$Article<-"-"
  
  raw_f2_2016$TX1[is.na(raw_f2_2016$TX1)]<-"++"
  raw_f2_2016$TX2[is.na(raw_f2_2016$TX2)]<-"++"
  raw_f2_2016$TX3[is.na(raw_f2_2016$TX3)]<-"++"
  
  test_fr<-subset(raw_f2_2016, Chapter=="+")
  ordch<-0
  
  
  for (i in 1:nrow(raw_f2_2016))
  {
    test_raw<-raw_f2_2016[i,]
    if (str_detect(test_raw$TX1[1],"КК")|str_detect(test_raw$TX1[1],"Кримінального кодексу")) 
    {test_raw$IsChp[1]<-TRUE
    ordch<-ordch+1
    test_raw$Article[1]<-test_raw$TX1[1]} else
      if (str_detect(test_raw$TX1[1],"ст\\.")) 
      {test_raw$Article[1]<-test_raw$TX1[1]} else
        if (str_detect(test_raw$TX2[1],"ст\\.")|str_detect(test_raw$TX2[1],"205\\-1")) 
        {test_raw$Article[1]<-test_raw$TX2[1]} else
          if (str_detect(test_raw$TX3[1],"ст\\.")&!str_detect(test_raw$TX3,"\\(")) 
          {test_raw$Article[1]<-test_raw$TX3[1]} 
    
    if (!test_raw$Article=="-") {if (!(str_detect(test_raw$Article[1],"ст.121 ч2")|str_detect(test_raw$Article[1],"ч\\.")))
    {test_raw$Chapter[1]<-chapter_ord[ordch]
    test_fr<-rbind(test_fr,test_raw)}
    }  
  }
  
  test_extract<-test_fr
  test_extract$Art<-"---"
  for (jj in 1:nrow(test_extract))
  {if (str_detect(test_extract$Article[jj],'\\d+\\-\\d+')) 
    test_extract$Art[jj]<-str_extract(test_extract$Article[jj],'\\d+\\-\\d+')else
      test_extract$Art[jj]<-str_extract(test_extract$Article[jj],'\\d+')}
 
  test_extract<-select(test_extract,13:17,4:12)
  
  write.csv(test_extract,file=paste0("DATAF2_3/",2015+ij,"-f2_3.csv"), row.names = FALSE)
  if (ij==1) main_f2_3<-test_extract else main_f2_3<-rbind(main_f2_3,test_extract)
  
}


save (main_f2_3,file = "f2_3.RData")

