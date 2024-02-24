#Скрипт для отримання та очищення статистичних даних Офісу Герального Прокурора


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
 
  Trange=c("A20:N92",
           "A22:P99",
           "A22:P99",
           "A22:P99",
           "A47:P356",
           "A47:P356",
           "A47:P367",
           "A47:P369"),
  Trange2=c("A5:N122",
           "A5:P130",
           "A5:P130",
           "A5:P130",
           "----",
           "----",
           "----",
           "----"),
  TNames=c("TX1 TX2 TX3 FS27X1 FS27X3 FS27X4 FS27X5 FS27X7 FS27X8",
           "TX1 TX2 TX3 FS27X1 FS27X2 FS27X3 FS27X4 FS27X5 FS27X6 FS27X7 FS27X8",
           "TX1 TX2 TX3 FS27X1 FS27X2 FS27X3 FS27X4 FS27X5 FS27X6 FS27X7 FS27X8",
           "TX1 TX2 TX3 FS27X1 FS27X2 FS27X3 FS27X4 FS27X5 FS27X6 FS27X7 FS27X8",
           "TX1 TX2 TX3 FS27X1 FS27X2 FS27X3 FS27X4 FS27X5 FS27X6 FS27X7 FS27X8",
           "TX1 TX2 TX3 FS27X1 FS27X2 FS27X3 FS27X4 FS27X5 FS27X6 FS27X7 FS27X8",
           "TX1 TX2 TX3 FS27X1 FS27X2 FS27X3 FS27X4 FS27X5 FS27X6 FS27X7 FS27X8",
           "TX1 TX2 TX3 FS27X1 FS27X2 FS27X3 FS27X4 FS27X5 FS27X6 FS27X7 FS27X8"
            ),
  colTypes=c(
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric"
    )
)


#Завантаження звітів з офіційного сайту


for (yr in ymin:ymax) 
  
{
  filename<-paste("RAWF2/raw_f2",as.character(yr),".xlsx",sep="")
  if (!file.exists(filename))
  {
    fileURL <-special_pgo$http[special_pgo$Year==yr] 
    curl_download (fileURL, filename)
    
  }
  
  
}
listf2<-list.files(path="RAWF2/")
for (ij in 1:length (listf2))
{ 


colT<-unlist(str_split(special_pgo$colTypes[ij],pattern = " "))
namesc<-unlist(str_split(special_pgo$TNames[ij], pattern = " "))
fileload<-paste0("RAWF2/",listf2[ij])

if (ij<5)
  {raw_f22016_1 <- read_excel(fileload, 
                        sheet = "7.1", 
                        range = special_pgo$Trange[ij], 
                        col_names = namesc, 
                        col_types = colT)

raw_f22016_2 <- read_excel(fileload, 
                         sheet = "7.2", 
                         range = special_pgo$Trange2[ij], 
                         col_names = namesc, 
                         col_types = colT)
raw_f2_2016<-rbind(raw_f22016_1,raw_f22016_2)} else
{raw_f2_2016 <- read_excel(fileload, 
                             sheet = "7", 
                             range = special_pgo$Trange[ij], 
                             col_names = namesc, 
                             col_types = colT)}




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
       if (str_detect(test_raw$TX2[1],"ст\\.")) 
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

if (ij==1){test_extract$FS27X2<-NA
test_extract$FS27X6<-NA
test_extract<-select(test_extract,c(10:14,4,15,5,6,7,16,8,9))
}
if (ij %in% 2:8) {
test_extract<-select(test_extract,c(12:16,4:11))
}


write.csv(test_extract,file=paste0("DATAF2_7/",2015+ij,"-f7.csv"), row.names = FALSE)
if (ij==1) main_f2_7<-test_extract else main_f2_7<-rbind(main_f2_7,test_extract)

}

save (main_f2_7,file = "f2_7.RData")

 
