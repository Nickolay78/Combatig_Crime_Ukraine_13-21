#Скрипт для отримання та очищення статистичних даних Офісу Герального Прокурора


library (xlsx)
library (stringr)
library(readxl)
library (curl)


chapter_ord<-c("I","II","III","IV","V","VI","VII","VIII","IX","X",
               "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX")   
ymin<-2016
ymax<-2023


special_pgo<-data.frame (
  Year=ymin:ymax,
  http=c("https://old.gp.gov.ua/ua/stst2011.html?_m=fslib&_t=fsfile&_c=download&file_id=200947",
         "https://old.gp.gov.ua/ua/stst2011.html?_m=fslib&_t=fsfile&_c=download&file_id=203953",
         "https://old.gp.gov.ua/ua/stst2011.html?_m=fslib&_t=fsfile&_c=download&file_id=205798",
         "https://old.gp.gov.ua/ua/stst2011.html?_m=fslib&_t=fsfile&_c=download&file_id=208206",
         "https://old.gp.gov.ua/ua/stst2011.html?_m=fslib&_t=fsfile&_c=download&file_id=210856",
         "https://old.gp.gov.ua/ua/file_downloader.html?_m=fslib&_t=fsfile&_c=download&file_id=215629",
         "https://old.gp.gov.ua/ua/file_downloader.html?_m=fslib&_t=fsfile&_c=download&file_id=225264",
         "https://old.gp.gov.ua/ua/file_downloader.html?_m=fslib&_t=fsfile&_c=download&file_id=241806"
         ),
  Trange=c("A20:AE93",
           "A78:X784",
           "A78:X786",
           "A78:X784",
           "A84:Y786",
           "A84:Y786",
           "A84:Y789",
           "A224:AB1090"),
  Trange2=c("A5:AE128",
           "A78:X784",
           "A78:X786",
           "A78:X784",
           "A84:Y786",
           "A84:Y786",
           "A84:Y789",
           "A224:AB1090"),
  TNames=c("TX1 TX2 TX3 FS1 FS3 FS5 FS6 FS7 FS8 FS9 FS10 FS11 FS12 FS13 FS14 FS15 FS16 FS17 FS18 FS19 FS20 FS21 FS22 FS23 FS24 FS25",
           "TX1 TX2 TX3 FS1 FS2 FS3 FS5 FS6 FS7 FS8 FS9 FS10 FS11 FS12 FS13 FS14 FS15 FS16 FS17 FS18 FS19 FS20 FS21 FS22 FS23 FS24 FS25",
           "TX1 TX2 TX3 FS1 FS2 FS3 FS5 FS6 FS7 FS8 FS9 FS10 FS11 FS12 FS13 FS14 FS15 FS16 FS17 FS18 FS19 FS20 FS21 FS22 FS23 FS24 FS25",
           "TX1 TX2 TX3 FS1 FS2 FS3 FS5 FS6 FS7 FS8 FS9 FS10 FS11 FS12 FS13 FS14 FS15 FS16 FS17 FS18 FS19 FS20 FS21 FS22 FS23 FS24 FS25",
           "TX1 TX2 TX3 FS1 FS2 FS3 FS4 FS5 FS6 FS7 FS8 FS9 FS10 FS11 FS12 FS13 FS14 FS15 FS16 FS17 FS18 FS19 FS20 FS21 FS22 FS23 FS24 FS25",
           "TX1 TX2 TX3 FS1 FS2 FS3 FS4 FS5 FS6 FS7 FS8 FS9 FS10 FS11 FS12 FS13 FS14 FS15 FS16 FS17 FS18 FS19 FS20 FS21 FS22 FS23 FS24 FS25",
           "TX1 TX2 TX3 FS1 FS2 FS3 FS4 FS5 FS6 FS7 FS8 FS9 FS10 FS11 FS12 FS13 FS14 FS15 FS16 FS17 FS18 FS19 FS20 FS21 FS22 FS23 FS24 FS25",
           "TX1 TX2 TX3 FS1 FS2 FS3 FS4 FS5 FS6 FS7 FS8 FS9 FS10 FS11 FS12 FS13 FS14 FS15 FS16 FS17 FS18 FS19 FS20 FS21 FS22 FS23 FS24 FS25"),
  colTypes=c(
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric")
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


colT<-unlist(str_split(special_pgo$colTypes[1],pattern = " "))
namesc<-unlist(str_split(special_pgo$TNames[1], pattern = " "))
fileload<-"RAWF2/raw_f22016.xlsx"

raw_f22016_1 <- read_excel(fileload, 
                        sheet = "1.1", 
                        range = special_pgo$Trange[1], 
                        col_names = namesc, 
                        col_types = colT)

raw_f22016_2 <- read_excel(fileload, 
                         sheet = "1.2", 
                         range = special_pgo$Trange2[1], 
                         col_names = namesc, 
                         col_types = colT)
raw_f2_2016<-rbind(raw_f22016_1,raw_f22016_2)
raw_f2_2016$Year<-2016
 raw_f2_2016$Chapter<-"-"
 raw_f2_2016$Article<-"-"
 raw_f2_2016$IsChp<-FALSE
 raw_f2_2016$TXT1[is.na]
 raw_f2_2016$TX1[is.na(raw_f2_2016$TX1)]<-"++"
 raw_f2_2016$TX2[is.na(raw_f2_2016$TX2)]<-"++"
 raw_f2_2016$TX3[is.na(raw_f2_2016$TX3)]<-"++"
 
 test_fr<-subset(raw_f2_2016, Chapter=="+")
ordch<-0
 #write.xlsx(raw_f2_2016,file="23.xlsx")
 
 for (i in 1:nrow(raw_f2_2016))
 {
   test_raw<-raw_f2_2016[i,]
   if (str_detect(test_raw$TX1[1],"КК")) 
   {test_raw$IsChp[1]<-TRUE
   ordch<-ordch+1
   test_raw$Article[1]<-test_raw$TX1[1]} else
     if (str_detect(test_raw$TX1[1],"ст\\.")) 
     {test_raw$Article[1]<-test_raw$TX1[1]} else
       if (str_detect(test_raw$TX2[1],"ст\\.")) 
       {test_raw$Article[1]<-test_raw$TX2[1]} else
         if (str_detect(test_raw$TX3[1],"ст\\.")) 
         {test_raw$Article[1]<-test_raw$TX3[1]} 
   
   if (!test_raw$Article=="-") {if (!str_detect(test_raw$Article[1],"ст.121 ч2"))
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


}


#test_fr<-select(test_fr,)
 
 