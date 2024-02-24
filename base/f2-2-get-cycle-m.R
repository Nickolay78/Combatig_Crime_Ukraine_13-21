#Скрипт для отримання та очищення статистичних даних Офісу Герального Прокурора
## Друга таблиця форми 2





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
    "A14:AA33",
    "A15:AA34",
    "A15:AA34",
    "A15:AA34",
    "A44:AB353",
    "A44:AB353",
    "A44:AB364",
    "A44:AB366"),
  Trange2=c(
    "A15:AB34",
    "A16:AD35",
    "A16:AD35",
    "A16:AD35",
    "A44:AG353",
    "A44:AG353",
    "A44:AG364",
    "A44:AG366"),
    TNames=c(
    
     "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20 FS22X21 FS22X22 FS22X23 FS22X24 FS22X27 FS22X28 FS22X29 FS22X32 FS22X33 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44",
     "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20 FS22X21 FS22X22 FS22X23 FS22X24 FS22X27 FS22X28 FS22X29 FS22X30 FS22X31 FS22X32 FS22X33 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44",
     "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20 FS22X21 FS22X22 FS22X23 FS22X24 FS22X27 FS22X28 FS22X29 FS22X30 FS22X31 FS22X32 FS22X33 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44",
     "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20 FS22X21 FS22X22 FS22X23 FS22X24 FS22X27 FS22X28 FS22X29 FS22X30 FS22X31 FS22X32 FS22X33 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44",
     "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X4 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20 FS22X21 FS22X22 FS22X23 FS22X24 FS22X25 FS22X26 FS22X27 FS22X28 FS22X29 FS22X33 FS22X32 FS22X31 FS22X30 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44 FS22X45",
     "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X4 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20 FS22X21 FS22X22 FS22X23 FS22X24 FS22X25 FS22X26 FS22X27 FS22X28 FS22X29 FS22X33 FS22X32 FS22X31 FS22X30 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44 FS22X45",
     "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X4 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20 FS22X21 FS22X22 FS22X23 FS22X24 FS22X25 FS22X26 FS22X27 FS22X28 FS22X29 FS22X33 FS22X32 FS22X31 FS22X30 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44 FS22X45",
     "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X4 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20 FS22X21 FS22X22 FS22X23 FS22X24 FS22X25 FS22X26 FS22X27 FS22X28 FS22X29 FS22X33 FS22X32 FS22X31 FS22X30 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X46 FS22X43 FS22X45 FS22X47"),

  #################TUT PROBLEMA!!!!!
  TNames1=c(
    
    "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20",
    
    "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20",
    "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20",
    "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20",
    
    "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X4 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20",
    "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X4 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20",
    "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X4 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20",
    "TX1 TX2 TX3 FS22X1 FS22X2 FS22X3 FS22X4 FS22X5 FS22X6 FS22X7 FS22X8 FS22X9 FS22X10 FS22X11 FS22X12 FS22X13 FS22X14 FS22X15 FS22X16 FS22X17 FS22X18 FS22X19 FS22X20"),
  
  TNames2=c(
    
    "TX1 TX2 TX3 FS22X21 FS22X22 FS22X23 FS22X24 FS22X27 FS22X28 FS22X29 FS22X32 FS22X33 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44",
    
    "TX1 TX2 TX3 FS22X21 FS22X22 FS22X23 FS22X24 FS22X27 FS22X28 FS22X29 FS22X30 FS22X31 FS22X32 FS22X33 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44",
    "TX1 TX2 TX3 FS22X21 FS22X22 FS22X23 FS22X24 FS22X27 FS22X28 FS22X29 FS22X30 FS22X31 FS22X32 FS22X33 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44",
    "TX1 TX2 TX3 FS22X21 FS22X22 FS22X23 FS22X24 FS22X27 FS22X28 FS22X29 FS22X30 FS22X31 FS22X32 FS22X33 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44",
    
    "TX1 TX2 TX3 FS22X21 FS22X22 FS22X23 FS22X24 FS22X25 FS22X26 FS22X27 FS22X28 FS22X29 FS22X33 FS22X32 FS22X31 FS22X30 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44 FS22X45",
    "TX1 TX2 TX3 FS22X21 FS22X22 FS22X23 FS22X24 FS22X25 FS22X26 FS22X27 FS22X28 FS22X29 FS22X33 FS22X32 FS22X31 FS22X30 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44 FS22X45",
    "TX1 TX2 TX3 FS22X21 FS22X22 FS22X23 FS22X24 FS22X25 FS22X26 FS22X27 FS22X28 FS22X29 FS22X33 FS22X32 FS22X31 FS22X30 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X42 FS22X43 FS22X44 FS22X45",
    "TX1 TX2 TX3 FS22X21 FS22X22 FS22X23 FS22X24 FS22X25 FS22X26 FS22X27 FS22X28 FS22X29 FS22X33 FS22X32 FS22X31 FS22X30 FS22X34 FS22X35 FS22X36 FS22X37 FS22X38 FS22X39 FS22X40 FS22X41 FS22X46 FS22X43 FS22X45 FS22X47"),
  
  
    colTypes=c(
      
      
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    
    
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric"),
  

  colTypes2=c(
    
    
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
 
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    
      
    
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric")
  
  
    
)


#Завантаження звітів з офіційного сайту



listf2<-list.files(path="RAWF2/")
for (ij in 1:length (listf2))

{ 


colT<-unlist(str_split(special_pgo$colTypes[ij],pattern = " "))
colT2<-unlist(str_split(special_pgo$colTypes2[ij],pattern = " "))
namesc<-unlist(str_split(special_pgo$TNames[ij], pattern = " "))
namesc1<-unlist(str_split(special_pgo$TNames1[ij], pattern = " "))
namesc2<-unlist(str_split(special_pgo$TNames2[ij], pattern = " "))

fileload<-paste0("RAWF2/",listf2[ij])

if (ij<9)#Для цього набору єдине правило
  {raw_f22016_1 <- read_excel(fileload, 
                        sheet = "5.1", 
                        range = special_pgo$Trange[ij], 
                        col_names = namesc1, 
                        col_types = colT)


raw_f22016_3 <- read_excel(fileload, 
                            sheet = "5.2", 
                            range = special_pgo$Trange2[ij], 
                            col_names = namesc2, 
                            col_types = colT2)
  
  
  r_f2_1<-raw_f22016_1
  r_f2_2<-raw_f22016_3
 r_f2_1<-subset(r_f2_1,!(is.na(r_f2_1$TX1)&is.na(r_f2_1$TX2)&is.na(r_f2_1$TX3)) )
 r_f2_2<-subset(r_f2_2,!(is.na(r_f2_2$TX1)&is.na(r_f2_2$TX2)&is.na(r_f2_2$TX3)) )

 raw_f2_2016<-cbind(r_f2_1,select(r_f2_2,4:ncol(r_f2_2)))

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

   if (ij>4)
   {if (str_detect(test_raw$TX1[1],"КК")|str_detect(test_raw$TX1[1],"Кримінального кодексу")) 
   {test_raw$IsChp[1]<-TRUE
   ordch<-ordch+1
   test_raw$Article[1]<-test_raw$TX1[1]} else
     if (str_detect(test_raw$TX1[1],"ст\\.")) 
     {test_raw$Article[1]<-test_raw$TX1[1]} else
       if (str_detect(test_raw$TX2[1],"ст\\.")|str_detect(test_raw$TX2[1],"205\\-1")|str_detect(test_raw$TX2[1],"114\\-2")) 
       {test_raw$Article[1]<-test_raw$TX2[1]} else
         if (str_detect(test_raw$TX3[1],"ст\\.")&!str_detect(test_raw$TX3,"\\(")) 
         {test_raw$Article[1]<-test_raw$TX3[1]} 
   } else
     if (str_detect(test_raw$TX2[1],"Злочини")) 
     {test_raw$IsChp[1]<-TRUE
     ordch<-ordch+1
     test_raw$Article[1]<-test_raw$TX2[1]}
   
   
   
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

if (ij==1){
test_extract$FS22X4<-NA
test_extract$FS22X25<-NA
test_extract$FS22X26<-NA
test_extract$FS22X30<-NA
test_extract$FS22X31<-NA
test_extract$FS22X45<-NA
test_extract$FS22X46<-NA
test_extract$FS22X47<-NA

test_extract<-select(test_extract,c(43:47,4:6,FS22X4,7:26,FS22X25,FS22X26,27:29,FS22X30,FS22X31,30:42,FS22X45,FS22X46,FS22X47))
right.names<-names(test_extract)}
if (ij %in% 2:4) {
  test_extract$FS22X4<-NA
  test_extract$FS22X25<-NA
  test_extract$FS22X26<-NA
  test_extract$FS22X45<-NA
  test_extract$FS22X46<-NA
  test_extract$FS22X47<-NA
  test_extract<-select(test_extract,right.names)
}
if (ij %in% 5:7) {
  test_extract$FS22X46<-NA
  test_extract$FS22X47<-NA
  test_extract<-select(test_extract,right.names)
}
if (ij %in% 8) {
  test_extract$FS22X42<-NA
  test_extract$FS22X44<-NA
  test_extract<-select(test_extract,right.names)
}

write.csv(test_extract,file=paste0("DATAF2_2-m/",2015+ij,"-f2-m_2.csv"), row.names = FALSE)
if (ij==1) main_f2_2_m<-test_extract else main_f2_2_m<-rbind(main_f2_2_m,test_extract)

}

#write.xlsx(names(test_extract),"extra2.xlsx")
save (main_f2_2_m,file = "f2_2-m.RData")

 