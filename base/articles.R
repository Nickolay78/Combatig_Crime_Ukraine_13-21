
library (stringr)
library (tidyverse)
library(officer)
crime_doc<-docx_summary(read_docx("kodeks-ukrajini-2341-iii-vid-05_04_2001-kriminalnij-kodeks-ukrajini.docx"))

number_rows<-c(1:length(crime_doc$doc_index))

crime_doc2<-data.frame(number=number_rows,#Кодекс трансформовано у фрейм, де кожна частина або назва - окремий рядок
                       cont=crime_doc$text)

result_art<-data.frame(count=c(1,2),#Створено фрейм для майбутніх результатів - переліку статей та номерів статей
                       text=c("2","2"),
                       short_n=c("1","1"),
                       header=c("d","d"),
                       chapter=c(1,1))
ind<-1
isosob<-FALSE
chapter_count<-0
for (ir in 1:nrow(crime_doc2))
{if (str_detect(crime_doc2[ir,]$cont,"ОСОБЛИВА")) isosob<-TRUE #Якщо в тексті почалася ОСОБЛИВА частина, зафіксували

if (isosob&substr(crime_doc2[ir,]$cont,1,6)=="Стаття") {correction<-0} #Різні види запису статей та скасованих статей
if ((isosob&substr(crime_doc2[ir,]$cont,1,6)=="{Статт")) {correction<-1}#корекція для того, щоб одним циклом описувати змінні

if (
  (isosob&substr(crime_doc2[ir,]$cont,1,6)=="Стаття")|
  ((isosob&substr(crime_doc2[ir,]$cont,1,6)=="{Статт")&(!str_detect(crime_doc2[ir,]$cont,"визнано такою")))#у деяких статтях у фігурних скобках йдеться про визнання статті неконституційною - це для нас неважливо
)
{result_art[ind,]$count<-ind
result_art[ind,]$text<-crime_doc2[ir,]$cont 
if ((substr(crime_doc2[ir,]$cont,11+correction,11+correction)==".")|(substr(crime_doc2[ir,]$cont,11+correction,11+correction)==" ")) 
{result_art[ind,]$short_n<-substr(crime_doc2[ir,]$cont,8+correction,10+correction)# визначаємо номер статті та її назву коли Стаття 123.
if (correction==0) {result_art[ind,]$header<-paste ("Стаття ",result_art[ind,]$short_n,".",
                                                    substr(crime_doc2[ir,]$cont,12+correction,nchar(crime_doc2[ir,]$cont)), sep="")}
else
{result_art[ind,]$header<-paste ("Стаття ",result_art[ind,]$short_n,".",# в тому числі коли йдеться про фігурні дужки
                                 substr(crime_doc2[ir,]$cont,1,nchar(crime_doc2[ir,]$cont)), sep="")}
}
else 
  
{result_art[ind,]$short_n<-paste(substr(crime_doc2[ir,]$cont,8+correction,10+correction),# ця частина для випадків 123-1
                                 "-",substr(crime_doc2[ir,]$cont,11+correction,11+correction),sep = "")
if (correction==0)
{result_art[ind,]$header<-paste ("Стаття ",result_art[ind,]$short_n,".",
                                 substr(crime_doc2[ir,]$cont,13,nchar(crime_doc2[ir,]$cont)), sep="")}
else
{result_art[ind,]$header<-paste ("Стаття ",result_art[ind,]$short_n,".", #тут знову ж таки про фігурні
                                 substr(crime_doc2[ir,]$cont,1,nchar(crime_doc2[ir,]$cont)), sep="")}
}
ind<-ind+1}

}  
articles_ukr<-select(result_art,short_n,header)
new_names<-c("Article","nazva")
colnames(articles_ukr)<-new_names

#Встановлення проблеми повторів через змінені статті та її розв'язання шляхом помітки відповідних рядків та їх подальшого знищення
articles_ukr$test<-0

for (test_i in 2:nrow(articles_ukr))
{if (articles_ukr$Article[test_i]==articles_ukr$Article[test_i-1])
  articles_ukr$test[test_i]<-1
}

articles_ukr<-subset(articles_ukr, test<1)





