#Creation of crime.RData

library (xlsx)
library (stringr)
library(readxl)
library (tidyverse)


forma1<-pgo_raw
forma6<-court_raw
forma6$Chapter<-as.character(forma6$Chapter)
forma7<-main.frame
forma7$Article[forma7$Article=="Article  345-2"|forma7$Article=="Article  345-3"|forma7$Article=="Article  345-4"]<-"Article  345-1"
forma7$Article[(forma7$Article=="Article  255"|forma7$Article=="Article  256"|forma7$Article=="Article  257")&forma7$Chapter=="VIII"]<-"Article  254"
forma6$Article[(forma6$Article=="Article  255"|forma6$Article=="Article  256"|forma6$Article=="Article  257")&forma6$Chapter=="VIII"]<-"Article  254"

write.csv(forma6,"forma6.csv")
write.csv(forma7,"forma7.csv")


commonCourt<-merge (forma6,forma7,by=c("Year","Article","Part","Chapter"))
write.csv(commonCourt,"commonCourt.csv")

commonCourt_P<-commonCourt
commonCourt_P<-summarise (group_by(commonCourt,Year,Article,Chapter),
                          CRTOT=sum(CRTOT),
                          CONVIC=sum(CONVIC),
                          ACQUIT=sum(ACQUIT),
                          CMED=sum(CMED),
                          CCLOSE=sum(CCLOSE),
                          CONFES=sum(CONFES),
                          RECONC=sum(RECONC),
                          CIRCUMS=sum(CIRCUMS),
                          SPONS=sum(SPONS),
                          AMNESTY=sum(AMNESTY),
                          DEATH=sum(DEATH),
                          COTHER=sum(COTHER),
                          PROB=sum(PROB),
                          RELAMN=sum(RELAMN),
                          RELOTHR=sum(RELOTHR),
                          SAMEVERD=sum(SAMEVERD),
                          CNOTCR=sum(CNOTCR),
                          DENOPR=sum(DENOPR),
                          CEDU=sum(CEDU),
                          LIFEIMP=sum(LIFEIMP),
                          IMP=sum(IMP),
                          IMP1=sum(IMP1),
                          IMP12=sum(IMP12),
                          IMP23=sum(IMP23),
                          IMP35=sum(IMP35),
                          IMP510=sum(IMP510),
                          IMP1015=sum(IMP1015),
                          IMP1525=sum(IMP1525),
                          RESTOL=sum(RESTOL),
                          DISBAT=sum(DISBAT),
                          ARREST=sum(ARREST),
                          CORRW=sum(CORRW),
                          SRVRSTR=sum(SRVRSTR),
                          PUBLW=sum(PUBLW),
                          FINE=sum(FINE),
                          DEPR=sum(DEPR),
                          G1=sum(G1),
                          G2=sum(G2),
                          G3=sum(G3),
                          G4=sum(G4),
                          G5=sum(G5),
                          G6=sum(G6),
                          G7=sum(G7),
                          G8=sum(G8),
                          G9=sum(G9),
                          G10=sum(G10),
                          G11=sum(G11),
                          G12=sum(G12),
                          G13=sum(G13),
                          G14=sum(G14),
                          G15=sum(G15),
                          G16=sum(G16),
                          G17=sum(G17),
                          G18=sum(G18),
                          G19=sum(G19),
                          G20=sum(G20),
                          G21=sum(G21),
                          G22=sum(G22),
                          G23=sum(G23),
                          G24=sum(G24),
                          G25=sum(G25),
                          G26=sum(G26),
                          G27=sum(G27),
                          G28=sum(G28),
                          G29=sum(G29),
                          G30=sum(G30),
                          G31=sum(G31),
                          G32=sum(G32),
                          G33=sum(G33),
                          G34=sum(G34),
                          G35=sum(G35),
                          G36=sum(G36),
                          G37=sum(G37),
                          G38=sum(G38),
                          G39=sum(G39),
                          G40=sum(G40),
                          G41=sum(G41),
                          G42=sum(G42),
                          G43=sum(G43),
                          G44=sum(G44),
                          G45=sum(G45),
                          G46=sum(G46),
                          G47=sum(G47),
                          G48=sum(G48),
                          G49=sum(G49),
                          G50=sum(G50),
                          G51=sum(G51),
                          G52=sum(G52),
                          G53=sum(G53),
                          G54=sum(G54),
                          G55=sum(G55),
                          G56=sum(G56),
                          G57=sum(G57),
                          G58=sum(G58),
                          G59=sum(G59),
                          G60=sum(G60),
                          G61=sum(G61),
                          G62=sum(G62),
                          G63=sum(G63),
                          G64=sum(G64),
                          G65=sum(G65),
                          G185=sum(G185),
                          G187=sum(G187)
                         )

forma1$Article<-str_remove(forma1$Article,"Article")
forma1$Article<-str_remove_all(forma1$Article," ")
forma1$Chapter<-as.character(forma1$Chapter)
forma1$Article<-str_remove_all(forma1$Article,"\\*")


commonCourt_P$Article<-str_remove(commonCourt_P$Article,"Article")
commonCourt_P$Article<-str_remove_all(commonCourt_P$Article," ")


commonBase<-merge (forma1,commonCourt_P,by=c("Year","Article","Chapter"),all=TRUE)

write.csv(commonBase,"commonBase.csv",)

commonBase<-commonBase[,1:114]
commonBase$nazva<-"-"

order_of_fields<-c(2,115,1,3:114)
commonBase<-commonBase[,order_of_fields]

order_of_fields<-c(1:26,30:50,27:29,51:115)
commonBase<-commonBase[,order_of_fields]

crimes_eng<-commonBase

articles_eng<-read_delim("article_engl.csv", delim = ";", escape_double = FALSE)

for (i in 1:nrow(crimes_eng))
{
  
  if (crimes_eng$Article[i] %in% articles_eng$short_n) 
    {crimes_eng$nazva[i]<-articles_eng$header[articles_eng$short_n==crimes_eng$Article[i]]}
}
save (crimes_eng,file = "crimes_eng.RData")


crimes_ukr<-commonBase

articles_ukr<-read_delim("article_ukr.csv", delim = ";", escape_double = FALSE)

for (i in 1:nrow(crimes_eng))
{
  
  if (crimes_ukr$Article[i] %in% articles_ukr$short_n) 
  {crimes_ukr$nazva[i]<-articles_ukr$header[articles_ukr$short_n==crimes_ukr$Article[i]]}
}
save (crimes_ukr,file = "crimes_ukr.RData")


