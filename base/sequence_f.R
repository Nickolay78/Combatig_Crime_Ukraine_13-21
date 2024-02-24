# Скрипт визначає послідовність виконання всіх скриптів проєкту

library (beepr)





exlist<-c("f2get-cycle.R",
          "f2-2-get-cycle.R",
          "f2-3-get-cycle.R",
          "f2get-cycle-m.R",
          "f2-2-get-cycle-m.R",
          "f2-3-get-cycle-m.R"
         
)

start<-Sys.time()

for (pow in 1:length(exlist))
{print (paste("IN PROGRESS NUMBER-------------------",pow))
  source(exlist[pow],encoding = "UTF-8",echo = TRUE)
}

beep(10)
print (c(paste("Роботу розпочато: ",start," Роботу закінчено ",Sys.time())))
#write.xlsx (main_f2_1,file = "mainf2-1.xlsx")
#write.xlsx (main_f2_1_m,file = "mainf2-1-m.xlsx")
#write.xlsx (main_f2_2,file = "mainf2-2.xlsx")
#write.xlsx (main_f2_2_m,file = "mainf2-2-m.xlsx")
#write.xlsx (main_f2_3,file = "mainf2-3.xlsx")
load(file = "group_chapter.RData")
load(file = "art_chap.RData")

add_ok<-function(main_f2_1)
  {
  main_f2_1$ChapterTXT<-"---"
  for (ind in 1:nrow(main_f2_1))
{main_f2_1$ChapterTXT[ind]<-chapters$ChapterTXT[chapters$ChapterR==main_f2_1$Chapter[ind]]
  if (!main_f2_1$IsChp[ind]) main_f2_1$Article[ind]<-articles$nazva[articles$Article==main_f2_1$Art[ind]]
  else main_f2_1$Article[ind]<-group_chapter$Article[group_chapter$Chapter==main_f2_1$Chapter[ind]]
  }
return(main_f2_1)
  
}

  t1<-add_ok(main_f2_1 = main_f2_1)
t1_m<-add_ok(main_f2_1 = main_f2_1_m)
t2<-add_ok(main_f2_1 = main_f2_2)
t2_m<-add_ok(main_f2_1 = main_f2_2_m)
t3<-add_ok(main_f2_1 = main_f2_3)
t3_m<-add_ok(main_f2_1 = main_f2_3_m)

t1_tot<-merge(t1, t1_m, by=c("Year", "Article","Chapter","IsChp","Art","ChapterTXT"), all=TRUE)
t2_tot<-merge(t2, t2_m, by=c("Year", "Article","Chapter","IsChp","Art","ChapterTXT"), all=TRUE)
t3_tot<-merge(t3, t3_m, by=c("Year", "Article","Chapter","IsChp","Art","ChapterTXT"), all=TRUE)

f2_tot<-merge(t1_tot,t2_tot,by=c("Year", "Article","Chapter","IsChp","Art","ChapterTXT"), all=TRUE)
f2_tot<-merge(f2_tot,t3_tot,by=c("Year", "Article","Chapter","IsChp","Art","ChapterTXT"))
#write.xlsx (names(f2_tot),"names.xlsx")

save (f2_tot,file = "f2_tot.RData")
load (file = "rik_post.RData")
load (file = "f2_tot.RData")
load (file = "names_f2tot.RData")

for (ik in 1:nrow(rik_post))
f2_tot<-cbind(f2_tot,f2_tot[ ,rik_post$rik[ik]]+f2_tot[ ,rik_post$post[ik]])

names(f2_tot)<-names_f2_tot$field_name
f2_total<-f2_tot
save (f2_total,file="f2_total.RData")
write.xlsx (f2_tot,file="newf2.xlsx")

library (labelled)
var_labels <- setNames(as.list(names_f2_tot$field_descript), names(f2_total))
f2_labelled <- f2_total %>%
  set_variable_labels(.labels = var_labels, .strict = FALSE)
save (f2_labelled,file = "RESULT/f2_label.RData")

#extract chapter data

f2_CHPT<-subset(f2_total,f2_total$IsChp)
f2_crimes<-subset(f2_total,!f2_total$IsChp)
save (f2_CHPT,f2_crimes, file = "RESULT/f2_crimes_CHPT.RData")
