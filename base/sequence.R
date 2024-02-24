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
write.xlsx (main_f2_1,file = "mainf2-1.xlsx")
write.xlsx (main_f2_1_m,file = "mainf2-1-m.xlsx")
write.xlsx (main_f2_2,file = "mainf2-2.xlsx")
write.xlsx (main_f2_2_m,file = "mainf2-2-m.xlsx")
write.xlsx (main_f2_3,file = "mainf2-3.xlsx")
main_f2_1$ChapterTXT<-"---"


for (ind in 1:nrow(main_f2_1))
{main_f2_1$ChapterTXT[ind]<-chapters$ChapterTXT[chapters$ChapterR==main_f2_1$Chapter[ind]]
  if (!main_f2_1$IsChp[ind]) main_f2_1$Article[ind]<-articles$nazva[articles$Article==main_f2_1$Art[ind]]
  else main_f2_1$Article[ind]<-group_chapter$Article[group_chapter$Chapter==main_f2_1$Chapter[ind]]
  }
