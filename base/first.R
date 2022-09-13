# This script defines the sequence of execution of all other project scripts

library (beepr)


exlist<-c("get-proceed-COURT.R",
           "get-proceed-PG0.R",
           "get.R",
           "load2.R",
          "codeC.R",
          "Create-Crimes.R"
           )
                           
start<-Sys.time()
  
for (pow in 1:length(exlist))
    {source(exlist[pow],encoding = "UTF-8",echo = TRUE)}

beep(2)
print (c(paste("Job was started: ",start,"  Job ended ",Sys.time())))
