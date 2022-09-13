library(tidyverse)
library (markdown)
library(shiny)
library(readr)
library(shinythemes)
library(DT)

chapter<-read_delim("chapter.csv", delim = ";", escape_double = FALSE)
rule<-read_delim("rule.csv", delim = ";", escape_double = FALSE)
  articles<-read_delim("article.csv", delim = ";", escape_double = FALSE)
load ("crimes_eng.RData")
crimes<-crimes_eng
name_n<-names(crimes)[5:115]  
crimes$Chapter<-as.numeric(as.roman(crimes$Chapter))
crimes[is.na(crimes)]=0
crimes$ChapterTXT<-1
for (j in 1:nrow(chapter))
  for (i in 1:nrow(crimes))
    if (crimes$Chapter[i]==chapter$chn[j]) crimes$ChapterTXT[i]<-chapter$chap[j]
bcrimes<-select(crimes,1:4,ChapterTXT,5:115)
crimes<-bcrimes
crimes$ChapterR<-as.character(as.roman(as.numeric(crimes$Chapter)))
field_n<-read_delim("field_n.csv", delim = ";", escape_double = FALSE)
chapter_label <- c("I","II","III","IV","V","VI","VII","VIII","IX","X",
                   "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX")
require(scales)
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)
ui <- 
  navbarPage ("Combating crime in Ukraine: infographic",
    
                  tabPanel( "About the manual", 
                           
                             navlistPanel(
                              tabPanel("Why should you pay attention?",
 h3("Interactive manual 'Combating crime in Ukraine: infographics'"),                                      
 hr(),
 tags$ul(
   tags$li(h4("the observation period covers ", strong("nine years (2013-2021)"))),
   tags$li(h4(strong("more than 100 parameters "),"of the analysis of the criminal legislation application", )), 
  tags$li(h4("a data set that includes ", strong ("about 980,000 "),
  " indicators of the functioning of the national criminal justice system")), 
  tags$li(h4("the mode of ", strong("creating own visualizations and data sets,"),
  " the ability to combine parameters, determine the observation period, 
  the level of generalization, and the need to calculate relative values")),
  tags$li(h4("one of the first attempts to systematize statistical
data on combating crime in the format of a", 
strong("reproducible study using the Data Science methodology.")))
  ),
 hr(),
 fluidRow(column(12,align="center",
                 HTML ('<iframe width="560" height="315" src="https://www.youtube.com/embed/Rxh0rq9sNnk" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')))
 ),
  
                              tabPanel("How the data was obtained and the visualizations were created",
h4("The manual systematizes the annual statistical reports of the Office of the General Prosecutor of Ukraine and the State Judicial Administration of Ukraine"), 
 hr(),  
tags$ul(
   tags$li(h5("A single report on criminal offences. Form No. 1.")),
   tags$li(h5("Report on persons brought to criminal liability and types of criminal punishment. Form No. 6")),
   tags$li(h5("Report on the composition of the convicts. Form No. 7."))
      ),
   hr(),
h4("The data collection was carried out by downloading the reports presented on the official Internet representations of the OGP and DSA. Links to the reports have been added to the corresponding software scripts. Running scripts allows you to get local copies of the input data"),
hr(),
h4("Data cleaning scripts were developed based on the analysis of the features of the structure of incoming reports. The received reports were transformed into data suitable for automated processing ('clean' data)"),
hr(),
h4("With the help of developed automated processing scripts, selections and generalizations for further visualizations were created from 'clean' data"),
hr(),

hr()
   
   ),
tabPanel ("Reproducible research using Data Science methodology",
          
h4("Statistical data make it possible to get an idea of the patterns and trends 
   in combating crime. Their analysis is an important component of 
   the decision-making process in the field of criminal law regulation. 
   At the same time, one of the urgent problems of contemporary social 
   science is the crisis of replication. The results of many studies cannot be obtained in a way to repeat them, it is difficult to verify. This calls into question the content and scope of knowledge about society."),
hr(),
h4("The main consequence of the replication crisis is the loss of trust in science as such. The impossibility of reproducing the research lowers the quality of scientific arguments in the public discourse, calls into question the expediency of their use."),
hr(),
h4("However, not everything is so sad 😊. Updating the methodology of modern research creates opportunities to overcome the crisis of replication. Scientific intelligence has a qualitatively new level of trust, where it is methodologically possible to reproduce the results. Such research is called reproducible research."),
hr(),
h4("This manual is the result of working with crime statistics data using the Data Science methodology and is ",
strong("fully reproducible."), "Researchers can repeat the process of loading input data from official Internet sources, reproduce the transformation of this data into a form that enables their automated processing, and also check the correctness of data processing and reproduce the visualizations that are given in the work."),
hr(),
h4("The repository with incoming reports and cleaned data, scripts developed by us for data collection, cleaning, processing is 
   ", tags$a("freely available", href= "https://github.com/Nickolay78/Combating-Crime-in-Ukraine-2013-2021"))
        
          ),

 
 tabPanel ("Work in constuctor mode",
h4("The ",strong("'Visualization Constructor'"), "link allows you to create your own visualizations"),
hr(),
h4("The visualization is constructed by defining the indicators for the research, the time of observations and the level of generalization. By default, the constructor is configured to display one indicator - The number of recorded proceedings - for the entire observation period (without distribution by year), for all articles of the Special Part of the Criminal Code (without distribution by sections or articles)."),
hr(),
h4("The choice of indicators for research, Sections and articles of the Special Part of the CC is ",
   strong("multiple."),"The user of the manual independently determines the number of indicators, sections or articles for visualization."),
hr(),
h4("It is possible to choose from a list or by entering the desired parameter (in this case, the manual will form a list for choosing from the parameters corresponding to the entered symbols)"),
fluidRow(column (6,align="center", img(src="list.png",height=200),
                 p(em("choice from the list"),style="text-align:center")),
         column (6,align="center", img (src="clist.png",height=200),
                 p(em("choice by entering characters")))
  ),#End of Row
hr(),
h4("If it is necessary to cancel the selected options of indicators, sections or articles, highlight the selected option with the left mouse button and press ", strong("Delete")),
hr(),
h4("If you need to build a visualization with relative values, you must specify a base variable. For selection, the manual will offer a list of selected research indicators. In the relative value visualization, the base variable will have a value of 100%"),
hr(),
h4("After defining the parameters in the ", strong ("'Constructor Data Visualization'"),
   "link, you will find a set of generated data in the form of tables with controls"),
hr(),
h3 ("Parameters for research, on the basis of which you can build a visualization"),
fluidRow(column(4,
                
                p("Criminal offenses have been recorded"),
                p("Criminal offenses in which persons have been presented a notice of suspicion"),
                p("Proceedings were sued to the court with an indictment"),
                p("The proceedings were sued to the court with a petition for condoning from criminal liability"),
                p("The proceedings were sued to the court with a petition for the application of coercive measures of a medical nature"),
                p("The proceedings were sued to the court with a petition to apply coercive measures of an educational nature"),
                p("Completed proceedings regarding offenses committed by persons who previously committed criminal offenses"),
                p("Completed proceedings regarding offenses committed by a group of persons"),
                p("Completed proceedings regarding offenses committed by persons under the state of physical drunkenness"),
                p("Completed proceedings regarding offenses committed by or involving minors"),
                p("Persons whose verdicts (resolutions) have entered into a legal force"),
                p("Convicted persons"),
                p("Acquitted persons"),
                p("Non-convicts to whom coercive measures of a medical nature have been applied"),
                p("Cases have been closed ( in all)"),
                p("Cases have been closed due to sincere remorse"),
                p("The cases have been closed due to reconciliation between the perpetrator and the victim"),
                p("Cases have been closed because of the change of the situation"),
                p("Cases have been closed due to bail of a person "),
                p("Cases have been closed due to amnesty"),
                p("Cases have been closed because of death"),
                p("Cases have been closed for other reasons"),
                p("Cases have been closed because there is a verdict or a decision to close criminal proceedings on the same charge"),
                p("Cases have been closed due to the lack of event, composition of the crime or the lack of the evidence of accusation"),
                p("Cases have been closed in connection with the refusal of the prosecutor or the victim, his representative from prosecution in criminal proceedings"),
                p("Cases have been closed in connection with the use of coercive measures of an educational nature against a minor"),
                p("Sentenced to life imprisonment"),
                p("Imprisonment for a certain period of time was imposed"),
                p("Imprisonment for 1 year"),
                p("Imprisonment for a period of more than 1 year to 2 years inclusive"),
                p("Imprisonment for a period of more than 2 years to 3 years inclusive"),
                p("Imprisonment for a period of more than 3 years to 5 years inclusive"),
                p("Imprisonment for a period of more than 5 years to 10 years inclusive"),
                p("Imprisonment for a period of more than 10 years to 15 years inclusive"),
                p("Imprisonment for a period of more than 15 years to 25 years inclusive"),
                p("Limitation of imprisonment has been assigned"),
                p("Detention in the disciplinary battalion has been assigned"),
                
                
                ),#firstC
         column(4,
                
                p("An arrest has been assigned"),
                p("Remedial works have been assigned"),
                p("A service restriction has been assigned to the military personnel"),
                p("Public works have been assigned"),
                p("A fine has been imposed"),
                p("Deprivation of the right to hold certain positions or engage in certain activities has been assigned"),
                p("Released from punishment on probation"),
                p("Released from punishment as a result of the amnesty act  "),
                p("Released from punishment from other reasons"),
                p("Total  convicted persons"),
                p("The citizens of Ukraine have been convicted"),
                p("The citizens of other state have been convicted"),
                p("Women have been convicted"),
                p("Convicted for offences committed by a group of people"),
                p("Convicted for offences committed by an organized group"),
                p("Convicted for offences committed by a criminal organization"),
                p("Convicted for offences committed under the state of alcohol intoxication"),
                p("Convicted for offences committed under the state of drug intoxication"),
                p("Convicted for offences committed at the age from 14 to 16"),
                p("Convicted for offences committed at the age from 16 to 18"),
                p("Convicted for offences committed at the age from 18 to 25"),
                p("Convicted for offences committed at the age from 25 to 30"),
                p("Convicted for offences committed at the age from 30 to 50"),
                p("Convicted for offences committed at the age from 50 to 65"),
                p("Convicted for offences committed at the age from 65 and older"),
                p("Employment of the convicted at the moment of committing offences - workers"),
                p("Employment of the convicted at the moment of committing offences – state officials "),
                p("Employment of the convicted at the moment of committing offences – other employees"),
                p("Employment of the convicted at the moment of committing offences - servicemen"),
                p("Employment of the convicted at the moment of committing offences - doctors, pharmacists"),
                p("Employment of the convicted at the moment of committing offences – teachers, lecturers"),
                p("Employment of the convicted at the moment of committing offences – mass media workers"),
                p("Employment of the convicted at the moment of committing offences – private entrepreneurs"),
                p("Employment of the convicted at the moment of committing offences - workers of business associations"),
                p("Employment of the convicted at the moment of committing offences – students of schools, lyceums, colleges, gymnasia"),
                p("Employment of the convicted at the moment of committing offences – students of educational institutions"),
                p("Employment of the convicted at the moment of committing offences - other occupations"),
                p("Employment of the convicted at the moment of committing offences - pensioners including invalids")
                
                ),#secondC
         column(4,
                p("Employment of the convicted at the moment of committing offences - unemployed"),
                p("Employment of the convicted at the moment of committing offences – employable people who did not work and did not study"),
                p("Convicted employable people who did not work and did not study and did not have eradicated and previous convictions ( from gr.31)"),
                p("Employment of the convicted at the moment of committing offences – minors to 16 who did not work and did not study"),
                p("Employment of the convicted at the moment of committing offences – detained in the agency of the execution of punishment, under a guard"),
                p("Education at the time of committing offences – complete higher"),
                p("Education at the time of committing offences – basic higher"),
                p("Education at the time of committing offences – vocational "),
                p("Education at the time of committing offences – complete general secondary"),
                p("Education at the time of committing offences – basic general secondary"),
                p("Education at the time of committing offences – primary general"),
                p("Education at the time of committing offences – uneducated"),
                p("Convicted people who committed crime before but were exempt from criminal liability"),
                p("Convicted people who were sued but were acknowledged such who do not have a criminal record"),
                p("Convicted people who were sued but their criminal record was removed or cancelled"),
                p("Convicted people who do not have removed or cancelled criminal record"),
                p("Convicted people who have one criminal record"),
                p("Convicted people who have two criminal records"),
                p("Convicted people who have three or more  criminal records"),
                p("Convicted people who have a criminal record for offences against person’s life or health "),
                p("Convicted people who have a criminal record for offences against sexual freedom and sexual inviolability of a person"),
                p("Convicted people who have a criminal record for offences against property"),
                p("Convicted people who have a criminal record for offences against safety of motion and transport operation"),
                p("Convicted people who have a criminal record for offences against public order, morality"),
                p("Convicted people who have a criminal record for offences related to the drugs"),
                p("Convicted people who have a criminal record for other criminal offences"),
                p("Convicted people who have a criminal record and served sentence fully "),
                p("Convicted people who have a criminal record and were exempt from punishment (total)"),
                p("Convicted people who have a criminal record and were exempt from punishment on parole"),
                p("Convicted people who have a criminal record and were exempt from punishment due to amnesty"),
                p("Convicted people who have a criminal record and were exempt from punishment from other reasons"),
                p("Convicted people who have a criminal record and committed a crime not serving their sentence (total)"),
                p("Convicted people who have a criminal record and committed a crime not serving their sentence  as imprisonment or limitation of imprisonment or arrest"),
                p("Convicted people who have a criminal record and committed a crime in places of detention or limitation of imprisonment or arrest"),
                p("Convicted people who have a criminal record and committed a crime during the probation period"),
                p("Convicted people who have a criminal record and committed a crime during the period of serving other sentence")
                
                
               
                
                )#thirdC
  
)#End of Row

            ),#End of Construct.me                
tabPanel("Author and developer",
         fluidPage(column (6, align="center",
                           img(src="km.png",height=500)),
                   column (6,
                           h3 ("Professor"),
                           h3 (strong("Mykola Karchevskyi")),
                           h4 (a("karchevskiy.org",href="https://karchevskiy.org/")))),
         hr()
         
           
         
         )#End of Author
                     )
                              
                  ),
              

    tabPanel("Constructor of visualizations",
             
             sidebarLayout(
               sidebarPanel(
               
                 selectInput(inputId = "fields",
                             label = "Select indicators for research:",
                             choices = c(
                               
                               "Criminal offenses have been recorded"="ACC",
                               "Criminal offenses in which persons have been presented a notice of suspicion"="SUSP",
                               "Proceedings were sued to the court with an indictment"="INDICM",
                               "The proceedings were sued to the court with a petition for condoning from criminal liability"="REL",
                               "The proceedings were sued to the court with a petition for the application of coercive measures of a medical nature"="MED",
                               "The proceedings were sued to the court with a petition to apply coercive measures of an educational nature"="EDU",
                               "Completed proceedings regarding offenses committed by persons who previously committed criminal offenses"="RECID",
                               "Completed proceedings regarding offenses committed by a group of persons"="GROUP",
                               "Completed proceedings regarding offenses committed by persons under the state of physical drunkenness"="INTOX",
                               "Completed proceedings regarding offenses committed by or involving minors"="JUVEN",
                               "Persons whose verdicts (resolutions) have entered into a legal force"="CRTOT",
                               "Convicted persons"="CONVIC",
                               "Acquitted persons"="ACQUIT",
                               "Non-convicts to whom coercive measures of a medical nature have been applied"="CMED",
                               "Cases have been closed ( in all)"="CCLOSE",
                               "Cases have been closed due to sincere remorse"="CONFES",
                               "The cases have been closed due to reconciliation between the perpetrator and the victim"="RECONC",
                               "Cases have been closed because of the change of the situation"="CIRCUMS",
                               "Cases have been closed due to bail of a person "="SPONS",
                               "Cases have been closed due to amnesty"="AMNESTY",
                               "Cases have been closed because of death"="DEATH",
                               "Cases have been closed for other reasons"="COTHER",
                               "Cases have been closed because there is a verdict or a decision to close criminal proceedings on the same charge"="SAMEVERD",
                               "Cases have been closed due to the lack of event, composition of the crime or the lack of the evidence of accusation"="CNOTCR",
                               "Cases have been closed in connection with the refusal of the prosecutor or the victim, his representative from prosecution in criminal proceedings"="DENOPR",
                               "Cases have been closed in connection with the use of coercive measures of an educational nature against a minor"="CEDU",
                               "Sentenced to life imprisonment"="LIFEIMP",
                               "Imprisonment for a certain period of time was imposed"="IMP",
                               "Imprisonment for 1 year"="IMP1",
                               "Imprisonment for a period of more than 1 year to 2 years inclusive"="IMP12",
                               "Imprisonment for a period of more than 2 years to 3 years inclusive"="IMP23",
                               "Imprisonment for a period of more than 3 years to 5 years inclusive"="IMP35",
                               "Imprisonment for a period of more than 5 years to 10 years inclusive"="IMP510",
                               "Imprisonment for a period of more than 10 years to 15 years inclusive"="IMP1015",
                               "Imprisonment for a period of more than 15 years to 25 years inclusive"="IMP1525",
                               "Limitation of imprisonment has been assigned"="RESTOL",
                               "Detention in the disciplinary battalion has been assigned"="DISBAT",
                               "An arrest has been assigned"="ARREST",
                               "Remedial works have been assigned"="CORRW",
                               "A service restriction has been assigned to the military personnel"="SRVRSTR",
                               "Public works have been assigned"="PUBLW",
                               "A fine has been imposed"="FINE",
                               "Deprivation of the right to hold certain positions or engage in certain activities has been assigned"="DEPR",
                               "Released from punishment on probation"="PROB",
                               "Released from punishment as a result of the amnesty act  "="RELAMN",
                               "Released from punishment from other reasons"="RELOTHR",
                               "The citizens of Ukraine have been convicted"="G2",
                               "The citizens of other state have been convicted"="G3",
                               "Women have been convicted"="G4",
                               "Convicted for offences committed by a group of people"="G5",
                               "Convicted for offences committed by an organized group"="G6",
                               "Convicted for offences committed by a criminal organization"="G7",
                               "Convicted for offences committed under the state of alcohol intoxication"="G8",
                               "Convicted for offences committed under the state of drug intoxication"="G9",
                               "Convicted for offences committed at the age from 14 to 16"="G10",
                               "Convicted for offences committed at the age from 16 to 18"="G11",
                               "Convicted for offences committed at the age from 18 to 25"="G12",
                               "Convicted for offences committed at the age from 25 to 30"="G13",
                               "Convicted for offences committed at the age from 30 to 50"="G14",
                               "Convicted for offences committed at the age from 50 to 65"="G15",
                               "Convicted for offences committed at the age from 65 and older"="G16",
                               "Employment of the convicted at the moment of committing offences - workers"="G17",
                               "Employment of the convicted at the moment of committing offences – state officials "="G18",
                               "Employment of the convicted at the moment of committing offences – other employees"="G19",
                               "Employment of the convicted at the moment of committing offences - servicemen"="G20",
                               "Employment of the convicted at the moment of committing offences - doctors, pharmacists"="G21",
                               "Employment of the convicted at the moment of committing offences – teachers, lecturers"="G22",
                               "Employment of the convicted at the moment of committing offences – mass media workers"="G23",
                               "Employment of the convicted at the moment of committing offences – private entrepreneurs "="G24",
                               "Employment of the convicted at the moment of committing offences - workers of business associations"="G25",
                               "Employment of the convicted at the moment of committing offences – students of schools, lyceums, colleges, gymnasia"="G26",
                               "Employment of the convicted at the moment of committing offences – students of educational institutions"="G27",
                               "Employment of the convicted at the moment of committing offences - other occupations"="G28",
                               "Employment of the convicted at the moment of committing offences - pensioners including invalids"="G29",
                               "Employment of the convicted at the moment of committing offences - unemployed"="G30",
                               "Employment of the convicted at the moment of committing offences – employable people who did not work and did not study"="G31",
                               "Convicted employable people who did not work and did not study and did not have eradicated and previous convictions ( from gr.31)"="G32",
                               "Employment of the convicted at the moment of committing offences – minors to 16 who did not work and did not study"="G33",
                               "Employment of the convicted at the moment of committing offences – detained in the agency of the execution of punishment, under a guard"="G34",
                               "Education at the time of committing offences – complete higher"="G35",
                               "Education at the time of committing offences – basic higher"="G36",
                               "Education at the time of committing offences – vocational "="G37",
                               "Education at the time of committing offences – complete general secondary"="G38",
                               "Education at the time of committing offences – basic general secondary"="G39",
                               "Education at the time of committing offences – primary general"="G40",
                               "Education at the time of committing offences – uneducated"="G41",
                               "Convicted people who committed crime before but were exempt from criminal liability"="G42",
                               "Convicted people who were sued but were acknowledged such who do not have a criminal record"="G43",
                               "Convicted people who were sued but their criminal record was removed or cancelled"="G44",
                               "Convicted people who do not have removed or cancelled criminal record"="G45",
                               "Convicted people who have one criminal record"="G46",
                               "Convicted people who have two criminal records"="G47",
                               "Convicted people who have three or more  criminal records"="G48",
                               "Convicted people who have a criminal record for offences against person’s life or health "="G49",
                               "Convicted people who have a criminal record for offences against sexual freedom and sexual inviolability of a person"="G50",
                               "Convicted people who have a criminal record for offences against property"="G51",
                               "Convicted people who have a criminal record for offences against safety of motion and transport operation"="G52",
                               "Convicted people who have a criminal record for offences against public order, morality"="G53",
                               "Convicted people who have a criminal record for offences related to the drugs"="G54",
                               "Convicted people who have a criminal record for other criminal offences"="G55",
                               "Convicted people who have a criminal record and served sentence fully "="G56",
                               "Convicted people who have a criminal record and were exempt from punishment (total)"="G57",
                               "Convicted people who have a criminal record and were exempt from punishment on parole"="G58",
                               "Convicted people who have a criminal record and were exempt from punishment due to amnesty"="G59",
                               "Convicted people who have a criminal record and were exempt from punishment from other reasons"="G60",
                               "Convicted people who have a criminal record and committed a crime not serving their sentence (total)"="G61",
                               "Convicted people who have a criminal record and committed a crime not serving their sentence  as imprisonment or limitation of imprisonment or arrest"="G62",
                               "Convicted people who have a criminal record and committed a crime in places of detention or limitation of imprisonment or arrest"="G63",
                               "Convicted people who have a criminal record and committed a crime during the probation period"="G64",
                               "Convicted people who have a criminal record and committed a crime during the period of serving other sentence"="G65"
                                         ),
                             selected = "ACC",
                             selectize = TRUE,
                             multiple = TRUE),
                 
               
                 checkboxInput("labels", "Add value captions"),
                 
                 checkboxInput("DefYear","Choose a time period"),
                 conditionalPanel(condition = "input.DefYear",           
                                  sliderInput("Rik",
                                              "Years:",
                                              min = 2013,
                                              max = 2021,
                                              value = c(2013,2021))),
                 
                 checkboxInput("DefChap","Choose the Section of the Special Part of Criminal Code"),
                 conditionalPanel(condition = "input.DefChap==true",
                                  uiOutput("DefChapOut"),
                                  fluidRow(actionButton ("all_CH","Choose all sections",width = "49.5%"),
                                           actionButton ("dis_CH","Cancel selection",width = "49.5%")),
                               br(),      
                               br()),
              
                 
                
                  checkboxInput("DefArt","Choose an article of the Special Part of the Criminal Code"),
                 conditionalPanel(condition = "input.DefArt==true",
                                  uiOutput("articleK"),
                                  fluidRow(actionButton ("all_Art","Choose all articles",width = "49.5%"),
                                           actionButton ("dis_Art","Cancel selection",width = "49.5%")),
                                                           
                                  
                                  ),
                 
                 
                 uiOutput ("vidsotkova"),
                 uiOutput("DefBase"),
                 br(),      
                 br(),                               
                 br(),      
                 br(),                               
                 br(),      
                 br(),                               
                 br(),      
                 br(),
               ),
               
             
             mainPanel (
               HTML("Visualizations and data sets are created on the basis of reports for the period from 2013 to 2021, presented on the official websites of the Office of the Prosecutor General of Ukraine and the State Judicial Administration"),
               br(),
               downloadButton("download_c", "Download the visualization"),
               plotOutput(outputId = "graph_c"),
               #textOutput("control"),
               textOutput("control2"),
               uiOutput ("Perc_g_but"),
               plotOutput (outputId = "proc_graph")
               
             )
             
             
             
             )
             ),
    
    
    tabPanel ("Visualization Constructor Data",
              HTML ("The data set is formed"),
              br(),
              downloadButton("download_table", "Download the dataset (CSV)"),
              DT::dataTableOutput(outputId = "construct"),
              uiOutput ("Perc_t"),
              br(),
              uiOutput ("Perc_but"),
              DT::dataTableOutput(outputId = "Perc"),
                             
              
              
              
              )   
              
              )
    
    
    
    
    




server <- function(input, output,session) {



  

  output$DefChapOut<-renderUI({
  selectInput("ChapterK", "Section of the Special Part of the Criminal Code:",
              choices=chapter$chap,
              selectize = TRUE,
              multiple = TRUE,
              selected =chapter$chap[1])
    })
  
  observeEvent(input$all_CH,{
    updateSelectInput(session, "ChapterK",
                      choices=chapter$chap,
                      selected =chapter$chap)
  })
  
  observeEvent(input$dis_CH,{
    updateSelectInput(session, "ChapterK",
                      choices=chapter$chap,
                      selected =NULL)
  })
  
  
  constr_ch<-reactive({if (input$DefChap) chapter$chn[chapter$chap %in% input$ChapterK] else chapter$chn})
  constr_art<-reactive({if (input$DefArt) 
    articles$header[articles$chapt %in% constr_ch()]
         })
  
  output$articleK<-renderUI ({
       
         selectInput("art_chK", "Article:",
                choices = constr_art(),
                selectize = TRUE,
                multiple = TRUE,
         selected = constr_art()[1]
         )
  })
  
  observeEvent(input$all_Art,{
    updateSelectInput(session, "art_chK",
                      choices=constr_art(),
                      selected =constr_art())
  })
  
  observeEvent(input$dis_Art,{
    updateSelectInput(session, "art_chK",
                      choices=constr_art(),
                      selected =NULL)
  })
  
  
  
  staloR<-reactive({rule%>%select(input$fields)%>%filter(row_number()==1)})
    
  fieldsT<-reactive({
    if (!input$DefYear&!input$DefChap&!input$DefArt)
      {unlist(staloR())} else
        if (!input$DefYear&input$DefChap&!input$DefArt) 
          {c("Section","Section name",unlist(staloR()))}else
            if (!input$DefYear&input$DefArt)
              {c("Article","Article Name", unlist(staloR()))}else
                if (input$DefYear&!input$DefChap&!input$DefArt) 
                  {c("Year", unlist(staloR()))}else  
                    if (input$DefYear&input$DefChap&!input$DefArt)     
                      {c("Year","Section","Section name", unlist(staloR()))}else  
                        if (input$DefYear&input$DefArt)  
                          {c("Year","Article", "Article name",unlist(staloR()))}  
                            })
  fieldsT2<-reactive({
    if (!input$DefYear&!input$DefChap&!input$DefArt)
    {unlist(staloR())} else
      if (!input$DefYear&input$DefChap&!input$DefArt) 
      {c("ChapterR", unlist(staloR()))}else
        if (!input$DefYear&input$DefArt)
        {c("Article", unlist(staloR()))}else
          if (input$DefYear&!input$DefChap&!input$DefArt) 
          {c("Year", unlist(staloR()))}else  
            if (input$DefYear&input$DefChap&!input$DefArt)     
            {c("Year","ChapterR", unlist(staloR()))}else  
              if (input$DefYear&input$DefArt)  
              {c("Year","Article", unlist(staloR()))}  
                })
  
  resultc <- reactive({
    
    if (!input$DefYear&!input$DefChap&!input$DefArt) 
      {crimes%>%select(input$fields)%>%
      summarise_if(is.numeric,sum)%>%setNames(fieldsT())} else
    
        if (!input$DefYear&input$DefChap&!input$DefArt) 
      {crimes%>%select(1:5,ChapterR,input$fields)%>%
      filter(ChapterTXT %in% input$ChapterK)%>%
      select(ChapterR,ChapterTXT,input$fields)%>%
      group_by(ChapterR,ChapterTXT)%>%
      summarise_if(is.numeric,sum)%>%setNames(fieldsT())} else
        
        if (!input$DefYear&input$DefArt) 
        {crimes%>%select(1:4,input$fields)%>%
            filter(nazva %in% input$art_chK)%>%
            select(Article,nazva,input$fields)%>%
            group_by(Article,nazva)%>%
            summarise_if(is.numeric,sum)%>%setNames(fieldsT())}else
        
        if (input$DefYear&!input$DefChap&!input$DefArt) 
        {crimes%>%select(Year,input$fields)%>%
            filter((Year>=input$Rik[1])&(Year<=input$Rik[2]))%>%
            group_by(Year)%>%
        summarise_if(is.numeric,sum)%>%setNames(fieldsT())} else
        
        if (input$DefYear&input$DefChap&!input$DefArt) 
                    {crimes%>%select(1:5,ChapterR,input$fields)%>%
                        filter(ChapterTXT %in% input$ChapterK)%>%
            filter((Year>=input$Rik[1])&(Year<=input$Rik[2]))%>%
                        select(Year,ChapterR,ChapterTXT,input$fields)%>%
                        group_by(Year, ChapterR, ChapterTXT)%>%
                        summarise_if(is.numeric,sum)%>%setNames(fieldsT())} else
        if (input$DefYear&input$DefArt) 
          {crimes%>%select(1:4,input$fields)%>%
            filter(nazva %in% input$art_chK)%>%
            filter((Year>=input$Rik[1])&(Year<=input$Rik[2]))%>%
            select(Year,Article,nazva,input$fields)%>%
                              group_by(Year,Article,nazva)%>%
                              summarise_if(is.numeric,sum)%>%setNames(fieldsT())}       
              
              
              
  })
  
  
  
  resultc2 <- reactive({
    
    if (!input$DefYear&!input$DefChap&!input$DefArt) 
    {crimes%>%select(input$fields)%>%
        summarise_if(is.numeric,sum)%>%setNames(fieldsT2())%>%
        as.data.frame%>%reshape(timevar = "Comment", 
              times=fieldsT(), 
              v.names = "QNT", 
              varying = fieldsT(), 
              direction = "long")
        
        } else
          
          if (!input$DefYear&input$DefChap&!input$DefArt) 
          {req (input$ChapterK)
                crimes%>%select(1:5,input$fields,ChapterR)%>%
              filter(ChapterTXT %in% input$ChapterK)%>%
              select(ChapterR,input$fields)%>%
              group_by(ChapterR)%>%
              summarise_if(is.numeric,sum)%>%
            setNames(fieldsT2())%>%
             as.data.frame()%>%
               reshape(timevar = "Comment", 
                      times=fieldsT2()[2:length(fieldsT2())],
                      v.names = "QNT", 
                      varying=fieldsT2()[2:length(fieldsT2())],
                      direction = "long")
            } else
                
                if (!input$DefYear&input$DefArt) 
                {req (input$art_chK)
                  crimes%>%select(1:4,input$fields)%>%
                    filter(nazva %in% input$art_chK)%>%
                    select(Article,input$fields)%>%
                    group_by(Article)%>%
                    summarise_if(is.numeric,sum)%>%
                  setNames(fieldsT2())%>%
                    as.data.frame()%>%
                    reshape(timevar = "Comment", 
                            times=fieldsT2()[2:length(fieldsT2())],
                            v.names = "QNT", 
                            varying=fieldsT2()[2:length(fieldsT2())],
                            direction = "long")
                  
                  
                  }else
                      
                      if (input$DefYear&!input$DefChap&!input$DefArt) 
                      {req (input$Rik)
                        crimes%>%select(Year,input$fields)%>%
                          filter((Year>=input$Rik[1])&(Year<=input$Rik[2]))%>%
                          group_by(Year)%>%
                          summarise_if(is.numeric,sum)%>%
                        setNames(fieldsT2())%>%
                          as.data.frame()%>%
                          reshape(timevar = "Comment", 
                                  times=fieldsT2()[2:length(fieldsT2())],
                                  v.names = "QNT", 
                                  varying=fieldsT2()[2:length(fieldsT2())],
                                  direction = "long")
                        
                        
                        
                        
                        
                        } else
                            
                            if (input$DefYear&input$DefChap&!input$DefArt) 
                            {req (input$Rik)
                              req (input$ChapterK)
                              crimes%>%select(1:5,input$fields,ChapterR)%>%
                                filter(ChapterTXT %in% input$ChapterK)%>%
                                filter((Year>=input$Rik[1])&(Year<=input$Rik[2]))%>%
                                select(Year,ChapterR,input$fields)%>%
                                group_by(Year, ChapterR)%>%
                                summarise_if(is.numeric,sum)%>%
                                setNames(fieldsT2())%>%
                                as.data.frame()%>%
                                reshape(timevar = "Comment", 
                                        times=fieldsT2()[3:length(fieldsT2())],
                                        v.names = "QNT", 
                                        varying=fieldsT2()[3:length(fieldsT2())],
                                        direction = "long")
                              
                              } 
    
    
    else
      if (input$DefYear&input$DefArt) 
                                  {req (input$Rik)
        req (input$art_chK)
        crimes%>%select(1:4,input$fields)%>%
                                      filter(nazva %in% input$art_chK)%>%
                                      filter((Year>=input$Rik[1])&(Year<=input$Rik[2]))%>%
                                      select(Year,Article,input$fields)%>%
                                      group_by(Year,Article)%>%
                                      summarise_if(is.numeric,sum)%>%
          setNames(fieldsT2())%>%
          as.data.frame()%>%
          reshape(timevar = "Comment", 
                  times=fieldsT2()[3:length(fieldsT2())],
                  v.names = "QNT", 
                  varying=fieldsT2()[3:length(fieldsT2())],
                  direction = "long")
        
          }       
    
    
    
  })
  
  
  output$construct <- renderDataTable({
    resultc()
  })
  
  output$download_table <- downloadHandler(
    filename = function() 
    {
      paste("karchevskyi-table-",format(Sys.Date(),"%d-%m-%Y"),".csv",sep = "")},
    content=function(file){
      
      write_csv(resultc(),file)}
  )
  

  plot1_1<-reactive({if(input$labels&!input$DefYear) 
  {plot1()+geom_text(aes(angle=90,y=max(resultc2()$QNT/2)))}
    else 
      if (input$labels&input$DefYear) {plot1()+geom_label(nudge_y = 0.01)}
    else {plot1()}
    
    
  })
  
  output$graph_c<-renderPlot(plot1_1())
  
  
  
  
  
  
  plot1<-reactive ({
    if (!input$DefYear&!input$DefChap&!input$DefArt) {
    ggplot(data = resultc2(), 
           aes( x = factor(Comment,levels = fieldsT2()),
                y = QNT, label=QNT,
                fill = factor(Comment,levels = fieldsT2()))) +
      geom_bar(stat = "identity")+ylab("absolute values")+
        scale_y_continuous(labels = point)+
        theme(axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              legend.title=element_blank())#+geom_text(aes(angle=90,y=max(resultc2()$QNT/2)))
        } 
    else if (!input$DefYear&input$DefChap&!input$DefArt) 
    {
      ggplot(data = resultc2(), 
             aes( x = factor(Comment,levels = fieldsT2()),y = QNT, 
                  label=QNT,fill = factor(Comment,levels = fieldsT2()))) +
        scale_y_continuous(labels = point)+
        geom_bar(stat = "identity")+ylab("absolute values")+
        facet_wrap (.~factor(ChapterR, 
                             levels=chapter_label), 
                    ncol=4)+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              legend.title=element_blank())#+geom_text(aes(angle=90,y=max(resultc2()$QNT/2)))
      }
      
    else if (!input$DefYear&input$DefArt) 
    {
      ggplot(data = resultc2(), 
             aes( x = factor(Comment,levels = fieldsT2()),y = QNT, label=QNT,
                  fill = factor(Comment,levels = fieldsT2()))) +
        facet_wrap (.~factor(Article), 
                    ncol=4)+
        scale_y_continuous(labels = point)+
        geom_bar(stat = "identity")+ylab("absolute values")+
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              legend.title=element_blank())#+geom_text(aes(angle=90,y=max(resultc2()$QNT/2)))
      }
    
    else if (input$DefYear&!input$DefChap&!input$DefArt)
    {
      ggplot (data = resultc2(),
              aes (x=factor(Year), y = QNT, label=QNT,
                   color=factor(Comment,levels = fieldsT2())))+
        scale_y_continuous(labels = point)+ylab("absolute values")+
        geom_point()+geom_line(aes(group=Comment))+
        theme(axis.title.x = element_blank(),
              legend.title=element_blank())#+geom_text(nudge_y = 0.01*max(resultc2()$QNT))
    }
    
    else if (input$DefYear&input$DefChap&!input$DefArt)
    {
      ggplot (data = resultc2(),
              aes (x=factor(Year), y = QNT, label=QNT,
                   color=factor(Comment,levels = fieldsT2())))+
        scale_y_continuous(labels = point)+ylab("absolute values")+
        geom_point()+geom_line(aes(group=Comment))+
        facet_wrap (.~factor(ChapterR, 
                             levels=chapter_label), 
                    ncol=4)+
        theme(axis.title.x = element_blank(),
              legend.title=element_blank(),
              axis.text.x=element_text(angle = 90))
    }
    
    else if (input$DefYear&input$DefArt)
    {
      ggplot (data = resultc2(),
              aes (x=factor(Year), y = QNT, label=QNT,
                   color=factor(Comment,levels = fieldsT())))+
        scale_y_continuous(labels = point)+ylab("absolute values")+
        geom_point()+geom_line(aes(group=Comment))+
        facet_wrap (.~factor(Article), 
                    ncol=4)+
        theme(axis.title.x = element_blank(),
              legend.title=element_blank(),
              axis.text.x=element_text(angle = 90))
    }
    
    })

  
  
  output$download_c <- downloadHandler(
    filename = function() 
    {
      paste ("construct-",format(Sys.Date(),"%d-%m-%Y"),".png")
    },
    content=function(file)
    {
      ggsave (filename= file, plot=plot1_1()+
                ggtitle(
"Visualizations and data sets are created on the basis of reports for the period
from 2013 to 2021, presented on the official websites of the Office 
of the Prosecutor General of Ukraine and the State Judicial Administration.
Created by web-application - Karchevskyi M. Combating crime in Ukraine: infographic",
subtitle = paste("URL :https://karchevskiy.org/i-dovidnyk/ (date :",format(Sys.Date(),"%d.%m.%Y"),")")),
              device="png",
              width = 16, height = 9, dpi = 300, units = "in" )
    }
  )  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$vidsotkova<-renderUI({
    if (length(input$fields)>1)
    {checkboxInput("percent","Add the visualization with relative values")}
  })
  
  output$DefBase<-renderUI({req(input$percent)    
    
    if (input$percent)
      {selectInput("base","Choose the base variable",
                  choices=unique(resultc2()$Comment))}
      
    
    }) 
  
  #---------------------------------------NEW DEF OF REACTIVE FOR GRAPH_Percentage
  
  
  baseR<-reactive({unlist(resultc()[input$base])})


 resultP<-reactive(
   {
     
     req(input$percent)
   req(input$base)
   
   
   if (!input$DefYear&!input$DefChap&!input$DefArt)
   {round(resultc()[ ,1:ncol(resultc())]/baseR()*100,1)} else
     if (!input$DefYear&input$DefChap&!input$DefArt) 
     {cbind(resultc()[ ,1:2],round(resultc()[ ,3:ncol(resultc())]/baseR()*100,1))}else
       if (!input$DefYear&input$DefArt)
       {cbind(resultc()[ ,1:2],round(resultc()[ ,3:ncol(resultc())]/baseR()*100,1))}else
         if (input$DefYear&!input$DefChap&!input$DefArt) 
         {cbind(resultc()[ ,1],round(resultc()[ ,2:ncol(resultc())]/baseR()*100,1))}else  
           if (input$DefYear&input$DefChap&!input$DefArt)     
           {cbind(resultc()[ ,1:3],round(resultc()[ ,4:ncol(resultc())]/baseR()*100,1))}else  
             if (input$DefYear&input$DefArt)  
             {cbind(resultc()[, 1:3],round(resultc()[ ,4:ncol(resultc())]/baseR()*100,1))}  
   }
   

   ) 
 
 
 output$control<-renderText({unlist(resultc()[input$base])})


 
 #Percentage table
 
 output$Perc <- renderDataTable({
   resultP()
 }) 


 
 output$download_tableP <- downloadHandler(
   filename = function() 
   {
     paste("karchevskyi-Ptable-",format(Sys.Date(),"%d-%m-%Y"),".csv",sep = "")},
   content=function(file){
     
     write_csv(resultP(),file)}
 )
 
 
 output$Perc_t<-renderText({req(input$percent)
   if (input$percent)
     HTML ("Percentage representation of the generated data set according to the defined base variable")})
 
 output$Perc_but<-renderUI({req(input$percent)
   if (input$percent)
     downloadButton("download_tableP", "Download percentage representation (CSV)")})
 

 output$Perc_g_but<-renderUI({req(input$percent)
   if (input$percent)
     downloadButton("Perc_g_but2", "Download the visualization with percentage representation")})
 
 output$Perc_g_but2<- downloadHandler(
   filename = function() 
   {
     paste ("constructP-",format(Sys.Date(),"%d-%m-%Y"),".png")
   },
   content=function(file)
   {
     ggsave (filename= file, plot=plot2_1()+
               ggtitle(
                 "Visualizations and data sets are created on the basis of reports for the period
from 2013 to 2021, presented on the official websites of the Office 
of the Prosecutor General of Ukraine and the State Judicial Administration.
Created by web-application - Karchevskyi M. Combating crime in Ukraine: infographic",
                 subtitle = paste("URL :https://karchevskiy.org/i-dovidnyk/ (date :",format(Sys.Date(),"%d.%m.%Y"),")")),
             device="png",
             width = 16, height = 9, dpi = 300, units = "in" )
   }
 )  

 
 fieldsT3<-reactive({
   if (!input$DefYear&!input$DefChap&!input$DefArt)
   {unlist(staloR())} else
     if (!input$DefYear&input$DefChap&!input$DefArt) 
     {c("ChapterR", "ChapterTXT", unlist(staloR()))}else
       if (!input$DefYear&input$DefArt)
       {c("Article","nazva", unlist(staloR()))}else
         if (input$DefYear&!input$DefChap&!input$DefArt) 
         {c("Year", unlist(staloR()))}else  
           if (input$DefYear&input$DefChap&!input$DefArt)     
           {c("Year","ChapterR", "ChapterTXT",unlist(staloR()))}else  
             if (input$DefYear&input$DefArt)  
             {c("Year","Article", "nazva",unlist(staloR()))}  
 })
 
 
 
 
 resultc2P_1 <- reactive({req (input$percent)
   
   if (!input$DefYear&!input$DefChap&!input$DefArt) 
   {resultP()%>%setNames(fieldsT3())%>%
       as.data.frame%>%reshape(timevar = "Comment", 
                               times=fieldsT(), 
                               v.names = "QNT", 
                               varying = fieldsT(), 
                               direction = "long")%>%filter(!is.na(QNT))
     
   } else
     
     if (!input$DefYear&input$DefChap&!input$DefArt) 
     {req (input$ChapterK)
       resultP()%>%
         setNames(fieldsT3())%>%
         as.data.frame()%>%
         reshape(timevar = "Comment", 
                 times=fieldsT3()[3:length(fieldsT3())],
                 v.names = "QNT", 
                 varying=fieldsT3()[3:length(fieldsT3())],
                 direction = "long")%>%filter(!is.na(QNT))
     } else
       
       if (!input$DefYear&input$DefArt) 
       {req (input$art_chK)
         resultP()%>%
           setNames(fieldsT3())%>%
           as.data.frame()%>%
           reshape(timevar = "Comment", 
                   times=fieldsT3()[3:length(fieldsT3())],
                   v.names = "QNT", 
                   varying=fieldsT3()[3:length(fieldsT3())],
                   direction = "long")%>%filter(!is.na(QNT))
         
         
       }else
         
         if (input$DefYear&!input$DefChap&!input$DefArt) 
         {req (input$Rik)
           resultP()%>%
             setNames(fieldsT3())%>%
             as.data.frame()%>%
             reshape(timevar = "Comment", 
                     times=fieldsT3()[2:length(fieldsT3())],
                     v.names = "QNT", 
                     varying=fieldsT3()[2:length(fieldsT3())],
                     direction = "long")%>%filter(!is.na(QNT))
           
           
           
           
           
         } else
           
           if (input$DefYear&input$DefChap&!input$DefArt) 
           {req (input$Rik)
             req (input$ChapterK)
             resultP()%>%
               setNames(fieldsT3())%>%
               as.data.frame()%>%
               reshape(timevar = "Comment", 
                       times=fieldsT3()[4:length(fieldsT3())],
                       v.names = "QNT", 
                       varying=fieldsT3()[4:length(fieldsT3())],
                       direction = "long")%>%filter(!is.na(QNT))
             
           } 
   
   
   else
     if (input$DefYear&input$DefArt) 
     {req (input$Rik)
       req (input$art_chK)
       resultP()%>%
         setNames(fieldsT3())%>%
         as.data.frame()%>%
         reshape(timevar = "Comment", 
                 times=fieldsT3()[4:length(fieldsT3())],
                 v.names = "QNT", 
                 varying=fieldsT3()[4:length(fieldsT3())],
                 direction = "long")%>%filter(!is.na(QNT))
       
     }       
   
   
   
 })
 
 
 
 plot2_1<-reactive(
   if(input$labels&!input$DefYear) 
 {plot2()+geom_text(aes(angle=90,y=max(resultc2P_1()$QNT/2)))}
   else 
     if (input$labels&input$DefYear) {plot2()+geom_label(nudge_y = 0.01)}
   else {plot2()}
   
   
 )
 

 output$proc_graph<-renderPlot(
   
   plot2_1()
   
   )
 
 
 plot2<-reactive ({
   if (!input$DefYear&!input$DefChap&!input$DefArt) {
     ggplot(data = resultc2P_1(), 
            aes( x = factor(Comment,levels = fieldsT2()),
                 y = QNT, label=QNT,
                 fill = factor(Comment,levels = fieldsT2()))) +
       geom_bar(stat = "identity")+ylab("relative values (%)")+
       scale_y_continuous(labels = point)+
       theme(axis.title.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.text.x = element_blank(),
             legend.title=element_blank())
   } 
   else if (!input$DefYear&input$DefChap&!input$DefArt) 
   {
     ggplot(data = resultc2P_1(), 
            aes( x = factor(Comment,levels = fieldsT2()),y = QNT, label=QNT,
                 fill = factor(Comment,levels = fieldsT2()))) +
       scale_y_continuous(labels = point)+
       geom_bar(stat = "identity")+ylab("relative values (%)")+
       facet_wrap (.~factor(ChapterR, 
                            levels=chapter_label), 
                   ncol=4)+
       theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             legend.title=element_blank())}
   
   else if (!input$DefYear&input$DefArt) 
   {
     ggplot(data = resultc2P_1(), 
            aes( x = factor(Comment,levels = fieldsT2()),y = QNT, label=QNT, 
                 fill = factor(Comment,levels = fieldsT2()))) +
       scale_y_continuous(labels = point)+
       facet_wrap (.~factor(Article), 
                   ncol=4)+
       geom_bar(stat = "identity")+ylab("relative values (%)")+
       theme(axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.ticks.x = element_blank(),
             legend.title=element_blank())}
   
   else if (input$DefYear&!input$DefChap&!input$DefArt)
   {
     ggplot (data = resultc2P_1(),
             aes (x=factor(Year), y = QNT, label=QNT,
                  color=factor(Comment,levels = fieldsT2())))+
       scale_y_continuous(labels = point)+ylab("relative values (%)")+
       geom_point()+geom_line(aes(group=Comment))+
       theme(axis.title.x = element_blank(),
             legend.title=element_blank())
   }
   
   else if (input$DefYear&input$DefChap&!input$DefArt)
   {
     ggplot (data = resultc2P_1(),
             aes (x=factor(Year), y = QNT, label=QNT,
                  color=factor(Comment,levels = fieldsT2())))+
       scale_y_continuous(labels = point)+ylab("relative values (%)")+
       geom_point()+geom_line(aes(group=Comment))+
       facet_wrap (.~factor(ChapterR, 
                            levels=chapter_label), 
                   ncol=4)+
       theme(axis.title.x = element_blank(),
             legend.title=element_blank(),
             axis.text.x=element_text(angle = 90))
   }
   
   else if (input$DefYear&input$DefArt)
   {
     ggplot (data = resultc2P_1(),
             aes (x=factor(Year), y = QNT, label=QNT,
                  color=factor(Comment,levels = fieldsT())))+
       scale_y_continuous(labels = point)+ylab("relative values (%)")+
       geom_point()+geom_line(aes(group=Comment))+
       facet_wrap (.~factor(Article), 
                   ncol=4)+
       theme(axis.title.x = element_blank(),
             legend.title=element_blank(),
             axis.text.x=element_text(angle = 90))
   }
   
 })
 
 
 
 
 
 
 }


shinyApp(ui = ui, server = server)
