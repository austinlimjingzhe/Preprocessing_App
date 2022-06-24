# if (!require("pacman")) install.packages("pacman")
# packages<-c("shiny","shinythemes","DT","tidyverse","DescTools","DataExplorer","e1071","ROSE","smotefamily","ica","Rtsne","MASS","lubridate","forcats","stringr","slider")
# pacman::p_load(char=packages)

library(shiny)
library(shinythemes)
library(DT)
library(DataExplorer)
library(tidyverse)
library(RColorBrewer)
library(DescTools)
library(Hmisc)
library(e1071)
library(ROSE)
library(smotefamily)
library(ica)
library(Rtsne)
library(MASS)
library(lubridate)
library(stringr)
library(slider)
library(fastDummies)
library(mice)
library(Amelia)
library(missForest)

ui <- fluidPage(theme = shinythemes::shinytheme("cerulean"),
                navbarPage("Data Preparation",
                           tabPanel("Upload Datasets",
                                    sidebarPanel(fileInput("upload", "Choose a file", buttonLabel = "Upload...",accept = ".csv",multiple = T),
                                                 checkboxInput("header", "Header", T),
                                                 checkboxInput("stringfactor","Strings As Factors",T),
                                                 radioButtons("sep", "Separator",
                                                              choices = c(Comma = ",",
                                                                          Semicolon = ";",
                                                                          Tab = "\t"),
                                                              selected = ",")),
                                    mainPanel(dataTableOutput("dataset"),
                                              hr(),
                                              plotOutput("structure"),
                                              uiOutput("datadensity"),
                                              uiOutput("databars"),
                                              style="overflow-x: scroll")),
                           tabPanel("Convert Data Types",
                                    sidebarPanel(div(strong("Control Panel"),style="text-align:center"),
                                                 hr(),
                                                 uiOutput("Columnconvert"),
                                                 radioButtons("To", "Convert to...",
                                                              choices = c(Numeric = "numeric",
                                                                          Factor = "factor",
                                                                          Datetime = "datetime"),
                                                              selected = "numeric"),
                                                 uiOutput("Datetimeformat"),
                                                 actionButton("Convert","Convert!",icon=icon("exchange-alt"),width = "100%"),
                                                 hr(),
                                                 downloadButton("DLConvert", "Download Dataset",style = "width:100%;")),
                                    mainPanel(dataTableOutput("PostConvert"),
                                              hr(),
                                              tableOutput("structure2"),
                                              style="overflow-x: scroll")),
                           tabPanel("Erroneous Data",
                                    sidebarPanel(div(strong("Control Panel"),style="text-align:center"),
                                                 hr(),
                                                 radioButtons("Handling", "Handling Erroneous Values",
                                                              choices = c("Drop Duplicated Rows" = "dupes",
                                                                          "Drop Rows with NA" = "drop",
                                                                          "Impute Missing" = "impute")),
                                                 fluidRow(column(5,uiOutput("ColumnImputeNum")),
                                                          column(5,uiOutput("ColumnImputeCat"))),
                                                 uiOutput("ImputeGroup"),
                                                 uiOutput("ImputeMethodNum"),
                                                 uiOutput("ImputeMethodCat"),
                                                 uiOutput("MissingAction"),
                                                 hr(),
                                                 downloadButton("DLHandling", "Download Dataset", style = "width:100%;")),
                                    mainPanel(dataTableOutput("PostHandling"),
                                              hr(),
                                              tableOutput("structure3"),
                                              style="overflow-x: scroll")),
                           tabPanel("Data Wrangling",
                                    sidebarPanel(div(strong("Control Panel"),style="text-align:center"),
                                                 hr(),
                                                 radioButtons("wranglingMethods","Select an Action to Perform:",
                                                              choices=c("select"="select",
                                                                        "filter"="filter",
                                                                        "arrange"="arrange",
                                                                        "summarize"="summarize",
                                                                        "rename"="rename")),
                                                 checkboxInput("groupby", "Group By:", T),
                                                 uiOutput("SelectConditions"),
                                                 uiOutput("FilterConditions"),
                                                 uiOutput("ArrangeOptions"),
                                                 uiOutput("SummaryOptions"),
                                                 uiOutput("wranglingGroup"),
                                                 uiOutput("renameConsole"),
                                                 actionButton("wrangleData","Wrangle",width="100%"),
                                                 hr(),
                                                 downloadButton("DLWrangledData","Download Wrangled Data",style = "width:100%;")),
                                    mainPanel(dataTableOutput("PostWrangle"),
                                              hr(),
                                              style="overflow-x:scroll")),
                           tabPanel("Transform Variables",
                                    sidebarPanel(div(strong("Control Panel"),style="text-align:center"),
                                                 hr(),
                                                 uiOutput("TransformNumericColumns"),
                                                 selectInput("TransformNumericlist","Transform Numeric Variables",
                                                             choices = c("normalize","standardize",
                                                                         "log","absolute","squareroot","square","boxcox",
                                                                         "winsorize","binning","ranking")),
                                                 uiOutput("binnums"),
                                                 checkboxInput("droporigin","Drop Original?",T),
                                                 actionButton("TransformNumeric","Transform",width = "100%"),
                                                 hr(),
                                                 uiOutput("TransformCategoricalColumns"),
                                                 selectInput("TransformCategoricallist","Transform Categorical Variables",
                                                             choices = c("onehotencoding","changelevel")),
                                                 uiOutput("relevelFrom"),
                                                 uiOutput("relevelTo"),
                                                 actionButton("TransformCategorical","Transform",width = "100%"),
                                                 hr(),
                                                 downloadButton("DLTransform", "Download Dataset",style = "width:100%;")),
                                    mainPanel(dataTableOutput("PostTransform"),
                                              hr(),
                                              style="overflow-x: scroll",
                                              fluidRow(column(12,uiOutput("histograms")),
                                                       column(12,uiOutput("frequencies"))))),
                           tabPanel("Advanced Options",
                                    tabsetPanel(type="tabs",
                                                tabPanel("Advanced Imputation",
                                                         sidebarPanel(div(strong("Control Panel"),style="text-align:center"),
                                                                      hr(),
                                                                      radioButtons("ImputePackages", "Select an Imputation Package:",
                                                                                   choices = c("MICE" = "MICE",
                                                                                               "Amelia" = "Amelia",
                                                                                               "missForest"="missForest")),
                                                                      uiOutput("miceconsole"),
                                                                      uiOutput("ameliaconsole"),
                                                                      uiOutput("missforestconsole"),
                                                                      actionButton("AdvancedImpute","Impute",width = "100%"),
                                                                      hr(),
                                                                      downloadButton("DLAdvancedImpute", "Download Dataset",style = "width:100%;")),
                                                          mainPanel(dataTableOutput("PostAdvImp"),
                                                                    hr(),
                                                                    style="overflow-x: scroll")),
                                                tabPanel("Dimensionality Reduction",
                                                         sidebarPanel(div(strong("Control Panel"),style="text-align:center"),
                                                                      hr(),
                                                                      radioButtons("DimRedmethod","Select a Dimensionality Reduction Method:",
                                                                                   choices=c("PCA"="PCA",
                                                                                             "LDA"="LDA",
                                                                                             "ICA"="ICA",
                                                                                             "t-SNE"="tSNE")),
                                                                      uiOutput("PCAconsole"),
                                                                      uiOutput("ICAconsole"),
                                                                      uiOutput("LDAconsole"),
                                                                      uiOutput("tSNEconsole"),
                                                                      actionButton("DimReduce","Reduce Dimensionality",width = "100%"),
                                                                      hr(),
                                                                      downloadButton("DLReducedData","Download Reduced Dataset",style="width: 100%;")),
                                                         mainPanel(dataTableOutput("ReducedData"),
                                                                   hr(),
                                                                   style="overflow-x:scroll")),
                                                tabPanel("Imbalanced Classes",
                                                         sidebarPanel(div(strong("Control Panel"),style="text-align:center"),
                                                                      hr(),
                                                                      radioButtons("Imbalancedmethod","Select a Package for Imbalanced Classes:",
                                                                                   choices = c("ROSE"="rose",
                                                                                               "smotefamily"="smote")),
                                                                      uiOutput("imbclasstarget"),
                                                                      uiOutput("roseconsole"),
                                                                      uiOutput("smoteconsole"),
                                                                      actionButton("imbclassresamp","Resample",width = "100%"),
                                                                      hr(),
                                                                      downloadButton("DLResampledData","Download Resampled Data",style = "width:100%;")),
                                                         mainPanel(dataTableOutput("ResampledData"),
                                                                   hr(),
                                                                   style="overflow-x:scroll",
                                                                   fluidRow(column(12,plotOutput("beforeResample")),
                                                                            column(12,plotOutput("afterResample"))))),
                                                tabPanel("Merge Datasets",
                                                         sidebarPanel(fileInput("upload2", "Choose another file", buttonLabel = "Upload...",accept = ".csv",multiple = T),
                                                                      checkboxInput("header2", "Header", T),
                                                                      checkboxInput("stringfactor2","Strings As Factors",T),
                                                                      radioButtons("sep2", "Separator",
                                                                                   choices = c(Comma = ",",
                                                                                               Semicolon = ";",
                                                                                               Tab = "\t"),
                                                                                   selected = ","),
                                                                      hr(),
                                                                      uiOutput("JoinCondition"),
                                                                      uiOutput("JoinonA"),
                                                                      uiOutput("JoinonB"),
                                                                      actionButton("mergedata","Merge",width="100%"),
                                                                      hr(),
                                                                      downloadButton("DLMergedData","Download Merged Data",style = "width:100%;")),
                                                         mainPanel(h1("Dataset 1:"),
                                                                   dataTableOutput("first_dataset"),
                                                                   hr(),
                                                                   h1("Dataset 2:"),
                                                                   dataTableOutput("second_dataset"),
                                                                   hr(),
                                                                   h1("Merged Data:"),
                                                                   dataTableOutput("merged_dataset"),
                                                                   style="overflow-x: scroll")),
                                                tabPanel("Rolling Windows & DateTime Functions",
                                                         sidebarPanel(div(strong("Control Panel - Rolling Windows"),style="text-align:center"),
                                                                      hr(),
                                                                      textInput("windowsize","Please enter a window size:",
                                                                                placeholder = "How many periods should the rolling window be?"),
                                                                      selectInput("rollingfunctions","Select a Transformation:",
                                                                                  choices = c("mean","median","min","max","sum","sd","IQR")),
                                                                      uiOutput("windowOptions"),
                                                                      actionButton("rollingTransform","Transform",width="100%"),
                                                                      hr(),
                                                                      div(strong("Control Panel - DateTime"),style="text-align:center"),
                                                                      hr(),
                                                                      selectInput("datetimeFunctions","Select a Transformation:",
                                                                                  choices = c("Extract","Calculate")),
                                                                      uiOutput("datetimeColumns"),
                                                                      uiOutput("datetimeConsole"),
                                                                      actionButton("datetimeTransform","Transform",width="100%"),
                                                                      hr(),
                                                                      downloadButton("DLtimefunctions","Download Dataset",style = "width:100%;")),
                                                         mainPanel(dataTableOutput("PostTimeFunctions"),
                                                                   hr(),
                                                                   style="overflow-x:scroll"))))
                )
)

server <- function(input, output, session) {
  
  #upload dataset
  v<-reactiveValues(df=NULL)
  observeEvent(input$upload, {
    v$df <- read.csv(input$upload$datapath,
                     header = input$header,
                     sep = input$sep,
                     stringsAsFactors = input$stringfactor)
    
    explore<-split_columns(v$df,binary_as_factor = T)
    pagesDensity<-ceiling(explore$num_continuous/16)
    pagesBars<-ceiling(explore$num_discrete/9)
    
    output$datadensity<-renderUI({
      lapply(1:pagesDensity, function(i){
        desityId <- paste0("densityplot_", i)
        plotOutput(desityId)
      
        output[[desityId]]<-renderPlot({
          pagenum<-paste0("page_",i)
          plottitle<-paste("Density Plots Page",i)
          return(plot_density(v$df,title = plottitle, theme_config=list("plot.title" = element_text(hjust = 0.5)))[[pagenum]])
        })
      })
    })
    
    output$databars<-renderUI({
      lapply(1:pagesBars, function(i){
        barId <- paste0("barplot_", i)
        plotOutput(barId)
        
        output[[barId]]<-renderPlot({
          pagenum<-paste0("page_",i)
          plottitle<-paste("Bar Plots Page",i)
          return(plot_bar(v$df,title = plottitle, theme_config=list("plot.title" = element_text(hjust = 0.5)))[[pagenum]])
        })
      })
    })
  })

  output$dataset <- renderDataTable({
    req(v$df)
    return(v$df)
  })
  
  output$structure<-renderPlot({
    req(v$df)
    df<-profile_missing(v$df)
    df$class=sapply(v$df,class)
    df$class=ordered(df$class,levels=unique(df$class))
    
    gg<-ggplot(df,aes(y=reorder(feature,as.numeric(class)),x=num_missing,fill=class))+
      geom_bar(stat = "identity")+
      ylab("Feature")+
      xlab("Missing rows")+
      ggtitle("Missingness Plot")+
      geom_label(aes(label=round(pct_missing,2)))+
      scale_fill_brewer(palette="Pastel1")+
      theme(legend.position="bottom",
            plot.title = element_text(hjust = 0.5))
    return(gg)
  })
  
  #convert variables
  output$Columnconvert<-renderUI({
    req(v$df)
    checkboxGroupInput("Columnstoconvert","Select columns:",
                       choices=names(v$df))
  })
  output$Datetimeformat<-renderUI({
    req(input$To)
    if(input$To=="datetime"){
      textInput("datetimeformat","Enter datetime format:",
                placeholder = "%Y-%m-%d %H:%M:%S")
    }
  })
  
  observeEvent(input$Convert, {
    if(input$To=="factor"){
      v$df[,input$Columnstoconvert]<-lapply(v$df[,input$Columnstoconvert],as.factor)
    }
    else if(input$To=="numeric"){
      v$df[,input$Columnstoconvert]<-lapply(v$df[,input$Columnstoconvert],as.numeric)
    }
    else if(input$To=="datetime"){
      v$df[,input$Columnstoconvert]<-lapply(v$df[,input$Columnstoconvert],as.Date.character,format=input$datetimeformat)
    }
  })
  
  output$PostConvert<-renderDataTable({
    req(v$df)
    return(v$df)
  })
  output$structure2<-renderTable({
    req(v$df)
    struct2<-data.frame(name=names(v$df),
                        class=sapply(v$df,class),
                        row.names = NULL)
    return(struct2)
  })
  
  output$DLConvert <- downloadHandler(
    filename = "dataset_cleaned.csv",
    content = function(file) {
      write.csv(v$df, file, row.names = FALSE)
    }
  )
  
  #handle erroneous values
  output$ColumnImputeNum<-renderUI({
    req(v$df)
    req(input$Handling)
    onlynumeric<-Filter(is.numeric,v$df)
    if(input$Handling=="impute"){
      checkboxGroupInput("ColumnstoimputeNum","Select columns:",
                         choices = names(which(colSums(is.na(onlynumeric))>0)))
    }
  })
  output$ColumnImputeCat<-renderUI({
    req(v$df)
    req(input$Handling)
    onlycategorical<-Filter(is.factor,v$df)
    if(input$Handling=="impute"){
      checkboxGroupInput("ColumnstoimputeCat","Select columns:",
                         choices = names(which(colSums(is.na(onlycategorical))>0)))
    }
  })
  output$ImputeGroup<-renderUI({
    req(v$df)
    req(input$Handling)
    onlycategorical<-Filter(is.factor,v$df)
    if(input$Handling=="impute"){
      selectInput("ImputebyGroup","Select a group to impute by:",
                  choices = c("None",names(onlycategorical)),selected = "None")
    }
  })
  output$ImputeMethodNum<-renderUI({
    req(v$df)
    req(input$Handling)
    if(input$Handling=="impute"){
      radioButtons("ImputebyMethodNum","Select Numeric Imputation Method:",
                   choices = c(mean = "mean",
                               median="median",
                               backfill="backfill",
                               frontfill="frontfill",
                               random = "random"))
    }
  })
  output$ImputeMethodCat<-renderUI({
    req(v$df)
    req(input$Handling)
    if(input$Handling=="impute"){
      radioButtons("ImputebyMethodCat","Select Categorical Imputation Method:",
                   choices = c(mode = "mode",
                               backfill="backfill",
                               frontfill="frontfill",
                               newcat = "newcategory"))
    }
  })
  output$MissingAction<-renderUI({
    if(input$Handling=="drop"){
      fluidRow(column(12,actionButton("DropNA","Drop Rows",width = "100%",icon = icon("trash-alt"))),
               column(12,actionButton("ShowNA","Show Rows with NA",width="100%",icon=icon("eye-slash"))),
               column(12,actionButton("ShowAllData","Show All Data",width="100%",icon=icon("eye"))))
    }
    else if(input$Handling=="impute"){
      actionButton("Impute","Impute Values",width = "100%", icon=icon("calculator"))
    }
    else{
      fluidRow(column(12,actionButton("showdupes","Show Duplicate Data",width = "100%",icon=icon("eye-slash"))),
               column(12,actionButton("showdata","Show All Data",width = "100%",icon=icon("eye"))),
               column(12,style="margin-top: 10px;",
                      actionButton("dropdupes","Drop Duplicate Data",width = "100%",icon=icon("trash-alt"))))
    }
  })
  
  observeEvent(input$DropNA,{
    v$df<-na.omit(v$df)
  })
  observeEvent(input$ShowAllData,{
    output$PostHandling<-renderDataTable({
      return(v$df)
    })
  })
  observeEvent(input$ShowNA,{
    narows<-v$df[!complete.cases(v$df),]
    output$PostHandling<-renderDataTable({
      return(narows)
    })
  })
  
  observeEvent(input$showdupes,{
    dupes<-v$df[duplicated(v$df),]
    output$PostHandling<-renderDataTable({
      return(dupes)
    })
  })
  observeEvent(input$showdata,{
    output$PostHandling<-renderDataTable({
      return(v$df)
    })
  })
  observeEvent(input$dropdupes,{
    v$df<-v$df[!duplicated(v$df),]
  })
  
  #to fix
  observeEvent(input$Impute,{
    if(length(input$ColumnstoimputeNum)>0){
      if(input$ImputebyMethodNum=="mean"){
        v$df<-v$df%>%group_by(across(input$ImputeGroup))%>%mutate(across(input$ColumnstoimputeNum,~Hmisc::impute(.x,fun=mean)))
      }
      else if(input$ImputebyMethodNum=="mean"){
        v$df<-v$df%>%group_by(across(input$ImputeGroup))%>%mutate(across(input$ColumnstoimputeNum,~Hmisc::impute(.x)))
      }
      else if(input$ImputebyMethodNum=="backfill"){
        v$df<-v$df%>%group_by(across(input$ImputeGroup))%>%tidyr::fill(where(is.numeric),.direction="up")
      }
      else if(input$ImputebyMethodNum=="frontfill"){
        v$df<-v$df%>%group_by(across(input$ImputeGroup))%>%tidyr::fill(where(is.numeric),.direction="down")
      }
      else if(input$ImputebyMethodNum=="random"){
        v$df<-v$df%>%group_by(across(input$ImputeGroup))%>%mutate(across(input$ColumnstoimputeNum,~Hmisc::impute(.x,fun="random")))
      }
    }
    if(length(input$ColumnstoimputeCat)>0){
      replace_factor_na_1 <- function(x){
        x <- as.character(x)
        x <- if_else(is.na(x), 'missing', x)
        x <- as.factor(x)
      }
      replace_factor_na_2 <- function(x){
        x <- as.character(x)
        x <- if_else(is.na(x), Mode(x,na.rm = T)[1], x)
        x <- as.factor(x)
      }
      
      if(input$ImputebyMethodCat=="mode"){
        v$df<-v$df%>%group_by(across(input$ImputeGroup))%>%mutate(across(input$ColumnstoimputeCat,replace_factor_na_2))
      }
      else if(input$ImputebyMethodCat=="backfill"){
        v$df<-v$df%>%group_by(across(input$ImputeGroup))%>%tidyr::fill(where(is.factor),.direction="up")
      }
      else if(input$ImputebyMethodCat=="frontfill"){
        v$df<-v$df%>%group_by(across(input$ImputeGroup))%>%tidyr::fill(where(is.factor),.direction="down")
      }
      else if(input$ImputebyMethodCat=="new category"){
        v$df<-v$df%>%group_by(across(input$ImputeGroup))%>%mutate(across(input$ColumnstoimputeCat,replace_factor_na_1))
      }
    }
  })

  output$PostHandling<-renderDataTable({
    req(v$df)
    return(v$df)
  })
  output$structure3<-renderTable({
    req(v$df)
    struct3<-data.frame(name=names(v$df),
                        missing=colSums(is.na(v$df)),
                        row.names = NULL)
    return(struct3)
  })
  
  output$DLHandling <- downloadHandler(
    filename = "dataset_cleaned.csv",
    content = function(file) {
      write.csv(v$df, file, row.names = FALSE)
    }
  )
  
  #Data Wrangling
  output$SelectConditions<-renderUI({
    req(v$df)
    req(input$wranglingMethods)
    if(input$wranglingMethods=="select"){
      checkboxGroupInput("columnsKeep","Select columns to keep:",
                         choices = names(v$df))
    }
  })
  output$FilterConditions<-renderUI({
    req(v$df)
    req(input$wranglingMethods)
    if(input$wranglingMethods=="filter"){
      fluidRow(column(4,selectInput("columntocheck","Filter by:",
                                    choices = names(v$df))),
               column(3,selectInput("filterOperator"," ",
                                    choices = c("==","!=","<","<=",">",">=","%in%","contains"))),
               column(5,textInput("filterExpression"," ",
                                  placeholder = "Type in your Expression")))
    }
  })
  output$wranglingGroup<-renderUI({
    req(v$df)
    req(input$wranglingMethods)
    if(input$groupby){
      selectInput("groupbyVariable","Group By:",
                  choices = c("None",names(v$df)))
    }
  })
  output$SummaryOptions<-renderUI({
    req(v$df)
    req(input$wranglingMethods)
    if(input$wranglingMethods=="summarize"){
      fluidRow(column(12,selectInput("summaryFunction","Select an aggregation function:",
                                     choices=c("mean","median","max","min","sum","count","sd","IQR"))),
               column(12,selectInput("columnSummarize","Select the column to summarize",
                                     choices = names(v$df))))
    }
  })
  output$ArrangeOptions<-renderUI({
    req(v$df)
    req(input$wranglingMethods)
    if(input$wranglingMethods=="arrange"){
      fluidRow(column(12,checkboxGroupInput("sortVariable","Sort By:",
                                            choices = names(v$df))),
               column(12,checkboxInput("ascdesc","Descending?",F)))
    }
  })
  output$renameConsole<-renderUI({
    req(v$df)
    req(input$wranglingMethods)
    if(input$wranglingMethods=="rename"){
      fluidRow(column(12,selectInput("beforeRename","Select a column to rename:",
                                     choices = names(v$df))),
               column(12,textInput("afterRename","Please enter a new name for the column:")))
    }
  })
  
  results<-reactiveValues(df=NULL)
  observeEvent(input$wrangleData,{
    req(v$df)
    results$df<-v$df
    if(input$wranglingMethods=="select"){
      results$df<-results$df%>%
        dplyr::select(input$columnsKeep)
    }
    else if(input$wranglingMethods=="filter"){
      onlynumeric<-Filter(is.numeric,results$df)
      onlycategorical<-Filter(is.factor,results$df)
      if(input$columntocheck %in% names(onlycategorical)){
        if(input$filterOperator=="=="){
          results$df<-results$df[results$df[input$columntocheck]==input$filterExpression,]
        }
        else if(input$filterOperator=="%in%"){
          results$df<-results$df%>%dplyr::filter(.data[[input$columntocheck]]%in%unlist(str_split(input$filterExpression)))
        }
        else if(input$filterOperator=="contains"){
          results$df<-results$df%>%dplyr::filter(grepl(input$filterExpression,.data[[input$columntocheck]]))
        }
      }
      else if(input$columntocheck %in% names(onlynumeric)){
        exp=as.numeric(input$filterExpression)
        if(input$filterOperator=="=="){results$df<-results$df[results$df[input$columntocheck]==exp,]}
        else if(input$filterOperator=="<"){results$df<-results$df[results$df[input$columntocheck]<exp,]}
        else if(input$filterOperator=="<="){results$df<-results$df[results$df[input$columntocheck]<=exp,]}
        else if(input$filterOperator==">"){results$df<-results$df[results$df[input$columntocheck]>exp,]}
        else if(input$filterOperator==">="){results$df<-results$df[results$df[input$columntocheck]>=exp,]}
        else{results$df<-results$df%>%dplyr::filter(.data[[input$columntocheck]]%in%as.numeric(unlist(str_split(input$filterExpression))))}
      }
    }
    else if(input$wranglingMethods=="arrange"){
      if(input$ascdesc){
        results$df<-results$df%>%
          arrange(across(input$sortVariable,desc))
      }
      results$df<-results$df%>%
        arrange(across(input$sortVariable))
    }
    else if(input$wranglingMethods=="rename"){
      names(results$df)[which(names(results$df)==input$beforeRename)]<-input$afterRename
    }
    else{
      func_list=list("mean"=mean,"median"=median,"max"=max,"min"=min,"sum"=sum,"count"=n,"sd"=sd,"IQR"=IQR)
      if(input$groupbyVariable=="None"){
        results$df<-results$df%>%
          dplyr::summarize(across(input$columnSummarize,func_list[[input$summaryFunction]]))
      }
      results$df<-results$df%>%
        group_by(across(input$groupbyVariable))%>%
        dplyr::summarize(across(input$columnSummarize,func_list[[input$summaryFunction]]))
    }
    
    output$PostWrangle<-renderDataTable({
      req(results$df)
      return(results$df)
    })
  })
  
  output$DLWrangledData <- downloadHandler(
    filename = "results_dataset.csv",
    content = function(file) {
      write.csv(results$df,file, row.names = FALSE)
    }
  )
  
  #transform variables
  output$TransformNumericColumns<-renderUI({
      req(v$df)
      onlynumeric<-Filter(is.numeric,v$df)
      checkboxGroupInput("NumericColumns","Select numeric columns:",
                         choices = names(onlynumeric))
      })
  output$TransformCategoricalColumns<-renderUI({
      req(v$df)
      onlycategorical<-Filter(is.factor,v$df)
      checkboxGroupInput("CategoricalColumns","Select categorical columns:",
                         choices = names(onlycategorical))
      })
  output$binnums<-renderUI({
    if(input$TransformNumericlist=="binning"){
      textInput("bins","Input number of bins:",value = 10)
    }
  })
  output$relevelFrom<-renderUI({
    req(v$df)
    req(input$TransformCategoricallist)
    if(input$TransformCategoricallist=="changelevel"){
      textInput("CategoryBefore","Input levels to change:",
                placeholder = "For multiple levels, separate by comma")
    }
    else{
      checkboxInput("remove_one_dummy","Remove 1 Dummy?",F)
    }
  })
  output$relevelTo<-renderUI({
    req(v$df)
    req(input$TransformCategoricallist)
    if(input$TransformCategoricallist=="changelevel"){
      textInput("CategoryAfter","New label:")
    }
    else{
      checkboxInput("remove_original_cat","Drop Original?",F)
    }
  })
  
  normalize=function(x, na.rm = TRUE) (x - min(x, na.rm = na.rm)) / (max(x, na.rm)-min(x,na.rm))
  square=function(x, na.rm = TRUE) x^2
  
  observeEvent(input$TransformNumeric,{
    if(input$TransformNumericlist=="normalize"){
      if(input$droporigin){
        v$df<-v$df%>%mutate(across(input$NumericColumns,normalize))
      }
      v$df<-v$df%>%mutate(across(input$NumericColumns,normalize,.names = "normalized_{.col}"))
    }
    else if(input$TransformNumericlist=="scaling"){
      if(input$droporigin){
        v$df<-v$df%>%mutate(across(input$NumericColumns,scale))
      }
      v$df<-v$df%>%mutate(across(input$NumericColumns,scale,.names = "scaled_{.col}"))
    }
    else if(input$TransformNumericlist=="log"){
      if(input$droporigin){
        v$df<-v$df%>%mutate(across(input$NumericColumns,log))
      }
      v$df<-v$df%>%mutate(across(input$NumericColumns,log,.names = "log_{.col}"))
    }
    else if(input$TransformNumericlist=="absolute"){
      if(input$droporigin){
        v$df<-v$df%>%mutate(across(input$NumericColumns,abs))
      }
      v$df<-v$df%>%mutate(across(input$NumericColumns,abs,.names = "abs_{.col}"))
    }
    else if(input$TransformNumericlist=="squareroot"){
      if(input$droporigin){
        v$df<-v$df%>%mutate(across(input$NumericColumns,sqrt))
      }
      v$df<-v$df%>%mutate(across(input$NumericColumns,sqrt,.names = "sqrt_{.col}"))
    }
    else if(input$TransformNumericlist=="square"){
      if(input$droporigin){
        v$df<-v$df%>%mutate(across(input$NumericColumns,square))
      }
      v$df<-v$df%>%mutate(across(input$NumericColumns,square,.names = "square_{.col}"))
    }
    else if(input$TransformNumericlist=="winsorize"){
      if(input$droporigin){
        v$df<-v$df%>%mutate(across(input$NumericColumns,Winsorize))
      }
      v$df<-v$df%>%mutate(across(input$NumericColumns,Winsorize,.names = "winsor_{.col}"))
    }
    else if(input$TransformNumericlist=="ranking"){
      if(input$droporigin){
        v$df<-v$df%>%mutate(across(input$NumericColumns,~rank(.x,ties.method = "first")))
      }
      v$df<-v$df%>%mutate(across(input$NumericColumns,~rank(.x,ties.method = "first"),.names = "ranked_{.col}"))
    }
    else if(input$TransformNumericlist=="binning"){
      if(input$droporigin){
        v$df<-v$df%>%mutate(across(input$NumericColumns,~ntile(.x,n=input$bins)))
      }
      v$df<-v$df%>%mutate(across(input$NumericColumns,~ntile(.x,n=input$bins),.names = "binned_{.col}"))
    }
  })
  
  observeEvent(input$TransformCategorical,{
    if(input$TransformCategoricallist=="onehotencoding"){
      v$df<-dummy_cols(v$df,select_columns = input$CategoricalColumns,
                       remove_first_dummy = input$remove_one_dummy,
                       remove_selected_columns = input$remove_original_cat)
    }
    else{
      before=unlist(str_split(input$CategoryBefore,","))
      v$df<-v$df%>%
        mutate(across(input$CategoricalColumns,~plyr::mapvalues(.x,from=before,to=rep(input$CategoryAfter,length(before)))))
    }
  })
  
  output$PostTransform<-renderDataTable({
    req(v$df)
    return(v$df)
  })
  
  output$histograms<-renderUI({
    explore<-split_columns(v$df,binary_as_factor = T)
    pagesHistogram<-ceiling(explore$num_continuous/16)
    
    lapply(1:pagesHistogram, function(i){
      histId <- paste0("histplot_", i)
      plotOutput(histId)
      
      output[[histId]]<-renderPlot({
        pagenum<-paste0("page_",i)
        plottitle<-paste("Histograms Page",i)
        return(plot_histogram(v$df,title = plottitle, theme_config=list("plot.title" = element_text(hjust = 0.5)))[[pagenum]])
      })
    })
  })
  
  output$frequencies<-renderUI({
    explore<-split_columns(v$df,binary_as_factor = T)
    pagesFrequencies<-ceiling(explore$num_discrete/9)
    
    lapply(1:pagesFrequencies, function(i){
      freqId <- paste0("freqplot_", i)
      plotOutput(freqId)
      
      output[[freqId]]<-renderPlot({
        pagenum<-paste0("page_",i)
        plottitle<-paste("Frequency Plots Page",i)
        return(plot_bar(v$df,title = plottitle, theme_config=list("plot.title" = element_text(hjust = 0.5)))[[pagenum]])
      })
    })
  })
  
  output$DLTransform <- downloadHandler(
    filename = "dataset_cleaned.csv",
    content = function(file) {
      write.csv(v$df, file, row.names = FALSE)
    }
  )
  
  #Advanced Options - Advanced Imputation
  output$miceconsole<-renderUI({
    req(input$ImputePackages)
    if(input$ImputePackages=="MICE"){
      h3("More customizability under construction!")
    }
  })
  output$missforestconsole<-renderUI({
    req(input$ImputePackages)
    if(input$ImputePackages=="missForest"){
      h3("More customizability under construction!")
    }
  })
  output$ameliaconsole<-renderUI({
    req(input$ImputePackages)
    if(input$ImputePackages=="Amelia"){
      h3("More customizability under construction!")
    }
  })
  
  observeEvent(input$AdvancedImpute,{
    req(v$df)
    req(input$ImputePackages)
    if(input$ImputePackages=="MICE"){
      missinglist=colSums(is.na(v$df))
      methods=ifelse(missinglist>0,"pmm","")
      mice_data=mice(v$df,meth=methods)
      v$df<-complete(mice_data)
    }
    else if(input$ImputePackages=="missForest"){
      v$df<-missForest(v$df)$ximp
    }
    else{
      onlycategorical=Filter(is.factor,v$df)
      amelia_data=amelia(v$df,m=1,noms=names(onlycategorical))
      v$df<-amelia_data$imputations$imp1
    }
  })
  
  output$PostAdvImp<-renderDataTable({
    req(v$df)
    return(v$df)
  })
  
  output$DLAdvancedImpute <- downloadHandler(
    filename = "dataset_cleaned.csv",
    content = function(file) {
      write.csv(v$df, file, row.names = FALSE)
    }
  )
  
  #Advanced Options - Dimensionality Reduction  
  output$PCAconsole<-renderUI({
    req(input$DimRedmethod)
    if(input$DimRedmethod=="PCA"){
      req(v$df)
      fluidRow(column(12,selectInput("targetPCA","Please Identify the Target Variable",
                                     choices = names(v$df))),
               column(12,uiOutput("PCAcomponentType")),
               column(12,uiOutput("PCAcriterion")))
    }
  })
  output$PCAcomponentType<-renderUI({
    req(input$targetPCA)
    radioButtons("manualautopca","Please Choose Either Manual or Auto PCA:",
                 choices = c("manual"="manual","auto"="auto"))
  })
  output$PCAcriterion<-renderUI({
    req(input$targetPCA)
    req(input$manualautopca)
    if(input$manualautopca=="manual"){
      textInput("manualpcanumber","Please enter the number of PCA components:")
    }
    else{
      textInput("autopcapercent","Please enter the percentage of variance PCA components should cover:",
                placeholder = "Please input percentage as a decimal")
    }
  })
  output$LDAconsole<-renderUI({
    req(input$DimRedmethod)
    if(input$DimRedmethod=="LDA"){
      req(v$df)
      selectInput("targetLDA","Please Identify the Target Variable",
                  choices = names(v$df))
    }
  })
  output$ICAconsole<-renderUI({
    req(input$DimRedmethod)
    if(input$DimRedmethod=="ICA"){
      req(v$df)
      textInput("icanumber","Please enter the number of ICA components:")
    }
  })
  output$tSNEconsole<-renderUI({
    req(input$DimRedmethod)
    if(input$DimRedmethod=="tSNE"){
      req(v$df)
      textInput("tsnenumber","Please enter the number of t-SNE components:")
    }
  })
  
  red<-reactiveValues(df=NULL)
  observeEvent(input$DimReduce,{
    req(v$df)
    if(input$DimRedmethod=="PCA"){
      req(input$targetPCA)
      data<-v$df%>%
        dplyr::select(-input$targetPCA)
      onlynumeric<-Filter(is.numeric,data)
      req(input$manualautopca)
      if(input$manualautopca=="manual"){
        pca=prcomp(onlynumeric,center = T,scale. = T,rank. = as.numeric(input$manualpcanumber))
        reduced_data=data.frame(pca$x)
        red$df<-reduced_data
      }
      else{
        pca=prcomp(onlynumeric,center = T,scale. = T)
        prop <- pca$sdev^2 / sum(pca$sdev^2)
        compnum=which(cumsum(prop) >= as.numeric(input$autopcapercent))[1]
        reduced_data=data.frame(pca$x[,1:compnum])
        red$df<-reduced_data
      }
    }
    else if(input$DimRedmethod=="LDA"){
      req(input$targetLDA)
      onlynumeric<-Filter(is.numeric,v$df)
      onlynumeric<-base::cbind(onlynumeric,v$df[input$targetLDA])
      f=paste0(input$targetLDA,"~.")
      lda=lda(as.formula(f),data=onlynumeric)
      reduced_data=data.frame(predict(lda,onlynumeric)$x)
      red$df<-reduced_data
    }
    else if(input$DimRedmethod=="ICA"){
      onlynumeric<-Filter(is.numeric,v$df)
      ica=ica::icafast(onlynumeric,as.numeric(input$icanumber))
      reduced_data=data.frame(ica$Y)
      red$df<-reduced_data
    }
    else if(input$DimRedmethod=="tSNE"){
      onlynumeric<-Filter(is.numeric,v$df)
      tsne<- Rtsne(onlynumeric,dims=as.numeric(input$tsnenumber),pca=FALSE,theta=0.0)
      reduced_data=data.frame(tsne$Y)
      red$df<-reduced_data
    }
    output$ReducedData<-renderDataTable({
      req(reduced_data)
      return(reduced_data)
    })
  })
  
  output$DLReducedData <- downloadHandler(
    filename = "reduced_dataset.csv",
    content = function(file) {
      write.csv(red$df,file, row.names = FALSE)
    }
  )
  
  #advanced options - imbalanced classes     # ovun.sample does not work;
  output$imbclasstarget<-renderUI({
    req(v$df)
    selectInput("imbtarget","Please Identify the Target Variable",
                choices = names(v$df))
  })
  output$roseconsole<-renderUI({
    if(input$Imbalancedmethod=="rose"){
      fluidRow(column(12,selectInput("rosemethod","Select a resampling method from the ROSE library:",
                                     choices = c("over","under","both","ROSE"))),
               column(12,textInput("roseResampProb","Please enter the probability of resampling from the rare class:",
                                   placeholder = "Enter probability as a decimal.")),
               column(12,textInput("roseSeed","Please enter a seed number",
                                   placeholder = "default:1")),
               column(12,h5("feature under construction! only rose method works.")))
    }
  })
  output$smoteconsole<-renderUI({
    if(input$Imbalancedmethod=="smote"){
      fluidRow(column(12,selectInput("smotemethod","Select a resampling method from the smotefamily library:",
                                     choices = c("ADASYN","Borderline","DBSmote","SMOTE"))),
               column(12,textInput("smoteK","Please enter the number of neighbors used to generate synthetic cases:",
                                   placeholder = "default:5")))
    }
  })
  
  res<-reactiveValues(df=NULL)
  observeEvent(input$imbclassresamp,{
    req(v$df)
    req(input$imbtarget)
    onlynumeric<-Filter(is.numeric,v$df)
    onlynumeric<-base::cbind(onlynumeric,v$df[input$imbtarget])
    if(input$Imbalancedmethod=="rose"){
      req(input$rosemethod)
      req(input$roseResampProb)
      req(input$roseSeed)
      f=paste0(input$imbtarget,"~.")
      if(input$rosemethod=="ROSE"){
        resampled_data<-ROSE(as.formula(f),data = onlynumeric,p=as.numeric(input$roseResampProb),
                             seed=as.integer(input$roseSeed))$data
        res$df<-resampled_data
      }
      else if(input$rosemethod %in% c("over","under","both")){
        resampled_data<-ovun.sample(as.formula(f),data = onlynumeric,p=as.numeric(input$roseResampProb),
                                    seed=as.integer(input$roseSeed),method = input$rosemethod)$data
        res$df<-resampled_data
      }
    }
    else{
      onlynumeric<-Filter(is.numeric,v$df)
      if(input$smotemethod=="ADASYN"){
        resampled_data<-ADAS(onlynumeric,v$df[input$imbtarget],K=as.integer(input$smoteK))$data
      }
      else if(input$smotemethod=="Borderline"){
        resampled_data<-BLSMOTE(onlynumeric,v$df[input$imbtarget],K=as.integer(input$smoteK),C=as.integer(input$smoteK))$data
      }
      else if(input$smotemethod=="DBSmote"){
        resampled_data<-DBSMOTE(onlynumeric,v$df[input$imbtarget])$data
      }
      else{
        resampled_data<-SMOTE(onlynumeric,v$df[input$imbtarget],K=as.integer(input$smoteK))$data
      }
      names(resampled_data)[which(names(resampled_data)=="class")]<-input$imbtarget
      res$df<-resampled_data
    }
    
    output$ResampledData<-renderDataTable({
      req(resampled_data)
      return(resampled_data)
    })
    
    output$afterResample<-renderPlot({
      req(resampled_data)
      req(input$imbtarget)
      counts=table(resampled_data[[input$imbtarget]])
      barplot(counts,main="Distribution of Target After Resampling",ylab ="counts",xlab="classes")
    })
  })
  
  output$beforeResample<-renderPlot({
    req(v$df)
    req(input$imbtarget)
    counts1<-table(v$df[[input$imbtarget]])
    barplot(counts1,main="Distribution of Target Before Resampling",ylab ="counts",xlab="classes")
  })
  
  output$DLReducedData <- downloadHandler(
    filename = "resampled_dataset.csv",
    content = function(file) {
      write.csv(res$df,file, row.names = FALSE)
    }
  )
  
  #Advanced options - Merged Datasets
  v2<-reactiveValues(df=NULL)
  
  observeEvent(input$upload2, {
    v2$df <- read.csv(input$upload2$datapath,
                     header = input$header2,
                     sep = input$sep2,
                     stringsAsFactors = input$stringfactor2)
  })
  
  output$first_dataset <- renderDataTable({
    req(v$df)
    return(v$df)
  })
  output$second_dataset <- renderDataTable({
    req(v2$df)
    return(v2$df)
  })
  
  output$JoinCondition<-renderUI({
    selectInput("typeofJoin","Select the Type of Join:",
                choices = c("left","right","inner","cross"))
  })
  output$JoinonA<-renderUI({
    req(v$df)
    selectInput("keyofA","Select the Key to Join On from Dataset 1:",
                choices = names(v$df))
  })
  output$JoinonB<-renderUI({
    req(v2$df)
    selectInput("keyofB","Select the Key to Join On from Dataset 2:",
                choices = names(v2$df))
  })
  
  v3<-reactiveValues(df=NULL)
  observeEvent(input$mergedata,{
    req(v$df)
    req(v2$df)
    req(input$typeofJoin)
    req(input$keyofA)
    req(input$keyofB)
    if(input$typeofJoin=="cross"){
      jointdata<-merge(v$df,v2$df,by=NULL)
      v3$df<-jointdata
    }
    else if(input$typeofJoin=="inner"){
      jointdata<-merge(v$df,v2$df,all=T,by.x=input$keyofA,by.y=input$keyofB)
      v3$df<-jointdata
    }
    else if(input$typeofJoin=="left"){
      jointdata<-merge(v$df,v2$df,all.x=T,by.x=input$keyofA,by.y=input$keyofB)
      v3$df<-jointdata
    }
    else{
      jointdata<-merge(v$df,v2$df,all.y=T,by.x=input$keyofA,by.y=input$keyofB)
      v3$df<-jointdata
    }
    output$merged_dataset<-renderDataTable({
      req(jointdata)
      return(jointdata)
    })
  })
  
  output$DLMergedData <- downloadHandler(
    filename = "merged_dataset.csv",
    content = function(file) {
      write.csv(res$df,file, row.names = FALSE)
    }
  )
  

  #Advanced Options - Rolling Windows
  output$windowOptions<-renderUI({
    req(v$df)
    selectInput("rollonColumn","Select a column to make a rolling window:",
                choice=names(v$df))
  })
  observeEvent(input$rollingTransform,{
    req(v$df)
    req(input$rollingfunctions)
    req(input$windowsize)
    if(input$rollingfunctions=="mean"){
      v$df<-v$df%>%
        mutate(across(input$rollonColumn,~slide_dbl(.,.f=mean,.before=as.integer(input$windowsize)),.names="mean_{.col}"))
    }
    else if(input$rollingfunctions=="median"){
      v$df<-v$df%>%
        mutate(across(input$rollonColumn,~slide_dbl(.,.f=median,.before=as.integer(input$windowsize)),.names="median_{.col}"))
    }
    else if(input$rollingfunctions=="min"){
      v$df<-v$df%>%
        mutate(across(input$rollonColumn,~slide_dbl(.,.f=min,.before=as.integer(input$windowsize)),.names="min_{.col}"))
    }
    else if(input$rollingfunctions=="max"){
      v$df<-v$df%>%
        mutate(across(input$rollonColumn,~slide_dbl(.,.f=min,.before=as.integer(input$windowsize)),.names="max_{.col}"))
    }
    else if(input$rollingfunctions=="sum"){
      v$df<-v$df%>%
        mutate(across(input$rollonColumn,~slide_dbl(.,.f=sum,.before=as.integer(input$windowsize)),.names="sum_{.col}"))
    }
    else if(input$rollingfunctions=="sd"){
      v$df<-v$df%>%
        mutate(across(input$rollonColumn,~slide_dbl(.,.f=sd,.before=as.integer(input$windowsize)),.names="sd_{.col}"))
    }
    else{
      v$df<-v$df%>%
        mutate(across(input$rollonColumn,~slide_dbl(.,.f=IQR,.before=as.integer(input$windowsize)),.names="iqr_{.col}"))
    }
    output$PostTimeFunctions<-renderDataTable({
      req(v$df)
      return(v$df)
    })
  })
  
  #Advanced options - Datetime Functions
  output$datetimeColumns<-renderUI({
    req(v$df)
    onlydatetime<-Filter(IsDate,v$df)
    checkboxGroupInput("datetimeColumns","Datetime Columns:",
                       choices = names(onlydatetime))
  })
  output$datetimeConsole<-renderUI({
    req(v$df)
    req(input$datetimeFunctions)
    if(input$datetimeFunctions=="Extract"){
      selectInput("extractedValues","Select the Datetime entities to extract:",
                  choices = c("year","semester","quarter","month","monthname","week","day","dayofweek","hour","minute","second"))
    }
    else{
      selectInput("timeunits","Select the units duration intervals should be expressed in:",
                  choices = c("years","months","weeks","days","hours","minutes","seconds"))
    }
  })
  
  observeEvent(input$datetimeTransform,{
    req(v$df)
    req(input$datetimeColumns)
    req(input$extractedValues)
    req(input$timeunits)
    req(input$datetimeFunctions)
    if(input$datetimeFunctions=="Extract"){
      if(input$extractedValues=="year"){
        v$df<-v$df%>%mutate(across(input$datetimeColumns,lubridate::year,.names="year_{.col}"))
      }
      else if(input$extractedValues=="semester"){
        v$df<-v$df%>%mutate(across(input$datetimeColumns,lubridate::semester,.names="semester_{.col}"))
      }
      else if(input$extractedValues=="quarter"){
        v$df<-v$df%>%mutate(across(input$datetimeColumns,lubridate::quarter,.names="quarter_{.col}"))
      }
      else if(input$extractedValues=="month"){
        v$df<-v$df%>%mutate(across(input$datetimeColumns,lubridate::month,.names="month_{.col}"))
      }
      else if(input$extractedValues=="monthname"){
        v$df<-v$df%>%mutate(across(input$datetimeColumns,~lubridate::month(.x,label=T),.names="day_{.col}"))
      }
      else if(input$extractedValues=="week"){
        v$df<-v$df%>%mutate(across(input$datetimeColumns,lubridate::week,.names="week_{.col}"))
      }
      else if(input$extractedValues=="day"){
        v$df<-v$df%>%mutate(across(input$datetimeColumns,lubridate::day,.names="day_{.col}"))
      }
      else if(input$extractedValues=="dayofweek"){
        v$df<-v$df%>%mutate(across(input$datetimeColumns,lubridate::wday(.,label = T),.names="dow_{.col}"))
      }
      else if(input$extractedValues=="hour"){
        v$df<-v$df%>%mutate(across(input$datetimeColumns,lubridate::hour,.names="hour_{.col}"))
      }
      else if(input$extractedValues=="minute"){
        v$df<-v$df%>%mutate(across(input$datetimeColumns,lubridate::minute,.names="minute_{.col}"))
      }
      else{
        v$df<-v$df%>%mutate(across(input$datetimeColumns,lubridate::second,.names="second_{.col}"))
      }
    }  
    else{
      if(length(input$datetimeColumns)==2){
        v$df<-v$df%>%
          mutate(duration=as.numeric(abs(difftime(v$df[[input$datetimeColumns[1]]],v$df[[input$datetimeColumns[2]]])),units=input$timeunits))
      }
    }
    output$PostTimeFunctions<-renderDataTable({
      req(v$df)
      return(v$df)
    })
  })
  
  output$DLtimefunctions <- downloadHandler(
    filename = "cleaned_dataset.csv",
    content = function(file) {
      write.csv(v$df,file, row.names = FALSE)
    }
  )
}

shinyApp(ui,server)
