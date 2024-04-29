Logged = FALSE;
PASSWORD <- data.frame(UN = c("u"),
                       Pass = c("p"))

function(input,output,session){
  
  source("www/Login.R", local = TRUE)  
  
  observeEvent(USER$Logged ,{
    
    if (USER$Logged == TRUE) {
      
      output$menu <- renderMenu({
        sidebarMenu(id="tabs",
                    menuItem("Similarity Analysis",tabName = "similarity_analysis") 
                    # actionButton('logout',label = tags$b('Log Out'),icon("sign-out"))
        )  
        
        
        
      })
      
      
      
      matrix_data<-matrix(c("Unit","Form","Columns 3 to n number of columns","Unit No.","This column represents the formulation of the tablet, which should be coded as 'Ref' and 'Test'. It indicates whether the dissolution measurements were performed on a 'Ref' or 'Test' version of the tablet.","These columns represent the percent dissolution at various time points. The column names indicate the specific time in minutes (e.g., T5 represents the measurement taken at 5 minutes). The number of columns may vary."),nrow=3,ncol=2)
      df<-as.data.frame(matrix_data)
      setnames(df,old=c('V1','V2'),new=c('Columns','Description'))
      output$desctable<-renderTable({df})
      
output$objtable<-renderText({
        paste("The objective is to compare the percent dissolution of reference and test formulations at various time points. This application provides:",
              "<br>",
              "a) Average dissolution profiles for test and reference formulations",
              "<br>",
              "b) Graphical view of Dissolution Summary",
              "<br>",
              "c) Estimate of Similarity Factor F2",
              "<br>",
              "d) 90% Confidence Interval")
      })
 
output$value_note<-renderText({
        paste("Note : For the above table dissolution % time starts from column 3 to last column")
      })
      
imported_data<-reactive({
    req(input$file)
    datafile<-read.csv(input$file$datapath,header=TRUE)
    return(datafile)
  })

imported_data_dnld<-reactive({
  df<-imported_data()
  names(df)[3:ncol(df)]<-paste0(as.numeric(gsub(".*?([0-9]+).*", "\\1", names(df)[3:ncol(df)]))," Min")
  return(df)
})

summstats_test<-reactive({
  original<-imported_data()
  trtlist<-names(original)[3:ncol(original)]
  cv <- function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))*100
  
  N_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], length)
  mean_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], mean)
  sd_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], sd)
  min_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], min)
  max_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], max)
  cv_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], cv)
  summstats<-rbind(N_summ,mean_summ,sd_summ,cv_summ,min_summ,max_summ)
  
  summstats_test<-subset(summstats,Form=="Test")
  
  
 
  summstats_test <- transpose(summstats_test)
  
  colnames(summstats_test) <- c("N","Mean","Std Dev","Coeff of Variation","Minimum","Maximum")
  summstats_test[, sapply(summstats_test, is.character)] <- apply(summstats_test[, sapply(summstats_test, is.character)], 2, as.numeric)
  
  summstats_test$Variable<-names(summstats)
  summstats_test <- summstats_test[-1,c(ncol(summstats_test),2:(ncol(summstats_test)-1))]
  summstats_test <- summstats_test %>% 
    mutate_if(is.numeric, round, 2)
  return(summstats_test)
  
})

summstats_ref<-reactive({
  original<-imported_data()
  trtlist<-names(original)[3:ncol(original)]
  cv <- function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))*100
  
  N_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], length)
  mean_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], mean)
  sd_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], sd)
  min_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], min)
  max_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], max)
  cv_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], cv)
  summstats<-rbind(N_summ,mean_summ,sd_summ,cv_summ,min_summ,max_summ)
  
  summstats_ref<-subset(summstats,Form=="Ref")

  summstats_ref <- transpose(summstats_ref)
  
  colnames(summstats_ref) <- c("N","Mean","Std Dev","Coeff of Variation","Minimum","Maximum")
  summstats_ref[, sapply(summstats_ref, is.character)] <- apply(summstats_ref[, sapply(summstats_ref, is.character)], 2, as.numeric)
  
  summstats_ref$Variable<-names(summstats)
  summstats_ref <- summstats_ref[-1,c(ncol(summstats_ref),2:(ncol(summstats_ref)-1))]
  
  summstats_ref <- summstats_ref %>% 
    mutate_if(is.numeric, round, 2)
  return(summstats_ref)
  
})




observeEvent(input$file,{
  data_med<-imported_data()
  if(is.integer(data_med[[1]]) && is.character(data_med[[2]]) && all(sapply(data_med[c(3:ncol(data_med))], is.numeric))){
    shinyalert("Uploaded Successfully. Please proceed further.")
  } else {
    shinyalert("Upload Unsuccessful. Please import data in correct format.")
  }
  
}) 

uploaded_status <- reactiveVal(NULL)

observeEvent(input$file, {
  data_med <- imported_data()
  if (is.integer(data_med[[1]]) && is.character(data_med[[2]]) && all(sapply(data_med[c(3:ncol(data_med))], is.numeric))) {
    uploaded_status("Uploaded Successful")
  } else {
    uploaded_status("Upload Unsuccessful")
  }
})

observe({
  req(uploaded_status()) 
  
  if (uploaded_status() == "Uploaded Successful") {

df_data<-reactive({
    # if (input$excludebutton>0){
      # data_med<-imported_data2()
    # } else {
      data_med<-imported_data()
    # }
    #Converting to long format
    library(dplyr)
    data_med_melt<-melt(data_med,id.vars = c("Form","Unit"),variable.name = "Time",value.name = "% Dissolution")
    
    #Getting mean,max,sd, min
    df_data<-data_med_melt%>%group_by(Form,Time)%>%summarise(Mean=mean(`% Dissolution`,na.rm=TRUE),
                                                             Min=min(`% Dissolution`,na.rm=TRUE),
                                                             Max=max(`% Dissolution`,na.rm = TRUE),
                                                             SD=sd(`% Dissolution`,na.rm=TRUE))
    
    df_data<-as.data.frame(df_data)
    df_data$CV<-(df_data$SD/df_data$Mean)*100
    df_data<-df_data[order(df_data$Time),]
    
    
    return(df_data)
  })
  
output$table<-renderTable({
      if (input$similarity_analysis>0){
      df_data<-df_data()
      df_data
      }
      }) 

output$plot_graph<-renderPlot({ 
      
      if (input$similarity_analysis>0){
          # if (input$excludebutton>0){
            # data_med<-imported_data2()
          # } else {
            data_med<-imported_data()
          # }
            f<-function(x)(mean(x,na.rm=TRUE))
      Agg_data_med<-aggregate(data_med[,3:ncol(data_med)], by=list(data_med$Form), FUN=f)
      #Converting the aggregating data into long format
      Graph_data<-melt(Agg_data_med)
      
      setnames(Graph_data,old=c('Group.1','variable','value'),new=c('Form','Time','Ref_Test'))
      
      #Creating graphs
        
        if(input$plot_graph=="Trendline"){
        ggplot(Graph_data,aes(x=Time,y=Ref_Test,group=Form,color=Form))+geom_line()+geom_point(size=3)+scale_color_manual(values = c("#009966","#0033CC"))+
            labs(y="% Dissolution",title="Trendline of % Dissolution for Ref and Test")+theme(plot.title = element_text(face = "bold",size=20),
                                                                                     axis.title = element_text(size = 17),axis.text = element_text(size = 15))
        }
        else if(input$plot_graph=="Bar Chart"){
          ggplot(Graph_data,aes(x=Time,y=Ref_Test))+geom_bar(stat="identity",aes(fill=Form),position="dodge")+scale_fill_manual(values = c("#009966","#0033CC"))+
            labs(x="Time",y="% Dissolution",title="Bar Chart of % Dissolution for Ref and Test")+theme(plot.title = element_text(face = "bold",size=20),
                                                                                              axis.title = element_text(size = 17),axis.text = element_text(size = 15))
        }
        else{
          data_med<-imported_data()
          data_med_melt<-melt(data_med,id.vars = c("Form","Unit"),variable.name = "Time",value.name = "% Dissolution")
          ggplot(data_med_melt, aes(x = Time, y = `% Dissolution`, fill = Form)) +
            geom_boxplot() +
            scale_fill_manual(values = c("#009966", "#0033CC")) +
            ggtitle("Boxplot of % Dissolution for Ref and Test") +
            theme(plot.title = element_text(face = "bold",size=20),axis.title = element_text(size = 17),axis.text = element_text(size = 15))
        }
      }
  })

f2<-reactive({
    if (input$similarity_analysis>0){
      # if (input$excludebutton>0){
        # data_med<-imported_data2()
      # } else {
        data_med<-imported_data()
      # }
        f<-function(x)(mean(x,na.rm=TRUE))
    Agg_data_med<-aggregate(data_med[,3:ncol(data_med)], by=list(data_med$Form), FUN=f)
    df_data_f2<-as.data.frame(t(Agg_data_med[-1]))
    setnames(df_data_f2,old = c('V1','V2'),new=c('RT','TT'))
    df_data_final_f2<-rownames_to_column(df_data_f2, "Time")
    df_data_final_f2$`RT-TT`<-(df_data_final_f2$RT-df_data_final_f2$TT)
    df_data_final_f2$`(RT-TT)^2`<-(df_data_final_f2$`RT-TT`)^2
    #Calculating Sum of squares of RT-TT
    Sum_sq<-sum(df_data_final_f2$`(RT-TT)^2`)
    
    #Total number of columns from 3rd to last
    nunit<-ncol(data_med[,3:ncol(data_med)])
    #Calculating f2
    f2_calc<-round(50*log10(100*((1+(1/nunit)*Sum_sq))^-0.5),2)
    }
    return(f2_calc)
  })

f2_original<-reactive({
  f2_original<-f2()
  f2_original_df<-data.frame(`Similarity factor(f2)`=f2_original)
  return(f2_original_df)
})

summboot<-reactive({
  original<-imported_data()
  original <- original[order(original$Form),]
  
  a<-data.frame(table(original$Unit))
  a$ID<-c(1:nrow(a))
  a<-a[,c(1,3)]
  setnames(a,"Var1","SUB")
  nsub<-length(unique(original$Unit))
  
  b<-data.frame(sample(original$Unit,size=input$bootsample*nsub,replace = T))
  names(b)<-"ID"
  
  
  BOOTSE <- function(B) {
    ORIG1 <- original[original$Form == 'Ref', ]
    ORIG2 <- original[original$Form == 'Test', ]
    
    BOOT1<-data.frame(sample(ORIG1$Unit,size=B*nsub,replace = T))
    names(BOOT1)<-"Unit"
    for (i in 1:input$bootsample){
      if (i==1){
        BOOT1$Sample[i:nsub]<-i
      } else if (i>1) {
        BOOT1$Sample[(nsub*(i-1)+1):(nsub*i)]<-i
      }
      
      
    }
    
    BOOT1<-merge(BOOT1,ORIG1,by="Unit",all.x = T)
    
    BOOT2<-data.frame(sample(ORIG2$Unit,size=B*nsub,replace = T))
    names(BOOT2)<-"Unit"
    for (i in 1:input$bootsample){
      if (i==1){
        BOOT2$Sample[i:nsub]<-i
      } else if (i>1) {
        BOOT2$Sample[(nsub*(i-1)+1):(nsub*i)]<-i
      }
      
      
    }
    
    BOOT2<-merge(BOOT2,ORIG2,by="Unit",all.x = T)
    BOOT<-rbind(BOOT1,BOOT2)
    
    return(BOOT)
  }
  BOOT <- BOOTSE(input$bootsample)
  BOOT<-BOOT[order(BOOT$Sample,BOOT$Form),]
  trtlist<-names(original)[3:ncol(original)]
  boot_mean <- aggregate(. ~ Sample+Form, data = BOOT[,c("Sample","Form",trtlist)], FUN=mean)
  boot_mean<-boot_mean[order(boot_mean$Sample),]
  
  
  boot_trsp1 <- transpose(boot_mean)
  
  boot_trsp1 <- reshape2::melt(boot_mean, id.vars = c("Sample","Form"), measure.vars = trtlist,
                               variable.name = "Category", value.name = "Value")
  boot_trsp1 <- reshape2::dcast(boot_trsp1, Sample + Category ~ Form, value.var = "Value")
  boot_trsp1$Ref<-as.numeric(boot_trsp1$Ref)
  boot_trsp1$Test<-as.numeric(boot_trsp1$Test)
  
  boot_trsp1$diff <- boot_trsp1$Ref - boot_trsp1$Test
  boot_trsp1$diff_square <- boot_trsp1$diff^2
  
  summation_boot <- aggregate(diff_square~Sample,data=boot_trsp1,FUN=sum)
  
  freq <- length(trtlist)
  summation_boot$f2_boot = 50 * log10(100 * ((1 + (summation_boot$diff_square / freq))^(-0.5)))
  
  return(summation_boot)
  
})

bootstrapdf<-reactive({
  summation_boot<-summboot()
  quartiles<-data.frame(Mean_F2=mean(summation_boot$f2_boot),Median_F2=median(summation_boot$f2_boot),
                        "Lower Limit"=quantile(summation_boot$f2_boot, 0.05),"Upper Limit"=quantile(summation_boot$f2_boot, 0.95))
  setnames(quartiles,old=c('Upper.Limit','Lower.Limit','Mean_F2','Median_F2'),new=c('Upper Limit','Lower Limit','Mean F2','Median F2'))
  
  quartiles$Similarity<-ifelse(quartiles$`Lower Limit`>50,"Yes","No")
  
  return(quartiles)
  
})



cibca<-reactive({
  f2_original<-f2()
  summation_boot<-summboot()
  original<-imported_data()
  trtlist<-names(original)[3:ncol(original)]
  summation_boot$lessthan<-ifelse(summation_boot$f2_boot < f2_original,1,0)
  
  propless <- sum(summation_boot$lessthan)/nrow(summation_boot)
  bias <- qnorm(propless)
  
  origjack <- original
  origjack$obsnum <- 1:nrow(origjack)
  lstjack<-list()
  
  for (i in 1:nrow(origjack)){
    lstjack[[i]]<-origjack[-i,]
    lstjack[[i]]$Sample<-i
  }
  jackdata<-do.call(rbind,lstjack)
  
  jack_mean <- aggregate(. ~ Sample+Form, data  = jackdata[c('Sample','Form',trtlist)], mean)
  jack_mean<-jack_mean[order(jack_mean$Sample),]
  
  
  jack_trsp1 <- reshape2::melt(jack_mean, id.vars = c("Sample","Form"), measure.vars = trtlist,
                               variable.name = "Category", value.name = "Value")
  jack_trsp1 <- reshape2::dcast(jack_trsp1, Sample + Category ~ Form, value.var = "Value")
  jack_trsp1$Ref<-as.numeric(jack_trsp1$Ref)
  jack_trsp1$Test<-as.numeric(jack_trsp1$Test)
  
  jack_trsp1$diff <- jack_trsp1$Ref - jack_trsp1$Test
  jack_trsp1$diff_square <- jack_trsp1$diff^2
  
  summation_jack <- aggregate(diff_square~Sample,data=jack_trsp1,FUN=sum)
  
  freq <- length(trtlist)
  summation_jack$f2 = 50 * log10(100 * ((1 + (summation_jack$diff_square / freq))^(-0.5)))
  summation_jack$cubed<-(mean(summation_jack$f2)-summation_jack$f2)^3
  summation_jack$squared<-(mean(summation_jack$f2)-summation_jack$f2)^2
  
  
  accel<-sum(summation_jack$cubed)/(6*(sum(summation_jack$squared))^1.5)
  
  part1<-(bias + qnorm(0.05)) / (1 - (accel*(bias + qnorm(0.05))))
  part2<-(bias + qnorm(0.95)) / (1 - (accel*(bias + qnorm(0.95))))
  alpha1<-pnorm(bias+part1)
  alpha2<-pnorm(bias+part2)
  n1<-alpha1*input$bootsample
  n2<-alpha2*input$bootsample
  
  n1<-ifelse(floor(n1)==0,1,n1)
  
  summation_boot<-summation_boot[order(summation_boot$f2_boot),]
  
  
  ci_bca<-data.frame("Lower Limit"=summation_boot$f2_boot[floor(n1)],"Upper Limit"=summation_boot$f2_boot[ceiling(n2)])
  setnames(ci_bca,old=c('Upper.Limit','Lower.Limit'),new=c('Upper Limit','Lower Limit'))
  ci_bca$Similarity<-ifelse(ci_bca$`Lower Limit`>50,"Yes","No")
  
  return(ci_bca)
  
})

quartiles2<-reactive({
  quartiles2<-bootstrapdf()
  # quartiles2<-quartiles[,c(1,3,4,2,5)]
  names(quartiles2)<-c("Mean f2","Median f2","Lower Confidence limit","Upper Confidence limit","Similarity")
  return(quartiles2)
})

cibca2<-reactive({
  ci_bca2<-cibca()
  names(ci_bca2)<-c("Lower Confidence limit","Upper Confidence limit","Similarity")
  return(ci_bca2)
})

observeEvent(input$btsbutton,{
  
  output$bootstrapdt<-renderTable({
    # if (input$similarity_analysis>0){
    
    bootstrapdf<-bootstrapdf()
    bootstrapdf
    # }
  }) 
  
  
  output$cibcadt<-renderTable({
    # if (input$similarity_analysis>0){
    cibca<-cibca()
    cibca
    # }
  })
  
})

observeEvent(input$calculated_value_f2,{
    calculated_value_f2<-f2()
    output$value<-renderText({
      paste("The similarity factor is",calculated_value_f2)
    })
  })
  
observeEvent(input$excludebutton,{
 
     
imported_data2<-reactive({
      # req(input$file)
      datafile<-imported_data()
      data_med<-datafile
      data_med_melt<-melt(data_med,id.vars = c("Form","Unit"),variable.name = "Time",value.name = "% Dissolution")
      
      #Getting mean,max,sd, min
      df_data<-data_med_melt%>%group_by(Form,Time)%>%summarise(Mean=mean(`% Dissolution`,na.rm=TRUE),
                                                               Min=min(`% Dissolution`,na.rm=TRUE),
                                                               Max=max(`% Dissolution`,na.rm = TRUE),
                                                               SD=sd(`% Dissolution`,na.rm=TRUE))
      
      df_data<-as.data.frame(df_data)
      df_data$CV<-(df_data$SD/df_data$Mean)*100
      df_data<-df_data[order(df_data$Time),]
      
      # if (input$excludebutton>0){
      texc<-as.character(df_data$Time[which(df_data$Mean>=85)[1]])
      
      if(is.na(texc)){
        datafile<-datafile
      }else{
        datafile<-subset(datafile, select = 1:which(colnames(datafile) == texc))
      }
      
      # } 
      return(datafile)
    })

imported_data_dnld<-reactive({
  df<-imported_data2()
  names(df)[3:ncol(df)]<-paste0(as.numeric(gsub(".*?([0-9]+).*", "\\1", names(df)[3:ncol(df)]))," Min")
  return(df)
})  

df_data<-reactive({
      # if (input$excludebutton>0){
      data_med<-imported_data2()
      # } else {
      #   data_med<-imported_data()
      # }
      #Converting to long format
      library(dplyr)
      data_med_melt<-melt(data_med,id.vars = c("Form","Unit"),variable.name = "Time",value.name = "% Dissolution")
      
      #Getting mean,max,sd, min
      df_data<-data_med_melt%>%group_by(Form,Time)%>%summarise(Mean=mean(`% Dissolution`,na.rm=TRUE),
                                                               Min=min(`% Dissolution`,na.rm=TRUE),
                                                               Max=max(`% Dissolution`,na.rm = TRUE),
                                                               SD=sd(`% Dissolution`,na.rm=TRUE))
      
      df_data<-as.data.frame(df_data)
      df_data$CV<-(df_data$SD/df_data$Mean)*100
      df_data<-df_data[order(df_data$Time),]
      
      
      return(df_data)
    })
    
output$table<-renderTable({
      if (input$similarity_analysis>0){
        df_data<-df_data()
        df_data
      }
    })
    
output$plot_graph<-renderPlot({ 
      
      if (input$similarity_analysis>0){
        if (input$excludebutton>0){
          data_med<-imported_data2()
        } else {
          data_med<-imported_data()
        }
        f<-function(x)(mean(x,na.rm=TRUE))
        Agg_data_med<-aggregate(data_med[,3:ncol(data_med)], by=list(data_med$Form), FUN=f)
        #Converting the aggregating data into long format
        Graph_data<-melt(Agg_data_med)
        
        setnames(Graph_data,old=c('Group.1','variable','value'),new=c('Form','Time','Ref_Test'))
        
        #Creating graphs
        
        if(input$plot_graph=="Trendline"){
          ggplot(Graph_data,aes(x=Time,y=Ref_Test,group=Form,color=Form))+geom_line()+geom_point(size=3)+scale_color_manual(values = c("#009966","#0033CC"))+
            labs(y="% Dissolution",title="Trendline of % Dissolution for Ref and Test")+theme(plot.title = element_text(face = "bold",size=20),
                                                                                     axis.title = element_text(size = 17),axis.text = element_text(size = 15))
        }
        else if(input$plot_graph=="Bar Chart"){
          ggplot(Graph_data,aes(x=Time,y=Ref_Test))+geom_bar(stat="identity",aes(fill=Form),position="dodge")+scale_fill_manual(values = c("#009966","#0033CC"))+
            labs(x="Time",y="% Dissolution",title="Bar Chart of % Dissolution for Ref and Test")+theme(plot.title = element_text(face = "bold",size=20),
                                                                                              axis.title = element_text(size = 17),axis.text = element_text(size = 15))
        }
        else{
          data_med<-imported_data2()
          data_med_melt<-melt(data_med,id.vars = c("Form","Unit"),variable.name = "Time",value.name = "% Dissolution")
          ggplot(data_med_melt, aes(x = Time, y = `% Dissolution`, fill = Form)) +
            geom_boxplot() +
            scale_fill_manual(values = c("#009966", "#0033CC")) +
            ggtitle("Boxplot of % Dissolution for Ref and Test") +
            theme(plot.title = element_text(face = "bold",size=20),axis.title = element_text(size = 17),axis.text = element_text(size = 15))
        }
      }
      
    })

f2<-reactive({
      if (input$similarity_analysis>0){
        if (input$excludebutton>0){
          data_med<-imported_data2()
        } else {
          data_med<-imported_data()
        }
        f<-function(x)(mean(x,na.rm=TRUE))
        Agg_data_med<-aggregate(data_med[,3:ncol(data_med)], by=list(data_med$Form), FUN=f)
        df_data_f2<-as.data.frame(t(Agg_data_med[-1]))
        setnames(df_data_f2,old = c('V1','V2'),new=c('RT','TT'))
        df_data_final_f2<-rownames_to_column(df_data_f2, "Time")
        df_data_final_f2$`RT-TT`<-(df_data_final_f2$RT-df_data_final_f2$TT)
        df_data_final_f2$`(RT-TT)^2`<-(df_data_final_f2$`RT-TT`)^2
        #Calculating Sum of squares of RT-TT
        Sum_sq<-sum(df_data_final_f2$`(RT-TT)^2`)
        
        nunit<-ncol(data_med[,3:ncol(data_med)])
        #Calculating f2
        f2_calc<-round(50*log10(100*((1+(1/nunit)*Sum_sq))^-0.5),2)
      }
      return(f2_calc)
    })

observeEvent(input$calculated_value_f2,{
      calculated_value_f2<-f2()
      output$value<-renderText({
        paste("The similarity factor is",calculated_value_f2)
      })
    })


summboot<-reactive({
  original<-imported_data2()
  original <- original[order(original$Form),]
  
  a<-data.frame(table(original$Unit))
  a$ID<-c(1:nrow(a))
  a<-a[,c(1,3)]
  setnames(a,"Var1","SUB")
  nsub<-length(unique(original$Unit))
  
  b<-data.frame(sample(original$Unit,size=input$bootsample*nsub,replace = T))
  names(b)<-"ID"
  
  
  BOOTSE <- function(B) {
    ORIG1 <- original[original$Form == 'Ref', ]
    ORIG2 <- original[original$Form == 'Test', ]
    
    BOOT1<-data.frame(sample(ORIG1$Unit,size=B*nsub,replace = T))
    names(BOOT1)<-"Unit"
    for (i in 1:input$bootsample){
      if (i==1){
        BOOT1$Sample[i:nsub]<-i
      } else if (i>1) {
        BOOT1$Sample[(nsub*(i-1)+1):(nsub*i)]<-i
      }
      
      
    }
    
    BOOT1<-merge(BOOT1,ORIG1,by="Unit",all.x = T)
    
    BOOT2<-data.frame(sample(ORIG2$Unit,size=B*nsub,replace = T))
    names(BOOT2)<-"Unit"
    for (i in 1:input$bootsample){
      if (i==1){
        BOOT2$Sample[i:nsub]<-i
      } else if (i>1) {
        BOOT2$Sample[(nsub*(i-1)+1):(nsub*i)]<-i
      }
      
      
    }
    
    BOOT2<-merge(BOOT2,ORIG2,by="Unit",all.x = T)
    BOOT<-rbind(BOOT1,BOOT2)
    
    return(BOOT)
  }
  BOOT <- BOOTSE(input$bootsample)
  BOOT<-BOOT[order(BOOT$Sample,BOOT$Form),]
  trtlist<-names(original)[3:ncol(original)]
  boot_mean <- aggregate(. ~ Sample+Form, data = BOOT[,c("Sample","Form",trtlist)], FUN=mean)
  boot_mean<-boot_mean[order(boot_mean$Sample),]
  
  
  boot_trsp1 <- transpose(boot_mean)
  
  boot_trsp1 <- reshape2::melt(boot_mean, id.vars = c("Sample","Form"), measure.vars = trtlist,
                               variable.name = "Category", value.name = "Value")
  boot_trsp1 <- reshape2::dcast(boot_trsp1, Sample + Category ~ Form, value.var = "Value")
  boot_trsp1$Ref<-as.numeric(boot_trsp1$Ref)
  boot_trsp1$Test<-as.numeric(boot_trsp1$Test)
  
  boot_trsp1$diff <- boot_trsp1$Ref - boot_trsp1$Test
  boot_trsp1$diff_square <- boot_trsp1$diff^2
  
  summation_boot <- aggregate(diff_square~Sample,data=boot_trsp1,FUN=sum)
  
  freq <- length(trtlist)
  summation_boot$f2_boot = 50 * log10(100 * ((1 + (summation_boot$diff_square / freq))^(-0.5)))
  
  return(summation_boot)
  
})

bootstrapdf<-reactive({
  summation_boot<-summboot()
  quartiles<-data.frame(Mean_F2=mean(summation_boot$f2_boot),Median_F2=median(summation_boot$f2_boot),
                        "Lower Limit"=quantile(summation_boot$f2_boot, 0.05),"Upper Limit"=quantile(summation_boot$f2_boot, 0.95))
  setnames(quartiles,old=c('Upper.Limit','Lower.Limit','Mean_F2','Median_F2'),new=c('Upper Limit','Lower Limit','Mean F2','Median F2'))
  
  quartiles$Similarity<-ifelse(quartiles$`Lower Limit`>50,"Yes","No")
  
  return(quartiles)
  
})


cibca<-reactive({
  f2_original<-f2()
  summation_boot<-summboot()
  original<-imported_data2()
  trtlist<-names(original)[3:ncol(original)]
  summation_boot$lessthan<-ifelse(summation_boot$f2_boot < f2_original,1,0)
  
  propless <- sum(summation_boot$lessthan)/nrow(summation_boot)
  bias <- qnorm(propless)
  
  origjack <- original
  origjack$obsnum <- 1:nrow(origjack)
  lstjack<-list()
  
  for (i in 1:nrow(origjack)){
    lstjack[[i]]<-origjack[-i,]
    lstjack[[i]]$Sample<-i
  }
  jackdata<-do.call(rbind,lstjack)
  
  jack_mean <- aggregate(. ~ Sample+Form, data  = jackdata[c('Sample','Form',trtlist)], mean)
  jack_mean<-jack_mean[order(jack_mean$Sample),]
  
  
  jack_trsp1 <- reshape2::melt(jack_mean, id.vars = c("Sample","Form"), measure.vars = trtlist,
                               variable.name = "Category", value.name = "Value")
  jack_trsp1 <- reshape2::dcast(jack_trsp1, Sample + Category ~ Form, value.var = "Value")
  jack_trsp1$Ref<-as.numeric(jack_trsp1$Ref)
  jack_trsp1$Test<-as.numeric(jack_trsp1$Test)
  
  jack_trsp1$diff <- jack_trsp1$Ref - jack_trsp1$Test
  jack_trsp1$diff_square <- jack_trsp1$diff^2
  
  summation_jack <- aggregate(diff_square~Sample,data=jack_trsp1,FUN=sum)
  
  freq <- length(trtlist)
  summation_jack$f2 = 50 * log10(100 * ((1 + (summation_jack$diff_square / freq))^(-0.5)))
  summation_jack$cubed<-(mean(summation_jack$f2)-summation_jack$f2)^3
  summation_jack$squared<-(mean(summation_jack$f2)-summation_jack$f2)^2
  
  
  accel<-sum(summation_jack$cubed)/(6*(sum(summation_jack$squared))^1.5)
  
  part1<-(bias + qnorm(0.05)) / (1 - (accel*(bias + qnorm(0.05))))
  part2<-(bias + qnorm(0.95)) / (1 - (accel*(bias + qnorm(0.95))))
  alpha1<-pnorm(bias+part1)
  alpha2<-pnorm(bias+part2)
  n1<-alpha1*input$bootsample
  n2<-alpha2*input$bootsample
  
  n1<-ifelse(floor(n1)==0,1,n1)
  
  summation_boot<-summation_boot[order(summation_boot$f2_boot),]
  
  
  ci_bca<-data.frame("Lower Limit"=summation_boot$f2_boot[floor(n1)],"Upper Limit"=summation_boot$f2_boot[ceiling(n2)])
  setnames(ci_bca,old=c('Upper.Limit','Lower.Limit'),new=c('Upper Limit','Lower Limit'))
  ci_bca$Similarity<-ifelse(ci_bca$`Lower Limit`>50,"Yes","No")
  
  return(ci_bca)
  
})
    
f2_original<-reactive({
  f2_original<-f2()
  f2_original_df<-data.frame(`Similarity factor(f2)`=f2_original)
  return(f2_original_df)
})

quartiles2<-reactive({
  quartiles2<-bootstrapdf()
  # quartiles2<-quartiles[,c(1,3,4,2,5)]
  names(quartiles2)<-c("Mean f2","Median f2","Lower Confidence limit","Upper Confidence limit","Similarity")
  return(quartiles2)
})

cibca2<-reactive({
  ci_bca2<-cibca()
  names(ci_bca2)<-c("Lower Confidence limit","Upper Confidence limit","Similarity")
  return(ci_bca2)
})

observeEvent(input$btsbutton,{
  
  output$bootstrapdt<-renderTable({
    # if (input$similarity_analysis>0){
    bootstrapdf<-bootstrapdf()
    bootstrapdf
    # }
  }) 
  
  
  output$cibcadt<-renderTable({
    # if (input$similarity_analysis>0){
    cibca<-cibca()
    cibca
    # }
  })
  
})

summstats_test<-reactive({
  original<-imported_data2()
  trtlist<-names(original)[3:ncol(original)]
  cv <- function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))*100
  
  N_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], length)
  mean_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], mean)
  sd_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], sd)
  min_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], min)
  max_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], max)
  cv_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], cv)
  summstats<-rbind(N_summ,mean_summ,sd_summ,cv_summ,min_summ,max_summ)
  
  summstats_test<-subset(summstats,Form=="Test")
  
  
  
  summstats_test <- transpose(summstats_test)
  
  colnames(summstats_test) <- c("N","Mean","Std Dev","Coeff of Variation","Minimum","Maximum")
  summstats_test[, sapply(summstats_test, is.character)] <- apply(summstats_test[, sapply(summstats_test, is.character)], 2, as.numeric)
  
  summstats_test$Variable<-names(summstats)
  summstats_test <- summstats_test[-1,c(ncol(summstats_test),2:(ncol(summstats_test)-1))]
  summstats_test <- summstats_test %>% 
    mutate_if(is.numeric, round, 2)
  return(summstats_test)
  
})

summstats_ref<-reactive({
  original<-imported_data2()
  trtlist<-names(original)[3:ncol(original)]
  cv <- function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))*100
  
  N_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], length)
  mean_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], mean)
  sd_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], sd)
  min_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], min)
  max_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], max)
  cv_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], cv)
  summstats<-rbind(N_summ,mean_summ,sd_summ,cv_summ,min_summ,max_summ)
  
  summstats_ref<-subset(summstats,Form=="Ref")
  
  summstats_ref <- transpose(summstats_ref)
  
  colnames(summstats_ref) <- c("N","Mean","Std Dev","Coeff of Variation","Minimum","Maximum")
  summstats_ref[, sapply(summstats_ref, is.character)] <- apply(summstats_ref[, sapply(summstats_ref, is.character)], 2, as.numeric)
  
  summstats_ref$Variable<-names(summstats)
  summstats_ref <- summstats_ref[-1,c(ncol(summstats_ref),2:(ncol(summstats_ref)-1))]
  
  summstats_ref <- summstats_ref %>% 
    mutate_if(is.numeric, round, 2)
  return(summstats_ref)
  
})
output$download<-downloadHandler(
  filename = function(){
    paste("Data for Similarity Factor.csv")
  },
  content=function(file){
    write.csv(raw_data(),file, row.names = FALSE)
  },
  contentType = "text/csv"
)

output$downloader <- 
  downloadHandler(
    paste("Report",".pdf",sep = ""),
    content = 
      function(file){ 
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...', value = 0,
                     for (i in 1:15) {
                       incProgress(1/15)})
        
        
        rmarkdown::render(
          input = "report_try.Rmd",
          output_file = file,
          params = list(table1 = imported_data_dnld(),
                        table2 = summstats_ref(),
                        table3 = summstats_test(),
                        table4 = f2_original(),
                        table5 = quartiles2(),
                        table6 = cibca2()
          ))
        readBin(con = file,
                what = "raw",
                n = file.info(file)[, "size"]) %>%
          writeBin(con = file)
      })
})
  
observeEvent(input$refresh,{
    
    
    imported_data<-reactive({
      req(input$file)
      datafile<-read.csv(input$file$datapath,header=TRUE)
      return(datafile)
    })
    
    imported_data_dnld<-reactive({
      df<-imported_data()
      names(df)[3:ncol(df)]<-paste0(as.numeric(gsub(".*?([0-9]+).*", "\\1", names(df)[3:ncol(df)]))," Min")
      return(df)
    })  
    
    summstats_test<-reactive({
      original<-imported_data()
      trtlist<-names(original)[3:ncol(original)]
      cv <- function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))*100
      
      N_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], length)
      mean_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], mean)
      sd_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], sd)
      min_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], min)
      max_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], max)
      cv_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], cv)
      summstats<-rbind(N_summ,mean_summ,sd_summ,cv_summ,min_summ,max_summ)
      
      summstats_test<-subset(summstats,Form=="Test")
      
      
      
      summstats_test <- transpose(summstats_test)
      
      colnames(summstats_test) <- c("N","Mean","Std Dev","Coeff of Variation","Minimum","Maximum")
      summstats_test[, sapply(summstats_test, is.character)] <- apply(summstats_test[, sapply(summstats_test, is.character)], 2, as.numeric)
      
      summstats_test$Variable<-names(summstats)
      summstats_test <- summstats_test[-1,c(ncol(summstats_test),2:(ncol(summstats_test)-1))]
      summstats_test <- summstats_test %>% 
        mutate_if(is.numeric, round, 2)
      return(summstats_test)
      
    })
    
    summstats_ref<-reactive({
      original<-imported_data()
      trtlist<-names(original)[3:ncol(original)]
      cv <- function(x) (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))*100
      
      N_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], length)
      mean_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], mean)
      sd_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], sd)
      min_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], min)
      max_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], max)
      cv_summ <- aggregate(. ~ Form, data = original[,c('Form',trtlist)], cv)
      summstats<-rbind(N_summ,mean_summ,sd_summ,cv_summ,min_summ,max_summ)
      
      summstats_ref<-subset(summstats,Form=="Ref")
      
      summstats_ref <- transpose(summstats_ref)
      
      colnames(summstats_ref) <- c("N","Mean","Std Dev","Coeff of Variation","Minimum","Maximum")
      summstats_ref[, sapply(summstats_ref, is.character)] <- apply(summstats_ref[, sapply(summstats_ref, is.character)], 2, as.numeric)
      
      summstats_ref$Variable<-names(summstats)
      summstats_ref <- summstats_ref[-1,c(ncol(summstats_ref),2:(ncol(summstats_ref)-1))]
      
      summstats_ref <- summstats_ref %>% 
        mutate_if(is.numeric, round, 2)
      return(summstats_ref)
      
    })
    df_data<-reactive({
      # if (input$excludebutton>0){
      # data_med<-imported_data2()
      # } else {
      data_med<-imported_data()
      # }
      #Converting to long format
      library(dplyr)
      data_med_melt<-melt(data_med,id.vars = c("Form","Unit"),variable.name = "Time",value.name = "% Dissolution")
      
      #Getting mean,max,sd, min
      df_data<-data_med_melt%>%group_by(Form,Time)%>%summarise(Mean=mean(`% Dissolution`,na.rm=TRUE),
                                                               Min=min(`% Dissolution`,na.rm=TRUE),
                                                               Max=max(`% Dissolution`,na.rm = TRUE),
                                                               SD=sd(`% Dissolution`,na.rm=TRUE))
      
      df_data<-as.data.frame(df_data)
      df_data$CV<-(df_data$SD/df_data$Mean)*100
      df_data<-df_data[order(df_data$Time),]
      
      
      return(df_data)
    })
    
    
    
    output$table<-renderTable({
      if (input$similarity_analysis>0){
        df_data<-df_data()
        df_data
      }
    })
    
    #Aggregating to get the mean of the table
    output$plot_graph<-renderPlot({ 
      
      if (input$similarity_analysis>0){
        # if (input$excludebutton>0){
        # data_med<-imported_data2()
        # } else {
        data_med<-imported_data()
        # }
        f<-function(x)(mean(x,na.rm=TRUE))
        Agg_data_med<-aggregate(data_med[,3:ncol(data_med)], by=list(data_med$Form), FUN=f)
        #Converting the aggregating data into long format
        Graph_data<-melt(Agg_data_med)
        
        setnames(Graph_data,old=c('Group.1','variable','value'),new=c('Form','Time','Ref_Test'))
        
        #Creating graphs
        
        if(input$plot_graph=="Trendline"){
          ggplot(Graph_data,aes(x=Time,y=Ref_Test,group=Form,color=Form))+geom_line()+geom_point(size=3)+scale_color_manual(values = c("#009966","#0033CC"))+
            labs(y="% Dissolution",title="Trendline of % Dissolution for Ref and Test")+theme(plot.title = element_text(face = "bold",size=20),
                                                                                     axis.title = element_text(size = 17),axis.text = element_text(size = 15))
        }
        else if(input$plot_graph=="Bar Chart"){
          ggplot(Graph_data,aes(x=Time,y=Ref_Test))+geom_bar(stat="identity",aes(fill=Form),position="dodge")+scale_fill_manual(values = c("#009966","#0033CC"))+
            labs(x="Time",y="% Dissolution",title="Bar Chart of % Dissolution for Ref and Test")+theme(plot.title = element_text(face = "bold",size=20),
                                                                                              axis.title = element_text(size = 17),axis.text = element_text(size = 15))
        }
        else{
          data_med<-imported_data()
          data_med_melt<-melt(data_med,id.vars = c("Form","Unit"),variable.name = "Time",value.name = "% Dissolution")
          ggplot(data_med_melt, aes(x = Time, y = `% Dissolution`, fill = Form)) +
            geom_boxplot() +
            scale_fill_manual(values = c("#009966", "#0033CC")) +
            ggtitle("Boxplot of % Dissolution for Ref and Test") +
            theme(plot.title = element_text(face = "bold",size=20),axis.title = element_text(size = 17),axis.text = element_text(size = 15))
        }
      }
    })
    # })
    #Calculating F2
    f2<-reactive({
      if (input$similarity_analysis>0){
        # if (input$excludebutton>0){
        # data_med<-imported_data2()
        # } else {
        data_med<-imported_data()
        # }
        f<-function(x)(mean(x,na.rm=TRUE))
        Agg_data_med<-aggregate(data_med[,3:ncol(data_med)], by=list(data_med$Form), FUN=f)
        df_data_f2<-as.data.frame(t(Agg_data_med[-1]))
        setnames(df_data_f2,old = c('V1','V2'),new=c('RT','TT'))
        df_data_final_f2<-rownames_to_column(df_data_f2, "Time")
        df_data_final_f2$`RT-TT`<-(df_data_final_f2$RT-df_data_final_f2$TT)
        df_data_final_f2$`(RT-TT)^2`<-(df_data_final_f2$`RT-TT`)^2
        #Calculating Sum of squares of RT-TT
        Sum_sq<-sum(df_data_final_f2$`(RT-TT)^2`)
        
        nunit<-ncol(data_med[,3:ncol(data_med)])
        #Calculating f2
        f2_calc<-round(50*log10(100*((1+(1/nunit)*Sum_sq))^-0.5),2)
      }
      return(f2_calc)
    })
    
    observeEvent(input$calculated_value_f2,{
      calculated_value_f2<-f2()
      output$value<-renderText({
        paste("The similarity factor is",calculated_value_f2)
      })
    })
    
    
    summboot<-reactive({
      original<-imported_data()
      original <- original[order(original$Form),]
      
      a<-data.frame(table(original$Unit))
      a$ID<-c(1:nrow(a))
      a<-a[,c(1,3)]
      setnames(a,"Var1","SUB")
      nsub<-length(unique(original$Unit))
      
      b<-data.frame(sample(original$Unit,size=input$bootsample*nsub,replace = T))
      names(b)<-"ID"
      
      
      BOOTSE <- function(B) {
        ORIG1 <- original[original$Form == 'Ref', ]
        ORIG2 <- original[original$Form == 'Test', ]
        
        BOOT1<-data.frame(sample(ORIG1$Unit,size=B*nsub,replace = T))
        names(BOOT1)<-"Unit"
        for (i in 1:input$bootsample){
          if (i==1){
            BOOT1$Sample[i:nsub]<-i
          } else if (i>1) {
            BOOT1$Sample[(nsub*(i-1)+1):(nsub*i)]<-i
          }
          
          
        }
        
        BOOT1<-merge(BOOT1,ORIG1,by="Unit",all.x = T)
        
        BOOT2<-data.frame(sample(ORIG2$Unit,size=B*nsub,replace = T))
        names(BOOT2)<-"Unit"
        for (i in 1:input$bootsample){
          if (i==1){
            BOOT2$Sample[i:nsub]<-i
          } else if (i>1) {
            BOOT2$Sample[(nsub*(i-1)+1):(nsub*i)]<-i
          }
          
          
        }
        
        BOOT2<-merge(BOOT2,ORIG2,by="Unit",all.x = T)
        BOOT<-rbind(BOOT1,BOOT2)
        
        return(BOOT)
      }
      BOOT <- BOOTSE(input$bootsample)
      BOOT<-BOOT[order(BOOT$Sample,BOOT$Form),]
      trtlist<-names(original)[3:ncol(original)]
      boot_mean <- aggregate(. ~ Sample+Form, data = BOOT[,c("Sample","Form",trtlist)], FUN=mean)
      boot_mean<-boot_mean[order(boot_mean$Sample),]
      
      
      boot_trsp1 <- transpose(boot_mean)
      
      boot_trsp1 <- reshape2::melt(boot_mean, id.vars = c("Sample","Form"), measure.vars = trtlist,
                                   variable.name = "Category", value.name = "Value")
      boot_trsp1 <- reshape2::dcast(boot_trsp1, Sample + Category ~ Form, value.var = "Value")
      boot_trsp1$Ref<-as.numeric(boot_trsp1$Ref)
      boot_trsp1$Test<-as.numeric(boot_trsp1$Test)
      
      boot_trsp1$diff <- boot_trsp1$Ref - boot_trsp1$Test
      boot_trsp1$diff_square <- boot_trsp1$diff^2
      
      summation_boot <- aggregate(diff_square~Sample,data=boot_trsp1,FUN=sum)
      
      freq <- length(trtlist)
      summation_boot$f2_boot = 50 * log10(100 * ((1 + (summation_boot$diff_square / freq))^(-0.5)))
      
      return(summation_boot)
      
    })
      
    bootstrapdf<-reactive({
      summation_boot<-summboot()
      quartiles<-data.frame(Mean_F2=mean(summation_boot$f2_boot),Median_F2=median(summation_boot$f2_boot),
                            "Lower Limit"=quantile(summation_boot$f2_boot, 0.05),"Upper Limit"=quantile(summation_boot$f2_boot, 0.95))
      setnames(quartiles,old=c('Upper.Limit','Lower.Limit','Mean_F2','Median_F2'),new=c('Upper Limit','Lower Limit','Mean F2','Median F2'))
      quartiles$Similarity<-ifelse(quartiles$`Lower Limit`>50,"Yes","No")
      
      return(quartiles)
      
    })
    
    
    cibca<-reactive({
      f2_original<-f2()
      summation_boot<-summboot()
      original<-imported_data()
      trtlist<-names(original)[3:ncol(original)]
      summation_boot$lessthan<-ifelse(summation_boot$f2_boot < f2_original,1,0)
      
      propless <- sum(summation_boot$lessthan)/nrow(summation_boot)
      bias <- qnorm(propless)
      
      origjack <- original
      origjack$obsnum <- 1:nrow(origjack)
      lstjack<-list()
      
      for (i in 1:nrow(origjack)){
        lstjack[[i]]<-origjack[-i,]
        lstjack[[i]]$Sample<-i
      }
      jackdata<-do.call(rbind,lstjack)
      
      jack_mean <- aggregate(. ~ Sample+Form, data  = jackdata[c('Sample','Form',trtlist)], mean)
      jack_mean<-jack_mean[order(jack_mean$Sample),]
      

      jack_trsp1 <- reshape2::melt(jack_mean, id.vars = c("Sample","Form"), measure.vars = trtlist,
                                   variable.name = "Category", value.name = "Value")
      jack_trsp1 <- reshape2::dcast(jack_trsp1, Sample + Category ~ Form, value.var = "Value")
      jack_trsp1$Ref<-as.numeric(jack_trsp1$Ref)
      jack_trsp1$Test<-as.numeric(jack_trsp1$Test)
      
      jack_trsp1$diff <- jack_trsp1$Ref - jack_trsp1$Test
      jack_trsp1$diff_square <- jack_trsp1$diff^2
      
      summation_jack <- aggregate(diff_square~Sample,data=jack_trsp1,FUN=sum)
      
      freq <- length(trtlist)
      summation_jack$f2 = 50 * log10(100 * ((1 + (summation_jack$diff_square / freq))^(-0.5)))
      summation_jack$cubed<-(mean(summation_jack$f2)-summation_jack$f2)^3
      summation_jack$squared<-(mean(summation_jack$f2)-summation_jack$f2)^2
      
      
      accel<-sum(summation_jack$cubed)/(6*(sum(summation_jack$squared))^1.5)
      
      part1<-(bias + qnorm(0.05)) / (1 - (accel*(bias + qnorm(0.05))))
      part2<-(bias + qnorm(0.95)) / (1 - (accel*(bias + qnorm(0.95))))
      alpha1<-pnorm(bias+part1)
      alpha2<-pnorm(bias+part2)
      n1<-alpha1*input$bootsample
      n2<-alpha2*input$bootsample
      
      n1<-ifelse(floor(n1)==0,1,n1)
      
      summation_boot<-summation_boot[order(summation_boot$f2_boot),]
      
      
      ci_bca<-data.frame("Lower Limit"=summation_boot$f2_boot[floor(n1)],"Upper Limit"=summation_boot$f2_boot[ceiling(n2)])
      setnames(ci_bca,old=c('Upper.Limit','Lower.Limit'),new=c('Upper Limit','Lower Limit'))
      ci_bca$Similarity<-ifelse(ci_bca$`Lower Limit`>50,"Yes","No")
      
      return(ci_bca)
      
    })
    
    f2_original<-reactive({
      f2_original<-f2()
      f2_original_df<-data.frame(`Similarity factor(f2)`=f2_original)
      return(f2_original_df)
    })
    
    quartiles2<-reactive({
      quartiles2<-bootstrapdf()
      # quartiles2<-quartiles[,c(1,3,4,2,5)]
      names(quartiles2)<-c("Mean f2","Median f2","Lower Confidence limit","Upper Confidence limit","Similarity")
      return(quartiles2)
    })
    cibca2<-reactive({
      ci_bca2<-cibca()
      names(ci_bca2)<-c("Lower Confidence limit","Upper Confidence limit","Similarity")
      return(ci_bca2)
    })
    
    observeEvent(input$btsbutton,{
      
    output$bootstrapdt<-renderTable({
      # if (input$similarity_analysis>0){
        bootstrapdf<-bootstrapdf()
        bootstrapdf
      # }
    }) 
    
    
    output$cibcadt<-renderTable({
      # if (input$similarity_analysis>0){
        cibca<-cibca()
        cibca
      # }
    })
    
    })
    
  })
  
output$download<-downloadHandler(
    filename = function(){
      paste("Data for Similarity Factor.csv")
    },
    content=function(file){
      write.csv(raw_data(),file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

output$downloader <- 
  downloadHandler(
    paste("Report",".pdf",sep = ""),
    content = 
      function(file){ 
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...', value = 0,
                     for (i in 1:15) {
                       incProgress(1/15)})
        
        
        rmarkdown::render(
          input = "report_try.Rmd",
          output_file = file,
          params = list(table1 = imported_data_dnld(),
                        table2 = summstats_ref(),
                        table3 = summstats_test(),
                        table4 = f2_original(),
                        table5 = quartiles2(),
                        table6 = cibca2()
          ))
        readBin(con = file,
                what = "raw",
                n = file.info(file)[, "size"]) %>%
          writeBin(con = file)
      })
  
raw_data<-reactive({
    raw_data<-imported_data()
  })
output$raw_data<-renderTable({
    raw_data()
  })
  


  }
})
  
observeEvent(input$logout, {js$reset()})
    }})

}