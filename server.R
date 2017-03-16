# server.R
library(ggplot2)
library(dplyr)
load("Shinydata.RData")
simcode$psi1.true<-as.factor(simcode$psi1.true)
simcode$mean.col<-as.factor(simcode$mean.col)
simcode$mean.surv<-as.factor(simcode$mean.surv)
simcode$pT.true<-as.factor(simcode$pT.true)
simcode$pF.true<-as.factor(simcode$pF.true)
simcode$prop.conf.true<-as.factor(simcode$prop.conf.true)
simcode<-filter(simcode,converged==TRUE,staticmodel==TRUE) 



shinyServer(function(input, output) {


  data<-reactive({
if(input$simset==1) {
      filter(simcode,complexdata==FALSE,NAs==FALSE)
    } else if(input$simset==2){
      filter(simcode,complexdata==FALSE,NAs==TRUE) 
    } else if(input$simset==3){
      filter(simcode,complexdata==TRUE,NAs==FALSE) 
    } else if(input$simset==4) {
      filter(simcode,complexdata==TRUE,NAs==TRUE) 
    } else {print("fail")}

  })
####bias plot###
  
  
  
  output$plot <- renderPlot({
    #fix this    


    
    summaries<-select_(data(),input$par1,input$par2,input$focal) %>% group_by_(input$par1,input$par2) %>% summarise_if(is.numeric,funs(mean,median),na.rm=T)
    colnames(summaries)[dim(summaries)[2]]<-"median"
    colnames(summaries)[dim(summaries)[2]-1]<-"mean"
    summaries

   facets<-paste(input$par2,"~.") 
   p<-ggplot(data(),aes_string(input$focal,fill=input$par1))+geom_density(alpha=0.5,aes_string(fill=input$par1),position="identity")+theme_bw()+xlab("Absolute Bias")+geom_vline(data=summaries,aes_string(xintercept=input$summarystat,colour=input$par1),size=1.5,linetype="dotted")+facet_grid(facets,scales="free_y")
  
p
    })
  
  ###bias summary table###
  
  
  output$plotsum<-renderTable({
    
    key1<-switch(input$par1,
                "pT.true"="detection probability",
                "pF.true"="false-positive rate",
                "mean.col"="colonisation rate",
                "mean.surv"="survival rate",
                "psi1.true"="initial occupancy",
                "prop.conf.true"="proportion confirmed obs")
    key2<-switch(input$par2,
                 "pT.true"="detection probability",
                 "pF.true"="false-positive rate",
                 "mean.col"="colonisation rate",
                 "mean.surv"="survival rate",
                 "psi1.true"="initial occupancy",
                 "prop.conf.true"="proportion confirmed obs")
    
    
    summaries<-data() %>% select_(input$par1,input$par2,input$focal) %>% group_by_(input$par1,input$par2) %>% summarise_if(is.numeric,funs(mean,median),na.rm=T)
    colnames(summaries)[1:2]<-c(key1,key2)
    colnames(summaries)[dim(summaries)[2]]<-"median"
    colnames(summaries)[dim(summaries)[2]-1]<-"mean"
    summaries
  })
  
  
  
  
  ####CI plot#####
  
  output$CIwidth <- renderPlot({
    
    key3<-switch(input$focal,
                 "occ.bias"="occ.CIwidth",
                 "pT.bias"="pT.CIwidth",
                 "pF.bias"="pF.CIwidth",
                 "col.bias"="col.CIwidth",
                 "surv.bias"="surv.CIwidth",
                 "psi1.bias"="psi1.CIwidth")
    
    
    ###need to select the correct parameter to plot- CURRENTLY PLOTTING BIAS, NOT CI WIDTH
    summaries<-data() %>% select_(input$par1,input$par2,key3) %>% group_by_(input$par1,input$par2) %>% summarise_if(is.numeric,funs(mean,median),na.rm=T)
    colnames(summaries)[dim(summaries)[2]]<-"median"
    colnames(summaries)[dim(summaries)[2]-1]<-"mean"
    summaries
    
    facets<-paste(input$par2,"~.") 
    pCI<-ggplot(data(),aes_string("occ.CIwidth",fill=input$par1))+geom_density(alpha=0.5,aes_string(fill=input$par1),position="identity")+theme_bw()+xlab("Width of occupancy 95% Credible Intervals")+geom_vline(data=summaries,aes_string(xintercept=input$summarystat,colour=input$par1),size=1.5,linetype="dotted")+facet_grid(facets,scales="free_y")
    
  pCI
  })
  
  
  
  
  ###CI summary table####
  
  
  output$CIsum<-renderTable({
    
    key1<-switch(input$par1,
                 "pT.true"="detection probability",
                 "pF.true"="false-positive rate",
                 "mean.col"="colonisation rate",
                 "mean.surv"="survival rate",
                 "psi1.true"="initial occupancy",
                 "prop.conf.true"="proportion confirmed obs")
    key2<-switch(input$par2,
                 "pT.true"="detection probability",
                 "pF.true"="false-positive rate",
                 "mean.col"="colonisation rate",
                 "mean.surv"="survival rate",
                 "psi1.true"="initial occupancy",
                 "prop.conf.true"="proportion confirmed obs")
    key3<-switch(input$focal,
                 "occ.bias"="occ.CIwidth",
                 "pT.bias"="pT.CIwidth",
                 "pF.bias"="pF.CIwidth",
                 "col.bias"="col.CIwidth",
                 "surv.bias"="surv.CIwidth",
                 "psi1.bias"="psi1.CIwidth")
    
    summaries<-data() %>% select_(input$par1,input$par2,key3) %>% group_by_(input$par1,input$par2) %>% summarise_if(is.numeric,funs(mean,median),na.rm=T)
    colnames(summaries)[1:2]<-c(key1,key2)
    colnames(summaries)[dim(summaries)[2]]<-"median"
    colnames(summaries)[dim(summaries)[2]-1]<-"mean"
    summaries
    
  })
  
  
  
})
