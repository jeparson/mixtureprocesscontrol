makeTargetPlot<-function(type="BLM",df,df2,Discriminator="CheeseValue"){
  require(reshape2)
  a<-blmmixfraction(df);
  a$mixnum<-c(1,1,1,1,1,1,2,2,2,2);
  a[11,]<-c(0,0,0,2);a[12,]<-c(0,0,0,2);
  a<-a[c(1:4,7:10),]#we *are* ignoring the points on the line.
  a<-as.data.frame(melt(a,id.vars=4))
  a<-data.frame(a[a$mixnum==1,],a[a$mixnum==2,])
  colnames(a)<-c("mixnum","variable","value1","blah","blah","value2")
  if(type=="BLM"){
    if(!missing(df2)){b<-blmmixfraction(df2);
                      b$mixnum<-c(1,1,1,1,1,1,2,2,2,2);
                      b[11,]<-c(0,0,0,2);b[12,]<-c(0,0,0,2);
                      b<-b[c(1:4,7:10),] #we *are* ignoring the points on the line.
                      b<-as.data.frame(melt(b,id.vars=4))
                      b<-data.frame(b[b$mixnum==1,],b[b$mixnum==2,])
                      colnames(b)<-c("mixnum","variable","value1","blah","blah","value2")
                      sumdf2<-data.frame(amean1=c(mean(b$value1[b$variable=="mixpropA"&b$value1>0]),mean(b$value1[b$variable=="mixpropB"&b$value1>0]),mean(b$value1[b$variable=="mixpropC"&b$value1>0])),
                                         amean2=c(mean(b$value2[b$variable=="mixpropA"&b$value2>0]),mean(b$value2[b$variable=="mixpropB"&b$value2>0]),mean(b$value2[b$variable=="mixpropC"&b$value2>0])),
                                         sd1=c(sd(b$value1[b$variable=="mixpropA"&b$value1>0]),sd(b$value1[b$variable=="mixpropB"&b$value1>0]),sd(b$value1[b$variable=="mixpropC"&b$value1>0])),
                                         sd2=c(sd(b$value2[b$variable=="mixpropA"&b$value2>0]),sd(b$value2[b$variable=="mixpropB"&b$value2>0]),sd(b$value2[b$variable=="mixpropC"&b$value2>0])))
                      
    }
    sumdf<-data.frame(amean1=c(mean(a$value1[a$variable=="mixpropA"&a$value1>0]),mean(a$value1[a$variable=="mixpropB"&a$value1>0]),mean(a$value1[a$variable=="mixpropC"&a$value1>0])),
                      amean2=c(mean(a$value2[a$variable=="mixpropA"&a$value2>0]),mean(a$value2[a$variable=="mixpropB"&a$value2>0]),mean(a$value2[a$variable=="mixpropC"&a$value2>0])),
                      sd1=c(sd(a$value1[a$variable=="mixpropA"&a$value1>0]),sd(a$value1[a$variable=="mixpropB"&a$value1>0]),sd(a$value1[a$variable=="mixpropC"&a$value1>0])),
                      sd2=c(sd(a$value2[a$variable=="mixpropA"&a$value2>0]),sd(a$value2[a$variable=="mixpropB"&a$value2>0]),sd(a$value2[a$variable=="mixpropC"&a$value2>0])))
    
  }
  #do the plotting
  if(!missing(df2)&type=="SEQC"){
    g<-ggplot(subset(a,sample=="C"))
    g+geom_point(aes(x=Apct,y=a$Apct[a$sample=="D"],color=as.factor(LID)),alpha=0.7,size=6)+
      scale_y_continuous(limits=c(0,0.5),expand=c(0,0))+scale_x_continuous(limits=c(0.5,1),expand=c(0,0))+
      facet_wrap(~ LID)+ylab("Amount of SEQC-A in SEQC-C")+xlab("Amount of SEQC-A in SEQC-D")+
      geom_point(aes(x=0.75,y=0.25),col="grey70")+theme(legend.position="none")+geom_path(data=circleFun(center=c(0.75,0.25),diameter=0.1,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.75,0.25),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.75,0.25),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.75,0.25),diameter=0.2,npoints=25),aes(x,y),col="grey")+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+theme(panel.margin=unit(1,"cm"))+
      theme(legend.text=element_text(size=rel(1.4)))+theme(axis.title=element_text(size=rel(1.6)))+theme(axis.text=element_text(size=rel(1)))+
      theme(strip.background = element_rect(fill = 'white'))+geom_pointrange(data=sumdf,aes(x=amean1,y=amean2,ymax=amean2+sd2,ymin=amean2-sd2))+
      geom_errorbarh(data=sumdf,aes(x=amean1,y=amean2,xmax=amean1+sd1,xmin=amean1-sd1))+geom_point(data=subset(b,sample=="C"),aes(x=Apct,y=b$Apct[b$sample=="D"],color=))
    
    
  }
  if(type=="SEQC"){
    g<-ggplot(subset(a,sample=="C"))
    return(g+geom_point(aes(x=Apct,y=a$Apct[a$sample=="D"],color=as.factor(LID)),alpha=0.7,size=6)+
             scale_y_continuous(limits=c(0,0.5),expand=c(0,0))+scale_x_continuous(limits=c(0.5,1),expand=c(0,0))+
             facet_wrap(~ LID)+ylab("Amount of SEQC-A in SEQC-C")+xlab("Amount of SEQC-A in SEQC-D")+
             geom_point(aes(x=0.75,y=0.25),col="grey70")+theme(legend.position="none")+geom_path(data=circleFun(center=c(0.75,0.25),diameter=0.1,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.75,0.25),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.75,0.25),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.75,0.25),diameter=0.2,npoints=25),aes(x,y),col="grey")+
             theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+theme(panel.margin=unit(1,"cm"))+
             theme(legend.text=element_text(size=rel(1.4)))+theme(axis.title=element_text(size=rel(1.6)))+theme(axis.text=element_text(size=rel(1)))+
             theme(strip.background = element_rect(fill = 'white'))+geom_pointrange(data=sumdf,aes(x=amean1,y=amean2,ymax=amean2+sd2,ymin=amean2-sd2))+
             geom_errorbarh(data=sumdf,aes(x=amean1,y=amean2,xmax=amean1+sd1,xmin=amean1-sd1)))
  }
  
  if(!missing(df2)&type=="BLM"){
    a$mcor<-1
    b$mcor<-0
    mergedm<-rbind(a,b)
    merged<-rbind(sumdf,sumdf2)
    merged$mcor<-c(1,1,1,0,0,0)
    g<-ggplot(mergedm)
    #print(g+geom_point(aes(x=value[mergedm$mixnum==1],y=mergedm$value[mergedm$mixnum==2],col=mergedm$variable[mergedm$mixnum==1],alpha=as.factor(mergedm$mcor[mergedm$mixnum==1])),size=5)+
    #        xlab("Amount of Tissue X in BLM-1")+ylab("Amount of Tissue X in BLM-2")+geom_point(aes(x=c(0.25,0.25,.5),y=c(0.25,0.5,0.25)),col="grey70",size=3)+
    #  geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.1,npoints=25),aes(x,y),col="grey") + geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.2,npoints=25),aes(x,y),color="grey")+geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.1,npoints=25),aes(x,y),col="grey")+  geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.2,npoints=25),aes(x,y),color="grey")+  geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.1,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.2,npoints=25),aes(x,y),color="grey")+
    #  theme(legend.position=c(0.6,0.7))+scale_color_manual(name="Tissue X",breaks=c("mixpropA","mixpropB","mixpropC"),labels=c("Brain","Liver","Muscle"),values=c("#CC6666","#99CC66","#6699CC"))+coord_cartesian(ylim=c(0,1),xlim=c(0,1))+theme(axis.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(size=rel(1.5)))+theme(legend.text=element_text(size=rel(1.5)))+theme(legend.title=element_text(size=rel(1.5)))+
    #  geom_pointrange(data=merged,aes(x=amean1,y=amean2,ymax=amean2+sd2,ymin=amean2-sd2))+geom_errorbarh(data=merged,aes(x=amean1,y=amean2,xmax=amean1+sd1,xmin=amean1-sd1,height=0))+scale_alpha_manual(name=Discriminator,breaks=c(1,0),labels=c("True","False"),values=c(0.3,1))
    #)
    return(g+
             geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.1,npoints=25),aes(x,y),col="grey") + geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.2,npoints=25),aes(x,y),color="grey")+geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.1,npoints=25),aes(x,y),col="grey")+  geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.2,npoints=25),aes(x,y),color="grey")+  geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.1,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.2,npoints=25),aes(x,y),color="grey")+
             geom_point(aes(x=value1,y=value2,col=variable,alpha=as.factor(mcor)),size=5)+
             xlab("Amount of Tissue in BLM-1")+ylab("Amount of Tissue in BLM-2")+geom_point(aes(x=c(0.25,0.25,.5),y=c(0.25,0.5,0.25)),col="grey70",size=3)+
             theme(legend.position=c(0.6,0.7))+scale_color_manual(name="Tissue",breaks=c("mixpropA","mixpropB","mixpropC","mixpropA","mixpropB","mixpropC"),labels=c("Brain","Liver","Muscle","Brain","Liver","Muscle"),values=rep(c("#CC6666","#99CC66","#6699CC"),2))+coord_cartesian(ylim=c(0,1),xlim=c(0,1))+theme(axis.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(size=rel(1.5)))+theme(legend.text=element_text(size=rel(1.5)))+theme(legend.title=element_text(size=rel(1.5)))+
             geom_pointrange(data=merged,aes(x=amean1,y=amean2,ymax=amean2+sd2,ymin=amean2-sd2,alpha=as.factor(mcor)))+geom_errorbarh(data=merged,aes(x=amean1,y=amean2,xmax=amean1+sd1,xmin=amean1-sd1,height=0,alpha=as.factor(mcor)))+scale_alpha_manual(name=Discriminator,breaks=c(1,0),labels=c("True","False"),values=c(0.3,1))+theme(aspect.ratio=1))+theme(axis.ticks.margin=unit(x = 0.25,units = "cm"))+theme(axis.text=element_text(size=16))
    
    
  }
  if(missing(df2)&type=="BLM"){
    a$mcor<-1
    mergedm<-a
    merged<-sumdf
    g<-ggplot(mergedm)
    
    return(g+xlab("Amount of Tissue in BLM-1")+ylab("Amount of Tissue in BLM-2")+geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.1,npoints=25),aes(x,y),col="grey") + geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.25),diameter=0.2,npoints=25),aes(x,y),color="grey")+geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.1,npoints=25),aes(x,y),col="grey")+  geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.5,0.25),diameter=0.2,npoints=25),aes(x,y),color="grey")+  geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.05,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.1,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.15,npoints=25),aes(x,y),col="grey")+geom_path(data=circleFun(center=c(0.25,0.5),diameter=0.2,npoints=25),aes(x,y),color="grey")+geom_point(aes(x=c(0.25,0.25,.5),y=c(0.25,0.5,0.25)),col="grey70",size=3)+geom_point(aes(x=value1,y=value2,col=variable),size=5)+theme(legend.position=c(0.6,0.7))+scale_color_manual(name="Tissue",breaks=c("mixpropA","mixpropB","mixpropC"),labels=c("Brain","Liver","Muscle"),values=rep(c("#CC6666","#99CC66","#6699CC"),1))+coord_cartesian(ylim=c(0,1),xlim=c(0,1))+theme(axis.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(size=rel(1.5)))+theme(legend.text=element_text(size=rel(1.5)))+theme(legend.title=element_text(size=rel(1.5)))+geom_pointrange(data=merged,aes(x=amean1,y=amean2,ymax=amean2+sd2,ymin=amean2-sd2))+geom_errorbarh(data=merged,aes(x=amean1,y=amean2,xmax=amean1+sd1,xmin=amean1-sd1,height=0))+theme(aspect.ratio=1))
    
  }
  
}

blmmixfraction<-function(subdf){
    
    value<-unname(lm(subdf[,2]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients/sum(lm(subdf[,2]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients))
    value1<-unname(lm(subdf[,3]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients/sum(lm(subdf[,3]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients))
    value2<-unname(lm(subdf[,4]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients/sum(lm(subdf[,4]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients))
    value3<-unname(lm(subdf[,5]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients/sum(lm(subdf[,5]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients))
    value4<-unname(lm(subdf[,6]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients/sum(lm(subdf[,6]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients))
    value5<-unname(lm(subdf[,7]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients/sum(lm(subdf[,7]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients))
    value6<-unname(lm(subdf[,8]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients/sum(lm(subdf[,8]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients))
    value7<-unname(lm(subdf[,9]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients/sum(lm(subdf[,9]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients))
    value8<-unname(lm(subdf[,10]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients/sum(lm(subdf[,10]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients))
    value9<-unname(lm(subdf[,11]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients/sum(lm(subdf[,11]~subdf[,12]+subdf[,13]+subdf[,14]+0)$coefficients))
    mfrac<-calcmrnafrac(subdf,selection=12:14)
    mfrac<-mfrac/sum(mfrac)
    Amfrac<-mfrac[1]
    Bmfrac<-mfrac[2]
    Cmfrac<-mfrac[3]
    mixpropC<-(value[3]/Cmfrac)/((value[1]/Amfrac)+(value[2]/Bmfrac)+(value[3]/Cmfrac))
    mixpropB<-(value[2]/Bmfrac)/((value[1]/Amfrac)+(value[2]/Bmfrac)+(value[3]/Cmfrac))
    mixpropA<-(value[1]/Amfrac)/((value[1]/Amfrac)+(value[2]/Bmfrac)+(value[3]/Cmfrac))
    outdf<-data.frame(mixpropA,mixpropB,mixpropC)
    #mixprop<-c(mixpropA,mixpropB,mixpropC)
    mixpropC<-(value1[3]/Cmfrac)/((value1[1]/Amfrac)+(value1[2]/Bmfrac)+(value1[3]/Cmfrac))
    mixpropB<-(value1[2]/Bmfrac)/((value1[1]/Amfrac)+(value1[2]/Bmfrac)+(value1[3]/Cmfrac))
    mixpropA<-(value1[1]/Amfrac)/((value1[1]/Amfrac)+(value1[2]/Bmfrac)+(value1[3]/Cmfrac))
    outdf<-rbind(outdf,c(mixpropA,mixpropB,mixpropC))
    mixpropC<-(value2[3]/Cmfrac)/((value2[1]/Amfrac)+(value2[2]/Bmfrac)+(value2[3]/Cmfrac))
    mixpropB<-(value2[2]/Bmfrac)/((value2[1]/Amfrac)+(value2[2]/Bmfrac)+(value2[3]/Cmfrac))
    mixpropA<-(value2[1]/Amfrac)/((value2[1]/Amfrac)+(value2[2]/Bmfrac)+(value2[3]/Cmfrac))
    outdf<-rbind(outdf,c(mixpropA,mixpropB,mixpropC))
    mixpropC<-(value3[3]/Cmfrac)/((value3[1]/Amfrac)+(value3[2]/Bmfrac)+(value3[3]/Cmfrac))
    mixpropB<-(value3[2]/Bmfrac)/((value3[1]/Amfrac)+(value3[2]/Bmfrac)+(value3[3]/Cmfrac))
    mixpropA<-(value3[1]/Amfrac)/((value3[1]/Amfrac)+(value3[2]/Bmfrac)+(value3[3]/Cmfrac))
    outdf<-rbind(outdf,c(mixpropA,mixpropB,mixpropC))
    mixpropC<-(value4[3]/Cmfrac)/((value4[1]/Amfrac)+(value4[2]/Bmfrac)+(value4[3]/Cmfrac))
    mixpropB<-(value4[2]/Bmfrac)/((value4[1]/Amfrac)+(value4[2]/Bmfrac)+(value4[3]/Cmfrac))
    mixpropA<-(value4[1]/Amfrac)/((value4[1]/Amfrac)+(value4[2]/Bmfrac)+(value4[3]/Cmfrac))
    outdf<-rbind(outdf,c(mixpropA,mixpropB,mixpropC))
    mixpropC<-(value5[3]/Cmfrac)/((value5[1]/Amfrac)+(value5[2]/Bmfrac)+(value5[3]/Cmfrac))
    mixpropB<-(value5[2]/Bmfrac)/((value5[1]/Amfrac)+(value5[2]/Bmfrac)+(value5[3]/Cmfrac))
    mixpropA<-(value5[1]/Amfrac)/((value5[1]/Amfrac)+(value5[2]/Bmfrac)+(value5[3]/Cmfrac))
    outdf<-rbind(outdf,c(mixpropA,mixpropB,mixpropC))
    mixpropC<-(value6[3]/Cmfrac)/((value6[1]/Amfrac)+(value6[2]/Bmfrac)+(value6[3]/Cmfrac))
    mixpropB<-(value6[2]/Bmfrac)/((value6[1]/Amfrac)+(value6[2]/Bmfrac)+(value6[3]/Cmfrac))
    mixpropA<-(value6[1]/Amfrac)/((value6[1]/Amfrac)+(value6[2]/Bmfrac)+(value6[3]/Cmfrac))
    outdf<-rbind(outdf,c(mixpropA,mixpropB,mixpropC))
    mixpropC<-(value7[3]/Cmfrac)/((value7[1]/Amfrac)+(value7[2]/Bmfrac)+(value7[3]/Cmfrac))
    mixpropB<-(value7[2]/Bmfrac)/((value7[1]/Amfrac)+(value7[2]/Bmfrac)+(value7[3]/Cmfrac))
    mixpropA<-(value7[1]/Amfrac)/((value7[1]/Amfrac)+(value7[2]/Bmfrac)+(value7[3]/Cmfrac))
    outdf<-rbind(outdf,c(mixpropA,mixpropB,mixpropC))
    mixpropC<-(value8[3]/Cmfrac)/((value8[1]/Amfrac)+(value8[2]/Bmfrac)+(value8[3]/Cmfrac))
    mixpropB<-(value8[2]/Bmfrac)/((value8[1]/Amfrac)+(value8[2]/Bmfrac)+(value8[3]/Cmfrac))
    mixpropA<-(value8[1]/Amfrac)/((value8[1]/Amfrac)+(value8[2]/Bmfrac)+(value8[3]/Cmfrac))
    outdf<-rbind(outdf,c(mixpropA,mixpropB,mixpropC))
    mixpropC<-(value9[3]/Cmfrac)/((value9[1]/Amfrac)+(value9[2]/Bmfrac)+(value9[3]/Cmfrac))
    mixpropB<-(value9[2]/Bmfrac)/((value9[1]/Amfrac)+(value9[2]/Bmfrac)+(value9[3]/Cmfrac))
    mixpropA<-(value9[1]/Amfrac)/((value9[1]/Amfrac)+(value9[2]/Bmfrac)+(value9[3]/Cmfrac))
    outdf<-rbind(outdf,c(mixpropA,mixpropB,mixpropC))
    
    
    
    
    return(outdf)
  }