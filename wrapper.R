#Goals:

#I would like to take as input a matrix of counts, alongside a vector of mixture information, and build out to 
#A: Deconvolution estimates / target plots / other figures of merit
#B: Mixture analysis performed to determine the variability between observed and predicted, etc etc


#Information the function will need to do deconvolution:
#Count matrix
#Assignment of the various columns as being mixtures or pure
#known proportions if they exist
#Knowledge of mRNA fraction (If you want the deconvolution to be TotalRNA based)
#'Deconvolute genome-scale data from mixtures.
#'An implementation of mixture deconvolution and mixture measurement assurance 
#'as described by Parsons et. al, 2015. (Ref)

#'current caveats:  
#'mRNA spike-ins are assumed to be at a constant mass fraction across mixtures.
#'replicates are not considered at all.  Mixture Analysis Doesn't do anything,
#' using known mix proportions (to generate a target plot) isn't implemented.

###I did update calcmrnafracgeneral in a diff package, FYI ME

#'@param countdf 
#'A data frame of counts from a genome-scale measurement of mixtures and pure components
#'@param pures
#'A vector which describes which count data comes from pure components. 
#'Replicate measurements should differ in trailing digits
#'(eg: Pa1, Pb1, Pc1 are replicates,  P2 is a different component)
#'@param mixtures
#'A vector which describes which count data in countdf is from mixtures 
#'Replicate measurements should differ in trailing digits
#'(eg: mixa1, mixa2, mixa3 are replicates,  mixb1 is a different component)
#'@param mixmeasure 
#'Should the mixture proportions be measured as fraction
#'of starting Total RNA(default,TRUE) or not (FALSE, instead measured 
#'as fraction of starting mRNA)?  Starting total RNA requires a measurement 
#'of mRNA content, by default calculated using ERCC-spike-in controls
#'@param geneids 
#'Where are gene identifying strings in your dataframe? (column number)
#'Would I just prefer implementing a specific datastructure and somehow building
#'it, rather than making a grand many diff parameters to this function?
#'@param controlgenes
#'How do i identify the control genes in your sheet?  
#'(Default:  finds row entries containing "ERCC-" within column geneids)
#'@param mixproportions
#'Vector of known mixture proportions (for validation/target plots)
#'@param replicatehandling
#'How are replicates handled? (Current default and only option: calculates mean of replicates)
#'@return The proportions of Pure samples within each Mixed sample 

deconvolutemix<-function(countdf,pures,mixtures,mixmeasure=TRUE,geneids=1,controlgenes="ERCC-",mixproportions,replicatehandling="Mean"){

  #first identify if there are replicates
  identifyreplicates<-function(countdf){
    a<-duplicated(regmatches(colnames(countdf),(regexpr("\\D+",colnames(countdf),perl=TRUE))))
  }
  a<-identifyreplicates(countdf)
 
  if(sum(a)>0){#if there are any replicate samples
    collapsereplicates<-function(a,countdf,mixtures,pures,replicatehandling){
      listreplicates<-function(names,a){
        rlist<-vector('list',length(names[a]))
        it<-0
        for(I in names[a]){
          it<-it+1
          base<-regmatches(I,regexpr("\\D+",I,perl=TRUE)) #extract the 'basename' (nondigit)
          item<-grep(paste0("^",base),names) #find things that contain that basename 
          #####(Note that this currently tags "bep" for "b", which it shouldn't)
          rlist[[it]]<-item
          names(rlist)[it]<-base
        }
        rlist}#make lists of which items *are* replicates 
      #####probably needs some interesting things for error prevention/tracking...
      reps<-listreplicates(colnames(countdf),a)
      meanfun<-function(countdf,numbers){rowMeans(countdf[,numbers])}
      if(replicatehandling=="Mean"){
        newlist<-lapply(reps,meanfun,countdf=countdf) #collapse replicates down into averages
        newcdf<-countdf[,!(1:length(countdf)%in%unlist(reps))] # BETTER, this now selects things that are not duplicated at all.
        for(I in 1:length(newlist)){
          newcdf[,1+length(newcdf)]<-newlist[[I]]
          names(newcdf)[length(newcdf)]<-paste0(names(newlist)[[I]],"99")
          #99 is an addition to make sure that a: the pseudoreplicate is "caught" by the regex below
          #b: the new name isn't one of the old names.  No one uses 99 replicates, riiiiight?
        }#work with a countdf that includes only the averages & non-replicated bits now
      }  
      #removing columns stops "pures" and "mixtures" from being useful parameters
      #because they're no longer in the original positions.
      #so i match the old parameters to the new ones.  This is likely in need of error handling.  
      mixtures<-match(regmatches(names(countdf[mixtures]),regexpr("\\D+",names(countdf[mixtures]),perl=TRUE)),regmatches(names(newcdf),regexpr("\\D+",names(newcdf),perl=TRUE)))
      mixtures<-unique(mixtures)[!is.na(unique(mixtures))]
      pures<-match(regmatches(names(countdf[pures]),regexpr("\\D+",names(countdf[pures]),perl=TRUE)),regmatches(names(newcdf),regexpr("\\D+",names(newcdf),perl=TRUE)))
      pures<-unique(pures)[!is.na(unique(pures))]
      return(list(newcdf,mixtures,pures))
    }
    ##collapses replicates using the current replication handling...
    updates<-collapsereplicates(a,countdf,mixtures,pures,replicatehandling)
    countdf<-updates[[1]]
    pures<-updates[[3]]
    mixtures<-updates[[2]]
  }#collapse replicate samples if they exist

  if(mixmeasure==TRUE){
    mFracs<-calcmrnafracgeneral(countdf[,c(geneids,mixtures,pures)],controlgenes)  
##### I should like to update this function with the ability to select data columns, eg: mixtures+pures, although maybe i dont need to
  }#calculate mRNA fraction using ERCC spike in controls
else if (mixmeasure==FALSE){mFracs<-rep(1,length(mixtures)+length(pures));names(mFracs)<-c(colnames(countdf[mixtures]),colnames(countdf[pures]))}
  deconbits<-GeneralLMest(countdf,controlgenes,components=colnames(countdf)[pures],
                          mfrac=mFracs[match(colnames(countdf)[pures],names(mFracs))],
                          mixes=colnames(countdf)[mixtures],spikemassfraction=0.05)
  if(!missing(mixproportions)){
    makegenericTargetPlot(deconbits,mixproportions)
  }
  return(deconbits)
#Return values:  
#Calculated proportions within each mixture as a function of the pure inputs: check
#What that calculated proportion corresponds to (mRNA or totalRNA) : no
#If known proportions exist, make a target plot : no
#Also, make it a little prettier, 99 for average is pretty kludgy.

}

#####Expand this function to include geneid stuff (as long as it knows which one...)

#known proportions
#currently works robustly for BLM data!!! and arbitrary numbers of mixes 
#(given a little massaging to get the input format right).  SEQC data to be determined
GeneralLMest<-function(infile,spikeID="ERCC-",components=c("bep","lep","mep"),mixes=c("a1"),mfrac,spikemassfraction=c(0.003,0.003,0.003)){
  ##formatting of infile:
  # Rownames:  gene_ids, with a phrase ("spikeID") that identifies Spike-In controls
  # Columns: 1 column per component, 1 column per mix
  #must input list of components and list of mixes: For example:  the BLM mix contains components= c("bep","lep","mep") mixes=c("a1","a2")
  #spikemassfraction is the mass proportion you targeted with your spike-in controls
  #input should already be averaged across replicates
  
  if(missing(mfrac)){mfrac<-calcmrnafracgeneral(infile[,components],spikeID,spikemassfraction) }
     #Mfrac goes into calcmrnafracgeneral, which calculates the mRNA fraction of each sample in the table using equation 3 - here we send it only the "components" of the mixture.
     if(length(mfrac)!=length(components)){stop("ERROR:  Number of components != Number of calculated mRNA fractions!")}
{mixval<-NULL;rdf<-NULL}#set up dummy variables
{
  for(mixtext in mixes){
    modeltext<-NULL
    modeltext<-paste(modeltext,(paste("I(",mfrac[1],"*",components[1],")+0")))
    for(I in 2:length(components)){
      modeltext<-paste(modeltext,(paste("+I(",mfrac[I],"*", components[I],")+0")),sep=" ")
    }#convincing LM to handle arbitrary columns is not simple: this block tries to do that.
    
    mixval<-coefficients(lm(data=infile, as.formula(paste(mixtext,"~",modeltext))))/sum(coefficients(lm(data=infile,as.formula(paste(mixtext,"~",modeltext)))))
    #Mixval for a mixture sample in "mixes" is here determined to be the coefficients of a linear model fit to the Identity "I" of the mRNA fraction "mfrac" value of a component in "components"
    #times the mixture expression of that component, plus the same value for all other components.  This is equation 2.  There is no intercept fit (+0) for this model.
    
    rdf<-rbind(rdf,mixval)
    
    
  }
  rownames(rdf)<-mixes
  colnames(rdf)<-components
  return(rdf)
}

}
#calculates the mRNA fraction of a given count table and returns the normalized component values.  


makegenericTargetPlot<-function(mprop,mixproportions,components){
#'Make a target plot from the Model Fit Deconvolution
#'To test the general goodness of your mixture sequencing and analysis etc 
#'@param mprop mixture proportion data (from deconvolutemix)
#'@param mixproportions matrix defining the designed composition of the mixtures.  Names:
#'rownames = mixture names Colnames = component names
#'@param components names of components, (same as deconvolutemix)
  #there needs to be some way of making sure that this analysis is generalizable and fed reproducibly 
  require(dplyr)
  require(reshape2)
  mprop<-melt(mprop)
sumdf<-group_by(mprop,Var1,Var2)%>%summarise(amean1=mean(value),amean2=mean(dval),sd1=sd(value),sd2=sd(dval))
if(!length(levels(mprop$Var1)==2)){stop("I'm confused by the number of different mixes and can't make a plot of one against the other")}
##I could concievably just facet this data and make an n by n set of images given the number of diff mixes: length(combn(levels(mprop$Var1),2))/2 = number of facets..
##and the non-length of it gives the various levels.
#do the plotting
###things that need changing: 1) ylimits 2) labels 3) central points 4) circle centers, number of circles, place circles come from, datasource for geom_pointrange and geom_errorbarh
g+geom_point(aes(x=value,y=dval,color=(method),pch=method),alpha=0.65,size=4)+coord_cartesian(ylim=c(0.1,0.4),xlim=c(0.6,0.9))+
  facet_wrap(~ site)+ylab(paste("Amount of",names(mprop)[components][1])+xlab("Amount of ",names(mprop)[components][1])+geom_point(aes(x=mixproportions[1,1],y=[1,2]),col="grey70")+theme(legend.position="none")+
  geom_path(data=circleFun(center=c(mixproportions[1,1],mixproportions[1,2]),diameter=0.1,npoints=25),aes(x,y),col="grey")+
  geom_path(data=circleFun(center=c(mixproportions[1,1],mixproportions[1,2]),diameter=0.05,npoints=25),aes(x,y),col="grey")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+theme(panel.margin=unit(1,"cm"))+theme(aspect.ratio=1)+
  theme(legend.text=element_text(size=rel(1.4)))+theme(axis.title=element_text(size=rel(1.6)))+
  theme(axis.text=element_text(size=rel(1)))+theme(strip.background = element_rect(fill = 'white'))+theme(strip.text=element_text(size=rel(1.3)))+
  geom_pointrange(data=fmean,aes(x=meanc,y=meand,ymax=meand+2*sdd,ymin=meand-2*sdd),size=1.15,shape=1)+geom_errorbarh(data=fmean,aes(x=meanc,y=meand,xmax=meanc+2*sdc,xmin=meanc-2*sdc),size=1.3)


}