#pc
 root<-"C:\\Users\\emolina\\Edmundo-RAND\\Projects\\Dissertation\\Model Development\\TechChange-RDM\\"
#load libraries
 library(data.table,lib=paste(root,"Rlibraries\\",sep=""))
 
#Set parameters
 dir.prim<-paste(root,"RDM Outputs\\",sep="")
 prim.file<-"prim.data_7_06_2015.csv"
 
#load prim data
 prim.data<-data.table(read.csv(paste(dir.prim,prim.file,sep="")) )
 
#compute relative variables to be used in prim
  prim.data[,Relative.A_N:=Are.N/Ace.N]
  prim.data[,Relative.A_S:=Are.S/Ace.S]
  prim.data[,ProductivityGap:=Relative.A_S/Relative.A_N]
  prim.data[,Relative.Gamma:=Gamma.re/Gamma.ce]
  prim.data[,Relative.Eta:=Eta.re/Eta.ce]
  prim.data[,Relative.Eta.Gamma:=Relative.Eta*Relative.Gamma]
  prim.data[,Relative.Nu:=Nu.re/Nu.ce]
  prim.data[,Relative.k:=exp(k.re)/exp(k.ce)]
  prim.data[,Relative.k.Nu:=(1/Relative.k)*Relative.Nu]
  prim.data[,Share.re_N:=Yre_N/(Yce_N+Yre_N)]
  prim.data[,Share.re_S:=Yre_S/(Yce_S+Yre_S)]
  prim.data[,DiffusionGap:=Share.re_S/Share.re_N]
#changes in consumption
  prim.data[,Policy.Costs.N:=Consumption.Total_N.300-Consumption.Total_N.300.fwa]
  prim.data[,Policy.Costs.S:=Consumption.Total_S.300-Consumption.Total_S.300.fwa]
  prim.data[,Policy.Costs.N.100:=Consumption.Total_N-Consumption.Total_N.fwa]
  prim.data[,Policy.Costs.S.100:=Consumption.Total_S-Consumption.Total_S.fwa]
  prim.data[,Total.Policy.Cost.100:=(Policy.Costs.N.100+Policy.Costs.S.100)/(Consumption.Total_N.fwa+Consumption.Total_S.fwa)]
  prim.data[,Total.Policy.Cost:=(Policy.Costs.N+Policy.Costs.S)/(Consumption.Total_N.300.fwa+Consumption.Total_S.300.fwa)]
  prim.data[,Total.Policy.Utility:=Utility.Consumer.Total_N+Utility.Consumer.Total_S]  
#Define outputs of interest
 prim.data[,Temp.Threshold.2C:=ifelse(Delta.Temp<=2,1,0)] 
 prim.data[,Stabilization:=ifelse(Policy.Duration+(Policy.Start.Time+2012)>=2123,0,
                                                 ifelse(Delta.Temp>2,0,
 												          ifelse(Delta.Temp.300yr<=2,1,0)))] 

#Combine Objectives with cost thresholds 
#TempRise Targets
 prim.data[,Temp.Threshold.2C.Cost.Zero:=ifelse(Total.Policy.Cost.100<0,0,
                                         ifelse(Temp.Threshold.2C<1,0,1))]
 prim.data[,Temp.Threshold.2C.Cost.5Percent:=ifelse(Total.Policy.Cost.100<(-1*.05),0,
                                             ifelse(Temp.Threshold.2C<1,0,1))]
 prim.data[,Temp.Threshold.2C.Cost.10Percent:=ifelse(Total.Policy.Cost.100<(-.10),0,
                                             ifelse(Temp.Threshold.2C<1,0,1))]
 prim.data[,Temp.Threshold.2C.Cost.15Percent:=ifelse(Total.Policy.Cost.100<(-.15),0,
                                             ifelse(Temp.Threshold.2C<1,0,1))] 														  
 prim.data[,Temp.Threshold.2C.Cost.20Percent:=ifelse(Total.Policy.Cost.100<(-.20),0,
                                             ifelse(Temp.Threshold.2C<1,0,1))]
 prim.data[,Temp.Threshold.2C.Cost.25Percent:=ifelse(Total.Policy.Cost.100<(-.25),0,
                                             ifelse(Temp.Threshold.2C<1,0,1))]
 prim.data[,Temp.Threshold.2C.Cost.30Percent:=ifelse(Total.Policy.Cost.100<(-.30),0,
                                             ifelse(Temp.Threshold.2C<1,0,1))]
#Stabilization
 prim.data[,Stabilization.Cost.Zero:=ifelse(Total.Policy.Cost.100<0,0,
                                         ifelse(Stabilization<1,0,1))]
 prim.data[,Stabilization.Cost.5Percent:=ifelse(Total.Policy.Cost.100<(-.05),0,
                                             ifelse(Stabilization<1,0,1))]
 prim.data[,Stabilization.Cost.10Percent:=ifelse(Total.Policy.Cost.100<(-.10),0,
                                             ifelse(Stabilization<1,0,1))]
 prim.data[,Stabilization.Cost.15Percent:=ifelse(Total.Policy.Cost.100<(-.15),0,
                                             ifelse(Stabilization<1,0,1))] 														  
 prim.data[,Stabilization.Cost.20Percent:=ifelse(Total.Policy.Cost.100<(-.20),0,
                                             ifelse(Stabilization<1,0,1))]
 prim.data[,Stabilization.Cost.25Percent:=ifelse(Total.Policy.Cost.100<(-.25),0,
                                             ifelse(Stabilization<1,0,1))]
 prim.data[,Stabilization.Cost.30Percent:=ifelse(Total.Policy.Cost.100<(-.30),0,
                                             ifelse(Stabilization<1,0,1))]


#round of policy values 
 prim.data[,ce.tax_N:=signif(ce.tax_N, digits = 2)]
 prim.data[,ce.tax_S:=signif(ce.tax_S, digits = 2)]
 prim.data[,RD.subsidy_N:=signif(RD.subsidy_N, digits = 2)]
 prim.data[,RD.subsidy_S:=signif(RD.subsidy_S, digits = 2)]
 prim.data[,RD.subsidy.GF_N:=signif(RD.subsidy.GF_N, digits = 2)]
 prim.data[,Tec.subsidy_N:=signif(Tec.subsidy_N, digits = 2)]
 prim.data[,Tec.subsidy_S:=signif(Tec.subsidy_S, digits = 2)]
 prim.data[,Tec.subsidy.GF_N:=signif(Tec.subsidy.GF_N, digits = 2)]

#create scenario bins 
 prim.data[,epsilon.bin:=ifelse(epsilon<5.3,'low.epsilon',ifelse(epsilon<7.6,'medium.epsilon','high.epsilon'))] 
 prim.data[,Relative.Eta.bin:=ifelse(Relative.Eta<1.0,'low.eta','high.eta')]
 prim.data[,Relative.Gamma.bin:=ifelse(Relative.Gamma<1.0,'low.gamma','high.gamma')]
 prim.data[,Relative.Nu.bin:=ifelse(Relative.Nu<1.0,'low.nu','high.nu')]
 prim.data[,Climate.bin:=ifelse(Beta.Delta.Temp<4.0,'low.beta',ifelse(Beta.Delta.Temp<5.0,'medium.beta','high.beta'))]

#Combine all bins into one single combination 
 prim.data[,bin.type:=paste(epsilon.bin,Relative.Eta.bin,Relative.Gamma.bin,Relative.Nu.bin,Climate.bin)]

#subset to considered policies
#merge codes for policies
 policy.names<-data.frame(least.cost.policy=c("P0","P1","P2","P3","P4","P5","P6","P7"),
                           policy.name=c("FWA","Nordhaus","Nordhaus+TechnologyPolicy.Both","Nordhaus+TraditionalGreenClimateFund","Nordhaus+TraditionalGreenClimateFund+R&DS","Nordhaus+CoR&DGreenClimateFund","Nordhaus+CoR&DGreenClimateFund+TecS","Nordhaus+R&DGreenClimateFund"))

 prim.data<-merge(prim.data,policy.names,by="policy.name")

#subset to policies of interest
 prim.data<-subset(prim.data,prim.data$least.cost.policy%in%c("P1","P2","P4","P6","P7"))
 

 policy.names<-data.frame(least.cost.policy=c("P1","P2","P3","P4","P5","P6","P7"),
                            policy.name=c("Nordhaus","Nordhaus+TechnologyPolicy.Both","Nordhaus+TraditionalGreenClimateFund","Nordhaus+TraditionalGreenClimateFund+R&DS","Nordhaus+CoR&DGreenClimateFund","Nordhaus+CoR&DGreenClimateFund+TecS","Nordhaus+R&DGreenClimateFund"))
 policy.chars<-c("Policy.Start.Time","Policy.Duration",
                "ce.tax_N","RD.subsidy_N","RD.subsidy.GF_N",
				 "Tec.subsidy_N","Tec.subsidy.GF_N","ce.tax_S",
				 "RD.subsidy_S","Tec.subsidy_S","Total.Policy.Cost.100","bin.type")
 uncertainties<-c("epsilon","rho","Climate.Model","Beta.Delta.Temp","Relative.Gamma","Relative.Eta","Relative.Nu")

 
objective<-"Stabilization.Cost.10Percent" 
regret.objective<-"Total.Policy.Cost.100"
density.threshold<-0.5
 
#=====================================================================================================================================================================================
# This Function Maps the Least Regret Policy Into Each Bin
#==================================================================================================================================================================================== 								   

robust.mapping<-function(prim.data,objective,regret.objective,density.threshold) {
 bins.table<-data.table(as.data.frame(prim.data[, j=list(Objective=max(get(objective))),
                                    by = list( bin.type,
									           Future.ID)]))
 total.succesful<-sum(bins.table$Objective)										   
 bins.table[,future.count:=1]											   
 
 bins.table<-as.data.frame(bins.table[, j=list(Objective.cases=sum(Objective),
                                                   total.futures=sum(future.count)),
                                    by = list( bin.type)])								
 bins.table$density.objective<-bins.table$Objective.cases/bins.table$total.futures
 bins.table$coverage.objective<-bins.table$Objective.cases/total.succesful

##calculate the total cost per policy per bin
 bins.policy.table<-as.data.frame(prim.data[, j=list(Total.Policy.Cost.100=sum(Total.Policy.Cost.100),
                                                                Total.Policy.Utility=sum(Total.Policy.Utility),
																Total.Policy.Cost=sum(Total.Policy.Cost)),
                                    by = list( bin.type,
									           least.cost.policy)])
#map the corresponding option
 bins.key<-data.frame(bin.type=unique(bins.policy.table$bin.type))
 
 bins.objective<-apply(bins.key,1,function(x) { pivot<-subset(bins.policy.table,bins.policy.table$bin.type==x);
                              subset(pivot[,c("bin.type","least.cost.policy")],pivot[,regret.objective]==max(pivot[,regret.objective]))
                                        })
 bins.objective<-do.call("rbind",bins.objective)
 colnames(bins.objective)<-c("bin.type","least.cost.policy")

#merge with bins with  density information
 dim(bins.table) 
 dim(bins.objective)
 bins.objective<-merge(bins.objective,bins.table[,c("bin.type","density.objective","coverage.objective")],by="bin.type")
#just consider on cases above a threshold for density
 bins.objective$density.objective<-ifelse(bins.objective$density.objective>density.threshold,bins.objective$density.objective,0)
 bins.objective$coverage.objective<-ifelse(bins.objective$density.objective>density.threshold,bins.objective$coverage.objective,0)
 bins.objective$least.cost.policy<-as.character(bins.objective$least.cost.policy)
 bins.objective$objective.policy<-ifelse(bins.objective$density.objective>0.0,bins.objective$least.cost.policy,"other")
 bins.objective$least.cost.policy<-NULL
#change colnames
 colnames(bins.objective)<-c("bin.type",paste("density.",objective,sep=""),paste("coverage.",objective,sep=""),paste("policy.",objective,sep=""))
 return(bins.objective)
 }

#objective<-"Stabilization" 
regret.objective<-"Total.Policy.Cost.100"
density.threshold<-0.5
 
bins.stabilization<-robust.mapping(prim.data,"Stabilization",regret.objective,density.threshold) 
bins.stabilization.zero<-robust.mapping(prim.data,"Stabilization.Cost.Zero",regret.objective,density.threshold) 
bins.stabilization.5<-robust.mapping(prim.data,"Stabilization.Cost.5Percent",regret.objective,density.threshold) 
bins.stabilization.10<-robust.mapping(prim.data,"Stabilization.Cost.10Percent",regret.objective,density.threshold) 
bins.stabilization.15<-robust.mapping(prim.data,"Stabilization.Cost.15Percent",regret.objective,density.threshold) 
bins.stabilization.20<-robust.mapping(prim.data,"Stabilization.Cost.20Percent",regret.objective,density.threshold) 
bins.stabilization.25<-robust.mapping(prim.data,"Stabilization.Cost.25Percent",regret.objective,density.threshold) 
bins.stabilization.30<-robust.mapping(prim.data,"Stabilization.Cost.30Percent",regret.objective,density.threshold) 

bins.temprise<-robust.mapping(prim.data,"Temp.Threshold.2C",regret.objective,density.threshold) 
bins.temprise.zero<-robust.mapping(prim.data,"Temp.Threshold.2C.Cost.Zero",regret.objective,density.threshold) 
bins.temprise.5percent<-robust.mapping(prim.data,"Temp.Threshold.2C.Cost.5Percent",regret.objective,density.threshold) 
bins.temprise.10percent<-robust.mapping(prim.data,"Temp.Threshold.2C.Cost.10Percent",regret.objective,density.threshold) 
bins.temprise.15percent<-robust.mapping(prim.data,"Temp.Threshold.2C.Cost.15Percent",regret.objective,density.threshold) 
bins.temprise.20percent<-robust.mapping(prim.data,"Temp.Threshold.2C.Cost.20Percent",regret.objective,density.threshold) 
bins.temprise.25percent<-robust.mapping(prim.data,"Temp.Threshold.2C.Cost.25Percent",regret.objective,density.threshold) 
bins.temprise.30percent<-robust.mapping(prim.data,"Temp.Threshold.2C.Cost.30Percent",regret.objective,density.threshold) 


#==============================================================================================================================================
#merge both stabilization and temperature 
#======================================================================================================================================
 bins.results<-Reduce(function(...) { merge(..., all=TRUE) }, list(bins.stabilization,
                                                                   bins.stabilization.zero,
																   bins.stabilization.5,
																   bins.stabilization.10,
																   bins.stabilization.15,
																   bins.stabilization.20,
																   bins.stabilization.25,
																   bins.stabilization.30,
																   bins.temprise,
																   bins.temprise.zero,
																   bins.temprise.5percent,
																   bins.temprise.10percent,
																   bins.temprise.15percent,
																   bins.temprise.20percent,
																   bins.temprise.25percent,
																   bins.temprise.30percent))


#prepare to make final table
 robust.mapping<-data.frame(subset(prim.data,prim.data$least.cost.policy=='P1'))
 robust.mapping<-unique(robust.mapping[,c("Future.ID","bin.type",policy.chars,uncertainties)])
 robust.mapping<-merge(robust.mapping,bins.results,by="bin.type")

#create final columns
 robust.mapping$least.cost.policy<-ifelse(robust.mapping$density.Stabilization>0.5,robust.mapping$policy.Stabilization,robust.mapping$policy.Temp.Threshold.2C)
 robust.mapping$objective<-ifelse(robust.mapping$density.Stabilization>0.5,'Stabilization',
                                  ifelse(robust.mapping$density.Temp.Threshold.2C>0.5,'Temperature.Rise','Failure'))

#write final file
   dir.prim<-paste(root,"RDM Outputs\\",sep="")  
   write.csv(robust.mapping, paste(dir.prim, "robust_mapping.csv", sep=""), row.names=FALSE)

#QA results using prim.data

#test the calculation in Tableau
   test<-data.frame(prim.data) 
   test<-subset(test,test$epsilon<5.3)
   test<-subset(test,test$Relative.Eta>=1.0)
   test<-subset(test,test$Relative.Gamma>=1.0)
   test<-subset(test,test$Relative.Nu<1.0)
   test<-subset(test,test$Beta.Delta.Temp>=4 & test$Beta.Delta.Temp<5)
   test$Freq<-1
   tab.test<-aggregate(test$Freq,list(test$least.cost.policy),sum)
   colnames(tab.test)<-c("policy","Freq")
   tab.test$share<-tab.test$Freq/sum(tab.test$Freq)


