#=====================================================================================================================================================================================
# Process Stabilization Target
#==================================================================================================================================================================================== 
objective<-"Stabilization"
policy.names<-data.frame(least.cost.policy=c("P1","P2","P3","P4","P5","P6","P7"),
                           policy.name=c("Nordhaus","Nordhaus+TechnologyPolicy.Both","Nordhaus+TraditionalGreenClimateFund","Nordhaus+TraditionalGreenClimateFund+R&DS","Nordhaus+CoR&DGreenClimateFund","Nordhaus+CoR&DGreenClimateFund+TecS","Nordhaus+R&DGreenClimateFund"))
policy.chars<-c("Policy.Start.Time","Policy.Duration",
                "ce.tax_N","RD.subsidy_N","RD.subsidy.GF_N",
				 "Tec.subsidy_N","Tec.subsidy.GF_N","ce.tax_S",
				 "RD.subsidy_S","Tec.subsidy_S","Total.Policy.Cost.100")
uncertainties<-c("epsilon","rho","Climate.Model","Beta.Delta.Temp","Relative.Gamma","Relative.Eta","Relative.Nu")


#P1.Nordhaus 
 #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P1.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P1.t$policy.name<-"P1"
 
#P2.Nordhauds+TechnologyPolicy.Both
 #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+TechnologyPolicy.Both"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P2.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P2.t$policy.name<-"P2"


#P3.Nordhaus+TraditionalGreenClimateFund
  #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+TraditionalGreenClimateFund"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P3.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P3.t$policy.name<-"P3"
   
#P4.Nordhaus+TraditionalGreenClimateFund+R&DS
   #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+TraditionalGreenClimateFund+R&DS"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P4.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P4.t$policy.name<-"P4"

#P5.Nordhaus+CoR&DGreenClimateFund
  #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+CoR&DGreenClimateFund"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P5.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P5.t$policy.name<-"P5"

#P6.Nordhaus+CoR&DGreenClimateFund+TecS
 #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+CoR&DGreenClimateFund+TecS"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P6.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P6.t$policy.name<-"P6"

 #P7.Nordhaus+R&DGreenClimateFund
  #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+R&DGreenClimateFund"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P7.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P7.t$policy.name<-"P7"
 
  
#with all these policies how many futures are not vulnerable
  all.futures<-unique(c(
                        prim.P1.t$Future.ID,
                        prim.P2.t$Future.ID,
#                        prim.P3.t$Future.ID,
#                        prim.P4.t$Future.ID,
#                        prim.P5.t$Future.ID,
#                        prim.P6.t$Future.ID,
                        prim.P7.t$Future.ID))		 

#build a table with the cost of each policy 
#  prim.all<-rbind(prim.P1.t,prim.P2.t,prim.P3.t,prim.P4.t,prim.P5.t,prim.P6.t,prim.P7.t)
#  prim.all<-rbind(prim.P1.t,prim.P2.t,prim.P4.t,prim.P6.t,prim.P7.t)
  prim.all<-rbind(prim.P1.t,prim.P2.t,prim.P7.t)
  prim.all$Policy.Costs.S<-round(prim.all$Policy.Costs.S,digits=0)
  prim.all$Policy.Costs.N<-round(prim.all$Policy.Costs.N,digits=0)
  prim.all$Total.Policy.Cost<-round(prim.all$Total.Policy.Cost,digits=0)
 
 #find dominated policies
  futures.key<-apply(data.frame(all.futures=all.futures),1,function(x) {policy.table.future<-subset(prim.all[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility")],prim.all$Future.ID==x);
                                     pivot<-subset(policy.table.future[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility")],policy.table.future$Total.Policy.Cost==max(policy.table.future$Total.Policy.Cost));
                                     #pivot<-subset(policy.table.future[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility")],policy.table.future$Total.Policy.Utility==max(policy.table.future$Total.Policy.Utility));
									 subset(pivot[,c("Future.ID","policy.name")],pivot$Delta.Temp==min(pivot$Delta.Temp))									  
                                               })
  futures.key<-do.call("rbind",futures.key)
  futures.key$Freq<-1
  futures.policy.table<-aggregate(futures.key$Freq,list(policy.name=futures.key$policy.name),sum)
  colnames(futures.policy.table)<-c("policy.name","Freq")
  futures.policy.table[order(-futures.policy.table$Freq),]

#reshape
  futures.key$Freq<-NULL
  colnames(futures.key)<-c("Future.ID","least.cost.policy")
  futures.policies.table<- futures.key
				 
#create robust mapping table
  all.futures.mapping<-futures.policies.table[,c("Future.ID","least.cost.policy")]
#subset to only selected policies
  all.policies<-subset(policy.names,policy.names$least.cost.policy%in%unique(futures.policies.table$least.cost.policy))
  all.policies$policy.name<-as.character(all.policies$policy.name)
  prim.data$policy.name<-as.character(prim.data$policy.name)
  policy.vector<-data.frame(prim.data[,c("Future.ID","policy.name",policy.chars,uncertainties),with=FALSE])
#map policy response to  
  robust.mapping<-subset(all.futures.mapping,all.futures.mapping$least.cost.policy==all.policies$least.cost.policy[1])
  policy.pivot<-subset(policy.vector,policy.vector$policy.name==all.policies$policy.name[1])
  robust.mapping<-merge(robust.mapping,policy.pivot,by="Future.ID")
  for (i in 2:nrow(all.policies))
  {
   robust.pivot<-subset(all.futures.mapping,all.futures.mapping$least.cost.policy==all.policies$least.cost.policy[i])
   policy.pivot<-subset(policy.vector,policy.vector$policy.name==all.policies$policy.name[i])
   robust.pivot<-merge(robust.pivot,policy.pivot,by="Future.ID")
   robust.mapping<-rbind(robust.mapping,robust.pivot)
  }
  robust.mapping.stabilization<-robust.mapping

 
#=====================================================================================================================================================================================
# Process The Temperature Rise target
#==================================================================================================================================================================================== 
objective<-"Temp.Threshold.2C"
policy.names<-data.frame(least.cost.policy=c("P1","P2","P3","P4","P5","P6","P7"),
                           policy.name=c("Nordhaus","Nordhaus+TechnologyPolicy.Both","Nordhaus+TraditionalGreenClimateFund","Nordhaus+TraditionalGreenClimateFund+R&DS","Nordhaus+CoR&DGreenClimateFund","Nordhaus+CoR&DGreenClimateFund+TecS","Nordhaus+R&DGreenClimateFund"))
policy.chars<-c("Policy.Start.Time","Policy.Duration",
                "ce.tax_N","RD.subsidy_N","RD.subsidy.GF_N",
				 "Tec.subsidy_N","Tec.subsidy.GF_N","ce.tax_S",
				 "RD.subsidy_S","Tec.subsidy_S","Total.Policy.Cost.100")
uncertainties<-c("epsilon","rho","Climate.Model","Beta.Delta.Temp","Relative.Gamma","Relative.Eta","Relative.Nu")

#P1.Nordhaus 
 #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P1.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P1.t$policy.name<-"P1"
 
#P2.Nordhauds+TechnologyPolicy.Both
 #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+TechnologyPolicy.Both"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P2.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P2.t$policy.name<-"P2"


#P3.Nordhaus+TraditionalGreenClimateFund
  #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+TraditionalGreenClimateFund"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P3.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P3.t$policy.name<-"P3"
   
#P4.Nordhaus+TraditionalGreenClimateFund+R&DS
   #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+TraditionalGreenClimateFund+R&DS"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P4.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P4.t$policy.name<-"P4"

#P5.Nordhaus+CoR&DGreenClimateFund
  #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+CoR&DGreenClimateFund"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P5.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P5.t$policy.name<-"P5"

#P6.Nordhaus+CoR&DGreenClimateFund+TecS
 #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+CoR&DGreenClimateFund+TecS"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P6.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P6.t$policy.name<-"P6"

 #P7.Nordhaus+R&DGreenClimateFund
  #subset to policy
   prim<-data.frame(subset(prim.data,prim.data$policy.name=="Nordhaus+R&DGreenClimateFund"))
 #subset to target
   prim<-subset(prim,prim[,objective]>0.5)
   prim.P7.t<-prim[,c("Future.ID",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility","Total.Policy.Cost.100")] 
   prim.P7.t$policy.name<-"P7"
 
  
#with all these policies how many futures are not vulnerable
  all.futures<-unique(c(
                        prim.P1.t$Future.ID,
                        prim.P2.t$Future.ID,
#                        prim.P3.t$Future.ID,
#                        prim.P4.t$Future.ID,
#                        prim.P5.t$Future.ID,
#                        prim.P6.t$Future.ID,
                        prim.P7.t$Future.ID))		 

#build a table with the cost of each policy 
#  prim.all<-rbind(prim.P1.t,prim.P2.t,prim.P3.t,prim.P4.t,prim.P5.t,prim.P6.t,prim.P7.t)
#   prim.all<-rbind(prim.P1.t,prim.P2.t,prim.P4.t,prim.P6.t,prim.P7.t)
  prim.all<-rbind(prim.P1.t,prim.P2.t,prim.P7.t)
  prim.all$Policy.Costs.S<-round(prim.all$Policy.Costs.S,digits=0)
  prim.all$Policy.Costs.N<-round(prim.all$Policy.Costs.N,digits=0)
  prim.all$Total.Policy.Cost<-round(prim.all$Total.Policy.Cost,digits=0)
 
 #find dominated policies
  futures.key<-apply(data.frame(all.futures=all.futures),1,function(x) { policy.table.future<-subset(prim.all[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility")],prim.all$Future.ID==x);
                                      pivot<-subset(policy.table.future[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp")],policy.table.future$Total.Policy.Cost==max(policy.table.future$Total.Policy.Cost));
                                      #pivot<-subset(policy.table.future[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost","Delta.Temp","Total.Policy.Utility")],policy.table.future$Total.Policy.Utility==max(policy.table.future$Total.Policy.Utility));
									  subset(pivot[,c("Future.ID","policy.name")],pivot$Delta.Temp==min(pivot$Delta.Temp))[1,]									  
                                               })
  futures.key<-do.call("rbind",futures.key)
  futures.key$Freq<-1
  futures.policy.table<-aggregate(futures.key$Freq,list(policy.name=futures.key$policy.name),sum)
  colnames(futures.policy.table)<-c("policy.name","Freq")
  futures.policy.table[order(-futures.policy.table$Freq),]

#reshape
  futures.key$Freq<-NULL
  colnames(futures.key)<-c("Future.ID","least.cost.policy")
  futures.policies.table<- futures.key
				 
#create robust mapping table
  all.futures.mapping<-futures.policies.table[,c("Future.ID","least.cost.policy")]
#subset to only selected policies
  all.policies<-subset(policy.names,policy.names$least.cost.policy%in%unique(futures.policies.table$least.cost.policy))
  all.policies$policy.name<-as.character(all.policies$policy.name)
  prim.data$policy.name<-as.character(prim.data$policy.name)
  policy.vector<-data.frame(prim.data[,c("Future.ID","policy.name",policy.chars,uncertainties),with=FALSE])
#map policy response to  
  robust.mapping<-subset(all.futures.mapping,all.futures.mapping$least.cost.policy==all.policies$least.cost.policy[1])
  policy.pivot<-subset(policy.vector,policy.vector$policy.name==all.policies$policy.name[1])
  robust.mapping<-merge(robust.mapping,policy.pivot,by="Future.ID")
  for (i in 2:nrow(all.policies))
  {
   robust.pivot<-subset(all.futures.mapping,all.futures.mapping$least.cost.policy==all.policies$least.cost.policy[i])
   policy.pivot<-subset(policy.vector,policy.vector$policy.name==all.policies$policy.name[i])
   robust.pivot<-merge(robust.pivot,policy.pivot,by="Future.ID")
   robust.mapping<-rbind(robust.mapping,robust.pivot)
  }
  robust.mapping.temperature.rise<-robust.mapping 

#merge both robust mappings 
  robust.mapping.stabilization$objective<-"Stabilization"
  robust.mapping.temperature.rise$objective<-"Temperature.Rise"
#subset temperature rise
  robust.mapping.temperature.rise<-subset(robust.mapping.temperature.rise,!(robust.mapping.temperature.rise$Future.ID%in%robust.mapping.stabilization$Future.ID))
#add futures with no solution 
  bad.futures<-subset(policy.vector,policy.vector$policy.name=="FWA")
  bad.futures<-subset(bad.futures,!(bad.futures$Future.ID%in%unique(c(robust.mapping.stabilization$Future.ID,robust.mapping.temperature.rise$Future.ID))))
  bad.futures$least.cost.policy<-"Expand"    
  bad.futures$RD.subsidy_N<-2
  bad.futures$RD.subsidy.GF_N<-2
  bad.futures$Tec.subsidy_N<-0.5
  bad.futures$Tec.subsidy.GF_N<-0.5
  bad.futures$ce.tax_S<-1
  bad.futures$ce.tax_N<-1
  bad.futures$RD.subsidy_S<-2 
  bad.futures$Tec.subsidy_S<-0.5
  bad.futures$Policy.Start.Time<-9.9 
  bad.futures$Policy.Duration<-300
  bad.futures$objective<-"Failure"
#rbind all futures together
  dim(robust.mapping.stabilization)
  dim(robust.mapping.temperature.rise)
  dim(bad.futures)
  robust.mapping<-rbind(robust.mapping.stabilization,robust.mapping.temperature.rise,bad.futures)
  dim(robust.mapping)
#write final file
   dir.prim<-paste(root,"RDM Outputs\\",sep="")  
   write.csv(robust.mapping, paste(dir.prim, "robust_mapping.csv", sep=""), row.names=FALSE)

#create bins with the data

 robust.mapping<-data.table(robust.mapping)
#create bins
 robust.mapping[,epsilon.bin:=ifelse(epsilon<5.3,'low.epsilon',ifelse(epsilon<7.6,'medium.epsilon','high.epsilon'))] 
 robust.mapping[,Relative.Eta.bin:=ifelse(Relative.Eta<1.0,'low.eta','high.eta')]
 robust.mapping[,Relative.Gamma.bin:=ifelse(Relative.Gamma<1.0,'low.gamma','high.gamma')]
 robust.mapping[,Relative.Nu.bin:=ifelse(Relative.Nu<1.0,'low.nu','high.nu')]
 robust.mapping[,Climate.bin:=ifelse(Beta.Delta.Temp<4.0,'low.beta',ifelse(Beta.Delta.Temp<5.0,'medium.beta','high.beta'))]

#Combine all bins into one single combination 
 robust.mapping[,bin.type:=paste(epsilon.bin,Relative.Eta.bin,Relative.Gamma.bin,Relative.Nu.bin,Climate.bin)]
 
#calculate stabilization density across all these scenarios 
 #bins chars
 #summary of performance
 robust.mapping[,future.count:=1]
 robust.mapping[,stabilization.count:=ifelse(objective=='Stabilization',1,0)]
 
 bins.table<-as.data.frame(robust.mapping[, j=list(stabilization.cases=sum(stabilization.count),
                                                   total.futures=sum(future.count)),
                                    by = list( bin.type)])
 bins.table$density.stabilization<-bins.table$stabilization.cases/bins.table$total.futures

#do this per policy
 robust.mapping.stabilization<-subset(robust.mapping,objective=='Stabilization')
 bins.policy.table<-as.data.frame(robust.mapping.stabilization[, j=list(stabilization.cases=sum(stabilization.count),
                                                   total.futures=sum(future.count)),
                                    by = list( bin.type,
									           least.cost.policy)])
 bins.policy.table$total.futures<-NULL
 library(reshape2,lib= paste(root,"Rlibraries\\",sep=""))
#cross reference strategies
 bins.policy.table<-data.table(bins.policy.table)
 bins.policy.table<- data.table(dcast(bins.policy.table, 
                          bin.type
						  ~ least.cost.policy, value.var="stabilization.cases"))
#make NAs zeros: 
 bins.policy.table[,P1:=ifelse(is.na(P1)==TRUE,0,P1)]						  
 bins.policy.table[,P2:=ifelse(is.na(P2)==TRUE,0,P2)]
 bins.policy.table[,P7:=ifelse(is.na(P7)==TRUE,0,P7)]
 bins.policy.table<-data.frame(bins.policy.table)
#merge with bins table
 bins.table<-merge(bins.table,bins.policy.table,by="bin.type",all.x=TRUE)
#zero out policy columns to low density stabilization scenarios
 bins.table$P1<-ifelse(is.na(bins.table$P1)==TRUE,0,bins.table$P1)
 bins.table$P2<-ifelse(is.na(bins.table$P2)==TRUE,0,bins.table$P2)
 bins.table$P7<-ifelse(is.na(bins.table$P7)==TRUE,0,bins.table$P7)

#calculate densities
 bins.table$P1<-bins.table$P1/bins.table$stabilization.cases
 bins.table$P2<-bins.table$P2/bins.table$stabilization.cases
 bins.table$P7<-bins.table$P7/bins.table$stabilization.cases
#decide on stabilization policy for that region: 
 bins.table<-subset(bins.table,bins.table$density.stabilization>0.5)

 #test
  bins.table$Stabilization.Policy<-ifelse(bins.table$P1>bins.table$P2,
                                            ifelse(bins.table$P1>bins.table$P7,"P1","P7"),
                                            ifelse(bins.table$P2>bins.table$P7,"P2","P7"))											
											    
 
 


 bins.policy.table$P1.Stabilization.Counts<-ifelse(bins.policy.table$least.cost.policy=='P1',bins.policy.table$stabilization.cases,0)
 bins.policy.table$P2.Stabilization.Counts<-ifelse(bins.policy.table$least.cost.policy=='P2',bins.policy.table$stabilization.cases,0)
 bins.policy.table$P7.Stabilization.Counts<-ifelse(bins.policy.table$least.cost.policy=='P7',bins.policy.table$stabilization.cases,0)
   
 bins.policy.table$least.cost.policy<-NULL 
 bins.policy.table$stabilization.cases<-NULL 
 bins.policy.table$total.futures<-NULL  
 
 bins.policy.table<-data.table(bins.policy.table)
 
 test<-as.data.frame(robust.mapping.stabilization[, j=list(stabilization.cases=sum(stabilization.count),
                                                   total.futures=sum(future.count)),
                                    by = list( bin.type,
									           least.cost.policy)])
#merge bins.policy table with original bins table




   
#test the calculation in Tableau 
   test<-subset(robust.mapping,robust.mapping$objective=="Stabilization")
   test<-subset(test,test$epsilon>=6.5)
   test<-subset(test,test$Relative.Eta>=1.0)
   test<-subset(test,test$Relative.Gamma>=1.0)
   test<-subset(test,test$Relative.Nu>=1.0)
   test<-subset(test,test$Beta.Delta.Temp>5.0)
   test$Freq<-1
   tab.test<-aggregate(test$Freq,list(test$least.cost.policy),sum)
   colnames(tab.test)<-c("policy","Freq")
   tab.test$share<-tab.test$Freq/sum(tab.test$Freq)


#use scenario discovery to ask, when P7 is preferred over P2
 #select prim inputs
 inputs.vector<-c("epsilon",
				  "Beta.Delta.Temp",
				  "Relative.Gamma",
				  "Relative.Eta",
				  "Relative.Nu"
				  )

 #analyzing P1
 
 prim<-test
 prim$objective<-ifelse(prim$least.cost.policy=='P7',1,0)
 
 sdprim(prim[,inputs.vector],prim$objective, thresh=0.5, threshtype=">",peel_crit = 1,repro = FALSE)  #objective is achive: > 0.5

