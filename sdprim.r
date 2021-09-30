## =====================================================================================================
## This section runs Scenario Discovery in our Model
## =====================================================================================================
#pc
 #root<-"C:\\Users\\emolina\\Edmundo-RAND\\Projects\\Dissertation\\Model Development\\TechChange-RDM\\"
 #Number.Cores<-4
#cloud
 #root<-"C:\\Users\\Administrator\\Documents\\Edmundo\\TechChange-RDM\\"
 #Number.Cores<-30
#zed
 #root<-"F:\\E Restore\\E\\emolina\\Project Work\\Dissertation\\TechChange-RDM\\"
 #Number.Cores<-10
#chopper
 root<-"E:\\Projects\\EaSM\\TechChange-RDM\\"
 Number.Cores<-10

#load libraries
 library(sdtoolkit,lib=paste(root,"Rlibraries\\",sep=""))
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
  prim.data[,Policy.Costs.N:=Consumption.Total_N-Consumption.Total_N.fwa]
  prim.data[,Policy.Costs.S:=Consumption.Total_S-Consumption.Total_S.fwa]
  prim.data[,Total.Policy.Cost:=Policy.Costs.N+Policy.Costs.S]
  
#Define outputs of interest
 prim.data[,Temp.Threshold.2C:=ifelse(Delta.Temp<=2,1,0)]
 prim.data[,Temp.Threshold.3C:=ifelse(Delta.Temp<=3,1,0)]
 prim.data[,Stabilization:=ifelse(Policy.Duration+(Policy.Start.Time+2012)>=2123,0,
                                                 ifelse(Delta.Temp>2,0,
												          ifelse(Delta.Temp.300yr<=2,1,0)))] 
 prim.data[,Cost.N:=ifelse(Policy.Costs.N>=0,1,0)]
 prim.data[,Cost.S:=ifelse(Policy.Costs.S>=0,1,0)]
 prim.data[,Cost.Both:=ifelse(Cost.N+Cost.S>=2,1,0)]
 
#round of policy values 
 prim.data[,ce.tax_N:=signif(ce.tax_N, digits = 2)]
 prim.data[,ce.tax_S:=signif(ce.tax_S, digits = 2)]
 prim.data[,RD.subsidy_N:=signif(RD.subsidy_N, digits = 2)]
 prim.data[,RD.subsidy_S:=signif(RD.subsidy_S, digits = 2)]
 prim.data[,RD.subsidy.GF_N:=signif(RD.subsidy.GF_N, digits = 2)]
 prim.data[,Tec.subsidy_N:=signif(Tec.subsidy_N, digits = 2)]
 prim.data[,Tec.subsidy_S:=signif(Tec.subsidy_S, digits = 2)]
 prim.data[,Tec.subsidy.GF_N:=signif(Tec.subsidy.GF_N, digits = 2)]
 

#summary of performance
 sum.table<-as.data.frame(prim.data[, j=list(Temp.Threshold.2C=mean(Temp.Threshold.2C),
									       Temp.Threshold.3C=mean(Temp.Threshold.3C),
										   Stabilization=mean(Stabilization)),
                                    by = list( policy.name)])

									
									
									

#Filter data for analysis
 dir.inputs<-paste(root,"RDM Inputs\\",sep="")
 Policies.File<-"Policies.csv"
 Climate.File<-"Climate.csv"
 Policies<-read.csv(paste(dir.inputs,Policies.File,sep=""))
 Climate<-read.csv(paste(dir.inputs,Climate.File,sep=""))
 
#select prim inputs
 inputs.vector<-c(#"Relative.A_N",
                  #"Relative.A_S",
				  #"ProductivityGap",
				  "epsilon",
				  "rho",
				  #"Gamma.re",
				  #"Gamma.ce",
				  #"Eta.re",
				  #"Eta.ce",
				  #"Nu.re",
				  #"Nu.ce"
				  #"RelPrice_N",
				  #"RelPrice_S",
				  #"Relative.Gamma",
				  #"Relative.Eta",
				  #"Relative.Eta.Gamma",
				  #"Relative.Nu",
				  #"Relative.k"
				  #"Relative.k.Nu",
				  #"Share.re_N",
				  #"DiffusionGap",
				  #"Share.re_S",
				  "Beta.Delta.Temp",
				  #"qsi",
				  #"Delta.S"
				  #"Climate.Coef.fwa"
                  #"CO2.2032",
				  "CO2.2032",
                  "CO2.2042",
	              "CO2.2052",
	              "CO2.2062",
	              "CO2.2072",
	              "CO2.2082",
	              "CO2.2092",
                  "CO2.2062"
                  #"Delta.Temp.2032",
                  #"Delta.Temp.2057"
				  #"CO2.Concentration",
				 # "CO2.Concentration.2050",
				 #"Policy.Start.Time",
				 #"Policy.Duration",
				 #"ce.tax_N",
				 #"RD.subsidy_N",
				 #"RD.subsidy.GF_N",
				 #"Tec.subsidy_N",
				 #"Tec.subsidy.GF_N",
				 #"ce.tax_S",
				 #"RD.subsidy_S",
				 #"Tec.subsidy_S"
				  )

#run prim
 #Choose subset options
 target.policy<-(
                 #"FWA"
                 "Nordhaus"
				 #"Nordhauds+TechnologyPolicy"
				 #"Nordhaus+TraditionalGreenClimateFund"
				 #"Nordhaus+R&DGreenClimateFund"
				 #"Nordhaus+Matching+R&D-GreenClimateFund"
				 )

 
# target.climate<-Climate$Climate.Model[6]
 target.policy
 #target.climate
#subset prim data
 prim<-subset(prim.data,prim.data$policy.name==target.policy)

# prim<-subset(prim,prim$Climate.Model==target.climate)
#remove extreme values for epsilon 
 # prim<-subset(prim,prim$epsilon>4.8)

# prim<-subset(prim,!(prim$epsilon%in%c(2,8)))
# prim<-subset(prim,!(prim$epsilon%in%c(5)))
 
 prim.inputs<-prim[,inputs.vector,with=FALSE]
 prim.objective <- prim[,"Stabilization",with=FALSE]
 #prim.objective <- prim[,"Cost.N",with=FALSE]
 #prim.objective <- prim[,"Cost.S",with=FALSE]
 #prim.objective <- prim[,"Temp.Threshold.4C",with=FALSE]
 
#
#analyse correlation of inputs
 #cor(prim.inputs)
 
#using PCA
 #library(psych,lib=paste(root,"Rlibraries\\",sep=""))
 #fit <- principal(prim.inputs, nfactors=4, rotate="varimax",scores=TRUE)
 #PCA.factors<-data.frame(fit$scores)


 #plot(prim$Share.re_S,prim$Share.re_N)
 sdprim(prim.inputs,prim.objective, thresh=0.5, threshtype=">",peel_crit = 3,repro = FALSE)  #objective is achive: > 0.5
 
 
##analyse taxonomy
 #create policy taxonomy
  policy.chars<-c("Policy.Start.Time","Policy.Duration",
                "ce.tax_N","RD.subsidy_N","RD.subsidy.GF_N",
				 "Tec.subsidy_N","Tec.subsidy.GF_N","ce.tax_S",
				 "RD.subsidy_S","Tec.subsidy_S")			 
   prim.data[,e1:=ifelse(Policy.Duration<100,1.0,0)]
   prim.data[,e2:=ifelse(ce.tax_N>ce.tax_S,1.0,
                         ifelse(ce.tax_N<ce.tax_S,-1.0,0))]
   prim.data[,e3:=ifelse(Tec.subsidy_N>Tec.subsidy_S,1.0,
                         ifelse(Tec.subsidy_N<Tec.subsidy_S,-1.0,0))]
   prim.data[,e4:=ifelse(Tec.subsidy.GF_N>Tec.subsidy_S,1.0,
                         ifelse(Tec.subsidy.GF_N<Tec.subsidy_S,-1.0,0))]
   prim.data[,e5:=ifelse(RD.subsidy_N>RD.subsidy_S,1.0,
                         ifelse(RD.subsidy_N<RD.subsidy_S,-1.0,0))]
   prim.data[,e6:=ifelse(RD.subsidy.GF_N>RD.subsidy_S,1.0,
                         ifelse(RD.subsidy.GF_N<RD.subsidy_S,-1.0,0))]	
   prim.data[,e7:=ifelse(Tec.subsidy_N>Tec.subsidy_S+Tec.subsidy.GF_N,1.0,
                         ifelse(Tec.subsidy_N<Tec.subsidy_S+Tec.subsidy.GF_N,-1.0,0))]
   prim.data[,e8:=ifelse(RD.subsidy_N>RD.subsidy_S+RD.subsidy.GF_N,1.0,
                         ifelse(RD.subsidy_N<RD.subsidy_S+RD.subsidy.GF_N,-1.0,0))]
 #Choose subset options
 target.policy<-(
                 #"Nordhaus"
				 #"Nordhauds+TechnologyPolicy"
				 #"Nordhaus+TraditionalGreenClimateFund"
				 "Nordhaus+R&DGreenClimateFund"
				 )
 
#P1.Nordhaus 
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e1,e2,sep=",")]
  P1.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e1,e2)])
  P1.Taxonomy[order(-P1.Taxonomy$freq),]
  prim<-data.frame(prim)
  prim.P1<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")] 
  prim.P1$policy.name<-"P1"

  
  
#P2.Nordhauds+TechnologyPolicy
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhauds+TechnologyPolicy")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e1,e2,sep=",")]
  P2.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e1,e2)])
  P2.Taxonomy[order(-P2.Taxonomy$freq),]
  
#see portfolios  
  prim<-data.frame(prim)
  prim.P2<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")] 
  prim.P2$policy.name<-"P2"

 
#P3.Nordhauds+TechnologyPolicy.Both
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+TechnologyPolicy.Both")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e1,e2,e3,e5,sep=",")]
  P3.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e1,e2,e3,e5)])
  P3.Taxonomy[order(-P3.Taxonomy$freq),]
  
#see portfolios  
  prim<-data.frame(prim)
  prim.P3<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")] 
  prim.P3$policy.name<-"P3"

 

 #P4.Nordhaus+TraditionalGreenClimateFund
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+TraditionalGreenClimateFund")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2","e3","e4"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e1,e2,e3,e4,e7,sep=",")]
  P4.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e1,e2,e3,e4,e7)])
  P4.Taxonomy[order(-P4.Taxonomy$freq),]

  prim<-data.frame(prim)
  prim.P4<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")] 
  prim.P4$policy.name<-"P4"

  
 #P5.Nordhaus+TraditionalGreenClimateFund+R&DS
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+TraditionalGreenClimateFund+R&DS")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2","e3","e4"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e1,e2,e3,e4,e5,e7,sep=",")]
  P5.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e1,e2,e3,e4,e5,e7)])
  P5.Taxonomy[order(-P5.Taxonomy$freq),]

  prim<-data.frame(prim)
  prim.P5<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")] 
  prim.P5$policy.name<-"P5"


#analyze the structure of portfolios
  target.portfolios<-c("1,0,1,0,1,-1")
  subset(prim[,policy.chars],prim$taxonomy%in%target.portfolios)


#P6.Nordhaus+CoR&DGreenClimateFund
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+CoR&DGreenClimateFund")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2","e3","e4"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e1,e2,e3,e5,e6,e8,sep=",")]
  P6.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e1,e2,e3,e5,e6,e8)])
  P6.Taxonomy[order(-P6.Taxonomy$freq),]

  prim<-data.frame(prim)
  prim.P6<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")] 
  prim.P6$policy.name<-"P6"


#analyze the structure of portfolios
  target.portfolios<-c("1,0,1,0,1,-1")
  subset(prim[,policy.chars],prim$taxonomy%in%target.portfolios)

 
#P7.Nordhaus+CoR&DGreenClimateFund+TecS
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+CoR&DGreenClimateFund+TecS")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2","e3","e4"),with=FALSE])
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e1,e2,e3,e5,e6,e8,sep=",")]
  P7.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(taxonomy,e1,e2,e3,e5,e6,e8)])
  P7.Taxonomy[order(-P7.Taxonomy$freq),]

   prim<-data.frame(prim)
  prim.P7<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")] 
  prim.P7$policy.name<-"P7"


#analyze the structure of portfolios
  target.portfolios<-c("1,0,1,0,1,-1")
  subset(prim[,policy.chars],prim$taxonomy%in%target.portfolios)

  
#P8.Nordhaus+R&DGreenClimateFund
 #subset to policy
   prim<-subset(prim.data,prim.data$policy.name=="Nordhaus+R&DGreenClimateFund")
 #subset to target
   prim<-subset(prim,prim$Stabilization>0.5)
 #taxonomy of success
   #unique(prim[,c("e1","e2","e3","e4","e5","e6"),with=FALSE]) 
 #frequency table
  prim[,freq:=1]
  prim[,taxonomy:=paste(e1,e2,e3,e4,e5,e6,e7,e8,sep=",")]
  P8.Taxonomy<-as.data.frame(prim[, j=list(freq=sum(freq)),
                                    by = list(e1,e2,e3,e4,e5,e6,e7,e8)])
  P8.Taxonomy[order(-P8.Taxonomy$freq),]
   
  prim<-data.frame(prim)
  prim.P8<-prim[,c("Future.ID","taxonomy",policy.chars,"Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")]
  prim.P8$policy.name<-"P8"  

#understanding change in futures
 
  subset(prim.P8$Future.ID,!(prim.P8$Future.ID%in%prim.P3$Future.ID))
   
#with all these policies how many futures are not vulnerable
  all.futures<-unique(c(
                        prim.P1$Future.ID,
                        prim.P2$Future.ID,
                        prim.P3$Future.ID,
                        prim.P4$Future.ID,
                        prim.P5$Future.ID,
                        prim.P6$Future.ID,
                        prim.P7$Future.ID,
                        prim.P8$Future.ID))		 
#find dominated policies
#build a table with the cost of each policy 
  
  prim.all<-rbind(prim.P1,prim.P2,prim.P3,prim.P4,prim.P5,prim.P6,prim.P7,prim.P8)
  
  futures.key<-apply(data.frame(all.futures=all.futures),1,function(x) { policy.table.future<-subset(prim.all[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")],prim.all$Future.ID==x);
                                      subset(policy.table.future[,c("Future.ID","policy.name")],policy.table.future$Total.Policy.Cost==max(policy.table.future$Total.Policy.Cost))   
                                               })
  futures.key<-do.call("rbind",futures.key)
  futures.key$Freq<-1
  futures.policy.table<-aggregate(futures.key$Freq,list(policy.name=futures.key$policy.name),sum)
  colnames(futures.policy.table)<-c("policy.name","Freq")
  futures.policy.table[order(-futures.policy.table$Freq),]
  
#now analyse for both independently
#emerging region 
 futures.key.S<-apply(data.frame(all.futures=all.futures),1,function(x) { policy.table.future<-subset(prim.all[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")],prim.all$Future.ID==x);
                                      subset(policy.table.future[,c("Future.ID","policy.name")],policy.table.future$Policy.Costs.S==max(policy.table.future$Policy.Costs.S))   
                                               })
  futures.key.S<-do.call("rbind",futures.key.S)
  colnames(futures.key.S)<-c("Future.ID","policy.name.S")
#advanced region 
 futures.key.N<-apply(data.frame(all.futures=all.futures),1,function(x) { policy.table.future<-subset(prim.all[,c("Future.ID","policy.name","Policy.Costs.S","Policy.Costs.N","Total.Policy.Cost")],prim.all$Future.ID==x);
                                      subset(policy.table.future[,c("Future.ID","policy.name")],policy.table.future$Policy.Costs.N==max(policy.table.future$Policy.Costs.N))   
                                               })
  futures.key.N<-do.call("rbind",futures.key.N)
  colnames(futures.key.N)<-c("Future.ID","policy.name.N")

#merge both
  futures.key.NS<-merge(futures.key.S,futures.key.N,by="Future.ID")
  futures.key.NS$Cop<-ifelse(futures.key.NS$policy.name.N==futures.key.NS$policy.name.S,1,0)
#cooperation
  futures.policy.table.Cop<-subset(futures.key.NS,futures.key.NS$Cop==1)
  futures.policy.table.Cop<-aggregate(futures.policy.table.Cop$Cop,list(policy.name=futures.policy.table.Cop$policy.name.S),sum)
  colnames(futures.policy.table.Cop)<-c("policy.name","Freq")
  futures.policy.table.Cop[order(-futures.policy.table.Cop$Freq),]
 
#non-cooperation 
  subset(futures.key.NS,futures.key.NS$Cop==0) 
  
 
#prim analysis to understand under which conditions each portfolio is implemented  
#merge with inputs 
  futures.key<-merge(futures.key,data.frame(unique(prim.data[,c("Future.ID",inputs.vector),with=FALSE])),by="Future.ID")
#flag
 futures.key$flag<-ifelse(futures.key$policy.name=="P8",1,0)
 summary(subset(futures.key,futures.key$flag==1))
 
#sdprim
 sdprim(futures.key[,inputs.vector],futures.key[,"flag"], thresh=0.5, threshtype=">",peel_crit = 1,repro = FALSE)
 














 
#do prim analysis
  target.portfolios<-c("1,0,1,0,1,0,0,1")
  prim<-data.frame(prim)
  subset(prim[,policy.chars],prim$taxonomy%in%target.portfolios)
 #
  prim.inputs<-prim[,inputs.vector]
  prim$flag <- ifelse(prim$taxonomy%in%target.portfolios,1,0)
  sdprim(prim.inputs,prim[,"flag"], thresh=0.5, threshtype=">",peel_crit = 1,repro = FALSE)
 
  
