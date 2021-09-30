## ==================================================================================================================================================
## This section run the model across the experimental design and prints an output file for each run 
## ==================================================================================================================================================
#Source Model
  dir.model<-paste(root,"TechChange Model\\",sep="")
  #model.version<-"InternationalGreenTechChangeModel_9_19_2015.r"
  model.version<-"InternationalGreenTechChangeModel_10_17_2015_test.r"
  source(paste(dir.model,model.version,sep=""))
#Source Experimental Design
  dir.exp<-paste(root,"RDM Inputs\\",sep="")
  #experiment.version<-"Exp.design_calib.csv"
  experiment.version<-"Exp.design_P3_p1.csv"
  #experiment.version<-"Exp.design_climate_tests2.csv"
  Exp.design<-read.csv(paste(dir.exp,experiment.version,sep=""))
#Define directory to print output files
  dir.harness<-paste(root,"RDM Harness\\",sep="")
#Clean output folder
  do.call(file.remove,list(paste(dir.harness,list.files(dir.harness, pattern="*.csv", full.names=FALSE),sep="")))
#Set up parallel environment
#Run Model in Parallel
  library(snow,lib=paste(root,"Rlibraries\\",sep=""))
  library(deSolve,lib=paste(root,"Rlibraries\\",sep=""))
  library(optimx,lib=paste(root,"Rlibraries\\",sep=""))
  nCore<-Number.Cores
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("Exp.design","TechChangeMod","dir.harness","dede","lagderiv","optimx") # dede, lagderiv are functions od deSolve
  clusterExport(cl,global.elements,envir=environment()) 
 #Execute code
  parApply(cl,Exp.design,1,function(x) {  
                                     params<-c(
                                     S.0 = as.numeric(x['S.0']), 
                                     TimeStep = as.numeric(x['TimeStep']), 
                                     EndTime = as.numeric(x['EndTime']), 
                                     alfa = as.numeric(x['alfa']), 
                                     epsilon = as.numeric(x['epsilon']),
                                     Gamma.re = as.numeric(x['Gamma.re']),
                                     k.re = as.numeric(x['k.re']), 
                                     Gamma.ce = as.numeric(x['Gamma.ce']), 
                                     k.ce = as.numeric(x['k.ce']), 
                                     Eta.re = as.numeric(x['Eta.re']), 
                                     Eta.ce = as.numeric(x['Eta.ce']), 
                                     Nu.re = as.numeric(x['Nu.re']), 
                                     Nu.ce = as.numeric(x['Nu.ce']), 
                                     qsi = as.numeric(x['qsi']), 
                                     Delta.S = as.numeric(x['Delta.S']),
						             Delta.Temp.Disaster = as.numeric(x['Delta.Temp.Disaster']),
						             Beta.Delta.Temp = as.numeric(x['Beta.Delta.Temp']),
						             CO2.base = as.numeric(x['CO2.base']),
						             CO2.Disaster = as.numeric(x['CO2.Disaster']),
                                     labor.growth_N = as.numeric(x['labor.growth_N']),
						             labor.growth_S = as.numeric(x['labor.growth_S']),
                                     lambda.S = as.numeric(x['lambda.S']),
						             sigma.utility = as.numeric(x['sigma.utility']),
						             rho = as.numeric(x['rho']),						 
                                     Yre.0_N = as.numeric(x['Yre.0_N']),
                                     Yce.0_N = as.numeric(x['Yce.0_N']), 
                                     Yre.0_S = as.numeric(x['Yre.0_S']), 
                                     Yce.0_S = as.numeric(x['Yce.0_S']), 
						             size.factor = as.numeric(x['size.factor']),
						             Run.ID = as.numeric(x['Run.ID']),
									 policy.name = as.character(x['policy.name']),
						             dir.harness = dir.harness);

#Policy dimensions
#start time
 st.0<-0.033 ;#initial start.time
 st.m<-0.03 ;#min start.time
 st.M<-0.10 ;#max start.time 	
#duration
 d.0<-ifelse(as.numeric(x['epsilon'])<4.0,0.99,
     ifelse(as.numeric(x['epsilon'])<5.0,0.67,
	 ifelse(as.numeric(x['epsilon'])<6.0,0.50,
	 ifelse(as.numeric(x['epsilon'])<8.0,0.25,
     ifelse(as.numeric(x['epsilon'])<9.2,0.22,
	 ifelse(as.numeric(x['epsilon'])<10,0.20,0.18))))));#initial duration 
 d.m<-0.1 ;#min duration
 d.M<-1.0 ;#max duration
#tax north
 ceN.0<-ifelse(x['Climate.Model']%in%c("GFDL-ESM2G","GFDL-ESM2M")==TRUE,0.30,0.25) ;#initial tax north
 ceN.m<-0.05 ;#min tax north
 ceN.M<-0.5 ;#max tax north 
#tax south
 ceS.0<-ifelse(x['Climate.Model']%in%c("GFDL-ESM2G","GFDL-ESM2M")==TRUE,0.30,0.25) ;#initial tax south
 ceS.m<-0.05 ;#min tax south
 ceS.M<-0.5 ;#max tax south
#Tech Subsidy North
 tN.0<-0.10 ;#initial Tech Subsidy North
 tN.m<-0.01 ;#min tax Tech Subsidy North
 tN.M<-0.15 ;#max tax Tech Subsidy North
#RD Subsidy North
 sN.0<-2.0 ;#initial RD Subsidy North
 sN.m<-0.5 ;#min tax RD Subsidy North
 sN.M<-3.0 ;#max tax RD Subsidy North	
#Tech Subsidy GCF
 tGF.0<-0.05 ;#initial Tech Subsidy GCF
 tGF.m<-0.01 ;#min tax Tech Subsidy GCF
 tGF.M<-0.15 ;#max tax Tech Subsidy GCF
#Tech Subsidy South
 tS.0<-0.05 ;#initial Tech Subsidy South
 tS.m<-0.01 ;#min tax Tech Subsidy South
 tS.M<-0.15 ;#max tax Tech Subsidy South
#RD Subsidy South
 sS.0<-0.50 ;#initial RD Subsidy South
 sS.m<-0.01 ;#min tax RD Subsidy South
 sS.M<-3.0 ;#max tax RD Subsidy South	
#RD Subsidy GCF
 sGF.0<-0.50 ;#initial RD Subsidy GCF
 sGF.m<-0.01 ;#min tax RD Subsidy GCF
 sGF.M<-3.0 ;#max tax RD Subsidy GCF	
	
if (x['policy.name']=="FWA")
 {
 TechChangeMod(c(0.0,0.0,0.0,0.0),params)

 } else{
       if (x['policy.name']=="Nordhaus")
        {
          optimx(c(st.0,d.0,ceN.0,ceS.0), TechChangeMod, lower=c(st.m,d.m,ceN.m,ceS.m), upper=c(st.M,d.M,ceN.M,ceS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01),parscale=c(0.1,10,10,10),maxit=200000),params=params)
         } else {
		 if (x['policy.name']=="Nordhauds+TechnologyPolicy")
		 {
		  optimx(c(st.0,d.0,ceN.0,ceS.0,tN.0,sN.0), TechChangeMod, lower=c(st.m,d.m,ceN.m,ceS.m,tN.m,sN.m), upper=c(st.M,d.M,ceN.M,ceS.M,tN.M,sN.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(1,10,10,10,3,3),maxit=200000),params=params)
		 } else {
		   if (x['policy.name']=="Nordhaus+TraditionalGreenClimateFund")
		  {
		   	optimx(c(st.0,d.0,ceN.0,ceS.0,tN.0,sN.0,tGF.0,tS.0), TechChangeMod, lower=c(st.m,d.m,ceN.m,ceS.m,tN.m,sN.m,tGF.m,tS.m), upper=c(st.M,d.M,ceN.M,ceS.M,tN.M,sN.M,tGF.M,tS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(1,10,10,10,3,3,3,3),maxit=200000),params=params)
		  } else {
		    if (x['policy.name']=="Nordhaus+R&DGreenClimateFund")
		     {
			  optimx(c(st.0,d.0,ceN.0,ceS.0,tN.0,sN.0,tGF.0,tS.0,sS.0,sGF.0), TechChangeMod, lower=c(st.m,d.m,ceN.m,ceS.m,tN.m,sN.m,tGF.m,tS.m,sS.m,sGF.m), upper=c(st.M,d.M,ceN.M,ceS.M,tN.M,sN.M,tGF.M,tS.M,sS.M,sGF.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(1,10,10,10,3,3,3,3,3,3),maxit=200000),params=params)
			 } else {
			  if (x['policy.name']=="Nordhaus+TechnologyPolicy.Both")
		      {
			   optimx(c(st.0,d.0,ceN.0,ceS.0,tN.0,sN.0,tS.0,sS.0), TechChangeMod, lower=c(st.m,d.m,ceN.m,ceS.m,tN.m,sN.m,tS.m,sS.m), upper=c(st.M,d.M,ceN.M,ceS.M,tN.M,sN.M,tS.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(1,10,10,10,3,3,3,3),maxit=200000),params=params)
			  } else {
			     if (x['policy.name']=="Nordhaus+TraditionalGreenClimateFund+R&DS")
		        {
			      optimx(c(st.0,d.0,ceN.0,ceS.0,tN.0,sN.0,tGF.0,tS.0,sS.0), TechChangeMod, lower=c(st.m,d.m,ceN.m,ceS.m,tN.m,sN.m,tGF.m,tS.m,sS.m), upper=c(st.M,d.M,ceN.M,ceS.M,tN.M,sN.M,tGF.M,tS.M,sS.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(1,10,10,10,3,3,3,3,3),maxit=200000),params=params)
				} else { 
			      if (x['policy.name']=="Nordhaus+CoR&DGreenClimateFund")
		           {
			         optimx(c(st.0,d.0,ceN.0,ceS.0,tN.0,sN.0,sS.0,sGF.0), TechChangeMod, lower=c(st.m,d.m,ceN.m,ceS.m,tN.m,sN.m,sS.m,sGF.m), upper=c(st.M,d.M,ceN.M,ceS.M,tN.M,sN.M,sS.M,sGF.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(1,10,10,10,3,3,3,3),maxit=200000),params=params)
				   } else {
				     if (x['policy.name']=="Nordhaus+CoR&DGreenClimateFund+TecS")
		              {
			            optimx(c(st.0,d.0,ceN.0,ceS.0,tN.0,sN.0,tS.0,sS.0,sGF.0), TechChangeMod, lower=c(st.m,d.m,ceN.m,ceS.m,tN.m,sN.m,tS.m,sS.m,sGF.m), upper=c(st.M,d.M,ceN.M,ceS.M,tN.M,sN.M,tS.M,sS.M,sGF.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(1,10,10,10,3,3,3,3,3),maxit=200000),params=params)
				      } else { "NA" }}}}}}}}}
	})
   #Stop cluster
   stopCluster(cl)

 