
 root<-"E:\\Projects\\EaSM\\TechChange-RDM\\"
 Number.Cores<-12
 
## ==================================================================================================================================================
## This section run the model across the experimental design and prints an output file for each run 
## ==================================================================================================================================================
#Source Model
  dir.model<-paste(root,"TechChange Model\\",sep="")
  model.version<-"InternationalGreenTechChangeModel_7_12_2015.r"
  source(paste(dir.model,model.version,sep=""))
#Source Experimental Design
  dir.exp<-paste(root,"RDM Inputs\\",sep="")
  experiment.version<-"Exp.design_climate_tests.csv"
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
 if (x['policy.name']=="FWA")
 {
 TechChangeMod(c(0.0,0.0,0.0,0.0),params)
 } else{
       if (x['policy.name']=="Nordhaus")
        {
          optimx(c(0.033,0.2,0.25,0.25), TechChangeMod, lower=c(0.03,0.1,0.01,0.01), upper=c(0.5,1.0,0.5,0.5),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01),parscale=c(0.1,0.1,10,10),maxit=200000),params=params)
         } else {
		 if (x['policy.name']=="Nordhauds+TechnologyPolicy")
		 {
		  optimx(c(0.033,0.2,0.25,0.25,0.05,1.0), TechChangeMod, lower=c(0.03,0.1,0.01,0.01,0.01,0.5), upper=c(0.5,1.0,0.5,0.5,0.1,2),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(0.1,0.1,10,10,0.1,0.1),maxit=200000),params=params)
		 } else {
		   if (x['policy.name']=="Nordhaus+TraditionalGreenClimateFund")
		  {
		   	optimx(c(0.033,0.2,0.25,0.25,0.05,1.0,0.05,0.05), TechChangeMod, lower=c(0.03,0.1,0.01,0.01,0.01,0.5,0.01,0.01), upper=c(0.5,1.0,0.5,0.5,0.1,2,0.05,0.05),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(0.1,0.1,10,10,0.1,0.1,0.1,0.1),maxit=200000),params=params)
		  } else {
		    if (x['policy.name']=="Nordhaus+R&DGreenClimateFund")
		     {
			  optimx(c(0.033,0.2,0.25,0.25,0.05,1.0,0.05,0.05,0.5,0.5), TechChangeMod, lower=c(0.03,0.1,0.01,0.01,0.01,0.5,0.01,0.01,0.01,0.01), upper=c(0.5,1.0,0.5,0.5,0.1,2,0.05,0.05,0.5,0.5),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(0.1,0.1,10,10,0.1,0.1,0.1,0.1,0.1,0.1),maxit=200000),params=params)
			 } else {
			   optimx(c(0.033,0.2,0.25,0.25,1.0,1.0), TechChangeMod, lower=c(0.03,0.1,0.01,0.01,0.5,0.5), upper=c(0.5,1.0,0.5,0.5,2,2),method="L-BFGS-B",control = list(fnscale = -1,maxit=200000),params=params)
		            }}}}}
	})
   #Stop cluster
   stopCluster(cl)

