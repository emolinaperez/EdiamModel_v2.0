#pc
 root<-"C:\\Users\\emolina\\Edmundo-RAND\\Projects\\Dissertation\\Model Development\\TechChange-RDM\\"
 #Number.Cores<-4
#cloud
 #root<-"C:\\Users\\Administrator\\Documents\\Edmundo\\TechChange-RDM\\"
 #Number.Cores<-30
#zed
 #root<-"F:\\E Restore\\E\\emolina\\Project Work\\Dissertation\\TechChange-RDM\\"
 #Number.Cores<-10
#chopper
 root<-"E:\\Projects\\EaSM\\TechChange-RDM\\"
 Number.Cores<-22
 
## =================================================================================================================================================
## This section creates the Experimental Design Based on Input Tables
## =================================================================================================================================================
  dir.exp.inputs<-paste(root,"RDM Inputs\\",sep="")
  Limits.File<-"Limits.csv"
  Policies.File<-"Policies.csv"
  Climate.File<-"Climate.csv"
  sample.size<-20
  Policy.Switch<-TRUE
  Climate.Switch<-TRUE
  source(paste(dir.exp.inputs,"create_experiment_function.r",sep=""))
  Exp.design<-exp.design.table(dir.exp.inputs,Limits.File,sample.size,Policies.File,Policy.Switch,Climate.File,Climate.Switch)
  #write.csv(Exp.design, paste(dir.exp.inputs, "Exp.design.csv", sep=""), row.names=FALSE)

## ==================================================================================================================================================
## This section run the model across the experimental design and prints an output file for each run 
## ==================================================================================================================================================
#Source Model
  dir.model<-paste(root,"TechChange Model\\",sep="")
  model.version<-"InternationalGreenTechChangeModel_7_12_2015.r"
  source(paste(dir.model,model.version,sep=""))
#Source Experimental Design
  dir.exp<-paste(root,"RDM Inputs\\",sep="")
  experiment.version<-"Exp.design.csv"
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
          #optimx(c(0.15,0.1,0.1), TechChangeMod, lower=c(0.1,0.05,0.05), upper=c(1,1,1),method="L-BFGS-B",control = list(fnscale = -1),params=params)
          #optimx(c(0.5,1.0,1.0), TechChangeMod, lower=c(0.1,0.05,0.05), upper=c(1,1,1),method="L-BFGS-B",control = list(fnscale = -1,parscale=c(1,0.5,0.5),maxit=200000),params=params)
          optimx(c(0.033,0.5,0.25,0.25), TechChangeMod, lower=c(0.03,0.01,0.01,0.01), upper=c(0.5,1.0,0.5,0.5),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01),maxit=200000),params=params)
          #optimx(c(0.15,0.1,0.1), TechChangeMod, lower=c(0.0,0.0,0.0), upper=c(1.0,1,1),method="L-BFGS-B",control = list(fnscale = -1,parscale=c(1,0.5,0.5)),params=params)
          #optimx(c(0.15,0.1,0.1), TechChangeMod, lower=c(0.0,0.0,0.0), upper=c(0.4,1,1),method="L-BFGS-B",control = list(fnscale = -1,factr=1e5),params=params)
         } else {
		 if (x['policy.name']=="Nordhauds+TechnologyPolicy")
		 {
		  optimx(c(0.033,0.5,0.25,0.25,0.05,1.0), TechChangeMod, lower=c(0.03,0.03,0.01,0.01,0.01,0.5), upper=c(0.5,1.0,0.5,0.5,0.1,2),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01),maxit=200000),params=params)
		 } else {
		   if (x['policy.name']=="Nordhaus+TraditionalGreenClimateFund")
		  {
		   	optimx(c(0.033,0.5,0.25,0.25,0.05,1.0,0.05,0.05), TechChangeMod, lower=c(0.03,0.03,0.01,0.01,0.01,0.5,0.01,0.01), upper=c(0.5,1.0,0.5,0.5,0.1,2,0.05,0.05),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),maxit=200000),params=params)
		  } else {
		    if (x['policy.name']=="Nordhaus+R&DGreenClimateFund")
		     {
			  optimx(c(0.033,0.5,0.25,0.25,0.05,1.0,0.05,0.05,0.5,0.5), TechChangeMod, lower=c(0.03,0.03,0.01,0.01,0.01,0.5,0.01,0.01,0.01,0.01), upper=c(0.5,1.0,0.5,0.5,0.1,2,0.05,0.05,0.5,0.5),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),maxit=200000),params=params)
			 } else {
			   optimx(c(0.033,0.5,0.25,0.25,1.0,1.0), TechChangeMod, lower=c(0.03,0.03,0.01,0.01,0.5,0.5), upper=c(0.5,1.0,0.5,0.5,2,2),method="L-BFGS-B",control = list(fnscale = -1,maxit=200000),params=params)
		            }}}}}
	})
   #Stop cluster
   stopCluster(cl)

   
## =====================================================================================================
## This section reads the output of simulations and reshapes into a format ready for scenario discovery
## =====================================================================================================
  Number.Cores<-2
 #Define directory parameters
  dir.inputs<-paste(root,"RDM Inputs\\",sep="")
  dir.harness<-paste(root,"RDM Harness\\",sep="")
  dir.output<-paste(root,"RDM Outputs\\",sep="")
 #load needed libraries
  library(reshape2,lib= paste(root,"Rlibraries\\",sep=""))
  library(data.table,lib=paste(root,"Rlibraries\\",sep=""))
  library(snow,lib=paste(root,"Rlibraries\\",sep=""))
#create vector with file names
  filenames <- list.files(dir.harness, pattern="*.csv", full.names=FALSE)
#source function to process harnessed output data
  source(paste(dir.inputs,"harness_processing.r",sep=""))
#run post-processing in parallel
  nCore<-Number.Cores
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("dir.harness","process.prim.data","data.table")
  clusterExport(cl,global.elements,envir=environment()) 
  prim.data <- parLapply(cl,filenames, function(x){data.table(process.prim.data(x,dir.harness))} )
  stopCluster(cl)
  prim.data<-rbindlist(prim.data)
#merge data with experimental design 
  experiment.version<-"Exp.design.csv"
  prim.data<-merge.exp.design(dir.inputs,experiment.version,prim.data)
#create future without action consumption
  #prim.data.fwa<-subset(prim.data,prim.data$policy.name=="FWA") 
  #prim.data.fwa<-prim.data.fwa[,c("Future.ID","Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S"),with=FALSE]
  #setnames( prim.data.fwa,c("Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S"),c("Y.Total_N.fwa","Y.Total_S.fwa","Consumption.Total_N.fwa","Consumption.Total_S.fwa"))
  #prim.data<-merge(prim.data,prim.data.fwa,by="Future.ID")
  write.csv(prim.data, paste(dir.output, "prim.data_7_06_2015.csv", sep=""), row.names=FALSE)

## =====================================================================================================
## This section reads the output of simulations and reshapes it into time series split by region, 
## =====================================================================================================
#Define directory parameters
 dir.inputs<-paste(root,"RDM Inputs\\",sep="")
 dir.harness<-paste(root,"RDM Harness\\",sep="") 
 dir.output<-paste(root,"RDM Outputs\\",sep="")
 
#crate vector with file names
 experiment.version<-"Exp.design.csv"
 filenames <- list.files(dir.harness, pattern="*.csv", full.names=FALSE)
#source function to process harnessed output data
 source(paste(dir.inputs,"harness_processing.r",sep=""))
#run post-processing in parallel
  library(data.table,lib=paste(root,"Rlibraries\\",sep=""))
  library(snow,lib=paste(root,"Rlibraries\\",sep=""))
  nCore<-Number.Cores
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("dir.inputs","experiment.version","dir.harness","process.harness.data")
  clusterExport(cl,global.elements,envir=environment()) 
  modelruns <- parLapply(cl,filenames, function(x){process.harness.data(x,dir.inputs,experiment.version,dir.harness)} )
  stopCluster(cl)
  modelruns<-rbindlist(modelruns) 
#print time series for model
  write.csv(modelruns, paste(dir.output, "model.runs_7_09_2015.csv", sep=""), row.names=FALSE)

#something is not rigth when we use other climates: 
  #MIROC  
  



 