#pc
# root<-"C:\\Users\\emolina\\Edmundo-RAND\\Projects\\Dissertation\\Model Development\\TechChange-RDM\\"
# Number.Cores<-4
#cloud
 #root<-"C:\\Users\\Administrator\\Documents\\Edmundo\\TechChange-RDM\\"
 #Number.Cores<-30
#zed
 #root<-"F:\\E Restore\\E\\emolina\\Project Work\\Dissertation\\TechChange-RDM\\"
 #Number.Cores<-10
#chopper
 root<-"E:\\Projects\\EaSM\\TechChange-RDM\\"
 Number.Cores<-10
## =================================================================================================================================================
## This section creates the Experimental Design Based on Input Tables
## =================================================================================================================================================
  dir.exp.inputs<-paste(root,"RDM Inputs\\",sep="")
  Limits.File<-"Limits.csv"
  Policies.File<-"Policies.csv"
  Climate.File<-"Climate.csv"
  sample.size<-1000
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
  model.version<-"InternationalGreenTechChangeModel_5_9_2015.r"
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
  nCore<-Number.Cores
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("Exp.design","TechChangeMod","dir.harness","dede","lagderiv") # dede, lagderiv are functions od deSolve
  clusterExport(cl,global.elements,envir=environment()) 
 #Execute code 
  parApply(cl,Exp.design,1,function(x) {
           TechChangeMod(as.numeric(x['S.0']), 
                         as.numeric(x['TimeStep']), 
                         as.numeric(x['EndTime']), 
                         as.numeric(x['alfa']), 
                         as.numeric(x['epsilon']),
                         as.numeric(x['Gamma.re']),
                         as.numeric(x['k.re']), 
                         as.numeric(x['Gamma.ce']), 
                         as.numeric(x['k.ce']), 
                         as.numeric(x['Eta.re']), 
                         as.numeric(x['Eta.ce']), 
                         as.numeric(x['Nu.re']), 
                         as.numeric(x['Nu.ce']), 
                         as.numeric(x['qsi']), 
                         as.numeric(x['Delta.S']),
						 as.numeric(x['Delta.Temp.Disaster']),
						 as.numeric(x['Beta.Delta.Temp']),
						 as.numeric(x['CO2.base']),
						 as.numeric(x['CO2.Disaster']),
                         as.numeric(x['labor.growth_N']),
						 as.numeric(x['labor.growth_S']),
                         as.numeric(x['lambda.S']),
						 as.numeric(x['sigma.utility']),
						 as.numeric(x['rho']),						 
                         as.numeric(x['tax.rate_N']), 
		                 as.numeric(x['epsi.re.subsidy_N']),
						 as.numeric(x['epsi.re.GFsubsidy_N']),
						 as.numeric(x['s.re.subsidy_N']),
                         as.numeric(x['s.re.GFsubsidy_N']),
						 as.numeric(x['policy.start.time_N']), 
                         as.numeric(x['policy.duration_N']), 
                         as.numeric(x['tax.rate_S']),
                         as.numeric(x['epsi.re.subsidy_S']), 
                         as.numeric(x['s.re.subsidy_S']),  
                         as.numeric(x['policy.start.time_S']), 
                         as.numeric(x['policy.duration_S']), 
                         as.numeric(x['Yre.0_N']),
                         as.numeric(x['Yce.0_N']), 
                         as.numeric(x['Yre.0_S']), 
                         as.numeric(x['Yce.0_S']), 
						 as.numeric(x['size.factor']),
						 as.numeric(x['Run.ID']),
						 dir.harness)})
   #Stop cluster
   stopCluster(cl)
 
## =====================================================================================================
## This section reads the output of simulations and reshapes into a format ready for scenario discovery
## =====================================================================================================
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
  prim.data.fwa<-subset(prim.data,prim.data$policy.name=="FWA") 
  prim.data.fwa<-prim.data.fwa[,c("Future.ID","Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S"),with=FALSE]
  setnames( prim.data.fwa,c("Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S"),c("Y.Total_N.fwa","Y.Total_S.fwa","Consumption.Total_N.fwa","Consumption.Total_S.fwa"))
  prim.data<-merge(prim.data,prim.data.fwa,by="Future.ID")
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
  library(data.table)
  library(snow)
  nCore<-Number.Cores
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("dir.inputs","experiment.version","dir.harness","process.harness.data")
  clusterExport(cl,global.elements,envir=environment()) 
  modelruns <- parLapply(cl,filenames, function(x){process.harness.data(x,dir.inputs,experiment.version,dir.harness)} )
  stopCluster(cl)
  modelruns<-rbindlist(modelruns) 
#print time series for model
  write.csv(modelruns, paste(dir.output, "model.runs_4_28_2015.csv", sep=""), row.names=FALSE)




 