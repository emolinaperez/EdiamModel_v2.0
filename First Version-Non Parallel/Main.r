#Create the experimental design
 dir.exp.inputs<-"C:\\Users\\emolina\\Edmundo-RAND\\Projects\\Dissertation\\Model Development\\TechChange-RDM\\RDM Inputs\\"
 Limits.File<-"Limits.csv"
 Policies.File<-"Policies.csv"
 Climate.File<-"Climate.csv"
 sample.size<-10
 Policy.Switch<-TRUE
 Climate.Switch<-FALSE
 source(paste(dir.exp.inputs,"create_experiment_function.r",sep=""))
 Exp.design<-exp.design.table(dir.exp.inputs,Limits.File,sample.size,Policies.File,Policy.Switch,Climate.File,Climate.Switch)
 write.csv(Exp.design, paste(dir.exp.inputs, "Exp.design.csv", sep=""), row.names=FALSE)

#Run the model
#Source Model
 dir.model<-"C:\\Users\\emolina\\Edmundo-RAND\\Projects\\Dissertation\\Model Development\\TechChange-RDM\\TechChange Model\\"
 model.version<-"InternationalGreenTechChangeModel_5_7_2015.r"
 source(paste(dir.model,model.version,sep=""))
#Source Experimental Design
 dir.exp<-"C:\\Users\\emolina\\Edmundo-RAND\\Projects\\Dissertation\\Model Development\\TechChange-RDM\\RDM Inputs\\"
 experiment.version<-"Exp.design.csv"
 Exp.design<-read.csv(paste(dir.exp,experiment.version,sep=""))
#Run Model 
pb=txtProgressBar(1,nrow(Exp.design),style=3)
 dir.harness<-"C:\\Users\\emolina\\Edmundo-RAND\\Projects\\Dissertation\\Model Development\\TechChange-RDM\\RDM Harness\\"
 modelruns<-
 
 apply(Exp.design, 1,function(x) { setTxtProgressBar(pb,as.numeric(x['Run.ID']));
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
                         as.numeric(x['tax.rate_N']), 
		                 as.numeric(x['epsi.re.subsidy_N']),
						 as.numeric(x['s.re.subsidy_N']),
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
	
 modelruns<-do.call("rbind", modelruns)

## =======================================================================
## The following section reshapes output of simulations
## =======================================================================

#merge output with experimental design
 library(data.table)
 modelruns<-data.table(modelruns)
 integers<-names(subset(sapply(Exp.design,is.integer),sapply(Exp.design,is.integer)==TRUE))
 Exp.design[,integers]<- lapply(Exp.design[,integers], as.numeric) 
 Exp.design<-data.table(Exp.design) 
 modelruns<-merge(modelruns,Exp.design,by="Run.ID")
#Reshape output and in  future calculate additional metrics 
 library(reshape2)
 modelruns[,TimeStep:=NULL]
 modelruns[,EndTime:=NULL]
#Adjust column names
 setnames(modelruns,c("Are.N","Ace.N","Are.S","Ace.S"),c("Are_N","Ace_N","Are_S","Ace_S"))
 
#Create levels for regions
 id.vars<-c("Run.ID","Future.ID","time",
            "S","Delta.Temp","CO2.Concentration",
            "alfa","epsilon","size.factor","Eta.re","Eta.ce","Gamma.re","Gamma.ce","Nu.re","Nu.ce","k.re","k.ce",
			"lambda.S","sigma.utility",
			"Climate.Model","Beta.Delta.Temp","CO2.base","CO2.Disaster","Delta.Temp.Disaster","qsi","Delta.S","S.0",
			"policy.name")
 measure.vars<-subset(colnames(modelruns),!(colnames(modelruns)%in%id.vars))
#reshape to generate region
 modelruns<-melt(modelruns, id.vars=id.vars, measure.vars=measure.vars,  variable.name="Variable.Region")
 modelruns<-cbind(modelruns,colsplit(modelruns$Variable.Region,("_"),c("Variable","Region")))
 modelruns[,Variable.Region:=NULL]
#reshape variables to wide
 modelruns <- data.table(dcast(modelruns, Run.ID+
						  Future.ID+
						  time+
						  S+ 
						  Delta.Temp+
						  CO2.Concentration+
						  alfa+
						  epsilon+
						  Eta.re+
						  Eta.ce+
						  Gamma.re+
						  Gamma.ce+
						  Nu.re+ 
						  Nu.ce+
						  k.re+
						  k.ce+
						  lambda.S+
						  sigma.utility+
						  Climate.Model+
						  Beta.Delta.Temp+
						  CO2.base+
						  CO2.Disaster+
						  Delta.Temp.Disaster+
						  qsi+
						  Delta.S+
						  S.0+
						  policy.name+
						  Region ~ Variable, value.var="value"))
 
 dir.output<-"C:\\Users\\emolina\\Edmundo-RAND\\Projects\\Dissertation\\Model Development\\TechChange-RDM\\RDM Outputs\\"
 write.csv(modelruns, paste(dir.output, "model.runs_4_28_2015.csv", sep=""), row.names=FALSE)

 