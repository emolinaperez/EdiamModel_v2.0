## =======================================================================
## The International Diffusion of Climate Change Mitigation Technologies
## =======================================================================
TechChangeMod<-function (policies,params){
#Simulation length, time step and Run.ID
 Run.ID <- as.numeric(params['Run.ID'])
 EndTime <- as.numeric(params['EndTime'])
 TimeStep <- as.numeric(params['TimeStep'])
#Determine policy parameters
 policy.vector<-c(
    #Start time of policies
	policy.start.time = as.numeric(policies[1]),
	#Duration of policies
	policy.duration=as.numeric(policies[2]),
    #carbon tax
	 tax.rate.N=as.numeric(policies[3]),
     tax.rate.S=as.numeric(policies[4]),
    #Technology push in North
	 epsi.re.subsidy.N = ifelse(as.character(params['policy.name'])%in%c("Nordhauds+TechnologyPolicy","Nordhaus+TraditionalGreenClimateFund","Nordhaus+R&DGreenClimateFund")==TRUE,as.numeric(policies[5]),0),
     s.re.subsidy.N = ifelse(as.character(params['policy.name'])%in%c("Nordhauds+TechnologyPolicy","Nordhaus+TraditionalGreenClimateFund","Nordhaus+R&DGreenClimateFund")==TRUE,as.numeric(policies[6]),0),
    #Traditional Green Climate Fund
	 epsi.re.GFsubsidy.N = ifelse(as.character(params['policy.name'])%in%c("Nordhaus+TraditionalGreenClimateFund","Nordhaus+R&DGreenClimateFund")==TRUE,as.numeric(policies[7]),0),
     epsi.re.subsidy.S = ifelse(as.character(params['policy.name'])%in%c("Nordhaus+TraditionalGreenClimateFund","Nordhaus+R&DGreenClimateFund")==TRUE,as.numeric(policies[8]),0),
    # R&D Green Climate Fund
	 s.re.subsidy.S = ifelse(as.character(params['policy.name'])%in%c("Nordhaus+R&DGreenClimateFund")==TRUE,as.numeric(policies[9]),0),
     s.re.GFsubsidy.N = ifelse(as.character(params['policy.name'])%in%c("Nordhaus+R&DGreenClimateFund")==TRUE,as.numeric(policies[10]),0))
	
#Load parameters required for determining initial conditions 
 alfa <- as.numeric(params['alfa'])
 epsilon <- as.numeric(params['epsilon'])
 size.factor<- as.numeric(params['size.factor'])
 Yre.N.0<-as.numeric(params['Yre.0_N']) 
 Yce.N.0<-as.numeric(params['Yce.0_N']) 
 Yre.S.0<-as.numeric(params['Yre.0_S']) 
 Yce.S.0<-as.numeric(params['Yce.0_S'])
 S.0<-as.numeric(params['S.0'])
 

#Initial Productivity conditions are determined by the initial levels of production of energy
#In the Northern Region
  Ace.N.0<-((Yce.N.0^((epsilon-1)/epsilon)+Yre.N.0^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Yce.N.0/Yre.N.0)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
  Are.N.0<-((Yce.N.0^((epsilon-1)/epsilon)+Yre.N.0^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Yre.N.0/Yce.N.0)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
 
#In the Southern Region
  Ace.S.0<-(1/size.factor)*((Yce.S.0^((epsilon-1)/epsilon)+Yre.S.0^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Yce.S.0/Yre.S.0)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
  Are.S.0<-(1/size.factor)*((Yce.S.0^((epsilon-1)/epsilon)+Yre.S.0^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Yre.S.0/Yce.S.0)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))

InitialConditions <- c(Are.N = Are.N.0, 
                       Ace.N = Ace.N.0, 
					   Are.S = Are.S.0, 
					   Ace.S = Ace.S.0, 
					   S = S.0)

#Put all parameters together		   
Parameters <- c(alfa = as.numeric(params['alfa']), 
                epsilon = as.numeric(params['epsilon']),
                size.factor= as.numeric(params['size.factor']),				
                Gamma.re = as.numeric(params['Gamma.re']), 
                k.re = as.numeric(params['k.re']), 
                Gamma.ce = as.numeric(params['Gamma.ce']), 
                k.ce = as.numeric(params['k.ce']), 
                Eta.re= as.numeric(params['Eta.re']), 
                Eta.ce= as.numeric(params['Eta.ce']), 
                Nu.re = as.numeric(params['Nu.re']), 
                Nu.ce= as.numeric(params['Nu.ce']),
                qsi=as.numeric(params['qsi']),
                Delta.S = as.numeric(params['Delta.S']),
				Delta.Temp.Disaster = as.numeric(params['Delta.Temp.Disaster']),
				Beta.Delta.Temp = as.numeric(params['Beta.Delta.Temp']),
				CO2.base = as.numeric(params['CO2.base']),
				CO2.Disaster = as.numeric(params['CO2.Disaster']),
				labor.growth.N = as.numeric(params['labor.growth_N']),
				labor.growth.S = as.numeric(params['labor.growth_S']),
				rho = as.numeric(params['rho']),
				lambda.S = as.numeric(params['lambda.S']),
				sigma.utility = as.numeric(params['sigma.utility']),
                policy.vector)

ModelEngine <- function(Time, State, Parameters) {
  with(as.list(c(State, Parameters)), {

  #Policy Instruments
     #status of policy objectives
	  dS.lag<-ifelse(Time-1<=0,0,lagderiv(Time-1,5))
	  policy.status<-ifelse(Time>=policy.start.time*EndTime,ifelse(Time<=(policy.start.time+policy.duration)*EndTime,1,0),0)
	  #policy.status<-ifelse(dS.lag>0.5,0,1)
	 #policies in the North
	  ce.tax.N<-ifelse(policy.status==1,tax.rate.N,0.0)
      RD.subsidy.N<-ifelse(policy.status==1,s.re.subsidy.N,0.0)
	  RD.subsidy.GF.N<-ifelse(policy.status==1,s.re.GFsubsidy.N,0.0)
	  Tec.subsidy.N<-ifelse(policy.status==1,epsi.re.subsidy.N,0.0)
	  Tec.subsidy.GF.N<-ifelse(policy.status==1,epsi.re.GFsubsidy.N,0.0)
	  
	 #policies in the South 
	  ce.tax.S<-ifelse(policy.status==1,tax.rate.S,0.0)
      RD.subsidy.S<-ifelse(policy.status==1,s.re.subsidy.S,0.0)+RD.subsidy.GF.N
	  Tec.subsidy.S<-ifelse(policy.status==1,epsi.re.subsidy.S,0.0)+Tec.subsidy.GF.N

  #Economic structure
	#Auxiliaries for both regions
     Phi<-(1-alfa)*(1-epsilon)
	 epsi.re<-alfa^2 #this is the cost of production of clean technologies
     epsi.ce<-alfa^2 #this is the cost of production of dirty technologies
	 
	#North Region
	#Auxiliaries in North
     L.N<-exp(labor.growth.N*Time)
	 Gamma.re.t.N<-Gamma.re*exp(-k.re*(Are.N/Are.N.0-1)) #gamma displays decreasing returns as in Stiligtz 
     Gamma.ce.t.N<-Gamma.ce*exp(-k.ce*(Ace.N/Ace.N.0-1)) #gamma displays decreasing returns as in Stiligtz 
	 
    #First we determine the equilibrium levels of relative input prices and relative labor
	 RelPrice.N<-((Ace.N/Are.N)^(1-alfa))*(((epsi.re*(1-Tec.subsidy.N))/epsi.ce)^alfa)
     RelLabor.N<-((1+ce.tax.N)^epsilon)*((((1-Tec.subsidy.N)*epsi.re)/epsi.ce)^(alfa*(1-epsilon)))*((Are.N/Ace.N)^(-1*Phi))

    #Second we determine the equilibrium conditions for each sector
     #clean sector
       Labor.re.N<-(RelLabor.N*L.N)/(1+RelLabor.N) #based on the assumption that Labor.re.N+Labor.ce.N=L.N
	   #Price.re.N<-RelPrice.N/(RelPrice.N^(1-epsilon)+(1+ce.tax.N)^(1-epsilon))^(1/(1-epsilon)) #based on the assumption that  Price.re.N^(1-epsilon)+(Price.ce.N*(1+ce.tax))^(1-epsilon)=1
	   Price.re.N<-RelPrice.N/(RelPrice.N^(1-epsilon)+(1+0)^(1-epsilon))^(1/(1-epsilon)) #based on the assumption that  Price.re.N^(1-epsilon)+Price.ce.N^(1-epsilon)=1
       Agg.demand.re.tech.N<-((((alfa^2)*Price.re.N)/((1-Tec.subsidy.N)*epsi.re))^(1/(1-alfa)))*Labor.re.N*Are.N
       Profits.re.N<-(1+RD.subsidy.N)*Eta.re*epsi.re*((1-alfa)/alfa)*Agg.demand.re.tech.N
       #Profits.re.N<-Eta.re*(1+RD.subsidy.N)*((epsi.re/alfa)-epsi.re)*Agg.demand.re.tech.N
	   Yre.N<-((((alfa^2)*Price.re.N)/((1-Tec.subsidy.N)*epsi.re))^(alfa/(1-alfa)))*Labor.re.N*Are.N

     #dirty sector
       Labor.ce.N<-L.N/(RelLabor.N+1)
       Price.ce.N<-Price.re.N/RelPrice.N
       Agg.demand.ce.tech.N<-((((alfa^2)*Price.ce.N)/(epsi.ce))^(1/(1-alfa)))*Labor.ce.N*Ace.N
       Profits.ce.N<-Eta.ce*epsi.ce*((1-alfa)/alfa)*Agg.demand.ce.tech.N
	   #Profits.ce.N<-Eta.ce*((epsi.ce/alfa)-epsi.ce)*Agg.demand.ce.tech.N
	   
       Yce.N<-((((alfa^2)*Price.ce.N)/(epsi.ce))^(alfa/(1-alfa)))*Labor.ce.N*Ace.N

     #Total Production
      Y.N<-((Yre.N)^((epsilon-1)/epsilon)+(Yce.N)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))

     #Allocation of Scientists
      sre.N<-exp(Profits.re.N)/(exp(Profits.ce.N)+exp(Profits.re.N))
	  #sre.N<-ifelse(Profits.re.N>Profits.ce.N,1,0)
      sce.N<-1-sre.N

    #South Region
	#Auxiliaries in South
	 L.S<-(exp(labor.growth.S*Time))*size.factor #the population of the South is 4.6 that of the North,   
   	 Gamma.re.t.S<-Gamma.re 
     Gamma.ce.t.S<-Gamma.ce 
	 
    #First we determine the equilibrium levels of relative input prices and relative labour
     RelPrice.S<-((Ace.S/Are.S)^(1-alfa))*(((epsi.re*(1-Tec.subsidy.S))/epsi.ce)^alfa)
	 #((80.29897443/250.3765696)^(1-alfa))*(((epsi.re*(1-Tec.subsidy.S))/epsi.ce)^alfa)
     RelLabor.S<-((1+ce.tax.S)^epsilon)*((((1-Tec.subsidy.S)*epsi.re)/epsi.ce)^(alfa*(1-epsilon)))*((Are.S/Ace.S)^(-1*Phi))

    #Second we determine the equilibrium conditions for each sector
     #clean sector
       Labor.re.S<-(L.S*RelLabor.S)/(RelLabor.S+1) #based on the assumption that Labor.re.S+Labor.ce.S=L.S
       #Price.re.S<-RelPrice.S/(RelPrice.S^(1-epsilon)+(1+ce.tax.S)^(1-epsilon))^(1/(1-epsilon)) #based on the assumption that  Price.re.S^(1-epsilon)+(Price.ce.S*(1+ce.tax))^(1-epsilon)=1
	   Price.re.S<-RelPrice.S/(RelPrice.S^(1-epsilon)+(1+0)^(1-epsilon))^(1/(1-epsilon)) #based on the assumption that  Price.re.S^(1-epsilon)+(Price.ce.S*(1+ce.tax))^(1-epsilon)=1
       Agg.demand.re.tech.S<-((((alfa^2)*Price.re.S)/((1-Tec.subsidy.S)*epsi.re))^(1/(1-alfa)))*Labor.re.S*Are.S
       Profits.re.S<-(1+RD.subsidy.S)*Eta.re*epsi.re*((1-alfa)/alfa)*Agg.demand.re.tech.S
	   #Profits.re.S<-(1+RD.subsidy.S)*Eta.re*((epsi.re/alfa)-epsi.re)*Agg.demand.re.tech.S
       Yre.S<-((((alfa^2)*Price.re.S)/((1-Tec.subsidy.S)*epsi.re))^(alfa/(1-alfa)))*Labor.re.S*Are.S

     #dirty sector
       Labor.ce.S<-L.S/(RelLabor.S+1)
       Price.ce.S<-Price.re.S/RelPrice.S
       Agg.demand.ce.tech.S<-((((alfa^2)*Price.ce.S)/(epsi.ce))^(1/(1-alfa)))*Labor.ce.S*Ace.S
       Profits.ce.S<-Eta.ce*epsi.ce*((1-alfa)/alfa)*Agg.demand.ce.tech.S
	   #Profits.ce.S<-Eta.ce*((epsi.ce/alfa)-epsi.ce)*Agg.demand.ce.tech.S
       Yce.S<-((((alfa^2)*Price.ce.S)/(epsi.ce))^(alfa/(1-alfa)))*Labor.ce.S*Ace.S
    
     #Total Production
      Y.S<-((Yre.S)^((epsilon-1)/epsilon)+(Yce.S)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))

     #Allocation of Scientists
      sre.S<-exp(Profits.re.S)/(exp(Profits.ce.S)+exp(Profits.re.S))
	  #exp(702.303580765841)/(exp(0.0180533887937141)+exp(702.303580765841))
	  #sre.S<-ifelse(Profits.re.S>Profits.ce.S,1,0)
      sce.S<-1-sre.S
    
    
   #Changes in Temperature
      Delta.Temp.Disaster<-Delta.Temp.Disaster #increase in temperature at which there is environmental disaster
      CO2.Concentration<-max(CO2.Disaster-S,CO2.base)
      Delta.Temp<-min(Beta.Delta.Temp*log(CO2.Concentration/CO2.base),Delta.Temp.Disaster)
	  
   #Welfare Calculations
     Consumption.N<-Y.N-
	                epsi.re*Agg.demand.re.tech.N-epsi.ce*Agg.demand.ce.tech.N 
					#epsi.re*(1+Tec.subsidy.N)*Agg.demand.re.tech.N - #including costs of technology subsidies
					#epsi.ce*Agg.demand.ce.tech.N - 
					#Tec.subsidy.GF.N*Agg.demand.re.tech.S - #green climate fund costs of technology subsidies
					#RD.subsidy.N*((epsi.re/alfa)-epsi.re)*Agg.demand.re.tech.N #costs of research subsidies 
     Consumption.S<-(Y.S -
	                epsi.re*Agg.demand.re.tech.S-epsi.ce*Agg.demand.ce.tech.S
					#epsi.re*(1+Tec.subsidy.S)*Agg.demand.re.tech.S - #including costs of technology subsidies
					#epsi.ce*Agg.demand.ce.tech.S-
					#RD.subsidy.S*((epsi.re/alfa)-epsi.re)*Agg.demand.re.tech.S #costs of research subsidies
					)*(1/size.factor) 
     lambda.S<-lambda.S 
     Cost.S.Damage<-((Delta.Temp.Disaster-Delta.Temp)^lambda.S-lambda.S*Delta.Temp.Disaster^(lambda.S-1)*(Delta.Temp.Disaster-Delta.Temp))/((1-lambda.S)*Delta.Temp.Disaster^lambda.S)
     sigma.utility<-sigma.utility
     Utility.Consumer.N<-ifelse(Cost.S.Damage==0,-100,(1+((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho*5)^Time)))
	 Utility.Consumer.S<-ifelse(Cost.S.Damage==0,-100,(1+((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho*5)^Time)))
	 #Utility.Consumer.N<-ifelse(Cost.S.Damage==0,0,(1+((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^Time)))
	 #Utility.Consumer.S<-ifelse(Cost.S.Damage==0,0,(1+((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^Time)))


   #State variables
    
    #Evolution of Productivity North Region
     dAre.N<-Gamma.re.t.N*Eta.re*sre.N*Are.N
     dAce.N<-Gamma.ce.t.N*Eta.ce*sce.N*Ace.N
	 
    #Evolution of Productivity South Region
     dAre.S<-Gamma.re.t.S*Nu.re*sre.S*(Are.N-Are.S)
     dAce.S<-Gamma.ce.t.S*Nu.ce*sce.S*(Ace.N-Ace.S)
	 
	 #dAre.S<-Gamma.re.t.S*Nu.re*sre.S*(Are.S)
     #dAce.S<-Gamma.ce.t.S*Nu.ce*sce.S*(Ace.S)

    #Environmental Quality
     dS<-Delta.S*S-qsi*(Yce.N+Yce.S)
	
    #Define variables to output
    vars.out<-list(c(dAre.N,dAce.N,dAre.S,dAce.S,dS),
                 RelPrice_N = RelPrice.N,
				 RelLabor_N = RelLabor.N,
				 Labor.re_N = Labor.re.N,
				 Price.re_N = Price.re.N,
				 Agg.demand.re.tech_N = Agg.demand.re.tech.N,
				 Profits.re_N = Profits.re.N,
				 Yre_N = Yre.N,
				 sre_N = sre.N,
				 Labor.ce_N = Labor.ce.N,
				 Price.ce_N = Price.ce.N,
				 Agg.demand.ce.tech_N = Agg.demand.ce.tech.N,
				 Profits.ce_N = Profits.ce.N,
				 Yce_N = Yce.N,
				 sce_N = sce.N,
				 Y_N = Y.N,
                 RelPrice_S = RelPrice.S,
				 RelLabor_S = RelLabor.S,
				 Labor.re_S = Labor.re.S,
				 Price.re_S = Price.re.S,
				 Agg.demand.re.tech_S = Agg.demand.re.tech.S,
				 Profits.re_S = Profits.re.S,
				 Yre_S = Yre.S,
				 sre_S = sre.S,
				 Labor.ce_S = Labor.ce.S,
				 Price.ce_S = Price.ce.S,
				 Agg.demand.ce.tech_S = Agg.demand.ce.tech.S,
				 Profits.ce_S = Profits.ce.S,
				 Yce_S = Yce.S,
				 sce_S = sce.S,
				 Y_S = Y.S,
                 Delta.Temp = Delta.Temp,
				 Gamma.re.t_N = Gamma.re.t.N,
				 Gamma.re.t_S = Gamma.re.t.S,
				 Gamma.ce.t_N = Gamma.ce.t.N,
				 Gamma.ce.t_S = Gamma.ce.t.S,
				 L_N = L.N,
				 L_S = L.S,
				 Consumption_N = Consumption.N,
				 Consumption_S = Consumption.S,
				 Utility.Consumer_N = Utility.Consumer.N,
				 Utility.Consumer_S = Utility.Consumer.S,
				 CO2.Concentration = CO2.Concentration,
				 Cost.S.Damage=Cost.S.Damage,
				 policy.status = policy.status,
                 Policy.Duration = policy.duration*EndTime,				 
                 Policy.Start.Time = policy.start.time*EndTime,				 
	             ce.tax_N=ce.tax.N,
				 RD.subsidy_N=RD.subsidy.N,
				 RD.subsidy.GF_N=RD.subsidy.GF.N,
				 Tec.subsidy_N=Tec.subsidy.N,
				 Tec.subsidy.GF_N=Tec.subsidy.GF.N,
				 ce.tax_S=ce.tax.S,
				 RD.subsidy_S=RD.subsidy.S,
				 Tec.subsidy_S=Tec.subsidy.S,
				 dS.lag=dS.lag)	
    return(vars.out)
  })
}
#Model Solver
 #library(deSolve)
  out <- as.data.frame(dede(InitialConditions, seq(0, EndTime, by = TimeStep),ModelEngine, Parameters))
  #out <- as.data.frame(dede(InitialConditions, seq(0, EndTime, by = TimeStep),method="vode", ModelEngine, Parameters))
  #method = c("lsoda", "lsode", 
  #  "lsodes", "lsodar", "vode", "daspk", "bdf", "adams", "impAdams", 
  #  "radau")
  out$Run.ID<-Run.ID
  out$time<-out$time+2012
  #consum<-data.frame(t(sapply(out[,c("Y_N","Y_S","Consumption_N","Consumption_S","policy.status","Utility.Consumer_N","Utility.Consumer_S")],sum)))
  #colnames(consum)<-c("Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S","Policy.Duration","Utility.Consumer.Total_N","Utility.Consumer.Total_S")
  #out<-merge(out,consum)
  write.csv(out, paste(params['dir.harness'], "output_run_",Run.ID,".csv", sep=""), row.names=FALSE)
  #1*(as.numeric(unique(out[,"Utility.Consumer.Total_N"]))+
  #  as.numeric(unique(out[,"Utility.Consumer.Total_S"])))
  1*(sum(as.numeric(out$Utility.Consumer_N))+sum(as.numeric(out$Utility.Consumer_S)))
}