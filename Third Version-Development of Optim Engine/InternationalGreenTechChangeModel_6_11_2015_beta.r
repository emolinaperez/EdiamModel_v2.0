## =======================================================================
## The International Diffusion of Climate Change Mitigation Technologies
## =======================================================================
TechChangeMod<-function (policies,params){
policy.duration.N<-as.numeric(policies[1])
tax.rate.N<-as.numeric(policies[2])
tax.rate.S<-as.numeric(policies[3])

                         S.0<-as.numeric(params[1]) 
                         TimeStep<-as.numeric(params[2]) 
                         EndTime<-as.numeric(params[3]) 
                         alfa<-as.numeric(params[4])
                         epsilon<-as.numeric(params[5])
                         Gamma.re<-as.numeric(params[6])
                         k.re<-as.numeric(params[7]) 
                         Gamma.ce<-as.numeric(params[8]) 
                         k.ce<-as.numeric(params[9]) 
                         Eta.re<-as.numeric(params[10]) 
                         Eta.ce<-as.numeric(params[11]) 
                         Nu.re<-as.numeric(params[12]) 
                         Nu.ce<-as.numeric(params[13]) 
                         qsi<-as.numeric(params[14]) 
                         Delta.S<-as.numeric(params[15])
						 Delta.Temp.Disaster<-as.numeric(params[16])
						 Beta.Delta.Temp<-as.numeric(params[17])
						 CO2.base<-as.numeric(params[18])
						 CO2.Disaster<-as.numeric(params[19])
						 labor.growth.N<-as.numeric(params[20])
						 labor.growth.S<-as.numeric(params[21])
						 lambda.S<-as.numeric(params[22])
						 sigma.utility<-as.numeric(params[23])
						 rho<-as.numeric(params[24])
                         epsi.re.subsidy.N<-as.numeric(params[25])
						 epsi.re.GFsubsidy.N<-as.numeric(params[26])
						 s.re.subsidy.N<-as.numeric(params[27])
						 s.re.GFsubsidy.N<-as.numeric(params[28])
						 policy.start.time.N<-as.numeric(params[29])  
                         epsi.re.subsidy.S<-as.numeric(params[30])
						 s.re.subsidy.S<-as.numeric(params[31])
						 policy.start.time.S<-as.numeric(params[32])
                         policy.duration.S<-as.numeric(params[33])   		 
                         Yre.N.0<-as.numeric(params[34]) 
                         Yce.N.0<-as.numeric(params[35]) 
                         Yre.S.0<-as.numeric(params[36]) 
                         Yce.S.0<-as.numeric(params[37])
                         size.factor<-as.numeric(params[38])						 
						 Run.ID<-as.numeric(params[39])
						 dir.harness<-params[40]
						
	
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

Parameters <- c(alfa = alfa, 
                epsilon = epsilon,
                size.factor= size.factor,				
                Gamma.re = Gamma.re, 
                k.re = k.re, 
                Gamma.ce = Gamma.ce, 
                k.ce = k.ce, 
                Eta.re= Eta.re, 
                Eta.ce= Eta.ce, 
                Nu.re = Nu.re, 
                Nu.ce= Nu.ce,
                qsi=qsi,
                Delta.S = Delta.S,
				Delta.Temp.Disaster = Delta.Temp.Disaster,
				Beta.Delta.Temp = Beta.Delta.Temp,
				CO2.base = CO2.base,
				CO2.Disaster = CO2.Disaster,
				labor.growth.N = labor.growth.N,
				labor.growth.S = labor.growth.S,
				rho = rho,
                tax.rate.N=tax.rate.N,
				epsi.re.subsidy.N = epsi.re.subsidy.N,
				epsi.re.GFsubsidy.N = epsi.re.GFsubsidy.N,
				s.re.subsidy.N = s.re.subsidy.N,
				s.re.GFsubsidy.N = s.re.GFsubsidy.N,
				policy.start.time.N = policy.start.time.N,
                policy.duration.N = policy.duration.N,
                tax.rate.S = tax.rate.S,
                epsi.re.subsidy.S = epsi.re.subsidy.S,
				s.re.subsidy.S = s.re.subsidy.S,
				policy.start.time.S = policy.start.time.S,
                policy.duration.S = policy.duration.S)

ModelEngine <- function(Time, State, Parameters) {
  with(as.list(c(State, Parameters)), {

  #Policy Instruments
     #status of policy objectives
	  dS.lag<-ifelse(Time-1<=0,0,lagderiv(Time-1,5))
	  #policy.status.N<-ifelse(Time>=policy.start.time.N,ifelse(dS.lag<0,1,0),0)
	  #policy.status.S<-ifelse(Time>=policy.start.time.S,ifelse(dS.lag<0,1,0),0)
	  policy.status.N<-ifelse(Time>=policy.start.time.N,ifelse(Time<=policy.start.time.N+policy.duration.N*EndTime,1,0),0)
	  policy.status.S<-ifelse(Time>=policy.start.time.S,ifelse(Time<=policy.start.time.N+policy.duration.N*EndTime,1,0),0)
	 #policies in the North
	  ce.tax.N<-ifelse(policy.status.N==1,tax.rate.N,0)
      RD.subsidy.N<-ifelse(policy.status.N==1,s.re.subsidy.N,0.0)
	  RD.subsidy.GF.N<-ifelse(policy.status.N==1,s.re.GFsubsidy.N,0.0)
	  Tec.subsidy.N<-ifelse(policy.status.N==1,epsi.re.subsidy.N,0)
	  Tec.subsidy.GF.N<-ifelse(policy.status.N==1,epsi.re.GFsubsidy.N,0)
	  
	 #policies in the South 
	  ce.tax.S<-ifelse(policy.status.S==1,tax.rate.S,0)
      RD.subsidy.S<-ifelse(policy.status.S==1,s.re.subsidy.S,0.0)+RD.subsidy.GF.N
	  Tec.subsidy.S<-ifelse(policy.status.S==1,epsi.re.subsidy.S,0.0)+Tec.subsidy.GF.N

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
	   Price.re.N<-RelPrice.N/(RelPrice.N^(1-epsilon)+(1+0)^(1-epsilon))^(1/(1-epsilon)) #based on the assumption that  Price.re.N^(1-epsilon)+Price.ce.N^(1-epsilon)=1
       Agg.demand.re.tech.N<-((((alfa^2)*Price.re.N)/((1-Tec.subsidy.N)*epsi.re))^(1/(1-alfa)))*Labor.re.N*Are.N
       Profits.re.N<-(1+RD.subsidy.N)*Eta.re*epsi.re*((1-alfa)/alfa)*Agg.demand.re.tech.N
       Yre.N<-((((alfa^2)*Price.re.N)/((1-Tec.subsidy.N)*epsi.re))^(alfa/(1-alfa)))*Labor.re.N*Are.N

     #dirty sector
       Labor.ce.N<-L.N/(RelLabor.N+1)
       Price.ce.N<-Price.re.N/RelPrice.N
       Agg.demand.ce.tech.N<-((((alfa^2)*Price.ce.N)/(epsi.ce))^(1/(1-alfa)))*Labor.ce.N*Ace.N
       Profits.ce.N<-Eta.ce*epsi.ce*((1-alfa)/alfa)*Agg.demand.ce.tech.N
       Yce.N<-((((alfa^2)*Price.ce.N)/(epsi.ce))^(alfa/(1-alfa)))*Labor.ce.N*Ace.N

     #Total Production
      Y.N<-((Yre.N)^((epsilon-1)/epsilon)+(Yce.N)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))

     #Allocation of Scientists
      sre.N<-exp(Profits.re.N)/(exp(Profits.ce.N)+exp(Profits.re.N))
      sce.N<-1-sre.N

    #South Region
	#Auxiliaries in South
	 L.S<-(exp(labor.growth.S*Time))*size.factor #the population of the South is 4.6 that of the North,   
   	 Gamma.re.t.S<-Gamma.re 
     Gamma.ce.t.S<-Gamma.ce 
	 
    #First we determine the equilibrium levels of relative input prices and relative labour
     RelPrice.S<-((Ace.S/Are.S)^(1-alfa))*(((epsi.re*(1-Tec.subsidy.S))/epsi.ce)^alfa)
     RelLabor.S<-((1+ce.tax.S)^epsilon)*((((1-Tec.subsidy.S)*epsi.re)/epsi.ce)^(alfa*(1-epsilon)))*((Are.S/Ace.S)^(-1*Phi))

    #Second we determine the equilibrium conditions for each sector
     #clean sector
       Labor.re.S<-(L.S*RelLabor.S)/(RelLabor.S+1) #based on the assumption that Labor.re.S+Labor.ce.S=L.S
       Price.re.S<-RelPrice.S/(RelPrice.S^(1-epsilon)+(1+0)^(1-epsilon))^(1/(1-epsilon)) #based on the assumption that  Price.re.S^(1-epsilon)+(Price.ce.S*(1+ce.tax))^(1-epsilon)=1
       Agg.demand.re.tech.S<-((((alfa^2)*Price.re.S)/((1-Tec.subsidy.S)*epsi.re))^(1/(1-alfa)))*Labor.re.S*Are.S
       Profits.re.S<-(1+RD.subsidy.S)*Eta.re*epsi.re*((1-alfa)/alfa)*Agg.demand.re.tech.S
       Yre.S<-((((alfa^2)*Price.re.S)/((1-Tec.subsidy.S)*epsi.re))^(alfa/(1-alfa)))*Labor.re.S*Are.S

     #dirty sector
       Labor.ce.S<-L.S/(RelLabor.S+1)
       Price.ce.S<-Price.re.S/RelPrice.S
       Agg.demand.ce.tech.S<-((((alfa^2)*Price.ce.S)/(epsi.ce))^(1/(1-alfa)))*Labor.ce.S*Ace.S
       Profits.ce.S<-Eta.ce*epsi.ce*((1-alfa)/alfa)*Agg.demand.ce.tech.S
       Yce.S<-((((alfa^2)*Price.ce.S)/(epsi.ce))^(alfa/(1-alfa)))*Labor.ce.S*Ace.S
    
     #Total Production
      Y.S<-((Yre.S)^((epsilon-1)/epsilon)+(Yce.S)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))

     #Allocation of Scientists
      sre.S<-exp(Profits.re.S)/(exp(Profits.ce.S)+exp(Profits.re.S))
      sce.S<-1-sre.S
    
    
   #Changes in Temperature
      Delta.Temp.Disaster<-Delta.Temp.Disaster #increase in temperature at which there is environmental disaster
      CO2.Concentration<-max(CO2.Disaster-S,CO2.base)
      Delta.Temp<-min(Beta.Delta.Temp*log(CO2.Concentration/CO2.base),Delta.Temp.Disaster)
	  
   #Welfare Calculations
     Consumption.N<-Y.N-epsi.re*(1+Tec.subsidy.N)*Agg.demand.re.tech.N-epsi.ce*Agg.demand.ce.tech.N - Tec.subsidy.GF.N*Agg.demand.re.tech.S #maybe we still have to account for research profits and the costs of research subsidies and technology subsidies
     Consumption.S<-(Y.S-epsi.re*(1+Tec.subsidy.S)*Agg.demand.re.tech.S-epsi.ce*Agg.demand.ce.tech.S)*(1/size.factor)
     lambda.S<-lambda.S
	 Delta.Temp.End.Century.Target<-Delta.Temp.Disaster-0.0
     #Delta.Temp.End.Century.Target<-Delta.Temp.Disaster-1.5	 
     Cost.S.Damage<-((Delta.Temp.End.Century.Target-Delta.Temp)^lambda.S-lambda.S*Delta.Temp.End.Century.Target^(lambda.S-1)*(Delta.Temp.End.Century.Target-Delta.Temp))/((1-lambda.S)*Delta.Temp.End.Century.Target^lambda.S)
	 #Cost.S.Damage<-((Delta.Temp.Disaster-Delta.Temp)^lambda.S-lambda.S*Delta.Temp.Disaster^(lambda.S-1)*(Delta.Temp.Disaster-Delta.Temp))/((1-lambda.S)*Delta.Temp.Disaster^lambda.S)
     sigma.utility<-sigma.utility
     Utility.Consumer.N<-ifelse(Cost.S.Damage==0,-10000,(((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))/((1+rho)^Time))
     Utility.Consumer.S<-ifelse(Cost.S.Damage==0,-10000,(((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))/((1+rho)^Time))
     
   #State variables
    
    #Evolution of Productivity North Region
     dAre.N<-Gamma.re.t.N*Eta.re*sre.N*Are.N
     dAce.N<-Gamma.ce.t.N*Eta.ce*sce.N*Ace.N
	 
    #Evolution of Productivity South Region
     dAre.S<-Gamma.re.t.S*Nu.re*sre.S*(Are.N-Are.S)
     dAce.S<-Gamma.ce.t.S*Nu.ce*sce.S*(Ace.N-Ace.S)

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
				 policy.status_N = policy.status.N,
				 policy.status_S = policy.status.S,
				 tax.rate_N=tax.rate.N,
				 tax.rate_S=tax.rate.S,
				 dS.lag=dS.lag)	
    return(vars.out)
  })
}
#Model Solver
 #library(deSolve)
 #out <- as.data.frame(ode(InitialConditions, seq(0, EndTime, by = TimeStep), ModelEngine, Parameters,method="rk4"))
 out <- as.data.frame(dede(InitialConditions, seq(0, EndTime, by = TimeStep), ModelEngine, Parameters))
 #Post-processing at individual run level
 out$Run.ID<-Run.ID
 out$time<-out$time+2012
 consum<-data.frame(t(sapply(out[,c("Y_N","Y_S","Consumption_N","Consumption_S","policy.status_N","policy.status_S","Utility.Consumer_N","Utility.Consumer_S")],sum)))
 colnames(consum)<-c("Y.Total_N","Y.Total_S","Consumption.Total_N","Consumption.Total_S","Policy.Status_N","Policy.Status_S","Utility.Consumer.Total_N","Utility.Consumer.Total_S")
 out<-merge(out,consum)
 #out<-out[out$time==2100,]
 #return(out)
 write.csv(out, paste(dir.harness, "output_run_",Run.ID,".csv", sep=""), row.names=FALSE)
 #1*((1/(size.factor+1))*as.numeric(unique(out[,"Utility.Consumer.Total_N"]))+
 #   (size.factor/(size.factor+1))*as.numeric(unique(out[,"Utility.Consumer.Total_S"])))
  1*(as.numeric(unique(out[,"Utility.Consumer.Total_N"]))+
    as.numeric(unique(out[,"Utility.Consumer.Total_S"])))
}
