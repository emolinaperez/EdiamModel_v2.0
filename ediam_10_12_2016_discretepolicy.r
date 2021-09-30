## =======================================================================================================================================================================================================
## The International Diffusion of Climate Change Mitigation Technologies
## =======================================================================================================================================================================================================
ediam<-function (policies){
#Define trace characteristics
#Simulation length, time step and Run.ID
 Run.ID <- 1 #as.numeric(params['Run.ID'])
 EndTime <- 300 #as.numeric(params['EndTime'])
 TimeStep <- 5 #as.numeric(params['TimeStep'])

 times <- seq(0, #initial time
              EndTime, #end of simulation
              TimeStep)#time step

#==========================================================================================================================================================================================================
#Define Initial Conditions
#==========================================================================================================================================================================================================
#Load parameters required for determining initial conditions
  alfa <- 0.33 # as.numeric(params['alfa'])
  epsilon <- 5.0 #as.numeric(params['epsilon'])
  size.factor<- 4.0 #as.numeric(params['size.factor'])
  Yre.N.0<-45.55 #as.numeric(params['Yre.0_N'])
  Yce.N.0<-193.2 #as.numeric(params['Yce.0_N'])
  Yre.S.0<-27.82 #as.numeric(params['Yre.0_S'])
  Yce.S.0<-257.54 #as.numeric(params['Yce.0_S'])
  CO2.Concentration.0<-382.2461 #as.numeric(params['CO2.Concentration.0'])

#Initial Productivity conditions are determined by the initial levels of production of energy
#In the Advaced Region
  Ace.N.0<-((Yce.N.0^((epsilon-1)/epsilon)+Yre.N.0^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Yce.N.0/Yre.N.0)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
  Are.N.0<-((Yce.N.0^((epsilon-1)/epsilon)+Yre.N.0^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Yre.N.0/Yce.N.0)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))

#In the Emerging Region
  Ace.S.0<-(1/size.factor)*((Yce.S.0^((epsilon-1)/epsilon)+Yre.S.0^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Yce.S.0/Yre.S.0)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
  Are.S.0<-(1/size.factor)*((Yce.S.0^((epsilon-1)/epsilon)+Yre.S.0^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Yre.S.0/Yce.S.0)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))

#Define vector of intital conditions
InitialConditions <- c(Are.N = Are.N.0,
                       Ace.N = Ace.N.0,
					             Are.S = Are.S.0,
					             Ace.S = Ace.S.0,
                       CO2.Concentration= CO2.Concentration.0)
#==========================================================================================================================================================================================================

#==========================================================================================================================================================================================================
#Define policy parameters
#==========================================================================================================================================================================================================
 policy.vector<-c(
    #carbon tax
	    tax.rate.N=as.numeric(policies[1])/100,
      tax.rate.S=as.numeric(policies[2])/100,
    #Technology push in North
	    epsi.re.subsidy.N = as.numeric(policies[3])/100,
	    s.re.subsidy.N = as.numeric(policies[4])/100,
    #Traditional Green Climate Fund
	    epsi.re.subsidy.S = as.numeric(policies[5])/100,
 	    epsi.re.GFsubsidy.N = as.numeric(policies[6])/100,
	  #R&D Green Climate Fund
	    s.re.subsidy.S = as.numeric(policies[7])/100,
	    s.re.GFsubsidy.N =as.numeric(policies[8])/100,
      policy.half.life = as.numeric(policies[9])/1000)
#==========================================================================================================================================================================================================

#==========================================================================================================================================================================================================
#Define simulation parameters
#==========================================================================================================================================================================================================
Parameters <- c(alfa = 0.33, #as.numeric(params['alfa']),
                epsilon = 5.0,#as.numeric(params['epsilon']),
                size.factor= 4.0,#as.numeric(params['size.factor']),
                Gamma.re = 0.25 ,#as.numeric(params['Gamma.re']),
                k.re = 0.0,#as.numeric(params['k.re']),
                Gamma.ce = 0.25,#as.numeric(params['Gamma.ce']),
                k.ce = 0.0,#as.numeric(params['k.ce']),
                Eta.re= 0.02,#as.numeric(params['Eta.re']),
                Eta.ce= 0.02,#as.numeric(params['Eta.ce']),
                Nu.re = 0.02,#as.numeric(params['Nu.re']),
                Nu.ce= 0.02,#as.numeric(params['Nu.ce']),
                qsi= as.numeric(0.0100539),#as.numeric(params['qsi']),
                Delta.S = 0.001822767,#as.numeric(params['Delta.S']),
				        Delta.Temp.Disaster = as.numeric(6.0) ,#as.numeric(params['Delta.Temp.Disaster']),
				        Beta.Delta.Temp = 5.0 ,#as.numeric(params['Beta.Delta.Temp']),
				        CO2.base = 289.415,#as.numeric(params['CO2.base']),
				        labor.growth.N = 0.0 ,#as.numeric(params['labor.growth_N']),
				        labor.growth.S = 0.0 ,#as.numeric(params['labor.growth_S']),
				        rho = 0.008,#as.numeric(params['rho']),
				        lambda.S = 0.1443,#as.numeric(params['lambda.S']),
				        sigma.utility = 2.0,#as.numeric(params['sigma.utility']),
                policy.vector)
#==========================================================================================================================================================================================================

#==========================================================================================================================================================================================================
#Load EDIAM Differential Equations Structure
#==========================================================================================================================================================================================================
EdiamModelEngine <- function(Time, State, Parameters) {
  with(as.list(c(State, Parameters)), {

  #Policy Instruments
	 #policies in the Advanced Region
	    ce.tax.N<-ifelse(Time<10,0,tax.rate.N*exp(-1*policy.half.life* (Time-10) ))
      RD.subsidy.N<-ifelse(Time<10,0,s.re.subsidy.N*exp(-1*policy.half.life* (Time-10) ))
	    RD.subsidy.GF.N<-ifelse(Time<10,0,s.re.GFsubsidy.N*exp(-1*policy.half.life* (Time-10) ))
	    Tec.subsidy.N<-ifelse(Time<10,0,epsi.re.subsidy.N*exp(-1*policy.half.life* (Time-10) ))
	    Tec.subsidy.GF.N<-ifelse(Time<10,0,epsi.re.GFsubsidy.N*exp(-1*policy.half.life* (Time-10) ))

	 #policies in the Emerging Region
	    ce.tax.S<-ifelse(Time<10,0,tax.rate.S*exp(-1*policy.half.life* (Time-10) ))
      RD.subsidy.S<-ifelse(Time<10,0,s.re.subsidy.S*exp(-1*policy.half.life* (Time-10) ))
	    Tec.subsidy.S<-ifelse(Time<10,0,epsi.re.subsidy.S*exp(-1*policy.half.life* (Time-10) ))

#Economic structure
#Auxiliaries for both regions
     Phi<-(1-alfa)*(1-epsilon)
	   epsi.re<-alfa^2 #this is the cost of production of clean technologies
     epsi.ce<-alfa^2 #this is the cost of production of dirty technologies
#Advanced Region
#Auxiliaries in Advanced Region
     L.N<-exp(labor.growth.N*Time)
	   Gamma.re.t.N<-Gamma.re*exp(-k.re*(Are.N/Are.N.0-1)) #gamma displays decreasing returns as in Stiligtz
     Gamma.ce.t.N<-Gamma.ce*exp(-k.ce*(Ace.N/Ace.N.0-1)) #gamma displays decreasing returns as in Stiligtz

#First we determine the equilibrium levels of relative input prices and relative labor
	   RelPrice.N<-((Ace.N/Are.N)^(1-alfa))*(((epsi.re*(1-Tec.subsidy.N))/epsi.ce)^alfa)
     RelLabor.N<-((1+ce.tax.N)^epsilon)*((((1-Tec.subsidy.N)*epsi.re)/epsi.ce)^(alfa*(1-epsilon)))*((Are.N/Ace.N)^(-1*Phi))
#Second we determine the equilibrium conditions for each sector
     #clean sector
      Labor.re.N<-(RelLabor.N*L.N)/(1+RelLabor.N) #based on the assumption that Labor.re.N+Labor.ce.N=L.N
	    Price.re.N<-RelPrice.N/(RelPrice.N^(1-epsilon)+(1)^(1-epsilon))^(1/(1-epsilon)) #based on the assumption that  Price.re.N^(1-epsilon)+Price.ce.N^(1-epsilon)=1
      Agg.demand.re.tech.N<-((((alfa^2)*Price.re.N)/((1-Tec.subsidy.N)*epsi.re))^(1/(1-alfa)))*Labor.re.N*Are.N
      Profits.re.N<-(1+RD.subsidy.N)*Eta.re*epsi.re*((1-alfa)/alfa)*Agg.demand.re.tech.N # Expected profits see annex IV. Equilibrium research profits
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
#Emerging Region
	#Auxiliaries in Emerging Region
	   L.S<-(exp(labor.growth.S*Time))*size.factor #the population of the South is 4.6 that of the North,
   	 Gamma.re.t.S<-Gamma.re
     Gamma.ce.t.S<-Gamma.ce
  #First we determine the equilibrium levels of relative input prices and relative labour
     RelPrice.S<-((Ace.S/Are.S)^(1-alfa))*(((epsi.re*(1-Tec.subsidy.S))/epsi.ce)^alfa)
     RelLabor.S<-((1+ce.tax.S)^epsilon)*((((1-Tec.subsidy.S)*epsi.re)/epsi.ce)^(alfa*(1-epsilon)))*((Are.S/Ace.S)^(-1*Phi))
  #Second we determine the equilibrium conditions for each sector
    #clean sector
       Labor.re.S<-(L.S*RelLabor.S)/(RelLabor.S+1) #based on the assumption that Labor.re.S+Labor.ce.S=L.S
   	   Price.re.S<-RelPrice.S/(RelPrice.S^(1-epsilon)+(1)^(1-epsilon))^(1/(1-epsilon)) #based on the assumption that  Price.re.S^(1-epsilon)+(Price.ce.S)^(1-epsilon)=1
       Agg.demand.re.tech.S<-((((alfa^2)*Price.re.S)/((1-Tec.subsidy.S)*epsi.re))^(1/(1-alfa)))*Labor.re.S*Are.S
       Profits.re.S<-(1+RD.subsidy.S)*Eta.re*epsi.re*((1-alfa)/alfa)*Agg.demand.re.tech.S # Expected profits see annex IV. Equilibrium research profits
       Yre.S<-((((alfa^2)*Price.re.S)/((1-Tec.subsidy.S)*epsi.re))^(alfa/(1-alfa)))*Labor.re.S*Are.S
    #dirty sector
       Labor.ce.S<-L.S/(RelLabor.S+1)
       Price.ce.S<-Price.re.S/RelPrice.S
       Agg.demand.ce.tech.S<-((((alfa^2)*Price.ce.S)/(epsi.ce))^(1/(1-alfa)))*Labor.ce.S*Ace.S
       Profits.ce.S<-Eta.ce*epsi.ce*((1-alfa)/alfa)*Agg.demand.ce.tech.S # Expected profits see annex IV. Equilibrium research profits
       Yce.S<-((((alfa^2)*Price.ce.S)/(epsi.ce))^(alfa/(1-alfa)))*Labor.ce.S*Ace.S
    #Total Production
      Y.S<-((Yre.S)^((epsilon-1)/epsilon)+(Yce.S)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))
    #Allocation of Scientists
      sre.S<-exp(Profits.re.S)/(exp(Profits.ce.S)+exp(Profits.re.S))
      sce.S<-1-sre.S

#Changes in Temperature
      Delta.Temp.Disaster<-Delta.Temp.Disaster #increase in temperature at which there is environmental disaster
      Delta.Temp<-min(Beta.Delta.Temp*log(CO2.Concentration/CO2.base),Delta.Temp.Disaster) #this equation is ok
   #Welfare Calculations
	    Consumption.N<-Y.N-epsi.re*Agg.demand.re.tech.N-epsi.ce*Agg.demand.ce.tech.N
      Consumption.S<-(Y.S-epsi.re*Agg.demand.re.tech.S-epsi.ce*Agg.demand.ce.tech.S)*(1/size.factor)
      Cost.S.Damage<-((Delta.Temp.Disaster-Delta.Temp)^lambda.S-lambda.S*Delta.Temp.Disaster^(lambda.S-1)*(Delta.Temp.Disaster-Delta.Temp))/((1-lambda.S)*Delta.Temp.Disaster^lambda.S)
	 #Budget restrictions
	    Budget.function.N<-ce.tax.N*Price.ce.N*Yce.N-
	                                 Tec.subsidy.N*epsi.re*Agg.demand.re.tech.N - #including costs of technology subsidies
									                 Tec.subsidy.GF.N*epsi.re*Agg.demand.re.tech.S - #- #green climate fund costs of technology subsidies
					                         RD.subsidy.N*Eta.re*((epsi.re/alfa)-epsi.re)*Agg.demand.re.tech.N - #costs of research subsidies
					                         RD.subsidy.GF.N*Eta.re*((epsi.re/alfa)-epsi.re)*Agg.demand.re.tech.S #cost of green climate fund R&D subsidies

	    Budget.function.S<-ce.tax.S*Price.ce.S*Yce.S-
	                                Tec.subsidy.S*epsi.re*Agg.demand.re.tech.S - #including costs of technology subsidies
					                        RD.subsidy.S*Eta.re*((epsi.re/alfa)-epsi.re)*Agg.demand.re.tech.S  #costs of research subsidies

	    Utility.Consumer.N<-ifelse(Time<(EndTime*0.83),
	                                              ifelse(Cost.S.Damage==0,-100,(1+((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^Time))),
						                                    ifelse(Delta.Temp>=2.0,-100,(1+((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^Time))))
	    Utility.Consumer.S<-ifelse(Time<(EndTime*0.83),
	                                              ifelse(Cost.S.Damage==0,-100,(1+((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^Time))),
	                                              ifelse(Delta.Temp>=2.0,-100,(1+((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^Time))))


   #State variables
    #Evolution of Productivity Advanced Region
     change.rate.Are.N<-Gamma.re.t.N*Eta.re*sre.N*Are.N
     change.rate.Ace.N<-Gamma.ce.t.N*Eta.ce*sce.N*Ace.N
     dAre.N<-Gamma.re.t.N*Eta.re*sre.N*Are.N
     dAce.N<-Gamma.ce.t.N*Eta.ce*sce.N*Ace.N
    #Evolution of Productivity Emering Region
     dAre.S<-Gamma.re.t.S*Nu.re*sre.S*(Are.N-Are.S)
     dAce.S<-Gamma.ce.t.S*Nu.ce*sce.S*(Ace.N-Ace.S)
    #Environmental Quality
     dCO2.Concentration<-qsi*(Yce.N+Yce.S)-Delta.S*CO2.Concentration
    #Define output variables
     vars.out<-list(c(dAre.N,dAce.N,dAre.S,dAce.S,dCO2.Concentration),
                      #RelPrice_N = RelPrice.N,
				              #RelLabor_N = RelLabor.N,
				              #Labor.re_N = Labor.re.N,
				              #Price.re_N = Price.re.N,
				              #Agg.demand.re.tech_N = Agg.demand.re.tech.N,
				              #Profits.re_N = Profits.re.N,
				              #Yre_N = Yre.N,
				              #sre_N = sre.N,
				              #Labor.ce_N = Labor.ce.N,
				              #Price.ce_N = Price.ce.N,
				              #Agg.demand.ce.tech_N = Agg.demand.ce.tech.N,
				              #Profits.ce_N = Profits.ce.N,
				              #Yce_N = Yce.N,
				              #sce_N = sce.N,
				              #Y_N = Y.N,
                      #RelPrice_S = RelPrice.S,
				              #RelLabor_S = RelLabor.S,
				              #Labor.re_S = Labor.re.S,
				              #Price.re_S = Price.re.S,
				              #Agg.demand.re.tech_S = Agg.demand.re.tech.S,
				              #Profits.re_S = Profits.re.S,
				              #Yre_S = Yre.S,
				              #sre_S = sre.S,
				              #Labor.ce_S = Labor.ce.S,
				              #Price.ce_S = Price.ce.S,
				              #Agg.demand.ce.tech_S = Agg.demand.ce.tech.S,
				              #Profits.ce_S = Profits.ce.S,
				              #Yce_S = Yce.S,
				              #sce_S = sce.S,
				              #Y_S = Y.S,
                      Delta.Temp = Delta.Temp,
				              #Gamma.re.t_N = Gamma.re.t.N,
				              #Gamma.re.t_S = Gamma.re.t.S,
				              #Gamma.ce.t_N = Gamma.ce.t.N,
				              #Gamma.ce.t_S = Gamma.ce.t.S,
				              #L_N = L.N,
				              #L_S = L.S,
				              Consumption_N = Consumption.N,
				              Consumption_S = Consumption.S,
				              Utility.Consumer_N = Utility.Consumer.N,
				              Utility.Consumer_S = Utility.Consumer.S,
				              #CO2.Concentration = CO2.Concentration,
				              #Cost.S.Damage=Cost.S.Damage,
				              #policy.status = policy.status,
                      #Policy.Start.Time = policy.start.time*EndTime,
	                    ce.tax_N=ce.tax.N,
				              RD.subsidy_N=RD.subsidy.N,
				              RD.subsidy.GF_N=RD.subsidy.GF.N,
				              Tec.subsidy_N=Tec.subsidy.N,
				              Tec.subsidy.GF_N=Tec.subsidy.GF.N,
				              ce.tax_S=ce.tax.S,
				              RD.subsidy_S=(RD.subsidy.S-RD.subsidy.GF.N),
				              Tec.subsidy_S=(Tec.subsidy.S-Tec.subsidy.GF.N),
				              Budget.function.N = Budget.function.N,
				              Budget.function.S = Budget.function.S,
                      #Tests of new variables
                      Relative.A_N=Are.N/Ace.N,
                      Relative.A_S=Are.S/Ace.S,
                      Decarb.Y_N=Yce.N/(Yce.N+Yre.N),
                      Decarb.Y_S=Yce.S/(Yce.S+Yre.S),
                      change.rate.Are.N=change.rate.Are.N,
                      change.rate.Ace.N=change.rate.Ace.N)
    return(vars.out)
  })
}
#==========================================================================================================================================================================================================

#==========================================================================================================================================================================================================
# Execute Simulation
#==========================================================================================================================================================================================================
out <- as.data.frame(ode(y = InitialConditions,
                          times = times,
                          func = EdiamModelEngine,
                          parms = Parameters,
                        method ="rk4" ))
#Calculate consumption growth rate for both regions
 out$Growth.Rate_N<-c(NA,exp(diff(log(out$Consumption_N)))-1)
 out$Growth.Rate_S<-c(NA,exp(diff(log(out$Consumption_S)))-1)
#set Run.ID
  out$Run.ID<-Run.ID
#return(out)
#Verify budget constraint
 Budget.N<-sum(out$Budget.function.N)
 Budget.S<-sum(out$Budget.function.S)
 ifelse(Budget.N<0,-10000,
         ifelse(Budget.S<0,-10000,
                   1*(sum(as.numeric(out$Utility.Consumer_N))+sum(as.numeric(out$Utility.Consumer_S)))
           ))
}
