## =======================================================================================================================================================================================================
## The International Diffusion of Climate Change Mitigation Technologies
## =======================================================================================================================================================================================================
ediam<-function (policies,verbose=FALSE){
#Define trace characteristics
#Simulation length, time step and Run.ID
 Run.ID <- 1 #as.numeric(params['Run.ID'])
 EndTime <- 300 #as.numeric(params['EndTime'])
 TimeStep <- 5 #as.numeric(params['TimeStep'])

 times <- seq(0, #initial time
              EndTime, #end of simulation
              TimeStep)#time step
##

cR<-rep(1,length(times))
cR_0<-cR[1]
##
#==========================================================================================================================================================================================================
#Define Initial Conditions
#==========================================================================================================================================================================================================
#Load parameters required for determining initial conditions
  alfa <- 0.33 # as.numeric(params['alfa'])
  alfa_1<-0.5*alfa
  alfa_2<-0.5*alfa
  epsilon <- 5.0 #as.numeric(params['epsilon'])
  size.factor<- 4.0 #as.numeric(params['size.factor'])
  Y_re0.N<-45.55 #as.numeric(params['Yre.0_N'])
  Y_ce0.N<-193.2 #as.numeric(params['Yce.0_N'])
  Y_re0.S<-27.82 #as.numeric(params['Yre.0_S'])
  Y_ce0.S<-257.54 #as.numeric(params['Yce.0_S'])
  CO2.Concentration_0<-382.2461 #as.numeric(params['CO2.Concentration_0'])

#subindices are undersores "_", supraindices are periods ".", subindices go before periods

#Initial Productivity conditions are determined by the initial levels of production of energy and initial prices of the exhaustable resource
     phi<-(1-alfa)*(1-epsilon)
     phi_1<-(1-alfa_1)*(1-epsilon)
	   epsi<-alfa^2
     teta<-( ( epsi^(alfa_1+alfa_2*alfa+alfa_2*phi) + alfa^(-2*alfa*(phi+alfa)) )^(1/(1-alfa)) )/
           ( ( (alfa_1^(2*alfa_1))*(alfa_2^alfa_2) )^(epsilon) )

#In the advanced region
  L_0.N<-1
  A_ce0.N <- ( ( Y_re0.N*( (alfa^(2*alfa*(1-epsilon)))*((Y_re0.N/(teta*Y_ce0.N))^(phi/(epsilon*(1-alfa))))+
                          ((epsi^alfa_2)*(alfa_2^alfa_2)*(alfa_1^(2*alfa_1)))^(1-epsilon) )^((alfa+phi)/phi)
               ) /
              ( L_0.N *
                       ((((cR_0^(-1*alfa_2*epsilon))*Y_re0.N)/(teta*Y_ce0.N))^(1/(epsilon*(1-alfa)))) *
                       (((epsi^alfa_2)*(alfa_1^(2*alfa_1))*(alfa_2^alfa_2))^((1/(1-alfa))-epsilon))
               )
             ) ^ ( phi/phi_1 )

  A_re0.N<- ( ( ( (cR_0^(-1*alfa_2*epsilon))*Y_re0.N) /
               ( teta*Y_ce0.N )
              )^(1/(epsilon*(1-alfa))) ) *
           ( (A_ce0.N)^((1-alfa_1)/(1-alfa)) )

#In the Emerging Region
 L_0.S<-4
 A_ce0.S <- ( ( Y_re0.S*( (alfa^(2*alfa*(1-epsilon)))*((Y_re0.S/(teta*Y_ce0.S))^(phi/(epsilon*(1-alfa))))+
                         ((epsi^alfa_2)*(alfa_2^alfa_2)*(alfa_1^(2*alfa_1)))^(1-epsilon) )^((alfa+phi)/phi)
              ) /
             ( L_0.S *
                      ((((cR_0^(-1*alfa_2*epsilon))*Y_re0.S)/(teta*Y_ce0.S))^(1/(epsilon*(1-alfa)))) *
                      (((epsi^alfa_2)*(alfa_1^(2*alfa_1))*(alfa_2^alfa_2))^((1/(1-alfa))-epsilon))
              )
            ) ^ ( phi/phi_1 )

 A_re0.S<- ( ( ( (cR_0^(-1*alfa_2*epsilon))*Y_re0.S) /
              ( teta*Y_ce0.S )
             )^(1/(epsilon*(1-alfa))) ) *
          ( (A_ce0.S)^((1-alfa_1)/(1-alfa)) )


#In the Advaced Region
#  A_ce0.N<-((Y_ce0.N^((epsilon-1)/epsilon)+Y_re0.N^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_ce0.N/Y_re0.N)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
#  A_re0.N<-((Y_ce0.N^((epsilon-1)/epsilon)+Y_re0.N^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_re0.N/Y_ce0.N)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))

#In the Emerging Region
#  A_ce0.S<-(1/size.factor)*((Y_ce0.S^((epsilon-1)/epsilon)+Y_re0.S^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_ce0.S/Y_re0.S)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
#  A_re0.S<-(1/size.factor)*((Y_ce0.S^((epsilon-1)/epsilon)+Y_re0.S^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_re0.S/Y_ce0.S)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))

#Define vector of intital conditions
InitialConditions <- c(A_re.N = A_re0.N,
                       A_ce.N = A_ce0.N,
					             A_re.S = A_re0.S,
					             A_ce.S = A_ce0.S,
                       CO2.Concentration= CO2.Concentration_0)
#==========================================================================================================================================================================================================

#==========================================================================================================================================================================================================
#Define policy parameters
#==========================================================================================================================================================================================================
 policy.vector<-c(
    #carbon tax
	    tax.rate.N=signif(as.numeric(policies[1]), digits = 2),
      tax.rate.S=signif(as.numeric(policies[2]), digits = 2),
    #Technology push in North
	    epsi.re.subsidy.N = signif(as.numeric(policies[3]), digits = 2),
	    s.re.subsidy.N = signif(as.numeric(policies[4]), digits = 2),
    #Traditional Green Climate Fund
	    epsi.re.subsidy.S = signif(as.numeric(policies[5]), digits = 2),
 	    epsi.re.GFsubsidy.N = signif(as.numeric(policies[6]), digits = 2),
	  #R&D Green Climate Fund
	    s.re.subsidy.S = signif(as.numeric(policies[7]), digits = 2),
	    s.re.GFsubsidy.N =signif(as.numeric(policies[8]), digits = 2),
      policy.half.life = as.numeric(policies[9]))
#==========================================================================================================================================================================================================

#==========================================================================================================================================================================================================
#Define simulation parameters
#==========================================================================================================================================================================================================
Parameters <- c(alfa = 0.33, #as.numeric(params['alfa']),
                epsilon = 5.0,#as.numeric(params['epsilon']),
                size.factor= 4.0,#as.numeric(params['size.factor']),
                Gamma_re = 0.25 ,#as.numeric(params['Gamma_re']),
                Gamma_ce = 0.25,#as.numeric(params['Gamma_ce']),
                Eta_re= 0.02,#as.numeric(params['Eta_re']),
                Eta_ce= 0.02,#as.numeric(params['Eta_ce']),
                Nu_re = 0.02,#as.numeric(params['Nu_re']),
                Nu_ce= 0.02,#as.numeric(params['Nu_ce']),
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
	    Tec.subsidy.N<-min(ifelse(Time<10,0,epsi.re.subsidy.N*exp(-1*policy.half.life* (Time-10) )),0.90)
	    Tec.subsidy.GF.N<-ifelse(Time<10,0,epsi.re.GFsubsidy.N*exp(-1*policy.half.life* (Time-10) ))

	 #policies in the Emerging Region
	    ce.tax.S<-ifelse(Time<10,0,tax.rate.S*exp(-1*policy.half.life* (Time-10) ))
      RD.subsidy.S<-ifelse(Time<10,0,s.re.subsidy.S*exp(-1*policy.half.life* (Time-10) ))+RD.subsidy.GF.N
	    Tec.subsidy.S<-min(ifelse(Time<10,0,epsi.re.subsidy.S*exp(-1*policy.half.life* (Time-10)))+Tec.subsidy.GF.N,0.90)

#Economic structure -NO OIL PRICES-
#======================================================================================================================================================
#Auxiliaries for both regions
#     phi<-(1-alfa)*(1-epsilon)
#	    epsi_re<-alfa^2 #this is the cost of production of clean technologies
#     epsi_ce<-alfa^2 #this is the cost of production of dirty technologies
#Advanced Region
#Auxiliaries in Advanced Region
#     L.N<-exp(labor.growth.N*Time)
#First we determine the equilibrium levels of relative input prices and relative labor
#     RelPrice.N<-((A_ce.N/A_re.N)^(1-alfa))*(((epsi_re*(1-Tec.subsidy.N))/epsi_ce)^alfa)
#     RelLabor.N<-((1+ce.tax.N)^epsilon)*((((1-Tec.subsidy.N)*epsi_re)/epsi_ce)^(alfa*(1-epsilon)))*((A_re.N/A_ce.N)^(-1*phi))
#Second we determine the equilibrium conditions for each sector
#     #clean sector
#      Labor_re.N<-(RelLabor.N*L.N)/(1+RelLabor.N) #based on the assumption that Labor_re.N+Labor_ce.N=L.N
#	     Price_re.N<-RelPrice.N/(RelPrice.N^(1-epsilon)+(1)^(1-epsilon))^(1/(1-epsilon)) #based on Price_re.N^(1-epsilon)+Price_ce.N^(1-epsilon)=1 [checked many times and it is correct]
#      Xtech_re.N<-((((alfa^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re))^(1/(1-alfa)))*Labor_re.N*A_re.N
#      Profits_re.N<-(1+RD.subsidy.N)*Eta_re*epsi_re*((1-alfa)/alfa)*Xtech_re.N # Expected profits see annex IV. Equilibrium research profits
#      Y_re.N<-((((alfa^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re))^(alfa/(1-alfa)))*Labor_re.N*A_re.N
#     #dirty sector
#      Labor_ce.N<-L.N/(RelLabor.N+1)
#      Price_ce.N<-Price_re.N/RelPrice.N
#      Xtech_ce.N<-((((alfa^2)*Price_ce.N)/(epsi_ce))^(1/(1-alfa)))*Labor_ce.N*A_ce.N
#      Profits_ce.N<-Eta_ce*epsi_ce*((1-alfa)/alfa)*Xtech_ce.N
#      Y_ce.N<-((((alfa^2)*Price_ce.N)/(epsi_ce))^(alfa/(1-alfa)))*Labor_ce.N*A_ce.N
#     #Total Production
#      Y.N<-((Y_re.N)^((epsilon-1)/epsilon)+(Y_ce.N)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))
#     #Allocation of Scientists
#      s_re.N<-exp(Profits_re.N)/(exp(Profits_ce.N)+exp(Profits_re.N))
#      s_ce.N<-1-s_re.N
#Emerging Region
#	#Auxiliaries in Emerging Region
#	   L.S<-(exp(labor.growth.S*Time))*size.factor #the population of the South is 4.6 that of the North,
#  #First we determine the equilibrium levels of relative input prices and relative labour
#     RelPrice.S<-((A_ce.S/A_re.S)^(1-alfa))*(((epsi_re*(1-Tec.subsidy.S))/epsi_ce)^alfa)
#     RelLabor.S<-((1+ce.tax.S)^epsilon)*((((1-Tec.subsidy.S)*epsi_re)/epsi_ce)^(alfa*(1-epsilon)))*((A_re.S/A_ce.S)^(-1*phi))
#  #Second we determine the equilibrium conditions for each sector
#    #clean sector
#       Labor_re.S<-(L.S*RelLabor.S)/(RelLabor.S+1) #based on the assumption that Labor_re.S+Labor_ce.S=L.S
#   	  Price_re.S<-RelPrice.S/(RelPrice.S^(1-epsilon)+(1)^(1-epsilon))^(1/(1-epsilon)) #based on the Price_re.S^(1-epsilon)+(Price_ce.S)^(1-epsilon)=1 [checked many times and it is correct]
#       Xtech_re.S<-((((alfa^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re))^(1/(1-alfa)))*Labor_re.S*A_re.S
#       Profits_re.S<-(1+RD.subsidy.S)*Eta_re*epsi_re*((1-alfa)/alfa)*Xtech_re.S # Expected profits see annex IV. Equilibrium research profits
#       Y_re.S<-((((alfa^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re))^(alfa/(1-alfa)))*Labor_re.S*A_re.S
#    #dirty sector
#       Labor_ce.S<-L.S/(RelLabor.S+1)
#       Price_ce.S<-Price_re.S/RelPrice.S
#       Xtech_ce.S<-((((alfa^2)*Price_ce.S)/(epsi_ce))^(1/(1-alfa)))*Labor_ce.S*A_ce.S
#       Profits_ce.S<-Eta_ce*epsi_ce*((1-alfa)/alfa)*Xtech_ce.S # Expected profits see annex IV. Equilibrium research profits
#       Y_ce.S<-((((alfa^2)*Price_ce.S)/(epsi_ce))^(alfa/(1-alfa)))*Labor_ce.S*A_ce.S
#    #Total Production
#      Y.S<-((Y_re.S)^((epsilon-1)/epsilon)+(Y_ce.S)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))
#    #Allocation of Scientists
#      s_re.S<-exp(Profits_re.S)/(exp(Profits_ce.S)+exp(Profits_re.S))
#      s_ce.S<-1-s_re.S
#======================================================================================================================================================


#Economic structure -WITH OIL PRICES-
#======================================================================================================================================================
#Auxiliaries for both regions
     alfa_1<-0.5*alfa
     alfa_2<-0.5*alfa
     cR<-1.0
     phi<-(1-alfa)*(1-epsilon)
	   epsi_re<-alfa^2 #this is the cost of production of clean technologies
     epsi_ce<-alfa^2 #this is the cost of production of dirty technologies
#Advanced Region
#Auxiliaries in Advanced Region
     L.N<-exp(labor.growth.N*Time)
#First we determine the equilibrium levels of relative input prices and relative labor
     RelPrice.N<-( (epsi_re^alfa)*(alfa_1^(2*alfa_1))*(alfa_2^alfa_2)*(A_ce.N^(1-alfa_1))*(1-Tec.subsidy.N) )/
                 ( (cR^alfa_2)*(epsi_ce^alfa_1)*(alfa^(2*alfa))*(A_re.N^(1-alfa)) )
     RelLabor.N<-((1+ce.tax.N)^epsilon)*
                 ( (
                    ( (alfa^(2*alfa))*(cR^alfa_2)*(epsi_ce^alfa_1) )/
                    ( ((1-Tec.subsidy.N)^alfa)*(alfa_2^alfa_2)*(alfa_1^(2*alfa_1))*(epsi_re^alfa) )
                    )^(epsilon-1) )*
                 (
                    ( A_re.N^(-1*phi) )/
                    ( A_ce.N^(-1*(1-alfa_1)*(1-epsilon)) )
                  )
#Second we determine the equilibrium conditions for each sector
     #clean sector
      Labor_re.N<-(RelLabor.N*L.N)/(1+RelLabor.N) #based on the assumption that Labor_re.N+Labor_ce.N=L.N
	    Price_re.N<-RelPrice.N/(RelPrice.N^(1-epsilon)+(1)^(1-epsilon))^(1/(1-epsilon)) #based on  Price_re.N^(1-epsilon)+Price_ce.N^(1-epsilon)=1 [checked many times and it is correct]
      Xtech_re.N<-((((alfa^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re))^(1/(1-alfa)))*Labor_re.N*A_re.N
      Profits_re.N<-(1+RD.subsidy.N)*Eta_re*epsi_re*((1-alfa)/alfa)*Xtech_re.N # Expected profits see annex IV. Equilibrium research profits
      Y_re.N<-((((alfa^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re))^(alfa/(1-alfa)))*Labor_re.N*A_re.N
     #dirty sector
      Labor_ce.N<-L.N/(RelLabor.N+1)
      Price_ce.N<-Price_re.N/RelPrice.N
      Re<-(alfa_1^2/epsi_ce)^( alfa_1/(1-alfa) )*
          (alfa_2*A_ce.N/cR)^((1-alfa_1)/(1-alfa))*
          Labor_ce.N*
          Price_ce.N^(1/(1-alfa))
      Xtech_ce.N<- (
                               (
                                ( (alfa_1^2)*(Price_ce.N)*(Re^alfa_2) )/
                                ( epsi_ce )
                               )^( 1/(1-alfa_1) )
                              )*
                              ( Labor_ce.N^( (1-alfa)/(1-alfa_1) ) )*
                              ( A_ce.N )
      Profits_ce.N<-Eta_ce*epsi_ce*((1-alfa_1)/alfa_1)*Xtech_ce.N
      Y_ce.N<-( (alfa_1^2/epsi_ce)^( alfa_1/(1-alfa) ) )*
             ( (alfa_2*A_ce.N/cR)^( alfa_2/(1-alfa) ) )*
             ( Price_ce.N^( alfa/(1-alfa) ) )*
             Labor_ce.N*
             A_ce.N
     #Total Production
      Y.N<-((Y_re.N)^((epsilon-1)/epsilon)+(Y_ce.N)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))
     #Allocation of Scientists
      s_re.N<-exp(Profits_re.N)/(exp(Profits_ce.N)+exp(Profits_re.N))
      s_ce.N<-1-s_re.N
#Emerging Region
	#Auxiliaries in Emerging Region
	   L.S<-(exp(labor.growth.S*Time))*size.factor #the population of the South is 4.6 that of the North,
  #First we determine the equilibrium levels of relative input prices and relative labour
     RelPrice.S<-( (epsi_re^alfa)*(alfa_1^(2*alfa_1))*(alfa_2^alfa_2)*(A_ce.S^(1-alfa_1))*(1-Tec.subsidy.S) )/
                 ( (cR^alfa_2)*(epsi_ce^alfa_1)*(alfa^(2*alfa))*(A_re.S^(1-alfa)) )
     RelLabor.S<-((1+ce.tax.S)^epsilon)*
                 ( (
                    ( (alfa^(2*alfa))*(cR^alfa_2)*(epsi_ce^alfa_1) )/
                    ( ((1-Tec.subsidy.S)^alfa)*(alfa_2^alfa_2)*(alfa_1^(2*alfa_1))*(epsi_re^alfa) )
                    )^(epsilon-1) )*
                 (
                    ( A_re.S^(-1*phi) )/
                    ( A_ce.S^(-1*(1-alfa_1)*(1-epsilon)) )
                  )
  #Second we determine the equilibrium conditions for each sector
    #clean sector
       Labor_re.S<-(L.S*RelLabor.S)/(RelLabor.S+1) #based on the assumption that Labor_re.S+Labor_ce.S=L.S
   	   Price_re.S<-RelPrice.S/(RelPrice.S^(1-epsilon)+(1)^(1-epsilon))^(1/(1-epsilon)) #based on  Price_re.S^(1-epsilon)+(Price_ce.S)^(1-epsilon)=1 [checked many times and it is correct]
       Xtech_re.S<-((((alfa^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re))^(1/(1-alfa)))*Labor_re.S*A_re.S
       Profits_re.S<-(1+RD.subsidy.S)*Eta_re*epsi_re*((1-alfa)/alfa)*Xtech_re.S # Expected profits see annex IV. Equilibrium research profits
       Y_re.S<-((((alfa^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re))^(alfa/(1-alfa)))*Labor_re.S*A_re.S
    #dirty sector
       Labor_ce.S<-L.S/(RelLabor.S+1)
       Price_ce.S<-Price_re.S/RelPrice.S
       Re<-(alfa_1^2/epsi_ce)^( alfa_1/(1-alfa) )*
           (alfa_2*A_ce.S/cR)^((1-alfa_1)/(1-alfa))*
           Labor_ce.S*
           Price_ce.S^(1/(1-alfa))
       Xtech_ce.S<- (
                                (
                                 ( (alfa_1^2)*(Price_ce.S)*(Re^alfa_2) )/
                                 ( epsi_ce )
                                )^( 1/(1-alfa_1) )
                               )*
                               ( Labor_ce.S^( (1-alfa)/(1-alfa_1) ) )*
                               ( A_ce.S )
       Profits_ce.S<-Eta_ce*epsi_ce*((1-alfa_1)/alfa_1)*Xtech_ce.S
       Y_ce.S<-( (alfa_1^2/epsi_ce)^( alfa_1/(1-alfa) ) )*
              ( (alfa_2*A_ce.S/cR)^( alfa_2/(1-alfa) ) )*
              ( Price_ce.S^( alfa/(1-alfa) ) )*
              Labor_ce.S*
              A_ce.N
    #Total Production
      Y.S<-((Y_re.S)^((epsilon-1)/epsilon)+(Y_ce.S)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))
    #Allocation of Scientists
      s_re.S<-exp(Profits_re.S)/(exp(Profits_ce.S)+exp(Profits_re.S))
      s_ce.S<-1-s_re.S

#Changes in Temperature
      Delta.Temp.Disaster<-Delta.Temp.Disaster #increase in temperature at which there is environmental disaster
      Delta.Temp<-min(Beta.Delta.Temp*log(CO2.Concentration/CO2.base),Delta.Temp.Disaster) #this equation is ok
   #Welfare Calculations
	    Consumption.N<-Y.N-epsi_re*Xtech_re.N-epsi_ce*Xtech_ce.N
      Consumption.S<-(Y.S-epsi_re*Xtech_re.S-epsi_ce*Xtech_ce.S)*(1/size.factor)
      Cost.S.Damage<-((Delta.Temp.Disaster-Delta.Temp)^lambda.S-lambda.S*Delta.Temp.Disaster^(lambda.S-1)*(Delta.Temp.Disaster-Delta.Temp))/((1-lambda.S)*Delta.Temp.Disaster^lambda.S)
	 #Budget restrictions
	    Budget.function.N<-ce.tax.N*Price_ce.N*Y_ce.N-
	                                 Tec.subsidy.N*epsi_re*Xtech_re.N - #including costs of technology subsidies
									                 Tec.subsidy.GF.N*epsi_re*Xtech_re.S - #- #green climate fund costs of technology subsidies
					                         RD.subsidy.N*Eta_re*((epsi_re/alfa)-epsi_re)*Xtech_re.N - #costs of research subsidies
					                         RD.subsidy.GF.N*Eta_re*((epsi_re/alfa)-epsi_re)*Xtech_re.S #cost of green climate fund R&D subsidies

	    Budget.function.S<-ce.tax.S*Price_ce.S*Y_ce.S-
	                                Tec.subsidy.S*epsi_re*Xtech_re.S - #including costs of technology subsidies
					                        RD.subsidy.S*Eta_re*((epsi_re/alfa)-epsi_re)*Xtech_re.S  #costs of research subsidies

	    Utility.Consumer.N<-ifelse(Time<(EndTime*0.83),
	                                              ifelse(Cost.S.Damage==0,-100,(1+((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^Time))),
						                                    ifelse(Delta.Temp>=2.0,-100,(1+((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^Time))))
	    Utility.Consumer.S<-ifelse(Time<(EndTime*0.83),
	                                              ifelse(Cost.S.Damage==0,-100,(1+((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^Time))),
	                                              ifelse(Delta.Temp>=2.0,-100,(1+((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^Time))))


   #State variables
    #Evolution of Productivity Advanced Region
     dA_re.N<-Gamma_re*Eta_re*s_re.N*A_re.N
     dA_ce.N<-Gamma_ce*Eta_ce*s_ce.N*A_ce.N
    #Evolution of Productivity Emering Region
     dA_re.S<-Gamma_re*Nu_re*s_re.S*(A_re.N-A_re.S)
     dA_ce.S<-Gamma_ce*Nu_ce*s_ce.S*(A_ce.N-A_ce.S)
    #Environmental Quality
     dCO2.Concentration<-qsi*(Y_ce.N+Y_ce.S)-Delta.S*CO2.Concentration
    #Define output variables
     vars.out<-list(c(dA_re.N,dA_ce.N,dA_re.S,dA_ce.S,dCO2.Concentration),
                      RelPrice.N = RelPrice.N,
				              RelLabor.N = RelLabor.N,
				              #Labor_re.N = Labor_re.N,
				              #Price_re.N = Price_re.N,
				              #Xtech_re.N = Xtech_re.N,
				              #Profits_re.N = Profits_re.N,
				              #Y_re.N = Y_re.N,
				              #s_re.N = s_re.N,
				              #Labor_ce.N = Labor_ce.N,
				              #Price_ce.N = Price_ce.N,
				              #Xtech_ce.N = Xtech_ce.N,
				              #Profits_ce.N = Profits_ce.N,
				              #Y_ce.N = Y_ce.N,
				              #s_ce.N = s_ce.N,
				              #Y.N = Y.N,
                      RelPrice.S = RelPrice.S,
				              RelLabor.S = RelLabor.S,
				              Labor_re.S = Labor_re.S,
				              Price_re.S= Price_re.S,
				              Xtech_re.S = Xtech_re.S,
				              #Profits_re.S = Profits_re.S,
				              #Y_re.S = Y_re.S,
				              #s_re.S = s_re.S,
				              #Labor_ce.S = Labor_ce.S,
				              Price_ce.S = Price_ce.S,
				              #Xtech_ce.S = Xtech_ce.S,
				              #Profits_ce.S = Profits_ce.S,
				              #Y_ce.S = Y_ce.S,
				              #s_ce.S = s_ce.S,
				              #Y.S = Y.S,
                      Delta.Temp = Delta.Temp,
				              #L.N = L.N,
				              #L.S = L.S,
				              Consumption.N = Consumption.N,
				              Consumption.S = Consumption.S,
				              Utility.Consumer.N = Utility.Consumer.N,
				              Utility.Consumer.S = Utility.Consumer.S,
				              #CO2.Concentration = CO2.Concentration,
				              #Cost.S.Damage=Cost.S.Damage,
				              #policy.status = policy.status,
                      #Policy.Start.Time = policy.start.time*EndTime,
	                    ce.tax.N=ce.tax.N,
				              RD.subsidy.N=RD.subsidy.N,
				              RD.subsidy.GF.N=RD.subsidy.GF.N,
				              Tec.subsidy.N=Tec.subsidy.N,
				              Tec.subsidy.GF.N=Tec.subsidy.GF.N,
				              ce.tax.S=ce.tax.S,
				              RD.subsidy.S=(RD.subsidy.S-RD.subsidy.GF.N),
				              #Tec.subsidy.S=(Tec.subsidy.S-Tec.subsidy.GF.N),
                      Tec.subsidy.S=Tec.subsidy.S,
				              Budget.function.N = Budget.function.N,
				              Budget.function.S = Budget.function.S,
                      #Tests of new variables
                      RelativeA.N=A_re.N/A_ce.N,
                      RelativeA.S=A_re.S/A_ce.S,
                      DecarbY.N=Y_ce.N/(Y_ce.N+Y_re.N),
                      DecarbY.S=Y_ce.S/(Y_ce.S+Y_re.S),
                      change.rate.A_re.N=Gamma_re*Eta_re*s_re.N*A_re.N,
                      change.rate.A_ce.N=Gamma_ce*Eta_ce*s_ce.N*A_ce.N)
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
#Define objetive function
 Social.Welfare.Function<-sum(as.numeric(out$Utility.Consumer_N))+sum(as.numeric(out$Utility.Consumer_S))

#Check constraints are valid
#Consumption constraint -In no year consumption can be negative-
 Social.Welfare.Function<-ifelse(min(out$Consumption.N)<0,-1000000,
                                 ifelse(min(out$Consumption.S)<0,-1000000,Social.Welfare.Function))

#Budget constraint -the cost of policies cannot exceed total budget-
 Social.Welfare.Function<-ifelse(sum(out$Budget.function.N)<0,-1000000,
                                 ifelse(sum(out$Budget.function.S)<0,-1000000,Social.Welfare.Function))

#Calculate consumption growth rate for both regions
 out$Growth.Rate_N<-c(NA,diff(out$Consumption.N)/out$Consumption.N[1:(length(out$Consumption.N)-1)])
 out$Growth.Rate_S<-c(NA,diff(out$Consumption.S)/out$Consumption.S[1:(length(out$Consumption.S)-1)])
#set Run.ID
 out$Run.ID<-Run.ID
#set social function value
 out$Social.Welfare.Function<-Social.Welfare.Function
if (verbose==FALSE) {
  return(Social.Welfare.Function)
 } else {return(out)}
}
