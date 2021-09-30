EdiamEquations <- function(t, State, Parameters) {
  with(as.list(c(State, Parameters)), {

#======================================================================================================================================================
#Specify Policy Instruments
#======================================================================================================================================================
	 #policies in the Advanced Region
	    ce.tax.N<-ifelse(t<10,0,ce.tax.N*exp(-1*policy.half.life* (t-10) ))
      RD.subsidy.N<-ifelse(t<10,0,RD.subsidy.N*exp(-1*policy.half.life* (t-10) ))
	    RD.subsidy.GF.N<-ifelse(t<10,0,RD.subsidy.GF.N*exp(-1*policy.half.life* (t-10) ))
	    Tec.subsidy.N<-min(ifelse(t<10,0,Tec.subsidy.N*exp(-1*policy.half.life* (t-10) )),0.90)
	    Tec.subsidy.GF.N<-ifelse(t<10,0,Tec.subsidy.GF.N*exp(-1*policy.half.life* (t-10) ))

	 #policies in the Emerging Region
	    ce.tax.S<-ifelse(t<10,0,ce.tax.S*exp(-1*policy.half.life* (t-10) ))
      RD.subsidy.S<-ifelse(t<10,0,RD.subsidy.S*exp(-1*policy.half.life* (t-10) ))+RD.subsidy.GF.N
	    Tec.subsidy.S<-min(ifelse(t<10,0,Tec.subsidy.S*exp(-1*policy.half.life* (t-10)))+Tec.subsidy.GF.N,0.90)

#======================================================================================================================================================
#Specify Economic Structure
#======================================================================================================================================================

#Economic structure -WITH OIL PRICES-
#======================================================================================================================================================
#Auxiliaries for both regions
     cR<-cR.Data(t) #cR[t] # Oil prices are exogenous
	#Advanced Region
#Auxiliaries in Advanced Region
     epsi_re.N<-epsi.N #this is the cost of production of clean technologies
     epsi_ce.N<-epsi.N #this is the cost of production of dirty technologies
     L.N<-L.N.Data(t)#exp(labor.growth.N*t)*L_0.N
#First we determine the equilibrium levels of relative input prices and relative labor
     RelPrice.N<-( (epsi_re.N^alfa.N)*(alfa_1.N^(2*alfa_1.N))*(alfa_2.N^alfa_2.N)*(A_ce.N^(1-alfa_1.N))*(1-Tec.subsidy.N) )/
                 ( (cR^alfa_2.N)*(epsi_ce.N^alfa_1.N)*(alfa.N^(2*alfa.N))*(A_re.N^(1-alfa.N)) )
     RelLabor.N<-((1+ce.tax.N)^epsilon.N)*
                 ( (
                    ( (alfa.N^(2*alfa.N))*(cR^alfa_2.N)*(epsi_ce.N^alfa_1.N) )/
                    ( ((1-Tec.subsidy.N)^alfa.N)*(alfa_2.N^alfa_2.N)*(alfa_1.N^(2*alfa_1.N))*(epsi_re.N^alfa.N) )
                    )^(epsilon.N-1) )*
                 (
                    ( A_re.N^(-1*phi.N) )/
                    ( A_ce.N^(-1*(1-alfa_1.N)*(1-epsilon.N)) )
                  )
#Second we determine the equilibrium conditions for each sector
     #clean sector
      Labor_re.N<-(RelLabor.N*L.N)/(1+RelLabor.N) #based on the assumption that Labor_re.N+Labor_ce.N=L.N
	    Price_re.N<-RelPrice.N/(RelPrice.N^(1-epsilon.N)+(1)^(1-epsilon.N))^(1/(1-epsilon.N)) #based on  Price_re.N^(1-epsilon)+Price_ce.N^(1-epsilon)=1 [checked many times and it is correct]
      Xtech_re.N<-((((alfa.N^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re.N))^(1/(1-alfa.N)))*Labor_re.N*A_re.N
      Profits_re.N<-(1+RD.subsidy.N)*Eta_re*epsi_re.N*((1-alfa.N)/alfa.N)*Xtech_re.N # Expected profits see annex IV. Equilibrium research profits
      Y_re.N<-((((alfa.N^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re.N))^(alfa.N/(1-alfa.N)))*Labor_re.N*A_re.N
     #dirty sector
      Labor_ce.N<-L.N/(RelLabor.N+1)
      Price_ce.N<-Price_re.N/RelPrice.N
      Re.N<-(alfa_1.N^2/epsi_ce.N)^( alfa_1.N/(1-alfa.N) )*
          (alfa_2.N*A_ce.N/cR)^((1-alfa_1.N)/(1-alfa.N))*
          Labor_ce.N*
          Price_ce.N^(1/(1-alfa.N))
      Xtech_ce.N<- (
                               (
                                ( (alfa_1.N^2)*(Price_ce.N)*(Re.N^alfa_2.N) )/
                                ( epsi_ce.N )
                               )^( 1/(1-alfa_1.N) )
                              )*
                              ( Labor_ce.N^( (1-alfa.N)/(1-alfa_1.N) ) )*
                              ( A_ce.N )
      Profits_ce.N<-Eta_ce*epsi_ce.N*((1-alfa_1.N)/alfa_1.N)*Xtech_ce.N
      Y_ce.N<-( (alfa_1.N^2/epsi_ce.N)^( alfa_1.N/(1-alfa.N) ) )*
             ( (alfa_2.N*A_ce.N/cR)^( alfa_2.N/(1-alfa.N) ) )*
             ( Price_ce.N^( alfa.N/(1-alfa.N) ) )*
             Labor_ce.N*
             A_ce.N
#Total Production
     Y.N<-((Y_re.N)^((epsilon.N-1)/epsilon.N)+(Y_ce.N)^((epsilon.N-1)/epsilon.N))^(epsilon.N/(epsilon.N-1))
#Allocation of scientists
      s_re.N<-Profits_re.N/(Profits_ce.N+Profits_re.N)
      s_ce.N<-1-s_re.N
#Emerging Region
	#Auxiliaries in Emerging Region
     epsi_re.S<-epsi.S #this is the cost of production of clean technologies
     epsi_ce.S<-epsi.S #this is the cost of production of dirty technologies
	   L.S<-L.S.Data(t)
  #First we determine the equilibrium levels of relative input prices and relative labour
     RelPrice.S<-( (epsi_re.S^alfa.S)*(alfa_1.S^(2*alfa_1.S))*(alfa_2.S^alfa_2.S)*(A_ce.S^(1-alfa_1.S))*(1-Tec.subsidy.S) )/
                 ( (cR^alfa_2.S)*(epsi_ce.S^alfa_1.S)*(alfa.S^(2*alfa.S))*(A_re.S^(1-alfa.S)) )
     RelLabor.S<-((1+ce.tax.S)^epsilon.S)*
                 ( (
                    ( (alfa.S^(2*alfa.S))*(cR^alfa_2.S)*(epsi_ce.S^alfa_1.S) )/
                    ( ((1-Tec.subsidy.S)^alfa.S)*(alfa_2.S^alfa_2.S)*(alfa_1.S^(2*alfa_1.S))*(epsi_re.S^alfa.S) )
                    )^(epsilon.S-1) )*
                 (
                    ( A_re.S^(-1*phi.S) )/
                    ( A_ce.S^(-1*(1-alfa_1.S)*(1-epsilon.S)) )
                  )
  #Second we determine the equilibrium conditions for each sector
    #clean sector
       Labor_re.S<-(L.S*RelLabor.S)/(RelLabor.S+1) #based on the assumption that Labor_re.S+Labor_ce.S=L.S
   	   Price_re.S<-RelPrice.S/(RelPrice.S^(1-epsilon.S)+(1)^(1-epsilon.S))^(1/(1-epsilon.S)) #based on  Price_re.S^(1-epsilon)+(Price_ce.S)^(1-epsilon)=1 [checked many times and it is correct]
       Xtech_re.S<-((((alfa.S^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re.S))^(1/(1-alfa.S)))*Labor_re.S*A_re.S
       Profits_re.S<-(1+RD.subsidy.S)*Nu_re*epsi_re.S*((1-alfa.S)/alfa.S)*Xtech_re.S # Expected profits see annex IV. Equilibrium research profits
       Y_re.S<-((((alfa.S^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re.S))^(alfa.S/(1-alfa.S)))*Labor_re.S*A_re.S
    #dirty sector
       Labor_ce.S<-L.S/(RelLabor.S+1)
       Price_ce.S<-Price_re.S/RelPrice.S
       Re.S<-(alfa_1.S^2/epsi_ce.S)^( alfa_1.S/(1-alfa.S) )*
           (alfa_2.S*A_ce.S/cR)^((1-alfa_1.S)/(1-alfa.S))*
           Labor_ce.S*
           Price_ce.S^(1/(1-alfa.S))
       Xtech_ce.S<- (
                                (
                                 ( (alfa_1.S^2)*(Price_ce.S)*(Re.S^alfa_2.S) )/
                                 ( epsi_ce.S )
                                )^( 1/(1-alfa_1.S) )
                               )*
                               ( Labor_ce.S^( (1-alfa.S)/(1-alfa_1.S) ) )*
                               ( A_ce.S )
       Profits_ce.S<-Nu_ce*epsi_ce.S*((1-alfa_1.S)/alfa_1.S)*Xtech_ce.S
       Y_ce.S<-( (alfa_1.S^2/epsi_ce.S)^( alfa_1.S/(1-alfa.S) ) )*
              ( (alfa_2.S*A_ce.S/cR)^( alfa_2.S/(1-alfa.S) ) )*
              ( Price_ce.S^( alfa.S/(1-alfa.S) ) )*
              Labor_ce.S*
              A_ce.S
  #Total Production
      Y.S<-((Y_re.S)^((epsilon.S-1)/epsilon.S)+(Y_ce.S)^((epsilon.S-1)/epsilon.S))^(epsilon.S/(epsilon.S-1))
#Allocation of Scientists
      s_re.S<-Profits_re.S/(Profits_ce.S+Profits_re.S)
      s_ce.S<-1-s_re.S

#======================================================================================================================================================
#Specify Enviromental Effects and Welfare Calculations
#======================================================================================================================================================
#Changes in Temperature
      Delta.Temp.Disaster<-Delta.Temp.Disaster #increase in temperature at which there is environmental disaster
      Delta.Temp<-min(Beta.Delta.Temp*log(CO2.Concentration/CO2.base),Delta.Temp.Disaster) #this equation is ok
#Welfare Calculations
	    Consumption.N<-(Y.N-epsi_re.N*Xtech_re.N-epsi_ce.N*Xtech_ce.N)*(1/L.N)
      Consumption.S<-(Y.S-epsi_re.S*Xtech_re.S-epsi_ce.S*Xtech_ce.S)*(1/L.S)
      Cost.S.Damage<-((Delta.Temp.Disaster-Delta.Temp)^lambda.S-lambda.S*Delta.Temp.Disaster^(lambda.S-1)*(Delta.Temp.Disaster-Delta.Temp))/((1-lambda.S)*Delta.Temp.Disaster^lambda.S)
#Budget restrictions
	    Budget.function.N<-ce.tax.N*Price_ce.N*Y_ce.N-
	                                 Tec.subsidy.N*epsi_re.N*Xtech_re.N - #including costs of technology subsidies
									                 Tec.subsidy.GF.N*epsi_re.S*Xtech_re.S - #- #green climate fund costs of technology subsidies
					                         RD.subsidy.N*Eta_re*((epsi_re.N/alfa.N)-epsi_re.N)*Xtech_re.N - #costs of research subsidies
					                         RD.subsidy.GF.N*Nu_re*((epsi_re.S/alfa.S)-epsi_re.S)*Xtech_re.S #cost of green climate fund R&D subsidies

	    Budget.function.S<-ce.tax.S*Price_ce.S*Y_ce.S-
	                                Tec.subsidy.S*epsi_re.S*Xtech_re.S - #including costs of technology subsidies
					                        RD.subsidy.S*Eta_re*((epsi_re.S/alfa.S)-epsi_re.S)*Xtech_re.S  #costs of research subsidies

	    Utility.Consumer.N<-ifelse(t<(EndTime*0.83),
	                                              ifelse(Cost.S.Damage==0,-100,(1+((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^t))),
						                                    ifelse(Delta.Temp>=2.0,-100,(1+((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^t))))
	    Utility.Consumer.S<-ifelse(t<(EndTime*0.83),
	                                              ifelse(Cost.S.Damage==0,-100,(1+((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^t))),
	                                              ifelse(Delta.Temp>=2.0,-100,(1+((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^t))))


#======================================================================================================================================================
#Specify Differential Equations of State Variables
#======================================================================================================================================================
 #Evolution of the world's technological frontier
     dA_re<-Gamma_re*A_re
     dA_ce<-Gamma_ce*A_ce

 #Evolution of Productivity Advanced Region
     dA_re.N<-Eta_re.N*s_re.N*A_re+(1-Eta_re.N)*s_re.N*A_re.N
     dA_ce.N<-Eta_ce.N*s_ce.N*A_ce+(1-Eta_ce.N)*s_ce.N*A_ce.N

 #Evolution of Productivity Emering Region
    dA_re.S<-Eta_re.S*s_re.S*A_re+(1-Eta_re.S)*s_re.S*A_re.S
    dA_ce.S<-Eta_ce.S*s_ce.S*A_ce+(1-Eta_ce.S)*s_ce.S*A_ce.S
 #Environmental Quality
     dCO2.Concentration<-qsi*(Y_ce.N+Y_ce.S)-Delta.S*CO2.Concentration

#======================================================================================================================================================
 #Define output variables
#======================================================================================================================================================
     vars.out<-list(c(dA_re,dA_ce,dA_re.N,dA_ce.N,dA_re.S,dA_ce.S,dCO2.Concentration),
                      RelPrice.N = RelPrice.N,
				              RelLabor.N = RelLabor.N,
				              #Labor_re.N = Labor_re.N,
				              #Price_re.N = Price_re.N,
				              Xtech_re.N = Xtech_re.N,
				              Profits_re.N = Profits_re.N,
				              Y_re.N = Y_re.N,
				              s_re.N = s_re.N,
				              #Labor_ce.N = Labor_ce.N,
				              Price_ce.N = Price_ce.N,
				              Xtech_ce.N = Xtech_ce.N,
				              Profits_ce.N = Profits_ce.N,
				              Y_ce.N = Y_ce.N,
				              s_ce.N = s_ce.N,
				              Y.N = Y.N,
                      RelPrice.S = RelPrice.S,
				              RelLabor.S = RelLabor.S,
				              Labor_re.S = Labor_re.S,
				              Price_re.S= Price_re.S,
				              Xtech_re.S = Xtech_re.S,
				              Profits_re.S = Profits_re.S,
				              Y_re.S = Y_re.S,
				              #s_re.S = s_re.S,
				              #Labor_ce.S = Labor_ce.S,
				              Price_ce.S = Price_ce.S,
				              #Xtech_ce.S = Xtech_ce.S,
				              Profits_ce.S = Profits_ce.S,
				              Y_ce.S = Y_ce.S,
				              #s_ce.S = s_ce.S,
				              Y.S = Y.S,
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
                      change.rate.A_ce.N=Gamma_ce*Eta_ce*s_ce.N*A_ce.N,
                      cR = cR,
                      L.S= L.S,
                      L.N = L.N)
    return(vars.out)
  })
}
