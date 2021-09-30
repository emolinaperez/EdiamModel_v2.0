EdiamEquations <- function(t,State,Parameters) {

#Integrate all data into only one object
 Data<-as.list(c(State,Parameters))

#Execute simulation environment
 with(Data, {
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
     Price.oil<-Price.oil.Data(t)  # Oil prices are exogenous to both regions
#Advanced Region
#Auxiliaries in Advanced Region
     cR.N <- Price.oil/Price.final.N
     epsi_re.N<-epsi_re0.N * exp(-1*lrng.re * 1e-2 * ((E_re.N-Xtech_re0.N)/Xtech_re0.N) ) #this is the cost of production of clean technologies
     epsi_ce.N<-epsi_ce0.N * exp(-1*lrng.ce * 1e-2 * ((E_ce.N-Xtech_ce0.N)/Xtech_ce0.N) )  #this is the cost of production of dirty technologies
     L.N<-L.N.Data(t) #exp(labor.growth.N*t)*L_0.N
#First we determine the equilibrium levels of relative input prices and relative labor
     RelPrice.N<-( (epsi_re.N^alfa.N)*(alfa_1.N^(2*alfa_1.N))*(alfa_2.N^alfa_2.N)*(A_ce.N^(1-alfa_1.N))*(1-Tec.subsidy.N) )/
                 ( (cR.N^alfa_2.N)*(epsi_ce.N^alfa_1.N)*(alfa.N^(2*alfa.N))*(A_re.N^(1-alfa.N)) )
     RelLabor.N<-((1+ce.tax.N)^epsilon.N)*
                 ( (
                    ( (alfa.N^(2*alfa.N))*(cR.N^alfa_2.N)*(epsi_ce.N^alfa_1.N) )/
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
      Profits_re.N<-(1+RD.subsidy.N)*Eta_re.N*epsi_re.N*((1-alfa.N)/alfa.N)*Xtech_re.N # Expected profits see annex IV. Equilibrium research profits
      Y_re.N<-((((alfa.N^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re.N))^(alfa.N/(1-alfa.N)))*Labor_re.N*A_re.N
     #dirty sector
      Labor_ce.N<-L.N/(RelLabor.N+1)
      Price_ce.N<-Price_re.N/RelPrice.N
      Re.N<-(alfa_1.N^2/epsi_ce.N)^( alfa_1.N/(1-alfa.N) )*
          (alfa_2.N*A_ce.N/cR.N)^((1-alfa_1.N)/(1-alfa.N))*
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
      Profits_ce.N<-Eta_ce.N*epsi_ce.N*((1-alfa_1.N)/alfa_1.N)*Xtech_ce.N
      Y_ce.N<-( (alfa_1.N^2/epsi_ce.N)^( alfa_1.N/(1-alfa.N) ) )*
             ( (alfa_2.N*A_ce.N/cR.N)^( alfa_2.N/(1-alfa.N) ) )*
             ( Price_ce.N^( alfa.N/(1-alfa.N) ) )*
             Labor_ce.N*
             A_ce.N
#Total Production
    #Y.N<-((Y_re.N)^((epsilon.N-1)/epsilon.N)+(Y_ce.N)^((epsilon.N-1)/epsilon.N))^(epsilon.N/(epsilon.N-1))
     Y.N<-pi.N*(((Y_re.N)^((epsilon.N-1)/epsilon.N)+(Y_ce.N)^((epsilon.N-1)/epsilon.N))^(epsilon.N/(epsilon.N-1)))
#Price of final good
     Price_Y.N<-(1/alfa_2.N) * Price.oil * ( Re.N/Y_ce.N) * ( (Y_re.N/Y_ce.N)^( (epsilon.N-1)/epsilon.N ) + 1 )^(1/(1-epsilon.N))
#Allocation of scientists
     Value.RE.N<-log((Profits_re.N/Profits0.N)^val.param)
     Value.CE.N<-log((Profits_ce.N/Profits0.N)^val.param)
      s_re.N<-exp(Value.RE.N)/(exp(Value.RE.N)+exp(Value.CE.N))
    #  s_re.N<-ifelse(Profits_re.N>=Profits_ce.N,1,0)
      s_ce.N<-1-s_re.N
#Emerging Region
#
#Auxiliaries in Emerging Region
     cR.S <- Price.oil/Price.final.S
     epsi_re.S<-epsi_re0.S * exp(-1 * lrng.re.S * 1e-2 * ((E_re.S-Xtech_re0.S)/Xtech_re0.S)  ) #this is the cost of production of clean technologies
     epsi_ce.S<-epsi_ce0.S * exp(-1 * lrng.ce.S * 1e-2 * ((E_ce.S-Xtech_ce0.S)/Xtech_ce0.S) )  #this is the cost of production of dirty technologies
     L.S<-L.S.Data(t) #exp(labor.growth.S*t)*L_0.S
#First we determine the equilibrium levels of relative input prices and relative labor
     RelPrice.S<-( (epsi_re.S^alfa.S)*(alfa_1.S^(2*alfa_1.S))*(alfa_2.S^alfa_2.S)*(A_ce.S^(1-alfa_1.S))*(1-Tec.subsidy.S) )/
                 ( (cR.S^alfa_2.S)*(epsi_ce.S^alfa_1.S)*(alfa.S^(2*alfa.S))*(A_re.S^(1-alfa.S)) )
     RelLabor.S<-((1+ce.tax.S)^epsilon.S)*
                 ( (
                    ( (alfa.S^(2*alfa.S))*(cR.S^alfa_2.S)*(epsi_ce.S^alfa_1.S) )/
                    ( ((1-Tec.subsidy.S)^alfa.S)*(alfa_2.S^alfa_2.S)*(alfa_1.S^(2*alfa_1.S))*(epsi_re.S^alfa.S) )
                    )^(epsilon.S-1) )*
                 (
                    ( A_re.S^(-1*phi.S) )/
                    ( A_ce.S^(-1*(1-alfa_1.S)*(1-epsilon.S)) )
                  )
#Second we determine the equilibrium conditions for each sector
     #clean sector
      Labor_re.S<-(RelLabor.S*L.S)/(1+RelLabor.S) #based on the assumption that Labor_re.S+Labor_ce.S=L.S
	    Price_re.S<-RelPrice.S/(RelPrice.S^(1-epsilon.S)+(1)^(1-epsilon.S))^(1/(1-epsilon.S)) #based on  Price_re.S^(1-epsilon)+Price_ce.S^(1-epsilon)=1 [checked many times and it is correct]
      Xtech_re.S<-((((alfa.S^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re.S))^(1/(1-alfa.S)))*Labor_re.S*A_re.S
      Profits_re.S<-(1+RD.subsidy.S)*Eta_re.S*epsi_re.S*((1-alfa.S)/alfa.S)*Xtech_re.S # Expected profits see annex IV. Equilibrium research profits
      Y_re.S<-((((alfa.S^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re.S))^(alfa.S/(1-alfa.S)))*Labor_re.S*A_re.S
     #dirty sector
      Labor_ce.S<-L.S/(RelLabor.S+1)
      Price_ce.S<-Price_re.S/RelPrice.S
      Re.S<-(alfa_1.S^2/epsi_ce.S)^( alfa_1.S/(1-alfa.S) )*
          (alfa_2.S*A_ce.S/cR.S)^((1-alfa_1.S)/(1-alfa.S))*
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
      Profits_ce.S<-Eta_ce.S*epsi_ce.S*((1-alfa_1.S)/alfa_1.S)*Xtech_ce.S
      Y_ce.S<-( (alfa_1.S^2/epsi_ce.S)^( alfa_1.S/(1-alfa.S) ) )*
             ( (alfa_2.S*A_ce.S/cR.S)^( alfa_2.S/(1-alfa.S) ) )*
             ( Price_ce.S^( alfa.S/(1-alfa.S) ) )*
             Labor_ce.S*
             A_ce.S
#Total Production
    #Y.S<-((Y_re.S)^((epsilon.S-1)/epsilon.S)+(Y_ce.S)^((epsilon.S-1)/epsilon.S))^(epsilon.S/(epsilon.S-1))
     Y.S<-pi.S*(((Y_re.S)^((epsilon.S-1)/epsilon.S)+(Y_ce.S)^((epsilon.S-1)/epsilon.S))^(epsilon.S/(epsilon.S-1)))
#Price of final good
     Price_Y.S<-(1/alfa_2.S) * Price.oil * ( Re.S/Y_ce.S) * ( (Y_re.S/Y_ce.S)^( (epsilon.S-1)/epsilon.S ) + 1 )^(1/(1-epsilon.S))
#Allocation of scientists
     Value.RE.S<-log((Profits_re.S/Profits0.S)^val.param.S)
     Value.CE.S<-log((Profits_ce.S/Profits0.S)^val.param.S)
      s_re.S<-exp(Value.RE.S)/(exp(Value.RE.S)+exp(Value.CE.S))
    #  s_re.S<-ifelse(Profits_re.S>=Profits_ce.S,1,0)
      s_ce.S<-1-s_re.S

#======================================================================================================================================================
#Specify Enviromental Effects and Welfare Calculations
#======================================================================================================================================================
#Changes in Temperature
      Delta.Temp.Disaster<-Delta.Temp.Disaster #increase in temperature at which there is environmental disaster
      Delta.Temp<-3.5 #min(Beta.Delta.Temp*log(CO2.Concentration/CO2.base),Delta.Temp.Disaster) #this equation is ok
#Welfare Calculations
      Consumption.N<-(Y.N-epsi_re.N*Xtech_re.N-epsi_ce.N*Xtech_ce.N-cR.N*Re.N)
      Consumption.S<-(Y.S-epsi_re.S*Xtech_re.S-epsi_ce.S*Xtech_ce.S-cR.S*Re.S)
      ConsumptionXcapita.N<-Consumption.N*(1/L.N)
      ConsumptionXcapita.S<-Consumption.S*(1/L.S)
      GDP.N<-Price_Y.N * Consumption.N
      GDP.S<-Price_Y.S * Consumption.S
      Cost.S.Damage<-((Delta.Temp.Disaster-Delta.Temp)^lambda.S-lambda.S*Delta.Temp.Disaster^(lambda.S-1)*(Delta.Temp.Disaster-Delta.Temp))/((1-lambda.S)*Delta.Temp.Disaster^lambda.S)
#
Utility.Consumer.N<-ifelse(t<(EndTime*0.83),
                                          ifelse(Cost.S.Damage==0,-100,(1+((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^t))),
                                          ifelse(Delta.Temp>=2.0,-100,(1+((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^t))))
Utility.Consumer.S<-ifelse(t<(EndTime*0.83),
                                          ifelse(Cost.S.Damage==0,-100,(1+((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^t))),
                                          ifelse(Delta.Temp>=2.0,-100,(1+((Cost.S.Damage*Consumption.S)^(1-sigma.utility))/(1-sigma.utility))*(1/((1+rho)^t))))


#Budget restrictions
	    Budget.function.N<-ce.tax.N*Price_ce.N*Y_ce.N-
	                                 Tec.subsidy.N*epsi_re.N*Xtech_re.N - #including costs of technology subsidies
									                 Tec.subsidy.GF.N*epsi_re.S*Xtech_re.S - #- #green climate fund costs of technology subsidies
					                         RD.subsidy.N*Eta_re.N*((epsi_re.N/alfa.N)-epsi_re.N)*Xtech_re.N - #costs of research subsidies
					                         RD.subsidy.GF.N*Eta_re.S*((epsi_re.S/alfa.S)-epsi_re.S)*Xtech_re.S #cost of green climate fund R&D subsidies

	    Budget.function.S<-ce.tax.S*Price_ce.S*Y_ce.S-
	                                Tec.subsidy.S*epsi_re.S*Xtech_re.S - #including costs of technology subsidies
					                        RD.subsidy.S*Eta_re.S*((epsi_re.S/alfa.S)-epsi_re.S)*Xtech_re.S  #costs of research subsidies



#======================================================================================================================================================
#Specify Differential Equations of State Variables
#======================================================================================================================================================
 #Evolution of the world's technological frontier
     A_re.t1<-(1+Gamma_re) * A_re
     A_ce.t1<-(1+Gamma_ce) * A_ce
  #   A_re.t1<-(1+Gamma_re * ((s_re.N+s_re.S)/2) ) * A_re
  #   A_ce.t1<-(1+Gamma_ce * ((s_ce.N+s_ce.S)/2) ) * A_ce

 #Evolution of Productivity Advanced Region
  #   A_re.N.t1<- Eta_re.N*A_re.t1+(1-Eta_re.N)*A_re.N
  #   A_ce.N.t1<- Eta_ce.N*A_ce.t1+(1-Eta_ce.N)*A_ce.N
  #   A_re.N.t1<-s_re.N * ( Eta_re.N*A_re.t1+(1-Eta_re.N)*A_re.N )
  #   A_ce.N.t1<-s_ce.N * ( Eta_ce.N*A_ce.t1+(1-Eta_ce.N)*A_ce.N )
     A_re.N.t1<-Eta_re.N*s_re.N*A_re.t1+(1-s_re.N*Eta_re.N)*A_re.N
     A_ce.N.t1<-Eta_ce.N*s_ce.N*A_ce.t1+(1-s_ce.N*Eta_ce.N)*A_ce.N

 #Evolution of Productivity Emerging Region
  #   A_re.S.t1<-Eta_re.S*A_re.t1+(1-Eta_re.S)*A_re.S
  #   A_ce.S.t1<-Eta_ce.S*A_ce.t1+(1-Eta_ce.S)*A_ce.S
  #   A_re.S.t1<-s_re.S * ( Eta_re.S*A_re.t1+(1-Eta_re.S)*A_re.S )
  #   A_ce.S.t1<-s_ce.S * ( Eta_ce.S*A_ce.t1+(1-Eta_ce.S)*A_ce.S )
     A_re.S.t1<-Eta_re.S*s_re.S*A_re.t1+(1-s_re.S*Eta_re.S)*A_re.S
     A_ce.S.t1<-Eta_ce.S*s_ce.S*A_ce.t1+(1-s_ce.S*Eta_ce.S)*A_ce.S
 #Acumulation of experience
     E_re.N.t1<-E_re.N+Xtech_re.N
     E_ce.N.t1<-E_ce.N+Xtech_ce.N
     E_re.S.t1<-E_re.S+Xtech_re.S
     E_ce.S.t1<-E_ce.S+Xtech_ce.S
 #Environmental Quality
     CO2.Concentration.t1<-qsi*(Y_ce.N+Y_ce.S)-Delta.S*CO2.Concentration

#======================================================================================================================================================
 #Define output variables
#======================================================================================================================================================
     vars.out<-data.frame(
              #State variables
                      A_re = A_re,
                      A_ce = A_ce,
                      A_re.N  = A_re.N,
                      A_ce.N = A_ce.N,
                      A_re.S = A_re.S,
                      A_ce.S = A_ce.S,
                      E_re.N = E_re.N ,
                      E_ce.N = E_ce.N,
                      E_re.S = E_re.S,
                      E_ce.S = E_ce.S,
                      CO2.Concentration = CO2.Concentration ,
               #Updated value
                      A_re.t1 = A_re.t1,
                      A_ce.t1 = A_ce.t1,
                      A_re.N.t1  = A_re.N.t1,
                      A_ce.N.t1 = A_ce.N.t1,
                      A_re.S.t1 = A_re.S.t1,
                      A_ce.S.t1 = A_ce.S.t1,
                      E_re.N.t1 = E_re.N.t1 ,
                      E_ce.N.t1 = E_ce.N.t1,
                      E_re.S.t1 = E_re.S.t1,
                      E_ce.S.t1 = E_ce.S.t1,
                      CO2.Concentration.t1 = CO2.Concentration.t1,
               #Auxiliaries
                      #RelPrice.N = RelPrice.N,
				              #RelLabor.N = RelLabor.N,
				              #Labor_re.N = Labor_re.N,
				              Price_re.N = Price_re.N,
				              Xtech_re.N = Xtech_re.N,
				              Profits_re.N = Profits_re.N,
				              Y_re.N = Y_re.N,
				              s_re.N = s_re.N,
				              #Labor_ce.N = Labor_ce.N,
				              Price_ce.N = Price_ce.N,
				              Xtech_ce.N = Xtech_ce.N,
                      #Re.N= Re.N,
				              Profits_ce.N = Profits_ce.N,
				              Y_ce.N = Y_ce.N,
				              s_ce.N = s_ce.N,
				              #Y.N = Y.N,
                      Price_Y.N = Price_Y.N,
                      GDP.N = GDP.N,
                      #RelPrice.S = RelPrice.S,
				              #RelLabor.S = RelLabor.S,
				              #Labor_re.S = Labor_re.S,
				              Price_re.S= Price_re.S,
				              Xtech_re.S = Xtech_re.S,
				              Profits_re.S = Profits_re.S,
				              Y_re.S = Y_re.S,
				              s_re.S = s_re.S,
				              #Labor_ce.S = Labor_ce.S,
				              Price_ce.S = Price_ce.S,
				              Xtech_ce.S = Xtech_ce.S,
				              Profits_ce.S = Profits_ce.S,
				              Y_ce.S = Y_ce.S,
				              s_ce.S = s_ce.S,
				              Y.S = Y.S,
                      Price_Y.S = Price_Y.S,
                      GDP.S = GDP.S,
                      #Delta.Temp = Delta.Temp,
				              #L.N = L.N,
				              #L.S = L.S,
				              #Consumption.N = Consumption.N,
				              #Consumption.S = Consumption.S,
				              #Utility.Consumer.N = Utility.Consumer.N,
				              #Utility.Consumer.S = Utility.Consumer.S,
				              #CO2.Concentration = CO2.Concentration,
				              #Cost.S.Damage=Cost.S.Damage,
				              #policy.status = policy.status,
                      #Policy.Start.Time = policy.start.time*EndTime,
	                    #ce.tax.N=ce.tax.N,
				              #RD.subsidy.N=RD.subsidy.N,
				              #RD.subsidy.GF.N=RD.subsidy.GF.N,
				              #Tec.subsidy.N=Tec.subsidy.N,
				              #Tec.subsidy.GF.N=Tec.subsidy.GF.N,
				              #ce.tax.S=ce.tax.S,
				              #RD.subsidy.S=(RD.subsidy.S-RD.subsidy.GF.N),
				              #Tec.subsidy.S=(Tec.subsidy.S-Tec.subsidy.GF.N),
                      #Tec.subsidy.S=Tec.subsidy.S,
				              #Budget.function.N = Budget.function.N,
				              #Budget.function.S = Budget.function.S,
                      #Tests of new variables
                      #RelativeA.N=A_re.N/A_ce.N,
                      #RelativeA.S=A_re.S/A_ce.S,
                      #DecarbY.N=Y_ce.N/(Y_ce.N+Y_re.N),
                      #DecarbY.S=Y_ce.S/(Y_ce.S+Y_re.S),
                      L.S= L.S,
                      L.N = L.N,
                      Price.oil = Price.oil,
                      cR.N = cR.N,
                      cR.S = cR.S)
    return(vars.out)
  })
}


#Estimate dynamic path

Ediam<-function(end.time,time.step,y,Parameters,func)
{
#end.time<-43
#time.step<-1
#y<-InitialConditions
#Parameters<-Parameters
#func<-EdiamEquations

 times<-seq(0,end.time,time.step)

 State<-c(A_re = as.numeric(y['A_re']),
          A_ce = as.numeric(y['A_ce']),
          A_re.N = as.numeric(y['A_re.N']),
          A_ce.N = as.numeric(y['A_ce.N']),
          A_re.S = as.numeric(y['A_re.S']),
          A_ce.S = as.numeric(y['A_ce.S']),
          E_re.N = as.numeric(y['E_re.N']),
          E_ce.N = as.numeric(y['E_ce.N']),
          E_re.S = as.numeric(y['E_re.S']),
          E_ce.S = as.numeric(y['E_ce.S']),
          CO2.Concentration = as.numeric(y['CO2.Concentration']))

#Estimate Initial Conditions
 out<-func(times[1],State,Parameters)
#update price of final good in regions
 Parameters['Price.final.N']<-out$Price_Y.N
 Parameters['Price.final.S']<-out$Price_Y.S

for (i in 2:(end.time/time.step+1) )
{
  State<-c(A_re = out$A_re.t[i-1],
           A_ce = out$A_ce.t[i-1],
           A_re.N = out$A_re.N.t[i-1],
           A_ce.N = out$A_ce.N.t[i-1],
           A_re.S = out$A_re.S.t[i-1],
           A_ce.S = out$A_ce.S.t[i-1],
           E_re.N = out$E_re.N.t[i-1],
           E_ce.N = out$E_ce.N.t[i-1],
           E_re.S = out$E_re.S.t[i-1],
           E_ce.S = out$E_ce.S.t[i-1],
           CO2.Concentration = out$CO2.Concentration.t[i-1] )

  out.t<-func(times[i],State,Parameters)
#update price of final good in regions
  Parameters['Price.final.N']<-out.t$Price_Y.N
  Parameters['Price.final.S']<-out.t$Price_Y.S
#rbind period with dynamic path
  out<-rbind(out,out.t)
}
 out$times<-times
#remove not needed variables
  out$A_re.t1<-NULL
  out$A_ce.t1<-NULL
  out$A_re.N.t1<-NULL
  out$A_ce.N.t1<-NULL
  out$A_re.S.t1<-NULL
  out$A_ce.S.t1<-NULL
  out$E_re.N.t1<-NULL
  out$E_ce.N.t1<-NULL
  out$E_re.S.t1<-NULL
  out$E_ce.S.t1<-NULL
  out$CO2.Concentration.t1<-NULL

return(out)

}
