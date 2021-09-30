#==========================================================================================================================================================================================================
#Define Initial Conditions
#==========================================================================================================================================================================================================
#subindices are undersores "_", supraindices are periods ".", subindices go before periods
ediamInitialConditions<-function(pi.N,alfa.N,alfa_1.N,alfa_2.N,phi.N,phi_1.N,epsilon.N,epsi.N,
                                 pi.S,alfa.S,alfa_1.S,alfa_2.S,phi.S,phi_1.S,epsilon.S,epsi.S,
                                 L_0.N,L_0.S,
                                 Y_0.N,Y_0.S,
                                 Y_re0.N,Y_ce0.N,Y_re0.S,Y_ce0.S,cR_0,CO2.Concentration_0,
                                 type="wExhaustableResource")
{

#test mode
#Determine initial conditions
#pi.N<-as.numeric(Parameters['pi.N'])
#alfa.N<-as.numeric(Parameters['alfa.N'])
#alfa_1.N<-as.numeric(Parameters['alfa_1.N'])
#alfa_2.N<-as.numeric(Parameters['alfa_2.N'])
#phi.N<-as.numeric(Parameters['phi.N'])
#phi_1.N<-as.numeric(Parameters['phi_1.N'])
#epsilon.N<-as.numeric(Parameters['epsilon.N'])
#epsi.N<-as.numeric(Parameters['epsi.N'])
#pi.S<-as.numeric(Parameters['pi.S'])
#alfa.S<-as.numeric(Parameters['alfa.S'])
#alfa_1.S<-as.numeric(Parameters['alfa_1.S'])
#alfa_2.S<-as.numeric(Parameters['alfa_2.S'])
#phi.S<-as.numeric(Parameters['phi.S'])
#phi_1.S<-as.numeric(Parameters['phi_1.S'])
#epsilon.S<-as.numeric(Parameters['epsilon.S'])
#epsi.S<-as.numeric(Parameters['epsi.S'])
#L_0.N<-as.numeric(Parameters['L_0.N'])
#L_0.S<-as.numeric(Parameters['L_0.S'])
#Y_0.N<-as.numeric(Parameters['Y_0.N'])
#Y_0.S<-as.numeric(Parameters['Y_0.S'])
#Y_re0.N<-as.numeric(Parameters['Y_re0.N'])
#Y_ce0.N<-as.numeric(Parameters['Y_ce0.N'])
#Y_re0.S<-as.numeric(Parameters['Y_re0.S'])
#Y_ce0.S<-as.numeric(Parameters['Y_ce0.S'])
#cR_0<-as.numeric(Parameters['cR_0'])
#CO2.Concentration_0<-as.numeric(Parameters['CO2.Concentration_0'])



#
if (type=="wExhaustableResource")
 {
#
#Initial Productivity conditions are determined by the initial levels of production of energy and initial prices of the exhaustable resource
#In the advanced region
  teta.N<-( ( epsi.N^(alfa_1.N+alfa_2.N*alfa.N+alfa_2.N*phi.N) * ( alfa.N^(-2*alfa.N*(phi.N+alfa.N)) ) )^(1/(1-alfa.N)) )/
        ( ( (alfa_1.N^(2*alfa_1.N))*(alfa_2.N^alfa_2.N) )^(epsilon.N) )

#Review the equationa below, something is not right

#Binding condition for CES function
 Y_re0.N.B<-( ( (Y_0.N^((epsilon.N-1)/epsilon.N))-(Y_ce0.N^((epsilon.N-1)/epsilon.N)) )^( epsilon.N/(epsilon.N-1) ) )
 #Y_re0.N.B<-( ( ( (Y_0.N^((epsilon.N-1)/epsilon.N))-(1-pi.N)*(Y_ce0.N^((epsilon.N-1)/epsilon.N)) )/pi.N )^( epsilon.N/(epsilon.N-1) ) )
#
 A_ce0.N <- ( ( Y_re0.N.B * ( (alfa.N^(2*alfa.N*(1-epsilon.N)))*((Y_re0.N/(teta.N*Y_ce0.N))^(phi.N/(epsilon.N*(1-alfa.N)))) +
                        ((epsi.N^alfa_2.N)*(alfa_2.N^alfa_2.N)*(alfa_1.N^(2*alfa_1.N)))^(1-epsilon.N) )^((alfa.N+phi.N)/phi.N)
             ) /
            ( L_0.N *
                     ((((cR_0^(-1*alfa_2.N*epsilon.N))*Y_re0.N)/(teta.N*Y_ce0.N))^(1/(epsilon.N*(1-alfa.N)))) *
                     (((epsi.N^alfa_2.N)*(alfa_1.N^(2*alfa_1.N))*(alfa_2.N^alfa_2.N))^((1/(1-alfa.N))-epsilon.N))
             )
           ) ^ ( phi.N/phi_1.N )



  A_re0.N<- ( ( ( (cR_0^(-1*alfa_2.N*epsilon.N))*Y_re0.N) /
               ( teta.N*Y_ce0.N )
              )^(1/(epsilon.N*(1-alfa.N))) ) *
           ( (A_ce0.N)^((1-alfa_1.N)/(1-alfa.N)) )

#In the Emerging Region
  teta.S<-( ( epsi.S^(alfa_1.S+alfa_2.S*alfa.S+alfa_2.S*phi.S) * ( alfa.S^(-2*alfa.S*(phi.S+alfa.S)) ) )^(1/(1-alfa.S)) )/
      ( ( (alfa_1.S^(2*alfa_1.S))*(alfa_2.S^alfa_2.S) )^(epsilon.S) )

#

#Binding condition for CES function
 Y_re0.S.B<-( ( (Y_0.S^((epsilon.S-1)/epsilon.S))-(Y_ce0.S^((epsilon.S-1)/epsilon.S)) )^( epsilon.S/(epsilon.S-1) ) )
 #Y_re0.S.B<-( ( ( (Y_0.S^((epsilon.S-1)/epsilon.S))-(1-pi.S)*(Y_ce0.S^((epsilon.S-1)/epsilon.S)) )/pi.S )^( epsilon.S/(epsilon.S-1) ) )
#
 A_ce0.S <- ( ( Y_re0.S.B * ( (alfa.S^(2*alfa.S*(1-epsilon.S)))*((Y_re0.S/(teta.S*Y_ce0.S))^(phi.S/(epsilon.S*(1-alfa.S)))) +
                        ((epsi.S^alfa_2.S)*(alfa_2.S^alfa_2.S)*(alfa_1.S^(2*alfa_1.S)))^(1-epsilon.S) )^((alfa.S+phi.S)/phi.S)
             ) /
            ( L_0.S *
                     ((((cR_0^(-1*alfa_2.S*epsilon.S))*Y_re0.S)/(teta.S*Y_ce0.S))^(1/(epsilon.S*(1-alfa.S)))) *
                     (((epsi.S^alfa_2.S)*(alfa_1.S^(2*alfa_1.S))*(alfa_2.S^alfa_2.S))^((1/(1-alfa.S))-epsilon.S))
             )
           ) ^ ( phi.S/phi_1.S )



  A_re0.S<- ( ( ( (cR_0^(-1*alfa_2.S*epsilon.S))*Y_re0.S) /
               ( teta.S*Y_ce0.S )
              )^(1/(epsilon.S*(1-alfa.S))) ) *
           ( (A_ce0.S)^((1-alfa_1.S)/(1-alfa.S)) )

#Define vector of intital conditions
  InitialConditions <- c(A_re.N = A_re0.N,
                         A_ce.N = A_ce0.N,
					               A_re.S = A_re0.S,
					               A_ce.S = A_ce0.S,
                         CO2.Concentration= CO2.Concentration_0)

  return(InitialConditions)
  } else {
  #In the Advaced Region
#     A_ce0.N<-((Y_ce0.N^((epsilon-1)/epsilon)+Y_re0.N^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_ce0.N/Y_re0.N)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
#     A_re0.N<-((Y_ce0.N^((epsilon-1)/epsilon)+Y_re0.N^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_re0.N/Y_ce0.N)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))

  #In the Emerging Region
#      A_ce0.S<-(1/L_0.S)*((Y_ce0.S^((epsilon-1)/epsilon)+Y_re0.S^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_ce0.S/Y_re0.S)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
#      A_re0.S<-(1/L_0.S)*((Y_ce0.S^((epsilon-1)/epsilon)+Y_re0.S^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_re0.S/Y_ce0.S)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
  #
#  InitialConditions <- c(A_re.N = A_re0.N,
#                         A_ce.N = A_ce0.N,
	#				               A_re.S = A_re0.S,
		#			               A_ce.S = A_ce0.S,
#                         CO2.Concentration= CO2.Concentration_0)
#   return(InitialConditions)
          }
}
