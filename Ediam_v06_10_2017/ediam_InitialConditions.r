#==========================================================================================================================================================================================================
#Define Initial Conditions
#==========================================================================================================================================================================================================
#subindices are undersores "_", supraindices are periods ".", subindices go before periods
ediamInitialConditions<-function(Parameters)
{
#Determine initial conditions
 alfa.N<-as.numeric(Parameters['alfa.N'])
 phi.N<-as.numeric(Parameters['phi.N'])
 epsilon.N<-as.numeric(Parameters['epsilon.N'])
 alfa.S<-as.numeric(Parameters['alfa.S'])
 phi.S<-as.numeric(Parameters['phi.S'])
 epsilon.S<-as.numeric(Parameters['epsilon.S'])

#Data
 Y_re0.N <<- Y_re.Nh[1]
 Y_ce0.N <<- Y_ce.Nh[1]
 Y_re0.S <<- Y_re.Sh[1]
 Y_ce0.S <<- Y_ce.Sh[1]
 Price.oil0<<-Price.oil.y[1]
 ReToGDP0.N<<-ReToGDP.Nh[1]
 ReToGDP0.S<<-ReToGDP.Sh[1]
 GDP0.N <<- GDP.Nh[1]
 GDP0.S <<- GDP.Sh[1]
 Re0.N <<- Re.Nh[1]
 Re0.S <<- Re.Sh[1]
 L_0.N <<- L.N.y[1]
 L_0.S <<- L.S.y[1]

 CO2.Concentration_0<-as.numeric(Parameters['CO2.Concentration_0'])

#
#Initial Productivity conditions are determined by the initial levels of production of energy and initial prices of the exhaustable resource

#Initial Conditions-Advanced Region-

#first estimate alfa 2
  a.alfa_2.N<-1
  b.alfa_2.N<-( (1/ReToGDP0.N) +1 - 2*alfa.N )
  c.alfa_2.N<-( alfa.N^2 - 1 )*(
                                (Y_re0.N/Y_ce0.N)^( (epsilon.N-1)/epsilon.N ) + 1
                                )

  alfa_2.N<-(-1*b.alfa_2.N + ( (b.alfa_2.N^2)-4*a.alfa_2.N*c.alfa_2.N)^0.5 )/(2*a.alfa_2.N)
#second estimate alfa 1
  alfa_1.N<-alfa.N-alfa_2.N
#third estimate price of final good "Y"
  Price_Y0.N<-(1/alfa_2.N) * Price.oil0 * ( Re0.N/Y_ce0.N) * ( (Y_re0.N/Y_ce0.N)^( (epsilon.N-1)/epsilon.N ) + 1 )^(1/(1-epsilon.N))
#fourth initial costs of technology and auxiliary
  epsi_re.N<-alfa.N^2
  epsi_ce.N<-alfa_1.N^2
  phi_1.N<-(1-alfa_1.N)*(1-epsilon.N)
#fifth productivity carbon sector
  A_ce0.N<-( L_0.N^((1-alfa.N)/(alfa_1.N-1)) ) *
           ( Y_ce0.N^((1-alfa.N)/(1-alfa_1.N)) ) *
           ( (epsi_ce.N/(alfa_1.N^2))^(alfa_1.N/(1-alfa_1.N)) ) *
           ( (Price.oil0/(Price_Y0.N*alfa_2.N))^(alfa_2.N/(1-alfa_1.N)) ) *
           ( ( (Y_re0.N/Y_ce0.N)^( (epsilon.N-1)/epsilon.N ) + 1 )^( (alfa.N+phi.N)/phi_1.N ) )
#sixth productivity renewable sector
  A_re0.N<- (
              ( ( (epsi_re.N^alfa.N) * (alfa_1.N^(2*alfa_1.N))* (alfa_2.N^alfa_2.N)* (A_ce0.N^(1-alfa_1.N)) )/( (epsi_ce.N^alfa_1.N)*(alfa.N^(2*alfa.N)) ) ) *
             ( ( Price_Y0.N/Price.oil0 )^alfa_2.N ) *
             ( (Y_re0.N/Y_ce0.N)^ (1/epsilon.N) )
            )^(1/(1-alfa.N))

 #Initial Conditions-Emerging Region-

 #first estimate alfa 2
   a.alfa_2.S<-1
   b.alfa_2.S<-( (1/ReToGDP0.S) +1 - 2*alfa.S )
   c.alfa_2.S<-( alfa.S^2 - 1 )*(
                                 (Y_re0.S/Y_ce0.S)^( (epsilon.S-1)/epsilon.S ) + 1
                                 )

   alfa_2.S<-(-1*b.alfa_2.S + ( (b.alfa_2.S^2)-4*a.alfa_2.S*c.alfa_2.S)^0.5 )/(2*a.alfa_2.S)
 #second estimate alfa 1
   alfa_1.S<-alfa.S-alfa_2.S
 #third estimate price of final good "Y"
   Price_Y0.S<-(1/alfa_2.S) * Price.oil0 * ( Re0.S/Y_ce0.S) * ( (Y_re0.S/Y_ce0.S)^( (epsilon.S-1)/epsilon.S ) + 1 )^(1/(1-epsilon.S))
 #fourth initial costs of technology and auxiliary
   epsi_re.S<-alfa.S^2
   epsi_ce.S<-alfa_1.S^2
   phi_1.S<-(1-alfa_1.S)*(1-epsilon.S)
 #fifth productivity carbon sector
   A_ce0.S<-( L_0.S^((1-alfa.S)/(alfa_1.S-1)) ) *
            ( Y_ce0.S^((1-alfa.S)/(1-alfa_1.S)) ) *
            ( (epsi_ce.S/(alfa_1.S^2))^(alfa_1.S/(1-alfa_1.S)) ) *
            ( (Price.oil0/(Price_Y0.S*alfa_2.S))^(alfa_2.S/(1-alfa_1.S)) ) *
            ( ( (Y_re0.S/Y_ce0.S)^( (epsilon.S-1)/epsilon.S ) + 1 )^( (alfa.S+phi.S)/phi_1.S ) )
 #sixth productivity renewable sector
   A_re0.S<- (
               ( ( (epsi_re.S^alfa.S) * (alfa_1.S^(2*alfa_1.S))* (alfa_2.S^alfa_2.S)* (A_ce0.S^(1-alfa_1.S)) )/( (epsi_ce.S^alfa_1.S)*(alfa.S^(2*alfa.S)) ) ) *
              ( ( Price_Y0.S/Price.oil0 )^alfa_2.S ) *
              ( (Y_re0.S/Y_ce0.S)^ (1/epsilon.S) )
             )^(1/(1-alfa.S))


#In the advanced region
# teta.N<-(
#         (  ( epsi_ce.N^alfa_1.N ) * ( alfa.N^(2*alfa.N) ) )/
#         ( ( epsi_re.N^alfa.N ) * (alfa_1.N^(2*alfa_1.N)) * (alfa_2.N^alfa_2.N) )
#         )^ ( epsilon.N )

# A_ce0.N <- ( ( Y_re0.N *
#                (
#                 (alfa.N^(2*alfa.N*(1-epsilon.N))) * (epsi_ce.N^(alfa_1.N*(1-epsilon.N))) * ((Y_re0.N/(teta.N*Y_ce0.N))^(phi.N/(epsilon.N*(1-alfa.N)))) +
#                 ( (epsi_re.N^alfa.N) * (alfa_2.N^alfa_2.N) * (alfa_1.N^(2*alfa_1.N)) )^(1-epsilon.N)
#                 )^((alfa.N+phi.N)/phi.N)
#             ) /
#            ( L_0.N *
#                     ((((cR_0^(-1*alfa_2.N*epsilon.N))*Y_re0.N)/(teta.N*Y_ce0.N))^(1/(epsilon.N*(1-alfa.N)))) *
#                     (epsi_re.N^(-alfa.N*epsilon.N)) *
#                     (alfa.N^(2*alfa.N/(1-alfa.N))) *
#                     (((alfa_1.N^(2*alfa_1.N))*(alfa_2.N^alfa_2.N))^((1/(1-alfa.N))-epsilon.N))
#             )
#           ) ^ ( phi.N/phi_1.N )
#


#  A_re0.N<- ( ( ( (cR_0^(-1*alfa_2.N*epsilon.N))*Y_re0.N) /
#               ( teta.N*Y_ce0.N )
#              )^(1/(epsilon.N*(1-alfa.N))) ) *
#           ( (A_ce0.N)^((1-alfa_1.N)/(1-alfa.N)) )

#In the Emerging Region
##
#teta.S<-(
#        (  ( epsi_ce.S^alfa_1.S ) * ( alfa.S^(2*alfa.S) ) )/
#        ( ( epsi_re.S^alfa.S ) * (alfa_1.S^(2*alfa_1.S)) * (alfa_2.S^alfa_2.S) )
#        )^ ( epsilon.S )

#A_ce0.S <- ( ( Y_re0.S * ( (alfa.S^(2*alfa.S*(1-epsilon.S)))*(epsi_ce.S^(alfa_1.S*(1-epsilon.S)))*((Y_re0.S/(teta.S*Y_ce0.S))^(phi.S/(epsilon.S*(1-alfa.S)))) +
#                       ((epsi_re.S^alfa.S)*(alfa_2.S^alfa_2.S)*(alfa_1.S^(2*alfa_1.S)))^(1-epsilon.S) )^((alfa.S+phi.S)/phi.S)
#            ) /
#           ( L_0.S *
#                    ((((cR_0^(-1*alfa_2.S*epsilon.S))*Y_re0.S)/(teta.S*Y_ce0.S))^(1/(epsilon.S*(1-alfa.S)))) *
#                    (epsi_re.S^(-alfa.S*epsilon.S)) *
#                    (alfa.S^(2*alfa.S/(1-alfa.S))) *
#                    (((alfa_1.S^(2*alfa_1.S))*(alfa_2.S^alfa_2.S))^((1/(1-alfa.S))-epsilon.S))
#            )
#          ) ^ ( phi.S/phi_1.S )

# A_re0.S<- ( ( ( (cR_0^(-1*alfa_2.S*epsilon.S))*Y_re0.S) /
#              ( teta.S*Y_ce0.S )
#             )^(1/(epsilon.S*(1-alfa.S))) ) *
#          ( (A_ce0.S)^((1-alfa_1.S)/(1-alfa.S)) )


#Define vector of intital conditions
  InitialConditions <-list(
                        StateVariables = c( A_re = max(A_re0.N,A_re0.S),
                                           A_ce = max(A_ce0.N,A_ce0.S),
                                           A_re.N = A_re0.N,
                                           A_ce.N = A_ce0.N,
                                           A_re.S = A_re0.S,
                                           A_ce.S = A_ce0.S,
                                           CO2.Concentration= CO2.Concentration_0),
                        Parameters = c(
                                          alfa_1.N = alfa_1.N,
                                          alfa_2.N = alfa_2.N,
                                          epsi_re.N = epsi_re.N,
                                          epsi_ce.N = epsi_ce.N,
                                          phi_1.N = phi_1.N,
                                          Price.final.N = Price_Y0.N,
                                          alfa_1.S = alfa_1.S,
                                          alfa_2.S = alfa_2.S,
                                          Price.final.S = Price_Y0.S,
                                          epsi_re.S = epsi_re.S,
                                          epsi_ce.S = epsi_ce.S,
                                          phi_1.N = phi_1.S)
                       )
  return(InitialConditions)
}
