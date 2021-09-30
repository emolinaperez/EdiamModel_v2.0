ediamEmpiricalParameters<-function(Parameters,t)
{
#deal with epsilon first
epsilon.N_0 <- as.numeric(Parameters['epsilon.N'])
epsilon.N  <- epsilon.N_0 #*(1+0.005*(t-1)) #

epsilon.S_0 <- as.numeric(Parameters['epsilon.S'])
epsilon.S  <- epsilon.S_0 #*(1+0.005*(t-1)) #

#Determine initial conditions
alfa.N<-as.numeric(Parameters['alfa.N'])
alfa.S<-as.numeric(Parameters['alfa.S'])
 phi.N <- (1-as.numeric(Parameters['alfa.N']))*(1-epsilon.N)
 phi.S <- (1-as.numeric(Parameters['alfa.S']))*(1-epsilon.S)

 #epsilon.N<-as.numeric(Parameters['epsilon.N'])

 #epsilon.S<-as.numeric(Parameters['epsilon.S'])


#Data
 Y_re0.N <<- Y_re.Nh[t]
 Y_ce0.N <<- Y_ce.Nh[t]
 Y_re0.S <<- Y_re.Sh[t]
 Y_ce0.S <<- Y_ce.Sh[t]
 Price.oil0<<-Price.oil.y[t]
 ReToGDP0.N<<-ReToGDP.Nh[t]
 ReToGDP0.S<<-ReToGDP.Sh[t]
 GDP0.N <<- GDP.Nh[t]
 GDP0.S <<- GDP.Sh[t]
 Re0.N <<- Re.Nh[t]
 Re0.S <<- Re.Sh[t]
 L_0.N <<- L.N.y[t]
 L_0.S <<- L.S.y[t]
 #erase after testing
 pi.N<-1
 pi.S<-1

#estimate all known factors:
#north
  Y_0.N<-(Y_re0.N^((epsilon.N-1)/epsilon.N)+Y_ce0.N^((epsilon.N-1)/epsilon.N))^(epsilon.N/(epsilon.N-1))
  Price_ce.N<-((Y_re0.N/Y_ce0.N)^((epsilon.N-1)/epsilon.N)+1)^(1/(epsilon.N-1))
  Price_re.N<-( (Y_re0.N/Y_ce0.N)^(-1/epsilon.N) )* Price_ce.N

#Empirical parameters-Advanced Region-
 a.alfa_2.N<-1
 b.alfa_2.N<--1*((GDP0.N/Y_0.N)*((Y_ce0.N*(Price_ce.N^epsilon.N))/(Price.oil0*Re0.N ))+2*alfa.N+1)
 c.alfa_2.N<-(1- ( (Price_re.N^(1-epsilon.N)) + (Price_ce.N^(1-epsilon.N)) ) *alfa.N^2)/(Price_ce.N^(1-epsilon.N))
 alfa_2.N<-(-1*b.alfa_2.N - ( (b.alfa_2.N^2)-4*a.alfa_2.N*c.alfa_2.N)^0.5 )/(2*a.alfa_2.N)
#
#second estimate alfa 1
  alfa_1.N<-alfa.N-alfa_2.N
#third estimate price of final good "Y"
  Price_Y0.N<-(1/alfa_2.N) * Price.oil0 * ( Re0.N/Y_ce0.N) * ( (Y_re0.N/Y_ce0.N)^( (epsilon.N-1)/epsilon.N ) + 1 )^(1/(1-epsilon.N))

#============================
#old approach
#first estimate alfa 2
#  a.alfa_2.N<-1
#  b.alfa_2.N<-( (1/ReToGDP0.N) +1 - 2*alfa.N )
#  c.alfa_2.N<-( alfa.N^2 - pi.N )*(
#                                (Y_re0.N/Y_ce0.N)^( (epsilon.N-1)/epsilon.N ) + 1
#                                )

#  alfa_2.N<-(-1*b.alfa_2.N + ( (b.alfa_2.N^2)-4*a.alfa_2.N*c.alfa_2.N)^0.5 )/(2*a.alfa_2.N)
#second estimate alfa 1
#  alfa_1.N<-alfa.N-alfa_2.N
#third estimate price of final good "Y"
#  Price_Y0.N<-(1/alfa_2.N) * Price.oil0 * ( Re0.N/Y_ce0.N) * ( (Y_re0.N/Y_ce0.N)^( (epsilon.N-1)/epsilon.N ) + 1 )^(1/(1-epsilon.N))
#============================


#fourth initial costs of technology and auxiliary
  epsi_re.N<-alfa.N^2
  epsi_ce.N<-alfa_1.N^2
  phi_1.N<-(1-alfa_1.N)*(1-epsilon.N)


#Empirical parameters-Emerging Region-
  Y_0.S<-(Y_re0.S^((epsilon.S-1)/epsilon.S)+Y_ce0.S^((epsilon.S-1)/epsilon.S))^(epsilon.S/(epsilon.S-1))
  Price_ce.S<-( ((Y_re0.S/Y_ce0.S)^((epsilon.S-1)/epsilon.S)+1)^(1/(epsilon.S-1)) )
  Price_re.S<-( (Y_re0.S/Y_ce0.S)^(-1/epsilon.S) )* Price_ce.S

  a.alfa_2.S<-1
  b.alfa_2.S<--1*((GDP0.S/Y_0.S)*((Y_ce0.S*(Price_ce.S^epsilon.S))/(Price.oil0*Re0.S ))+2*alfa.S+1)
  c.alfa_2.S<-(1- ( (Price_re.S^(1-epsilon.S)) + (Price_ce.S^(1-epsilon.S)) ) *alfa.S^2)/(Price_ce.S^(1-epsilon.S))

  alfa_2.S<-(-1*b.alfa_2.S - ( (b.alfa_2.S^2)-4*a.alfa_2.S*c.alfa_2.S)^0.5 )/(2*a.alfa_2.S)
#
#============================
#old approah
#first estimate alfa 2
#  a.alfa_2.S<-1
#  b.alfa_2.S<-( (1/ReToGDP0.S) +1 - 2*alfa.S )
#  c.alfa_2.S<-( alfa.S^2 - pi.S )*(
#                                (Y_re0.S/Y_ce0.S)^( (epsilon.S-1)/epsilon.S ) + 1
#                                )

#  alfa_2.S<-(-1*b.alfa_2.S + ( (b.alfa_2.S^2)-4*a.alfa_2.S*c.alfa_2.S)^0.5 )/(2*a.alfa_2.S)
#second estimate alfa 1
#  alfa_1.S<-alfa.S-alfa_2.S
#third estimate price of final good "Y"
#  Price_Y0.S<-(1/alfa_2.S) * Price.oil0 * ( Re0.S/Y_ce0.S) * ( (Y_re0.S/Y_ce0.S)^( (epsilon.S-1)/epsilon.S ) + 1 )^(1/(1-epsilon.S))
#=================================

#
 #second estimate alfa 1
   alfa_1.S<-alfa.S-alfa_2.S
 #third estimate price of final good "Y"
   Price_Y0.S<-(1/alfa_2.S) * Price.oil0 * ( Re0.S/Y_ce0.S) * ( (Y_re0.S/Y_ce0.S)^( (epsilon.S-1)/epsilon.S ) + 1 )^(1/(1-epsilon.S))
 #fourth initial costs of technology and auxiliary
   epsi_re.S<-alfa.S^2
   epsi_ce.S<-alfa_1.S^2
   phi_1.S<-(1-alfa_1.S)*(1-epsilon.S)

#return values
EmpiricalParams <- data.frame(
                              alfa.N = alfa.N,
                              alfa.S = alfa.S,
                              alfa_1.N = alfa_1.N,
                              alfa_2.N = alfa_2.N,
                              epsi_re0.N = epsi_re.N,
                              epsi_ce0.N = epsi_ce.N,
                              phi_1.N = phi_1.N,
                              Price.final.N = Price_Y0.N,
                              alfa_1.S = alfa_1.S,
                              alfa_2.S = alfa_2.S,
                              Price.final.S = Price_Y0.S,
                              epsi_re0.S = epsi_re.S,
                              epsi_ce0.S = epsi_ce.S,
                              phi_1.S = phi_1.S,
                              Y_0.N = Y_0.N,
                              Y_0.S =Y_0.S,
                              epsilon.N=epsilon.N,
                              epsilon.S=epsilon.S,
                              phi.N=phi.N,
                              phi.S = phi.S
                            )
return(EmpiricalParams)

}
