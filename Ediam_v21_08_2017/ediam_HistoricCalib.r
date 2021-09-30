## =======================================================================================================================================================================================================
## Exploratory Dynamic Integreated Assessment Model
## =======================================================================================================================================================================================================
ediamCalib<-function (calib.params, dir.model, verbose=FALSE)
{
#==========================================================================================================================================================================================================
#Define policy parameters
#==========================================================================================================================================================================================================
 policy.vector<-c(
    #Carbon tax
      ce.tax.N=0.0,
      ce.tax.S=0.0,
    #Technology push in North
      Tec.subsidy.N = 0.0,
      RD.subsidy.N = 0.0,
    #Traditional Green Climate Fund
      Tec.subsidy.S = 0.0,
      Tec.subsidy.GF.N = 0.0,
   #R&D Green Climate Fund
      RD.subsidy.S = 0.0,
      RD.subsidy.GF.N = 0.0,
      policy.half.life = 0.0 )
#==========================================================================================================================================================================================================

#==========================================================================================================================================================================================================
#Define model parameters
#==========================================================================================================================================================================================================
 Parameters <- c(Run.ID = 1, #as.numeric(params['Run.ID'])
                 EndTime = 32, #as.numeric(params['EndTime'])
                 TimeStep = 1, #as.numeric(params['TimeStep'])
                #economic parameters North
                 pi.N = as.numeric(calib.params['pi.N']),
                 alfa.N = as.numeric(calib.params['alfa.N']),
                 alfa_1.N = as.numeric(calib.params['alfa_1.N'])*as.numeric(calib.params['alfa.N']), #a scalar with respect to alfa
                 alfa_2.N = as.numeric(calib.params['alfa.N'])*(1-as.numeric(calib.params['alfa_1.N'])),
                 epsilon.N = as.numeric(calib.params['epsilon.N']),
                #economic parameters south
                 pi.S = as.numeric(calib.params['pi.S']),
                 alfa.S = as.numeric(calib.params['alfa.S']), #alfa.N
                 alfa_1.S = as.numeric(calib.params['alfa_1.S'])*as.numeric(calib.params['alfa.S']), # alfa_1.S
                 alfa_2.S = as.numeric(calib.params['alfa.S'])*(1-as.numeric(calib.params['alfa_1.S'])), # alfa_2.S
                 epsilon.S = as.numeric(calib.params['epsilon.S']),
                #Technological paramters
                 Gamma_re = as.numeric(calib.params['Gamma_re']),# 0.2 as.numeric(params['Gamma_re']),
                 Gamma_ce = as.numeric(calib.params['Gamma_ce']),# 0.2 as.numeric(params['Gamma_ce']),
                 Eta_re= as.numeric(calib.params['Eta_re']),#as.numeric(params['Eta_re']),
                 Eta_ce= as.numeric(calib.params['Eta_ce']),#as.numeric(params['Eta_ce']),
                 Nu_re = as.numeric(calib.params['Nu_re']),#as.numeric(params['Nu_re']),
                 Nu_ce= as.numeric(calib.params['Nu_ce']),#as.numeric(params['Nu_ce']),
                 #others
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
                 Y_0.N = Y_0.N,
                 Y_re0.N = Y_re0.N, #as.numeric(params['Yre.0_N']),
                 Y_ce0.N = Y_ce0.N, #as.numeric(params['Yce.0_N'])
                 Y_0.S = Y_0.S,
                 Y_re0.S = Y_re0.S, #as.numeric(params['Yre.0_S'])
                 Y_ce0.S = Y_ce0.S, #as.numeric(params['Yce.0_S'])
                 CO2.Concentration_0 = 382.2461 #as.numeric(params['CO2.Concentration_0'])
                )
#Add additional parameters
  Parameters<-c(Parameters,
                phi.N = (1-as.numeric(Parameters['alfa.N']))*(1-as.numeric(Parameters['epsilon.N'])),
                phi_1.N = (1-as.numeric(Parameters['alfa_1.N']))*(1-as.numeric(Parameters['epsilon.N'])),
                phi.S = (1-as.numeric(Parameters['alfa.S']))*(1-as.numeric(Parameters['epsilon.S'])),
                phi_1.S = (1-as.numeric(Parameters['alfa_1.S']))*(1-as.numeric(Parameters['epsilon.S'])),
                epsi.N = as.numeric(Parameters['alfa.N'])^2,
                epsi.S = as.numeric(Parameters['alfa.S'])^2,
                cR_0 = 1.0,
                L_0.N = L.N.y[1],
                L_0.S = L.S.y[1],
                policy.vector)

#==========================================================================================================================================================================================================

#==========================================================================================================================================================================================================
#Define Initial Conditions
#==========================================================================================================================================================================================================

#Load Initial Conditions Function
 source(paste(dir.model,"ediam_InitialConditions.r",sep=""))

#Determine initial conditions
 InitialConditions <-ediamInitialConditions( as.numeric(Parameters['pi.N']),
                                             as.numeric(Parameters['alfa.N']),
                                             as.numeric(Parameters['alfa_1.N']),
                                             as.numeric(Parameters['alfa_2.N']),
                                             as.numeric(Parameters['phi.N']),
                                             as.numeric(Parameters['phi_1.N']),
                                             as.numeric(Parameters['epsilon.N']),
                                             as.numeric(Parameters['epsi.N']),
                                             as.numeric(Parameters['pi.S']),
                                             as.numeric(Parameters['alfa.S']),
                                             as.numeric(Parameters['alfa_1.S']),
                                             as.numeric(Parameters['alfa_2.S']),
                                             as.numeric(Parameters['phi.S']),
                                             as.numeric(Parameters['phi_1.S']),
                                             as.numeric(Parameters['epsilon.S']),
                                             as.numeric(Parameters['epsi.S']),
                                             as.numeric(Parameters['L_0.N']),
                                             as.numeric(Parameters['L_0.S']),
                                             as.numeric(Parameters['Y_0.N']),
                                             as.numeric(Parameters['Y_0.S']),
                                             as.numeric(Parameters['Y_re0.N']),
                                             as.numeric(Parameters['Y_ce0.N']),
                                             as.numeric(Parameters['Y_re0.S']),
                                             as.numeric(Parameters['Y_ce0.S']),
                                             as.numeric(Parameters['cR_0']),
                                             as.numeric(Parameters['CO2.Concentration_0']),
                                             type="wExhaustableResource"
                                            )
#==========================================================================================================================================================================================================
#Load EDIAM Differential Equations Structure and Execute Simulation
#==========================================================================================================================================================================================================

#Load EDIAM Differential Equations
 source(paste(dir.model,"ediam_Equations.r",sep=""))

#Load Data Series
#for Oil
 cR.Data <<- approxfun( x = seq(0,32,1), #times
                       y = cR.y, # Actual data points
                       method = "linear",
                       rule = 2)
#for Population
 L.N.Data<<-approxfun( x = seq(0,32,1), #times
                       y = L.N.y, # Actual data points
                       method = "linear",
                       rule = 2)
#
 L.S.Data<<-approxfun( x = seq(0,32,1), #times
                      y = L.S.y, # Actual data points
                      method = "linear",
                      rule = 2)
#Run the model
 out <- as.data.frame(
                      ode(y = InitialConditions,
                          times = seq(0,as.numeric(Parameters['EndTime']),as.numeric(Parameters['TimeStep'])),
                          func = EdiamEquations,
                          parms = Parameters,
                          method ="rk4" ))

#==========================================================================================================================================================================================================
# Post-processing data and validation of constraints
#==========================================================================================================================================================================================================

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
 out$Run.ID<-as.numeric(Parameters['Run.ID'])
#set social function value
 out$Social.Welfare.Function<-Social.Welfare.Function
if (verbose==FALSE) {
                      return(Social.Welfare.Function)
                    } else {
                      return(out)
                           }
 }
