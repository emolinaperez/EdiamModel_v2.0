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
                 alfa.N = as.numeric(1/3),
                 epsilon.N = as.numeric(calib.params['epsilon.N']),
                #economic parameters south
                 alfa.S = as.numeric(1/3), #alfa.N
                 epsilon.S = as.numeric(calib.params['epsilon.S']),
                #Technological paramters
                 Gamma_re = as.numeric(calib.params['Gamma_re']),# 0.2 as.numeric(params['Gamma_re']),
                 Gamma_ce = as.numeric(calib.params['Gamma_ce']),# 0.2 as.numeric(params['Gamma_ce']),
                 Eta_re.N= as.numeric(calib.params['Eta_re.N']),#as.numeric(params['Eta_re']),
                 Eta_ce.N= as.numeric(calib.params['Eta_ce.N']),#as.numeric(params['Eta_ce']),
                 Eta_re.S = as.numeric(calib.params['Eta_re.S']),#as.numeric(params['Nu_re']),
                 Eta_ce.S= as.numeric(calib.params['Eta_ce.S']),#as.numeric(params['Nu_ce']),
                 #others
                 qsi= as.numeric(0.0100539),#as.numeric(params['qsi']),
                 Delta.S = 0.001822767,#as.numeric(params['Delta.S']),
				         Delta.Temp.Disaster = as.numeric(6.0) ,#as.numeric(params['Delta.Temp.Disaster']),
				         Beta.Delta.Temp = 5.0 ,#as.numeric(params['Beta.Delta.Temp']),
				         CO2.base = 289.415,#as.numeric(params['CO2.base']),
				         rho = 0.008,#as.numeric(params['rho']),
				         lambda.S = 0.1443,#as.numeric(params['lambda.S']),
				         sigma.utility = 2.0,#as.numeric(params['sigma.utility']),
                 CO2.Concentration_0 = 382.2461 #as.numeric(params['CO2.Concentration_0'])
                )
#Add additional parameters
  Parameters<-c(Parameters,
                phi.N = (1-as.numeric(Parameters['alfa.N']))*(1-as.numeric(Parameters['epsilon.N'])),
                phi.S = (1-as.numeric(Parameters['alfa.S']))*(1-as.numeric(Parameters['epsilon.S'])),
#                epsi_re.N = as.numeric(Parameters['alfa.N'])^2,
#                epsi_ce.N = as.numeric(Parameters['alfa_1.N'])^2,
#                epsi_re.S = as.numeric(Parameters['alfa.S'])^2,
#                epsi_ce.S = as.numeric(Parameters['alfa_1.S'])^2,
                 policy.vector)

#==========================================================================================================================================================================================================

#==========================================================================================================================================================================================================
#Define Initial Conditions
#==========================================================================================================================================================================================================

#Load Initial Conditions Function
 source(paste(dir.model,"ediam_InitialConditions.r",sep=""))

#Determine initial conditions and parameters
 InitialConditions <-ediamInitialConditions(Parameters)

#Add additional parameters
 Parameters<-c(Parameters,InitialConditions$Parameters)

#Initial conditions for state variables
 InitialConditions <- InitialConditions$StateVariables

#==========================================================================================================================================================================================================
#Load EDIAM Differential Equations Structure and Execute Simulation
#==========================================================================================================================================================================================================

#Load EDIAM Equations
 source(paste(dir.model,"ediam_Equations_discrete_test.r",sep=""))

#Load Data Series
#for Oil
 Price.oil.Data <<- approxfun( x = seq(0,43,1), #times
                       y = Price.oil.y, # Actual data points
                       method = "linear",
                       rule = 2)
#for final good
 Price_Y.N.Data <<- approxfun( x = seq(0,43,1), #times
                      y = Price.oil.y, # Actual data points
                      method = "linear",
                      rule = 2)

#for Population
 L.N.Data<<-approxfun( x = seq(0,43,1), #times
                       y = L.N.y, # Actual data points
                       method = "linear",
                       rule = 2)
#
 L.S.Data<<-approxfun( x = seq(0,43,1), #times
                      y = L.S.y, # Actual data points
                      method = "linear",
                      rule = 2)
#Run the model
 out <- Ediam(end.time = 43 ,
              time.step = 1,
              y = InitialConditions,
              func = EdiamEquations,
              Parameters = Parameters)

#==========================================================================================================================================================================================================
# Post-processing data and validation of constraints
#==========================================================================================================================================================================================================

#Define objetive function
# Social.Welfare.Function<-sum(as.numeric(out$Utility.Consumer_N))+sum(as.numeric(out$Utility.Consumer_S))

#Check constraints are valid
#Consumption constraint -In no year consumption can be negative-
# Social.Welfare.Function<-ifelse(min(out$Consumption.N)<0,-1000000,
#                                 ifelse(min(out$Consumption.S)<0,-1000000,Social.Welfare.Function))

#Budget constraint -the cost of policies cannot exceed total budget-
# Social.Welfare.Function<-ifelse(sum(out$Budget.function.N)<0,-1000000,
#                                 ifelse(sum(out$Budget.function.S)<0,-1000000,Social.Welfare.Function))

#Calculate consumption growth rate for both regions
# out$Growth.Rate_N<-c(NA,diff(out$Consumption.N)/out$Consumption.N[1:(length(out$Consumption.N)-1)])
# out$Growth.Rate_S<-c(NA,diff(out$Consumption.S)/out$Consumption.S[1:(length(out$Consumption.S)-1)])
#set Run.ID
# out$Run.ID<-as.numeric(Parameters['Run.ID'])
#set social function value
# out$Social.Welfare.Function<-Social.Welfare.Function
#if (verbose==FALSE) {
#                      return(Social.Welfare.Function)
#                    } else {
                      return(out)
#                           }
 }
