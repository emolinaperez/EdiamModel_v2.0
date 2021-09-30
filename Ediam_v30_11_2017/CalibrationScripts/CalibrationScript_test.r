#Test new model changes
#  library("deSolve")

#incloud
  dir.model<<-"C:\\Users\\Administrator\\Documents\\Edmundo\\Projectos\\TechChange-RDM\\TechChange Model\\Ediam_v30_11_2017\\"
  dir.data<-"C:\\Users\\Administrator\\Documents\\Edmundo\\Projectos\\TechChange-RDM\\TechChange Model\\Ediam_v30_11_2017\\CalibrationScripts\\CalibrationData\\"

#in pc
  dir.model<<-"C:\\Users\\L03054557\\Edmundo-ITESM\\Proyectos\\Disertation\\Model Disertation\\TechChange Model\\Ediam_v30_11_2017\\"
  dir.data<-"C:\\Users\\L03054557\\Edmundo-ITESM\\Proyectos\\Disertation\\Model Disertation\\TechChange Model\\Ediam_v30_11_2017\\CalibrationScripts\\CalibrationData\\"
#
 library(mFilter)
#Read data file
  HistData<<-read.csv(paste(dir.data,"HistData.csv",sep=""))

#subset the data to the years that we have data for
#   HistData<<-subset(HistData,HistData$Time%in%c(1995:2014))
   HistData<<-subset(HistData,HistData$Time%in%c(1971:2014))
   HistData$Y_ce.h<-HistData[,"TPES"]*HistData[,"FOSSILTPES"] #MTOE
   HistData$Y_re.h<-HistData[,"TPES"]*HistData[,"RENTPES"] #MTOE
#Create NonOECD region
  HistDataNonOECD<-subset(HistData,HistData$Country=="World")
  HistDataNonOECD$Country<-"NonOECD"
  HistDataNonOECD$CountryID<-"NonOECD"
  HistDataNonOECD$TPES<-HistDataNonOECD$TPES-HistData$TPES[HistData$Country=="Memo: OECD Total"]
  HistDataNonOECD$Y_ce.h<-HistDataNonOECD$Y_ce.h-HistData$Y_ce.h[HistData$Country=="Memo: OECD Total"]
  HistDataNonOECD$Y_re.h<-HistDataNonOECD$Y_re.h-HistData$Y_re.h[HistData$Country=="Memo: OECD Total"]
  HistDataNonOECD$GDP<-HistDataNonOECD$GDP-HistData$GDP[HistData$Country=="Memo: OECD Total"]
  HistDataNonOECD$POP<-HistDataNonOECD$POP-HistData$POP[HistData$Country=="Memo: OECD Total"]
  HistDataNonOECD$OILTPES<-HistDataNonOECD$OILTPES-HistData$OILTPES[HistData$Country=="Memo: OECD Total"]
#Rbind
  HistData<-rbind(HistData,HistDataNonOECD)
#Additional operations
   HistData$c_oil<-HistData[,"OILTPES"]/(HistData[,"TPES"]*HistData[,"FOSSILTPES"])#[1]
   HistData$Roil.h<-HistData[,"OILTPES"] #MTOE
   HistData$Re.h<-HistData$Roil.h/HistData$c_oil #MTOE
   HistData$Price.Oil<- HistData$Price.Oil*7.1428571428571*1e6/1e9 # (billion USD per Mtoe) ; Asumming that 1 toe= 7.1428571428571 boe
   HistData$ReToGDP.h<-(HistData$Re.h*HistData$Price.Oil)/(HistData$GDP) # [1]
#hp filter to oil prices
   HistData$Price.Oil.hp<-rep(as.numeric(hpfilter(subset(HistData[,"Price.Oil"],HistData[,"Country"]=="World"),freq=100)$trend),length(unique(HistData$Country)))

  ValVars<<-c("GDP.N","Y_ce.N","Y_re.N","GDP.S","Y_ce.S","Y_re.S")
  source(paste(dir.model,"ediam_HistoricCalib.r",sep=""))

ediamMSE<-function(x,verbose=FALSE){
#test mode
#using this
#x<-c(1.690737261,
#     1.761944876,
#     0.035644167,
#     0.005046794,
#     0.779497795,
#     0.510539272,
#     0.003407432,
#     0.004677823,
     #1.038849066,
#     0.005,
#     0.015
#     )


 calib.params<<-c(
                   epsilon.N = round(x[1],3),
                   epsilon.S = round(x[2],3),
                   Gamma_re =  round(x[3],3),
                   Gamma_ce=   round(x[4],3),
                   Eta_re.N =  round(x[5],3),
                   Eta_ce.N =  round(x[6],3),
                   Eta_re.S =  round(x[7],3),
                   Eta_ce.S =  round(x[8],3),
                   val.param = round(x[9],3),
                   val.param.S = round(x[10],3),
                   pi.N = 1.0, #round(x[10],3),
                   pi.S = 1.0, #round(x[11],3),
                   lrng.re = round(x[11],3),
                   lrng.ce = round(x[12],3), #,
                   lrng.re.S = round(x[13],3),
                   lrng.ce.S = round(x[14],3),
                   cf.re.N = 1.0, #round(x[9],3),
                   cf.ce.N = 1.0, #round(x[10],3),
                   cf.re.S = 1.0, #round(x[11],3),
                   cf.ce.S = 1.0, #round(x[12],3)
                   alfa.N = round(x[15],3),
                   alfa.S = round(x[16],3)
                   )


#historic data
#  Y_re.Nh <<-subset(HistData[,"Y_re.h"],HistData[,"Country"]=="Memo: OECD Total") #Mtoe
# Y_ce.Nh <<-subset(HistData[,"Y_ce.h"],HistData[,"Country"]=="Memo: OECD Total") #Mtoe
#  Y_re.Sh <<-subset(HistData[,"Y_re.h"],HistData[,"Country"]=="NonOECD") #Mtoe
#  Y_ce.Sh <<-subset(HistData[,"Y_ce.h"],HistData[,"Country"]=="NonOECD") #Mtoe

#historic data with hp
  Y_re.Nh <<-as.numeric(hpfilter(subset(HistData[,"Y_re.h"],HistData[,"Country"]=="Memo: OECD Total"),freq=100)$trend) #Mtoe
  Y_ce.Nh <<-as.numeric(hpfilter(subset(HistData[,"Y_ce.h"],HistData[,"Country"]=="Memo: OECD Total"),freq=100)$trend) #Mtoe
  Y_re.Sh <<-as.numeric(hpfilter(subset(HistData[,"Y_re.h"],HistData[,"Country"]=="NonOECD"),freq=100)$trend) #Mtoe
  Y_ce.Sh <<-as.numeric(hpfilter(subset(HistData[,"Y_ce.h"],HistData[,"Country"]=="NonOECD"),freq=100)$trend) #Mtoe


#Oil Supply
  Re.Nh<<-subset(HistData[,"Re.h"],HistData[,"Country"]=="Memo: OECD Total") # Mtoe
  Re.Sh<<-subset(HistData[,"Re.h"],HistData[,"Country"]=="NonOECD") #Mtoe
  ReToGDP.Nh<<-subset(HistData[,"ReToGDP.h"],HistData[,"Country"]=="Memo: OECD Total") # [1]
  ReToGDP.Sh<<-subset(HistData[,"ReToGDP.h"],HistData[,"Country"]=="NonOECD")

#Oil prices
   #Price.oil.y<<-subset(HistData[,"Price.Oil"],HistData[,"Country"]=="World")
   Price.oil.y<<- subset(HistData[,"Price.Oil.hp"],HistData[,"Country"]=="World")

#GDP
#    GDP.Nh<<-subset(HistData[,"GDP"],HistData[,"Country"]=="Memo: OECD Total") #(billion 2010 USD using exchange rates)
#    GDP.Sh<<-subset(HistData[,"GDP"],HistData[,"Country"]=="NonOECD") #(billion 2010 USD using exchange rates)

#GDP with hp
    GDP.Nh<<-as.numeric(hpfilter(subset(HistData[,"GDP"],HistData[,"Country"]=="Memo: OECD Total"),freq=100)$trend) #(billion 2010 USD using exchange rates)
    GDP.Sh<<-as.numeric(hpfilter(subset(HistData[,"GDP"],HistData[,"Country"]=="NonOECD"),freq=100)$trend) #(billion 2010 USD using exchange rates)


#Population
   L.N.y <<-subset(HistData[,"POP"],HistData[,"Country"]=="Memo: OECD Total") #millions
   L.S.y <<-subset(HistData[,"POP"],HistData[,"Country"]=="NonOECD") #millions

#Define initial conditions

#Simulate model
   SimulData<-ediamCalib(calib.params,dir.model,verbose=TRUE)
#Estimate mean square differences
   MSEp1<-((SimulData[,ValVars[1]]-GDP.Nh)/GDP.Nh)^2
   MSEp1.1<-((SimulData[,ValVars[2]]-Y_ce.Nh)/Y_ce.Nh)^2
   MSEp1.2<-((SimulData[,ValVars[3]]-Y_re.Nh)/Y_re.Nh)^2
   MSEp2<-((SimulData[,ValVars[4]]-GDP.Sh)/GDP.Sh)^2
   MSEp2.1<-((SimulData[,ValVars[5]]-Y_ce.Sh)/Y_ce.Sh)^2
   MSEp2.2<-((SimulData[,ValVars[6]]-Y_re.Sh)/Y_re.Sh)^2
#estimate mean MSE
   MSE<-mean(c(mean(MSEp1),mean(MSEp1.1),mean(MSEp1.2),mean(MSEp2),mean(MSEp2.1),mean(MSEp2.2)))
#check constraints
   tolerance<-0.011
   MSE<- ifelse(mean(MSEp1)<=tolerance,
               ifelse(mean(MSEp1.1)<=tolerance,
                       ifelse(mean(MSEp1.2)<=tolerance,
                                ifelse(mean(MSEp2)<=tolerance,
                                     ifelse(mean(MSEp2.1)<=tolerance,
                                          ifelse(mean(MSEp2.2)<=tolerance,MSE,1e3)
                                                                 ,1e3)
                                                        ,1e3)
                                                    ,1e3)
                                            ,1e3)
                                    ,1e3)

   #MSE<-max(c(mean(MSEp1),mean(MSEp1.1),mean(MSEp1.2),mean(MSEp2),mean(MSEp2.1),mean(MSEp2.2)))
  # MSE<-max(c(mean(MSEp1.1),mean(MSEp1.2),mean(MSEp2.1),mean(MSEp2.2)))
  out<-data.frame(v1 = mean(MSEp1),
                  v2 = mean(MSEp1.1),
                  v3 = mean(MSEp1.2),
                  v4 = mean(MSEp2),
                  v5 = mean(MSEp2.1),
                  v6 = mean(MSEp2.2),
                  v7 = MSE )

  colnames(out)<-paste("MSE",c(ValVars[1],ValVars[2],ValVars[3],ValVars[4],ValVars[5],ValVars[6],"all"),sep=".")
if (verbose == TRUE)
{ return(out)
  #return(SimulData)
} else {
        return(as.numeric(out$MSE.all))
       }

}

#Test
ediamMSE(x, verbose = TRUE)

#############################################################################################

#Optimization
 library(snow)
#  nCore<-40
  nCore<-16
#  nCore<-8
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("ediamMSE","ediamCalib","HistData","ValVars","dir.model","hpfilter")
  clusterExport(cl,global.elements,envir=environment())

library(rgenoud)
set.seed(55555)
genoud(ediamMSE,max=FALSE,
       #nvars=8,
      #  nvars=9,
      #nvars=10,
      # nvars=8,
    #   nvars=12,
    #   nvars=13,
      nvars=16,
       starting.values = c( 2.27,  #epsilon.N
                            2.91, #epsilon.S
                            0.024, #Gamma_re
                            0.006, #Gamma_ce
                            0.484, #Eta_re.N
                            0.64, #Eta_ce.N
                            0.052, #Eta_re.S
                            0.026, #, #Eta_ce.S
                            1.0,   #val.param
                            1.0,   #val.param.S
                        #    1.0, #pi.N
                        #    1.0 #pi.S
                             0.000 , #lrng.re
                             0.000 ,#, #lrng.ce
                             0.000 , #lrng.re.N
                             0.000 , #lrng.ce.S
                        #1.0, #cf.re.N
                        #1.0, #cf.ce.N
                        #1.0, #cf.ce.S
                        #1.0  #cf.re.S,
                        0.333, #alfa.N
                        0.333 #alfa.S
                          ),
       pop.size=10000,
       Domains=matrix(c(#inferior limits
                            1.5,  #epsilon.N
                            1.5, #epsilon.S
                            0.001, #Gamma_re
                            0.001, #Gamma_ce
                          #  0.5, #Eta_re.N
                          #  0.5, #Eta_ce.N
                            0.001, #Eta_re.N
                            0.001, #Eta_ce.N
                            0.001, #Eta_re.S
                            0.001, #Eta_ce.S
                            0.85, #val.param
                            0.85, #val.param.S
                        #    0.90, #pi.N
                        #    0.90, #pi.S
                            0.000 , #lrng.re
                            0.000 , #lrng.ce
                            0.000 , #lrng.re.S
                            0.000 , #lrng.ce.S,
                         #0.5, #cf.re.N
                         #0.5, #cf.ce.N
                         #0.5, #cf.ce.S
                         #0.5,  #cf.re.S
                         0.15, #alfa.N
                         0.15, #alfa.S
                        #superior limits
                            10,  #epsilon.N
                            10, #epsilon.S
                            0.3, #Gamma_re
                            0.3, #Gamma_ce
                            0.95, #Eta_re.N
                            0.95, #Eta_ce.N
                            0.95, #Eta_re.S
                            0.95, #Eta_ce.S
                          #  0.5, #Eta_re.S
                          #  0.5 , #Eta_ce.S
                            1.5 , #val.param
                            1.5 , #val.param.S
                        #    1.15, #pi.N
                        #    1.15 #, #pi.S
                           1.0 , #lrng.re
                           1.0 ,#, #lrng.ce
                           1.0 , #lrng.re.S
                           1.0 , #lrng.ce.S
                        #  1.0, #cf.re.N
                        #  1.0, #cf.ce.N
                        #  1.0, #cf.ce.S
                        #  1.0  #cf.re.S
                        0.5, #alfa.N
                        0.5 #alfa.S
                               ),
                            ncol=2),
       cluster=cl,
       #boundary.enforcement=2, # 0-> out of bounds individuals are valid, 2-> respect the bounds
       print.level=1)

stopCluster(cl)

# preliminary solution:
#with MSE<-max(c(mean(MSEp1),mean(MSEp1.1),mean(MSEp1.2),mean(MSEp2),mean(MSEp2.1),mean(MSEp2.2)))
x<-c( 1.308369984,
      2.583652772,
      0.085906537,
      0.011615490,
      0.785861985,
      0.949595114,
      0.005257706,
      0.008549853
    )
#
x<-c( 5.874427541,
      5.313916355,
      0.016068452,
      0.011489372,
      0.928709656,
      0.589898636,
      0.007651999,
      0.006903619
    )

# using mean of the six metrics, MSE -> 0.03

x<-c( 5.835751014,
      2.572445351,
      0.009581364,
      0.006735779,
      0.949508809,
      0.500001646,
      0.005747258,
      0.007586791
    )
# with four digit precision, and meand of six metrics, MSE-> 0.027;  does not really work,
x<-c( 2.018834513,
      2.472389055,
      0.015719916,
      0.007292539,
      1.347237041,
      0.730534302,
      0.038649978,
      0.013547548
    )

#with three digit precision, and mean of six metrics, with more ample range of variables, MSE-> 0.027
x<-c(1.952274182,
     1.758486111,
     0.016601666,
     0.007448338,
     0.948859835,
     0.612587071,
     0.012081008,
     0.010135533)
#with three digit precision, and mean of six metrics, with full ample range of variables, MSE-> 0.028

x<-c(2.076887035,
     1.942263490,
     0.015672427,
     0.007212712,
     0.947350939,
     0.623176194,
     0.012563009,
     0.009524876)

#with three digit precision, and mean of six metrics, with full ample range of variables, and parameters for utility curve, MSE-> 0.028

 x<-c(2.765008361,
      2.664990599,
      0.013747057,
      0.008295270,
      0.949886760,
      0.500018544,
      0.012661668,
      0.009493165,
      0.878047902
    )

# #with three digit precision,mean of six metrics, with full ample range of variables,parameters for utility curve, and alfas mean of time series, MSE-> 0.01899383
x<-c(2.099801708,
     1.801308302,
     0.013785963,
     0.006729991,
     0.949895956,
     0.500016685,
     0.009181968,
     0.009960502,
     0.850361491
     )
# #with three digit precision,mean of six metrics, with full ample range of variables,parameters for utility curve, and alfas mean of time series, MSE-> 0.01809197
x<-c(
    2.435035938,
    2.166027960,
    0.013088467,
    0.006155519,
    0.949883830,
    0.501322201,
    0.010770493,
    0.010407637,
    0.854119335,
    0.918747611,
    0.900473023
   )
#
x<-c(2.036596978,
     1.987457716,
     0.013551754,
     0.005526362,
     0.949888888,
     0.500006480,
     0.012003445,
     0.009559719,
     0.850184585,
     1.125609042,
     0.900362333
   )

#with expirience effects
x<-c(1.851035593,
     2.222008289,
     0.009048464,
     0.001052660,
     0.798287002,
     0.517277644,
     0.002455807,
     0.005602681,
     0.884596483,
     1.065165355,
     0.900427550
   )

# with new equations, MSE <- 0.01957496
x<-c(2.026083013,
     1.885898742,
     0.035785582,
     0.007512269,
     0.800296899,
     0.589545771,
     0.004771776,
     0.004716079,
     0.971937087
   )

# with new equations and fix learning effects, MSE <- 0.01918114
x<-c(1.690737261,
     1.761944876,
     0.035644167,
     0.005046794,
     0.779497795,
     0.510539272,
     0.003407432,
     0.004677823,
     #1.038849066,
     0.005,
     0.015
     )

#with subset of data, using hp on oil prices, MSE <- 0.009060982, the initial conditions of scientist work very well,
x<-c(2.661574286,
     3.024003416,
     0.034760373,
     0.008686765,
     0.503230476,
     0.500829660,
     0.012843348,
     0.01202013
   )
   ,
     0,
     0
   )

#with all data, MSE <- 0.01950367 , using hp on oil prices
x<-c( 1.586041455,
      1.500163073,
      0.035946370,
      0.007805154,
      0.830009031,
      0.505057891,
      0.004448388,
      0.005027390
    )

#
#with subset of data, using hp on oil prices and learning effects, MSE <- 0.009130603, the initial conditions of scientist work very well,
x<-c(2.320532701,
     2.617778986,
     0.030902945,
     0.005769407,
     0.503822554,
     0.502556862,
     0.011456857,
     0.011347857,
     0.004186601,
     0.006987290
     )

#with subset of data and with learning effects, using hp in all data,  MSE<-0.006904071
x<-c(2.01645869,
     2.07943367,
     0.07473788,
     0.04576528,
     0.54206899,
     0.50041786,
     0.03766725,
     0.01558345,
     -0.03816771,
     -0.04307069
   )

#
#with subset of data and no learning effects, using hp in all data,  MSE<-0.007123998
x<-c(2.708620317,
     3.072172398,
     0.027624473,
     0.007599719,
     0.594950238,
     0.500426955,
     0.014612121,
     0.012506037
    )

#with subset of data , no learning effects and only using the first 10 years for calib, using hp in all data, MSE<-0.005713424
x<-c(2.730083223,
     3.652614936,
     0.032080035,
     0.005901811,
     0.512197522,
     0.534714174,
     0.021259224,
     0.012158445
    )

#
#with subset of data , no learning effects and only using the first 5 years for calib, using hp in all data, MSE<- 0.005457215

x<-c(1.93183216,
     2.41318213,
     0.03289583,
     0.01029363,
    0.50019508,
    0.52265649,
    0.03892894,
    0.01305748
  )

#with subset of data , no learning effects, using the first 5 years for calib, and utility parameter, using hp in all data, MSE<- 0.005400138

x<-c(1.78567871,
     2.47630495,
     0.02697169,
     0.01331100,
     0.50323702,
     0.50620615,
     0.13221371,
     0.01349833,
     0.93674131
    )

##with subset of data , learning effects, using the first 5 years for calib, and utility parameter, using hp in all data, MSE<- 0.005390138
x<-c(1.693854662,
     1.818887312,
     0.017511237,
     -0.024222903,
     0.501465897,
     0.524979445,
     0.008283795,
     0.008209794,
     0.973485089,
     0.015586638,
     0.045274561
   )

#
##with subset of data , learning effects, using the first 5 years for calib, utility parameter and regional parameters, using hp in all data, MSE<- 0.004927148
x<-c(1.651887709,
     1.766585777,
     0.013509042,
     -0.054909157,
     0.559717785,
     0.500332775,
     0.008442410,
     0.003700389,
     0.850374062,
     0.952998071,
     0.902350935,
     0.014840359,
     0.073980410
   )

##with all data , learning effects, using the first 5 years for calib, utility parameter, using hp in all data, MSE<- 0.01752242
x<-c(
   1.673130226,
   1.759998384,
   0.034598033,
   0.004919573,
   0.717483537,
   0.562604209,
   0.003926619,
   0.004729008,
   0.937530085,
   0.005321617,
   0.005722687
  )

#
##with all data , learning effects, using the first 5 years for calib, utility parameter, regional parameters, using hp in all data, MSE<- 0.0163252
x<-c(1.555145967,
     1.559183136,
     0.033502447,
     0.005349919,
     0.765986709,
     0.527181985,
     0.005064022,
     0.005166904,
     1.066192855,
     0.918931425,
     0.900475480,
     0.003444086,
     0.005333684
    )

#
##with subset of data , learning complex effects, using the first 5 years for calib, utility parameter, regional parameters, using hp in all data, MSE<- 0.004697113
x<-c(2.270192340,
     3.901752490,
     0.001767026,
     -0.042595361,
     0.504520782,
     0.516452341,
     0.032431522,
     0.003835961,
     1.080635292,
     1.141288443,
     0.906599771,
     0.019856429,
     0.061075726,
     0.874249657,
     0.884933750
   )

##with subset of data , learning effects with 5 decimal points, using the first 5 years for calib, utility parameter, regional parameters, using hp in all data, MSE<- 0.00533322
x<-c( 2.5111682568,
      4.1886218902,
      0.0268541552,
      0.0068663746,
      0.5266202588,
      0.5059141979,
      0.0451236002,
      0.0115071087,
      0.8673278770,
      1.1240855348,
      0.9002710168,
      -0.0007031042,
      0.0034149150
    )

#with subset of data , no learning effects with 5 decimal points, using the first 5 years for calib, utility parameter, regional parameters, using hp in all data, MSE<- 0.005515412
# tolerance restrictions at 0.02
x<-c(2.396915170,
     2.325687653,
     0.041821309,
     0.007995445,
     0.500763091,
     0.648583709,
     0.013275226,
     0.013422702,
     0.855798601,
     1.106424623,
     0.900156393
    )

##with subset of data ,learning effects with 5 decimal points by region, using the first 5 years for calib, using hp in all data, MSE<- 0.005447599
# tolerance restrictions at 0.02

x<-c(3.1491629140,
     5.2279963006,
     0.0186293366,
     0.0080153104,
     0.5041075397,
     0.7661850681,
     0.0275000342,
     0.0115580472,
     0.0069783167,
     0.0001517864,
     0.0010334693,
     0.0029298448
   )
#
##with subset of data ,learning effects with 5 decimal points by region, using the first 5 years for calib, using hp in all data, MSE<- 0.004432355
# without tolerance restrictions, respecting boundaries and full open range
#very good with minimizing error, but scientist conditions are not good
x<-c(1.860615,
    1.127212,
    0.03186453,
    0.01208603,
    0.5,
    0.4697033,
    0.012,
    0.01250069,
    0.003387158,
    0.0001485483,
    0.003083108,
    0.00687424
  )

#very final runs for paper
##with subset of data,, using the first 5 years for calib, using hp in all data, MSE<-0.004856723
#respecting boundary conditions

x<-c(2.003084,
     2.57088,
     0.03734595,
     0.01125021,
     0.3523465,
     0.4068401,
     0.0429749,
     0.01425271
    )

##with subset of data, using the first 5 years for calib, using hp in all data, MSE<-0.004711125
#not respecting boundaries

x<-c(2.001532434,
     2.572292895,
     0.053719635,
     0.009242237,
     0.176025602,
     0.515600168,
     0.040811559,
     0.014083057
     )


#with learning effects 6 digits, learning effects by region  , MSE<-0.004640707
#respecting boundaries
x<-c(1.903168593,
     3.314407657,
     0.028480694,
     0.010555528,
     0.246288893,
     0.400459090,
     0.625778994,
     0.014418859,
     0.002595738,
     0.002151376,
     0.003216520,
     0.001009716
   )

# check this out

x<-c(2.661574286,
     3.024003416,
     0.034760373,
     0.008686765,
     0.503230476,
     0.500829660,
     0.012843348,
     0.01202013
   )

#with the very first equations
#with subet of data
#scientist conditions work very well
#MSE<-0.004397741 #the best so far
 x<-c(2.270999666,
      2.913543720,
      0.023822526,
      0.006168928,
      0.484651241,
      0.637886125,
      0.052447403,
      0.026364650
     )
#with entire data
#MSE<-0.01550248
x<-c(1.664458055,
     1.782509369,
     0.015172628,
     0.003557554,
     0.720154354,
     0.455488318,
     0.027413390,
     0.013057437
   )

#
#using the adjustment for costs
#MSE<-0.004258104
#population 10,000
x<-c(2.07353485,
    3.17407240,
    0.02576111,
    0.00551705,
    0.52622733,
    0.86807481,
    0.05873098,
    0.02451303,
    1.00000769,
    0.99999776,
    0.73181129,
    0.82963896
  )

#with utility parameter differenciated by region
#MSE<- 0.004342337
x<-c(2.2700000,
     2.9100000,
     0.0240000,
     0.0060000,
     0.6067142,
     0.9380724,
     0.0520000,
     0.0260000,
     0.9489913,
     1.0985971
   )
#with 10,000 pop
$value
[1] 0.004280484

$par
 [1] 2.27000000 2.89795900 0.02623258 0.00600000 0.62257084 0.77459096 0.05200000 0.02600000 1.18979276 1.49992492



#alfa parameters, subset of data, with alfa by region
#with 10,000 pop
#MSE<-0.004254538
x<-c(2.27000000,
     2.92492559,
     0.02571846,
     0.00600000,
     0.66104448,
     0.88082188,
     0.05200000,
     0.02600000,
     1.20210188,
     1.49992306,
     0.31991824,
     0.30724045
    )

#alfa parameters, all data
#with 10,000 pop
#MSE<-0.01488052

x<-c(1.627149819,
     1.814917316,
     0.012922016,
     0.004417407,
     0.310772866,
     0.660008525,
     0.031857169,
     0.019505122,
     1.145331810,
     1.024535191,
     0.327010721,
     0.205050104
   )

#alfa parameters, subset of data, learning Effects
#MSE<-0.004193859

x<-c(2.27000000,
     2.91000000,
     0.02453531,
     0.00600000,
     0.52577957,
     0.69132343,
     0.05418693,
     0.02600000,
     1.00000000,
     1.49895532,
     0.00000000,
     0.00000000,
     0.10126171,
     0.27304493,
     0.33193600,
     0.31014238
   )

#alfa parameters, subset of data, learning Effects
#MSE<-0.004193859
#tolerance<-0.011

x<-c(2.270008e+00,
     2.879429e+00,
     2.490752e-02,
     5.687152e-03,
     4.840018e-01,
     7.773862e-01,
     5.260620e-02 ,
     2.531791e-02 ,
     8.806384e-01 ,
     1.425176e+00 ,
     1.028267e-01 ,
     -8.921408e-07,
     1.384415e-01,
     6.128041e-01,
     3.371347e-01,
     3.223382e-01
   )
##alfa parameters, all data, learning Effects
#MSE<-0.01234146

x<-c(1.537581177,
     1.840202353,
     0.005459022,
     0.003417549,
     0.176664977,
     0.559690277,
     0.003501534,
     0.004122687,
     1.075556602,
     1.193103218,
     0.472986492,
     0.041228237,
     0.310514641,
     0.998759662,
     0.408460563,
     0.386325317
   )




source(paste(dir.model,"ediam_HistoricCalib.r",sep=""))
#first show MSE
ediamMSE(x, verbose = TRUE)
#Compare visually the fittness of the solution
#
calib.params<<-c(
                  epsilon.N = round(x[1],3),
                  epsilon.S = round(x[2],3),
                  Gamma_re =  round(x[3],3),
                  Gamma_ce=   round(x[4],3),
                  Eta_re.N =  round(x[5],3),
                  Eta_ce.N =  round(x[6],3),
                  Eta_re.S =  round(x[7],3),
                  Eta_ce.S =  round(x[8],3),
                  val.param = round(x[9],3),
                  val.param.S = round(x[10],3),
                  pi.N = 1.0, #round(x[10],3),
                  pi.S = 1.0, #round(x[11],3),
                  lrng.re = round(x[11],3),
                  lrng.ce = round(x[12],3), #,
                  lrng.re.S = round(x[13],3),
                  lrng.ce.S = round(x[14],3),
                  cf.re.N = 1.0, #round(x[9],3),
                  cf.ce.N = 1.0, #round(x[10],3),
                  cf.re.S = 1.0, #round(x[11],3),
                  cf.ce.S = 1.0, #round(x[12],3)
                  alfa.N = round(x[15],3),
                  alfa.S = round(x[16],3)
                  )

 SimulData<-ediamCalib(calib.params,dir.model,verbose=TRUE)
 SimulData$GDPpc.OECD<-SimulData$GDP.N/SimulData$L.N
 SimulData$GDPpc.NonOECD<-SimulData$GDP.S/SimulData$L.S


#design the effects of experience
#  v<-SimulData$Xtech_ce.N
#  Experience <- sapply(1:(length(v)),function(x){sum(v[1:x])})
#  lrng.re<-1.0 #range from 0 to 1.0
#  cost <- exp(-1* lrng.re* 1e-2 *(Experience-v[1])/v[1])

#Reshape simulated data into usable format for comparison
 times<-seq(min(HistData$Time),max(HistData$Time))
 SimulData<-data.frame( Time = rep(times,11),
            Value = c(SimulData[,"GDP.N"],SimulData[,"GDP.S"], SimulData[,"Y_re.N"], SimulData[,"Y_ce.N"],SimulData[,"Y_re.S"],SimulData[,"Y_ce.S"],
                      SimulData[,"L.N"],SimulData[,"L.S"],SimulData[,"Price.oil"],SimulData[,"GDPpc.OECD"],SimulData[,"GDPpc.NonOECD"]),
            Variable= rep(c("GDP.OECD","GDP.NonOECD","Y_re.OECD","Y_ce.OECD","Y_re.NonOECD","Y_ce.NonOECD",
                            "POP.OECD","POP.NonOECD","Price.Oil","GDPpc.OECD","GDPpc.NonOECD"),each=length(times)),
            Data.type="Simulation")

#Reshape historcial data into usable format for comparison
 HistoricData.hp<-data.frame( Time = rep(times,11),
            Value = c(hpfilter(HistData[HistData$Country=="Memo: OECD Total","GDP"],freq=100)$trend,
                      hpfilter(HistData[HistData$Country=="NonOECD","GDP"],freq=100)$trend,
                      hpfilter(HistData[HistData$Country=="Memo: OECD Total","Y_re.h"],freq=100)$trend,
                      hpfilter(HistData[HistData$Country=="Memo: OECD Total","Y_ce.h"],freq=100)$trend,
                      hpfilter(HistData[HistData$Country=="NonOECD","Y_re.h"],freq=100)$trend,
                      hpfilter(HistData[HistData$Country=="NonOECD","Y_ce.h"],freq=100)$trend,
                      hpfilter(HistData[HistData$Country=="Memo: OECD Total","POP"],freq=100)$trend,
                      hpfilter(HistData[HistData$Country=="NonOECD","POP"],freq=100)$trend,
                      hpfilter(HistData[HistData$Country=="Memo: OECD Total","Price.Oil"],freq=100)$trend,
                      hpfilter(HistData[HistData$Country=="Memo: OECD Total","GDP"]/HistData[HistData$Country=="Memo: OECD Total","POP"],freq=100)$trend,
                      hpfilter(HistData[HistData$Country=="NonOECD","GDP"]/HistData[HistData$Country=="NonOECD","POP"],freq=100)$trend),
            Variable= rep(c("GDP.OECD","GDP.NonOECD","Y_re.OECD","Y_ce.OECD","Y_re.NonOECD","Y_ce.NonOECD",
                           "POP.OECD","POP.NonOECD","Price.Oil","GDPpc.OECD","GDPpc.NonOECD"),each=length(times)),
            Data.type="Historic")

#use hp on all historic data
HistoricData<-data.frame( Time = rep(times,11),
           Value = c(HistData[HistData$Country=="Memo: OECD Total","GDP"],
                     HistData[HistData$Country=="NonOECD","GDP"],
                     HistData[HistData$Country=="Memo: OECD Total","Y_re.h"],
                     HistData[HistData$Country=="Memo: OECD Total","Y_ce.h"],
                     HistData[HistData$Country=="NonOECD","Y_re.h"],
                     HistData[HistData$Country=="NonOECD","Y_ce.h"],
                     HistData[HistData$Country=="Memo: OECD Total","POP"],
                     HistData[HistData$Country=="NonOECD","POP"],
                     HistData[HistData$Country=="Memo: OECD Total","Price.Oil"],
                     HistData[HistData$Country=="Memo: OECD Total","GDP"]/HistData[HistData$Country=="Memo: OECD Total","POP"],
                     HistData[HistData$Country=="NonOECD","GDP"]/HistData[HistData$Country=="NonOECD","POP"]),
           Variable= rep(c("GDP.OECD","GDP.NonOECD","Y_re.OECD","Y_ce.OECD","Y_re.NonOECD","Y_ce.NonOECD",
                          "POP.OECD","POP.NonOECD","Price.Oil","GDPpc.OECD","GDPpc.NonOECD"),each=length(times)),
           Data.type="Historic")

#Calibration fitness
# CalibFitness<-rbind(SimulData,HistoricData)
 CalibFitness<-rbind(SimulData,HistoricData.hp)
library(ggplot2)
#GDP
 calib.plot <- ggplot(data=subset(CalibFitness,CalibFitness$Variable%in%c("GDP.OECD","GDP.NonOECD")), aes(x=Time, y=Value, linetype = Data.type))
 calib.plot + geom_line()+scale_linetype_manual(values=c(2,1)) + facet_wrap(~ Variable)

#GDP per capita
  calib.plot <- ggplot(data=subset(CalibFitness,CalibFitness$Variable%in%c("GDPpc.OECD","GDPpc.NonOECD")), aes(x=Time, y=Value, linetype = Data.type))
  calib.plot + geom_line()+scale_linetype_manual(values=c(2,1)) + facet_wrap(~ Variable)

#FOSSILTPES
calib.plot <- ggplot(data=subset(CalibFitness,CalibFitness$Variable%in%c("Y_ce.OECD","Y_ce.NonOECD")), aes(x=Time, y=Value, linetype = Data.type))
calib.plot + geom_line()+scale_linetype_manual(values=c(2,1)) + facet_wrap(~ Variable)

#RENTPES
calib.plot <- ggplot(data=subset(CalibFitness,CalibFitness$Variable%in%c("Y_re.OECD","Y_re.NonOECD")), aes(x=Time, y=Value, linetype = Data.type))
calib.plot + geom_line()+scale_linetype_manual(values=c(2,1)) + facet_wrap(~ Variable)


#notes for tomorrow
  # 1) check why I cannot subset the time series
  # 2) check how the errors look if you see GDP per capita, energy primary supply in rates-> errors look the same
  # 3) check how the oil and population plots look like -it may be necessary to filter the oil data series-

#next steps
  # try calibration with the utility parameter
  # try calibration with learning effects
  # try calibration with tolerance value

#then,it is done


# Population
 calib.plot <- ggplot(data=subset(CalibFitness,CalibFitness$Variable%in%c("POP.OECD","POP.NonOECD")), aes(x=Time, y=Value, linetype = Data.type))
 calib.plot + geom_line()+scale_linetype_manual(values=c(2,1)) + facet_wrap(~ Variable)
#oil price
 calib.plot <- ggplot(data=subset(CalibFitness,CalibFitness$Variable=="Price.Oil"), aes(x=Time, y=Value, linetype = Data.type))
 calib.plot + geom_line()+scale_linetype_manual(values=c(2,1)) + facet_wrap(~ Variable)
# hp oil price
 library(mFilter)
 Price.oil.hp<- hpfilter(subset(HistData[,"Price.Oil"],HistData[,"Country"]=="World"),freq=50)
 Price.oil.hp<-data.frame(Time=c(1971:2014),Price.oil.hp=as.numeric(Price.oil.hp$trend))
 calib.plot <- ggplot(data=Price.oil.hp, aes(x=Time, y=Price.oil.hp))
 calib.plot + geom_line()


#Show transition plot

#OECD
 TPES.OECD<-subset(SimulData,SimulData$Variable%in%c("Y_re.OECD","Y_ce.OECD"))
 TPES.OECD<-aggregate(Value ~ Time, data = TPES.OECD, FUN = sum)
 TPES.OECD$Region<-'OECD'

#NonOECD
 TPES.NonOECD<-subset(SimulData,SimulData$Variable%in%c("Y_re.NonOECD","Y_ce.NonOECD"))
 TPES.NonOECD<-aggregate(Value ~ Time, data = TPES.NonOECD, FUN = sum)
 TPES.NonOECD$Region<-'NonOECD'

#rbind
 Data<-rbind(TPES.OECD,TPES.NonOECD)
 Data$Type<-'Simulation'

#Prepare Historic Data
 TPES.Hist<-subset(HistData,HistData$Country%in%c("Memo: OECD Total","NonOECD"))
 TPES.Hist$Value<-(TPES.Hist$FOSSILTPES+TPES.Hist$RENTPES)*TPES.Hist$TPES
 TPES.Hist$Region<-ifelse(TPES.Hist$Country=='Memo: OECD Total','OECD','NonOECD')
 TPES.Hist$Type<-'Historic'
 TPES.Hist<-TPES.Hist[,c('Time','Region','Value','Type')]

#rbind all
 Data<-rbind(Data,TPES.Hist)

calib.plot <- ggplot(data=Data, aes(x=Time, y=Value, colour = Region))
calib.plot + geom_line() + facet_wrap(~ Type)


#designing the value function of entrepreneurs
 SimulData$Profits_re.N
 SimulData$Profits_ce.N

 x<-c(SimulData$Profits_re.N,SimulData$Profits_ce.N)
 x<-c(1:1000)
#type 1

 Value.t1<-data.frame(x=x)
 #Value.t1$Util<-log(0.2*Value.t1$x)
 Value.t1$Util<-log(Value.t1$x^1.1)
 Value.t1$type<-"type1"
#
#type 2
 Value.t2<-data.frame(x=x)
 #Value.t2$Util<-log(0.4*Value.t2$x)
 Value.t2$Util<-log(Value.t2$x^1.2)
 Value.t2$type<-"type2"
#
#type 3
 Value.t3<-data.frame(x=x)
 #Value.t3$Util<-log(0.6*Value.t3$x)
 Value.t3$Util<-log(Value.t3$x^1.3)
 Value.t3$type<-"type3"
#
#type 4
 Value.t4<-data.frame(x=x)
 #Value.t4$Util<-log(0.8*Value.t4$x)
 Value.t4$Util<-log(Value.t4$x^1.4)
 Value.t4$type<-"type4"
#
#type 5
 Value.t5<-data.frame(x=x)
 #Value.t5$Util<-log(1.0*Value.t5$x)
 Value.t5$Util<-log(Value.t5$x^1.5)
 Value.t5$type<-"type5"
#
#type 6
 Value.t6<-data.frame(x=x)
 #Value.t6$Util<-log(1.2*Value.t6$x)
 Value.t6$Util<-log(Value.t6$x^1.6)
 Value.t6$type<-"type6"
#
#type 7
 Value.t7<-data.frame(x=x)
 #Value.t7$Util<-log(1.4*Value.t7$x)
 Value.t7$Util<-log(Value.t7$x^1.7)
 Value.t7$type<-"type7"
#
#type 8
 Value.t8<-data.frame(x=x)
 #Value.t8$Util<-log(1.6*Value.t8$x)
 Value.t8$Util<-log(Value.t8$x^1.8)
 Value.t8$type<-"type8"
#
#type 9
 Value.t9<-data.frame(x=x)
 #Value.t9$Util<-log(1.8*Value.t9$x)
 Value.t9$Util<-log(Value.t9$x^1.9)
 Value.t9$type<-"type9"
#
#type 10
 Value.t10<-data.frame(x=x)
# Value.t10$Util<-log(2.0*Value.t10$x)
 Value.t10$Util<-log(Value.t10$x^1.10)
 Value.t10$type<-"type10"

#

Value.S<-rbind(Value.t1,Value.t2,Value.t3,Value.t4,Value.t5,Value.t6,Value.t7,Value.t8,Value.t9,Value.t10)

ggplot(data=Value.S, aes(x=x, y=Util,linetype=type))+geom_line()

#now let´s simulate this

  test<-data.frame( RE=SimulData$Profits_re.N,
                    CE=SimulData$Profits_ce.N,
                    Value.RE=log((SimulData$Profits_re.N/SimulData$Profits_re.N[1])^2.5),
                    Value.CE=log((SimulData$Profits_ce.N/SimulData$Profits_re.N[1])^2.5)
                  )
 test$s.re<- exp(test$Value.RE)/(exp(test$Value.CE)+exp(test$Value.CE))



#error analysis

library(lhs)
set.seed(5000)
# sample.size<-100000
sample.size<-1000
params.names<-c("epsilon.N",
                "epsilon.S",
                "Gamma_re",
                "Gamma_ce",
                "Eta_re.N",
                "Eta_ce.N",
                "Eta_re.S",
                "Eta_ce.S")
#
lhs.sample<-data.frame(randomLHS(sample.size,length(params.names)),Run.ID=1:sample.size)
colnames(lhs.sample)<-c(params.names,"Run.ID")
#Define range of values
lhs.sample$epsilon.N<-qunif(lhs.sample$epsilon.N,1.5,10)
lhs.sample$epsilon.S<-qunif(lhs.sample$epsilon.S,1.5,10)
lhs.sample$Gamma_re<-qunif(lhs.sample$Gamma_re,0.001,0.2)
lhs.sample$Gamma_ce<-qunif(lhs.sample$Gamma_ce,0.001,0.2)
lhs.sample$Eta_re.N<-qunif(lhs.sample$Eta_re.N,0.5,0.95)
lhs.sample$Eta_ce.N<-qunif(lhs.sample$Eta_ce.N,0.5,0.95)
lhs.sample$Eta_re.S<-qunif(lhs.sample$Eta_re.S,0.001,0.10)
lhs.sample$Eta_ce.S<-qunif(lhs.sample$Eta_re.S,0.001,0.10)
#

#multi-core runs
library(snow)
#  nCore<-40
 nCore<-8
 cl <- makeSOCKcluster(names = rep('localhost',nCore))
 global.elements<-list("ediamMSE","ediamCalib","HistData","ValVars","dir.model")
 clusterExport(cl,global.elements,envir=environment())

 test.out<- parApply(cl,lhs.sample,1,function(x) {out<-ediamMSE(c( as.numeric(x['epsilon.N']),
                                                                   as.numeric(x['epsilon.S']),
                                                                   as.numeric(x['Gamma_re']),
                                                                   as.numeric(x['Gamma_ce']),
                                                                   as.numeric(x['Eta_re.N']),
                                                                   as.numeric(x['Eta_ce.N']),
                                                                   as.numeric(x['Eta_re.S']),
                                                                   as.numeric(x['Eta_ce.S'])
                                                                 ),verbose=TRUE);
                                                 out$Run.ID<-as.numeric(x['Run.ID'])
                                       return(out)})
#
 stopCluster(cl)

 test.out<-do.call("rbind",test.out)

#merge with parametrs
dim(test.out)
dim(lhs.sample)
 test.out<-merge(test.out,lhs.sample, by = "Run.ID")
dim(test.out)

test.out<-test.out[order(test.out$MSE.all ),]
head(test.out)
