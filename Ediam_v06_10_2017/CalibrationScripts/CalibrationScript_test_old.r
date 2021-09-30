#Test new model changes
#  library("deSolve")

#incloud
  dir.model<<-"C:\\Users\\Administrator\\Documents\\Edmundo\\Projectos\\TechChange-RDM\\TechChange Model\\Ediam_v06_10_2017\\"
  dir.data<-"C:\\Users\\Administrator\\Documents\\Edmundo\\Projectos\\TechChange-RDM\\TechChange Model\\Ediam_v06_10_2017\\CalibrationScripts\\CalibrationData\\"
#in pc
  dir.model<<-"C:\\Users\\L03054557\\Edmundo-ITESM\\Proyectos\\Disertation\\Model Disertation\\TechChange Model\\Ediam_v06_10_2017\\"
  dir.data<-"C:\\Users\\L03054557\\Edmundo-ITESM\\Proyectos\\Disertation\\Model Disertation\\TechChange Model\\Ediam_v06_10_2017\\CalibrationScripts\\CalibrationData\\"
#

  HistData<<-read.csv(paste(dir.data,"WorldEnergyIndicators2016_OECD_2016.csv",sep=""))
  HistPrice<<-read.csv(paste(dir.data,"historic_energy_prices.csv",sep="")) # in 2010 USD

#
#make some changes
  colnames(HistData)[1]<-"CountryID"
#subset to some countries only
  target.countries<-c("World",
                      "Africa",
                      "Middle East",
                      "Asia (excluding China)",
                      "China (P.R. of China and Hong Kong, China)",
                      "Memo: G7",
                      "Memo: G8",
                      "Memo: G20",
                      "Memo: European Union-28",
                      "Memo: OECD Total",
                      "OECD Asia Oceania",
                      "OECD Americas",
                      "OECD Europe")

#subset to some targets only
 target.metrics<-c("Total primary energy supply (TPES) (Mtoe)", #TPES
                  "GDP (billion 2010 USD using exchange rates)", #GDP
                  "GDP (billion 2010 USD using PPPs)", #GDPPPP
                  "Population (millions)", #POP
                  "TPES/GDP (toe per thousand 2010 USD)", #TPESGDP
                  "TPES/GDP (toe per thousand 2010 USD PPP)", #TPESGDPPPP
                  "Oil supply (Mtoe)", #OILTPES
                  "Oil supply/GDP (toe per thousand 2010 USD)",
                  "Oil supply/GDP (toe per thousand 2010 USD PPP)",
                  "TPES/GDP (toe per thousand 2010 USD)",
                  "Share of fossil in TPES",
                  "Share of renewable sources in TPES") #RENTPES


#subset to the target metrics and regions
  HistData<-subset(HistData,HistData$Country%in%target.countries)
  HistData<-subset(HistData,HistData$Flow%in%target.metrics)
# test<-subset(HistData,HistData$Country=="Memo: OECD Total")

  library(reshape2)
   HistData<-dcast(HistData[,c("CountryID","Country","FLOW","Time","Value")],
                CountryID + Country + Time ~ FLOW, value.var= "Value")

# Now load data on oil consumption
#Dir data
  oil.data<-read.csv(paste(dir.data,"WBAL-2016-1-EN-20161119T103155.csv",sep=""))

#subset to some countries only
  target.countries<-c("World",
                      "Africa",
                      "Middle East",
                      "Asia (excluding China)",
                      "China (P.R. of China and Hong Kong, China)",
                      "Memo: G7",
                      "Memo: G8",
                      "Memo: G20",
                      "Memo: European Union-28",
                      "Memo: OECD Total",
                      "OECD Asia Oceania",
                      "OECD Americas",
                      "OECD Europe")

#subset to some targets only
  target.metrics<-c("Coal and coal products",
                    "Natural gas",
                    "Memo: Primary and secondary oil")

#first subset to primary energy supply
  oil.data<-subset(oil.data,oil.data$Flow=='Total primary energy supply')
#subset to the target metrics and regions
  oil.data<-subset(oil.data,oil.data$Country%in%target.countries)
  oil.data<-subset(oil.data,oil.data$Product%in%target.metrics)
#subset to ktoe
  oil.data<-subset(oil.data,oil.data$Unit=="ktoe")
#re-shape file
  oil.data<-dcast(oil.data[,c("Country","PRODUCT","Time","Value")],
                  Country + Time ~ PRODUCT, value.var= "Value")
#
#target.data<-dcast(target.data[,c("Country","Product","Time","Value")],
#             Country + Time ~ Product, value.var= "Value")

#subset the data to the years that we have data for
  #oil.data<-subset(oil.data,oil.data$Time%in%c(1971:2014))

#Now let's merge all data together
 class(HistData$Country)
 class(oil.data$Country)
 class(HistData$Time)
 class(oil.data$Time)

#change clases
  HistData$Country<-as.character(HistData$Country)
  oil.data$Country<-as.character(oil.data$Country)

#check that merges checks
  unique(oil.data$Country%in%HistData$Country)
  unique(HistData$Country%in%oil.data$Country)
  unique(oil.data$Time%in%HistData$Time)
  unique(HistData$Time%in%oil.data$Time)

#  HistData<-Reduce(function(...) { merge(..., all=TRUE) }, list(HistData,HistPrice))
  HistData<-Reduce(function(...) { merge(..., all=TRUE) }, list(HistData,HistPrice))
  HistData<-HistData[order(HistData$Country),]

#write joint table
 write.csv(HistData,paste(dir.data,"HistData.csv",sep=""),row.names=FALSE)

#Read data file
  HistData<<-read.csv(paste(dir.data,"HistData.csv",sep=""))

#subset the data to the years that we have data for
   HistData<-subset(HistData,HistData$Time%in%c(1971:2014))
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
   #HistData$Price.Oil<- HistData$Price.Oil*7.1428571428571*1e6/(1000*1e6) # BillionUSD/MTOE; Asumming that 1 toe= 7.1428571428571 boe
   #HistData$Price.Oil<- HistData$Price.Oil*7.1428571428571/1000 # (thousand 1000 USD per toe)

#  HistData$Price.Oil<- HistData$Price.Oil*1e9 # (USD per billion of barrel)
#  HistData$Re.h<-HistData$Re.h*7.1428571428571/1000 # (billion of barrel) ; Asumming that 1 toe= 7.1428571428571 boe
#  HistData$ReToGDP.h<-(HistData$Re.h*HistData$Price.Oil)/(HistData$GDP*1e9)

  HistData$Price.Oil<- HistData$Price.Oil*7.1428571428571*1e6/1e9 # (billion USD per Mtoe) ; Asumming that 1 toe= 7.1428571428571 boe
  HistData$ReToGDP.h<-(HistData$Re.h*HistData$Price.Oil)/(HistData$GDP) # [1]

   #HistData$RoilToGDP.h<-HistData[,"OILSUPGDP"]
   #HistData$RoilToGDP.h<-((1/HistData[,"OILSUPGDP"])*HistData[,"OILTPES"]*1e6)/(HistData$GDP*1e6)

   #*HistData$GDP



   #HistData$ReToGDP.h<-(HistData$Price.Oil*HistData$Re.h)/HistData$GDP [1]

#
#test<-subset(HistData,HistData$Country=="World")
#

  ValVars<<-c("GDP.N","Y_ce.N","Y_re.N","GDP.S","Y_ce.S","Y_re.S")
  source(paste(dir.model,"ediam_HistoricCalib.r",sep=""))


ediamMSE<-function(x,verbose=FALSE){
#test mode
#x<-c( 1.27,
#      1.68,
#      0.2,
#      0.2,
#      0.9,
#      0.9,
#      0.5,
#      0.5
#    )

#
 calib.params<<-c( epsilon.N = round(x[1],3),
                   epsilon.S = round(x[2],3),
                   Gamma_re = round(x[3],3),
                   Gamma_ce= round(x[4],3),
                   Eta_re.N = round(x[5],3),
                   Eta_ce.N = round(x[6],3),
                   Eta_re.S = round(x[7],3),
                   Eta_ce.S = round(x[8],3)
                   )


#Normalize historical data
  Y_re.Nh <<-subset(HistData[,"Y_re.h"],HistData[,"Country"]=="Memo: OECD Total") #Mtoe
  Y_ce.Nh <<-subset(HistData[,"Y_ce.h"],HistData[,"Country"]=="Memo: OECD Total") #Mtoe
  Y_re.Sh <<-subset(HistData[,"Y_re.h"],HistData[,"Country"]=="NonOECD") #Mtoe
  Y_ce.Sh <<-subset(HistData[,"Y_ce.h"],HistData[,"Country"]=="NonOECD") #Mtoe

#Oil Supply
  Re.Nh<<-subset(HistData[,"Re.h"],HistData[,"Country"]=="Memo: OECD Total") # Mtoe
  Re.Sh<<-subset(HistData[,"Re.h"],HistData[,"Country"]=="NonOECD") #Mtoe
  ReToGDP.Nh<<-subset(HistData[,"ReToGDP.h"],HistData[,"Country"]=="Memo: OECD Total") # [1]
  ReToGDP.Sh<<-subset(HistData[,"ReToGDP.h"],HistData[,"Country"]=="NonOECD")

#Oil prices
   Price.oil.y<<-subset(HistData[,"Price.Oil"],HistData[,"Country"]=="World")
#Final output
    GDP.Nh<<-subset(HistData[,"GDP"],HistData[,"Country"]=="Memo: OECD Total") #(billion 2010 USD using exchange rates)
    GDP.Sh<<-subset(HistData[,"GDP"],HistData[,"Country"]=="NonOECD") #(billion 2010 USD using exchange rates)
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
#Check constraints
   MSE<-max(c(mean(MSEp1),mean(MSEp1.1),mean(MSEp1.2),mean(MSEp2),mean(MSEp2.1),mean(MSEp2.2)))
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
{ #return(out)
  return(SimulData)
} else {
        return(as.numeric(out$MSE.all))
       }

}


#x<-c(0.313434112, 0.541677378 , 2.295451322, 0.308394557 ,0.537742834 ,2.872877532 ,0.069757015 ,0.013042933 ,0.404165332 ,0.484269168 ,0.003710107 ,0.001566315 ,1.114483965 ,0.787932262 ,0.952493981 ,0.820435777)

#Test
ediamMSE(x3, verbose = TRUE)

#


  ediamMSE(c( round(0.33,3), # alfa.N
              round(0.5,3), # alfa_1.N
              round(5,3), # epsilon.N
              round(0.33,3), # alfa.S
              round(0.5,3), # alfa_1.S
              round(5,3), # epsilon.S
              round(0.02,3), # Gamma_re
              round(0.02,3), # Gamma_ce
              round(0.9,3), # Eta_re.N
              round(0.9,3), # Eta_ce.N
              round(0.5,3), # Eta_re.S
              round(0.5,3), # Eta_ce.S
              round(1.0,3), # A_re0.N
              round(1.0,3), # A_ce0.N
              round(1.0,3), # A_re0.S
              round(1.0,3)  # A_ce0.S
             ), verbose = TRUE
             )

#


#Test initial fitneess using LHC sampling
 library(lhs)
 set.seed(5000)
# sample.size<-100000
 sample.size<-1000
 params.names<-c("alfa.N",
                 "alfa_1.N",
                 "epsilon.N",
                 "alfa.S",
                 "alfa_1.S",
                 "epsilon.S",
                 "Gamma_re",
                 "Gamma_ce",
                 "Eta_re.N",
                 "Eta_ce.N",
                 "Eta_re.S",
                 "Eta_ce.S",
                 "A_re0.N",
                 "A_ce0.N",
                 "A_re0.S",
                 "A_ce0.S")
#
 lhs.sample<-data.frame(randomLHS(sample.size,length(params.names)),Run.ID=1:sample.size)
 colnames(lhs.sample)<-c(params.names,"Run.ID")
#Define range of values
 lhs.sample$alfa.N<-qunif(lhs.sample$alfa.N,0.3,0.35)
 lhs.sample$alfa_1.N<-qunif(lhs.sample$alfa_1.N,0.48,0.52)
 lhs.sample$epsilon.N<-qunif(lhs.sample$epsilon.N,1.5,10)
 lhs.sample$alfa.S<-qunif(lhs.sample$alfa.S,0.3,0.35)
 lhs.sample$alfa_1.S<-qunif(lhs.sample$alfa_1.S,0.48,0.52)
 lhs.sample$epsilon.S<-qunif(lhs.sample$epsilon.S,1.5,10)
 lhs.sample$Gamma_re<-qunif(lhs.sample$Gamma_re,0.005,0.1)
 lhs.sample$Gamma_ce<-qunif(lhs.sample$Gamma_ce,0.005,0.1)
 lhs.sample$Eta_re.N<-qunif(lhs.sample$Eta_re.N,0.3,0.8)
 lhs.sample$Eta_ce.N<-qunif(lhs.sample$Eta_ce.N,0.3,0.8)
 lhs.sample$Eta_re.S<-qunif(lhs.sample$Eta_re.S,0.01,0.05)
 lhs.sample$Eta_ce.S<-qunif(lhs.sample$Eta_re.S,0.01,0.05)
 lhs.sample$A_re0.N<-qunif(lhs.sample$A_re0.N,0.5,1.5)
 lhs.sample$A_ce0.N<-qunif(lhs.sample$A_ce0.N,0.5,1.5)
 lhs.sample$A_re0.S<-qunif(lhs.sample$A_re0.S,0.5,1.5)
 lhs.sample$A_ce0.S<-qunif(lhs.sample$A_ce0.S,0.5,1.5)

#for (i in 1:nrow(lhs.sample))
#{
i<-497 # 877
calib.params<<-c( alfa.N = round(lhs.sample[i,'alfa.N'],2),
                  alfa_1.N = round(lhs.sample[i,'alfa_1.N'],2),
                  epsilon.N = round(lhs.sample[i,'epsilon.N'],2),
                  alfa.S = round(lhs.sample[i,'alfa.S'],2),
                  alfa_1.S = round(lhs.sample[i,'alfa_1.S'] ,2),
                  epsilon.S = round(lhs.sample[i,'epsilon.S'],2),
                  Gamma_re = round(lhs.sample[i,'Gamma_re'],2),
                  Gamma_ce= round(lhs.sample[i,'Gamma_ce'],2),
                  Eta_re.N = round(lhs.sample[i,'Eta_re.N'],2),
                  Eta_ce.N = round(lhs.sample[i,'Eta_ce.N'],2),
                  Eta_re.S = round(lhs.sample[i,'Eta_re.S'],2),
                  Eta_ce.S = round(lhs.sample[i,'Eta_ce.S'],2)
                  )


#i<-2
#ediamMSE(c(  round(lhs.sample[i,'alfa.N'],2),
#             round(lhs.sample[i,'alfa_1.N'],2),
#             round(lhs.sample[i,'epsilon.N'] ,2),
#             round(lhs.sample[i,'alfa.S'] ,2),
#             round(lhs.sample[i,'alfa_1.S'] ,2),
#             round(lhs.sample[i,'epsilon.S'],2),
#             round(lhs.sample[i,'Gamma_re'],2),
#             round(lhs.sample[i,'Gamma_ce'],2),
#             round(lhs.sample[i,'Eta_re.N'],2),
#             round(lhs.sample[i,'Eta_ce.N'],2),
#             round( lhs.sample[i,'Eta_re.S'],2),
#             round(lhs.sample[i,'Eta_ce.S'],2)
#           ), verbose = TRUE
#           )

#   print(lhs.sample[i,'Run.ID'])
#}


#multi-core runs
 library(snow)
# nCore<-40
  nCore<-8
 cl <- makeSOCKcluster(names = rep('localhost',nCore))
 global.elements<-list("ediamMSE","ediamCalib","HistData","ValVars","dir.model")
 clusterExport(cl,global.elements,envir=environment())
 test.out<- parApply(cl,lhs.sample,1,function(x) {out<-ediamMSE(c(  as.numeric(x['alfa.N']),
                                                                   as.numeric(x['alfa_1.N']),
                                                                   as.numeric(x['epsilon.N']),
                                                                   as.numeric(x['alfa.S']),
                                                                   as.numeric(x['alfa_1.S']),
                                                                   as.numeric(x['epsilon.S']),
                                                                   as.numeric(x['Gamma_re']),
                                                                   as.numeric(x['Gamma_ce']),
                                                                   as.numeric(x['Eta_re.N']),
                                                                   as.numeric(x['Eta_ce.N']),
                                                                   as.numeric(x['Eta_re.S']),
                                                                   as.numeric(x['Eta_ce.S']),
                                                                   as.numeric(x['A_re0.N']),
                                                                   as.numeric(x['A_ce0.N']),
                                                                   as.numeric(x['A_re0.S']),
                                                                   as.numeric(x['A_ce0.S'])
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
#order
 test.out<-test.out[order(test.out$MSE.all ),]
 head(test.out)


#subset
Targetpts<-subset(test.out,test.out$MSE.Y.N<0.01 &  test.out$MSE.Y_ce.N<0.01  &   test.out$MSE.Y.S<0.01 & test.out$MSE.Y_ce.S<0.01)

####





#############################################################################################

#Optimization
 library(snow)
#  nCore<-40
  nCore<-8
  cl <- makeSOCKcluster(names = rep('localhost',nCore))
  global.elements<-list("ediamMSE","ediamCalib","HistData","ValVars","dir.model")
  clusterExport(cl,global.elements,envir=environment())
#

library(rgenoud)
genoud(ediamMSE,max=FALSE,nvars=8,
       starting.values = c( 5.0,  #epsilon.N
                            5.00, #epsilon.S
                            0.02, #Gamma_re
                            0.02, #Gamma_ce
                            0.9, #Eta_re.N
                            0.9, #Eta_ce.N
                            0.02, #Eta_re.S
                            0.02 #Eta_re.S
                         ),
       pop.size=1000,
       Domains=matrix(c(#inferior limits
                            1.5,  #epsilon.N
                            1.5, #epsilon.S
                            0.001, #Gamma_re
                            0.001, #Gamma_ce
                            0.5, #Eta_re.N
                            0.5, #Eta_ce.N
                            0.001, #Eta_re.S
                            0.001, #Eta_ce.S,
                        #superior limits
                            10,  #epsilon.N
                            10, #epsilon.S
                            0.2, #Gamma_re
                            0.2, #Gamma_ce
                            0.95, #Eta_re.N
                            0.95, #Eta_ce.N
                            0.10, #Eta_re.S
                            0.10 #Eta_ce.S
                               ),
                            ncol=2),
       cluster=cl,
       #boundary.enforcement=2, # 0-> out of bounds individuals are valid, 2-> respect the bounds
       print.level=1)

stopCluster(cl)
# preliminary solution:
[1] 1.308369984 2.583652772 0.085906537 0.011615490 0.785861985 0.949595114 0.005257706 0.008549853



Y_re.Nh <- HistData$Yre.h[HistData$Region=="N"]/HistData$Y.h[HistData$Region=="N"]
Y_ce.Nh <- HistData$Yce.h[HistData$Region=="N"]/HistData$Y.h[HistData$Region=="N"]
Y_re.Sh <- HistData$Yre.h[HistData$Region=="S"]/HistData$Y.h[HistData$Region=="S"]
Y_ce.Sh <- HistData$Yce.h[HistData$Region=="S"]/HistData$Y.h[HistData$Region=="S"]
#Oil prices
cR.y<-HistPrice$cR/HistPrice$cR[1]
#Population
L.N.y <-HistPopData$OECD/HistPopData$World
L.S.y <-HistPopData$NonOECD/HistPopData$World
#Define initial conditions
Y_re0.N <- Y_re.Nh[1]
Y_ce0.N <- Y_ce.Nh[1]
Y_re0.S <- Y_re.Sh[1]
Y_ce0.S <- Y_ce.Sh[1]

Hist<-data.frame( Time = rep(c(1980:2012),6),
            Value = c(rep(1,length(Y_re.Nh)),rep(1,length(Y_re.Nh)),Y_re.Nh, Y_ce.Nh, Y_re.Sh,Y_ce.Sh),
            Variable= rep(c("Y.N","Y.S","Y_re.N","Y_ce.N","Y_re.S","Y_ce.S"),each=length(Y_re.Nh)),
            Data.type="Hist")
###

# THIS IS A PRELIMINARY SOLUTION, IT WORKS, BUT STILL NEED TO CONSIDER THE ISSUE OF ALLOCATION OF SCIENTISTS AND LEARNING BY DOING

x1<-c(0.163819384,
      0.500661054,
      1.296239254,
      0.182494521,
      0.549619835,
      1.726091469,
      0.004515184,
      0.027939558,
      0.598462168,
      0.466012633,
      -0.001973042,
      0.002329774,
      1.020028423,
      0.968183059,
      0.592764913,
      1.160422713)
#
x2<-c(0.29963895,
      0.55823390,
      1.45180262,
      0.36098665,
      0.52848815,
      2.33866304,
      0.02166766,
      0.02259134,
      0.50567283,
      0.40380299,
      0.01748830,
      0.01823972,
      1.49982043,
      0.87608531,
      1.32427254,
      0.76674915)
      $par
x3<-c(0.130349401,
      0.421037655,
      1.279784814,
      0.130137666,
      0.599562782,
      1.681632295,
      0.018722614,
      0.009124393,
      0.622799839,
      0.468477442,
      0.125097625,
      0.027676454,
      1.0, #1.020516958,
      1.0, #0.959000501,
      1.0, #0.838409039,
      1.0 )#1.027368058)

###
SimData<-ediamCalib(c( alfa.N = round(x[1],3),
                       alfa_1.N = round(x[2],3),
                       epsilon.N = round(x[3],3),
                       alfa.S = round(x[4],3),
                       alfa_1.S = round(x[5],3),
                       epsilon.S = round(x[6],3),
                       Gamma_re = round(x[7],3),
                       Gamma_ce= round(x[8],3),
                       Eta_re.N = round(x[9],3),
                       Eta_ce.N = round(x[10],3),
                       Eta_re.S = round(x[11],3),
                       Eta_ce.S = round(x[12],3),
                       A_re0.N = round(x[13],3),
                       A_ce0.N = round(x[14],3),
                       A_re0.S = round(x[15],3),
                       A_ce0.S = round(x[16],3)
                       ),
                      dir.model,verbose=TRUE)
#
SimData<-data.frame( Time = rep(c(1980:2012),6),
            Value = c(SimData[,"Y.N"],SimData[,"Y.S"], SimData[,"Y_re.N"], SimData[,"Y_ce.N"],SimData[,"Y_re.S"],SimData[,"Y_ce.S"]),
            Variable= rep(c("Y.N","Y.S","Y_re.N","Y_ce.N","Y_re.S","Y_ce.S"),each=length(Y_re.Nh)),
            Data.type="Sim")
#
calib.data<-rbind(Hist,SimData)

library(ggplot2)
calib.plot <- ggplot(data=calib.data, aes(x=Time, y=Value, colour = Data.type))
calib.plot + geom_line() + facet_wrap(~ Variable)

#

#Show transition plot

#OECD
 SimData.N<-subset(SimData,SimData$Variable%in%c('Y_ce.N','Y_re.N'))
 SimData.N$Y.h<-c(HistData$Y.h[HistData$Region=="N"],HistData$Y.h[HistData$Region=="N"])
 SimData.N$Value<-SimData.N$Value*SimData.N$Y.h
 SimData.N<-aggregate(Value ~ Time, data = SimData.N, FUN = sum)
 SimData.N$Region<-'OECD'

#NonOECD
 SimData.S<-subset(SimData,SimData$Variable%in%c('Y_ce.S','Y_re.S'))
 SimData.S$Y.h<-c(HistData$Y.h[HistData$Region=="S"],HistData$Y.h[HistData$Region=="S"])
 SimData.S$Value<-SimData.S$Value*SimData.S$Y.h
 SimData.S<-aggregate(Value ~ Time, data = SimData.S, FUN = sum)
 SimData.S$Region<-'NonOECD'

#rbind
 Data<-rbind(SimData.N,SimData.S)
 Data$Type<-'Simulation'

 HistData$Time<-HistData$time
 HistData$Value<-HistData$Yce.h+HistData$Yre.h
 HistData$Region<-ifelse(HistData$Region=='N','OECD','NonOECD')
 HistData$Type<-'Historic'
# rbind all

 Data<-rbind(Data,HistData[,c('Time','Region','Value','Type')])


calib.plot <- ggplot(data=Data, aes(x=Time, y=Value, colour = Region))
calib.plot + geom_line() + facet_wrap(~ Type)




#Different utility functions

  out<- data.frame( expected.profits = seq(0,500,by=0.05)
                  )
  out$exp.profits<-exp(out$expected.profits)
  #out$utility<-out$expected.profits^0.15
  out$utility<-log(out$expected.profits)
  out$exp.utility<-exp(out$utility)

  utility.plot <- ggplot(data=out, aes(x=expected.profits, y=utility))
  utility.plot + geom_line()
#

ediamMSE(c(0.33,0.33*0.5,5.0))

#we want to minimize the value of ediamMSE
eps<-11

(
(HistData$Yre.h[HistData$Region=="N"]/HistData.Y.N)^((eps-1)/eps)+
(HistData$Yce.h[HistData$Region=="N"]/HistData.Y.N)^((eps-1)/eps)
)^(eps/(eps-1))


#debugging initial conditions expressions
#no oil prices

#parameters
 L_0.S<-4.0
 epsilon<-5
 alfa<-0.30
 ce.tax.N<-0
 Tec.subsidy.N<-0
 ce.tax.S<-0
 Tec.subsidy.S<-0

#initial values
 Y_re0.N <- Y_re.Nh[1]
 Y_ce0.N <- Y_ce.Nh[1]
 Y_re0.S <- Y_re.Sh[1]
 Y_ce0.S <- Y_ce.Sh[1]

#in the advanced region
 A_ce0.N<-((Y_ce0.N^((epsilon-1)/epsilon)+Y_re0.N^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_ce0.N/Y_re0.N)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
 A_re0.N<-((Y_ce0.N^((epsilon-1)/epsilon)+Y_re0.N^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_re0.N/Y_ce0.N)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))

#In the Emerging Region
 A_ce0.S<-(1/L_0.S)*((Y_ce0.S^((epsilon-1)/epsilon)+Y_re0.S^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_ce0.S/Y_re0.S)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
 A_re0.S<-(1/L_0.S)*((Y_ce0.S^((epsilon-1)/epsilon)+Y_re0.S^((epsilon-1)/epsilon))^(epsilon/(epsilon-1)))*(1+(Y_re0.S/Y_ce0.S)^((1-epsilon)/epsilon))^(1/((1-alfa)*(1-epsilon)))
#print
 A_ce0.N
 A_re0.N
 A_ce0.S
 A_re0.S
#Now with this value, estimate production
#Economic structure -NO OIL PRICES-
#======================================================================================================================================================

#Auxiliaries for both regions
     phi<-(1-alfa)*(1-epsilon)
     epsi_re<-alfa^2 #this is the cost of production of clean technologies
     epsi_ce<-alfa^2 #this is the cost of production of dirty technologies
#Advanced Region
#Auxiliaries in Advanced Region
     L.N<-1.0
#First we determine the equilibrium levels of relative input prices and relative labor
     RelPrice.N<-((A_ce0.N/A_re0.N)^(1-alfa))*(((epsi_re*(1-Tec.subsidy.N))/epsi_ce)^alfa)
     RelLabor.N<-((1+ce.tax.N)^epsilon)*((((1-Tec.subsidy.N)*epsi_re)/epsi_ce)^(alfa*(1-epsilon)))*((A_re0.N/A_ce0.N)^(-1*phi))
#Second we determine the equilibrium conditions for each sector
     #clean sector
      Labor_re.N<-(RelLabor.N*L.N)/(1+RelLabor.N) #based on the assumption that Labor_re.N+Labor_ce.N=L.N
      Price_re.N<-RelPrice.N/(RelPrice.N^(1-epsilon)+(1)^(1-epsilon))^(1/(1-epsilon)) #based on Price_re.N^(1-epsilon)+Price_ce.N^(1-epsilon)=1 [checked many times and it is correct]
      Y_re.N<-((((alfa^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re))^(alfa/(1-alfa)))*Labor_re.N*A_re0.N
     #dirty sector
      Labor_ce.N<-L.N/(RelLabor.N+1)
      Price_ce.N<-Price_re.N/RelPrice.N
      Y_ce.N<-((((alfa^2)*Price_ce.N)/(epsi_ce))^(alfa/(1-alfa)))*Labor_ce.N*A_ce0.N
     #Total Production
      Y.N<-((Y_re.N)^((epsilon-1)/epsilon)+(Y_ce.N)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))
#compare estimation
 Y_re0.N
 Y_re.N
 Y_ce0.N
 Y_ce.N

#Emerging Region
 #Auxiliaries in Emerging Region
    L.S<-L_0.S #the population of the South is 4.6 that of the North,
  #First we determine the equilibrium levels of relative input prices and relative labour
     RelPrice.S<-((A_ce0.S/A_re0.S)^(1-alfa))*(((epsi_re*(1-Tec.subsidy.S))/epsi_ce)^alfa)
     RelLabor.S<-((1+ce.tax.S)^epsilon)*((((1-Tec.subsidy.S)*epsi_re)/epsi_ce)^(alfa*(1-epsilon)))*((A_re0.S/A_ce0.S)^(-1*phi))
  #Second we determine the equilibrium conditions for each sector
    #clean sector
     Labor_re.S<-(L.S*RelLabor.S)/(RelLabor.S+1) #based on the assumption that Labor_re.S+Labor_ce.S=L.S
       Price_re.S<-RelPrice.S/(RelPrice.S^(1-epsilon)+(1)^(1-epsilon))^(1/(1-epsilon)) #based on the Price_re.S^(1-epsilon)+(Price_ce.S)^(1-epsilon)=1 [checked many times and it is correct]
       Y_re.S<-((((alfa^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re))^(alfa/(1-alfa)))*Labor_re.S*A_re0.S
    #dirty sector
       Labor_ce.S<-L.S/(RelLabor.S+1)
       Price_ce.S<-Price_re.S/RelPrice.S
       Y_ce.S<-((((alfa^2)*Price_ce.S)/(epsi_ce))^(alfa/(1-alfa)))*Labor_ce.S*A_ce0.S
    #Total Production
      Y.S<-((Y_re.S)^((epsilon-1)/epsilon)+(Y_ce.S)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))
#
 Y_re0.S
 Y_re.S
 Y_ce0.S
 Y_ce.S

####
#now with oil prices
###
#i<-454
#calib.params<-c(alfa = as.numeric(lhs.sample[i,'alfa']),
#                alfa_1 = as.numeric(lhs.sample[i,'alfa_1']),
#                epsilon =  as.numeric(lhs.sample[i,'epsilon']))

#test.params<-c( alfa.N = round(0.01159775 ,2),
#                         alfa_1.N= round(0.89663578 ,2),
#                         epsilon.N = round(5.52116609 ,2),
#                         alfa.S = round(0.04577620 ,2),
#                         alfa_1.S = round(0.77277290 ,2),
#                         epsilon.S = round(9.99991257 ,2),
#                         Gamma_re = round(1.13219801 ,2),
#                         Gamma_ce = round(0.88160998 ,2),
#                         Eta_re = round(0.01299530 ,2),
#                         Eta_ce = round(0.01199288 ,2),
#                         Nu_re = round(0.01833847 ,2),
#                         Nu_ce = round(0.01223270 ,2))

i<-14
test.params<-c( alfa.N = round(lhs.sample[i,'alfa.N'] ,2),
                  alfa_1.N = round(lhs.sample[i,'alfa_1.N'],2),
                  epsilon.N = round(lhs.sample[i,'epsilon.N'] ,2),
                  alfa.S = round(lhs.sample[i,'alfa.S'] ,2),
                  alfa_1.S = round(lhs.sample[i,'alfa_1.S'] ,2),
                  epsilon.S = round(lhs.sample[i,'epsilon.S'],2),
                  Gamma_re = round(lhs.sample[i,'Gamma_re'] ,2),
                  Gamma_ce= round(lhs.sample[i,'Gamma_ce'],2),
                  Eta_re = round(lhs.sample[i,'Eta_re'],2),
                  Eta_ce =round(lhs.sample[i,'Eta_ce'],2),
                  Nu_re = round( lhs.sample[i,'Nu_re'],2),
                  Nu_ce = round(lhs.sample[i,'Nu_ce'],2)
                  )

#parameters
 L_0.S<-L.S.y[1]
 L_0.N<-L.N.y[1]
 #economic parameters North
  alfa.N <- as.numeric(test.params['alfa.N'])
  alfa_1.N <- as.numeric(test.params['alfa_1.N'])*as.numeric(test.params['alfa.N']) #a scalar with respect to alfa
  alfa_2.N <- as.numeric(test.params['alfa.N'])*(1-as.numeric(test.params['alfa_1.N']))
  epsilon.N <- as.numeric(test.params['epsilon.N'])
 #economic parameters south
  alfa.S <- as.numeric(test.params['alfa.S']) #alfa.N
  alfa_1.S <- as.numeric(test.params['alfa_1.S'])*as.numeric(test.params['alfa.S']) # alfa_1.S
  alfa_2.S <- as.numeric(test.params['alfa.S'])*(1-as.numeric(test.params['alfa_1.S'])) # alfa_2.S
  epsilon.S <- as.numeric(test.params['epsilon.S'])
 #Technological paramters
  Gamma_re <- as.numeric(test.params['Gamma_re'])# 0.2 as.numeric(params['Gamma_re']),
  Gamma_ce <- as.numeric(test.params['Gamma_ce'])# 0.2 as.numeric(params['Gamma_ce']),
  Eta_re <- as.numeric(test.params['Eta_re'])#as.numeric(params['Eta_re']),
  Eta_ce <- as.numeric(test.params['Eta_ce'])#as.numeric(params['Eta_ce']),
  Nu_re <- as.numeric(test.params['Nu_re'])#as.numeric(params['Nu_re']),
  Nu_ce <- as.numeric(test.params['Nu_ce'])#as.numeric(params['Nu_ce']),


#Policies
 ce.tax.N<-0
 Tec.subsidy.N<-0
 RD.subsidy.N<-0
 ce.tax.S<-0
 Tec.subsidy.S<-0
 RD.subsidy.S<-0

 cR_0<-1.0
 epsi.N<-alfa.N^2
 epsi.S<-alfa.S^2
 phi.N<-(1-alfa.N)*(1-epsilon.N)
 phi_1.N<-(1-alfa_1.N)*(1-epsilon.N)
 phi.S<-(1-alfa.S)*(1-epsilon.S)
 phi_1.S<-(1-alfa_1.S)*(1-epsilon.S)

#initial values
 Y_0.N <- Y_h.N[1]
 Y_re0.N <- Y_re.Nh[1]
 Y_ce0.N <- Y_ce.Nh[1]
 Y_0.S <- Y_h.S[1]
 Y_re0.S <- Y_re.Sh[1]
 Y_ce0.S <- Y_ce.Sh[1]


#Productivity
#Initial Productivity conditions are determined by the initial levels of production of energy and initial prices of the exhaustable resource
#In the advanced region
  teta.N<-( ( epsi.N^(alfa_1.N+alfa_2.N*alfa.N+alfa_2.N*phi.N) * ( alfa.N^(-2*alfa.N*(phi.N+alfa.N)) ) )^(1/(1-alfa.N)) )/
        ( ( (alfa_1.N^(2*alfa_1.N))*(alfa_2.N^alfa_2.N) )^(epsilon.N) )

  A_ce0.N <- ( ( ( ( (Y_0.N^((epsilon.N-1)/epsilon.N))-(Y_ce0.N^((epsilon.N-1)/epsilon.N)) )^( epsilon.N/(epsilon.N-1) ) ) *( (alfa.N^(2*alfa.N*(1-epsilon.N)))*((Y_re0.N/(teta.N*Y_ce0.N))^(phi.N/(epsilon.N*(1-alfa.N))))+
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

  A_ce0.S <- ( ( ( ( (Y_0.S^((epsilon.S-1)/epsilon.S))-(Y_ce0.S^((epsilon.S-1)/epsilon.S)) )^( epsilon.S/(epsilon.S-1) ) )*( (alfa.S^(2*alfa.S*(1-epsilon.S)))*((Y_re0.S/(teta.S*Y_ce0.S))^(phi.S/(epsilon.S*(1-alfa.S))))+
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



#print
 A_ce0.N
 A_re0.N
 A_ce0.S
 A_re0.S
#
#SimData[i,c("A_re.N","A_ce.N","A_re.S","A_ce.S")]
#
#Economic structure -WITH OIL PRICES-
#======================================================================================================================================================
#Auxiliaries for both regions
     cR<-1.0 #cR[t] # Oil prices are exogenous
	#Advanced Region
#Auxiliaries in Advanced Region
     epsi_re.N<-epsi.N #this is the cost of production of clean technologies
     epsi_ce.N<-epsi.N #this is the cost of production of dirty technologies
     L.N<-L_0.N#exp(labor.growth.N*t)*L_0.N
#First we determine the equilibrium levels of relative input prices and relative labor
     RelPrice.N<-( (epsi_re.N^alfa.N)*(alfa_1.N^(2*alfa_1.N))*(alfa_2.N^alfa_2.N)*(A_ce0.N^(1-alfa_1.N))*(1-Tec.subsidy.N) )/
                 ( (cR^alfa_2.N)*(epsi_ce.N^alfa_1.N)*(alfa.N^(2*alfa.N))*(A_re0.N^(1-alfa.N)) )
     RelLabor.N<-((1+ce.tax.N)^epsilon.N)*
                 ( (
                    ( (alfa.N^(2*alfa.N))*(cR^alfa_2.N)*(epsi_ce.N^alfa_1.N) )/
                    ( ((1-Tec.subsidy.N)^alfa.N)*(alfa_2.N^alfa_2.N)*(alfa_1.N^(2*alfa_1.N))*(epsi_re.N^alfa.N) )
                    )^(epsilon.N-1) )*
                 (
                    ( A_re0.N^(-1*phi.N) )/
                    ( A_ce0.N^(-1*(1-alfa_1.N)*(1-epsilon.N)) )
                  )
#Second we determine the equilibrium conditions for each sector
     #clean sector
      Labor_re.N<-(RelLabor.N*L.N)/(1+RelLabor.N) #based on the assumption that Labor_re.N+Labor_ce.N=L.N
	    Price_re.N<-RelPrice.N/(RelPrice.N^(1-epsilon.N)+(1)^(1-epsilon.N))^(1/(1-epsilon.N)) #based on  Price_re.N^(1-epsilon)+Price_ce.N^(1-epsilon)=1 [checked many times and it is correct]
      Xtech_re.N<-((((alfa.N^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re.N))^(1/(1-alfa.N)))*Labor_re.N*A_re0.N
      Profits_re.N<-(1+RD.subsidy.N)*Eta_re*epsi_re.N*((1-alfa.N)/alfa.N)*Xtech_re.N # Expected profits see annex IV. Equilibrium research profits
      Y_re.N<-((((alfa.N^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re.N))^(alfa.N/(1-alfa.N)))*Labor_re.N*A_re0.N
     #dirty sector
      Labor_ce.N<-L.N/(RelLabor.N+1)
      Price_ce.N<-Price_re.N/RelPrice.N
      Re.N<-(alfa_1.N^2/epsi_ce.N)^( alfa_1.N/(1-alfa.N) )*
          (alfa_2.N*A_ce0.N/cR)^((1-alfa_1.N)/(1-alfa.N))*
          Labor_ce.N*
          Price_ce.N^(1/(1-alfa.N))
      Xtech_ce.N<- (
                               (
                                ( (alfa_1.N^2)*(Price_ce.N)*(Re.N^alfa_2.N) )/
                                ( epsi_ce.N )
                               )^( 1/(1-alfa_1.N) )
                              )*
                              ( Labor_ce.N^( (1-alfa.N)/(1-alfa_1.N) ) )*
                              ( A_ce0.N )
      Profits_ce.N<-Eta_ce*epsi_ce.N*((1-alfa_1.N)/alfa_1.N)*Xtech_ce.N
      Y_ce.N<-( (alfa_1.N^2/epsi_ce.N)^( alfa_1.N/(1-alfa.N) ) )*
             ( (alfa_2.N*A_ce0.N/cR)^( alfa_2.N/(1-alfa.N) ) )*
             ( Price_ce.N^( alfa.N/(1-alfa.N) ) )*
             Labor_ce.N*
             A_ce0.N
     #Total Production
      Y.N<-((Y_re.N)^((epsilon.N-1)/epsilon.N)+(Y_ce.N)^((epsilon.N-1)/epsilon.N))^(epsilon.N/(epsilon.N-1))
     #Allocation of Scientists
#     if ( exp(Profits_re.N)==Inf | exp(Profits_ce.N)==Inf )
#     {
#       s_re.N<-ifelse(Profits_re.N>Profits_ce.N,1,0)
#       s_ce.N<-1-s_re.N
#      } else {
          s_re.N<-exp(Profits_re.N)/(exp(Profits_ce.N)+exp(Profits_re.N))
          s_ce.N<-1-s_re.N
#      }
#Emerging Region
	#Auxiliaries in Emerging Region
     epsi_re.S<-epsi.S #this is the cost of production of clean technologies
     epsi_ce.S<-epsi.S #this is the cost of production of dirty technologies
	   L.S<-L_0.S#(exp(labor.growth.S*t))*L_0.S #the population of the South is 4.6 that of the North,
  #First we determine the equilibrium levels of relative input prices and relative labour
     RelPrice.S<-( (epsi_re.S^alfa.S)*(alfa_1.S^(2*alfa_1.S))*(alfa_2.S^alfa_2.S)*(A_ce0.S^(1-alfa_1.S))*(1-Tec.subsidy.S) )/
                 ( (cR^alfa_2.S)*(epsi_ce.S^alfa_1.S)*(alfa.S^(2*alfa.S))*(A_re0.S^(1-alfa.S)) )
     RelLabor.S<-((1+ce.tax.S)^epsilon.S)*
                 ( (
                    ( (alfa.S^(2*alfa.S))*(cR^alfa_2.S)*(epsi_ce.S^alfa_1.S) )/
                    ( ((1-Tec.subsidy.S)^alfa.S)*(alfa_2.S^alfa_2.S)*(alfa_1.S^(2*alfa_1.S))*(epsi_re.S^alfa.S) )
                    )^(epsilon.S-1) )*
                 (
                    ( A_re0.S^(-1*phi.S) )/
                    ( A_ce0.S^(-1*(1-alfa_1.S)*(1-epsilon.S)) )
                  )
  #Second we determine the equilibrium conditions for each sector
    #clean sector
       Labor_re.S<-(L.S*RelLabor.S)/(RelLabor.S+1) #based on the assumption that Labor_re.S+Labor_ce.S=L.S
   	   Price_re.S<-RelPrice.S/(RelPrice.S^(1-epsilon.S)+(1)^(1-epsilon.S))^(1/(1-epsilon.S)) #based on  Price_re.S^(1-epsilon)+(Price_ce.S)^(1-epsilon)=1 [checked many times and it is correct]
       Xtech_re.S<-((((alfa.S^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re.S))^(1/(1-alfa.S)))*Labor_re.S*A_re0.S
       Profits_re.S<-(1+RD.subsidy.S)*Nu_re*epsi_re.S*((1-alfa.S)/alfa.S)*Xtech_re.S # Expected profits see annex IV. Equilibrium research profits
       Y_re.S<-((((alfa.S^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re.S))^(alfa.S/(1-alfa.S)))*Labor_re.S*A_re0.S
    #dirty sector
       Labor_ce.S<-L.S/(RelLabor.S+1)
       Price_ce.S<-Price_re.S/RelPrice.S
       Re.S<-(alfa_1.S^2/epsi_ce.S)^( alfa_1.S/(1-alfa.S) )*
           (alfa_2.S*A_ce0.S/cR)^((1-alfa_1.S)/(1-alfa.S))*
           Labor_ce.S*
           Price_ce.S^(1/(1-alfa.S))
       Xtech_ce.S<- (
                                (
                                 ( (alfa_1.S^2)*(Price_ce.S)*(Re.S^alfa_2.S) )/
                                 ( epsi_ce.S )
                                )^( 1/(1-alfa_1.S) )
                               )*
                               ( Labor_ce.S^( (1-alfa.S)/(1-alfa_1.S) ) )*
                               ( A_ce0.S )
       Profits_ce.S<-Nu_ce*epsi_ce.S*((1-alfa_1.S)/alfa_1.S)*Xtech_ce.S
       Y_ce.S<-( (alfa_1.S^2/epsi_ce.S)^( alfa_1.S/(1-alfa.S) ) )*
              ( (alfa_2.S*A_ce0.S/cR)^( alfa_2.S/(1-alfa.S) ) )*
              ( Price_ce.S^( alfa.S/(1-alfa.S) ) )*
              Labor_ce.S*
              A_ce0.S
    #Total Production
      Y.S<-((Y_re.S)^((epsilon.S-1)/epsilon.S)+(Y_ce.S)^((epsilon.S-1)/epsilon.S))^(epsilon.S/(epsilon.S-1))
    #Allocation of Scientists

#print and compare

#compare estimation
 Y_re0.N
 Y_re.N
 Y_ce0.N
 Y_ce.N

 #compare estimation
  Y_re0.S
  Y_re.S
  Y_ce0.S
  Y_ce.S

SimData[1,c("Y_re.N","Y_ce.N","Y_re.S","Y_ce.S")]
































#++++PREVIOUS EQUATIONS+++++++++++++

#Initial Productivity conditions are determined by the initial levels of production of energy and initial prices of the exhaustable resource
   teta<-( ( epsi^(alfa_1+alfa_2*alfa+alfa_2*phi)*( alfa^(-2*alfa*(phi+alfa)) ) )^(1/(1-alfa)) )/
           ( ( (alfa_1^(2*alfa_1))*(alfa_2^alfa_2) )^(epsilon) )

#In the advanced region
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

#
#print
 A_ce0.N
 A_re0.N
 A_ce0.S
 A_re0.S
#
 A_re0.N<-1.487240e-08 +0.000000e+00
 A_ce0.N<-12.68011 + 0.05072046



#initial conditions do no match

#then economic structure
#
#Auxiliaries for both regions
     cR<-0.3003950 #1.0 #cR[t] # Oil prices are exogenous
	   epsi_re<-epsi #this is the cost of production of clean technologies
     epsi_ce<-epsi #this is the cost of production of dirty technologies
#Advanced Region
#Auxiliaries in Advanced Region
     L.N<-L_0.N
#First we determine the equilibrium levels of relative input prices and relative labor
     RelPrice.N<-( (epsi_re^alfa)*(alfa_1^(2*alfa_1))*(alfa_2^alfa_2)*(A_ce0.N^(1-alfa_1))*(1-Tec.subsidy.N) )/
                 ( (cR^alfa_2)*(epsi_ce^alfa_1)*(alfa^(2*alfa))*(A_re0.N^(1-alfa)) )
     RelLabor.N<-((1+ce.tax.N)^epsilon)*
                 ( (
                    ( (alfa^(2*alfa))*(cR^alfa_2)*(epsi_ce^alfa_1) )/
                    ( ((1-Tec.subsidy.N)^alfa)*(alfa_2^alfa_2)*(alfa_1^(2*alfa_1))*(epsi_re^alfa) )
                    )^(epsilon-1) )*
                 (
                    ( A_re0.N^(-1*phi) )/
                    ( A_ce0.N^(-1*(1-alfa_1)*(1-epsilon)) )
                  )
#Second we determine the equilibrium conditions for each sector
     #clean sector
      Labor_re.N<-(RelLabor.N*L.N)/(1+RelLabor.N) #based on the assumption that Labor_re.N+Labor_ce.N=L.N
	    Price_re.N<-RelPrice.N/(RelPrice.N^(1-epsilon)+(1)^(1-epsilon))^(1/(1-epsilon)) #based on  Price_re.N^(1-epsilon)+Price_ce.N^(1-epsilon)=1 [checked many times and it is correct]
      Y_re.N<-((((alfa^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re))^(alfa/(1-alfa)))*Labor_re.N*A_re0.N
      Xtech_re.N<-((((alfa^2)*Price_re.N)/((1-Tec.subsidy.N)*epsi_re))^(1/(1-alfa)))*Labor_re.N*A_re0.N
      Profits_re.N<-(1+RD.subsidy.N)*Eta_re*epsi_re*((1-alfa)/alfa)*Xtech_re.N # Expected profits see annex IV. Equilibrium research profits
     #dirty sector
      Labor_ce.N<-L.N/(RelLabor.N+1)
      Price_ce.N<-Price_re.N/RelPrice.N
      Re<-(alfa_1^2/epsi_ce)^( alfa_1/(1-alfa) )*
          (alfa_2*A_ce0.N/cR)^((1-alfa_1)/(1-alfa))*
          Labor_ce.N*
          Price_ce.N^(1/(1-alfa))
      Y_ce.N<-( (alfa_1^2/epsi_ce)^( alfa_1/(1-alfa) ) )*
             ( (alfa_2*A_ce0.N/cR)^( alfa_2/(1-alfa) ) )*
             ( Price_ce.N^( alfa/(1-alfa) ) )*
             Labor_ce.N*
             A_ce0.N
     #Total Production
      Y.N<-((Y_re.N)^((epsilon-1)/epsilon)+(Y_ce.N)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))

#
#
Xtech_ce.N<- (
                         (
                          ( (alfa_1^2)*(Price_ce.N)*(Re^alfa_2) )/
                          ( epsi_ce )
                         )^( 1/(1-alfa_1) )
                        )*
                        ( Labor_ce.N^( (1-alfa)/(1-alfa_1) ) )*
                        ( A_ce0.N )
Profits_ce.N<-Eta_ce*epsi_ce*((1-alfa_1)/alfa_1)*Xtech_ce.N

#

if ( exp(Profits_re.N)==Inf | exp(Profits_ce.N)==Inf )
{
  s_re.N<-ifelse(Profits_re.N>Profits_ce.N,1,0)
  s_ce.N<-1-s_re.N
 } else {
     s_re.N<-exp(Profits_re.N)/(exp(Profits_ce.N)+exp(Profits_re.N))
     s_ce.N<-1-s_re.N
 }
#compare estimation
 Y_re0.N
 Y_re.N
 Y_ce0.N
 Y_ce.N
 s_re.N
 s_ce.N

#Emerging Region
	#Auxiliaries in Emerging Region
	   L.S<-L_0.S #the population of the South is 4.6 that of the North,
  #First we determine the equilibrium levels of relative input prices and relative labour
     RelPrice.S<-( (epsi_re^alfa)*(alfa_1^(2*alfa_1))*(alfa_2^alfa_2)*(A_ce0.S^(1-alfa_1))*(1-Tec.subsidy.S) )/
                 ( (cR^alfa_2)*(epsi_ce^alfa_1)*(alfa^(2*alfa))*(A_re0.S^(1-alfa)) )
     RelLabor.S<-((1+ce.tax.S)^epsilon)*
                 ( (
                    ( (alfa^(2*alfa))*(cR^alfa_2)*(epsi_ce^alfa_1) )/
                    ( ((1-Tec.subsidy.S)^alfa)*(alfa_2^alfa_2)*(alfa_1^(2*alfa_1))*(epsi_re^alfa) )
                    )^(epsilon-1) )*
                 (
                    ( A_re0.S^(-1*phi) )/
                    ( A_ce0.S^(-1*(1-alfa_1)*(1-epsilon)) )
                  )
  #Second we determine the equilibrium conditions for each sector
    #clean sector
       Labor_re.S<-(L.S*RelLabor.S)/(RelLabor.S+1) #based on the assumption that Labor_re.S+Labor_ce.S=L.S
   	   Price_re.S<-RelPrice.S/(RelPrice.S^(1-epsilon)+(1)^(1-epsilon))^(1/(1-epsilon)) #based on  Price_re.S^(1-epsilon)+(Price_ce.S)^(1-epsilon)=1 [checked many times and it is correct]
       Y_re.S<-((((alfa^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re))^(alfa/(1-alfa)))*Labor_re.S*A_re0.S
       Xtech_re.S<-((((alfa^2)*Price_re.S)/((1-Tec.subsidy.S)*epsi_re))^(1/(1-alfa)))*Labor_re.S*A_re0.S
       Profits_re.S<-(1+RD.subsidy.S)*Eta_re*epsi_re*((1-alfa)/alfa)*Xtech_re.S # Expected profits see annex IV. Equilibrium research profits
    #dirty sector
       Labor_ce.S<-L.S/(RelLabor.S+1)
       Price_ce.S<-Price_re.S/RelPrice.S
       Re<-(alfa_1^2/epsi_ce)^( alfa_1/(1-alfa) )*
           (alfa_2*A_ce0.S/cR)^((1-alfa_1)/(1-alfa))*
           Labor_ce.S*
           Price_ce.S^(1/(1-alfa))
       Y_ce.S<-( (alfa_1^2/epsi_ce)^( alfa_1/(1-alfa) ) )*
              ( (alfa_2*A_ce0.S/cR)^( alfa_2/(1-alfa) ) )*
              ( Price_ce.S^( alfa/(1-alfa) ) )*
              Labor_ce.S*
              A_ce0.S
#
Xtech_ce.S<- (
                         (
                          ( (alfa_1^2)*(Price_ce.S)*(Re^alfa_2) )/
                          ( epsi_ce )
                         )^( 1/(1-alfa_1) )
                        )*
                        ( Labor_ce.S^( (1-alfa)/(1-alfa_1) ) )*
                        ( A_ce0.S )
Profits_ce.S<-Eta_ce*epsi_ce*((1-alfa_1)/alfa_1)*Xtech_ce.S

    #Total Production
      Y.S<-((Y_re.S)^((epsilon-1)/epsilon)+(Y_ce.S)^((epsilon-1)/epsilon))^(epsilon/(epsilon-1))
#
#Allocation of Scientists
  s_re.S<-exp(Profits_re.S)/(exp(Profits_ce.S)+exp(Profits_re.S))
  s_ce.S<-1-s_re.S
#
#
  Y_re0.S
  Y_re.S
  Y_ce0.S
  Y_ce.S
  s_re.S
  s_ce.S
