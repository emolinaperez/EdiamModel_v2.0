#define vector for output
 #root<-"C:\\Users\\emolina\\Edmundo-RAND\\Projects\\Dissertation\\Model Development\\TechChange-RDM\\"
 root<-"E:\\Projects\\EaSM\\TechChange-RDM\\"

#this script has been created to fin the optimal value of policies for a future id,
  library(deSolve,lib=paste(root,"Rlibraries\\",sep=""))
  library(optimx,lib=paste(root,"Rlibraries\\",sep=""))
  
  dir.harness<-paste(root,"RDM Harness\\",sep="")
#Source Experimental Design
  dir.exp<-paste(root,"RDM Inputs\\",sep="")
  #experiment.version<-"Exp.design_climate_tests.csv"
  #experiment.version<-"Exp.design_p3.csv"
  experiment.version<-"Exp.design.csv"
 Exp.design<-read.csv(paste(dir.exp,experiment.version,sep=""))
#run the model once

#Source Model
  dir.model<-paste(root,"TechChange Model\\",sep="")
  #model.version<-"InternationalGreenTechChangeModel_7_12_2015_debug_mode.r"
  #model.version<-"InternationalGreenTechChangeModel_7_12_2015_cte_sy.r"
  #model.version<-"InternationalGreenTechChangeModel_9_19_2015.r"
  model.version<-"InternationalGreenTechChangeModel_10_22_2015_test.r"
  source(paste(dir.model,model.version,sep=""))
  
target.run<-3219
params<-c(
                         S.0=as.numeric(Exp.design[target.run,'S.0']), 
                         TimeStep=as.numeric(Exp.design[target.run,'TimeStep']), 
                         EndTime=as.numeric(Exp.design[target.run,'EndTime']), 
                         alfa=as.numeric(Exp.design[target.run,'alfa']), 
                         epsilon=as.numeric(Exp.design[target.run,'epsilon']),
                         Gamma.re=as.numeric(Exp.design[target.run,'Gamma.re']),
                         k.re=as.numeric(Exp.design[target.run,'k.re']), 
                         Gamma.ce=as.numeric(Exp.design[target.run,'Gamma.ce']), 
                         k.ce=as.numeric(Exp.design[target.run,'k.ce']), 
                         Eta.re=as.numeric(Exp.design[target.run,'Eta.re']), 
                         Eta.ce=as.numeric(Exp.design[target.run,'Eta.ce']), 
                         Nu.re=as.numeric(Exp.design[target.run,'Nu.re']), 
                         Nu.ce=as.numeric(Exp.design[target.run,'Nu.ce']), 
                         qsi=as.numeric(Exp.design[target.run,'qsi']), 
                         Delta.S=as.numeric(Exp.design[target.run,'Delta.S']),
						 Delta.Temp.Disaster=as.numeric(Exp.design[target.run,'Delta.Temp.Disaster']),
						 Beta.Delta.Temp=as.numeric(Exp.design[target.run,'Beta.Delta.Temp']),
						 CO2.base=as.numeric(Exp.design[target.run,'CO2.base']),
						 CO2.Disaster=as.numeric(Exp.design[target.run,'CO2.Disaster']),
                         labor.growth_N=as.numeric(Exp.design[target.run,'labor.growth_N']),
						 labor.growth_S=as.numeric(Exp.design[target.run,'labor.growth_S']),
                         lambda.S=as.numeric(Exp.design[target.run,'lambda.S']),
						 sigma.utility=as.numeric(Exp.design[target.run,'sigma.utility']),
						 rho=as.numeric(Exp.design[target.run,'rho']),						 
                         Yre.0_N=as.numeric(Exp.design[target.run,'Yre.0_N']),
                         Yce.0_N=as.numeric(Exp.design[target.run,'Yce.0_N']), 
                         Yre.0_S=as.numeric(Exp.design[target.run,'Yre.0_S']), 
                         Yce.0_S=as.numeric(Exp.design[target.run,'Yce.0_S']), 
						 size.factor=as.numeric(Exp.design[target.run,'size.factor']),
						 Run.ID= as.numeric(Exp.design[target.run,'Run.ID']),
						 policy.name = as.character(Exp.design[target.run,'policy.name']),
						 dir.harness=dir.harness)

optimx(c(st.0,d.0,ceN.0,ceS.0,tN.0,sN.0,tGF.0,tS.0,sS.0,sGF.0), TechChangeMod, lower=c(st.m,d.m,ceN.m,ceS.m,tN.m,sN.m,tGF.m,tS.m,sS.m,sGF.m), upper=c(st.M,d.M,ceN.M,ceS.M,tN.M,sN.M,tGF.M,tS.M,sS.M,sGF.M),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01),parscale=c(1,10,10,10,3,3,3,3,3,3),maxit=200000),params=params)

						 
optimx(c(0.033,0.2,0.25,0.25), TechChangeMod, lower=c(0.03,0.1,0.01,0.01), upper=c(0.5,1.0,0.5,0.5),method="L-BFGS-B",control = list(fnscale = -1,ndeps=c(0.01,0.1,0.01,0.01),maxit=200000),params=params)

						 
TechChangeMod(c(9/300,1.0,0.247492886,0.241384843,0.0501494,1.0,0.049117283,0.098234565,0.999997892,0.499998946),params)



TechChangeMod(c(0.03227914,0.5000188,0.3484284,0.3484284),params)

optimx(c(0.033,0.5,0.25,0.25), TechChangeMod, lower=c(0.03,0.01,0.01,0.01), upper=c(0.5,1.0,0.5,0.5),method="L-BFGS-B",control = list(fnscale = -1,parscale=c(0.1,5,10,10),maxit=200000),params=params)

dir.harness<-paste(root,"RDM Harness\\",sep="")

#test1
TechChangeMod(c(0.04803707,1,0.5,0.3459613),params)
test1<-read.csv(paste(dir.harness,"output_run_",target.run,".csv",sep=""))
test1$Run.ID<-NULL
#test2
TechChangeMod(c(0.04803707,0.2,0.5,0.3459613),params)
test2<-read.csv(paste(dir.harness,"output_run_",target.run,".csv",sep=""))
test2$Run.ID<-NULL

 0.07161492   
 
TechChangeMod(c(0.07161492,0.01,0.249954,0.2495828),params)

optimx(c(0.033,0.5,0.25,0.25), TechChangeMod, lower=c(0.03,0.01,0.01,0.01), upper=c(0.5,1.0,0.5,0.5),method="L-BFGS-B",control = list(fnscale = -1,maxit=1000),params=params)


#RDnordhaus
TechChangeMod(c(0.03,1.0,0.2,0.2,2.0,2.0),params)


#Exp.design[target.run,]						 
#optim(c(0.5,0.1,0.1), TechChangeMod, lower=c(0.0,0.0,0.0), upper=c(1,1,1), method="L-BFGS-B",control = list(fnscale = -1),params=params)
#optim(c(0.5,0.1), TechChangeMod, lower=c(0.0,0.0), upper=c(1,1), method="L-BFGS-B",control = list(fnscale = -1),params=params)
#optim(c(0.5,0.1), TechChangeMod, lower=c(0.0,0.0), upper=c(1,1), method="L-BFGS-B",control = list(fnscale = -1,maxit=200000,parscale=c(0.75,0.25)),params=params)

#methods for optimization: "L-BFGS-B", "spg"
optimx(c(0.5,0.1,0.1), TechChangeMod, lower=c(0.0,0.0,0.0), upper=c(1,1,1),method="L-BFGS-B",control = list(fnscale = -1),params=params)


#two issues seem to be important: 
  #1) we need to modify how reaching to high temperature affects the utility or the cost of doing that 
  #2) the optimization is not finding the global maximum, but a local maximum  

[[6]]
         p1       p2        p3    value fevals gevals niter convcode kkt1  kkt2 xtimes
L-BFGS-B  1 0.839516 0.2113513 517.1528     17     17    NA        0 TRUE FALSE 117.92


 TechChangeMod(c(0.5,0.839516,0.2113513),params)


[[8]]
         p1        p2        p3    value fevals gevals niter convcode kkt1  kkt2 xtimes
L-BFGS-B  1 0.5631934 0.2234534 517.1476     34     34    NA        0 TRUE FALSE 244.92
  
 TechChangeMod(c(0.5,0.5631934,0.2234534),params)



#Case1
 TechChangeMod(c(0.0,0.0,0.7),params)
#Case2
 TechChangeMod(c(10,10,1),params)

tax.case1<-read.csv(paste(dir.harness,"output_run_1.csv",sep=""))
tax.case2<-read.csv(paste(dir.harness,"output_run_1 - Copy.csv",sep=""))

tax.case1<-tax.case1[,c("time","Y_N","Y_S","Delta.Temp","Consumption_N","Consumption_S",
                  "Utility.Consumer_N","Utility.Consumer_S",
				  "Utility.Consumer.Total_N","Utility.Consumer.Total_S",
				  "Are.N","Ace.N","Are.S","Ace.S","Cost.S.Damage")]
				  
tax.case2<-tax.case2[,c("time","Y_N","Y_S","Delta.Temp","Consumption_N","Consumption_S",
                  "Utility.Consumer_N","Utility.Consumer_S",
				  "Utility.Consumer.Total_N","Utility.Consumer.Total_S",
				   "Are.N","Ace.N","Are.S","Ace.S","Cost.S.Damage")]
#tax.p5<-read.csv(paste(dir.harness,"output_run_5.csv",sep=""))
#tax.p5<-tax.p5[,c("time","Y_N","Y_S","Delta.Temp","Consumption_N","Consumption_S",
#                  "Utility.Consumer_N","Utility.Consumer_S",
#				  "Utility.Consumer.Total_N","Utility.Consumer.Total_S")]
				  
compare<-merge(tax.case1,tax.case2,by="time")				  			  
x<-case1
y<-case2

compare[,c("time","Ace.N.x","Ace.N.y","Ace.S.x","Ace.S.y")]
compare[,c("time","Y_N.x","Y_N.y","Y_S.x","Y_S.y")]
compare[,c("time","Consumption_N.x","Consumption_N.y","Consumption_S.x","Consumption_S.y")]
compare[,c("time","Delta.Temp.x","Delta.Temp.y")]
compare[,c("time","Cost.S.Damage.x","Cost.S.Damage.y")]
compare[,c("time","Utility.Consumer_N.x","Utility.Consumer_N.y",
                  "Utility.Consumer_S.x","Utility.Consumer_S.y")]
compare[,c("time","Utility.Consumer.Total_N.x","Utility.Consumer.Total_N.y",
                  "Utility.Consumer.Total_S.x","Utility.Consumer.Total_S.y")]


#compare years 82 and 83
 compare[compare$time%in%c(2082,2083),c("time","Y_N.x","Y_N.y","Y_S.x","Y_S.y")]
 compare[compare$time%in%c(2082,2083),c("time","Consumption_N.x","Consumption_N.y","Consumption_S.x","Consumption_S.y")]
 compare[compare$time%in%c(2082,2083),c("time","Cost.S.Damage.x","Cost.S.Damage.y")]
 compare[compare$time%in%c(2082,2083),c("time","Utility.Consumer_N.x","Utility.Consumer_N.y",
                  "Utility.Consumer_S.x","Utility.Consumer_S.y")]

# Analysis of Cost.S.Damage Function
 Delta.Temp.Disaster<-7.5
 lambda.S<-0.1443*5
 Delta.Temp.End.Century.Target<-Delta.Temp.Disaster-0.0 
# 
 Cost.Damage<-data.frame(Delta.Temp=seq(0,7.5,0.1))
 Cost.Damage$Cost.Damage<-((Delta.Temp.End.Century.Target-Cost.Damage$Delta.Temp)^lambda.S-lambda.S*Delta.Temp.End.Century.Target^(lambda.S-1)*(Delta.Temp.End.Century.Target-Cost.Damage$Delta.Temp))/((1-lambda.S)*Delta.Temp.End.Century.Target^lambda.S)
 Cost.Damage




Delta.Temp<-6
((Delta.Temp.Disaster-Delta.Temp)^lambda.S-lambda.S*Delta.Temp.Disaster^(lambda.S-1)*(Delta.Temp.Disaster-Delta.Temp))/((1-lambda.S)*Delta.Temp.Disaster^lambda.S)

t_disaster<-7.5
lambda<-0.1443/5
t<-7.5
((t_disaster - t)^lambda - lambda * t_disaster ^(lambda -1) * (t_disaster -t))/((1-lambda)*t_disaster ^lambda)



 
 
#Year=83
sigma.utility<-2.0
rho<-0.015
Consumption.N<-compare[compare$time==2083,"Consumption_N.x"]
Cost.S.Damage<-1
Consumption.N
(-1*((Cost.S.Damage*Consumption.N)^(1-sigma.utility))/(1-sigma.utility))/((1+rho)^70)

(((Cost.S.Damage*235.2951)^(1-sigma.utility))/(1-sigma.utility))/((1+rho)^70)



(1/(1-sigma.utility))*(1/(1+rho)^70)*(-1*235.2951)^(1-sigma.utility)

(1/(1-sigma.utility))*(1*235.2951)^(1-sigma.utility)


Util(j)=-(1/(1-sigma))*Teste3(j)*(phiS(S(j))*C(j))^(1-sigma




 #optim(c(0,0), fr,lower = c(0, 0))
#optim(c(0,0), fr, NULL, method = "BFGS", hessian = TRUE)

#this is how to do it with a function, one part are parameters, the other is the rest of the inputs, see x=x at the and, 
#x <- rnorm(100,1,1)
#optim(c(1,1), foo.unconstr, lower=c(0,0), upper=c(5,5), method="L-BFGS-B", x=x)
#constrOptim(c(1,1), foo.unconstr, grad=NULL, ui=u1, ci=c1, x=x)
#optim(c(0,0,0.5), fr, lower=c(0,0,0), upper=c(1,1,1), method="L-BFGS-B",control = list(fnscale = -1))




#  x <= 0.9,  y - x > 0.1
constrOptim(c(0.001,0.001), fr,NULL, ui = rbind(c(1,0), c(0,1)), ci = c(0,0),control = list(fnscale = -1),x=x)

fr(c(0.1,0,))

 

x1<-0.0001059265 
x2<-0.0001638046
fr(c(x1,x2))
fr(c(0.1,0))


