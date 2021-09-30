#load the model into the R session
#Load required libraries for the ediam model
 library(deSolve)
 library(shiny)
 library(ggplot2)
 library(Rmisc)
 library(scales)
 library(extrafont)
 library(data.table)
 library(reshape2)
#Load the Ediam model to your R session
#specify directory where you have saved the model
  #dir.model<-"C:\\Users\\L03054557\\Edmundo-ITESM\\Proyectos\\Disertation\\Model Disertation\\TechChange Model\\"
  dir.model<-"C:\\Users\\Administrator\\Documents\\Edmundo\\Projectos\\TechChange-RDM\\TechChange Model\\"
#specify the model version you are using
  model.version<-"ediam_10_12_2016.r"
#load the model in your session
 source(paste(dir.model,model.version,sep=""))
#policies has 9 dimensiones
#test run
policies1<-c(1,1,0.7,4,0.7,0,4,0,0.01)
#policies2<-c(0,0,0,0,0,0,0,0,0.0)
ediam(policies1)

out<-ediam(policies1)
head(out)

#Let's try all with a cluster
#option1: Snow
 library(snow)
 #nCore<-70 #0 hours 44 minutes and 41 seconds
 nCore<-40 #0 hours 20 minutes and 33 seconds
 #nCore<-60 #26 minutes and 47 seconds
 #nCore<-40 #0 hours 40 minutes and 43 seconds


 cl <- makeSOCKcluster(names = rep('localhost',nCore))
 global.elements<-list("ediam","ode","genoud")
 clusterExport(cl,global.elements,envir=environment())

#option2: parallel

#we want to optimize policies
library(rgenoud)

genoud(ediam,max=TRUE,nvars=9,
       starting.values=c(0,0,0,0,0,0,0,0,0),
       pop.size=1000,
       Domains=matrix(c(0,0,0.0,0,0.0,0.0,0,0.0,0.0,
                        1,1,0.7,4,0.7,0.7,4,0.7,0.1),ncol=2),
       cluster=cl,
       print.level=0)

stopCluster(cl)

#I need to test the model, I am getting NA's and that sucks, big time because the optimization is getting all bad,

#create a latin hypercube sample

 library(lhs)
 set.seed(5000)
 sample.size<-10000
 lhs.sample<-data.frame(randomLHS(sample.size,9),Run.ID=1:sample.size)
 lhs.sample$X9<-lhs.sample$X9/10
#specify directory where you have saved the model
library(deSolve)

   dir.model<-"C:\\Users\\L03054557\\Edmundo-ITESM\\Proyectos\\Disertation\\Model Disertation\\TechChange Model\\"
 #specify the model version you are using
   model.version<-"ediam_10_12_2016.r"
 #load the model in your session
  source(paste(dir.model,model.version,sep=""))

 nCore<-8
 cl <- makeSOCKcluster(names = rep('localhost',nCore))
 global.elements<-list("ediam","ode")
 clusterExport(cl,global.elements,envir=environment())


test.out<- parApply(cl,lhs.sample,1,function(x) {value<-ediam(c(as.numeric(x['X1']),
                                                    as.numeric(x['X2']),
                                                    as.numeric(x['X3']),
                                                    as.numeric(x['X4']),
                                                    as.numeric(x['X5']),
                                                    as.numeric(x['X6']),
                                                    as.numeric(x['X7']),
                                                    as.numeric(x['X8']),
                                                    as.numeric(x['X9'])));
                                        out<-data.frame(Run.ID=x['Run.ID'],Value=value)
                                        return(out)})
#
stopCluster(cl)

test.out<-do.call("rbind",test.out)

#now let's try to understand invidual problematic traces
#load the model in your session
 source(paste(dir.model,model.version,sep=""))
target.id<-6440
x<-subset(lhs.sample,lhs.sample$Run.ID==target.id)
policies<-c(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9])

#with genoud
policies<-signif(c(0.636431287,0.378830521,0.344954471,3.659474438,0.331589117,0.068499898,0.444998176,0.569464605,0.006498925),2)

test<-ediam(policies,verbose=TRUE)



test$Growth.Rate_N<-diff(test$Consumption_N)/test$Consumption_N[1:(length(test$Consumption_N)-1)]


test$Growth.Rate_S<-diff(test$Consumption_S)

#No cta eladio:, es mejor hacerlo del celular
#0194882563


#Test new model changes
  library("deSolve")
  dir.model<-"C:\\Users\\L03054557\\Edmundo-ITESM\\Proyectos\\Disertation\\Model Disertation\\TechChange Model\\Ediam_v21_08_2017\\"
#specify the model version you are using
  source(paste(dir.model,"ediam_Main.r",sep=""))
  policies<-c(0.457298,0.1386218,0.5352016,0.3710303,0.3282335,0.2285067,0.3489157,0.2420963,0.01124358)
  ediam(policies,dir.model,verbose=TRUE)

#with genoud this is what I find:
  $value
  [1] 46.37826

  $par
  [1] 0.636431287 0.378830521 0.344954471 3.659474438 0.331589117 0.068499898 0.444998176 0.569464605 0.006498925


#This links tell you how to run a windows app from R
 #http://stackoverflow.com/questions/19404270/run-vba-script-from-r
 #http://stackoverflow.com/questions/2050505/way-to-run-excel-macros-from-command-line-or-batch-file
