server_function_ediam<-
function(input, output) {
#Create the function with the model
   Ediam.Shiny.App<-function(tax.rate.N,
	                        tax.rate.S,
													epsi.re.subsidy.N,
													s.re.subsidy.N,
													epsi.re.subsidy.S,
													s.re.subsidy.S,
													alfa,
													epsilon,
													Gamma.re,
													Gamma.ce,
													Eta.re,
													Eta.ce,
													Nu.re,
													Nu.ce,
													Beta.Delta.Temp,
													CO2.base,
													Delta.S,
                          CO2.Concentration.0,
													lambda.S,
													sigma.utility,
													rho,
													Yre.0_N,
													Yre.0_S,
													Yce.0_N,
													Yce.0_S){

  policies<-c(
    #carbon tax
	    tax.rate.N=tax.rate.N,
      tax.rate.S=tax.rate.S,
    #Technology push in North
	    epsi.re.subsidy.N = epsi.re.subsidy.N,
	    s.re.subsidy.N = s.re.subsidy.N,
    #Traditional Green Climate Fund
	    epsi.re.subsidy.S = epsi.re.subsidy.S,
 	    epsi.re.GFsubsidy.N = 0,
	  #R&D Green Climate Fund
	    s.re.subsidy.S = s.re.subsidy.S,
	    s.re.GFsubsidy.N = 0)
  params<-c(
          CO2.Concentration.0 = CO2.Concentration.0, #
          TimeStep = as.numeric(5),#
          EndTime = as.numeric(300), #
          alfa = alfa,
          epsilon = epsilon,
          Gamma.re = Gamma.re,
          k.re = as.numeric(0.0),#
          Gamma.ce = Gamma.ce,
          k.ce = as.numeric(0.0),#
          Eta.re = Eta.re,
          Eta.ce = Eta.ce,
          Nu.re = Nu.re,
          Nu.ce = Nu.ce,
          qsi = as.numeric(0.0100539),#
          Delta.S = Delta.S,
          Delta.Temp.Disaster = as.numeric(6.0),#
          Beta.Delta.Temp = Beta.Delta.Temp,
          CO2.base = CO2.base,
          labor.growth_N = as.numeric(0),#
          labor.growth_S = as.numeric(0),#
          lambda.S = lambda.S,
          sigma.utility = sigma.utility,
          rho = rho,
          Yre.0_N = Yre.0_N,
          Yce.0_N = Yce.0_N,
          Yre.0_S = Yre.0_S,
          Yce.0_S = Yce.0_S,
          size.factor = as.numeric(4.0),#
          Run.ID = as.numeric(1.0)
          )

#source model
#  dir.model<-"C:\\Users\\L03054557\\Edmundo-ITESM\\Projectos\\Disertation\\Model Disertation\\TechChange Model\\"
#  model.version<-"InternationalGreenTechChangeModel_07_07_2016.r"
#  source(paste(dir.model,model.version,sep=""))
#run simulation
  out<-TechChangeMod(policies,params)
}

#Tell the server how to use the EDIAM app
 out<-reactive({Ediam.Shiny.App(input$tax.rate.N,
	                             input$tax.rate.S,
															 input$epsi.re.subsidy.N,
															 input$s.re.subsidy.N,
															 input$epsi.re.subsidy.S,
															 input$s.re.subsidy.S,
		 													 input$alfa,
															 input$epsilon,
															 input$Gamma.re,
															 input$Gamma.ce,
															 input$Eta.re,
															 input$Eta.ce,
															 input$Nu.re,
															 input$Nu.ce,
															 input$Beta.Delta.Temp,
															 input$CO2.base,
                               input$Delta.S,
                               input$CO2.Concentration.0,
															 input$lamda.S,
															 input$sigma.utility,
															 input$rho,
															 input$Yre.0_N,
															 input$Yre.0_S,
															 input$Yce.0_N,
															 input$Yce.0_S)})

#Source output plots
#define theme of plots
theme_ed<- theme( panel.border = element_rect(fill = NA, colour = "grey", size = 1),
				          text=element_text(family="Arial"),
				          #style of title
				          plot.title = element_text(size=16),
				          #style axis titles
				          axis.title.x = element_text(size=14),
				          axis.title.y = element_text(size=14),
  			          #style axis
				          axis.text.x = element_text(size=12),
				          axis.text.y = element_text(size=12),
				          panel.grid.major = element_blank(),
				          panel.grid.major.x = element_blank(),
				          panel.grid.minor = element_blank(),
				          panel.grid.minor.x = element_blank())

# Define id vars of runs
id.vars<-c("time")

#create function to use data by region
#measure.vars<-c("Consumption_N","Consumption_S","Decarb.Y_N","Decarb.Y_S","Relative.A_N","Relative.A_S","sre_N","sre_S")
data.by.region<-function(data,id.vars,measure.vars)
{
	data<-data[,c(id.vars,measure.vars)]
	data<-melt(data, id.vars=id.vars, measure.vars=measure.vars,  variable.name="Variable.Region")
	data<-cbind(data,colsplit(data$Variable.Region,("_"),c("Variable","Region")))
	data$Variable.Region<-NULL
	new.form<-as.formula(paste(id.vars,"Region~Variable",sep="+"))
	data<-dcast(data,new.form, value.var="value")
	data$Region<-ifelse(data$Region=="N","Advanced Region","Emerging Region")
	return(data)
}

# good websites for ggplot support
# http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations
# http://www.sthda.com/english/download/3-ebooks/5-ggplot2-guide-to-create-beautiful-graphics-in-r
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour


# Temperature Anomaly Growth
output$Plot1 <- renderPlot({ggplot(out(), aes(time,Delta.Temp))+ geom_line()+geom_line(size=1)+
                            geom_hline(yintercept = 2.0,linetype = 2)+geom_hline(yintercept = 6.0,linetype = 2)+
                            ggtitle("Global Temperature Anomaly With Respect to Historic Mean")+
                            scale_x_continuous(name="Time [Years]", breaks=seq(0,300,by=50)) +
                            scale_y_continuous(name="Degrees Celsius",limits=c(0, 6.0))+theme_ed})
#
#Graph Consumption
output$Plot2<-renderPlot({ggplot(data.by.region(out(),id.vars,c("Consumption_N","Consumption_S")), aes(time,Consumption,colour =Region)) + geom_line()+geom_line(size=1)+
                          ggtitle("Consumers Gross Consumption")+
                          scale_x_continuous(name="Time [Years]", breaks=seq(0,300,by=50)) +
			                    scale_y_continuous(name="GDP US Dollars")+theme_ed})

#Graph Rate of decarbonization
output$Plot3<-renderPlot({ggplot(data.by.region(out(),id.vars,c("Decarb.Y_N","Decarb.Y_S")), aes(time,Decarb.Y,colour =Region)) + geom_line()+geom_line(size=1)+
                          ggtitle("Share of Fossil Fuels Used in Energy Production")+
                          scale_x_continuous(name="Time [Years]", breaks=seq(0,300,by=50)) +
                          scale_y_continuous(name="Percent of Total", breaks=seq(0,1,by=0.20),limits=c(0, 1.0),labels=percent)+theme_ed})

#Graph Relative Productivity
output$Plot4<-renderPlot({ggplot(data.by.region(out(),id.vars,c("Relative.A_N","Relative.A_S")), aes(time,Relative.A,colour =Region)) + geom_line()+ geom_line(size=1)+
		                      ggtitle("Relative Productivity of Renewable Energy Technologies")+
		                      scale_x_continuous(name="Time [Years]", breaks=seq(0,300,by=50)) +
													scale_y_continuous(name="RETs/FETs",labels=percent)+theme_ed})

#Graph Scientists
output$Plot5<-renderPlot({ggplot(data.by.region(out(),id.vars,c("sre_N","sre_S")), aes(time,sre,colour =Region)) + geom_line()+geom_line(size=1)+
                         ggtitle("Share of Scientits in Renewable Energy R&D")+
                         scale_x_continuous(name="Time [Years]", breaks=seq(0,300,by=50)) +
                         scale_y_continuous(name="Percent of Total", breaks=seq(0,1,by=0.20),limits=c(0, 1.0),labels=percent)+theme_ed})
#Phase diagram

output$Plot6<-renderPlot({ggplot(out(), aes(Relative.A_N,change.rate.Are.N)) + geom_line()+geom_line(size=1)+theme_ed})
output$Plot7<-renderPlot({ggplot(out(), aes(Relative.A_S,change.rate.Are.S)) + geom_line()+geom_line(size=1)+theme_ed})


#Policy Vectors
#Carbon Tax
output$Plot10<-renderPlot({ggplot(data.by.region(out(),id.vars,c("ce.tax_N","ce.tax_S")), aes(time,ce.tax,colour =Region)) + geom_line()+geom_line(size=1)+
                          ggtitle("Carbon Tax")+
                          scale_x_continuous(name="Time [Years]", breaks=seq(0,300,by=50)) +
			                    scale_y_continuous(name="Percent Markup",limits=c(0, 1.0))+theme_ed})
#Tech Subsidy
output$Plot11<-renderPlot({ggplot(data.by.region(out(),id.vars,c("Tec.subsidy_N","Tec.subsidy_S")), aes(time,Tec.subsidy,colour =Region)) + geom_line()+geom_line(size=1)+
                          ggtitle("Technology subsidy")+
                          scale_x_continuous(name="Time [Years]", breaks=seq(0,300,by=50)) +
			                    scale_y_continuous(name="Percent Markup",limits=c(0, 1.0))+theme_ed})

# R&D Subsidy
output$Plot12<-renderPlot({ggplot(data.by.region(out(),id.vars,c("RD.subsidy_N","RD.subsidy_S")), aes(time,RD.subsidy,colour =Region)) + geom_line()+geom_line(size=1)+
                          ggtitle("R&D Subsidy")+
                          scale_x_continuous(name="Time [Years]", breaks=seq(0,300,by=50)) +
			                    scale_y_continuous(name="Percent Markup",limits=c(0,3.0))+theme_ed})
}
