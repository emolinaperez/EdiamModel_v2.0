#This is the user interface for the EDIAM model developed by Dr. Edmundo Molina (edmundo.molina@itesm.mx)
user_interface_ediam<-
fluidPage(
	             theme="simplex.min.css",
				       tags$style(type="text/css","label {font-size: 12px;}",".recalculating {opacity: 1.0;}"),
				# Application title
			         tags$h2("International Technological Change and Climate Policy"),
				       p("An interactive tool to use the Exploratory Dynamic Integrated Assesment Model (EDIAM) developed by",
               tags$a(href="http://www.edmundomolinamx.org/", "Dr. Edmundo Molina")),
				       hr(),
	      #create the sliders for policy parameters

				#Create Panel with Input values
                fluidRow(column(3,
									         fluidRow(column(6, tags$h3("Policy Inputs"))),
				                   wellPanel(
					                          fluidRow(
						                                column(6,
							                                       sliderInput("tax.rate.N", "Carbon Tax AR", min = 0, max = 1, value = 0,step=0.1),
							                                       sliderInput("epsi.re.subsidy.N", "Technology Subsiy AR", min = 0, max = 1, value = 0, step = 0.1),
							                                       sliderInput("s.re.subsidy.N", "R&D Subsidy ER", min = 0.0, max = 3.0, value = 0.0, step = 0.1)
						                                        ),
						                                column(6,
							                                       sliderInput("tax.rate.S", "Carbon Tax ER", min = 0, max = 1, value = 0,step=0.1),
							                                       sliderInput("epsi.re.subsidy.S", "Technology Subsiy ER", min = 0, max = 1, value = 0, step = 0.1),
							                                       sliderInput("s.re.subsidy.S", "R&D Subsidy ER", min = 0.0, max = 3.0, value = 0.0, step = 0.1)
							                                     )
					                                     ),
					                                  p(actionButton("x","Re-run simulation", icon("random")))
					                              ),
														 fluidRow(column(6, tags$h3("Model Parameters"))),
														 wellPanel(
																			fluidRow(
																				      h4("Uncertain Parameters"),
																							h5("Technological Uncertainty"),
																							column(6,
																											 sliderInput("Gamma.re", "R&D Returns SETs", min = 0.0, max = 1.0, value = 0.25, step = 0.05),
																											 sliderInput("Eta.re", "Innovation Propensity SETs", min = 0.01, max = 0.026, value = 0.02, step = 0.01),
																											 sliderInput("Nu.re", "Transferability SETs", min = 0.01, max = 0.026, value = 0.02, step = 0.01)
																											),
																							column(6,
																								       sliderInput("Gamma.ce", "R&D Returns FETs", min = 0.0, max = 1.0, value = 0.25, step = 0.05),
																											 sliderInput("Eta.ce", "Innovation Propensity FETs", min = 0.01, max = 0.026, value = 0.02, step = 0.01),
																											 sliderInput("Nu.ce", "Transferability FETs", min = 0.01, max = 0.026, value = 0.02, step = 0.01)
																										 ),
																							h5("Climate Change Uncertainty"),
																							column(6,
																											 sliderInput("Beta.Delta.Temp", "Climate Sensitivity to GHG", min = 3.0, max = 6.5, value = 5.0, step = 0.10)
																											),
																							column(6,
																								       sliderInput("Delta.S", "Carbon Sink Capacity", min = 0.0005, max = 0.0035, value =0.001822767, step = .0002)
																										 ),
																							 h5("Economic Uncertainty"),
	 																						 column(6,
	 																											 sliderInput("epsilon", "Elasticity of Substitution", min = 3.0, max = 10.0, value = 5.0, step = 0.5)
	 																											),
	 																						 column(6,
	 																								       sliderInput("rho", "Discount Rate", min = 0.001, max = 0.015, value = 0.008, step = 0.001)
	 																										 )
	 																								 ),
																					fluidRow(
																						      h4("Fixed Parameters"),
																									column(6,
																													 numericInput("alfa", "alfa", value = 0.33),
																													 numericInput("lambda.S", "lambda.S", value = 0.1443 ),
																													 numericInput("CO2.base", "CO2.base",value = 289.4150457),
																													 numericInput("CO2.Concentration.0", "CO2.Concentration.0",value = 382.2461),
																										       numericInput("sigma.utility", "sigma.utility", value = 2.0)
																													),
																									column(6,
																										       numericInput("Yre.0_N", "Yre.0_N", value = 45.55074),
																										       numericInput("Yre.0_S", "Yre.0_S", value = 27.82166),
																										       numericInput("Yce.0_N", "Yce.0_N", value = 193.2),
																										       numericInput("Yce.0_S", "Yce.0_S", value = 257.54634)
																												 )
																										 ),
																					p(actionButton("x","Re-run simulation", icon("random")))
																					)
															     ),
													column(6,
                                 fluidRow(column(6, tags$h3("Simulation Output"))),
																 tabsetPanel(type = "tabs",
															                      tabPanel("Temperature Anomaly",plotOutput("Plot1",height = "600px")),
																 	                  tabPanel("Consumers Consumption",plotOutput("Plot2",height = "600px")),
																										tabPanel("Use of Fossil Fuels",plotOutput("Plot3",height = "600px")),
																										tabPanel("Relative Productivity RETs",plotOutput("Plot4",height = "600px")),
																										tabPanel("Entrepreneurs R&D",plotOutput("Plot5",height = "600px")),
																										tabPanel("Phase Diagram",plotOutput("Plot6",height = "600px"))
																              )
														          ),
													column(3,
														      fluidRow(column(6, tags$h3("Policy Vectors"))),
																	tabsetPanel(type = "tabs",
																										 tabPanel("Carbon Tax",plotOutput("Plot10",height = "300px")),
																										 tabPanel("Technology Subsidy",plotOutput("Plot11",height = "300px")),
																										 tabPanel("R&D Subsidy",plotOutput("Plot12",height = "300px"))
																							 )
																	)
										     )
						   )
