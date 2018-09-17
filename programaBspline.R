#install.packages("shiny", dependencies = TRUE)
library(shiny)

### sudo apt-get install r-cran-ggplot2 libcairo2-dev (LINUX)
#install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

epsilom <<- 1.0e-8
############################################################
###
### FUNCIONES AUXILIARES
###
############################################################
source("pantallaEnBlanco.R")

source("dibujaDatos.R")
source("dibujaBspline.R")
source("dibujaFuncionesBase.R")

source("calculaLimites.R")

source("creaBspline.R")
source("creaBsplineDerivada.R")

source("calculaNudos.R")
source("calculaNudosUniforme.R")
source("calculaNudosNoUniforme.R")

source("calculaPuntosControl.R")
source("dame_AlfaBetaGamma.R")
source("dame_LambdaDelta.R")
source("dame_X.R")

source("calculaPuntosControlDerivada.R")

source("muestraDatos.R")
source("muestraNudos.R")
source("muestraPuntosControl.R")

source("puntosEnCurva.R")
source("puntosEnFuncionesBase.R")

source("N.R")

############################################################
###
### USER INTERFACE
###
############################################################

ui <- shinyUI(fluidPage(
	sidebarLayout(fluid = TRUE,
	    mainPanel(width = 12,
	      	tabsetPanel(
	        	tabPanel("Datos", plotOutput("plot0", click="plot_click")),
	        	tabPanel("Bspline", plotOutput("plot1")), 
	        	tabPanel("Derivada", plotOutput("plot2")), 
	        	tabPanel("FBase_Bspline", plotOutput("plot3")),
	        	tabPanel("FBase_Bspline_Derivada", plotOutput("plot4"))
	      	)
	    ),
	    sidebarPanel(width = 6,
	      	radioButtons("tipo", label = "Tipo", choices = list("Uniforme" = 1, "Chord Length" = 2, "Centripeta" = 3), selected = 1, inline=T),
	      	actionButton("do", "Create"),
	      	actionButton("new", "New")
	    )
	)
))

############################################################
###
### SERVER
###
############################################################

server <- shinyServer(function(input, output) {
                info <- reactiveValues(
                    click  = NULL,
                    inicio = TRUE,
                    dibuja = FALSE,
                    datos = NULL,
                    bspline = NULL,
                    derivada = NULL
                )
              
                ### Click Mouse
                observeEvent(input$plot_click, {
                    if (is.null(info$click)) {
                        info$click  <- input$plot_click
                        info$dibuja <- TRUE
                    } else {
                        info$click <- NULL
                    }
                })
              
                ### Boton Create
                observeEvent(input$do, {
                    info$bspline  <- creaBspline(info$datos, 3, input$tipo)
                    info$derivada <- creaBsplineDerivada(info$bspline)
                })
                
                ### Boton NEW
                observeEvent(input$new, {
                    info$click  <- NULL
                    info$inicio <- TRUE
                    info$dibuja <- FALSE
                    info$datos <- NULL
                    info$bspline <- NULL
                    info$derivada <- NULL
                })
            
                ### Recogida de datos
                output$plot0 <- renderPlot({
                    if (info$dibuja == TRUE) {
                        new_data <- data.frame(x=info$click$x, y = info$click$y)
                        if (info$inicio == TRUE) {
                            info$datos <- new_data
                        } else {
                            info$datos <- rbind(info$datos, new_data)
                        }
                        info$dibuja <- FALSE
                        info$click  <- NULL
                        info$inicio <- FALSE
                    }
                    if (info$inicio == TRUE) {
                        pantallaEnBlanco()
                    } else {
                        dibujaDatos(info$datos)
                    }
                 })
    
                ### Dibujamos bspline
                output$plot1 <- renderPlot({
                		dibujaBspline(info$bspline, info$datos)
                })
              
                ### Dibujamos la derivada de la bspline
                
              	output$plot2 <- renderPlot({
              	    print(info$derivada)
                  	dibujaBspline(info$derivada, NULL)
                })
              
                ### Dibujamos las funciones base de la bspline
              	output$plot3 <- renderPlot({
                  	dibujaFuncionesBase(info$bspline)
                })
              
                ### Dibujamos las funciones derivadas de la bspline
              	output$plot4 <- renderPlot({
                  	dibujaFuncionesBase(info$derivada)
              	})
})

############################################################
###
### APLICACION
###
############################################################

shinyApp(ui, server)