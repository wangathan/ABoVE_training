#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(lubridate)
library(shiny)
library(raster)
library(rgdal)
library(rgeos)
library(data.table)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(h1("ABoVE Training Data!")),
   
   fluidRow(
   	column(width = 5,
	   	wellPanel(
	   		fluidRow(
	   			textInput(inputId = "user",
	   								label = "What's your name?",
	   								value = "<enter name here>"),
	   			textOutput("diag"),
	   			textOutput("aYear")
	   		),
	   		fluidRow(
	   		  column(width = 6, uiOutput("tilepick")),
	   		  column(width = 6, uiOutput("samplepick"))
	   		  #tableOutput("isInDatNull")
	   		),
	   		fluidRow(
	   			h3("What is the land cover type? Click one button from each row")
	   		),
	   		fluidRow(
	   			hr(),
	   			p("SURFACE TYPE"),
	   			actionButton(inputId = "isWater",
	   									 label="Water"),
	   			actionButton(inputId = "isVeg",
	   									 label = "Vegetated"),
	   			actionButton(inputId = "isBare",
	   									 label = "Barren"),
	   			actionButton(inputId = "isIce",
	   			             label = "Permanent Snow/Ice"),
	   			actionButton(inputId = "resetSurface",
	   			             label = "Reset")
	   		),
	   		fluidRow(
	   			hr(),
	   			p("VEG FORM"),
	   			actionButton(inputId = "isMoss",
	   									 label = "Moss / Lichen"),
	   			actionButton(inputId = "isGrass",
	   									 label = "Grassland"),
	   			actionButton(inputId = "isShrub",
	   									 label = "Shrubland"),
	   			actionButton(inputId = "isTree",
	   									 label = "Forest"),
	   			actionButton(inputId = "resetVeg",
	   			             label = "Reset")
	   		),
				fluidRow(
					hr(),
					p("PHENOTYPE"),
					actionButton(inputId = "DBF",
											 label = "Deciduous"),
					actionButton(inputId = "ENF",
											 label = "Evergreen"),
					actionButton(inputId = "MXF",
											 label = "Mixed"),
					actionButton(inputId = "resetPheno",
					             label = "Reset")
				),
				# fluidRow(
				#   hr(),
				#   p("LEAF TYPE"),
				#   actionButton(inputId = "isBroad",
				#                label = "Broadleaf"),
				#   actionButton(inputId = "isNeedle",
				#                label = "Needleleaf"),
				#   actionButton(inputId = "isMixedLeaf",
				#                label = "Mixed Leaf"),
				#   actionButton(inputId = "resetLeaf",
				#                label = "Reset")
				# ),
	   		fluidRow(
	   			hr(),
	   			p("DENSITY"),
	   			actionButton(inputId = "isSparse",
	   			             label = "Sparse"),
	   			actionButton(inputId = "isOpen",
	   									 label = "Open"),
	   			actionButton(inputId = "isDense",
	   									 label = "Dense"),
	   			actionButton(inputId = "resetDensity",
	   			             label = "Reset")
	   		),fluidRow(
	   		  hr(),
	   		  p("UNDERSTORY (if open/sparse forest)"),
	   		  actionButton(inputId = "isUnderBare",
	   		               label = "Bare"),
	   		  actionButton(inputId = "isUnderMoss",
	   		               label = "Moss/Lichen"),
	   		  actionButton(inputId = "isUnderVasc",
	   		               label = "Grass/Shrub"),
	   		  actionButton(inputId = "isUnderWater",
	   		               label = "Water"),
	   		  actionButton(inputId = "resetUnder",
	   		               label = "Reset")
	   		),
	   		fluidRow(
	   			hr(),
	   			p("WETLAND?"),
	   			actionButton(inputId = "isWet",
	   									 label = "Wetland"),
	   			actionButton(inputId = "isDry",
	   									 label = "Not Wetland"),
	   			actionButton(inputId = "isSubmerged",
	   			             label = "Submerged"),
	   			actionButton(inputId = "resetWet",
	   			             label = "Reset")
	   		),
				fluidRow(
				  hr(),
				  p("LAND USE"),
				  actionButton(inputId = "isUrban",
				               label = "Urban"),
				  actionButton(inputId = "isAgriculture",
				               label = "Agriculture"),
				  actionButton(inputId = "isPasture",
				               label = "Pasture"),
				  actionButton(inputId = "isTimber",
				               label = "Timber"),
				  actionButton(inputId = "isRecovering",
				               label = "Recovering"),
				  actionButton(inputId = "resetLandUse",
				               label = "Reset")
				),
				fluidRow(
				  hr(),
				  p("CONFIDENCE IN LABEL"),
				  actionButton(inputId = "lowConfidence",
				               label = "Low"),
				  actionButton(inputId = "medConfidence",
				               label = "Medium"),
				  actionButton(inputId = "highConfidence",
				               label = "High"),
				  actionButton(inputId = "resetConfidence",
				               label = "Reset")
				),
	   		fluidRow(
	   			hr(),
	   			div(tableOutput("trainingRow"), style = "font-size:70%"),
	   			br(),
	   			div(tableOutput("legendRow"), style = "font-size:70%")
	   		),
				fluidRow(
				  br(),
				  br(),
				  hr(),
				  actionButton(inputId = "save",
				               label = "Submit Label"),
				  actionButton(inputId = "skip",
				               label = "Skip Labelling")
				),
				fluidRow(
				  br(),
				  radioButtons(inputId = "skipBox",
				               label = "Why skip this label?",
				               choices = list("Not Skip", "Insufficient Imagery","Mixed Pixel", "Unclear", "Misregistration", "Shadow", "I'm Lazy", "Other"))
				)
	   	)
	   	),
   	column(width=7,
   	wellPanel(
   		fluidRow( ## Landsat data display
   		  h3("Landsat data"),
   		  radioButtons(inputId = "LSband", label="LS Band", inline=T, 
   		               choices = c("blue", "grn", "red", "nir", "swir1", "swir2", "bt", "ndvi", "ndwi", "lswi", "evi-nbr", "tcb", "tcg", "tcw", "tcw-tcg")),
   		  column(width=6,
   		         radioButtons(inputId = "LSplotType", label="Plot Type", inline=T, 
   		               choices = c("Years", "DoY"))),
   		 column(width=6,
   		         sliderInput("yearRange", label = "Year Range", min = 1984, max = 2016, value = c(1984, 2016))),
   		  plotOutput("LSplot", height=330)
   		),
   		br(),
   		br(),
   		br(),
   		br(),
   		br(),
   		br(),
   		fluidRow( # Console for data selection
   		  column(width = 4,
   		         radioButtons(inputId = "falseCol", label="Band Composite", choices=c("321","432"))),
   		  column(width = 4,
   		         radioButtons(inputId = "zoomChoice", label="Zoom Choice", choices=c("z321","z432", "elev", "aspect", "slope"))),
   		  column(width = 4,
   		         uiOutput("filepick"),
   		         textOutput("coords"),
   		         textOutput("coords_aea"),
   		         textOutput("wetcomb"))
   		),
   		br(),
   		fluidRow(
   			column(width = 6,
   						 h3("RGB"),
   						 plotOutput("RGBplot"),height = 500),
   			column(width = 6,
   						 h3("NDVI"),
   						 plotOutput("NDVIplot"),height=500)
   		),
   		hr(),
   		fluidRow(
   			column(width=6,
   						 h3("Zoom"),
   						 plotOutput("zoomPlot"),height=300),
   			column(width=6,
   						 h3("Panchromatic"),
   						 plotOutput("panPlot"),height=300)
   		)
   	)
   )
	)
)

# reactive values:
# isLand, isWater, 
# isVeg, isBare, isUrban,
# isTree, isMoss, isGrass, isShrub,
# isOpen, isDense,
# isWet, isDry

# needed outputs:
# RGBplot
# zoomPlot
# panPlot

server <- function(input, output, session) {

  #setwd("F:/Dropbox/LCSC/ABoVE/ABoVE_training/")
  #setwd("C:/Users/wanga/Dropbox/LCSC/ABoVE/buildTraining/")
  setwd("D:/Dropbox/LCSC/ABoVE/buildTraining/")
  #setwd("/Users/leticia/Dropbox/LCSC/ABoVE/buildTraining")
  
  availableTiles = reactive({
    
    # generate list of available ABoVE tiles based on folder contents
    tileShapes = list.files("../../../ABoVE_samples/shapefiles")
    tileShapes = tileShapes[!grepl("zip",tileShapes)]
    
    tileSamplesIn = list.files("../../../ABoVE_samples/stamps",
                               pattern="EOSD")
    tileSamples = sapply(strsplit(tileSamplesIn, "_"), "[[",2)
    
    # don't list completed tiles
    tilesDoneIn = list.files("../buildTraining/completedDt/")
    tilesDone = sapply(strsplit(tilesDoneIn, "_"),"[[",1)
    #tileSamples = tileSamples[!tileSamples %in% tilesDone]
    
    tileLSIn = list.files("../../../ABoVE_samples/LS",
                          pattern="filtered")
    tileLS = sapply(strsplit(tileLSIn,"_"), "[[",1)
    
    #asterIn = list.files("../../../ABoVE_samples/ASTER")
    
    # Get the intersection and only present tiles that have all their data set up
    #return(Reduce(intersect, list(tileShapes, tileSamples, tileLS,asterIn)))
    return(Reduce(intersect, list(tileShapes, tileSamples, tileLS)))
  })
  
  output$tilepick = renderUI({
    selectInput(inputId = "tilepick",
                label = "Select Tile",
                choices = availableTiles(),
                selected="Bh11v11")
  })
  
  inData = reactive({
    
    if(!is.null(input$tilepick)){
      tifPath  = paste0("../../../ABoVE_samples/stamps/EOSD_",input$tilepick, "_sample")
      asterPath = paste0("../../../ABoVE_samples/ASTER/",input$tilepick)
      sampleShapes = readOGR(dsn = paste0('../../../ABoVE_samples/shapefiles/',input$tilepick),
                             layer = paste0("EOSD_",input$tilepick,"_sample"))
      LS_dat = fread(paste0("../../../ABoVE_samples/LS/",input$tilepick,"_LS_filtered.csv"))
                            
      tifs = list.files(tifPath, full.names=T, pattern="tif")
      astertifs = list.files(asterPath, full.names=T, pattern="tif")
      
      tifsPan  = tifs[grepl("pan",list.files(tifPath,
                                             full.names=T,
                                             pattern="tif"))]
      tifsFull = tifs[grepl("mul",list.files(tifPath,
                                             full.names=T,
                                             pattern="tif"))]
      tifsMin  = tifs[grepl("min",list.files(tifPath,
                                             full.names=T,
                                             pattern="tif"))]
      
      dt = paste0("../buildTraining/inProgress/",input$tilepick,"_trainingdt.csv")
      inSamps = sort(as.numeric(unique(sapply(strsplit(tifsFull,"_pp|_yd"), "[[", 2))))
                            
      return(list(sampleShapes=sampleShapes, LS_dat=LS_dat, tifsPan=tifsPan, tifsFull=tifsFull,tifsMin=tifsMin,dt=dt,inSamps=inSamps, astertifs = astertifs))
    }
    })
  
  alaskaWetland = raster("../AlaskanWetlands/alaska_wetland_map.tif")
  
  output$samplepick = renderUI({
    selectInput(inputId = "samplepick",
                label = "Select Sample",
                choices = inData()$inSamps,
                selected= isolate(inData()$inSamps[row$i]))
  })
  
   # # 
   #    output$isInDatNull <- renderTable({
   #      
   #      row$flags[]
   #    })
   #   # 
     # output$isInDatNull <- renderText({
     #   print(!is.null(inData()) & !is.null(input$tilepick))
     # })

		# stolen from https://spatiallyexplicit.wordpress.com/2011/06/07/crop-circles/
	linstretch<-function(img,minmax=NA){
		#if(is.na(minmax)) minmax<-c(min(getValues(img),na.rm=T),max(getValues(img),na.rm=T))
	  theMin = quantile(values(img), 0.02, na.rm=T)
	  theMax = quantile(values(img), 0.98, na.rm=T)
		temp<-calc(img,fun=function(x) (255*(x-theMin))/(theMax-theMin))
		#set all values above or below minmax to 0 or 255
		temp[temp<0]<-0;temp[temp>255]<-255;
		return(temp)
	}
 
 	# initialize data set
 	row = reactiveValues(flags = data.frame(tile="Bh11v11",
 																					samp = 1,
 																					surfaceType = 0,
 																					vegForm = 0,
 																					phenotype = 0,
 																					leafType = 0,
 																					density = 0,
 																					under = 0,
 																					wetlandFlag = 0,
 																					landUse = 0,
 																					year = NA,
 																					completed = 0,
 																					confidence = 0,
 																					skipped = 0,
 																					whySkipped=""),
 											 i = 1)
 
 	rowReady = T
 	# 	
 	# update row when tilepick happens
 	observeEvent(input$tilepick, {
 		if(!is.null(inData()) & !is.null(input$tilepick)){
 			#if(inData()$dt %in% list.files("../buildTraining")){
 			if(file.exists(inData()$dt)){
 			  row$flags = read.csv(inData()$dt)
 			  row$i = min(which(row$flags$completed == 0))
 			}else{
 			  row$flags = data.frame(tile = rep(input$tilepick, length(inData()$inSamps)),
 			                         samp = inData()$inSamps,
 			                         surfaceType = rep(0, length(inData()$inSamps)),
 			                         vegForm = rep(0, length(inData()$inSamps)),
 			                         phenotype = rep(0, length(inData()$inSamps)),
 			                         leafType = rep(0, length(inData()$inSamps)),
 			                         density = rep(0, length(inData()$inSamps)),
 			                         under = rep(0, length(inData()$inSamps)),
 			                         wetlandFlag = rep(0, length(inData()$inSamps)),
 			                         landUse = rep(0, length(inData()$inSamps)),
 			                         year = rep(NA, length(inData()$inSamps)),
 			                         completed = rep(0, length(inData()$inSamps)),
 			                         confidence = rep(0, length(inData()$inSamps)),
 			                         skipped = rep(0, length(inData()$inSamps)),
 			                         whySkipped = rep("", length(inData()$inSamps))
 			                        )
 			  row$i = 1
 			}
 		}
 	})
 	
 	observeEvent(input$samplepick, {
 	  if(!is.null(inData()) & !is.null(input$samplepick)){
 	    
 	    theSamp = input$samplepick
 	    #print(theSamp)
 	    row$i = which(inData()$inSamps == theSamp)
 	    
 	  }
 	})

  	inTifsFull = reactive({
 
  	  if(!is.null(inData())){
 
  	  # gather list of all images available for a sample
  		vars = paste0(sapply(strsplit(inData()$tifsFull[grep(paste0("pp",inData()$inSamps[row$i],"_"), inData()$tifsFull)], "pp|sample/"),"[[",2),
  									"pp",
  									inData()$inSamps[row$i],
  									"_yd",
  									sapply(strsplit(inData()$tifsFull[grep(paste0("pp",inData()$inSamps[row$i],"_"), inData()$tifsFull)], "_yd|\\.tif|_mul|_pan|_min"),"[[",2))
 
		return(as.list(vars))
 
  	  }
  	})
   
  	
    
    	# pickers
    	
    	output$filepick = renderUI({
    	  if(!is.null(inData()) & !is.null(inTifsFull())){
    	    
      	  selectInput(inputId = "filepick",
      								label = "Select File",
      								choices = inTifsFull(),
      								selected = inTifsFull()[1])
    	    
    	  }
    	})
    	
    	# observeEvent(input$samplepick,{
    	# 	row$i = input$samplepick
    	# })
    	
    	########
    	
    	# Determine the year the sample is being picked from 
    	output$aYear = renderText({
    	  if(!is.null(inData()) & !is.null(input$filepick)){
    	    
    		  as.numeric(substr(strsplit(input$filepick,"_yd")[[1]][2],1,4))
    	  }
    	})
    
    	output$trainingRow <- renderTable({
    		row$flags[row$i,"year"] = as.numeric(substr(strsplit(input$filepick,"_yd")[[1]][2],1,4))
       	row$flags[row$i,]
    	  },digits=0)
    	
    	
    	 output$legendRow <- renderTable({
      	 		therow = row$flags[row$i,]
      	 		lType = switch(therow$surfaceType+1,"No Label","Water", "Bare", "Veg","Ice")
      	 		lForm = switch(therow$vegForm+1,"No Label","Moss","Grass","Shrub","Tree")
      	 		lPheno = switch(therow$phenotype+1,"No Label", "Deciduous", "Evergreen","Mixed")
      	 		lLeaf = switch(therow$leafType+1,"No Label", "Broad", "Needle","Mixed")
      	 		lDense = 	switch(therow$density+1,"No Label","Sparse", "Open","Dense")
      	 		lUnder = 	switch(therow$under+1,"No Label","Bare", "Moss","Vascular", "Water")
      	 		lWet = switch(therow$wetlandFlag+1,"No Label","Not Wetland","Wetland", "Submerged")
      	 		lUse = switch(therow$landUse+1,"No Label","Urban","Agriculture" ,"Pasture", "Timber", "Recovery")
      	 		lConf = switch(therow$confidence+1,"No Label","Low","Medium" ,"High")
      	 		lYear = therow$year
      
      	 lrow = data.frame(tile = input$tilepick,
      	 									 samp = inData()$inSamps[row$i],
      	 									 surfaceType = lType,
      	 									 vegForm = lForm,
      	 									 phenotype = lPheno,
      	 									 leafType = lLeaf,
      	 									 density = lDense,
      	 									 under = lUnder,
      	 									 wetlandFlag = lWet,
      	 									 landUse = lUse,
      	 									 confidence = lConf,
      	 									 year = lYear,
      	 									 skipped = " ",
      	 									 whySkipped = " ")
      	 		
      	 lrow
      	 },
    	 digits=0)
    	
    	 
    	# Record label, save, and move to next sample
    	observeEvent(input$save,
    	             if(!is.null(inData()) & rowReady & !is.null(input$tilepick)){
    	               
    							 {
    							  row$flags[row$i, "completed"] = 1
    							 	write.csv(row$flags, inData()$dt, row.names=F)
    							 	row$i = row$i + 1
    							 	
    							 }})
    	
    	observeEvent(input$skip,
    	             if(!is.null(inData())){
    	             
    	             {
    							 	row$flags[row$i,"skipped"] = 1
    							 	row$flags[row$i, "completed"] = 1
    							 	write.csv(row$flags, inData()$dt, row.names=F)
    							 	row$i = row$i + 1
    							 }
    	               })
    	
    	
    	
    	
    	# all them buttons
    	## SURFACE TYPE
    	observeEvent(input$isWater,
    							 {
    							   if(rowReady){
    							     row$flags[row$i,"surfaceType"]=1
    							   }
    							 })
    	observeEvent(input$isBare,
    	             {
    	               if(rowReady){
    	                row$flags[row$i,"surfaceType"]=2
    	               }
    	             })
    	observeEvent(input$isVeg,
    							 {
    							   if(rowReady){
    							 	    row$flags[row$i,"surfaceType"]=3
    							   }
    							 })
    	observeEvent(input$isIce,
    	             {
    	               if(rowReady){
    	                  row$flags[row$i,"surfaceType"]=4
    	               }
    	             })
    	observeEvent(input$resetSurface,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"surfaceType"]=0
    	               }
    	             })
    	
    	## VEG FORM
    	observeEvent(input$isMoss,
    							 {
    							   if(rowReady){
    							 	    row$flags[row$i,"vegForm"]=1
    							 	    row$flags[row$i,"surfaceType"]=3
    							   }
    							 })
    	observeEvent(input$isGrass,
    							 {
    							   if(rowReady){
    							 	    row$flags[row$i,"vegForm"]=2
    							 	    row$flags[row$i,"surfaceType"]=3
    							   }
    							 })
    	observeEvent(input$isShrub,
    							 {
    							   if(rowReady){
    							 	    row$flags[row$i,"vegForm"]=3
    							 	    row$flags[row$i,"surfaceType"]=3
    							   }
    							 })
    	observeEvent(input$isTree,
    							 {
    							   if(rowReady){
    							 	    row$flags[row$i,"vegForm"]=4
    							 	    row$flags[row$i,"surfaceType"]=3
    							   }
    							 })
    	observeEvent(input$resetVeg,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"vegForm"]=0
    	               }
    	             })
    	
    	## PHENOTYPE
    	observeEvent(input$DBF,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"phenotype"]=1
    	                 row$flags[row$i,"surfaceType"]=3
    	               }
    	             })
    	observeEvent(input$ENF,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"phenotype"]=2
    	                 row$flags[row$i,"surfaceType"]=3
    	               }
    	             })
    	observeEvent(input$MXF,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"phenotype"]=3
    	                 row$flags[row$i,"surfaceType"]=3
    	               }
    	             })
    	observeEvent(input$resetPheno,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"phenotype"]=0
    	               }
    	             })
    	
    	## LEAF TYPE
    	observeEvent(input$isBroad,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"leafType"]=1
    	                 row$flags[row$i,"surfaceType"]=3
    	               }
    	             })
    	observeEvent(input$isNeedle,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"leafType"]=2
    	                 row$flags[row$i,"surfaceType"]=3
    	               }
    	             })
    	observeEvent(input$isMixedLeaf,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"leafType"]=3
    	                 row$flags[row$i,"surfaceType"]=3
    	               }
    	             })
    	observeEvent(input$resetLeaf,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"leafType"]=0
    	               }
    	             })
    	
    	## DENSITY
    	observeEvent(input$isSparse,
    	             {
    	               if(rowReady){
    	                  row$flags[row$i,"density"]=1
    	                  row$flags[row$i,"surfaceType"]=3
    	               }
    	             })
    	observeEvent(input$isOpen,
    							 {
    							   if(rowReady){
    							 	    row$flags[row$i,"density"]=2
    							 	    row$flags[row$i,"surfaceType"]=3
    							   }
    							 })
    	observeEvent(input$isDense,
    							 {
    							   if(rowReady){
    							 	    row$flags[row$i,"density"]=3
    							 	    row$flags[row$i,"surfaceType"]=3
    							   }
    							 })
    	observeEvent(input$resetDensity,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"density"]=0
    	               }
    	             })
    	
    	##UNDERSTORY
    	observeEvent(input$isUnderBare,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"under"]=1
    	               }
    	             })
    	observeEvent(input$isUnderMoss,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"under"]=2
    	               }
    	             })
    	observeEvent(input$isUnderVasc,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"under"]=3
    	               }
    	             })
    	observeEvent(input$isUnderWater,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"under"]=4
    	               }
    	             })
    	observeEvent(input$resetUnder,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"under"]=0
    	               }
    	             })
    	
    	
    	## WETLAND FLAG
    	
    	observeEvent(input$isDry,
    							 {
    							   if(rowReady){
    							 	    row$flags[row$i,"wetlandFlag"]=1
    							   }
    							 })
    	observeEvent(input$isWet,
    							 {
    							   if(rowReady){
    							 	    row$flags[row$i,"wetlandFlag"]=2
    							   }
    							 })
    	observeEvent(input$isSubmerged,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"wetlandFlag"]=3
    	               }
    	             })
    	observeEvent(input$resetWet,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"wetlandFlag"]=0
    	               }
    	             })
    	
    	
    	
    	## LAND USE
    	observeEvent(input$isUrban,
    	             {
    	               if(rowReady){
    	                  row$flags[row$i,"landUse"]=1
    	               }
    	             })
    	observeEvent(input$isAgriculture,
    	             {
    	               if(rowReady){
    	                row$flags[row$i,"landUse"]=2
    	               }
    	             })
    	observeEvent(input$isPasture,
    	             {
    	               if(rowReady){
    	                  row$flags[row$i,"landUse"]=3
    	               }
    	             })
    	observeEvent(input$isTimber,
    	             {
    	               if(rowReady){
    	                  row$flags[row$i,"landUse"]=4
    	               }
    	             })
    	observeEvent(input$isRecovering,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"landUse"]=5
    	               }
    	             })
    	observeEvent(input$resetLandUse,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"landUse"]=0
    	               }
    	             })
    	
    	## CONFIDENCE
    	observeEvent(input$highConfidence,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"confidence"]=3
    	               }
    	             })
    	observeEvent(input$medConfidence,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"confidence"]=2
    	               }
    	             })
    	observeEvent(input$lowConfidence,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"confidence"]=1
    	               }
    	             })
    	observeEvent(input$resetConfidence,
    	             {
    	               if(rowReady){
    	                 row$flags[row$i,"confidence"]=0
    	               }
    	             })
    	
    	## SKIP
     	observeEvent(input$skipBox,
     							 {
     							   if(rowReady){
         							 	row$flags[row$i,"whySkipped"]=input$skipBox
         							 	row$flags[row$i,"skipped"] = 1
     							   }
    							 })
    
    	output$diag = renderText({
    	  if(!is.null(inData()) & rowReady){
    	    
    		  paste0("row$i = ",row$i," and nrow(row$flags) = ", nrow(row$flags))
    	  }
    	})	
    	
      output$LSplot <- renderPlot({
        
        if(!is.null(inData()) & rowReady){
            
          # determine band and sample
          theSamp =inData()$inSamps[row$i]
          band = input$LSband
          
          hasCoefs = file.exists(paste0("../../../ABoVE_samples/sample_coefs/",input$tilepick, "_sampled_coefs.csv"))
          print(hasCoefs)
          ### if we have coefficients, calculate some synthetic data to compare with the real data ### 
          if(hasCoefs){
            # get coefficients                                              
            nmcoefs = c(rep("robust_a0_",7),                                
                        rep("robust_c1_",7),                                   
                        rep("robust_a1_",7),                                   
                        rep("robust_b1_",7),                                   
                        rep("robust_a2_",7),                                   
                        rep("robust_b2_",7),                                   
                        rep("robust_a3_",7),                                   
                        rep("robust_b3_",7))                                   
            bands = c("blue", "green", "red", "nir", "swir1", "swir2", "bt")
            cb = data.table(nmcoefs=nmcoefs, bands=bands)                   
            cb[,coefs_nms:=paste0(nmcoefs,bands)]                           
            
            # build synthetic time series                                                                  
            getPredict = function(coefs_r){                                                                
              w = 2*pi/365.25                                                                             
              days = coefs_r$start:coefs_r$end                                                            
              coefs = matrix(unlist(coefs_r[1,cb$coefs_nms,with=F]),                                      
                             byrow=T,                                                                     
                             ncol=7)                                                                      
              
              #coefs=as.matrix(coefs)                                                                     
              
              #rcoefs_names = paste0("robust_", coefs_names)                                              
              #rcoefs = as.matrix(coefs_r[1,rcoefs_names,with=F])                                         
              xmatr = cbind(rep(1,length(days)),                                                        
                            days,                                                                         
                            cos(w*days),                                                                  
                            sin(w*days),                                                                  
                            cos(2*w*days),                                                                
                            sin(2*w*days),                                                                
                            cos(3*w*days),                                                                
                            sin(3*w*days))                                                                
              
              
              synth = xmatr %*% coefs
              
              out = as.data.table(cbind.data.frame(days,synth))
              
              out[,date := ymd("0001-1-1") + days(days)]
              out[,yr:=year(date)]
              out[,doy:=yday(date)]
              
              setnames(out, c("days","blue","green","red","nir","swir1","swir2","bt", "date", "yr", "doy"))
              out[,ndvi:=(nir-red)/(nir+red)]
              return(out)
              #   rsynth = xmatr %*% t(rcoefs)
            }
            
            
            # get synthetic data for sample, all models for that sample
            coefs_dt = fread(paste0("../../../ABoVE_samples/sample_coefs/",input$tilepick, "_sampled_coefs.csv"))
            sampleRows = coefs_dt[samp==theSamp,]
            # for each sample, get the synthetic data of each time series
            synthLS_l= lapply(1:nrow(sampleRows),function(i)getPredict(sampleRows[i,]))
            synthLS = rbindlist(synthLS_l, use.names=T)
            
          }

            if(band == "evi-nbr"){
              nirBand = paste0("nir",theSamp)
              blueBand = paste0("blue",theSamp)
              redBand = paste0("red", theSamp)
              swirBand = paste0("swir2",theSamp)
              
              nir = inData()$LS_dat[[nirBand]]/10000
              swir = inData()$LS_dat[[swirBand]]/10000
              red = inData()$LS_dat[[redBand]]/10000
              blue = inData()$LS_dat[[blueBand]]/10000
              
              # a trick for distinguishing wetlands, thanks to Damien - is summertime NBR - EVI > 0.4 or so?
              
              nbr = (nir - swir) / (nir + swir)
              
              G = 2.5; L = 1; C1 = 6; C2 = 7.5
              
              evi = G * (nir - red) / (nir + C1*red - C2*blue + L)
              # evi[evi > 1] = 1
              # evi[evi < -1] = -1
              
              plotLimits = quantile(c(nbr, evi), c(0.01, 0.99), na.rm=T)
              
              dt = data.table(date = strptime(inData()$LS_dat$date,format="%Y%j"), 
                              evi = evi,
                              nbr = nbr,
                              doy = yday(strptime(inData()$LS_dat$date,format="%Y%j")))
              
              dtm = melt(dt, id.vars = c("date", "doy"))

              # plotting synthetic data
              
              if(hasCoefs){
                nir = synthLS$nir/10000
                swir = synthLS$swir2/10000
                red = synthLS$red/10000
                blue = synthLS$blue/10000
                
                # a trick for distinguishing wetlands, thanks to Damien - is summertime NBR - EVI > 0.4 or so?
                
                nbr = (nir - swir) / (nir + swir)
                
                G = 2.5; L = 1; C1 = 6; C2 = 7.5
                
                evi = G * (nir - red) / (nir + C1*red - C2*blue + L)
                # evi[evi > 1] = 1
                # evi[evi < -1] = -1
                
                dt = data.table(date = strptime(synthLS$date,format="%Y-%m-%d"), 
                                evi = evi,
                                nbr = nbr,
                                doy = yday(strptime(synthLS$date,format="%Y-%m-%d")))
                
                synth_dtm = melt(dt, id.vars = c("date", "doy"))
              }
                            
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dtm[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date, y=value, color= variable)) + geom_point() +
                  theme_bw() + 
                  scale_color_manual("", values=c("evi" = "darkgreen", "nbr" = "red"), guide=F) +
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits) + xlim(strptime(paste0(c(as.character(min(input$yearRange)),as.character(max(input$yearRange)+1)),"-01-01"), "%Y-%m-%d"))
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = synth_dtm[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date,y=value,color=variable), alpha = 0.3)
                }
                
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dtm[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy, y=value, color= variable)) + geom_point() +
                  theme_bw() + 
                  scale_color_manual("", values=c("evi" = "darkgreen", "nbr" = "red"), guide=F) +
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = synth_dtm[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy,y=value,color=variable), alpha=0.3)
                }
              }
              
            }else if(band == "ndwi"){
              
              nirBand = paste0("nir",theSamp)
              greenBand = paste0("grn",theSamp)
              
              nir = inData()$LS_dat[[nirBand]]/10000
              green = inData()$LS_dat[[greenBand]]/10000
              
              # additional information for determining wetlands, thanks to
              # http://onlinelibrary.wiley.com/doi/10.1002/2014WR015634/full
              
              ndwi = (green - nir) / (nir + green)
              plotLimits = quantile(ndwi, c(0.01, 0.99), na.rm=T)

              dt = data.table(date = strptime(inData()$LS_dat$date,format="%Y%j"), 
                              ndwi = ndwi,
                              doy = yday(strptime(inData()$LS_dat$date,format="%Y%j")))

              if(hasCoefs){
                nir = synthLS$nir/10000
                green = synthLS$green/10000
                
                ndwi = (green - nir) / (green + nir)

                sdt = data.table(date = strptime(synthLS$date,format="%Y-%m-%d"), 
                                ndwi = ndwi,
                                doy = yday(strptime(synthLS$date,format="%Y-%m-%d")))
                }
              
              
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date, y=ndwi)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits) + xlim(strptime(paste0(c(as.character(min(input$yearRange)),as.character(max(input$yearRange)+1)),"-01-01"), "%Y-%m-%d"))
                
                if(hasCoefs){
                  print(head(sdt))
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date,y=ndwi), alpha = 0.3)
                }
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy, y=ndwi)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy,y=ndwi), alpha = 0.3)
                }
                
              }
              
            }else if(band == "lswi"){
              
              nirBand = paste0("nir",theSamp)
              swir1Band = paste0("swir1",theSamp)
              
              nir = inData()$LS_dat[[nirBand]]/10000
              swir1 = inData()$LS_dat[[swir1Band]]/10000
              
              # additional information for determining wetlands, thanks to
              # http://onlinelibrary.wiley.com/doi/10.1002/2014WR015634/full
              
              lswi = (nir - swir1) / (nir + swir1)
              plotLimits = quantile(lswi, c(0.01, 0.99), na.rm=T)
              
              dt = data.table(date = strptime(inData()$LS_dat$date,format="%Y%j"), 
                              lswi = lswi,
                              doy = yday(strptime(inData()$LS_dat$date,format="%Y%j")))
              
              
              if(hasCoefs){
                nir = synthLS$nir/10000
                swir1 = synthLS$swir1/10000
                
                lswi = (nir - swir1) / (nir + swir1)
                
                sdt = data.table(date = strptime(synthLS$date,format="%Y-%m-%d"), 
                                lswi = lswi,
                                doy = yday(strptime(synthLS$date,format="%Y-%m-%d")))
                
              }
              
              
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date, y=lswi)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits) + xlim(strptime(paste0(c(as.character(min(input$yearRange)),as.character(max(input$yearRange)+1)),"-01-01"), "%Y-%m-%d"))
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date,y=lswi), alpha = 0.3)
                }
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy, y=lswi)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy,y=lswi), alpha = 0.3)
                }
              }
              
            }else if(band == "tcb"){
              
              nirBand = paste0("nir",theSamp)
              greenBand = paste0("grn",theSamp)
              blueBand = paste0("blue",theSamp)
              redBand = paste0("red",theSamp)
              swir1Band = paste0("swir1",theSamp)
              swir2Band = paste0("swir2",theSamp)
              
              nir = inData()$LS_dat[[nirBand]]/10000
              swir1 = inData()$LS_dat[[swir1Band]]/10000
              swir2 = inData()$LS_dat[[swir2Band]]/10000
              green = inData()$LS_dat[[greenBand]]/10000
              blue = inData()$LS_dat[[blueBand]]/10000
              red = inData()$LS_dat[[redBand]]/10000
              
              # additional information for determining wetlands, thanks to
              # http://www.sciencedirect.com/science/article/pii/S003442571300388X Huang et al 2014 RSE
              # coefficients from
              # http://www.sciencedirect.com/science/article/pii/0034425785901026 Crist et al 1985 RSE
              
              coefB = 0.2043
              coefG = 0.4158
              coefR = 0.5524
              coefN = 0.5741
              coefS1 = 0.3124
              coefS2 = 0.2303
              
              tcb = coefB*blue + coefG*green + coefR*red + coefN*nir + coefS1*swir1 + coefS2*swir2
              plotLimits = quantile(tcb, c(0.01, 0.99), na.rm=T)
              
              dt = data.table(date = strptime(inData()$LS_dat$date,format="%Y%j"), 
                              tcb = tcb,
                              doy = yday(strptime(inData()$LS_dat$date,format="%Y%j")))
              
              if(hasCoefs){
                nir = synthLS$nir/10000
                swir1 = synthLS$swir1/10000
                swir2 = synthLS$swir2/10000
                green = synthLS$green/10000
                blue = synthLS$blue/10000
                red = synthLS$red/10000
                
                # additional information for determining wetlands, thanks to
                # http://www.sciencedirect.com/science/article/pii/S003442571300388X Huang et al 2014 RSE
                # coefficients from
                # http://www.sciencedirect.com/science/article/pii/0034425785901026 Crist et al 1985 RSE
                
                tcb = coefB*blue + coefG*green + coefR*red + coefN*nir + coefS1*swir1 + coefS2*swir2
                sdt = data.table(date = strptime(synthLS$date,format="%Y-%m-%d"), 
                                tcb = tcb,
                                doy = yday(strptime(synthLS$date,format="%Y-%m-%d")))
              }
              
              
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date, y=tcb)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits) + xlim(strptime(paste0(c(as.character(min(input$yearRange)),as.character(max(input$yearRange)+1)),"-01-01"), "%Y-%m-%d"))
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date,y=tcb), alpha = 0.3)
                }
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy, y=tcb)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy,y=tcb), alpha = 0.3)
                }
              }
              
            }else if(band == "tcg"){
              
              nirBand = paste0("nir",theSamp)
              greenBand = paste0("grn",theSamp)
              blueBand = paste0("blue",theSamp)
              redBand = paste0("red",theSamp)
              swir1Band = paste0("swir1",theSamp)
              swir2Band = paste0("swir2",theSamp)
              
              nir = inData()$LS_dat[[nirBand]]/10000
              swir1 = inData()$LS_dat[[swir1Band]]/10000
              swir2 = inData()$LS_dat[[swir2Band]]/10000
              green = inData()$LS_dat[[greenBand]]/10000
              blue = inData()$LS_dat[[blueBand]]/10000
              red = inData()$LS_dat[[redBand]]/10000
              
              # additional information for determining wetlands, thanks to
              # http://www.sciencedirect.com/science/article/pii/S003442571300388X Huang et al 2014 RSE
              # coefficients from
              # http://www.sciencedirect.com/science/article/pii/0034425785901026 Crist et al 1985 RSE
              
              coefB = -0.1603
              coefG = -0.2819
              coefR = -0.4934
              coefN = 0.7940
              coefS1 = 0.0002
              coefS2 = 0.1446
              
              tcg = coefB*blue + coefG*green + coefR*red + coefN*nir + coefS1*swir1 + coefS2*swir2
              plotLimits = quantile(tcg, c(0.01, 0.99), na.rm=T)
              
              dt = data.table(date = strptime(inData()$LS_dat$date,format="%Y%j"), 
                              tcg = tcg,
                              doy = yday(strptime(inData()$LS_dat$date,format="%Y%j")))
              if(hasCoefs){
                nir = synthLS$nir/10000
                swir1 = synthLS$swir1/10000
                swir2 = synthLS$swir2/10000
                green = synthLS$green/10000
                blue = synthLS$blue/10000
                red = synthLS$red/10000
                
                # additional information for determining wetlands, thanks to
                # http://www.sciencedirect.com/science/article/pii/S003442571300388X Huang et al 2014 RSE
                # coefficients from
                # http://www.sciencedirect.com/science/article/pii/0034425785901026 Crist et al 1985 RSE
                
                tcg = coefB*blue + coefG*green + coefR*red + coefN*nir + coefS1*swir1 + coefS2*swir2
                sdt = data.table(date = strptime(synthLS$date,format="%Y-%m-%d"), 
                                 tcg = tcg,
                                 doy = yday(strptime(synthLS$date,format="%Y-%m-%d")))
              }
              
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date, y=tcg)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits) + xlim(strptime(paste0(c(as.character(min(input$yearRange)),as.character(max(input$yearRange)+1)),"-01-01"), "%Y-%m-%d"))
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date,y=tcg), alpha = 0.3)
                }
                
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy, y=tcg)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy,y=tcg), alpha = 0.3)
                }
              }
              
            }else if(band == "tcw"){
              
              nirBand = paste0("nir",theSamp)
              greenBand = paste0("grn",theSamp)
              blueBand = paste0("blue",theSamp)
              redBand = paste0("red",theSamp)
              swir1Band = paste0("swir1",theSamp)
              swir2Band = paste0("swir2",theSamp)
              
              nir = inData()$LS_dat[[nirBand]]/10000
              swir1 = inData()$LS_dat[[swir1Band]]/10000
              swir2 = inData()$LS_dat[[swir2Band]]/10000
              green = inData()$LS_dat[[greenBand]]/10000
              blue = inData()$LS_dat[[blueBand]]/10000
              red = inData()$LS_dat[[redBand]]/10000
              
              # additional information for determining wetlands, thanks to
              # http://www.sciencedirect.com/science/article/pii/S003442571300388X Huang et al 2014 RSE
              # coefficients from
              # http://www.sciencedirect.com/science/article/pii/0034425785901026 Crist et al 1985 RSE
              
              coefB = 0.0315
              coefG = 0.2021
              coefR = 0.3102
              coefN = 0.1594
              coefS1 = 0.6806
              coefS2 = 0.6109
              
              tcw = coefB*blue + coefG*green + coefR*red + coefN*nir + coefS1*swir1 + coefS2*swir2
              plotLimits = quantile(tcw, c(0.01, 0.99), na.rm=T)
              
              dt = data.table(date = strptime(inData()$LS_dat$date,format="%Y%j"), 
                              tcw = tcw,
                              doy = yday(strptime(inData()$LS_dat$date,format="%Y%j")))
              
              if(hasCoefs){
                nir = synthLS$nir/10000
                swir1 = synthLS$swir1/10000
                swir2 = synthLS$swir2/10000
                green = synthLS$green/10000
                blue = synthLS$blue/10000
                red = synthLS$red/10000
                
                # additional information for determining wetlands, thanks to
                # http://www.sciencedirect.com/science/article/pii/S003442571300388X Huang et al 2014 RSE
                # coefficients from
                # http://www.sciencedirect.com/science/article/pii/0034425785901026 Crist et al 1985 RSE
                
                tcw = coefB*blue + coefG*green + coefR*red + coefN*nir + coefS1*swir1 + coefS2*swir2

                sdt = data.table(date = strptime(synthLS$date,format="%Y-%m-%d"), 
                                 tcw = tcw,
                                 doy = yday(strptime(synthLS$date,format="%Y-%m-%d")))
              }
              
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date, y=tcw)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits) + xlim(strptime(paste0(c(as.character(min(input$yearRange)),as.character(max(input$yearRange)+1)),"-01-01"), "%Y-%m-%d"))
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date,y=tcw), alpha = 0.3)
                }
                
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy, y=tcw)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy,y=tcw), alpha = 0.3)
                }
                
              }
              
            }else if(band == "tcw-tcg"){
              
              nirBand = paste0("nir",theSamp)
              greenBand = paste0("grn",theSamp)
              blueBand = paste0("blue",theSamp)
              redBand = paste0("red",theSamp)
              swir1Band = paste0("swir1",theSamp)
              swir2Band = paste0("swir2",theSamp)
              
              nir = inData()$LS_dat[[nirBand]]/10000
              swir1 = inData()$LS_dat[[swir1Band]]/10000
              swir2 = inData()$LS_dat[[swir2Band]]/10000
              green = inData()$LS_dat[[greenBand]]/10000
              blue = inData()$LS_dat[[blueBand]]/10000
              red = inData()$LS_dat[[redBand]]/10000
              
              # additional information for determining wetlands, thanks to
              # http://www.sciencedirect.com/science/article/pii/S003442571300388X Huang et al 2014 RSE
              # coefficients from
              # http://www.sciencedirect.com/science/article/pii/0034425785901026 Crist et al 1985 RSE
              
              # wetness
              coefB = 0.0315
              coefG = 0.2021
              coefR = 0.3102
              coefN = 0.1594
              coefS1 = 0.6806
              coefS2 = 0.6109
              
              tcw = coefB*blue + coefG*green + coefR*red + coefN*nir + coefS1*swir1 + coefS2*swir2
              
              # greenness
              coefB = -0.1603
              coefG = -0.2819
              coefR = -0.4934
              coefN = 0.7940
              coefS1 = 0.0002
              coefS2 = 0.1446
              
              tcg = coefB*blue + coefG*green + coefR*red + coefN*nir + coefS1*swir1 + coefS2*swir2
              
              tcwgd = tcw-tcg
              
              plotLimits = quantile(tcwgd, c(0.01, 0.99), na.rm=T)
              
              dt = data.table(date = strptime(inData()$LS_dat$date,format="%Y%j"), 
                              tcwgd = tcwgd,
                              doy = yday(strptime(inData()$LS_dat$date,format="%Y%j")))
              
              if(hasCoefs){
                nir = synthLS$nir/10000
                swir1 = synthLS$swir1/10000
                swir2 = synthLS$swir2/10000
                green = synthLS$green/10000
                blue = synthLS$blue/10000
                red = synthLS$red/10000
                
                # additional information for determining wetlands, thanks to
                # http://www.sciencedirect.com/science/article/pii/S003442571300388X Huang et al 2014 RSE
                # coefficients from
                # http://www.sciencedirect.com/science/article/pii/0034425785901026 Crist et al 1985 RSE
                
                # wetness
                coefB = 0.0315
                coefG = 0.2021
                coefR = 0.3102
                coefN = 0.1594
                coefS1 = 0.6806
                coefS2 = 0.6109
                
                tcw = coefB*blue + coefG*green + coefR*red + coefN*nir + coefS1*swir1 + coefS2*swir2
                
                # greenness
                coefB = -0.1603
                coefG = -0.2819
                coefR = -0.4934
                coefN = 0.7940
                coefS1 = 0.0002
                coefS2 = 0.1446
                
                tcg = coefB*blue + coefG*green + coefR*red + coefN*nir + coefS1*swir1 + coefS2*swir2
                
                tcwgd = tcw-tcg
                
                sdt = data.table(date = strptime(synthLS$date,format="%Y-%m-%d"), 
                                 tcwgd = tcwgd,
                                 doy = yday(strptime(synthLS$date,format="%Y-%m-%d")))
              }
              
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date, y=tcwgd)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits) + xlim(strptime(paste0(c(as.character(min(input$yearRange)),as.character(max(input$yearRange)+1)),"-01-01"), "%Y-%m-%d"))
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date,y=tcwgd), alpha = 0.3)
                }
                
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy, y=tcwgd)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy,y=tcwgd), alpha = 0.3)
                }
                
              }
              
            }else{
              
              bandToPlot =paste0(band,theSamp)
              
              dt = data.table(date = strptime(inData()$LS_dat$date,format="%Y%j"), 
                              val = inData()$LS_dat[[bandToPlot]], 
                              doy = yday(strptime(inData()$LS_dat$date,format="%Y%j")))
              
              plotLimits = quantile(dt$val, c(0.01, 0.99), na.rm=T)
              
              
              if(hasCoefs){
                if(band=="grn")band="green"
                
                print(head(synthLS))
                
                sdt = data.table(date = strptime(synthLS$date, format="%Y-%m-%d"),
                                 val = synthLS[[band]],
                                 doy = yday(strptime(synthLS$date, format="%Y-%m-%d")))
                
                print(head(sdt))
              }
              
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date, y=val)) + geom_point() + 
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits) + xlim(strptime(paste0(c(as.character(min(input$yearRange)),as.character(max(input$yearRange)+1)),"-01-01"), "%Y-%m-%d"))
                
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=date,y=val), alpha = 0.3)
                }
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy, y=val)) + geom_point() + 
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                if(hasCoefs){
                  theLSplot = theLSplot + geom_line(data = sdt[year(date) %in% seq(min(input$yearRange),max(input$yearRange)+1),], aes(x=doy,y=val), alpha = 0.3)
                }
              }
              
            }
            theLSplot
          }
      })
    	
    	output$RGBplot <- renderPlot({
    	  
    	  if(!is.null(inData()) & rowReady){
    	    
        		rgbTif =brick(inData()$tifsFull[grep(input$filepick,inData()$tifsFull)]) 
        		# world view has different band assignments
        		if(input$falseCol == "321"){
        			if(grepl("WV03",input$filepick)){
        			  plotRGB(rgbTif,
        							  r=5,g=3,b=2, stretch = 'lin')
        			}else if(grepl("WV01",input$filepick)){
        			  plotRGB(rgbTif,
        			          r=1,g=1,b=1, stretch = 'lin')
        			}else if(grepl("WV02",input$filepick) & nlayers(rgbTif) > 4){
        			  plotRGB(rgbTif,
        			          r=4,g=3,b=2, stretch = 'lin')
        			}else{
        			  plotRGB(rgbTif,
        			          r=3,g=2,b=1, stretch = 'lin')
        			}
        		}
        		if(input$falseCol == "432"){
        		  if(grepl("WV03",input$filepick)){
        		    plotRGB(rgbTif,
        		            r=7,g=5,b=2, stretch = 'lin')
        		  }else if(grepl("WV02",input$filepick) & nlayers(rgbTif) > 4){
        		    plotRGB(rgbTif,
        		            r=6,g=4,b=3, stretch = 'lin')
        		  }else if(grepl("WV01",input$filepick)){
        		    plotRGB(rgbTif,
        		            r=1,g=1,b=1, stretch = 'lin')
        		  }else{
        		    plotRGB(rgbTif,
        		            r=4,g=3,b=2, stretch = 'lin')
        		  }
        		}
        		plot(spTransform(inData()$sampleShapes[inData()$inSamps[isolate(row$i)],],CRSobj = crs(rgbTif)), col=NA, border = "red", add=T)
        		
    	  }
    	})
    	
    	output$NDVIplot <- renderPlot({
    	  
    	  if(!is.null(inData()) & rowReady){
    	    
        		rgbTif =brick(inData()$tifsFull[grep(input$filepick,inData()$tifsFull)]) 
        		
        		xext = extent(rgbTif)@xmax/2 + extent(rgbTif)@xmin/2
        		yext = extent(rgbTif)@ymax/2 + extent(rgbTif)@ymin/2
        		
        		miniExt = extent(c(xext-100, xext+100, yext-100, yext+100))
        		
        		f_NDVI = function(x) {
        			(x[,1]-x[,2])/(x[,1]+x[,2])
        		}
        		
        		rgbTif = crop(rgbTif, miniExt)
        		
        		if(grepl("WV03",input$filepick)){
        		  NDVI = brick(calc(rgbTif[[c(7,5)]], f_NDVI))
        		}else if(grepl("WV02",input$filepick) & nlayers(rgbTif) > 4){
        		  NDVI = brick(calc(rgbTif[[c(6,4)]], f_NDVI))
        		}else{
        		  NDVI = brick(calc(rgbTif[[c(4,3)]], f_NDVI))
        		}
        		
        
        		plotRGB(NDVI,
        						r=1,g=1,b=1, stretch="lin")
        		# just grabbing the legend
        		plot(NDVI, legend.only = T,
        				 col = gray.colors(100),
        				 smallplot = c(0.9,0.92,0.3,0.7))
        		plot(spTransform(inData()$sampleShapes[inData()$inSamps[isolate(row$i)],],CRSobj = crs(rgbTif)), col=NA, border = "red", add=T)
        		
    	  }
    	})
    	
    	
    	output$zoomPlot <- renderPlot({
    	  
    	  if(!is.null(inData()) & rowReady){
    	    
    	  
        		zoomTif =brick(inData()$tifsFull[grep(input$filepick,inData()$tifsFull)]) 
        		astertif = try(brick(inData()$astertifs[inData()$inSamps[isolate(row$i)]]))

        		
        		# get center of extent, then go back out 100
        		xext = extent(zoomTif)@xmax/2 + extent(zoomTif)@xmin/2
        		yext = extent(zoomTif)@ymax/2 + extent(zoomTif)@ymin/2
        		
        		miniExt = extent(c(xext-100, xext+100, yext-100, yext+100))
        		
        		if(grepl("WV03",input$filepick)){
          		# before cropping, set stretch
          		rast_b = linstretch(raster(zoomTif, layer=2), minmax = quantile(raster(zoomTif, layer=2), c(0.02,0.98)))
          		rast_g = linstretch(raster(zoomTif, layer=3), minmax = quantile(raster(zoomTif, layer=3), c(0.02,0.98)))
          		rast_r = linstretch(raster(zoomTif, layer=5), minmax = quantile(raster(zoomTif, layer=5), c(0.02,0.98)))
          		rast_n = linstretch(raster(zoomTif, layer=7), minmax = quantile(raster(zoomTif, layer=7), c(0.02,0.98)))
          		
          		zoomTif_str = stack(rast_b, rast_g, rast_r, rast_n)
          		
          		zoomTif = crop(zoomTif_str, miniExt)
        		}else if(grepl("WV02",input$filepick) & nlayers(zoomTif) > 4){
        		  # before cropping, set stretch
        		  rast_b = linstretch(raster(zoomTif, layer=2), minmax = quantile(raster(zoomTif, layer=2), c(0.02,0.98)))
        		  rast_g = linstretch(raster(zoomTif, layer=3), minmax = quantile(raster(zoomTif, layer=3), c(0.02,0.98)))
        		  rast_r = linstretch(raster(zoomTif, layer=4), minmax = quantile(raster(zoomTif, layer=4), c(0.02,0.98)))
        		  rast_n = linstretch(raster(zoomTif, layer=6), minmax = quantile(raster(zoomTif, layer=6), c(0.02,0.98)))
        		  
        		  zoomTif_str = stack(rast_b, rast_g, rast_r, rast_n)
        		  
        		  zoomTif = crop(zoomTif_str, miniExt)
        		}else{
        		  # before cropping, set stretch
        		  rast_b = linstretch(raster(zoomTif, layer=1), minmax = quantile(raster(zoomTif, layer=1), c(0.02,0.98)))
        		  rast_g = linstretch(raster(zoomTif, layer=2), minmax = quantile(raster(zoomTif, layer=2), c(0.02,0.98)))
        		  rast_r = linstretch(raster(zoomTif, layer=3), minmax = quantile(raster(zoomTif, layer=3), c(0.02,0.98)))
        		  rast_n = linstretch(raster(zoomTif, layer=4), minmax = quantile(raster(zoomTif, layer=4), c(0.02,0.98)))
        		  
        		  zoomTif_str = stack(rast_b, rast_g, rast_r, rast_n)
        		  
        		  zoomTif = crop(zoomTif_str, miniExt)
        		}
        		
        		if(input$zoomChoice == "z321"){
        		    plotRGB(zoomTif,
        		            r=3,g=2,b=1)#, stretch = "lin")
        		}
        		if(input$zoomChoice == "z432"){
        		    plotRGB(zoomTif,
        		            r=4,g=3,b=2)#, stretch = "lin")
        		}
        		if(input$zoomChoice == "elev"){
        		  plotRGB(astertif,
        		          r=1,g=1,b=1, stretch="lin")
        		  # just grabbing the legend
        		  plot(astertif,1, legend.only = T,
        		       col = gray.colors(100),
        		       smallplot = c(0.9,0.92,0.3,0.7))
        		}
        		if(input$zoomChoice == "aspect"){
        		  plotRGB(astertif,
        		          r=2,g=2,b=2, stretch="lin")
        		  # just grabbing the legend
        		  plot(astertif,2, legend.only = T,
        		       col = gray.colors(100),
        		       smallplot = c(0.9,0.92,0.3,0.7))
        		  
        		}
        		if(input$zoomChoice == "slope"){
        		  plotRGB(astertif,
        		          r=3,g=3,b=3, stretch="lin")
        		  # just grabbing the legend
        		  plot(astertif, 3, legend.only = T,
        		       col = gray.colors(100),
        		       smallplot = c(0.9,0.92,0.3,0.7))
        		  
        		}
        		
        		plot(spTransform(inData()$sampleShapes[inData()$inSamps[isolate(row$i)],],CRSobj = crs(zoomTif)), col=NA, border = "red", add=T)
    	  }
    	})
    	
    	output$panPlot <- renderPlot({
    	  
    	  if(!is.null(inData()) & rowReady){
    	    
        		#if(any(grepl(paste0("pp",inSamps[row$i]), tifsPan))){
        		if(any(grepl(input$filepick, inData()$tifsPan))){
        						zoomPan = brick(inData()$tifsPan[grep(input$filepick,inData()$tifsPan)]) 
        			
        			# # get center of extent, then go back out 100
        			# xext = extent(zoomPan)@xmax/2 + extent(zoomPan)@xmin/2
        			# yext = extent(zoomPan)@ymax/2 + extent(zoomPan)@ymin/2
        			# 
        			# miniExt = extent(c(xext-100, xext+100, yext-100, yext+100))
        			# zoomPan = crop(zoomPan, miniExt)
        			# 
        			plotRGB(zoomPan,
        							r=1,g=1,b=1,stretch="lin")
        			plot(spTransform(inData()$sampleShapes[inData()$inSamps[isolate(row$i)],],CRSobj = crs(zoomPan)), col=NA, border = "red", add=T)
        		}else{
        			rgbTif =brick(inData()$tifsFull[grep(input$filepick,inData()$tifsFull)]) 
        			plotRGB(rgbTif,
        							r=3,g=2,b=1,stretch="lin")
        			plot(spTransform(inData()$sampleShapes[inData()$inSamps[isolate(row$i)],],CRSobj = crs(rgbTif)), col=NA, border = "red", add=T)
        		}
    	  }
    	})
    	
    	output$coords = renderText({
    	  if(!is.null(inData()) & rowReady){
    	    
      		theExt = extent(spTransform(inData()$sampleShapes[inData()$inSamps[row$i],],CRSobj = crs("+proj=longlat +datum=WGS84")))
      		lats = mean(theExt@ymin, theExt@ymax)
      		lons = mean(theExt@xmin, theExt@xmax)
      		paste0("LAT: ", round(lats,3), "   LON: ", round(lons,3))
    		
      		
    	  }
    	})

    	output$coords_aea = renderText({
    	  if(!is.null(inData()) & rowReady){
    	    
    	    theExt_aea = extent(spTransform(inData()$sampleShapes[inData()$inSamps[row$i],],CRSobj = crs("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=WGS84 +units=m +nodefs")))
    	    lats_aea = mean(theExt_aea@ymin, theExt_aea@ymax)
    	    lons_aea = mean(theExt_aea@xmin, theExt_aea@xmax)
    	    paste0("X: ", round(lons_aea),"     Y: ",round(lats_aea))
    	    
    	    
    	  }
    	})
    	
    	output$wetcomb = renderText({
    	  if(!is.null(inData()) & rowReady){
    	    
    	    theExt_aea = extent(spTransform(inData()$sampleShapes[inData()$inSamps[row$i],],CRSobj = crs("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=WGS84 +units=m +nodefs")))
    	    #lats_aea = mean(theExt_aea@ymin, theExt_aea@ymax)
    	    #lons_aea = mean(theExt_aea@xmin, theExt_aea@xmax)
    	    
    	    wetl = try(extract(alaskaWetland, theExt_aea), silent=T)
    	    if(class(wetl) == "try-error"){
    	      print("Not in Alaska")
    	    }else{
    	      wetl_out = "No Data"
    	      wetl_1 = wetl %/% 5
    	      wetl_2 = wetl %% 5
    	      if(wetl_1 == 1) wetl_out = "estuarine emergent"
    	      if(wetl_1 == 2) wetl_out = "estuarine shrub"
    	      if(wetl_1 == 3) wetl_out = "estuarine forest"
    	      if(wetl_1 == 4) wetl_out = "riverine emergent"
    	      if(wetl_1 == 5) wetl_out = "lacustrine emergent"
    	      if(wetl_1 == 6) wetl_out = "palustrine moss/lichen"
    	      if(wetl_1 == 7) wetl_out = "palustrine emergent"
    	      if(wetl_1 == 8) wetl_out = "palustrine shrub"
    	      if(wetl_1 == 9) wetl_out = "palustrine forest"
    	      #if(wetl_1 == 10) wetl_out = "other"

    	      if(wetl_2 == 1) wetl_out = paste0(wetl_out, " - perm flood")
    	      if(wetl_2 == 2) wetl_out = paste0(wetl_out, " - reg exposed")
    	      if(wetl_2 == 3) wetl_out = paste0(wetl_out, " - seasonal")
    	      if(wetl_2 == 4) wetl_out = paste0(wetl_out, " - intermittent")
    	      if(wetl_2 == 0 & wetl != 50) wetl_out = paste0(wetl_out, " - saturated")

    	      if(wetl == 46) wetl_out = "decid forest"
    	      if(wetl == 47) wetl_out = "barren"
    	      if(wetl == 48) wetl_out = "uplands"
    	      if(wetl == 49) wetl_out = "ocean"
    	      
    	      print(paste0(wetl, ":",wetl_out))
    	      
    	    }
    	    
    	    #paste0("X: ", round(lons_aea),"     Y: ",round(lats_aea))
    	    
    	    
    	  }
    	})
    	  		
}

	 

# Run the application 
shinyApp(ui = ui, server = server)

