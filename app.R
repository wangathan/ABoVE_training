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
				fluidRow(
				  hr(),
				  p("LEAF TYPE"),
				  actionButton(inputId = "isBroad",
				               label = "Broadleaf"),
				  actionButton(inputId = "isNeedle",
				               label = "Needleleaf"),
				  actionButton(inputId = "isMixedLeaf",
				               label = "Mixed Leaf"),
				  actionButton(inputId = "resetLeaf",
				               label = "Reset")
				),
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
	   		),
	   		fluidRow(
	   			hr(),
	   			p("WETLAND?"),
	   			actionButton(inputId = "isWet",
	   									 label = "Wetland"),
	   			actionButton(inputId = "isDry",
	   									 label = "Not Wetland"),
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
				               choices = list("Not Skip", "Insufficient Imagery","Mixed Pixel", "Unclear", "I'm Lazy", "Other"))
				)
	   	)
	   	),
   	column(width=7,
   	wellPanel(
   		fluidRow( ## Landsat data display
   		  h3("Landsat data"),
   		  radioButtons(inputId = "LSband", label="LS Band", inline=T, 
   		               choices = c("blue", "grn", "red", "nir", "swir1", "swir2", "bt", "ndvi", "nbr","ndwi", "evi-nbr")),
   		  radioButtons(inputId = "LSplotType", label="Plot Type", inline=T, 
   		               choices = c("Years", "DoY")),
   		  plotOutput("LSplot", height=330)
   		),
   		fluidRow( # Console for data selection
   		  column(width = 6,
   		         radioButtons(inputId = "falseCol", label="Band Composite", choices=c("321","432"))),
   		  column(width = 6,
   		         uiOutput("filepick"),
   		         textOutput("coords"))
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

	setwd("F:/Dropbox/LCSC/ABoVE/ABoVE_training/")
  #setwd("C:/Users/wanga/Dropbox/LCSC/ABoVE/buildTraining/")
  
  availableTiles = reactive({
    
    # generate list of available ABoVE tiles based on folder contents
    tileShapes = list.files("../../../ABoVE_samples/shapefiles")
    tileShapes = tileShapes[!grepl("zip",tileShapes)]
    
    tileSamplesIn = list.files("../../../ABoVE_samples/stamps",
                               pattern="EOSD")
    tileSamples = sapply(strsplit(tileSamplesIn, "_"), "[[",2)
    
    tileLSIn = list.files("../../../ABoVE_samples/LS",
                          pattern="filtered")
    tileLS = sapply(strsplit(tileLSIn,"_"), "[[",1)
    
    # Get the intersection and only present tiles that have all their data set up
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
      sampleShapes = readOGR(dsn = paste0('../../../ABoVE_samples/shapefiles/',input$tilepick),
                             layer = paste0("EOSD_",input$tilepick,"_sample"))
      LS_dat = fread(paste0("../../../ABoVE_samples/LS/",input$tilepick,"_LS_filtered.csv"))
                            
      tifs = list.files(tifPath, full.names=T, pattern="tif")
      
      tifsPan  = tifs[grepl("pan",list.files(tifPath,
                                             full.names=T,
                                             pattern="tif"))]
      tifsFull = tifs[grepl("mul",list.files(tifPath,
                                             full.names=T,
                                             pattern="tif"))]
      tifsMin  = tifs[grepl("min",list.files(tifPath,
                                             full.names=T,
                                             pattern="tif"))]
      dt = paste0("../buildTraining/",input$tilepick,"_trainingdt.csv")
      inSamps = sort(as.numeric(unique(sapply(strsplit(tifsFull,"_pp|_yd"), "[[", 2))))
                            
      return(list(sampleShapes=sampleShapes, LS_dat=LS_dat, tifsPan=tifsPan, tifsFull=tifsFull,tifsMin=tifsMin,dt=dt,inSamps=inSamps))
    }
    })
  
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
 			  row$i = max(which(row$flags$completed != 0))+1
 			}else{
 			  row$flags = data.frame(tile = rep(input$tilepick, length(inData()$inSamps)),
 			                         samp = inData()$inSamps,
 			                         surfaceType = rep(0, length(inData()$inSamps)),
 			                         vegForm = rep(0, length(inData()$inSamps)),
 			                         phenotype = rep(0, length(inData()$inSamps)),
 			                         leafType = rep(0, length(inData()$inSamps)),
 			                         density = rep(0, length(inData()$inSamps)),
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
 			  
 			#   
 			# 	row$flags = rbind(read.csv(inData()$dt), data.frame(tile=input$tilepick,
 			# 																											samp = inData()$inSamps[nrow(read.csv(inData()$dt))+1],
 			# 																											surfaceType = 0,
 			# 																											vegForm = 0,
 			# 																											phenotype = 0,
 			# 																											density = 0,
 			# 																											wetlandFlag = 0,
 			# 																											landUse = 0,
 			# 																											year = NA,
 			# 																											skipped = 0,
 			# 																											whySkipped=""))
 			# 	row$i = nrow(read.csv(inData()$dt))+1
 			# }else{
 			# 	row$flags = data.frame(tile=input$tilepick,
 			# 												 samp = inData()$inSamps[1],
 			# 												 surfaceType = 0,
 			# 												 vegForm = 0,
 			# 												 phenotype = 0,
 			# 												 density = 0,
 			# 												 wetlandFlag = 0,
 			# 												 landUse = 0,
 			# 												 year = NA,
 			# 												 skipped = 0,
 			# 												 whySkipped="")
 			# 	row$i = 1
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
      	 		lWet = switch(therow$wetlandFlag+1,"No Label","Not Wetland","Wetland")
      	 		lUse = switch(therow$landUse+1,"No Label","Urban","Agriculture" ,"Pasture", "Timber")
      	 		lConf = switch(therow$confidence+1,"No Label","Low","Medium" ,"High")
      	 		lYear = therow$year
      
      	 lrow = data.frame(tile = input$tilepick,
      	 									 samp = inData()$inSamps[row$i],
      	 									 surfaceType = lType,
      	 									 vegForm = lForm,
      	 									 phenotype = lPheno,
      	 									 leafType = lLeaf,
      	 									 density = lDense,
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
            
            samp =inData()$inSamps[row$i]
            band = input$LSband

            if(band == "evi-nbr"){
              nirBand = paste0("nir",samp)
              blueBand = paste0("blue",samp)
              redBand = paste0("red", samp)
              swirBand = paste0("swir2",samp)
              
              nir = inData()$LS_dat[[nirBand]]/10000
              swir = inData()$LS_dat[[swirBand]]/10000
              red = inData()$LS_dat[[redBand]]/10000
              blue = inData()$LS_dat[[blueBand]]/10000
              
              # a trick for distinguishing wetlands, thanks to Damien
              
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
              
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dtm, aes(x=date, y=value, color= variable)) + geom_point() +
                  theme_bw() + 
                  scale_color_manual("", values=c("evi" = "darkgreen", "nbr" = "red"), guide=F) +
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dtm, aes(x=doy, y=value, color= variable)) + geom_point() +
                  theme_bw() + 
                  scale_color_manual("", values=c("evi" = "darkgreen", "nbr" = "red"), guide=F) +
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
              }
              
            }else if(band == "ndwi"){
              
              nirBand = paste0("nir",samp)
              greenBand = paste0("grn",samp)
              
              nir = inData()$LS_dat[[nirBand]]/10000
              green = inData()$LS_dat[[greenBand]]/10000
              
              # additional information for determining wetlands, thanks to
              # http://onlinelibrary.wiley.com/doi/10.1002/2014WR015634/full
              
              ndwi = (green - nir) / (nir + green)
              plotLimits = quantile(ndwi, c(0.01, 0.99), na.rm=T)

              dt = data.table(date = strptime(inData()$LS_dat$date,format="%Y%j"), 
                              ndwi = ndwi,
                              doy = yday(strptime(inData()$LS_dat$date,format="%Y%j")))

              
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dt, aes(x=date, y=ndwi)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dt, aes(x=doy, y=ndwi)) + geom_point() +
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
              }
              
            }else{
              
              bandToPlot =paste0(band,samp)
              
              dt = data.table(date = strptime(inData()$LS_dat$date,format="%Y%j"), 
                              val = inData()$LS_dat[[bandToPlot]], 
                              doy = yday(strptime(inData()$LS_dat$date,format="%Y%j")))
              
              plotLimits = quantile(dt$val, c(0.01, 0.99), na.rm=T)
              
              
              if(input$LSplotType == "Years"){
                theLSplot = ggplot(data = dt, aes(x=date, y=val)) + geom_point() + 
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
              }else if(input$LSplotType == "DoY"){
                theLSplot = ggplot(data = dt, aes(x=doy, y=val)) + geom_point() + 
                  theme_bw() + 
                  theme(axis.text = element_text(size = 14, face="bold")) +
                  ylim(plotLimits)
                
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
        			if(grepl("WV02|WV03",input$filepick)){
        			  plotRGB(rgbTif,
        							  r=5,g=3,b=2, stretch = 'lin')
        			}else if(grepl("WV01",input$filepick)){
        			  plotRGB(rgbTif,
        			          r=1,g=1,b=1, stretch = 'lin')
        			}else{
        			  plotRGB(rgbTif,
        			          r=3,g=2,b=1, stretch = 'lin')
        			}
        		}
        		if(input$falseCol == "432"){
        		  if(grepl("WV02|WV03",input$filepick)){
        		    plotRGB(rgbTif,
        		            r=7,g=5,b=2, stretch = 'lin')
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
        		
        		if(grepl("WV02|WV03",input$filepick)){
        		  NDVI = brick(calc(rgbTif[[c(7,5)]], f_NDVI))
        		}else{
        		  NDVI = brick(calc(rgbTif[[c(4,3)]], f_NDVI))
        		}
        		
        
        		plotRGB(NDVI,
        						r=1,g=1,b=1, stretch="lin")
        		plot(NDVI, legend.only = T,
        				 col = gray.colors(100),
        				 smallplot = c(0.9,0.92,0.3,0.7))
        		plot(spTransform(inData()$sampleShapes[inData()$inSamps[isolate(row$i)],],CRSobj = crs(rgbTif)), col=NA, border = "red", add=T)
        		
    	  }
    	})
    	
    	
    	output$zoomPlot <- renderPlot({
    	  
    	  if(!is.null(inData()) & rowReady){
    	    
    	  
        		zoomTif =brick(inData()$tifsFull[grep(input$filepick,inData()$tifsFull)]) 
        		# get center of extent, then go back out 100
        		xext = extent(zoomTif)@xmax/2 + extent(zoomTif)@xmin/2
        		yext = extent(zoomTif)@ymax/2 + extent(zoomTif)@ymin/2
        		
        		miniExt = extent(c(xext-100, xext+100, yext-100, yext+100))
        		
        		if(grepl("WV02|WV03",input$filepick)){
          		# before cropping, set stretch
          		rast_b = linstretch(raster(zoomTif, layer=2), minmax = quantile(raster(zoomTif, layer=2), c(0.02,0.98)))
          		rast_g = linstretch(raster(zoomTif, layer=3), minmax = quantile(raster(zoomTif, layer=3), c(0.02,0.98)))
          		rast_r = linstretch(raster(zoomTif, layer=5), minmax = quantile(raster(zoomTif, layer=5), c(0.02,0.98)))
          		rast_n = linstretch(raster(zoomTif, layer=7), minmax = quantile(raster(zoomTif, layer=7), c(0.02,0.98)))
          		
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
        		
        		# world view has different band assignments
        		if(input$falseCol == "321"){
        		    plotRGB(zoomTif,
        		            r=3,g=2,b=1, stretch = 'lin')
        		}
        		if(input$falseCol == "432"){
        		    plotRGB(zoomTif,
        		            r=4,g=3,b=2, stretch = 'lin')
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
  		
}

	 

# Run the application 
shinyApp(ui = ui, server = server)

