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
	   		  uiOutput("tilepick")
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
	   			             label = "Permanent Snow/Ice")
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
	   									 label = "Forest")
	   		),
				fluidRow(
					hr(),
					p("PHENOTYPE"),
					actionButton(inputId = "DBF",
											 label = "Deciduous Broadleaf"),
					actionButton(inputId = "ENF",
											 label = "Evergreen Needleleaf"),
					actionButton(inputId = "MXF",
											 label = "Mixed Forest")
				),
	   		fluidRow(
	   			hr(),
	   			p("DENSITY"),
	   			actionButton(inputId = "isSparse",
	   			             label = "Sparse"),
	   			actionButton(inputId = "isOpen",
	   									 label = "Open"),
	   			actionButton(inputId = "isDense",
	   									 label = "Dense")
	   		),
	   		fluidRow(
	   			hr(),
	   			p("WETLAND?"),
	   			actionButton(inputId = "isWet",
	   									 label = "Wetland"),
	   			actionButton(inputId = "isDry",
	   									 label = "Not Wetland")
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
				               label = "Timber")
				),
	   		fluidRow(
	   			hr(),
	   			tableOutput("trainingRow"),
	   			br(),
	   			tableOutput("legendRow")
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
				               choices = list("Not Skip", "Cloud", "Unclear", "I'm Lazy", "Other"))
				)
	   	)
	   	),
   	column(width=7,
   	wellPanel(
   		fluidRow( ## Landsat data display
   		  h3("Landsat data"),
   		  radioButtons(inputId = "LSband", label="LS Band", inline=T, 
   		               choices = c("blue", "grn", "red", "nir", "swir1", "swir2", "bt", "ndvi", "nbr")),
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

	setwd("F:/Dropbox/LCSC/ABoVE/buildTraining/")
  #setwd("C:/Users/wanga/Dropbox/LCSC/ABoVE/buildTraining/")
  
	tile = "Bh11v11"
	#tile = "sampleBh11v11"
	tifPath = paste0("../../../ABoVE_samples/EOSD_",input$tilepick, "_sample")
	shapePath = paste0('../../../ABoVE_samples/shapefiles/_sample',input$tilepick)
	
	sampleShapes = readOGR(dsn = shapePath,
												 layer = paste0("EOSD_",input$tilepick,"_sample"))
	
	LS_dat = fread(paste0("../../../ABoVE_samples/LS/",input$tilepick,"_LS_filtered.csv"))
	
	tifs		 = list.files(tifPath,
												full.names=T,
												pattern="tif")
	
	tifsPan  = tifs[grepl("pan",tifs)]
	tifsFull = tifs[grepl("mul",tifs)]
	tifsMin = tifs[grepl("min",tifs)]
	
	# stolen from https://spatiallyexplicit.wordpress.com/2011/06/07/crop-circles/
	linstretch<-function(img,minmax=NA){
		#if(is.na(minmax)) minmax<-c(min(getValues(img),na.rm=T),max(getValues(img),na.rm=T))
		temp<-calc(img,fun=function(x) (255*(x-minmax[1]))/(minmax[2]-minmax[1]))
		#set all values above or below minmax to 0 or 255
		temp[temp<0]<-0;temp[temp>255]<-255;
		return(temp)
	}
	
	# which samples do we have files for
	inSamps = sort(as.numeric(unique(sapply(strsplit(tifsFull,"_pp|_yd"), "[[", 2))))
	
	# tifsHave = as.numeric(sapply(strsplit(tifsFull,"_"), "[[", 8))

	dt = paste0(tileSample,"_trainingdt.csv")
	
	if(dt %in% list.files()){
			row <- reactiveValues(
			flags = rbind(read.csv(dt), data.frame(tile=tileSample,
																						 samp = inSamps[nrow(read.csv(dt))+1],
																						 surfaceType = 0,
																						 vegForm = 0,
																						 phenotype = 0,
																						 density = 0,
																						 wetlandFlag = 0,
																						 landUse = 0,
																						 year = NA,
																						 skipped = 0,
																						 whySkipped="")),
			i = nrow(read.csv(dt))+1
			)
	}else{
		row <- reactiveValues(
			flags = data.frame(tile=tileSample,
							samp = inSamps[1],
							surfaceType = 0,
							vegForm = 0,
							phenotype = 0,
							density = 0,
							wetlandFlag = 0,
							landUse = 0,
							year = NA,
							skipped = 0,
							whySkipped=""),
			i = 1)
	}
	
	inTifsFull = reactive({
	  # gather list of all images available for a sample		
		vars = paste0(sapply(strsplit(tifsFull[grep(paste0("pp",inSamps[row$i],"_"), tifsFull)], "pp|sample/"),"[[",2),
									"pp",
									inSamps[row$i],
									"_yd",
									sapply(strsplit(tifsFull[grep(paste0("pp",inSamps[row$i],"_"), tifsFull)], "_yd|\\.tif|_mul|_pan|_min"),"[[",2))

		return(as.list(vars))
	})

	availableTiles = reactive({
	  
	  # generate list of available ABoVE tiles based on folder contents
	  tileShapes = list.files("../../../ABoVE_samples/shapefiles")
	  tileSamplesIn = list.files("../../../ABoVE_samples/",
	                           pattern="EOSD")
	  tileSamples = sapply(strsplit(tileSamplesIn, "_"), "[[",2)
	  
	  tileLSIn = list.files("../../../ABoVE_samples/LS",
	                        pattern="filtered")
	  tileLS = sapply(strsplit(tileLSIn,"_"), "[[",1)
	})
	
	# pickers
	
	output$filepick = renderUI({
		selectInput(inputId = "filepick",
								label = "Select File",
								choices = inTifsFull())
	})
	
	output$tilepick = renderUI({
	  selectInput(inputId = "tilepick",
	              label = "Select Tile",
	              choices = availableTiles())
	})
	
		
	########
	########
	########
	
	
	########
	########
	########
	
	# Determine the year the sample is being picked from 
	output$aYear = renderText({
		as.numeric(substr(strsplit(input$filepick,"_yd")[[1]][2],1,4))
	})

	output$trainingRow <- renderTable({
		row$flags[row$i,"year"] = as.numeric(substr(strsplit(input$filepick,"_yd")[[1]][2],1,4))
		row$flags[row$i,]
	}, digits=0)
	
	
	 output$legendRow <- renderTable({
	 		
	 		theRow = row$flags[row$i,]
	 		lType = switch(theRow$surfaceType+1,"No Label","Water", "Bare", "Veg","Ice")
	 		lForm = switch(theRow$vegForm+1,"No Label","Moss","Grass","Shrub","Tree")
	 		lPheno = switch(theRow$phenotype+1,"No Label", "Deciduous", "Evergreen","Mixed")
	 		lDense = 	switch(theRow$density+1,"No Label","Sparse", "Open","Dense")
	 		lWet = switch(theRow$wetlandFlag+1,"No Label","Not Wetland","Wetland")
	 		lUse = switch(theRow$landUse+1,"No Label","Urban","Agriculture" ,"Pasture", "Timber")
	 		lYear = theRow$year

	 lRow = data.frame(tile = "tileSample",
	 									 samp = inSamps[row$i],
	 									 surfaceType = lType,
	 									 vegForm = lForm,
	 									 phenotype = lPheno,
	 									 density = lDense,
	 									 wetlandFlag = lWet,
	 									 landUse = lUse,
	 									 year = lYear,
	 									 skipped = " ",
	 									 whySkipped = " ")
	 		
	 lRow
	 },
	 digits=0)
	
	 
	# Record label, save, and move to next sample
	observeEvent(input$save,
							 {
							 	write.csv(row$flags, dt, row.names=F)
							 	row$i = row$i + 1
							 	row$flags = rbind(row$flags, data.frame(tile=tileSample,
							 																					samp = inSamps[row$i],
							 																					surfaceType = 0,
							 																					vegForm = 0,
							 																					phenotype = 0,
							 																					density = 0,
							 																					wetlandFlag = 0,
							 																					landUse = 0,
							 																					year=NA,
							 																					skipped = 0,
							 																					whySkipped=""))
							 })
	
	observeEvent(input$skip,
							 {
							 	row$flags[row$i,"skipped"]=1
							 	write.csv(row$flags, dt, row.names=F)
							 	row$i = row$i + 1
							 	row$flags = rbind(row$flags, data.frame(tile=tileSample,
							 																					samp = inSamps[row$i],
							 																					surfaceType = 0,
							 																					vegForm = 0,
							 																					phenotype = 0,
							 																					density = 0,
							 																					wetlandFlag = 0,
							 																					landUse = 0,
							 																					year=NA,
							 																					skipped = 0,
							 																					whySkipped=""))
							 })
	
	
	
	
	# all them buttons
	observeEvent(input$isWater,
							 {
							 	row$flags[row$i,"surfaceType"]=1
							 })
	observeEvent(input$isBare,
	             {
	               row$flags[row$i,"surfaceType"]=2
	             })
	observeEvent(input$isVeg,
							 {
							 	row$flags[row$i,"surfaceType"]=3
							 })
	observeEvent(input$isIce,
	             {
	               row$flags[row$i,"surfaceType"]=4
	             })
	observeEvent(input$isMoss,
							 {
							 	row$flags[row$i,"vegForm"]=1
							 	row$flags[row$i,"surfaceType"]=3
							 })
	observeEvent(input$isGrass,
							 {
							 	row$flags[row$i,"vegForm"]=2
							 	row$flags[row$i,"surfaceType"]=3
							 })
	observeEvent(input$isShrub,
							 {
							 	row$flags[row$i,"vegForm"]=3
							 	row$flags[row$i,"surfaceType"]=3
							 })
	observeEvent(input$isTree,
							 {
							 	row$flags[row$i,"vegForm"]=4
							 	row$flags[row$i,"surfaceType"]=3
							 })
	observeEvent(input$isSparse,
	             {
	               row$flags[row$i,"density"]=1
	               row$flags[row$i,"surfaceType"]=3
	             })
	observeEvent(input$isOpen,
							 {
							 	row$flags[row$i,"density"]=2
							 	row$flags[row$i,"surfaceType"]=3
							 })
	observeEvent(input$isDense,
							 {
							 	row$flags[row$i,"density"]=3
							 	row$flags[row$i,"surfaceType"]=3
							 })
	observeEvent(input$isDry,
							 {
							 	row$flags[row$i,"wetlandFlag"]=1
							 })
	observeEvent(input$isWet,
							 {
							 	row$flags[row$i,"wetlandFlag"]=2
							 })
	observeEvent(input$DBF,
							 {
							 	row$flags[row$i,"phenotype"]=1
							 	row$flags[row$i,"surfaceType"]=3
							 })
	observeEvent(input$ENF,
							 {
							 	row$flags[row$i,"phenotype"]=2
							 	row$flags[row$i,"surfaceType"]=3
							 })
	observeEvent(input$MXF,
							 {
							 	row$flags[row$i,"phenotype"]=3
							 	row$flags[row$i,"surfaceType"]=3
							 })
	observeEvent(input$isUrban,
	             {
	               row$flags[row$i,"landUse"]=1
	             })
	observeEvent(input$isAgriculture,
	             {
	               row$flags[row$i,"landUse"]=2
	             })
	observeEvent(input$isPasture,
	             {
	               row$flags[row$i,"landUse"]=3
	             })
	observeEvent(input$isTimber,
	             {
	               row$flags[row$i,"landUse"]=4
	             })
	observeEvent(input$skipBox,
							 {
							 	row$flags[row$i,"whySkipped"]=input$skipBox
							 	row$flags[row$i,"skipped"] = 1
							 })

	output$diag = renderText({
		paste0("row$i = ",row$i," and nrow(row$flags) = ", nrow(row$flags))
	})	
	
  output$LSplot <- renderPlot({
    samp =inSamps[row$i]
    band = input$LSband
    bandToPlot =paste0(band,samp)
    dt = data.table(date = strptime(LS_dat$date,format="%Y%j"), val = LS_dat[[bandToPlot]], doy = yday(strptime(LS_dat$date,format="%Y%j")))
    if(input$LSplotType == "Years"){
      theLSplot = ggplot(data = dt, aes(x=date, y=val)) + geom_point()
    }else if(input$LSplotType == "DoY"){
      theLSplot = ggplot(data = dt, aes(x=doy, y=val)) + geom_point()
    }
    theLSplot
  })
	
	output$RGBplot <- renderPlot({
		rgbTif =brick(tifsFull[grep(input$filepick,tifsFull)]) 
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
		            r=7,g=5,b=3, stretch = 'lin')
		  }else if(grepl("WV01",input$filepick)){
		    plotRGB(rgbTif,
		            r=1,g=1,b=1, stretch = 'lin')
		  }else{
		    plotRGB(rgbTif,
		            r=4,g=3,b=2, stretch = 'lin')
		  }
		}
		plot(spTransform(sampleShapes[inSamps[isolate(row$i)],],CRSobj = crs(rgbTif)), col=NA, border = "red", add=T)
	})
	
	output$NDVIplot <- renderPlot({
		rgbTif =brick(tifsFull[grep(input$filepick,tifsFull)]) 
		
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
		plot(spTransform(sampleShapes[inSamps[isolate(row$i)],],CRSobj = crs(rgbTif)), col=NA, border = "red", add=T)
	})
	
	
	output$zoomPlot <- renderPlot({
		zoomTif =brick(tifsFull[grep(input$filepick,tifsFull)]) 
		# get center of extent, then go back out 100
		xext = extent(zoomTif)@xmax/2 + extent(zoomTif)@xmin/2
		yext = extent(zoomTif)@ymax/2 + extent(zoomTif)@ymin/2
		
		miniExt = extent(c(xext-100, xext+100, yext-100, yext+100))
		
		# before cropping, set stretch
		rast_b = linstretch(raster(zoomTif, layer=1), minmax = quantile(raster(zoomTif, layer=1), c(0.02,0.98)))
		rast_g = linstretch(raster(zoomTif, layer=2), minmax = quantile(raster(zoomTif, layer=2), c(0.02,0.98)))
		rast_r = linstretch(raster(zoomTif, layer=3), minmax = quantile(raster(zoomTif, layer=3), c(0.02,0.98)))
		rast_n = linstretch(raster(zoomTif, layer=4), minmax = quantile(raster(zoomTif, layer=4), c(0.02,0.98)))
		
		zoomTif_str = stack(rast_b, rast_g, rast_r, rast_n)
		
		zoomTif = crop(zoomTif_str, miniExt)
		
		# world view has different band assignments
		if(input$falseCol == "321"){
		  if(grepl("WV02|WV03",input$filepick)){
		    plotRGB(zoomTif,
		            r=5,g=3,b=2, stretch = 'lin')
		  }else if(grepl("WV01",input$filepick)){
		    plotRGB(zoomTif,
		            r=1,g=1,b=1, stretch = 'lin')
		  }else{
		    plotRGB(zoomTif,
		            r=3,g=2,b=1, stretch = 'lin')
		  }
		}
		if(input$falseCol == "432"){
		  if(grepl("WV02|WV03",input$filepick)){
		    plotRGB(zoomTif,
		            r=7,g=5,b=3, stretch = 'lin')
		  }else if(grepl("WV01",input$filepick)){
		    plotRGB(zoomTif,
		            r=1,g=1,b=1, stretch = 'lin')
		  }else{
		    plotRGB(zoomTif,
		            r=4,g=3,b=2, stretch = 'lin')
		  }
		}
		
		plot(spTransform(sampleShapes[inSamps[isolate(row$i)],],CRSobj = crs(zoomTif)), col=NA, border = "red", add=T)
	})
	
	output$panPlot <- renderPlot({
		#if(any(grepl(paste0("pp",inSamps[row$i]), tifsPan))){
		if(any(grepl(input$filepick, tifsPan))){
						zoomPan = brick(tifsPan[grep(input$filepick,tifsPan)]) 
			
			# # get center of extent, then go back out 100
			# xext = extent(zoomPan)@xmax/2 + extent(zoomPan)@xmin/2
			# yext = extent(zoomPan)@ymax/2 + extent(zoomPan)@ymin/2
			# 
			# miniExt = extent(c(xext-100, xext+100, yext-100, yext+100))
			# zoomPan = crop(zoomPan, miniExt)
			# 
			plotRGB(zoomPan,
							r=1,g=1,b=1,stretch="lin")
			plot(spTransform(sampleShapes[inSamps[isolate(row$i)],],CRSobj = crs(zoomPan)), col=NA, border = "red", add=T)
		}else{
			rgbTif =brick(tifsFull[grep(input$filepick,tifsFull)]) 
			plotRGB(rgbTif,
							r=3,g=2,b=1,stretch="lin")
			plot(spTransform(sampleShapes[inSamps[isolate(row$i)],],CRSobj = crs(rgbTif)), col=NA, border = "red", add=T)
		}
	})
	
	output$coords = renderText({
		
		theExt = extent(spTransform(sampleShapes[inSamps[row$i],],CRSobj = crs("+proj=longlat +datum=WGS84")))
		lats = mean(theExt@ymin, theExt@ymax)
		lons = mean(theExt@xmin, theExt@xmax)
		paste0("LAT: ", round(lats,3), "   LON: ", round(lons,3))
	})
		
}

	 

# Run the application 
shinyApp(ui = ui, server = server)

