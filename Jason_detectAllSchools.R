#This is a script to detect schools in all echograms. It will open up the file, get as far as making the school object, detect schools, record the number of schools detected in the file, and then close.

#Startup sourcing. Reads libraries, local paths, all functions to be used
source("init_library.R")

##### Open Up the File #####

#Read in and filter useable stations.
ev.metadata = read.csv(local.EVMetadata.loc, stringsAsFactors = FALSE)
ev.repo = ev.metadata %>% filter(has_gps == TRUE, category %in% c("ES_Transect", "Non_Transect_Usable"))

#Define boundary line names
surface.line = 'surface'
bottom.line = 'bottom_off'

#Select an EV file to be opened. In my metadata, "station_name" is a unique identifier for a given station which is associated with both the EV file and all subsequent analyses/exports.
#I'm specifying i here only because I already know which station I want to open

ev.school.report = bind_cols(ev.names = rep(NA, nrow(ev.repo)), n.schools = rep(NA, nrow(ev.repo)))


# The main loop
for(i in 1:nrow(ev.repo)){
  grand.test = try({
  ev.file.name = ev.repo$evs[i]
  ev.station.name = ev.repo$station_name[i]
  
  
  
  #Create a COM link to Echoview
  EVAppObj <- ev.restart(EVAppObj) #This function should be robust to instances where Echoview is already open, but I suggest it be closed first anyway
  
  #Open Existing EV File
  EV.File <-EVOpenFile(EVAppObj,ev.file.name)$EVFile
  
  ##### Get as far as making the school object #####
  
  #Write the file name to the school report
  ev.school.report$ev.names[i] = ev.file.name
  
  #Establish the existing acoustic variables. Used later on to test to see if a process has already been performed
  aco.var.list = EVListAcoVars(EV.File)
  
  #If the school detection variable already exists, close the file unchanged
  if(('[T4 Sv] Fish-school samples [for aggregation detection]' %in% aco.var.list)){
    ev.school.report$n.schools[i] = EVRegionCountbyClass(EV.File, "schools")
    EVCloseFile(EV.File)
    next
  }
  
  # Creating the school object
  
  #Call to Sv, TS, and angular position raw variables.
  SvRaw<-EV.File[['Variables']]$FindByName('Fileset1: Sv pings T1')  #Sv pings
  TSRaw<-EV.File[['Variables']]$FindByName('Fileset1: TS pings T1')  #TS pings
  apRaw<-EV.File[['Variables']]$FindByName('Fileset1: angular position pings T1')  #angular position pings
  
  #Call to processed Sv variables
  SvProc<-EV.File[['Variables']]$FindByName('Full_Processed_Sv')
  TSProc = EV.File[['Variables']]$FindByName('Full_Processed_TS')
  
  
  ### PROCESS 1: School detection from fully processed Sv data #####
  #Create Data Range Bitmap for Fish-School Samples = TRUE from Sv
  #JWM: Possibly unnecessary
  if(!('[Tx Bn] Data Range Filter' %in% aco.var.list)){
    new.drb = EVNewAcousticVar(EV.File, oldVarName = SvProc[["Name"]], enum=1)
    name.new.drb = new.drb[["Name"]]
    EVRenameAcousticVar(EV.File, acoVarName = new.drb[["Name"]], newName = '[Tx Bn] Data Range Filter' )
    
  } 
  DRBit<-EV.File[['Variables']]$FindByName('[Tx Bn] Data Range Filter' ) #Calls to Data Range Bitmap variable
  DRBitProp<-DRBit[['Properties']][['DataRangeBitmap']] #Opens Variable Properties
  DRBitProp[['RangeMinimum']] <- -998 #Changes Minimum in-range data value
  DRBitProp[['RangeMaximum']] <- 999 #Changes maximum in-range data value
  
  #Create Mask for Fish-School Samples over Processed Sv
  if(!('[T4 Sv] Fish-school samples [for aggregation detection]' %in% aco.var.list)){
    new.mask = EVNewAcousticVar(EV.File,oldVarName = SvProc[["Name"]], enum=3) #Won't show in Dataflow window in Echoview until 2nd variable is added
    name.new.mask = new.mask[["Name"]]
    EVRenameAcousticVar(EV.File, acoVarName = new.mask[["Name"]], newName = '[T4 Sv] Fish-school samples [for aggregation detection]')
  }
  school.var<-EV.File[['Variables']]$FindByname('[T4 Sv] Fish-school samples [for aggregation detection]') #Call to Mask variable
  school.var$SetOperand(2,DRBit) #Make Data Range Bitmap 2nd operand
  school.varPropDis<-school.var[['Properties']][['Display']] #Call to Display properties 
  school.varPropDis[['ColorMinimum']] <- -60 #Set minimum color display to -60
  school.varPropDat<-school.var[['Properties']][['Data']] #Call to data properties
  school.varPropDat[['ApplyMinimumThreshold']] = TRUE #Set Data threshold to minimum color display
  
  
  #Set Exclusion Lines and Cell Grid for School Detection Sv Variable
  #Find the fish school variable
  school.var = EV.File[['Variables']]$FindByName('[T4 Sv] Fish-school samples [for aggregation detection]')
  #Set exclusion lines based on the surface and bottom lines defined above
  EVsetExclusionLines(EV.File, 
                      acoVarName = school.var[['Name']],
                      newAboveExclusionLine = surface.line,
                      newBelowExclusionLine = bottom.line) #For TS raw pings
  
  #Create a 90m x 10m Grid
  EVChangeVariableGrid(EV.File,
                       acoVarName = school.var[['Name']],
                       timeDistanceGridType = 5,
                       depthGridType = 2,
                       timeDistanceGridDistance = 90,
                       depthGridDistance = 10,
                       EVLineName = bottom.line)
  
  #Run School Detection from Processed data variable
  #If there's already schools, record the number of schools in the file, then move to the next one
  school.count = EVRegionCountbyClass(EV.File, 'schools')
  if(school.count != 0){
    ev.school.report$n.schools[i] = EVRegionCountbyClass(EV.File, "schools")
    EVCloseFile(EV.File)
    next
  }
  
  
  school.try = tryCatch({EVRegionClassFinder(EV.File, 'schools')}, error = function(e){return(FALSE)})
  if(class(school.try) != 'list'){
    EVNewRegionClass(EV.File, 'schools')
  }
  # School detection parameters #####
  EVSchoolsDetect(EVFile = EV.File,
                  acoVarName= school.var[['Name']],
                  outputRegionClassName = 'schools',
                  deleteExistingRegions = TRUE, #Good for when you're playing with detection parameters
                  distanceMode = "GPS distance",
                  maximumHorizontalLink = 0.5, #m
                  maximumVerticalLink = 0.25,#m
                  minimumCandidateHeight = 1.75, #m
                  minimumCandidateLength = 2.5, #m
                  minimumSchoolHeight = 3, #m
                  minimumSchoolLength = 7, #m
                  dataThreshold = -60) #check your schools and adjust these parameters if needed
  
  school.count.new = EVRegionCountbyClass(EV.File, 'schools')
  ev.school.report$n.schools[i] = school.count.new
  EVSaveFile(EV.File)
  EVCloseFile(EV.File)
  })
  if(class(grand.test) == 'try-error'){
    ev.school.report$n.schools[i] = NA
    next
  }
  print(i)
}

write_csv(ev.school.report, path = './reports/ev_school_report.csv')
