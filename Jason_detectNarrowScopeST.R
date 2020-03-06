### Apply the Sawada block to all files that need it (those that contain schools + school borders) using a copy/paste from template method
#Startup sourcing. Reads libraries, local paths, all functions to be used
source("init_library.R")

#Read in and filter useable stations.
met = read_csv(local.EVMetadata.loc)

#Define boundary line names
surface.line = 'surface'
bottom.line = 'bottom_off'


#Apply Sawada to All

for(i in 1:nrow(met)){
  if(met$borders.detected[i] == 0){
    next
  }
  if(met$sawada.applied[i] == 1){
    next
  }
  ev.file.name = met$evs[i]
  ev.station.name = met$station_name[i]
  
  #Create a COM link to Echoview
  ev <- ev.restart(ev) #This function should be robust to instances where Echoview is already open, but I suggest it be closed first anyway
  
  #Open Existing EV File
  EV.File <-EVOpenFile(ev, ev.file.name)$EVFile
  #Establish the existing acoustic variables. Used later on to test to see if a process has already been performed
  aco.var.list = EVListAcoVars(EV.File)
  
  #Call to Sv, TS, and angular position raw variables.
  SvRaw<-EV.File[['Variables']]$FindByName('Fileset1: Sv pings T1')  #Sv pings
  TSRaw<-EV.File[['Variables']]$FindByName('Fileset1: TS pings T1')  #TS pings
  apRaw<-EV.File[['Variables']]$FindByName('Fileset1: angular position pings T1')  #angular position pings
  
  #Call to processed variables. These are the variables that will be masked with the borders-only bitmap
  SvProc<-EV.File[['Variables']]$FindByName('Processed Sv')
  TSProc = EV.File[['Variables']]$FindByName('Processed TS')
  
  ##Define border region, filter TS samples into border
  #Create a Region Bitmap of Fish-schools + border regions = TRUE
  if(!('[Tx Bn] Fish-school + border regions = TRUE (1)' %in% aco.var.list)){
    rbm.fishSchoolsBorder.true = EVNewAcousticVar(EV.File, oldVarName = '[T4 Lin] X=11 Y=801 max [for aggregation detection]',enum=36)
    EVRenameAcousticVar(EV.File,acoVarName = rbm.fishSchoolsBorder.true[["Name"]],newName = '[Tx Bn] Fish-school + border regions = TRUE (1)')
    # ECHOVIEW DOES NOT HAVE A COM OBJECT FOR REGION BITMAP, SO YOU MUST GO INTO THE VARIABLE PROPERTIES AND MANUALLY SELECT "ANALYSIS" FOR REGION TYPE AND "school+border" FOR REGION CLASS.
    xx = invisible(readline(prompt = 'Change the Fish-school + border regions = TRUE region type to Analysis and region to School+Border, then press [ENTER]'))
  }
  
  #Create a borders-only bitmap (bitwise AND combination of the Schools = FALSE and Schools+border = TRUE)
  if(!('[Tx Bn] Borders-Only Bitmap' %in%aco.var.list)){
    bord.true.bm = EVNewAcousticVar(EVFile = EV.File, oldVarName = '[Tx Bn] Fish-school + border regions = TRUE (1)', 18)
    EVRenameAcousticVar(EV.File, bord.true.bm[["Name"]], '[Tx Bn] Borders-Only Bitmap')
    schools.false.bm = EV.File[['Variables']]$FindByName('[Tx Bn] Fish-school regions = FALSE')
    bord.true.bm$SetOperand(2, schools.false.bm)
  }
  bord.true.bm = EV.File[['Variables']]$FindByName('[Tx Bn] Borders-Only Bitmap')
  #Mask non-fish echo Sv and TS to generate TS/Sv border samples 
  if(!('[T4 Sv] Border samples' %in% aco.var.list)){
    mask.fishSchoolsBorder.true = EVNewAcousticVar(EV.File,oldVarName = SvProc[["Name"]],enum=3)
    M4 = EVRenameAcousticVar(EV.File,acoVarName = mask.fishSchoolsBorder.true[["Name"]],newName = '[T4 Sv] Border samples')
    if(class(M4) == "list"){
      M4 = M4$varObj
    }
    M4$SetOperand(2, bord.true.bm)
    M4Prop<-M4[['Properties']][['Mask']]
    M4Prop[['Value']] <- 'no data'
  }
  M4 = EV.File[['Variables']]$FindByName('[T4 Sv] Border samples')
  
  #Mask Fish-school + border regions = TRUE over Fish-school samples = NO DATA
  if(!('[T4 TS] Border samples' %in% aco.var.list)){
    mask.fishSchoolBorder.true = EVNewAcousticVar(EV.File,oldVarName = TSProc[["Name"]],enum=3)
    EVRenameAcousticVar(EV.File,acoVarName = mask.fishSchoolBorder.true[["Name"]],newName = '[T4 TS] Border samples')
    M6<-EV.File[['Variables']]$FindByName('[T4 TS] Border samples')
    M6$SetOperand(2, bord.true.bm) #RB3 = [Tx Bn] Fish-school + border regions = TRUE (1)
    M6Prop<-M6[['Properties']][['Mask']]
    M6Prop[['Value']] <- 'no data'
  }
  M6<-EV.File[['Variables']]$FindByName('[T4 TS] Border samples')

  
  #Detect single targets in the border (narrow-scope)
  if(!('[Tx ST] Border Single Targets' %in% aco.var.list)){
    M5<-EV.File[['Variables']]$FindByName('[T4 TS] Border samples')
    STDet2 = EVNewAcousticVar(EV.File,oldVarName = M5[["Name"]],enum=75)
    EVRenameAcousticVar(EV.File,acoVarName = STDet2[["Name"]],newName = '[Tx ST] Border Single Targets')
    STDet2<-EV.File[['Variables']]$FindByName('[Tx ST] Border Single Targets')
    STDet2$SetOperand(2,apRaw) #apRaw = raw angular position data
    STDetProp2<-STDet2[['Properties']][['SingleTargetDetectionSplitBeamParameters']]
    STDetProp2[['MaximumBeamCompensation']] <- 6
  }
  
  STDet2<-EV.File[['Variables']]$FindByName('[Tx ST] Border Single Targets')
  
  #5x5m grid for border single targets
  EVChangeVariableGrid(EV.File, 
                       acoVarName = STDet2[["Name"]],
                       timeDistanceGridType = 5,
                       depthGridType = 2,
                       timeDistanceGridDistance = 5,
                       depthGridDistance = 5,
                       EVLineName = bottom.line) 
  
#Create cell statistic of mean border ST
  if(!('[T2 TS] Cell-mean TS of single targets' %in% aco.var.list)){
    cellst = EVNewAcousticVar(EV.File,oldVarName = STDet2[['Name']],enum=135) #sets up cell statistic [single targets] variable
    EVRenameAcousticVar(EV.File,acoVarName = cellst[["Name"]],newName = '[T2 TS] Cell-mean TS of single targets')
    cellst<-EV.File[['Variables']]$FindByName('[T2 TS] Cell-mean TS of single targets') #calls to cell statistic [single targets] variable
    cellst$SetOperand(2,apRaw) #changes 2nd operand in cell statistic to angular position
    EVChangeVariableGrid(EV.File, 
                         acoVarName = cellst[["Name"]],
                         timeDistanceGridType = 5,
                         depthGridType = 2,
                         timeDistanceGridDistance = 5,
                         depthGridDistance = 5,
                         EVLineName = bottom.line) 
  }
  #Fetch cell mean ST, apply grid (in case not already applied)
  cellst<-EV.File[['Variables']]$FindByName('[T2 TS] Cell-mean TS of single targets')
  EVChangeVariableGrid(EV.File,acoVarName = '[T2 TS] Cell-mean TS of single targets',
                       timeDistanceGridType = 5,
                       depthGridType = 2,
                       timeDistanceGridDistance = 5,
                       depthGridDistance = 5,
                       EVLineName = bottom.line) 
  
  #Grid Sv border samples
  bordsv = EV.File[['Variables']]$FindByName('[T4 Sv] Border samples')
  EVChangeVariableGrid(EV.File, 
                       acoVarName = STDet2[["Name"]],
                       timeDistanceGridType = 5,depthGridType = 2,
                       timeDistanceGridDistance = 5,
                       depthGridDistance = 5,
                       EVLineName = bottom.line) 
  
  #Create cell statistic variable fom Sv border samples
  if(!('[T2 Sv] Cell-mean Sv of samples' %in% aco.var.list)){
    cellsv = EVNewAcousticVar(EV.File,oldVarName = '[T4 Sv] Border samples',enum=133)
    EVRenameAcousticVar(EV.File,acoVarName = cellsv[["Name"]],newName = '[T2 Sv] Cell-mean Sv of samples')
  }
  cellsv<-EV.File[['Variables']]$FindByName('[T4 Sv] Border samples')
  EVChangeVariableGrid(EV.File, 
                       acoVarName = cellsv[["Name"]],
                       timeDistanceGridType = 5,
                       depthGridType = 2,
                       timeDistanceGridDistance = 5,
                       depthGridDistance = 5,
                       EVLineName = bottom.line) 
  #Create a 5 ping x 5m Grid in cell statistic
  EVChangeVariableGrid(EV.File, 
                       acoVarName = '[T2 Sv] Cell-mean Sv of samples',
                       timeDistanceGridType = 5,
                       depthGridType = 2,
                       timeDistanceGridDistance = 5,
                       depthGridDistance = 5,
                       EVLineName = bottom.line) 
  
  #Create a minus variable for Sv-TS
  if(!('[T2 dB] n (ind/m3 in dB)' %in% aco.var.list)){
    EVNewAcousticVar(EV.File,oldVarName = '[T2 Sv] Cell-mean Sv of samples',enum=7)
    EVRenameAcousticVar(EV.File,acoVarName = 'Minus 1',newName = '[T2 dB] n (ind/m3 in dB)')
    SvminusTS<-EV.File[['Variables']]$FindByName('[T2 dB] n (ind/m3 in dB)')
    SvminusTS$SetOperand(2,cellst)
  }
  SvminusTS<-EV.File[['Variables']]$FindByName('[T2 dB] n (ind/m3 in dB)')
  
  #Create a dB to linear variable for Sv-TS
  if(!('[T2 Lin] n (ind/m3)' %in% aco.var.list)){
    db.t.lin = EVNewAcousticVar(EV.File,oldVarName = '[T2 dB] n (ind/m3 in dB)',enum=21)
    EVRenameAcousticVar(EV.File,acoVarName = db.t.lin[["Name"]],newName = '[T2 Lin] n (ind/m3)')
  }
  #Create a formula variable for the number of fish in the sampling volume
  if(!('[T2 Lin] Nv (ind/sampling volume)' %in% aco.var.list)){
    
    invisible(readline("Copy-paste the Sawada block template.\nThen:\nBlock 1 Op {Border ST , Border Sv};\nBlock 2 Op {n(ind/m), ns(ind/m3)};\nBlock 3 Op {n(ind/m)};\nBlock 4 Op {M%, Nv}"))
    #NOTE: ECHOVIEW DOES NOT HAVE A COM OBJECT TO ENTER THE FORMULA! You must do this manually by selecting the 'Formula' tab under the variable properties and clicking on 'Edit Formula'.
    #The Nv equation can be found in the variable properties from the EV template or in Sawada et al., 1993 (Equation 4)
  }
  #Grid all Sawada block variables
  
  
  #Create a 5 ping x 5m Grid in cell statistic
  EVChangeVariableGrid(EV.File,acoVarName = '[T2 Lin] ns (ind/m3)',
                       timeDistanceGridType = 5,
                       depthGridType = 2,
                       timeDistanceGridDistance = 5,
                       depthGridDistance = 5,
                       EVLineName = bottom.line) 
  EVChangeVariableGrid(EV.File,acoVarName = '[T2 Lin] M (%)',
                       timeDistanceGridType = 5,
                       depthGridType = 2,
                       timeDistanceGridDistance = 5,
                       depthGridDistance = 5,
                       EVLineName = bottom.line) 
  EVChangeVariableGrid(EV.File,acoVarName = '[T2 Lin] Nv (ind/sampling volume)',
                       timeDistanceGridType = 5,
                       depthGridType = 2,
                       timeDistanceGridDistance = 5,
                       depthGridDistance = 5,
                       EVLineName = bottom.line) 
  
  #NOTE: ECHOVIEW DOES NOT HAVE A COM OBJECT TO ENTER THE FORMULA! You must do this manually by selecting the 'Formula' tab under the variable properties and clicking on 'Edit Formula'.
  #The M equation can be found in the variable properties from the EV template or in Sawada et al., 1993 (Equation 7)
  
  #Create Formula variable to filter Nv and M%
  MNvThresh = EV.File[['Variables']]$FindByName('[T2 Bn] M<threshold AND Nv<threshold = TRUE')
  
  #NOTE: ECHOVIEW DOES NOT HAVE A COM OBJECT TO ENTER THE FORMULA! You must do this manually by selecting the 'Formula' tab under the variable properties and clicking on 'Edit Formula'.
  #Set thresholds for M% and Nv (such as M is less than or equal to 100 and Nv is less than or equal to 0.1)
  
  ##### Detecting Sawada-filtered (narrow-scope) single targets #####
  #Create Single target detection split beam method 2 for sawada filtered border targets
  if(!('[T2 ST] Sawada Filtered border single targets' %in% aco.var.list)){
    sawada.ST = EVNewAcousticVar(EV.File,oldVarName = '[Tx ST] Border Single Targets', enum=3)
    sawada.ST = EVRenameAcousticVar(EV.File,acoVarName = sawada.ST[["Name"]],newName = '[T2 ST] Sawada Filtered border single targets')
    if(is.list(sawada.ST)){
      sawada.ST = sawada.ST$varObj
    }
    sawada.ST = EV.File[['Variables']]$FindByName('[T2 ST] Sawada Filtered border single targets')
    sawada.ST$SetOperand(2, MNvThresh)
  }

  EVSaveFile(EV.File)
  EVCloseFile(EV.File)
  met$sawada.applied[i] = 1
  print(i)
  
}
