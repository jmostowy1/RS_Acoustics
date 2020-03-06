source("init_library.R")

met = read_csv(local.EVMetadata.loc)
met = met %>% filter(has_gps == TRUE, category %in% c("ES_Transect", "Non_Transect_Useable"))

#If there are schools present in an echogram, add a buffer
check = vector()

#I'll want to remove separated fish tracks at some point. Search for them here
for(i in 25:nrow(met)){
  ev = ev.restart(ev)
  evf = ev$OpenFile(met$evs[i])
  reg = EVListAllRegionNames(evf)
  if(any(str_detect(reg$class, 'eparate'))){
    check = check %>% append(i)
  }
  EVCloseFile(evf)
}
check.evs = met$evs[check]



######  Generating and detecting the school buffer region #####

#First, prep all ecograms with schools by adding the variables they require. This will involve manual adjustments to some variables #####


#Establish the existing acoustic variables. Used later on to test to see if a process has already been performed
for(i in 1:nrow(met)){
  if(met$new.school.count[i] == 0){
    next
  }
  ev = ev.restart(ev)
  EV.File = ev$OpenFile(met$evs[i])
  
  aco.var.list = EVListAcoVars(EV.File)
  
  #Call to Sv, TS, and angular position raw variables.
  SvRaw<-EV.File[['Variables']]$FindByName('Fileset1: Sv pings T1')  #Sv pings
  TSRaw<-EV.File[['Variables']]$FindByName('Fileset1: TS pings T1')  #TS pings
  apRaw<-EV.File[['Variables']]$FindByName('Fileset1: angular position pings T1')  #angular position pings
  
  #Call to processed Sv variables
  SvProc<-EV.File[['Variables']]$FindByName('Full_Processed_Sv')
  TSProc = EV.File[['Variables']]$FindByName('Full_Processed_TS')
  
  #Call to school variable
  school.var<-EV.File[['Variables']]$FindByname('[T4 Sv] Fish-school samples [for aggregation detection]')
  
  #Create Region Bitmap for Fish-School Regions = FALSE
  if(!('[Tx Bn] Fish-school regions = FALSE' %in% aco.var.list)){
    new.reg.bm = EVNewAcousticVar(EV.File,oldVarName = school.var[["Name"]],enum=36)
    EVRenameAcousticVar(EV.File,acoVarName = new.reg.bm[["Name"]], newName = '[Tx Bn] Fish-school regions = FALSE')
    xx = invisible(readline(prompt = 'Change the Fish-school regions = FALSE bitmap Region to Schools and Invert the Output, then press [ENTER]'))
    # AS OF ECHOVIEW V. 10, THERE IS NO COM OBJECT TO ACCESS REGION BITMAP VARIABLE PROPERTIES. SO YOU MUST MANUALLY GO IN AND SET THE REGION TO SCHOOLS AND SELECT 'INVERT OUTPUT'
  }
  
  #Create Mask of Fish-School Regions = FALSE over processed Sv data
  #JWM: Sv data with schools removed
  #M2 = school.false.mask
  if(!('[T4 Sv] Fish-school samples = no data' %in% aco.var.list)){
    school.false.mask = EVNewAcousticVar(EV.File,oldVarName = SvProc[["Name"]],enum=3) #Won't show in Dataflow window in Echoview until 2nd operand is added
    EVRenameAcousticVar(EV.File,acoVarName = school.false.mask[["Name"]],newName = '[T4 Sv] Fish-school samples = no data')
  }
  new.reg.bm = EV.File[['Variables']]$FindByName('[Tx Bn] Fish-school regions = FALSE')
  school.false.mask = EV.File[['Variables']]$FindByName('[T4 Sv] Fish-school samples = no data')
  school.false.mask$SetOperand(2, new.reg.bm) #Make Data Range Bitmap 2nd operand
  
  
  ##### Generating the border region 
  #Create Region Bitmap for Fish-School Regions = TRUE
  #JWM: Since this is just the inverted version of a bitmap we already made, I'm going to increase the automation by turning this variable from a region bitmap into a bitwise NOT variable
  if(!('[Tx Bn] Fish-school regions = TRUE' %in% aco.var.list)){
    school.true.bitmap = EVNewAcousticVar(EV.File, oldVarName = new.reg.bm[["Name"]],enum=20)
    EVRenameAcousticVar(EV.File, acoVarName = school.true.bitmap[["Name"]], newName = '[Tx Bn] Fish-school regions = TRUE')
    # AS OF ECHOVIEW V. 10, THERE IS NO COM OBJECT TO ACCESS REGION BITMAP VARIABLE PROPERTIES. SO YOU MUST MANUALLY GO IN AND SET THE REGION TO SCHOOLS
  }
  
  
  #Create Data Generator Variable with All Samples = 1
  #Essentially just an echogram of 1's, to be masked by the School Regions = TRUE bitmap
  if(!('[T4 Lin] All samples = 1' %in% aco.var.list)){
    dat.gen = EVNewAcousticVar(EV.File,oldVarName = SvProc[["Name"]],enum=31)
    EVRenameAcousticVar(EV.File,acoVarName = dat.gen[["Name"]],newName = '[T4 Lin] All samples = 1')
  }
  DGProp<-EV.File[['Variables']]$FindByName('[T4 Lin] All samples = 1')
  DGPropGen<-DGProp[['Properties']][['Generator']]
  DGPropGen[['OutputType']] <- 'Linear'
  DGPropGen[['Algorithm']] <- 'Constant'
  DGPropGen[['Constant']] <- 1
  
  #Create a mask of Fish - School samples = 1
  if(!('[T4 Lin] Fish-school samples = 1' %in% aco.var.list)){
    mask.fish.school = EVNewAcousticVar(EV.File, oldVarName = '[T4 Lin] All samples = 1',enum=3)
    EVRenameAcousticVar(EV.File,acoVarName = mask.fish.school[["Name"]],newName = '[T4 Lin] Fish-school samples = 1')
    mask.fish.school = EV.File[['Variables']]$FindByName('[T4 Lin] Fish-school samples = 1')
    RB2 = EV.File[['Variables']]$FindByName('[Tx Bn] Fish-school regions = TRUE')
    mask.fish.school$SetOperand(2,RB2)
  }
  mask.fish.school = EV.File[['Variables']]$FindByName('[T4 Lin] Fish-school samples = 1')
  RB2 = EV.File[['Variables']]$FindByName('[Tx Bn] Fish-school regions = TRUE')
  
  #Create Xxy statistic of X=11 and Y=61 for aggregation detection
  if(!('[T4 Lin] X=11 Y=801 max [for aggregation detection]' %in% aco.var.list)){
    xy.stat = EVNewAcousticVar(EV.File,oldVarName = '[T4 Lin] Fish-school samples = 1',enum=136)
    EVRenameAcousticVar(EV.File,acoVarName = xy.stat[["Name"]],newName = '[T4 Lin] X=11 Y=801 max [for aggregation detection]')
    #THERE IS NO COM OBJECT FOR THE XXY STATISTIC PROPERTY, SO YOU MUST MANUALLY CHANGE STATISTIC TO MAXIMUM, ROWS = 801, COLUMNS = 11
    xx = invisible(readline(prompt = 'Change the XY statistic to Maximum, Rows = 801, Columns = 11, then press [ENTER]'))
  }
  xy.stat = EV.File[['Variables']]$FindByName('[T4 Lin] X=11 Y=801 max [for aggregation detection]')
  EVNewRegionClass(EV.File,className = 'school+border')
  EVSaveFile(EV.File)
  EVCloseFile(EV.File)
  print(i)
}


#Create new region class for schools + border

#Run School Detection to select schools + border

met$borders.detected = 0


for(i in 1:nrow(met)){
  if(met$new.school.count[i] == 0){
    next
  }
  ev = ev.restart(ev)
  EV.File = ev$OpenFile(met$evs[i])
  
  xy.stat = EV.File[['Variables']]$FindByName('[T4 Lin] X=11 Y=801 max [for aggregation detection]')
  EVSchoolsDetect(EVFile = EV.File,
                  acoVarName= xy.stat[["Name"]],
                  outputRegionClassName = 'school+border',
                  deleteExistingRegions = TRUE, #Good for when you're playing with detection parameters
                  distanceMode = "GPS distance",
                  maximumHorizontalLink = 0.8, #m
                  maximumVerticalLink = 0.25,#m
                  minimumCandidateHeight = 1.75, #m
                  minimumCandidateLength = 2.5, #m
                  minimumSchoolHeight = 2, #m
                  minimumSchoolLength = 7, #m
                  dataThreshold = 0.10) #check your schools and adjust these parameters if needed
  EVSaveFile(EV.File)
  EVCloseFile(EV.File)
  print(i)
  met$borders.detected[i] = 1
}

recheck = met %>% filter(new.school.count > 0) 
check.ind = which(recheck$borders.detected == 0)

for(i in 1:nrow(recheck)){
  if(recheck$borders.detected[i] != 0){
    next
  }
  ev = ev.restart(ev)
  EV.File = ev$OpenFile(recheck$evs[i])
  
  xy.stat = EV.File[['Variables']]$FindByName('[T4 Lin] X=11 Y=801 max [for aggregation detection]')
  EVSchoolsDetect(EVFile = EV.File,
                  acoVarName= xy.stat[["Name"]],
                  outputRegionClassName = 'school+border',
                  deleteExistingRegions = TRUE, #Good for when you're playing with detection parameters
                  distanceMode = "GPS distance",
                  maximumHorizontalLink = 0.8, #m
                  maximumVerticalLink = 0.25,#m
                  minimumCandidateHeight = 1.75, #m
                  minimumCandidateLength = 2.5, #m
                  minimumSchoolHeight = 2, #m
                  minimumSchoolLength = 7, #m
                  dataThreshold = 0.10) #check your schools and adjust these parameters if needed
  EVSaveFile(EV.File)
  EVCloseFile(EV.File)
  print(i)
  recheck$borders.detected[i] = 1
}

recheck = recheck %>% select(evs, borders.detected)

met$borders.detected[which(met$evs %in% recheck$evs)] = 1

#Merging metadata changes back into original
all.met = read_csv(local.EVMetadata.loc)
met = met %>% select(evs, borders.detected)
all.met = left_join(all.met, met, by = "evs")
all.met = all.met %>% mutate(borders.detected = case_when(borders.detected %in% c(1,0) ~ borders.detected,
                                                          is.na(borders.detected) ~ 0))

write_csv(all.met, local.EVMetadata.loc)

#Backup metadata
metadata_backup(all.met)
