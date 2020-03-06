source("init_library.R")

met = read_csv(local.EVMetadata.loc)
met = met %>% filter(has_gps == TRUE, category %in% c("ES_Transect", "Non_Transect_Useable"))

for(i in 1:nrow(met)){
  ev = ev.restart(ev)
  EV.File = ev$OpenFile(met$evs[i])
  
  aco.var.list = EVListAcoVars(EV.File)
  
  #Check to see if the wide-scope single targets object already exists. If so, close the file and move on to the next one
  if('[T4 ST] Filtered in and out border single targets [for fish track detection 1]' %in% aco.var.list){
    EVCloseFile(EV.File)
    next
  }
  
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
  sch.F.rbm = EV.File[['Variables']]$FindByName('[Tx Bn] Fish-school regions = FALSE')
  
  #Mask Processed TS Data by school FALSE bitmap
  if(!('[T4 TS] Fish-school samples = NO DATA' %in% aco.var.list)){
    mask.schoolRegionsTS.false = EVNewAcousticVar(EV.File,oldVarName = TSProc[["Name"]],enum=3)
    EVRenameAcousticVar(EV.File,acoVarName = mask.schoolRegionsTS.false[["Name"]],newName = '[T4 TS] Fish-school samples = NO DATA')
    TS.school.masked = EV.File[['Variables']]$FindByName('[T4 TS] Fish-school samples = NO DATA')
    sch.F.rbm = EV.File[['Variables']]$FindByName('[Tx Bn] Fish-school regions = FALSE')
    TS.school.masked$SetOperand(2, sch.F.rbm) #sch.F.rbm = [Tx Bn] Fish-school regions = FALSE
    TS.school.masked_PROP = TS.school.masked[['Properties']][['Mask']]
    TS.school.masked_PROP[['Value']] = 'no data'
  }
  
  if(!('[T4 ST] Filtered in and out border single targets [for fish track detection 1]' %in% aco.var.list)){
    M5<-EV.File[['Variables']]$FindByName('[T4 TS] Fish-school samples = NO DATA')
    single.targ.1 = EVNewAcousticVar(EV.File,oldVarName = M5[["Name"]],enum=75)
    EVRenameAcousticVar(EV.File,acoVarName = single.targ.1[["Name"]],newName = '[T4 ST] Filtered in and out border single targets [for fish track detection 1]')
    STDet<-EV.File[['Variables']]$FindByName('[T4 ST] Filtered in and out border single targets [for fish track detection 1]')
    STDet$SetOperand(2,apRaw) #apRaw = raw angular position data
    STDetProp<-STDet[['Properties']][['SingleTargetDetectionSplitBeamParameters']]
    STDetProp[['MaximumBeamCompensation']] <- 9
    xx = invisible(readline("Change the STD pulse envelope parameters to 0.5 min and 2 max"))
  }
  EVSaveFile(EV.File)
  EVCloseFile(EV.File)
  print(i)
  
}
