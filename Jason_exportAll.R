source("init_library.R")
#Specify some parameters: Cell grid sizes and exclusion line names
horz.dim.cells = 90
vert.dim.cells = 10
surface.line = 'surface'
bottom.line = 'bottom_off'

#First thing to do is load in the metadata. This will be used to decide whether we will export just cell-stratified single targets, 
# or cell-stratified single targets, school regions and narrow-scope single targets

met = read_csv(local.EVMetadata.loc)

# Loop over files, determine if they are ready for export
met$export.type = 0
met$prepped = 0

for(i in 1:nrow(met)){
  #Filter out non-transect cases
  
  #TEMP: Type 2 only
  if(is.na(met$has_gps[i])){
    next
  }
  if(met$has_gps[i] == FALSE){
    next
  }
  if(!(met$category[i] %in% c("ES_Transect", "Non_Transect_Usable"))){
    next
  }
  #Filter out cases where a type has already been assigned
  if(met$export.type[i] != 0){
    next
  }
  #Open file, extract variable list, count schools
  ev = ev.restart(ev)
  evf = ev$OpenFile(met$evs[i])  
  aco.var.list = EVListAcoVars(evf)
  school.count=  EVRegionCountbyClass(evf, 'schools')
  #Handler if no-schools: Set export type to 1, if export variable is present mark file as prepped
  if(school.count == 0){
    met$export.type[i] = 1
    if("[T4 ST] Filtered in and out border single targets [for fish track detection 1]" %in% aco.var.list){
      met$prepped[i] = 1
    }
    EVCloseFile(evf)
    next
  }
  #Handler if schools: Set type to 2, mark as prepped if both school export variables are present
  met$export.type[i] = 2
  if('[T4 Sv] Fish-school samples [for aggregation detection]' %in% aco.var.list){
    if('[T2 ST] Sawada Filtered border single targets' %in% aco.var.list){
      met$prepped[i] = 1
    }
  }
  EVCloseFile(evf)
  next
}

write_csv(met, local.EVMetadata.loc)

#Exports
#The export flow should have three processes: 1) Identify the export class and form output paths; 2) Prep the necessary variables for export (gridding, exclusion, etc.); 3) Export the variables

export.path = paste0(local.export.root, "\\transect_exports")

met$exported = 0

for(i in 1:nrow(met)){
  #Filter only those files with an assigned export type, or those which have already been exported
  if(met$export.type[i] == 0){
    next
  }
  if(met$exported[i] == 1){
    next
  }
  #Open file, set export variables form paths. Some paths may not be used but making them now takes a trivial amount of time
  grand.test = try({
    ev = ev.restart(ev)
    evf = ev$OpenFile(met$evs[i])
    
    EVSetExportVariables(evf, export.variable.list)
    
    WSST.path = form_path(met$station_name[i], export.path, "WSST", ".csv")
    NSST.path = form_path(met$station_name[i], export.path, "NSST", ".csv")
    schools.path = form_path(met$station_name[i], export.path, "schools", ".csv")
    
    #Export wide-scope single targets by cell
    WSST = evf[['Variables']]$FindByName('[T4 ST] Filtered in and out border single targets [for fish track detection 1]')
    EVChangeVariableGrid(evf,
                         WSST[["Name"]],
                         timeDistanceGridType = 5,
                         depthGridType = 2,
                         timeDistanceGridDistance = horz.dim.cells,
                         depthGridDistance = vert.dim.cells,
                         EVLineName = bottom.line)
    EVsetExclusionLines(evf,
                        WSST[["Name"]],
                        newAboveExclusionLine = surface.line,
                        newBelowExclusionLine = bottom.line)
    WSST$ExportSingleTargetsByCellsAll(WSST.path)
    
    #If volume targets are present, export them by cell and region and the Sawada filtered single targets (narrow-scope single targets) around them
    if(met$export.type[i] == 2){
      #Exporting schools
      school.var = evf[['Variables']]$FindByName('[T4 Sv] Fish-school samples [for aggregation detection]')
      EVChangeVariableGrid(evf,
                           school.var[["Name"]],
                           timeDistanceGridType = 5,
                           depthGridType = 2,
                           timeDistanceGridDistance = horz.dim.cells,
                           depthGridDistance = vert.dim.cells,
                           EVLineName = bottom.line)
      EVsetExclusionLines(evf,
                          school.var[["Name"]],
                          newAboveExclusionLine = surface.line,
                          newBelowExclusionLine = bottom.line)
      EVIntegrationByRegionsByCellsExport(evf, acoVarName = school.var[["Name"]], regionClassName = 'schools', exportFn = schools.path, dataThreshold = -60)
      
      #Exporting NNST
      NSST = evf[['Variables']]$FindByName('[T2 ST] Sawada Filtered border single targets')
      EVsetExclusionLines(evf,
                          NSST[["Name"]],
                          newAboveExclusionLine = surface.line,
                          newBelowExclusionLine = bottom.line)
      NSST$ExportData(NSST.path)
    }
  })
  if(any(class(grand.test) == 'try-error')){
    met$exported[i] = 2
    print(i)
    EVSaveFile(evf)
    EVCloseFile(evf)
    next
  }
  met$exported[i] = 1
  print(i)
  EVSaveFile(evf)
  EVCloseFile(evf)
}

write_csv(met, local.EVMetadata.loc)
metadata_backup(met)
