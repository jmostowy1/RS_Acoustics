source("init_library.R")

grid.size = 0.5
surface.line = 'surface'
bottom.line = 'bottom_off'

met = read_csv(local.EVMetadata.loc)

output.path = paste0(local.export.root,"\\school_approximations_1m")

save.file = FALSE

#The purpose of this script is to generate 1mx1m approximations of the school dimensions in each EV file. These approximations will serve as a more accurate way to associate single targets with the proper schools
#This script will filter only stations containing schools, copy the Sv export data, apply a 1mx1m grid to the copy, export the Sv data by region by cell, and then save and close the file.

for(i in 1:nrow(met)){
  #Case handling: Only operate on files where schools have been detected (as determined by new.school.count > 0)
  if((is.na(met$new.school.count[i]))|(met$new.school.count[i] == 0)){
    next
  }
  #Open file
  ev = ev.restart(ev)
  evf = ev$OpenFile(met$evs[i])
  
  #If the school copy already exists, skip this part
  aco.var.list = EVListAcoVars(evf)
  if(!("School Copy for Fine Gridding" %in% aco.var.list)){
    #Find school variable, copy it, rename copy
    school.var = EVAcoVarNameFinder(evf, "[T4 Sv] Fish-school samples [for aggregation detection]")$EVVar
    school.copy = EVNewAcousticVar(evf, school.var[["Name"]], 16)
    EVRenameAcousticVar(evf, school.copy[["Name"]], "School Copy for Fine Gridding")
  }
  school.copy = EVAcoVarNameFinder(evf, "School Copy for Fine Gridding")$EVVar
  #Apply fine grid to copy
  EVChangeVariableGrid(evf,
                       school.copy[["Name"]],
                       timeDistanceGridType = 5,
                       depthGridType = 2,
                       timeDistanceGridDistance = grid.size,
                       depthGridDistance = grid.size,
                       EVLineName = bottom.line)
  EVsetExclusionLines(evf,
                      school.copy[["Name"]],
                      newAboveExclusionLine = surface.line,
                      newBelowExclusionLine = bottom.line)
  
  #Form export path
  export.path = form_path(met$station_name[i], output.path, 'fineGridSchools', '.csv')
  #Update export variables (just in case)
  EVSetExportVariables(evf, export.variable.list)
  #Export by region by cell 
  EVIntegrationByRegionsByCellsExport(evf, school.copy[["Name"]], 'schools', exportFn = export.path, dataThreshold = -60)
  #Save and close file
  if(save.file){
    EVSaveFile(evf)
  }
  EVCloseFile(evf)
}
