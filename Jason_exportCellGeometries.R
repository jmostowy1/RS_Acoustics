source("init_library.R")

met = read_csv(local.EVMetadata.loc)

geoms.exported = rep(NA, nrow(met))
for(i in 1:nrow(met)){
  #Case handing: Skip non-processed EVs
  if(is.na(met$processed[i])){
    print(i)
    next
  }
  ev = ev.restart(ev)
  grand.test = try({
    station.name = met$station_name[i]
    output.path = paste0(local.export.root, "//cell_geometries") %>% normalizePath()
    output.file.name = form_path(station.name, output.path, 'cell_geoms_fine', '.csv')
    # l = list.files(output.path, full.names = TRUE) %>% normalizePath()
    # 
    # #Skip EVs where the geometry file already exists
    # if(output.file.name %in% l){
    #   print(i)
    #   next
    # }
    
    
    evf = ev$OpenFile(met$evs[i])
    
    aco.var.list = EVListAcoVars(evf)
    
    if(!("ST Geom Export" %in% aco.var.list)){
      TSProc = EVAcoVarNameFinder(evf, "Processed TS")$EVVar
      angRaw = EVAcoVarNameFinder(evf, "Fileset 1: angular position pings T1") $EVVar
      geom.copy = EVNewAcousticVar(evf, TSProc[["Name"]], 75)
      geom.copy$SetOperand(2, angRaw) 
      EVRenameAcousticVar(evf, geom.copy[["Name"]], "ST Geom Export")
    }
    
    geom.copy= EVAcoVarNameFinder(evf, "ST Geom Export")$EVVar
    
    prop = geom.copy[["Properties"]][["SingleTargetDetectionParameters"]]
    prop[["MinimumPulseLength"]] = 0.1
    prop[["MaximumPulseLength"]] = 3
    prop[["PLDL"]] = 3
    prop[["TsThreshold"]] = -100
    sbprop = geom.copy[["Properties"]][["SingleTargetDetectionSplitBeamParameters"]]
    sbprop[["MaximumBeamCompensation"]] = 12
    sbprop[["MaximumStdDevOfMajorAxisAngles"]] = 2
    sbprop[["MaximumStdDevOfMinorAxisAngles"]] = 2
    EVminThresholdSet(geom.copy, -100)
    
    
    #Fetch the grid of the school detection Sv
    school.var= EVAcoVarNameFinder(evf, "[T4 Sv] Fish-school samples [for aggregation detection]")$EVVar
    
    props = c("DepthRangeMode", "DepthRangeReferenceLine", "DepthRangeSeparation", "TimeDistanceMode", "TimeDistanceSeparation")
    prop.df = tibble(depth.grid.type = 0, ev.line.name = '', depth.grid.distance = 0, time.distance.mode = 0, time.distance.sep = 0)
    
    
    for(j in 1:5){
      cur.prop = school.var[["Properties"]][["Grid"]][[props[j]]]
      if(is.numeric(cur.prop)){
        prop.df[,j] = cur.prop
        next
      }
      prop.df[,j] = cur.prop[["Name"]]
    }
    
    EVChangeVariableGrid(evf,
                         geom.copy[["Name"]],
                         timeDistanceGridType = 5,
                         depthGridType = 2,
                         timeDistanceGridDistance = 5,
                         depthGridDistance = 10,
                         EVLineName = 'bottom_off')
    EVminThresholdSet(geom.copy, -100)
    
    EVsetExclusionLines(evf, 
                        geom.copy[["Name"]],
                        newAboveExclusionLine = 'surface',
                        newBelowExclusionLine = 'bottom_off')
    EVSetExportVariables(evf, export.variable.list)
    
    
    geom.copy = EVAcoVarNameFinder(evf, "ST Geom Export")$EVVar
    
    geom.copy$ExportSingleTargetsByCellsAll(output.file.name)
  })
  if(any(class(grand.test) == 'try-error')){
    geoms.exported[i] = 2
    EVCloseFile(evf)
    next
  }
  geoms.exported[i] = 1
  #EVSaveFile(evf)
  EVCloseFile(evf)
  print(i)
}
 