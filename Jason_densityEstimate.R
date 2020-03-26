source("init_library.R")

data.location = paste0(local.export.root, "\\transect_exports_fine")
met = read_csv(local.EVMetadata.loc)

met$processed = 0

for(i in 1:nrow(met)){
  #Case handling: skip if targets.consolidated is NA
  if(met$targets.consolidated[i] == 0){
    next
  }
  #Specify station name and output file name
  station.name = met$station_name[i]
  output.file.name = form_path(station.name, data.location,'processed_Abundance_fine', '.csv')
  #Case 3: Schools, but no NSST to scale- process WSST only. Skip for now
  if(met$targets.consolidated[i] == 3){
    met$processed[i] = 3
    next
  }
  #Case 4: Empty echogram- 0
  if(met$targets.consolidated[i] == 4){
    target.data = tibble(station_name = station.name, target.abundance = 0)
    write_csv(target.data, output.file.name)
    met$processed[i] = 4
    next
  }
  
  station.data.file = str_subset(list.files(data.location, full.names = TRUE), paste0(station.name, '.csv$')) %>% str_subset(pattern = 'targetAbundance')


  #Read in cell target data
  target.data = read_csv(station.data.file)
  if(nrow(target.data) == 0){
    target.data = tibble(station_name = station.name, target.abundance = 0)
    write_csv(target.data, output.file.name)
    met$processed[i] = 4
    next
  }
  # Estimate volumetric and aereal density per cell (cell metrics)
  target.data = target.data %>% mutate(VDens = target.abundance/Beam_volume_sum, ADens = VDens*Height_mean)
  write_csv(target.data, output.file.name)
  met$processed[i] = 1
  }

# write_csv(met, local.EVMetadata.loc)
# metadata_backup(met)
