#What do I want? For each station, there should be a spreadsheet of cells with a target abundance within that cell. Irrespective of depth interval or anything else.

source("init_library.R")

transect.data.loc = paste0(local.export.root, '\\transect_exports')
cell.geom.loc =paste0(local.export.root, "\\cell_geometries")

met = read_csv(local.EVMetadata.loc)

for(i in 1:nrow(met)){
  #Case handling: Skip unprocessed sites, skip class 3 sites for now
  if(met$processed[i] == 0){
    next
  }
  if(met$processed[i] == 3){
    next
  }
  station.name = met$station_name[i]
  cur.ev = met$evs[i]
  transect.data.file = str_subset(list.files(transect.data.loc, full.names = TRUE), paste0(station.name, '.csv$')) %>% str_subset(pattern = 'processed_Abundance')
  cell.geom.file = str_subset(list.files(cell.geom.loc, full.names = TRUE), paste0(station.name, '.csv$')) %>% str_subset(pattern = 'cell_geoms')
  #Load data
  transect.data = read_csv(transect.data.file)
  geom.data = read_csv(cell.geom.file)
  
  #Geom.Data needs an Int_Lay key
  geom.data = geom.data %>% mutate(Int_Lay = paste(Interval, Layer, sep = "_"))
  
  #Fix a problem with some geom.data exports, where the bottom of the cell is anomalously deep
  geom.data = geom.data %>% mutate(Layer_depth_max = ifelse(Layer_depth_max > (2*Layer_depth_min+Height_mean), Layer_depth_min+Height_mean, Layer_depth_max))
  
  #Case handling: If no targets were detected in a transect, transect data will have only 2 columns. In this case, transect data will be set to the geom bottom data with VDens == 0
  if((ncol(transect.data) == 2)&(transect.data$target.abundance[1] == 0)){
    transect.data = geom.data %>% mutate(VDens = 0)
  }
  
  #Link the cell abundance data to the cell geometry, clear up missing Lon/Lat values
  transect.data = transect.data %>% filter(Int_Lay %in% geom.data$Int_Lay) %>% select(Int_Lay, VDens)
  
  geom.data = left_join(geom.data, transect.data, by = "Int_Lay") %>% mutate(VDens = if_else(is.na(VDens), 0, VDens))
  
  output.file.name = form_path(station.name, paste0(local.export.root, "\\transect_mosaics"), 'mosaic','.csv')
  write_csv(geom.data, output.file.name)
}
