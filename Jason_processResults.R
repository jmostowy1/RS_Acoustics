source("init_library.R")

transect.data.loc = paste0(local.export.root, '\\transect_exports')
cell.geom.loc =paste0(local.export.root, "\\cell_geometries")

met = read_csv(local.EVMetadata.loc)

sum.df = tibble(ev = NA, station.name = NA, Lon_M = NA, Lat_M= NA, demersal.abundance = NA, demersal.density = NA, .rows = 0)
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
  #Fix a problem with some geom.data exports, where the bottom of the cell is anomalously deep
  geom.data = geom.data %>% mutate(Layer_depth_max = ifelse(Layer_depth_max > (2*Layer_depth_min+Height_mean), Layer_depth_min+Height_mean, Layer_depth_max))
  #Select only the bottom layer (Layers within 15m of the max depth in that interval interval)
  int.max.depths = geom.data %>% group_by(Interval) %>% summarise(max.depth = max(Layer_depth_max))
  keep = rep(0, nrow(geom.data))
  for(j in 1:nrow(geom.data)){
    cur.interval = geom.data$Interval[j]
    #I will keep the interval in the bottom layer so long as the minimum depth of the layer is within 20 m of the bottom
    if(geom.data$Layer_depth_min[j] > (int.max.depths$max.depth[which(int.max.depths$Interval == cur.interval)]-20)){
      keep[j] = 1
    }
  }
  geom.data.bottom = geom.data[keep == 1,] %>% mutate(Int_Lay = paste0(Interval,"_", Layer))
  #Case handling: If no targets were detected in a transect, transect data will have only 2 columns. In this case, transect data will be set to the geom bottom data with VDens == 0
  if((ncol(transect.data) == 2)&(transect.data$target.abundance[1] == 0)){
    transect.data = geom.data.bottom %>% mutate(VDens = 0)
  }
    
  #Link the cell abundance data to the cell geometry, clear up missing Lon/Lat values
  transect.data = transect.data %>% filter(Int_Lay %in% geom.data.bottom$Int_Lay) %>% select(Int_Lay, VDens)
  ll.ref = geom.data.bottom %>% filter(!(Lon_M == 999)) %>% summarize(mean.Lon = mean(Lon_M), mean.Lat = mean(Lat_M))
  geom.data.bottom = full_join(geom.data.bottom, transect.data, by = "Int_Lay") %>% 
    mutate(VDens = ifelse(is.na(VDens), 0, VDens)) %>% 
    mutate(Abun = VDens*Wedge_volume_sampled) %>% 
    mutate(Lon_M = ifelse(Lon_M == 999, ll.ref$mean.Lon, Lon_M), Lat_M = ifelse(Lat_M == 999, ll.ref$mean.Lat, Lat_M))
  #Form output, append to summary data frame
  out.tib = tibble(ev = cur.ev,
                   station.name = station.name, 
                   Lon_M = mean(geom.data.bottom$Lon_M),
                   Lat_M = mean(geom.data.bottom$Lat_M),
                   demersal.abundance = sum(geom.data.bottom$Abun),
                   demersal.density = sum(geom.data.bottom$Abun)/sum(geom.data$Wedge_volume_sampled))
  sum.df = bind_rows(sum.df, out.tib)
}

sum.df %>% ggplot() + geom_point(aes(x = Lon_M, y = Lat_M, size = demersal.density*1000))
hi = sum.df %>% top_n(20, demersal.density) %>% arrange(desc(demersal.density))
