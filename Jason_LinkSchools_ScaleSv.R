#Authors Jason and Marta
source("init_library.R")

#Load in metadata
met = read_csv(local.EVMetadata.loc)

#Specify file locations
data.location = paste0(local.export.root, "\\transect_exports")
school.dim.loc = paste0(local.export.root, "\\school_approximations_1m")


##### Set global station parameters #####
#<PARAMETER> Distance threshold, school border distance (how close to a school should a fish track be to be associated with it)
distance.threshold = 7

#Define new metadata column
#met$targets.consolidated = 0

##### Load data files #####

for(i in 1:nrow(met)){
  #Case handler: Skip non-transect files
  if(met$export.type[i] == 0){
    next
  }
  station.name = met$station_name[i]
  grand.test = try({
    #Load the WSST data
    WSST.data.file = str_subset(list.files(data.location, full.names = TRUE), paste0(station.name, '.csv$')) %>% str_subset(pattern = 'WSST')
    #Case detection: missing WSST data file will be designated target consolidation case 4
    if(is_empty(WSST.data.file)){
      met$targets.consolidated[i] = 4
      next
    }
    WSST.data = read_csv(WSST.data.file)
    
    #If the echogram includes schools, load the school data
    if(met$export.type[i] == 2){
      #Find the school data, NSST data and school dimension data
      school.data.file = str_subset(list.files(data.location, full.names = TRUE), paste0(station.name, '.csv$')) %>% str_subset(pattern = 'schools')
      NSST.data.file = str_subset(list.files(data.location, full.names = TRUE), paste0(station.name, '.csv$')) %>% str_subset(pattern = 'NSST')
      school.dim.file = str_subset(list.files(school.dim.loc, full.names = TRUE), paste0(station.name, '.csv$')) %>% str_subset(pattern = 'fineGridSchools')
      #Load the data
      school.data = read_csv(school.data.file) %>% mutate(Region_class = tolower(Region_class)) %>% filter(Region_class == 'schools')
      NSST.data = read_csv(NSST.data.file)
      #Exception: stations with schools but no NSST data are skipped, get designation case 3
      
      if(nrow(NSST.data) == 0){
        met$targets.consolidated[i] = 3
      } else{
        school.dim = read_csv(school.dim.file)
        #Error case: Filter cells where no lat/lon was reported (Lat/Lon == 999)
        school.dim = school.dim %>% filter(between(Lon_M, -360, 360))
        #Ensure the critical column Region_ID is properly named in the school.dim dataset, since sometimes extra characters are added to the export column names
        colnames(school.dim)[which(str_detect(colnames(school.dim), pattern = "Region_ID"))] = "Region_ID"
        #Associate fish with schools by finding targets that fall within the distance threshold from any school cell. 
        print.progress = FALSE
        NSST.data$assoc.schools = NA
        if(nrow(NSST.data) > 100){
          cat("Many NSST, this might take a while...")
          print.progress = TRUE
        }
        for(j in 1:nrow(NSST.data)){
          possible.cells = school.dim[which(distHaversine(cbind(NSST.data$Target_longitude[j], NSST.data$Target_latitude[j]), cbind(school.dim$Lon_M, school.dim$Lat_M)) < distance.threshold),]
          if(nrow(possible.cells) == 0){
            NSST.data$assoc.schools[j] == ' '
            next
          }
          hav.dists = distHaversine(cbind(NSST.data$Target_longitude[j], NSST.data$Target_latitude[j]), cbind(possible.cells$Lon_M, possible.cells$Lat_M))
          dpt.diff = abs(possible.cells$Depth_mean - NSST.data$Target_on_axis_depth[j])
          x2D.distance = sqrt(hav.dists^2 + dpt.diff^2)
          out.list = possible.cells$Region_ID[which(x2D.distance < distance.threshold)]
          out.list = unique(out.list)
          NSST.data$assoc.schools[j] = paste(out.list, collapse = ' , ')
          if(print.progress){
            if(j %% 100 == 0){
              text.out = paste0(100*round(j/nrow(NSST.data), 2), "% done")
              cat(text.out)
            }
          }
        }
        school.data$target.density = NA
        for(j in 1:nrow(school.data)){
          current.name = as.character(school.data$Region_ID[j])
          assoc.targets = NSST.data[str_which(NSST.data$assoc.schools, pattern = current.name),]
          ts.linear = 10^(assoc.targets$TS_comp/10)
          ts.mean = mean(ts.linear)
          sv.linear = 10^(school.data$Sv_mean[j]/10)
          school.data$target.density[j] = sv.linear/ts.mean
        }
        #Estimate target abundance in-school
        school.data$target.abundance = school.data$target.density*school.data$Beam_volume_sum
        school.data = school.data %>% mutate(Int_Lay = paste(Interval, Layer, sep = "_")) %>% select(Int_Lay, Sv_mean, target.abundance)
      }
    }
    #Merge WSST data with school target data
    #Combining school target abundance data with WSST target numbers by cell
    WSST.data = WSST.data %>% mutate(Int_Lay = paste(Interval, Layer, sep = "_"))
    if((met$export.type[i] == 2 & met$targets.consolidated[i] != 3)){
      comb.data = full_join(WSST.data, school.data, by = 'Int_Lay') %>%
        mutate(target.abundance = ifelse(is.na(target.abundance), 0, target.abundance)) %>%
        mutate(Num_targets = ifelse(is.na(Num_targets), 0, Num_targets)) %>%
        mutate(total.targets = round(target.abundance + Num_targets, 0))
    } else{
      comb.data = WSST.data
      comb.data$target.abundance = comb.data$Num_targets
    }
    #Write file
    output.path = form_path(station.name, data.location, 'targetAbundance', '.csv')
    write.csv(comb.data, output.path)
  })
  if(any(class(grand.test) == 'try-error')){
    met$targets.consolidated[i] = 2
    next
  }
  if(met$targets.consolidated[i] == 3){
    next
  }
  met$targets.consolidated[i] = 1
}

req = readline(prompt = 'Overwrite metadata? Y/N'){
  if(req %in% c("Y", 'y')){
    write_csv(met, local.EVMetadata.loc)
    metadata_backup(met)
  }
}


# #
# 

#######################################################################################

