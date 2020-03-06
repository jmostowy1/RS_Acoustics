source("init_library.R")


ev.school.report = read_csv("./reports/ev_school_report.csv") %>% mutate(edited = NA)
ev.repo = read_csv(local.EVMetadata.loc) %>% filter(has_gps == TRUE, category %in% c("ES_Transect", "Non_Transect_Usable")) %>% select(evs, station_name)

for(i in 1:nrow(ev.school.report)){
  ev = ev.restart(ev)
  if(ev.school.report$n.schools[i] == 0){
    ev.school.report$edited[i] = TRUE
    next
  }
  evf = ev$OpenFile(ev.school.report$ev.names[i])
  req1 = request("Edit the schools in the EV file", "DONE")
  if(req1 == TRUE){
    EVSaveFile(evf)
    EVCloseFile(evf)
    ev.school.report$edited[i] = TRUE
  }
}

#School recount +school record update

ev.school.report$new.school.count = NA

for(i in 1:nrow(ev.school.report)){
  ev = ev.restart(ev)
  evf = ev$OpenFile(ev.school.report$ev.names[i])
  ev.school.report$new.school.count[i] = EVRegionCountbyClass(evf, 'schools')
  EVCloseFile(evf)
}


#Merge school data with metadata
met = read_csv(local.EVMetadata.loc) %>% select(-X1)
colnames(ev.school.report)[1] = 'evs'

met.j = left_join(met, ev.school.report, by = "evs")

#Rewrite metadata
#write_csv(met.j, local.EVMetadata.loc)

#Separated fish tracks!