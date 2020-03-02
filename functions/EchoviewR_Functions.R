require(EchoviewR)
require(tidyverse)

if("beepr" %in% installed.packages()){
  beepr = T
  library(beepr)
} else{beepr = F}

cast.long = function(some.arrayed.ping, preserve.ping.geometry = T){
  output = c()
  for(i in 1:nrow(some.arrayed.ping)){
    output = c(output, some.arrayed.ping[i,])
  }
  if(preserve.ping.geometry){
    dims = dim(some.arrayed.ping)
    n.beams = dims[2]
    n.samp = dims[1]
    beam.ind =rep(c(1:n.beams), n.samp)
    samp.ind = rep(c(1:n.samp), each = n.beams)
    return(data.frame(beam = beam.ind, samp = samp.ind, value = output))
  } else{return(output)}
}

cast.long.neo = function(some.arrayed.ping, preserve.ping.geometry = T){
  output = c()
  microbenchmark(
  for(i in 1:nrow(some.arrayed.ping)){
    output = c(output, some.arrayed.ping[i,])
  }
  )
  if(preserve.ping.geometry){
    dims = dim(some.arrayed.ping)
    n.beams = dims[2]
    n.samp = dims[1]
    beam.ind =rep(c(1:n.beams), n.samp)
    samp.ind = rep(c(1:n.samp), each = n.beams)
    return(data.frame(beam = beam.ind, samp = samp.ind, value = output))
  } else{return(output)}
}

cast.wide = function(a.ping.df){
  max.samp = max(a.ping.df$samp)
  max.beam = max(a.ping.df$beam)
  out.mat = matrix(NA, nrow = max.beam, ncol = max.samp)
  a.ping.df$inds = max.beam * (a.ping.df$samp - 1) + a.ping.df$beam
  a.ping.df = a.ping.df[order(a.ping.df$inds),]
  out.mat[a.ping.df$inds] = a.ping.df$value
  out.mat = t(out.mat)
  return(out.mat)
}

#Ev-com: Opens the COM object 
ev.com = function(){
  return(COMCreate('EchoviewCom.EvApplication'))
}

#Creates a specific file dispatch, for call by other functions. Requires a previously opened COM object (coded as e by default)
ev.disp = function(ev.file.name, ev.com = e){
  return(EVOpenFile(ev.com, ev.file.name)$EVFile)
}


#Function: Pick a bottom line with an optional vertical offset. Bottom picking algorithm will use the Sv data with a median filter. User will be prompted to first change the dimensions of the convolution in the XxY statistic page- this is monkey work, and should be automated if possible. User will next manually edit the bottom line picked by the convolution.

pick.bottom = function(ev.file.dispatch, sv.name = 'Fileset 1: Sv pings T1', offset.depth = 0.25){
  ev = ev.file.dispatch
  EVNewAcousticVar(EVFile = ev, sv.name, enum = 145)
  EVNewAcousticVar(ev, 'Impulse noise removal 1', enum = 136)
  invisible(readline(prompt = 'Change the XxY statistic to 1 sample x 3 pings, then press [ENTER]'))
  EVBottomDetection(ev, 'XxY statistic 1', LineName = "bottom_vrt")
  if(beepr){
    beep()
  }
  invisible(readline(prompt = 'Edit the detected bottom line, then press [ENTER] to continue'))
  EVCreateEditableLine(ev, 'bottom_vrt', editableLineName = 'bottom_off', Add = offset.depth*-1, SpanGaps = TRUE)
}

#Short function to create a surface line
fixed.depth.line = function(ev.file.dispatch, depth = 5, name = 'surface'){
  EVNewFixedDepthLine(ev.file.dispatch, depth = depth ,lineName = name)
}


#Function to create processed Sv and TS variables. Creates a copy of the Sv data, and requests the user manually remove areas of noise. Then, once the noise is removed, creates a processed Sv variable and applies the pre-picked exclusion lines. Duplicates this process for the TS data.

processed.data = function(ev.file.dispatch, sv.name = 'Fileset 1: Sv pings T1', ts.name = 'Fileset 1: TS pings T1', bottom.line.name = 'bottom_off', surface.line.name = 'surface'){
  ev = ev.file.dispatch
  #Create copies of Sv, TS. Rename them.
  EVNewAcousticVar(ev, sv.name, 16)
  EVNewAcousticVar(ev, ts.name, 16)
  EVRenameAcousticVar(ev, 'Copy 1', 'Sv Copy')
  EVRenameAcousticVar(ev, 'Copy 1 (1)', 'TS Copy')
  #Apply line exclusions
  EVsetExclusionLines(ev, "Sv Copy", newAboveExclusionLine = surface.line.name, newBelowExclusionLine =  bottom.line.name)
  EVsetExclusionLines(ev, "TS Copy", newAboveExclusionLine = surface.line.name, newBelowExclusionLine = bottom.line.name)
  #Create processed data variables. Rename them.
  EVNewAcousticVar(ev, 'Sv Copy', 63)
  EVNewAcousticVar(ev, 'TS Copy', 63)
  EVRenameAcousticVar(ev, 'Processed Data 1', 'Processed Sv')
  EVRenameAcousticVar(ev, 'Processed Data 1 (1)', 'Processed TS')
  }
  

peaks = function(x){
  peaksy = rep(0, length(x))
  for(i in 3:length(x)){
    if(is.na(x[i])){
      next
    }
    if(is.na(x[i-1])){
      next
    }
    if(is.na(x[i-2])){
      next
    }
    if(x[i] < x[i-1]){
      if(x[i-1] > x[i-2]){
        peaksy[i-1] = 1
      }
    }
  }
  return(peaksy)
}

troughs = function(x){
  troughy = rep(0, length(x))
  for(i in 3:length(x)){
    if(is.na(x[i])){
      next
    }
    if(is.na(x[i-1])){
      next
    }
    if(is.na(x[i-2])){
      next
    }
    if(x[i] > x[i-1]){
      if(x[i-1] < x[i-2]){
        troughy[i-1] = 1
      }
    }
  }
  return(troughy)
}



#School detection function:
# detect.schools = function(ev.file.dispatch, acoVarName, outputRegionClassName, deleteExistingRegions, distanceMode, maximumHorizontalLink, maximumVerticalLink, minimumCandidateHeight, minimumCandidateLength, minimumSchoolHeight, minimumSchoolLength = , dataThreshold = NULL)
#First, create a school region class

#Next, create a copy of the processed SV data
#Request that the user manually set the Sv threshold
#Finally, run the detection algorithm

insert = function(a.vector, a.value, a.position){
  return(c(a.vector[1:(a.position-1)], a.value, a.vector[(a.position):length(a.vector)]))
}

calib_to_ecs = function(calib_file_location, calib_file_name, blank_ecs_file_path,output_file_path){
  require(tidyverse)
  setwd(calib_file_location)
  simrad.text = readLines(calib_file_name)
  
  simrad.text.trunc = simrad.text[which(grepl("<CalibrationResults>", simrad.text)):which(grepl("</CalibrationResults>", simrad.text))]
  simrad.values = rep(NA, length(simrad.text.trunc))
  for(i in 1:length(simrad.text.trunc)){
    val = simrad.text.trunc[i] %>% strsplit(split = '>') %>% unlist()
    if(length(val) == 1){
      simrad.values[i] = val
    } else{
      simrad.values[i] = val[2] %>% strsplit(split = '</') %>% unlist() %>% .[1]
    }
  }
  simrad.values = as.numeric(simrad.values)
  
  simrad.names = c('Frequency', 'Gain', 'SaCorrection', 'BeamWidthAlongship', 'BeamWidthAthwartship', 'AngleOffsetAlongship','AngleOffsetAthwartship')
  
  ev.names = c('Frequency','TransducerGain','EK60SaCorrection', 'MinorAxis3dbBeamAngle', 'MajorAxis3dbBeamAngle', 'MinorAxisAngleOffset', 'MajorAxisAngleOffset')
  
  match.indices = rep(NA, length(simrad.names))
  for(i in 1:length(simrad.names)){
    match.indices[i] = which(grepl(simrad.names[i], simrad.text.trunc))
  }
  
  write.values = simrad.values[match.indices]
  write.values[which(simrad.names == 'Frequency')] = write.values[which(simrad.names == 'Frequency')]/1000
  
  insert.lines = paste(ev.names, write.values, sep = ' = ')
  
  
  blank.ecs = readLines(blank_ecs_file_path)
  blank.ecs[1] = paste0("#",blank.ecs[1])
  start.line = which(grepl('FILESET SETTINGS', blank.ecs)) + 2
  
  new.ecs = insert(blank.ecs, insert.lines, start.line)
  
  #Write the file to a .ecs file
  output.file.date = calib_file_name %>% strsplit("-") %>% unlist() %>% .[2] %>% strsplit('D') %>% unlist() %>% .[2]
  output_file_name = paste0('calib_file_', output.file.date, '.ecs')
  
  setwd(output_file_path)
  if(output_file_name %in% list.files()){
    output_file_name = paste0('calib_file_', output.file.date, '_1.ecs')
  }
  
  write_lines(new.ecs, output_file_name)
}

#List acoustic variables in an EV file
EVListAcoVars = function(ev.file){
  cnt = ev.file$Variables()$Count()
  out.vec = rep(NA, cnt)
  for(j in 0:(cnt-1)){
    out.vec[j+1] = ev.file$Variables()$Item(j)$Name()
  }
  message('Reminder: If you intend to use these names to find a specific variable index, remember that COM indices start at 0')
  out.vec
}

ping.plot = function(ping.df){
  require(ggplot2)
  pplot = ggplot(data = ping.df, aes(x = ping.df$beam, y = ping.df$samp, fill = ping.df$value)) + geom_tile() + scale_y_reverse()
  return(pplot)
}

str_snatch = function(a.string, leading.chunk, following.chunk){
  require(stringr)
  probe.lead = str_locate(a.string, leading.chunk)
  probe.follow = str_locate(a.string, following.chunk)
  return(str_sub(a.string, start = probe.lead[,2] + 1, end = probe.follow[,1] -1))
}

form_path = function(an.ev.identifier, a.repository.directory, a.prefix, an.extension){
  if(a.prefix == ''){
    return(paste0(a.repository.directory, "\\", an.ev.identifier, an.extension))
  } else{
    return(paste0(a.repository.directory, "\\", a.prefix, '_', an.ev.identifier, an.extension))
  }
}

get.ev.file.repo = function(){
  return("G:\\My Drive\\School\\Thesis\\Data_Analysis\\Acoustic_Analysis\\EV_File_Repository")
}

get.ev.file.metadata = function(){
  return(read.csv("G:\\My Drive\\School\\Thesis\\Data_Analysis\\Acoustic_Analysis\\EV_file_metadata\\ev_file_metadata.csv", header = TRUE, stringsAsFactors = FALSE))
}

is.try.error = function(an.object){
  if(class(an.object) == 'try-error'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#Utility function
which.between = function(x, left.vec, right.vec){
  #Given a value x and 2 equally sized vectors of left and right boundaries, return any indices where x falls between the left and right boundary
  return(which((left.vec <= x)&(right.vec >= x)))
}

#Function to check if an EV COM object is still open
ev.persists = function(ev.object){
  t = tryCatch({ev.object[["Version"]]}, error = function(e){
    return(FALSE)
  })
  if(t == FALSE){
    return(FALSE)
  }
  return(TRUE)
}

#Function which restarts an EV COM object if it is closed. If it finds the active COM object, it returns the object as is. If not, it restarts the COM object and also initializes it in the R environment with the same name. Thus this can be used to start an EV COM as well as to retain an existing COM object.
ev.restart = function(ev.object){
  test = ev.persists(ev.object)
  if(test == FALSE){
    return(eval({ev.object = ev.com()}))
  }
  return(ev.object)
}

EVListAllRegionNames = function(EV.file){
  regions = EV.File[["Regions"]]
  cnt = regions[["Count"]]
  output = tibble(reg_name = NA, reg_id = NA, class = NA, .rows = cnt)
  for(ind in 0:(cnt-1)){
    output$reg_name[ind+1] = regions$Item(ind)[["Name"]]
    output$reg_id[ind+1] = regions$Item(ind)[["Id"]]
    output$class[ind+1] = regions$Item(ind)[["RegionClass"]][["Name"]]
  }
  return(output)
}

EVRegionCountbyClass = function(EV.file, region.class.name){
  region.class.name = tolower(region.class.name)
  intermed = EVListAllRegionNames(EV.file)
  intermed$class = tolower(intermed$class)
  outp = sum(intermed$class == region.class.name)
  return(outp)
  }

EVSetExportVariables = function(EV.file, export.variable.list){
  exp.vars = EV.File[["Properties"]][["Export"]][["Variables"]]
  for(item in 0:(length(export.variable.list)-1)){
    cur.item = export.variable.list[item+1]
    cur.item.pointer = exp.vars$Item(cur.item)
    cur.item.pointer[["Enabled"]] = TRUE
  }
}
