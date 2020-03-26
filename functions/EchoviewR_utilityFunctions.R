require(EchoviewR)
require(tidyverse)

#Utility: Insert a value at a position in a vector
insert = function(a.vector, a.value, a.position){
  return(c(a.vector[1:(a.position-1)], a.value, a.vector[(a.position):length(a.vector)]))
}

#List the names acoustic variables in an EV file
EVListAcoVars = function(ev.file){
  cnt = ev.file$Variables()$Count()
  out.vec = rep(NA, cnt)
  for(j in 0:(cnt-1)){
    out.vec[j+1] = ev.file$Variables()$Item(j)$Name()
  }
  message('Reminder: If you intend to use these names to find a specific variable index, remember that COM indices start at 0')
  out.vec
}

#General utility: Returns a substring between two specified patterns. Good for extracting station names from uniform file types
str_snatch = function(a.string, leading.chunk, following.chunk){
  require(stringr)
  probe.lead = str_locate(a.string, leading.chunk)
  probe.follow = str_locate(a.string, following.chunk)
  return(str_sub(a.string, start = probe.lead[,2] + 1, end = probe.follow[,1] -1))
}

#General utility: Forms a file path based on a unique station (EV) identifier, a destination directory, a file naming prefix and an extension type
form_path = function(an.ev.identifier, a.repository.directory, a.prefix, an.extension){
  if(a.prefix == ''){
    return(paste0(a.repository.directory, "\\", an.ev.identifier, an.extension))
  } else{
    return(paste0(a.repository.directory, "\\", a.prefix, '_', an.ev.identifier, an.extension))
  }
}

#General utility: Test for a try-error
is.try.error = function(an.object){
  if(class(an.object) == 'try-error'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#Utility function: Given a value x and 2 equally sized vectors of left and right boundaries, return any indices in the boundaries where x falls between the left and right boundary 
which.between = function(x, left.vec, right.vec){
  return(which((left.vec <= x)&(right.vec >= x)))
}

#EV function: Function to check if an EV COM object is still open
ev.persists = function(ev.object){
  t = tryCatch({ev.object[["Version"]]}, error = function(e){
    return(FALSE)
  })
  if(t == FALSE){
    return(FALSE)
  }
  return(TRUE)
}


#EV function: Ev-com: Opens a COM object 
ev.com = function(){
  return(COMCreate('EchoviewCom.EvApplication'))
}

#Creates a specific file dispatch, for call by other functions. Requires a previously opened COM object (coded as e by default)
ev.disp = function(ev.file.name, ev.com = e){
  return(EVOpenFile(ev.com, ev.file.name)$EVFile)
}

#EV utility: Function which restarts an EV COM object if it is closed. If it finds the active COM object, it returns the object as is. If not, it restarts the COM object and also initializes it in the R environment with the same name. Thus this can be used to start an EV COM as well as to retain an existing COM object.
ev.restart = function(ev.object){
  test = ev.persists(ev.object)
  if(test == FALSE){
    return(eval({ev.object = ev.com()}))
  }
  return(ev.object)
}

#EV utility: List all region names, ID and classes for a given EV file
EVListAllRegionNames = function(EV.File){
  regions = EV.File[["Regions"]]
  cnt = regions[["Count"]]
  if(cnt == 0){
    return(NULL)
  }
  output = tibble(reg_name = NA, reg_id = NA, class = NA, .rows = cnt)
  for(ind in 0:(cnt-1)){
    output$reg_name[ind+1] = regions$Item(ind)[["Name"]]
    output$reg_id[ind+1] = regions$Item(ind)[["Id"]]
    output$class[ind+1] = regions$Item(ind)[["RegionClass"]][["Name"]]
  }
  return(output)
}

#EV Utility: Returns the number of instances of a region class from an EV file
EVRegionCountbyClass = function(EV.file, region.class.name){
  region.class.name = tolower(region.class.name)
  intermed = EVListAllRegionNames(EV.file)
  if(is.null(intermed)){
    return(0)
  }
  intermed$class = tolower(intermed$class)
  outp = sum(intermed$class == region.class.name)
  return(outp)
  }

#EV Utility: Set all export variables for a file. Takes an EV file and a vector of export variables to be set to ENABLED
EVSetExportVariables = function(EV.file, export.variable.list){
  exp.vars = EV.file[["Properties"]][["Export"]][["Variables"]]
  for(item in 0:(length(export.variable.list)-1)){
    cur.item = export.variable.list[item+1]
    cur.item.pointer = exp.vars$Item(cur.item)
    cur.item.pointer[["Enabled"]] = TRUE
  }
}

#Request is essentially a request to enter a specific keyword to continue. It repeats its request until the keyword (case-insensitive) is input, at which time it returns TRUE
request = function(prompt, keyword){
  full.prompt = paste0(prompt, ";\n", "Type ", keyword, " to continue." )
  req = readline(prompt = full.prompt)
  
  req.lower = tolower(req)
  keyword.lower = tolower(keyword)
  
  if(req.lower == keyword.lower){
    return(TRUE)
  } else{
    request(prompt, keyword)
  }
}

#Backup metadata: Requests that user enters a keyword, then backs up the metadata. Otherwise it will not proceed
metadata_backup = function(metadata.object, local.backup.location = local.EVMetadata.backup.loc, keyword = 'Proceed'){
  req1 = request("This function will overwrite the metadata backup.\nPress [Esc] to cancel", keyword = keyword)
  if(req1 == TRUE){
    write_csv(metadata.object, local.backup.location)
  }
}
