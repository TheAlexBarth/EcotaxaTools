#' taxo_warning - a function to give a warning before editing the category file
taxo_warning <- function(){
  qual <- F
  while(!qual){
    cat("","Caution: you will update the renamer memmory",
        "FYI: You can alway access the file in the data folder of package",
        "Do you wish to proceed?","",sep = "\n")
    proc <- as.logical(readline("TRUE or FALSE: "))
    if(!is.na(proc)){
      qual <- T
    } else {
      cat("","Non-logical entry given","Try again -  use 'FALSE' to exit","",
          sep = "\n")
    }
  }
  return(proc)
}

#' select_instrument - a little function to get user input for which category
select_instrument <- function(){
  qualify <- F
  while(qualify == F){
    cat("Select which instrument (enter number):","",sep = "\n")
    cat(paste(c(1:length(names(taxo_list))),
              rep("-",length(names(taxo_list))),
              names(taxo_list)),"", sep = "\n")
    instru <- readline(prompt = "Choice: ")
    if(instru == "EXIT"){
      stop("Exit command given")
    }
    if(instru %in% c(1:length(names(taxo_list)))){
      qualify <- T
    } else {
      cat("","Incompatible selection","Try Again or type 'EXIT'","",sep = "\n")
    }
  }
  return(names(taxo_list)[as.numeric(instru)])
}

#' new_instrument - adding a new instrument to the taxa categorizer memmory
#'
new_instrument <- function(){
  #check for exit
  warn <- taxo_warning()
  if(!warn){ stop("Exit command given")}
  qual <- F
  while(!qual){
    cat("What is the new instrument?","")
    new_instr <- readline(">>> ")
    if(new_instr == "EXIT"){
      stop("Exit command given")
    }
    if(!(new_instr %in% names(taxo_list))){
      qual <- T
    } else {
      cat("","Cannot use exiting instrument name",
          "Try Again or type 'EXIT'","",sep = "\n")
    }
  }
  old_names <- names(taxo_list)
  taxo_list <<- c(taxo_list,NA)
  names(taxo_list) <<- c(old_names,new_instr)
}


#' taxo_rename - A function to remember and rename taxonomic groups
#' 
#' This function stores a list of different types of categories to rename
#' ecotaxa output. The column header must be object_annotation_category.
#' it prompts user input from the command line to select category.
#' If all categories are known, it will return a text vector with the new names
#' 
#' @param instrument Zooscan UVP or PlanktoScope
#' 