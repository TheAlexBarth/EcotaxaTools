#' find_multiple - locate all multiple cateogories
#' 
#' Helper function to identify all types of multiple classes in 
#' object_annotation_category
#' 
#'
#' 
#' @param df a data frame with object_annotation_category
#' 
#' @noRd
find_multiple <- function(df){
  taxa_names <- unique(df$object_annotation_category)
  if(length(taxa_names) == 0){
    stop("No taxa names provided")
  } else {
    mults <- grep("^multiple<",taxa_names)
  }
  if(length(mults) == 0){
    stop("No multiples found")
  } else{
    return(taxa_names[mults])
  }
}

#' get_selection - helper function for user input
#' 
#' This calls for user input then asks which multiples to include
#' then it provides the index for all multiples of interest
#' 
#' @importFrom utils menu
#'  
#' @param df The input dataframe
#' 
#' @noRd
get_selection <-  function(df){
  idMult <- find_multiple(df)
  user_choice <- menu(c(idMult,"All"), title = cat("Found these multipe labels.","Select one?",
                             sep = "\n"))
  if(user_choice > length(idMult)){
    idMult = idMult
  } else {
    idMult = idMult[user_choice]
    
  }
  index = which(df$object_annotation_category %in% idMult)
  return(index)
}

#' select_msg - helper function to select a message
#' 
#' for inside multimanager, asks the iteration number and gives a response
#' 
#' @noRd
select_msg <- function(iter){
 msg <-  switch(as.character(iter),
         "1" = c("What Organism is in this vignette","Only Provide one value",
                 ">>>"),
         c("Are there others?","If no, click enter/return",">>>"))
 return(msg)
}

#' chk_input - helper to check input against known values
chk_input <- function(input,known){
  if(input %in% known){
    return(TRUE)
  } else {
    resp <- menu(c("Yes","No, re-enter"), title = cat(paste("'",input,"'"," in not existing name",
                                           sep = ""),
                                     "Continue?",sep ="\n"))
    return(c(T,F)[resp])
  }
}

#' multi_manager - a tool to re-label vignettes with multiple individuals
#' 
#' This function is a tool which facilitates quick renaming of vignettes 
#' with multiple organisms
#' It will select all multiple-object vignettes then request user input for the 
#' names of the new file
#' For each ID entered, it will create a new line with that estimate
#' Note that there will 
#' 
#' @importFrom ecotaxar read_ecotaxa_tsv
#' @importFrom svDialogs dlg_open
#' 
#' @param path location of file to choose, if not entered, a box will open to select file
#' @param morpho_include if true, will create a column of boolean values. True indicates that row will be included in morpho-measurements. New rows from multimanager default to F.
#' 
#' @export
multi_manager = function(path = NULL, morpho_include = T){
  
  ##
  # Set up:
  ##
  
  #if no path is provided:
  if(is.null(path)){
    path <- dlg_open()$res
  }
  ogDf <- as.data.frame(read_ecotaxa_tsv(path,trim = F)) #open originalDf
  outDf <- ogDf
  
  known <- unique(ogDf$object_annotation_category) #get the known names to cross ref
  if(is.null(known)){stop("no 'object_annotation_category' column")} #format check
  
  if(morpho_include == T){
    outDf$morpho_include = rep(T, nrow(outDf))
  }
  
  # Get index of multiples:
  index <- get_selection(ogDf)
  
  ##
  # Main Driver
  ##
  
  new_id <- NULL # set up vector of unknown length
  new_name <- NULL # set up vector for new names
  for(i in 1:length(index)){
    done <- FALSE #set up for while loop
    vig <- ogDf$object_id[index[i]] # Get object_id
    catg <- ogDf$object_annotation_category[index[i]]
    
    vig_names <- NULL #set up smaller holder
    iter <- 0
    while(!done){
      iter <- iter + 1
      cat("Vignette: ",vig,"\n",
          "Listed as: ",catg,"\n",sep = "")
      tmp_answer <- readline(cat(select_msg(iter),sep ="\n"))
      if(tmp_answer == "" & iter > 1){ #check for exit
        done <- T
        break
      } else {
        inCheck <- chk_input(tmp_answer,known)
        if(inCheck){
          known <- c(known,tmp_answer)
          number <- as.numeric(readline(cat("How Many?",">>> ",sep = "\n")))
          new_catg <- rep(tmp_answer,number)
        } else {
          iter <- iter - 1 #undo this lap
          next #go back and start over
        }
      }
      vig_names <- c(vig_names,new_catg)
    }
    
  }
  

}


