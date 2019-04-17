#divide_string function: divide text into strings segments using intersections words as anchors
divide_string <- function(vec, intersect_vec){
  position <- c(0,which(vec %in% intersect_vec), length(vec))
  string_list = list()
  for(i in 1:(length(position)-1)){
    str = character(0)
    if(position[i+1] - position[i] > 1 ) {
      for(j in (position[i]+1) : (position[i+1]-1)){
        str = c(str, vec[j])
      } 
    }
    
    string_list[[i]] = str
  }
  return(string_list)
}


#count_error_char function: input two lists of strings, count total number of error characters
count_error_char <- function(list1, list2){
  if(length(list1) == length(list2)){
    no_of_error <- 0
    for(i in 1:length(list1)){
      no_of_error <- no_of_error + sum(diag(adist(list1[[i]], list2[[i]])))
    }
  }
  else{ no_of_error <- NA }
  
  return(no_of_error)
}

