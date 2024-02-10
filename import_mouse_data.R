# Functions

#### morpho.tools.GM::tag2array wrapped; suppresses errors - MEANS MUST CHECK RESULT ####
t2a <- function (dir = NULL, ID = NULL, string_del = NULL, propagated = FALSE, 
                 save.txt = FALSE){
  if (is.null(dir) == TRUE) {
    path <- getwd()
  }
  else {
    path <- dir
  }
  tag_list <- dir(path = path, pattern = "*.tag")
  n_land <- vector("numeric", length = length(tag_list))
  for (i in 1:length(tag_list)) {
    if (isTRUE(propagated) == TRUE) {
      n_land[i] <- length(count.fields(tag_list[[i]])) - 
        4
    }
    else {
      n_land[i] <- length(count.fields(tag_list[[i]])) - 
        3
    }
  }
  if (is.null(ID) == TRUE) {
    if (is.null(string_del) == TRUE) {
      dimnames_tag <- gsub(".tag", "", tag_list)
    }
    else {
      dimnames_tag <- gsub(string_del, "", gsub(".tag", 
                                                "", tag_list))
    }
  }
  else {
    dimnames_tag <- ID
  }
  if (length(unique(n_land)) != 1) {
    stop("Specimens have different number of landmarks.")
  }
  LM_array <- array(data = NA, dim = c(n_land[1], 3, length(tag_list)))
  dimnames(LM_array)[[3]] <- dimnames_tag
  LM_type <- vector(mode = "character", length(tag_list))
  if (isTRUE(propagated) == TRUE) {
    for (i in 1:length(tag_list)) {
      LM_array[, 1, i] <- suppressWarnings(read.table(file = tag_list[[i]], 
                                                      skip = 5, sep = " ", header = F, fill = T))[, 2]
      LM_array[, 2, i] <- suppressWarnings(read.table(file = tag_list[[i]], 
                                                      skip = 5, sep = " ", header = F, fill = T))[, 3]
      LM_array[, 3, i] <- suppressWarnings(read.table(file = tag_list[[i]], 
                                                      skip = 5, sep = " ", header = F, fill = T))[, 4]
    }
  }
  else {
    for (i in 1:length(tag_list)) {
      LM_array[, 1, i] <- suppressWarnings(read.table(file = tag_list[[i]], 
                                                      skip = 4, sep = " ", header = F, fill = T))[, 2]
      LM_array[, 2, i] <- suppressWarnings(read.table(file = tag_list[[i]], 
                                                      skip = 4, sep = " ", header = F, fill = T))[, 3]
      LM_array[, 3, i] <- suppressWarnings(read.table(file = tag_list[[i]], 
                                                      skip = 4, sep = " ", header = F, fill = T))[, 4]
    }
  }
  if (isTRUE(save.txt) == TRUE) {
    for (i in 1:dim(LM_array)[3]) {
      write.table(LM_array[, , i], paste0(dimnames(LM_array)[[3]][i], 
                                          ".txt"), col.names = FALSE, row.names = FALSE)
    }
  }
  return(LM_array)
}



#### CONVERT ARRAY TO NUMERIC VALUES ####

array3d.as.numeric <- function(array = NULL, remove.row = FALSE) {
  if(remove.row){
    array <- array[-1,,]}
  
  array[,,] <- array(as.numeric(array), dim = dim(array), dimnames = dimnames(array))
}


