symmetrize_wrapped <- function(lms){
  rem <- c(337:430,  94, 325) # These are duplicated or extra landmarks.
  
  # First, check how many landmarks
  if(dim(lms)[1] == 844){
    lms <- lms[-rem,,]
    rownames(lms) <- 1:748
  } else if(dim(lms)[1] != 748){
    stop("Check the number of landmarks")
  }
  
  # Symmetrize landmarks...
  sym <- readRDS("~/Documents/bhlab_misc/landmark_sym.Rds")
  require(Morpho)
  sym.lms <- Morpho::symmetrize(lms, as.matrix(sym$sym.pairings))
  rm(rem); return(sym.lms)
}