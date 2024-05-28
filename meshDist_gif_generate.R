# Function to make heatmap gifs like hanne's !! woop woop

require(Morpho)
require(magick)

# Provide the following:

# lm = procD.lm(shape ~ continous variable)
# max = value you want to use to model continuous variable
# frames = number of frames TOTAL
# path = directory for images and gif
# gif.name = final gif name

coloursofthewind <- read.table("/home/kristen/Documents/bhlab_misc/red_blue_hex_100.txt")$x
col_invert <- rev(coloursofthewind)

# lm <- lm.score$coefficients
# max <- 0.25
# frames <- 30
# path <- "Images"
# col.heatmap <- coloursofthewind
# atlas.lms <- atlas_lms
# atlas.mesh <- atlas_mesh

# rm(lm, max, frames, path, col.heatmap, atlas.mesh, atlas.lms)

morph.hm.gif <- function(lm, max = 1, frames = 30, path, col.heatmap, atlas.lms, atlas.mesh, control.morph = gpa$consensus, view = "top"){
  frames <- frames/2
  selected.scores <- c(seq(-max, max, length.out = frames), seq(max, -max, length.out = frames))
  file.names <- paste0("temp", 1:length(selected.scores), ".png")
  dists.meshes <- c("min.morph", "max.morph", "control.morph")
  dists <- NULL
  
  # Get dists for heatmap
  for(i in 1:3){
    datamod <- ~ c(-max,max,0)[i]
    mat <- model.matrix(datamod)
    pred <- mat %*% lm
    
    pred.lm <- arrayspecs(pred, p, k)
    
    assign(dists.meshes[i], tps3d(atlas.mesh, atlas.lms, pred.lm[,,1]))
  }
  
  for(i in 1:3){
    dists <- c(dists, max(abs(meshDist(control.morph, get(dists.meshes[i]), plot=F)$dists)))
  }
  dists <- max(dists)

  # Generate images...
  
  for(i in 1:length(selected.scores)){
    datamod <- ~ selected.scores[i] # this matches the input of the linear model.
    mat <- model.matrix(datamod)
    pred <- mat %*% lm
    
    pred.lm <- arrayspecs(pred, p, k)
    
    pred.mesh <- tps3d(atlas.mesh, atlas.lms, pred.lm[,,1])

    meshDist(pred.mesh, control.morph, rampcolors = col.heatmap, steps = 100, from = -dists, to = dists)
    if (view == "top"){view3d(180, 0, 0)}
    if (view == "side"){view3d(180, -90, 0)}
    par3d(windowRect = c(20, 30, 800, 800))
    rgl.snapshot(paste0(path, "/", file.names[i]))
    close3d()
  }

  img_list <- lapply(paste0(path, "/", file.names), image_read)
  gif <- image_animate(image_join(img_list), fps=10)
  return(gif)
  file.remove(paste0("Images/", file.names))
}
