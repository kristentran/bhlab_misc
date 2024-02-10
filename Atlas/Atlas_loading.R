# Use source() to load mouse atlas

require(geomorph)
require(Morpho)
coloursofthewind <- read.table("C:/Users/k-ann/Documents/R_scripts_misc/red_blue_hex_100.txt")$x

dir <- "C:/Users/k-ann/Documents/R_scripts_misc/Atlas"
atlas_mesh <- read.ply(paste0(dir, "/Atlas_mesh.ply"))

temp_lms <- suppressWarnings(read.table(paste0(dir, '/Atlas_landmarks.tag'), skip=5))
temp_lms <- as.matrix(na.omit(temp_lms[,1:3]))

atlas_lms <- matrix(as.numeric(temp_lms), nrow(temp_lms), ncol(temp_lms))
rm(temp_lms, dir)

# shade3d(atlas_mesh, col = "red")
# plot3d(atlas_lms, add=T)

meshDist_wrapped <- function(x,y){
  library(Morpho)

if(class(x)[1] != "mesh3d"){x <- tps3d(atlas_mesh, atlas_lms, x)}
if(class(y)[1] != "mesh3d"){y <- tps3d(atlas_mesh, atlas_lms, y)}

  Heatmap <- meshDist(x,y)
  meshDist(x, y, steps = 100, rampcolors = coloursofthewind, from = -max(abs(Heatmap$dists)), to = max(abs(Heatmap$dists)))
  view3d(180, 0, 0)
}
