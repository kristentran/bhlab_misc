# Use source() to load mouse atlas

require(geomorph)
require(Morpho)
coloursofthewind <- read.table("/home/kristen/Documents/bhlab_misc/red_blue_hex_100.txt")$x

dir <- "/home/kristen/Documents/bhlab_misc/Atlas"
atlas_mesh <- read.ply(paste0(dir, "/Atlas_mesh.ply"))

temp_lms <- suppressWarnings(read.table(paste0(dir, '/Atlas_landmarks.tag'), skip=5))
temp_lms <- as.matrix(na.omit(temp_lms[,1:3]))

atlas_lms <- matrix(as.numeric(temp_lms), nrow(temp_lms), ncol(temp_lms))
rm(temp_lms, dir)

# Remove some landmarks
rem <- c(337:430,  94, 325) # These are dupes or extra landmarks.
atlas_lms <- atlas_lms[-rem,]
rownames(atlas_lms) <- 1:748

# symmetrize atlas_lms
sym <- readRDS("/home/kristen/Documents/bhlab_misc/landmark_sym.Rds")
require(Morpho)
atlas_lms <- Morpho::symmetrize(atlas_lms, as.matrix(sym$sym.pairings))

rm(sym, rem)

# open3d()
# shade3d(atlas_mesh, col="white", alpha=0.5)
# points3d(atlas_lms[sym$sym.pairings$side.1,], size = 10, col="red", add =T)
# points3d(atlas_lms[sym$sym.pairings$side.2,], size = 10, col="blue", add =T)
# text3d(atlas_lms[c(sym$sym.pairings$side.1, sym$sym.pairings$side.2),], texts = c(sym$sym.pairings$side.1, sym$sym.pairings$side.2), offset=1, cex = 2)

meshDist_wrapped <- function(x,y, col = "normal", view = "top"){
  library(Morpho)

if(class(x)[1] != "mesh3d"){x <- tps3d(atlas_mesh, atlas_lms, x)}
if(class(y)[1] != "mesh3d"){y <- tps3d(atlas_mesh, atlas_lms, y)}

  Heatmap <- meshDist(x,y)
  if (col == "reverse") {coloursofthewind <- rev(coloursofthewind)}
  
  meshDist(x, y, steps = 100, rampcolors = coloursofthewind, from = -max(abs(Heatmap$dists)), to = max(abs(Heatmap$dists)))
  
  if (view == "top"){view3d(180, 0, 0)}
  if (view == "side"){view3d(180, -90, 0)}
}

close3d()


