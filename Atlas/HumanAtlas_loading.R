# Load all visualization tools for human dense 5k

atlas <- read.ply("/mnt/Storage4/FaceBase_3/Data/Images/Atlas/Dense_5k/dense_5k_atlas.ply", ShowSpecimen = F)
# coloursofthewind <- read.table("~/Documents/bhlab_misc/red_blue_hex_100.txt")$x
# 
# coloursofthewind <- as.character(read.table("/home/kristen/Documents/bhlab_misc/voicesofthemountains.txt"))
# coloursofthewind <- coloursofthewind[!stringr::str_detect(coloursofthewind, ",")]

coloursofthewind <- heatmap <- colorRampPalette(colors = c(
  "#300033", "#450e49", "#591e5d", "#6c2a71", "#7e3584", "#8f4096", "#a04aa8", "#b054b9", "#bf5dca", "#ce66da", "#dd6eeb", "#eb77fa", "#ed8cf8", "#eca1f4", "#ecb3f1", "#edc2f0", "#eecef1", "#f0d9f2", "#f3e3f4", "#f6ebf7", "#f8f2f9", "#fcfbfc", "#ffffff", "#f5f9f9", "#e5f1f1", "#d9edec", "#cde8e7", "#bce3e1", "#abdddc", "#94d7d5", "#77d1cf", "#4bcbc8", "#2cc1bf", "#2db5b3", "#2da8a5", "#2c9a98", "#2a8c8a", "#277d7c", "#246f6e", "#1f5f5e", "#194f4f", "#123e3e", "#082c2c", "#001919"
))(100)

coloursofthewind <- rev(coloursofthewind)

angleview <- as.matrix(read.table("/mnt/Storage4/FaceBase_3/Data/Images/Atlas/angle_user_matrix.txt"))
frontview <- as.matrix(read.table("/mnt/Storage4/FaceBase_3/Data/Images/Atlas/front_user_matrix.txt"))
sideview <- as.matrix(read.table("/mnt/Storage4/FaceBase_3/Data/Images/Atlas/side_user_matrix.txt"))
views <- c("frontview", "sideview", "angleview")

meshDist_wrapped <- function(x,y, col = "normal", view = "top", dists = NULL, ...){
  library(Morpho)
  
  if(class(x)[1] != "mesh3d"){x <- tps3d(atlas_mesh, atlas_lms, x)}
  if(class(y)[1] != "mesh3d"){y <- tps3d(atlas_mesh, atlas_lms, y)}
  
  if (col == "reverse") {coloursofthewind <- rev(coloursofthewind)}
  
  if(is.null(dists)){
    Heatmap <- meshDist(x,y)
    dists <- max(abs(Heatmap$dists))
  }
  
  if (col == "reverse") {coloursofthewind <- rev(coloursofthewind)}
  
  return(meshDist(x, y, steps = 100, rampcolors = coloursofthewind, from = -dists, to = dists))
  
  if (view == "top"){view3d(180, 0, 0)}
  if (view == "side"){view3d(180, -90, 0)}
}
