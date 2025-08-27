# Function to make heatmap gifs

# Provide the following:

# lm = linear model to describe axis to morph along e.g. procD.lm(shape ~ variable)$coefficients
# max = maximum score to visualize along axis (i.e. whatever variable is tested against shape in linear model)
# frames = number of frames TOTAL
# path = directory for images and gif
# col = inverse or normal
# view = top or side view (i.e. superior or lateral)
# inverse.morph = T or F; 
# control.morph = baseline landmarks

# rm(lm, max, frames, path, col.heatmap, atlas.mesh, atlas.lms)

meshDist_gif <- function(lm, max = 1, frames = 30, path, col = "inverse", palette = "GreMag", atlas.lms = atlas_mesh, atlas.mesh = atlas_lms, view = "top", inverse.morph = T){
  require(Morpho)
  require(magick)
  
  # check if arguments are the correct class
  stopifnot(!is.null(lm),
            !is.null(max),
            is.numeric(frames),
            is.character(path),
            is.character(col),
            is.character(palette),
            is.character(view),
            is.logical(inverse.morph))
  
  # make sure provided arguments are the correct choices
  col <- match.arg(col, c("normal", "reverse"))
  palette <- match.arg(palette, c("GreMag", "BluRed"))
  view <- match.arg(view, c("top", "side", "mandtop", "mandside"))
  
  # Choose colour palette: Green and Magenta | Blue and Red
  if (palette == "GreMag"){
    # Save two different colour schemes
    coloursofthewind <- colorRampPalette(colors = c(
      "#300033", "#450e49", "#591e5d", "#6c2a71", "#7e3584", "#8f4096", "#a04aa8", "#b054b9", "#bf5dca", "#ce66da", "#dd6eeb", "#eb77fa", "#ed8cf8", "#eca1f4", "#ecb3f1", "#edc2f0", "#eecef1", "#f0d9f2", "#f3e3f4", "#f6ebf7", "#f8f2f9", "#fcfbfc", "#ffffff", "#f5f9f9", "#e5f1f1", "#d9edec", "#cde8e7", "#bce3e1", "#abdddc", "#94d7d5", "#77d1cf", "#4bcbc8", "#2cc1bf", "#2db5b3", "#2da8a5", "#2c9a98", "#2a8c8a", "#277d7c", "#246f6e", "#1f5f5e", "#194f4f", "#123e3e", "#082c2c", "#001919"
    ))(100); coloursofthewind <- rev(coloursofthewind) # green should be inward vs pink should be outward
  } else {
    coloursofthewind <- c("#053061", "#083669", "#0B3C72", "#0E427A", "#114882", "#154E8B", "#185493", "#1B5A9B", "#1E60A4", "#2166AC", "#2166AC", "#256BAF", "#2970B1", "#2C75B4", "#307AB6", "#347FB9", "#3884BB", "#3B89BE", "#3F8EC0", "#4393C3", "#4393C3", "#4C99C6", "#559EC9", "#5DA4CC", "#66A9CF", "#6FAFD2", "#78B4D5", "#80BAD8", "#89BFDB", "#92C5DE", "#92C5DE", "#99C9E0", "#A0CCE2", "#A7D0E4", "#AED3E6", "#B5D7E8", "#BCDAEA", "#C3DEEC", "#CAE1EE", "#D1E5F0", "#D1E5F0", "#D5E7F1", "#D9E9F2", "#DEEBF2", "#E2EDF3", "#E6EFF4", "#EAF1F5", "#EFF3F5", "#F3F5F6", "#F7F7F7", "#F7F7F7", "#F8F4F2", "#F8F1EC", "#F9EEE7", "#FAEBE2", "#FAE7DC", "#FBE4D7", "#FCE1D2", "#FCDECC", "#FDDBC7", "#FDDBC7", "#FCD5BF", "#FBCFB8", "#FAC9B0", "#F9C3A8", "#F8BDA1", "#F7B799", "#F6B191", "#F5AB8A", "#F4A582", "#F4A582", "#F19D7C", "#ED9676", "#EA8E70", "#E7866A", "#E37F65", "#E0775F", "#DD6F59", "#D96853", "#D6604D", "#D6604D", "#D25849", "#CE5045", "#CA4842", "#C6403E", "#C2383A", "#BE3036", "#BA2833", "#B6202F", "#B2182B", "#B2182B", "#AA152A", "#A11328", "#991027", "#910D26", "#880B24", "#800823", "#780522", "#6F0320", "#67001F")
  }
  
  frames <- frames/2
  
  # Calculate scores for each frame
  if(inverse.morph){# show both ends of axis?
    selected.scores <- seq(-max, max, length.out = frames)
  } else {# show gif from control to max.morph
    selected.scores <- seq(0, max, length.out = frames)
  }
  file.names <- paste0("temp", 1:length(selected.scores), ".png")
  
  # Calculate distances...
  dists.meshes <- c("min.morph", "max.morph", "control.morph")
  dists <- NULL
  
  # First create meshes of extremes + control morph
  for(i in 1:3){
    datamod <- ~ c(-max,max,0)[i]
    mat <- model.matrix(datamod)
    pred <- mat %*% lm
    
    pred.lm <- arrayspecs(pred, p, k)
    
    assign(dists.meshes[i], tps3d(atlas.mesh, atlas.lms, pred.lm[,,1]))
  }
  
  # Take the largest absolute dist from each meshDist
  for(i in 1:3){
    dists <- c(dists, max(abs(meshDist(control.morph, get(dists.meshes[i]), plot=F)$dists)))}
  dists <- max(dists)

  # Generate images for each frame... creates temp files that will be removed at the end
  for(i in 1:length(selected.scores)){
    datamod <- ~ selected.scores[i]
    mat <- model.matrix(datamod)
    pred <- mat %*% lm
    
    pred.lm <- arrayspecs(pred, p, k)
    pred.mesh <- tps3d(atlas.mesh, atlas.lms, pred.lm[,,1])

    meshDist(pred.mesh, control.morph, rampcolors = palette, steps = 100, from = -dists, to = dists)
    if (view == "top"){view3d(180, 0, 0)}
    if (view == "side"){view3d(180, -90, 0)}
    if (view == "mandside"){view3d(90, 0, 0)}
    if (view == "mandtop"){view3d(0, -90, 0)}
    
    # Change size of images
    par3d(windowRect = c(20, 30, 1100, 1100))
    rgl.snapshot(paste0(path, "/", file.names[i]))
    close3d()
  }

  # Read images
  img_list <- lapply(paste0(path, "/", file.names), image_read)
  img_list <- c(img_list, rev(img_list))
  
  # stitch into gif and return
  gif <- image_animate(image_join(img_list), fps=10)
  return(gif)
  
  # remove temporary files
  file.remove(paste0(path, "/", file.names))
}
