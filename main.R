# Install required packages
if (!require('jpeg')) install.packages('jpeg'); 
if (!require('ggplot2')) install.packages('ggplot');

# Load required packages
library(jpeg)
library(ggplot2)

output_dir <- file.path("files")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  
}

# Download image from url
url <- "https://bringatrailer.com/wp-content/uploads/2019/08/1998_lamborghini_diablo_vt_roadster_156686808165ef66e7dff9f987625-e1568242425520.jpg?w=400"
download.file(url, "files/image.jpg")

# Check file size
print(paste("File size = ", file.info("files/image.jpg")$size))

# Read image file
readImage <- readJPEG("files/image.jpg")
print(paste("Dimension of image = ",nrow(readImage), "x", ncol(readImage)))

# Create image datafarame
dm <- dim(readImage)
dm
rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))

# Plot the image
jpeg(file="files/image_plot.jpg")
plot(y ~ x, data=rgbImage, main="image_plot",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".")
dev.off()
print(paste("File size of image_plot", file.info("files/image_plot.jpg")$size))

#===============Image Color Quantization using K-Means clustering=========

# Cluster the image colors using k-means and plot for different k values
kColors <- list(5,10,15,20,25)
for (k in kColors) {
  kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                   centers = k)
  colorCluster <- rgb(kMeans$centers[kMeans$cluster, ])
  
  filename <- paste("files/image_plot_k", toString(k), ".jpg", sep="")
  jpeg(file=filename)
  plot(y ~ x, data=rgbImage,
       col = colorCluster,
       axes=FALSE)
  dev.off()
  print(paste("File size of image_plot_k", toString(k), " = ", file.info(filename)$size, sep=""))
}

kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = 20)
colorCluster <- rgb(kMeans$centers[kMeans$cluster, ])

filename <- paste("image_plot_k", toString(18), ".jpg", sep="")
jpeg(file=filename)
plot(y ~ x, data=rgbImage,
     col = colorCluster,
     axes=FALSE)
dev.off()
print(paste("File size of image_plot_k", toString(18), " = ", file.info(filename)$size, sep=""))


#===============Image Compression using Principal Component Analysis=================
  
# Principal Component Analysis of image for each scheme
r <- readImage[,,1]
g <- readImage[,,2]
b <- readImage[,,3]

r.pca <- prcomp(r, center = FALSE)
g.pca <- prcomp(g, center = FALSE)
b.pca <- prcomp(b, center = FALSE)

summary(r.pca)

rgb.pca <- list(r.pca, g.pca, b.pca)

# Compression
compress <- function(pr, k)
{
  compressed.img <- pr$x[,1:k] %*% t(pr$rotation[,1:k])
  compressed.img
}

# Reconstruct the image for number of principal components
pcNums <- list(25,50,100,150,200)
for (p in pcNums){
  pcaImage <- sapply(rgb.pca, compress, k=p, simplify="array")
  filename <- paste("files/image_plot_pc", toString(p), ".jpg", sep="")
  writeJPEG(pcaImage, filename)
  print(paste("File size of image_plot_pc", toString(p), " = ", file.info(filename)$size, sep=""))
}

pcaImage <- sapply(rgb.pca, compress, k=174, simplify="array")
filename <- paste("image_plot_pc", toString(174), ".jpg", sep="")
writeJPEG(pcaImage, filename)
print(paste("File size of image_plot_pc", toString(174), " = ", file.info(filename)$size, sep=""))
