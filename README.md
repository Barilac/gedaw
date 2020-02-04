# Gedaw

Now we are developing a new geostatistical package. Our package *gedaw* combines geophysical methods and compositional data.

##
<center>
```{r pca, echo=FALSE, message=FALSE, warning=FALSE, comment=FALSE, webgl=TRUE, fig.width=6, fig.height=6}
library(dplyr)
naschange = function (a, b=NULL, factor=2) { #factor je roven 2, lze p??enastavit (XRF)
  
  if (is.null(b)) {vars=1:length(a)} else {vars=b} #vars je z dplyr, fce select
  
  for (i in vars) {
    
    a[which(is.na(a[,i])),i]=min(a[,i], na.rm=T)/factor}
  
  a
  
}

b2=read.table(file="C://Users//janov//Documents//Martin//Tel Burna_článek//Tel Burna_geochemistry//19 11 03 tel_burna_2018_B2.txt", header=TRUE, sep="\t", dec=",")
tel_pca_b2=b2[,c(15:17, 19:23, 25,26,29,30,34)]
tel_pca_b2=naschange(tel_pca_b2)

library(robCompositions)

p <- pcaCoDa(tel_pca_b2, method="robust")


s <- summary(p)


# Define color for each of the 5 squares
colors <- c("brown1", "lightblue", "gray", "yellow2", "chocolate")
colors <- colors[as.numeric(b2$Square)]

# Define shapes
shapes = c(16, 16, 16, 16, 16) 
shapes <- shapes[as.numeric(b2$Square)]


  
  # Plot, nutne takto slozite definovat mimo biplot
  plot(p$scores[,1], p$scores[,2], xlab=paste("PC1 (76.97%)", sep = ""),
       ylab=paste("PC2 (10.26%)", sep = ""),
       pch=shapes, col=colors, cex=1, las=1, asp=1, main="Area B2", cex.main=1.5, cex.lab=1, cex.axis=1)
  
  abline(v=0, lty=2, col="grey50", lwd=2)
  abline(h=0, lty=2, col="grey50", lwd=2)
  
  
  # text(p$scores[,1], p$scores[,2], labels=b2$Square, pos=c(1,3,4,2), font=2,cex=0.35)
  
  # Get co-ordinates of variables (loadings), and multiply by 2
  l.x <- p[["loadings"]][,1]*2
  l.y <- p[["loadings"]][,2]*2
  
  # Draw arrows
  arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="red", length=0.15, lwd=.8)
  
  # Label position
  l.pos <- l.y # Create a vector of y axis coordinates
  lo <- which(l.y < 0) # Get the variables on the bottom half of the plot
  hi <- which(l.y > 0) # Get variables on the top half
  # Replace values in the vector
  l.pos <- replace(l.pos, lo, "1")
  l.pos <- replace(l.pos, hi, "3")
  
  # Variable labels
  text(l.x, l.y, labels=row.names(p[["loadings"]]), col="black", pos=l.pos, cex=1)
  
  # Legenda
  legend("topright", legend=levels(b2$Square), pch=c(16, 16, 16, 16, 16),
         col= c("brown1", "lightblue", "gray", "yellow2", "chocolate"), cex=1, inset=0.02,0.02,0.02,0.02)
```
</center>
##

