### R code from vignette source 'tagcloud.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: tagcloud.Rnw:16-17
###################################################
cairo <- function(name, width, height, ...) grDevices::cairo_pdf(file = paste0(name, ".pdf"), width = width, height = height)


###################################################
### code chunk number 2: tagcloud.Rnw:20-21
###################################################
png <- function(name, width, height, ...) grDevices::png(file = paste0(name, ".png"), width = width*300, height = height*300)


###################################################
### code chunk number 3: fig1plot
###################################################
library(tagcloud)
data(gambia)
tags <- strmultline(gambia$Term)[1:40]
weights <- -log(gambia$Pvalue)[1:40]
or <- gambia$OddsRatio[1:40]
colors <- smoothPalette(or, max=4)
tagcloud(tags, weights=weights, col=colors)


###################################################
### code chunk number 4: fig2plot
###################################################
par( mfrow=c( 3, 2 ) )
tagcloud(tags, weights=weights, col=colors, algorithm="oval")
tagcloud(tags, weights=weights, col=colors, algorithm="fill")
tagcloud(tags, weights=weights, col=colors, algorithm="snake")
tagcloud(tags, weights=weights, col=colors, algorithm="random")
tags2 <- gambia$Term[1:20]
cols2 <- colors[1:20]
wei2 <- weights[1:20]
tagcloud(tags2, weights=wei2, col=cols2, algorithm="list")
tagcloud(tags2, weights=wei2, col=cols2, algorithm="clist")


###################################################
### code chunk number 5: fig3plot
###################################################
par(mfrow=c(1, 2))
tagcloud(tags, weights=weights, col=colors, fvert=0.3)
tagcloud(tags, weights=weights, col=colors, fvert=0.7)


###################################################
### code chunk number 6: fig4plot
###################################################
par(mfrow=c(1, 2))
tagcloud(tags, weights=weights, col=colors, order="size")
tagcloud(tags, weights=weights, col=colors, order="random")


###################################################
### code chunk number 7: fig5plot (eval = FALSE)
###################################################
## library(extrafont)
## library(RColorBrewer)
## fnames <- sample(fonts(), 40)
## fweights <- rgamma(40, 1)
## fcolors <- colorRampPalette( brewer.pal( 12, "Paired" ) )( 40 )
## tagcloud( fnames, weights=fweights, col=fcolors, family=fnames )


###################################################
### code chunk number 8: fig6plot
###################################################
library(RColorBrewer)
colors <- smoothPalette(weights, pal= brewer.pal( 11, "Spectral" ) )
tagcloud(tags, weights=weights, col=colors, order="size")


###################################################
### code chunk number 9: fig7plot
###################################################
palf <- colorRampPalette( c( "blue", "grey", "red" ) )
colors <- smoothPalette(weights, palfunc= palf )
tagcloud(tags, weights=weights, col=colors, order="size")


