## #######################################################################

## TAMERS survey data analysis -- Katalin Csillery -- 8 Sept 2026

## 8. plot the main results figure using the model of acceptance
## scores (planter/nativer) and constructs

## ########################################################################

library(pals)
library(RColorBrewer)
library(maps)

load(file="data/constructs_blups.RData")
source("functions.r")

## for part a
blue.col <- hcl.colors(30, "Blues 3")[1:2]
my.xlim <- range(c(0,
                   vec.df$planter - vec.df$se.p,
                   vec.df$planter + vec.df$se.p))
my.ylim <- range(c(0,
                   vec.df$nativer - vec.df$se.n,
                   vec.df$nativer + vec.df$se.n))
my.xlim[2] <- my.xlim[2] * 1.32
my.ylim[2] <- my.ylim[2] * 1.10

cols.planter <- make.div.cols("BrBG")
cols.nativer <- make.div.cols("PiYG")

zlim.planter <- max(abs(country.map$planter), na.rm = TRUE)
zlim.nativer <- max(abs(country.map$nativer), na.rm = TRUE)

europe.bg <- c(
    "Ireland", "UK", "Portugal", "Spain", "France",
    "Belgium", "Netherlands", "Luxembourg", "Germany",
    "Denmark", "Norway", "Sweden", "Finland",
    "Estonia", "Latvia", "Lithuania",
    "Poland", "Czech Republic", "Slovakia",
    "Austria", "Switzerland", "Italy",
    "Slovenia", "Croatia", "Bosnia and Herzegovina",
    "Serbia", "Montenegro", "Albania", "Macedonia",
    "Greece", "Bulgaria", "Romania", "Hungary",
    "Moldova", "Ukraine", "Belarus", "Turkey"
)

map.xlim <- c(-12, 42)
map.ylim <- c(35, 60)

## ##############################################################
pdf("figures/Fig4_Effects_in_attitude_space.pdf", width=9.1, height=7)
## ##############################################################

layout(matrix(c(1,2,
                1,4,
                1,3,
                1,5),
              nrow=4, byrow=TRUE),
       widths=c(1.85,1),
       heights=c(1,.33,1,.33))

## #####################################################################
## a. Construct effects
## #####################################################################

par(mar=c(5.2,5.2,3.1,5), bty="n", family="sans", xpd=T)

plot(NA,
     xlim = my.xlim,
     ylim = my.ylim,
     xlab = "Effect on general willingness to plant",
     ylab = "Effect on native species preference",
     main = "",
     cex.lab = 1.6, cex.axis=1.4)
abline(h = 0, v = 0,  col = "grey70", lty = 2, lwd = 1)
abline(a = 0, b = 1, col = "grey85", lty = 3, lwd = 1)

## SE bars first
for (i in seq_len(nrow(vec.df))) {

    x <- vec.df$planter[i]
    y <- vec.df$nativer[i]

    ex <- vec.df$se.p[i]
    ey <- vec.df$se.n[i]

    col.bar.p <- if (vec.df$sig.p[i]) blue.col[1] else "grey75"
    col.bar.n <- if (vec.df$sig.n[i]) blue.col[1] else "grey75"

    segments(x - ex, y, x + ex, y, col = col.bar.p, lwd = 2)
    segments(x, y - ey, x, y + ey, col = col.bar.n, lwd = 2)
}

## arrows and labels
for (i in seq_len(nrow(vec.df))) {
    
    col.i <- if (vec.df$sig.any[i]) blue.col[2] else "grey65"
    lwd.i <- if (vec.df$sig.any[i]) 3 else 1
    
    x <- vec.df$planter[i]
    y <- vec.df$nativer[i]
    lab <- vec.df$label[i]
    
    arrows(0, 0, x, y, length = 0.08, col = col.i, lwd = lwd.i)
    
    ## equal distance above and below the SE cross
    h <- strheight(lab, cex = 1.2)
    gap <- 0.003
    
    if (vec.df$var[i] %in%
        c("barrier.orientation",
          "infosource.level",
          "action.strategy.score")) {
        y.lab <- y - 0.65*h - gap
    } else y.lab <- y + 0.65*h + gap
    
    ## labels readable regardless of significance
    boxed.text(x + 0.004, y.lab, lab,
               col = if (vec.df$sig.any[i]) blue.col[2] else "grey35",
               cex = 1.5,
               font = if (vec.df$sig.any[i]) 2 else 1,
               adj = c(0, 0.5))

}
## legend for a
legend("topright", legend = c("Significant effect (P < 0.05)",
                              "No significant effect",
                              "Outcome-specific P < 0.05"),
       col = c(blue.col[2], "grey65", blue.col[1]), lwd = c(3, 1, 2), bty = "n", cex = 1.5, seg.len = 2.2)

mtext("a", side = 3, line = 1, adj = 0, font = 2, cex = 1.3, xpd = NA)
mtext("Standardized construct effects", side = 3, line = 1, adj = 0.5, font = 2, cex = 1.1, xpd = NA)

## #####################################################################
## b. General willingness to plant
## #####################################################################

par(mar=c(0,.15,2.1,.15),bty="n",family="sans")

plot.new()
plot.window(xlim = map.xlim, ylim = map.ylim, xaxs = "i", yaxs = "i")

map("world", regions = europe.bg, fill = TRUE,
    col = "grey96", border = "grey65", add = TRUE)

for (i in seq_len(nrow(country.map)))
    map("world", regions = country.map$map.name[i], fill = TRUE,
        col = map.col(country.map$planter[i], country.map$sig.planter[i],
                      zlim.planter, cols.planter),
        border = "grey55", add = TRUE)

mtext("b",side=3,line=.35,adj=0,font=2,cex=1.3,xpd=NA)
mtext("General willingness to plant",side=3,line=.35,adj=.5,font=2,cex=1.1,xpd=NA)

## #####################################################################
## c. Native species preference
## #####################################################################

par(mar=c(0,.15,2.1,.15),bty="n",family="sans")

plot.new()
plot.window(xlim = map.xlim, ylim = map.ylim, xaxs = "i", yaxs = "i")

map("world", regions = europe.bg, fill = TRUE,
    col = "grey96", border = "grey65", add = TRUE)

for (i in seq_len(nrow(country.map)))
    map("world", regions = country.map$map.name[i], fill = TRUE,
        col = map.col(country.map$nativer[i], country.map$sig.nativer[i],
                      zlim.nativer, cols.nativer),
        border = "grey55", add = TRUE)

mtext("c",side=3,line=.35,adj=0,font=2,cex=1.3,xpd=NA)
mtext("Native species preference",side=3,line=.35,adj=.5,font=2,cex=1.1,xpd=NA)

## add scales
draw.scale.panel(cols.planter,zlim.planter,"Regional BLUP, adjusted for covariates",
                 cex.title=1.25,cex.scale=1.25,bar.frac=0.60)

draw.scale.panel(cols.nativer,zlim.nativer,"Regional BLUP, adjusted for covariates",
                 cex.title=1.25,cex.scale=1.25,bar.frac=0.60)

dev.off()
