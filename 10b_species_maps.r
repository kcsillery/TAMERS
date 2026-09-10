## #######################################################################

## TAMERS survey data analysis -- Katalin Csillery -- 9 Sept 2026

## 10_species_maps.r -- species-preference maps

## ########################################################################
library(RColorBrewer)
library(maps)

load("data/species_model_results.RData")
source("functions.r")

## extract random effects
## #########################################################################
ranef.tab <- summary(fit.species, coef=TRUE)$coef.random
region.levels <- levels(model.data$region)
species.levels <- levels(model.data$species)

normalize.name <- function(x) tolower(gsub("[^[:alnum:]]","",x))

match.level <- function(x, lev) {
    x <- normalize.name(x)
    lev.norm <- normalize.name(lev)
    hit <- which(vapply(lev.norm,function(z) grepl(z,x,fixed=TRUE),logical(1)))
    if(length(hit)==0) return(NA_character_)
    lev[hit[which.max(nchar(lev.norm[hit]))]]
}

rn <- rownames(ranef.tab)
r.region <- vapply(rn,match.level,character(1),lev=region.levels)
r.species <- vapply(rn,match.level,character(1),lev=species.levels)

is.region <- !is.na(r.region) & is.na(r.species)
is.species <- is.na(r.region) & !is.na(r.species)
is.region.species <- !is.na(r.region) & !is.na(r.species)

region.effects <- data.frame(region=r.region[is.region], region.effect=ranef.tab[is.region,"solution"], stringsAsFactors=FALSE)
species.effects.map <- data.frame(species=r.species[is.species], species.effect=ranef.tab[is.species,"solution"], stringsAsFactors=FALSE)
region.species <- data.frame(region=r.region[is.region.species], species=r.species[is.region.species], region.species.effect=ranef.tab[is.region.species,"solution"], stringsAsFactors=FALSE)

## fixed origin-status effects on the logit scale
## #########################################################################
origin.effects <- origin.pred$pvals[,c("origin.status","predicted.value")]
names(origin.effects)[2] <- "origin.effect"
origin.effects$origin.status <- as.character(origin.effects$origin.status)

## country x species predictions
## #########################################################################
species.map <- unique(species.long[,c("country","region","species","origin.status")])
species.map$origin.status <- as.character(species.map$origin.status)

species.map <- merge(species.map,origin.effects,by="origin.status",all.x=TRUE,sort=FALSE)
species.map <- merge(species.map,region.effects,by="region",all.x=TRUE,sort=FALSE)
species.map <- merge(species.map,species.effects.map,by="species",all.x=TRUE,sort=FALSE)
species.map <- merge(species.map,region.species,by=c("region","species"),all.x=TRUE,sort=FALSE)

ok <- complete.cases(species.map[,c("origin.effect","region.effect","species.effect","region.species.effect")])
species.map$eta <- NA_real_
species.map$eta[ok] <- with(species.map[ok,],origin.effect + region.effect + species.effect + region.species.effect)
species.map$prob <- plogis(species.map$eta)

species.map$map.name <- species.map$country
species.map$map.name[species.map$map.name=="Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
species.map$map.name[species.map$map.name=="Czechia"] <- "Czech Republic"
species.map$map.name[species.map$map.name=="United Kingdom"] <- "UK:Great Britain"

## ###################################
## combined figure for manuscript
## ###################################

europe.bg <- c("Ireland","UK","Portugal","Spain","France","Belgium","Netherlands","Luxembourg","Germany","Denmark","Norway","Sweden","Finland","Estonia","Latvia","Lithuania","Poland","Czech Republic","Slovakia","Austria","Switzerland","Italy","Slovenia","Croatia","Bosnia and Herzegovina","Serbia","Montenegro","Albania","Macedonia","Greece","Bulgaria","Romania","Hungary","Moldova","Ukraine","Belarus","Turkey")

## which maps to show?
## geographical variation in predicted planting preference
sp.var <- aggregate(prob ~ species, data=species.map, FUN=var, na.rm=TRUE)
names(sp.var)[2] <- "variance"

sp.var$european <- native.europe[sp.var$species]

eu.top <- sp.var[sp.var$european,]
eu.top <- eu.top[order(eu.top$variance,decreasing=TRUE),][1:4,]

noneu.top <- sp.var[!sp.var$european,]
noneu.top <- noneu.top[order(noneu.top$variance,decreasing=TRUE),][1:4,]

map.species <- c(eu.top$species,noneu.top$species)
sp.var[match(map.species,sp.var$species),]

map.native <- c("Picea abies", "Pinus nigra", "Larix decidua", "Populus alba", "Tilia tomentosa", "Fagus orientalis")
map.nnt <- c("Cedrus atlantica", "Abies grandis", "Sequoia sempervirens", "Picea sitchensis", "Robinia pseudoacacia", "Quercus rubra")

## combined species-effect figure and selected maps
## #########################################################################
## selected species
map.native <- c("Picea abies","Pinus nigra","Larix decidua",
                "Populus alba","Tilia tomentosa","Fagus orientalis")

map.nnt <- c("Cedrus atlantica","Abies grandis","Sequoia sempervirens",
             "Picea sitchensis","Robinia pseudoacacia","Quercus rubra")

map.species <- c(map.native,map.nnt)

## colours
eu.col <- hcl.colors(30,"Greens",rev=TRUE)[20]
noneu.col <- hcl.colors(30,"Blues 3",rev=TRUE)[20]

cols.eu <- hcl.colors(101,"Greens",rev=TRUE)
cols.noneu <- hcl.colors(101,"Blues 3",rev=TRUE)

prob.col <- function(p, cols) {
    if(is.na(p)) return("#F2F2F2")
    p <- max(0,min(1,p))
    cols[round(p*(length(cols)-1))+1]
}

## species-effect data
plot.df <- species.summary
plot.df <- plot.df[order(plot.df$effect),]
plot.df$col <- ifelse(plot.df$european.native,eu.col,noneu.col)
plot.df$shown <- plot.df$species%in%map.species
y <- seq_len(nrow(plot.df))

map.xlim <- c(-12, 42)
map.ylim <- c(35,60)

## #########################################################################
pdf("figures/Fig5_species_preferences_maps.pdf",width=12,height=9.5)

layout(matrix(c(1,2,3,4,
                1,5,6,7,
                1,8,9,10,
                1,11,12,13,
                1,14,14,14), nrow=5, byrow=TRUE),
       widths=c(1.9,1,1,1), heights=c(1,1,1,1,.42))

## panel a
## #########################################################################
par(mar=c(5,14,2.5,1.2),family="sans",las=1,bty="l")

plot(plot.df$effect,y,type="n",yaxt="n",
     xlim=range(c(plot.df$lower95,plot.df$upper95)),
     ylim=c(0.5,nrow(plot.df)+0.5),
     xlab="Species deviation in planting preference", ylab="", cex.lab=1.5)

abline(v=0,lty=2,col="grey60")

## thicker intervals; significant ones thicker again
for(i in seq_len(nrow(plot.df))) {
    segments(plot.df$lower95[i], y[i], plot.df$upper95[i], y[i],
             col=plot.df$col[i], lwd=if(plot.df$sig[i]) 4 else 2.2)
}

points(plot.df$effect, y, pch=16, col=plot.df$col, cex=2)

axis(2, at=y, labels=plot.df$species, tick=FALSE, las=1, font=3, cex.axis=1.4)

## circle species shown in maps
points(plot.df$effect[plot.df$shown],y[plot.df$shown],
       pch=1,col="grey20", cex=2,lwd=1.5)

legend("bottomright",
       legend=c("European","Non-European","Shown on maps"),
       col=c(eu.col,noneu.col,"grey20"), pch=c(16,16,1), cex=1.4, bty="o", bg="white", horiz=FALSE)

mtext("a",side=3,line=.7,adj=0,font=2,cex=1.3)

## panels b-m: 4 x 3 maps
## #########################################################################
for(k in seq_along(map.species)) {

    sp <- map.species[k]
    tmp <- species.map[species.map$species==sp,]
    cols.i <- if(sp%in%map.native) cols.eu else cols.noneu

    par(mar=c(0,.15,2.1,.15),bty="n",family="sans")
    plot.new()
    plot.window(xlim=map.xlim,ylim=map.ylim,xaxs="i",yaxs="i")

    map("world",regions=europe.bg,fill=TRUE,col="grey96",border="grey65",add=TRUE)

    for(i in seq_len(nrow(tmp)))
        map("world",regions=tmp$map.name[i],fill=TRUE,
            col=prob.col(tmp$prob[i],cols.i),border="grey55",add=TRUE)

    mtext(sp, side=3, line=.35, adj=.5, font=3, cex=.88, xpd=NA)

    if(k==1)
    mtext("b", side=3, line=.35, adj=0, font=2, cex=1.3, xpd=NA)
    
}

## common probability scales
par(mar=c(1.6,4,1,4), bty="n")
plot.new()
plot.window(xlim=c(-0.22,1.06), ylim=c(0,1), xaxs="i", yaxs="i")

xx <- seq(0,1,length.out=length(cols.eu)+1)

text(0.5,0.93,"Predicted positive planting preference", cex=1.5, xpd=NA)

text(-0.02,0.67,"European", adj=1, cex=1.5, xpd=NA)
rect(xx[-length(xx)],0.57,xx[-1],0.76, col=cols.eu, border=NA)

text(-0.02,0.35,"Non-European", adj=1, cex=1.5, xpd=NA)
rect(xx[-length(xx)],0.25,xx[-1],0.44, col=cols.noneu, border=NA)

ticks <- seq(0,1,.25)
text(ticks,0.06, labels=paste0(ticks*100,"%"), cex=1.5, xpd=NA)

dev.off()
