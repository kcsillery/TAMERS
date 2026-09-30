## #######################################################################

## TAMERS survey data analysis -- Katalin Csillery -- 9 Sept 2026

## script 9 looking into the reasons why respondent preferred or not a
## species, but here comparing it between foresters and researcers

## ########################################################################
library(pals)
library(RColorBrewer)

load("data/dat_all_choice.RData")
source("functions.r")

## identify all yes/no reason columns
yes.cols <- grep("^why_yes_.*_bin$", names(dat.choice), value=TRUE)
no.cols <- grep("^why_no_.*_bin$", names(dat.choice), value=TRUE)

## reasons are the suffixes that occur across species
yes.reasons <- c("Aesthetic",
                 "Biodiversity_value",
                 "Economic_value",
                 "Increase_genetic_diversity",
                 "Maintains_forest_composition",
                 ##"Replacement_for_Spruce_Picea_abies",
                 "Species_adapted_to_drought",
                 "Species_good_for_soil_protection")

no.reasons <- c("Ecosystem_collapse",
                "Frost_susceptibility",
                "Hybridizes_with_native_species",
                "Importing_pests_and_pathogens",
                "Invasive",
                "Outbreeding_depression",
                "Risk_to_be_affected_by_a_disease")

## species list from nativeness table
nativeness <- read.csv("data/Forest_species_nativeness_byCountry.csv", check.names=FALSE)
species <- names(nativeness)[names(nativeness)!="Country"]
length(species)
## 30

yes.answers <- c("Yes in pure stocks","Yes in mixed stocks","Yes, but sporadically")

## pooled counts, used only to retain the original species ordering
## #########################################################################
yes.mat <- matrix(0, nrow=length(species), ncol=length(yes.reasons), dimnames=list(species,yes.reasons))
no.mat <- matrix(0, nrow=length(species), ncol=length(no.reasons), dimnames=list(species,no.reasons))

for(sp in species) {
    for(r in yes.reasons) {
        v <- paste0("why_yes_",sub(" ","_",sp),"_",r,"_bin")
        if(v%in%names(dat.choice))
            yes.mat[sp,r] <- sum(dat.choice[[v]]==1,na.rm=TRUE)
    }
    for(r in no.reasons) {
        v <- paste0("why_no_",sub(" ","_",sp),"_",r,"_bin")
        if(v%in%names(dat.choice))
            no.mat[sp,r] <- sum(dat.choice[[v]]==1,na.rm=TRUE)
    }
}

## percentage selecting each reason among Yes or No decisions
## separately for forestry practitioners and researchers
## #########################################################################
yes.forester <- matrix(0, nrow=length(species), ncol=length(yes.reasons), dimnames=list(species,yes.reasons))
no.forester <- matrix(0, nrow=length(species), ncol=length(no.reasons), dimnames=list(species,no.reasons))
yes.researcher <- matrix(0, nrow=length(species), ncol=length(yes.reasons), dimnames=list(species,yes.reasons))
no.researcher <- matrix(0, nrow=length(species), ncol=length(no.reasons), dimnames=list(species,no.reasons))

for(sp in species) {

    sp.code <- sub(" ","_",sp,fixed=TRUE)
    choice.var <- paste0("choice_",sp.code)

    for(group in c("forester","researcher")) {

        is.group <- dat.choice$occupation.model==group
        yes.ii <- is.group & dat.choice[[choice.var]]%in%yes.answers
        no.ii <- is.group & dat.choice[[choice.var]]=="No"

        for(r in yes.reasons) {
            v <- paste0("why_yes_",sp.code,"_",r,"_bin")
            if(v%in%names(dat.choice) && sum(yes.ii,na.rm=TRUE)>0) {
                z <- 100*sum(dat.choice[[v]]==1 & yes.ii,na.rm=TRUE)/sum(yes.ii,na.rm=TRUE)
                if(group=="forester") yes.forester[sp,r] <- z else yes.researcher[sp,r] <- z
            }
        }

        for(r in no.reasons) {
            v <- paste0("why_no_",sp.code,"_",r,"_bin")
            if(v%in%names(dat.choice) && sum(no.ii,na.rm=TRUE)>0) {
                z <- 100*sum(dat.choice[[v]]==1 & no.ii,na.rm=TRUE)/sum(no.ii,na.rm=TRUE)
                if(group=="forester") no.forester[sp,r] <- z else no.researcher[sp,r] <- z
            }
        }
    }
}

## prettier labels
species.lab <- gsub("_"," ",species)

yes.lab <- c("Aesthetic",
             "Biodiversity value",
             "Economic value",
             "Increase genetic diversity",
             "Maintains forest composition",
             "Adapted to drought",
             "Soil protection")

no.lab <- c("Ecosystem collapse",
            "Frost susceptibility",
            "Hybridizes with native species",
            "Importing pests/pathogens",
            "Invasive",
            "Outbreeding depression",
            "Disease risk")

## colour families
## #########################################################################

## NO: restrained cool colours
no.col <- c(
    "#253B5B",  # dark navy
    "#4776A8",  # blue
    "#8BB6D6",  # light blue
    "#397F80",  # teal
    "#72A68C",  # muted green-teal
    "#68658A",  # blue-grey violet
    "#A8B6BC"   # light blue-grey
)

yes.col <- c(
    "#68483A",  # Aesthetic
    "#95604A",  # Biodiversity value
    "#C28A72",  # Economic value
    "#C77C43",  # Increase genetic diversity
    "#A58A67",  # Maintains forest composition
    "#D5A83E",  # Adapted to drought
    "#9B9B94"   # Soil protection
)

## retain original pooled species ordering
## #########################################################################
ord <- order(rowSums(yes.mat))

yes.forester <- yes.forester[ord,,drop=FALSE]
no.forester <- no.forester[ord,,drop=FALSE]
yes.researcher <- yes.researcher[ord,,drop=FALSE]
no.researcher <- no.researcher[ord,,drop=FALSE]
species.lab <- species.lab[ord]

## common y positions and x-range
## #########################################################################
bp <- barplot(t(yes.forester), horiz=TRUE, beside=FALSE, plot=FALSE, space=0.2)

ymax <- max(bp) + 0.7
yesmax <- max(c(rowSums(yes.forester), rowSums(yes.researcher)), na.rm=TRUE)
nomax <- max(c(rowSums(no.forester), rowSums(no.researcher)), na.rm=TRUE)
xmax <- max(yesmax, nomax)

ticks <- pretty(c(-xmax, xmax))

pdf("figures/Fig6_species_reasons_occupation.pdf", width=15, height=8)

layout(matrix(1:4, nrow=1), widths=c(1.15,1.25,1.25,1.1))

## species names and common y-axis
## #########################################################################
par(mar=c(5,0,2.5,0), family="sans")
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,ymax), xaxs="i", yaxs="i")

segments(0.98,0.4,0.98,ymax-0.2, col="grey30")

segments(0.94,bp,0.98,bp, col="grey30")
text(0.90,bp,labels=species.lab, adj=1, font=3, cex=1)

## a. forestry practitioners
## #########################################################################
par(mar=c(5,0.5,2.5,0.5), family="sans")

barplot(t(yes.forester), horiz=TRUE, beside=FALSE, col=yes.col, border=NA,
        xlim=c(-nomax, yesmax), ylim=c(0,ymax), space=0.2,
        xlab="", axes=FALSE, axisnames=FALSE)

barplot(-t(no.forester), horiz=TRUE, beside=FALSE, col=no.col, border=NA,
        add=TRUE, axes=FALSE, axisnames=FALSE, space=0.2)

abline(v=0, col="grey30", lwd=1)
axis(1, at=ticks, labels=abs(ticks))

mtext("Reasons selected (% of species decisions)", side=1, line=3)
mtext("a", side=3, line=.7, adj=0, font=2, cex=1.2)
mtext("Forestry practitioners", side=3, line=.7, adj=.5, font=2)

## b. researchers
## #########################################################################
par(mar=c(5,0.5,2.5,0.5), family="sans")

barplot(t(yes.researcher), horiz=TRUE, beside=FALSE, col=yes.col, border=NA,
        xlim=c(-nomax, yesmax), ylim=c(0,ymax), space=0.2,
        xlab="", axes=FALSE, axisnames=FALSE)

barplot(-t(no.researcher), horiz=TRUE, beside=FALSE, col=no.col, border=NA,
        add=TRUE, axes=FALSE, axisnames=FALSE, space=0.2)

abline(v=0, col="grey30", lwd=1)
axis(1, at=ticks, labels=abs(ticks))

mtext("Reasons selected (% of species decisions)", side=1, line=3)
mtext("b", side=3, line=.7, adj=0, font=2, cex=1.2)
mtext("Researchers", side=3, line=.7, adj=.5, font=2)

## reasons legend
## #########################################################################
par(mar=c(5,0.5,2.5,0.5), family="sans")
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,1))

legend(x=0, y=0.7, title="No reasons", legend=no.lab,
       col=no.col, pch=15, pt.cex=3, bty="n", cex=1.2,
       x.intersp=1.1, y.intersp=1.2)

legend(x=0, y=0.4, title="Yes reasons", legend=yes.lab,
       col=yes.col, pch=15, pt.cex=3, bty="n", cex=1.2,
       x.intersp=1.1, y.intersp=1.2)

dev.off()


## ############################################################################

## with 3rd panel that compares forester-researcher yes/no distribution

## ############################################################################



## model forester-researcher differences for individual reasons
## #########################################################################

dat.choice$region <- recode.group(dat.choice$country, how="minimalist")

is.forester <- dat.choice$occupation.model=="forester"
region.tab <- table(dat.choice$region[is.forester])
keep.region <- names(region.tab[region.tab>=3])

reason.group <- list()
k <- 1

for(sp in species) {

    sp.code <- sub(" ","_",sp,fixed=TRUE)
    choice.var <- paste0("choice_",sp.code)

    ## YES reasons
    ii <- dat.choice[[choice.var]]%in%yes.answers & dat.choice$region%in%keep.region & !is.na(dat.choice$occupation.model)

    for(r in yes.reasons) {
        v <- paste0("why_yes_",sp.code,"_",r,"_bin")
        if(v%in%names(dat.choice)) {
            reason.group[[k]] <- data.frame(Response_ID=dat.choice$Response_ID[ii], region=dat.choice$region[ii],
                                            species=sp, occupation.model=dat.choice$occupation.model[ii],
                                            decision="Yes", reason=r, selected=as.integer(dat.choice[[v]][ii]==1),
                                            stringsAsFactors=FALSE)
            k <- k+1
        }
    }

    ## NO reasons
    ii <- dat.choice[[choice.var]]=="No" & dat.choice$region%in%keep.region & !is.na(dat.choice$occupation.model)

    for(r in no.reasons) {
        v <- paste0("why_no_",sp.code,"_",r,"_bin")
        if(v%in%names(dat.choice)) {
            reason.group[[k]] <- data.frame(Response_ID=dat.choice$Response_ID[ii], region=dat.choice$region[ii],
                                            species=sp, occupation.model=dat.choice$occupation.model[ii],
                                            decision="No", reason=r, selected=as.integer(dat.choice[[v]][ii]==1),
                                            stringsAsFactors=FALSE)
            k <- k+1
        }
    }
}

reason.group <- do.call(rbind,reason.group)
reason.group$Response_ID <- factor(reason.group$Response_ID)
reason.group$region <- factor(reason.group$region)
reason.group$species <- factor(reason.group$species)
reason.group$occupation.model <- factor(reason.group$occupation.model,levels=c("forester","researcher"))

reason.diff <- list()
k <- 1

for(dec in c("No","Yes")) {

    reasons <- if(dec=="No") no.reasons else yes.reasons

    for(r in reasons) {

        d <- droplevels(reason.group[reason.group$decision==dec & reason.group$reason==r & !is.na(reason.group$selected),])

        fit <- asreml(fixed=selected ~ occupation.model,
                      random=~ Response_ID + region + species,
                      family=asr_binomial(link="logit"),data=d,maxit=100)

        fit <- update.asreml(fit)

        pp <- predict(fit,classify="occupation.model",sed=TRUE)
        p <- pp$pvals$transformed.value
        sed <- pp$sed[1,2]

        diff.logit <- pp$pvals$predicted.value[2]-pp$pvals$predicted.value[1]
        z <- diff.logit/sed
        pval <- 2*pnorm(-abs(z))

        ## difference on probability scale
        diff <- 100*(p[2]-p[1])

        ## approximate CI for difference using probability-scale SEs
        se.diff <- 100*sqrt(sum(pp$pvals$approx.se^2))
        lower <- diff-1.96*se.diff
        upper <- diff+1.96*se.diff

        reason.diff[[k]] <- data.frame(decision=dec,reason=r,
                                       forester=100*p[1],researcher=100*p[2],
                                       difference=diff,lower=lower,upper=upper,p=pval,
                                       stringsAsFactors=FALSE)
        k <- k+1
    }
}

reason.diff <- do.call(rbind,reason.diff)

reason.diff$label <- c(no.lab,yes.lab)[match(reason.diff$reason,c(no.reasons,yes.reasons))]
reason.diff$col <- c(no.col,yes.col)[match(reason.diff$reason,c(no.reasons,yes.reasons))]


## common y positions and x-range for panels a and b
## #########################################################################

bp <- barplot(t(yes.forester),horiz=TRUE,beside=FALSE,plot=FALSE,space=.2)
ymax <- max(bp)+.7

yesmax <- max(c(rowSums(yes.forester),rowSums(yes.researcher)),na.rm=TRUE)
nomax <- max(c(rowSums(no.forester),rowSums(no.researcher)),na.rm=TRUE)
xmax <- max(yesmax,nomax)
ticks <- pretty(c(-xmax,xmax))

pdf("figures/Fig5_species_reasons_occupation.pdf",width=15,height=8)

layout(matrix(1:4,nrow=1),widths=c(1.05,1.2,1.2,1.05))

## species names
## #########################################################################

par(mar=c(5,0,2.5,0),family="sans")
plot.new()
plot.window(xlim=c(0,1),ylim=c(0,ymax),xaxs="i",yaxs="i")

segments(.98,.4,.98,ymax-.2,col="grey30")
segments(.94,bp,.98,bp,col="grey30")
text(.90,bp,labels=species.lab,adj=1,font=3,cex=1)

## a. forestry practitioners
## #########################################################################

par(mar=c(5,.5,2.5,.5),family="sans")

barplot(t(yes.forester),horiz=TRUE,beside=FALSE,col=yes.col,border=NA,
        xlim=c(-xmax,xmax),ylim=c(0,ymax),space=.2,xlab="",axes=FALSE,axisnames=FALSE)

barplot(-t(no.forester),horiz=TRUE,beside=FALSE,col=no.col,border=NA,
        add=TRUE,axes=FALSE,axisnames=FALSE,space=.2)

abline(v=0,col="grey30",lwd=1)
axis(1,at=ticks,labels=abs(ticks))

mtext("Reasons selected (% of species decisions)",side=1,line=3)
mtext("a",side=3,line=.7,adj=0,font=2,cex=1.2)
mtext("Forestry practitioners",side=3,line=.7,adj=.5,font=2)

## b. researchers
## #########################################################################

par(mar=c(5,.5,2.5,.5),family="sans")

barplot(t(yes.researcher),horiz=TRUE,beside=FALSE,col=yes.col,border=NA,
        xlim=c(-xmax,xmax),ylim=c(0,ymax),space=.2,xlab="",axes=FALSE,axisnames=FALSE)

barplot(-t(no.researcher),horiz=TRUE,beside=FALSE,col=no.col,border=NA,
        add=TRUE,axes=FALSE,axisnames=FALSE,space=.2)

abline(v=0,col="grey30",lwd=1)
axis(1,at=ticks,labels=abs(ticks))

mtext("Reasons selected (% of species decisions)",side=1,line=3)
mtext("b",side=3,line=.7,adj=0,font=2,cex=1.2)
mtext("Researchers",side=3,line=.7,adj=.5,font=2)

## c. model-adjusted forester-researcher differences
## #########################################################################

rd <- reason.diff
rd <- rd[c(which(rd$decision=="No"),which(rd$decision=="Yes")),]
yy <- rev(seq_len(nrow(rd)))

xlim.c <- max(abs(c(rd$lower,rd$upper)),na.rm=TRUE)

par(mar=c(5,8.5,2.5,.5),family="sans",las=1)

plot(NA,xlim=c(-xlim.c,xlim.c),ylim=c(.5,nrow(rd)+.5),
     xlab="Researcher - forestry practitioner (percentage points)",
     ylab="",yaxt="n",bty="l")

abline(v=0,lty=2,col="grey60")

segments(rd$lower,yy,rd$upper,yy,col=rd$col,lwd=2)
points(rd$difference,yy,pch=15,col=rd$col,cex=1.8)

axis(2,at=yy,labels=rd$label,tick=FALSE,las=1,cex.axis=.85)

## emphasize supported contrasts
ii <- rd$p<.05
points(rd$difference[ii],yy[ii],pch=0,col="grey20",cex=1.55,lwd=1.3)

mtext("c",side=3,line=.7,adj=0,font=2,cex=1.2)
mtext("Difference between respondent groups",side=3,line=.7,adj=.5,font=2)

dev.off()
