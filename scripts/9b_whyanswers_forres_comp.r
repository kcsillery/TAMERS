## #######################################################################

## TAMERS survey data analysis -- Katalin Csillery -- 9 Sept 2026

## script 9 looking into the reasons why respondent preferred or not a
## species, but here comparing it between foresters and researcers

## ########################################################################
library(pals)
library(RColorBrewer)
library(asreml)

setwd("~/Dropbox/projects/TAMERS_LandscapePaper/analysis/")
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

## Descriptive counts of benefits and risks per species evaluation
## ##############################################################

reason.counts <- list()
k.count <- 1

for(sp in species) {

    sp.code <- sub(" ", "_", sp, fixed=TRUE)
    choice.var <- paste0("choice_", sp.code)

    if(!choice.var %in% names(dat.choice)) next

    choice <- dat.choice[[choice.var]]
    ii <- which(!is.na(choice))

    if(length(ii) == 0) next

    count.selected <- function(reasons, prefix) {

        vars <- paste0(prefix, sp.code, "_", reasons, "_bin")

        ## Do not silently omit a missing reason column
        stopifnot(all(vars %in% names(dat.choice)))

        x <- dat.choice[ii, vars, drop=FALSE]

        ## Matches the original descriptive calculation:
        ## missing checkbox values contribute no selections.
        rowSums(x == 1, na.rm=TRUE)
    }

    reason.counts[[k.count]] <- data.frame(
        Response_ID=dat.choice$Response_ID[ii],
        species=sp,
        choice=choice[ii],
        n.benefit=count.selected(yes.reasons, "why_yes_"),
        n.risk=count.selected(no.reasons, "why_no_"),
        stringsAsFactors=FALSE
    )

    k.count <- k.count + 1
}

reason.counts <- do.call(rbind, reason.counts)

summarize.counts <- function(x) {
    c(
        n=length(x),
        mean=mean(x),
        median=median(x),
        q25=unname(quantile(x, 0.25)),
        q75=unname(quantile(x, 0.75)),
        percent.any=100 * mean(x >= 1),
        percent.two.plus=100 * mean(x >= 2),
        percent.three.plus=100 * mean(x >= 3)
    )
}

## Benefits and risks across all evaluations
reason.summary.all <- rbind(
    Benefits=summarize.counts(reason.counts$n.benefit),
    Risks=summarize.counts(reason.counts$n.risk)
)

## Conditional summaries corresponding to the previous manuscript text
accepted <- reason.counts$choice %in% yes.answers
rejected <- reason.counts$choice %in% "No"

reason.summary.conditional <- rbind(
    Benefits.among.accepted=
        summarize.counts(reason.counts$n.benefit[accepted]),
    Risks.among.rejected=
        summarize.counts(reason.counts$n.risk[rejected])
)

round(reason.summary.all, 2)
round(reason.summary.conditional, 2)

## create overview table of the answers when confirming the decision
## (ie when benefit for accepted or risk for rejected) vs when it's
## not
##########################################################
## Order of planting-choice categories
answer.order <- c(
    "Yes, but sporadically",
    "Yes in mixed stocks",
    "Yes in pure stocks",
    "No",
    "Not applicable for my reference forest",
    "I don't know"
)

## Check that the labels match your data
setdiff(unique(reason.counts$choice), answer.order)
benefit.table <- make.reason.table("n.benefit")
risk.table <- make.reason.table("n.risk")

benefit.table
risk.table

## verify counts of contradicting selections
## #########################################

accepted <- reason.counts$choice %in% yes.answers
rejected <- reason.counts$choice %in% "No"

rejected.only.benefits <- sum(
    rejected &
    reason.counts$n.benefit > 0 &
    reason.counts$n.risk == 0
)

accepted.only.risks <- sum(
    accepted &
    reason.counts$n.risk > 0 &
    reason.counts$n.benefit == 0
)

c(rejected.only.benefits, accepted.only.risks)

100 * (rejected.only.benefits + accepted.only.risks) /
    sum(accepted | rejected)


## which risks were associated different types of yes responses?
## ##########################################################################

acceptance.order <- c(
    "Yes in pure stocks",
    "Yes in mixed stocks",
    "Yes, but sporadically",
    "No"
)

risk.pattern <- list()
k.pattern <- 1

for(sp in species) {

    sp.code <- sub(" ", "_", sp, fixed=TRUE)
    choice.var <- paste0("choice_", sp.code)

    if(!choice.var %in% names(dat.choice)) next

    choice <- dat.choice[[choice.var]]

    for(answer in acceptance.order) {

        ii <- which(choice %in% answer)

        for(r in no.reasons) {

            v <- paste0("why_no_", sp.code, "_", r, "_bin")

            if(!v %in% names(dat.choice)) next

            x <- dat.choice[[v]][ii]

            risk.pattern[[k.pattern]] <- data.frame(
                species=sp,
                acceptance=answer,
                risk=r,
                n.accepted=length(ii),
                n.observed=sum(!is.na(x)),
                n.selected=sum(x == 1, na.rm=TRUE),
                stringsAsFactors=FALSE
            )

            k.pattern <- k.pattern + 1
        }
    }
}

risk.pattern.species <- do.call(rbind, risk.pattern)

## Pool counts across species before calculating percentages
risk.pattern.pooled <- aggregate(
    cbind(n.accepted, n.observed, n.selected) ~ acceptance + risk,
    data=risk.pattern.species,
    FUN=sum
)

add.percent <- function(d) {
    d$Percent <- NA_real_
    ok <- d$n.observed > 0
    d$Percent[ok] <- round(100 * d$n.selected[ok] / d$n.observed[ok], 1)
    d
}

risk.pattern.species <- add.percent(risk.pattern.species)
risk.pattern.pooled <- add.percent(risk.pattern.pooled)

## Order rows consistently
risk.pattern.pooled <- risk.pattern.pooled[
    order(
        match(risk.pattern.pooled$risk, no.reasons),
        match(risk.pattern.pooled$acceptance, acceptance.order)
    ),
]

risk.pattern.species <- risk.pattern.species[
    order(
        match(risk.pattern.species$species, species),
        match(risk.pattern.species$risk, no.reasons),
        match(risk.pattern.species$acceptance, acceptance.order)
    ),
]

rownames(risk.pattern.pooled) <- NULL
rownames(risk.pattern.species) <- NULL

## Full tables, including denominators
risk.pattern.pooled
risk.pattern.species

## Compact pooled percentage table
risk.percent.table <- matrix(
    NA_real_,
    nrow=length(no.reasons),
    ncol=length(acceptance.order),
    dimnames=list(no.reasons, acceptance.order)
)

for(i in seq_len(nrow(risk.pattern.pooled))) {

    d <- risk.pattern.pooled[i, ]

    risk.percent.table[d$risk, d$acceptance] <- d$Percent
}

risk.percent.table

## Example: inspect the pattern for one species
risk.pattern.species[
    risk.pattern.species$species == "Robinia pseudoacacia",
]


## pooled counts to retain the original species ordering
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

## percentage selecting each reason among all species decisions
## separately for forestry practitioners and researchers
## #########################################################################

yes.forester <- matrix(0, nrow=length(species), ncol=length(yes.reasons), dimnames=list(species, yes.reasons))
no.forester <- matrix(0, nrow=length(species), ncol=length(no.reasons), dimnames=list(species, no.reasons))
yes.researcher <- matrix(0, nrow=length(species), ncol=length(yes.reasons), dimnames=list(species, yes.reasons))
no.researcher <- matrix(0, nrow=length(species), ncol=length(no.reasons), dimnames=list(species, no.reasons))

for(sp in species) {

    sp.code <- sub(" ", "_", sp, fixed=TRUE)
    choice.var <- paste0("choice_", sp.code)

    for(group in c("forester", "researcher")) {

        ii <- dat.choice$occupation.model == group & !is.na(dat.choice[[choice.var]])
        N <- sum(ii, na.rm=TRUE)

        for(r in yes.reasons) {
            v <- paste0("why_yes_", sp.code, "_", r, "_bin")
            if(v %in% names(dat.choice) && N > 0) {
                z <- 100 * sum(dat.choice[[v]] == 1 & ii, na.rm=TRUE) / N
                if(group == "forester") yes.forester[sp, r] <- z else yes.researcher[sp, r] <- z
            }
        }

        for(r in no.reasons) {
            v <- paste0("why_no_", sp.code, "_", r, "_bin")
            if(v %in% names(dat.choice) && N > 0) {
                z <- 100 * sum(dat.choice[[v]] == 1 & ii, na.rm=TRUE) / N
                if(group == "forester") no.forester[sp, r] <- z else no.researcher[sp, r] <- z
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

## ############################################################################

## with 3rd panel that compares forester-researcher yes/no distribution

## ############################################################################

## model forester-researcher differences across all species decisions
## #########################################################################

dat.choice$region <- recode.group(dat.choice$country, how="minimalist")

is.forester <- dat.choice$occupation.model == "forester"
region.tab <- table(dat.choice$region[is.forester])
keep.region <- names(region.tab[region.tab >= 3])

reason.group <- list()
k <- 1

for(sp in species) {

    sp.code <- sub(" ", "_", sp, fixed=TRUE)
    choice.var <- paste0("choice_", sp.code)

    ii <- !is.na(dat.choice[[choice.var]]) &
          dat.choice$region %in% keep.region &
          !is.na(dat.choice$occupation.model)

    ## YES reasons

    for(r in yes.reasons) {
        v <- paste0("why_yes_", sp.code, "_", r, "_bin")

        if(v %in% names(dat.choice)) {
            reason.group[[k]] <- data.frame(
                Response_ID=dat.choice$Response_ID[ii],
                region=dat.choice$region[ii],
                species=sp,
                occupation.model=dat.choice$occupation.model[ii],
                decision="Yes",
                reason=r,
                selected=as.integer(dat.choice[[v]][ii] == 1),
                stringsAsFactors=FALSE
            )
            k <- k + 1
        }
    }

    ## NO reasons

    for(r in no.reasons) {
        v <- paste0("why_no_", sp.code, "_", r, "_bin")

        if(v %in% names(dat.choice)) {
            reason.group[[k]] <- data.frame(
                Response_ID=dat.choice$Response_ID[ii],
                region=dat.choice$region[ii],
                species=sp,
                occupation.model=dat.choice$occupation.model[ii],
                decision="No",
                reason=r,
                selected=as.integer(dat.choice[[v]][ii] == 1),
                stringsAsFactors=FALSE
            )
            k <- k + 1
        }
    }
}

reason.group <- do.call(rbind, reason.group)
reason.group$Response_ID <- factor(reason.group$Response_ID)
reason.group$region <- factor(reason.group$region)
reason.group$species <- factor(reason.group$species)
reason.group$occupation.model <- factor(reason.group$occupation.model,
                                        levels=c("forester", "researcher"))

## model with asreml
## ###########################
reason.diff <- list()
risk.region <- list()
k <- 1
kr <- 1

for(dec in c("No","Yes")) {

    reasons <- if(dec=="No") no.reasons else yes.reasons

    for(r in reasons) {

        d <- droplevels(reason.group[reason.group$decision==dec & reason.group$reason==r & !is.na(reason.group$selected),])

        fit <- asreml(fixed=selected ~ occupation.model, random=~ Response_ID + species + region, family=asr_binomial(link="logit"), data=d, maxit=100)
        fit <- update.asreml(fit)

        ## Regional BLUPs for risks only
        if(dec=="No") {
            cr <- summary(fit, coef=TRUE)$coef.random
            ii <- grep("^region", rownames(cr))
            tmp <- data.frame(reason=r, region=sub("^region[_!: ]*", "", rownames(cr)[ii]), BLUP=cr[ii, 1], SE=cr[ii, 2], stringsAsFactors=FALSE)
            tmp$z <- tmp$BLUP / tmp$SE
            tmp$lower <- tmp$BLUP - 1.96 * tmp$SE
            tmp$upper <- tmp$BLUP + 1.96 * tmp$SE
            tmp$supported <- abs(tmp$z) > 1.96
            risk.region[[kr]] <- tmp
            kr <- kr + 1
        }

        pp <- predict(fit, classify="occupation.model", sed=TRUE, vcov=TRUE)

        ## Explicit order: forester, researcher
        idx <- match(c("forester", "researcher"), as.character(pp$pvals$occupation.model))
        stopifnot(!anyNA(idx))

        ## Logit-scale predictions and their covariance
        eta <- pp$pvals$predicted.value[idx]
        V <- as.matrix(pp$vcov)[idx, idx, drop=FALSE]
        stopifnot(all(is.finite(eta)), all(is.finite(V)))

        ## Predicted probabilities
        p <- plogis(eta)

        ## Logit-scale contrast and P value
        contrast <- c(-1, 1)
        diff.logit <- sum(contrast * eta)
        se.logit <- sqrt(drop(t(contrast) %*% V %*% contrast))
        z <- diff.logit / se.logit
        pval <- 2 * pnorm(-abs(z))

        ## Probability difference in percentage points
        diff <- 100 * (p[2] - p[1])

        ## Delta-method CI, accounting for covariance
        gradient <- c(-p[1] * (1 - p[1]), p[2] * (1 - p[2]))
        se.diff <- 100 * sqrt(drop(t(gradient) %*% V %*% gradient))
        lower <- diff - qnorm(0.975) * se.diff
        upper <- diff + qnorm(0.975) * se.diff

        reason.diff[[k]] <- data.frame(decision=dec, reason=r, forester=100*p[1], researcher=100*p[2], difference=diff, lower=lower, upper=upper, p=pval, stringsAsFactors=FALSE)
        k <- k + 1
    }
}

## create joint objects
reason.diff <- do.call(rbind, reason.diff)
reason.diff$label <- c(no.lab,yes.lab)[match(reason.diff$reason,c(no.reasons,yes.reasons))]
reason.diff$col <- c(no.col,yes.col)[match(reason.diff$reason,c(no.reasons,yes.reasons))]

## ########################################################################
##            test for regional significant BLUPS in risks
## ########################################################################

risk.region <- do.call(rbind, risk.region)
risk.region$label <- no.lab[match(risk.region$reason, no.reasons)]

## inspect all regional BLUPs
risk.region[order(risk.region$reason, -abs(risk.region$z)), ]

## only supported regional deviations
risk.region.sig <- risk.region[risk.region$supported, ]
risk.region.sig <- risk.region.sig[order(-abs(risk.region.sig$z)), ]
risk.region.sig

## species order: most preferred at top, least preferred at bottom
## ################################################################
benefit.order <- aggregate(n.benefit ~ species, data=reason.counts, FUN=mean)
benefit.order <- benefit.order[order(benefit.order$n.benefit), ]
plot.order <- benefit.order$species

yes.forester <- yes.forester[plot.order, , drop=FALSE]
no.forester <- no.forester[plot.order, , drop=FALSE]
yes.researcher <- yes.researcher[plot.order, , drop=FALSE]
no.researcher <- no.researcher[plot.order, , drop=FALSE]

species.lab <- plot.order

stopifnot(
    identical(rownames(yes.forester), species.lab),
    identical(rownames(no.forester), species.lab),
    identical(rownames(yes.researcher), species.lab),
    identical(rownames(no.researcher), species.lab)
)

bp <- barplot(t(yes.forester), horiz=TRUE, beside=FALSE,
              plot=FALSE, space=.2)

ymax <- max(bp) + .7

## common y positions and x-range for panels a and b
## #########################################################################

yesmax <- max(c(rowSums(yes.forester),rowSums(yes.researcher)),na.rm=TRUE)
nomax <- max(c(rowSums(no.forester),rowSums(no.researcher)),na.rm=TRUE)
xmax <- max(yesmax,nomax)
ticks <- pretty(c(-xmax,xmax))

## ############################################################################
pdf("figures/Fig5_species_reasons_occupation.pdf",width=14.2,height=7)
## ############################################################################

layout(matrix(1:4,nrow=1),
       widths=c(.5,1.2,1.2,1.2))

## species names
## #########################################################################

par(mar=c(6,0.5,2.5,0),family="sans")
plot.new()
plot.window(xlim=c(0,1), ylim=c(0,ymax), xaxs="i",yaxs="i")

segments(.98, .4, .98, ymax-.2, col="grey30")
segments(.94, bp, .98, bp, col="grey30")
text(.90, bp, labels=species.lab, adj=1, font=3, cex=1.2)

## a. forestry practitioners
## #########################################################################

par(mar=c(6,.5,2.5,.5),family="sans")

barplot(t(yes.forester), horiz=TRUE, beside=FALSE, col=yes.col, border=NA, xlim=c(-nomax, yesmax),
        ylim=c(0, ymax), yaxs="i", space=.2, xlab="", axes=FALSE, axisnames=FALSE)

barplot(-t(no.forester), horiz=TRUE, beside=FALSE, col=no.col, border=NA, ylim=c(0, ymax),
        yaxs="i", add=TRUE, axes=FALSE, axisnames=FALSE, space=.2)

abline(v=0,col="grey30",lwd=1)
axis(1,at=ticks,labels=abs(ticks), cex.axis=1.4)

mtext("Reason selections per 100 species decisions", side=1, line=3)
mtext("a",side=3,line=.7,adj=0,font=2,cex=1.2)
mtext("Forestry practitioners",side=3,line=.7,adj=.5,font=2)

## b. researchers
## #########################################################################

par(mar=c(6,.5,2.5,.5),family="sans")

barplot(t(yes.researcher), horiz=TRUE, beside=FALSE, col=yes.col, border=NA, xlim=c(-nomax, yesmax),
        ylim=c(0, ymax), yaxs="i", space=.2, xlab="", axes=FALSE, axisnames=FALSE)

barplot(-t(no.researcher), horiz=TRUE, beside=FALSE, col=no.col, border=NA, ylim=c(0, ymax),
        yaxs="i", add=TRUE, axes=FALSE, axisnames=FALSE, space=.2)

abline(v=0,col="grey30",lwd=1)
axis(1,at=ticks,labels=abs(ticks), cex.axis=1.4)

mtext("Reason selections per 100 species decisions", side=1, line=3)
mtext("b",side=3,line=.7,adj=0,font=2,cex=1.2)
mtext("Researchers",side=3,line=.7,adj=.5,font=2)

## c. model-adjusted forester-researcher differences
## #########################################################################

rd <- reason.diff
rd <- rd[c(which(rd$decision=="No"),which(rd$decision=="Yes")),]
yy <- rev(seq_len(nrow(rd)))

xlim.c <- max(abs(c(rd$lower,rd$upper)),na.rm=TRUE)

par(mar=c(6,15,2.5,3), family="sans", las=1, cex.axis=1.4)

plot(NA,xlim=c(-xlim.c,xlim.c),ylim=c(.5,nrow(rd)+.5),
     xlab="", ylab="",yaxt="n",bty="l")

abline(v=0,lty=2,col="grey60")

segments(rd$lower, yy, rd$upper, yy, col=rd$col, lwd=2)
points(rd$difference, yy, pch=15, col=rd$col, cex=2)

axis(2, at=yy,labels=rd$label,tick=FALSE,las=1, cex.axis=1.2)

## emphasize supported contrasts
ii <- rd$p<.05
points(rd$difference[ii],yy[ii],pch=0,col="grey20", cex=2.5, lwd=2)

mtext("c", side=3, line=.7, adj=-1, font=2, cex=1.2)
mtext("Difference between groups", side=3, line=.7,adj=.5,font=2)

mtext("Researcher - forestry practitioner\n(percentage points)",side=1,line=4.5)
mtext("Researchers", side=3, line=-3, outer=T, at=5)


dev.off()



## #########################################################################
##       Reasons selected for native versus non-native species
## #########################################################################

yes.native <- matrix(0, nrow=length(species), ncol=length(yes.reasons), dimnames=list(species, yes.reasons))
no.native <- matrix(0, nrow=length(species), ncol=length(no.reasons), dimnames=list(species, no.reasons))
yes.nonnative <- matrix(0, nrow=length(species), ncol=length(yes.reasons), dimnames=list(species, yes.reasons))
no.nonnative <- matrix(0, nrow=length(species), ncol=length(no.reasons), dimnames=list(species, no.reasons))

country.row <- match(dat.choice$country, nativeness$Country)

for(sp in species) {

    sp.code <- sub(" ", "_", sp, fixed=TRUE)
    choice.var <- paste0("choice_", sp.code)

    is.native <- nativeness[country.row, sp] == 1
    is.nonnative <- nativeness[country.row, sp] == 0
    has.choice <- !is.na(dat.choice[[choice.var]])

    N.native <- sum(has.choice & is.native, na.rm=TRUE)
    N.nonnative <- sum(has.choice & is.nonnative, na.rm=TRUE)

    for(r in yes.reasons) {
        v <- paste0("why_yes_", sp.code, "_", r, "_bin")
        if(v %in% names(dat.choice) && N.native > 0) yes.native[sp, r] <- 100 * sum(dat.choice[[v]] == 1 & has.choice & is.native, na.rm=TRUE) / N.native
        if(v %in% names(dat.choice) && N.nonnative > 0) yes.nonnative[sp, r] <- 100 * sum(dat.choice[[v]] == 1 & has.choice & is.nonnative, na.rm=TRUE) / N.nonnative
    }

    for(r in no.reasons) {
        v <- paste0("why_no_", sp.code, "_", r, "_bin")
        if(v %in% names(dat.choice) && N.native > 0) no.native[sp, r] <- 100 * sum(dat.choice[[v]] == 1 & has.choice & is.native, na.rm=TRUE) / N.native
        if(v %in% names(dat.choice) && N.nonnative > 0) no.nonnative[sp, r] <- 100 * sum(dat.choice[[v]] == 1 & has.choice & is.nonnative, na.rm=TRUE) / N.nonnative
    }
}

## same species order as forester/researcher figure
yes.native <- yes.native[plot.order, , drop=FALSE]
no.native <- no.native[plot.order, , drop=FALSE]
yes.nonnative <- yes.nonnative[plot.order, , drop=FALSE]
no.nonnative <- no.nonnative[plot.order, , drop=FALSE]
species.lab <- plot.order

## common y positions and common x scale
bp <- barplot(t(yes.native), horiz=TRUE, beside=FALSE, plot=FALSE, space=.2)
ymax <- max(bp) + .7

yesmax <- max(c(rowSums(yes.native), rowSums(yes.nonnative)), na.rm=TRUE)
nomax <- max(c(rowSums(no.native), rowSums(no.nonnative)), na.rm=TRUE)
ticks <- pretty(c(-nomax, yesmax))

## Plot
## ######

pdf("figures/Fig5_species_reasons_nativeness.pdf", width=10.5, height=7)

layout(matrix(1:3, nrow=1), widths=c(.55, 1.2, 1.2))

## species names
par(mar=c(6, .5, 2.5, 0), family="sans")
plot.new()
plot.window(xlim=c(0, 1), ylim=c(0, ymax), xaxs="i", yaxs="i")
segments(.98, .4, .98, ymax-.2, col="grey30")
segments(.94, bp, .98, bp, col="grey30")
text(.90, bp, labels=species.lab, adj=1, font=3, cex=1.2)

## a. native
par(mar=c(6, .5, 2.5, .5), family="sans")
barplot(t(yes.native), horiz=TRUE, beside=FALSE, col=yes.col, border=NA, xlim=c(-nomax, yesmax), ylim=c(0, ymax), yaxs="i", space=.2, xlab="", axes=FALSE, axisnames=FALSE)
barplot(-t(no.native), horiz=TRUE, beside=FALSE, col=no.col, border=NA, ylim=c(0, ymax), yaxs="i", add=TRUE, axes=FALSE, axisnames=FALSE, space=.2)
abline(v=0, col="grey30", lwd=1)
axis(1, at=ticks, labels=abs(ticks), cex.axis=1.4)
mtext("Reason selections per 100 species decisions", side=1, line=3)
mtext("a", side=3, line=.7, adj=0, font=2, cex=1.2)
mtext("Native", side=3, line=.7, adj=.5, font=2)

## b. non-native
par(mar=c(6, .5, 2.5, .5), family="sans")
barplot(t(yes.nonnative), horiz=TRUE, beside=FALSE, col=yes.col, border=NA, xlim=c(-nomax, yesmax), ylim=c(0, ymax), yaxs="i", space=.2, xlab="", axes=FALSE, axisnames=FALSE)
barplot(-t(no.nonnative), horiz=TRUE, beside=FALSE, col=no.col, border=NA, ylim=c(0, ymax), yaxs="i", add=TRUE, axes=FALSE, axisnames=FALSE, space=.2)
abline(v=0, col="grey30", lwd=1)
axis(1, at=ticks, labels=abs(ticks), cex.axis=1.4)
mtext("Reason selections per 100 species decisions", side=1, line=3)
mtext("b", side=3, line=.7, adj=0, font=2, cex=1.2)
mtext("Non-native", side=3, line=.7, adj=.5, font=2)

legend("bottomright", title="Risks", legend=no.lab, fill=no.col, border=NA, bty="n", cex=1.2, inset=c(0, 0.25))
legend("bottomright", title="Benefits", legend=yes.lab, fill=yes.col, border=NA, bty="n", cex=1.2, inset=c(0.025, 0))

dev.off()
