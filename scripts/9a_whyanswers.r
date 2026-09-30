## #######################################################################

## TAMERS survey data analysis -- Katalin Csillery -- 9 Sept 2026

## 9 looking into the reasons why respondent preferred or not a species

## ########################################################################
library(pals)
library(RColorBrewer)

load("data/dat_all_choice.RData")
source("functions.r")


## identify all yes/no reason columns
yes.cols <- grep("^why_yes_.*_bin$", names(dat.choice), value = TRUE)
no.cols  <- grep("^why_no_.*_bin$", names(dat.choice), value = TRUE)

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
nativeness <- read.csv("data/Forest_species_nativeness_byCountry.csv",
                       check.names = FALSE)
species <- names(nativeness)[names(nativeness) != "Country"]
length(species)
## 30

## how many respondents selected each reason for each species
yes.mat <- matrix(0, nrow = length(species), ncol = length(yes.reasons),
                  dimnames = list(species, yes.reasons))

no.mat <- matrix(0, nrow = length(species), ncol = length(no.reasons),
                 dimnames = list(species, no.reasons))

for(sp in species){
    for(r in yes.reasons){
        v <- paste0("why_yes_", sub(" ", "_", sp), "_", r, "_bin")
        if(v %in% names(dat.choice))
            yes.mat[sp, r] <- sum(dat.choice[[v]] == 1, na.rm = TRUE)
    }
    for(r in no.reasons){
        v <- paste0("why_no_", sub(" ", "_", sp), "_", r, "_bin")
        if(v %in% names(dat.choice))
            no.mat[sp, r] <- sum(dat.choice[[v]] == 1, na.rm = TRUE)
    }
}


## prettier labels
species.lab <- gsub("_", " ", species)

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

## colour families: warm for NO, cool for YES

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

## order species, for example by total number of YES selections
ord <- order(rowSums(yes.mat))
yes.plot <- yes.mat[ord, , drop = FALSE]
no.plot  <- no.mat[ord, , drop = FALSE]
species.lab <- species.lab[ord]

## same absolute scale on both sides
yesmax <- max(rowSums(yes.plot))
nomax <- max(rowSums(no.plot)) * 2.5

pdf("figures/Fig5_species_reasons.pdf", width = 12, height = 8)

par(mar = c(5, 12, 2, 2), family = "sans")

bp <- barplot(t(yes.plot),
              horiz = TRUE,
              beside = FALSE,
              col = yes.col,
              border = NA,
              xlim = c(-nomax, yesmax),
                           las = 1,
              xlab = "Number of responses",
              axes = FALSE)

## NO bars: negative values, on exactly the same y positions
barplot(-t(no.plot),
        horiz = TRUE,
        beside = FALSE,
        col = no.col,
        border = NA,
        add = TRUE,
        axes = FALSE, axisnames = F)

abline(v = 0, col = "grey30", lwd = 1)

ticks <- pretty(c(-nomax, yesmax))
axis(1, at = ticks, labels = abs(ticks))

legend("topleft", title="No reasons", cex=1,
       legend = no.lab,
       fill = no.col,
       border = NA,
       bty = "n",
       xpd = TRUE)

legend("bottomright", title="Yes reasons", cex=1,
       legend = yes.lab,
       fill = yes.col,
       border = NA,
       bty = "n",
       xpd = TRUE)

dev.off()



## ########################################################
##              account for nativeness
## ########################################################

## Matrices: NATIVE
yes.native <- matrix(0,
                     nrow = length(species),
                     ncol = length(yes.reasons) + 1,
                     dimnames = list(species,
                                     c(yes.reasons, "Not_defined")))

no.native <- matrix(0,
                    nrow = length(species),
                    ncol = length(no.reasons) + 1,
                    dimnames = list(species,
                                    c(no.reasons, "Not_defined")))


## Matrices: NON-NATIVE
yes.nonnative <- matrix(0,
                        nrow = length(species),
                        ncol = length(yes.reasons) + 1,
                        dimnames = list(species,
                                        c(yes.reasons, "Not_defined")))

no.nonnative <- matrix(0,
                       nrow = length(species),
                       ncol = length(no.reasons) + 1,
                       dimnames = list(species,
                                       c(no.reasons, "Not_defined")))


## Match respondent country to nativeness table
country.row <- match(dat.choice$country, nativeness$Country)


for(sp in species){

    ## Native status of this species for every respondent
    is.native <- nativeness[country.row, sp] == 1
    is.nonnative <- nativeness[country.row, sp] == 0

    ## YES reasons
    ## ################
    v.yes <- paste0("why_yes_",
                    sub(" ", "_", sp),
                    "_",
                    yes.reasons,
                    "_bin")

    v.yes <- v.yes[v.yes %in% names(dat.choice)]

    tmp <- dat.choice[, v.yes, drop = FALSE]

    ## counts for individual reasons
    for(r in yes.reasons){

        v <- paste0("why_yes_",
                    sub(" ", "_", sp),
                    "_",
                    r,
                    "_bin")

        if(v %in% names(dat.choice)){

            yes.native[sp, r] <-
                sum(dat.choice[[v]] == 1 & is.native,
                    na.rm = TRUE)

            yes.nonnative[sp, r] <-
                sum(dat.choice[[v]] == 1 & is.nonnative,
                    na.rm = TRUE)
        }
    }

    ## Not defined
    eligible <- rowSums(!is.na(tmp)) > 0
    n.selected <- rowSums(tmp == 1, na.rm = TRUE)

    yes.native[sp, "Not_defined"] <-
        sum(eligible & n.selected == 0 & is.native,
            na.rm = TRUE)

    yes.nonnative[sp, "Not_defined"] <-
        sum(eligible & n.selected == 0 & is.nonnative,
            na.rm = TRUE)

    ## NO reasons
    ## ####################
    v.no <- paste0("why_no_",
                   sub(" ", "_", sp),
                   "_",
                   no.reasons,
                   "_bin")

    v.no <- v.no[v.no %in% names(dat.choice)]

    tmp <- dat.choice[, v.no, drop = FALSE]

    ## counts for individual reasons
    for(r in no.reasons){

        v <- paste0("why_no_",
                    sub(" ", "_", sp),
                    "_",
                    r,
                    "_bin")

        if(v %in% names(dat.choice)){

            no.native[sp, r] <-
                sum(dat.choice[[v]] == 1 & is.native,
                    na.rm = TRUE)

            no.nonnative[sp, r] <-
                sum(dat.choice[[v]] == 1 & is.nonnative,
                    na.rm = TRUE)
        }
    }

    ## Not defined
    eligible <- rowSums(!is.na(tmp)) > 0
    n.selected <- rowSums(tmp == 1, na.rm = TRUE)

    no.native[sp, "Not_defined"] <-
        sum(eligible & n.selected == 0 & is.native,
            na.rm = TRUE)

    no.nonnative[sp, "Not_defined"] <-
        sum(eligible & n.selected == 0 & is.nonnative,
            na.rm = TRUE)
}

yes.native.plot <- yes.native[ord, , drop = FALSE]
no.native.plot <- no.native[ord, , drop = FALSE]

yes.nonnative.plot <- yes.nonnative[ord, , drop = FALSE]
no.nonnative.plot <- no.nonnative[ord, , drop = FALSE]

## drop Not defined columns
yes.native.plot <- yes.native.plot[,1:ncol(yes.plot)]
no.native.plot <- no.native.plot[,1:ncol(no.plot)]
yes.nonnative.plot <- yes.nonnative.plot[,1:ncol(yes.plot)]
no.nonnative.plot <- no.nonnative.plot[,1:ncol(no.plot)]


pdf("figures/Fig5_species_reasons_nativeness.pdf", width = 12, height = 16)

par(mar = c(5, 12, 2, 2), family = "sans", mfcol=c(2,1))

bp <- barplot(t(yes.native.plot),
              horiz = TRUE,
              beside = FALSE,
              col = yes.col,
              border = NA,
              xlim = c(-nomax, yesmax),
                           las = 1,
              xlab = "Number of responses",
              axes = FALSE)

## NO bars: negative values, on exactly the same y positions
barplot(-t(no.native.plot),
        horiz = TRUE,
        beside = FALSE,
        col = no.col,
        border = NA,
        add = TRUE,
        axes = FALSE, axisnames = F)

abline(v = 0, col = "grey30", lwd = 1)

ticks <- pretty(c(-nomax, yesmax))
axis(1, at = ticks, labels = abs(ticks))

legend("topleft", title="No reasons", cex=1,
       legend = no.lab,
       fill = no.col,
       border = NA,
       bty = "n",
       xpd = TRUE)

legend("bottomright", title="Yes reasons", cex=1,
       legend = yes.lab,
       fill = yes.col,
       border = NA,
       bty = "n",
       xpd = TRUE)

## ###########################

bp <- barplot(t(yes.nonnative.plot),
              horiz = TRUE,
              beside = FALSE,
              col = yes.col,
              border = NA,
              xlim = c(-nomax, yesmax),
                           las = 1,
              xlab = "Number of responses",
              axes = FALSE)

## NO bars: negative values, on exactly the same y positions
barplot(-t(no.nonnative.plot),
        horiz = TRUE,
        beside = FALSE,
        col = no.col,
        border = NA,
        add = TRUE,
        axes = FALSE, axisnames = F)

abline(v = 0, col = "grey30", lwd = 1)

ticks <- pretty(c(-nomax, yesmax))
axis(1, at = ticks, labels = abs(ticks))

legend("topleft", title="No reasons", cex=1,
       legend = no.lab,
       fill = no.col,
       border = NA,
       bty = "n",
       xpd = TRUE)

legend("bottomright", title="Yes reasons", cex=1,
       legend = yes.lab,
       fill = yes.col,
       border = NA,
       bty = "n",
       xpd = TRUE)

dev.off()


## ################################################################

##            why selected those yes/no answers?

## ################################################################

## number of reasons selected for each respondent x species decision
## #########################################################################

yes.answers <- c("Yes in pure stocks","Yes in mixed stocks","Yes, but sporadically")

resp.id <- if("Response_ID" %in% names(dat.choice)) dat.choice$Response_ID else rownames(dat.choice)

reason.decision <- list()
k <- 1

for(sp in species){

    sp.code <- sub(" ","_",sp,fixed=TRUE)
    choice.var <- paste0("choice_",sp.code)

    if(!choice.var %in% names(dat.choice)) next

    ## YES reasons
    yes.vars <- paste0("why_yes_",sp.code,"_",yes.reasons,"_bin")
    yes.vars <- yes.vars[yes.vars %in% names(dat.choice)]

    ii <- which(dat.choice[[choice.var]] %in% yes.answers)

    if(length(ii) > 0 && length(yes.vars) > 0){
        nr <- rowSums(dat.choice[ii,yes.vars,drop=FALSE] == 1,na.rm=TRUE)

        reason.decision[[k]] <- data.frame(
            Response_ID = resp.id[ii],
            species = sp,
            decision = "Yes",
            n.reason = nr,
            stringsAsFactors = FALSE
        )
        k <- k + 1
    }

    ## NO reasons
    no.vars <- paste0("why_no_",sp.code,"_",no.reasons,"_bin")
    no.vars <- no.vars[no.vars %in% names(dat.choice)]

    ii <- which(dat.choice[[choice.var]] == "No")

    if(length(ii) > 0 && length(no.vars) > 0){
        nr <- rowSums(dat.choice[ii,no.vars,drop=FALSE] == 1,na.rm=TRUE)

        reason.decision[[k]] <- data.frame(
            Response_ID = resp.id[ii],
            species = sp,
            decision = "No",
            n.reason = nr,
            stringsAsFactors = FALSE
        )
        k <- k + 1
    }
}

reason.decision <- do.call(rbind,reason.decision)


table(reason.decision$decision,reason.decision$n.reason)

round(100 * prop.table(table(reason.decision$decision,reason.decision$n.reason),1),1)


aggregate(n.reason ~ decision,data=reason.decision,function(x)
    c(n=length(x),mean=mean(x),median=median(x),q25=quantile(x,.25),q75=quantile(x,.75),
      prop.multiple=mean(x >= 2),prop.3plus=mean(x >= 3)))

## by species
## ##############
yes.dec <- subset(reason.decision,decision=="Yes")
no.dec  <- subset(reason.decision,decision=="No")

yes.by.species <- do.call(rbind,lapply(split(yes.dec,yes.dec$species),function(d)
    data.frame(species=d$species[1],n=nrow(d),mean=mean(d$n.reason),median=median(d$n.reason),
               prop.multiple=mean(d$n.reason >= 2),prop.3plus=mean(d$n.reason >= 3))))

no.by.species <- do.call(rbind,lapply(split(no.dec,no.dec$species),function(d)
    data.frame(species=d$species[1],n=nrow(d),mean=mean(d$n.reason),median=median(d$n.reason),
               prop.multiple=mean(d$n.reason >= 2),prop.3plus=mean(d$n.reason >= 3))))

yes.by.species <- yes.by.species[order(-yes.by.species$mean),]
no.by.species <- no.by.species[order(-no.by.species$mean),]

yes.by.species
no.by.species


## Check whether a subset of respondents are habitual “multi-tickers”

yes.by.respondent <- do.call(rbind,lapply(split(yes.dec,yes.dec$Response_ID),function(d)
    data.frame(Response_ID=d$Response_ID[1],n.accepted=nrow(d),
               mean.reasons=mean(d$n.reason),median.reasons=median(d$n.reason),
               prop.multiple=mean(d$n.reason >= 2))))

no.by.respondent <- do.call(rbind,lapply(split(no.dec,no.dec$Response_ID),function(d)
    data.frame(Response_ID=d$Response_ID[1],n.rejected=nrow(d),
               mean.reasons=mean(d$n.reason),median.reasons=median(d$n.reason),
               prop.multiple=mean(d$n.reason >= 2))))

summary(yes.by.respondent$mean.reasons)
summary(yes.by.respondent$prop.multiple)

hist(yes.by.respondent$mean.reasons,breaks=20,xlab="Mean number of reasons per accepted species",main="")


plot(yes.by.respondent$n.accepted,yes.by.respondent$mean.reasons,pch=16,col="grey60",
     xlab="Number of species accepted",ylab="Mean number of reasons selected per accepted species")


## no evidence for multi-tickers!!


## Chi2 test across countries for reasons
## #########################################

## add country/group to respondent x species data
country.id <- data.frame(Response_ID=resp.id, Country=dat.choice$Country, stringsAsFactors=FALSE)
country.id$Country.group2 <- recode.group(country.id$Country, how="minimalist")

reason.decision <- merge(reason.decision, country.id[,c("Response_ID","Country.group2")], by="Response_ID", all.x=TRUE)

## indicators
reason.decision$any.reason <- reason.decision$n.reason >= 1
reason.decision$multiple.reasons <- reason.decision$n.reason >= 2

chi.country <- function(decision, variable){

    d <- reason.decision[reason.decision$decision==decision & !is.na(reason.decision$Country.group2),]

    tab <- table(d$Country.group2, factor(d[[variable]], levels=c(FALSE,TRUE)))

    ## remove very small groups if necessary
    tab <- tab[rowSums(tab) >= 5,,drop=FALSE]

    test <- chisq.test(tab)

    cat("\n",decision,"-",variable,"\n")
    print(tab)
    print(round(prop.table(tab,1)*100,1))
    print(test)

    cat("\nMinimum expected count:",min(test$expected),"\n")

    V <- sqrt(as.numeric(test$statistic)/(sum(tab)*(min(dim(tab))-1)))
    cat("Cramer's V:",round(V,3),"\n")

    invisible(list(table=tab,test=test,V=V))
}

yes.any <- chi.country("Yes","any.reason")
no.any  <- chi.country("No","any.reason")

yes.mult <- chi.country("Yes","multiple.reasons")
no.mult  <- chi.country("No","multiple.reasons")

country.reason.summary <- aggregate(
    cbind(any.reason,multiple.reasons) ~ Country.group2 + decision,
    data=reason.decision,
    FUN=mean
)

country.reason.summary$any.reason <- round(100*country.reason.summary$any.reason,1)
country.reason.summary$multiple.reasons <- round(100*country.reason.summary$multiple.reasons,1)

country.reason.summary

## run a mixed model
## ###########################

library(lme4)

## make sure variables are factors
reason.decision$Response_ID <- factor(reason.decision$Response_ID)
reason.decision$species <- factor(reason.decision$species)
reason.decision$Country.group2 <- factor(reason.decision$Country.group2)

reason.decision$any.reason <- as.integer(reason.decision$n.reason >= 1)
reason.decision$multiple.reasons <- as.integer(reason.decision$n.reason >= 2)

fit.reason.glmm <- function(decision, variable){

    d <- droplevels(reason.decision[
        reason.decision$decision == decision &
        !is.na(reason.decision$Country.group2), ])

    form.full <- as.formula(
        paste0(variable," ~ Country.group2 + (1|Response_ID) + (1|species)")
    )

    form.null <- as.formula(
        paste0(variable," ~ 1 + (1|Response_ID) + (1|species)")
    )

    full <- glmer(form.full, data=d, family=binomial,
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5)))

    null <- glmer(form.null, data=d, family=binomial,
                  control=glmerControl(optimizer="bobyqa",
                                       optCtrl=list(maxfun=2e5)))

    cat("\n\n",decision,"-",variable,"\n")
    print(anova(null,full,test="Chisq"))
    cat("\nRandom effects:\n")
    print(VarCorr(full))
    cat("\nSingular:",isSingular(full),"\n")

    list(full=full,null=null,data=d)
}


yes.any.glmm  <- fit.reason.glmm("Yes","any.reason")
no.any.glmm   <- fit.reason.glmm("No","any.reason")

yes.mult.glmm <- fit.reason.glmm("Yes","multiple.reasons")
no.mult.glmm  <- fit.reason.glmm("No","multiple.reasons")


## disease-related reasons among NO decisions
## #########################################################################

no.disease <- list()
k <- 1

for(sp in species){

    sp.code <- sub(" ","_",sp,fixed=TRUE)
    choice.var <- paste0("choice_",sp.code)

    if(!choice.var %in% names(dat.choice)) next

    ii <- which(dat.choice[[choice.var]] == "No")
    if(length(ii) == 0) next

    disease.var <- paste0("why_no_",sp.code,"_Risk_to_be_affected_by_a_disease_bin")
    pest.var <- paste0("why_no_",sp.code,"_Importing_pests_and_pathogens_bin")

    disease <- if(disease.var %in% names(dat.choice)) dat.choice[[disease.var]][ii] else NA
    pest <- if(pest.var %in% names(dat.choice)) dat.choice[[pest.var]][ii] else NA

    no.disease[[k]] <- data.frame(
        Response_ID = resp.id[ii],
        species = sp,
        Country = dat.choice$Country[ii],
        disease = as.integer(disease == 1),
        pests_pathogens = as.integer(pest == 1),
        stringsAsFactors = FALSE
    )

    k <- k + 1
}

no.disease <- do.call(rbind,no.disease)

no.disease$Country.group2 <- recode.group(no.disease$Country, how="minimalist")
no.disease$Response_ID <- factor(no.disease$Response_ID)
no.disease$species <- factor(no.disease$species)
no.disease$Country.group2 <- factor(no.disease$Country.group2)

disease.country <- aggregate(disease ~ Country.group2, data=no.disease,
                             FUN=function(x) c(n=length(x),percent=100*mean(x)))

disease.country

pest.country <- aggregate(pests_pathogens ~ Country.group2, data=no.disease,
                          FUN=function(x) c(n=length(x),percent=100*mean(x)))

pest.country

library(lme4)

fit.disease <- glmer(
    disease ~ Country.group2 + (1|Response_ID) + (1|species),
    data=no.disease,
    family=binomial,
    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

fit.disease.null <- glmer(
    disease ~ 1 + (1|Response_ID) + (1|species),
    data=no.disease,
    family=binomial,
    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

anova(fit.disease.null,fit.disease,test="Chisq")

fit.pest <- glmer(
    pests_pathogens ~ Country.group2 + (1|Response_ID) + (1|species),
    data=no.disease,
    family=binomial,
    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

fit.pest.null <- glmer(
    pests_pathogens ~ 1 + (1|Response_ID) + (1|species),
    data=no.disease,
    family=binomial,
    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
)

anova(fit.pest.null,fit.pest,test="Chisq")

library(emmeans)

disease.emm <- emmeans(fit.disease, ~ Country.group2, type="response")
pest.emm <- emmeans(fit.pest, ~ Country.group2, type="response")

disease.emm
pest.emm

disease.df <- as.data.frame(disease.emm)
disease.df <- disease.df[order(-disease.df$prob),]
disease.df

pest.df <- as.data.frame(pest.emm)
pest.df <- pest.df[order(-pest.df$prob),]
pest.df
