## #######################################################################

## TAMERS survey data analysis -- Katalin Csillery -- 8 Sept 2026

## 8_models.r -- acceptance scores (planter/nativer) and models

## ########################################################################
library(pals)
library(RColorBrewer)
library(asreml)
library(maps)

load("data/dat_all_choice.RData")
source("functions.r")

## simplified covariate levels for modelling (rare categories merged)
## #########################################################################

## mygarden as numeric
dat.choice$mygarden <- as.numeric(dat.choice$mygarden)

## age as numeric score
dat.choice$age <- factor(dat.choice$age) ## drop <20 level
dat.choice$age <- ordered(dat.choice$age)
dat.choice$age.score <- as.numeric(dat.choice$age)

## degree simplified
dat.choice$degree2 <- as.character(dat.choice$degree)
dat.choice$degree2[dat.choice$degree2 %in% c("Mandatory school", "Technical training")] <- "Mandatory school / technical training"
dat.choice$degree2 <- ordered(factor(dat.choice$degree2))
dat.choice$degree2.num <- as.numeric(dat.choice$degree2)

## role simplified
dat.choice$detailed.role2 <- as.character(dat.choice$detailed.role)
dat.choice$detailed.role2[dat.choice$detailed.role2=="Forest owner and manager"] <- "Forest owner"
dat.choice$detailed.role2[grepl("Researcher", dat.choice$detailed.role2)] <- "Researcher"

## years of experience
dat.choice$years.exper <- ordered(dat.choice$years.exper)
dat.choice$years.exper.num <- as.numeric(dat.choice$years.exper)

## include researchers in forester who answered the experience and knowledge
table(dat.choice$occupation.model, exclude = NULL) 
##   forester researcher 
## 525        131
is.forester.model <- dat.choice$occupation.model == "forester"

## reduce countries to regions and filter based on response counts per region
## #########################################################################
dat.choice$region <- recode.group(dat.choice$country, how="minimalist")

native.prop.tab <- read.csv("data/Region_minimalist_native_species_proportion.csv")
dat.choice$native.prop <- native.prop.tab$native.prop[match(dat.choice$region, native.prop.tab$Country)]

min.region.n <- 3
region.tab <- table(dat.choice$region[is.forester.model])
keep.region.forester <- names(region.tab[region.tab >= min.region.n])
region.tab.r <- table(dat.choice$region[!is.forester.model])
keep.region.researcher <- names(region.tab.r[region.tab.r >= min.region.n])

dat.forester <- subset(dat.choice, is.forester.model & region%in%keep.region.forester)
dim(dat.forester)
##[1] 520 824

## PCA and correlation between response variables
## ##########################################

expl.vars <- c(
    "years.exper.num",
    "degree2.num",
    "cc.damage.experience",
    "action.strategy.score",
    "barrier.orientation",
    "ecoevo.knowledge.score",
    "infosource.level",
    "knowledge.transfer.score",
    "goal.preference",
    "policy.urgency",
    "support.philosophy",
    "mygarden"
)

## complete cases
X <- dat.forester[, expl.vars]
keep <- complete.cases(X)
X <- X[keep, ]

dat.forester$region.fac <- factor(dat.forester$region)
region <- droplevels(dat.forester$region.fac[keep])

## PCA
pca <- prcomp(X, center = TRUE, scale. = TRUE)
var.exp <- pca$sdev^2 / sum(pca$sdev^2)
summary(pca)
round(pca$rotation, 2)

pdf("figures/FigSX_PCA_scores.pdf", width = 10, height = 10)
par(mfcol=c(2,2))
cols <- pals::glasbey(17)

plot(pca, type="b", pch=16, main="Scree plot")

plot(pca$x[,1], pca$x[,2], pch=16, col=cols[region], xlab=paste0("PC1 (",round(100*summary(pca)$importance[2,1],1),"%)"), ylab=paste0("PC2 (",round(100*summary(pca)$importance[2,2],1),"%)"), main="Individuals"); abline(h=0,v=0,lty=3,col="grey70"); legend("topright", legend=levels(region), col=cols, pch=16, bty="n", cex=.7)

plot(c(-1.1,1.1), c(-1.1,1.1), type="n", asp=1, xlab="PC1", ylab="PC2", main="Variables"); symbols(0,0,circles=1,inches=FALSE,add=TRUE); arrows(0,0,pca$rotation[,1],pca$rotation[,2],length=.08); text(pca$rotation[,1],pca$rotation[,2],labels=rownames(pca$rotation),cex=.7,pos=4); abline(h=0,v=0,lty=3,col="grey70")

corrplot::corrplot(cor(X), method="ellipse", type="upper", tl.cex=.7)

dev.off()

## #####################################################################
## Standardized MANOVA-equivalent via ASReml-R (region as random effect)
## #####################################################################
library(asreml)

## substantive explanatory constructs
construct.vars <- c(
    "cc.damage.experience",
    "action.strategy.score",
    "barrier.orientation",
    "ecoevo.knowledge.score",
    "infosource.level",
    "knowledge.transfer.score",
    "goal.preference",
    "policy.urgency",
    "support.philosophy")

## adjustment covariates
covar.vars <- c(
    "native.prop",
    "years.exper.num",
    "degree2.num",
    "mygarden")

expl.vars <- c(covar.vars, construct.vars)
model.vars <- c("planter", "nativer", "region", expl.vars)
dat.manova <- dat.forester[complete.cases(dat.forester[, model.vars]), model.vars]

scale.vars <- c(
    "planter",
    "nativer",
    "native.prop",
    "years.exper.num",
    "degree2.num",
    "cc.damage.experience",
    "action.strategy.score",
    "barrier.orientation",
    "ecoevo.knowledge.score",
    "infosource.level",
    "knowledge.transfer.score",
    "goal.preference",
    "policy.urgency",
    "support.philosophy"
)

dat.manova[, scale.vars] <- lapply(dat.manova[, scale.vars], function(x) as.numeric(scale(x, TRUE, TRUE)))
dat.manova$region <- factor(dat.manova$region)

fe.terms <- paste("trait:", expl.vars, sep = "", collapse = " + ")
fe.form <- paste("cbind(planter, nativer) ~ trait +", fe.terms)
fit.asr <- asreml(
    fixed    = as.formula(fe.form),
    random   = ~ us(trait):region,
    residual = ~ units:us(trait),
    data     = dat.manova,
    maxit    = 500)

## check convergence
fit.asr <- update.asreml(fit.asr)   # re-run if not converged; repeat as needed

summary(fit.asr)$varcomp
wald.asreml(fit.asr, denDF = "numeric", ssType = "conditional")

## #####################################################################
## Step 1: coefficients + individual SEs
## #####################################################################

coef.tab <- summary(fit.asr, coef = TRUE)$coef.fixed
cf <- coef.tab[, "solution"]
se <- coef.tab[, "std error"]
nm <- rownames(coef.tab)

build.vec.df <- function(var) {

    p.name <- paste0("trait_planter:", var)
    n.name <- paste0("trait_nativer:", var)

    if (!(p.name %in% nm) || !(n.name %in% nm))
        return(NULL)

    data.frame(
        var     = var,
        planter = cf[p.name],
        nativer = cf[n.name],
        se.p    = se[p.name],
        se.n    = se[n.name]
    )
}

## extract significance of covariates for Table 2
## ###############################################

covar.vars <- c("native.prop", "years.exper.num", "degree2.num", "mygarden")

covar.df <- do.call(rbind, lapply(covar.vars, build.vec.df))
rownames(covar.df) <- NULL

covar.df <- merge(covar.df,
                  wald.tab[, c("var", "Pr")],
                  by = "var", sort = FALSE)

covar.df <- covar.df[match(covar.vars, covar.df$var), ]

covar.df$z.p <- covar.df$planter / covar.df$se.p
covar.df$z.n <- covar.df$nativer / covar.df$se.n

covar.df$p.p <- 2 * pnorm(-abs(covar.df$z.p))
covar.df$p.n <- 2 * pnorm(-abs(covar.df$z.n))

covar.df


## extract data for Figure 3: substantive constructs only
## ################################################

## Adjustment covariates, including mygarden, remain in fit.asr
## but are not displayed as constructs.
vec.df <- do.call(rbind, lapply(construct.vars, build.vec.df))
rownames(vec.df) <- NULL

## attach joint significance from Wald table
wald.tab <- wald.asreml(
    fit.asr,
    denDF = "numeric",
    ssType = "conditional"
)$Wald

wald.tab$var <- sub("^trait:", "", rownames(wald.tab))

vec.df <- merge(
    vec.df,
    wald.tab[, c("var", "Pr")],
    by = "var",
    sort = FALSE
)


## create second part of Table 2 with construct effects
## #######################################################
construct.tab <- vec.df[, c("var", "planter", "se.p", "nativer", "se.n", "Pr")]

construct.tab$p.p <- 2 * pnorm(-abs(construct.tab$planter / construct.tab$se.p))
construct.tab$p.n <- 2 * pnorm(-abs(construct.tab$nativer / construct.tab$se.n))
construct.tab$group <- "Theory-driven constructs"

covar.tab <- covar.df[, c("var", "planter", "se.p", "nativer", "se.n", "Pr", "p.p", "p.n")]
covar.tab$group <- "Adjustment covariates"

effects.tab <- rbind(
    construct.tab[, c("group", "var", "planter", "se.p", "p.p",
                      "nativer", "se.n", "p.n", "Pr")],
    covar.tab[, c("group", "var", "planter", "se.p", "p.p",
                  "nativer", "se.n", "p.n", "Pr")]
)

effects.tab

## restore construct ordering after merge
vec.df <- vec.df[match(construct.vars, vec.df$var), ]

vec.df$sig <- vec.df$Pr < 0.05

construct.labels <- c(
    "cc.damage.experience"      = "Climate-change\ndisturbance experience",
    "action.strategy.score"     = "Adaptive management strategy",
    "barrier.orientation"       = "Barrier orientation",
    "ecoevo.knowledge.score"    = "Eco-evolutionary knowledge",
    "infosource.level"          = "Information-source level",
    "knowledge.transfer.score"  = "Knowledge-transfer preference",
    "goal.preference"           = "Management goal preference",
    "policy.urgency"            = "Policy urgency",
    "support.philosophy"        = "Support philosophy"
)

vec.df$label <- construct.labels[vec.df$var]

## #####################################################################
## Step 2: 2D effect-vector plot with crosses
## #####################################################################

## joint Wald significance
vec.df$sig.joint <- vec.df$Pr < 0.05

## outcome-specific significance
vec.df$z.p <- vec.df$planter / vec.df$se.p
vec.df$z.n <- vec.df$nativer / vec.df$se.n

vec.df$sig.p <- abs(vec.df$z.p) > 1.96
vec.df$sig.n <- abs(vec.df$z.n) > 1.96

## blue if significant in either outcome OR in the joint test
vec.df$sig.any <- vec.df$sig.joint | vec.df$sig.p | vec.df$sig.n


## #####################################################################
## Step 3: Extract adjusted country effects (BLUPs) from the model
## #####################################################################
ranef <- fit.asr$coefficients$random[, 1]
rn <- rownames(fit.asr$coefficients$random)

is.planter <- grepl("trait_planter", rn)
is.nativer <- grepl("trait_nativer", rn)

extract.region <- function(x) sub(".*region_", "", x)

blup.df <- data.frame(
    region  = extract.region(rn),
    trait   = ifelse(is.planter, "planter", ifelse(is.nativer, "nativer", NA)),
    value   = as.numeric(ranef),
    stringsAsFactors = FALSE
)
blup.df <- blup.df[!is.na(blup.df$trait), ]

region.map <- reshape(
    blup.df,
    idvar     = "region",
    timevar   = "trait",
    direction = "wide"
)
names(region.map) <- sub("value\\.", "", names(region.map))
names(region.map)[names(region.map) == "region"] <- "Country"

region.map

## Expand grouped regions to individual countries (unchanged from yours)
## #####################################################################
country.groups <- country.groups.minimalist

expand.region <- function(region.name) {
  if(region.name %in% names(country.groups)) country.groups[[region.name]] else region.name
}
country.map <- do.call(rbind, lapply(seq_len(nrow(region.map)), function(i) {
  countries <- expand.region(region.map$Country[i])
  data.frame(Country = countries, planter = region.map$planter[i], nativer = region.map$nativer[i],
             stringsAsFactors = FALSE)
}))

## name mismatches with the `maps` package
country.map$map.name <- country.map$Country
country.map$map.name[country.map$map.name == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
country.map$map.name[country.map$map.name == "Czechia"] <- "Czech Republic"
country.map$map.name[country.map$map.name == "United Kingdom"] <- "UK:Great Britain"

## #####################################################################
## extract regional BLUPs and approximate 95% significance against zero
## #####################################################################
ranef.tab <- summary(fit.asr, coef = TRUE)$coef.random
rn <- rownames(ranef.tab)

blup.df <- data.frame(region = sub(".*region_", "", rn),
                      trait = ifelse(grepl("trait_planter", rn), "planter", ifelse(grepl("trait_nativer", rn), "nativer", NA)),
                      value = ranef.tab[, "solution"],
                      se = ranef.tab[, "std.error"],
                      z.ratio = ranef.tab[, "z.ratio"], stringsAsFactors = FALSE)
blup.df <- blup.df[!is.na(blup.df$trait), ]
blup.df$sig <- abs(blup.df$z.ratio) > 2

region.value <- reshape(blup.df[, c("region", "trait", "value")], idvar = "region", timevar = "trait", direction = "wide")
region.sig <- reshape(blup.df[, c("region", "trait", "sig")], idvar = "region", timevar = "trait", direction = "wide")
region.map <- merge(region.value, region.sig, by = "region")
names(region.map) <- sub("^value\\.", "", names(region.map))
names(region.map) <- sub("^sig\\.", "sig.", names(region.map))
names(region.map)[names(region.map) == "region"] <- "Country"

country.map <- do.call(rbind, lapply(seq_len(nrow(region.map)), function(i){
    countries <- expand.region(region.map$Country[i])
    data.frame(Country = countries, planter = region.map$planter[i], nativer = region.map$nativer[i], sig.planter = region.map$sig.planter[i], sig.nativer = region.map$sig.nativer[i], stringsAsFactors = FALSE)
}))

country.map$map.name <- country.map$Country
country.map$map.name[country.map$map.name == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
country.map$map.name[country.map$map.name == "Czechia"] <- "Czech Republic"
country.map$map.name[country.map$map.name == "United Kingdom"] <- "UK:Great Britain"

table(country.map$sig.planter, useNA = "ifany")
table(country.map$sig.nativer, useNA = "ifany")


save(vec.df, blup.df, country.map, file="data/constructs_blups.RData")
