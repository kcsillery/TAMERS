## #######################################################################

## TAMERS survey data analysis - Anastazija Dimitrova 9 Sept 2026

## 10_species_model.r -- species-level planting preferences

## ########################################################################
library(asreml)

load("data/dat_all_choice.RData")
source("functions.r")

## species and country-level nativeness
## #########################################################################
nativeness <- read.csv("data/Forest_species_nativeness_byCountry.csv", check.names=FALSE)
species <- names(nativeness)[names(nativeness)!="Country"]
species.cols <- paste0("choice_",gsub(" ","_",species,fixed=TRUE))

## geographical grouping and minimum regional sample size
## based on forestry-practitioner model group, as in 8_models.r
## #########################################################################
dat.choice$region <- recode.group(dat.choice$country, how="minimalist")

is.forester.model <- dat.choice$occupation.model=="forester"

min.region.n <- 3
region.tab <- table(dat.choice$region[is.forester.model])
keep.region <- names(region.tab[region.tab>=min.region.n])

## retain both respondent groups within regions represented by >=3 foresters
## supplementary forester-researcher comparison uses these same regions
dat.species <- subset(dat.choice, region%in%keep.region)

## respondent x species dataset
## #########################################################################
n.respondent <- nrow(dat.species)

species.long <- data.frame(Response_ID=rep(dat.species$Response_ID,times=length(species)),
                           country=rep(dat.species$country,times=length(species)),
                           region=rep(dat.species$region,times=length(species)),
                           occupation.model=rep(dat.species$occupation.model,times=length(species)),
                           species=rep(species,each=n.respondent),
                           original.response=unlist(dat.species[,species.cols,drop=FALSE],use.names=FALSE),
                           stringsAsFactors=FALSE)

yes.answers <- c("Yes in pure stocks","Yes in mixed stocks","Yes, but sporadically")

species.long$acceptance <- NA_integer_
species.long$acceptance[species.long$original.response%in%yes.answers] <- 1L
species.long$acceptance[species.long$original.response=="No"] <- 0L

species.long$response.type <- NA_character_
species.long$response.type[species.long$original.response%in%yes.answers] <- "Accept"
species.long$response.type[species.long$original.response=="No"] <- "Reject"
species.long$response.type[species.long$original.response=="Not applicable for my reference forest"] <- "Not applicable"
species.long$response.type[species.long$original.response=="I don't know"] <- "Don't know"
species.long$response.type[is.na(species.long$original.response)] <- "Missing"

## country-specific nativeness and European origin
## #########################################################################
native.mat <- as.matrix(nativeness[,species,drop=FALSE])
country.ind <- match(species.long$country,nativeness$Country)
species.ind <- match(species.long$species,species)

species.long$native <- native.mat[cbind(country.ind,species.ind)]

native.europe <- colSums(native.mat==1,na.rm=TRUE)>0
names(native.europe) <- species
species.long$native.europe <- native.europe[species.long$species]

species.long$origin.status <- NA_character_
species.long$origin.status[species.long$native==1] <- "Locally native"
species.long$origin.status[species.long$native==0 & species.long$native.europe] <- "European-native, locally non-native"
species.long$origin.status[species.long$native==0 & !species.long$native.europe] <- "Non-European-native"

species.long$origin.status <- factor(species.long$origin.status,
                                     levels=c("Locally native","European-native, locally non-native","Non-European-native"))

## model dataset: yes/no species decisions with known nativeness
## #########################################################################
model.data <- species.long[!is.na(species.long$acceptance) & !is.na(species.long$origin.status) & !is.na(species.long$occupation.model),]

model.data$Response_ID <- factor(model.data$Response_ID)
model.data$country <- factor(model.data$country)
model.data$region <- factor(model.data$region)
model.data$species <- factor(model.data$species,levels=species)
model.data$native <- factor(model.data$native,levels=c(0,1),labels=c("Non-native","Native"))
model.data$occupation.model <- factor(model.data$occupation.model,levels=c("forester","researcher"))

## preserve full model dataset for forester-researcher comparison
## #########################################################################
model.data.all <- model.data
species.long.all <- species.long

## forester-researcher comparison
## #########################################################################
fit.speciesFR <- asreml(fixed=acceptance ~ origin.status * occupation.model,
                        random=~ Response_ID + region + species + region:species,
                        family=asr_binomial(link="logit"),data=model.data.all,maxit=500)

fit.speciesFR <- update.asreml(fit.speciesFR)
wald.speciesFR <- wald.asreml(fit.speciesFR,denDF="numeric",ssType="conditional")$Wald

origin.occupation.pred <- predict(fit.speciesFR,classify="origin.status:occupation.model",sed=TRUE)

## primary species-level model: forestry practitioners only
## #########################################################################
model.data <- droplevels(subset(model.data.all,occupation.model=="forester"))
species.long <- subset(species.long.all,occupation.model=="forester")

fit.species <- asreml(fixed=acceptance ~ origin.status,
                      random=~ Response_ID + region + species + region:species,
                      family=asr_binomial(link="logit"),data=model.data,maxit=500)

fit.species <- update.asreml(fit.species)
wald.species <- wald.asreml(fit.species,denDF="numeric",ssType="conditional")$Wald

## adjusted origin-status predictions
origin.pred <- predict(fit.species,classify="origin.status",sed=TRUE)

## raw species summaries
## #########################################################################
sp.split <- split(model.data$acceptance,model.data$species,drop=TRUE)

species.raw <- data.frame(species=names(sp.split),
                          n=sapply(sp.split,length),
                          accepted=sapply(sp.split,sum),
                          rejected=sapply(sp.split,function(x) sum(x==0)),
                          raw.acceptance=sapply(sp.split,mean),
                          stringsAsFactors=FALSE)

species.raw$european.native <- native.europe[species.raw$species]

origin.tab <- table(model.data$origin.status,model.data$acceptance)

origin.summary <- data.frame(origin.status=rownames(origin.tab),
                             n=rowSums(origin.tab),
                             accepted=origin.tab[,"1"],
                             rejected=origin.tab[,"0"],
                             raw.acceptance=origin.tab[,"1"]/rowSums(origin.tab),
                             row.names=NULL)

## species BLUPs, adjusted for origin status and the other random effects
## #########################################################################
ranef.tab <- summary(fit.species,coef=TRUE)$coef.random
rn <- rownames(ranef.tab)
ii <- grepl("^species_",rn)

species.effects <- data.frame(species=sub("^species_","",rn[ii]),
                              effect=ranef.tab[ii,"solution"],
                              se=ranef.tab[ii,"std.error"],
                              z.ratio=ranef.tab[ii,"z.ratio"],
                              stringsAsFactors=FALSE)

species.effects$lower95 <- species.effects$effect - 1.96*species.effects$se
species.effects$upper95 <- species.effects$effect + 1.96*species.effects$se
species.effects$sig <- abs(species.effects$z.ratio)>2

ii <- match(species.effects$species,species.raw$species)
species.summary <- cbind(species.effects,species.raw[ii,-1,drop=FALSE])
species.summary <- species.summary[order(species.summary$effect,decreasing=TRUE),]

## save objects for manuscript results, table and figures
## #########################################################################
save(species.long,model.data,species.raw,origin.summary,origin.pred,
     species.effects,species.summary,wald.species,fit.species,
     species.long.all,model.data.all,fit.speciesFR,wald.speciesFR,
     origin.occupation.pred, native.europe,
     file="data/species_model_results.RData")

## species-level effects figure
## #########################################################################
eu.col <- hcl.colors(30,"Greens",rev=TRUE)[20]
noneu.col <- hcl.colors(30,"Blues 3",rev=TRUE)[20]

plot.df <- species.summary
plot.df <- plot.df[order(plot.df$effect),]
plot.df$col <- ifelse(plot.df$european.native,eu.col,noneu.col)
y <- seq_len(nrow(plot.df))

pdf("figures/Fig_species_preferences.pdf",width=6.5,height=7.5)

par(mar=c(5,10,1.5,1),family="sans",las=1,bty="l")

plot(plot.df$effect,y,type="n",yaxt="n",
     xlim=range(c(plot.df$lower95,plot.df$upper95)),
     ylim=c(0.5,nrow(plot.df)+0.5),
     xlab="Species deviation in planting preference (log-odds)",ylab="")

abline(v=0,lty=2,col="grey60")

segments(plot.df$lower95,y,plot.df$upper95,y,col=plot.df$col,lwd=1.5)
points(plot.df$effect,y,pch=16,col=plot.df$col,cex=1)

axis(2,at=y,labels=gsub("_"," ",plot.df$species),tick=FALSE,las=1,font=3)

## mark species whose approximate 95% interval excludes zero
points(plot.df$effect[plot.df$sig],y[plot.df$sig],pch=1,cex=1.45,lwd=1)

legend("bottomright",legend=c("European","Non-European"),col=c(eu.col,noneu.col),
       pch=16,bty="n",horiz=F)

dev.off()

