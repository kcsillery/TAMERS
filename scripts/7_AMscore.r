## #######################################################################

## TAMERS survey data analysis -- Katalin Csillery -- 07.07.2026

## 7_AMscore.r -- acceptance scores (planter/nativer) and models

## ########################################################################

load("data/dat_all_scores.RData")
source("functions.r")

rownames(dat.all) <- dat.all$Response_ID

## define choice data
## ####################
choice.vars <- grep("^choice_", names(dat.all), value = TRUE)
choice.vars <- choice.vars[!grepl("Other", choice.vars)]   ## exclude "Other species", same as before
choice.vars <- choice.vars[!grepl("_num$", choice.vars)]   ## use the English TEXT columns

X.text <- na.omit(dat.all[, choice.vars])
keep.ids <- rownames(X.text)
dat.choice <- dat.all[keep.ids,]

## planter score: general species-acceptance index
## #########################################################################
accept.text  <- c("Yes in pure stocks", "Yes in mixed stocks", "Yes, but sporadically")
reject.text  <- "No"
neutral.text <- c("Not applicable for my reference forest", "I don't know")

X <- as.data.frame(lapply(X.text, recode.accept), stringsAsFactors = FALSE)
dat.choice$planter <- rowSums(X)

## Incorporate species proposed in the two "Other species" fields
## #########################################################################

other.broadleaf <- read.csv("data/OtherNativeness/other_species_broadleaf_codebook_checked.csv", stringsAsFactors = FALSE, check.names = FALSE)
other.conifer <- read.csv("data/OtherNativeness/other_species_conifer_codebook_checked.csv", stringsAsFactors = FALSE, check.names = FALSE)
broadleaf.other.var <- find.other.var(dat.choice, other.broadleaf)
conifer.other.var <- find.other.var(dat.choice, other.conifer)

## lookup based on Original text
other.broadleaf$match.text <- clean.other(other.broadleaf$`Original text`)
other.conifer$match.text <- clean.other(other.conifer$`Original text`)

## species represented by the predefined choice columns
proposed.species <- gsub("^choice_", "", choice.vars)
proposed.species <- gsub("_", " ", proposed.species)
proposed.species.clean <- clean.species(proposed.species)
names(proposed.species.clean) <- choice.vars

## audit variables
dat.choice$other.species.new.n <- 0L
dat.choice$other.species.reassigned.n <- 0L

for(i in seq_len(nrow(dat.choice))){
    seen <- character(0)

    res <- add.other.species(i, dat.choice[[broadleaf.other.var]][i], other.broadleaf, seen)
    seen <- res$seen
    dat.choice$other.species.new.n[i] <- dat.choice$other.species.new.n[i] + res$new
    dat.choice$other.species.reassigned.n[i] <- dat.choice$other.species.reassigned.n[i] + res$reassigned

    res <- add.other.species(i, dat.choice[[conifer.other.var]][i], other.conifer, seen)
    seen <- res$seen
    dat.choice$other.species.new.n[i] <- dat.choice$other.species.new.n[i] + res$new
    dat.choice$other.species.reassigned.n[i] <- dat.choice$other.species.reassigned.n[i] + res$reassigned
}

dat.choice$planter.orig <- dat.choice$planter
dat.choice$planter <- rowSums(X) + dat.choice$other.species.new.n

pdf("figures/Planter_adjusted_country.pdf", 7, 15)
par(mar=c(4,12,1,1))
boxplot(dat.choice$planter - dat.choice$planter.orig ~
            reorder(dat.choice$Country,
                    dat.choice$planter - dat.choice$planter.orig,
                    FUN = mean), las = 2,
        ylab = "",
        xlab = "Increase in planter score", horizontal=T)
dev.off()

## nativer score: nativeness-weighted acceptance index
## #########################################################################
nat.country <- read.csv("data/Forest_species_nativeness_byCountry.csv", check.names = FALSE)
sp <- gsub("^choice_", "", choice.vars)
sp <- gsub("_", " ", sp)

nat.mat <- matrix(NA, nrow = nrow(X), ncol = ncol(X))

for(i in seq_len(nrow(X))) {
  cc <- dat.choice[i, "Country"]
  hit <- match(cc, nat.country$Country)
  if(!is.na(hit)) nat.mat[i, ] <- as.numeric(nat.country[hit, sp])
}
nat.mat[nat.mat == 0] <- -1   ## non-native -> -1, native -> +1 (matches X's +1/-1 scale)

## coding: native+accept or non-native+reject => +1 ("aligned with nativeness")
##         non-native+accept or native+reject => -1 ("against nativeness")
##         neutral answer, or country missing from the nativeness table => NA
X.nat <- X * nat.mat

dat.choice$nativer <- rowSums(X.nat)

## #########################################################################
## Adjust nativer score using newly proposed Other species
## #########################################################################

other.broadleaf <- read.csv("data/OtherNativeness/other_species_broadleaf_codebook_checked.csv", stringsAsFactors = FALSE, check.names = FALSE)
other.conifer <- read.csv("data/OtherNativeness/other_species_conifer_codebook_checked.csv", stringsAsFactors = FALSE, check.names = FALSE)
nat.other <- read.csv("data/OtherNativeness/Forest_Other_species_nativeness_byCountry.csv", stringsAsFactors = FALSE, check.names = FALSE)

broadleaf.other.var <- find.other.var(dat.choice, other.broadleaf)
conifer.other.var <- find.other.var(dat.choice, other.conifer)

other.broadleaf$match.text <- clean.other(other.broadleaf$`Original text`)
other.conifer$match.text <- clean.other(other.conifer$`Original text`)

## predefined 30 species, already included in the original nativer score
proposed.species <- gsub("^choice_", "", choice.vars)
proposed.species <- gsub("_", " ", proposed.species)
proposed.species.clean <- clean.species(proposed.species)

## clean species and country names in the Other-species nativeness table
nat.other$Species.clean <- clean.species(nat.other$Species)

dat.choice$other.native.n <- 0L
dat.choice$other.nonnative.n <- 0L
dat.choice$other.nativeness.known.n <- 0L
dat.choice$other.nativeness.adjustment <- 0

for(i in seq_len(nrow(dat.choice))){
    species <- c(get.other.species(dat.choice[[broadleaf.other.var]][i], other.broadleaf),
                 get.other.species(dat.choice[[conifer.other.var]][i], other.conifer))

    species <- unique(species)
    species.clean <- clean.species(species)

    ## country
    country <- as.character(dat.choice$Country[i])
    
    ## "many" cannot be assigned a nativeness status
    keep <- species.clean != "many" & species.clean != ""

    ## ignore species already represented among the predefined 30 species
    keep <- keep & !species.clean %in% proposed.species.clean

    species <- species[keep]
    species.clean <- species.clean[keep]

    if(length(species.clean) == 0) next

    hit <- match(species.clean, nat.other$Species.clean)
    status <- as.numeric(nat.other[hit, country])
    status <- status[!is.na(status)]

    if(length(status) == 0) next

    dat.choice$other.native.n[i] <- sum(status == 1)
    dat.choice$other.nonnative.n[i] <- sum(status == 0)
    dat.choice$other.nativeness.known.n[i] <- length(status)

    p.native <- mean(status == 1)
    p.nonnative <- mean(status == 0)

    dat.choice$other.nativeness.adjustment[i] <- p.native - p.nonnative
}

dat.choice$nativer.orig <- dat.choice$nativer
dat.choice$nativer <- dat.choice$nativer + dat.choice$other.nativeness.adjustment


save(dat.choice, file="data/dat_all_choice.RData")

## plot the distribution of the two response variables
## #####################################################
pdf("figures/FigSX_planter_distribution.pdf", width = 9, height = 4)
par(mfcol=c(1,2), xpd=T)
hist(dat.choice$planter.orig, breaks = 30, xlab = "Number of species accepted or proposed", main = "", col = "grey85")
hist(dat.choice$planter, breaks = 30, xlab = "", main = "", col = adjustcolor("blue", alpha.f = 0.2), add=T)

hist(dat.choice$nativer.orig, breaks = 30, xlab = "Number of native species accepted or proposed", main = "", col = "grey85")
hist(dat.choice$nativer, breaks = 30, xlab = "", main = "", col = adjustcolor("blue", alpha.f = 0.2), add=T)

legend(-20, 120, fill=c("grey85", adjustcolor("blue", alpha.f = 0.2)), legend=c("30 species score", "Ajusted score with Other species"), cex=.8)
dev.off()


## ################################################################################
pdf("figures/Fig3_nativeness_heatmap_country_crosses_planter_nativer.pdf", width = 10, height = 8.5)
## #############################################################################

par(mar = c(4.5,4.8,1,1), las = 1, bty = "l", family = "sans", cex.lab=1.3)

####################### part a

load(file="data/data_nativeness_heatmap.RData")

layout(matrix(c(1,0,0,5,
                2,3,0,5,
                2,3,0,5,
                2,3,0,0,
                2,3,0,6,
                2,3,0,6,
                4,0,0,6), nrow = 7, byrow = TRUE), widths = c(5,1.25,0.45,4.8), heights = c(1.1,1.5,1.5,0.40,1.5,1.5,0.8))

par(las = 1, bty = "o", family = "sans", cex.lab = 1.1)

xpos <- seq(0.5, ncol(z) - 0.5)
ypos <- seq(0.5, nrow(z) - 0.5)
cols <- colorRampPalette(c("#f7fbff", "#deebf7", "#9ecae1", "#4292c6", "#08519c", "#08306b"))(100)

## top marginal barplot
par(mar = c(0.1,12,2.0,0), xaxs = "i", yaxs = "i")
barplot(tab.ftype, col = "grey75", border = "grey45", axes = FALSE, names.arg = FALSE, space = 0, xlim = c(0,ncol(z)))
axis(2)
mtext("Number of\n responses per\n forest type", side = 2, line = 2.9, las = 1, cex=0.85)

mtext("a", side=3, line=0.5, adj=-0.12, font=2, cex=1.3, xpd=NA)
mtext("Native-species representation", side = 3, line = 0.5, adj = 0.55, font = 2, cex = 1.1, xpd = NA)

## heatmap
par(mar = c(14,12,0,0), xaxs = "i", yaxs = "i")
image(x = xpos, y = ypos, z = t(z), axes = FALSE, xlab = "", ylab = "", col = cols, zlim = c(0,max(z, na.rm = TRUE)), xlim = c(0,ncol(z)), ylim = c(0,nrow(z)))

axis(2, at = ypos, labels = rownames(z), tick = TRUE, cex.axis=1.2)
axis(1, at = xpos, labels = rep("", length(xpos)), tick = TRUE)
text(xpos, par("usr")[3] - 0.45, labels = colnames(z), srt = 45, adj = 1, xpd = TRUE, cex=1.2)

##mtext("Forest type", side = 1, line = 6.5, las = 0)

abline(v = 0:ncol(z), col = "white", lwd = 0.6)
abline(h = 0:nrow(z), col = "white", lwd = 0.6)

for(i in seq_len(nrow(z))) for(j in seq_len(ncol(z)))
                               text(xpos[j], ypos[i], round(z[i,j]), cex = 1, col = ifelse(z[i,j] > max(z, na.rm = TRUE) * 0.8, "white", "grey15"))

box()

## right marginal barplot
par(mar = c(14,0.1,0,1.0), xaxs = "i", yaxs = "i")
barplot(tab.country, horiz = TRUE, col = "grey75", border = "grey45", axes = FALSE, names.arg = FALSE, space = 0, ylim = c(0,nrow(z)))
axis(1)
mtext("Number of\n responses\n per country", side = 1, line = 4.8, las = 0, cex=0.8)

## color scale
par(mar = c(3.6,10,0.6,0), xaxs = "i", yaxs = "i", bty = "n", family = "sans")
zlim <- c(0,max(z, na.rm = TRUE))
image(x = seq(zlim[1], zlim[2], length.out = length(cols)), y = 1, z = matrix(seq(zlim[1], zlim[2], length.out = length(cols)), nrow = length(cols), ncol = 1), col = cols, axes = FALSE, xlab = "", ylab = "")
axis(1, at = pretty(zlim))
mtext("% species that are native", side = 1, line = 2.1, las = 0, cex = 1)
box()

######################### part b

## planter vs nativer plot in the COUNTRY space
## #####################################################

par(mar = c(4.5,4,2.2,3), las = 1, bty = "l", family = "sans", cex.lab=1.5)
## country/group means and standard errors in planter-nativer space
dat.plot <- dat.choice
dat.plot$Country.group2 <- recode.group(dat.plot$Country, how = "minimalist")

## optional minimum sample size after grouping
n.group <- table(dat.plot$Country.group2)
dat.plot <- dat.plot[dat.plot$Country.group2 %in% names(n.group[n.group >= 4]), ]

country.xy <- aggregate(cbind(planter, nativer) ~ Country.group2, data = dat.plot, FUN = mean)

country.se <- aggregate(cbind(planter, nativer) ~ Country.group2, data = dat.plot, FUN = function(x) 1.96 * sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
## SE: FUN = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
names(country.se)[2:3] <- c("planter.se", "nativer.se")

country.n <- as.data.frame(table(dat.plot$Country.group2))
names(country.n) <- c("Country.group2", "n")

country.xy <- merge(country.xy, country.se, by = "Country.group2")
country.xy <- merge(country.xy, country.n, by = "Country.group2")

country.xy <- subset(country.xy, n>=5)
##country.xy <- subset(country.xy, Country.group2!="Turkey")

plot(country.xy$planter, country.xy$nativer, type = "n", xlab = "General willingness to plant", ylab = "Native species preference", xlim = range(country.xy$planter - country.xy$planter.se, country.xy$planter + country.xy$planter.se+1), ylim = range(country.xy$nativer - country.xy$nativer.se, country.xy$nativer + country.xy$nativer.se))

abline(h = 0, v = 0, lty = 2, col = "grey70")

segments(country.xy$planter - country.xy$planter.se, country.xy$nativer, country.xy$planter + country.xy$planter.se, country.xy$nativer, col = "grey60", lwd=3)
segments(country.xy$planter, country.xy$nativer - country.xy$nativer.se, country.xy$planter, country.xy$nativer + country.xy$nativer.se, col = "grey60", lwd=3)

points(country.xy$planter, country.xy$nativer, pch = 16, cex = 0.6 + sqrt(country.xy$n)/3, col = "grey60")

lab.pos <- rep(4, nrow(country.xy))
names(lab.pos) <- country.xy$Country.group2

lab.pos[c("Poland", "South-East Balkan", "Carpathian","Italy")] <- 4
lab.pos[c("Slovenia-Hungary", "Baltic")] <- 2
lab.pos[c("Switzerland", "Belgium", "Spain", "France", "Ireland")] <- 2
lab.pos[c("Czechia", "Western Balkans", "Germany")] <- 4
lab.pos[c("United Kingdom", "Austria", "Portugal", "Denmark")] <- 4

text(country.xy$planter, country.xy$nativer+0.5, labels = country.xy$Country.group2, pos = lab.pos, offset = .8, cex = 1.1, xpd = TRUE)

mtext("b", side = 3, line = 0.6, adj = 0, font = 2, cex = 1.3, xpd = NA)
mtext("Countries", side = 3, line = 0.6, adj = 0.55, font = 2, cex = 1.1, xpd = NA)

######################### part c

## planter vs nativer plot in the REFERENCE FOREST space
## #####################################################

par(mar = c(4.5,4,2.2,3), las = 1, bty = "l", family = "sans", cex.lab=1.5)

dat.plot <- dat.choice
forest.types <- read.csv("data/Forest_species_nativeness_byForestType.csv", check.names = FALSE, stringsAsFactors = FALSE)

dat.plot$ref.forest.long <- dat.plot$ref.forest

dat.plot$ref.forest.long <- gsub("\u00a0", " ", dat.plot$ref.forest.long)
dat.plot$ref.forest.long <- gsub("-", "-", dat.plot$ref.forest.long)
dat.plot$ref.forest.long <- gsub("\\s*\\(forest types\\)\\s*$", "", dat.plot$ref.forest.long)
dat.plot$ref.forest.long <- trimws(dat.plot$ref.forest.long)

forest.types$`Forest type long` <- gsub("\u00a0", " ", forest.types$`Forest type long`)
forest.types$`Forest type long` <- gsub("-", "-", forest.types$`Forest type long`)
forest.types$`Forest type long` <- trimws(forest.types$`Forest type long`)

hit <- match(dat.plot$ref.forest.long, forest.types$`Forest type long`)
dat.plot$ref.forest <- forest.types$`Forest type`[hit]

## remove missing/unknown reference forests
dat.plot <- subset(dat.plot, !ref.forest %in% c("None applies or do not know", "No answer"))

## optional minimum sample size
n.group <- table(dat.plot$ref.forest)
dat.plot <- dat.plot[dat.plot$ref.forest %in% names(n.group[n.group >= 5]), ]

ref.xy <- aggregate(cbind(planter, nativer) ~ ref.forest, data = dat.plot, FUN = mean)

ref.se <- aggregate(cbind(planter, nativer) ~ ref.forest, data = dat.plot, FUN = function(x) 1.96 * sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
names(ref.se)[2:3] <- c("planter.se", "nativer.se")

ref.n <- as.data.frame(table(dat.plot$ref.forest))
names(ref.n) <- c("ref.forest", "n")

ref.xy <- merge(ref.xy, ref.se, by = "ref.forest")
ref.xy <- merge(ref.xy, ref.n, by = "ref.forest")


plot(ref.xy$planter, ref.xy$nativer, type = "n", xlab = "General willingness to plant", ylab = "Native species preference", xlim = range(ref.xy$planter - ref.xy$planter.se, ref.xy$planter + ref.xy$planter.se + 1), ylim = range(ref.xy$nativer - ref.xy$nativer.se, ref.xy$nativer + ref.xy$nativer.se))

abline(h = 0, v = 0, lty = 2, col = "grey70")

segments(ref.xy$planter - ref.xy$planter.se, ref.xy$nativer, ref.xy$planter + ref.xy$planter.se, ref.xy$nativer, col = "grey60", lwd = 3)
segments(ref.xy$planter, ref.xy$nativer - ref.xy$nativer.se, ref.xy$planter, ref.xy$nativer + ref.xy$nativer.se, col = "grey60", lwd = 3)

points(ref.xy$planter, ref.xy$nativer, pch = 16, cex = 0.6 + sqrt(ref.xy$n)/3, col = "grey60")

lab.pos <- rep(4, nrow(ref.xy))
names(lab.pos) <- ref.xy$ref.forest

text(ref.xy$planter, ref.xy$nativer + 0.2, labels = ref.xy$ref.forest, pos = lab.pos, offset = 0.8, cex = 1.1, xpd = TRUE)

mtext("c", side = 3, line = 0.6, adj = 0, font = 2, cex = 1.3, xpd = NA)
mtext("Reference forest types", side = 3, line = 0.6, adj = 0.55, font = 2, cex = 1.1, xpd = NA)

dev.off()






## #########################################################################
## PCA of raw species responses
## #########################################################################
X.var <- X[, apply(X, 2, function(x) var(x, na.rm = TRUE) > 0), drop = FALSE]
pca <- prcomp(X.var, center = TRUE, scale = TRUE)

pdf("figures/FigSX_PCA_species_responses_by_groups.pdf", width = 9, height = 8)
par(mfrow = c(2, 2), mar = c(4.2, 4.5, 2.2, 1), las = 1, bty = "l", family = "sans")
plot.pca.group(pca, dat.choice$Country, "Country")
plot.pca.group(pca, dat.choice$ref.forest, "Reference forest type")
plot.pca.group(pca, dat.choice$years.exper, "Forestry-related experience")
plot.pca.group(pca, dat.choice$degree, "Highest degree")
dev.off()


