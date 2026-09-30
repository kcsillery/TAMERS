## #######################################################################

## TAMERS survey data analysis - Katalin Csillery 08 Sept 2026

## 5. Species nativeness figures

## ########################################################################

load("data/codes2.RData")
source("functions.r")
load(file="data/country_ftype.RData") ## map code to country and forest type
nat.country <- read.csv("data/Forest_species_nativeness_byCountry.csv", head = TRUE, check.names = FALSE)
nat.ftype <- read.csv("data/Forest_species_nativeness_byForestType.csv", head = TRUE, check.names = FALSE)

## new CSV format is wide: rows = forest types, columns = species.
## rename the first two columns then immediately reshape to long format
## so all downstream code (xtabs, merge, match) remains unchanged.
names(nat.ftype)[1:2] <- c("Forest.type.long", "Forest.type")

## trailing space in at least one Forest.type short name (e.g.
## "Coniferous Mediterranean Anatolian ") -- strip here so it can't
## cause a silent mismatch when matching against ref.forest later
nat.ftype$Forest.type.long <- trimws(nat.ftype$Forest.type.long)
nat.ftype$Forest.type      <- trimws(nat.ftype$Forest.type)

## wide -> long: one row per (forest type, species) combination
species.cols.ftype <- names(nat.ftype)[-(1:2)]
nat.ftype <- do.call(rbind, lapply(species.cols.ftype, function(sp) {
  data.frame(
    Forest.type.long = nat.ftype$Forest.type.long,
    Forest.type      = nat.ftype$Forest.type,
    Species          = sp,
    Native           = nat.ftype[[sp]],
    stringsAsFactors = FALSE
  )
}))

## 3 ways of calculating the proportion of proposed species that are native:
## country, minimalist regional groups, and broad regional groups
## ########################################################################

## 1. Country level
native.prop.country <- nat.country
native.prop.country$native.prop <- rowMeans(native.prop.country[, -1, drop = FALSE], na.rm = TRUE)

write.csv(
    native.prop.country[, c("Country", "native.prop")],
    "data/Country_native_species_proportion.csv",
    row.names = FALSE
)

## 2. Minimalist regional grouping
native.prop.minimalist <- nat.country
native.prop.minimalist$Country <- recode.group(native.prop.minimalist$Country, how = "minimalist")

## species is native to the group if native in at least one constituent country
native.prop.minimalist <- aggregate(
    native.prop.minimalist[, -1, drop = FALSE],
    by = list(Country = native.prop.minimalist$Country),
    FUN = max,
    na.rm = TRUE
)

native.prop.minimalist$native.prop <- rowMeans(
    native.prop.minimalist[, -1, drop = FALSE],
    na.rm = TRUE
)

write.csv(
    native.prop.minimalist[, c("Country", "native.prop")],
    "data/Region_minimalist_native_species_proportion.csv",
    row.names = FALSE
)

## 3. Broad regional grouping
native.prop.region <- nat.country
native.prop.region$Country <- recode.group(native.prop.region$Country)

## species is native to the group if native in at least one constituent country
native.prop.region <- aggregate(
    native.prop.region[, -1, drop = FALSE],
    by = list(Country = native.prop.region$Country),
    FUN = max,
    na.rm = TRUE
)

native.prop.region$native.prop <- rowMeans(
    native.prop.region[, -1, drop = FALSE],
    na.rm = TRUE
)

write.csv(
    native.prop.region[, c("Country", "native.prop")],
    "data/Region_native_species_proportion.csv",
    row.names = FALSE
)

## continue with the preparing the data for the heatmap....
## #########################################################

## ## countries dropped entirely (too few responses, no adequate match for
## ## a group): Norway (n=1)
## exclude.countries <- c("Norway")
## country <- country[!(country %in% exclude.countries)]

## relabel the survey country variable
country <- recode.group(country)
nat.country$Country <- recode.group(nat.country$Country)

## for the heatmap use the strong grouping of countries to regions
## collapse the now-duplicated group rows into one row per group
nat.country <- aggregate(nat.country[, -1], by = list(Country = nat.country$Country), FUN = max)

## keep all countries/groups with at least one survey response 
nat.country <- subset(nat.country, Country %in% names(table(country)))


## clean forest type labels
ref.forest <- gsub("\\s*\\(forest types\\).*", "", ref.forest)
ref.forest <- iconv(ref.forest, from = "UTF-8", to = "ASCII//TRANSLIT")
ref.forest <- gsub("[[:space:]]+", " ", ref.forest)
ref.forest <- trimws(ref.forest)
## remove "None applies or do not know"
ref.forest <- ref.forest[ref.forest != "None applies or do not know"]
nat.ftype <- nat.ftype[nat.ftype$Forest.type.long %in% ref.forest, ]

## standardize species names
fix.species <- function(x) gsub(" ", ".", trimws(x))
plot.species <- function(x) gsub("\\.", " ", x)

names(nat.country)[-1] <- fix.species(names(nat.country)[-1])
nat.ftype$Species <- fix.species(nat.ftype$Species)

conifer.species <- c("Abies.alba", "Pinus.nigra", "Pinus.sylvestris", "Abies.grandis", "Larix.kaempferi", "Pinus.strobus", "Sequoia.sempervirens", "Pseudotsuga.menziesii", "Pinus.mugo", "Picea.abies", "Taxus.baccata", "Larix.decidua", "Cedrus.atlantica", "Picea.sitchensis", "Pinus.contorta")
broadleaf.species <- c("Robinia.pseudoacacia", "Castanea.sativa", "Quercus.rubra", "Juglans.nigra", "Acer.campestre", "Fagus.orientalis", "Paulownia.tomentosa", "Eucalyptus.globulus", "Populus.alba", "Quercus.petraea", "Tilia.tomentosa", "Ulmus.laevis", "Sorbus.torminalis", "Betula.pendula", "Fagus.sylvatica")

## country summary
species.cols <- names(nat.country)[-1]
native.prop <- colMeans(nat.country[, species.cols], na.rm = TRUE)
native.tab <- sort(native.prop)

conifer.tab <- native.tab[names(native.tab) %in% conifer.species]
broadleaf.tab <- native.tab[names(native.tab) %in% broadleaf.species]

## forest type summary
nat.ftype.wide <- xtabs(Native ~ Forest.type + Species, data = nat.ftype)
native.ftype.prop <- colMeans(nat.ftype.wide, na.rm = TRUE)
native.ftype.tab <- sort(native.ftype.prop)

conifer.ftype.tab <- native.ftype.tab[names(native.ftype.tab) %in% conifer.species]
broadleaf.ftype.tab <- native.ftype.tab[names(native.ftype.tab) %in% broadleaf.species]


plot.native.bar <- function(tab, ylab, panel, main) {
  tmp <- sort(tab)
  bp <- barplot(tmp, border = "grey40", ylim = c(0, 1), ylab = ylab, names.arg = FALSE)
  axis(1, at = bp, labels = FALSE, tick = FALSE)
  text(x = bp, y = par("usr")[3] - 0.06, labels = plot.species(names(tmp)), srt = 45, adj = 1, xpd = TRUE)
  abline(h = seq(0, 1, by = 0.2), col = "grey90")
  title(main)
  mtext(panel, side = 3, line = 0.2, adj = -0.08, font = 2)
  box()
}

pdf("figures/FigSX_species_barplots.pdf", width = 8, height = 7)

layout(matrix(c(1:4), nrow = 2, byrow=T))
par(mar = c(8, 5, 1, 1), las = 1, bty = "l", family = "sans")

plot.native.bar(conifer.tab, "Prop. of countries species native", "", "Conifers")
plot.native.bar(broadleaf.tab, "", "", "Broadleaves")

plot.native.bar(conifer.ftype.tab, "Prop. of forest types species native", "", "")
plot.native.bar(broadleaf.ftype.tab, "", "", "")

dev.off()


## nativeness per country and ftype
## #################################

nat.country.long <- data.frame(Country = rep(nat.country[[1]], times = ncol(nat.country) - 1), Species = rep(names(nat.country)[-1], each = nrow(nat.country)), Native.country = as.vector(as.matrix(nat.country[, -1])), stringsAsFactors = FALSE)

nat.merge <- merge(nat.country.long, nat.ftype[, c("Forest.type", "Species", "Native")], by = "Species")

nat.merge$Native.both <- nat.merge$Native.country == 1 & nat.merge$Native == 1

country.ftype <- aggregate(Native.both ~ Country + Forest.type, data = nat.merge, FUN = mean)
country.ftype$Native.percent <- 100 * country.ftype$Native.both

z <- xtabs(Native.percent ~ Country + Forest.type, data = country.ftype)
z <- as.matrix(z)

z <- z[order(rowMeans(z, na.rm = TRUE)), order(colMeans(z, na.rm = TRUE)), drop = FALSE]

row.means <- rowMeans(z, na.rm = TRUE)
col.means <- colMeans(z, na.rm = TRUE)

## add number of responses per country and forest type
tab.country <- table(country)[names(row.means)]
tmp <- as.numeric(tab.country); names(tmp) <- names(tab.country)
tab.country <- tmp

ref.forest.short <- nat.ftype[match(ref.forest, nat.ftype$Forest.type.long), "Forest.type"]
tab.ftype <- table(ref.forest.short)[names(col.means)]

#########################################################################################
## save data to load in AM score script and create Figure 2.

save(z, tab.ftype, tab.country, file="data/data_nativeness_heatmap.RData")
#########################################################################################


## Figure 2: nativeness and number of responses
#################################################

pdf("figures/Fig2_species_nativeness_heatmap.pdf", width = 7, height = 10)

##layout(matrix(c(1,0,2,3), nrow = 2, byrow = TRUE), widths = c(5, 1.25), heights = c(1.2, 7))
layout(matrix(c(1,0,2,3,4,0), nrow = 3, byrow = TRUE), widths = c(5, 1.25), heights = c(1.2, 7, 0.75))

par(las = 1, bty = "o", family = "sans", cex.lab = 1.1)

xpos <- seq(0.5, ncol(z) - 0.5)
ypos <- seq(0.5, nrow(z) - 0.5)
cols <- colorRampPalette(c("#f7fbff", "#deebf7", "#9ecae1", "#4292c6", "#08519c", "#08306b"))(100)

## top marginal barplot
par(mar = c(0.1, 12, 1.6, 0), xaxs = "i", yaxs = "i")
barplot(tab.ftype, col = "grey75", border = "grey45", axes = FALSE, names.arg = FALSE, space = 0, xlim = c(0, ncol(z)))
axis(2)
mtext("Number of\n responses per\n forest type",  side = 2, line = 2.9, las = 1, cex=0.85)

## heatmap
par(mar = c(14, 12, 0, 0), xaxs = "i", yaxs = "i")
image(x = xpos, y = ypos, z = t(z), axes = FALSE, xlab = "", ylab = "", col = cols, zlim = c(0, max(z, na.rm = TRUE)), xlim = c(0, ncol(z)), ylim = c(0, nrow(z)))

axis(2, at = ypos, labels = rownames(z), tick = TRUE, cex.axis=1.2)
axis(1, at = xpos, labels = rep("", length(xpos)), tick = TRUE)
text(xpos, par("usr")[3] - 0.45, labels = colnames(z), srt = 45, adj = 1, xpd = TRUE, cex=1.2)

##mtext("Forest type", side = 1, line = 6.5, las = 0)

abline(v = 0:ncol(z), col = "white", lwd = 0.6)
abline(h = 0:nrow(z), col = "white", lwd = 0.6)

for(i in seq_len(nrow(z))) for(j in seq_len(ncol(z)))
                               text(xpos[j], ypos[i], round(z[i, j]), cex = 1, col = ifelse(z[i, j] > max(z, na.rm = TRUE) * 0.8, "white", "grey15"))

box()

## right marginal barplot
par(mar = c(14, 0.1, 0, 1.0), xaxs = "i", yaxs = "i")
barplot(tab.country, horiz = TRUE, col = "grey75", border = "grey45", axes = FALSE, names.arg = FALSE, space = 0, ylim = c(0, nrow(z)))
axis(1)
mtext("Number of\n responses\n per country", side = 1, line = 4.8, las = 0, cex=0.8)

## color scale
par(mar = c(3.2, 10, 0.6, 0), xaxs = "i", yaxs = "i", bty = "n", family = "sans")
zlim <- c(0, max(z, na.rm = TRUE))
image(x = seq(zlim[1], zlim[2], length.out = length(cols)), y = 1, z = matrix(seq(zlim[1], zlim[2], length.out = length(cols)), nrow = length(cols), ncol = 1), col = cols, axes = FALSE, xlab = "", ylab = "")
axis(1, at = pretty(zlim))
mtext("% species that are native", side = 1, line = 2.1, las = 0, cex = 0.8)
box()

dev.off()

