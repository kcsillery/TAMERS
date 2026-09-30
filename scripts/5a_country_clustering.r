## #######################################################################

## TAMERS survey data analysis - Katalin Csillery 08.05.2026

## 5a. Quantitative check for which countries to group in the nativity
## heatmap.

## ########################################################################

## IMPORTANT: this clusters on species-IDENTITY overlap (which species are
## native, via Jaccard/binary distance), NOT on % native. Two countries can
## land on the same % native via completely different species -- that would
## not be an ecologically meaningful basis for grouping them. Jaccard
## distance only goes to zero when the actual native species SETS match.

load(file="data/country_ftype.RData")
nat.country <- read.csv("data/Forest_species_nativeness_byCountry.csv", check.names = FALSE)
rownames(nat.country) <- nat.country$Country
species.cols <- setdiff(names(nat.country), "Country")

## binary (= Jaccard) distance on the species presence/absence matrix.
## base R's dist(method="binary") is exactly the Jaccard distance for 0/1
## data: proportion of species where the two countries disagree, among all
## species where at least one of them is native.
d  <- dist(nat.country[, species.cols], method = "binary")
hc <- hclust(d, method = "complete")

tab.country <- table(country)[rownames(nat.country)]

pdf("figures/FigSX_country_clustering_check.pdf", width = 9, height = 9)
plot(hc, labels = paste0(rownames(nat.country), " (n=", tab.country, ")"),
     main = "Country clustering by native species overlap (Jaccard distance)",
     xlab = "", sub = "")
dev.off()

## nearest-neighbour table: for each country, who shares the most similar
## native species set, and how does that compare to its own sample size?
dm <- as.matrix(d)
diag(dm) <- NA
nearest <- data.frame(
  country   = rownames(nat.country),
  n         = as.integer(tab.country),
  nearest   = rownames(nat.country)[apply(dm, 1, which.min)],
  nearest.n = as.integer(tab.country[apply(dm, 1, which.min)]),
  distance  = apply(dm, 1, min, na.rm = TRUE)
)
nearest <- nearest[order(nearest$distance), ]
print(nearest, row.names = FALSE)

## candidate merges: near-identical native species sets
## (threshold below is a starting point -- adjust to taste)
candidate.merge <- subset(nearest, distance < 0.1)
print(candidate.merge, row.names = FALSE)

## for any candidate pair, show exactly which species differ -- useful for
## confirming the merge is a minor range-edge difference rather than a real
## ecological mismatch
species.diff <- function(a, b) {
  sa <- species.cols[nat.country[a, species.cols] == 1]
  sb <- species.cols[nat.country[b, species.cols] == 1]
  list(only.in.a = setdiff(sa, sb), only.in.b = setdiff(sb, sa))
}
## example: species.diff("Romania", "Ukraine")
