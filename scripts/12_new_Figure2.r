
#########################################################################################

## New Figure 2 combining Figure 2 and 3

#########################################################################################

load(file = "data/dat_all_demographics.RData")
load(file = "data/dat_for_Fig2.RData")
load(file = "data/data_nativeness_heatmap.RData")
source("functions.r")

## figure

## define colors for the plot
blue.cols <- hcl.colors(30, "Grays", rev=TRUE)[5:25]

forester.roles <- c("Forest manager", "Public administration",
                    "Forest owner and manager", "Forest owner")

role.blue.dark <- hcl.colors(30, "Greens", rev=TRUE)[3]
role.blue.light <- hcl.colors(30, "Greens", rev=TRUE)[18]
role.cols <- c(role.blue.light, role.blue.dark)

pdf("figures/Fig2_combined.pdf", width = 15*.85, height = 16*.8)

mat <- matrix(c(
  1, 1, 2, 2, 3, 3,
  4, 4, 4, 4, 4, 0,
  5, 5, 5, 5, 5, 6,
  0, 7, 7, 7, 7, 0
), nrow = 4, byrow = TRUE)

layout(mat,
       widths  = c(1, 1.2, 2, 1, 2, 1.5),
       heights = c(3.2, 1.2, 5.0, 1))

par(las = 1, bty = "o", cex = 1, family = "sans", cex.lab = 1.3)

## panel a
par(mar = c(6, 6, 2, 0.5))

plot.heat(age.exper[-1,], "", "",
          "", "", zmax = max(age.exper, na.rm = TRUE))

mtext("Age class", 2, line = 4, las = 0)
mtext("Forestry-related experience in years", 1, line = 2.5, las = 0)
mtext("a Age and experience", side = 3, line = 0.25, adj = 0, font = 2, cex = 1.2)


## panel b
par(mar = c(7.5, 11, 2, .5))

plot.heat(field.degree, "", "",
          "", "",
          zmax = max(field.degree, na.rm = TRUE),
          str45 = TRUE)

mtext("Field of studies", 2, line = 8.8, las = 0)
mtext("Highest degree", 1, line = 6, las = 0)
mtext("b Education profile", side = 3, line = 0.25, adj = 0, font = 2, cex = 1.2)


## panel c
par(mar = c(5.2, 12, 2, 2), xpd = TRUE)

role.plot <- role.tab[nrow(role.tab):1, , drop = FALSE]

barplot(t(role.plot),
        horiz = TRUE,
        beside = FALSE,
        col = role.cols,
        border = "grey45",
        names.arg = rownames(role.plot),
        las = 1,
        xlab = "Number of respondents",
        cex.lab = 0.98)

mtext("c Current work role/sector", side = 3, line = 0.25, adj = 0, font = 2, cex = 1.2)

legend(35, 2,
       legend = c("Forestry practitioner", "Researcher"),
       fill = role.cols,
       border = "grey45",
       bty = "n")


## --------------------------------------------------------
## panel d: transposed / landscape version
## --------------------------------------------------------

par(las = 1, bty = "o", family = "sans", cex.lab = 1.3)

## x = countries
## y = forest types
xpos <- seq(0.5, nrow(z) - 0.5)
ypos <- seq(0.5, ncol(z) - 0.5)

cols <- colorRampPalette(c(
  "#f7fbff", "#deebf7", "#9ecae1",
  "#4292c6", "#08519c", "#08306b"
))(100)


## top marginal: responses per country
par(mar = c(0.1, 15, 3, 0), xaxs = "i", yaxs = "i")

barplot(tab.country,
        col = "grey75",
        border = "grey45",
        axes = FALSE,
        names.arg = FALSE,
        space = 0,
        xlim = c(0, nrow(z)))

axis(2)

mtext("Number of\nresponses per\ncountry/region", side = 2, line = 3.0, las = 1, cex = 1)

## optional panel label
mtext("d Nativeness of the proposed species by country and forest type", side = 3, line = 1.2, adj = 0, font = 2, cex = 1.2)

## heatmap
par(mar = c(6, 15, 0, 0),
    xaxs = "i", yaxs = "i")

image(x = xpos,
      y = ypos,
      z = z,
      axes = FALSE,
      xlab = "",
      ylab = "",
      col = cols,
      zlim = c(0, max(z, na.rm = TRUE)),
      xlim = c(0, nrow(z)),
      ylim = c(0, ncol(z)))


axis(2,
     at = ypos,
     labels = colnames(z),
     tick = TRUE,
     cex.axis = 0.9)

axis(1,
     at = xpos,
     labels = rep("", length(xpos)),
     tick = TRUE)

text(xpos,
     par("usr")[3] - 0.40,
     labels = rownames(z),
     srt = 45,
     adj = 1,
     xpd = T,
     cex = 0.9)

par(xpd=F)
abline(v = 0:nrow(z), col = "white", lwd = 0.6)
abline(h = 0:ncol(z), col = "white", lwd = 0.6)

for(i in seq_len(nrow(z))) {
  for(j in seq_len(ncol(z))) {

    text(xpos[i],
         ypos[j],
         round(z[i, j]),
         cex = 0.9,
         col = ifelse(
           z[i, j] > max(z, na.rm = TRUE) * 0.8,
           "white",
           "grey15"
         ))
  }
}

box()


## right marginal: responses per forest type
par(mar = c(6, 0.1, 0, 1.0),
    xaxs = "i", yaxs = "i")

barplot(tab.ftype,
        horiz = TRUE,
        col = "grey75",
        border = "grey45",
        axes = FALSE,
        names.arg = FALSE,
        space = 0,
        ylim = c(0, ncol(z)))

axis(1)

mtext("Number of\nresponses per\nforest-type", side = 1, line = 4.4, las = 0, cex = 1)

## colour scale
par(mar = c(4, 2, 1, 2), xaxs = "i", yaxs = "i", bty = "n", family = "sans")
zlim <- c(0, max(z, na.rm = TRUE))
image(
  x = seq(zlim[1], zlim[2], length.out = length(cols)),
  y = 1,
  z = matrix(
    seq(zlim[1], zlim[2], length.out = length(cols)),
    nrow = length(cols),
    ncol = 1
  ), col = cols, axes = FALSE, xlab = "", ylab = "")

axis(1, at = pretty(zlim))
mtext("% species that are native", side = 1, line = 2.1, las = 0, cex = 1)
box()

dev.off()
