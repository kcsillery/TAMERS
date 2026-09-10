library(RColorBrewer)

## country groupings for regional analyses (Jaccard distance on native
## species overlap, see chat methods discussion / 5_species.r). Used as a
## general regional predictor wherever per-country sample sizes are too
## small to analyse individually. Norway is included here (assigned to
## Baltoscandia) since this is a general-purpose grouping; 5_species.r
## drops Norway BEFORE calling recode.group() specifically because its
## native species composition didn't match Baltoscandia well -- that
## exclusion is local to the nativity figure, not part of this lookup.
country.groups <- list(
    "Baltic"   = c("Finland", "Latvia"), ## identical lists
    "Scandinavia"      = c("Sweden", "Norway"), ## Sweden native: "Acer campestre" "Ulmus laevis"  
    "Denmark-Belgium" = c("Denmark", "Belgium"),##, "United Kingdom")
    "Oder-Elbe Region"  = c("Poland", "Czechia", "Germany"), ## identical lists
    "Western Balkans"   = c("Croatia", "Serbia", "Montenegro", "Kosovo", "Albania", "Bosnia & Herzegovina"), ## identical lists
    "France-Italy" = c("France", "Italy"), ## identical lists
    "Carpathian"        = c("Slovakia", "Ukraine"), ## identical lists
    "South-East Balkan" = c("North Macedonia", "Bulgaria", "Greece"), ## identical lists
    "Austria-Slovenia" = c("Slovenia", "Austria") ## identical lists
)

country.groups.minimalist <- list(
    "Baltic"   = c("Finland", "Latvia"), ## identical lists
    "Scandinavia"      = c("Sweden", "Norway"), ## Sweden native: "Acer campestre" "Ulmus laevis" 
    "Western Balkans"   = c("Croatia", "Serbia", "Montenegro", "Kosovo", "Albania", "Bosnia & Herzegovina"), ## identical lists
    "Slovakia"        = c("Slovakia", "Ukraine"), ## identical lists
    "Slovenia-Hungary" = c("Slovenia", "Hungary"),
    "South-East Balkan" = c("North Macedonia", "Bulgaria", "Greece") ## identical lists
)


## lookup: original country name -> group name; countries not listed in
## any group keep their own name
recode.group <- function(x, how="default"){
    if(how=="default"){
        group.lookup <- setNames(rep(names(country.groups),
                                     lengths(country.groups)), unlist(country.groups))
        res <- ifelse(x %in% names(group.lookup), unname(group.lookup[x]), x)
    }
    else{
        group.lookup <- setNames(rep(names(country.groups.minimalist),
                                     lengths(country.groups.minimalist)),
                                 unlist(country.groups.minimalist))
        res <- ifelse(x %in% names(group.lookup), unname(group.lookup[x]), x)
    }
    res
}

get.vars <- function(pattern) {
  full.name[grepl(pattern, short.name)]
}

## helper: recode answer codes to English labels
make.lookup <- function(qtitle) {
  z <- subset(codes2, title == qtitle)
  setNames(z$answer, z$code)
}

recode.labels <- function(x, qtitle) {
  lookup <- make.lookup(qtitle)
  out <- lookup[as.character(x)]
  out[is.na(out)] <- "No answer"
  out
}

## percentage table
pct.tab <- function(x) {
  z <- table(x, useNA = "no")
  z <- z[z > 0]
  100 * z / sum(z)
}

shorten <- function(x, n = 45) {
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 3), "..."), x)
}

clean.html <- function(x) {
  x <- gsub("&lt;", "<", x)
  x <- gsub("&gt;", ">", x)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

remove.small <- function(x, threshold = 0.5) {
  x[x >= threshold]
}

add.panel <- function(panel.label, main) {
  mtext(panel.label, side = 3, line = 1.0, adj = -0.08,
        font = 2, cex = 1.15)
  mtext(main, side = 3, line = 1.0, adj = 0.5,
        font = 2, cex = 0.9)
}

plot.barh <- function(x, panel.label, main,
                      xlab = "Respondents (%)",
                      col = "#5A5A5A",
                      left.mar = 10) {
  
  x <- x[!is.na(x)]
  x <- x[x > 0]
  
  par(mar = c(4.2, left.mar, 3.0, 1.2))
  
  barplot(
    rev(x),
    horiz = TRUE,
    las = 1,
    col = col,
    border = NA,
    xlab = xlab,
    cex.names = 0.72,
    cex.axis = 0.8,
    cex.lab = 0.85,
    xlim = c(0, max(x) * 1.15)
  )
  
  add.panel(panel.label, main)
}

plot.cross <- function(x, y, panel.label, main,
                       xlab = "",
                       ylab = "Respondents (%)",
                       cols = NULL,
                       legend.cex = 0.58,
                       bottom.mar = 8,
                       left.mar = 5) {
  
  ok <- !is.na(x) & !is.na(y) & x != "No answer" & y != "No answer"
  tab <- table(x[ok], y[ok])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0]
  prop <- prop.table(tab, 1) * 100
  
  if(is.null(cols)) cols <- terrain.colors(ncol(prop))
  
  par(mar = c(bottom.mar, left.mar, 3.0, 6))
  
  barplot(
    t(prop),
    beside = FALSE,
    col = cols,
    border = NA,
    ylab = ylab,
    xlab = xlab,
    las = 2,
    cex.names = 0.65,
    cex.axis = 0.8,
    cex.lab = 0.85
  )
  
  add.panel(panel.label, main)
  
  par(xpd = TRUE)
  legend(
    "topright",
    inset = c(-0.24, 0),
    legend = colnames(prop),
    fill = cols,
    border = NA,
    bty = "n",
    cex = legend.cex
  )
  par(xpd = FALSE)
}

plot.heat <- function(tab, panel, main, xlab, ylab, mylas.x = 0, mylas.y = 1, zmax = NULL, str45 = F) {
  
    z <- as.matrix(tab)
    z <- z[nrow(z):1, , drop = FALSE]
    z.plot <- z
    z.plot[z.plot == 0] <- NA
    
    if(is.null(zmax)) zmax <- max(z.plot, na.rm = TRUE)
    
    image(
        x = seq_len(ncol(z.plot)),
        y = seq_len(nrow(z.plot)),
        z = t(z.plot),
        axes = FALSE,
        xlab = xlab,
        ylab = "",
        col = blue.cols,
        zlim = c(0, zmax)
    )
    
    if(str45){
        axis(1, at = seq_len(ncol(z)), labels = FALSE)
        
        text(
            x = seq_len(ncol(z)),
            y = par("usr")[3] - 0.45,
            labels = colnames(z),
            srt = 45,
            adj = 1,
            xpd = TRUE,
            cex = 0.9
        )
    }
    else{
        axis(1, at = seq_len(ncol(z)), labels = colnames(z),
             las = mylas.x, cex.axis = 0.9)
    }
    axis(2, at = seq_len(nrow(z)), labels = rownames(z),
         las = mylas.y, cex.axis = 0.9)
    
    for(i in seq_len(nrow(z))) {
        for(j in seq_len(ncol(z))) {
            if(z[i, j] > 0) {
                text(j, i, z[i, j], cex = 0.9)
            }
        }
    }
    
    box()
    mtext(panel, side = 3, line = 0.25, adj = -0.08, font = 2, cex = 1.1)
    mtext(main, side = 3, line = 0.25, adj = 0.5, font = 2, cex = 0.9)
}

plot.role <- function(tab, panel, main = "Current work role/sector") {
  
  tab <- sort(tab, decreasing = TRUE)
  
  barplot(
    rev(tab),
    horiz = TRUE,
    las = 1,
    col = "grey80",
    border = "grey50",
    xlab = "Number of respondents",
    cex.names = 0.85,
    cex.axis = 0.9,
    cex.lab = 0.9,
    xlim = c(0, max(tab) * 1.15)
  )
  
  mtext(panel, side = 3, line = 0.25, adj = -0.08, font = 2, cex = 1.1)
  mtext(main, side = 3, line = 0.25, adj = 0.5, font = 2, cex = 0.9)
}

compare.groups <- function(x, group, var.name) {
  
  tab <- table(group, x)
  
  cat("\n============================\n")
  cat(var.name, "\n")
  cat("============================\n")
  
  print(tab)
  
  test <- chisq.test(tab)
  
  print(test)
  
  ## standardized residuals
  cat("\nStandardized residuals:\n")
  print(round(test$stdres, 2))
  
  ## effect size
  suppressPackageStartupMessages(library(vcd))
  
  cat("\nCramer's V:\n")
  print(assocstats(tab)$cramer)
}

## functions to check completeness for creating datasets
## ###########################################################

make.blocks <- function(vars) {
  qid <- sub("\\..*$", "", short.name[match(vars, full.name)])
  split(vars, qid)
}

answered.block <- function(dat, vars) {
  rowMeans(!is.na(dat[, vars, drop = FALSE])) >= 1 / length(vars)
}

complete.blocks <- function(dat, blocks) {
  block.done <- sapply(blocks, function(v) answered.block(dat, v))
  if(is.vector(block.done)) block.done <- matrix(block.done, ncol = 1)
  rowSums(block.done) > 0
}

## for species questions:
complete.enough <- function(dat, vars, min.prop = 0.5) {
    rowMeans(!is.na(dat[, vars, drop = FALSE])) >= min.prop
}

## functions for recoding language answers to English
## ###################################################

clean.txt <- function(x) {
  x <- gsub("\\\\'", "'", x)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&nbsp;", " ", x)
  x <- gsub("\u00a0", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

## extract dropdown labels
extract.select.labels <- function(txt) {

  z <- gregexpr('<option value="option[0-9]+">[^<]+</option>', txt, perl = TRUE)
  z <- regmatches(txt, z)[[1]]

  out <- sub('.*">([^<]+)</option>', "\\1", z)

  clean.txt(out)
}

recode.species <- function(x) {
  x2 <- clean.txt(as.character(x))
  out <- lookup[x2]
  out[is.na(x)] <- NA
  unname(out)
}


extract.checkbox.labels <- function(txt, column) {
  start.txt <- paste0("Insert checkboxes into column ", column)
  start <- regexpr(start.txt, txt, fixed = TRUE)
  if(start[1] < 0) return(character(0))

  txt2 <- substr(txt, start[1], nchar(txt))

  next.start <- regexpr(paste0("Insert checkboxes into column ", column + 1), txt2, fixed = TRUE)
  if(next.start[1] > 0) txt2 <- substr(txt2, 1, next.start[1] - 1)

  z <- gregexpr('value="[^"]+"', txt2, perl = TRUE)
  z <- regmatches(txt2, z)[[1]]

  out <- sub('value="([^"]+)"', "\\1", z)
  clean.txt(out)
}

make.checkbox.lookup <- function(column) {
  eng.txt <- question.l10n$question[question.l10n$title == "G06Q02d" & question.l10n$language == "en"][1]
  eng.labels <- extract.checkbox.labels(eng.txt, column)

  lookup.list <- list()

  for(i in seq_len(nrow(question.l10n))) {
    labs <- extract.checkbox.labels(question.l10n$question[i], column)

    if(length(labs) == length(eng.labels)) {
      lookup.list[[length(lookup.list) + 1]] <- data.frame(from = labs, to = eng.labels, stringsAsFactors = FALSE)
    }
  }

  out <- unique(do.call(rbind, lookup.list))
  out$from <- clean.txt(out$from)
  out$to <- clean.txt(out$to)
  out
}

recode.multi <- function(x, lookup.df) {
  map <- lookup.df$to
  names(map) <- lookup.df$from

  out <- rep(NA_character_, length(x))

  for(i in seq_along(x)) {
    if(is.na(x[i]) || x[i] == "") next

    parts <- trimws(unlist(strsplit(as.character(x[i]), ",")))
    parts <- clean.txt(parts)

    rec <- unname(map[parts])
    rec <- rec[!is.na(rec)]

    if(length(rec) > 0) out[i] <- paste(unique(rec), collapse = ",")
  }

  out
}


## helper to split comma-separated multiple-choice answers into 0/1 columns
make.dummies <- function(dat, vars, options, prefix) {
  out <- data.frame(row.names = seq_len(nrow(dat)))

  for(v in vars) {
    base <- gsub("[^A-Za-z0-9]+", "_", v)
    base <- gsub("_+$", "", base)

    for(opt in options) {
      cname <- paste(prefix, base, gsub("[^A-Za-z0-9]+", "_", opt), sep = "__")
      out[[cname]] <- as.integer(grepl(paste0("(^|,)", opt, "(,|$)"), dat[[v]]))
      out[[cname]][is.na(dat[[v]])] <- NA
    }
  }

  out
}


## shorten names in the species dataset
## ###########################################
clean.species.analysis.names <- function(nm) {

  out <- make.names(nm)
  out <- gsub("\\.", "_", out)
  out <- gsub("_+", "_", out)

  ## "Other" is the only species token that exists in BOTH grids (conifer
  ## and broadleaf each have their own "Other" row). Without a qualifier,
  ## both collapse to the identical name (e.g. "choice_Other"), silently
  ## losing one of the two whenever the result is subset by name. Tag it
  ## using the *original* (pre-cleaning) text, which still distinguishes
  ## "coniferous" from "broadleaved".
  is.conifer   <- grepl("coniferous", nm, ignore.case = TRUE)
  is.broadleaf <- grepl("broadleaved", nm, ignore.case = TRUE)

  get.species <- function(x) {
    sp <- sub(".*_([A-Za-z]+_[A-Za-z]+|Other)_(Your_choice|why_yes|Why_yes|why_no|Why_no).*", "\\1", x)
    ifelse(sp == x, NA, sp)
  }

  is.choice <- grepl("_Your_choice_?$", out)
  is.whyyes <- grepl("_why_yes_?$|_Why_yes_?$", out)
  is.whyno <- grepl("_why_no_?$|_Why_no_?$", out)
  is.whyyes.bin <- grepl("^why_yes_", out)
  is.whyno.bin <- grepl("^why_no_", out)

  sp <- get.species(out)
  sp[sp == "Other" & is.conifer]   <- "Other_conifer"
  sp[sp == "Other" & is.broadleaf] <- "Other_broadleaf"

  out[is.choice] <- paste0("choice_", sp[is.choice])
  out[is.whyyes & !is.whyyes.bin] <- paste0("why_yes_", sp[is.whyyes & !is.whyyes.bin])
  out[is.whyno & !is.whyno.bin] <- paste0("why_no_", sp[is.whyno & !is.whyno.bin])

  out[is.whyyes.bin] <- sub("^(why_yes)_.*_([A-Za-z]+_[A-Za-z]+|Other)_+(why_yes|Why_yes)_(.*)$", "\\1__\\2__\\4", out[is.whyyes.bin])
  out[is.whyno.bin] <- sub("^(why_no)_.*_([A-Za-z]+_[A-Za-z]+|Other)_+(why_no|Why_no)_(.*)$", "\\1__\\2__\\4", out[is.whyno.bin])

  ## the dummy-coded (why_yes/why_no binary) names go through the sub()
  ## above rather than the sp[] vector, so disambiguate "Other" here too
  out[is.whyyes.bin & is.conifer]   <- gsub("__Other__", "__Other_conifer__", out[is.whyyes.bin & is.conifer])
  out[is.whyyes.bin & is.broadleaf] <- gsub("__Other__", "__Other_broadleaf__", out[is.whyyes.bin & is.broadleaf])
  out[is.whyno.bin & is.conifer]    <- gsub("__Other__", "__Other_conifer__", out[is.whyno.bin & is.conifer])
  out[is.whyno.bin & is.broadleaf]  <- gsub("__Other__", "__Other_broadleaf__", out[is.whyno.bin & is.broadleaf])

  out <- gsub("_+", "_", out)
  out <- gsub("_$", "", out)
  out
}

## if sepcies choice was selected, and none of the why, all the why
## options should get 0 and not NA
fix.reason.na <- function(bin.dat, choice.dat, reason.type) {

  for(sp in names(choice.dat)) {

    sp.clean <- gsub("^choice_", "", sp)

##      reason.cols <- grep(paste0("^", reason.type, "__", sp.clean, "__"), names(bin.dat), value = TRUE)
      reason.cols <- grep(paste0("^", reason.type, "_", sp.clean, "_"), names(bin.dat), value = TRUE)

    if(length(reason.cols) > 0) {
      evaluated <- !is.na(choice.dat[[sp]])
      bin.dat[evaluated, reason.cols] <- lapply(bin.dat[evaluated, reason.cols, drop = FALSE], function(x) {
        x[is.na(x)] <- 0
        x
      })
    }
  }

  bin.dat
}


## functions for 7_AMscores
## ############################
plot.pca.group <- function(pca.obj, group, main) {
    pc.var <- round(100 * summary(pca.obj)$importance[2, 1:2], 1)
    group <- factor(group)
    cols <- hcl.colors(nlevels(group), "Dark 3")
    plot(pca.obj$x[,1], pca.obj$x[,2], pch = 16, col = adjustcolor(cols[group], alpha.f = 0.75),
         xlab = paste0("PC1 (", pc.var[1], "%)"), ylab = paste0("PC2 (", pc.var[2], "%)"), main = main)
    legend("topright", legend = levels(group), col = cols, pch = 16, bty = "n", cex=.8)
}

draw.scale <- function(cols, labs) {
  usr <- par("usr")
  x <- seq(usr[1] + 0.15 * diff(usr[1:2]), usr[1] + 0.85 * diff(usr[1:2]), length.out = length(cols) + 1)
  y1 <- usr[3] - 0.10 * diff(usr[3:4])
  y2 <- usr[3] - 0.06 * diff(usr[3:4])
  par(xpd = TRUE)
  rect(x[-length(x)], y1, x[-1], y2, col = cols, border = NA)
  text(seq(x[1], x[length(x)], length.out = length(labs)), y1 - 0.03 * diff(usr[3:4]), labs, cex = 0.75)
  par(xpd = FALSE)
}

recode.accept <- function(x) {
  out <- rep(NA_real_, length(x))
  out[x %in% accept.text]  <- 1
  out[x %in% reject.text]  <- -1
  out[x %in% neutral.text] <- 0
  out
}


clean.var <- function(x) {
  x <- gsub("\u00a0", " ", x)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub(".*\\[([^\\[]+)\\]$", "\\1", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

clean.match <- function(x){
    x <- trimws(tolower(x))
    x <- gsub("\u00a0", " ", x)
    x
}

recode.other <- function(dat, all.vars, codebook.file, category.col = "Category", original.col = "Original", extra.prefix, exclude.extra = character(0)) {
    other.var <- all.vars[grepl(".Other", all.vars)]
    if(length(other.var) != 1) stop("Expected exactly one .Other column")

    cb <- read.csv(codebook.file, stringsAsFactors = FALSE)
    names(cb) <- clean.var(names(cb))
    cb[[original.col]] <- clean.match(cb[[original.col]])
    cb[[category.col]] <- clean.var(cb[[category.col]])

    txt <- clean.match(dat[[other.var]])
    answered <- rowSums(!is.na(dat[, all.vars, drop = FALSE])) > 0

    cats <- setdiff(unique(cb[[category.col]]), c(NA, exclude.extra))

    for(cat in cats) {
        target <- paste0(extra.prefix, cat)
        if(!target %in% names(dat)) dat[[target]] <- ifelse(answered, 0L, NA_integer_)
    }

    for(i in which(!is.na(txt) & txt != "")) {
        hit <- match(txt[i], cb[[original.col]])
        if(is.na(hit)) next
        cat <- cb[[category.col]][hit]
        if(cat %in% cats) dat[i, paste0(extra.prefix, cat)] <- 1L
    }

    dat
}


## standardized text for matching raw free-text answers to the codebooks
clean.other <- function(x){
    x <- as.character(x)
    x <- gsub("\u00a0", " ", x)
    x <- trimws(tolower(x))
    x <- gsub("[[:space:]]+", " ", x)
    x
}

## standardized species names for matching codebook species to proposed species
clean.species <- function(x){
    x <- as.character(x)
    x <- gsub("\u00a0", " ", x)
    x <- gsub("_", " ", x)
    x <- trimws(tolower(x))
    x <- gsub("[[:space:]]+", " ", x)
    x
}

## automatically identify the original Other-species column represented by a codebook
find.other.var <- function(dat, cb){
    cb.text <- unique(clean.other(cb$`Original text`))
    candidate.vars <- names(dat)[vapply(dat, function(x) is.character(x) || is.factor(x), logical(1))]
    overlap <- vapply(candidate.vars, function(v) sum(clean.other(dat[[v]]) %in% cb.text, na.rm = TRUE), numeric(1))
    if(max(overlap) == 0) stop("No matching Other-species column found for the codebook")
    if(sum(overlap == max(overlap)) > 1) warning("Several columns had the same maximum codebook overlap; using the first")
    candidate.vars[which.max(overlap)]
}


## main workhorse function to add species proposed on Other species
add.other.species <- function(i, raw.answer, cb, already.seen = character(0)){
    if(is.na(raw.answer) || trimws(raw.answer) == "") return(list(seen = already.seen, new = 0L, reassigned = 0L))

    hit <- match(clean.other(raw.answer), cb$match.text)
    if(is.na(hit)) return(list(seen = already.seen, new = 0L, reassigned = 0L))

    species <- unlist(strsplit(as.character(cb$Species[hit]), "/", fixed = TRUE))
    species <- trimws(species)
    species <- species[!is.na(species) & species != ""]
    species.clean <- clean.species(species)

    ## avoid counting the same species twice within one respondent
    keep <- !species.clean %in% already.seen
    species <- species[keep]
    species.clean <- species.clean[keep]

    new.n <- 0L
    reassigned.n <- 0L

    for(j in seq_along(species.clean)){
        s <- species.clean[j]

        ## "many" is treated as one additional recommendation
        if(s == "many"){
            new.n <- new.n + 1L
            next
        }

        proposed.hit <- match(s, proposed.species.clean)

        if(!is.na(proposed.hit)){
            v <- names(proposed.species.clean)[proposed.hit]

            ## If already accepted, nothing changes.
            ## If rejected or answered neutrally, the Other recommendation
            ## supersedes that answer and is recoded as accepted.
            if(is.na(X[i, v]) || X[i, v] != 1){
                X[i, v] <- 1
                reassigned.n <- reassigned.n + 1L
            }
        } else {
            ## genuinely new species or genus: one point per listed item
            new.n <- new.n + 1L
        }
    }

    list(seen = unique(c(already.seen, species.clean)), new = new.n, reassigned = reassigned.n)
}


## function for correcting the nativer score with new species
get.other.species <- function(raw.answer, cb){
    if(is.na(raw.answer) || trimws(raw.answer) == "") return(character(0))

    hit <- match(clean.other(raw.answer), cb$match.text)
    if(is.na(hit)) return(character(0))

    species <- unlist(strsplit(as.character(cb$Species[hit]), "/", fixed = TRUE))
    species <- trimws(species)
    species <- species[!is.na(species) & species != ""]
    species
}


## for parts b and c of 8_models_figure.r
make.div.cols <- function(pal, n = 101, mid = "#C8C8C8") {

    p <- brewer.pal(11, pal)
    nside <- (n - 1) / 2

    c(colorRampPalette(c(p[1], mid))(nside + 1)[1:nside],
      mid,
      colorRampPalette(c(mid, p[11]))(nside + 1)[-1])
}



draw.scale.panel <- function(cols, zlim, title, cex.title = 0.95, cex.scale = 0.85, bar.frac = 0.65) {
    par(mar = c(0, 0.5, 0, 0.5), bty = "n")
    plot.new()
    xmax <- zlim / bar.frac
    plot.window(xlim = c(-xmax, xmax), ylim = c(0, 1), xaxs = "i", yaxs = "i")
    text(0, 0.92, title, cex = cex.title)
    xx <- seq(-zlim, zlim, length.out = length(cols) + 1)
    rect(xx[-length(xx)], 0.38, xx[-1], 0.68, col = cols, border = NA)
    ticks <- seq(-zlim, zlim, length.out = 5)
    text(ticks, 0.18, labels = round(ticks, 2), cex = cex.scale)
}

map.col <- function(z, sig, zlim, cols) {

    if (is.na(z))
        return("#F2F2F2")

    ## constrain values to plotted scale
    z <- max(-zlim, min(zlim, z))

    ## exact position between -zlim and +zlim
    ind <- round((z + zlim) / (2 * zlim) * (length(cols) - 1)) + 1

    out <- cols[ind]

    ## fade non-significant estimates, but not to near-white
    if (!isTRUE(sig)) {
        x <- col2rgb(out)
        bg <- col2rgb("#E5E5E5")
        x <- 0.55 * x + 0.45 * bg
        out <- rgb(x[1], x[2], x[3], maxColorValue = 255)
    }

    out
}

boxed.text <- function(x, y, label, col = "grey35", cex = 0.85, font = 1, adj = c(0, 0.5)) {
    w <- strwidth(label, cex = cex)
    h <- strheight(label, cex = cex)
    rect(x - 0.02*w, y - 0.65*h, x + 1.02*w, y + 0.65*h,
         col = "white", border = NA)
    text(x, y, labels = label, col = col, cex = cex,
         font = font, adj = adj)
}
