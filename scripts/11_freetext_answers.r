## #######################################################################

## TAMERS survey data analysis - Katalin Csillery - 23.09.2026
## Emerging topics from free-text answers

## ########################################################################

library(ggplot2)
library(grid)
setwd("~/Dropbox/projects/TAMERS_LandscapePaper/analysis/")
load("data/codes2.RData")
source("functions.r")
load("data/dat_all_translated.RData")

dat <- dat.all

questions <- c(
    G02Q01="Disturbance experience",
    G02Q02="Adaptation actions",
    G02Q06="Barriers",
    G03Q03="Knowledge needed",
    G03Q04="Information sources",
    G05Q03="Support measures"
)

codebooks <- c(
    G02Q01="experience",
    G02Q02="action",
    G02Q06="barriers",
    G03Q03="knowlneeded",
    G03Q04="infosource",
    G05Q03="support"
)

clean.match <- function(x) {
    x <- trimws(tolower(as.character(x)))
    gsub("\u00a0", " ", x, fixed=TRUE)
}

norm <- function(x) {
    tolower(gsub("[^[:alnum:]]", "", x))
}

predefined <- list()
other <- list()
audit <- list()

for(q in names(questions)) {

    vars <- intersect(
        full.name[grepl(paste0("^", q), short.name)],
        names(dat)
    )

    other.var <- vars[grepl("\\.Other", vars)]
    stopifnot(length(other.var) == 1)

    pre.vars <- setdiff(vars, other.var)
    answered <- rowSums(!is.na(dat[, vars, drop=FALSE])) > 0
    N <- sum(answered)

    ## Predefined options
    n <- colSums(dat[, pre.vars, drop=FALSE] == "Y", na.rm=TRUE)
    labels <- sub("^.*\\[([^][]+)\\]$", "\\1", pre.vars)
    labels <- gsub("_", " ", labels, fixed=TRUE)
    predefined[[q]] <- data.frame(
        question=q,
        domain=questions[q],
        category=labels,
        n=as.integer(n),
        N=N,
        pct=100*n/N
    )

    ## Coded Other answers
    cb <- read.csv(paste0("data/", codebooks[q], "_translation_codebook.csv"), stringsAsFactors=FALSE)
    cb$Original <- clean.match(cb$Original)
    cb <- cb[!duplicated(cb$Original), ]
    raw <- clean.match(dat[[other.var]])
    filled <- !is.na(raw) & nzchar(raw)
    category <- cb$Category[match(raw, cb$Original)]
    category[!filled | is.na(category) | !nzchar(trimws(category))] <- NA
    n <- sort(table(category), decreasing=TRUE)
    other[[q]] <- data.frame(
        question=rep(q, length(n)),
        domain=rep(questions[[q]], length(n)),
        category=names(n),
        n=as.integer(n),
        N=rep(N, length(n)),
        N_other=rep(sum(filled), length(n)),
        pct=100*as.integer(n)/N,
        pct_other=100*as.integer(n)/sum(filled)
    )

    audit[[q]] <- data.frame(
        question=q,
        domain=questions[q],
        N=N,
        N_other=sum(filled),
        N_coded=sum(filled & !is.na(category)),
        N_unmatched=sum(filled & is.na(category))
    )
}

predefined <- do.call(rbind, predefined)
other <- do.call(rbind, other)
audit <- do.call(rbind, audit)

predefined <- predefined[order(predefined$question, -predefined$n), ]
other <- other[order(other$question, -other$n), ]

## Existing versus newly proposed categories
other$origin <- "New topic"

for(q in names(questions)) {
    pre <- get.vars(paste0("^", q))
    pre <- clean.var(pre[!grepl("\\.Other", pre)])
    pre <- tolower(gsub("[^a-z0-9]", "", tolower(pre)))
    i <- which(other$question == q)
    for(j in i) {
        x <- tolower(gsub("[^a-z0-9]", "", tolower(other$category[j])))
        if(any(endsWith(pre, x))) other$origin[j] <- "Reclassified to a proposed topic"
    }
}

other$origin <- factor(other$origin, levels=c("New topic", "Reclassified to a proposed topic"))

## Inspect classification
other[, c("domain", "category", "origin")]
table(other$origin, other$domain)
write.csv(other, "tables/S_other_frequencies.csv", row.names=FALSE)

## Figure labels
audit$strip <- paste0(audit$domain, "\nOther: ", round(100 * audit$N_other / 580, 1), "%")
other$strip <- audit$strip[match(other$question, audit$question)]
other$strip <- factor(other$strip, levels=audit$strip)
other$label <- gsub("_", " ", other$category, fixed=TRUE)
other$label.id <- paste(other$question, other$label, sep=" | ")
other$label.id <- factor(other$label.id, levels=unique(other$label.id[order(other$n)]))

## Supplementary figure
p <- ggplot(other, aes(x=n, y=label.id, fill=origin)) +
    geom_col(width=.7) +
    geom_text(aes(label=n), hjust=-.2, size=5) +
    facet_wrap(~strip, scales="free_y", ncol=2) +
    scale_fill_manual(
        values=c("New topic"="#8B1A4A",
                 "Reclassified to a proposed topic"="#4F7DA8"),
        limits=c("New topic", "Reclassified to a proposed topic"),
        drop=FALSE,
        name=NULL
    ) +
    scale_y_discrete(labels=function(x) {
        x <- sub("^.* \\| ", "", x)
        vapply(x, function(z) paste(strwrap(z, width=60), collapse="\n"), character(1))
    }) +
    scale_x_continuous(expand=expansion(mult=c(0, .18))) +
    labs(x="Number of respondents", y=NULL) +
    guides(fill=guide_legend(nrow=1)) +
    theme_bw(base_size=14) +
    theme(
        legend.position="top",
        legend.text=element_text(size=15),
        strip.text=element_text(size=13),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=13),
        panel.grid.major.y=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing.x=unit(1.5, "lines"),
        panel.spacing.y=unit(2, "lines")
    )

ggsave("figures/SFig_emerging_topics.pdf", p, width=17, height=17)
