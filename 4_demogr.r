## #######################################################################

## TAMERS survey data analysis, Katalin Csillery, 8 Sept 2026

## 4. Respondent profile figure

## ########################################################################

load("data/dat_all.RData")
load("data/codes2.RData")
source("functions.r")

time.vars <- grep("_time", names(dat.all))
dat.all <- dat.all[, -time.vars]

## split variable names into short and long versions
name.split <- strsplit(names(dat.all), "\\.\\.", fixed = FALSE)
short.name <- sapply(name.split, `[`, 1)
full.name <- sapply(name.split, function(x) {
  if(length(x) > 1) {
    paste(x[-1], collapse = "..")
  } else {
    x
  }
})

names(dat.all) <- full.name

## variables
country.var <- get.vars("^G01Q01$")
age.var <- get.vars("^G01Q02$")
degree.var <- get.vars("^G01Q03$")
field.var <- get.vars("^G01Q04$")
occupation.var <- get.vars("^G01Q05$")
research.role.var <- get.vars("^G01Q06$")
forestry.role.var <- get.vars("^G01Q66$")
years.exper.var <- get.vars("^G01Q07$")
ref.forest.var <- get.vars("^G02Q30$")

## NOTE: this is the actual "MyGardenOfTrees" question -- G10Q35 (used
## previously) is an unrelated, generic "are you part of any research
## project/network" question. G07Q32 is the one that specifically asks
## about MyGardenOfTrees participation (https://www.mygardenoftrees.eu).
## G10Q35 is kept too, as its own separate flag (research.network) below.
mygarden.var <- get.vars("^G07Q32$")
research.network.var <- get.vars("^G10Q35$")

## recode variables
country <- clean.html(recode.labels(dat.all[[country.var]], "G01Q01"))
age <- clean.html(recode.labels(dat.all[[age.var]], "G01Q02"))
degree <- clean.html(recode.labels(dat.all[[degree.var]], "G01Q03"))
field <- clean.html(recode.labels(dat.all[[field.var]], "G01Q04"))
occupation <- clean.html(recode.labels(dat.all[[occupation.var]], "G01Q05"))
research.role <- clean.html(recode.labels(dat.all[[research.role.var]], "G01Q06"))
forestry.role <- clean.html(recode.labels(dat.all[[forestry.role.var]], "G01Q66"))
years.exper <- clean.html(recode.labels(dat.all[[years.exper.var]], "G01Q07"))
ref.forest <- clean.html(recode.labels(dat.all[[ref.forest.var]], "G02Q30"))
save(country, ref.forest, file="data/country_ftype.RData")

## simplify labels
degree <- gsub("Completed mandatory school",
               "Mandatory school", degree)
degree <- gsub("Technical training, apprenticeship \\(2-3 years\\)",
               "Technical training", degree)
degree <- gsub("Bachelor \\(or equivalent 3-4 years University degree\\)",
               "Bachelor", degree)

field <- gsub("Agronomy/Agricultural Sciences", "Agronomy", field)
field <- gsub("Public Administration", "Governance & Policy", field)
field <- gsub("Ecology/Evolution", "Ecology & Evolution", field)
field <- gsub("Forest technician", "Forestry", field)

## short, consistent labels for occupation
occupation <- gsub("^Forestry sector$", "forester", occupation)
occupation <- gsub("^Research$", "researcher", occupation)
occupation <- gsub("^Other$", "other", occupation)
occupation <- factor(occupation, levels = c("forester", "researcher", "other"))

years.exper <- gsub("< 5", "<5", years.exper)
age <- gsub("< 20", "<20", age)

## detailed occupation category
detailed.role <- rep(NA_character_, nrow(dat.all))

is.forestry <- dat.all[[occupation.var]] == "AO01"
is.research <- dat.all[[occupation.var]] == "AO02"
is.other <- dat.all[[occupation.var]] == "AO03"

detailed.role[is.forestry] <- forestry.role[is.forestry]
detailed.role[is.research] <- research.role[is.research]
detailed.role[is.other] <- "Other occupation"

detailed.role <- clean.html(detailed.role)
detailed.role <- gsub("[[:space:]\u00A0]+", " ", detailed.role)
detailed.role <- trimws(detailed.role)

detailed.role[detailed.role == "Academia"] <- "Researcher in academia"
detailed.role[detailed.role %in% c("Private", "Not-for-profit", "Consulting")] <- "Researcher in private sector"
detailed.role[detailed.role == "Government"] <- "Researcher in public sector"
detailed.role[detailed.role == "A forest manager"] <- "Forest manager"
detailed.role[detailed.role == "A forest owner"] <- "Forest owner"
detailed.role[detailed.role == "Both forest owner and manager"] <- "Forest owner and manager"
detailed.role[detailed.role == "A forest administrator"] <- "Public administration"

## correct inconsistencies
tmp <- which(degree == "Doctorate" & age == "<20")
age[tmp] <- "20-30"

## explicit orders -- field is nominal (no natural order), so it gets a
## display order via levels= without being flagged ordered=TRUE; age,
## degree, years.exper genuinely are ordinal
age <- factor(age, levels = c("<20", "20-30", "31-40", "41-50", "51-60", ">60"), ordered = TRUE)
degree <- factor(degree, levels = c("Mandatory school", "Technical training", "Bachelor", "Master", "Doctorate"), ordered = TRUE)
field <- factor(field, levels = c("Forestry", "Agronomy", "Ecology & Evolution", "Governance & Policy", "Other"))
years.exper <- factor(years.exper, levels = c("<5", "5-10", "10-20", ">20"), ordered = TRUE)

## MyGardenOfTrees participation: G07Q32 has four options, not a Yes/No,
## so this is coded TRUE for actual participation (AO01: participant,
## AO02: helps collect data from a micro-garden), FALSE for AO03/AO04
## (approached-but-not-participating / never approached), and NA for
## anyone who never reached this question (End-of-survey group).
mygarden.raw <- dat.all[[mygarden.var]]
mygarden <- rep(NA, nrow(dat.all))
mygarden[mygarden.raw %in% c("AO01", "AO02")] <- TRUE
mygarden[mygarden.raw %in% c("AO03", "AO04")] <- FALSE
table(mygarden.raw, mygarden, useNA = "ifany")

## general research-project/network participation (any project, not
## specifically MyGardenOfTrees) -- G10Q35 is a plain Yes/No question
research.network.raw <- dat.all[[research.network.var]]
research.network <- rep(NA, nrow(dat.all))
research.network[research.network.raw == "AO01"] <- TRUE
research.network[research.network.raw == "AO02"] <- FALSE
table(research.network.raw, research.network, useNA = "ifany")

age.exper <- table(age, years.exper)
field.degree <- table(field, degree)

## who are the forestry practicioners (ie model group used later in the mixed models)
experience.vars <- get.vars("^G02Q0[1-6]")
has.experience.data <- rowSums(!is.na(dat.all[, experience.vars])) > 0

occupation.model <- ifelse(occupation == "researcher" & !has.experience.data,
                           "researcher", "forester")
occupation.model[is.na(occupation)] <- NA
occupation.model <- factor(occupation.model, levels=c("forester","researcher"))

role.tab <- table(detailed.role, occupation.model)
role.tab <- role.tab[order(rowSums(role.tab), decreasing=TRUE),,drop=FALSE]

## define colors for the plot
blue.cols <- hcl.colors(30, "Blues 3", rev=TRUE)[5:25]

forester.roles <- c("Forest manager", "Public administration",
                    "Forest owner and manager", "Forest owner")

role.blue.dark <- hcl.colors(30, "Greens", rev=TRUE)[3]
role.blue.light <- hcl.colors(30, "Greens", rev=TRUE)[18]
role.cols <- c(role.blue.light, role.blue.dark)

## figure
pdf("figures/Fig2_respondent_profiles.pdf", width=13, height=4)

layout(matrix(1:3, nrow=1), widths=c(1.1,1.6,1.6))

par(las=1, bty="o", cex=1, family="sans", cex.lab=1.1)

## panel a
par(mar=c(5.2,5,1.6,0.2))
plot.heat(age.exper[-1,], "a", "Age and experience",
          "", "", zmax=max(age.exper, na.rm=TRUE))
mtext("Age class", 2, line=4, las=0)
mtext("Forestry-related experience in years", 1, line=2.5, las=0)

## panel b
par(mar=c(7.5,11,1.6,0.8))
plot.heat(field.degree, "b", "Education profile", "", "", zmax=max(field.degree, na.rm=TRUE), str45=T)
mtext("Field of studies", 2, line=8.8, las=0)
mtext("Highest degree", 1, line=6, las=0)

## panel c
par(mar=c(5.2,12,1.6,3.2), xpd=T)

role.plot <- role.tab[nrow(role.tab):1,,drop=FALSE]

barplot(t(role.plot), horiz=TRUE, beside=FALSE,
        col=role.cols, border="grey45",
        names.arg=rownames(role.plot), las=1, 
        xlab="Number of respondents", cex.lab=0.98)
mtext("c", side = 3, line = 0.25, adj = -0.22, font = 2, cex = 1.1)
mtext("Current work role/sector", side = 3, line = 0.25, adj = 0.5, font = 2, cex = 0.9)

legend(35, 2,
       legend=c("Forestry practitioner","Researcher"),
       fill=role.cols, border="grey45", bty="n")

dev.off()

## #########################################################################
## attach recoded demographic variables to dat.all and save everything
## 6_datasets.r needs to build the final analysis dataset. No row removal,
## no column reordering, no separate meta.vars bookkeeping here -- that
## all now lives in 6_datasets.r.
## #########################################################################

dat.all$country <- country
dat.all$age <- age
dat.all$degree <- degree
dat.all$field <- field
dat.all$years.exper <- years.exper
dat.all$occupation <- occupation
dat.all$detailed.role <- detailed.role
dat.all$ref.forest <- ref.forest
dat.all$mygarden <- mygarden
dat.all$research.network <- research.network

dat.all$research.network[dat.all$mygarden] <- TRUE

## country as text, via the codes2 lookup directly (kept as a cross-check
## against the recode.labels()-derived `country` above -- the two should
## always agree; if they ever don't, something upstream is wrong)
country.codes <- unique(codes2[codes2$title == "G01Q01", c("code", "answer")])
country.lookup <- country.codes$answer
names(country.lookup) <- country.codes$code
dat.all$Country <- country.lookup[as.character(dat.all[[country.var]])]
if(!identical(as.character(dat.all$country), as.character(dat.all$Country))) {
  warning("dat.all$country and dat.all$Country disagree for ",
          sum(as.character(dat.all$country) != as.character(dat.all$Country), na.rm = TRUE),
          " rows -- check recode.labels() vs the codes2 lookup.")
}

save(dat.all, full.name, short.name, file = "data/dat_all_demographics.RData")
