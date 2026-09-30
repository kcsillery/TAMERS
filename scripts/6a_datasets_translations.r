## #######################################################################

## TAMERS survey data analysis - Katalin Csillery - 07.07.2026

## building ONE analysis-ready dataset for 7_AMscore.r

## ########################################################################

library(XML)
load("data/dat_all_demographics.RData")  ## produced by 4_demogr_figures_MGOTtests.r
load("data/codes2.RData")
source("functions.r")

required.vars <- c("Response_ID", "country", "Country", "ref.forest", "years.exper",
                    "degree", "age", "field", "detailed.role", "occupation",
                    "mygarden", "research.network", "Last_page")

## completion level
dat.all$lastpage <- dat.all$Last_page
stage.levels <- c("Personal info or earlier", "Reached Knowledge",
                   "Reached Conifer species", "Reached Broadleaf species",
                   "Reached Governance", "Completed")
dat.all$completion.stage <- factor(
  stage.levels[findInterval(dat.all$lastpage, c(-Inf, 6, 7, 8, 9, 10, Inf))],
  levels = stage.levels, ordered = TRUE
)
table(dat.all$completion.stage, dat.all$lastpage, exclude = NULL)

## experience / knowledge / governance
experience.vars <- get.vars("^G02Q0[1-6]")
knowledge.vars  <- get.vars("^G03Q0[1-4]")
governance.vars <- get.vars("^G05Q")

dat.all$has.experience.data <- rowSums(!is.na(dat.all[, experience.vars])) > 0
dat.all$has.knowledge.data  <- rowSums(!is.na(dat.all[, knowledge.vars])) > 0
dat.all$has.governance.data <- rowSums(!is.na(dat.all[, governance.vars])) > 0

table(dat.all$occupation, dat.all$has.experience.data, exclude = NULL)
table(dat.all$occupation, dat.all$has.knowledge.data, exclude = NULL)

## occupation.model: which of the two planned models a respondent
## belongs in. A handful of "researcher" respondents have Experience
## answers (left over from before they revised their occupation answer
## mid-session -- confirmed genuine via groupTime788, not junk data)
## => they're redirected to the forester-style model so that real,
## usable experience data isn't thrown away
dat.all$occupation.model <- ifelse(dat.all$occupation == "researcher" & !dat.all$has.experience.data,
                                    "researcher", "forester")
dat.all$occupation.model[is.na(dat.all$occupation)] <- NA
dat.all$occupation.model <- factor(dat.all$occupation.model, levels = c("forester", "researcher"))

table(dat.all$occupation, dat.all$occupation.model, exclude = NULL)

## #########################################################################
## species: completion + threshold for the sum-based AM scores
## #########################################################################

## extract suggested Other species
species.vars <- na.omit(get.vars("^G06Q"))
other.conifer <- unique(dat.all[, "G06Q28"]) ## conifer species (1 or more) in the Other box
other.broad <- unique(dat.all[, "G07Q29"]) ## broadleaf species (1 or more) in the Other box

species.choice.vars <- species.vars[grep("..Your_choice", species.vars)]
species.whyyes.vars <- species.vars[c(grep("..why_yes.", species.vars, fixed=T),
                                      grep("..Why_yes.", species.vars, fixed=T))]
species.whyno.vars  <- species.vars[c(grep("..why_no.", species.vars, fixed=T),
                                      grep("..Why_no.", species.vars, fixed=T))]
length(species.choice.vars); length(species.whyyes.vars); length(species.whyno.vars)
## all 3 are length 32, good

## completion measured on NAMED-species choice columns only
named.choice.vars <- species.choice.vars[!grepl("Other", species.choice.vars)]
dat.all$species.completion <- rowMeans(!is.na(dat.all[, named.choice.vars]))
##round(quantile(dat.all$species.completion, c(0, .1, .25, .5, .75, .9, .95, 1)), 3)

## #########################################################################
## species answers: English translation (kept as readable text) +
## numeric/binary versions for the AM scores
doc <- xmlParse("data/limesurvey_survey_116296_question_code.lss")

ql <- getNodeSet(doc, "//question_l10ns/rows/row")
question.l10n <- data.frame(
  qid = sapply(ql, function(x) xmlValue(x[["qid"]])),
  language = sapply(ql, function(x) xmlValue(x[["language"]])),
  question = sapply(ql, function(x) xmlValue(x[["question"]])),
  stringsAsFactors = FALSE
)

qs <- getNodeSet(doc, "//questions/rows/row")
questions <- data.frame(
  qid = sapply(qs, function(x) xmlValue(x[["qid"]])),
  title = sapply(qs, function(x) xmlValue(x[["title"]])),
  stringsAsFactors = FALSE
)

question.l10n <- merge(question.l10n, questions, by = "qid")
question.l10n <- question.l10n[question.l10n$title %in% c("G06Q02d", "G06Q03d"), ]

eng.txt <- question.l10n$question[question.l10n$title == "G06Q02d" & question.l10n$language == "en"][1]
eng.labels <- extract.select.labels(eng.txt)

lookup.list <- list()
for(i in seq_len(nrow(question.l10n))) {
  labs <- extract.select.labels(question.l10n$question[i])
  if(length(labs) == length(eng.labels)) {
    lookup.list[[length(lookup.list) + 1]] <- data.frame(from = labs, to = eng.labels, stringsAsFactors = FALSE)
  }
}
species.lookup <- unique(do.call(rbind, lookup.list))
species.lookup$from <- clean.txt(species.lookup$from)
species.lookup$to <- clean.txt(species.lookup$to)

## recode choice answers to English (in place, on dat.all)
lookup <- species.lookup$to
names(lookup) <- species.lookup$from
for(v in species.choice.vars) dat.all[[v]] <- recode.species(dat.all[[v]])

## recode why_yes / why_no answers to English (in place, on dat.all)
whyyes.lookup <- make.checkbox.lookup(2)
whyno.lookup <- make.checkbox.lookup(3)
for(v in species.whyyes.vars) dat.all[[v]] <- recode.multi(dat.all[[v]], whyyes.lookup)
for(v in species.whyno.vars) dat.all[[v]] <- recode.multi(dat.all[[v]], whyno.lookup)

## dummy columns for why_yes / why_no multiple-choice options
whyyes.options <- unique(whyyes.lookup$to)
whyno.options <- unique(whyno.lookup$to)
whyno.options <- whyno.options[whyno.options != "'+val+'"]

dat.species.whyyes.bin <- make.dummies(dat.all, species.whyyes.vars, whyyes.options, "why_yes")
dat.species.whyno.bin <- make.dummies(dat.all, species.whyno.vars, whyno.options, "why_no")

## clean species column names
is.species.col <- names(dat.all) %in% species.vars
names(dat.all)[is.species.col] <- clean.species.analysis.names(names(dat.all)[is.species.col])
names(dat.species.whyyes.bin) <- clean.species.analysis.names(names(dat.species.whyyes.bin))
names(dat.species.whyno.bin) <- clean.species.analysis.names(names(dat.species.whyno.bin))

choice.vars <- grep("^choice_", names(dat.all), value = TRUE)
choice.text <- dat.all[, choice.vars]   ## English text 

## if a species' choice was answered, its why_yes/why_no reasons should
## be 0 (evaluated, reason not cited) rather than NA
dat.species.whyyes.bin <- fix.reason.na(dat.species.whyyes.bin, choice.text, "why_yes")
dat.species.whyno.bin <- fix.reason.na(dat.species.whyno.bin, choice.text, "why_no")

names(dat.species.whyyes.bin) <- paste0(names(dat.species.whyyes.bin), "_bin")
names(dat.species.whyno.bin) <- paste0(names(dat.species.whyno.bin), "_bin")

dat.all <- cbind(dat.all, dat.species.whyyes.bin, dat.species.whyno.bin)

## ###################################
## G02Q02: adaptation actions
## ###################################
g02q02.all <- get.vars("^G02Q02")
g02q02.vars <- g02q02.all[!grepl(".Other", g02q02.all)]
g02q02.short <- paste0("action_", clean.var(g02q02.vars))
g02q02.short <- sub("^action_Please_select_all_forest_management_actions_you_have_conducted_so_far_in_your_reference_forest_to_adapt_or_mitigate_the_negative_effects_of_climate_change_by_planting_we_mean_either_seedlings_or_sowing_seeds_", "action_", g02q02.short)

answered <- rowSums(!is.na(dat.all[, g02q02.all, drop = FALSE])) > 0
for(i in seq_along(g02q02.short)){
    x <- dat.all[[g02q02.vars[i]]]
    z <- rep(NA_integer_, length(x))
    z[answered] <- 0
    z[answered & x == "Y"] <- 1
    dat.all[[g02q02.short[i]]] <- z
}

dat.all <- recode.other(dat.all, all.vars = g02q02.all, codebook.file = "data/action_translation_codebook.csv", extra.prefix = "action_")

## ###########################################################
## G05Q03: support measures
## ###########################################################
g05q03.all <- get.vars("^G05Q03")
g05q03.vars <- g05q03.all[!grepl(".Other", g05q03.all)]
g05q03.short <- paste0("support_", clean.var(g05q03.vars))
g05q03.short <- sub("Which_of_the_following_measures_would_more_effectively_support_the_adaptation_of_forests_to_climate_change_in_the_country_where_you_work_", "", g05q03.short)

answered <- rowSums(!is.na(dat.all[, g05q03.all, drop = FALSE])) > 0
for(i in seq_along(g05q03.short)){
    x <- dat.all[[g05q03.vars[i]]]
    z <- rep(NA_integer_, length(x))
    z[answered] <- 0
    z[answered & x == "Y"] <- 1
    dat.all[[g05q03.short[i]]] <- z
}

dat.all <- recode.other(dat.all, all.vars = g05q03.all, codebook.file = "data/support_translation_codebook.csv", extra.prefix = "support_")

## ###########################################################
## G02Q01: experience with damage/disturbance
## ###########################################################
g02q01.all <- get.vars("^G02Q01")
g02q01.vars <- g02q01.all[!grepl(".Other", g02q01.all)]
g02q01.short <- paste0("experience_", clean.var(g02q01.vars))
g02q01.short <- sub("I_observed_the_following_events_increasing_in_my_reference_forest_in_the_past_years_", "", g02q01.short)

answered <- rowSums(!is.na(dat.all[, g02q01.all, drop = FALSE])) > 0
for(i in seq_along(g02q01.short)){
    x <- dat.all[[g02q01.vars[i]]]
    z <- rep(NA_integer_, length(x))
    z[answered] <- 0
    z[answered & x == "Y"] <- 1
    dat.all[[g02q01.short[i]]] <- z
}

dat.all <- recode.other(dat.all, all.vars = g02q01.all, codebook.file = "data/experience_translation_codebook.csv", extra.prefix = "experience_")


## #####################################################################
## G02Q06: barriers
## #####################################################################
g02q06.all <- get.vars("^G02Q06")
g02q06.vars <- g02q06.all[!grepl(".Other", g02q06.all)]
g02q06.short <- paste0("barrier_", clean.var(g02q06.vars))
g02q06.short <- sub("If_you_tried_to_introduce_forest_reproductive_material_not_coming_from_natural_regeneration_in_your_reference_forest_in_the_past_what_challenges_or_barriers_have_you_experienced_", "", g02q06.short)

answered <- rowSums(!is.na(dat.all[, g02q06.all, drop = FALSE])) > 0
for(i in seq_along(g02q06.short)){
    x <- dat.all[[g02q06.vars[i]]]
    z <- rep(NA_integer_, length(x))
    z[answered] <- 0
    z[answered & x == "Y"] <- 1
    dat.all[[g02q06.short[i]]] <- z
}

dat.all <- recode.other(dat.all, all.vars = g02q06.all, codebook.file = "data/barriers_translation_codebook.csv", extra.prefix = "barrier_")


## #####################################################################
## G03Q04: information sources
## #####################################################################
g03q04.all <- get.vars("^G03Q04")
g03q04.vars <- g03q04.all[!grepl(".Other", g03q04.all)]
g03q04.short <- paste0("infosource_", clean.var(g03q04.vars))
g03q04.short <- gsub("_Which_source_s_of_information_related_to_adaptive_forests_management_do_you_use_mark_all_that_apply", "", g03q04.short)

answered <- rowSums(!is.na(dat.all[, g03q04.all, drop = FALSE])) > 0
for(i in seq_along(g03q04.short)){
    x <- dat.all[[g03q04.vars[i]]]
    z <- rep(NA_integer_, length(x))
    z[answered] <- 0
    z[answered & x == "Y"] <- 1
    dat.all[[g03q04.short[i]]] <- z
}

dat.all <- recode.other(dat.all, all.vars = g03q04.all, codebook.file = "data/infosource_translation_codebook.csv", extra.prefix = "infosource_", exclude.extra = c("None_of_the_above"))


## #####################################################################
## G03Q03: knowledge needed
## #####################################################################
g03q03.all <- get.vars("^G03Q03")
g03q03.vars <- g03q03.all[!grepl(".Other", g03q03.all)]
g03q03.short <- paste0("knowlneeded_", clean.var(g03q03.vars))
g03q03.short <- sub("^knowlneeded_In_your_opinion_which_of_the_following_aspects_will_help_improve_our_knowledge_about_potential_adaptive_forest_management_strategies_", "knowlneeded_", g03q03.short)

answered <- rowSums(!is.na(dat.all[, g03q03.all, drop = FALSE])) > 0
for(i in seq_along(g03q03.short)){
    x <- dat.all[[g03q03.vars[i]]]
    z <- rep(NA_integer_, length(x))
    z[answered] <- 0
    z[answered & x == "Y"] <- 1
    dat.all[[g03q03.short[i]]] <- z
}

dat.all <- recode.other(dat.all, all.vars = g03q03.all, codebook.file = "data/knowlneeded_translation_codebook.csv", extra.prefix = "knowlneeded_")


## ########################################################################
save(dat.all, full.name, short.name, file = "data/dat_all_translated.RData")
