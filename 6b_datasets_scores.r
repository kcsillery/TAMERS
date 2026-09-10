## #######################################################################

## TAMERS survey data analysis - Katalin Csillery - 07.07.2026

## this script calculates analysis ready scores

## ########################################################################

library(psych)
library(XML)
load("data/dat_all_translated.RData")  ## produced by 6_datasets_translations.r
load("data/codes2.RData")
source("functions.r")

## #####################################################################
##    experience scores
## #####################################################################

## climate change experience scores
## #####################################################################
experience.vars <- grep("^experience_", names(dat.all), value = TRUE)
experience.vars <- setdiff(experience.vars, "experience_None_of_the_above")
dat.all$cc.damage.experience <- rowSums(dat.all[, experience.vars], na.rm = FALSE)


## actions: Do you adapt the forest by changing the forest itself, or by protecting the existing forest?
## #####################################################################
active.vars <- c("action_AM",
                 "action_Direct_seeding",
                 "action_Forest_rejuvenation_by_planting_seedlings_in_general",
                 "action_Planting_individuals_of_existing_species_but_different_provenances_to_increase_genetic_diversity",
                 "action_Young_forest_management_Management_of_the_natural_regeneration",
                 "action_AFM",
                 "action_Increasing_structural_diversity",
                 "action_Reducing_the_rotation_period_or_target_diameter_through_forestry_early_utilization")

threat.vars <- c("action_Monitor_and_control_pests_and_pathogens",
                 "action_Increasing_the_resistance_of_individual_trees_to_disturbances")

dat.all$action.active <- rowMeans(dat.all[, active.vars], na.rm = TRUE)
dat.all$action.threat <- rowMeans(dat.all[, threat.vars], na.rm = TRUE)

answered <- rowSums(!is.na(dat.all[, c(active.vars, threat.vars)])) > 0
dat.all$action.active[!answered] <- NA
dat.all$action.threat[!answered] <- NA

dat.all$action.strategy.score <- dat.all$action.active - dat.all$action.threat

## −1: almost exclusively responds by controlling threats (game, pests, pathogens).
## 0: balanced approach.
## +1: primarily modifies the forest itself (species, regeneration, silviculture).

## G02Q03a: FRM sources used
## #####################################################################
g02q03.vars <- get.vars("^G02Q03a")
g02q03.short <- paste0("frm_", clean.var(g02q03.vars))
g02q03.short <- sub("^frm_Which_of_the_following_forest_reproductive_material_sources_have_you_used_definition_", "frm_", g02q03.short)
g02q03.short <- gsub("Forest_reproductive_material_originating_in_a_different_region_of_provenance_to_the_one_of_your_reference_forest_Typically_another_country_or_several_hundreds_of_km_away", "different_region",  g02q03.short)
g02q03.short <- gsub("Forest_reproductive_material_originating_in_the_same_region_of_provenance_as_your_reference_forest_Planting_using_indigeneous_material", "same_region",    g02q03.short)
g02q03.short <- gsub("Natural_regeneration_Autochthonous", "natural_regeneration", g02q03.short)
dat.all[, g02q03.short] <- dat.all[, g02q03.vars]

frm.vars <- grep("^frm_.*Scale_[12]$", names(dat.all), value = TRUE)

for(v in frm.vars)
    dat.all[[v]] <- ifelse(dat.all[[v]] == "AO01", 1L,
                    ifelse(dat.all[[v]] == "AO02", 0L, NA))

## Native species
answered <- rowSums(!is.na(dat.all[, grep("_Scale_1$", g02q03.short), drop = FALSE])) > 0
dat.all$agf.ready.native <- with(dat.all,
    ifelse(!answered, NA,
    ifelse(frm_different_region_Scale_1 == 1, 2,
    ifelse(frm_same_region_Scale_1 == 1, 1, 0))))

## Non-native species
answered <- rowSums(!is.na(dat.all[, grep("_Scale_2$", g02q03.short), drop = FALSE])) > 0
dat.all$agf.ready.nonnative <- with(dat.all,
    ifelse(!answered, NA,
    ifelse(frm_different_region_Scale_2 == 1, 2,
    ifelse(frm_same_region_Scale_2 == 1, 1, 0))))


## 0 = only autochthonous (natural regeneration)
## 1 = same region of provenance
## 2 = different region of provenance

## FRM details => see translated comments
## #####################################################################

##   barrier score
## #####################################################################

## was the barrier local or central (eg legislation related)
local.barrier.vars <- c("barrier_High_costs_associated_with_tree_planting",
                        "barrier_Lack_of_available_forest_reproductive_material_from_different_regions_of_provenance",
                        "barrier_Lack_of_knowledge_on_appropriate_regions_of_provenance_to_select_the_forest_reproductive_material_from",
                        "barrier_Difficulties_obtaining_traceable_good_quality_FRM",
                        "barrier_Abiotic_and_biotic_stress",
                        "barrier_Browsing_pressure")
external.barrier.vars <- c("barrier_Legislative_barriers")
exclude.barrier.vars <- c("barrier_I_have_never_tried_introducing_new_forest_reproductive_material",
                          "barrier_No_barriers",
                          "barrier_Ethics",
                          "barrier_Local_FRM_best")

local.barrier.vars <- local.barrier.vars[local.barrier.vars %in% names(dat.all)]
external.barrier.vars <- external.barrier.vars[external.barrier.vars %in% names(dat.all)]
exclude.barrier.vars <- exclude.barrier.vars[exclude.barrier.vars %in% names(dat.all)]
barrier.local <- rowMeans(dat.all[, local.barrier.vars, drop = FALSE], na.rm = TRUE)
barrier.external <- rowMeans(dat.all[, external.barrier.vars, drop = FALSE], na.rm = TRUE)

answered <- rowSums(!is.na(dat.all[, c(local.barrier.vars, external.barrier.vars), drop = FALSE])) > 0
barrier.local[!answered] <- NA
barrier.external[!answered] <- NA

dat.all$barrier.orientation <- barrier.external - barrier.local
dat.all$barrier.orientation[rowSums(dat.all[, exclude.barrier.vars, drop = FALSE], na.rm = TRUE) > 0] <- 0


## #########################################
## knowledge scores
## #########################################

## eco evolutionary knowledge
## #########################################
ecoevo.knowledge.vars  <- get.vars("^G03Q02_SQ")
dat.all$ecoevo.knowledge.score <- NA_real_
dat.all$ecoevo.knowledge.score <- rowSums(dat.all[, ecoevo.knowledge.vars] == "Y", na.rm = TRUE)

## information source orientation
## #########################################
info.none.vars <- c("infosource_None_of_the_above",
                    "infosource_Own_experience")

nonspecialized.vars <- c("infosource_Radio_Programmes",
                         "infosource_Internet_i_e_Social_media_like_LinkedIn")

info.local.vars <- c("infosource_Advice_from_other_foresters_forest_owners",
                     "infosource_Reports_or_news_blogs_from_forest_associations",
                     "infosource_Events")

info.national.vars <- c("infosource_Advice_from_government_extension_agencies_researchers",
                        "infosource_Government_reports",
                        "infosource_Government_reports_uncertain",
                        "infosource_Continuous_education")

info.science.vars <- c("infosource_Scientific_forestry_journals")

## keep only variables that exist
info.none.vars <- info.none.vars[info.none.vars %in% names(dat.all)]
nonspecialized.vars <- nonspecialized.vars[nonspecialized.vars %in% names(dat.all)]
info.local.vars <- info.local.vars[info.local.vars %in% names(dat.all)]
info.national.vars <- info.national.vars[info.national.vars %in% names(dat.all)]
info.science.vars <- info.science.vars[info.science.vars %in% names(dat.all)]

answered <- rowSums(!is.na(dat.all[, c(info.none.vars,
                                      nonspecialized.vars,
                                      info.local.vars,
                                      info.national.vars,
                                      info.science.vars), drop = FALSE])) > 0

dat.all$infosource.level <- NA_real_
dat.all$infosource.level[answered] <- 0

dat.all$infosource.level <- dat.all$infosource.level +
    rowSums(dat.all[, nonspecialized.vars, drop = FALSE], na.rm = TRUE) * 1 +
    rowSums(dat.all[, info.local.vars, drop = FALSE], na.rm = TRUE) * 2 +
    rowSums(dat.all[, info.national.vars, drop = FALSE], na.rm = TRUE) * 3 +
    rowSums(dat.all[, info.science.vars, drop = FALSE], na.rm = TRUE) * 4


## knowledge needed: To what extent do respondents believe improving knowledge/transfer is the solution?
## ############################################################################################

transfer.vars <- c("knowlneeded_Improved_collaboration_between_academic_researchers_and_practitioners",
                   "knowlneeded_Increase_events_where_researchers_and_foresters_can_exchange_i_e_workshops",
                   "knowlneeded_Increase_formal_and_informal_training_opportunities",
                   "knowlneeded_More_or_improved_communication_between_researchers_and_foresters_via_written_media_i_e_magazines_social_media_posts")

## the other responses are actully NOT about knowledge transfer
generation.vars <- c("knowlneeded_More_research_and_experiments",
                     "knowlneeded_Use_existing_knowledge")

external.vars <- c("knowlneeded_Policy_governance",
                   "knowlneeded_Resources_capacity_limited",
                   "knowlneeded_Wildlife_management",
                   "knowlneeded_Fake_news")

dat.all$knowledge.transfer.score <- rowSums(dat.all[, transfer.vars], na.rm = FALSE)## - rowSums(dat.all[, c(generation.vars, external.vars)], na.rm = FALSE)


## #####################################################################
##    gouvernance scores
## #####################################################################

## G05Q01: management goals, Likert 1-5
## #########################################
g05q00.vars <- get.vars("^G05Q00")
g05q00.short <- paste0("goal_", clean.var(g05q00.vars))
dat.all[, g05q00.short] <- dat.all[, g05q00.vars]

g05q00.short <- sub("^goal_Which_goals_do_you_think_should_be_pursued_when_managing_a_forest_1_not_important_5_highly_important_", "goal_", g05q00.short)
names(dat.all)[match(g05q00.vars, names(dat.all))] <- g05q00.short

nature.vars <- c(
    "goal_Maximize_biodiversity",
    "goal_Maximize_carbon_sink_capacity",
    "goal_Maximize_forests_protective_capacity_towards_natural_hazards",
    "goal_Maximize_forests_provision_of_drinking_water"
)

economy.vars <- c(
    "goal_Maximize_the_production_of_energy_and_industrial_wood_i_e_pulp",
    "goal_Maximize_the_production_of_wood_for_material_use"
)

dat.all$goal.nature.score <- rowMeans(dat.all[, nature.vars], na.rm = TRUE)
dat.all$goal.economy.score <- rowMeans(dat.all[, economy.vars], na.rm = TRUE)

dat.all$goal.preference <- dat.all$goal.nature.score - dat.all$goal.economy.score

dat.all$goal.recreation <- dat.all$goal_Maximize_recreational_use ## keep separate because it didn't fit

cor(dat.all$goal.nature.score,
    dat.all$goal.economy.score,
    use = "pairwise.complete.obs")
## [1] 0.09488469 weakly correlated = difference score is informative
## respondents are not trading off nature against production. Instead, some respondents simply value everything highly

## G05Q02: policy / legal urgency (Likert 1-5)
## #####################################################################

g05q02.vars <- get.vars("^G05Q02")
g05q02.short <- paste0("legal_", clean.var(g05q02.vars))
g05q02.short <- sub("^legal_Do_you_agree_with_the_following_statements_1_strongly_disagree_5_strongly_agree_", "legal_", g05q02.short)

dat.all[, g05q02.short] <- dat.all[, g05q02.vars]

## policy urgency: higher = respondent thinks EU/national government should do more
policy.satisfaction.vars <- c("legal_The_EU_is_doing_enough_to_support_forest_adaptation_to_climate_change",
                              "legal_The_national_government_is_doing_enough_to_support_forest_adaptation_to_climate_change")

policy.satisfaction <- rowMeans(dat.all[, policy.satisfaction.vars], na.rm = TRUE)
policy.satisfaction[rowSums(!is.na(dat.all[, policy.satisfaction.vars])) == 0] <- NA
dat.all$policy.urgency <- 6 - policy.satisfaction

## AM policy variables kept separate
dat.all$current.policy.AM.friendly <- dat.all$legal_Current_national_policies_support_the_introduction_of_species_from_different_regions_of_provenance_for_the_adaptation_of_forests_to_climate_change
dat.all$noAM.policy.preferred <- 6 - dat.all$legal_National_policies_should_only_support_natural_regeneration_as_a_strategy_to_adapt_forests_to_climate_change


##    support needed: know enough or build knowledge
## #####################################################################

## Negative values: We already know enough. Give practitioners more autonomy, remove constraints, improve governance.
## Positive values: We still need to build capacity through science, education and collaboration.

helpme.vars <- c("support_Professional_autonomy",
                 "support_Policy_effectiveness",
                 "support_Monetary_incentives_i_e_subsidies_and_or_tax_deductions",
                 "support_Changing_legislation_concerning_forest_reproductive_material_from_different_provenances_and_species")
build.vars <- c("support_Education",
                "support_Public_support_for_increased_forester_researcher_collaboration")

dat.all$support.philosophy <-
    rowMeans(dat.all[, build.vars], na.rm = TRUE) -
    rowMeans(dat.all[, helpme.vars], na.rm = TRUE)

answered <- rowSums(!is.na(dat.all[, c(helpme.vars, build.vars)])) > 0
dat.all$support.philosophy[!answered] <- NA

## ##################################################
save(dat.all, file="data/dat_all_scores.RData")
