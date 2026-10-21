########################################.
#
#   Data Prep
#   Daniel Redhead & Elly Power
#   Edited by Tom R
#
#   NOTE: Parts of standardize_data.R, which this script sources,
#   rely on being run in a fresh environment.
#
########################################.

# Load libraries

## Loading plyr when dplyr is already loaded can cause problems:
## https://stackoverflow.com/questions/31644739/loading-dplyr-after-plyr-is-causing-issues
## which necessitates the below block.

# if("dplyr" %in% (.packages())){
#   detach("package:dplyr", unload=TRUE)
#   detach("package:plyr", unload=TRUE)
# }
library(plyr); library(dplyr)
library(tidyverse)
library(kinship2)
library(geosphere)
library(igraph)
library(reshape2)


normalize <- function(y) {
  x <- y[!is.na(y)]
  x <- (x - min(x)) / (max(x) - min(x))
  y[!is.na(y)] <- x
  return(y)
}

source("standardize_data.R")

# EndowGitHub <- getwd()
# EndowDatabase <- "../endow-database"

output_path <-
  file.path(EndowGitHub,
            "DerivedData")

# NOTE: Temporarily removing PE, as it's an idiosyncratic site with "supraunits"
# At end, will source process-PE.R to integrate
# saving two, so can be re-integrated
# pe_people_obs <- people_observations$PE
# pe_sharing_unit <- sharing_unit$PE
# networks$PE <- NULL
# people_observations$PE <- NULL
# sharing_unit$PE <- NULL
# partnerships$PE <- NULL
# people$PE <- NULL
# su_distances$PE <- NULL
# poss_cost$PE <- NULL
# su_wealth$PE <- NULL


## making a list of dataframes of residents
## NOTE: this considers only those who are defined as residents in each site;
## in some cases, there may be some non-residents who are still considered sharing unit members
## BD, for example, has is_community_member alongside is_resident but many other sites do not
## further, can't expect that assets held by non-residents will be captured in the sharing unit data
## so for consistency, still limiting residents to this, which is then later used for counting susize, etc.
residents <- lapply(people_observations, function(df) {
  df[df$is_resident == 1,]
})


## CLEANING NETWORKS
# for (site in names(networks)) {
#   print(site)
#   print(unique(networks[[site]]$alterid)[!unique(networks[[site]]$alterid) %in% people_observations[[site]]$personid])
# }

## AH, BM, CP, CR, KA, KM [because alters = SUs], KO, MA, MG, MK, MY, PS, RA, SH, TP [all SUs or ORGs]
# for_TP <- subset(networks$TP, networks$TP$alterid %in% unique(networks$TP$alterid)[!unique(networks$TP$alterid) %in% people_observations$TP$personid])

## removing tie type from BY which is villages visited.
# networks$BY <- networks$BY[networks$BY$tie != "villages visited",]



## splitting TN and MC

# TN and MC have two sites within their repos and need to be divided out.
# TN should become AZ and TE; MC should become MC and AP -- NEED TO DECIDE IF WE SHOULD RENAME MC==1 to different two-letter code!
# In most cases, can leave as the full set. Only really need to subset in a few cases.

# people$AZ <- people$TN
# people$TE <- people$TN
# people$AP <- people$MC
#
# people_observations$AZ <- people_observations$TN
# people_observations$TE <- people_observations$TN
# people_observations$AP <- people_observations$MC
#
# #poss_cost$AZ <- poss_cost$TN
# #poss_cost$TE <- poss_cost$TN
# #poss_cost$AP <- poss_cost$MC
#
# residents$AZ <- people_observations$TN[
#   people_observations$TN$is_resident == 1
#   & !is.na(people_observations$TN$is_resident)
#   & people_observations$AZ$location == "alakapuram",]
#
# residents$TE <- people_observations$TN[
#   people_observations$TN$is_resident == 1
#   & !is.na(people_observations$TN$is_resident)
#   & people_observations$TN$location == "tenpatti",]
#
# residents$MC <- people_observations$MC[people_observations$MC$is_resident == 1 & !is.na(people_observations$MC$is_resident) & people_observations$MC$location == "1",]
#
# residents$AP <- people_observations$MC[people_observations$MC$is_resident == 1 & !is.na(people_observations$MC$is_resident) & people_observations$MC$location == "2",]
#
# ## may not need to subset down to only SUs with occupants
# sharing_unit$AZ <- sharing_unit$TN[sharing_unit$TN$su_id %in% unique(residents$AZ$su_id),]
# sharing_unit$TE <- sharing_unit$TN[sharing_unit$TN$su_id %in% unique(residents$TE$su_id),]
# sharing_unit$AP <- sharing_unit$MC[sharing_unit$MC$su_id %in% unique(residents$AP$su_id),]
# sharing_unit$MC <- sharing_unit$MC[sharing_unit$MC$su_id %in% unique(residents$MC$su_id),]
#
# networks$AZ <- networks$TN[networks$TN$personid %in% residents$AZ$personid,]
# networks$TE <- networks$TN[networks$TN$personid %in% residents$TE$personid,]
# networks$AP <- networks$MC[networks$MC$personid %in% residents$AP$personid,]
# networks$MC <- networks$MC[networks$MC$personid %in% residents$MC$personid,]
#
# partnerships$AZ <- partnerships$TN
# partnerships$TE <- partnerships$TN
# partnerships$AP <- partnerships$MC
#
# su_distances$AZ <- su_distances$TN
# su_distances$TE <- su_distances$TN
# su_distances$AP <- su_distances$MC
#
# # removing the aggregate ones that aren't meaningful until subset
# networks$TN <- NULL
# #partnerships$TN <- NULL
# #people$TN <- NULL
# #people_observations$TN <- NULL
# residents$TN <- NULL
# sharing_unit$TN <- NULL # note, this means that any SUs without residents are effectively removed.
# #su_distances$TN <- NULL


## NOW BACK TO PROCESSING!

## making a list with the set of surveyed individuals from each site; note this is missing for MY
egos <- lapply(networks, function(df) unique(df$personid))

## Adding variable to note in person was asked networks questions, and then tallying number of respondents from each SU. Note: won't work cleanly for places like MY or KM, where egos aren't individuals!
num_surv <- list()
for (site in setdiff(names(egos), c("MY"))) {
  people_observations[[site]]$surveyed <- people_observations[[site]]$personid %in% egos[[site]]
  residents[[site]]$surveyed <- residents[[site]]$personid %in% egos[[site]]
  num_surv[[site]] <- data.frame(table(people_observations[[site]]$su_id[people_observations[[site]]$surveyed == TRUE]))
  ## EXCEPTION FOR NP: consistent with note on line ~679: some non-residents completed the survey and ethnog wants them dropped; as don't use, shouldn't include in count
  if(site == "NP") {num_surv[[site]] <- data.frame(table(people_observations[[site]]$su_id[people_observations[[site]]$surveyed == TRUE & people_observations[[site]]$is_resident == 1]))}
  sharing_unit[[site]]$num_surv <- num_surv[[site]]$Freq[match(sharing_unit[[site]]$su_id, num_surv[[site]]$Var1)]
  #sharing_unit[[site]]$num_surv[is.na(sharing_unit[[site]]$num_surv)] <- 0 ## DO WE WANT TO KEEP THIS LINE? SEEMS LIKE IT WOULD CAUSE ERRORS.
}

# sharing_unit$MY$num_surv <- 1 ## joint response

## Now we are making a list of number of people in each sharing unit who
## were posed a particular gender question.

### See the conversation on Mattermost beginning November 6

### We will call them
### num_surv_male_q
### num_surv_female_q

normal_gender_q_sites <-
  c("AH",
    "AR",
    "AV",
    "AZ",
    "BD", ### this was fixed upstream by Dan
    "BY",
    "CP",
    "CR",
    "DJ",
    "EK",
    "FF",
    "FJ",
    "HE",
    "HI",
    "KA",
    "KM",
    "KS",
    "KT",
    "LB",
    "MK",
    "MN",
    "NI",
    "NP",
    "PS",
    "PT",
    "SH",
    "SN",
    "TE",
    "TI",
    "TM",
    "TN",
    "TP",
    "TS",
    "UP",
    "VT",
    "YI",
    "YN",
    "EG",
    "EX",
    "SI",
    "SM")

for (site in intersect(normal_gender_q_sites, names(sharing_unit))) {

  sharing_unit[[site]]$num_surv_male_q <- sharing_unit[[site]]$num_surv
  sharing_unit[[site]]$num_surv_female_q <- sharing_unit[[site]]$num_surv

}


### Now we go case-by-case.

#### MY: A few questions were directed at the female HH only (#5,7)
#### or the male HH only (#6,8), and their alters list should apply to ONLY
#### the female HH or male HH respectively.").
#### [DAN: MYSU009, MYSU020, MYSU032, MYSU087 don't seem to have the cells for
#### the MHH and FHH in the nodelist exactly right - check those; MYSU081 is the
#### only exception with one respondent who answered all questions (the FHH MY0032)
#### so in that case there should be one respondent.

#### Tom: Do we need an exception for FHH MY0032? I don't think so, just that the
#### FHH answered in place of the MHH.

# sharing_unit[["MY"]]$num_surv_male_q <- 1
# sharing_unit[["MY"]]$num_surv_female_q <- 1
#
# sharing_unit[["BM"]]$num_surv_male_q <- 1
# sharing_unit[["BM"]]$num_surv_female_q <- 1
#
# sharing_unit[["KO"]]$num_surv_male_q <- 1
# sharing_unit[["KO"]]$num_surv_female_q <- 1
#
# sharing_unit[["MA"]]$num_surv_male_q <- 1
# sharing_unit[["MA"]]$num_surv_female_q <- 1
#
# sharing_unit[["MG"]]$num_surv_male_q <- 1
# sharing_unit[["MG"]]$num_surv_female_q <- 1
#
#
# #### For RA, when the surveyed person is a man, but not when it is RA1059,
# #### we should treat the num_surveyed for the female questions as 0.
#
# people_observations[["RA"]]$surveyed_male <-
#   people_observations[["RA"]]$personid %in% egos[["RA"]]
#   ## Elly's post suggests that women were asked the male questions.
#   # &
#   # people_observations[["RA"]]$gender == "male"
# people_observations[["RA"]]$surveyed_female <-
#   people_observations[["RA"]]$personid %in% egos[["RA"]] &
#   (people_observations[["RA"]]$gender == "female" |
#      people_observations[["RA"]]$personid == "ra1059")
#
# # residents[[site]]$surveyed <- residents[[site]]$personid %in% egos[[site]] ## don't think this is needed here
# num_surv_male_q <-
#   data.frame(table(people_observations[["RA"]]$su_id[people_observations[["RA"]]$surveyed_male == TRUE]))
# sharing_unit[["RA"]]$num_surv_male_q <-
#   num_surv_male_q$Freq[match(sharing_unit[["RA"]]$su_id, num_surv_male_q$Var1)]
# #sharing_unit[["RA"]]$num_surv_male_q[is.na(sharing_unit[["RA"]]$num_surv_male_q)] <- 0 ## DO WE WANT TO KEEP THIS LINE? SEEMS LIKE IT WOULD CAUSE ERRORS.
# ## There's a problem here with deleting the above line. It then leads us to divide by NAs later, making the sum weights NAs. But dividing by 0 does not seem smarter.
# ## See analyse_data_and_create_networks.R. Ultimately, 0s and NAs are treated the same, so this doesn't matter.
# num_surv_female_q <-
#   data.frame(table(people_observations[["RA"]]$su_id[people_observations[["RA"]]$surveyed_female == TRUE]))
# sharing_unit[["RA"]]$num_surv_female_q <-
#   num_surv_female_q$Freq[match(sharing_unit[["RA"]]$su_id, num_surv_female_q$Var1)]
# #sharing_unit[["RA"]]$num_surv_female_q[is.na(sharing_unit[["RA"]]$num_surv_female_q)] <- 0 ## DO WE WANT TO KEEP THIS LINE? SEEMS LIKE IT WOULD CAUSE ERRORS.
#
# gender_specific_sites <-
#   c("IH",
#     "WH",
#     "PC",
#     "WL",
#     "AV",
#     "DJ",
#     "PQ",
#     "PE",
#     "TZ",
#     "AP",
#     "MC")
#
# for (site in gender_specific_sites) {
#
#   people_observations[[site]]$surveyed_male <-
#     people_observations[[site]]$personid %in% egos[[site]] &
#     people_observations[[site]]$gender == "male"
#   people_observations[[site]]$surveyed_female <-
#     people_observations[[site]]$personid %in% egos[[site]] &
#     (people_observations[[site]]$gender == "female" |
#        people_observations[[site]]$personid == "ra1059")
#
#   # residents[[site]]$surveyed <- residents[[site]]$personid %in% egos[[site]] ## don't think this is needed here
#   num_surv_male_q <-
#     data.frame(table(people_observations[[site]]$su_id[people_observations[[site]]$surveyed_male == TRUE]))
#   sharing_unit[[site]]$num_surv_male_q <-
#     num_surv_male_q$Freq[match(sharing_unit[[site]]$su_id, num_surv_male_q$Var1)]
#   #sharing_unit[[site]]$num_surv_male_q[is.na(sharing_unit[[site]]$num_surv_male_q)] <- 0 ## DO WE WANT TO KEEP THIS LINE? SEEMS LIKE IT WOULD CAUSE ERRORS.
#   num_surv_female_q <-
#     data.frame(table(people_observations[[site]]$su_id[people_observations[[site]]$surveyed_female == TRUE]))
#   sharing_unit[[site]]$num_surv_female_q <-
#     num_surv_female_q$Freq[match(sharing_unit[[site]]$su_id, num_surv_female_q$Var1)]
#   #sharing_unit[[site]]$num_surv_female_q[is.na(sharing_unit[[site]]$num_surv_female_q)] <- 0 ## DO WE WANT TO KEEP THIS LINE? SEEMS LIKE IT WOULD CAUSE ERRORS.
#
# }


## making a list of resident SUs.
res_su <- lapply(residents, function(df) unique(df$su_id))
res_su <- lapply(res_su, function(x) x[!is.na(x)])
res_su <- lapply(res_su, function(x) x[x != ""])

## and a list of sampled SUs (for network questions)
sampled_su <- lapply(people_observations, function(df){
  unique(df$su_id[df$personid %in% unlist(egos)])
})
sampled_su <- lapply(sampled_su, function(x) x[x != ""])

# sampled_su$MY <- unique(networks$MY$su_id)

## Adding variable to note if a person's sharing unit was asked networks questions.
for (site in intersect(names(residents), names(sampled_su))) {
  residents[[site]]$su_sampled <- residents[[site]]$su_id %in% unlist(sampled_su)
  sharing_unit[[site]]$su_sampled <- sharing_unit[[site]]$su_id %in% unlist(sampled_su)
}

su_sum <- lapply(residents, function(df) {
  df %>%
    dplyr::group_by(su_id) %>%
    dplyr::summarize(su_size = n(),
      age_av = if (all(is.na(age))) NA_real_ else mean(age, na.rm = TRUE), # won't work for at least MK, which doesn't have full ages
      age_max = if (all(is.na(age))) NA_real_ else max(age, na.rm = TRUE),
      adult_count = if (all(is.na(is_adult))) NA_integer_ else sum(is_adult == 1, na.rm = TRUE),
      able_count = if (all(is.na(work_ability))) NA_integer_ else sum(work_ability == "full", na.rm = TRUE),
      status_any = if (all(is.na(status))) NA_real_ else as.numeric(any(status == 1, na.rm = TRUE)),
      status_count = if (all(is.na(status))) NA_integer_ else sum(status == 1, na.rm = TRUE),
      edu_max = if (all(is.na(years_education))) years_education[NA_integer_] else max(years_education, na.rm = TRUE),
      other_noetic_any = if (all(is.na(other_noetic_true))) NA_real_ else as.numeric(any(other_noetic_true == TRUE, na.rm = TRUE)),
      other_noetic_count = if (all(is.na(other_noetic_true))) NA_integer_ else sum(other_noetic_true == TRUE, na.rm = TRUE)
    )
})
su_sum <- lapply(su_sum, function(x) subset(x, x$su_id !=""))


su_heads_sum <- lapply(residents, function(df){
  df %>%
    dplyr::group_by(su_id) %>%
    filter(head == 1) %>%
    dplyr::summarize(age_av_hh = mean(age, na.rm = TRUE),
      able_count_hh = if (all(is.na(work_ability))) NA_integer_ else sum(work_ability == "full", na.rm = TRUE),
      status_any_hh = if (all(is.na(status))) NA_real_ else as.numeric(any(status == 1, na.rm = TRUE)),
      status_count_hh = if (all(is.na(status))) NA_integer_ else sum(status == 1, na.rm = TRUE),
      edu_max_hh = if (all(is.na(years_education))) years_education[NA_integer_] else max(years_education, na.rm = TRUE),
      other_noetic_any_hh = if (all(is.na(other_noetic_true))) NA_real_ else as.numeric(any(other_noetic_true == TRUE, na.rm = TRUE)),
      other_noetic_count_hh = if (all(is.na(other_noetic_true))) NA_integer_ else sum(other_noetic_true == TRUE, na.rm = TRUE)
      )
})
su_heads_sum <- lapply(su_heads_sum, function(x) subset(x, x$su_id !=""))

## Adding summary variables for SU. Could add more, once variables cleaned!
for (site in names(sharing_unit)) {
  sharing_unit[[site]]$hofh <- ifelse(!is.na(sharing_unit[[site]]$femalehead) & is.na(sharing_unit[[site]]$malehead), "female",
                                      ifelse(!is.na(sharing_unit[[site]]$femalehead) & !is.na(sharing_unit[[site]]$malehead), "both",
                                             ifelse(is.na(sharing_unit[[site]]$femalehead) & !is.na(sharing_unit[[site]]$malehead), "male", NA)))
  sharing_unit[[site]]$su_size <- su_sum[[site]]$su_size[match(sharing_unit[[site]]$su_id, su_sum[[site]]$su_id)]
  sharing_unit[[site]]$age_av <- su_sum[[site]]$age_av[match(sharing_unit[[site]]$su_id, su_sum[[site]]$su_id)]
  sharing_unit[[site]]$age_av_hh <- su_heads_sum[[site]]$age_av_hh[match(sharing_unit[[site]]$su_id, su_heads_sum[[site]]$su_id)]
  sharing_unit[[site]]$age_max <- su_sum[[site]]$age_max[match(sharing_unit[[site]]$su_id, su_sum[[site]]$su_id)]
  sharing_unit[[site]]$adult_count <- su_sum[[site]]$adult_count[match(sharing_unit[[site]]$su_id, su_sum[[site]]$su_id)]
  sharing_unit[[site]]$able_count <- su_sum[[site]]$able_count[match(sharing_unit[[site]]$su_id, su_sum[[site]]$su_id)]
  sharing_unit[[site]]$able_count_hh <- su_heads_sum[[site]]$able_count_hh[match(sharing_unit[[site]]$su_id, su_heads_sum[[site]]$su_id)]
  sharing_unit[[site]]$status_any <- su_sum[[site]]$status_any[match(sharing_unit[[site]]$su_id, su_sum[[site]]$su_id)]
  sharing_unit[[site]]$status_any_hh <- su_heads_sum[[site]]$status_any_hh[match(sharing_unit[[site]]$su_id, su_heads_sum[[site]]$su_id)]
  sharing_unit[[site]]$status_count <- su_sum[[site]]$status_count[match(sharing_unit[[site]]$su_id, su_sum[[site]]$su_id)]
  sharing_unit[[site]]$status_count_hh <- su_heads_sum[[site]]$status_count_hh[match(sharing_unit[[site]]$su_id, su_heads_sum[[site]]$su_id)]
  sharing_unit[[site]]$edu_max <- su_sum[[site]]$edu_max[match(sharing_unit[[site]]$su_id, su_sum[[site]]$su_id)]
  sharing_unit[[site]]$edu_max_hh <- su_heads_sum[[site]]$edu_max_hh[match(sharing_unit[[site]]$su_id, su_heads_sum[[site]]$su_id)]
  sharing_unit[[site]]$other_noetic_any <- su_sum[[site]]$other_noetic_any[match(sharing_unit[[site]]$su_id, su_sum[[site]]$su_id)]
  sharing_unit[[site]]$other_noetic_any_hh <- su_heads_sum[[site]]$other_noetic_any_hh[match(sharing_unit[[site]]$su_id, su_heads_sum[[site]]$su_id)]
  sharing_unit[[site]]$other_noetic_count <- su_sum[[site]]$other_noetic_count[match(sharing_unit[[site]]$su_id, su_sum[[site]]$su_id)]
  sharing_unit[[site]]$other_noetic_count_hh <- su_heads_sum[[site]]$other_noetic_count_hh[match(sharing_unit[[site]]$su_id, su_heads_sum[[site]]$su_id)]
}

## Adding SUs that are associated with residents but which do not appear in the sharing_unit database.

#missing_su_in_SUdf <- list()
#for (name in intersect(names(residents), names(sharing_unit))) {
#  print(name)
#  print(setdiff(res_su[[name]], sharing_unit[[name]]$su_id))
#}
#rm(name)

for (site in names(sharing_unit)) {
  missing_su <- setdiff(res_su[[site]], sharing_unit[[site]]$su_id)
  missing_su <- missing_su[is.na(missing_su) == FALSE]
  missing_su <- missing_su[!missing_su == ""]
  if(length(missing_su) > 0) new_rows <- data.frame(matrix(NA, nrow = length(missing_su), ncol = ncol(sharing_unit[[site]])))
  if(length(missing_su) > 0) colnames(new_rows) <- colnames(sharing_unit[[site]])
  if(length(missing_su) > 0) new_rows$su_id <- missing_su
  if(length(missing_su) > 0) sharing_unit[[site]] <- rbind(sharing_unit[[site]], new_rows)
}
rm(site, new_rows)

######################################################################################################
#
#   Specify kinship & proximity
#
######################################################################################################

# Prepare kinship/relatedness

kinship <- lapply(people, function(x) select(x, "personid", "mother", "father"))
# kinship$TN <- NULL # removing, as won't be using, and as have removed things like residents$TN, which the below code would require

dads <- list()
mums <- list()
new_dads <- list()
new_mums <- list()
kin_mat <- list()
kin_edge <- list()
av_rel <- list()
su_dyads <- list()
primary_kin <- list()
secondary_kin <- list()
for (site in names(kinship)){
  kinship[[site]]$personid[kinship[[site]]$personid == ""] <- NA
  kinship[[site]] <- kinship[[site]][!is.na(kinship[[site]]$personid),]
  kinship[[site]]$father[kinship[[site]]$father == 999 |
                        kinship[[site]]$father == 0 |
                        kinship[[site]]$father == "empty" |
                        kinship[[site]]$father == "missing" |
                        kinship[[site]]$father == "" |
                        kinship[[site]]$father == " " |
                        kinship[[site]]$father == "dk" ] <- NA
  kinship[[site]]$mother[kinship[[site]]$mother == 999 |
                        kinship[[site]]$mother == 0 |
                        kinship[[site]]$mother == "empty" |
                        kinship[[site]]$mother == "missing" |
                        kinship[[site]]$mother == "" |
                        kinship[[site]]$mother == " " |
                        kinship[[site]]$mother == "dk" ] <- NA
  kinship[[site]]$sex <- people_observations[[site]]$gender[match(kinship[[site]]$personid, people_observations[[site]]$personid)]
  kinship[[site]]$sex[is.na(kinship[[site]]$sex)] <- 3
  kinship[[site]]$sex[kinship[[site]]$sex == ""] <- 3
  kinship[[site]]$sex[kinship[[site]]$sex == "unknown"] <- 3
  kinship[[site]]$sex[kinship[[site]]$sex == "male"] <- 1
  kinship[[site]]$sex[kinship[[site]]$sex == "female"] <- 2
  kinship[[site]]$sex <- as.numeric(kinship[[site]]$sex)
  kinship[[site]]$father <- ifelse(is.na(kinship[[site]]$father) & !is.na(kinship[[site]]$mother),
                                paste0(kinship[[site]]$personid, "_dad"),
                                kinship[[site]]$father)
  kinship[[site]]$mother <- ifelse(is.na(kinship[[site]]$mother) & !is.na(kinship[[site]]$father),
                                paste0(kinship[[site]]$personid, "_mum"),
                                kinship[[site]]$mother)
  dads[[site]] <- unique(kinship[[site]]$father[!kinship[[site]]$father %in% kinship[[site]]$personid & !is.na(kinship[[site]]$father) & !kinship[[site]]$father == 0])
  mums[[site]] <- unique(kinship[[site]]$mother[!kinship[[site]]$mother %in% kinship[[site]]$personid & !is.na(kinship[[site]]$mother) & !kinship[[site]]$mother == 0])
  if(length(dads[[site]]) > 0) new_dads[[site]] <- data.frame(personid = dads[[site]],
                                                        father = NA,
                                                        mother = NA,
                                                        sex = rep(1))
  if(length(dads[[site]]) > 0) kinship[[site]] <- rbind(kinship[[site]], new_dads[[site]])
  if(length(mums[[site]]) > 0) new_mums[[site]] <- data.frame(personid = mums[[site]],
                                                        father = NA,
                                                        mother = NA,
                                                        sex = rep(2))
  if(length(mums[[site]]) > 0) kinship[[site]] <- rbind(kinship[[site]], new_mums[[site]])


  kin_mat[[site]] <- 2*kinship(pedigree(id = kinship[[site]]$personid,
                                     dadid = kinship[[site]]$father,
                                     momid = kinship[[site]]$mother,
                                     sex = kinship[[site]]$sex))
  kin_mat[[site]] <- kin_mat[[site]][row.names(kin_mat[[site]]) %in% residents[[site]]$personid, colnames(kin_mat[[site]]) %in% residents[[site]]$personid ]
  kin_mat[[site]] <- kin_mat[[site]][match(residents[[site]]$personid, rownames(kin_mat[[site]])), match(residents[[site]]$personid, colnames(kin_mat[[site]]))]

  # Calculate average relatedness for each individual
  av_rel[[site]] <- data.frame(personid = rownames(kin_mat[[site]]), av_rel = rowMeans(kin_mat[[site]]))
  people_observations[[site]]$av_rel <- av_rel[[site]]$av_rel[match(people_observations[[site]]$personid, av_rel[[site]]$personid)]
  people_observations[[site]]$z_av_rel <- normalize(people_observations[[site]]$av_rel)
  residents[[site]]$av_rel <- av_rel[[site]]$av_rel[match(residents[[site]]$personid, av_rel[[site]]$personid)]
  residents[[site]]$z_av_rel <- normalize(residents[[site]]$av_rel)

  # rework output to get to sharing unit-level relatedness, by first turning matrix into edgelist
  kin_edge[[site]] <- reshape2::melt(kin_mat[[site]], varnames = c('i', 'j'), value.name = "r", na.rm = T)
  kin_edge[[site]] <- subset(kin_edge[[site]], i!=j)
  kin_edge[[site]] <- kin_edge[[site]][!kin_edge[[site]]$i == "", ]
  kin_edge[[site]] <- kin_edge[[site]][!kin_edge[[site]]$j == "", ]
  kin_edge[[site]] <- kin_edge[[site]][!is.na(kin_edge[[site]]$i), ]
  kin_edge[[site]] <- kin_edge[[site]][!is.na(kin_edge[[site]]$j), ]

  # Bring in the su id for both i and j. NEED TO FIGURE OUT WHAT TO DO FOR RESIDENTS WITH MISSING SUIDs.
  kin_edge[[site]]$sui <- residents[[site]]$su_id[match(kin_edge[[site]]$i, residents[[site]]$personid)]
  kin_edge[[site]]$suj <- residents[[site]]$su_id[match(kin_edge[[site]]$j, residents[[site]]$personid)]
  # create a dyad id
  kin_edge[[site]]$dyad_id <- paste(kin_edge[[site]]$sui, "--", kin_edge[[site]]$suj, sep = "")

  # Get average and max relatedness between households
  su_dyads[[site]] <- kin_edge[[site]] %>%
    dplyr::group_by(dyad_id) %>%
    dplyr::summarise(
      avg_r = mean(r, na.rm = TRUE),
      max_r = max(r, na.rm = TRUE)
    )
  su_dyads[[site]]$primary_kin_tie <- ifelse (su_dyads[[site]]$max_r >= 0.5, 1, 0)
  su_dyads[[site]]$secondary_kin_tie <- ifelse (su_dyads[[site]]$max_r >= 0.25, 1, 0)



  # su_dyads <- lapply(kin_edge, function(df) {
  #   df %>%
  #     dplyr::group_by(dyad_id) %>%
  #     dplyr::summarize(avg_r = mean(r, na.rm = TRUE),
  #               max_r = max(r, na.rm = TRUE),
  #               count_r0.5 = sum(r >= 0.5, na.rm = TRUE), ## note that these will double-count people!
  #              count_r0.25 = sum(r >= 0.25, na.rm = TRUE))
  # })

  # Separate out dyad ID again
  su_dyads[[site]] <- separate(data = su_dyads[[site]], col = dyad_id, into = c("sui", "suj"), sep = "--")
  su_dyads[[site]] <- su_dyads[[site]][!su_dyads[[site]]$sui == "", ]
  su_dyads[[site]] <- su_dyads[[site]][!su_dyads[[site]]$suj == "", ]
  su_dyads[[site]] <- su_dyads[[site]][!su_dyads[[site]]$sui == "NA", ]
  su_dyads[[site]] <- su_dyads[[site]][!su_dyads[[site]]$suj == "NA", ]
  su_dyads[[site]] <- su_dyads[[site]][!is.na(su_dyads[[site]]$sui), ]
  su_dyads[[site]] <- su_dyads[[site]][!is.na(su_dyads[[site]]$suj), ]


  # Add su physical distance as a column in the dyads table
  su_dyads[[site]] <- left_join(su_dyads[[site]], select(su_distances[[site]][su_distances[[site]]$sui %in% residents[[site]]$su_id & su_distances[[site]]$suj %in% residents[[site]]$su_id,], sui, suj, distance), by = c('sui', 'suj'))
  su_dyads[[site]]$distance[su_dyads[[site]]$sui == su_dyads[[site]]$suj] <- 0

  primary_kin[[site]] <- subset (su_dyads[[site]], primary_kin_tie == 1 & sui != suj, select = c(sui, suj))
  primary_kin[[site]] <- as.data.frame(table(primary_kin[[site]]$sui))

  secondary_kin[[site]] <- subset (su_dyads[[site]], secondary_kin_tie == 1 & sui != suj, select = c(sui, suj))
  secondary_kin[[site]] <- as.data.frame(table(secondary_kin[[site]]$sui))


}



## Adding variable for primary kin and secondary kin
for (site in names(sharing_unit)) {
  sharing_unit[[site]]$primary_kin <- primary_kin[[site]]$Freq[match(sharing_unit[[site]]$su_id, primary_kin[[site]]$Var1)]
  sharing_unit[[site]]$primary_kin[is.na(sharing_unit[[site]]$primary_kin)] <- 0

  sharing_unit[[site]]$secondary_kin <- secondary_kin[[site]]$Freq[match(sharing_unit[[site]]$su_id, secondary_kin[[site]]$Var1)]
  sharing_unit[[site]]$secondary_kin[is.na(sharing_unit[[site]]$secondary_kin)] <- 0

}

# housekeeping
rm(missing_su, site, new_dads, new_mums)


########################################.
#
#   Making SU-by-SU matrices
#
########################################.


su_m_avr <- list()
su_m_maxr <- list()
su_m_sib <- list()
su_m_par <- list()
su_m_chi <- list()
su_m_dist <- list()

for (site in names(su_dyads)){
  message(paste0("Processing sharing-unit level kinship for site ", site, "."))

  su_m_avr[[site]] <- su_m_maxr[[site]] <- su_m_sib[[site]] <- su_m_par[[site]] <- su_m_chi[[site]] <- su_m_dist[[site]] <- matrix(0,
                               nrow = length(res_su[[site]]),
                               ncol =  length(res_su[[site]]),
                               dimnames = list(sort(res_su[[site]]), sort(res_su[[site]])))

  for(i in 1:nrow(su_dyads[[site]])){
    su_m_avr[[site]][rownames(su_m_avr[[site]])==su_dyads[[site]]$sui[i],colnames(su_m_avr[[site]])==su_dyads[[site]]$suj[i]] <- su_dyads[[site]]$avg_r[i]
  }

  for(i in 1:nrow(su_dyads[[site]])){
    su_m_maxr[[site]][rownames(su_m_maxr[[site]])==su_dyads[[site]]$sui[i],colnames(su_m_maxr[[site]])==su_dyads[[site]]$suj[i]] <- su_dyads[[site]]$max_r[i]
  }

  for(i in 1:nrow(su_dyads[[site]])){
    su_m_dist[[site]][rownames(su_m_dist[[site]])==su_dyads[[site]]$sui[i],colnames(su_m_dist[[site]])==su_dyads[[site]]$suj[i]] <- su_dyads[[site]]$distance[i]
  }

fad <- FamAgg::FAData(
    pedigree =
    data.frame(
        id = kinship[[site]]$personid,
        father = kinship[[site]]$father,
        mother = kinship[[site]]$mother,
        sex = kinship[[site]]$sex,
        family = 1
        )
    )

# create empty matrices
su_m_par[[site]] <- su_m_chi[[site]] <- su_m_sib[[site]] <- matrix(0,
    nrow = length(res_su[[site]]),
    ncol =  length(res_su[[site]]),
    dimnames = list(sort(res_su[[site]]), sort(res_su[[site]])))

for (sui in res_su[[site]]) {
    hhmembs <- setdiff(residents[[site]]$personid[residents[[site]]$su_id == sui], NA)

    children <- FamAgg::getChildren(fad, id = hhmembs, max.generations = 1)
    parents <- FamAgg::getAncestors(fad, id = hhmembs, max.generations = 1)
    siblings <- FamAgg::getSiblings(fad, id = hhmembs)

    children_su <- setdiff(residents[[site]]$su_id[match(children, residents[[site]]$personid)], NA)
    parents_su <- setdiff(residents[[site]]$su_id[match(parents, residents[[site]]$personid)], NA)
    siblings_su <- setdiff(residents[[site]]$su_id[match(siblings, residents[[site]]$personid)], NA)

    for (suj in children_su) {
        su_m_chi[[site]][rownames(su_m_chi[[site]]) == sui, colnames(su_m_chi[[site]]) == suj] <- 1
    }

    for (suj in parents_su) {
        su_m_par[[site]][rownames(su_m_par[[site]]) == sui, colnames(su_m_par[[site]]) == suj] <- 1
    }

    for (suj in siblings_su) {
        su_m_sib[[site]][rownames(su_m_sib[[site]]) == sui, colnames(su_m_sib[[site]]) == suj] <- 1
    }

}

  # Write out the adjacency matrices
  write.csv(su_m_avr[[site]], file.path(output_path, paste0(site, "-su-avrel.csv")))
  write.csv(su_m_maxr[[site]], file.path(output_path, paste0(site, "-su-maxrel.csv")))
  write.csv(su_m_dist[[site]], file.path(output_path, paste0(site, "-su-dist.csv")))

}


######################################################################################################
#
#   Specify the social support networks
#
######################################################################################################


su_nets <- list()
su_nets_recipient_collapse <- list()
su_meta <- list()
su_alters_all <- list()
su_alters_req <- list()
su_alters_each <- list()
su_ex_alters_all <- list()
su_ex_alters_req <- list()
su_ex_alters_main <- list()
su_ex_alters_each <- list()
su_alters <- list()
su_externals <- list()

su_alters_each_indiv <- list()
su_alters_req_indiv <- list()
su_alters_each_pair <- list()
su_alters_req_pair <- list()


for (site in names(residents)) {
  message(paste0("Processing sharing-unit level social support networks for site ", site, "."))
  # Add sharing unit IDs so can recode
  if(!site %in% c("MY", "PE", "BD", "FF", "LB", "YN")) networks[[site]]$sui <- residents[[site]]$su_id[match(networks[[site]]$personid, residents[[site]]$personid)]
  ## other sites where people who are listed as non-residents responded to surveys
  ## Note: also the case for NP, BUT Ivan has said these should be seen as extra and NOT included
  if(site %in% c("FF", "LB", "YN")) networks[[site]]$sui <- people_observations[[site]]$su_id[match(networks[[site]]$personid, people_observations[[site]]$personid)]
  if(site == "BD") networks[[site]]$sui <- gsub("NA | NA", "",
    paste(
      people_observations[[site]]$su_id[match(networks[[site]]$personid, people_observations[[site]]$personid)],
      gsub("bdsu00", "bdsu0", people_observations[[site]]$altsu[match(networks[[site]]$personid, people_observations[[site]]$personid)])
    )
  ) ## BD has many people who are nominally not resident but are part of SUs and were surveyed, with info stored in 'altsu' column. So, drawing from people_observations not res and bringing together the two SU entries. Everyone has one or the other. Also correcting mistaken # of 0s in altsu
  #if(site == "KM") networks[[site]]$sui <- networks[[site]]$personid
  if(site == "MY") networks[[site]]$sui <- networks[[site]]$su_id

  if(!site %in% c("KM","PE", "BD", "FF", "LB", "YN")) networks[[site]]$suj <- residents[[site]]$su_id[match(networks[[site]]$alterid, residents[[site]]$personid)] ## NOTE: here, I'm using residents not people_observations. At least in sites like AZ/TE, this means some people who actually *do* have a sharing unit will be recorded here without one, as that sharing unit is not for this community. I think that's the right call!
  ## BUT the sites below have some people not listed as residents who reported, and it seems legit
  if(site %in% c("FF", "LB", "YN")) networks[[site]]$suj <- people_observations[[site]]$su_id[match(networks[[site]]$alterid, people_observations[[site]]$personid)]
  if(site == "BD") networks[[site]]$suj <- gsub("NA | NA", "",
    paste(
      people_observations[[site]]$su_id[match(networks[[site]]$alterid, people_observations[[site]]$personid)],
      gsub("bdsu00", "bdsu0", people_observations[[site]]$altsu[match(networks[[site]]$alterid, people_observations[[site]]$personid)])
    )
  )

  ## note: sites with SUs as alters: KM [all], RA [a few], TP [a few, also "orgs"...]
  if(site == "KM") networks[[site]]$suj <- networks[[site]]$alterid

  if(site %in% c("RA","TP")) networks[[site]]$suj[grepl("su", networks[[site]]$alterid)] <- networks[[site]]$alterid[grepl("su", networks[[site]]$alterid)]
  # if(site == "TP") networks[[site]]$suj[grepl("org", networks[[site]]$alterid)] <- networks[[site]]$alterid[grepl("org", networks[[site]]$alterid)] ## but unclear where info on orgs is stored

  ## creating some lists and counts of alters here still with full set of nominees, before restricting to residents
  su_alters_all[[site]] <- networks[[site]] %>% ## for all nominations across ALL questions, including the double-sampled ones and any extra prompts
    dplyr::group_by(sui) %>%
    dplyr::summarise(alterids = list(unique(alterid))) %>%
    dplyr::mutate(tie = "all")

  su_alters_req[[site]] <- networks[[site]][networks[[site]]$tie %in% c(1, 3, 5, 6, 7, 8, 9, 10, "5/6a","7/8"),] %>% ## for main prompts of *requested* support, not *given* support (i.e., no double-sampled). Note the two funny ones are from MK.
    dplyr::group_by(sui) %>%
    dplyr::summarise(alterids = list(unique(alterid))) %>%
    dplyr::mutate(tie = "req")

  su_alters_each[[site]] <- networks[[site]] %>%
    dplyr::group_by(sui, tie) %>%
    dplyr::summarise(alterids = list(unique(alterid)), .groups = "drop")

  su_ex_alters_all[[site]] <- networks[[site]] %>%
    dplyr::group_by(sui) %>%
    filter(!alterid %in% residents[[site]]$personid) %>%
    dplyr::summarise(externalids = list(unique(alterid))) %>%
    dplyr::mutate(tie = "all")

  ## KM has alters as SUs, but a few seem to be of the form KMSU##N##
  ## where SU## is the nominating SU and N## is an ID for an external person
  ## so could filter either by filter(alterid %in% sharing_unit$KM$su_id)
  ## or grepl("n", networks$KM$alterid)
  if(site == "KM") su_ex_alters_all$KM <- networks$KM %>%
    dplyr::group_by(sui) %>%
    filter(grepl("n", alterid)) %>%
    dplyr::summarise(externalids = list(unique(alterid))) %>%
    dplyr::mutate(tie = "all")

  #if(site == "BD") bd_su_members <- people_observations$BD$personid[!is.na(people_observations$BD$su_id)]
  #if(site == "BD") su_ex_alters_all$BD <- networks$BD %>%
  #  group_by(sui) %>%
  #  filter(!alterid %in% bd_su_members) %>%
  #  summarise(externalids = list(unique(alterid))) %>%
  #  mutate(tie = "all")

  su_ex_alters_req[[site]] <- networks[[site]][networks[[site]]$tie %in% c(1, 3, 5, 6, 7, 8, 9, 10, "5/6a","7/8"),] %>% ## for main prompts of *requested* support, not *given* support (i.e., no double-sampled). Note the two funny ones are from MK.
    dplyr::group_by(sui) %>%
    filter(!alterid %in% residents[[site]]$personid) %>%
    dplyr::summarise(externalids = list(unique(alterid))) %>%
    dplyr::mutate(tie = "req")

  if(site == "KM") su_ex_alters_req$KM <- networks$KM[networks$KM$tie %in% c(1, 3, 5, 6, 7, 8, 9, 10),] %>%
    dplyr::group_by(sui) %>%
    filter(grepl("n", alterid)) %>%
    dplyr::summarise(externalids = list(unique(alterid))) %>%
    dplyr::mutate(tie = "req")

  #if(site == "BD") su_ex_alters_req$BD <- networks$BD[networks$BD$tie %in% c(1, 3, 5, 6, 7, 8, 9, 10),] %>%
  #  group_by(sui) %>%
  #  filter(!alterid %in% bd_su_members) %>%
  #  summarise(externalids = list(unique(alterid))) %>%
  #  mutate(tie = "req")


  su_ex_alters_main[[site]] <- networks[[site]][networks[[site]]$tie %in% c(9, 10),] %>% ## keeping only the two prompts focused on external alters
    dplyr::group_by(sui) %>%
    filter(!alterid %in% residents[[site]]$personid) %>%
    dplyr::summarise(externalids = list(unique(alterid)))%>%
    dplyr::mutate(tie = "external_main")

  if(site == "KM") su_ex_alters_main$KM <- networks$KM[networks$KM$tie %in% c(9, 10),] %>%
    dplyr::group_by(sui) %>%
    filter(grepl("n", alterid)) %>%
    dplyr::summarise(externalids = list(unique(alterid))) %>%
    dplyr::mutate(tie = "external_main")

  #if(site == "BD") su_ex_alters_main$BD <- networks$BD[networks$BD$tie %in% c(9, 10),] %>% ## keeping only the two prompts focused on external alters
  #  group_by(sui) %>%
  #  filter(!alterid %in% bd_su_members) %>%
  #  summarise(externalids = list(unique(alterid)))%>%
  #  mutate(tie = "external_main")

  su_ex_alters_each[[site]] <- networks[[site]] %>%
    dplyr::group_by(sui, tie) %>%
    filter(!alterid %in% residents[[site]]$personid) %>%
    dplyr::summarise(externalids = list(unique(alterid)), .groups = "drop")

  if(site == "KM") su_ex_alters_each$KM <- networks$KM %>%
    dplyr::group_by(sui, tie) %>%
    filter(grepl("n", alterid)) %>%
    dplyr::summarise(externalids = list(unique(alterid)), .groups = "drop")

  #if(site == "BD") su_ex_alters_each$BD <- networks$BD %>%
  #  group_by(sui, tie) %>%
  #  filter(!alterid %in% bd_su_members) %>%
  #  summarise(externalids = list(unique(alterid)), .groups = "drop")

  # make dataframes at the individual respondent level
  # Can't use MY, as don't have singular respondent
  if (site != "MY") {
    su_alters_each_indiv[[site]] <- networks[[site]] %>%
      dplyr::group_by(personid, tie) %>%
      dplyr::summarise(
        alterids = list(unique(alterid)),
        sualterids = list(setdiff(unique(suj), NA)), .groups = "drop") %>%
    dplyr::mutate(
      alter_count = map_int(alterids, length),
      su_alter_count = map_int(sualterids, length)
      )
    su_alters_each_indiv[[site]]$sui <- networks[[site]]$sui[match(su_alters_each_indiv[[site]]$personid, networks[[site]]$personid)]

    su_alters_req_indiv[[site]] <- networks[[site]][networks[[site]]$tie %in% c(1, 3, 5, 6, 7, 8, 9, 10, "5/6a","7/8"),] %>% ## for main prompts of *requested* support, not *given* support (i.e., no double-sampled). Note the two funny ones are from MK.
    dplyr::group_by(personid) %>%
    dplyr::summarise(
      alterids = list(unique(alterid)),
      sualterids = list(setdiff(unique(suj), NA)), .groups = "drop") %>%
    dplyr::mutate(
      alter_count = map_int(alterids, length),
      su_alter_count = map_int(sualterids, length)
    )
    su_alters_req_indiv[[site]]$sui <- networks[[site]]$sui[match(su_alters_req_indiv[[site]]$personid, networks[[site]]$personid)]
  }

  # to look at concordance of nominations within a SU
  # Only interested in sites where there are at least 10 SUs where more than one person reported

  if (site %in% names(which(lapply(sharing_unit, function(x) sum(x$num_surv >= 2, na.rm = TRUE)) > 10))) {
    su_alters_each_pair[[site]] <- su_alters_each_indiv[[site]] %>%
      inner_join(su_alters_each_indiv[[site]], by = c("sui", "tie"), relationship = "many-to-many") %>%
      filter(personid.x < personid.y) %>%  # Avoid duplicate and self-pairs
      dplyr::mutate(
        intersection_indiv = map2(alterids.x, alterids.y, ~ intersect(.x, .y)),
        union_indiv = map2(alterids.x, alterids.y, ~ union(.x, .y)),
        prop_agree_indiv = map2_dbl(alterids.x, alterids.y, ~ length(intersect(.x, .y)) / length(union(.x, .y))),
        intersection_su = map2(sualterids.x, sualterids.y, ~ intersect(.x, .y)),
        union_su = map2(sualterids.x, sualterids.y, ~ union(.x, .y)),
        prop_agree_su = map2_dbl(sualterids.x, sualterids.y, ~ length(intersect(.x, .y)) / length(union(.x, .y)))
      ) %>%
      dplyr::select(sui, tie, ID1 = personid.x, ID2 = personid.y, alters1 = alterids.x, alters2 = alterids.y, , sualters1 = sualterids.x, sualters2 = sualterids.y, intersection_indiv, union_indiv, prop_agree_indiv, intersection_su, union_su, prop_agree_su)

      su_alters_req_pair[[site]] <- su_alters_req_indiv[[site]] %>%
      inner_join(su_alters_req_indiv[[site]], by = "sui", relationship = "many-to-many") %>%
      filter(personid.x < personid.y) %>%  # Avoid duplicate and self-pairs
      dplyr::mutate(
        intersection_indiv = map2(alterids.x, alterids.y, ~ intersect(.x, .y)),
        union_indiv = map2(alterids.x, alterids.y, ~ union(.x, .y)),
        prop_agree_indiv = map2_dbl(alterids.x, alterids.y, ~ length(intersect(.x, .y)) / length(union(.x, .y))),
        intersection_su = map2(sualterids.x, sualterids.y, ~ intersect(.x, .y)),
        union_su = map2(sualterids.x, sualterids.y, ~ union(.x, .y)),
        prop_agree_su = map2_dbl(sualterids.x, sualterids.y, ~ length(intersect(.x, .y)) / length(union(.x, .y)))
      ) %>%
      dplyr::select(sui, ID1 = personid.x, ID2 = personid.y, alters1 = alterids.x, alters2 = alterids.y, , sualters1 = sualterids.x, sualters2 = sualterids.y, intersection_indiv, union_indiv, prop_agree_indiv, intersection_su, union_su, prop_agree_su)
  }


  su_alters[[site]] <- rbind(su_alters_all[[site]], su_alters_req[[site]], su_alters_each[[site]])
  su_externals[[site]] <- rbind(su_ex_alters_all[[site]], su_ex_alters_req[[site]], su_ex_alters_main[[site]], su_ex_alters_each[[site]])

  su_alters[[site]] <- su_alters[[site]] %>%
    dplyr::mutate(
      alter_count = map_int(alterids, length),
      alter_status_count = map_int(alterids, ~ sum(people_observations[[site]]$status[people_observations[[site]]$personid %in% .x] == 1, na.rm = TRUE)),
      alter_res_count = map_int(alterids, ~ sum(people_observations[[site]]$is_resident[people_observations[[site]]$personid %in% .x] == 1, na.rm = TRUE)),
      alter_age_av = map_dbl(alterids, ~ mean(people_observations[[site]]$age[people_observations[[site]]$personid %in% .x], na.rm = TRUE))
      )

  su_externals[[site]] <- su_externals[[site]] %>%
    dplyr::mutate(
      externals_count = map_int(externalids, length),
      externals_status_count = map_int(externalids, ~ sum(people_observations[[site]]$status[people_observations[[site]]$personid %in% .x] == 1, na.rm = TRUE)),
      externals_wealth_count = map_int(externalids, ~ sum(people_observations[[site]]$external_wealth[people_observations[[site]]$personid %in% .x] == 1, na.rm = TRUE))
    )

  su_alters[[site]] <- su_alters[[site]] %>%
    left_join(su_externals[[site]], by = c("sui", "tie"))

  ## remove externals -- NOTE: this means (among other things) that if someone names another person associated with a sharing unit but who is not resident (e.g., a grown son living elsewhere) that link to that sharing unit will *not* be retained.
  if(!site %in% c("KM", "MY", "BD", "FF", "LB", "YN")) net <- networks[[site]][networks[[site]]$personid %in% residents[[site]]$personid & networks[[site]]$alterid %in% residents[[site]]$personid ,]
  if(site == "KM") net <- networks[[site]][networks[[site]]$personid %in% residents[[site]]$personid & networks[[site]]$alterid %in% res_su[[site]] ,]
  if(site == "MY") net <- networks[[site]][networks[[site]]$su_id %in% res_su[[site]] & networks[[site]]$alterid %in% residents[[site]]$personid ,]
  ## DECISION: for sites where some nominal 'non-residents' gave responses and those responses seem appropriate, NOT subsetting egos to residents BUT still subsetting alters to residents
  ## So still only retaining 'within community' ties, but with a wider set of nominators
  if(site %in% c("BD", "FF", "LB", "YN")) net <- networks[[site]][networks[[site]]$alterid %in% residents[[site]]$personid ,]
  ## an alternative is to retain all cases where egos and alters have a SUID:
  #if(site %in% c("BD", "FF", "LB", "YN")) net <- networks[[site]][networks[[site]]$suj != "NA",]

  ## As here only interested in SU-SU ties, further making sure that all nodes have a SUID.
  net <- net[!is.na(net$sui) & !is.na(net$suj),]

  net$tie <- gsub("[[:punct:]]","",net$tie)

  ## some sites don't have nominations to other sharing units for some prompts, esp. 9 and 10
  ## (BM, CP, HE, MN, NI, SH, TS, VT)
  ## so still want to create empty adjacency matrices for them
  ## for some other sites, they are missing prompts, so don't want to create adj matrices for them
  tietypes <- unique(net$tie)
  if (!site %in% c("MK", "IH", "PC", "WH", "WL")) tietypes <- c(1:10, tietypes[!tietypes %in% 1:10])
  if (site %in% c("IH", "PC", "WH", "WL")) tietypes <- c(c(1, 3:8), tietypes[!tietypes %in% c(1, 3:8)]) ## sites are missing 2, 9, and 10
  if (site == "MK") tietypes <- c(1:4, tietypes[! tietypes %in% c(1:4)]) ## MK is missing 9 and 10, and has collapsed 5/6 and 7/8 questions
  tietypes <- sort(tietypes)
  tietypes <- tietypes[!tietypes %in% c("relations",
                                        "relbin",
                                        "relord",
                                        "relations-close",
                                        "relations-affine",
                                        "relation",
                                        "relative",
                                        "lineage",
                                        "villages visited")]

  tietypes <- gsub("[[:punct:]]","",tietypes) ## removing punctuation, list from MK abnormal names.

  ## creating adjacency matrices that sum up all nominations from one SU to another.
  ## so, e.g., if one person names two people in another SU, will end up with a weight of 2.
  ## and if two people from the same household name two people in another SU, will end up with a weight of 4.
  sn <- sort(res_su[[site]])
  adjmats <- list()
  for (m in 1:length(tietypes)) {
    thistie <- dplyr::select(net[net$tie == tietypes[[m]], ], ego = sui, alter = suj)

    tab <- table(factor(thistie$ego, levels = sn), factor(thistie$alter, levels = sn))
    adjmats[[m]] <- matrix(as.integer(tab), nrow = length(sn), ncol = length(sn),
                            dimnames = list(sn, sn))

    names(adjmats)[m] <- paste0("e", tietypes[m])

    # Write out the adjacency matrices
    write.csv(adjmats[[m]], file.path(output_path, paste0(site, "-su-adjmat-", tietypes[m], ".csv")))
  }

  ## We also want to create a collapsed network, where we collapse ties within respondent.
  ## Collapse ties within respondent (if you nominate two members of the other household, that is just one tie), then divide by the number of egos.

  adjmats_respondent_collapse <- list()

  # "thistie" corresponds to a given tie type (e.g., layer 1)
  # it is a two column dataframe that lists all (directed) ties,
  # with an ego column and an alter column.

  for (m in 1:length(tietypes)) {
    if (site != "MY") {
      thistie <-
        dplyr::select(net[net$tie == tietypes[[m]], ],
               ego = sui,
               alter = suj,
               person_ego = personid,
               person_alter = alterid)
    } else if (site == "MY") {
      ## In site MY, responses are based on joint responses from
      ## simultaneous interviews of boths heads of household.
      ## As a result, the net object does not have a personid
      ## but instead a female_hh and male_hh id.
      ## Using either is arbitrary. We can just use the female_hh id.
      ## (Since either way there was only one opportunity to nominate.)
      thistie <-
        dplyr::select(net[net$tie == tietypes[[m]], ],
               ego = sui,
               alter = suj,
               person_ego = female_hh,
               person_alter = alterid)
    }


    # collapse ties within respondent: one row per unique (ego, alter, person_ego)
    thistie_unique <- dplyr::distinct(thistie, ego, alter, person_ego)

    tab <- table(factor(thistie_unique$ego, levels = sn), factor(thistie_unique$alter, levels = sn))
    adjmats_respondent_collapse[[m]] <- matrix(as.integer(tab), nrow = length(sn), ncol = length(sn),
                                                dimnames = list(sn, sn))

    names(adjmats_respondent_collapse)[m] <- paste0("e", tietypes[m])

    # Write out the adjacency matrices
    write.csv(adjmats_respondent_collapse[[m]], file.path(output_path, paste0(site, "-su-adjmat-respondent-collapse-", tietypes[m], ".csv")))
  }

  # if(!site %in% c("MY")) { ## MY has num_surv now.

    su_nets[[site]] <-
      lapply(adjmats,
            function(x) {x <- graph_from_adjacency_matrix(x, mode = "directed", weighted = TRUE)}
            %>% set_vertex_attr("sampled", value = V(x)$name %in% sampled_su[[site]])
            %>% set_vertex_attr("num_surv", value = sharing_unit[[site]]$num_surv[match(V(x)$name, sharing_unit[[site]]$su_id)])
            %>% set_vertex_attr("num_surv_male_q", value = sharing_unit[[site]]$num_surv_male_q[match(V(x)$name, sharing_unit[[site]]$su_id)])
            %>% set_vertex_attr("num_surv_female_q", value = sharing_unit[[site]]$num_surv_female_q[match(V(x)$name, sharing_unit[[site]]$su_id)])
            %>% set_vertex_attr("su_wealth", value = sharing_unit[[site]]$su_wealth[match(V(x)$name, sharing_unit[[site]]$su_id)])
            %>% set_vertex_attr("susize", value = sharing_unit[[site]]$su_size[match(V(x)$name, sharing_unit[[site]]$su_id)])
    )

    su_nets_recipient_collapse[[site]] <-
      lapply(adjmats_respondent_collapse,
             function(x) {x <- graph_from_adjacency_matrix(x, mode = "directed", weighted = TRUE)}
             %>% set_vertex_attr("sampled", value = V(x)$name %in% sampled_su[[site]])
             %>% set_vertex_attr("num_surv", value = sharing_unit[[site]]$num_surv[match(V(x)$name, sharing_unit[[site]]$su_id)])
             %>% set_vertex_attr("num_surv_male_q", value = sharing_unit[[site]]$num_surv_male_q[match(V(x)$name, sharing_unit[[site]]$su_id)])
             %>% set_vertex_attr("num_surv_female_q", value = sharing_unit[[site]]$num_surv_female_q[match(V(x)$name, sharing_unit[[site]]$su_id)])
             %>% set_vertex_attr("su_wealth", value = sharing_unit[[site]]$su_wealth[match(V(x)$name, sharing_unit[[site]]$su_id)])
             %>% set_vertex_attr("susize", value = sharing_unit[[site]]$su_size[match(V(x)$name, sharing_unit[[site]]$su_id)])
             )

  # }

  # if(site == "MY") {
  #
  #   su_nets[[site]] <- lapply(adjmats,
  #                             function(x) {x <- graph_from_adjacency_matrix(x, mode = "directed", weighted = TRUE)}
  #                             %>% set_vertex_attr("sampled", value = V(x)$name %in% sampled_su[[site]])
  #                             %>% set_vertex_attr("num_surv", value = NA)
  #                             %>% set_vertex_attr("su_wealth", value = sharing_unit[[site]]$su_wealth[match(V(x)$name, sharing_unit[[site]]$su_id)])
  #                             %>% set_vertex_attr("susize", value = sharing_unit[[site]]$su_size[match(V(x)$name, sharing_unit[[site]]$su_id)])
  #   )
  #
  #   su_nets_recipient_collapse[[site]] <-
  #     lapply(adjmats_respondent_collapse,
  #            function(x) {x <- graph_from_adjacency_matrix(x, mode = "directed", weighted = TRUE)}
  #            %>% set_vertex_attr("sampled", value = V(x)$name %in% sampled_su[[site]])
  #            %>% set_vertex_attr("num_surv", value = NA)
  #            %>% set_vertex_attr("su_wealth", value = sharing_unit[[site]]$su_wealth[match(V(x)$name, sharing_unit[[site]]$su_id)])
  #            %>% set_vertex_attr("susize", value = sharing_unit[[site]]$su_size[match(V(x)$name, sharing_unit[[site]]$su_id)]))
  #
  # }


  su_nets[[site]]$`avrel` <- graph_from_adjacency_matrix(su_m_avr[[site]], mode = "undirected", weighted = TRUE)
  su_nets[[site]]$`avrel` <- delete_edges(
    su_nets[[site]]$`avrel`,
    which(edge_attr(su_nets[[site]]$`avrel`, "weight") == 0)
  )

  su_nets_recipient_collapse[[site]]$`avrel` <- graph_from_adjacency_matrix(su_m_avr[[site]], mode = "undirected", weighted = TRUE)
  su_nets_recipient_collapse[[site]]$`avrel` <- delete_edges(
    su_nets_recipient_collapse[[site]]$`avrel`,
    which(edge_attr(su_nets_recipient_collapse[[site]]$`avrel`, "weight") == 0)
  )

  su_nets[[site]]$`maxrel` <- graph_from_adjacency_matrix(su_m_maxr[[site]], mode = "undirected", weighted = TRUE)
  su_nets[[site]]$`maxrel` <- delete_edges(
    su_nets[[site]]$`maxrel`,
    which(edge_attr(su_nets[[site]]$`maxrel`, "weight") == 0)
  )

  su_nets_recipient_collapse[[site]]$`maxrel` <- graph_from_adjacency_matrix(su_m_maxr[[site]], mode = "undirected", weighted = TRUE)
  su_nets_recipient_collapse[[site]]$`maxrel` <- delete_edges(
    su_nets_recipient_collapse[[site]]$`maxrel`,
    which(edge_attr(su_nets_recipient_collapse[[site]]$`maxrel`, "weight") == 0)
  )

  su_nets[[site]]$`sibs` <- su_nets_recipient_collapse[[site]]$`sibs` <- graph_from_adjacency_matrix(su_m_sib[[site]], mode = "undirected", weighted = FALSE)
  su_nets[[site]]$`parents` <- su_nets_recipient_collapse[[site]]$`parents`  <- graph_from_adjacency_matrix(su_m_par[[site]], mode = "directed", weighted = FALSE)
  su_nets[[site]]$`children` <- su_nets_recipient_collapse[[site]]$`children` <- graph_from_adjacency_matrix(su_m_chi[[site]], mode = "directed", weighted = FALSE)

  ## STUPID BUG: NA entries in su_m_dist keep igraph from recognizing a matrix as symmetric
  su_m_dist[[site]][is.na(su_m_dist[[site]])] <- 9999999999
  su_nets[[site]]$`distance` <- graph_from_adjacency_matrix(su_m_dist[[site]], mode = "undirected", weighted = TRUE)
  su_nets[[site]]$`distance` <- delete_edges(
    su_nets[[site]]$`distance`,
    which(edge_attr(su_nets[[site]]$`distance`, "weight") == 9999999999)
  )

  su_nets_recipient_collapse[[site]]$`distance` <- graph_from_adjacency_matrix(su_m_dist[[site]], mode = "undirected", weighted = TRUE)
  su_nets_recipient_collapse[[site]]$`distance` <- delete_edges(
    su_nets_recipient_collapse[[site]]$`distance`,
    which(edge_attr(su_nets_recipient_collapse[[site]]$`distance`, "weight") == 9999999999)
  )


  # Writing out sharing unit info just for resident SUs
  su_meta[[site]] <- sharing_unit[[site]][sharing_unit[[site]]$su_id %in% res_su[[site]],]
  su_meta[[site]] <- su_meta[[site]][order(su_meta[[site]]$su_id),]
  write.csv(su_meta[[site]], file.path(output_path, paste0(site, "-su-meta.csv")), row.names = FALSE)

  ## quick attempt to get all nominations at individual level. if resident not nominated, won't appear as isolate.

  # each_i <- split(networks[[site]], networks[[site]]$tie)
  # each_i <- each_i[!names(each_i) %in% c("relations",
  #                                        "relbin",
  #                                        "relord",
  #                                        "relations-close",
  #                                        "relations-affine",
  #                                        "relation",
  #                                        "relative",
  #                                        "lineage",
  #                                        "villages visited")]
  #
  # if(site != "MY") indiv_nets <- lapply(each_i, function(x) {x <- igraph::graph_from_data_frame(x[,2:4], directed = TRUE)}
  #                      %>% set_vertex_attr("surveyed", value = V(x)$name %in% egos[[site]])
  #                      %>% set_vertex_attr("resident", value = V(x)$name %in% residents[[site]]$personid)
  # )
  #
  # if(site == "MY") indiv_nets <- lapply(each_i, function(x) {x <- igraph::graph_from_data_frame(x[,2:4], directed = TRUE)})
  #
  # indiv_nets$`kinship` <- graph_from_adjacency_matrix(kin_mat[[site]], mode = "undirected", weighted = TRUE)
  #
  # indiv_nets$`kinship` <- delete_edges(indiv_nets$`kinship`, which(E(indiv_nets$`kinship`)$weight == 0))
  # ## keeping diagonal, as could potentially get > 1

}


# message("Processing PE separately.")
# source("process-PE.R")


## REMOVE SITES ##
message(paste0("Dropping ", paste(SITES_TO_DROP, collapse = ", "), ". If this isn't as intended, revisit SITES_TO_DROP in 00_run_all.R"))
su_nets <- su_nets[!names(su_nets) %in% SITES_TO_DROP]
su_nets_recipient_collapse <- su_nets_recipient_collapse[!names(su_nets_recipient_collapse) %in% SITES_TO_DROP]
su_meta <- su_meta[!names(su_meta) %in% SITES_TO_DROP]
su_alters <- su_alters[!names(su_alters) %in% SITES_TO_DROP]

message(paste0("Saving su_nets, su_nets_recipient_collapse, su_meta, and su_alters to: ", output_path))
save(su_nets, file = file.path(output_path, "su_nets.rdata"))
save(su_nets_recipient_collapse, file = file.path(output_path, "su_nets_recipient_collapse.rdata"))
save(su_meta, file = file.path(output_path, "su_meta.rdata"))
save(su_alters, file = file.path(output_path, "su_alters.rdata"))
save(people_observations, file = file.path(output_path, "people_observations.rdata")) # saving to allow for use in descriptive stats

rm(
  adjmats,
  av_rel,
  egos,
  j,
  kin_edge,
  thistie,
  m,
  tietypes,
  su_alters_all,
  su_alters_req,
  su_alters_each,
  su_ex_alters_all,
  su_ex_alters_req,
  su_ex_alters_main,
  su_ex_alters_each,
  num_surv,
  num_surv_male_q,
  num_surv_female_q
)
