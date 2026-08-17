######################################
## CLEANING/STANDARDISING VARIABLES ##
######################################

# this file can be sourced to clean the following variables:

# from people_observations
# - age (also create a binary "is_adult" variable)
# - status
# - external wealth
# - head (binary)
# - years education (some actually on ordinal scale, not years)
# - other noetic (mix of binary and ordinal)
# - work ability (results in two diff ordinal scales; one with 3 and one with 4 levels)
# - is resident (binary, just recoding NAs to 0s)

# from sharing_unit
# - food security measures (creates also ordinal versions of them)
#    - smaller_meals
#    - fewer_meals
#    - no_food
#    - sleep_hungry
#    - without_eating
# - parental edu
# - appends su wealth

# load database

# set name for Endow Database folder, relative to your current working directory
# EndowDatabase <- "../endow-database"

people_observations <-
  Map(read.csv,
      Sys.glob(file.path(EndowDatabase,
                         "people_observations",
                         "*_people_observations.csv")))
people <-
  Map(read.csv,
      Sys.glob(file.path(EndowDatabase,
                         "people",
                         "*_people.csv")))
sharing_unit <-
  Map(read.csv,
      Sys.glob(file.path(EndowDatabase,
                         "su_observations",
                         "*_su_observations.csv")))
poss_cost <-
  Map(read.csv,
      Sys.glob(file.path(EndowDatabase,
                         "possession_costs",
                         "*_possession_costs.csv")))
networks <-
  Map(read.csv,
      Sys.glob(file.path(EndowDatabase,
                         "networks",
                         "*_networks.csv")))
partnerships <-
  Map(read.csv,
      Sys.glob(file.path(EndowDatabase,
                         "partnerships",
                         "*_partnerships.csv")))
residents <-
  Map(read.csv,
      Sys.glob(file.path(EndowDatabase,
                         "residents",
                         "*_residents.csv")))
su_distances <-
  Map(read.csv,
      Sys.glob(file.path(EndowDatabase,
                         "su_distances",
                         "*_su_distances.csv")))
su_wealth <-
  Map(read.csv,
      Sys.glob(file.path("su-wealth-output",
                         "*_SU_wealth.csv")))


# Make a list of the above list object names
obj_list <- c("people_observations", "people", "sharing_unit", "poss_cost",
              "networks", "partnerships", "residents", "su_distances", "su_wealth")

# Renaming list elements to just site codes
extract_capitals_after_last_slash <- function(input_string) {
  pattern <- ".*/([A-Z]{2}).*\\.csv"
  match <- sub(pattern, "\\1", input_string)

  # If no match is found, try to match directly without a forward slash
  if (match == input_string) {
    pattern <- "([A-Z]{2}).*\\.csv"
    match <- sub(pattern, "\\1", input_string)
  }

  # Check if a match was found
  if (grepl("[A-Z]{2}", match)) {
    return(match)
  } else {
    stop("Error: No match found.")
  }
}

for (obj_name in obj_list) {
  element_names <- names(get(obj_name))
  new_names <- lapply(element_names, extract_capitals_after_last_slash)
  assign(obj_name, setNames(get(obj_name), new_names))
}
rm("obj_list", "obj_name", "new_names", "element_names")

## Basic recoding
values_to_NA <- c(
  "",
  " ",
  "missing",
  "unknown",
  "999",
  "unclear",
  "not known",
  "dont know",
  "dontknow",
  "n/a",
  "dk",
  "98 unknown",
  "99 missing or not applicable",
  "no sabe",
  "?"
)

recode_NAs <- function(df, values_to_NA) {
  df[] <- lapply(df, function(x) {
    x[x %in% values_to_NA] <- NA
    return(x)
  })
  return(df)
}

people_observations <- lapply(people_observations, recode_NAs, values_to_NA = values_to_NA)
sharing_unit <- lapply(sharing_unit, recode_NAs, values_to_NA = values_to_NA)



###### PEOPLE_OBSERVATIONS

## AGE

# ## MN has someone at 2016.870637
# ## TS has a good number over 100, up to 121.119781
# ## KM has someone that's 124? (and a good number over 100)
# ## MK has very few ages -- additional variable that's an ordinal ranking
# ## MY has 40-50, "adult","old adult", "very old adult", and "young adult" -- those seem to be only for externals, so maybe fine as unlikely to use that
# people_observations$CP$age[is.na(people_observations$CP$age) == TRUE] <- 2019 - people_observations$CP$birth_year[is.na(people_observations$CP$age) == TRUE]
# people_observations$MY$age <- dplyr::recode(people_observations$MY$age, "40-50" = "45")
# people_observations$PS$age <- dplyr::recode(people_observations$PS$age, "20s?" = "25", "50s?" = "55", "60s?" = "65") ## change agreed by Emily Post
# people_observations$VT$age[is.na(people_observations$VT$age) == TRUE] <- 2018 - people_observations$VT$birth_year[is.na(people_observations$VT$age) == TRUE]
# people_observations$YI$birth_year[people_observations$YI$birth_year == "shuyang"] <- 1967
# people_observations$YI$birth_year[people_observations$YI$birth_year == "1983-4"] <- 1984
# people_observations$YI$birth_year <- as.numeric(people_observations$YI$birth_year)
# people_observations$YI$age[is.na(people_observations$YI$age) == TRUE] <- 2018 - people_observations$YI$birth_year[is.na(people_observations$YI$age) == TRUE]

## note this turns some text entries (e.g., MY ones) to NAs (those at least are preserved in MY$other_age variable)
for (site in names(people_observations)){
  if (!is.numeric(people_observations[[site]]$age)) {
    people_observations[[site]]$age <- as.numeric(people_observations[[site]]$age)
  }
}


## creating binary "adult" variable to deal with cases of unclear ages, like MK, so can tally adults
people_observations <- lapply(people_observations, function(x) {
  x$is_adult <- ifelse(x$age >= 18, 1, 0)
  return(x)
})

# people_observations$MK$is_adult <- ifelse(people_observations$MK$age >= 18 | people_observations$MK$agecode >= 3, 1, 0)
# people_observations$MY$is_adult <- ifelse(people_observations$MY$age >= 18 |people_observations$MY$other_age %in% c("adult", "old adult", "very old adult"), 1, 0)
# people_observations$MY$is_adult[people_observations$MY$other_age == "young adult"] <- 0


## STATUS
## Pulling this partially from https://github.com/danielRedhead/ENDOW-leadership-analysis/blob/main/code/analyses.R
## This almost surely needs some further consideration, specific to the analytical aim. Some have very few, others many more.

## LB and YN -- Siobhan's two China sites -- have very different meaning...
# Local	1
# Ninglang Area	2
# Province	3
# China (and Tibet)	4
# Outside China	5
## Assuming something similar for her VT site (can't make heads or tails of it!)
# people_observations$LB$status <- ifelse(people_observations$LB$status > 3, 1, 0) ## measure is *where* the person is (local up to elsewhere in China or world; dichotomising as all outside the province), but note very different type of measure!
# people_observations$YN$status <- ifelse(people_observations$YN$status > 3, 1, 0) ## measure is *where* the person is (local up to elsewhere in China or world; dichotomising as all outside the province), but note very different type of measure!
# people_observations$VT$status <- NA ## remove/revise once we hear from Siobhan!
# people_observations$SH$status <- ifelse(people_observations$SH$status > 19, 1, 0) ## SH is # of years on land -- Katie says to set threshhold as 20 years or more
# people_observations$CR$status[people_observations$CR$status == 2] <- 1 ## 2 was for past leadership
# people_observations$HE$status[people_observations$HE$status %in% c("2", "3", "4", "5")] <- 1 ## varieties of leaders
# people_observations$AV$status <- ifelse(people_observations$AV$status > 14, 1, 0) ## higher ranging numbers, meaning we think is nominations as 'respect', as placeholder doing above 3rd quart for adult res
# people_observations$DJ$status <- ifelse(people_observations$DJ$status > 11, 1, 0) ## higher ranging numbers, meaning we think is nominations as 'respect', as placeholder doing above 3rd quart for adult res
# people_observations$MC$status[people_observations$MC$status == "998"] <- 0
# people_observations$MC$status <- ifelse(people_observations$MC$status >= 0.5, 1, 0) ## some values of -1, one 998; partial answers, per Curtis, reflect the average of responses when multiple people reported; recoding >=0.5 to 1

for(site in names(people_observations)) {
  people_observations[[site]]$status[people_observations[[site]]$status %in% c(
    "participant in a village government organization",
    "leader in a village government organization",
    "yes",
    "leader",
    "high",
    "government",
    "guardobosque",
    "indigenous territorial government member",
    "indigenous territorial government president",
    "kipla women's president",
    "ngo",
    "politician",
    "religious leader",
    "village leader",
    "l"
  )] <- 1

  people_observations[[site]]$status[people_observations[[site]]$status %in% c(
    "not a participant in a village government organization",
    "non-leader",
    "no",
    "middle",
    "low"
  )] <- 0

  people_observations[[site]]$status[people_observations[[site]]$status %in% c(
    "",
    " ",
    "dont know",
    "dontknow",
    "dk",
    "other",
    "ddd",
    "missing"
  )] <- NA

  #people_observations[[site]]$status <- round(as.numeric(people_observations[[i]]$status),0)
}

## status for external alters
## this gets folded into the general "status" variable
## missing: MG, MK [says is missing, but did record NGO/GO affil], MY [says is missing; have only wealth measure], TS [only 2 in full sample have status==1, neither external], Siobhan (LB, YN, VT); Cody (IH, PC, WH, WL); Augusto (DJ, AV)
# HE has externalstatus
## MG says to use villagegov (status) or tangalamena; but either missing or all "no" for external alters
## BY says to use status_farmer or statuswealth_bayakadance [note that we're also using this variable for external wealth, so doubling up.]
## PT implies there in status measure, but all 0s; going off of occupation

# people_observations$BY$external_status <- ifelse(people_observations$BY$statuswealth_bayakadance == "owns dance" | people_observations$BY$status_farmer != 0, 1, 0)
# 
# people_observations$MK$external_status <- ifelse(people_observations$MK$org == 1 | people_observations$MK$govwork == 1 | people_observations$MK$churchwork == 1, 1, 0) ## directed not to count militia
# 
# people_observations$PE$external_status <- people_observations$PE$statusofalter
# people_observations$PE$external_status[people_observations$PE$external_status == "na"] <- NA
# 
# people_observations$PT$external_status <- ifelse(people_observations$PT$occupation %in% c("city councilor", "civil servant","doctor","navy","ngo","parkranger","policeman"), 1, 0)

## incorporating external status measures in with status variable; in all cases non-overlapping
## note, though, that in some cases these measures could mean different things! e.g., MK internal status is spirit mediums, external status (which includes some internals, too) is more roles
# people_observations$HE$status <- ifelse(people_observations$HE$status == 1 | people_observations$HE$externalstatus == 1, 1, 0)
# people_observations$BY$status <- ifelse(people_observations$BY$status == 1 | people_observations$BY$external_status == 1, 1, 0)
# people_observations$PE$status <- ifelse(people_observations$PE$status == 1 | people_observations$PE$external_status == 1, 1, 0)
# people_observations$PT$status <- ifelse(people_observations$PT$status == 1 | people_observations$PT$external_status == 1, 1, 0)



## EXTERNAL WEALTH

## missing: IH/WH/WL/PC [Cody] and AV/DJ [Augusto], MG [says is missing], PS external_wealth measure actually was only asked of *community* members, so would be missing fully for externals.
## ordinal: BM, KM, VT
## many levels: HE, MA
## CP combines homeownership, car/motorcycle ownership, and occupation prestige...

# people_observations$BD$external_wealth <- ifelse(people_observations$BD$ownland_a == 1, 1, 0) ## specifies this in collection-entry metadata
# people_observations$BM$external_wealth <- ifelse(people_observations$BM$external_wealth >= 0.5, 1, 0) # house material: 0 = natural, 0.5 = mixed, 1 = store bought; Shane says dichotomy should be natural = 0, mixed/store = 1
# people_observations$BY$external_wealth <- ifelse(people_observations$BY$wealth_farmer == 1 | people_observations$BY$statuswealth_bayakadance == "owns dance", 1, 0)
# people_observations$BY$external_wealth[people_observations$BY$wealth_farmer != 1 | people_observations$BY$statuswealth_bayakadance != "owns dance"] <- 0 ## this line shouldn't really be necessary and only doesn't create issues because the variables do not overlap; trying to also add the 0s, which do not overlap
# 
# people_observations$CP$external_wealth <- people_observations$CP$car ## using car! homeowner seems quite pervasive
# people_observations$FF$external_wealth <- people_observations$FF$car ## in collection-entry-metadata, says to use car if need to use one; has brickhouse, cattle (count), smallstock, and car
# people_observations$FF$external_wealth <- dplyr::recode(people_observations$FF$external_wealth,
#   "n0" = "0",
#   "no" = "0",
#   "yes" = "1"
# )
# people_observations$HE$external_wealth <- dplyr::recode(people_observations$HE$external_wealth,
#   "cement" = "1",
#   "timbered" = "1",
#   "sago" = "0"
# ) ## Gianluca supports this dichotimizing in 18 Sept 2023 email
# people_observations$HI$external_wealth <- dplyr::recode(people_observations$HI$external_wealth,
#   "poor" = "0",
#   "rich" = "1"
# )
# people_observations$KA$external_wealth <- people_observations$KA$cartru ## electing to go for this one for the moment!
# people_observations$KM$external_wealth <- dplyr::recode(people_observations$KM$external_wealth,
#   "low wealth" = "0",
#   "middle wealth" = "0",
#   "upper wealth" = "1"
# ) # Drew would put low and middle together and separate them from “upper.”
# people_observations$MA$external_livestock <- people_observations$MA$external_wealth
# people_observations$MA$external_wealth <- ifelse(people_observations$MA$external_livestock >= 200, 1, 0) ## ARBITRARY CUTOFF FOR NOW!
# people_observations$MC$external_wealth <- ifelse(people_observations$MC$external_wealth >= 0.5, 1, 0) ## per Curtis, reflect the average of responses when multiple people reported; recoding >=0.5 to 1
# 
# people_observations$MY$external_wealth <- ifelse(people_observations$MY$other_status > 1, 1, 0) ## other_status is actually a wealth measure: 0 = equivalent to wealth of village, 1 = a little wealthier, 2 = moderately wealthier, 3 = much wealthier
# people_observations$PE$external_wealth <- dplyr::recode(people_observations$PE$wealthofalter,
#   "x" = "0",
#   "y" = "1",
#   "na" = NA_character_
# )
# 
# people_observations$PT$external_wealth <- dplyr::recode(people_observations$PT$external_wealth,
#   "medium" = "0",
#   "high" = "1"
# )
# people_observations$PT$external_wealth <- NA ## per Emily Post: We were unable to record the same material wealth measure for external alters (i.e. modernized bathroom) because often residents explained to us they had never actually been to the homes of their external alters, primarily because many of these were connections they maintained after the alter had moved away to live on another islands/countries, which the ego had never been able to visit. So just to confirm, we have no recorded external wealth in this way for external alters.
# people_observations$SN$external_wealth <- ifelse(people_observations$SN$car == 1 | people_observations$SN$motorcycle == 1 | people_observations$SN$bicycle == 1, 1, 0) ## TEMP reconstruction of variable before this is run through in source files
# people_observations$TN$external_wealth <- ifelse(people_observations$TN$fridge == 1 & people_observations$TN$vundi == 1, 1, 0) ## if they have BOTH a fridge and a vehicle, 1 else 0
# people_observations$VT$external_wealth <- ifelse(people_observations$VT$external_wealth > 2, 1, 0) ## THIS IS ARBITRARY! DON'T KNOW WHAT 1, 2, 3, 4 for this variable means!
# people_observations$YI$external_wealth <- people_observations$YI$own_vehicle_yn
# people_observations$YI$external_wealth <- dplyr::recode(people_observations$YI$external_wealth,
#   "n" = "0",
#   "y" = "1"
# )

for (site in names(people_observations)) {
  people_observations[[site]]$external_wealth <- as.numeric(people_observations[[site]]$external_wealth)
}


## HEAD

## missing for AV/DJ

# people_observations$LB$head <- as.numeric(people_observations$LB$personid %in% setdiff(unique(c(sharing_unit$LB$malehead, sharing_unit$LB$femalehead)), NA))
# people_observations$YN$head <- as.numeric(people_observations$YN$personid %in% setdiff(unique(c(sharing_unit$YN$malehead, sharing_unit$YN$femalehead)), NA))
# 
# people_observations$TZ$head[people_observations$TZ$head == "in 2023 census but family not interviewed"] <- NA

for (site in setdiff(names(people_observations), c("AV", "DJ"))){
  if (!is.numeric(people_observations[[site]]$head)) {
    people_observations[[site]]$head <- dplyr::recode(people_observations[[site]]$head,
      "no" = "0",
      "yes" = "1",
      "false" = "0",
      "true" = "1",
      "household head" = "1",
      "household member" = "0",
      "self" = "1"
    )
    people_observations[[site]]$head <- as.numeric(people_observations[[site]]$head)
  }
}


## YEARS EDUCATION

## BD is ordinal (none, sign name, literate, primary edu, secondary, higher sec, degree, unknown, missing or NA)
## BY has "attended" for 4 indivs -- setting to 1, as this is the mean, median, and mode
## HI has binary no/yes -- treating as ordinal?
## KM has general, higher, middle, none -- note from Drew: "“None” is the lowest, followed by “General” (this corresponds roughly to our grades K-11), followed by “Middle” (this is roughly equivalent to technical training and/or some university but no degree) and then “Higher” (equivalent to an undergraduate degree and beyond to graduate school…in this case, I expect most of the people in this category have undergraduate degrees). For what it’s worth, I did record the number of years of school for all people in “General” category. I don’t have it in my data file currently, but with some time I could add it.  This number would probably pick up some generational differences related to education in the community.  Older folks (alive and deceased) often only have 4-6 years of General education, while younger folks typically have 9-11 years)."
## PS has college, none, preschool, primary, secondary, university

# people_observations$BD$years_education <- dplyr::recode(people_observations$BD$years_education,
#   "0 none" = "0"
# )
# people_observations$BY$years_education <- dplyr::recode(people_observations$BY$years_education,
#   "attended" = "1"
# ) ## this is the modal response, the median response, and the rounded mean.
# people_observations$MG$years_education <- dplyr::recode(people_observations$MG$years_education,
#   "no education" = "0",
#   "preschool" = "0.5",
#   "1-year education (primary school)" = "1",
#   "2-year education (primary school)" = "2",
#   "3-year education (primary school)" = "3",
#   "4-year education (primary school)" = "4",
#   "5-year education (primary school)" = "5",
#   "6-year education (secondary school 1)" = "6",
#   "7-year education (secondary school 1)" = "7",
#   "8-year education (secondary school 1)" = "8",
#   "9-year education (secondary school 1)" = "9",
#   "10-year education (secondary school 2)" = "10",
#   "11-year education (secondary school 2)" = "11",
#   "12-year education (secondary school 2)" = "12",
#   "13-year education (higher education)" = "13"
# )
# 
# people_observations$PS$years_education <- dplyr::recode(people_observations$PS$years_education,
#   "none" = "0"
# )
# people_observations$VT$years_education <- dplyr::recode(people_observations$VT$years_education,
#   "13+" = "13",
#   "12+" = "12",
#   "7+" = "7",
#   "o" = "0"
# )
# people_observations$MC$years_education[people_observations$MC$years_education == 48] <- 18 # recoding one crazy outlier to next highest!
# people_observations$YN$years_education <- dplyr::recode(people_observations$YN$years_education, "kg" = "0.5")
# 
# for (site in c("BY", "MG", "VT","YN")) { ## these all have character strings in the full people_obs
#   people_observations[[site]]$years_education <- as.numeric(people_observations[[site]]$years_education)
# }

## Treating some as ordinal, as this will still work with "max"
# people_observations$BD$years_education <- ordered(people_observations$BD$years_education,
#   levels = c("0", "1 sign name", "2 literate", "3 primary education", "4 secondary education", "5 higher secondary certificate", "6 degree")
# )
# people_observations$HI$years_education <- ordered(people_observations$HI$years_education,
#   levels = c("no", "yes")
# )
# people_observations$KM$years_education <- ordered(people_observations$KM$years_education,
#   levels = c("none", "general", "middle", "higher")
# ) # “None” is the lowest, followed by “General” (this corresponds roughly to our grades K-11), followed by “Middle” (this is roughly equivalent to technical training and/or some university but no degree) and then “Higher” (equivalent to an undergraduate degree and beyond to graduate school…in this case, I expect most of the people in this category have undergraduate degrees).
# people_observations$PS$years_education <- ordered(people_observations$PS$years_education,
#   levels = c("0", "preschool", "primary", "secondary", "college", "university")
# )


## OTHER NOETIC
## missing: HI, LB, VT, YN
## ordinal: BD, BY, FJ, KA, KM, KO, MA, MN, MY, TI, TP, TS, NP
## PT is categorical: 0 – no noetic wealth, 1 – ability to fix large boats, 2 – ability produce handcraft, 3 – ability to build canoes

# AV and DJ have ranges with minimum of 2 and max of 10 or 12, median of 5
# people_observations$AV$other_noetic_true <- ifelse(people_observations$AV$other_noetic > 5, TRUE, FALSE)
# people_observations$DJ$other_noetic_true <- ifelse(people_observations$DJ$other_noetic > 5, TRUE, FALSE)
# 
# people_observations$BD$other_noetic <- ordered(people_observations$BD$knowquran_r,
#   levels = c("4 none", "3 basic", "2 good", "1 very good")
# )# Mary says either good/very good or just very good (latter being on 24 individuals; former being ~150) in 17 Sept 2023 email; fine with either
# people_observations$BD$other_noetic_true <- people_observations$BD$other_noetic > "3 basic"
# 
# people_observations$BY$other_noetic <- ordered(people_observations$BY$other_noetic,
#   levels = c("none", "intermediate", "full")
# )  # intermediate == understands but doesn't speak Lingala; if dichotomize put to full == 1, else == 0
# people_observations$BY$other_noetic_true <- people_observations$BY$other_noetic > "intermediate"
# 
# people_observations$CP$other_noetic <- as.numeric(people_observations$CP$other_noetic)
# people_observations$CR$other_noetic <- dplyr::recode(people_observations$CR$other_noetic,
#   `2` = 1L
# ) ## CR has 2?, given the metadata, this seems like a coding error and should be 1.
# people_observations$FF$other_noetic <- ordered(people_observations$FF$englishcompetency,
#   levels = c("none", "little", "conversational")
# ) ## if dichotomize presumably conversational == 1 else == 0
# people_observations$FF$other_noetic_true <- people_observations$FF$other_noetic > "little"
# 
# people_observations$FJ$other_noetic <- ordered(people_observations$FJ$other_noetic,
#   levels = c("0", "1", "2")
# )
# # FJ: English speaking; 0 = not at all, 1 = somewhat, 2 = very well; 12 August 2025 decide with Matt on 0/1 versus 2
# people_observations$FJ$other_noetic_true <- people_observations$FJ$other_noetic > "1"
# 
# # IH, PC, WH, WL Number of languages spoken, presumably, so 2 == 1 else 0
# people_observations$IH$other_noetic <- people_observations$IH$languages
# people_observations$IH$other_noetic_true <- ifelse(people_observations$IH$languages > 1, TRUE, FALSE)
# people_observations$PC$other_noetic <- people_observations$PC$languages
# people_observations$PC$other_noetic_true <- ifelse(people_observations$PC$languages > 1, TRUE, FALSE)
# people_observations$WH$other_noetic <- people_observations$WH$languages
# people_observations$WH$other_noetic_true <- ifelse(people_observations$WH$languages > 1, TRUE, FALSE)
# people_observations$WL$other_noetic <- people_observations$WL$languages
# people_observations$WL$other_noetic_true <- ifelse(people_observations$WL$languages > 1, TRUE, FALSE)
# 
# people_observations$KA$other_noetic <- ordered(people_observations$KA$bhsind, levels = c(0, 1, 2),
#   labels = c("none", "a little", "fluent")
# ) ## 0 = none, 1 = a little, 2 = fluent; Geoff says to use fluent == 1 for binary
# people_observations$KA$other_noetic_true <- ifelse(people_observations$KA$bhsind == 2, TRUE, FALSE)
# 
# people_observations$KM$other_noetic <- ordered(people_observations$KM$other_noetic,
#   levels = c("inexperienced", "average", "above average", "master")
# ) ## experience in subsistence; Drew says he would go with inexperienced/average == 0 and above average/master == 1.
# people_observations$KM$other_noetic_true <- people_observations$KM$other_noetic > "average"
# 
# people_observations$KO$other_noetic <- ordered(people_observations$KO$other_noetic,
#   levels = c("none", "alittle", "good", "verygood")
# ) ## if dichotomize, presumably good/very good or just very good
# people_observations$KO$other_noetic_true <- people_observations$KO$other_noetic > "alittle"
# people_observations$MA$other_noetic <- ordered(people_observations$MA$other_noetic,
#   levels = c("none", "alittle", "good", "verygood")
# )
# people_observations$MA$other_noetic_true <- people_observations$MA$other_noetic > "alittle"
# # MG has a set of variables: fishnets, diveseafood, growvanilla, cookvanilla, construction; Chris and Bapu suggest summing or using cook vanilla if need one
# people_observations$MG <- people_observations$MG %>%
#   dplyr::mutate(other_noetic_count = rowSums(across(c(fishnets, diveseafood, growvanilla, cookvanilla, construction), ~ . == "yes")))
# ## mostly yes for fishnets, grow vanilla, cook vanilla; fewer for diveseafood and construction (but those two heavily male)
# people_observations$MG$other_noetic <- ifelse(people_observations$MG$cookvanilla == "yes", 1, 0) ## following an email convo with Chris and Bapu ("endow-MG questions")
# # MK has a set of variables: other_noetic, othernoetic2 - othernoetic7; unclear how should combine to binary! The first one selected arbitrarily and shouldn't be given precedence! other_noetic, othernoetic3, othernoetic5 all "easy" while othernoetic2, othernoetic4, othernoetic6 are "hard"; othernoetic7 is divining; 5 is counting money, 6 is read/write; as only one person without any years of edu can read-write, would propose we leave those two and focus on 2 (forage honey) and 4 (grow cotton)
# # suggest 1 if any 2, 4 == 1
# people_observations$MK <- people_observations$MK %>%
#   dplyr::mutate(other_noetic_count = rowSums(across(c(othernoetic1, othernoetic2, othernoetic3, othernoetic4, othernoetic5, othernoetic6, othernoetic7)), na.rm = TRUE))
# people_observations$MK$other_noetic <- ifelse(people_observations$MK$othernoetic2 == 1 | people_observations$MK$othernoetic4 == 1, 1, 0)
# 
# people_observations$MN$other_noetic <- ordered(people_observations$MN$other_noetic,
#   levels = c("0", "0.5", "1")
# )
# people_observations$MN$other_noetic_true <- people_observations$MN$other_noetic > "0.5"
# 
# people_observations$MY$other_noetic <- ordered(people_observations$MY$other_noetic,
#   levels = c("0", "1", "2", "3")
# )
# # Measure of spanish proficiency (Maya is everyone's first language)
# # 0=none,  1=a little (for adults based on interview; assigned to kids in grade 1),  2=basic conversational (for adults based on interview; assigned to kids in grades 2-3, 3=fluent (for adults based on interview; assigned to kids in grades 4+); blank=children 6 and younger who haven't yet gone to school; the older ones may speak some Spanish
# people_observations$MY$other_noetic_true <- people_observations$MY$other_noetic > "2"
# 
# people_observations$NI$other_noetic <- cut(people_observations$NI$other_noetic,
#   breaks = c(0, 1, 33, 66, 100),
#   include.lowest = TRUE,
#   labels = c("none", "alittle", "some", "alot")
# ) # Jeremy says if need to binarize, do 70+, which accords to this top category
# people_observations$NI$other_noetic_true <- people_observations$NI$other_noetic == "alot"
# 
# people_observations$NP$other_noetic <- ordered(people_observations$NP$other_noetic,
#   levels = c("0", "1", "2")
# ) ## 0 = low, 1 = partial, 2 = good; if need to dichotomize, do 0/1 vs 2
# people_observations$NP$other_noetic_true <- people_observations$NP$other_noetic > "1"
# 
# people_observations$PT$other_noetic <- ifelse(people_observations$PT$other_noetic > 1, 1, 0) ## per email of 16 Oct, 1 = fix boats, 2 = handicrafts, 3 = build canoes
# people_observations$TI$other_noetic <- ordered(people_observations$TI$other_noetic,
#   levels = c("0", "1", "2")
# ) ## per email on 19 Sept 2023, if need to dichotomize, do 0 verus 1/2.
# people_observations$TI$other_noetic_true <- people_observations$TI$other_noetic > "0"
# 
# people_observations$TM$other_noetic <- ordered(people_observations$TM$other_noetic,
#   levels = c("none", "moderate", "full")
# ) ## Spanish fluency
# people_observations$TM$other_noetic_true <- people_observations$TM$other_noetic > "moderate"
# 
# people_observations$TP$other_noetic <- ordered(people_observations$TP$other_noetic,
#   levels = c("0", "1", "2", "3")
# )## 0 = speaks neither, 1 = only Dutch, 2 = only Sranan Tango, 3 = both
# people_observations$TP$other_noetic_true <- people_observations$TP$other_noetic == "3"
# 
# people_observations$TS$other_noetic <- ordered(people_observations$TS$other_noetic,
#   levels = c("0", "0.5", "1")
# )
# people_observations$TS$other_noetic_true <- people_observations$TS$other_noetic > "0.5"
# 
# people_observations$TZ$other_noetic <- ordered(people_observations$TZ$other_noetic,
#   levels = c("0", "1", "2")
# )
# people_observations$TZ$other_noetic <- people_observations$TZ$other_noetic > "1"
# 
# people_observations$YI$other_noetic <- people_observations$YI$mandarin_level ## we have literacy_level, mandarin_level, and skills_yn; collection-entry-metadata implies mandarin_level probably best
# people_observations$YI$other_noetic_true <- ifelse(people_observations$YI$mandarin_level == 3, TRUE, FALSE)
# 
# create binary other_noetic_true
for (site in intersect(c("AH", "AR", "BM", "CP", "CR", "EK", "HE", "KS", "KT", "MC", "MG", "MK", "PE", "PS", "PT", "RA", "SH", "SN", "TN", "TZ", "UP", "EG", "EX", "SI", "SM"), names(people_observations))) {
  people_observations[[site]]$other_noetic_true <- as.logical(people_observations[[site]]$other_noetic)
}
# for (site in c("HI", "LB", "VT", "YN")) {
#   people_observations[[site]]$other_noetic_true <- NA ## not recorded in these sites
# }

## WORK ABILITY

## missing: IH, AV, DJ
## BD, PT, RA have 4 level scale
## FF has "child" for one...
## HE -- 1=full and 3=none, confirmed with Gianluca in 18 Sept 2023 email
## HE asked only of household heads, and most recorded as "moderate" -- limit any summary variable to them, to avoid different age interpretations?
## HI: hi1026, hi0837, hi0135, hi0970 recorded in work_ability == "deceased" but listed as is_alive == 1?
## see with: people_observations$HI$is_alive[people_observations$HI$work_ability == "deceased"]
## KA has one person at 0, and then 1, 2, 3... assuming 0 and 1 should both be "none"
## KO and MA: "none" seems to be assigned to some small children, moderate to some mid-age children? [expectation was that this was age-appropriate work ability, so shouldn't be] MA also has two "agriculture" entries? probably bleed over from prior column of "occupation"?
## MN has 2 with 0.5 and 1 with 1
## MY has 1, 2, 2a, 2d, 3, 3b 1=Full; 2=moderate; 3=none  (A=normal age-related disability; D= down's syndrome; B=blind)
## TS has 0, 1, 1.5?
## YI has a coding of dont do, no need, study, and young all to "other"?

# people_observations$AR$work_ability <- dplyr::recode(people_observations$AR$work_ability,
#   "none" = "none",
#   "moderate" = "moderate",
#   "total" = "full"
# )
# people_observations$BD$work_ability <- dplyr::recode(people_observations$BD$work_ability,
#   "1 full" = "full",
#   "2 moderate" = "moderate",
#   "3 little" = "little",
#   "4 none" = "none"
# )
# people_observations$FF$work_ability <- dplyr::recode(people_observations$FF$work_ability,
#   "child" = "full"
# )
# for (site in c("FJ", "LB", "TI", "VT", "YN")) {
# people_observations[[site]]$work_ability <- dplyr::recode(people_observations[[site]]$work_ability,
#   "0" = "none",
#   "1" = "moderate",
#   "2" = "full"
# )
# }
# people_observations$HE$work_ability <- dplyr::recode(people_observations$HE$work_ability,
#   "3" = "none",
#   "2" = "moderate",
#   "1" = "full"
# )
# people_observations$HI$work_ability <- dplyr::recode(people_observations$HI$work_ability,
#   "deceased" = NA_character_
# )
# people_observations$KA$work_ability <- dplyr::recode(people_observations$KA$work_ability,
#   "0" = "none", # Geoff confirms this is a typo and should be 1 == none
#   "1" = "none",
#   "2" = "moderate",
#   "3" = "full"
# )
# people_observations$KM$work_ability <- dplyr::recode(people_observations$KM$work_ability,
#   "not able" = "none",
#   "work half-time" = "moderate",
#   "work full-time" = "full"
# )
# people_observations$MA$work_ability <- dplyr::recode(people_observations$MA$work_ability,
#   "agriculture" = "full"
# ) ## ASSUMPTION!
# people_observations$MC$work_ability <- dplyr::recode(people_observations$MC$work_ability,
#  "0" = "none",
#  "1" = "moderate",
#  "2" = "full",
#  "9999" = NA_character_
# )
# people_observations$MG$work_ability <- dplyr::recode(people_observations$MG$work_ability,
#   "incapable of doing physical labor or play" = "none",
#   "can do some but not strenuous activities" = "moderate",
#   "no limitations on the ability to do the full range of work or play" = "full"
# )
# people_observations$MN$work_ability <- dplyr::recode(people_observations$MN$work_ability,
#   "1" = "full",
#   "0.5" = "moderate"
# )
# people_observations$MY$work_ability <- dplyr::recode(people_observations$MY$work_ability,
#   "1" = "full",
#   "2" = "moderate",
#   "3" = "none",
#   "2a" = "moderate", # a = normal age-related disability
#   "2d" = "moderate", # d = down's syndrome
#   "3b" = "none"
# ) # b = blind
# people_observations$NP$work_ability <- dplyr::recode(people_observations$NP$work_ability,
#   "none" = "none",
#   "partial" = "moderate",
#   "full" = "full"
# )
# people_observations$PE$work_ability <- dplyr::recode(people_observations$PE$work_ability,
#   "weak" = "none", ## note recoding of 'weak' to 'none'! assuming this corresponds to the 'none' category
#   "medium" = "moderate",
#   "strong" = "full",
#   "na" = NA_character_
# )
# people_observations$PS$work_ability <- dplyr::recode(people_observations$PS$work_ability,
#   "1" = "none",
#   "2" = "moderate",
#   "3" = "full"
# )
# people_observations$PT$work_ability <- dplyr::recode(people_observations$PT$work_ability,
#   "low" = "little"
# )
# people_observations$RA$work_ability <- dplyr::recode(people_observations$RA$work_ability,
#   "limited" = "little"
# )
# # unclear what to do with TS!
# people_observations$TS$work_ability <- dplyr::recode(people_observations$TS$work_ability,
#   "0" = "none",
#   "1" = "moderate",
#   "1.5" = "full"
# )
# people_observations$TZ$work_ability <- dplyr::recode(people_observations$TZ$work_ability,
#   "low" = "none",
#   "good" = "full",
# )
# people_observations$YI$work_ability <- dplyr::recode(people_observations$YI$work_ability,
#   "3" = "none",
#   "2" = "moderate",
#   "1" = "full",
#   "other" = NA_character_
# )

for (site in names(people_observations)[!names(people_observations) %in% c("BD", "PT", "RA", "SH", "IH", "AV", "DJ")]) {
  people_observations[[site]]$work_ability <- ordered(people_observations[[site]]$work_ability,
    levels = c("none", "moderate", "full")
  )
}

# for (site in c("BD", "PT", "RA", "SH")) {
#   people_observations[[site]]$work_ability <- ordered(people_observations[[site]]$work_ability,
#     levels = c("none", "little", "moderate", "full")
#   )
# } ## Mary Shenk says to combine little with none, if need to collapse to 3 levels in 17 Sept 2023 email; Katie Starkweather said to combine little with moderate in 16 October email, but fine to go with consensus

## GROUP_ID
# lapply(people_observations, function(x) table(x$group_id))

## missing: BD, KM, LB, TS
## only one category: BM, IH, SN [Ziker says there are no internal divisions. Group is basically just community, so could use location for externals, if absolutely need be]
## mixed memberships: CR, MK, PS, TP
## EK's measure is clan membership, but there are 76 categories, many with only 1 or 2 people.
## MG is whether they participant in "a church group" but not the church groups themselves...
## TZ is 'group bone', and has a number of groups with only 1 or 2 members



## IS RESIDENT

## Can assume that anyone with is_resident == NA is not a resident and can be recoded to 0
for (site in names(people_observations)){
  people_observations[[site]]$is_resident[is.na(people_observations[[site]]$is_resident)] <- 0
}




## SU_OBSERVATIONS

## FOOD SECURITY QUESTIONS

# BY - two!
# missing: IH, PC, WH, WL, AV, DJ
# LB all 0s? VT more levels, YI and YN only 0s and 1s

food_sec_qs <- c("smaller_meals", "fewer_meals","no_food", "sleep_hungry","without_eating")

# ## Recoding BD, which is missing the sleep_hungry variable
# sharing_unit$BD$smaller_meals <- dplyr::recode(sharing_unit$BD$smaller_meals,
#   "1 always have enough" = 0L,
#   "3 don't have enough few times a month" = 1L,
#   "4 don't have enough few times a month" = 2L,
#   "5 never or almost never have enough" = 3L
# )
# 
# sharing_unit$BD[food_sec_qs[c(2, 3, 5)]] <- lapply(
#   sharing_unit$BD[food_sec_qs[c(2, 3, 5)]],
#   function(x) {
#     dplyr::recode(x,
#       "0 never" = 0L,
#       "1 rarely" = 1L,
#       "2 sometimes" = 2L,
#       "3 often" = 3L
#     )
#   }
# )
# 
# ## Recoding MC
# ## I ASKED HOW MANY DAYS EACH WEEK THEY EXPERIENCED THE THING, IF NOT THEN HOW MANY TIMES A MONTH.
# sharing_unit$MC[food_sec_qs] <- lapply(
#   sharing_unit$MC[food_sec_qs],
#   function(x) {
#     dplyr::recode(x,
#       "0" = 0L,
#       "1" = 1L,
#       "2" = 1L,
#       "2.142857143" = 1L, # 30/2.142857143 = 14, so one day every other week
#       "3" = 1L,
#       "4.285714286" = 1L, # 30/4.285714286 = 7, so one day per week per month
#       "8.571428571" = 2L, # 30/8.571428571 = 3.5, so two days per week per month
#       "12.85714286" = 2L, # so three days per week per month
#       "17.14285714" = 3L, # four days per week per month
#       "21.42857143" = 3L, # five days per week per month
#       "30" = 3L # every day
#     )
#   }
# )
# 
# ## Recoding MG
# sharing_unit$MG[food_sec_qs] <- lapply(
#   sharing_unit$MG[food_sec_qs],
#   function(x) {
#     dplyr::recode(x,
#       "never" = 0L,
#       "rarely" = 1L,
#       "sometimes (1-2 times a week)" = 2L,
#       "often" = 3L
#     )
#   }
# )
# 
# ## Recoding KM, NI, PE, NP, UP
# for (site in c("KM", "NI", "PE", "NP", "UP")) {
#   sharing_unit[[site]][food_sec_qs] <- lapply(
#     sharing_unit[[site]][food_sec_qs],
#     function(x) {
#       dplyr::recode(x,
#         "never" = 0L,
#         "rarely" = 1L,
#         "sometimes" = 2L,
#         "often" = 3L
#       )
#     }
#   )
# }
# 
# ## these sites used a 1:4 scale, not a 0:3 scale
# for (site in c("EK", "TS", "MN")) {
#   sharing_unit[[site]][food_sec_qs] <- lapply(
#     sharing_unit[[site]][food_sec_qs],
#     function(x) x - 1
#   )
# }
# 
# 
# ## VT has values that are the # of times in the past month; Siobhan suggested this recoding
# sharing_unit$VT[food_sec_qs] <- lapply(
#   sharing_unit$VT[food_sec_qs],
#   function(x) {
#     dplyr::recode(x,
#       `0` = 0L,
#       `1` = 1L,
#       `2` = 1L,
#       `3` = 2L,
#       `4` = 2L
#     )
#   }
# )


sharing_unit <- lapply(sharing_unit, function(x){
  if ("smaller_meals" %in% names(x)) x$smaller_meals_ord <- factor(x$smaller_meals,
    levels = c(0:3), ordered = TRUE,
    labels = c("never", "rarely", "sometimes", "often")
  )
  if ("fewer_meals" %in% names(x)) x$fewer_meals_ord <- factor(x$fewer_meals,
    levels = c(0:3), ordered = TRUE,
    labels = c("never", "rarely", "sometimes", "often")
  )
  if ("no_food" %in% names(x)) x$no_food_ord <- factor(x$no_food,
    levels = c(0:3), ordered = TRUE,
    labels = c("never", "rarely", "sometimes", "often")
  )
  if ("sleep_hungry" %in% names(x)) x$sleep_hungry_ord <- factor(x$sleep_hungry,
    levels = c(0:3), ordered = TRUE,
    labels = c("never", "rarely", "sometimes", "often")
  )
  if ("without_eating" %in% names(x)) x$without_eating_ord <- factor(x$without_eating,
    levels = c(0:3), ordered = TRUE,
    labels = c("never", "rarely", "sometimes", "often")
  )
  return(x)
})



## MALEHHFATHEREDU / FEMALEHHFATHEREDU / MALEHHMOTHEREDU / FEMALEHHMOTHEREDU

# missing: AV, DJ, HI, IH, KO, LB, MA, MG, PC, PS, WH, WL, YN
# very partial responses: KT, RA (all 0s)

parent_edu_qs <- c("malehhfatheredu", "malehhmotheredu", "femalehhfatheredu", "femalehhmotheredu")

# sharing_unit$BD[parent_edu_qs] <- lapply(
#   sharing_unit$BD[parent_edu_qs],
#   function(x) {
#     x <- dplyr::recode(x, "0 none" = "0")
#     ordered(x, levels = c("0", "1 sign name", "2 literate", "3 primary education", "4 secondary education", "5 higher secondary certificate", "6 degree"))
#   }
# )
# sharing_unit$BY[parent_edu_qs] <- lapply(
#   sharing_unit$BY[parent_edu_qs],
#   function(x) {
#     as.numeric(dplyr::recode(x, `attended` = "1")) ## recoding as in the indiv file, where there 1 is the mean, mode, and median.
#   }
# )
# sharing_unit$KM[parent_edu_qs] <- lapply(
#   sharing_unit$KM[parent_edu_qs],
#   function(x) {
#     ordered(x, levels = c("none", "general", "middle", "higher"))
#   }
# )
# sharing_unit$MC[parent_edu_qs] <- lapply(
#   sharing_unit$MC[parent_edu_qs],
#   function(x) {
#     dplyr::recode(x, `999` = NA_integer_)
#   }
# )
# sharing_unit$MN[parent_edu_qs] <- lapply(
#   sharing_unit$MN[parent_edu_qs],
#   function(x) {
#     as.numeric(x)
#   }
# )
# sharing_unit$TS[parent_edu_qs] <- lapply(
#   sharing_unit$TS[parent_edu_qs],
#   function(x) {
#     as.numeric(x)
#   }
# )


## Parental other noetic
## presumably following the main other noetic variable, very many of these are not binary; if need to dichotomize, will need to do a bit more
## numeric: FF (0,1,2), FJ (0,1,2), KA (0,1,2), MK (2,3,4,5,6), MN (0,0.5,1), MY (0,1,2,3), NI (0,30,40,50,60,70,75,80,85,100), PT (0,1,2,3), TI (1,2), TP (0,2,3), TM (0, 0.5, 1), VT (0,1,2), YI (1,2,3) [these based on malehhfathernoeticother, presume hold for others]
## missing: HI, IH, PC, PE, WH, WL, NP, UP, AV, DJ
## BY has two: _lingala and _dance
## EK has two: malehhfathernoeticlanguage and malehhfathernoeticecolo
## HE has femalehhfatherenglish
## KO has "hhnoetic2dad_amharic"           "hhnoetic3mom_education"         "hhnoetic2mom_amharic"           "wife_hhnoetic_father_education" "wife_hhnoetic_dad_amharic"      "wife_hhnoetic_mom_education" "wife_hhnoetic_mom_amharic"
# LB has "femalehhmothernoeticlocal"    "femalehhmothernoeticmandarin"
# MA "hhnoetic_father_education"      "hhnoetic_dad_traditional"       "hhnoetic_mom_education"         "hhnoetic_mom_traditional"       "wife_hhnoetic_father_education" "wife_hhnoetic_dad_traditional"  "wife_hhnoetic_mom_education"   "wife_hhnoetic_mom_traditional"
# MC has "secondHH" for both noetic and othernoetic; ignoring for now
# MG has "femalehhmothermedplants"  "femalehhfatheredu"        "femalehhfatherdive"       "femalehhfatherconstruct" THAT'S IT
# PS has "malehfathernoetic"  "malehmothernoetic"   "femalehfatheredu"    "femalehmotheredu"    "femalehfathernoetic" "femalehmothernoetic"
# TS has two ...noeticother1 and ...noeticother2
# TZ has two ..noeticother_literacy and ..noeticother_languages
# YN has "femalehhmothernoeticlocal"    "femalehhmothernoeticmandarin" "femalehhfathernoeticlocal"    "femalehhfathernoeticmandarin" "malehhmothernoeticlocal"      "malehhmothernoeticmandarin"   "malehhfathernoeticlocal"      "malehhfathernoeticmandarin"


parent_other_noetic_qs <- c("malehhfathernoeticother", "malehhmothernoeticother","femalehhfathernoeticother", "femalehhmothernoeticother") # AH, BM, CP, CR, FF (yes, there in both; use the other), FJ, KA, KM, MK, MN, NI, PT, RA, SH, SN, TN, TP, VT
# parent_other_noetic_qs_alt <- c("malehhfatherothernoetic", "malehhmotherothernoetic", "femalehhfatherothernoetic", "femalehhmotherothernoetic") # BD, FF, TI, YI

# for(site in c("BD","FF","TI","YI")){
#  sharing_unit[[site]]$malehhfathernoeticother <- sharing_unit[[site]]$malehhfatherothernoetic
#  sharing_unit[[site]]$femalehhfathernoeticother <- sharing_unit[[site]]$femalehhfatherothernoetic
#  sharing_unit[[site]]$malehhmothernoeticother <- sharing_unit[[site]]$malehhmotherothernoetic
#  sharing_unit[[site]]$femalehhmothernoeticother <- sharing_unit[[site]]$femalehhmotherothernoetic
#  sharing_unit[[site]]$malehhfatherothernoetic <- NA
#  sharing_unit[[site]]$femalehhfatherothernoetic <- NA
#  sharing_unit[[site]]$malehhmotherothernoetic <- NA
#  sharing_unit[[site]]$femalehhmotherothernoetic <- NA
# }
# 
# sharing_unit$BD[parent_other_noetic_qs] <- lapply(
#   sharing_unit$BD[parent_other_noetic_qs],
#   function(x) {
#     ordered(x, levels = c("4 none", "3 basic", "2 good", "1 very good"))
#   }
# )
# sharing_unit$KM[parent_other_noetic_qs] <- lapply(
#   sharing_unit$KM[parent_other_noetic_qs],
#   function(x) {
#     ordered(x, levels = c("inexperienced", "average", "above average", "master"))
#   }
# )
# sharing_unit$NI[parent_other_noetic_qs] <- lapply(
#   sharing_unit$NI[parent_other_noetic_qs],
#   function(x) {
#     cut(x,
#   breaks = c(0, 1, 33, 66, 100),
#   include.lowest = TRUE,
#   labels = c("none", "alittle", "some", "alot")
#     )
#   }
# ) # Jeremy says if need to binarize, do 70+, which accords to this top category


# assigning the su_wealth variable into the sharing_unit data frame
for (site in names(sharing_unit)) {
  sharing_unit[[site]]$su_wealth <- su_wealth[[site]]$cash_value[match(sharing_unit[[site]]$su_id, su_wealth[[site]]$su_id)]
}

rm(site, food_sec_qs, parent_edu_qs, parent_other_noetic_qs, recode_NAs, values_to_NA)
