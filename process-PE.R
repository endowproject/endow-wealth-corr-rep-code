########################################.
#
#   Prep PE Data
#   Daniel Redhead & Elly Power
#   NOTE: Should be sourced by "process_data.R".
#

# TO DO:
#   - Add network stuff to su_metadata?
#
########################################.
# Load libraries

# A lot of problems caused by reloading plyr.
# Since this script is sourced by another script (process_data.R) that
# loads plyr and dplyr, let's not reload.

## Loading plyr when dplyr is already loaded can cause problems:
## https://stackoverflow.com/questions/31644739/loading-dplyr-after-plyr-is-causing-issues
## which necessitates the below block.

# if("dplyr" %in% (.packages())){
#   detach("package:dplyr", unload=TRUE)
#   detach("package:plyr", unload=TRUE)
# }
# library(plyr)

#########################################

site <- "PE"

# Load in functions
read_csv <- function(file) {
  tryCatch({
    read.csv(file)
  }, error = function(e) {
    message(sprintf("Error reading %s: %s", file, e$message))
    return(NULL)
  })
}

# Extract names from filenames
extract_name <- function(file) {
  # Extract the basename (filename) from the full path
  basename <- basename(file)
  # Use regex to extract the part after "PE" and before ".csv"
  name <- str_match(basename, "PE_(.*)\\.csv")[2]
  return(name)
}

# Process edgelists and create matrices
process_ties <- function(net, tie_value, att2) {
  data <- net %>% filter(tie == tie_value) %>% select(ego = sui, alter = suj) #%>% distinct()
  matrix_data <- matrix(NA, nrow = length(unique(att2$su_id_1)), ncol = length(unique(att2$su_id_1)), dimnames = list(unique(att2$su_id_1), unique(att2$su_id_1)))

  for (i in 1:nrow(matrix_data)) {
    for (j in 1:ncol(matrix_data)) {
      if (nrow(data[data$ego == rownames(matrix_data)[i] & data$alter == colnames(matrix_data)[j], ]) > 0) {
        matrix_data[i, j] <- nrow(data[data$ego == rownames(matrix_data)[i] & data$alter == colnames(matrix_data)[j], ])
      } else {
        matrix_data[i, j] <- 0
      }
    }
  }

  return(matrix_data)
}

process_ties_collapse <- function(net, tie_value, att2) {
  data <-
    net %>%
    filter(tie == tie_value) %>%
    select(ego = sui, alter = suj, person_ego = personid, person_alter = alterid)

  matrix_data <- matrix(NA, nrow = length(unique(att2$su_id_1)), ncol = length(unique(att2$su_id_1)), dimnames = list(unique(att2$su_id_1), unique(att2$su_id_1)))

  for (i in 1:nrow(matrix_data)) {
    for (j in 1:ncol(matrix_data)) {
      if (nrow(data[data$ego == rownames(matrix_data)[i] & data$alter == colnames(matrix_data)[j], ]) > 0) {
        list_is_nominate_js <-
          unique(data$person_ego[data$ego == rownames(matrix_data)[i] & data$alter == colnames(matrix_data)[j]])
        matrix_data[i, j] <- length(list_is_nominate_js)
      } else {
        matrix_data[i, j] <- 0
      }
    }
  }

  return(matrix_data)
}


# Load in PE data
# Get the list of files that include "PE" in their names
pe_files <- list.files(EndowDatabase, pattern = "PE.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Read all the matched files into a list
pe_database <- map(pe_files, read_csv)

# Set names for the data_list objects
names(pe_database) <- map_chr(pe_files, extract_name)

people$PE <- pe_database[["people"]]
people_observations$PE <- pe_people_obs ## saved from process_data.R
networks$PE <- pe_database[["networks"]]
residents$PE <- pe_database[["residents"]]
sharing_unit$PE <- pe_sharing_unit ## saved from process_data.R
su_distances$PE <- pe_database[["su_distances"]]
partnerships$PE <- pe_database[["partnerships"]]
poss_cost$PE <- pe_database[["poss_cost"]]

######################################################################################################
#
#   Prepare data & specify attributes
#
######################################################################################################

#add polygenous males su IDs
people_observations$PE$su_id_1[people_observations$PE$personid == "pe0101"] <- "pesu101" # Also in pesu108
people_observations$PE$su_id_1[people_observations$PE$personid == "pe2401"] <- "pesu123" # Also in pesu124
people_observations$PE$su_id_1[people_observations$PE$personid == "pe3101"] <- "pesu131" # Also in pesu132
people_observations$PE$su_id_1[people_observations$PE$personid == "pe4501"] <- "pesu145" # Also in pesu157
people_observations$PE$su_id_1[people_observations$PE$personid == "pe5201"] <- "pesu152" # Also in pesu154
people_observations$PE$su_id_1[people_observations$PE$personid == "pe6101"] <- "pesu104" # Also in pesu161

att <- dplyr::left_join(people_observations$PE, dplyr::select(people$PE, -site_code, -dob, -dod), by = "personid")
#att <- att[!att$su_id_1 == "", ]
att$su_id_1[att$su_id_1 == ""] <- NA


######################################################################################################
#
#   Specify kinship & proximity
#
######################################################################################################

# Prepare kinship/relatedness
att$gender[att$gender == "male"] <- 1
att$gender[att$gender == "female"] <- 2
att$gender[is.na(att$gender) | att$gender == "" |
             att$gender == " " | att$gender == "NA"] <- 3
att$father[att$father == 999 |att$father == 0|
             att$father == "empty" | att$father == "missing"] <- NA
att$mother[att$mother == 999 |att$mother == 0|
             att$mother == "empty" |att$mother == "missing"] <- NA


att2  <- att[!is.na(att$su_id_1),]

kin_dat <- dplyr::select(att, personid, father, mother, gender)

# Create pedigree data
# Add missing father ids
kin_dat$father <- ifelse(is.na(kin_dat$father) & !is.na(kin_dat$mother),
                         paste0(kin_dat$personid, "_dad"),
                         kin_dat$father)

kin_dat$mother <- ifelse(is.na(kin_dat$mother) & !is.na(kin_dat$father),
                         paste0(kin_dat$personid, "_mum"),
                         kin_dat$mother)
dads <- unique(kin_dat$father[!kin_dat$father %in% kin_dat$personid & !is.na(kin_dat$father) & !kin_dat$father == 0])
mums <- unique(kin_dat$mother[!kin_dat$mother %in% kin_dat$personid & !is.na(kin_dat$mother) & !kin_dat$mother == 0])
if(length(dads) > 0) new_dads <- data.frame(personid = dads,
                                            father = NA,
                                            mother = NA,
                                            gender = rep(1))
if(length(dads) > 0) kin_dat <- rbind(kin_dat, new_dads)
if(length(mums) > 0) new_mums <- data.frame(personid = mums,
                                            father = NA,
                                            mother = NA,
                                            gender = rep(2))
if(length(mums) > 0) kin_dat <- rbind(kin_dat, new_mums)

k <- kinship(pedigree(id = kin_dat$personid,
                      dadid = kin_dat$father, momid = kin_dat$mother,
                      sex = as.numeric(kin_dat$gender)))

fad <- FamAgg::FAData(
    pedigree =
    data.frame(
        id = kin_dat$personid,
        father = kin_dat$father,
        mother = kin_dat$mother,
        sex = as.numeric(kin_dat$gender),
        family = 1
        )
    )


# Make sure ordering of rows/columns matches other data
k2 <- k[row.names(k) %in% att$personid, ]
k2 <- k2[, colnames(k) %in% att$personid ]
k2 <- k2[match(att$personid, rownames(k2)), match(att$personid, colnames(k2))]
kin_mat$PE <- 2 * k2

# Add individual-level kinship attributes
av_rel$PE <- data.frame(personid = rownames(kin_mat$PE), av_rel = rowMeans(kin_mat$PE))
people_observations$PE$av_rel <- av_rel$PE$av_rel[match(people_observations$PE$personid, av_rel$PE$personid)]
people_observations$PE$z_av_rel <- normalize(people_observations$PE$av_rel)
residents$PE$av_rel <- av_rel$PE$av_rel[match(residents$PE$personid, av_rel$PE$personid)]
residents$PE$z_av_rel <- normalize(residents$PE$av_rel)



# Create an edgelist from the kinship2 output
kedge <- data.frame(su_i = rownames(k2)[col(k2)],
                    su_j = colnames(k2)[row(k2)],
                    val = c(t(k2)),
                    stringsAsFactors = FALSE)

# Add polygynous men to all of their associated SU's
poly_men <- c("pe0101",
              "pe2401",
              "pe3101",
              "pe4501",
              "pe5201",
              "pe6101")

add_kin <- kedge[kedge$su_i %in% poly_men | kedge$su_j %in% poly_men ,]

#add polygenous males second su IDs
add_kin$su_i[add_kin$su_i == "pe0101"] <- "pesu108"
add_kin$su_i[add_kin$su_i == "pe2401"] <- "pesu124"
add_kin$su_i[add_kin$su_i == "pe3101"] <- "pesu132"
add_kin$su_i[add_kin$su_i == "pe4501"] <- "pesu157"
add_kin$su_i[add_kin$su_i == "pe5201"] <- "pesu154"
add_kin$su_i[add_kin$su_i == "pe6101"] <- "pesu161"
add_kin$su_j[add_kin$su_j == "pe0101"] <- "pesu108"
add_kin$su_j[add_kin$su_j == "pe2401"] <- "pesu124"
add_kin$su_j[add_kin$su_j == "pe3101"] <- "pesu132"
add_kin$su_j[add_kin$su_j == "pe4501"] <- "pesu157"
add_kin$su_j[add_kin$su_j == "pe5201"] <- "pesu154"
add_kin$su_j[add_kin$su_j == "pe6101"] <- "pesu161"

# Remove cases where the person id for i and j are the same
kedge <- kedge[!kedge$su_i == kedge$su_j, ]
kedge <- kedge[!kedge$su_i == "", ]
kedge <- kedge[!kedge$su_j == "", ]
kedge <- kedge[!is.na(kedge$su_i), ]
kedge <- kedge[!is.na(kedge$su_j), ]

# Limit to only residents
kedge <- kedge[kedge$su_i %in% att$personid[att$is_resident == 1],]
kedge <- kedge[kedge$su_j %in% att$personid[att$is_resident == 1],]

# Append to full networks table
kedge <- add_row(kedge, add_kin)

# Bring in the su id for both i and j
kedge$su_i <- att$su_id_1[match(kedge$su_i, att$personid)]
kedge$su_j <- att$su_id_1[match(kedge$su_j, att$personid)]

kedge <- kedge[!is.na(kedge$su_i), ]
kedge <- kedge[!is.na(kedge$su_j), ]

# create a dyad id
kedge$dyad_id <- paste(kedge$su_i, "_", kedge$su_j, sep = "")

# Multiply by 2 because kinship2 is allelic
kedge$val <- kedge$val*2

kedge$primary_kin <- ifelse(kedge$val >= 0.5, 1, 0)
kedge$secondary_kin <- ifelse(kedge$val >= 0.25, 1, 0)

# Get average relatedness between households
su_dyads$PE <- ddply(kedge, .(dyad_id), dplyr::summarize, avg_r = mean(val), max_r = max(val))
# Separate out dyad ID again
su_dyads$PE <- separate(data = su_dyads$PE, col = dyad_id, into = c("sui", "suj"), sep = "_")

su_dyads$PE$primary_kin_tie <- ifelse (su_dyads$PE$max_r >= 0.5, 1, 0)
su_dyads$PE$secondary_kin_tie <- ifelse (su_dyads$PE$max_r >= 0.25, 1, 0)

# Add su physical distance as a column in the dyads table
su_distances$PE$sui <- att$su_id_1[match(su_distances$PE$sui, att$su_id_1)]
su_distances$PE$suj <- att$su_id_1[match(su_distances$PE$suj, att$su_id_1)]

su_dyads$PE <- dplyr::left_join(su_dyads$PE, dplyr::select(su_distances$PE[su_distances$PE$sui %in% att$su_id_1 & su_distances$PE$suj %in% att$su_id_1,], sui, suj, distance), by = c('sui', 'suj'))
su_dyads$PE$distance[su_dyads$PE$sui == su_dyads$PE$suj] <- 0

primary_kin$PE <- subset(su_dyads$PE, primary_kin_tie == 1 & sui != suj, select = c(sui, suj))
primary_kin$PE <- as.data.frame(table(primary_kin$PE$sui))

secondary_kin$PE <- subset(su_dyads$PE, secondary_kin_tie == 1 & sui != suj, select = c(sui, suj))
secondary_kin$PE <- as.data.frame(table(secondary_kin$PE$sui))

sharing_unit$PE$primary_kin <- primary_kin$PE$Freq[match(sharing_unit$PE$su_id, primary_kin$PE$Var1)]
sharing_unit$PE$primary_kin[is.na(sharing_unit$PE$primary_kin)] <- 0

sharing_unit$PE$secondary_kin <- secondary_kin$PE$Freq[match(sharing_unit$PE$su_id, secondary_kin$PE$Var1)]
sharing_unit$PE$secondary_kin[is.na(sharing_unit$PE$secondary_kin)] <- 0

res_su <- setdiff(unique(residents$PE$su_id_1), c(NA, ""))

# create SU x SU matrices
su_m_avr$PE <- su_m_maxr$PE <- su_m_dist$PE <- su_m_par$PE <- su_m_chi$PE <- su_m_sib$PE <- matrix(0,
  nrow = length(res_su),
  ncol = length(res_su),
  dimnames = list(res_su, res_su))

for (i in 1:nrow(su_dyads$PE)){
  su_m_avr$PE[rownames(su_m_avr$PE)==su_dyads$PE$sui[i],colnames(su_m_avr$PE)==su_dyads$PE$suj[i]] <- su_dyads$PE$avg_r[i]
}

su_m_avr$PE <- su_m_avr$PE[match(unique(att2$su_id_1), rownames(su_m_avr$PE)), match(unique(att2$su_id_1), colnames(su_m_avr$PE))]

# create SU x SU max relatedness matrix

for (i in 1:nrow(su_dyads$PE)){
  su_m_maxr$PE[rownames(su_m_maxr$PE)==su_dyads$PE$sui[i],colnames(su_m_maxr$PE)==su_dyads$PE$suj[i]] <- su_dyads$PE$max_r[i]
}
# Note here that the max relatedness DOESN'T include relatedness of polygynous men to themselves! Not sure whether we want that? Will need to assign 1 to those cases.
su_m_maxr$PE <- su_m_maxr$PE[match(unique(att2$su_id_1), rownames(su_m_maxr$PE)), match(unique(att2$su_id_1), colnames(su_m_maxr$PE))]


for (i in 1:nrow(su_dyads$PE)) {
  su_m_dist$PE[rownames(su_m_dist$PE) == su_dyads$PE$sui[i], colnames(su_m_dist$PE) == su_dyads$PE$suj[i]] <- su_dyads$PE$distance[i]
}

su_m_dist$PE <- su_m_dist$PE[match(unique(att2$su_id_1), rownames(su_m_dist$PE)), match(unique(att2$su_id_1), colnames(su_m_dist$PE))]

## making sure poly_men tied to both of their sus
to_add <- data.frame(id = c(poly_men, poly_men), su_id_1 = c("pesu108", "pesu124", "pesu132", "pesu157", "pesu154", "pesu161", "pesu101", "pesu123", "pesu131", "pesu145", "pesu152", "pesu104"))


for (sui in res_su) {
    hhmembs <- setdiff(residents$PE$personid[residents$PE$su_id_1 == sui], NA)

    # adding polygynous men
    if (sui %in% c("pesu108", "pesu101")) hhmembs <- c(hhmembs, "pe0101")
    if (sui %in% c("pesu124", "pesu123")) hhmembs <- c(hhmembs, "pe2401")
    if (sui %in% c("pesu132", "pesu131")) hhmembs <- c(hhmembs, "pe3101")
    if (sui %in% c("pesu157", "pesu145")) hhmembs <- c(hhmembs, "pe4501")
    if (sui %in% c("pesu154", "pesu152")) hhmembs <- c(hhmembs, "pe5201")
    if (sui %in% c("pesu161", "pesu104")) hhmembs <- c(hhmembs, "pe6101")

    children <- FamAgg::getChildren(fad, id = hhmembs, max.generations = 1)
    parents <- FamAgg::getAncestors(fad, id = hhmembs, max.generations = 1)
    siblings <- FamAgg::getSiblings(fad, id = hhmembs)

    children_su <- setdiff(residents$PE$su_id_1[match(children, residents$PE$personid)], c(NA, ""))
    parents_su <- setdiff(residents$PE$su_id_1[match(parents, residents$PE$personid)], c(NA, ""))
    siblings_su <- setdiff(residents$PE$su_id_1[match(siblings, residents$PE$personid)], c(NA, ""))

   if(any(children %in% poly_men)) {
    children_su <- unique(c(children_su, to_add$su_id_1[to_add$id %in% children]))
   }
   if(any(parents %in% poly_men)) {
    parents_su <- unique(c(parents_su, to_add$su_id_1[to_add$id %in% parents]))
   }
   if(any(siblings %in% poly_men)) {
    siblings_su <- unique(c(siblings_su, to_add$su_id_1[to_add$id %in% siblings]))
   }

  for (suj in children_su) {
      su_m_chi$PE[rownames(su_m_chi$PE) == sui, colnames(su_m_chi$PE) == suj] <- 1
  }

  for (suj in parents_su) {
      su_m_par$PE[rownames(su_m_par$PE) == sui, colnames(su_m_par$PE) == suj] <- 1
  }

  for (suj in siblings_su) {
      su_m_sib$PE[rownames(su_m_sib$PE) == sui, colnames(su_m_sib$PE) == suj] <- 1
  }

}



# housekeeping
rm(add_kin, kedge, i, k, k2, mums, dads, new_mums, new_dads)


######################################################################################################
#
#   Add polygynous men
#
######################################################################################################
# Add polygynous men to all of their associated SU's

## add to networks
add_people <- networks$PE[networks$PE$personid %in% poly_men | networks$PE$alterid %in% poly_men, ]
add_people$personid[add_people$personid %in% poly_men] <- paste(add_people$personid[add_people$personid %in% poly_men], "_2", sep = "")
add_people$alterid[add_people$alterid %in% poly_men] <- paste(add_people$alterid[add_people$alterid %in% poly_men], "_2", sep = "")
networks$PE <- rbind(networks$PE, add_people)

## add to people_observations
add_people <- people_observations$PE[people_observations$PE$personid %in% poly_men, ]
add_people$personid[add_people$personid %in% poly_men] <- paste(add_people$personid[add_people$personid %in% poly_men], "_2", sep = "")
add_people$su_id_1[add_people$personid == "pe0101_2"] <- "pesu108"
add_people$su_id_1[add_people$personid == "pe2401_2"] <- "pesu124"
add_people$su_id_1[add_people$personid == "pe3101_2"] <- "pesu132"
add_people$su_id_1[add_people$personid == "pe4501_2"] <- "pesu157"
add_people$su_id_1[add_people$personid == "pe5201_2"] <- "pesu154"
add_people$su_id_1[add_people$personid == "pe6101_2"] <- "pesu161"
people_observations$PE <- rbind(people_observations$PE, add_people)

## can now add sui and suj to networks
networks$PE$sui <- people_observations$PE$su_id_1[match(networks$PE$personid, people_observations$PE$personid)]
networks$PE$suj <- people_observations$PE$su_id_1[match(networks$PE$alterid, people_observations$PE$personid)]


## add to att2 (i.e., residents?)
add_people <- att2[att2$personid %in% poly_men, ]
add_people$personid[add_people$personid %in% poly_men] <- paste(add_people$personid[add_people$personid %in% poly_men], "_2", sep = "")
add_people$su_id_1[add_people$personid == "pe0101_2"] <- "pesu108"
add_people$su_id_1[add_people$personid == "pe2401_2"] <- "pesu124"
add_people$su_id_1[add_people$personid == "pe3101_2"] <- "pesu132"
add_people$su_id_1[add_people$personid == "pe4501_2"] <- "pesu157"
add_people$su_id_1[add_people$personid == "pe5201_2"] <- "pesu154"
add_people$su_id_1[add_people$personid == "pe6101_2"] <- "pesu161"
att2 <- rbind(att2, add_people)


## with polygynous men added, can derive other variables/objects

residents$PE <- people_observations$PE[people_observations$PE$is_resident == 1, ]
residents$PE <- residents$PE[!is.na(residents$PE$is_resident), ] # for some reason not removing NAs

egos <- unique(networks$PE$personid)
sampled_su <- unique(people_observations$PE$su_id_1[people_observations$PE$personid %in% unlist(egos)])
sampled_su_2 <- unique(people_observations$PE$su_id_2[people_observations$PE$personid %in% unlist(egos)])

sharing_unit$PE$su_sampled <- sharing_unit$PE$su_id %in% unlist(sampled_su)
residents$PE$su_sampled <- residents$PE$su_id_1 %in% unlist(sampled_su)

people_observations$PE$surveyed <- people_observations$PE$personid %in% egos
residents$PE$surveyed <- residents$PE$personid %in% egos
num_surv <- data.frame(table(people_observations$PE$su_id_1[people_observations$PE$surveyed == TRUE]))
num_surv_female_q <- data.frame(table(people_observations$PE$su_id_1[people_observations$PE$surveyed == TRUE & people_observations$PE$gender == "female"]))
num_surv_male_q <- data.frame(table(people_observations$PE$su_id_1[people_observations$PE$surveyed == TRUE & people_observations$PE$gender == "male"]))
sharing_unit$PE$num_surv <- num_surv$Freq[match(sharing_unit$PE$su_id, num_surv$Var1)]
sharing_unit$PE$num_surv_female_q <- num_surv_female_q$Freq[match(sharing_unit$PE$su_id, num_surv_female_q$Var1)]
sharing_unit$PE$num_surv_male_q <- num_surv_male_q$Freq[match(sharing_unit$PE$su_id, num_surv_male_q$Var1)]

residents$PE$is_adult <- ifelse(residents$PE$age >= 18, 1, 0)

su_sum$PE <- residents$PE %>%
    dplyr::group_by(su_id_1) %>%
    dplyr::summarize(su_size = n(),
                     age_av = mean(age, na.rm = TRUE),
                     age_max = max(age, na.rm = TRUE),
                     adult_count = sum(is_adult == 1, na.rm = TRUE),
                     able_count = sum(work_ability %in% c("full" , "moderate"), na.rm = TRUE),
                     status_any = as.numeric(any(status == 1, na.rm = TRUE)),
                     status_count = sum(status == 1, na.rm = TRUE),
                     edu_max = max(years_education, na.rm = TRUE),
                     other_noetic_any = as.numeric(any(other_noetic_true == TRUE, na.rm = TRUE)),
                     other_noetic_count = sum(other_noetic_true == TRUE, na.rm = TRUE)
    )

su_sum$PE <- su_sum$PE[!is.na(su_sum$PE$su_id_1),]

## NOTE: PE now only has one head per SU
su_heads_sum$PE <-
  people_observations$PE %>%
    dplyr::group_by(su_id_1) %>%
    filter(head == 1) %>%
    dplyr::summarize(age_av_hh = mean(age, na.rm = TRUE),
                     able_count_hh = sum(work_ability %in% c("strong" , "medium"), na.rm = TRUE),
                     status_any_hh = as.numeric(any(status == 1, na.rm = TRUE)),
                     status_count_hh = sum(status == 1, na.rm = TRUE),
                     edu_max_hh = max(years_education, na.rm = TRUE),
                     other_noetic_any_hh = as.numeric(any(other_noetic_true == TRUE, na.rm = TRUE)),
                     other_noetic_count_hh = sum(other_noetic_true == TRUE, na.rm = TRUE)
    )


sharing_unit$PE$hofh <- ifelse(!is.na(sharing_unit$PE$femalehead) & is.na(sharing_unit$PE$malehead), "female",
                                    ifelse(!is.na(sharing_unit$PE$femalehead) & !is.na(sharing_unit$PE$malehead), "both",
                                           ifelse(is.na(sharing_unit$PE$femalehead) & !is.na(sharing_unit$PE$malehead), "male", NA)))

sharing_unit$PE$su_size <- su_sum$PE$su_size[match(sharing_unit$PE$su_id, su_sum$PE$su_id_1)]
sharing_unit$PE$age_av <- su_sum$PE$age_av[match(sharing_unit$PE$su_id, su_sum$PE$su_id_1)]
sharing_unit$PE$age_av_hh <- su_heads_sum$PE$age_av_hh[match(sharing_unit$PE$su_id, su_heads_sum$PE$su_id_1)]
sharing_unit$PE$age_max <- su_sum$PE$age_max[match(sharing_unit$PE$su_id, su_sum$PE$su_id_1)]
sharing_unit$PE$adult_count <- su_sum$PE$adult_count[match(sharing_unit$PE$su_id, su_sum$PE$su_id_1)]
sharing_unit$PE$able_count <- su_sum$PE$able_count[match(sharing_unit$PE$su_id, su_sum$PE$su_id_1)]
sharing_unit$PE$able_count_hh <- su_heads_sum$PE$able_count_hh[match(sharing_unit$PE$su_id, su_heads_sum$PE$su_id_1)]
sharing_unit$PE$status_any <- su_sum$PE$status_any[match(sharing_unit$PE$su_id, su_sum$PE$su_id_1)]
sharing_unit$PE$status_any_hh <- su_heads_sum$PE$status_any_hh[match(sharing_unit$PE$su_id, su_heads_sum$PE$su_id_1)]
sharing_unit$PE$status_count <- su_sum$PE$status_count[match(sharing_unit$PE$su_id, su_sum$PE$su_id_1)]
sharing_unit$PE$status_count_hh <- su_heads_sum$PE$status_count_hh[match(sharing_unit$PE$su_id, su_heads_sum$PE$su_id_1)]
sharing_unit$PE$edu_max <- su_sum$PE$edu_max[match(sharing_unit$PE$su_id, su_sum$PE$su_id_1)]
sharing_unit$PE$edu_max_hh <- su_heads_sum$PE$edu_max_hh[match(sharing_unit$PE$su_id, su_heads_sum$PE$su_id_1)]
sharing_unit$PE$other_noetic_any <- su_sum$PE$other_noetic_any[match(sharing_unit$PE$su_id, su_sum$PE$su_id_1)]
sharing_unit$PE$other_noetic_any_hh <- su_heads_sum$PE$other_noetic_any_hh[match(sharing_unit$PE$su_id, su_heads_sum$PE$su_id_1)]
sharing_unit$PE$other_noetic_count <- su_sum$PE$other_noetic_count[match(sharing_unit$PE$su_id, su_sum$PE$su_id_1)]
sharing_unit$PE$other_noetic_count_hh <- su_heads_sum$PE$other_noetic_count_hh[match(sharing_unit$PE$su_id, su_heads_sum$PE$su_id_1)]



# For all nominations across ALL questions, including the double-sampled ones and any extra prompts
su_alters_all$PE <- aggregate(alterid ~ sui, data = networks$PE, FUN = function(x) list(unique(x)))
su_alters_all$PE$tie <- "all"
colnames(su_alters_all$PE)[2] <- "alterids"

su_alters_req$PE <- aggregate(alterid ~ sui, data = networks$PE[networks$PE$tie %in% c(1, 3, 5, 6, 7, 8, 9, 10, "5/6a","7/8"),], FUN = function(x) list(unique(x)))
su_alters_req$PE$tie <- "req"
colnames(su_alters_req$PE)[2] <- "alterids"

# For each unique combination of su_id_1 and tie
su_alters_each$PE <- aggregate(alterid ~ sui + tie, data = networks$PE, FUN = function(x) list(unique(x)))
su_alters_each$PE <- su_alters_each$PE[,c(1,3,2)] ## getting in same order to align with above
colnames(su_alters_each$PE)[2] <- "alterids"

su_ex_alters_all$PE <- aggregate(alterid ~ sui, data = networks$PE[!networks$PE$alterid %in% att2$personid, ], FUN = function(x) list(unique(x)))
su_ex_alters_all$PE$tie <- "all"
colnames(su_ex_alters_all$PE)[2] <- "externalids"

su_ex_alters_req$PE <- aggregate(alterid ~ sui, data = networks$PE[!networks$PE$alterid %in% att2$personid & networks$PE$tie %in% c(1, 3, 5, 6, 7, 8, 9, 10, "5/6a","7/8"), ], FUN = function(x) list(unique(x)))
su_ex_alters_req$PE$tie <- "req"
colnames(su_ex_alters_req$PE)[2] <- "externalids"

su_ex_alters_main$PE <- aggregate(alterid ~ sui, data = networks$PE[!networks$PE$alterid %in% att2$personid & networks$PE$tie %in% c(9, 10), ], FUN = function(x) list(unique(x)))
su_ex_alters_main$PE$tie <- "external_main"
colnames(su_ex_alters_main$PE)[2] <- "externalids"

su_ex_alters_each$PE <- aggregate(alterid ~ sui + tie, data = networks$PE[!networks$PE$alterid %in% att2$personid, ], FUN = function(x) list(unique(x)))
su_ex_alters_each$PE <- su_ex_alters_each$PE[,c(1,3,2)] ## getting in same order to align with above
colnames(su_ex_alters_each$PE)[2] <- "externalids"

su_alters_each_indiv$PE <- networks$PE %>%
  dplyr::group_by(personid, tie) %>%
  dplyr::summarise(
    alterids = list(unique(alterid)),
    sualterids = list(setdiff(unique(suj), NA)), .groups = "drop") %>%
dplyr::mutate(
  alter_count = map_int(alterids, length),
  su_alter_count = map_int(sualterids, length)
  )
su_alters_each_indiv$PE$sui <- networks$PE$sui[match(su_alters_each_indiv$PE$personid, networks$PE$personid)]

su_alters_req_indiv$PE <- networks$PE[networks$PE$tie %in% c(1, 3, 5, 6, 7, 8, 9, 10, "5/6a","7/8"),] %>% ## for main prompts of *requested* support, not *given* support (i.e., no double-sampled)
  dplyr::group_by(personid) %>%
  dplyr::summarise(
    alterids = list(unique(alterid)),
    sualterids = list(setdiff(unique(suj), NA)), .groups = "drop") %>%
  dplyr::mutate(
    alter_count = map_int(alterids, length),
    su_alter_count = map_int(sualterids, length)
  )

su_alters_req_indiv$PE$sui <- networks$PE$sui[match(su_alters_req_indiv$PE$personid, networks$PE$personid)]

su_alters_each_pair$PE <- su_alters_each_indiv$PE %>%
  dplyr::inner_join(su_alters_each_indiv$PE, by = c("sui", "tie"), relationship = "many-to-many") %>%
  dplyr::filter(personid.x < personid.y) %>%  # Avoid duplicate and self-pairs
  dplyr::mutate(
    intersection_indiv = map2(alterids.x, alterids.y, ~ intersect(.x, .y)),
    union_indiv = map2(alterids.x, alterids.y, ~ union(.x, .y)),
    prop_agree_indiv = map2_dbl(alterids.x, alterids.y, ~ length(intersect(.x, .y)) / length(union(.x, .y))),
    intersection_su = map2(sualterids.x, sualterids.y, ~ intersect(.x, .y)),
    union_su = map2(sualterids.x, sualterids.y, ~ union(.x, .y)),
    prop_agree_su = map2_dbl(sualterids.x, sualterids.y, ~ length(intersect(.x, .y)) / length(union(.x, .y)))
  ) %>%
  dplyr::select(sui, tie, ID1 = personid.x, ID2 = personid.y, alters1 = alterids.x, alters2 = alterids.y, , sualters1 = sualterids.x, sualters2 = sualterids.y, intersection_indiv, union_indiv, prop_agree_indiv, intersection_su, union_su, prop_agree_su)

su_alters_req_pair$PE <- su_alters_req_indiv$PE %>%
  dplyr::inner_join(su_alters_req_indiv$PE, by = "sui", relationship = "many-to-many") %>%
  dplyr::filter(personid.x < personid.y) %>%  # Avoid duplicate and self-pairs
  dplyr::mutate(
    intersection_indiv = map2(alterids.x, alterids.y, ~ intersect(.x, .y)),
    union_indiv = map2(alterids.x, alterids.y, ~ union(.x, .y)),
    prop_agree_indiv = map2_dbl(alterids.x, alterids.y, ~ length(intersect(.x, .y)) / length(union(.x, .y))),
    intersection_su = map2(sualterids.x, sualterids.y, ~ intersect(.x, .y)),
    union_su = map2(sualterids.x, sualterids.y, ~ union(.x, .y)),
    prop_agree_su = map2_dbl(sualterids.x, sualterids.y, ~ length(intersect(.x, .y)) / length(union(.x, .y)))
  ) %>%
  dplyr::select(sui, ID1 = personid.x, ID2 = personid.y, alters1 = alterids.x, alters2 = alterids.y, , sualters1 = sualterids.x, sualters2 = sualterids.y, intersection_indiv, union_indiv, prop_agree_indiv, intersection_su, union_su, prop_agree_su)


su_alters$PE <- rbind(su_alters_all$PE, su_alters_req$PE, su_alters_each$PE)
su_externals$PE <- rbind(su_ex_alters_all$PE, su_ex_alters_req$PE, su_ex_alters_main$PE, su_ex_alters_each$PE)

su_alters$PE <- su_alters$PE %>%
  dplyr::mutate(
    alter_count = map_int(alterids, length),
    alter_status_count = map_int(alterids, ~ sum(people_observations$PE$status[people_observations$PE$personid %in% .x] == 1, na.rm = TRUE)),
    alter_res_count = map_int(alterids, ~ sum(people_observations$PE$is_resident[people_observations$PE$personid %in% .x] == 1, na.rm = TRUE)),
    alter_age_av = map_dbl(alterids, ~ mean(people_observations$PE$age[people_observations$PE$personid %in% .x], na.rm = TRUE))
  )

su_externals$PE <- su_externals$PE %>%
  dplyr::mutate(
    externals_count = map_int(externalids, length),
    externals_status_count = map_int(externalids, ~ sum(people_observations$PE$status[people_observations$PE$personid %in% .x] == 1, na.rm = TRUE)),
    externals_wealth_count = map_int(externalids, ~ sum(people_observations$PE$external_wealth[people_observations$PE$personid %in% .x] == 1, na.rm = TRUE))
  )

su_alters$PE <- su_alters$PE %>%
  dplyr::left_join(su_externals$PE, by = c("sui", "tie"))


#people_observations$PE$wealthofalter[people_observations$PE$wealthofalter == "y"] <-  1
#people_observations$PE$wealthofalter[people_observations$PE$wealthofalter == c("n", "x")] <-  0
#people_observations$PE$wealthofalter[people_observations$PE$wealthofalter %in% c("", "missing", "na")] <-  NA
#people_observations$PE$statusofalter[people_observations$PE$statusofalter == c("n", "x")] <-  0
#people_observations$PE$statusofalter[people_observations$PE$statusofalter %in% c("", "missing", "na")] <-  NA

#su_externals$PE <- su_externals$PE %>%
#  mutate(
#    externals_count = map_int(alterid, length),
#    externals_status_count = map_int(alterid, ~ sum(people_observations$PE$statusofalter[people_observations$PE$personid %in% .x] == 1, na.rm = TRUE)),
#    externals_wealth_count = map_int(alterid, ~ sum(people_observations$PE$wealthofalter[people_observations$PE$personid %in% .x] == 1, na.rm = TRUE))
#  )

#su_alters <- su_alters %>%
#  left_join(su_externals, by = c("su_id_1", "tie"))

########################################################################################################
# ONLY INTERNALS
# Remove externals
net <- networks$PE[networks$PE$personid %in% att2$personid & networks$PE$alterid %in% att2$personid ,]

adjmats <- list()

# Apply the function to different tie values
adjmats$e1 <- process_ties(net, 1, att2)
adjmats$e2 <- process_ties(net, 2, att2)
adjmats$e3 <- process_ties(net, 3, att2)
adjmats$e4 <- process_ties(net, 4, att2)
adjmats$e5 <- process_ties(net, 5, att2)
adjmats$e6 <- process_ties(net, 6, att2)
adjmats$e7 <- process_ties(net, 7, att2)
adjmats$e8 <- process_ties(net, 8, att2)
adjmats$e9 <- process_ties(net, 9, att2)
adjmats$e10 <- process_ties(net, 10, att2)
adjmats$e11 <- process_ties(net, 11, att2)

adjmats_recipients_collapse <- list()

adjmats_recipients_collapse$e1 <- process_ties_collapse(net, 1, att2)
adjmats_recipients_collapse$e2 <- process_ties_collapse(net, 2, att2)
adjmats_recipients_collapse$e3 <- process_ties_collapse(net, 3, att2)
adjmats_recipients_collapse$e4 <- process_ties_collapse(net, 4, att2)
adjmats_recipients_collapse$e5 <- process_ties_collapse(net, 5, att2)
adjmats_recipients_collapse$e6 <- process_ties_collapse(net, 6, att2)
adjmats_recipients_collapse$e7 <- process_ties_collapse(net, 7, att2)
adjmats_recipients_collapse$e8 <- process_ties_collapse(net, 8, att2)
adjmats_recipients_collapse$e9 <- process_ties_collapse(net, 9, att2)
adjmats_recipients_collapse$e10 <- process_ties_collapse(net, 10, att2)
adjmats_recipients_collapse$e11 <- process_ties_collapse(net, 11, att2)

su_nets$PE <- lapply(adjmats,
  function(x) {x <- graph_from_adjacency_matrix(x, mode = "directed", weighted = TRUE)}
  %>% set_vertex_attr("sampled", value = V(x)$name %in% sampled_su)
  %>% set_vertex_attr("num_surv", value = sharing_unit$PE$num_surv[match(V(x)$name, sharing_unit$PE$su_id)])
  %>% set_vertex_attr("num_surv_female_q", value = sharing_unit$PE$num_surv_female_q[match(V(x)$name, sharing_unit$PE$su_id)])
  %>% set_vertex_attr("num_surv_male_q", value = sharing_unit$PE$num_surv_male_q[match(V(x)$name, sharing_unit$PE$su_id)])
  %>% set_vertex_attr("su_wealth", value = sharing_unit$PE$su_wealth[match(V(x)$name, sharing_unit$PE$su_id)])
  %>% set_vertex_attr("susize", value = sharing_unit$PE$su_size[match(V(x)$name, sharing_unit$PE$su_id)])
)

su_nets_recipient_collapse$PE <-
  lapply(adjmats_recipients_collapse,
         function(x) {x <- graph_from_adjacency_matrix(x, mode = "directed", weighted = TRUE)}
         %>% set_vertex_attr("sampled", value = V(x)$name %in% sampled_su)
         %>% set_vertex_attr("num_surv", value = sharing_unit$PE$num_surv[match(V(x)$name, sharing_unit$PE$su_id)])
         %>% set_vertex_attr("num_surv_female_q", value = sharing_unit$PE$num_surv_female_q[match(V(x)$name, sharing_unit$PE$su_id)])
         %>% set_vertex_attr("num_surv_male_q", value = sharing_unit$PE$num_surv_male_q[match(V(x)$name, sharing_unit$PE$su_id)])
         %>% set_vertex_attr("su_wealth", value = sharing_unit$PE$su_wealth[match(V(x)$name, sharing_unit$PE$su_id)])
         %>% set_vertex_attr("susize", value = sharing_unit$PE$su_size[match(V(x)$name, sharing_unit$PE$su_id)])
  )

su_nets$PE$`avrel` <- graph_from_adjacency_matrix(su_m_avr$PE, mode = "undirected", weighted = TRUE)
su_nets$PE$`avrel` <- delete_edges(su_nets$PE$`avrel`, which(E(su_nets$PE$`avrel`)$weight == 0))

su_nets_recipient_collapse$PE$`avrel` <- graph_from_adjacency_matrix(su_m_avr$PE, mode = "undirected", weighted = TRUE)
su_nets_recipient_collapse$PE$`avrel` <- delete_edges(su_nets_recipient_collapse$PE$`avrel`, which(E(su_nets_recipient_collapse$PE$`avrel`)$weight == 0))

su_nets$PE$`maxrel` <- graph_from_adjacency_matrix(su_m_maxr$PE, mode = "undirected", weighted = TRUE)
su_nets$PE$`maxrel` <- delete_edges(su_nets$PE$`maxrel`, which(E(su_nets$PE$`maxrel`)$weight == 0))

su_nets_recipient_collapse$PE$`maxrel` <- graph_from_adjacency_matrix(su_m_maxr$PE, mode = "undirected", weighted = TRUE)
su_nets_recipient_collapse$PE$`maxrel` <- delete_edges(su_nets_recipient_collapse$PE$`maxrel`, which(E(su_nets_recipient_collapse$PE$`maxrel`)$weight == 0))

su_nets$PE$`sibs` <- su_nets_recipient_collapse$PE$`sibs` <- graph_from_adjacency_matrix(su_m_sib$PE, mode = "undirected", weighted = FALSE)
su_nets$PE$`parents` <- su_nets_recipient_collapse$PE$`parents`  <- graph_from_adjacency_matrix(su_m_par$PE, mode = "directed", weighted = FALSE)
su_nets$PE$`children` <- su_nets_recipient_collapse$PE$`children` <- graph_from_adjacency_matrix(su_m_chi$PE, mode = "directed", weighted = FALSE)

## STUPID BUG: NA entries in su_m_dist keep igraph from recognizing a matrix as symmetric
su_m_dist$PE[is.na(su_m_dist$PE)] <- 9999999999
su_nets$PE$`distance` <- graph_from_adjacency_matrix(su_m_dist$PE, mode = "undirected", weighted = TRUE)
su_nets$PE$`distance` <- delete_edges(su_nets$PE$`distance`, which(E(su_nets$PE$`distance`)$weight == 9999999999))

su_nets_recipient_collapse$PE$`distance` <- graph_from_adjacency_matrix(su_m_dist$PE, mode = "undirected", weighted = TRUE)
su_nets_recipient_collapse$PE$`distance` <- delete_edges(su_nets_recipient_collapse$PE$`distance`, which(E(su_nets_recipient_collapse$PE$`distance`)$weight == 9999999999))


# Sanity checks
#all(rownames(share_mat2) == unique(att2$su_id_1))
#all(colnames(share_mat2) == unique(att2$su_id_1))
#all(rownames(share_mat2) == rownames(share_mat1))
#all(colnames(share_mat2) == colnames(share_mat1))
#all(rownames(share_mat2) == rownames(su_m_avr$PE))
#all(colnames(share_mat2) == colnames(su_m_avr$PE))
#all(rownames(share_mat2) == rownames(su_m_dist$PE))
#all(colnames(share_mat2) == colnames(su_m_dist$PE))

# Write out the matrices
write.csv(su_m_avr$PE, file.path(output_path,"PE-su-avrel.csv"))
write.csv(su_m_maxr$PE, file.path(output_path,"PE-su-maxrel.csv"))
write.csv(su_m_dist$PE, file.path(output_path,"PE-su-dist.csv"))
write.csv(adjmats$e1, file.path(output_path,"PE-su-adjmat-1.csv"))
write.csv(adjmats$e2, file.path(output_path,"PE-su-adjmat-2.csv"))
write.csv(adjmats$e3, file.path(output_path,"PE-su-adjmat-3.csv"))
write.csv(adjmats$e4, file.path(output_path,"PE-su-adjmat-4.csv"))
write.csv(adjmats$e5, file.path(output_path,"PE-su-adjmat-5.csv"))
write.csv(adjmats$e6, file.path(output_path,"PE-su-adjmat-6.csv"))
write.csv(adjmats$e7, file.path(output_path,"PE-su-adjmat-7.csv"))
write.csv(adjmats$e8, file.path(output_path,"PE-su-adjmat-8.csv"))
write.csv(adjmats$e9, file.path(output_path,"PE-su-adjmat-9.csv"))
write.csv(adjmats$e10, file.path(output_path,"PE-su-adjmat-10.csv"))
write.csv(adjmats$e11, file.path(output_path,"PE-su-adjmat-11.csv"))
write.csv(adjmats_recipients_collapse$e1, file.path(output_path,"PE-su-adjmat-respondent-collapse-1.csv"))
write.csv(adjmats_recipients_collapse$e2, file.path(output_path,"PE-su-adjmat-respondent-collapse-2.csv"))
write.csv(adjmats_recipients_collapse$e3, file.path(output_path,"PE-su-adjmat-respondent-collapse-3.csv"))
write.csv(adjmats_recipients_collapse$e4, file.path(output_path,"PE-su-adjmat-respondent-collapse-4.csv"))
write.csv(adjmats_recipients_collapse$e5, file.path(output_path,"PE-su-adjmat-respondent-collapse-5.csv"))
write.csv(adjmats_recipients_collapse$e6, file.path(output_path,"PE-su-adjmat-respondent-collapse-6.csv"))
write.csv(adjmats_recipients_collapse$e7, file.path(output_path,"PE-su-adjmat-respondent-collapse-7.csv"))
write.csv(adjmats_recipients_collapse$e8, file.path(output_path,"PE-su-adjmat-respondent-collapse-8.csv"))
write.csv(adjmats_recipients_collapse$e9, file.path(output_path,"PE-su-adjmat-respondent-collapse-9.csv"))
write.csv(adjmats_recipients_collapse$e10, file.path(output_path,"PE-su-adjmat-respondent-collapse-10.csv"))
write.csv(adjmats_recipients_collapse$e11, file.path(output_path,"PE-su-adjmat-respondent-collapse-11.csv"))


# Writing out sharing unit info just for resident SUs
su_meta$PE <- sharing_unit$PE ## all sampled
su_meta$PE <- su_meta$PE[order(su_meta$PE$su_id),]
write.csv(su_meta$PE, file.path(output_path, "PE-su-meta.csv"), row.names = FALSE)

rm(
  add_people,
  att,
  att2,
  kin_dat,
  net,
  pe_database,
  pe_files,
  pe_people_obs,
  pe_sharing_unit,
  poly_men,
  sampled_su,
  sampled_su_2,
  site
)