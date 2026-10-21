## ==========================================================================
## simulate_endow_database.R
## Generate a fully simulated ENDOW wave-1 database: the eight relational
## tables, internally consistent so that every cross-table key resolves.
##
## The data are entirely synthetic (drawn from random distributions with a
## fixed seed). No real fieldwork data is used or referenced. Re-running with
## the same seed reproduces the dataset exactly.
##
## Usage:
##   source("simulate_endow_database.R")
##   simulate_endow_database(out_dir = "sim_output", n_su = 6, seed = 20260101)
## ==========================================================================

simulate_endow_database <- function(out_dir = "/endow-database-sim",
                                    n_su       = 60,       # number of sharing units
                                    site_code  = "SM",    # 2-letter simulated site code
                                    wave       = 1,
                                    seed       = 20260101) {
  set.seed(seed)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  pid <- function(i) sprintf("%s%04d", tolower(site_code), i)
  sid <- function(i) sprintf("%ssu%03d", tolower(site_code), i)

  ## ---- 1. build households (sharing units) and their resident people ----
  su_ids <- sapply(seq_len(n_su), sid)
  people <- list(); resid <- list(); next_id <- 1; fieldwork_year <- 2023
  for (s in seq_len(n_su)) {
    n_res <- sample(3:7, 1)                        # household size
    ids   <- sapply(next_id:(next_id + n_res - 1), pid)
    next_id <- next_id + n_res
    # roles: one male head, one female head (spouse), rest children/relatives
    rel  <- c("focal", "wife", sample(c("son","daughter","mother","father","grandchild"),
                                      n_res - 2, replace = TRUE))
    sex  <- c("male", "female", sample(c("male","female"), n_res - 2, replace = TRUE))
    head <- c(1, 1, rep(0, n_res - 2))
    # ages: heads adult, children younger
    age  <- c(sample(35:60, 1), sample(30:55, 1),
              if (n_res > 2) sample(1:25, n_res - 2, replace = TRUE) else integer(0))
    for (k in seq_len(n_res)) {
      people[[length(people) + 1]] <- data.frame(
        personid = ids[k], su_id = su_ids[s], sex = sex[k], age = age[k],
        birthyear = fieldwork_year - age[k], rel = rel[k], head = head[k],
        stringsAsFactors = FALSE)
      resid[[length(resid) + 1]] <- data.frame(su_id = su_ids[s], personid = ids[k],
                                               stringsAsFactors = FALSE)
    }
  }
  people <- do.call(rbind, people); residents_df <- do.call(rbind, resid)
  n_people <- nrow(people)

  ## ---- 2. genealogy: assign parents; some are other residents, rest ancestors (8xxx band) ----
  anc <- data.frame(); anc_start <- 8001
  people$father <- NA_character_; people$mother <- NA_character_
  heads <- which(people$head == 1)

  # pool of adult residents eligible to be parents (age >= 30), by sex
  adult_idx   <- which(!is.na(people$age) & people$age >= 30)
  male_pool   <- people$personid[adult_idx][people$sex[adult_idx] == "male"]
  female_pool <- people$personid[adult_idx][people$sex[adult_idx] == "female"]

  p_resident_parent <- 0.3   # chance a head's parent is another community resident, not a synthetic ancestor

  for (h in heads) {
    self_id <- people$personid[h]

    # father
    candidates_f <- setdiff(male_pool, self_id)
    if (runif(1) < p_resident_parent && length(candidates_f) > 0) {
      fa <- sample(candidates_f, 1)
    } else {
      fa <- pid(anc_start); anc_start <- anc_start + 1
      anc <- rbind(anc, data.frame(personid = fa, su_id = NA, sex = "male", age = NA,
                                    birthyear = NA, rel = "ancestor", head = 0,
                                    father = NA, mother = NA, stringsAsFactors = FALSE))
    }

    # mother
    candidates_m <- setdiff(female_pool, self_id)
    if (runif(1) < p_resident_parent && length(candidates_m) > 0) {
      mo <- sample(candidates_m, 1)
    } else {
      mo <- pid(anc_start); anc_start <- anc_start + 1
      anc <- rbind(anc, data.frame(personid = mo, su_id = NA, sex = "female", age = NA,
                                    birthyear = NA, rel = "ancestor", head = 0,
                                    father = NA, mother = NA, stringsAsFactors = FALSE))
    }

    people$father[h] <- fa
    people$mother[h] <- mo
  }
  # children in a household descend from that household's two heads
  for (s in su_ids) {
    hh <- people[people$su_id == s & !is.na(people$su_id), ]
    fa <- hh$personid[hh$rel == "focal"][1]; mo <- hh$personid[hh$rel == "wife"][1]
    kids <- people$su_id == s & people$rel %in% c("son","daughter","grandchild") & !is.na(people$su_id)
    if (!is.na(fa)) people$father[kids] <- fa
    if (!is.na(mo)) people$mother[kids] <- mo
  }
  people <- rbind(people, anc)

  ## ---- 3. partnerships: each household's two heads are a married pair ----
  partnerships <- do.call(rbind, lapply(su_ids, function(s) {
    hh <- people[people$su_id == s & !is.na(people$su_id), ]
    fa <- hh$personid[hh$rel == "focal"][1]; mo <- hh$personid[hh$rel == "wife"][1]
    if (is.na(fa) || is.na(mo)) return(NULL)
    data.frame(waveid = wave, individ_i = fa, individ_j = mo, status = "married",
               stringsAsFactors = FALSE)
  }))

  ## ---- 4. networks: 10 name-generator questions, adults nominate others ----
  adults <- people$personid[!is.na(people$age) & people$age >= 15]
  net <- list()
  for (ego in adults) for (q in 1:10) {
    k <- sample(0:4, 1)                             # nominations for this question
    if (k == 0) next
    alters <- sample(setdiff(adults, ego), min(k, length(adults) - 1))
    net[[length(net) + 1]] <- data.frame(waveid = wave, personid = ego,
                                         alterid = alters, tie = q, stringsAsFactors = FALSE)
  }
  networks <- do.call(rbind, net)

  ## ---- 5. su_distances: symmetric pairwise distances (metres), incl self=0 ----
  coords <- data.frame(su = su_ids, x = runif(n_su, 0, 2000), y = runif(n_su, 0, 2000))
  su_distances <- do.call(rbind, lapply(su_ids, function(i) do.call(rbind, lapply(su_ids, function(j) {
    d <- if (i == j) 0 else round(sqrt((coords$x[coords$su==i]-coords$x[coords$su==j])^2 +
                                         (coords$y[coords$su==i]-coords$y[coords$su==j])^2))
    data.frame(waveid = wave, sui = i, suj = j, distance = d, stringsAsFactors = FALSE)
  }))))

  ## ---- 6. possession_costs: the asset price list (US$) ----
  assets <- c(cow=350, bull=400, goat=60, chicken=8, motorcycle=900, bicycle=90,
              tv=180, smartphone=140, refrigerator=300, sofa=120, cot=45,
              mattress=70, sewingmachine=110, tractor=6000, plow=140, ownedland=1200,
              washingmachine=260, radio=25, laptop=450, car=5000)
  possession_costs <- data.frame(item = names(assets), cost = as.integer(assets),
                                 stringsAsFactors = FALSE)

  ## ---- 7. su_observations: food security + asset counts per household ----
  fs <- function() sample(c("never","rarely","sometimes","often"), 1)
  su_observations <- do.call(rbind, lapply(su_ids, function(s) {
    counts <- setNames(as.list(sapply(names(assets), function(a)
      sample(0:3, 1, prob = c(.5,.3,.15,.05)))), names(assets))
    hh <- people[people$su_id == s & !is.na(people$su_id), ]
    cbind(data.frame(waveid = wave, su_id = s,
                     smaller_meals = fs(), fewer_meals = fs(), no_food = fs(),
                     sleep_hungry = fs(), without_eating = fs(),
                     malehead = hh$personid[hh$rel=="focal"][1],
                     femalehead = hh$personid[hh$rel=="wife"][1], stringsAsFactors = FALSE),
          as.data.frame(counts))
  }))

  ## ---- 8. people (identity) and people_observations (wave attributes) ----
  people_tbl <- data.frame(
    personid = people$personid, site_code = site_code,
    dob = ifelse(is.na(people$birthyear), NA, paste0(people$birthyear, "-99-99")),
    dod = NA_character_, father = people$father, mother = people$mother,
    stringsAsFactors = FALSE)

  occ <- c("farmer","labourer","trader","teacher","homemaker","student","herder", NA)
  edu <- function(a) if (is.na(a)) NA else if (a < 6) 0 else sample(0:12, 1)
  people_observations <- data.frame(
    waveid = wave, personid = people$personid, su_id = people$su_id,
    birth_year = people$birthyear, age = people$age, gender = people$sex,
    status = sample(0:1, nrow(people), replace = TRUE, prob = c(.85,.15)),
    group_id = sample(c("groupA","groupB","groupC"), nrow(people), replace = TRUE),
    external_wealth = NA, relationshiptohh = people$rel, head = people$head,
    marital_status = ifelse(people$age >= 18 & !is.na(people$age),
                            sample(c("married","unmarried","widowed"), nrow(people), replace = TRUE),
                            "unmarried"),
    years_education = sapply(people$age, edu),
    other_noetic = NA, occupation = ifelse(people$age >= 15 & !is.na(people$age),
                                           sample(occ, nrow(people), replace = TRUE), NA),
    work_ability = sample(c("none","moderate", "full"), nrow(people), replace = TRUE, prob = c(.1,.2, .7)),
    location = ifelse(is.na(people$su_id), "external", "community"),
    is_resident = ifelse(is.na(people$su_id), 0, 1),
    is_alive = 1, stringsAsFactors = FALSE)

  ## ---- write ----
  wr <- function(x, n, site_code) write.csv(x, file.path(out_dir, n, paste0(site_code, "_", n, ".csv")), row.names = FALSE)
  wr(people_tbl, "people", site_code); wr(people_observations, "people_observations", site_code)
  wr(su_observations, "su_observations", site_code); wr(networks, "networks", site_code)
  wr(residents_df_named <- transform(residents_df, waveid = wave)[, c("waveid","su_id","personid")], "residents", site_code)
  wr(su_distances, "su_distances", site_code); wr(partnerships, "partnerships", site_code)
  wr(possession_costs, "possession_costs", site_code)

  ## ---- validate referential integrity ----
  ok <- TRUE; chk <- function(cond, msg) { if (!cond) { cat("  FAIL:", msg, "\n"); ok <<- FALSE } }
  cat("Referential integrity checks:\n")
  chk(all(people_observations$personid %in% people_tbl$personid), "obs.personid in people")
  chk(all(na.omit(people_tbl$father) %in% people_tbl$personid), "people.father resolves")
  chk(all(na.omit(people_tbl$mother) %in% people_tbl$personid), "people.mother resolves")
  chk(all(residents_df$personid %in% people_tbl$personid), "residents.personid in people")
  chk(all(networks$personid %in% people_tbl$personid), "networks ego in people")
  chk(all(networks$alterid %in% people_tbl$personid), "networks alter in people")
  chk(all(partnerships$individ_i %in% people_tbl$personid), "partnership i in people")
  chk(all(partnerships$individ_j %in% people_tbl$personid), "partnership j in people")
  chk(all(su_observations$su_id %in% su_distances$sui), "su_obs su in distances")
  chk(all(residents_df$su_id %in% su_observations$su_id), "residents su in su_obs")
  chk(!any(duplicated(people_tbl$personid)), "personid unique")
  if (ok) cat("  ALL PASS\n")
  cat(sprintf("\nWrote 8 tables to '%s/': %d people (%d resident), %d households, %d ties\n",
              out_dir, nrow(people_tbl), sum(!is.na(people$su_id)), n_su, nrow(networks)))
  invisible(ok)
}

database_dir <- "endow-database-sim"

subdirs <- c(
  "networks",
  "partnerships",
  "people",
  "people_observations",
  "possession_costs",
  "residents",
  "su_distances",
  "su_observations"
)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Created: ", path)
  } else {
    message("Already exists: ", path)
  }
}

# Create base dir first, then each subdir
ensure_dir(database_dir)
invisible(lapply(file.path(database_dir, subdirs), ensure_dir))

simulate_endow_database(out_dir = database_dir, n_su = 60, seed = 20260101, site_code = "EG")
simulate_endow_database(out_dir = database_dir, n_su = 70, seed = 20260102, site_code = "EX")
simulate_endow_database(out_dir = database_dir, n_su = 100, seed = 20260103, site_code = "SM")
simulate_endow_database(out_dir = database_dir, n_su = 40, seed = 20260104, site_code = "SI")
