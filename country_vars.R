require(tidyverse)
# remotes::install_github("xmarquez/democracyData")
# devtools::install_github("vdeminstitute/vdemdata")

# Load data
# Note: This is in another repo!!
eds <- read.csv(file.path(EndowDerivedData,"ethnographer_data_status.csv"),
                stringsAsFactors = FALSE,
                header = TRUE)

# split out TN into TE and AZ
# eds[nrow(eds) + 1, ] <- eds[eds$SiteCode == "TN",]
# eds$SiteCode[nrow(eds)] <- "TE"
# eds$SiteCode[eds$SiteCode == "TN"] <- "AZ"
# eds[nrow(eds) + 1, ] <- eds[eds$SiteCode == "MC",]
# eds$SiteCode[nrow(eds)] <- "AP"

country_data <- eds[, c("SiteCode", "Name", "Country", "Fieldwork.year", "Wave1_Start", "Wave1_End")]


country_data$iso2c <- dplyr::case_match(
  country_data$SiteCode,
  "AN" ~ "AO",
  "AR" ~ "AR",
  c("BD", "SH") ~ "BD",
  "BZ" ~ "BZ",
  c("AV", "DJ") ~ "BJ",
  c("MN", "TI", "TS") ~ "BO",
  "PT" ~ "BR",
  "BF" ~ "BF",
  c("LB", "YI", "YN") ~ "CN",
  c("IH", "PC", "WH", "WL", "EG") ~ "CO",
  "PS" ~ "DM",
  c("CR", "SP", "SY", "EC", "SI") ~ "EC",
  c("KO") ~ "ET", # also Calvert site to come
  "FJ" ~ "FJ",
  c("MC", "AP") ~ "GY",
  "RA" ~ "HT",
  c("TE", "AZ", "EK", "UP", "EX") ~ "IN",
  "KA" ~ "ID",
  c("MG", "MK") ~ "MG",
  "DA" ~ "MW",
  "KT" ~ "MY",
  c("BM", "MY", "TM") ~ "MX",
  c("BH", "OT") ~ "MN",
  "TZ" ~ "MA",
  "SN" ~ "MZ",
  c("FF","HI", "SM") ~ "NA",
  "NP" ~ "NP",
  "NI" ~ "NI",
  c("AH", "HE") ~ "PG",
  c("CP", "KS") ~ "PE",
  "BY" ~ "CG",
  "KM" ~ "RU",
  "TP" ~ "SR",
  c("MA", "PE") ~ "TZ",
  "VT" ~ "VU",
  "ZS" ~ "ZM"
)

country_data$iso3c <- dplyr::case_match(
  country_data$SiteCode,
  "AN" ~ "AGO",
  "AR" ~ "ARG",
  c("BD", "SH") ~ "BGD",
  "BZ" ~ "BLZ",
  c("AV", "DJ") ~ "BEN",
  c("MN", "TI", "TS") ~ "BOL",
  "PT" ~ "BRA",
  "BF" ~ "BFA",
  c("LB", "YI", "YN") ~ "CHN",
  c("IH", "PC", "WH", "WL", "EG") ~ "COL",
  "PS" ~ "DMA",
  c("CR", "SP", "SY", "EC", "SI") ~ "ECU",
  c("KO") ~ "ETH", # also Calvert site to come
  "FJ" ~ "FJI",
  c("MC", "AP") ~ "GUY",
  "RA" ~ "HTI",
  c("TE", "AZ", "EK", "UP", "EX") ~ "IND",
  "KA" ~ "IDN",
  c("MG", "MK") ~ "MDG",
  "DA" ~ "MWI",
  "KT" ~ "MYS",
  c("BM", "MY", "TM") ~ "MEX",
  c("BH", "OT") ~ "MNG",
  "TZ" ~ "MAR",
  "SN" ~ "MOZ",
  c("FF","HI", "SM") ~ "NAM",
  "NP" ~ "NPL",
  "NI" ~ "NIC",
  c("AH", "HE") ~ "PNG",
  c("CP", "KS") ~ "PER",
  "BY" ~ "COG",
  "KM" ~ "RUS",
  "TP" ~ "SUR",
  c("MA", "PE") ~ "TZA",
  "VT" ~ "VUT",
  "ZS" ~ "ZMB"
)

country_data$code3_alt <- dplyr::case_match(
  country_data$SiteCode,
  "AN" ~ "ANG",
  "AR" ~ "ARG",
  c("BD", "SH") ~ "BNG",
  "BZ" ~ "BLZ",
  c("AV", "DJ") ~ "BEN",
  c("MN", "TI", "TS") ~ "BOL",
  "PT" ~ "BRA",
  "BF" ~ "BFO",
  c("LB", "YI", "YN") ~ "CHN",
  c("IH", "PC", "WH", "WL", "EG") ~ "COL",
  "PS" ~ "DMA",
  c("CR", "SP", "SY", "EC", "SI") ~ "ECU",
  c("KO") ~ "ETH", # also Calvert site to come
  "FJ" ~ "FJI",
  c("MC", "AP") ~ "GUY",
  "RA" ~ "HAI",
  c("TE", "AZ", "EK", "UP", "EX") ~ "IND",
  "KA" ~ "INS",
  c("MG", "MK") ~ "MAG",
  "DA" ~ "MAL",
  "KT" ~ "MAW",
  c("BM", "MY", "TM") ~ "MEX",
  c("BH", "OT") ~ "MON",
  "TZ" ~ "MOR",
  "SN" ~ "MZM",
  c("FF","HI", "SM") ~ "NAM",
  "NP" ~ "NEP",
  "NI" ~ "NIC",
  c("AH", "HE") ~ "PNG",
  c("CP", "KS") ~ "PER",
  "BY" ~ "CON",
  "KM" ~ "RUS",
  "TP" ~ "SUR",
  c("MA", "PE") ~ "TAZ",
  "VT" ~ "VUT",
  "ZS" ~ "ZAM"
)

country_data$iso2c[country_data$Name == "Scott Calvert"] <- "ET"
country_data$iso3c[country_data$Name == "Scott Calvert"] <- "ETH"
country_data$code3_alt[country_data$Name == "Scott Calvert"] <- "ETH"

## (for swiid)
country_data$Country[country_data$Country == "Republic of the Congo"] <- "Congo-Brazzaville"

# Function to find closest year with data for a specific variable
# Defaults to choose only prior years, but with use_later = TRUE can select any year
find_closest <- function(
  data,
  target_country,
  target_year,
  variable_name,
  country_col,
  year_col_dataset,
  use_later = FALSE) {

  country_vals <- data[[country_col]]
  year_vals    <- as.numeric(data[[year_col_dataset]])
  var_vals     <- data[[variable_name]]
  target_year  <- as.numeric(target_year)

  if (use_later) {
    keep <- country_vals == target_country & !is.na(var_vals)
    if (!any(keep, na.rm = TRUE)) return(list(value = NA, year_used = NA))
    sub      <- data[keep, ]
    sub_year <- as.numeric(sub[[year_col_dataset]])
    sub      <- sub[order(abs(sub_year - target_year), -sub_year), ]
  } else {
    keep <- country_vals == target_country &
            year_vals <= target_year &
            !is.na(var_vals)
    if (!any(keep, na.rm = TRUE)) return(list(value = NA, year_used = NA))
    sub  <- data[keep, ]
    sub  <- sub[order(-as.numeric(sub[[year_col_dataset]])), ]
  }

  return(list(
    value     = sub[[variable_name]][1],
    year_used = sub[[year_col_dataset]][1]
  ))
}

# Function to merge variables; first go for closest prior year, then fallback to any year if no prior data available
merge_variables <- function(
  df,
  dataset,
  vars,
  country_col_df,
  country_col_dataset,
  year_col_df = "Fieldwork.year",
  year_col_dataset = "year") {

  # Align country codes
  dataset[[country_col_df]] <- dataset[[country_col_dataset]]

  # Select only the variables desired
  dataset_subset <- dataset %>%
    dplyr::select(all_of(country_col_df), all_of(year_col_dataset), all_of(vars)) %>%
    filter(!is.na(.data[[country_col_df]]))

  # Initialize the result dataframe
  result_df <- df

  # For each variable, add value and year columns
  for (var in vars) {
    # Initialize columns
    value_col <- paste0(var, "_value")
    year_col_name <- paste0(var, "_year")
    result_df[[value_col]] <- NA
    result_df[[year_col_name]] <- NA

    # For each row in the original dataframe
    for (i in 1:nrow(df)) {
      target_country <- df[[country_col_df]][i]
      target_year <- df[[year_col_df]][i]

      # First attempt: prior years only
      closest_data <- find_closest(
        data = dataset_subset,
        target_country = target_country,
        target_year = target_year,
        variable_name = var,
        country_col = country_col_df,
        year_col_dataset = year_col_dataset,
        use_later = FALSE
      )

      # Fallback: any year if no prior data found
      if (is.na(closest_data$value)) {
        closest_data <- find_closest(
          data = dataset_subset,
          target_country = target_country,
          target_year = target_year,
          variable_name = var,
          country_col = country_col_df,
          year_col_dataset = year_col_dataset,
          use_later = TRUE
        )
      }

      result_df[[value_col]][i] <- closest_data$value
      result_df[[year_col_name]][i] <- closest_data$year_used
    }
  }

  return(result_df)
}

# VDem
# Missing for: Belize, Dominica
# select V-Dem variables
vdem_variables <- c(
  "v2x_corr", #Political corruption index (D) (v2x_corr) How pervasive is political corruption?
    #   The index is arrived at by taking the average of (a) public sector corruption index
    # (v2x_pubcorr); (b) executive corruption index (v2x_execorr); (c) the indicator for legislative
    # corruption (v2lgcrrpt); and (d) the indicator for judicial corruption (v2jucorrdc). In other
    # words, these four different government spheres are weighted equally in the resulting index. We
    # replace missing values for countries with no legislature by only taking the average of a, b and
    # d.
  "v2x_pubcorr", #Public sector corruption index (D) (v2x_pubcorr)
    # We estimate the index by averaging two indicators: public sector bribery (v2excrptps)
    # and embezzlement (v2exthftps).
  "v2xpe_exlecon", #Exclusion by Socio-Economic Group (D) (v2xpe_exlecon)
    # The index is formed by taking the point estimates from a Bayesian factor analysis
    # model of the indicators power distributed by socio-economic group (v2pepwrses), soci-economic
    # position equality in respect for civil liberties (v2clacjust), access to public services by socio-
    # economic group (v2peapsecon), access to state jobs by socio-economic group (v2peasjsoecon),
    # and access to state business opportunities by socio-economic group (v2peasbecon)
  "v2xpe_exlgender", #Exclusion by Gender index (D) (v2xpe_exlgender)
    # The index is formed by taking the point estimates from a Bayesian factor analysis
    # model of the indicators power distributed bygender (v2pepwgen), equality in respect for civil
    # liberties by gender (v2clgencl), access to public services by gender (v2peapsgen), access to state
    # jobs by gender (v2peasjgen), and access to state business opportunities by gender (v2peasbgen).
  "v2xpe_exlgeo", #Exclusion by Urban-Rural Location index (D) (v2xpe_exlgeo)
    # The index is formed by taking the point estimates from a Bayesian factor analysis
    # model of the indicators power distributed by urban-rural location (v2pepwrgeo), urban-rural
    # equality in respect for civil liberties (v2clgeocl), access to public services by urban-rural location
    # (v2peapsgeo), access to state jobs byurban-rural location (v2peasjgeo), and access to state
    # business opportunities by urban-rural location (v2peasbgeo).
  "v2xpe_exlpol", #Exclusion by Political Group index (D) (v2xpe_exlpol)
    # The index is formed by taking the point estimates from a Bayesian factor analysis
    # model of the indicators political group equality in respect for civil liberties (v2clpolcl), access
    # to public services by political group (v2peapspol), access to state jobs by political group
    # (v2peasjpol), and access to state business opportunities by political group (v2peasbpol)
  "v2xpe_exlsocgr", #Exclusion by Social Group index (D) (v2xpe_exlsocgr)
    # The index is formed by taking the point estimates from a Bayesian factor analysis
    # model of the indicators power distributed by social group (v2pepwrsoc), social group equality
    # in respect for civil liberties (v2clsocgrp), access to public services by social group (v2peapssoc),
    # access to state jobs by social group (v2peasjsoc), and access to state business opportunities by
    # social group (v2peasbsoc)
  "v2x_rule", #Rule of law index (D) (v2x_rule)
    # The index is formed by taking the point estimates from a Bayesian factor analysis
    # model of the indicators for compliance with high court (v2juhccomp), compliance with
    # judiciary (v2jucomp), high court independence (v2juhcind), lower court independence
    # (v2juncind), executive respects constitution (v2exrescon), rigorous and impartial public
    # administration (v2clrspct), transparent laws with predictable enforcement (v2cltrnslw),
    # access to justice for men (v2clacjstm), access to justice for women (v2clacjstw), judicial
    # accountability (v2juaccnt), judicial corruption decision (v2jucorrdc), public sector corrupt
    # exchanges (v2excrptps), public sector theft (v2exthftps), executive bribery and corrupt
    # exchanges (v2exbribe), executive embezzlement and theft (v2exembez)
  "v2cltrnslw", #Transparent laws with predictable enforcement (C) (v2cltrnslw) Are the laws of the land clear, well publicized, coherent (consistent with each other), relatively stable from year to year, and enforced in a predictable manner?
    # 0: Transparency and predictability are almost non-existent. The laws of the land are created
    # and/or enforced in completely arbitrary fashion.
    # 1: Transparency and predictability are severely limited. The laws of the land are more often
    # than not created and/or enforced in arbitrary fashion.
    # 2: Transparency and predictability are somewhat limited. The laws of the land are mostly
    # created in a non-arbitrary fashion but enforcement is rather arbitrary in some parts of the
    # country.
    # 3: Transparency and predictability are fairly strong. The laws of the land are usually created
    # and enforced in a non-arbitrary fashion.
    # 4: Transparency and predictability are very strong. The laws of the land are created and
    # enforced in a non-arbitrary fashion.
  "v2clacjust", #Social class equality in respect for civil liberty (C) (v2clacjust) Do poor people enjoy the same level of civil liberties as rich people do?
    # 0: Poor people enjoy much fewer civil liberties than rich people.
    # 1: Poor people enjoy substantially fewer civil liberties than rich people.
    # 2: Poor people enjoy moderately fewer civil liberties than rich people.
    # 3: Poor people enjoy slightly fewer civil liberties than rich people.
    # 4: Poor people enjoy the same level of civil liberties as rich people.
  "v2clsocgrp", #Social group equality in respect for civil liberties (C) (v2clsocgrp) Do all social groups, as distinguished by language, ethnicity, religion, race, region, orcaste, enjoy the same level of civil liberties, or are some groups generally in a more favorable position?
    # 0: Members of some social groups enjoy much fewer civil liberties than the general population.
    # 1: Members of some social groups enjoy substantially fewer civil liberties than the general
    # population.
    # 2: Members of some social groups enjoy moderately fewer civil liberties than the general
    # population.
    # 3: Members of some social groups enjoy slightly fewer civil liberties than the general population.
    # 4: Members of all salient social groups enjoy the same level of civil liberties.
  "v2xcl_acjst", #Access to justice (D) (v2xcl_acjst)
    # We estimate the index by averaging two indicators: access to justice for men
    # (v2clacjstm) and women (v2clacjstw).
  "v2clacjstm", #Access to justice for men (C) (v2clacjstm) Do men enjoy secure and effective access to justice?
    # 0: Secure and effective access to justice for men is non-existent.
    # 1: Secure and effective access to justice for men is usually not established or widely respected.
    # 2: Secure and effective access to justice for men is inconsistently observed. Minor problems
    # characterize most cases or occur rather unevenly across different parts of the country.
    # 3: Secure and effective access to justice for men is usually observed.
    # 4: Secure and effective access to justice for men is almost always observed.
  "v2clacjstw", #Access to justice for women (C) (v2clacjstw)
  "v2clstown", #State ownership of economy (C) (v2clstown) Does the state own or directly control important sectors of the economy?
    # 0: Virtually all valuable capital belongs to the state or is directly controlled by the state.
    # Private property may be officially prohibited.
    # 1: Most valuable capital either belongs to the state or is directly controlled by the state.
    # 2: Many sectors of the economy either belong to the state or are directly controlled by the
    # state, but others remain relatively free of direct state control.
    # 3: Some valuable capital either belongs to the state or is directly controlled by the state, but
    # most remains free of direct state control.
    # 4: Very little valuable capital belongs to the state or is directly controlled by the state.
  "v2xcl_prpty", #Property rights (D) (v2xcl_prpty)
    # We estimate the index by averaging two indicators: property rights for men
    # (v2clprptym) and women (v2clprptyw).
  "v2clprptym", #Property rights for men (C) (v2clprptym) Do men enjoy the right to private property?
    # 0: Virtually no men enjoy private property rights of any kind.
    # 1: Some men enjoy some private property rights, but most have none.
    # 2: Many men enjoy many private property rights, but a smaller proportion enjoys few or none.
    # 3: More than half of men enjoy most private property rights, yet a smaller share of men have
    # much more restricted rights.
    # 4: Most men enjoy most private property rights but a small minority does not.
    # 5: Virtually all men enjoy all, or almost all property rights.
  "v2clprptyw", #Property rights for women (C) (v2clprptyw)
  "v2xcl_slave", #Freedom from forced labor (D) (v2xcl_slave)
    # We estimate the index by averaging two indicators: freedom from forced labor for
    # men (v2clslavem) and women (v2clslavef)
  "v2clfmove", #Freedom of foreign movement (C) (v2clfmove) Is there freedom of foreign travel and emigration?
    # 0: Not respected by public authorities. Citizens are rarely allowed to emigrate or travel out of
    # the country. Transgressors (or their families) are severely punished. People discredited by the
    # public authorities are routinely exiled or prohibited from traveling.
    # 1: Weakly respected by public authorities. The public authorities systematically restrict the
    # right to travel, especially for political opponents or particular social groups. This can take the
    # form of general restrictions on the duration of stays abroad or delays/refusals of visas.
    # 2: Somewhat respected by the public authorities. The right to travel for leading political
    # opponents or particular social groups is occasionally restricted but ordinary citizens only met
    # minor restrictions.
    # 3: Mostly respected by public authorities. Limitations on freedom of movement and residence
    # are not directed at political opponents but minor restrictions exist. For example, exit visas
    # may be required and citizens may be prohibited from traveling outside the country when
    # accompanied by other members of their family.
    # 4: Fully respected by the government. The freedom of citizens to travel from and to the
    # country, and to emigrate and repatriate, is not restricted by public authorities.
  "v2xcl_dmove", #Freedom of domestic movement (D) (v2xcl_dmove)
    # We estimate the index by averaging two indicators: freedom of domestic movement
    # for men (v2cldmovem) and women (v2cldmovew)
  "v2cldmovem", #Freedom of domestic movement for men (C) (v2cldmovem) Do men enjoy freedom of movement within the country?
    # 0: Virtually no men enjoy full freedom of movement (e.g., North Korea).
    # 1: Some men enjoy full freedom of movement, but most do not (e.g., Apartheid South Africa).
    # 2: Most men enjoy some freedom of movement but a sizeable minority does not. Alternatively
    # all men enjoy partial freedom of movement.
    # 3: Most men enjoy full freedom of movement but a small minority does not.
    # 4: Virtually all men enjoy full freedom of movement.
  "v2cldmovew", #Freedom of domestic movement for women (C) (v2cldmovew) Do women enjoy freedom of movement within the country?
  "v2xcl_disc", #Freedom of discussion (D) (v2xcl_disc)
    # We estimate the index by averaging two indicators: freedom of discussion for men
    # (v2cldiscm) and women (v2cldiscw).
  "v2cldiscm", #Freedom of discussion for men (C) (v2cldiscm) Are men able to openly discuss political issues in private homes and in public spaces?
    # 0: Not respected. Hardly any freedom of expression exists for men. Men are subject to
    # immediate and harsh intervention and harassment for expression of political opinion.
    # 1: Weakly respected. Expressions of political opinions by men are frequently exposed to
    # intervention and harassment.
    # 2: Somewhat respected. Expressions of political opinions by men are occasionally exposed to
    # intervention and harassment.
    # 3: Mostly respected. There are minor restraints on the freedom of expression in the private
    # sphere, predominantly limited to a few isolated cases or only linked to soft sanctions. But as
    # a rule there is no intervention or harassment if men make political statements.
    # 4: Fully respected. Freedom of speech for men in their homes and in public spaces is not
    # restricted.
  "v2cldiscw", #Freedom of discussion for women (C) (v2cldiscw)
  "v2csreprss", #CSO repression (C) (v2csreprss) Does the government attempt to repress civil society organizations (CSOs)?
    # 0: Severely. The government violently and actively pursues all real and even some imagined
    # members of CSOs. They seek not only to deter the activity of such groups but to effectively
    # liquidate them. Examples include Stalinist Russia, Nazi Germany, and Maoist China.
    # 1: Substantially. In addition to the kinds of harassment outlined in responses 2 and 3 below,
    # the government also arrests, tries, and imprisons leaders of and participants in oppositional
    # CSOs who have acted lawfully. Other sanctions include disruption of public gatherings and
    # violent sanctions of activists (beatings, threats to families, destruction of valuable property).
    # Examples include Mugabe’s Zimbabwe, Poland under Martial Law, Serbia under Milosevic.
    # 2: Moderately. In addition to material sanctions outlined in response 3 below, the
    # government also engages in minor legal harassment (detentions, short-term incarceration) to
    # dissuade CSOs from acting or expressing themselves. The government may also restrict the
    # scope of their actions through measures that restrict association of civil society organizations
    # with each other or political parties, bar civil society organizations from taking certain
    # actions, or block international contacts. Examples include post-Martial Law Poland, Brazil in
    # the early 1980s, the late Franco period in Spain.
    # 3: Weakly. The government uses material sanctions (fines, firings, denial of social services) to
    # deter oppositional CSOs from acting or expressing themselves. They may also use
    # burdensome registration or incorporation procedures to slow the formation of new civil
    # society organizations and sidetrack them from engagement. The government may also
    # organize Government Organized Movements or NGOs (GONGOs) to crowd out independent
    # organizations. One example would be Singapore in the post-Yew phase or Putin’s Russia.
    # 4: No. Civil society organizations are free to organize, associate, strike, express themselves,
    # and to criticize the government without fear of government sanctions or harassment.
  "v2pepwrses", #Power distributed by socioeconomic position (C) (v2pepwrses) s political power distributed according to socioeconomic position?
    # 0: Wealthy people enjoy a virtual monopoly on political power. Average and poorer people
    # have almost no influence.
    # 1: Wealthy people enjoy a dominant hold on political power. People of average income have
    # little say. Poorer people have essentially no influence.
    # 2: Wealthy people have a very strong hold on political power. People of average or poorer
    # income have some degree of influence but only on issues that matter less for wealthy people.
    # 3: Wealthy people have more political power than others. But people of average income have
    # almost as much influence and poor people also have a significant degree of political power.
    # 4: Wealthy people have no more political power than those whose economic status is average
    # or poor. Political power is more or less equally distributed across economic groups.
  "v2pepwrsoc", #Power distributed by social group (C) (v2pepwrsoc) Is political power distributed according to social groups?
    # 0: Political power is monopolized by one social group comprising a minority of the
    # population. This monopoly is institutionalized, i.e., not subject to frequent change.
    # 1: Political power is monopolized by several social groups comprising a minority of the
    # population. This monopoly is institutionalized, i.e., not subject to frequent change.
    # 2: Political power is monopolized by several social groups comprising a majority of the
    # population. This monopoly is institutionalized, i.e., not subject to frequent change.
    # 3: Either all social groups possess some political power, with some groups having more power
    # than others; or different social groups alternate in power, with one group controlling much of
    # the political power for a period of time, followed by another — but all significant groups have
    # a turn at the seat of power.
    # 4: All social groups have roughly equal political power or there are no strong ethnic, caste,
    # linguistic, racial, religious, or regional differences to speak of. Social group characteristics are
    # not relevant to politics
  "v2pepwrgen", #Power distributed by gender (C) (v2pepwrgen) Is political power distributed according to gender
    # 0: Men have a near-monopoly on political power.
    # 1: Men have a dominant hold on political power. Women have only marginal influence.
    # 2: Men have much more political power but women have some areas of influence.
    # 3: Men have somewhat more political power than women.
    # 4: Men and women have roughly equal political power
  "v2peedueq", #Educational equality (C) (v2peedueq) To what extent is high quality basic education guaranteed to all, sufficient to enable them to exercise their basic rights as adult citizens?
    # 0: Extreme. Provision of high quality basic education is extremely unequal and at least 75
    # percent (%) of children receive such low-quality education that undermines their ability to
    # exercise their basic rights as adult citizens.
    # 1: Unequal. Provision of high quality basic education is extremely unequal and at least 25
    # percent (%) of children receive such low-quality education that undermines their ability to
    # exercise their basic rights as adult citizens.
    # 2: Somewhat equal. Basic education is relatively equal in quality but ten to 25 percent (%) of
    # children receive such low-quality education that undermines their ability to exercise their basic
    # rights as adult citizens.
    # 3: Relatively equal. Basic education is overall equal in quality but five to ten percent (%) of
    # children receive such low-quality education that probably undermines their ability to exercise
    # their basic rights as adult citizens.
    # 4: Equal. Basic education is equal in quality and less than five percent (%) of children receive
    # such low-quality education that probably undermines their ability to exercise their basic rights
    # as adult citizens.
  "v2pehealth", #Health equality (C) (v2pehealth) To what extent is high quality basic healthcare guaranteed to all, sufficient to enable them to exercise their basic political rights as adult citizens?
    # 0: Extreme. Because of poor-quality healthcare, at least 75 percent (%) of citizens’ ability to
    # exercise their political rights as adult citizens is undermined.
    # 1: Unequal. Because of poor-quality healthcare, at least 25 percent (%) of citizens’ ability to
    # exercise their political rights as adult citizens is undermined.
    # 2: Somewhat equal. Because of poor-quality healthcare, ten to 25 percent (%) of citizens’
    # ability to exercise their political rights as adult citizens is undermined.
    # 3: Relatively equal. Basic health care is overall equal in quality but because of poor-quality
    # healthcare, five to ten percent (%) of citizens’ ability to exercise their political rights as adult
    # citizens is undermined.
    # 4: Equal. Basic health care is equal in quality and less than five percent (%) of citizens cannot
    # exercise their basic political rights as adult citizens.
  "v2peapsecon", #Access to public services distributed by socio-economic position (C)(v2peapsecon) Is access to basic public services, such as order and security, primary education, clean water, and healthcare, distributed equally according to socioeconomic position?
    # 0: Extreme. Because of poverty or low income, 75 percent (%) or more of the population lack
    # access to basic public services of good quality.
    # 1: Unequal. Because of poverty or low income, 25 percent (%) or more of the population lack
    # access to basic public services of good quality.
    # 2: Somewhat Equal. Because of poverty or low income, 10 to 25 percent (%) of the population
    # lack access to basic public services of good quality.
    # 3: Relatively Equal. Because of poverty or low income, 5 to 10 percent (%) of the population
    # lack access to basic public services of good quality.
    # 4: Equal. Because of poverty or low income, less than 5 percent (%) of the population lack
    # access to basic public services of good quality.
  "v2peasjsoecon", #Access to state jobs by socio-economic position (C) (v2peasjsoecon) Are state jobs equally open to qualified individuals regardless of socio-economic position?
    # 0: Extreme. Because of poverty or low income, 75 percent (%) or more of the population, even
    # if qualified, lack access to state jobs.
    # 1: Unequal. Because of poverty or low income, makes 25 percent (%) or more of the population,
    # even if qualified, lack access to state jobs.
    # 2: Somewhat Equal. Because of poverty or low income, 10 to 25 percent (%) of the population,
    # even if qualified, lack access to state jobs.
    # 3: Relatively Equal. Because of poverty or low income, 5 to 10 percent (%) of the population,
    # even if qualified, lack access to state jobs.
    # 4: Equal. Because of poverty or low income, less than 5 percent (%) of the population, even if
    # qualified, lack access to state jobs.
  "v2x_gender", #Women political empowerment index (D) (v2x_gender)
    # The index is formed by taking the average of women’s civil liberties index (v2x_gencl),
    # women’s civil society participation index (v2x_gencs), and women’s political participation
    # index (v2x_genpp).
  "v2x_gencl", #Women civil liberties index (D) (v2x_gencl)
    # The index is formed by taking the point estimates from a Bayesian factor analysis
    # model of the indicators for freedom of domestic movement for women (v2cldmovew), freedom
    # from forced labor for women (v2clslavef), property rights for women (v2clprptyw), and access
    # to justice for women (v2clacjstw).
  "v2xcs_ccsi", #Core civil society index (D) (v2xcs_ccsi)
    # The index is formed by taking the point estimates from a Bayesian factor analysis
    # model of the indicators for CSO entry and exit (v2cseeorgs), CSO repression (v2csreprss) and
    # CSO participatory environment (v2csprtcpt)
  "v2x_clphy", #Physical violence index (D) (v2x_clphy)
    # We estimate the index by averaging two indicators: freedom from torture (v2cltort)
    # and freedom from political killings (v2clkill)
  "v2peprisch", #Primary school enrolment (A) (v2peprisch) What percentage of the primary school-aged population is enrolled in primary school? up to 2010
  "v2pesecsch", #Secondary school enrolment (A) (v2pesecsch) What percentage of the secondary school-aged population is enrolled in secondary school? up to 2010
  "v2petersch" #Tertiary school enrolment (A) (v2petersch) What percentage of the tertiary school-aged population is enrolled in tertiary school? up to 2010

  )

country_data <- merge_variables(
  country_data,
  dataset = vdemdata::vdem,
  vars = vdem_variables,
  country_col_df = "iso3c",
  country_col_dataset = "country_text_id",
  year_col_df = "Fieldwork.year"
)

# Polity V
# Missing for: Belize, Dominica, Vanuatu

polityv_variables <- c(
  "democ",
  "autoc",
  "polity",
  "polity2",
  "durable"
)

country_data <- merge_variables(
  country_data,
  dataset = democracyData::polity5,
  vars = polityv_variables,
  country_col_df = "code3_alt",
  country_col_dataset = "scode",
  year_col_df = "Fieldwork.year"
)


## World Bank WDI data
## really quite poor coverage

wdi_variables <- c(
  "NY.GDP.PCAP.KD", # GDP per capita
  "SI.POV.GINI", # Gini index
  "IQ.CPA.TRAN.XQ", # Trade in services (% of GDP)
  "per_sa_allsa.adq_pop_tot", # Population with access to improved sanitation facilities (% of population)
  "IQ.CPA.PROT.XQ", # Social protection rating (1 = low, 6 = high)
  "SE.ADT.LITR.ZS" # literacy rate
)

#wdi_dat <- WDI::WDI(
#  country = unique(country_data$iso2c),
#  #country = "all",
#  indicator = wdi_variables,
#  start = 1960,
#  end = NULL,
#  extra = FALSE,
#3  cache = NULL,
#  latest = NULL,
#  language = "en"
#)

wdi_dat <- wbwdi::wdi_get(
  entities = "all",
  indicators = wdi_variables,
  start_year = 2010,
  end_year = 2026,
  format = "long"
)

wdi_dat <- wdi_dat |>
  dplyr::select(entity_id, year, indicator_id, value) |>
  pivot_wider(names_from = indicator_id, values_from = value)

country_data <- merge_variables(
  country_data,
  dataset = wdi_dat,
  vars = wdi_variables,
  country_col_df = "iso3c",
  country_col_dataset = "entity_id",
  year_col_df = "Fieldwork.year"
)

# World Bank PIP data
# has all but Dominica, Argentina, Suriname
# NOTE: AR exists not at national level, but instead at URBAN level, so excluded
#devtools::install_github("worldbank/pipr")
pip_dict <- pipr::get_aux("dictionary")

pip_dat <- pipr::get_stats(country = "all", reporting_level = "national")

pip_variables <- c(
  "poverty_line", #"Minimum value representing the basic necessities an individual requires to live (per day)"
  "headcount", #"Proportion (%) of the population that live in households where the consumption or income per person is below the poverty line"
  "poverty_gap", #"Measures the \"intensity\" or \"depth\" of poverty, showing the average shortfall of the total population from the poverty line"
  "poverty_severity", #"Averages the squares of the poverty gaps relative to the poverty line"
  "watts", #"A distribution-sensitive poverty measure computed by dividing the poverty line by income (or consumption), taking logs, and taking the sum over the poor"
  "mean", #"The average daily household per capita income or consumption expenditure from the survey in 2011 PPP"
  "median", #"The median of daily household per capita income or consumption expenditure from the survey in 2011 PPP"
  "mld", #"Index of inequality, given by the mean across the population of the log of the overall mean divided by individual income"
  "gini", #"Measure of inequality between 0 (everyone has the same income) and 100 (richest person has all the income)"
  "polarization", #"Also known as the Wolfson polarization index, it measures the extent to which the distribution of welfare is “spread out”.  It ranges from 0 (no polarization) to 1 (complete polarization)"
  "decile1", #"Income or Consumption share held by lowest/1st decile (%)"
  "decile2", #"Income or Consumption share held by the second/2nd decile (%)"
  "decile3", #"Income or Consumption share held by the third/3rd decile (%)"
  "decile4", #"Income or Consumption share held by the fourth decile (%)"
  "decile5", #"Income or Consumption share held by the fifth/5th decile (%)"
  "decile6", #"Income or Consumption share held by the 6th decile (%)"
  "decile7", #"Income or Consumption share held by the 7th decile (%)"
  "decile8", #"Income or Consumption share held by the 8th decile (%)"
  "decile9", #"Income or Consumption share held by the 9th decile (%)"
  "decile10", #"Income or Consumption share held by the highest/10th decile (%)"
  "cpi", #"Consumer Price Index"
  "ppp", #"Purchasing Power Parity (International Comparison Program - ICP 2011)"
  "pop",
  "gdp"
)

country_data <- merge_variables(
  country_data,
  dataset = pip_dat,
  vars = pip_variables,
  country_col_df = "iso3c",
  country_col_dataset = "country_code",
  year_col_df = "Fieldwork.year"
)


# oecd
# MANY MORE in OECD Data Explorer
# https://data-explorer.oecd.org
# https://github.com/expersso/OECD
# NOTE: Elly at least needed to install OECD from GitHub to get most up-to-date version with proper URLs!
# devtools::install_github("expersso/OECD")

#Proportion of people living below 50 per cent of median income
prop_below50_code <- "OECD.WISE.RSB,DSD_SDG@DF_SDG_G_10,2.0"
prop_below50_filter <- "..10_2..._T._T._T._T._T."
prop_below50 <- OECD::get_dataset(prop_below50_code, prop_below50_filter)

# World Observatory on Subnational Government Finance and Investment (SNG-WOFI) basic socio-economic indicators
# https://www.sng-wofi.org/
sng_wofi_code <- "OECD.CFE.RDG,DSD_SNG_WOFI@DF_SOCIO,1.0"

sng_wofi <- OECD::get_dataset(sng_wofi_code)
# remove case of double entries for HDI
sng_wofi <- sng_wofi[!(sng_wofi$DECIMALS == 0 & sng_wofi$MEASURE == "HDI"),]

sng_wofi_wide <- sng_wofi %>%
  pivot_wider(names_from = MEASURE,
              values_from = c(ObsValue, UNIT_MEASURE, UNIT_MULT, DECIMALS))

colnames(sng_wofi_wide) <- gsub("ObsValue", "sng_wofi", colnames(sng_wofi_wide))

country_data <- merge_variables(
  country_data,
  dataset = sng_wofi_wide,
  vars = colnames(sng_wofi_wide)[grep("sng_wofi", colnames(sng_wofi_wide))],
  country_col_df = "iso3c",
  country_col_dataset = "REF_AREA",
  year_col_df = "Fieldwork.year",
  year_col_dataset = "TIME_PERIOD"
)

# insurance indicators
# missing for most countries
insurance_code <- "OECD.DAF.CM,DSD_INS@DF_IND,1.0"
insurance_filter <- ".A....._T........"
insurance <- OECD::get_dataset(insurance_code, insurance_filter)

# country_data <- merge_variables(
#   country_data,
#   dataset = insurance,
#   vars = "ObsValue",
#   country_col_df = "iso3c",
#   country_col_dataset = "REF_AREA",
#   year_col_df = "Fieldwork.year",
#   year_col_dataset = "TIME_PERIOD"
# )

# NEED TO PROCESS THESE LAST TWO, AND LOOK FOR MORE IN OECD DATABASES

# Gender, Institutions and Development Database (GID-DB) 2023

# https://data-explorer.oecd.org/vis?fs[0]=Reference%20area%2C1%7CNon-OECD%20economies%23WXOECD%23%7CBolivia%23BOL%23&fs[1]=Reference%20area%2C1%7CNon-OECD%20economies%23WXOECD%23%7CBurkina%20Faso%23BFA%23&pg=40&fc=Reference%20area&snb=84&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_GID%40DF_GID_2023&df[ag]=OECD.DEV.NPG&df[vs]=1.0&dq=......&pd=%2C&to[TIME_PERIOD]=false&vw=tb

gid_code <- "OECD.DEV.NPG,DSD_GID@DF_GID_2023,1.0"
gid <- OECD::get_dataset(gid_code)

# country_data <- merge_variables(
#   country_data,
#   dataset = gid,
#   vars = FILL,
#   country_col_df = "iso3c",
#   country_col_dataset = "REF_AREA",
#   year_col_df = "Fieldwork.year",
#   year_col_dataset = "TIME_PERIOD"
# )

# https://sdmx.oecd.org/public/rest/data/OECD.DEV.NPG,DSD_SIGI@DF_SIGI_2023,1.0/all?dimensionAtObservation=AllDimensions

sigi_code <- "OECD.DEV.NPG,DSD_SIGI@DF_SIGI_2023,1.0"
sigi <- OECD::get_dataset(gid_code)


## CIA Factbook
# https://www.cia.gov/the-world-factbook
# DOESN'T have a package or any working repo (mosaic::ciadata is not working)

## SWIID
# The Standardized World Income Inequality Database
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LM4OWF
# https://github.com/fsolt/swiid
# No package yet, have to download
# Tries to deal with uncertainty via 100-long imputation set
# Claims to be most comprehensive and aligned source of income inequality data
# gini_disp: Estimate of Gini index of inequality in equivalized (square root scale) household
# disposable (post-tax, post-transfer) income, using Luxembourg Income Study data as the
# standard.
# gini_mkt: Estimate of Gini index of inequality in equivalized (square root scale) household
# market (pre-tax, pre-transfer) income, using Luxembourg Income Study data as the standard.
# also has gini_disp_se and gini_mkt_se, which are standard errors of the estimates

# having downloaded from dataverse, this CSV is equivalent to 9.9 version
swiid_summary <- read.csv(
  "https://raw.githubusercontent.com/fsolt/swiid/master/data/swiid_summary.csv"
)

country_data <- merge_variables(
  country_data,
  dataset = swiid_summary,
  vars = c("gini_disp", "gini_mkt"),
  country_col_df = "Country",
  country_col_dataset = "country",
  year_col_df = "Fieldwork.year"
)

# write out the data
EndowDerivedData <- file.path(EndowGitHub, "DerivedData")

write.csv(country_data, file.path(EndowDerivedData, "site_country_descriptives.csv"), row.names = FALSE)