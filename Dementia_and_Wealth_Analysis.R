# Loading Data

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(scales)
library(kableExtra)
library(purrr)
library(scales)



alp_fb12 <- read.csv('/Users/zaynabadr/Documents/NMP Scholars/ALP financial data_2012/ms284_weighted(Sheet1).csv')
alp_fb18 <- read.csv('/Users/zaynabadr/Documents/NMP Scholars/ALP financial data_2018/alp18.csv')

#---------------#
# Recoding Data
#---------------#

# Rule 1: collapse frequency scales (1 = Never, 2-5 = 1/2/3/4+ times) to binary,
#         with 98/99 folded into "Missing". True skip-pattern NA stays NA.
recode_freq <- function(x) {
  out <- dplyr::case_when(
    x == 1 ~ "Never",
    x %in% c(2, 3, 4, 5) ~ "One or more time",
    x %in% c(98, 99) ~ "Missing",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Never", "One or more time", "Missing"))
}

# Rule 2 (standalone use): fold 98/99 into "Missing" for an ordinary yes/no
#         (1 = Yes, 2 = No) or other small nominal item. True NA stays NA.
recode_yesno_missing <- function(x, yes_val = 1, no_val = 2) {
  out <- dplyr::case_when(
    x == yes_val ~ "Yes",
    x == no_val ~ "No",
    x %in% c(98, 99) ~ "Missing",
    TRUE ~ NA_character_
  )
  factor(out, levels = c("Yes", "No", "Missing"))
}

# Generic version for nominal items with more than two substantive categories
# (e.g., J3 spending-vs-income: 1/2/3). Keeps original category labels,
# only folds 98/99 into "Missing".
recode_missing_generic <- function(x, labels = NULL) {
  out <- as.character(x)
  out[out %in% c("98", "99")] <- "Missing"
  if (!is.null(labels)) {
    out[out %in% names(labels)] <- labels[out[out %in% names(labels)]]
  }
  factor(out)
}

# Rule 3: combine single/spouse/partner variants of a yes/no question.
#   - "Yes" if ANY supplied component == 1, even if another component is
#     missing (98/99) or not applicable (NA) — Yes overrides everything.
#   - else "Missing" if ANY component is 98/99.
#   - else "No" if at least one component == 2 (No) and none are missing.
#   - else NA if every component is NA (question truly not applicable, e.g.
#     no spouse/partner and not asked as single).
combine_yesno <- function(...) {
  mat <- cbind(...)
  apply(mat, 1, function(row) {
    if (any(row == 1, na.rm = TRUE)) {
      return("Yes")
    } else if (any(row %in% c(98, 99), na.rm = TRUE)) {
      return("Missing")
    } else if (any(row == 2, na.rm = TRUE)) {
      return("No")
    } else {
      return(NA_character_)
    }
  }) |> factor(levels = c("Yes", "No", "Missing"))
}

# Rule 4: turn a 1-10 / 1-7 Likert/scale item into a clean continuous numeric
#         variable, with 98/99 -> NA (can't average "don't know" into a scale).
recode_continuous <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[x %in% c(98, 99)] <- NA_real_
  x
}

# Safe combine: only combines columns that actually exist in the data frame,
# so the same call works across waves with slightly different column sets.
safe_combine_yesno <- function(df, cols) {
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(rep(NA, nrow(df)))
  do.call(combine_yesno, as.list(df[cols]))
}

# -----------------------------------------------------------------------------
# 2. Apply recoding — 2018 wave (alp_fb18)
# -----------------------------------------------------------------------------
alp_fb18 <- alp_fb18 %>%
  mutate(
    person_id = sub(":.*", "", prim_key),
    
    highesteducation_clean = dplyr::case_when(
      highesteducation %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ "Less than High School",
      highesteducation == 9 ~ "High School Diploma",
      highesteducation %in% c(10, 11, 12) ~ "Some College - No Bachelor's",
      highesteducation == 13 ~ "Bachelor's Degree",
      highesteducation %in% c(14, 15, 16) ~ "Graduate Level Degree",
      TRUE ~ NA_character_
    ),
    
    familyincome_clean = dplyr::case_when(
      familyincome %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ "Less than 30,000",
      familyincome %in% c(9, 10, 11) ~ "30,000 - 50,000",
      familyincome %in% c(12, 13) ~ "50,000 - 75,000",
      familyincome %in% c(14) ~ "75,000+",
      TRUE ~ NA_character_
    ),
    
    a9_clean = dplyr::case_when(
      a9 %in% c(1, 2, 3) ~ "Employed",
      a9 %in% c(4, 5, 7) ~ "Unemployed",
      a9 %in% c(6, 8) ~ "Retired/Sick or Disabled",
      TRUE ~ NA_character_
    ),
    
    g23_clean = dplyr::case_when(
      g23 %in% c(1, 2, 3) ~ "Disagree",
      g23 == 4 ~ "Neutral",
      g23 %in% c(5, 6, 7) ~ "Agree",
      TRUE ~ NA_character_
    ),
    
    a6_clean = dplyr::case_when(
      a6 == 1 ~ "Married",
      a6 == 2 ~ "Single",
      a6 %in% c(3, 4) ~ "Divorced/Separated",
      a6 == 5 ~ "Widowed/Widower",
      TRUE ~ NA_character_
    ),
    
    # --- Rule 4: continuous scales -------------------------------------
    j2_cont  = recode_continuous(j2),    # risk tolerance, 1-10
    
    # --- Rule 1: frequency items (G25_1-5 / high-cost borrowing) -------
    g5_1_bin = recode_freq(g5_1),        # auto title loan
    g5_2_bin = recode_freq(g5_2),        # payday loan
    g5_3_bin = recode_freq(g5_3),        # tax refund advance
    g5_4_bin = recode_freq(g5_4),        # pawn shop
    g5_5_bin = recode_freq(g5_5),        # rent-to-own
    
    # --- Rule 3: combine single/spouse/partner (Yes if any Yes) --------
    b4_comb  = safe_combine_yesno(cur_data(), c("b4single", "b4spouse", "b4partner")),   # overdraws checking
    b14_comb = safe_combine_yesno(cur_data(), c("b14single", "b14household")),           # has investments
    c10_comb = safe_combine_yesno(cur_data(), c("c10single", "c10spouse", "c10partner")),# retirement loan
    c11_comb = safe_combine_yesno(cur_data(), c("c11single", "c11spouse", "c11partner")),# hardship withdrawal
    j9_comb = safe_combine_yesno(cur_data(), c("j9")),
    j10_comb = safe_combine_yesno(cur_data(), c("j10single", "j10household")),           # unexpected income drop
    
    # --- Rule 2: fold 98/99 -> "Missing" for remaining nominal items ---
    j3_clean   = recode_missing_generic(j3),      # spending vs income (3 cats)
    j8_clean   = recode_yesno_missing(j8),         # tried to figure retirement savings
    j9_clean   = recode_yesno_missing(j9),         # spouse/partner retirement savings target
    e7_clean   = recode_yesno_missing(e7),         # has mortgage
    e8_clean   = recode_yesno_missing(e8),         # has home equity loan
    e20_clean  = recode_yesno_missing(e20),        # owes more than home worth
    e15_clean  = recode_missing_generic(e15),      # late on mortgage (never/once/more than once)
    f2_4_clean = recode_yesno_missing(f2_4),       # charged late fee
    f2_5_clean = recode_yesno_missing(f2_5),       # charged over-limit fee
    g20_clean  = recode_yesno_missing(g20),        # unpaid medical bills
    m6_clean   = recode_missing_generic(m6),       # compound interest quiz
    m7_clean   = recode_missing_generic(m7),       # inflation quiz
    m8_clean   = recode_missing_generic(m8),       # bond price quiz
    m9_clean   = recode_yesno_missing(m9, yes_val = 1, no_val = 2),  # mortgage term T/F
    m10_clean  = recode_yesno_missing(m10, yes_val = 1, no_val = 2), # stock vs fund T/F
    a10a_clean = recode_missing_generic(a10a),      # household retirement status
    g21_clean  = recode_yesno_missing(g21),
    g22_clean  = recode_yesno_missing(g22),
    g4_clean   = recode_yesno_missing(g4),
    j11_clean  = recode_yesno_missing(j11),
    j12_clean  = recode_yesno_missing(j12),
    f2_1_clean = recode_yesno_missing(f2_1)
  ) %>%
  mutate(wave = 2018)

# -----------------------------------------------------------------------------
# 3. Apply recoding — 2012 wave (alp_fb12)
#    Note: 2012 has no top-level c10 / c11 (only *single/*spouse/*partner),
#    so the combine step is what CREATES those variables for this wave.
# -----------------------------------------------------------------------------
alp_fb12 <- alp_fb12 %>%
  mutate(
    person_id = sub(":.*", "", prim_key),
    
    highesteducation_clean = dplyr::case_when(
      highesteducation %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ "Less than High School",
      highesteducation == 9 ~ "High School Diploma",
      highesteducation %in% c(10, 11, 12) ~ "Some College - No Bachelor's",
      highesteducation == 13 ~ "Bachelor's Degree",
      highesteducation %in% c(14, 15, 16) ~ "Graduate Level Degree",
      TRUE ~ NA_character_
    ),
    
    familyincome_clean = dplyr::case_when(
      familyincome %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ "Less than 30,000",
      familyincome %in% c(9, 10, 11) ~ "30,000 - 50,000",
      familyincome %in% c(12, 13) ~ "50,000 - 75,000",
      familyincome %in% c(14) ~ "75,000+",
      TRUE ~ NA_character_
    ),
    
    a9_clean = dplyr::case_when(
      a9civilian %in% c(1, 2, 3) ~ "Employed",
      a9civilian %in% c(4, 5, 7) ~ "Unemployed",
      a9civilian %in% c(6, 8) ~ "Retired/Sick or Disabled",
      TRUE ~ NA_character_
    ),
    
    g23_clean = dplyr::case_when(
      g23 %in% c(1, 2, 3) ~ "Disagree",
      g23 == 4 ~ "Neutral",
      g23 %in% c(5, 6, 7) ~ "Agree",
      TRUE ~ NA_character_
    ),
    
    a6_clean = dplyr::case_when(
      a6 == 1 ~ "Married",
      a6 == 2 ~ "Single",
      a6 %in% c(3, 4) ~ "Divorced/Separated",
      a6 == 5 ~ "Widowed/Widower",
      TRUE ~ NA_character_
    ),
    
    # --- Rule 4: continuous scales -------------------------------------
    j2_cont  = recode_continuous(j2),
    
    # --- Rule 1: frequency items ----------------------------------------
    g5_1_bin = recode_freq(g5_1),
    g5_2_bin = recode_freq(g5_2),
    g5_3_bin = recode_freq(g5_3),
    g5_4_bin = recode_freq(g5_4),
    g5_5_bin = recode_freq(g5_5),
    
    # --- Rule 3: combine single/spouse/partner --------------------------
    b4_comb  = safe_combine_yesno(cur_data(), c("b4single", "b4spouse", "b4partner")),
    b14_comb = safe_combine_yesno(cur_data(), c("b14single", "b14household")),
    c10_comb = safe_combine_yesno(cur_data(), c("c10single", "c10spouse", "c10partner")),
    c11_comb = safe_combine_yesno(cur_data(), c("c11single", "c11spouse", "c11partner")),
    j9_comb  = safe_combine_yesno(cur_data(), c("j9self", "j9spouse", "j9partner")),
    j10_comb = safe_combine_yesno(cur_data(), c("j10single", "j10household")),
    
    # --- Rule 2: fold 98/99 -> "Missing" for remaining nominal items ---
    j3_clean   = recode_missing_generic(j3),
    j8_clean   = recode_yesno_missing(j8),
    e7_clean   = recode_yesno_missing(e7),
    e8_clean   = recode_yesno_missing(e8),
    e20_clean  = recode_yesno_missing(e20),
    e15_clean  = recode_missing_generic(e15),
    f2_1_clean = recode_yesno_missing(f2_1),
    f2_4_clean = recode_yesno_missing(f2_4),
    f2_5_clean = recode_yesno_missing(f2_5),
    g20_clean  = recode_yesno_missing(g20),
    m6_clean   = recode_missing_generic(m6),
    m7_clean   = recode_missing_generic(m7),
    m8_clean   = recode_missing_generic(m8),
    m9_clean   = recode_yesno_missing(m9),
    m10_clean  = recode_yesno_missing(m10),
    a10a_clean = recode_missing_generic(a10a),
    g21_clean  = recode_yesno_missing(g21),
    g22_clean  = recode_yesno_missing(g22),
    g4_clean   = recode_yesno_missing(g4),
    j11_clean  = recode_yesno_missing(j11),
    j12_clean  = recode_yesno_missing(j12)
  ) %>%
  mutate(wave = 2012)


# Adding Question Scores


alp_fb12 <- alp_fb12 %>%
  mutate(
    three_qs = as.integer(m6 == 1) +
      as.integer(m7 == 3) +
      as.integer(m10 == 2)
  )

alp_fb18 <- alp_fb18 %>%
  mutate(
    three_qs = as.integer(m6 == 1) +
      as.integer(m7 == 3) +
      as.integer(m10 == 2)
  )

alp_fb12 <- alp_fb12 %>%
  mutate(
    five_qs = as.integer(m6 == 1) +
      as.integer(m7 == 3) +
      as.integer(m8 == 2) +
      as.integer(m9 == 1) +
      as.integer(m10 == 2),
      age_group = case_when(
        calcage < 50 ~ "Under 50",
        calcage >= 50 & calcage <= 64 ~ "50-64",
        calcage >= 65 & calcage <= 74 ~ "65-74",
        calcage >= 75 ~ "75+",
        TRUE ~ NA_character_
      ),
      age_group = factor(age_group, levels = c("Under 50", "50-64", "65-74", "75+"))
  )

alp_fb18 <- alp_fb18 %>%
  mutate(
    five_qs = as.integer(m6 == 1) +
      as.integer(m7 == 3) +
      as.integer(m8 == 2) +
      as.integer(m9 == 1) +
      as.integer(m10 == 2),
    age_group = case_when(
      calcage < 50 ~ "Under 50",
      calcage >= 50 & calcage <= 64 ~ "50-64",
      calcage >= 65 & calcage <= 74 ~ "65-74",
      calcage >= 75 ~ "75+",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("Under 50", "50-64", "65-74", "75+"))
  )

# ---------------------------#
# Making Table/Variable Coding
# --------------------------#


# List of Variables for Table

vars <- list(
  list(var = "gender",
       label = "Gender",
       levels = c("1" = "Male", "2" = "Female")),
  
  list(var = "ethnicity",
       label = "Ethnicity",
       levels = c("1" = "White/Caucasian", 
                  "2" = "Black/African American", 
                  "3" = "American Indian or Alaskan Native",
                  "4" = "Asian or Pacific Islander",
                  "5" = "Other")),
  
  list(var = "highesteducation_clean",
       label = "Highest Ed",
       levels = c("Less than High School" = "Less than High School",
                  "High School Diploma" = "High School Diploma",
                  "Some College - No Bachelor's" = "Some College - No Bachelor's",
                  "Bachelor's Degree" = "Bachelor's Degree",
                  "Graduate Level Degree" = "Graduate Level Degree")),
  
  list(var = "familyincome_clean",
       label = "Family Income",
       levels = c("Less than 30,000" = "Less than 30,000",
                  "30,000 - 50,000" = "30,000 - 50,000",
                  "50,000 - 75,000" = "50,000 - 75,000",
                  "75,000+" = "75,000+")),
  
  list(var = "age_group",
       label = "Age Group",
       levels = c("Under 50" = "Under 50",
                  "50-64" = "50-64", 
                  "65-74" = "65-74",
                  "75+" = "75+")),
  
  list(var = "a9_clean",
       label = "Work status",
       levels = c("Employed" = "Employed",
                  "Unemployed" = "Unemployed",
                  "Retired/Sick or Disabled" = "Retired/Sick or Disabled")),
  
  list(var = "a6_clean",
       label = "Marital Status",
       levels = c("Married" = "Married",
                  "Single" = "Single",
                  "Divorced/Separated" = "Divorced/Separated",
                  "Widowed/Widower" = "Widowed/Widower")),
  
  # --- Rule 2 recoded (98/99 -> single "Missing") ---
  list(var = "g20_clean", label = "Unpaid medical bills",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "g21_clean", label = "Has student loans",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "g22_clean", label = "Concerned about paying off student loans",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "g23_clean", label = "Amount of debt agreement scale",
       levels = c("Disagree" = "Disagree",
                  "Neutral" = "Neutral",
                  "Agree" = "Agree")),
  
  list(var = "g4_clean", label = "Bankruptcy",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "j8_clean", label = "Tried to figure out retirement savings needed",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "j11_clean", label = "Has checked credit report",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "j12_clean", label = "Has checked credit score",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "e7_clean", label = "Has mortgage",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "e8_clean", label = "Has home equity loan",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "e20_clean", label = "Owes more on home than it's worth",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "e15_clean", label = "Late on mortgage payments (past year)",
       levels = c("1" = "Never", "2" = "Once", "3" = "More than once", "Missing" = "Missing")),
  
  list(var = "f2_1_clean", label = "Always paid credit card in full",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "f2_4_clean", label = "Charged late fee (past year)",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "f2_5_clean", label = "Charged over-limit fee (past year)",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "m6_clean", label = "Compound interest quiz",
       levels = c("1" = "More than $102 (correct)", "2" = "Exactly $102",
                  "3" = "Less than $102", "Missing" = "Missing")),
  
  list(var = "m7_clean", label = "Inflation/purchasing power quiz",
       levels = c("1" = "More than today", "2" = "Exactly the same",
                  "3" = "Less than today (correct)", "Missing" = "Missing")),
  
  list(var = "m8_clean", label = "Bond price/interest rate quiz",
       levels = c("1" = "They will rise", "2" = "They will fall (correct)",
                  "3" = "They will stay the same", "4" = "No relationship",
                  "Missing" = "Missing")),
  
  list(var = "m9_clean", label = "Mortgage term true/false",
       levels = c("Yes" = "True (correct)", "No" = "False", "Missing" = "Missing")),
  
  list(var = "m10_clean", label = "Single stock vs. mutual fund true/false",
       levels = c("Yes" = "True", "No" = "False (correct)", "Missing" = "Missing")),
  
  list(var = "a10a_clean", label = "Household retirement status",
       levels = c("1" = "Non-retired household", "2" = "Retired--respondent retired",
                  "3" = "Retired--respondent not working, spouse retired", "Missing" = "Missing")),
  
  # --- Rule 1 recoded (frequency -> Never / One or more time) ---
  list(var = "g5_1_bin", label = "Auto title loan (past 5 yrs)",
       levels = c("Never" = "Never", "One or more time" = "One or more time", "Missing" = "Missing")),
  
  list(var = "g5_2_bin", label = "Payday loan (past 5 yrs)",
       levels = c("Never" = "Never", "One or more time" = "One or more time", "Missing" = "Missing")),
  
  list(var = "g5_3_bin", label = "Tax refund advance (past 5 yrs)",
       levels = c("Never" = "Never", "One or more time" = "One or more time", "Missing" = "Missing")),
  
  list(var = "g5_4_bin", label = "Pawn shop (past 5 yrs)",
       levels = c("Never" = "Never", "One or more time" = "One or more time", "Missing" = "Missing")),
  
  list(var = "g5_5_bin", label = "Rent-to-own (past 5 yrs)",
       levels = c("Never" = "Never", "One or more time" = "One or more time", "Missing" = "Missing")),
  
  # --- Rule 3 recoded (combined single/spouse/partner) ---
  list(var = "b4_comb", label = "Overdraws checking account",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "b14_comb", label = "Has investments (stocks/bonds/mutual funds)",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "c10_comb", label = "Took retirement account loan",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "c11_comb", label = "Took hardship withdrawal",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "j9_comb", label = "Tried to figure out retirement savings needed (self/spouse/partner)",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "j10_comb", label = "Experienced unexpected income drop",
       levels = c("Yes" = "Yes", "No" = "No", "Missing" = "Missing")),
  
  list(var = "three_qs", label = "Three-Question Cognitive Score",
       levels = c("0" = "0", "1" = "1", "2" = "2", "3" = "3"))
)


# -----------------------------------------------#
# Creating Summary Statistics Table of Variables #
# -----------------------------------------------#


alp_merged <- alp_fb12 %>%
  inner_join(alp_fb18, by = "prim_key", suffix = c("_12", "_18")) %>%
  mutate(three_qs_change = three_qs_18 - three_qs_12) %>%
  mutate(five_qs_change = five_qs_18 - five_qs_12)

alp_dropped <- alp_fb12 %>% filter(!(prim_key %in% alp_fb18$prim_key))
alp_remained <- alp_fb12 %>% filter(prim_key %in% alp_fb18$prim_key)

alp_improved_unchanged_3q <- alp_merged %>% filter(three_qs_change >= 0)
alp_declined_3q <- alp_merged %>% filter(three_qs_change < 0)

# resolve correct column name, accounting for _12/_18 suffixes
resolve_col <- function(data, var, suffix = "") {
  candidate <- paste0(var, suffix)
  if (candidate %in% names(data)) return(candidate)
  if (var %in% names(data)) return(var)
  return(NA_character_)
}

# get category counts + total non-missing n for a variable
get_counts <- function(data, colname, levels) {
  if (is.na(colname) || !colname %in% names(data)) {
    return(list(counts = rep(NA_integer_, length(levels)), total = NA_integer_))
  }
  tbl <- table(factor(data[[colname]], levels = names(levels)), useNA = "no")
  list(counts = as.integer(tbl), total = sum(tbl))
}

# proportion test per category
chisq_pval <- function(res1, res2) {
  if (any(is.na(res1$counts)) || any(is.na(res2$counts)) ||
      is.na(res1$total) || is.na(res2$total) ||
      res1$total == 0 || res2$total == 0) {
    return(NA_real_)
  }
  tbl <- rbind(res1$counts, res2$counts)  # 2 rows (groups) x k cols (categories)
  tryCatch(
    suppressWarnings(chisq.test(tbl)$p.value),
    error = function(e) NA_real_
  )
}

# Build the full summary table
cat_summary <- lapply(vars, function(item) {
  var <- item$var
  levels <- item$levels
  
  col_total <- resolve_col(alp_fb12, var) # what this variable is called in the 2012 df
  res_total <- get_counts(alp_fb12, col_total, levels) # category counts and total sample size (not including NAs but including "missing")
  
  col_drop <- resolve_col(alp_dropped, var) # what is this variable in the dropped people
  res_drop <- get_counts(alp_dropped, col_drop, levels) # what is the category counts and total sample size from the dropped people
  
  col_remain <- resolve_col(alp_remained, var) # what is this variable in the remaining ppl
  res_remain <- get_counts(alp_remained, col_remain, levels) # what is the category counts and total sample size from teh remaining ppl
  
  col_decl <- resolve_col(alp_declined_3q, var, suffix = "_12") # what is this variable in the declining ppl
  res_decl <- get_counts(alp_declined_3q, col_decl, levels) # category counts and total sample size of declining ppl
  
  col_imp <- resolve_col(alp_improved_unchanged_3q, var, suffix = "_12") 
  res_imp <- get_counts(alp_improved_unchanged_3q, col_imp, levels)
  
  p_drop_remain <- chisq_pval(res_drop, res_remain)
  p_decl_imp <- chisq_pval(res_decl, res_imp)
  
  data.frame(
    Variable_name = var,
    Variable_Description = item$label,
    Category = levels[names(levels)],
    Pct = paste0(round(100 * res_total$counts / res_total$total, 1), "%"),
    Dropped_Pct = paste0(round(100 * res_drop$counts / res_drop$total, 1), "%"),
    Remained_Pct = paste0(round(100 * res_remain$counts / res_remain$total, 1), "%"),
    p_dropped_vs_remained = round(p_drop_remain, 4),
    Declined_Pct = paste0(round(100 * res_decl$counts / res_decl$total, 1), "%"),
    ImprovedUnchanged_Pct = paste0(round(100 * res_imp$counts / res_imp$total, 1), "%"),
    p_declined_vs_improved = round(p_decl_imp, 4),
    Total_Respondents = res_total$total,
    Total_Remained = res_remain$total,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

fb_sum <- kable(cat_summary, row.names = FALSE)

library(writexl)

write_xlsx(cat_summary, "Summary Statistics and Comparisons_3.xlsx")

#------------------------------ Extra code for Summary Statistics Table -------------------------------------#
# -------------Filling in P-values where they weren't included due to one category proportion being 0 -------#
# a10a_resdrop <- get_counts(alp_dropped, "a10a_clean", c("1" = "Non-retired household", "2" = "Retired--respondent retired",
#                                                         "3" = "Retired--respondent not working, spouse retired"))
# a10a_resremained <- get_counts(alp_remained, "a10a_clean", c("1" = "Non-retired household", "2" = "Retired--respondent retired",
#                                                              "3" = "Retired--respondent not working, spouse retired"))
# 
# a10a_resdeclined <- get_counts(alp_declined_3q, "a10a_clean_12", c("1" = "Non-retired household", "2" = "Retired--respondent retired"))
# a10a_resimp <- get_counts(alp_improved_unchanged_3q, "a10a_clean_12", c("1" = "Non-retired household", "2" = "Retired--respondent retired"))
# 
# chisq_pval(a10a_resdrop, a10a_resremained)
# 
# chisq_pval(a10a_resdeclined, a10a_resimp)

# -------------------------------------------------------------------------------------------

# a10a_resdrop <- get_counts(alp_dropped, "c10_comb", c("Yes" = "Yes", "No" = "No"))
# a10a_resremained <- get_counts(alp_remained, "c10_comb", c("Yes" = "Yes", "No" = "No"))
# 
# a10a_resdeclined <- get_counts(alp_declined_3q, "c10_comb_12", c("Yes" = "Yes", "No" = "No"))
# a10a_resimp <- get_counts(alp_improved_unchanged_3q, "c10_comb_12", c("Yes" = "Yes", "No" = "No"))
# 
# chisq_pval(a10a_resdrop, a10a_resremained)
# 
# chisq_pval(a10a_resdeclined, a10a_resimp)

# ------------------------------------------------------------------------------------------

# a10a_resdrop <- get_counts(alp_dropped, "c11_comb", c("Yes" = "Yes", "No" = "No"))
# a10a_resremained <- get_counts(alp_remained, "c11_comb", c("Yes" = "Yes", "No" = "No"))
# 
# a10a_resdeclined <- get_counts(alp_declined_3q, "c11_comb_12", c("Yes" = "Yes", "No" = "No"))
# a10a_resimp <- get_counts(alp_improved_unchanged_3q, "c11_comb_12", c("Yes" = "Yes", "No" = "No"))
# 
# chisq_pval(a10a_resdrop, a10a_resremained)
# 
# chisq_pval(a10a_resdeclined, a10a_resimp)

# -------------------------------------------------------------------------------------------


# ----------------------------#
# Creating Visuals for Poster #
# ----------------------------#


# FIRST VISUAL: Plot of Mean Financial Literacy Questions in Both Waves

age_trend_data <- bind_rows(
  alp_fb12 %>% select(three_qs, calcage) %>% mutate(wave = 2012),
  alp_fb18 %>% select(three_qs, calcage) %>% mutate(wave = 2018)
) %>%
  mutate(
    age_group = case_when(
      calcage < 50 ~ "Under 50",
      calcage >= 50 & calcage <= 64 ~ "50-64",
      calcage >= 65 & calcage <= 74 ~ "65-74",
      calcage >= 75 ~ "75+",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("Under 50", "50-64", "65-74", "75+"))
  )

plot_data <- age_trend_data %>%
  filter(!is.na(age_group), !is.na(three_qs)) %>%
  group_by(wave, age_group) %>%
  summarise(mean_score = mean(three_qs, na.rm = TRUE),
            n = n(),
            .groups = "drop")

ggplot(plot_data, aes(x = wave, y = mean_score,
                      color = age_group, group = age_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = c(2012, 2018),      # only label the two years that actually exist
    limits = c(2012, 2018)       # widen the panel so points aren't flush against the edges
  ) +
  scale_y_continuous(
    limits = c(1.9, 2.6),
    breaks = seq(1.9, 2.6, by = 0.1)   # label every 0.1
  ) +
  labs(
    x = "Year",
    y = "Mean 3-Question Financial Literacy Score",
    color = "Age Group",
    title = "Decline in Financial Literacy Score \n for Older vs Younger Groups (2012 vs 2018)"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17)) +
  theme_minimal(base_size = 11)


# SECOND VISUAL: Demographics Table


# Recoding variables where needed

recode_race <- function(x) {
  case_when(
    x == 1 ~ "White/Caucasian",
    x == 2 ~ "Black/African American",
    x %in% c(3, 4, 5) ~ "Asian/Pacific Islander/Native American/Other",
    TRUE ~ NA_character_
  )
}

alp_fb12   <- alp_fb12   %>% mutate(race_clean = recode_race(ethnicity))
alp_dropped <- alp_dropped %>% mutate(race_clean = recode_race(ethnicity))
alp_remained <- alp_remained %>% mutate(race_clean = recode_race(ethnicity))
alp_declined_3q <- alp_declined_3q %>% mutate(race_clean_12 = recode_race(ethnicity_12))
alp_improved_unchanged_3q <- alp_improved_unchanged_3q %>% mutate(race_clean_12 = recode_race(ethnicity_12))

# --- Highest Ed: combine Bachelor's + Graduate into one category
recode_educ <- function(x) {
  case_when(
    x %in% c("Bachelor's Degree", "Graduate Level Degree") ~ "Bachelor's Degree or Higher",
    TRUE ~ x
  )
}

alp_fb12   <- alp_fb12   %>% mutate(highesteducation_combined = recode_educ(highesteducation_clean))
alp_dropped <- alp_dropped %>% mutate(highesteducation_combined = recode_educ(highesteducation_clean))
alp_remained <- alp_remained %>% mutate(highesteducation_combined = recode_educ(highesteducation_clean))
alp_declined_3q <- alp_declined_3q %>% mutate(highesteducation_combined_12 = recode_educ(highesteducation_clean_12))
alp_improved_unchanged_3q <- alp_improved_unchanged_3q %>% mutate(highesteducation_combined_12 = recode_educ(highesteducation_clean_12))

# Again a list of variables for the table
vars2 <- list(
  list(var = "gender",
       label = "Gender",
       levels = c("1" = "Male", "2" = "Female"),
       omit = "Male"),
  
  list(var = "race_clean",
       label = "Race",
       levels = c("White/Caucasian" = "White/Caucasian",
                  "Black/African American" = "Black/African American",
                  "Asian/Pacific Islander/Native American/Other" = "Asian/Pacific Islander/Native American/Other"),
       omit = "White/Caucasian"),
  
  list(var = "highesteducation_combined",
       label = "Highest Ed",
       levels = c("Less than High School" = "Less than High School",
                  "High School Diploma" = "High School Diploma",
                  "Some College - No Bachelor's" = "Some College - No Bachelor's",
                  "Bachelor's Degree or Higher" = "Bachelor's Degree or Higher"),
       omit = "High School Diploma"),
  
  list(var = "familyincome_clean",
       label = "Family Income",
       levels = c("Less than 30,000" = "Less than 30,000",
                  "30,000 - 50,000" = "30,000 - 50,000",
                  "50,000 - 75,000" = "50,000 - 75,000",
                  "75,000+" = "75,000+"),
       omit = "Less than 30,000"),
  
  list(var = "age_group",
       label = "Age Group",
       levels = c("Under 50" = "Under 50",
                  "50-64" = "50-64",
                  "65-74" = "65-74",
                  "75+" = "75+"),
       omit = "Under 50"),
  
  list(var = "a6_clean",
       label = "Marital Status",
       levels = c("Married" = "Married",
                  "Single" = "Single",
                  "Divorced/Separated" = "Divorced/Separated",
                  "Widowed/Widower" = "Widowed/Widower"),
       omit = "Married")
)

cat_summary2 <- lapply(vars2, function(item) {
  var <- item$var
  levels <- item$levels
  omit <- item$omit
  
  col_total <- resolve_col(alp_fb12, var)
  res_total <- get_counts(alp_fb12, col_total, levels)
  
  col_drop <- resolve_col(alp_dropped, var)
  res_drop <- get_counts(alp_dropped, col_drop, levels)
  
  col_decl <- resolve_col(alp_declined_3q, var, suffix = "_12")
  res_decl <- get_counts(alp_declined_3q, col_decl, levels)
  
  col_imp <- resolve_col(alp_improved_unchanged_3q, var, suffix = "_12")
  res_imp <- get_counts(alp_improved_unchanged_3q, col_imp, levels)
  
  # group label with the omitted (reference) category noted
  group_label <- paste0(
    item$label, " (ref: ", omit, ")",
    "  \u2014 n: Dropped = ", res_drop$total,
    ", Declined = ", res_decl$total,
    ", Improved/Unchanged = ", res_imp$total
  )
  
  df <- data.frame(
    Variable_Description = group_label,
    Category = levels[names(levels)],
    Dropped_Pct = paste0(round(100 * res_drop$counts / res_drop$total, 1), "%"),
    Declined_Pct = paste0(round(100 * res_decl$counts / res_decl$total, 1), "%"),
    ImprovedUnchanged_Pct = paste0(round(100 * res_imp$counts / res_imp$total, 1), "%"),
    stringsAsFactors = FALSE
  )
  
  # drop the omitted category's row from what's displayed
  df %>% filter(Category != omit)
}) %>% bind_rows()

write_xlsx(cat_summary2, "Demographic Summary Statistics.xlsx")

# Building the table with the gt library

library(gt)
library(gtExtras)

cat_summary2 %>%
  gt(groupname_col = "Variable_Description") %>%
  tab_header(
    title = md("Characteristics by group based on <br> financial literacy score changes (2012 vs. 2018)")
  ) %>%
  tab_spanner(
    label = "Group Based on Financial Literacy Score Results (2012 vs 2018)",
    columns = c(Dropped_Pct, Declined_Pct, ImprovedUnchanged_Pct)
  ) %>%
  cols_label(
    Category = "Category",
    Dropped_Pct = "In 2012 only (%)",
    Declined_Pct = "Declined (%)",
    ImprovedUnchanged_Pct = "Improved/Unchanged (%)"
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold"), cell_fill(color = "#f0f0f0")),
    locations = cells_row_groups()
  ) %>%
  gt_theme_pff() %>%
  cols_width(
    Dropped_Pct ~ px(100),
    Declined_Pct ~ px(100)
  )


# THIRD VISUAL: BAR PLOTS

# alp_dropped is already only form 2012 
# alp_declined_3q is from merged so use vars with suffix _12
# alp_improved_unchanged_3q is also from merged so use vars with suffix _12

alp_dropped_old <- alp_dropped %>% filter(calcage > 50)
alp_declined_3q_old <- alp_declined_3q %>% filter(calcage_12 > 50)
alp_improved_unchanged_3q_old <- alp_improved_unchanged_3q %>% filter(calcage_12 > 50)

bar_list <- tibble::tribble(
  ~base_var,     ~yes_vals,                 ~label,
  "j11_clean",   list("Yes"),               "Checked Credit Report (Past Year)",
  "j12_clean",   list("Yes"),               "Checked Credit Score (Past Year)",
  "f2_1_clean",  list("Yes"),               "Always Paid Credit Card in Full (Past Year)",
  "f2_4_clean",  list("Yes"),               "Charged Credit Card Late Fee (Past Year)",
  "f2_5_clean",  list("Yes"),               "Charged Credit Card Over-Limit Fee (Past Year)",
  "g5_1_bin",    list("One or more time"),  "Taken an Auto Title Loan (Past Year)",
  "g5_2_bin",    list("One or more time"),  "Taken a Payday Loan (Past Year)",
  "g5_3_bin",    list("One or more time"),  "Gotten a Tax Refund Advance (Past Year)",
  "g5_4_bin",    list("One or more time"),  "Used a Pawn Shop (Past Year)",
  "b4_comb",     list("Yes"),               "Overdraws Checking Occasionally",
  "c10_comb",    list("Yes"),               "Taken a Retirement Loan (Past Year)",
  "c11_comb",    list("Yes"),               "Taken a Hardship Withdrawal (Past Year)"
)

prop_yes <- function(data, col, yes_vals) {
  if (!col %in% names(data)) {
    warning(paste0("Column '", col, "' not found in dataset — returning NA"))
    return(tibble::tibble(proportion = NA_real_, n = NA_integer_))
  }
  x <- data[[col]]
  tibble::tibble(
    proportion = mean(x %in% unlist(yes_vals), na.rm = FALSE),
    n = length(x)
  )
}

plot_data <- bar_list %>%
  mutate(
    col_dropped  = base_var,
    col_merged   = paste0(base_var, "_12"),
    res_dropped  = map2(col_dropped, yes_vals, ~ prop_yes(alp_dropped, .x, .y)),
    res_declined = map2(col_merged,  yes_vals, ~ prop_yes(alp_declined_3q, .x, .y)),
    res_improved = map2(col_merged,  yes_vals, ~ prop_yes(alp_improved_unchanged_3q, .x, .y)),
    prop_dropped  = map_dbl(res_dropped, "proportion"),
    n_dropped     = map_int(res_dropped, "n"),
    prop_declined = map_dbl(res_declined, "proportion"),
    n_declined    = map_int(res_declined, "n"),
    prop_improved = map_dbl(res_improved, "proportion"),
    n_improved    = map_int(res_improved, "n")
  ) %>%
  select(base_var, label, starts_with("prop_"), starts_with("n_"))

# reshape to long format for plotting
plot_data_long <- plot_data %>%
  pivot_longer(
    cols = c(prop_dropped, prop_declined, prop_improved),
    names_to = "group", values_to = "proportion"
  ) %>%
  mutate(group_suffix = sub("prop_", "", group)) %>%
  rowwise() %>%
  mutate(n = get(paste0("n_", group_suffix))) %>%
  ungroup() %>%
  mutate(
    se = sqrt(proportion * (1 - proportion) / n),
    ci_lower = pmax(0, proportion - 1.96 * se),
    ci_upper = pmin(1, proportion + 1.96 * se),
    group = recode(group,
                   prop_dropped  = "In 2012 Only",
                   prop_declined = "Score Declined",
                   prop_improved = "Score Improved/Unchanged"),
    group = factor(group, levels = c("In 2012 Only",
                                     "Score Declined",
                                     "Score Improved/Unchanged"))
  ) %>%
  select(-starts_with("n_"))


target_vars <- c(
  "Always Paid Credit Card in Full (Past Year)",
  "Charged Credit Card Late Fee (Past Year)",
  "Taken a Payday Loan (Past Year)",
  "Gotten a Tax Refund Advance (Past Year)"
)


combined_df <- plot_data_long %>%
  filter(label %in% target_vars) %>%
  mutate(label = factor(label, levels = target_vars))


dodge_width <- 0.75

combined_chart <- ggplot(combined_df, aes(x = label, y = proportion, fill = group)) +
  geom_col(position = position_dodge(width = dodge_width),
           width = 0.65, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = dodge_width),
                width = 0.2, linewidth = 0.4) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, min(1, max(combined_df$ci_upper, na.rm = TRUE) * 1.15)),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(values = c(
    "In 2012 Only" = "#a6bddb",
    "Score Declined" = "#3182bd",
    "Score Improved/Unchanged" = "#08519c"
  )) +
  scale_x_discrete(labels = function(x) gsub(" \\(Past Year\\)", "\n(Past Year)", x)) +
  labs(x = "Financial Behavior", y = "Proportion of Respondents", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 10)
  )

combined_chart


# Creating Datasets for Regression

reg_data <- bind_rows(
  alp_remained %>% mutate(year = 2012),
  alp_fb18 %>% mutate(year = 2018)
)

reg_data <- reg_data %>%
  mutate(
    j11_clean = dplyr::case_when(j11_clean %in% c("No", "Missing") ~ 0,
                                 j11_clean == "Yes" ~ 1,
                                 TRUE ~ NA_real_),

    j12_clean = dplyr::case_when(j12_clean %in% c("No", "Missing") ~ 0,
                                 j12_clean == "Yes" ~ 1,
                                 TRUE ~ NA_real_),

    f2_1_clean = dplyr::case_when(f2_1_clean %in% c("No", "Missing") ~ 0,
                                  f2_1_clean == "Yes" ~ 1,
                                  TRUE ~ NA_real_),

    across(c(f2_4_clean, f2_5_clean,
                  g5_1_bin, g5_2_bin, g5_3_bin, g5_4_bin,
                  b4_comb, c10_comb, c11_comb),
                ~ case_when(. %in% c("Yes", "One or more time") ~ 1,
                            . %in% c("No", "Never") ~ 0,
                            TRUE ~ NA_real_))
    )

# ---------------------------------------------------------------------
# Regression Analysis 1: Using fixed effects and with NAs counted as NA
# ---------------------------------------------------------------------

library(fixest)

# j11_reg <- feols(j11_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# j12_reg <- feols(j12_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# 
# f2_1_reg <- feols(f2_1_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# f2_4_reg <- feols(f2_4_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# f2_5_reg <- feols(f2_5_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# 
# g5_1_reg <- feols(g5_1_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# g5_2_req <- feols(g5_2_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# g5_3_reg <- feols(g5_3_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# g5_4_reg <- feols(g5_4_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# 
# b4_reg <- feols(b4_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# 
# c10_reg <- feols(c10_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)
# c11_reg <- feols(c11_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data, cluster = ~ prim_key)

# --------------------------------------------------------------------
# Regression Analysis 2: Using Fixed Effects but with NAs counted as 0
# --------------------------------------------------------------------

# reg_data2 <- reg_data %>%
#   mutate(
#     j11_clean = dplyr::case_when(j11_clean %in% c("No", "Missing") ~ 0,
#                                  is.na(j11_clean) ~ 0,
#                                  j11_clean == "Yes" ~ 1),
#     
#     j12_clean = dplyr::case_when(j12_clean %in% c("No", "Missing") ~ 0,
#                                  is.na(j12_clean) ~ 0,
#                                  j12_clean == "Yes" ~ 1),
#     
#     f2_1_clean = dplyr::case_when(f2_1_clean %in% c("No", "Missing") ~ 0,
#                                   is.na(f2_1_clean) ~ 0,
#                                   f2_1_clean == "Yes" ~ 1),
#     
#     across(c(f2_4_clean, f2_5_clean,
#              g5_1_bin, g5_2_bin, g5_3_bin, g5_4_bin,
#              b4_comb, c10_comb, c11_comb),
#            ~ case_when(. %in% c("Yes", "One or more time") ~ 1,
#                        . %in% c("No", "Never", "Missing") ~ 0,
#                        is.na(.) ~ 0,
#                        TRUE ~ NA_real_))
#   )
# 
# j11_reg <- feols(j11_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# j12_reg <- feols(j12_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# 
# f2_1_reg <- feols(f2_1_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# f2_4_reg <- feols(f2_4_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# f2_5_reg <- feols(f2_5_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# 
# g5_1_reg <- feols(g5_1_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# g5_2_req <- feols(g5_2_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# g5_3_reg <- feols(g5_3_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# g5_4_reg <- feols(g5_4_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# 
# b4_reg <- feols(b4_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# 
# c10_reg <- feols(c10_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)
# c11_reg <- feols(c11_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean) | prim_key, data = reg_data2, cluster = ~ prim_key)

# --------------------------------------------------
# Regression Analysis 3: No individual fixed-effects
# --------------------------------------------------

# j11_reg <- feols(j11_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                  + factor(familyincome_clean),
#                  data = reg_data, cluster = ~ prim_key)
#
# j12_reg <- feols(j12_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                  + factor(familyincome_clean),
#                  data = reg_data, cluster = ~ prim_key)
#
# f2_1_reg <- feols(f2_1_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                   + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                   + factor(familyincome_clean),
#                   data = reg_data, cluster = ~ prim_key)
#
# f2_4_reg <- feols(f2_4_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                   + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                   + factor(familyincome_clean),
#                   data = reg_data, cluster = ~ prim_key)
#
# f2_5_reg <- feols(f2_5_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                   + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                   + factor(familyincome_clean),
#                   data = reg_data, cluster = ~ prim_key)
#
# g5_1_reg <- feols(g5_1_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                   + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                   + factor(familyincome_clean),
#                   data = reg_data, cluster = ~ prim_key)
#
# g5_2_req <- feols(g5_2_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                   + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                   + factor(familyincome_clean),
#                   data = reg_data, cluster = ~ prim_key)
#
# g5_3_reg <- feols(g5_3_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                   + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                   + factor(familyincome_clean),
#                   data = reg_data, cluster = ~ prim_key)
#
# g5_4_reg <- feols(g5_4_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                   + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                   + factor(familyincome_clean),
#                   data = reg_data, cluster = ~ prim_key)
#
# b4_reg <- feols(b4_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                 + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                 + factor(familyincome_clean),
#                 data = reg_data, cluster = ~ prim_key)
#
# c10_reg <- feols(c10_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                  + factor(familyincome_clean),
#                  data = reg_data, cluster = ~ prim_key)
#
# c11_reg <- feols(c11_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
#                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
#                  + factor(familyincome_clean),
#                  data = reg_data, cluster = ~ prim_key)

# -------------------------------------------------------------------------------------
# Regression Analysis 4: Same as part one but using data subsetted for older group only
# -------------------------------------------------------------------------------------

reg_data_old1 <- reg_data %>% filter(calcage >= 50)

j11_reg <- feols(j11_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                 + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                 + factor(familyincome_clean),
                 data = reg_data_old1, cluster = ~ prim_key)

j12_reg <- feols(j12_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                 + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                 + factor(familyincome_clean),
                 data = reg_data_old1, cluster = ~ prim_key)

f2_1_reg <- feols(f2_1_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                  + factor(familyincome_clean),
                  data = reg_data_old1, cluster = ~ prim_key)

f2_4_reg <- feols(f2_4_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                  + factor(familyincome_clean),
                  data = reg_data_old1, cluster = ~ prim_key)

f2_5_reg <- feols(f2_5_clean ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                  + factor(familyincome_clean),
                  data = reg_data_old1, cluster = ~ prim_key)

g5_1_reg <- feols(g5_1_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                  + factor(familyincome_clean),
                  data = reg_data_old1, cluster = ~ prim_key)

g5_2_req <- feols(g5_2_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                  + factor(familyincome_clean),
                  data = reg_data_old1, cluster = ~ prim_key)

g5_3_reg <- feols(g5_3_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                  + factor(familyincome_clean),
                  data = reg_data_old1, cluster = ~ prim_key)

g5_4_reg <- feols(g5_4_bin ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                  + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                  + factor(familyincome_clean),
                  data = reg_data_old1, cluster = ~ prim_key)

b4_reg <- feols(b4_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                + factor(familyincome_clean),
                data = reg_data_old1, cluster = ~ prim_key)

c10_reg <- feols(c10_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                 + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                 + factor(familyincome_clean),
                 data = reg_data_old1, cluster = ~ prim_key)

c11_reg <- feols(c11_comb ~ three_qs + factor(year) + calcage + factor(a9_clean) + factor(a6_clean)
                 + factor(gender) + factor(ethnicity) + factor(highesteducation_clean)
                 + factor(familyincome_clean),
                 data = reg_data_old1, cluster = ~ prim_key)

# ----------------------------------#
# Building Regression Summary Table #
# ----------------------------------#

library(modelsummary)

models <- list(
  "Checked Credit Report" = j11_reg,
  "Checked Credit Score" = j12_reg,
  "Paid CC in Full" = f2_1_reg,
  "CC Late Fee" = f2_4_reg,
  "CC Over-Limit Fee" = f2_5_reg,
  "Auto Title Loan" = g5_1_reg,
  "Payday Loan" = g5_2_req,
  "Tax Refund Advance" = g5_3_reg,
  "Pawn Shop" = g5_4_reg,
  "Overdraws Checking" = b4_reg,
  "Retirement Loan" = c10_reg,
  "Hardship Withdrawal" = c11_reg
)

coef_map <- c("three_qs" = "3-Question Financial Literacy Score")

mean_dep_var <- sapply(models, function(m) round(mean(fitted(m)), 4))
mean_dep_row <- data.frame(term = "Mean Dep. Var.", t(mean_dep_var), check.names = FALSE)

modelsummary(
  models,
  coef_map = coef_map,
  statistic = "conf.int",
  conf_level = 0.95,
  stars = c('*' = .1, '**' = .05, '***' = .01, '****' = .001),
  gof_map = c("nobs"),
  add_rows = mean_dep_row,
  title = "Effect of 3-Question Financial Literacy Score on Financial Behaviors for Older Group",
  notes = "Each observation is a person-wave. All regressions control for year, age, gender, ethnicity, education, family income, work status, and marital status. Standard errors clustered by person. 95% confidence intervals in brackets.",
  output = "gt"
)


