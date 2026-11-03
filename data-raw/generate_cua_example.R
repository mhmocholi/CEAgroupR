
# ============================================================
# CEAgroupR - Synthetic example data
# ============================================================
# Generates:
#   1. cua_example
#   2. cua_parameters
#
# All data are synthetic and reproducible.
# ============================================================

set.seed(12345)

# ============================================================
# 1. Treatment strategies
# ============================================================

strategies <- c(
  "usual_care",
  "exercise_A",
  "exercise_B"
)

# ============================================================
# 2. Economic parameters
# ============================================================

cua_parameters <- data.frame(
  parameter = c(
    "gp_visit",
    "specialist_visit",
    "hospital_day",
    "trainer_cost_A",
    "trainer_cost_B",
    "facility_cost_A",
    "facility_cost_B"
  ),
  value = c(
    65,
    120,
    650,
    30000,
    8000,
    12000,
    4000
  ),
  unit = c(
    "EUR/visit",
    "EUR/visit",
    "EUR/day",
    "EUR/year",
    "EUR/year",
    "EUR/year",
    "EUR/year"
  )
)

# ============================================================
# 3. Individual participant data
# ============================================================

n_per_group <- 200

cua_example <- data.frame(
  id = seq_len(n_per_group * length(strategies)),
  group = rep(strategies, each = n_per_group)
)

n <- nrow(cua_example)

# ============================================================
# 4. Sociodemographic characteristics
# ============================================================

cua_example$age <- round(
  pmin(pmax(rnorm(n, mean = 70, sd = 8), 55), 90)
)

cua_example$sex <- sample(
  c("Female", "Male"),
  size = n,
  replace = TRUE,
  prob = c(0.55, 0.45)
)

cua_example$location <- sample(
  c("Urban", "Rural"),
  size = n,
  replace = TRUE,
  prob = c(0.65, 0.35)
)

# ============================================================
# 5. Clinical characteristics
# ============================================================

cua_example$diabetes <- rbinom(n, 1, 0.25)

cua_example$HTA <- rbinom(n, 1, 0.45)

cua_example$n_comorbidities <-
  cua_example$diabetes +
  cua_example$HTA +
  rpois(n, lambda = 0.5)

# ============================================================
# 6. Healthcare resource utilization (12 months)
# ============================================================

# Expected utilization according to clinical complexity

lambda_gp <- 2.5 + 0.6 * cua_example$n_comorbidities

lambda_specialist <- 0.8 + 0.3 * cua_example$n_comorbidities

# Primary care visits

cua_example$gp_visits <- rpois(
  n,
  lambda = lambda_gp
)

# Specialist visits

cua_example$specialist_visits <- rpois(
  n,
  lambda = lambda_specialist
)

# Hospitalization days

hospitalized <- rbinom(
  n,
  size = 1,
  prob = pmin(
    0.08 + 0.04 * cua_example$n_comorbidities,
    0.50
  )
)

cua_example$hospital_days <-
  hospitalized * (1 + rpois(n, lambda = 2))

# Annual pharmaceutical expenditure (EUR)

cua_example$drug_cost <- round(
  rgamma(
    n,
    shape = 2,
    scale = 100 + 50 * cua_example$n_comorbidities
  ),
  2
)

# ============================================================
# 7. Health utilities and QALYs (12 months)
# ============================================================

# Baseline utility: influenced by age and comorbidities
cua_example$utility_baseline <- pmin(
  1,
  pmax(
    0,
    0.90 -
      0.002 * (cua_example$age - 55) -
      0.035 * cua_example$n_comorbidities +
      rnorm(n, 0, 0.06)
  )
)

# Expected utility change by strategy
expected_change <- c(
  usual_care = 0.000,
  exercise_A = 0.060,
  exercise_B = 0.035
)

# Utility at 12 months
cua_example$utility_followup <- pmin(
  1,
  pmax(
    0,
    cua_example$utility_baseline +
      unname(expected_change[cua_example$group]) +
      rnorm(n, 0, 0.04)
  )
)

# QALYs using the trapezoidal method
cua_example$effect <- (
  cua_example$utility_baseline +
    cua_example$utility_followup
) / 2


# ============================================================
# 8. Individual costs (12 months)
# ============================================================

# Retrieve unit costs
prices <- setNames(
  cua_parameters$value,
  cua_parameters$parameter
)

# Healthcare costs
cua_example$healthcare_cost <-
  cua_example$gp_visits * prices["gp_visit"] +
  cua_example$specialist_visits * prices["specialist_visit"] +
  cua_example$hospital_days * prices["hospital_day"] +
  cua_example$drug_cost

# Shared intervention costs per participant
intervention_cost <- c(
  usual_care = 0,
  exercise_A = unname(
    (prices["trainer_cost_A"] + prices["facility_cost_A"]) /
      n_per_group
  ),
  exercise_B = unname(
    (prices["trainer_cost_B"] + prices["facility_cost_B"]) /
      n_per_group
  )
)

# Assign intervention costs to each participant
cua_example$intervention_cost <- unname(
  intervention_cost[cua_example$group]
)

# Total individual costs
cua_example$cost_total <-
  cua_example$healthcare_cost +
  cua_example$intervention_cost

# ============================================================
# . Initial validation
# ============================================================

cat("\nParticipants per strategy:\n")
print(table(cua_example$group))

cat("\nMissing values:\n")
print(colSums(is.na(cua_example)))

cat("\nDataset dimensions:\n")
print(dim(cua_example))

# ============================================================
# 9. Save package datasets
# ============================================================

usethis::use_data(
  cua_example,
  cua_parameters,
  overwrite = TRUE
)
