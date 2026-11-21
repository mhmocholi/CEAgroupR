
#' Synthetic Dataset for Cost-Utility Analysis
#'
#' @description
#' A synthetic individual-level dataset designed to demonstrate
#' the economic evaluation methods implemented in CEAgroupR.
#'
#' The dataset contains 600 observations representing participants
#' allocated to usual care or exercise intervention groups.
#'
#' It includes demographic characteristics, clinical variables,
#' healthcare resource utilisation, health-related quality of life,
#' and economic outcomes.
#'
#' @format A data frame with 600 rows and 18 variables:
#' \describe{
#'   \item{id}{
#'     Unique participant identifier.
#'   }
#'   \item{group}{
#'     Treatment group or intervention strategy.
#'   }
#'   \item{age}{
#'     Participant age in years.
#'   }
#'   \item{sex}{
#'     Participant sex.
#'   }
#'   \item{location}{
#'     Residential setting (e.g., urban or rural).
#'   }
#'   \item{diabetes}{
#'     Indicator of diabetes (0 = no; 1 = yes).
#'   }
#'   \item{HTA}{
#'     Indicator of arterial hypertension (0 = no; 1 = yes).
#'   }
#'   \item{n_comorbidities}{
#'     Number of comorbidities.
#'   }
#'   \item{gp_visits}{
#'     Number of general practitioner consultations.
#'   }
#'   \item{specialist_visits}{
#'     Number of specialist consultations.
#'   }
#'   \item{hospital_days}{
#'     Number of hospitalisation days.
#'   }
#'   \item{drug_cost}{
#'     Medication costs in euros.
#'   }
#'   \item{utility_baseline}{
#'     Health utility value at baseline.
#'   }
#'   \item{utility_followup}{
#'     Health utility value at follow-up.
#'   }
#'   \item{effect}{
#'     Individual effectiveness outcome, expressed as QALYs.
#'   }
#'   \item{healthcare_cost}{
#'     Total healthcare costs in euros.
#'   }
#'   \item{intervention_cost}{
#'     Intervention-related costs in euros.
#'   }
#'   \item{cost_total}{
#'     Total individual costs in euros.
#'   }
#' }
#'
#' @details
#' This dataset is intended exclusively for demonstration,
#' testing and reproducible examples of economic evaluation.
#'
#' All observations are synthetic and do not represent
#' actual patients or clinical trial participants.
#'
#' The dataset can be used to illustrate incremental
#' cost-effectiveness analysis, cost-utility analysis,
#' subgroup comparisons and sensitivity analyses.
#'
#' @usage data(cua_example)
#'
#' @examples
#' data(cua_example)
#'
#' head(cua_example)
#'
#' table(cua_example$group)
#'
#' @keywords datasets
"cua_example"


#' Economic Parameters for Cost-Utility Analysis
#'
#' @description
#' A synthetic table of economic parameters designed to
#' complement the individual-level dataset cua_example.
#'
#' The table contains unit costs and annual intervention
#' resource costs that can be used in economic evaluation
#' examples and sensitivity analyses.
#'
#' @format A data frame with 7 rows and 3 variables:
#' \describe{
#'   \item{parameter}{
#'     Unique identifier of the economic parameter.
#'   }
#'   \item{value}{
#'     Numerical value assigned to the parameter.
#'   }
#'   \item{unit}{
#'     Measurement unit of the parameter,
#'     such as EUR/visit, EUR/day or EUR/year.
#'   }
#' }
#'
#' @details
#' Parameter values are illustrative and should not be
#' interpreted as official healthcare tariffs, validated
#' unit costs or empirically estimated intervention costs.
#'
#' The parameters include healthcare resource unit costs
#' and annual costs associated with exercise interventions.
#'
#' This dataset is intended for package demonstrations,
#' reproducible examples and methodological testing.
#'
#' @usage data(cua_parameters)
#'
#' @examples
#' data(cua_parameters)
#'
#' cua_parameters
#'
#' @keywords datasets
"cua_parameters"
