utils::globalVariables(c(
  # Existing global variables
  ".data",
  "Delta_Cost", "Delta_Effect", "EVPI", "prob", "lambda",
  "dataset", "subgroup_var", "subgroup_level", "comparison",
  "group_uid", "x", "y", "label", "z",

  # General analysis and scenario variables
  "computed_by",
  "parameter", "parameter_value",
  "parameter_1", "parameter_2",
  "scenario", "scenario_1", "scenario_2",
  "pair_uid",

  # Sensitivity analysis values
  "value_1", "value_2", "plot_value",
  "value_label", "lower_value", "upper_value",

  # Plot aesthetics
  "mean_colour", "point_shape",
  "contour_group",

  # Plot boundaries
  "xmin", "xmax", "ymin", "ymax",
  ".xmin", ".xmax", ".ymin", ".ymax"
))
