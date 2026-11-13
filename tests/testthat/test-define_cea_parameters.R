test_that(
  "define_cea_parameters supports deterministic and probabilistic specifications",
  {

    parameters <- define_cea_parameters(

      gp_visit = list(
        type = "cost",
        resource = "gp_visits",
        base = 65,
        deterministic = list(
          base = 65,
          lower = 50,
          upper = 80
        ),
        probabilistic = list(
          distribution = "normal",
          mean = 65,
          sd = 10
        )
      ),

      adherence = list(
        type = "effect",
        probabilistic = list(
          distribution = "beta",
          shape1 = 8,
          shape2 = 2
        )
      ),

      hospital_day = list(
        type = "cost",
        resource = "hospital_days",
        base = 650,
        probabilistic = list(
          distribution = "gamma",
          shape = 25,
          rate = 25 / 650
        )
      ),

      specialist_visit = list(
        type = "cost",
        resource = "specialist_visits",
        base = 120,
        probabilistic = list(
          distribution = "lognormal",
          meanlog = log(120),
          sdlog = 0.2
        )
      ),

      qaly_general = list(
        type = "effect",
        comparison = "exercise_A",
        probabilistic = list(
          distribution = "uniform",
          min = -0.2,
          max = 0.2
        )
      )
    )

    # Verify object structure
    expect_s3_class(parameters, "cea_parameters")

    expect_true("summary" %in% names(parameters))
    expect_true("parameters" %in% names(parameters))

    # Verify all parameters are preserved
    expect_equal(
      names(parameters$parameters),
      c(
        "gp_visit",
        "adherence",
        "hospital_day",
        "specialist_visit",
        "qaly_general"
      )
    )

    # Verify baseline values
    expect_equal(parameters$parameters$gp_visit$base, 65)
    expect_equal(parameters$parameters$hospital_day$base, 650)
    expect_equal(parameters$parameters$specialist_visit$base, 120)

    # Verify probabilistic distributions
    expect_equal(
      parameters$parameters$gp_visit$probabilistic$distribution,
      "normal"
    )

    expect_equal(
      parameters$parameters$adherence$probabilistic$distribution,
      "beta"
    )

    expect_equal(
      parameters$parameters$hospital_day$probabilistic$distribution,
      "gamma"
    )

    expect_equal(
      parameters$parameters$specialist_visit$probabilistic$distribution,
      "lognormal"
    )

    expect_equal(
      parameters$parameters$qaly_general$probabilistic$distribution,
      "uniform"
    )
  }
)


test_that(
  "probabilistic cost parameters require a baseline value",
  {

    # Missing baseline must produce an error
    expect_error(
      define_cea_parameters(
        gp_visit = list(
          type = "cost",
          resource = "gp_visits",
          probabilistic = list(
            distribution = "gamma",
            shape = 25,
            rate = 25 / 65
          )
        )
      ),
      "base.*required"
    )

    # Baseline provided: valid specification
    expect_no_error(
      define_cea_parameters(
        gp_visit = list(
          type = "cost",
          resource = "gp_visits",
          base = 65,
          probabilistic = list(
            distribution = "gamma",
            shape = 25,
            rate = 25 / 65
          )
        )
      )
    )

    # Effect parameters do not require a baseline
    expect_no_error(
      define_cea_parameters(
        qaly_general = list(
          type = "effect",
          probabilistic = list(
            distribution = "normal",
            mean = 0,
            sd = 0.10
          )
        )
      )
    )

    # Deterministic-only parameters remain compatible
    expect_no_error(
      define_cea_parameters(
        gp_visit = list(
          type = "cost",
          resource = "gp_visits",
          deterministic = list(
            base = 65,
            lower = 50,
            upper = 80
          )
        )
      )
    )
  }
)
