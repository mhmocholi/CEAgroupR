test_that("define_cea_parameters supports deterministic and probabilistic specifications", {

  parameters <- define_cea_parameters(

    gp_visit = list(
      type = "cost",
      resource = "gp_visits",
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
      probabilistic = list(
        distribution = "gamma",
        shape = 25,
        rate = 25 / 650
      )
    ),

    specialist_visit = list(
      type = "cost",
      resource = "specialist_visits",
      probabilistic = list(
        distribution = "lognormal",
        meanlog = log(120),
        sdlog = 0.20
      )
    ),

    qaly_general = list(
      type = "effect",
      comparison = "exercise_A",
      probabilistic = list(
        distribution = "uniform",
        min = -0.20,
        max = 0.20
      )
    )
  )

  expect_s3_class(parameters, "cea_parameters")

  expect_named(
    parameters,
    c("summary", "parameters")
  )

  expect_named(
    parameters$parameters,
    c(
      "gp_visit",
      "adherence",
      "hospital_day",
      "specialist_visit",
      "qaly_general"
    )
  )

  expect_equal(
    parameters$parameters$gp_visit$deterministic$base,
    65
  )

  expect_equal(
    vapply(
      parameters$parameters,
      function(p) p$probabilistic$distribution,
      character(1)
    ),
    c(
      gp_visit = "normal",
      adherence = "beta",
      hospital_day = "gamma",
      specialist_visit = "lognormal",
      qaly_general = "uniform"
    )
  )

})
