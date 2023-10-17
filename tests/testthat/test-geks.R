dat <- data.frame(
  price = 1:65, quantity = 65:1, period = 1:13, product = rep(1:5, each = 13)
)
dat <- dat[c(65:60, 3:59, 1:2), ]
dat2 <- dat[-c(2:3, 7, 15, 64), ]

test_that("geks works in corner cases", {
  expect_identical(
    fisher_geks(integer(0), numeric(0), logical(0), character(0)),
    list()
  )
  expect_identical(
    fisher_geks(integer(0), numeric(0), factor(logical(0), 1:5), character(0)),
    list(c("2" = NaN, "3" = NaN, "4" = NaN, "5" = NaN))
  )
  expect_equal(
    fisher_geks(c(1, 5, 4, 2, 3, 6, 1, 2),
                c(1, 1, 2, 2, 1, 2, 3, 1),
                c(1, 1, 1, 1, 2, 2, 2, 2),
                c("a", "b", "c", "d", "a", "b", "c", "d")),
    list(c("2" = fisher_index(c(3, 6, 1, 2),
                              c(1, 5, 4, 2),
                              c(1, 2, 3, 1),
                              c(1, 1, 2, 2))))
  )
  expect_equal(tornqvist_geks(1:2, 1:2, letters[1:2], c(1, 1)),
               list(c(b = 2)))
})

test_that("geks agrees with IndexNumR", {
  expect_equal(
    cumprod(
      as.numeric(
        unlist(with(dat, tornqvist_geks(price, quantity, period, product)))
      )
    ),
    c(1.07334245809641, 1.13758500234551, 1.19943273670278, 1.26078373138759,
      1.32242752173496, 1.38478458426165, 1.44813211799635, 1.51269173915522,
      1.57867050804161, 1.64628410287697, 1.71577307463249, 1.78741712742392)
  )
  test <- with(dat, fisher_geks(price, quantity, period, product, 11))
  expect_equal(
    cumprod(as.numeric(c(test[[1]], test[[2]][10], test[[3]][10]))),
    c(1.05584109330239, 1.11216779883715, 1.16904493358996, 1.22654588965604,
      1.28475480484066, 1.34376919838486, 1.40370325012859, 1.46469196029632,
      1.5268965162016, 1.59051132643364, 1.65525192178965, 1.721202953357)
  )
  expect_equal(
    cumprod(
      as.numeric(
        unlist(with(dat, fisher_geks(price, quantity, period, product, n = 11)))
      )
    ),
    c(1.05374791146818, 1.1079745122232, 1.16274614004633, 1.21813825064384,
      1.27423777574742, 1.33114601874993, 1.38898229585753, 1.44788860470373,
      1.50803571488446, 1.56963124638734, 1.63293056684376)
  )
  expect_equal(
    cumprod(
      as.numeric(
        unlist(with(dat, fisher_geks(price, quantity, period, product, n = 10)))
      )
    ),
    c(1.05146069583139, 1.10343861884982, 1.15600537603593, 1.20924346504472,
      1.26324902214539, 1.31813527765125, 1.37403698640446, 1.43111620765476,
      1.4895699714369, 1.54964061999289)
  )
  expect_equal(
    cumprod(
      as.numeric(
        unlist(with(dat, fisher_geks(price, quantity, period, product, 2)))
      )
    ),
    c(1.05488895039924, 1.11045527234441, 1.1667173418342, 1.2236967401878,
      1.28141852837846, 1.33991159494773, 1.39920908948289, 1.45934895785986,
      1.52037460080832, 1.58233568433829, 1.64528913987317, 1.70930040456311)
  )
  expect_equal(
    cumprod(
      as.numeric(
        unlist(
          with(
            dat,
            geks(arithmetic_index("Walsh1"))(price, quantity, period, product)
          )
        )
      )
    ),
    c(1.0566699129383, 1.11378295838144, 1.17139426903106, 1.22956960896297,
      1.28838882562161, 1.34795095779689, 1.40838221049365, 1.46984928172641,
      1.53258374680655, 1.59693277586978, 1.66348762735518, 1.7335492978583)
  )
  (test <- with(
    dat,
    geks(arithmetic_index("Walsh1"))(price, quantity, period, product, 10, 3)
  ))
  expect_equal(
    cumprod(
      as.numeric(
        unlist(
          as.numeric(c(test[[1]], test[[2]][3], test[[3]][3], test[[4]][3]))
        )
      )
    ),
    c(1.044688300757481, 1.090268005747500, 1.136945278111909,
      1.184556889920806, 1.233276530383314, 1.283484684164579)
  )
  expect_equal(
    cumprod(
      as.numeric(
        unlist(
          with(
            dat2,
            geks(balanced(geometric_index("Tornqvist")))(
              price, quantity, period, product, na.rm = TRUE
            )
          )
        )
      )
    ),
    c(0.97012367078552, 1.07680890198379, 1.07558882705777, 1.12676970820557,
      1.17801945580172, 1.22966120693395, 1.28190412021848, 1.33491131065706,
      1.38883148578389, 1.40614308033419, 1.5157156152315, 1.55767693905705)
  )
  expect_equal(
    as.numeric(
      with(
        dat2,
        geks(balanced(fisher_index))(
          price, quantity, period, product, na.rm = TRUE, n = 1
        )
      )
    ),
    1.02676364331238
  )
  expect_equal(
    cumprod(
      as.numeric(
        unlist(
          with(
            dat2,
            geks(balanced(arithmetic_index("Walsh1")))(
              price, quantity, period, product, 2, na.rm = TRUE
            )
          )
        )
      )
    ),
    c(1.03718005998263, 1.07464654973022, 1.11241262026669, 1.16674135968705,
      1.22177863428673, 1.27755224912525, 1.33409420817545, 1.39144132753221,
      1.44963608201481, 1.49468455394966, 1.53991593521434, 1.60081842092565)
  )
})

test_that("geks works as a quantity index", {
  expect_equal(
    with(
      dat,
      geks(balanced(fisher_index))(
        price, quantity, period, product, na.rm = TRUE
      )
    ),
    with(
      dat,
      quantity_index(geks(balanced(fisher_index)))(
        period, p = quantity, product, na.rm = TRUE, q = price
      )
    )
  )
  expect_equal(
    with(
      dat2,
      geks(balanced(fisher_index))(
        price, quantity, period, product, na.rm = TRUE
      )
    ),
    with(
      dat2,
      geks(balanced(quantity_index(fisher_index)))(
        period, p = quantity, product, na.rm = TRUE, q = price
      )
    )
  )
})

test_that("'n' doesn't change the value in subsequent periods", {
  expect_equal(
    with(
      dat,
      fisher_geks(price, quantity, period, product, na.rm = TRUE, n = 7)[[1]]
    ),
    with(
      dat,
      fisher_geks(price, quantity, period, product, na.rm = TRUE)
    )[[1]][6:12]
  )
  expect_equal(
    with(
      dat2,
      fisher_geks(
        price, quantity, period, product, na.rm = TRUE, n = 7
      )[[1]][6:7]
    ),
    with(
      dat2,
      fisher_geks(price, quantity, period, product, na.rm = TRUE, n = 2)
    )[[1]]
  )
  expect_equal(
    lapply(
      with(
        dat2,
        tornqvist_geks(
          price, quantity, period, product, na.rm = TRUE, window = 9, n = 6
        )
      ), `[`, 6),
    with(
      dat2,
      tornqvist_geks(
        price, quantity, period, product, na.rm = TRUE, window = 9, n = 1
      )
    )
  )
})

test_that("errors work for geks", {
  expect_error(
    with(dat, tornqvist_geks(price, quantity, period[-1], product))
  )
  expect_error(
    with(dat, tornqvist_geks(price, quantity, period, product, n = 0))
  )
  expect_error(
    with(dat, tornqvist_geks(price, quantity, period, product, n = 13))
  )
  expect_error(
    with(dat, tornqvist_geks(price, quantity, period, product, window = 1))
  )
  expect_error(
    with(dat, tornqvist_geks(price, quantity, period, product, window = 14))
  )
})