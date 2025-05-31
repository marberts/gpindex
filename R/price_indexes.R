#' Factory to make Pythagorean indexes
#' @noRd
pythagorean_index <- function(r) {
  types <- switch(r + 2,
    c("Coggeshall", "Laspeyres", "Paasche", "Young"),
    c(
      "Jevons", "Laspeyres", "Paasche",
      "Tornqvist", "Vartia1", "MontgomeryVartia",
      "Vartia2", "SatoVartia", "Walsh2",
      "Young", "Theil", "Rao"
    ),
    c(
      "Carli", "Dutot", "Laspeyres",
      "Palgrave", "Drobisch", "Unnamed",
      "Walsh1", "MarshallEdgeworth", "GearyKhamis",
      "Lowe", "Young", "HybridCSWD"
    )
  )
  gen_mean <- generalized_mean(r)

  function(type) {
    type <- match.arg(type, types)
    weights <- index_weights(type)

    switch(type,
      Carli = ,
      Dutot = ,
      Jevons = ,
      Coggeshall = function(p1, p0, na.rm = FALSE) {
        check_pqs(p1, p0)
        gen_mean(p1 / p0, weights(p0), na.rm)
      },
      Laspeyres = function(p1, p0, q0, na.rm = FALSE) {
        check_pqs(p1, p0, q0)
        gen_mean(p1 / p0, weights(p0, q0), na.rm)
      },
      Paasche = ,
      Palgrave = function(p1, p0, q1, na.rm = FALSE) {
        check_pqs(p1, p0, q1)
        gen_mean(p1 / p0, weights(p1, q1), na.rm)
      },
      Drobisch = ,
      Unnamed = ,
      Vartia2 = ,
      SatoVartia = ,
      Walsh2 = ,
      Tornqvist = ,
      Theil = ,
      Rao = function(p1, p0, q1, q0, na.rm = FALSE) {
        check_pqs(p1, p0, q1, q0)
        gen_mean(p1 / p0, weights(p1, p0, q1, q0), na.rm)
      },
      Vartia1 = ,
      MontgomeryVartia = function(p1, p0, q1, q0, na.rm = FALSE) {
        check_pqs(p1, p0, q1, q0)
        exp(sum(log(p1 / p0) * weights(p1, p0, q1, q0), na.rm = na.rm))
      },
      Walsh1 = ,
      MarshallEdgeworth = ,
      GearyKhamis = function(p1, p0, q1, q0, na.rm = FALSE) {
        check_pqs(p1, p0, q1, q0)
        gen_mean(p1 / p0, weights(p0, q1, q0), na.rm)
      },
      Lowe = function(p1, p0, qb, na.rm = FALSE) {
        check_pqs(p1, p0, qb)
        gen_mean(p1 / p0, weights(p0, qb), na.rm)
      },
      Young = function(p1, p0, pb, qb, na.rm = FALSE) {
        check_pqs(p1, p0, pb, qb)
        gen_mean(p1 / p0, weights(pb, qb), na.rm)
      },
      HybridCSWD = function(p1, p0, na.rm = FALSE) {
        check_pqs(p1, p0)
        gen_mean(p1 / p0, weights(p1, p0), na.rm)
      }
    )
  }
}

#' Index weights
#'
#' Calculate weights for a variety of different price indexes.
#'
#' The `index_weights()` function returns a function to calculate weights
#' for a variety of price indexes. Weights for the following types of indexes
#' can be calculated.
#' - Carli / Jevons / Coggeshall
#' - Dutot
#' - Laspeyres / Lloyd-Moulton
#' - Hybrid Laspeyres (for use in a harmonic mean)
#' - Paasche / Palgrave
#' - Hybrid Paasche (for use in an arithmetic mean)
#' - Törnqvist / Unnamed
#' - Drobisch
#' - Walsh-I (for an arithmetic Walsh index)
#' - Walsh-II (for a geometric Walsh index)
#' - Marshall-Edgeworth
#' - Geary-Khamis
#' - Montgomery-Vartia / Vartia-I
#' - Sato-Vartia / Vartia-II
#' - Theil
#' - Rao
#' - Lowe
#' - Young
#' - Hybrid-CSWD
#'
#' The weights need not sum to 1, as this normalization isn't always
#' appropriate (i.e., for the Vartia-I weights).
#'
#' @param type The name of the index. See details for the possible types of
#' indexes.
#'
#' @returns
#' A function of current and base period prices/quantities that calculates
#' the relevant weights.
#'
#' @note
#' Naming for the indexes and weights generally follows the CPI manual (2020),
#' Balk (2008), von der Lippe (2007), and Selvanathan and Rao (1994). In several
#' cases two or more names correspond to the same weights (e.g., Paasche and
#' Palgrave, or Sato-Vartia and Vartia-II). The calculations are given in the
#' examples.
#'
#' @seealso
#' [update_weights()] for price-updating weights.
#'
#' [quantity_index()] to remap the arguments in these functions for a
#' quantity index.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020).
#' *Consumer Price Index Manual: Concepts and Methods*.
#' International Monetary Fund.
#'
#' von der Lippe, P. (2007). *Index Theory and Price Statistics*. Peter Lang.
#'
#' Selvanathan, E. A. and Rao, D. S. P. (1994).
#' *Index Numbers: A Stochastic Approach*. MacMillan.
#'
#' @examples
#' p1 <- price6[[2]]
#' p2 <- price6[[3]]
#' q1 <- quantity6[[2]]
#' q2 <- quantity6[[3]]
#' pb <- price6[[1]]
#' qb <- quantity6[[1]]
#'
#' # Explicit calculation for each of the different weights
#' # Carli/Jevons/Coggeshall
#'
#' all.equal(index_weights("Carli")(p2), rep(1, length(p1)))
#'
#' # Dutot
#'
#' all.equal(index_weights("Dutot")(p1), p1)
#'
#' # Laspeyres / Lloyd-Moulton
#'
#' all.equal(index_weights("Laspeyres")(p1, q1), p1 * q1)
#'
#' # Hybrid Laspeyres
#'
#' all.equal(index_weights("HybridLaspeyres")(p2, q1), p2 * q1)
#'
#' # Paasche / Palgrave
#'
#' all.equal(index_weights("Paasche")(p2, q2), p2 * q2)
#'
#' # Hybrid Paasche
#'
#' all.equal(index_weights("HybridPaasche")(p1, q2), p1 * q2)
#'
#' # Tornqvist / Unnamed
#'
#' all.equal(
#'   index_weights("Tornqvist")(p2, p1, q2, q1),
#'   0.5 * p1 * q1 / sum(p1 * q1) + 0.5 * p2 * q2 / sum(p2 * q2)
#' )
#'
#' # Drobisch
#'
#' all.equal(
#'   index_weights("Drobisch")(p2, p1, q2, q1),
#'   0.5 * p1 * q1 / sum(p1 * q1) + 0.5 * p1 * q2 / sum(p1 * q2)
#' )
#'
#' # Walsh-I
#'
#' all.equal(
#'   index_weights("Walsh1")(p1, q2, q1),
#'   p1 * sqrt(q1 * q2)
#' )
#'
#' # Marshall-Edgeworth
#'
#' all.equal(
#'   index_weights("MarshallEdgeworth")(p1, q2, q1),
#'   p1 * (q1 + q2)
#' )
#'
#' # Geary-Khamis
#'
#' all.equal(
#'   index_weights("GearyKhamis")(p1, q2, q1),
#'   p1 / (1 / q1 + 1 / q2)
#' )
#'
#' # Montgomery-Vartia / Vartia-I
#'
#' all.equal(
#'   index_weights("MontgomeryVartia")(p2, p1, q2, q1),
#'   logmean(p1 * q1, p2 * q2) / logmean(sum(p1 * q1), sum(p2 * q2))
#' )
#'
#' # Sato-Vartia / Vartia-II
#'
#' all.equal(
#'   index_weights("SatoVartia")(p2, p1, q2, q1),
#'   logmean(p1 * q1 / sum(p1 * q1), p2 * q2 / sum(p2 * q2))
#' )
#'
#' # Walsh-II
#'
#' all.equal(
#'   index_weights("Walsh2")(p2, p1, q2, q1),
#'   sqrt(p1 * q1 * p2 * q2)
#' )
#'
#' # Theil
#'
#' all.equal(index_weights("Theil")(p2, p1, q2, q1), {
#'   w0 <- scale_weights(p1 * q1)
#'   w1 <- scale_weights(p2 * q2)
#'   (w0 * w1 * (w0 + w1) / 2)^(1 / 3)
#' })
#'
#' # Rao
#'
#' all.equal(index_weights("Rao")(p2, p1, q2, q1), {
#'   w0 <- scale_weights(p1 * q1)
#'   w1 <- scale_weights(p2 * q2)
#'   w0 * w1 / (w0 + w1)
#' })
#'
#' # Lowe
#'
#' all.equal(index_weights("Lowe")(p1, qb), p1 * qb)
#'
#' # Young
#'
#' all.equal(index_weights("Young")(pb, qb), pb * qb)
#'
#' # Hybrid CSWD (to approximate a CSWD index)
#'
#' all.equal(index_weights("HybridCSWD")(p2, p1), sqrt(p1 / p2))
#'
#' @family price index functions
#' @export
index_weights <- function(
    type = c(
      "Carli", "Jevons", "Coggeshall", "Dutot",
      "Laspeyres", "HybridLaspeyres", "LloydMoulton",
      "Palgrave", "Paasche", "HybridPaasche",
      "Drobisch", "Unnamed", "Tornqvist",
      "Walsh1", "Walsh2", "MarshallEdgeworth",
      "GearyKhamis", "Vartia1", "MontgomeryVartia",
      "Vartia2", "SatoVartia", "Theil", "Rao",
      "Lowe", "Young", "HybridCSWD"
    )) {
  switch(match.arg(type),
    Carli = ,
    Jevons = ,
    Coggeshall = function(p0) {
      p0[] <- 1 # keep attributes
      p0
    },
    Dutot = function(p0) p0,
    Young = function(pb, qb) {
      check_pqs(pb, qb)
      pb * qb
    },
    Lowe = function(p0, qb) {
      check_pqs(p0, qb)
      p0 * qb
    },
    LloydMoulton = ,
    Laspeyres = function(p0, q0) {
      check_pqs(p0, q0)
      p0 * q0
    },
    HybridLaspeyres = function(p1, q0) {
      check_pqs(p1, q0)
      p1 * q0
    },
    Palgrave = ,
    Paasche = function(p1, q1) {
      check_pqs(p1, q1)
      p1 * q1
    },
    HybridPaasche = function(p0, q1) {
      check_pqs(p0, q1)
      p0 * q1
    },
    Drobisch = function(p1, p0, q1, q0) {
      check_pqs(p1, p0, q1, q0)
      v0 <- scale_weights(p0 * q0)
      v01 <- scale_weights(p0 * q1)
      (v0 + v01) / 2
    },
    Unnamed = ,
    Tornqvist = function(p1, p0, q1, q0) {
      check_pqs(p1, p0, q1, q0)
      v0 <- scale_weights(p0 * q0)
      v1 <- scale_weights(p1 * q1)
      (v0 + v1) / 2
    },
    Walsh1 = function(p0, q1, q0) {
      check_pqs(p0, q1, q0)
      p0 * sqrt(q0 * q1)
    },
    Walsh2 = function(p1, p0, q1, q0) {
      check_pqs(p1, p0, q1, q0)
      sqrt(p0 * q0 * p1 * q1)
    },
    MarshallEdgeworth = function(p0, q1, q0) {
      check_pqs(p0, q1, q0)
      p0 * (q0 + q1)
    },
    GearyKhamis = function(p0, q1, q0) {
      check_pqs(p0, q1, q0)
      p0 / (1 / q0 + 1 / q1)
    },
    Vartia1 = ,
    MontgomeryVartia = function(p1, p0, q1, q0) {
      check_pqs(p1, p0, q1, q0)
      v0 <- p0 * q0
      v1 <- p1 * q1
      logmean(v0, v1) / logmean(sum(v0, na.rm = TRUE), sum(v1, na.rm = TRUE))
    },
    Vartia2 = ,
    SatoVartia = function(p1, p0, q1, q0) {
      check_pqs(p1, p0, q1, q0)
      v0 <- scale_weights(p0 * q0)
      v1 <- scale_weights(p1 * q1)
      logmean(v0, v1)
    },
    Theil = function(p1, p0, q1, q0) {
      check_pqs(p1, p0, q1, q0)
      w0 <- scale_weights(p0 * q0)
      w1 <- scale_weights(p1 * q1)
      ((w0 + w1) / 2 * w0 * w1)^(1 / 3)
    },
    Rao = function(p1, p0, q1, q0) {
      check_pqs(p1, p0, q1, q0)
      w0 <- scale_weights(p0 * q0)
      w1 <- scale_weights(p1 * q1)
      w0 * w1 / (w0 + w1)
    },
    HybridCSWD = function(p1, p0) {
      check_pqs(p1, p0)
      sqrt(p0 / p1)
    }
  )
}

#' Price indexes
#'
#' Calculate a variety of price indexes using information on prices and
#' quantities at two points in time.
#'
#' The `arithmetic_index()`, `geometric_index()`, and
#' `harmonic_index()` functions return a function to calculate a given
#' type of arithmetic, geometric (logarithmic), and harmonic index. Together,
#' these functions produce functions to calculate the following indexes.
#' - **Arithmetic indexes**
#' - Carli
#' - Dutot
#' - Laspeyres
#' - Palgrave
#' - Unnamed index (arithmetic mean of Laspeyres and Palgrave)
#' - Drobisch (or Sidgwick, arithmetic mean of Laspeyres and Paasche)
#' - Walsh-I (arithmetic Walsh)
#' - Marshall-Edgeworth
#' - Geary-Khamis
#' - Lowe
#' - Young
#' - Hybrid-CSWD
#' - **Geometric indexes**
#' - Jevons
#' - Geometric Laspeyres (or Jöhr)
#' - Geometric Paasche
#' - Geometric Young
#' - Törnqvist (or Törnqvist-Theil)
#' - Montgomery-Vartia / Vartia-I
#' - Sato-Vartia / Vartia-II
#' - Walsh-II (geometric Walsh)
#' - Theil
#' - Rao
#' - **Harmonic indexes**
#' - Coggeshall (equally weighted harmonic index)
#' - Paasche
#' - Harmonic Laspeyres
#' - Harmonic Young
#'
#' Along with the `lm_index()` function to calculate the Lloyd-Moulton
#' index, these are just convenient wrappers for
#' [generalized_mean()] and [index_weights()].
#'
#' The Laspeyres, Paasche, Jevons, Lowe, and Young indexes are among the most
#' common price indexes, and so they get their own functions. The
#' `laspeyres_index()`, `lowe_index()`, and `young_index()`
#' functions correspond to setting the appropriate `type` in
#' `arithmetic_index()`; `paasche_index()` and `jevons_index()`
#' instead come from the `harmonic_index()` and `geometric_index()`
#' functions.
#'
#' In addition to these indexes, there are also functions for calculating a
#' variety of indexes based on nested generalized means. The Fisher index is the
#' geometric mean of the arithmetic Laspeyres and Paasche indexes; the Harmonic
#' Laspeyres Paasche (or Harmonic Paasche Laspeyres) index is the harmonic
#' analog of the Fisher index (8054 on Fisher's list). The
#' Carruthers-Sellwood-Ward-Dalen and Carruthers-Sellwood-Ward-Dalen-Balk
#' indexes are sample analogs of the Fisher
#' index; the Balk-Walsh index is the sample analog of the Walsh index. The AG
#' mean index is the arithmetic or geometric mean of the geometric and
#' arithmetic Laspeyres indexes, weighted by the elasticity of substitution.
#' The `stuvel_index()` function returns a function to calculate a Stuvel
#' index of the given parameters. The Lehr index is an alternative to the
#' Geary-Khamis index, and is the implicit price index for Fisher's index 4153.
#' The Martini index is a Lowe index where the quantities are the weighted
#' geometric average of current and base period quantities.
#'
#' @inherit index_weights note
#'
#' @aliases price_indexes
#' @name price_indexes
#' @inheritParams index_weights
#' @param elasticity The elasticity of substitution for the Lloyd-Moulton and
#' AG mean indexes.
#' @param a,b Parameters for the generalized Stuvel index or Martini index.
#' @param p1 Current-period prices.
#' @param p0 Base-period prices.
#' @param q1 Current-period quantities.
#' @param q0 Base-period quantities.
#' @param pb Period-b prices for the Lowe/Young index.
#' @param qb Period-b quantities for the Lowe/Young index.
#' @param na.rm Should missing values be removed? By default missing values for
#' prices or quantities return a missing value.
#'
#' @returns
#' `arithmetic_index()`, `geometric_index()`, `harmonic_index()`, and
#' `stuvel_index()` each return a function to compute the relevant price
#' indexes; `lm_index()`, `arithmetic_agmean_index()`, and
#' `geometric_agmean_index()` each return a function to calculate the
#' relevant index for a given elasticity of substitution. The others return a
#' numeric value giving the change in price between the base period and current
#' period.
#'
#' @note
#' There are different ways to deal with missing values in a price index,
#' and care should be taken when relying on these functions to remove missing
#' values. Setting `na.rm = TRUE` removes price relatives with missing
#' information, either because of a missing price or a missing weight, while
#' using all available non-missing information to make the weights.
#'
#' Certain properties of an index-number formula may not work as expected when
#' removing missing values if there is ambiguity about how to remove missing
#' values from the weights (as in, e.g., a Törnqvist or Sato-Vartia index). The
#' [balanced()] operator may be helpful, as it balances the removal of missing
#' values across prices and quantities prior to making the weights.
#'
#' @seealso
#' [generalized_mean()] for the generalized mean that powers
#' most of these functions.
#'
#' [contributions()] for calculating percent-change contributions.
#'
#' [quantity_index()] to remap the arguments in these functions for a
#' quantity index.
#'
#' [price6()] for an example of how to use these functions with more
#' than two time periods.
#'
#' The \pkg{piar} package has more functionality working with price indexes for
#' multiple groups of products over many time periods.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' Fisher, I. (1922). *The Making of Index Numbers*. Houghton Mifflin
#' Company.
#'
#' IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020).
#' *Consumer Price Index Manual: Concepts and Methods*.
#' International Monetary Fund.
#'
#' von der Lippe, P. (2007). *Index Theory and Price Statistics*. Peter Lang.
#'
#' Selvanathan, E. A. and Rao, D. S. P. (1994).
#' *Index Numbers: A Stochastic Approach*. MacMillan.
#'
#' @examples
#' p1 <- price6[[2]]
#' p2 <- price6[[3]]
#' q1 <- quantity6[[2]]
#' q2 <- quantity6[[3]]
#'
#' # Most indexes can be calculated by combining the appropriate weights
#' # with the correct type of mean.
#'
#' laspeyres_index(p2, p1, q1)
#' arithmetic_mean(p2 / p1, index_weights("Laspeyres")(p1, q1))
#'
#' geometric_index("Laspeyres")(p2, p1, q1)
#' geometric_mean(p2 / p1, index_weights("Laspeyres")(p1, q1))
#'
#' # NAs get special treatment.
#'
#' p_na <- replace(p1, 6, NA)
#'
#' laspeyres_index(p2, p_na, q1, na.rm = TRUE) # drops the last price relative
#'
#' sum(p2 * q1, na.rm = TRUE) /
#'   sum(p_na * q1, na.rm = TRUE) # drops the last period-0 price
#'
#' # von Bortkiewicz decomposition
#'
#' paasche_index(p2, p1, q2) / laspeyres_index(p2, p1, q1) - 1
#'
#' wl <- scale_weights(index_weights("Laspeyres")(p1, q1))
#' pl <- laspeyres_index(p2, p1, q1)
#' ql <- quantity_index(laspeyres_index)(q2, q1, p1)
#'
#' sum(wl * (p2 / p1 / pl - 1) * (q2 / q1 / ql - 1))
#'
#' # Similar decomposition for geometric Laspeyres/Paasche.
#'
#' wp <- scale_weights(index_weights("Paasche")(p2, q2))
#' gl <- geometric_index("Laspeyres")(p2, p1, q1)
#' gp <- geometric_index("Paasche")(p2, p1, q2)
#'
#' log(gp / gl)
#'
#' sum(scale_weights(wl) * (wp / wl - 1) * log(p2 / p1 / gl))
#'
#' @family price index functions
#' @export
arithmetic_index <- pythagorean_index(1)

#' Geometric indexes
#' @rdname price_indexes
#' @export
geometric_index <- pythagorean_index(0)

#' Harmonic indexes
#' @rdname price_indexes
#' @export
harmonic_index <- pythagorean_index(-1)

#' Laspeyres index
#' @rdname price_indexes
#' @export
laspeyres_index <- arithmetic_index("Laspeyres")

#' Paasche index
#' @rdname price_indexes
#' @export
paasche_index <- harmonic_index("Paasche")

#' Jevons index
#' @rdname price_indexes
#' @export
jevons_index <- geometric_index("Jevons")

#' Lowe index
#' @rdname price_indexes
#' @export
lowe_index <- arithmetic_index("Lowe")

#' Young index
#' @rdname price_indexes
#' @export
young_index <- arithmetic_index("Young")

#' Factory to make nested indexes
#' @noRd
nested_index <- function(r, s) {
  nest_mean <- nested_mean(r, s)

  function(p1, p0, q1, q0, na.rm = FALSE) {
    check_pqs(p1, p0, q1, q0)
    nest_mean(p1 / p0, p0 * q0, p1 * q1, na.rm)
  }
}

#' Fisher index
#' @rdname price_indexes
#' @export
fisher_index <- nested_index(0, c(1, -1))

#' Harmonic Laspeyres-Paasche index
#' @rdname price_indexes
#' @export
hlp_index <- nested_index(-1, c(1, -1))

#' Lloyd-Moulton index
#' @rdname price_indexes
#' @export
lm_index <- function(elasticity) {
  gen_mean <- generalized_mean(1 - elasticity)

  function(p1, p0, q0, na.rm = FALSE) {
    check_pqs(p1, p0, q0)
    gen_mean(p1 / p0, p0 * q0, na.rm)
  }
}

#' Caruthers Sellwood Ward Dalen index
#' @rdname price_indexes
#' @export
cswd_index <- function(p1, p0, na.rm = FALSE) {
  check_pqs(p1, p0)
  fisher_mean(p1 / p0, na.rm = na.rm)
}

#' Caruthers Sellwood Ward Dalen Balk index
#' @rdname price_indexes
#' @export
cswdb_index <- function(p1, p0, q1, q0, na.rm = FALSE) {
  check_pqs(p1, p0, q1, q0)
  sqrt(arithmetic_mean(p1 / p0, na.rm = na.rm) /
    arithmetic_mean(q1 / q0, na.rm = na.rm) *
    arithmetic_mean(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

#' Balk Walsh index
#' @rdname price_indexes
#' @export
bw_index <- function(p1, p0, na.rm = FALSE) {
  check_pqs(p1, p0)
  rel <- sqrt(p1 / p0)
  arithmetic_mean(rel, na.rm = na.rm) * harmonic_mean(rel, na.rm = na.rm)
}

#' Generalized Stuvel index
#' @rdname price_indexes
#' @export
stuvel_index <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (not_finite_scalar(a)) {
    stop("'a' must be a finite length 1 numeric")
  }
  if (not_finite_scalar(b)) {
    stop("'b' must be a finite length 1 numeric")
  }

  function(p1, p0, q1, q0, na.rm = FALSE) {
    check_pqs(p1, p0, q1, q0)
    v0 <- p0 * q0
    v1 <- p1 * q1
    pl <- arithmetic_mean(p1 / p0, v0, na.rm)
    ql <- arithmetic_mean(q1 / q0, v0, na.rm)
    v <- sum(v1, na.rm = na.rm) / sum(v0, na.rm = na.rm)
    (pl - b / a * ql) / 2 + sqrt((pl - b / a * ql)^2 / 4 + b / a * v)
  }
}

#' Factory to make AG mean index
#' @noRd
agmean_index <- function(r) {
  force(r)

  function(elasticity) {
    nest_mean <- nested_mean(r, c(0, 1), c(elasticity, 1 - elasticity))
    function(p1, p0, q0, na.rm = FALSE) {
      check_pqs(p1, p0, q0)
      v0 <- p0 * q0
      nest_mean(p1 / p0, v0, v0, na.rm)
    }
  }
}

#' Arithmetic AG mean index
#' @rdname price_indexes
#' @export
arithmetic_agmean_index <- agmean_index(1)

#' Geometric AG mean index
#' @rdname price_indexes
#' @export
geometric_agmean_index <- agmean_index(0)

#' Lehr index
#' @rdname price_indexes
#' @export
lehr_index <- function(p1, p0, q1, q0, na.rm = FALSE) {
  check_pqs(p1, p0, q1, q0)
  v1 <- p1 * q1
  v0 <- p0 * q0
  v <- (v1 + v0) / (q1 + q0)
  sum(v1, na.rm = na.rm) / sum(v0, na.rm = na.rm) *
    sum(v * q0, na.rm = na.rm) / sum(v * q1, na.rm = na.rm)
}

#' Martini index
#' @rdname price_indexes
#' @export
martini_index <- function(a) {
  if (not_finite_scalar(a)) {
    stop("'a' must be a finite length 1 numeric")
  }
  if (a > 1 || a < 0) {
    stop("'a' must be between 0 and 1")
  }

  function(p1, p0, q1, q0, na.rm = FALSE) {
    check_pqs(p1, p0, q1, q0)
    arithmetic_mean(p1 / p0, p0 * q0 * (q1 / q0)^a, na.rm = na.rm)
  }
}
