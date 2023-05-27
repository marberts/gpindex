#---- Index weights ----
index_weights <- function(
    type = c("Carli", "Jevons", "Coggeshall", "Dutot",
             "Laspeyres", "HybridLaspeyres", "LloydMoulton",
             "Palgrave", "Paasche", "HybridPaasche",
             "Drobisch", "Unnamed", "Tornqvist",
             "Walsh1", "Walsh2", "MarshallEdgeworth",
             "GearyKhamis", "Vartia1", "MontgomeryVartia",
             "Vartia2", "SatoVartia", "Theil", "Rao",
             "Lowe", "Young")
) {
  switch(
    match.arg(type),
    Carli = ,
    Jevons = ,
    Coggeshall = function(p0) {
      p0[] <- 1 # keep attributes
      p0
    },
    Dutot = function(p0) p0,
    Young = function(pb, qb) pb * qb,
    Lowe = function(p0, qb) p0 * qb,
    LloydMoulton = ,
    Laspeyres = function(p0, q0) p0 * q0,
    HybridLaspeyres = function(p1, q0) p1 * q0,
    Palgrave = ,
    Paasche = function(p1, q1) p1 * q1,
    HybridPaasche = function(p0, q1) p0 * q1,
    Drobisch = function(p1, p0, q1, q0) {
      v0 <- scale_weights(p0 * q0)
      v01 <- scale_weights(p0 * q1)
      (v0 + v01) / 2
    },
    Unnamed = ,
    Tornqvist = function(p1, p0, q1, q0) {
      v0 <- scale_weights(p0 * q0)
      v1 <- scale_weights(p1 * q1)
      (v0 + v1) / 2
    },
    Walsh1 = function(p0, q1, q0) p0 * sqrt(q0 * q1),
    Walsh2 = function(p1, p0, q1, q0) sqrt(p0 * q0 * p1 * q1),
    MarshallEdgeworth = function(p0, q1, q0) p0 * (q0 + q1),
    GearyKhamis = function(p0, q1, q0) p0 / (1 / q0 + 1 / q1),
    Vartia1 = ,
    MontgomeryVartia = function(p1, p0, q1, q0) {
      v0 <- p0 * q0
      v1 <- p1 * q1
      logmean(v0, v1) / logmean(sum(v0, na.rm = TRUE), sum(v1, na.rm = TRUE))
    },
    Vartia2 = ,
    SatoVartia = function(p1, p0, q1, q0) {
      v0 <- scale_weights(p0 * q0)
      v1 <- scale_weights(p1 * q1)
      logmean(v0, v1)
    },
    Theil = function(p1, p0, q1, q0) {
      w0 <- scale_weights(p0 * q0)
      w1 <- scale_weights(p1 * q1)
      ((w0 + w1) / 2 * w0 * w1)^(1 / 3)
    },
    Rao = function(p1, p0, q1, q0) {
      w0 <- scale_weights(p0 * q0)
      w1 <- scale_weights(p1 * q1)
      w0 * w1 / (w0 + w1)
    }
  )
}

#---- Pythagorean indexes ----
pythagorean_index <- function(r) {
  types <- switch(
    r + 2,
    c("Coggeshall", "Laspeyres", "Paasche", "Young"),
    c("Jevons", "Laspeyres", "Paasche",
      "Tornqvist", "Vartia1", "MontgomeryVartia",
      "Vartia2", "SatoVartia", "Walsh2",
      "Young", "Theil", "Rao"),
    c("Carli", "Dutot", "Laspeyres",
      "Palgrave", "Drobisch", "Unnamed",
      "Walsh1", "MarshallEdgeworth", "GearyKhamis",
      "Lowe", "Young")
  )
  gen_mean <- generalized_mean(r)

  function(type) {
    type <- match.arg(type, types)
    weights <- index_weights(type)

    switch(
      type,
      Carli = ,
      Dutot = ,
      Jevons = ,
      Coggeshall = function(p1, p0, na.rm = FALSE) {
        gen_mean(p1 / p0, weights(p0), na.rm)
      },
      Laspeyres = function(p1, p0, q0, na.rm = FALSE) {
        gen_mean(p1 / p0, weights(p0, q0), na.rm)
      },
      Paasche = ,
      Palgrave = function(p1, p0, q1, na.rm = FALSE) {
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
        gen_mean(p1 / p0, weights(p1, p0, q1, q0), na.rm)
      },
      Vartia1 = ,
      MontgomeryVartia = function(p1, p0, q1, q0, na.rm = FALSE) {
        exp(sum(log(p1 / p0) * weights(p1, p0, q1, q0), na.rm = na.rm))
      },
      Walsh1 = ,
      MarshallEdgeworth = ,
      GearyKhamis = function(p1, p0, q1, q0, na.rm = FALSE) {
        gen_mean(p1 / p0, weights(p0, q1, q0), na.rm)
      },
      Lowe = function(p1, p0, qb, na.rm = FALSE) {
        gen_mean(p1 / p0, weights(p0, qb), na.rm)
      },
      Young = function(p1, p0, pb, qb, na.rm = FALSE) {
        gen_mean(p1 / p0, weights(pb, qb), na.rm)
      }
    )
  }
}

arithmetic_index <- pythagorean_index(1)

geometric_index <- pythagorean_index(0)

harmonic_index <- pythagorean_index(-1)

#---- Common indexes ----
laspeyres_index <- arithmetic_index("Laspeyres")

paasche_index <- harmonic_index("Paasche")

jevons_index <- geometric_index("Jevons")

lowe_index <- arithmetic_index("Lowe")

young_index <- arithmetic_index("Young")

#---- Nested indexes ----
nested_index <- function(r, s) {
  nest_mean <- nested_mean(r, s)

  function(p1, p0, q1, q0, na.rm = FALSE) {
    nest_mean(p1 / p0, p0 * q0, p1 * q1, na.rm)
  }
}

fisher_index <- nested_index(0, c(1, -1))

hlp_index <- nested_index(-1, c(1, -1))

#---- Lloyd Moulton index ----
lm_index <- function(elasticity) {
  gen_mean <- generalized_mean(1 - elasticity)

  function(p1, p0, q0, na.rm = FALSE) {
    gen_mean(p1 / p0, p0 * q0, na.rm)
  }
}

#---- Caruthers Sellwood Ward Dalen index ----
cswd_index <- function(p1, p0, na.rm = FALSE) {
  fisher_mean(p1 / p0, na.rm = na.rm)
}

#---- Caruthers Sellwood Ward Dalen Balk index ----
cswdb_index <- function(p1, p0, q1, q0, na.rm = FALSE) {
  sqrt(arithmetic_mean(p1 / p0, na.rm = na.rm) /
         arithmetic_mean(q1 / q0, na.rm = na.rm) *
         arithmetic_mean(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

#---- Balk Walsh index ----
bw_index <- function(p1, p0, na.rm = FALSE) {
  rel <- sqrt(p1 / p0)
  arithmetic_mean(rel, na.rm = na.rm) * harmonic_mean(rel, na.rm = na.rm)
}

#---- Generalized Stuvel index ----
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
    v0 <- p0 * q0
    v1 <- p1 * q1
    pl <- arithmetic_mean(p1 / p0, v0, na.rm)
    ql <- arithmetic_mean(q1 / q0, v0, na.rm)
    v <- sum(v1, na.rm = na.rm) / sum(v0, na.rm = na.rm)
    (pl - b / a * ql) / 2 + sqrt((pl - b / a * ql)^2 / 4 + b / a * v)
  }
}

#---- AG mean index ----
agmean_index <- function(r) {
  force(r)

  function(elasticity) {
    nest_mean <- nested_mean(r, c(0, 1), c(elasticity, 1 - elasticity))
    function(p1, p0, q0, na.rm = FALSE) {
      v0 <- p0 * q0
      nest_mean(p1 / p0, v0, v0, na.rm)
    }
  }
}

arithmetic_agmean_index <- agmean_index(1)

geometric_agmean_index <- agmean_index(0)

#---- Lehr index ----
lehr_index <- function(p1, p0, q1, q0, na.rm = FALSE) {
  v1 <- p1 * q1
  v0 <- p0 * q0
  v <- (v1 + v0) / (q1 + q0)
  sum(v1, na.rm = na.rm) / sum(v0, na.rm = na.rm) *
    sum(v * q0, na.rm = na.rm) / sum(v * q1, na.rm = na.rm)
}
