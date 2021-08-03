#---- Index weights ----
index_weights <- function(type = c("Carli", "Jevons", "Coggeshall",
                                   "Dutot", "Laspeyres", "HybridLaspeyres",
                                   "Palgrave", "Paasche", "HybridPaasche",
                                   "Drobish", "Unnamed", "Tornqvist",
                                   "Walsh1", "Walsh2", "MarshallEdgeworth",
                                   "GearyKhamis", "Vartia1", "MontgomeryVartia",
                                   "Vartia2", "SatoVartia", "Lowe",
                                   "Young", "LloydMoulton")) {
  # return function
  res <- switch(
    match.arg(type),
    Carli = ,
    Jevons = ,
    Coggeshall = function(p0) replace(p0, values = 1), # keep attributes
    Dutot = function(p0) p0,
    Young = function(pb, qb) pb * qb,
    Lowe = function(p0, qb) p0 * qb,
    LloydMoulton = ,
    Laspeyres = function(p0, q0) p0 * q0,
    HybridLaspeyres = function(p1, q0) p1 * q0,
    Palgrave = ,
    Paasche = function(p1, q1) p1 * q1,
    HybridPaasche = function(p0, q1) p0 * q1,
    Drobish = function(p1, p0, q1, q0) 
      (p0 * q0 / v(p0, q0) + p0 * q1 / v(p0, q1)) / 2,
    Unnamed = ,
    Tornqvist = function(p1, p0, q1, q0) 
      (p0 * q0 / v(p0, q0) + p1 * q1 / v(p1, q1)) / 2,
    Walsh1 = function(p0, q1, q0) p0 * sqrt(q0 * q1),
    Walsh2 = function(p1, p0, q1, q0) sqrt(p0 * q0 * p1 * q1),
    MarshallEdgeworth = function(p0, q1, q0) p0 * (q0 + q1),
    GearyKhamis = function(p0, q1, q0) p0 / (1 / q0 + 1 / q1),
    Vartia1 = ,
    MontgomeryVartia = function(p1, p0, q1, q0) 
      logmean(p0 * q0, p1 * q1) / logmean(v(p0, q0), v(p1, q1)),
    Vartia2 = ,
    SatoVartia = function(p1, p0, q1, q0) 
      logmean(p0 * q0 / v(p0, q0), p1 * q1 / v(p1, q1))
  )
  # clean up enclosing environment
  environment(res) <- getNamespace("gpindex")
  res
}

#---- Pythagorean indexes ----
pythagorean_index <- function(class = c("arithmetic", "geometric", "harmonic")) {
  types <- switch(match.arg(class),
                  arithmetic = c("Carli", "Dutot", "Laspeyres",
                                 "Palgrave", "Drobish", "Unnamed",
                                 "Walsh1", "MarshallEdgeworth", "GearyKhamis",
                                 "Lowe", "Young"),
                  geometric = c("Jevons", "Laspeyres", "Paasche",
                                "Tornqvist", "Vartia1", "MontgomeryVartia",
                                "Vartia2", "SatoVartia", "Walsh2",
                                "Young"),
                  harmonic = c("Coggeshall", "Laspeyres", "Paasche", "Young"))
  r <- switch(class, arithmetic = 1, geometric = 0, harmonic = -1)
  gen_mean <- generalized_mean(r)
  # return function
  function(type) {
    type <- match.arg(type, types)
    # return function
    res <- switch(
      type,
      Carli = ,
      Dutot = ,
      Jevons = ,
      Coggeshall = function(p1, p0, na.rm = FALSE)
        gen_mean(p1 / p0, index_weights(type)(p0), na.rm),
      Laspeyres = function(p1, p0, q0, na.rm = FALSE) 
        gen_mean(p1 / p0, index_weights(type)(p0, q0), na.rm),
      Paasche = ,
      Palgrave = function(p1, p0, q1, na.rm = FALSE)
        gen_mean(p1 / p0, index_weights(type)(p1, q1), na.rm),
      Drobish = ,
      Unnamed = ,
      Vartia2 = ,
      SatoVartia = ,
      Walsh2 = ,
      Tornqvist = function(p1, p0, q1, q0, na.rm = FALSE)
        gen_mean(p1 / p0, index_weights(type)(p1, p0, q1, q0), na.rm),
      Vartia1 = ,
      MontgomeryVartia = function(p1, p0, q1, q0, na.rm = FALSE)
        exp(sum(log(p1 / p0) * index_weights(type)(p1, p0, q1, q0), na.rm = na.rm)),
      Walsh1 = ,
      MarshallEdgeworth = ,
      GearyKhamis = function(p1, p0, q1, q0, na.rm = FALSE)
        gen_mean(p1 / p0, index_weights(type)(p0, q1, q0), na.rm),
      Lowe = function(p1, p0, qb, na.rm = FALSE)
        gen_mean(p1 / p0, index_weights(type)(p0, qb), na.rm),
      Young = function(p1, p0, pb, qb, na.rm = FALSE)
        gen_mean(p1 / p0, index_weights(type)(pb, qb), na.rm)
    )
    # clean up enclosing environment
    enc <- list(gen_mean = gen_mean, type = type)
    environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
    res
  }
}

arithmetic_index <- pythagorean_index("arithmetic")

geometric_index <- pythagorean_index("geometric")

harmonic_index <- pythagorean_index("harmonic")

#---- Common indexes ----
laspeyres_index <- arithmetic_index("Laspeyres")

paasche_index <- harmonic_index("Paasche")

jevons_index <- geometric_index("Jevons")

lowe_index <- arithmetic_index("Lowe")

young_index <- arithmetic_index("Young")

#---- Fisher index ----
fisher_index <- function(p1, p0, q1, q0, na.rm = FALSE) {
  wl <- index_weights("Laspeyres")(p0, q0)
  wp <- index_weights("Paasche")(p1, q1)
  fisher_mean(p1 / p0, wl, wp, na.rm)
}

#---- Harmonic Laspeyres Paasche index ----
hlp_index <- function(p1, p0, q1, q0, na.rm = FALSE) {
  wl <- index_weights("Laspeyres")(p0, q0)
  wp <- index_weights("Paasche")(p1, q1)
  nested_mean(-1, c(1, -1))(p1 / p0, wl, wp, na.rm)
}

#---- Lloyd Moulton index ----
lm_index <- function(p1, p0, q0, elasticity, na.rm = FALSE) {
  w <- index_weights("LloydMoulton")(p0, q0)
  generalized_mean(1 - elasticity)(p1 / p0, w, na.rm)
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

#---- Generalized Stuval index ----
stuval_index <- function(a, b) {
  if (not_number(a)) {
    stop(gettext("'a' must be a finite length 1 numeric"))
  }
  if (not_number(b)) {
    stop(gettext("'b' must be a finite length 1 numeric"))
  }
  function(p1, p0, q1, q0, na.rm = FALSE) {
    pl <- laspeyres_index(p1, p0, q0, na.rm)
    ql <- laspeyres_index(q1, q0, p0, na.rm)
    v <- sum(p1 * q1, na.rm = na.rm) / sum(p0 * q0, na.rm = na.rm)
    (pl - b / a * ql) / 2 + sqrt((pl - b / a * ql)^2 / 4 + b / a * v)
  }
}

#---- AG mean index ----
agmean_index <- function(r) {
  force(r)
  function(p1, p0, q0, elasticity, na.rm = FALSE) {
    w <- index_weights("Laspeyres")(p0, q0)
    nested_mean(r, c(0, 1), c(elasticity, 1 - elasticity))(p1 / p0, w, w, na.rm)
  }
}

arithmetic_agmean_index <- agmean_index(1)

geometric_agmean_index <- agmean_index(0)