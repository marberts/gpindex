index_weights <- function(p0, p1, q0, q1, type, name) {
  # check input
  stopifnot(is.numeric(p0), is.numeric(p1), 
            missing(q0) || is.numeric(q0),
            missing(q1) || is.numeric(q1), 
            length(p0) == length(p1),
            missing(q0) || length(p0) == length(q0),
            missing(q1) || length(p0) == length(q1)
            )
  # match index and type arguments
  type <- match.arg(type, c("arithmetic", "geometric", "harmonic"))
  name <- match.arg(name, c("Carli", 
                            "Dutot",
                            "Laspeyres", 
                            "Paasche", 
                            "Palgrave", 
                            "Unnamed", 
                            "Drobish", 
                            "Walsh", 
                            "MarshallEdgeworth", 
                            "GearyKhamis",
                            "Jevons",
                            "Tornqvist")
                    )
  if (length(p0) == 0L) return(numeric(0))
  # arithmetic indices
  if(type == "arithmetic") {
    switch(name,
           Carli = rep_len(1 / length(p0), length(p0)),
           Dutot = p0 / sum(p0),
           Laspeyres = p0 * q0 / sum(p0 * q0),
           Paasche = p0 * q1 / sum(p0 * q1),
           Palgrave = p1 * q1 / sum(p1 * q1),
           Unnamed = 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1),
           Drobish = 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p0 * q1 / sum(p0 * q1),
           Walsh = p0 * sqrt(q0 * q1) / sum(p0 * sqrt(q0 * q1)),
           MarshallEdgeworth = p0 * (q0 + q1) / sum(p0 * (q0 + q1)),
           GearyKhamis = p0 * (1 / p0 + 1 / p1) / sum(p0 * (1 / p0 + 1 / p1)),
           stop("Don't know how to make weights for an arithmetic ", name, " index.")
    ) 
  # geometric indices
  } else if (type == "geometric") {
    switch(name,
           Jevons = rep_len(1 / length(p0), length(p0)),
           Laspeyres = p0 * q0 / sum(p0 * q0),
           Paasche = p1 * q1 / sum(p1 * q1),
           Tornqvist = 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1),
           stop("Don't know how to make weights for a geometric ", name, " index.")
    ) 
  # harmonic indices
  } else {
    switch(name,
           Jevons = 1 / length(p0),
           Laspeyres = p1 * q0 / sum(p1 * q0),
           Paasche = p1 * q1 / sum(p1 * q1),
           stop("Don't know how to make weights for a harmonic ", name, " index.")
    ) 
  }
}
