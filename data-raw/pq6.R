# Table 3.1 in Balk (2008), adapted from table 19.1 in PPI manual
price6 <- data.frame(p1 = c(1.0, 1.2, 1.0, 0.8, 1.0),
                     p2 = c(1.0, 3.0, 1.0, 0.5, 1.0),
                     p3 = c(1.0, 1.3, 1.5, 1.6, 1.6),
                     p4 = c(1.0, 0.7, 0.5, 0.3, 0.1),
                     p5 = c(1.0, 1.4, 1.7, 1.9, 2.0),
                     p6 = c(1.0, 0.8, 0.6, 0.4, 0.2)
)
price6 <- as.matrix(price6)
# Table 3.2 in Balk (2008), adapted from table 19.2 in PPI manual
quantity6 <- data.frame(q1 = c(1.0, 0.8, 1.0, 1.2, 0.9),
                        q2 = c(1.0, 0.9, 1.1, 1.2, 1.2),
                        q3 = c(2.0, 1.9, 1.8, 1.9, 2.0),
                        q4 = c(1.0, 1.3, 3.0, 6.0, 12.0),
                        q5 = c(4.5, 4.7, 5.0, 5.6, 6.5),
                        q6 = c(0.5, 0.6, 0.8, 1.3, 2.5)
)
quantity6 <- as.matrix(quantity6)