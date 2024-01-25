library(readxl)

Ettlinger <- as.data.frame(read_excel("Ettlinger_etal.xlsx"))
Ettlinger$AGL_rank <- rank(abs(Ettlinger$AGL), ties.method = "average")

rankSumMonoling <- sum(Ettlinger[Ettlinger$Biling == 0,"AGL_rank"])
nrowMonoling <- nrow(Ettlinger[Ettlinger$Biling == 0, ])

rankSumBiling <- sum(Ettlinger[Ettlinger$Biling == 1,"AGL_rank"])
nrowBiling <- nrow(Ettlinger[Ettlinger$Biling == 1, ])
  
u <- rankSumMonoling - ((nrowMonoling*(nrowMonoling+1))/2)
uPrim <- rankSumBiling - ((nrowBiling*(nrowBiling+1))/2)

muU <- (u*uPrim)/2
sigmaU <- sqrt(length(Ettlinger[Ettlinger$Biling == 0,"AGL_rank"]) * length(Ettlinger[Ettlinger$Biling == 1,"AGL_rank"]) *
                 (length(Ettlinger[Ettlinger$Biling == 0,"AGL_rank"]) + length(Ettlinger[Ettlinger$Biling == 1,"AGL_rank"]) +1) /12)
z <- (uPrim - muU)/sigmaU

Ettlinger$diff_sc <- Ettlinger$Simple - Ettlinger$Complex
Ettlinger$diff_sc_rank <- rank(abs(Ettlinger$diff_sc), ties.method = "average")

w <- sum(Ettlinger$diff_sc_rank * sign(Ettlinger$diff_sc))
sigmaW <- sqrt((length(Ettlinger[Ettlinger$diff_sc != 0,"diff_sc"]) * (length(Ettlinger[Ettlinger$diff_sc != 0,"diff_sc"]) + 1) * ((2*length(Ettlinger[Ettlinger$diff_sc != 0,"diff_sc"])) + 1))/6)

z <- w / sigmaW
