obs <- rbind(c(566, 12), c(5979434, 163988))

obsmg <- addmargins(obs)

exp_Z1S1 <- obsmg[1,3] * obsmg[3,1] / obsmg[3,3]
exp_Z1S2 <- obsmg[1,3] * obsmg[3,1] / obsmg[3,3]
exp_Z2S1 <- obsmg[1,3] * obsmg[3,1] / obsmg[3,3]
exp_Z2S2 <- obsmg[1,3] * obsmg[3,1] / obsmg[3,3]

exp <- rbind(c(exp_Z1S1, exp_Z1S2), c(exp_Z2S1, exp_Z2S2))