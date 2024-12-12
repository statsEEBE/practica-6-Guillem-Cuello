#EXERCICI ESTIMACIÓ PER INTERVALS
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)

#Apartat a) interval confiança del 90% per la mitja poblacional? 

#X -> N(mu, sigma^2), sabem sigma pq ens ho diu el fabricant, però no sabem mu
#Podem aproximar mu com la mitja de la mostra però es comet un error, per això
#creem un interval de confiança, per tenir en compte l'error que estem cometent

#IC per la mitja mu(μ) al 90%: P(-m < x_bar - μ < m) = 0,9

#Cas 1: condició 1: X -> N(μ, σ^2)
#       condició 2: coneixo σ^2   m = Z_0,05 * σ/sqrt(n)

#IC = (1-α)% --> 	α significació estadística = 	α = 0,1

xbar <- mean(x)
xbar
sigma <- sqrt(25)
n <- length(x)
z_0.05 <- qnorm(0.95) # α = 0.1, confiança = 0.9 (90%)

c(xbar - z_0.05 * sigma / sqrt(n), xbar + z_0.05 * sigma / sqrt(n))

#Instalem llibreria de funcions
install.packages("BSDA")
library(BSDA)
z.test(x, sigma.x = sigma, conf.level = 0.9)

#H_sub0: μ = μ_0 = 500 -> Hipotesis Nula
#H_sub1: μ ≠ μ_0 = 500 -> Hipotesis Alternativa
#Per saber si rebutjar o no la hipotesis: 

#Criteri 1: IC
#Si μ_0 pertany al IC: H_sub0 és correcte
#Si μ_0 no pertany al IC: H_sub0 la rebutjem

#Criteri 2: Si H_sub0 és veritat: z = (xbar - μ_0)/(sigma/sqrt(n))

#Anem a provar que les caixes tenen un per diferent de 500g
# HO: mu = 500
# H1: mu ≠ 500 
zc <- qnorm(0.95)
mu0 <- 500
z_observat <- (xbar-mu0)/(sigma/sqrt(n))
z_observat

#Com z_obs és més gran que zc, i per tant, cau fora de la zona d'acceptació, rebutjem la H0

#Criteri 3: p-valor
#Si p-valor >= α -> No rebutjem H0
#Si p-valor < α -> Rebutjem H0 

pvalue <- 2*pnorm(-z_observat)
pvalue

#Cua superior:
#H0: mu <= mu_0
#H1: mu > mu_0
z.test(x, sigma.x = sigma, conf.level = 0.9, mu=mu0, alternative = "greater")

#Cua inferior:
#H0: mu >= mu_0
#H1: mu < mu_0


#Apartat b) l = 2, 95% conf
#m=z_0.025*sigma/sqrt(n) -> n = (z_0.025*sigma)^2
n <- (qnorm(0.975)*sigma)^2
n
z_0.25 <- qnorm(0.975)
c(xbar - z_0.25 * sigma / sqrt(n), xbar + z_0.25 * sigma / sqrt(n))
# 504.75 - 502.75 = 2 --> està bé

#Apartat c) sigma desconeguda, conf 99%, X -> N(mu, sigma^2)
#No tenim nimu ni sigma, els estimem amb la mitja/variança muestral (xbar, S^2)
#A l'estimar mu i sigma cometem un error: (l, n) = (xbar-t*S/sqrt(n),xbar+t*S/sqrt(n))

xbar <- mean(x)
n <- length(x)
t005 <- qt(0.995, n-1)
s <- sd(x)

c(xbar - t005*s/sqrt(n), xbar + t005*s/sqrt(n))
t.test(x, conf.level = 0.99)

#Hipotesis-> H0: mu = mu0 = 500, H1: mu != mu0 = 500 (two.sided)

t.test(x, alternative = "two.sided", conf.level = 0.99, mu=500)
