# Exploration of scaling of equilibria Nneg
# for beta = 1/3 in AE and different 
# with m scaling as b
# Code developed by Denise Cammarota

library(latex2exp)
library(RColorBrewer)
library(ggplot2)
library(reshape)
library(viridis)
library(hrbrthemes)

N_pos <- function(M,pars){
  a0 = pars[1]
  b0 = pars[2]
  m0 = pars[3]
  C0 = pars[4]
  s = pars[5]
  beta = pars[6]
  delta = pars[7]
  term1 = ((a0*(M**-1))/(2*s))*(1 - (m0/b0)*(M**(delta+0.25)))
  term2 = (a0*m0*C0*(M**(beta+delta-(3/4))))/(b0*s)
  res_pos = term1 + sqrt(term1**2 - term2)
  return(res_pos)
}

N_neg <- function(M,pars){
  a0 = pars[1]
  b0 = pars[2]
  m0 = pars[3]
  C0 = pars[4]
  s = pars[5]
  beta = pars[6]
  delta = pars[7]
  term1 = 0.5*((a0*(M**-1))/(s))*(1 - (m0/b0)*(M**(delta+0.25)))
  term2 = (a0*m0*C0*(M**(beta+delta-(3/4))))/(b0*s)
  res_neg = term1 - sqrt(term1**2 - term2)
  return(res_neg)
}

# Scaling of Nneg for beta = 1/3 as a function of m/b with m scaling -0.25

# Doing a basic sensibility plot of scaling vs C0
a0 = 32*1024 
b0 = 5.73 
a = 0.3 
n = 100 
beta = 0
s = a*(n-1) + 1 
C0 = 0.01
delta_seq = seq(-0.75,-0.15,0.05)
m0_seq = b0*seq(0.01,0.2,0.01)
M = seq(10**-1,10**3,10) 
slope_mat = matrix(nrow = length(delta_seq),ncol = length(m0_seq),0)
i = 0
j = 0
for (delta in delta_seq){
  j = 0
  i = i+1
  for(m0 in m0_seq){
    j = j+1
    pars = c(a0,b0,m0,C0,s,beta,delta)
    x = log(M) 
    y = log(N_neg(M, pars))
    fit = lm(y ~ x)
    print(i)
    print(j)
    slope_mat[i,j] = fit$coefficients[['x']]
  }
}
colnames(slope_mat) = m0_seq/b0
rownames(slope_mat) = delta_seq
data_melt <- melt(slope_mat) 
ggp <- ggplot(data_melt, aes(X1, X2)) +                     
  geom_tile(aes(fill = value)) + theme_bw() +
  labs(x = TeX('$\\delta$'), y = TeX('$m_0/b_0$'), title = TeX('$\\beta = 0, C_0 = 0.1$')) + 
  scale_fill_viridis(discrete=FALSE) + guides(fill = guide_colourbar(title = "Slope"))
ggp    

# dependence of slope with delta
plot(delta_seq,slope_mat[,10],type = 'b', col = 'red', 
     xlab = TeX('$\\delta$'), ylab = TeX('Slope'), lwd = 2)