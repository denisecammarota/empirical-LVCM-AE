# Exploration of scaling of equilibria Nneg 
# and Npos for delta and beta

library(latex2exp)
library(RColorBrewer)
library(ggplot2)
library(reshape)
library(viridis)
library(hrbrthemes)
library(cowplot)

N_pos <- function(M,pars){
  a0 = pars[1]
  b0 = pars[2]
  m0 = pars[3]
  C0 = pars[4]
  s = pars[5]
  beta = pars[6]
  delta = pars[7]
  term1 = (((a0/2*s)*(M**-1)) - ((M**(-beta))/(2*C0))) #ok
  term2 = (M**(-beta-1))*(a0/(C0*s))*(((m0*(M**(delta+0.25)))/b0) - 1) #ok
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
  term1 = (((a0/2*s)*(M**-1)) - ((M**(-beta))/(2*C0))) #ok
  term2 = (M**(-beta-1))*(a0/(C0*s))*(((m0*(M**(delta+0.25)))/b0) - 1) #ok 
  res_neg = term1 - sqrt(term1**2 - term2)
  return(res_neg)
}

# Doing this for Nneg and Npos for beta and delta
a0 = 32*1024 
b0 = 5.73 
a = 0.3 
n = 100 
s = a*(n-1) + 1 
delta = -1/4
C0 = 100
delta_seq = seq(-0.5,0.5,0.01)
beta_seq = seq(0.0,1/3,0.005)
m0 = b0*10
M = seq(10**-1,10**4,10) 
slope_mat_neg = matrix(nrow = length(beta_seq),ncol = length(delta_seq),0)
slope_mat_pos = matrix(nrow = length(beta_seq),ncol = length(delta_seq),0)

i = 0
j = 0
for (beta in beta_seq){
  j = 0
  i = i+1
  for(delta in delta_seq){
    j = j+1
    pars = c(a0,b0,m0,C0,s,beta,delta)
    x = log(M) 
    y_neg = log(N_neg(M, pars))
    y_pos = log(N_pos(M, pars))
    fit_neg = lm(y_neg ~ x)
    fit_pos = lm(y_pos ~ x)
    print(i)
    print(j)
    slope_mat_neg[i,j] = fit_neg$coefficients[['x']]
    slope_mat_pos[i,j] = fit_pos$coefficients[['x']]
  }
}

# plot for positive sign equilibrium
colnames(slope_mat_pos) = delta_seq
rownames(slope_mat_pos) = beta_seq
data_melt_pos <- melt(slope_mat_pos) 
ggp_pos <- ggplot(data_melt_pos, aes(X1, X2)) +                     
  geom_tile(aes(fill = value)) + theme_bw() +
  labs(x = TeX('$\\beta$'), y = TeX('$\\delta$'), title = TeX('Slope for $N_{pos}$')) + 
  scale_fill_viridis(discrete=FALSE, limits = c(-1.25, -0.8)) + guides(fill = guide_colourbar(title = "Slope")) +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14))

# plot for negative sign equilibrium
colnames(slope_mat_neg) = delta_seq
rownames(slope_mat_neg) = beta_seq
data_melt_neg <- melt(slope_mat_neg) 
ggp_neg <- ggplot(data_melt_neg, aes(X1, X2)) +                     
  geom_tile(aes(fill = value)) + theme_bw() +
  labs(x = TeX('$\\beta$'), y = TeX('$\\delta$'), title = TeX('Slope for $N_{crit}$')) + 
  scale_fill_viridis(discrete=FALSE) + guides(fill = guide_colourbar(title = "Slope")) +
  theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14))

# put plots side to side
plot_grid(ggp_pos, ggp_neg, labels = "AUTO")





