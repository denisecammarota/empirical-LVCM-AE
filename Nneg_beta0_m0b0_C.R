# Exploration of scaling of equilibria Nneg
# for beta = 1/3 in AE and different C and m0/b0
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

# Scaling of Nneg for beta = 1/3 as a function of m/b with m scaling -0.25

## scaling plot for different m0 and fixed C = 1
a0 = 32*1024 #ok
b0 = 5.73 #ok
C0 = 1 # trying diff values
a = 0.3 #ok
n = 100 #ok
beta = 0 #ok
s = a*(n-1) + 1 #ok
delta = -1/4
m0_seq = b0*c(2,5,10,25,50,100,250,500) #ok
M = seq(10**-1,10**4,10) #ok
cols <- brewer.pal(length(m0_seq),'Spectral')
plot(1, type = "n", xlab = TeX('$log(M)$'),
     ylab = TeX('$log(N_{crit})$'), xlim = c(-2, 9), 
     ylim = c(-9, 1), main = TeX('$C_0 = 1$') )
i = 1
for(m0 in m0_seq){
  print(m0)
  pars = c(a0,b0,m0,C0,s,beta,delta)
  lines(log(M),log(N_neg(M, pars)),lw = 2, col = cols[i])
  i = i + 1
}
legend(x = "topright",          # Position
       legend = round(m0_seq/b0),  # Legend texts
       col = cols,           # Line colors
       lwd = 3,
       cex = 0.6)         

## scaling plot for different m0 and fixed C = 100
a0 = 32*1024 #ok
b0 = 5.73 #ok
C0 = 100 # trying diff values
a = 0.3 #ok
n = 100 #ok
beta = 0 #ok
s = a*(n-1) + 1 #ok
delta = -1/4
m0_seq = b0*c(2,5,10,25,50,100,250,500) #ok
M = seq(10**-1,10**4,10) #ok
cols <- brewer.pal(length(m0_seq),'Spectral')
plot(1, type = "n", xlab = TeX('$log(M)$'),
     ylab = TeX('$log(N_{crit})$'), xlim = c(-2, 9), 
     ylim = c(-11, -2), main = TeX('$C_0 = 10$') )
i = 1
for(m0 in m0_seq){
  print(m0)
  pars = c(a0,b0,m0,C0,s,beta,delta)
  lines(log(M),log(N_neg(M, pars)),lw = 2, col = cols[i])
  i = i + 1
}
legend(x = "topright",          # Position
       legend = round(m0_seq/b0),  # Legend texts
       col = cols,           # Line colors
       lwd = 3,
       cex = 0.6)   

## scaling plot for different m0 and fixed C = 100
a0 = 32*1024 #ok
b0 = 5.73 #ok
C0 = 100 # trying diff values
a = 0.3 #ok
n = 100 #ok
beta = 0 #ok
s = a*(n-1) + 1 #ok
delta = -1/4
m0_seq = b0*c(2,5,10,25,50,100,250,500) #ok
M = seq(10**-1,10**4,10) #ok
cols <- brewer.pal(length(m0_seq),'Spectral')
plot(1, type = "n", xlab = TeX('$log(M)$'),
     ylab = TeX('$log(N_{crit})$'), xlim = c(-2, 9), 
     ylim = c(-13, -4), main = TeX('$C_0 = 100$') )
i = 1
for(m0 in m0_seq){
  print(m0)
  pars = c(a0,b0,m0,C0,s,beta,delta)
  lines(log(M),log(N_neg(M, pars)),lw = 2, col = cols[i])
  i = i + 1
}
legend(x = "topright",          # Position
       legend = round(m0_seq/b0),  # Legend texts
       col = cols,           # Line colors
       lwd = 3,
       cex = 0.6)   

## scaling plot for different m0 and fixed C = 1000
a0 = 32*1024 #ok
b0 = 5.73 #ok
C0 = 1000 # trying diff values
a = 0.3 #ok
n = 100 #ok
beta = 0 #ok
s = a*(n-1) + 1 #ok
delta = -1/4
m0_seq = b0*c(2,5,10,25,50,100,250,500) #ok
M = seq(10**-1,10**4,10) #ok
cols <- brewer.pal(length(m0_seq),'Spectral')
plot(1, type = "n", xlab = TeX('$log(M)$'),
     ylab = TeX('$log(N_{crit})$'), xlim = c(-2, 9), 
     ylim = c(-16, -6), main = TeX('$C_0 = 1000$') )
i = 1
for(m0 in m0_seq){
  print(m0)
  pars = c(a0,b0,m0,C0,s,beta,delta)
  lines(log(M),log(N_neg(M, pars)),lw = 2, col = cols[i])
  i = i + 1
}
legend(x = "topright",          # Position
       legend = round(m0_seq/b0),  # Legend texts
       col = cols,           # Line colors
       lwd = 3,
       cex = 0.6)   

# Doing a basic sensibility plot of scaling vs C0
a0 = 32*1024 
b0 = 5.73 
a = 0.3 
n = 100 
beta = 0
s = a*(n-1) + 1 
delta = -1/4
C0_seq = seq(1,500,1)
m0_seq = b0*seq(5,500,5)
M = seq(10**-1,10**4,10) 
slope_mat = matrix(nrow = length(C0_seq),ncol = length(m0_seq),0)
i = 0
j = 0
for (C0 in C0_seq){
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
rownames(slope_mat) = C0_seq
data_melt <- melt(slope_mat) 
ggp <- ggplot(data_melt, aes(X1, X2)) +                     
  geom_tile(aes(fill = value)) + theme_bw() +
  labs(x = TeX('$C_0$'), y = TeX('$m_0/b_0$'), title = TeX('$\\beta = 0, \\delta = -0.25$')) + 
  scale_fill_viridis(discrete=FALSE) + guides(fill = guide_colourbar(title = "Slope"))
ggp    
