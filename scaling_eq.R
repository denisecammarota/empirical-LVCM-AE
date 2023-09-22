# Exploration of scaling of equilibria Npos and Nneg
# as functions of unknown parameters (intercepts)
# Code developed by Denise Cammarota

N_pos <- function(M,pars){
  a0 = pars[1]
  b0 = pars[2]
  m0 = pars[3]
  C0 = pars[4]
  s = pars[5]
  beta = pars[6]
  term1 = (((a0/2*s)*M) - ((M**(-beta))/(2*C0)))
  term2 = (M**(-beta+1))*(a0/(C0*s))*((m0/b0) - 1)
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
  term1 = ((a0/2*s)*M) - ((M**(-beta))/(2*C0))
  term2 = (M**(-beta+1))*(a0/(C0*s))*((m0/b0) - 1)
  res_neg = term1 - sqrt(term1**2 - term2)
  return(res_neg)
}

# Scaling of Npos for beta = 0 as a function of m/b
a0 = 32*1024 #ok
b0 = 5.73 #ok
C0 = 2 # trying diff values
a = 0.3 #ok
n = 100 #ok
beta = 0 #ok
s = a*(n-1) + 1 #ok
m0_seq = seq(0.5,20,10) #ok
M = seq(10**-1,10**4,10) #ok
for(m0 in m0_seq){
  print('here')
  pars = c(a0,b0,m0,C0,s,beta)
  plot(M,N_pos(M, pars))
}


