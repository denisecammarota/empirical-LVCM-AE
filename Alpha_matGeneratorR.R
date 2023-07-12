## Generación matriz alfa para modelo LV competencia (el que use en maestria)
# Basado en el codigo matlab de Hugo (/home/user/Escritorio/TIOJORGE/DOCTORADO/POP_FLUCTUATIONS/COMP_PRED_MODEL/Alpha_matGenerator.m)

Alpha_matGeneratorR<-function (N, bc,means_v,sigmav){

#N=10 # number of populations
#bc=1 # Si hay periodic boundary conditions (bc=0) o el eje es finito (bc=1)
#means_v<- 1:N /N # Vector of mean position in the niche axis (length==N)
#sigmav<- runif(length(means_v),0,0.5) # Vector with standard deviation for each species in the niche axis

dif_means_Mat<-matrix(0,nrow=N,ncol=N)
sum_means_Mat<-matrix(0,nrow=N,ncol=N)
dif_means_Mat_sigmas<-matrix(0,nrow=N,ncol=N)
sum_means_Mat_sigmas<-matrix(0,nrow=N,ncol=N)
normalization_Mat<-matrix(0,nrow=N,ncol=N)
means_Mat<-matrix(0,nrow=N,ncol=N)
sum_erf_sym<-matrix(0,nrow=N,ncol=N)
Lminus_means_Mat<-matrix(0,nrow=N,ncol=N)
# en ?rnorm al final para hacer la erf
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1


if (bc==0){ # PBC
    for (i in 1:N){
        for (j in 1:N){

            dif_means_Mat[i,j]=min(abs(means_v[i]-means_v[j]), 1-abs(means_v[i]-means_v[j])) # implements the PBC
            
            dif_means_Mat_sigmas[i,j]= dif_means_Mat[i,j]/sqrt(2*(sigmav[i]^2 + sigmav[j]^2));
            normalization_Mat[i,j]=sqrt((2*sigmav[i]*sigmav[j])/(sigmav[i]^2+sigmav[j]^2));
        }# enl if loop
    }#end for loop

    alpha_Mat=normalization_Mat*exp( -(dif_means_Mat_sigmas^2));

} else if (bc==1){   # FINITE NICHE of length Lmax- means_v tiene que estar normalizado a Lmax=1

    Lmax=1;
    for (i in 1:N){
        for (j in 1:N){
            
            dif_means_Mat[i,j]=means_v[i]-means_v[j];
            
            dif_means_Mat_sigmas[i,j]= dif_means_Mat[i,j]/sqrt(2*(sigmav[i]^2+sigmav[j]^2));

            means_Mat[i,j]=means_v[i];             
            sum_means_Mat[i,j]=means_v[i]+means_v[j]; 
            
            sum_means_Mat_sigmas[i,j]= (means_v[i]+means_v[j]) / (sigmav[i]+sigmav[j]); 
            
            Lminus_means_Mat[i,j]=(2*Lmax-(means_v[i]+means_v[j])) / (sigmav[i]+sigmav[j]);
            
            normalization_Mat[i,j]=sqrt((2*sigmav[i]*sigmav[j])/(sigmav[i]^2+sigmav[j]^2));
        }
    }             
    
    alpha_Mat0=normalization_Mat*exp(-(dif_means_Mat_sigmas^2)); 

    alpha_Mat=alpha_Mat0 *((erf(Lminus_means_Mat) + erf(sum_means_Mat_sigmas) ) / ( erf((((Lmax-means_v)/sigmav)*matrix(1,nrow=N,ncol=N))) + erf((means_v/sigmav)*matrix(1,nrow=N,ncol=N))))
}

return(alpha_Mat)

}