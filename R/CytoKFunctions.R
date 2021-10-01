#' @useDynLib cytoKernel
#' @importFrom Rcpp evalCpp
#' @exportPattern '^[[:alpha:]]+'
NULL

.traceM <-
  function(X){sum(diag(X))}

.inverseLogit <-
  function(x){return(exp(x)/(exp(x)+1))}

.distKernel <-
  function(x,y,rho){
    out=exp(-(x-y)^2/rho)
    return(out)
  }

#' @importFrom stats glm
#' @importFrom stats binomial
#' 
.KST<- function(featureVec,group_factor,lowerRho,upperRho,gridRho)
{
  x<- featureVec
  n<- length(group_factor)
  ## Null model glm fit
  fit_glm<- stats::glm(group_factor~1,family=binomial)
  beta_hat<- fit_glm$coef
  mu_0<- .inverseLogit(beta_hat) 
  D_0<- diag(mu_0*(1-mu_0),n)
  X<- rep(1,n)
  P_0<- D_0-D_0%*%X%*%solve(t(X)%*%D_0%*%X)%*%t(X)%*%D_0
  rho<- seq(lowerRho,upperRho,length=gridRho)
  mu<- Q<- sigma<- array(0,dim=c(gridRho))
  for(l in seq_len(gridRho)){
    k<- matrix(NA,n,n)  
    for (i in seq_len(n)){
      for(j in seq_len(n)){
        k[i,j]<- .distKernel(x[i],x[j],rho[l])
      }
    }
    Q[l]<- t(group_factor-mu_0)%*%k%*%(group_factor-mu_0)
    mu[l]<- .traceM(P_0%*% k)  
    sigma[l]<- sqrt(2*.traceM(P_0%*% k%*%P_0%*% k))
  }
  return(list(Q1=data.frame(Q),mu1=data.frame(mu),
              sigma1=data.frame(sigma)))
}

#' @importFrom ashr ash

.postmean_EB<- function(QStorevec,sigmaStorevec)
{
  beta_ash3<- ashr::ash(QStorevec,sigmaStorevec, mode="estimate",
                            mixcompdist="normal", method="shrink")
  mu_post<- beta_ash3$result$PosteriorMean
  sigma_post<- beta_ash3$result$PosteriorSD
  return(list(mu_post1=data.frame(mu_post),
              sigma_post1=data.frame(sigma_post)))
}

#' @importFrom stats pnorm
#' 
.p_value<- function(Q_vec, mu_vec, sigma_vec)
{
  Score_s<- (Q_vec - mu_vec)/ sigma_vec
  M<- max(Score_s)
  W<- 0
  for(i in seq_len(length(Q_vec)-1)){
    W<- W+abs(Score_s[i+1]-Score_s[i])
  }
  pValue<- stats::pnorm(-M)+W*exp(-M^2/2)/sqrt(8*pi) ## (12) in Liu et al. (2008)
  return(pValue)
}