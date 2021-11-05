### Compute the MDL penalty
penalty.MDL <- function(y,x,cp,x.min, x.max, x.inc) {                  # y   : response
  # x   : predictor with varying coefficient
  # cp  : cpt chromosome (m;xi_1,...,xi_m)
  m <- cp[1]                                       # m   : number of cpts
  m.max = 10
  n = length(x)
  if (m == 0) {
    pnt <- log(n)
  } else {
    xi <- c(x.min-x.inc,cp[-1],x.max+x.inc)        # xi  : cpt locations (xi_1,...,xi_m,x.max+x.inc)
    n.r <- numeric(length=m+1)                     # n.r : no. of obs in each regime
    for (i in 1:(m+1)) {                           # [!CAUTION!] This does not handle missing values!
      n.r[i] <- length(y[xi[i] <= x & x < xi[i+1]])
    }
    rank = cumsum(n.r)
    # Try rank scheme for penalty term
    # pnt <- log(m+1)+.5*sum(log(sort(n.r)[-1])) #hange n.r to xi
    # 
    ## Look at Jasa 2012 for MDL terms
    
    ## Compare logliklihood using true knots 
    ## vs our modles
    ## break up likelihood into three terms, likelihood, penalty and combo
    
    # 1 on n=250, .933 on n=1000
    pnt <- log(m+1) + (m+2)*log(n) + .5*sum(log(n.r[-1])) # + .5*sum(log(sort(n.r)[-1]))# try with m instead
    # pnt = (m+3)*log(n)
    ## Try BIC
  }
  return(pnt)                                      # smaller is better
}

### ML estimation via glm() without exact covariates, returning negative likelihood value
nloglik.M0_glm <- function(y,x,cp) {               # y   : response
  # x   : predictor with varying coefficient
  # cp  : changepoint chromosome (m; tau_1,...,tau_m)
  # link: link function for Poisson regression
  m <- cp[1]                                       # m   : number of cpts
  
  if (m == 0) {
    X <- cbind(x)
  } else {
    x.spl <- outer(x,cp[-1],">")*outer(x,cp[-1],"-")
    X <- cbind(x,x.spl)
  }
  p <- ncol(X)-m
  
  fit.MLE_out <- glm(y~X,family=gaussian(link="identity"),start=c(rep(0.5,1+p),rep(0,m)))
  nllik.glm <- -as.numeric(logLik(fit.MLE_out))
  
  return(nllik.glm)
}
###

### Compute the penalized log-likelihood with MDL penalty
pnllik.MDL.M0 <- function(y,x,cp,x.min,x.max,x.inc) {       # y   : response
  # x   : predictor with varying coefficient
  # i.g : indicator variable for group
  # cp  : changepoint chromosome (m; tau_1,...,tau_m)
  # link: link function for Poisson regression
  pnllik.MDL <- nloglik.M0_glm(y=y,x=x,cp=cp)+penalty.MDL(y=y,x=x,cp=cp,x.min=x.min,x.max=x.max,x.inc=x.inc)
  
  return(pnllik.MDL)
}



### ML estimation via glm() with exact covariates, returning negative log-likelihood value
nloglik.M0Z_glm <- function(y,z,x,cp) {            # y   : response
  # z   : predictor without measurement error
  # x   : predictor with varying coefficient
  # cp  : changepoint chromosome (m; tau_1,...,tau_m)
  # link: link function for Poisson regression
  m <- cp[1]                                       # m   : number of cpts
  
  if (m == 0) {
    X <- cbind(z,x)
  } else {
    x.spl <- outer(x,cp[-1],">")*outer(x,cp[-1],"-")
    X <- cbind(z,x,x.spl)
  }
  p <- ncol(X)-m
  
  fit.MLE_out <- glm(y~X,family=gaussian(link="identity"),start=c(rep(0.5,1+p),rep(0,m)))
  nllik.glm <- -as.numeric(logLik(fit.MLE_out))
  pen.bic = BIC(fit.MLE_out)
  # pen.aic = AIC(fit.MLE_out)
  return(nllik.glm)
}
###

### ML estimation via glm(), returning the glm fit result
fit.glm_M0Z <- function(y,z,x,cp) {                # y   : response
  # z   : predictor without measurement error
  # x   : predictor with varying coefficient
  # cp  : changepoint chromosome (m; tau_1,...,tau_m)
  # link: link function for Poisson regression
  m <- cp[1]                                       # m   : number of cpts
  
  if (m == 0) {
    X <- cbind(z,x)
  } else {
    x.spl <- outer(x,cp[-1],">")*outer(x,cp[-1],"-")
    X <- cbind(z,x,x.spl)
  }
  p <- ncol(X)-m
  
  fit.glm_out <- glm(y~X,family=gaussian(link="identity"),start=c(rep(0.5,1+p),rep(0,m)))
  
  return(fit.glm_out)
}
###

### Compute the penalized log-likelihood with MDL penalty
pnllik.MDL.M0Z <- function(y,z,x,cp,x.min,x.max,x.inc) {             # y   : response
  # z   : predictor without measurement error
  # x   : predictor with varying coefficient
  # i.g : indicator variable for group
  # cp  : changepoint chromosome (m; tau_1,...,tau_m)
  # link: link function for Poisson regression
  pnllik.MDL <- nloglik.M0Z_glm(y=y,z=z,x=x,cp=cp) +penalty.MDL(y=y,x=x,cp=cp,x.min=x.min,x.max=x.max,x.inc=x.inc)
  
  return(pnllik.MDL)
}


