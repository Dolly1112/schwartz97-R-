.clean.rda.data <- function(tmp.list, idx = 1:6)      ##一、Removes NAs from the internal futures data sets. 
{
  na.idx <- apply(is.na(tmp.list$futures[,idx]), 1, any)
  tmp.list$futures <- tmp.list$futures[!na.idx, idx]
  tmp.list$maturity <- tmp.list$maturity[!na.idx, idx]
  return(tmp.list)
}
##二、Check whether data is of a particular format and return a clean version of data.
.get.data <- function(data, type = c("uv", "mv"))     
{

  type <- match.arg(type)
  if(type == "uv"){
    if(!is.null(dim(data))){
      if(min(dim(data)) == 1){
        data.clean <- as.numeric(data)
      }else{
        stop(paste(deparse(substitute(x)), "must be univariate!"))
      }
    }else{
        data.clean <- as.numeric(data)
    }
  }
  return(data.clean)
  
}
##三、计算Schwartz两因素模型的联合正态分布状态变量的均值向量。 状态变量是现货对数价格和现货便利性收益。
.mu.state.schwartz2f <- function(x0, delta0, mu, sigmaS, kappa,
                                 alpha, sigmaE, rho,
                                 time, as.mat = FALSE)
{
  mX <- x0 + (mu - 0.5 * sigmaS^2 - alpha) * time +
    (alpha - delta0) * (1 - exp(-kappa * time))/kappa

  mD <- exp(-kappa * time) * delta0 + alpha *(1 - exp(-kappa * time))

  if(!as.mat)
    return(c(mX, mD))
  else
    return(matrix(c(mX, mD), 2))
}
#########################################################################################################################
##.mu.state.schwartz2f <- function(x0, delta0, mu, sigmaS, kappa,
##                                 alpha, sigmaE,m,sigmaI, rho1,rho2,rho3,
##                                 time, as.mat = FALSE)
###################################################################################################################
##四、计算Schwartz两因素模型的联合正态分布状态变量的协方差矩阵。 状态变量是现货对数价格和现货便利性收益。
.sigma.state.schwartz2f <- function(sigmaS, kappa, sigmaE,
                                    rho, time)
{
  vX <- sigmaE^2 / kappa^2 *
    (1 / (2 * kappa) * (1 - exp(-2 * kappa * time)) -
     2 / kappa * (1 - exp(-kappa * time)) + time) +
       2 * sigmaS * sigmaE * rho / kappa *
         (1 / kappa * (1 - exp(-kappa * time)) - time) +
           sigmaS^2 * time

  vD <- sigmaE^2 / (2 * kappa) * (1 - exp(-2 * kappa * time))

  vXD <- 1 / kappa *
    ((sigmaS * sigmaE * rho - sigmaE^2 / kappa) *
     (1 - exp(-kappa * time)) + sigmaE^2 / (2 * kappa) *
     (1 - exp(-2 * kappa * time)))

##  browser()
  return(cbind(c(vX, vXD), c(vXD, vD)))
}
##五、计算A（T）
.A.schwartz2f <- function(kappa, sigmaS, sigmaE, rho,
                          alphaT, r, ttm)
{
  term1 <- (r - alphaT + 0.5 * sigmaE^2 / kappa^2 -
            (sigmaS * sigmaE * rho) / kappa) * ttm
  term2 <- 0.25 * sigmaE^2 * (1 - exp(-2 * kappa * ttm)) / kappa^3
  term3 <- (alphaT * kappa + sigmaS * sigmaE * rho -
            sigmaE^2 / kappa) * (1 - exp(- kappa * ttm)) / kappa^2

  return(term1 + term2 + term3)
}
###################################################################################################################
.C.schwartz2f <- function(kappa, sigmaS, sigmaE, sigmaI,rho1,rho2,rho3,a
                          alphaT, r, ttm)
{
  term1 <- (kappa*alphaT+sigmaS*sigmaE*rho1)*((1-exp(-kappa*ttm)-kappa*ttm)/kappa^2
  term2 <- -0.25 * sigmaE^2 * (4*(1-exp(-kappa*ttm)-(1-exp(-2*kappa*ttm)-2*kappa*ttm))) / kappa^3
  term3 <- -(a*m+sigmaS*sigmaI*rho3)*((1-exp(-kappa*ttm)-a*ttm)/a^2
  term4 <- -0.25 * sigmaI^2 * (4*(1-exp(-a*ttm)-(1-exp(-2*a*ttm)-2*a*ttm))) / a^3
  term5 <- sigmaE*sigmaI*rho2*(((1-exp(-kappa*ttm))+(1-exp(-a*ttm))-(1-exp(-(kappa+a)*ttm)))/(kappa*a*(kappa+a))
+(kappa^2*(1-exp(-a*ttm))+a^2*(1-exp(-kappa*ttm))-kappa*a^2*ttm-a*kappa^2*ttm)/(kappa^2*a^2*(kappa+a)))

  return(term1 + term2 + term3 + term4 + term5)
}
.A.schwartz2f <- function(kappa, sigmaS, sigmaE, sigmaI,rho1,rho2,rho3,a
                          alphaT, r, ttm)
  {
   term1 <- r*(1-exp(-kappa*ttm))/a
  return(term + .C.schwartz2f)
}
###################################################################################################################                                      
##六、计算B（T）
.B.schwartz2f <- function(kappa, ttm)
{
    return((exp(-kappa * ttm) - 1) / kappa)
}
##七、计算mu
.mu.fut.schwartz2f <- function(x0, delta0, mu, sigmaS,
                               kappa, sigmaE, rho,
                               alpha, alphaT, r, time, ttm,
                               measure = "P")
{
  compA <- .A.schwartz2f(kappa = kappa, sigmaS = sigmaS,
                         sigmaE = sigmaE, rho = rho,
                         alphaT = alphaT, r = r, ttm = ttm - time)

  compB <- .B.schwartz2f(kappa = kappa, ttm = ttm - time)

  if(measure == "P"){
    mu.state <- .mu.state.schwartz2f(x0 = x0, delta0 = delta0, mu = mu,
                                     sigmaS = sigmaS, kappa = kappa,
                                     alpha = alpha, sigmaE = sigmaE,
                                     rho = rho, time = time)
  }else{
    mu.state <- .mu.state.schwartz2f(x0 = x0, delta0 = delta0, mu = r,
                                     sigmaS = sigmaS, kappa = kappa,
                                     alpha = alphaT, sigmaE = sigmaE,
                                     rho = rho, time = time)
  }
  return(sum(c(mu.state, 1) * c(1, compB, compA)))    #my.state第一列的数乘一个数列1，compB，compA
}
##八、
.sigma.fut.schwartz2f <- function(sigmaS, kappa, sigmaE, rho, time, ttm)
{
  compB <- .B.schwartz2f(kappa, ttm - time)
  sigma.state <- .sigma.state.schwartz2f(sigmaS = sigmaS, kappa = kappa,
                                         sigmaE = sigmaE, rho = rho,
                                         time = time)

  prod <- matrix(c(1, compB), ncol = 1)
  return(as.numeric(t(prod) %*% sigma.state %*% prod))  ## %*%是两个矩阵的乘积
}
##九、计算sigma
.sigma.opt.schwartz2f <- function(time, Time, kappa, sigmaS, sigmaE, rho)
{
  term1 <- sigmaS^2 * time

  term2 <- 2 * sigmaS * sigmaE * rho / kappa *
    (1 / kappa * exp(-kappa * Time) * (exp(kappa * time) - 1) -
     time)

  term3 <- sigmaE^2 / kappa^2 *
    (time +
     1 / (2 * kappa) * exp(-2 * kappa * Time) *
     (exp(2 * kappa * time) - 1) -
     2 / kappa * exp(-kappa * Time) * (exp(kappa * time) - 1))

  return(sqrt(term1 + term2 + term3))
}

.state.space.2f <- function(y, ttm, deltat, x0, delta0, kappa,
                            mu, alpha, lambda, sigmaS, sigmaE, rho,
                            gg, r, d, n)
{

  ## Transition equation for the Schwartz two-factor model
  ## ------------------------------------------------------------
  ## Exact transition density:
  Tt <- array(c(1, 0, 1/kappa * (exp(-kappa * deltat) - 1),
                exp(-kappa * deltat)), c(2, 2, 1))

  dt <- matrix(c((mu - 1/2 * sigmaS^2 - alpha) * deltat +
                 alpha / kappa * (1 - exp(-kappa * deltat)),
                 alpha * (1 - exp(- kappa * deltat))), 2, 1)

  HHt <- array(.sigma.state.schwartz2f(sigmaS = sigmaS, kappa = kappa,
                                       sigmaE = sigmaE, rho = rho,
                                       time = deltat), c(2, 2, 1))

  ## ------------------------------------------------------------
  ## Density of the linear approx. of the transition equation:
  ##   Tt <- array(matrix(c(1, 0, -deltat, 1 - kappa * deltat), 2, 2),
  ##               c(2, 2, 1))

  ##   dt <- matrix(c((mu - 1/2 * sigmaS^2) * deltat,                      ##dt是两行一列
  ##                  kappa * alpha * deltat), 2, 1)

  ##   HHt <- array(matrix(c(sigmaS^2 * deltat,
  ##                         rho * sigmaS * sigmaE * deltat,
  ##                         rho * sigmaS * sigmaE * deltat,
  ##                         sigmaE^2 * deltat),
  ##                       2, 2), c(2, 2, 1))
  ## ------------------------------------------------------------


  ## Measurement equation for the Schwartz two-factor model
  yt <- t(y)                                                ## t()是行列转换

  ct <- t(.A.schwartz2f(kappa = kappa,
                        sigmaS = sigmaS, sigmaE = sigmaE, rho = rho,
                        alphaT = alpha - lambda / kappa,
                        r = r, ttm = ttm))
###################################################################################################################
  ct <- t(.A.schwartz2f(kappa = kappa,
                        sigmaS = sigmaS, sigmaE = sigmaE, sigmaI=sigmaI,
                        rho1 = rho1,rho2 = rho2,rho3 = rho3,
                        alphaT = alpha - lambda / kappa,
                        r = r, ttm = ttm))
###################################################################################################################
  Zt <- array(1, c(d, 2, n))
  Zt[,2,] <- t(.B.schwartz2f(kappa = kappa, ttm = ttm))

  GGt <- array(diag(gg^2, d), c(d, d, 1))

  a0 <- c(x0 ,delta0)
  P0 <- HHt[,,1]

  return(list(a0 = a0, P0 = P0, Tt = Tt, dt = dt, HHt = HHt,
              yt = yt, Zt = Zt, ct = ct, GGt = GGt))
}

##十、模拟
.sim.futures <- function(time, dt, ttm = NA, obj = schwartz2f(), r = 0.03, lambda = 0, sd = 0.01)
{
  n <- time / dt

  traj <- simstate(n, time, obj)
  
  if(any(is.na(ttm))){
    d <- 6
    ttm <- seq(0.2, 2, length = d)
  }else{
    d <- length(ttm)
  }

  ttm.mat <- matrix(ttm, byrow = TRUE, ncol = d, nrow = n)

  price.fut <- function(x, sigmaS, alpha, kappa, sigmaE, rho, lambda, r, ttm)
    {
      pricefutures(ttm, s0 = as.numeric(x[1]), delta0 = x[2], sigmaS = sigmaS,
                   alpha = alpha, kappa = kappa, sigmaE = sigmaE, rho = rho,
                   lambda = lambda, r = r)
    }

  coefs <- coef(obj)
  f.curves <- t(apply(traj, 1, price.fut, sigmaS = coefs$sigmaS,
                      alpha = coefs$alpha, kappa = coefs$kappa, sigmaE = coefs$sigmaE,
                      rho = coefs$rho,  lambda = lambda, r = r, ttm = ttm))

  f.curves <- f.curves * exp(rnorm(prod(dim(f.curves)), sd = sd))
  
  return(list(ttm = ttm.mat, fut = f.curves, traj = traj))
}


.check.lengths <- function(s0, delta0, mu, sigmaS, kappa, alpha,
                           sigmaE, rho, lambda, alphaT, time, ttm, r)
{
  if(!missing(s0)){
    if(length(s0) > 1){
      stop("'s0' must be a scalar!")
    }
  }

  if(!missing(delta0)){
    if(length(delta0) > 1){
      stop("'delta0' must be a scalar!")
    }
  }

  if(!missing(mu)){
    if(length(mu) > 1){
      stop("'mu' must be a scalar!")
    }
  }

  if(!missing(sigmaS)){
    if(length(sigmaS) > 1){
      stop("'sigmaS' must be a scalar!")
    }
  }

  if(!missing(kappa)){
    if(length(kappa) > 1){
      stop("'kappa' must be a scalar!")
    }
  }

  if(!missing(alpha)){
    if(length(alpha) > 1){
      stop("'alpha' must be a scalar!")
    }
  }

  if(!missing(sigmaE)){
    if(length(sigmaE) > 1){
      stop("'sigmaE' must be a scalar!")
    }
  }

  if(!missing(rho)){
    if(length(rho) > 1){
      stop("'rho' must be a scalar!")
    }
  }

  if(!missing(lambda)){
    if(length(lambda) > 1){
      stop("'lambda' must be a scalar!")
    }
  }

  if(!missing(time)){
    if(length(time) > 1){
      stop("'time' must be a scalar!")
    }
  }

  if(!missing(ttm)){
    if(length(ttm) > 1){
      stop("'ttm' must be a scalar!")
    }
  }

  if(!missing(r)){
    if(length(r) > 1){
      stop("'r' must be a scalar!")
    }
  }

  if(!missing(alphaT)){
    if(length(alphaT) > 1){
      stop("'alphaT' must be a scalar!")
    }
  }
}
