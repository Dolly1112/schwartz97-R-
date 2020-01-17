setClass("schwartz2f",
         representation(call = "call",                                     
                        s0 = "numeric",
                        delta0 = "numeric",
                        mu = "numeric",
                        sigmaS = "numeric",
                        kappaE = "numeric",
                        alpha = "numeric",
                        sigmaE = "numeric",
                        rhoSE = "numeric"),
         prototype(call = call("schwartz2f"),
                   s0 = 100,
                   delta0 = 0,
                   mu = 0,
                   sigmaS = 0.3,
                   kappaE = 1,
                   alpha = 0,
                   sigmaE = 0.3,
                   rhoSE = 0.5)
         )
##参数定义
##setClass(Class类名, representation, prototype属性的默认值, contains=character()父类，继承关系,
##          validity属性的类型检查, access, where存储空间, version, sealed如果设置TRUE，则同名类不能被再次定义, package所属的包,
##         S3methods = FALSE, slots属性和属性类型)
##数值型 (numeric); 逻辑型 (logical):取TRUE和FALSE两个固定值; 字符型 (character); 矩阵（matrix）          
setClass("schwartz2f.fit",
         representation(n.iter = "numeric",
                        llh = "numeric",
                        converged = "logical",
                        error.code = "numeric",
                        error.message = "character",
                        fitted.params = "logical",
                        trace.pars = "matrix",
                        r = "numeric",
                        alphaT = "numeric",
                        lambdaE = "numeric",
                        meas.sd = "numeric",
                        deltat = "numeric"),
         prototype(n.iter = numeric(0),
                   llh = numeric(0),
                   converged = FALSE,
                   error.code = 0,
                   error.message = character(0),
                   fitted.params = logical(0),
                   trace.pars = matrix(0),
                   r = 0.05,
                   alphaT = 0,
                   lambdaE = 0,
                   meas.sd = 0.01,
                   deltat = 0.01),
         contains = "schwartz2f"
         )
         
         
### <---------------------------------------------------------------------->

schwartz2f <- function(s0 = 100, delta0 = 0,
                       mu = 0.1, sigmaS = 0.3,
                       kappa = 1, alpha = 0, sigmaE = 0.3,
                       rho = 0.5)
{
   call <- match.call()
  ##这个函数会在一般都是写在另外一个函数里边。其会返回其宿主函数体的入参的匹配关系。？？？？？？？？？？？？？？？？？？？？？ 
  ## <------------------------------------------------------------>
  ## Check input
  ## <------------------------------------------------------------>
  
  ## vector
  if(!is.vector(s0)){
    stop("'S0' must be a vector!")
  }
  if(!is.vector(delta0)){
    stop("'delta0' must be a vector!")
  }
  if(!is.vector(mu)){
    stop("'mu' must be a vector!")
  }
  if(!is.vector(sigmaS)){
    stop("'sigmaS' must be a vector!")
  }
  if(!is.vector(kappa)){
    stop("'kappa' must be a vector!")
  }
  if(!is.vector(alpha)){
    stop("'alpha' must be a vector!")
  }
  if(!is.vector(sigmaE)){
    stop("'sigmaE' must be a vector!")
  }
  if(!is.vector(rho)){
    stop("'rho' must be a vector!")
  }
  
   ## finite and numeric:
  if(!is.finite(s0) | !is.numeric(s0)){
    stop("'S0' must be numeric and finite!")
  }
  if(!is.finite(delta0) | !is.numeric(delta0)){
    stop("'delta0' must be numeric and finite!")
  }
  if(!is.finite(mu) | !is.numeric(mu)){
    stop("'mu' must be numeric and finite!")
  }
  if(!is.finite(sigmaS) | !is.numeric(sigmaS)){
    stop("'sigmaS' must be numeric and finite!")
  }
  if(!is.finite(kappa) | !is.numeric(kappa)){
    stop("'kappa' must be numeric and finite!")
  }
  if(!is.finite(alpha) | !is.numeric(alpha)){
    stop("'alpha' must be numeric and finite!")
  }
  if(!is.finite(sigmaE) | !is.numeric(sigmaE)){
    stop("'sigmaE' must be numeric and finite!")
  }
  if(!is.finite(rho) | !is.numeric(rho)){
    stop("'rho' must be numeric and finite!")
  }
         
 ## length:
  if(length(s0) != 1){
    stop("'S0' must have length 1!")
  }
  if(length(delta0) != 1){
    stop("'delta0' must have length 1!")
  }
  if(length(mu) != 1){
    stop("'mu' must have length 1!")
  }
  if(length(sigmaS) != 1){
    stop("'sigmaS' must have length 1!")
  }
  if(length(kappa) != 1){
    stop("'kappa' must have length 1!")
  }
  if(length(alpha) != 1){
    stop("'alpha' must have length 1!")
  }
  if(length(sigmaE) != 1){
    stop("'sigmaE' must have length 1!")
  }
  if(length(rho) != 1){
    stop("'rho' must have length 1!")
  }
         
       ## special cases:
  if(sigmaE <= 0){
    stop("'sigmaE' must be greater than 0!")
  }
  if(sigmaS <= 0){
    stop("'sigmaS' must be greater than 0!")
  }
  if(kappa <= 0){
    stop("'kappa' must be greater than 0!")
  }

  return(new("schwartz2f",
             call = call,
             s0 = unname(s0),
             delta0 = unname(delta0),
             mu = unname(mu),
             sigmaS = unname(sigmaS),
             kappaE = unname(kappa),
             alpha = unname(alpha),
             sigmaE = unname(sigmaE),
             rhoSE = unname(rho)))
}
                      ##> unname(L)
                      ##去掉列表L里的对象名。

### <---------------------------------------------------------------------->
