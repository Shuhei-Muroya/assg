#' Perform Lasso Regression Using lars and glmnet
#'
#' This function performs Lasso regression using both lars and glmnet, with default
#' and custom configurations. It compares computation times and returns the models,
#' cross-validation results, and coefficients.You can check the difference of three ways.
#'
#' @param X A matrix or data frame of predictor variables.
#' @param y A numeric vector of response variables.
#' @param nlambda The number of lambda values for the custom glmnet configuration. Default is 100.
#' @param thresh A numeric value specifying the convergence threshold for glmnet. Default is `1e-7`.
#' @param seed An integer seed for reproducibility. Default is 1.
#'
#' @return A list containing:
#' \item{lars}{A list with lars cross-validation results, the lars model, and coefficients for the optimal model.}
#' \item{glmnet_default}{A list with default glmnet cross-validation results, the glmnet model, and coefficients for the optimal model.}
#' \item{glmnet_set}{A list with custom glmnet cross-validation results, the glmnet model, and coefficients for the optimal model.}
#' \item{time}{A list with computation times for lars, default glmnet, and custom glmnet configurations.}
#'
#' @details
#' This function compares the performance of Lasso regression using lars and glmnet
#' with both default and custom configurations. For glmnet, the function allows customization
#' of the number of lambda values and the convergence threshold.
#'
#' The function calculates the coefficients for the optimal model based on cross-validation results.
#' Additionally, it measures the computation time for each method.
#'
#' @examples
#' \dontrun{
#' # Example data
#' x <- matrix(rnorm(100 * 10), 100, 10)
#' y <- rnorm(100)
#'
#' # Perform Lasso regression
#' result <- all_lasso(X = x, y = y)
#' print(result)
#' }
#'
#' @import glmnet
#' @import lars
#' @export
all_lasso<-function(X,y,nlambda=100,thresh=1e-7,seed=1){
  set.seed(seed)
  y<-as.numeric(y)
  X<-as.matrix(X)
  p<-length(y)

  a<-Sys.time()
  cvlars_p<-cv.lars(X,y,plot.it = FALSE,type = "lasso" ,max.step=p,use.Gram=FALSE)
  lars.model_p<-lars(X,y,use.Gram=FALSE,type = "lasso",trace = FALSE)
  b<-Sys.time()
  time_lars<-b-a
  coef_p<-coef(lars.model_p,mode="lambda",s=cvlars_p$index[which.min(cvlars_p$cv)])

  lars_list<-list(cv_lars=cvlars_p,lars_model=lars.model_p,lars_coef.cvmin=coef_p)
  #デフォルト
  set.seed(seed)
  a<-Sys.time()
  cv_def<-cv.glmnet(X,y,family = "gaussian",alpha = 1)
  model_def<-glmnet(X,y,family = "gaussian",alpha = 1)
  b<-Sys.time()
  time_def<-b-a
  coef_def<-coef(model_def,s=cv_def$lambda.min)
  def_list<-list(def_cv=cv_def,def_model=model_def,def_coef.cvmin=coef_def)
  ##auto
  lam<-numeric()
  #pl<-ncol(X)#増やすlambdaの数
  pl<-nlambda
  t<-thresh
  minn<-min(model_def$lambda)####修正
  len_model1_lambda<-length(model_def$lambda)
  model1_lambda<-model_def$lambda
  #print(model1_lambda)
  if(pl==0){
    lam<-model1_lambda
  }else if(pl<len_model1_lambda){
    add<-seq(minn,0,by=-(minn/(100-len_model1_lambda)))[-1]#100まで増やす必要ない。
    #単に長さplにする
    add<-seq(minn,0,length.out=pl)#[-1]
    lam<-add##ここの修正
  }else{
    add<-seq(minn,0,by=-(minn/(pl-len_model1_lambda)))[-1]#続きを増やしている。
    lam<-c(model1_lambda,add)#増やしたlambdaの候補
  }

  pl_l<-length(lam)
  st<-Sys.time()
  cv_set<-cv.glmnet(X,y,family = "gaussian",alpha = 1,thresh=t,lambda=lam)
  model_set<-glmnet(X,y,family = "gaussian",alpha = 1,thresh=t,lambda=lam)
  gt<-Sys.time()
  time_set<-gt-st
  coef_set<-coef(model_set,s=cv_set$lambda.min)
  set_list<-list(set_cv=cv_set,set_model=model_set,set_coef.cvmin=coef_set)

  time_list<-list(time_lars=time_lars,
                  time_def=time_def,
                  time_set=time_set)
  result<-list(lars=lars_list,
               glmnet_default=def_list,
               glmnet_set=set_list,
               time=time_list)
  return(result)

}

