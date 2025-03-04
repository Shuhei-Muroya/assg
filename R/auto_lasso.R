#'Lasso Regression using the better package lars or glmnet with considering its computation time
#'
#' This function automatically performs Lasso regression using either lars or glmnet,
#' depending on the specified computation time threshold (`T_hope`). It selects the best
#' model based on cross-validation and can optionally make predictions on new data.
#'
#'
#' @param X A matrix or data frame of predictor variables.
#' @param y A numeric vector of response variables.
#' @param new_x Optional. A matrix or data frame for prediction. If provided, predictions will be made for `new_x`.
#' @param size An integer specifying the number of samples for the glmnet lambda search. Default is 1000.
#' @param T_hope A numeric value specifying the computation time threshold to decide between lars and glmnet. Default is 20.
#' @param seed An integer specifying the random seed for reproducibility. Default is 1.
#' @param message Logical. If `TRUE`, progress messages will be displayed. Default is `TRUE`.
#'
#' @return A list containing:
#' \item{method}{A character string indicating the method used ("lars" or "glmnet").}
#' \item{cv_lars}{Cross-validation results from lars (if lars is used).}
#' \item{lars_model}{The lars model object (if lars is used).}
#' \item{best_step}{The optimal step for the lars model (if lars is used).}
#' \item{cv_glmnet}{Cross-validation results from glmnet (if glmnet is used).}
#' \item{glmnet_model}{The glmnet model object (if glmnet is used).}
#' \item{best_lambda}{The optimal lambda value for the glmnet model (if glmnet is used).}
#' \item{coef_cv.min}{The regression coefficients for the best model.}
#' \item{prediction_cv.min}{Predictions for `new_x` (if `new_x` is provided).}
#' \item{settings}{The selected hyperparameters for glmnet (if glmnet is used).}
#'
#' @details
#' The function decides between lars and glmnet based on the computation time threshold (`T_hope`).
#' If `T_hope` is greater than the estimated time for lars, the function uses lars for Lasso regression.
#' Otherwise, glmnet is used. Cross-validation is performed to select the optimal model.
#'
#' @import dplyr
#' @import glmnet
#' @import lars
#' @export
auto_lasso<- function(X,y, new_x=NULL,size = 1000, T_hope = 20, seed=1,message = TRUE,line=TRUE) {
  X<-as.matrix(X)
  y<-as.numeric(y)
  lars_time<-forecast_lars_cptime(X=X,T_hope=T_hope,message=message)
  print(lars_time)
  if(T_hope>lars_time){#lasso by lars
    method<-"lars"

    set.seed(seed)

    cvlars<-cv.lars(X,y,n_lambdaot.it = FALSE,type = "lasso" ,mode ="step",max.step=p)
    lars_model<-lars(X,y,type = "lasso",trace = FALSE)
    beststep<-cvlars$index[which.min(cvlars$cv)]
    coef<-coef(lmodel,mode = "step",s=beststep)

    result_list<-list(method=method,
                      cv_lars=cvlars,
                      lars_model=lars_model,
                      best_step=beststep,
                      coef_cv.min=coef)

    #prediction
    if(!is.null(new_x)){
      new_x<-as.matrix(new_x)
      prediction<-predict(lmodel,newx = new_x,mode = "step",s=beststep)
      result_list<-list(method=method,
                        cv_lars=cvlars,
                        lars_model=lars_model,
                        best_step=beststep,
                        coef_cv.min=coef,
                        prediction_cv.min =prediction)
    }
  }else{##lasso by glmnet
    method<-"glmnet"
    set_list<-auto_settingvalue(X=X,size=size,T_hope=T_hope,seed=seed,message = message,line)


    n_lambda<-round(set_list$hyperparameters$nlambda)
    thresh<-set_list$hyperparameters$thresh

    model1<-glmnet(X,y,family = "gaussian",alpha = 1,intercept=FALSE)
    minn<-min(model1$lambda)
    len_model1_lambda<-length(model1$lambda)
    model1_lambda<-model1$lambda

    lam<-numeric()
    if(n_lambda==0){
      lam<-model1_lambda
    }else if(n_lambda<len_model1_lambda){
      add<-seq(minn,0,by=-(minn/(100-len_model1_lambda)))[-1]

      add<-seq(minn,0,length.out=n_lambda)#[-1]
      lam<-add
    }else{
      add<-seq(minn,0,by=-(minn/(n_lambda-len_model1_lambda)))[-1]
      lam<-c(model1_lambda,add)
    }


    cv_set<-cv.glmnet(X,y,family = "gaussian",alpha = 1,intercept=FALSE,thresh=thresh,lambda=lam)
    glmnet_model<-glmnet(X,y,family = "gaussian",alpha = 1,intercept=FALSE,thresh=thresh,lambda=lam)
    coef_glmnet<-coef(glmnet_model,s=cv_set$lambda.min)
    result_list<-list(method=method,
                      cv_glmnet=cv_set,
                      glmnet_model=glmnet_model,
                      best_lambda=cv_set$lambda.min,
                      coef_cv.min=coef,
                      settings=set_list)

    #predictionまで
    if(!is.null(new_x)){
      new_x<-as.matrix(new_x)
      prediction<-predict(glmnet_model,newx = new_x,s=cv_set$lambda.min)
      result_list$prediction_cv.min<-prediction
    }

  }

  return(result_list)
}
