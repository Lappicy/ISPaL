# Selecting predictors ########################################################
select.pred <-
  function(lag, which.month, dates, y.reg, x.reg, ...){
    
    # Variaveis internas (...)
    Var.int <- list(...)
    
    # j = month (which.month) to predict
    month.x <- (which.month - lag - 1) %% 12 + 1
    lag.year <- ceiling(abs(min(which.month - 1 - lag, 0))/12)
    
    # Calculate the yreg and xreg
    yreg <- window(y.reg, deltat = 1,
                   start = c(dates[1] + lag.year, which.month),
                   end = c(dates[2], which.month))
    xreg <- window(x.reg, deltat = 1,
                   start = c(dates[1], month.x),
                   end = c(dates[2] - lag.year, month.x))
    
    # What the function returns
    return(list(xreg = xreg, yreg = yreg))
  }


# Select lambda ############################################################ 
select.lambda <- 
  function(formula, data, lambda = seq(0.5, 50, by = 0.05), ...){
    require(pls)
    require(MASS)
    require(stats)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    
    X <- model.matrix(mt, mf)
    X <- X[,colnames(X)!="Intercept"]
    ridge <- lm.ridge(formula, data, lambda = lambda)  
    lambdaopt <- as.numeric(names(which.min(ridge$GCV)))
    lambdaopt
  }


# cross validation for linear regression ######################################
cv1 <-
  function(DATA.1, DATA.2, formula, model.name = ""){
    # Criar objetos que vão tomar valores específicos
    N <- length(DATA.1$y)
    L <- array(NA, N)
    ci.conf <- array(NA, c(N, 2))
    ci.pred <- array(NA, c(N, 2))
    
    # Fazer a regressão linear
    reg <- lm(formula, DATA.1)
    
    # Fazer a previsão na validação com intervalos de confiança e prediction
    L <- predict(reg, DATA.2)
    ci.conf <- predict(reg, DATA.2, interval = c("confidence"))[,2:3]
    ci.pred <- predict(reg, DATA.2, interval = c("prediction"))[,2:3]
    
    # Coeficientes usados no modelo
    Coeficientes.Modelo <- model.coef(coeficientes_modelo = reg$coefficients) 
    
    # Calcular o NSE e KGE com os componentes
    coef.NSE <- NSE(sim = as.numeric(L), obs = as.numeric(DATA.2$y))
    coef.KGE <- KGE(sim = as.numeric(L), obs = as.numeric(DATA.2$y))
    
    # Tabela final com NSE, KGE, componentes e coeficientes
    Tabela.Final.proxy <-
      cbind(data.frame(Model = model.name,
                       CoefB0 = sum(Coeficientes.Modelo[1],
                                    Coeficientes.Modelo[2]),
                       CoefBvazao = Coeficientes.Modelo[3],
                       CoefBX1 = Coeficientes.Modelo[4],
                       CoefBX2 = Coeficientes.Modelo[5],
                       CoefBX3 = Coeficientes.Modelo[6]),
            coef.NSE, coef.KGE)
    
    # Retornar uma lista
    CV <- list()
    CV[[1]] <- L
    CV[[2]] <- ci.conf
    CV[[3]] <- ci.pred
    CV[[4]] <- Tabela.Final.proxy
    CV[[5]] <- AIC(reg, k = length(reg$coefficients))
    
    # Retornar o CV
    return(CV)
  }


# cross validation for ridge regression #######################################
cv.ridge <-
  function(DATA.1, DATA.2, formula, model.name = ""){
    require(pls)
    require(MASS)
    require(glmnet)
    N <- length(DATA.1$y)
    n.pred <- ncol(DATA.1) - 1
    
    lambda <- select.lambda(formula, data = DATA.1,
                            lambda = c(seq(0, 99.95, by = 0.05),
                                       seq(100, 1000, by = 1)))
    aux <- lm.ridge(formula, DATA.1, lambda = lambda)
    beta <- coef(aux)
    
    # R2 fit
    intercp <- rep(1, N)
    X.c <- cbind(intercp, DATA.1[,2:(n.pred+1)])
    L.c <- as.matrix(X.c) %*% as.matrix(beta) 
    r2.fit <- (cor(L.c, DATA.1$y, use = "complete.obs"))^2 # R2 - coef !!!!!!!!!!!
    
    nv <- nrow(DATA.2)
    intercp <- rep(1, nv)
    X <- cbind(intercp, DATA.2[,2:(n.pred+1)])
    
    # Previsões
    L <- as.matrix(X) %*% as.matrix(beta) 
    
    # Coeficientes usados no modelo
    Coeficientes.Modelo <- model.coef(coeficientes_modelo = beta)
    
    # Calcular o NSE e KGE com os componentes
    coef.NSE <- NSE(sim = L, obs = as.numeric(DATA.2$y))
    coef.KGE <- KGE(sim = L, obs = as.numeric(DATA.2$y))
    
    # Tabela final com NSE, KGE, componentes e coeficientes
    Tabela.Final.proxy <-
      cbind(data.frame(Model = model.name,
                       CoefB0 = sum(Coeficientes.Modelo[1],
                                    Coeficientes.Modelo[2]),
                       CoefBvazao = Coeficientes.Modelo[3],
                       CoefBX1 = Coeficientes.Modelo[4],
                       CoefBX2 = Coeficientes.Modelo[5],
                       CoefBX3 = Coeficientes.Modelo[6]),
            coef.NSE, coef.KGE)
    
    # Criar lista de retorno
    CV <- list()
    CV[[1]] <- L
    CV[[2]] <- Tabela.Final.proxy
    CV[[3]] <- lambda
    
    # retornar CV
    CV
}

# Model's coeficients ####
model.coef <-
  function(coeficientes_modelo,
           nomes_coef = c("(Intercept)", "", "x0", "x1", "x2", "x3")){
    
    nome_modelo <- nomes_coef %in% names(coeficientes_modelo)
    nome_final <- rep(NULL, length(nomes_coef))
    
    for(i_nomes in 1:length(nomes_coef)){
      ifelse(nome_modelo[i_nomes],
             nome_final[i_nomes] <-
               coeficientes_modelo[names(coeficientes_modelo) %in%
                                     nomes_coef[i_nomes]],
             nome_final[i_nomes] <- 0)
    }
    
    return(nome_final)
  }

# NASH-SUTCLIFF coeficient and its components ####
NSE <- function(sim, obs, ...){
  
  # Cuidados preliminares ####
  # Se os dados tiverem valores em branco, parar de fazer essa conta
  if(sum(is.na(sim), is.na(obs)) != 0) stop("Valores em branco nos seus dados.")
  
  
  # Componentes "alpha", "beta" e "r" do NSE ####
  # Calcular ALPHA como sendo:
  # a razão do desvio padrão sim e obs
  alpha.NSE <- sd(sim)/sd(obs)
  
  # Calcular BETA como sendo:
  # a razão da diferença da média sim / obs pelo desvio padrão obs
  beta.NSE <- (mean(sim) - mean(obs))/sd(obs)
  
  # Calcular R como sendo:
  # a correlação linear entre as vazões
  ifelse(sd(sim) == 0 | sd(obs) == 0 ,
         r.NSE <- 0,
         r.NSE <- cor(x = sim, y = obs))
  
  
  # Cálculo do NSE pelas componentes seguindo Gupta et al. 2009
  # NSE = 2 * alpha * r - alpha^2 - beta^2
  NSE.valor <- 2 * alpha.NSE * r.NSE - alpha.NSE^2 - beta.NSE^2
  
  
  # NSE sem componentes ####
  NSE.valor.2 <-
    1 -
    (sum((sim - obs)^2))/ #(sum((sim - obs)^2)/length(sim))/
    (sum((obs - mean(obs))^2)) #(sum((obs - mean(obs))^2)/(length(sim)-1))
  
  
  # Criar tabela com todas informações ####
  NSE.tabela <- data.frame(NSE.orig = NSE.valor.2,
                           NSE.comp = NSE.valor,
                           Alpha.NSE = alpha.NSE,
                           Beta.NSE = beta.NSE,
                           r.NSE = r.NSE)
  
  return(NSE.tabela)
}


# KLING-GUPTA coeficient and its components ####
KGE <- function(sim, obs, ...){
  
  # Cuidados preliminares ####
  # Se os dados tiverem valores em branco, parar de fazer essa conta
  if(sum(is.na(sim), is.na(obs)) != 0) stop("Valores em branco nos seus dados.")
  
  
  # Componentes "alpha", "beta" e "r" do KGE ####
  # Calcular KGE como sendo:
  # a razão do desvio padrão sim e obs
  alpha.KGE <- sd(sim)/sd(obs)
  
  # Calcular BETA como sendo:
  # a razão da diferença da média sim / obs pelo desvio padrão obs
  beta.KGE <- mean(sim)/mean(obs)
  
  # Calcular R como sendo:
  # a correlação linear entre as vazões
  ifelse(sd(sim) == 0 | sd(obs) == 0,
         r.KGE <- 0,
         r.KGE <- cor(x = sim, y = obs))
  
  
  # Cálculo do KGE pelas componentes seguindo Gupta et al. 2009
  # KGE = raiz quadrada das somas das componentes menos 1 ao quadrado
  KGE.valor <- 1 - sqrt((r.KGE - 1)^2 + (alpha.KGE - 1)^2 + (beta.KGE - 1)^2)
  
  
  # Criar tabela com todas informações ####
  KGE.tabela <- data.frame(KGE.orig = KGE.valor,
                           Alpha.KGE = alpha.KGE,
                           Beta.KGE = beta.KGE,
                           r.KGE = r.KGE)
  
  return(KGE.tabela)
}
