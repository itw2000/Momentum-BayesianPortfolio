#Backtesting

#tasks to do
#3) Sometimes we are using prices for another stock to that stock.  

#Setup of the code:
#Basically, the code consists of three parts:
#1) The first part is the initialization of the portfolio, given a start cash position
#2) In the start of every month other than the starting month considered in part 1, we have to take the existing position into account. We do this by considering all possible portfolio constructions given the two new stocks and the existing 10 stocks
#3) The third part consists of two subparts. 1) If we are at the last day of the trading period, we just want to unwind our position. 2) We are in the middle of the month, so we are not trading but only doing risk management
#Notice that before we start the day index of each month, we run Ians function to get the stocks that we are going to consider that month
#The first part has the below components
#1) Portfolio allocation 2) Risk management 3) Accounting 4) Updating
#The second part consists of the same parts as part 1, but with the differnce that we in part 2 consider all possible portfolio constructions in the portfolio allocation


####################################################
########Required packages and data function#########
####################################################
library("MASS")
library("alabama")
library("rstan")
library("nloptr")
library("gtools")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source("rsi.R") #Source the rsi functions

####################################################
#################Main function######################
####################################################
backtesting <- function(S0, Number_of_top_stocks, Number_of_top_stocks_start, constant_transaction_costs, start_month_input, end_month_input, year_data, iter_fit, chains, upper_bound, lower_bound, lambda, p, time_horizon, number_of_simulations, perc_VaR, perc_ES, limit_for_changing_position){
  #Initial data
  results <- list()#list containing the results, first element is P/L, second is VaR, third is ES, and fourth is the cash position
  capital <- S0
  date_vector <- 1 #date vector, that has to be concatenated with the other results
  index_storing <- 1 #index telling in what row to store things. Have to do it in this way, since we are using more than one .csv file
  transaction_file <- list() #list containing all the transactions we do
  cumulated_transaction_costs <- 0
  iter_fit_old <- iter_fit
  cash <- S0
  position_old <- rep(0,Number_of_top_stocks_start) #our initial old position; i.e., no stocks
  break_variable <- 0
  position_list <- list()
  
  ####################################################
  ########Defining needed functions and lists#########
  ####################################################
  #Determination of weights
  P_0_inv_fun <- function(m,returns){ #m is the only input, since this is the only thing that changes
    rho_0 <- vector(length=m*(m-1)/2)
    k <- 1
    for (i in 2:m){
      for (j in 1:(i-1)){
        rho_0[k] <- cor(returns[,i],returns[,j])
        k <- k + 1
      }
    }
    rho_0 <- mean(rho_0)
    P_0 <- diag(m)
    P_0[lower.tri(P_0)] <- rho_0
    P_0[upper.tri(P_0)] <- rho_0
    P_0_inv <- solve(P_0)
    P_0_inv
  }
  #Weight optimization
  eval_f <- function( w ) {
    return( list( "objective" = -(t(w)%*%mu-lambda/2*t(w)%*%Sigma%*%w),
                  "gradient" = c( -mu[1]+lambda*(Sigma[1,1]*w[1]+Sigma[1,2]*w[2]+Sigma[1,3]*w[3]+Sigma[1,4]*w[4]+Sigma[1,5]*w[5]+Sigma[1,6]*w[6]+Sigma[1,7]*w[7]+Sigma[1,8]*w[8]+Sigma[1,9]*w[9]+Sigma[1,10]*w[10]),
                                  -mu[2]+lambda*(Sigma[2,1]*w[1]+Sigma[2,2]*w[2]+Sigma[2,3]*w[3]+Sigma[2,4]*w[4]+Sigma[2,5]*w[5]+Sigma[2,6]*w[6]+Sigma[2,7]*w[7]+Sigma[2,8]*w[8]+Sigma[2,9]*w[9]+Sigma[2,10]*w[10]),
                                  -mu[3]+lambda*(Sigma[3,1]*w[1]+Sigma[3,2]*w[2]+Sigma[3,3]*w[3]+Sigma[3,4]*w[4]+Sigma[3,5]*w[5]+Sigma[3,6]*w[6]+Sigma[3,7]*w[7]+Sigma[3,8]*w[8]+Sigma[3,9]*w[9]+Sigma[3,10]*w[10]),
                                  -mu[4]+lambda*(Sigma[4,1]*w[1]+Sigma[4,2]*w[2]+Sigma[4,3]*w[3]+Sigma[4,4]*w[4]+Sigma[4,5]*w[5]+Sigma[4,6]*w[6]+Sigma[4,7]*w[7]+Sigma[4,8]*w[8]+Sigma[4,9]*w[9]+Sigma[4,10]*w[10]),
                                  -mu[5]+lambda*(Sigma[5,1]*w[1]+Sigma[5,2]*w[2]+Sigma[5,3]*w[3]+Sigma[5,4]*w[4]+Sigma[5,5]*w[5]+Sigma[5,6]*w[6]+Sigma[5,7]*w[7]+Sigma[5,8]*w[8]+Sigma[5,9]*w[9]+Sigma[5,10]*w[10]),
                                  -mu[6]+lambda*(Sigma[6,1]*w[1]+Sigma[6,2]*w[2]+Sigma[6,3]*w[3]+Sigma[6,4]*w[4]+Sigma[6,5]*w[5]+Sigma[6,6]*w[6]+Sigma[6,7]*w[7]+Sigma[6,8]*w[8]+Sigma[6,9]*w[9]+Sigma[6,10]*w[10]),
                                  -mu[7]+lambda*(Sigma[7,1]*w[1]+Sigma[7,2]*w[2]+Sigma[7,3]*w[3]+Sigma[7,4]*w[4]+Sigma[7,5]*w[5]+Sigma[7,6]*w[6]+Sigma[7,7]*w[7]+Sigma[7,8]*w[8]+Sigma[7,9]*w[9]+Sigma[7,10]*w[10]),
                                  -mu[8]+lambda*(Sigma[8,1]*w[1]+Sigma[8,2]*w[2]+Sigma[8,3]*w[3]+Sigma[8,4]*w[4]+Sigma[8,5]*w[5]+Sigma[8,6]*w[6]+Sigma[8,7]*w[7]+Sigma[8,8]*w[8]+Sigma[8,9]*w[9]+Sigma[8,10]*w[10]),
                                  -mu[9]+lambda*(Sigma[9,1]*w[1]+Sigma[9,2]*w[2]+Sigma[9,3]*w[3]+Sigma[9,4]*w[4]+Sigma[9,5]*w[5]+Sigma[9,6]*w[6]+Sigma[9,7]*w[7]+Sigma[9,8]*w[8]+Sigma[9,9]*w[9]+Sigma[9,10]*w[10]),
                                  -mu[10]+lambda*(Sigma[10,1]*w[1]+Sigma[10,2]*w[2]+Sigma[10,3]*w[3]+Sigma[10,4]*w[4]+Sigma[10,5]*w[5]+Sigma[10,6]*w[6]+Sigma[10,7]*w[7]+Sigma[10,8]*w[8]+Sigma[10,9]*w[9]+Sigma[10,10]*w[10])
                  ) ) )
  }
  # constraint functions
  # equalities
  eval_g_eq <- function( w ) {
    constr <- c( sum(w) - 1 )
    grad   <- rep(1,10)
    return( list( "constraints"=constr, "jacobian"=grad ) )
  }
  local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                      "xtol_rel"  = .Machine$double.eps )
  opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
                "xtol_rel"  = .Machine$double.eps, 
                "maxeval"   = 2000,
                "local_opts" = local_opts )
  
  mu_Sigma_Bayes <- function(){ #extracting mu and Sigma from the fit object from Rstan and annualizing
    mu <- vector(length=m)
    for (i in 1:m){
      mu[i] <- summary(fit)$summary[i,1]
    }
    Sigma <- matrix(nrow=m,ncol=m)
    for (i in 1:m){
      for (j in 1:m){
        Sigma[i,j] <- summary(fit)$summary[j+i*m,1]
      }
    }
    #Annualizing mu and Sigma 
    mu <- mu*252
    Sigma <- Sigma*252
    list(mu,Sigma)
  }
  
  #accounting. In other words, we are using this function to determine how much capital we need to invest in order to not have a negative cash position and also use all we the cash we have in order to take the new position
  accounting <- function(x){
    cap_alloc <- x*capital*weights #capital allocated to each stock
    position <- floor(cap_alloc/(current_stock_prices)) #In this way, we just estimate our transaction costs by two times the constant_transaction_costs
    temp_trans <- transaction_costs_fun(position)
    
    #If we have an already existing position, then we are not allowing that the new position plus the costs by taking the new position are not allowed to exceed the capital we have. In other words, we do not allow to lend money to buy stocks for
    capital - current_stock_prices%*%position - temp_trans #sum(cap_alloc)
  }
  
  #Function to calculate the transaction costs in the accounting function
  transaction_costs_fun <- function(position){
    trans_temp <- 0
    
    #Accounting
    sell_old_portfolio <- position_old
    for (sell_buy_index_temp in 1:Number_of_top_stocks_start){
      if (any(names(position[sell_buy_index_temp])==names(position_old))){ #Then we already have it in our portfolio
        sell_old_portfolio[which(names(position[sell_buy_index_temp])==names(position_old))] <- 0
        if (position[sell_buy_index_temp]>position_old[which(names(position[sell_buy_index_temp])==names(position_old))]){ #Then it is a buy
          trans_temp <- trans_temp + (position[sell_buy_index_temp]-position_old[which(names(position[sell_buy_index_temp])==names(position_old))])*current_stock_prices[sell_buy_index_temp] * constant_transaction_costs
        }
        else{
          trans_temp <- trans_temp - (position[sell_buy_index_temp]-position_old[which(names(position[sell_buy_index_temp])==names(position_old))])*current_stock_prices[sell_buy_index_temp] * constant_transaction_costs
        }
      }
      else{
        trans_temp <- trans_temp + (position[sell_buy_index_temp])*current_stock_prices[sell_buy_index_temp] * constant_transaction_costs #Then it is a buy
      }
    }
    #then we gotta take care of those stocks from the old portfolio that we completely unwind
    if (any(sell_old_portfolio>0)){
      for (i_temp_ind in 1:length(which(sell_old_portfolio>0))){
        trans_temp <- trans_temp + sell_old_portfolio[which(sell_old_portfolio>0)][i_temp_ind] * as.numeric(as.character(unlist(data[start_est-index,c(1:ncol(data))[which(names(which(sell_old_portfolio!=0))[i_temp_ind]==names(data))]]))) * constant_transaction_costs
      }
    }
    trans_temp
  }
  
  #Function to calculate the gain or loss in the cash position
  cash_fun <- function(position){
    cash_temp <- 0
    
    #Accounting
    sell_old_portfolio <- position_old
    for (sell_buy_index_temp in 1:Number_of_top_stocks_start){
      if (any(names(position[sell_buy_index_temp])==names(position_old))){ #Then we already have it in our portfolio
        sell_old_portfolio[which(names(position[sell_buy_index_temp])==names(position_old))] <- 0
        if (position[sell_buy_index_temp]>position_old[which(names(position[sell_buy_index_temp])==names(position_old))]){ #Then it is a buy
          cash_temp <- cash_temp - (position[sell_buy_index_temp]-position_old[which(names(position[sell_buy_index_temp])==names(position_old))])*current_stock_prices[sell_buy_index_temp] * (1+constant_transaction_costs)
        }
        else{
          cash_temp <- cash_temp - (position[sell_buy_index_temp]-position_old[which(names(position[sell_buy_index_temp])==names(position_old))])*current_stock_prices[sell_buy_index_temp] * (1-constant_transaction_costs)
        }
      }
      else{
        cash_temp <- cash_temp - (position[sell_buy_index_temp])*current_stock_prices[sell_buy_index_temp] * (1+constant_transaction_costs) #Then it is a buy
      }
    }
    #then we gotta take care of those stocks from the old portfolio that we completely unwind
    if (any(sell_old_portfolio>0)){
      for (i_temp_ind in 1:length(which(sell_old_portfolio>0))){
        cash_temp <- cash_temp + sell_old_portfolio[which(sell_old_portfolio>0)][i_temp_ind] * as.numeric(as.character(unlist(data[start_est-index,c(1:ncol(data))[which(names(which(sell_old_portfolio!=0))[i_temp_ind]==names(data))]]))) * (1-constant_transaction_costs)
      }
    }
    cash_temp
  }
  
  
  
  
  #Risk managemet
  closed_form_GBM <- function(mu,sigma,S0,t,ran_var){
    S0*exp((mu-sigma^2/2)*t+sigma*sqrt(t)*ran_var) 
  }
  risk_management <- function(current_stock_prices, position, covariance_matrix, mean_vector, p, time_horizon, number_of_simulations){ #the p VaR is the p percentile of the loss distribution, meaning that we, if p=0.99, do not expect to exceed this amount more than 1 in 100
    rand_numbers <- mvrnorm(number_of_simulations, rep(0,length(current_stock_prices)), covariance_matrix)
    for (i in 1:length(current_stock_prices)){ #standardizing the draws
      rand_numbers[,i] <- rand_numbers[,i]/sqrt(covariance_matrix[i,i])
    }
    sim_vec <- vector(length=number_of_simulations)
    initial_position <- current_stock_prices%*%position
    for (i in 1:number_of_simulations){
      sim_vec[i] <- initial_position - position%*%closed_form_GBM(mu=mean_vector,sigma=sqrt(diag(covariance_matrix)),S0=current_stock_prices,t=time_horizon,ran_var=rand_numbers[i,])
    }
    VaR <- quantile(sim_vec,p) #VaR
    ES <- mean(sim_vec[which(sim_vec>VaR)]) #expected shortfall
    list("VaR"=VaR, "ES"=ES)
  }
  risk_reduction <- function(){
    if(VaR>perc_VaR*capital){
      perc_redu <- perc_VaR*capital/VaR #percentage we have to reduce the allocated capital
      position <- floor(perc_redu*position) #the position, as an integer
      risk <- risk_management(current_stock_prices, position, covariance_matrix=Sigma_estimate, mean_vector=mu_estimate, p, time_horizon, number_of_simulations)
      VaR <- risk[[1]]
      ES <- risk[[2]]
    }
    if(ES>perc_ES*capital){
      perc_redu <- perc_ES*capital/ES #percentage we have to reduce the allocated capital
      position <- floor(perc_redu*position) #the position, as an integer
      risk <- risk_management(current_stock_prices, position, covariance_matrix=Sigma_estimate, mean_vector=mu_estimate, p, time_horizon, number_of_simulations)
      VaR <- risk[[1]]
      ES <- risk[[2]]
    }
    cap_alloc <- current_stock_prices*position
    list(cap_alloc,position,VaR,ES)
  }
  
  ####################################################
  #############Start of the calculations##############
  ####################################################
  for (index_year in 1:length(year_data)){ #looping through the years
    if(break_variable==1){
      print("break")
      break
    }
    start_month <- ifelse(index_year==1, start_month_input, 1)
    end_month <- ifelse(index_year==length(year_data), end_month_input, 12)
    for (index_monthly in start_month:end_month){ # looping through the months
      if(break_variable==1){
        print("break")
        break
      }
      print(year_data[index_year])
      print(index_monthly)
      ####################################################
      ####################Data preparation################
      ####################################################
      if(index_monthly==start_month & index_year==1){ #Then we have to find Number_of_top_stocks_start stocks
        monthlyData <- stockPrice(singledate, ticker, year_data[index_year], index_monthly,Number_of_top_stocks_start)
        data <- rbind(monthlyData[[2]],monthlyData[[1]])
        #data <- cbind(data[,1][!duplicated(data[,2:ncol(data)])],unique(data[,2:ncol(data)]))
        start_est <- which(format(as.Date(data[,1]), "%m")!=format(as.Date(data[1,1]), "%m"))[1] #the start index in the data frame for the estimation period
        est_data <- data[start_est:nrow(data),] #the data used for estimation
        m <- ncol(est_data)-1 #number of stocks
        N <- nrow(est_data) - 1#number of return observations per stock
        returns <- matrix(nrow=nrow(data)-1,ncol=ncol(data)-1)
        for (i in 1:(ncol(data)-1)){
          returns[,i] <- -diff(log(as.numeric(data[,i+1])))  
        }
      }
      else{ #Then we have to find Number_of_top_stocks+Number_of_top_stocks_start stocks
        monthlyData <- stockPrice(singledate, ticker, year_data[index_year], index_monthly,Number_of_top_stocks,optionalData=TRUE, extraStocks=names(position_old))
        data <- rbind(monthlyData[[2]],monthlyData[[1]])
        #data <- cbind(data[,1][!duplicated(data[,2:ncol(data)])],unique(data[,2:ncol(data)]))
        start_est <- which(format(as.Date(data[,1]), "%m")!=format(as.Date(data[1,1]), "%m"))[1] #the start index in the data frame for the estimation period
        est_data <- data[start_est:nrow(data),] #the data used for estimation
        m <- ncol(est_data)-1 #number of stocks
        N <- nrow(est_data) - 1#number of return observations per stock
        returns <- matrix(nrow=nrow(data)-1,ncol=ncol(data)-1)
        for (i in 1:(ncol(data)-1)){
          returns[,i] <- -diff(log(as.numeric(data[,i+1])))  
        }
      }
      
      
      
      #Generation of the date vector, that has to be concatenated with the other results
      date_vector <- c(date_vector,format(as.Date(rev(data[1:(start_est-1),1])), "%Y-%m-%d"))
      
      for (index in 1:(start_est-1)){
        if (index==1){ #Then we have to re-allocate
          if (!any(position_old!=0)){ #If this is not the case, we have to take the existing position into account. In other words, re-allocation.
            ####################################################
            ######################Part 1########################
            ####################################################
            
            ############Portfolio allocation############
            #Calculation of the average of the pairwise correlation
            for (P_0_inv_check in 1:Number_of_top_stocks_start){ #If a stock doesnt move, then we will have problems simulating from a covariance matrix with a row and column full of zeros. Thus, we change these to .Machine$double.eps
              if(sum(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),P_0_inv_check]^2) ==0){
                returns[(start_est-(index-1)):(start_est-(index-1)+N-1),P_0_inv_check][seq(1,length(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),P_0_inv_check]),2)] <- .Machine$double.eps
              }
            }
            P_0_inv <- P_0_inv_fun(m,returns[(start_est-(index-1)):(start_est-(index-1)+N-1),])
            
            fit <- stan("stan_file.stan", data=list(N=N, m=m, y=returns[(start_est-(index-1)):(start_est-(index-1)+N-1),], P_0_inv=P_0_inv, 
                                                    epsilon1=0.0001, epsilon2=0.0001, vec1=rep(1,m), kappa_0=0.1*N), iter=iter_fit, chains=chains) #it seems that we got convergence by using approximately 500 iterations per chain
            iter_increase <- 1
            while(summary(fit)$summary[nrow(summary(fit)$summary),ncol(summary(fit)$summary)]>1.1 | summary(fit)$summary[1,ncol(summary(fit)$summary)]>1.1){ #if Rhat<1.1 for lp or the first mu, we increase the number of iterations per chain by 200. We continue doing this till convergence is reached
              print("Rhat too high. Trying with more iterations.")
              iter_fit <- iter_fit + iter_increase*200
              fit <- stan(fit=fit, data=list(N=N, m=m, y=returns[(start_est-(index-1)):(start_est-(index-1)+N-1),], P_0_inv=P_0_inv, 
                                             epsilon1=0.0001, epsilon2=0.0001, vec1=rep(1,m), kappa_0=0.1*N), iter=iter_fit, chains=chains)
              iter_increase <- iter_increase + 1
            }
            iter_fit <- iter_fit_old
            #Optimization part
            #Extracting the right parameters
            mu <- mu_Sigma_Bayes()[[1]]
            Sigma <- mu_Sigma_Bayes()[[2]]
            #Determination of the weights
            weights <- nloptr( x0=rep(0.1,10),
                               eval_f=eval_f,
                               lb=lower_bound,
                               ub=upper_bound,
                               eval_g_eq=eval_g_eq,
                               opts=opts)$solution
            
            if (sum(weights)-1>0.00000001 | 1-sum(weights)>0.00000001){
              warning("Sum of weights>1")
              break_variable <- 1
              break
            }
            
            current_stock_prices <- unlist(data[start_est-index+1,2:ncol(data)]) #the prices of the stocks we want to buy
            #Determination of the percentage of the capital we are gonna invest, in order to have a zero cash position
            perc_accounting <- uniroot(accounting,interval=c(0,10), tol=.Machine$double.eps)$root 
            cap_alloc <- perc_accounting*capital*weights #capital allocated to each stock
            position <- floor(cap_alloc/(current_stock_prices))
            
            #################Risk management####################
            #Estimation of the covariance matrix and the mu in a multi-dimensional GBM
            Sigma_estimate <- cov(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),])*252 #annualized covariance matrix
            for (sigma_check in 1:Number_of_top_stocks_start){ #If a stock doesnt move, then we will have problems simulating from a covariance matrix with a row and column full of zeros. Thus, we change these to .Machine$double.eps
              if(sum(Sigma_estimate[,sigma_check]^2) ==0){
                Sigma_estimate[,sigma_check] <- .Machine$double.eps
                Sigma_estimate[sigma_check,] <- .Machine$double.eps
              }
            }
            mu_estimate <- colMeans(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),])*252+diag(Sigma_estimate)/2 #annualized mu estimate in a GBM
            risk <- risk_management(current_stock_prices, position, covariance_matrix=Sigma_estimate, mean_vector=mu_estimate, p, time_horizon, number_of_simulations)
            VaR <- risk[[1]]
            ES <- risk[[2]]
            risk_red <- risk_reduction() #reducing risk, if VaR and/or ES are too high
            cap_alloc <- risk_red[[1]]
            position <- risk_red[[2]]
            names(position) <- names(current_stock_prices)
            VaR <- risk_red[[3]]
            ES <- risk_red[[4]]
            
            #################Accounting####################
            #UPDATE TO THE CLOSING PRICES FOR THE TRADING DAY
            current_stock_prices <- unlist(data[start_est-index,2:ncol(data)]) #the prices of the stocks we want to buy
            #Accounting
            cash <- cash_fun(position) + cash
            capital <- current_stock_prices%*%position + cash
            cumulated_transaction_costs <- cumulated_transaction_costs + transaction_costs_fun(position)
            #Keeping track of transactions #Be aware that the below calculations are different from what we do, when we have to re-allocate, since we then have to take the old position into account
            trans_row <- ifelse(length(nrow(do.call(rbind, transaction_file)))==0,0,nrow(do.call(rbind, transaction_file)))
            trans_temp <- 0
            for (trans_index in 1:m){
              if(position[trans_index]>0){
                transaction_file[[trans_row+trans_index-trans_temp]] <- as.character(data[start_est-index,1]) #date where the trading happens
                transaction_file[[trans_row+trans_index-trans_temp]][2] <- "buy" 
                transaction_file[[trans_row+trans_index-trans_temp]][3] <- colnames(data)[trans_index+1] #ticker
                transaction_file[[trans_row+trans_index-trans_temp]][4] <- position[trans_index]
                transaction_file[[trans_row+trans_index-trans_temp]][5] <- data[start_est-index,(trans_index+1)] #PRICE
              }
              else{
                trans_temp <- trans_temp + 1 
              }
            }
            
            #################Updating####################
            position_list[[index_storing]] <- position
            position_old <- position
            weights_old <- weights
            cap_alloc_old <- cap_alloc
            results[[index_storing]] <- capital - S0
            results[[index_storing]][2] <- VaR
            results[[index_storing]][3] <- ES
            results[[index_storing]][4] <- cash
            results[[index_storing]][5] <- current_stock_prices%*%position
            results[[index_storing]][6] <- capital
            results[[index_storing]][7] <- cumulated_transaction_costs
            
          }
          else{ #Then we have to take the existing position into account. In other words, re-allocation.
            ####################################################
            ######################Part 2########################
            ####################################################
            
            ############Portfolio allocation############
            #Calculation of the average of the pairwise correlation
            for (P_0_inv_check in 1:(Number_of_top_stocks_start+Number_of_top_stocks)){ #If a stock doesnt move, then we will have problems simulating from a covariance matrix with a row and column full of zeros. Thus, we change these to .Machine$double.eps
              if(sum(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),P_0_inv_check]^2) ==0){
                returns[(start_est-(index-1)):(start_est-(index-1)+N-1),P_0_inv_check][seq(1,length(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),P_0_inv_check]),2)] <- .Machine$double.eps
              }
            }
            P_0_inv <- P_0_inv_fun(m,returns[(start_est-(index-1)):(start_est-(index-1)+N-1),])
            fit <- stan(fit=fit, data=list(N=N, m=m, y=returns[(start_est-(index-1)):(start_est-(index-1)+N-1),], P_0_inv=P_0_inv, 
                                           epsilon1=0.0001, epsilon2=0.0001, vec1=rep(1,m), kappa_0=0.1*N), iter=iter_fit, chains=chains) #it seems that we got convergence by using approximately 500 iterations per chain
            iter_increase <- 1
            while(summary(fit)$summary[nrow(summary(fit)$summary),ncol(summary(fit)$summary)]>1.1 | summary(fit)$summary[1,ncol(summary(fit)$summary)]>1.1){ #if Rhat<1.1 for lp or the first mu, we increase the number of iterations per chain by 200. We continue doing this till convergence is reached
              print("Rhat too high. Trying with more iterations.")
              iter_fit <- iter_fit + iter_increase*200
              fit <- stan(fit=fit, data=list(N=N, m=m, y=returns[(start_est-(index-1)):(start_est-(index-1)+N-1),], P_0_inv=P_0_inv, 
                                             epsilon1=0.0001, epsilon2=0.0001, vec1=rep(1,m), kappa_0=0.1*N), iter=iter_fit, chains=chains)
              iter_increase <- iter_increase + 1
            }
            iter_fit <- iter_fit_old
            #Extracting the right parameters
            mu_old <- mu_Sigma_Bayes()[[1]]
            Sigma_old <- mu_Sigma_Bayes()[[2]]
            
            #At this point, we have to calculate all the different possibilities and choose the most profitable
            profit_vec <- vector(length=choose(Number_of_top_stocks+Number_of_top_stocks_start,Number_of_top_stocks_start))
            transactions_vec <- vector(length=choose(Number_of_top_stocks+Number_of_top_stocks_start,Number_of_top_stocks_start))
            index_choose_storing <- combinations(Number_of_top_stocks+Number_of_top_stocks_start,Number_of_top_stocks) #by choosing the row number that corresponds to the optimal value in profit_vec, we will be able to re-calculate the position outside the while loop
            counter_choose <- 0
            while (counter_choose<(length(profit_vec))){
              index_choose <- index_choose_storing[counter_choose+1,]
              mu <- mu_old[-index_choose]
              Sigma <- Sigma_old[-index_choose,-index_choose]
              
              #Determination of the weights
              weights <- nloptr( x0=rep(0.1,10),
                                 eval_f=eval_f,
                                 lb=lower_bound,
                                 ub=upper_bound,
                                 eval_g_eq=eval_g_eq,
                                 opts=opts)$solution
              
              if (sum(weights)-1>0.00000001){
                warning("Sum of weights>1")
              }
              current_stock_prices <- unlist(data[start_est-index+1,c(2:ncol(data))[-index_choose]]) #the prices of the stocks we want to buy
              #Determination of the percentage of the capital we are gonna invest, taking into account the possible gains from sells, and loss from buys, and also transaction costs
              perc_accounting <- uniroot(accounting,interval=c(0,10), tol=.Machine$double.eps)$root
              cap_alloc <- perc_accounting*capital*weights #capital allocated to each stock
              position <- floor(cap_alloc/(current_stock_prices))
              
              #Risk management
              #Estimation of the covariance matrix and the mu in a multi-dimensional GBM
              Sigma_estimate <- cov(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),-index_choose])*252 #annualized covariance matrix
              for (sigma_check in 1:Number_of_top_stocks_start){ #If a stock doesnt move, then we will have problems simulating from a covariance matrix with a row and column full of zeros. Thus, we change these to .Machine$double.eps
                if(sum(Sigma_estimate[,sigma_check]^2) ==0){
                  Sigma_estimate[,sigma_check] <- .Machine$double.eps
                  Sigma_estimate[sigma_check,] <- .Machine$double.eps
                }
              }
              mu_estimate <- colMeans(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),-index_choose])*252+diag(Sigma_estimate)/2 #annualized mu estimate in a GBM
              risk <- risk_management(current_stock_prices, position, covariance_matrix=Sigma_estimate, mean_vector=mu_estimate, p, time_horizon, number_of_simulations)
              VaR <- risk[[1]]
              ES <- risk[[2]]
              risk_red <- risk_reduction() #reducing risk, if VaR and/or ES are too high
              cap_alloc <- risk_red[[1]]
              position <- risk_red[[2]]
              names(position) <- names(current_stock_prices)
              transaction_costs_temp <- transaction_costs_fun(position)
              
              #Using the posterior predictive distribution to forecast the following month, and incorporating that in our portfolio re-allocation problem
              predictive_posterior <- matrix(ncol=Number_of_top_stocks_start,nrow=iter_fit*chains/2)
              extract_fit_mu <- extract(fit)$mu
              extract_fit_Sigma <- extract(fit)$Sigma
              mu_sim <- vector(length=Number_of_top_stocks_start)
              Sigma_sim <- matrix(nrow=Number_of_top_stocks_start,ncol=Number_of_top_stocks_start)
              for(i_extract in 1:(iter_fit*chains/2)){
                mu_sim <- extract_fit_mu[i_extract,-index_choose]*21
                Sigma_sim <- extract_fit_Sigma[i_extract,-index_choose,-index_choose]*21
                predictive_posterior[i_extract,] <- mvrnorm(1, mu_sim, Sigma_sim)
              }
              expected_returns <- colMeans(predictive_posterior)  
              
              #Calculating the expected profit of taking that position, and subtracting the transaction costs of taking the position
              profit_vec[counter_choose+1] <- sum(position*current_stock_prices*expected_returns)
              transactions_vec[counter_choose+1] <- transaction_costs_temp 
              
              counter_choose <- counter_choose + 1
            }
            
            #Determine whether we change position or not
            max_row <- which(profit_vec-transactions_vec==max(profit_vec-transactions_vec))[1]
            if(profit_vec[max_row]/transactions_vec[max_row]>limit_for_changing_position){
              index_choose <- index_choose_storing[which(profit_vec-transactions_vec==max(profit_vec-transactions_vec))[1],]
              mu <- mu_old[-index_choose]
              Sigma <- Sigma_old[-index_choose,-index_choose]
              
              #Determination of the weights
              weights <- nloptr( x0=rep(0.1,10),
                                 eval_f=eval_f,
                                 lb=lower_bound,
                                 ub=upper_bound,
                                 eval_g_eq=eval_g_eq,
                                 opts=opts)$solution
              
              if (sum(weights)-1>0.00000001 | 1-sum(weights)-1>0.00000001){
                warning("Sum of weights!=1")
                break_variable <- 1
                break
              }
              current_stock_prices <- unlist(data[start_est-index+1,c(2:ncol(data))[-index_choose]]) #the prices of the stocks we want to buy
              #Determination of the percentage of the capital we are gonna invest, taking into account the possible gains from sells, and loss from buys, and also transaction costs
              perc_accounting <- uniroot(accounting,interval=c(0,10), tol=.Machine$double.eps)$root
              cap_alloc <- perc_accounting*capital*weights #capital allocated to each stock
              position <- floor(cap_alloc/(current_stock_prices))
            }
            else{
              index_choose <- c(1:Number_of_top_stocks)
              current_stock_prices <- unlist(data[start_est-index+1,c(2:ncol(data))[-index_choose]]) #the prices of the stocks we want to buy
              position <- position_old
              weights <- weights_old
              cap_alloc <- cap_alloc_old
            }
            
            #################Risk management####################
            #Estimation of the covariance matrix and the mu in a multi-dimensional GBM
            Sigma_estimate <- cov(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),-index_choose])*252 #annualized covariance matrix
            for (sigma_check in 1:Number_of_top_stocks_start){ #If a stock doesnt move, then we will have problems simulating from a covariance matrix with a row and column full of zeros. Thus, we change these to .Machine$double.eps
              if(sum(Sigma_estimate[,sigma_check]^2) ==0){
                Sigma_estimate[,sigma_check] <- .Machine$double.eps
                Sigma_estimate[sigma_check,] <- .Machine$double.eps
              }
            }
            mu_estimate <- colMeans(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),-index_choose])*252+diag(Sigma_estimate)/2 #annualized mu estimate in a GBM
            risk <- risk_management(current_stock_prices, position, covariance_matrix=Sigma_estimate, mean_vector=mu_estimate, p, time_horizon, number_of_simulations)
            VaR <- risk[[1]]
            ES <- risk[[2]]
            risk_red <- risk_reduction() #reducing risk, if VaR and/or ES are too high
            cap_alloc <- risk_red[[1]]
            position <- risk_red[[2]]
            names(position) <- names(current_stock_prices)
            VaR <- risk_red[[3]]
            ES <- risk_red[[4]]
            
            ##############Accounting##############
            #UPDATE TO THE CLOSING PRICES FOR THE TRADING DAY
            current_stock_prices <- unlist(data[start_est-index,c(2:ncol(data))[-index_choose]])
            #Accounting
            if((cash_fun(position) + cash+current_stock_prices%*%position>capital*1.1) | ((cash_fun(position) + cash+current_stock_prices%*%position)*1.2<capital)){
              break_variable <- 1
              break
            }
            cash <- cash_fun(position) + cash
            capital <- current_stock_prices%*%position + cash
            cumulated_transaction_costs <- cumulated_transaction_costs + transaction_costs_fun(position)
            
            #Keeping track of transactions due to either buy of new stocks or adjustment of existing stocks
            trans_row <- ifelse(length(nrow(do.call(rbind, transaction_file)))==0,0,nrow(do.call(rbind, transaction_file)))
            trans_temp <- 0
            for (trans_index in 1:(m-Number_of_top_stocks)){
              if(any(names(position[trans_index])==names(position_old))){ #then we also have the stock in the old position
                if(position[trans_index]-position_old[which(names(position[trans_index])==names(position_old))]!=0){ #then we made a change to this position
                  adjust_existing_stock <- (position[trans_index]-position_old[which(names(position[trans_index])==names(position_old))]!=0)
                  buy_or_sell <- position[trans_index]-position_old[which(names(position[trans_index])==names(position_old))]
                  transaction_file[[trans_row+trans_index-trans_temp]] <- as.character(data[start_est-index,1]) #date where the trading happens
                  transaction_file[[trans_row+trans_index-trans_temp]][2] <- ifelse(buy_or_sell>0,"buy","sell")  
                  transaction_file[[trans_row+trans_index-trans_temp]][3] <- names(adjust_existing_stock) #ticker
                  transaction_file[[trans_row+trans_index-trans_temp]][4] <- buy_or_sell
                  transaction_file[[trans_row+trans_index-trans_temp]][5] <- data[start_est-index,which(names(adjust_existing_stock)==names(data[start_est-index,]))]
                }
                else{
                  trans_temp <- trans_temp + 1 
                }
              }
              else{
                if(position[trans_index]>0){
                  transaction_file[[trans_row+trans_index-trans_temp]] <- as.character(data[start_est-index,1]) #date where the trading happens
                  transaction_file[[trans_row+trans_index-trans_temp]][2] <- "buy" 
                  transaction_file[[trans_row+trans_index-trans_temp]][3] <- names(position[trans_index]) #ticker
                  transaction_file[[trans_row+trans_index-trans_temp]][4] <- position[trans_index]
                  transaction_file[[trans_row+trans_index-trans_temp]][5] <- data[start_est-index,which(names(position[trans_index])==names(data[start_est-index,]))]
                }
                else{
                  trans_temp <- trans_temp + 1 
                }
              }
            }
            #Keeping track of transactions due to the fact that it is not in our portfolio anymore 
            trans_row <- ifelse(length(nrow(do.call(rbind, transaction_file)))==0,0,nrow(do.call(rbind, transaction_file)))
            trans_temp <- 0
            for (trans_index in 1:(m-Number_of_top_stocks)){
              sold_old_stock <- ((!any(names(position_old[trans_index])==names(position))) & (position_old[trans_index]>0))
              if(sold_old_stock==TRUE){
                transaction_file[[trans_row+trans_index-trans_temp]] <- as.character(data[start_est-index,1]) #date where the trading happens
                transaction_file[[trans_row+trans_index-trans_temp]][2] <- "sell" 
                transaction_file[[trans_row+trans_index-trans_temp]][3] <- names(sold_old_stock) #ticker
                transaction_file[[trans_row+trans_index-trans_temp]][4] <- -position_old[trans_index]
                transaction_file[[trans_row+trans_index-trans_temp]][5] <- data[start_est-index,which(names(sold_old_stock)==names(data[start_est-index,]))]
                
              }
              else{
                trans_temp <- trans_temp + 1 
              }
            }
            
            ##############Updating##############
            position_list[[index_storing]] <- position
            position_old <- position
            weights_old <- weights
            cap_alloc_old <- cap_alloc
            results[[index_storing]] <- capital - results[[index_storing-1]][6] #P/L
            results[[index_storing]][2] <- VaR
            results[[index_storing]][3] <- ES
            results[[index_storing]][4] <- cash
            results[[index_storing]][5] <- current_stock_prices%*%position
            results[[index_storing]][6] <- capital
            results[[index_storing]][7] <- cumulated_transaction_costs
            
            
          }
          
        } 
        else{
          ####################################################
          ######################Part 3########################
          ####################################################
          
          ############Determination whether we are at the last trading day or just doing risk management in the middle of the month############
          if(index_monthly==end_month_input & index_year==length(year_data) & index==(start_est-1)){ #Then we are at the last day in the period, and, thus, we want to unwind the position
            if(index_monthly==start_month_input & index_year==1){
              current_stock_prices <- unlist(data[start_est-index,c(2:ncol(data))]) #the prices of the stocks we want to buy
            }
            else{
              current_stock_prices <- unlist(data[start_est-index,c(2:ncol(data))[-index_choose]]) #the prices of the stocks we want to buy
            }
            capital <- (current_stock_prices*(1-constant_transaction_costs))%*%position + results[[index_storing-1]][4]
            cumulated_transaction_costs <- cumulated_transaction_costs + (current_stock_prices*constant_transaction_costs)%*%position
            
            #Keeping track of transactions
            for (index_last_loop in 1:Number_of_top_stocks_start){
              position[index_last_loop] <- 0 #position still has to be a vector, which is why I am doing it in this silly way
            }
            
            trans_row <- ifelse(length(nrow(do.call(rbind, transaction_file)))==0,0,nrow(do.call(rbind, transaction_file)))
            trans_temp <- 0
            if(index_monthly==start_month & index_year==1){
              upper_for <- m
            }
            else{
              upper_for <- m-Number_of_top_stocks
            }
            for (trans_index in 1:upper_for){
              new_stock <- (position[trans_index]>0 & names(position[trans_index])!=names(position_old[trans_index]))
              adjust_existing_stock <- (position[trans_index]!=position_old[trans_index] & names(position[trans_index])==names(position_old[trans_index]))
              
              if(new_stock==TRUE){
                transaction_file[[trans_row+trans_index-trans_temp]] <- as.character(data[start_est-index,1]) #date where the trading happens
                transaction_file[[trans_row+trans_index-trans_temp]][2] <- "buy" 
                transaction_file[[trans_row+trans_index-trans_temp]][3] <- names(new_stock) #ticker
                transaction_file[[trans_row+trans_index-trans_temp]][4] <- position[trans_index]
                transaction_file[[trans_row+trans_index-trans_temp]][5] <- data[start_est-index,which(names(new_stock)==names(data[start_est-index,]))]
              }
              else{
                if(adjust_existing_stock==TRUE){
                  buy_or_sell <- position[trans_index]-position_old[trans_index]
                  transaction_file[[trans_row+trans_index-trans_temp]] <- as.character(data[start_est-index,1]) #date where the trading happens
                  transaction_file[[trans_row+trans_index-trans_temp]][2] <- ifelse(buy_or_sell>0,"buy","sell")  
                  transaction_file[[trans_row+trans_index-trans_temp]][3] <- names(adjust_existing_stock) #ticker
                  transaction_file[[trans_row+trans_index-trans_temp]][4] <- buy_or_sell
                  transaction_file[[trans_row+trans_index-trans_temp]][5] <- data[start_est-index,which(names(adjust_existing_stock)==names(data[start_est-index,]))]
                }
                else{
                  trans_temp <- trans_temp + 1 
                }
              }
            }
            
            results[[index_storing]] <- capital - results[[index_storing-1]][6] #P/L
            results[[index_storing]][2] <- NA
            results[[index_storing]][3] <- NA
            results[[index_storing]][4] <- capital
            results[[index_storing]][5] <- 0
            results[[index_storing]][6] <- capital
            results[[index_storing]][7] <- cumulated_transaction_costs
            
          }
          else{
            #Subtracting the right prices, which is the prices given the day before we trade
            if(index_monthly==start_month_input & index_year==1){
              current_stock_prices <- unlist(data[start_est-index+1,c(2:ncol(data))])
            }
            else{
              current_stock_prices <- unlist(data[start_est-index+1,c(2:ncol(data))[-index_choose]])
            }
            
            #################Risk management####################
            #Estimation of the covariance matrix and the mu in a multi-dimensional GBM
            if(index_monthly==start_month_input & index_year==1){
              Sigma_estimate <- cov(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),])*252 #annualized covariance matrix
              for (sigma_check in 1:Number_of_top_stocks_start){ #If a stock doesnt move, then we will have problems simulating from a covariance matrix with a row and column full of zeros. Thus, we change these to .Machine$double.eps
                if(sum(Sigma_estimate[,sigma_check]^2) ==0){
                  Sigma_estimate[,sigma_check] <- .Machine$double.eps
                  Sigma_estimate[sigma_check,] <- .Machine$double.eps
                }
              }
              mu_estimate <- colMeans(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),])*252+diag(Sigma_estimate)/2 #annualized mu estimate in a GBM
            }
            else{
              Sigma_estimate <- cov(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),-index_choose])*252 #annualized covariance matrix
              for (sigma_check in 1:Number_of_top_stocks_start){ #If a stock doesnt move, then we will have problems simulating from a covariance matrix with a row and column full of zeros. Thus, we change these to .Machine$double.eps
                if(sum(Sigma_estimate[,sigma_check]^2) ==0){
                  Sigma_estimate[,sigma_check] <- .Machine$double.eps
                  Sigma_estimate[sigma_check,] <- .Machine$double.eps
                }
              }
              mu_estimate <- colMeans(returns[(start_est-(index-1)):(start_est-(index-1)+N-1),-index_choose])*252+diag(Sigma_estimate)/2 #annualized mu estimate in a GBM
            }
            risk <- risk_management(current_stock_prices, position, covariance_matrix=Sigma_estimate, mean_vector=mu_estimate, p, time_horizon, number_of_simulations)
            VaR <- risk[[1]]
            ES <- risk[[2]]
            risk_red <- risk_reduction() #reducing risk, if VaR and/or ES are too high
            cap_alloc <- risk_red[[1]]
            position <- risk_red[[2]]
            names(position) <- names(current_stock_prices)
            VaR <- risk_red[[3]]
            ES <- risk_red[[4]]
            
            ##############Accounting##############
            #UPDATE TO THE CLOSING PRICES FOR THE TRADING DAY
            #Subtracting the right prices, which is the prices given the day before we trade
            if(index_monthly==start_month_input & index_year==1){
              current_stock_prices <- unlist(data[start_est-index,c(2:ncol(data))])
            }
            else{
              current_stock_prices <- unlist(data[start_est-index,c(2:ncol(data))[-index_choose]])
            }
            
            #Accounting
            if((cash_fun(position) + cash+current_stock_prices%*%position>capital*1.1) | ((cash_fun(position) + cash+current_stock_prices%*%position)*1.2<capital)){
              break_variable <- 1
              break
            }
            cash <- cash_fun(position) + cash
            capital <- current_stock_prices%*%position + cash
            cumulated_transaction_costs <- cumulated_transaction_costs + transaction_costs_fun(position)
            
            #Keeping track of transactions
            trans_row <- ifelse(length(nrow(do.call(rbind, transaction_file)))==0,0,nrow(do.call(rbind, transaction_file)))
            trans_temp <- 0
            if(index_monthly==start_month_input & index_year==1){
              upper_for <- m
            }
            else{
              upper_for <- m-Number_of_top_stocks
            }
            for (trans_index in 1:upper_for){
              adjust_existing_stock <- (position[trans_index]!=position_old[trans_index] & names(position[trans_index])==names(position_old[trans_index]))
              if(adjust_existing_stock==TRUE){
                buy_or_sell <- position[trans_index]-position_old[trans_index]
                transaction_file[[trans_row+trans_index-trans_temp]] <- as.character(data[start_est-index,1]) #date where the trading happens
                transaction_file[[trans_row+trans_index-trans_temp]][2] <- ifelse(buy_or_sell>0,"buy","sell")  
                transaction_file[[trans_row+trans_index-trans_temp]][3] <- names(adjust_existing_stock) #ticker
                transaction_file[[trans_row+trans_index-trans_temp]][4] <- buy_or_sell
                transaction_file[[trans_row+trans_index-trans_temp]][5] <- data[start_est-index,which(names(adjust_existing_stock)==names(data[start_est-index,]))]
              }
              else{
                trans_temp <- trans_temp + 1 
              }
              
            }
            
            ##############Updating##############
            position_list[[index_storing]] <- position
            position_old <- position
            weights_old <- weights
            cap_alloc_old <- cap_alloc
            results[[index_storing]] <- capital - results[[index_storing-1]][6] #P/L
            results[[index_storing]][2] <- VaR
            results[[index_storing]][3] <- ES
            results[[index_storing]][4] <- cash
            results[[index_storing]][5] <- current_stock_prices%*%position
            results[[index_storing]][6] <- capital
            results[[index_storing]][7] <- cumulated_transaction_costs
          }
        }
        index_storing <- index_storing + 1 
      }
    }
  }
  
  output1 <- cbind(date_vector[-1],do.call(rbind, results))
  colnames(output1) <- c("Date","P/L","VaR","ES","Cash","Stock position","Capital","Cumulated transaction costs")
  output2 <- do.call(rbind, transaction_file)
  colnames(output2) <- c("Date","Buy/Sell","Ticker","Num. of shares","price")
  
  return(list(output1,output2))
}

####################################################
#############Input to the function##################
####################################################
#Common data
S0 <- 10000000 #start capital
Number_of_top_stocks <- 2 #Number of stocks we are considering every month (besides the first month, where we choose 10 stocks)
Number_of_top_stocks_start <- 10
constant_transaction_costs <- 0.002
#Data used in the data preparation part
start_month_input <- 1
end_month_input <- 12
year_data <- 1991:2014
singledate <- read.csv('singledate.csv', header=TRUE) #I saved my workspace with theses guys
ticker <- read.csv('ticker.csv', header=TRUE) #I saved my workspace with theses guys 
#Data used in the portfolio allocation part
iter_fit <- 1000 #number of iterations per chain (as a start; if we are not having convergence, then we increase this number by 200, if we still have lack of convergence, then we increase it by another 200, and so on)
chains <- 4 #number of chains
upper_bound <- rep(0.5,10)
lower_bound <- rep(0,10)
lambda <- 2.5
#Data used in the risk management part
p=0.99 
time_horizon=1/252 
number_of_simulations=10000 
perc_VaR <- 0.02 #percentage of the capital that the VaR is not allowed to be greater than
perc_ES <- 0.1 #percentage of the capital that the ES is not allowed to be greater than
limit_for_changing_position <- 5 #This means that we want the expected return to be at least 5 times higher than the transaction costs; otherwise, we will just stick to the already existing position

system.time(
  backtesting_results <- backtesting(S0, Number_of_top_stocks, Number_of_top_stocks_start, constant_transaction_costs, start_month_input, end_month_input, year_data, iter_fit, chains, upper_bound, lower_bound, lambda, p, time_horizon, number_of_simulations, perc_VaR, perc_ES, limit_for_changing_position)
)
write.csv(output1,"Backtest_results_1998.csv")
write.csv(output2,"Backtest_transactions_1998.csv")

plot.ts(output1[,7])

#which(-as.numeric(output1[2:(nrow(output1)),2])>as.numeric(output1[1:(nrow(output1)-1),3])) #showing the number of times the VaR is exceeded. Should be 1%, assuming the model is correct

#strange things: making to much money from 1994-07-31 to 1994-08-01. I suspect that I do not calculate the profits correctly, in the last piece of this code that I am still a little worried about