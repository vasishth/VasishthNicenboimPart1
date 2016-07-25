library(MASS)


#############
### Simulation of typical dataset of psycholinguistics
### in latin-square assuming:
###  perfectly lognormally distributed RTs and no outliers
### and only 2 conditions a,b coded as -1,1 for every item 


#Function to create the data sets
data_2cond <- function(N_subj,N_item,alpha,beta,sdev_subj,sdev_item,sdev,rho_subjs,rho_items){

    N_coef <- 2 

    #We create a latin square for two conditions:
    conditions <- data.frame(c=rep(c("a","b"),(N_item+1)*N_subj/2),r=rep(1:(N_item+1),N_subj/2) )
    conditions<-conditions[conditions$r!=(N_item+1),c("c")]

    #This will be the dataset:
    datasim <- data.frame(subj =factor(rep(seq(1:N_subj),each=N_item)),
                item =factor(rep(seq(1:N_item),times=N_subj)),
                conditions = conditions)

    #sum coded contrasts:
    contrasts(datasim$c) <- contr.sum(2)
    X <- model.matrix(~1+c,data=datasim)

    # Correlation matrix by subj
    rho_subj<- matrix(rep(rho_subjs,N_coef*N_coef),nrow=N_coef)
    diag(rho_subj) <-1

    # Correlation matrix by item
    rho_item<- matrix(rep(rho_items,N_coef*N_coef),nrow=N_coef)
    diag(rho_item) <-1


    b_subj <- sdev_subj %*% t(sdev_subj)  
    Sigma_subj <- b_subj * rho_subj  #variance covariance matrix for subj r.e.
    raneff_subj <- mvrnorm(n = N_subj, rep(0,N_coef), Sigma_subj)


    b_item <- sdev_item %*% t(sdev_item)  
    Sigma_item <- b_item * rho_item  #variance covariance matrix for item r.e.
    raneff_item <- mvrnorm(n = N_item, rep(0,N_coef), Sigma_item)

    #I create rts by exponentiating the alpha + X *beta + r.e subj + r.e items + normally distributed noise

    datasim$rt <- round(exp( 
    rowSums(matrix(rep(c(alpha,beta),N_subj* N_item),ncol=2, byrow = TRUE)  * X) +
    rowSums(raneff_subj[datasim$subj,] * X)+
    rowSums(raneff_item[datasim$item,] * X)+
    rnorm(N_subj*N_item, 0, sdev)),0)

    datasim$code <- X[,2]

    return(datasim)
}