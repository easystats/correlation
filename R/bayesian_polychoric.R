#' ######################################################################
#' #######################################
#' # Main Function
#' #
#' #This function calculates polychoric correlation using the Gibbs
#' #Sampling method
#' #
#' ##Arguments:
#' #u1: It can be a contingency table or a vector of the first ordinal
#' #variable
#' ##u2: If u1 is a vector, then u2 must be a un-null vector given the
#' #second ordinal variable
#' ##iter: number of iterations of Gibbs sampling
#' ##t0: First t0 samplings will be excluded due to non-convergence
#' ##everyN: Only every N-th sampling will be taken
#' ##trace: If TRUE, the program will print out the number of iterations
#' #executed for every 100 iterations
#' ##graph: If TRUE, a graph will be printed showing the sampling history
#' #of the correlation and thresholds
#' #
#' #Outputs: rho, c, d
#' #rho is the vector of Gibbs sampled rho
#' #c is the vector of thresholds of X
#' #d is the vector of thresholds of Y
#' ######################################################################
#' ########################################
#' polycorGibbs<-function(u1, u2 = NULL, iter = 1e4, t0 = 200, everyN=10, trace = FALSE){
#'
#'   if(class(u1) == "table"){
#'     data<-.convert(u1)
#'     x<-data$x
#'     y<-data$y
#'   }else{
#'     if(class(u1) == "integer"){
#'       if(is.null(u2)){
#'         print("The argument u2 cannot be null ")
#'       }else{
#'         x<-u1
#'         y<-u2
#'       }
#'     }else{
#'       print("Invalid input")
#'       return(NULL)
#'     }
#'   }
#'   if (length(unique(x))==1) {
#'     print("Error: x is a constant vector")
#'     return(NULL)
#'   }
#'   if (length(unique(y))==1) {
#'     print("Error: y is a constant vector")
#'     return(NULL)
#'   }
#'
#'   if (length(x) != length(y)){
#'     print("Please make sure the length of the two input vectors are
#' the same!")
#'     return(NULL)
#'   }
#'   xd <- sort(unique(x))
#'   yd <- sort(unique(y))
#'
#'   x_rec<-x
#'   y_rec<-y
#'   for(i in 1:length(x)){
#'     for (j in 1:length(xd)){
#'       if (x[i] == xd[j]){
#'         x_rec[i]<-j
#'       }
#'     }
#'   }
#'   for(i in 1:length(y)){
#'     for (j in 1:length(yd)){
#'       if (y[i] == yd[j]){
#'         y_rec[i]<-j
#'       }
#'     }
#'   }
#'   x_rec = as.integer(x_rec)
#'   y_rec = as.integer(y_rec)
#'   x<-x_rec
#'   y<-y_rec
#'   plc<-.Gibbs(x, y, iter, trace)
#'   rho<-as.matrix(plc$rho, ncol = 1)
#'   c<-as.matrix(plc$c, ncol = length(unique(x_rec))-1)
#'   d<-as.matrix(plc$d, ncol = length(unique(y_rec))-1)
#'
#'   res<-data.frame(rho, c, d)
#'   res<-res[-(1:t0), ]
#'   res<-res[seq(1,nrow(res),by= everyN),]
#'   colnames(res)<-c("rho", paste("c", 1:(length(unique(x))-1), sep =
#'                                   ""), paste("d", 1:(length(unique(y))-1), sep = ""))
#'   mn<-signif(apply(res, 2, mean), 3)
#'   md<-signif(apply(res, 2, median), 3)
#'   sd<-signif(apply(res, 2, .BatchMeans), 3)
#'   qsd<-signif(apply(res, 2, sd), 3)
#'   cor<-signif(cor(res), 3)
#'
#'   cor[lower.tri(cor)]<-NA
#'   diag(cor)<-NA
#'
#'   smy<-cbind(mn, md, sd, qsd)
#'   colnames(smy)<-c("mean", "median", "SD", "Numeric SD")
#'
#'   cat("Summary\n")
#'   print(smy)
#'   cat("\nCorrelation\n")
#'   print(cor)
#'
#'   plc
#'
#' }
#'
#'
#'
#'
#'
#'
#' #' @keywords internal
#' .convert<-function(tbl){
#'
#'   data<-NULL
#'   for(i in 1:nrow(tbl)){
#'     for(j in 1:ncol(tbl)){
#'       data<-rbind(data, matrix(rep(c(i, j), tbl[i, j]), ncol = 2,
#'                                byrow = T))
#'     }
#'   }
#'   data<-as.data.frame(data)
#'   colnames(data)<-c("x", "y")
#'   data
#' }
#'
#'
#'
#'
#' #' @keywords internal
#' .BatchMeans<-function(vals,bs = "sqroot",warn = FALSE){
#'
#'   N<-length(vals)
#'   if(N < 1000){
#'     if(warn){ # if warning
#'       cat("WARNING: too few samples (less than 1000)\n")
#'     }
#'     if(N < 10){
#'       return(NA)
#'     }
#'   }
#'
#'   if(bs=="sqroot"){
#'     b<-floor(sqrt(N)) # batch size
#'     a<-floor(N/b) # number of batches
#'   }else{
#'     if(bs=="cuberoot"){
#'       b<-floor(N^(1/3)) # batch size
#'       a<-floor(N/b) # number of batches
#'     }else{ # batch size provided
#'       stopifnot(is.numeric(bs))
#'       b<-floor(bs) # batch size
#'       if(b > 1){ # batch size valid
#'         a <- floor(N/b) # number of batches
#'       }else{
#'         stop("batch size invalid (bs=",bs,")")
#'       }
#'     }
#'   }
#'
#'   Ys<-sapply(1:a,function(k) return(mean(vals[((k-1)*b+1):(k*b)])))
#'
#'   muhat<-mean(Ys)
#'   sigmahatsq<-b*sum((Ys-muhat)^2)/(a-1)
#'
#'   bmse<-sqrt(sigmahatsq/N)
#'
#'   bmse
#'
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#' #' @keywords internal
#' .Gibbs<-function(x, y, iter = 1e4, trace = FALSE){
#'
#'   library("truncnorm")
#'
#'   n<-length(x)
#'
#'   rho<-0.5
#'   c<-c(-Inf, qnorm(as.vector(cumsum(table(x)))/n, lower.tail = T))
#'   d<-c(-Inf, qnorm(as.vector(cumsum(table(y)))/n, lower.tail = T))
#'   tmp.c<-c
#'   tmp.c[1]<-c[2]-.1
#'   tmp.c[length(tmp.c)]<-c[length(c)-1]+.1
#'   tmp.d<-d
#'   tmp.d[1]<-d[2]-.1
#'   tmp.d[length(tmp.d)]<-d[length(d)-1]+.1
#'   theta1<-NULL
#'   theta2<-NULL
#'   for(i in 1:n){
#'     theta1<-c(theta1, runif(1, tmp.c[x[i]], tmp.c[x[i]+1]))
#'     theta2<-c(theta2, runif(1, tmp.d[y[i]], tmp.d[y[i]+1]))
#'   }
#'
#'   gibbs.rho<-NULL
#'   gibbs.c<-NULL
#'   gibbs.d<-NULL
#'
#'   for(i in 1:iter){
#'     st<-.SampleTheta(rho, c, d, x, y, theta1, theta2) # Formula 2.4 in
#'     theta1<-st$theta1
#'     theta2<-st$theta2
#'
#'     rho<-.SampleRho(c, d, theta1, theta2, rho) # Corresponds to
#'
#'     scd<-.SampleThreshold(theta1, theta2, rho, c, d)
#'     c<-scd$c
#'     d<-scd$d
#'     gibbs.rho<-c(gibbs.rho, rho)
#'     gibbs.c<-rbind(gibbs.c, c[2:(length(c)-1)])
#'     gibbs.d<-rbind(gibbs.d, d[2:(length(d)-1)])
#'     if(trace && i %% 100 == 0){
#'
#'       cat(i)
#'       cat(" iterations completed.")
#'       cat("\n")
#'     }
#'   }
#'
#'   list(rho = gibbs.rho, c = gibbs.c, d = gibbs.d)
#'
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' #' @keywords internal
#' .SampleTheta<-function(rho, c, d, x, y, theta1, theta2){
#'
#'   n<-length(x)
#'   new.theta1<-NULL
#'   new.theta2<-NULL
#'   tmp<-theta2[1]
#'   for(i in 1:n){
#'
#'     tmp1<-rtruncnorm(1, a = c[x[i]], b = c[x[i]+1], mean =
#'                        rho*theta2[i], sd = sqrt(1-rho^2))
#'
#'     if(tmp1 < c[x[i]] || tmp1 > c[x[i]+1]){
#'       print("error in sampling theta")
#'       stop()
#'     }
#'     new.theta1<-c(new.theta1, tmp1)
#'
#'     tmp2<-rtruncnorm(1, a = d[y[i]], b = d[y[i]+1], mean = rho*tmp1,
#'                      sd = sqrt(1-rho^2))
#'     if(tmp2 < d[y[i]] || tmp2 > d[y[i]+1]){
#'       print("error in sampling theta")
#'       stop()
#'     }
#'     new.theta2<-c(new.theta2, tmp2)
#'   }
#'
#'   list(theta1 = new.theta1, theta2 = new.theta2)
#'
#' }
#'
#'
#'
#' #' @keywords internal
#' .SampleThreshold<-function(theta1, theta2, rho, c, d){
#'
#'   new.c<-c(-Inf)
#'   for(i in 2:(length(c)-1)){
#'     upper.c<-min(c[i+1], min(theta1[theta1>=c[i]]))
#'     lower.c<-max(c[i-1], max(theta1[theta1<=c[i]]))
#'     new.c<-c(new.c, runif(1, lower.c, upper.c))
#'   }
#'   new.d<-c(-Inf)
#'   for(i in 2:(length(d)-1)){
#'     upper.d<-min(d[i+1], min(theta2[theta2>=d[i]]))
#'     lower.d<-max(d[i-1], max(theta2[theta2<=d[i]]))
#'     new.d<-c(new.d, runif(1, lower.d, upper.d))
#'   }
#'
#'   new.c<-c(new.c, Inf)
#'   new.d<-c(new.d, Inf)
#'
#'   list(c = new.c, d = new.d)
#'
#' }
#'
#'
#'
#'
#'
#' #' @keywords internal
#' .SampleRho<-function(c, d, theta1, theta2, rho){
#'   n<-length(theta1)
#'   s11<-sum(theta1^2)
#'   s12<-sum(theta1 * theta2)
#'   s22<-sum(theta2^2)
#'   phi0<-.NROfPhi(n = n, s11 = s11, s12 = s12, s22 = s22, start =
#'                   (1+rho)/(1-rho))
#'   der<-.DerOfPhi(phi0, n = n, s11 = s11, s12 = s12, s22 = s22)
#'   ThirdDer.phi<-der$ThirdDer.phi
#'   Hess.phi<-der$Hess.phi
#'   nu<-1 + phi0 * ThirdDer.phi/(3 * Hess.phi)
#'   mu<-.NROfTau(n = n, s11 = s11, s12 = s12, s22 = s22, nu = nu, start =
#'                 (phi0^nu - 1)/nu)
#'   Hess.tau<-.DerOfTau(mu, n = n, s11 = s11, s12 = s12, s22 = s22, nu =
#'                        nu)$Hess.tau
#'   sigma2<-1/(-Hess.tau)
#'   max.try<-100
#'   while(max.try > 0){
#'     max.try<-max.try - 1
#'     tau<-rnorm(1, mu, sqrt(sigma2))
#'     if(nu * tau + 1 > 0){
#'       rho<-((nu * tau + 1)^(1/nu) - 1)/((nu * tau + 1)^(1/nu) + 1)
#'       max.try<-0
#'     }
#'   }
#'   if(is.na(rho)){
#'     stop("Error in SampleRho")
#'   }
#'   rho
#' }
#'
#'
#'
#' #' @keywords internal
#' .NROfTau<-function(n, s11, s12, s22, nu, start){
#'
#'   max.try<-100
#'
#'   lower<-min((.001^nu-1)/nu, (10^nu-1)/nu)
#'   upper<-max((.001^nu-1)/nu, (10^nu-1)/nu)
#'
#'   while(max.try > 0){
#'     max.try<-max.try-1
#'     op<-optim(par = runif(1,-start,20-start), fn=.NegLoglikOfTau,
#'               gr=.NegGradOfTau, method = "Brent", lower = lower, upper = upper, n =
#'                 n, s11 = s11, s12 = s12, s22 = s22, nu = nu)
#'     if(op$convergence == 0){
#'       return(op$par)
#'     }
#'   }
#'
#'   if(op$convergence != 0){
#'     print("Error in finding tau")
#'     stop()
#'   }
#' }
#'
#'
#'
#' #' @keywords internal
#' .DerOfTau<-function(tau, n, s11, s12, s22, nu){
#'
#'   phi<-(nu * tau + 1)^(1/nu)
#'   der<-.DerOfPhi(phi, n = n, s11 = s11, s12 = s12, s22 = s22)
#'   L.phi<-der$L.phi
#'   Grad.phi<-der$Grad.phi
#'   Hess.phi<-der$Hess.phi
#'
#'   d.phi<-(nu * tau + 1)^(1/nu - 1)
#'   d2.phi<-(1 - nu) * (nu * tau + 1)^(1/nu - 2)
#'
#'   L.tau<-L.phi + log(d.phi)
#'   Grad.tau<-Grad.phi * d.phi + (1 - nu)/(nu * tau + 1)
#'   Hess.tau<-Hess.phi * d.phi^2 + Grad.phi * d2.phi - nu * (1 - nu)/(nu
#'                                                                     * tau + 1)^2
#'
#'   list(L.tau = L.tau, Grad.tau = Grad.tau, Hess.tau = Hess.tau)
#'
#' }
#'
#'
#'
#' #' @keywords internal
#' .NegGradOfTau<-function(phi, n, s11, s12, s22, nu){
#'
#'   phi<-(nu * tau + 1)^(1/nu)
#'   der<-.DerOfPhi(phi, n = n, s11 = s11, s12 = s12, s22 = s22)
#'   L.phi<-der$L.phi
#'   Grad.phi<-der$Grad.phi
#'   Hess.phi<-der$Hess.phi
#'
#'   d.phi<-(nu * tau + 1)^(1/nu - 1)
#'   Grad.tau<-Grad.phi * d.phi + (1 - nu)/(nu * tau + 1)
#'
#'   -Grad.tau
#'
#' }
#'
#'
#' #' @keywords internal
#' .NegLoglikOfTau<-function(tau, n, s11, s12, s22, nu){
#'   phi<-(nu * tau + 1)^(1/nu)
#'   der<-.DerOfPhi(phi, n = n, s11 = s11, s12 = s12, s22 = s22)
#'   L.phi<-der$L.phi
#'   Grad.phi<-der$Grad.phi
#'   Hess.phi<-der$Hess.phi
#'
#'   d.phi<-(nu * tau + 1)^(1/nu - 1)
#'
#'   L.tau<-L.phi + log(d.phi)
#'
#'   -L.tau
#'
#' }
#'
#'
#'
#' #' @keywords internal
#' .NegLoglikOfPhi<-function(phi, n, s11, s12, s22){
#'
#'   -( -n/2 * log(phi) + (n - 2) * log(phi + 1) - ((s11+s22)/8 - s12/4)
#'      * phi - ((s11+s22)/8 + s12/4)/phi )
#'
#' }
#'
#' #' @keywords internal
#' .NegGradOfPhi<-function(phi, n, s11, s12, s22){
#'
#'   -(-n/(2 * phi) + (n - 2)/(phi + 1) - (s11 + s22)/8 + s12/4 + ((s11 +
#'                                                                    s22)/8 + s12/4)/phi^2 )
#'
#' }
#'
#'
#' #' @keywords internal
#' .DerOfPhi<-function(phi, n, s11, s12, s22){
#'
#'   L.phi<-( -n/2 * log(phi) + (n - 2) * log(phi + 1) - ((s11+s22)/8 -
#'                                                          s12/4) * phi - ((s11+s22)/8 + s12/4)/phi )
#'   Grad.phi<-(-n/(2 * phi) + (n - 2)/(phi + 1) - (s11 + s22)/8 + s12/4
#'              + ((s11 + s22)/8 + s12/4)/phi^2 )
#'   Hess.phi<-( n/(2 * phi^2) - (n - 2)/(phi + 1)^2 - ((s11 + s22)/4 +
#'                                                        s12/2)/phi^3 )
#'   ThirdDer.phi<-( -n/phi^3 + 2 * (n - 2)/(phi + 1)^3 + 3 * ((s11 +
#'                                                                s22)/4 + s12/2)/phi^4 )
#'
#'   list(L.phi = L.phi, Grad.phi = Grad.phi, Hess.phi = Hess.phi,
#'        ThirdDer.phi = ThirdDer.phi)
#' }
#'
#'
#' #' @keywords internal
#' .NROfPhi<-function(n, s11, s12, s22, start){
#'
#'   max.try<-100
#'
#'   while(max.try > 0){
#'     max.try<-max.try-1
#'     op<-optim(par = runif(1,-start,20-start), fn=.NegLoglikOfPhi,
#'               gr=.NegGradOfPhi, lower = 1e-3, upper = 50, method = "Brent", n = n,
#'               s11 = s11, s12 = s12, s22 = s22)
#'     if(op$convergence == 0){
#'       return(op$par)
#'     }
#'   }
#'
#'   if(op$convergence != 0){
#'     print("Error in finding phi")
#'     stop()
#'   }
#'
#' }
