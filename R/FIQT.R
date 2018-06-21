#' A function for FDR Inverse Quantile Transformation (FIQT)
#'
#' This function computes the FIQT (winner's curse) adjusted assocation Z-scores
#'
#' @param z a vector of association Z-scores
#' @param min.p minimum p-value admitted (to avoid zero p-values/adjusted p-values which give troubles with inverse cdf)
#' very large Z-scores corresponding to min.p (i.e., z > 37) are not adjusted, as their bias is essentially zero
#'
#' @return mu.z a vector of FIQT (winner's curse) adjusted association Z-scores
#'

fiqt <- function(z=z, min.p=10^-300){
  pvals<-2*pnorm(abs(z),low=F)
  pvals[pvals<min.p]<- min.p
  adj.pvals<-p.adjust(pvals,method="fdr")
  mu.z<-sign(z)*qnorm(adj.pvals/2,low=F)
  mu.z[abs(z)>qnorm(min.p/2,low=F)]<-z[abs(z)>qnorm(min.p/2,low=F)]
  return(mu.z)
}
