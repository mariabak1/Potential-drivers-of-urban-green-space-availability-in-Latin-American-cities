
create.table <-function(modobj)  
{
  cc = as.data.frame(confint(modobj))
  table = as.data.frame(cbind(est=(coef(modobj)), cc))
  table$est = as.numeric(table$est)
  table$est = round(table$est, 3)
  table$pred = row.names(table)
  table = table[!table$pred == "(Intercept)" ,]
  table$id = 1:nrow(table)
  table = merge(table, varlabels, by="pred", all.x = T)
  table <- table[order(table$id),]
  table = table[,c(6,2:4)]
  table$`95CI` = paste(round(table$`2.5 %`, 3), round(table$`97.5 %`, 3), sep = ", ")
  table = table[,c(1,2,5)]
  names(table)=c("Variable", "Beta", "95% CI")
  table[nrow(table)+1,1]="Model statistics"
  table[nrow(table)+1,1]="R2"
  table[nrow(table),2]= round(as.numeric(summary(modobj)$r.squared),3)
  table[nrow(table)+1, 1]= "N (cities)"
  table[nrow(table), 2]=nobs(modobj)
  
  return(table)
  
}


create.table_ml <-function(modobj)  
{
  cc = as.data.frame(confint(modobj))
  cc=cc[3:nrow(cc),]
  table = as.data.frame(cbind(est=coef(summary(modobj)), cc))
  table$est = as.numeric(table$est.Estimate)
  table$est = round(table$est, 3)
  table$pred = row.names(table)
  table = table[!table$pred == "(Intercept)" ,]
  table$id = 1:nrow(table)
  table = merge(table, varlabels, by="pred", all.x = T)
  table <- table[order(table$id),]
  table$`95CI` = paste(round(table$`2.5 %`, 3), round(table$`97.5 %`, 3), sep = ", ")
  table = table[,c(11,9,12)]
  table[nrow(table)+1,1]="Model statistics"
  table[nrow(table)+1, 1]= "N (cities)"
  table[nrow(table), 2]=nobs(modobj)
  table[nrow(table)+1, 1]= "AIC"
  table[nrow(table), 2] = AIC(modobj)
  names(table)=c("Variable", "Beta", "95% CI")
  
  return(table)
  
}


# function to compute summary statistics
summary_by_cat = function(dat){
  dat = skim(dat) %>% filter(skim_variable %in% names(becl1ux2))
  dat = dat[c(1,2,7,6,3,5,4),]%>%
    select(skim_variable, numeric.mean, numeric.sd) %>%
    mutate(mean_sd = paste0(round(numeric.mean, 3), " (", round(numeric.sd, 2), ")", sept="")) %>%
    select(skim_variable, mean_sd)
  dat_sum=transpose(dat)
  dat_sum = dat_sum[c(2),]
  return(dat_sum)
}







# functions for correlation matrices
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}



# function to compute a correlation matrix between selected variables of a dataframe
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


