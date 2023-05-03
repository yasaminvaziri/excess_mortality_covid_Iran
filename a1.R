library(data.table)
library(ggplot2)

Sys.setlocale(locale = 'persian')


#reading data

d = fread('C:\\Users\\Yasmine\\Documents\\daaa\\iranprovs_mortality_monthly (1).csv', encoding = "UTF-8") 

#using another variable 

d$ym_num = d$y + d$m / 12 - 1/24
ds = d[, .(n = sum(n)), .(y, m, ym_num)]

#there is a connection between the first and second part of the year and the mortality

ggplot(ds, aes(ym_num, n))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = 1389:1401)+
  scale_y_continuous(limits = c(0, 71000))+
  geom_vline(xintercept = 1398 + 10/12 -1/24, linetype = 'dashed')

#also clear that when did corona start: Oct-Nov (Dey) 1398

###################

ds = d[, .(n = sum(n)), .(y, m, ym_num, prov)]


ym_num_covid = 1398 + 10/12 - 1/24

# to avoid dealing with non-linear patterns we only look at the last 5 years
ym_num_start = ym_num_covid - 5

#adding new columns to the data set

ds$excess_mortality=0
ds$norm_excess_mortality=0
ds$num_predict=0

#start fitting

for (i in 1:31) { 
  PROV = unique(ds$prov)[i]
  for (j in 1:12) {
    M = j
    dsm = ds[prov == PROV & m == M,]
    dsm = dsm[ym_num > ym_num_start]
    dsm2fit = dsm[ym_num < ym_num_covid]
    fit = lm(n ~ ym_num, dsm2fit)
    pvalue = summary(fit)$coefficients[,4][2]
    if(pvalue > .7){
      dsm$num_predict = mean(dsm2fit$n)
    } else{
      dsm$num_predict = predict(fit,dsm)
    }
    sigman = summary(fit)$sigma
    dsm$excess_mortality = dsm$n-dsm$num_predict
    
    
    
    dsm[dsm$num_predict + 2*sigman > dsm$n]$excess_mortality = 0
    ds[prov == PROV & m == M & ym_num > ym_num_start]$excess_mortality = dsm$excess_mortality
    ds[prov == PROV & m == M & ym_num > ym_num_start]$norm_excess_mortality = dsm$excess_mortality/sum(dsm$n)
    #random province
    if(i == 1){
      
     ggplot(dsm)+
        geom_smooth(aes(ym_num, num_predict), method = 'lm')+
        geom_point(aes(ym_num, n), size = 1)+
        scale_x_continuous(breaks = 1389:1401)+
         geom_vline(xintercept = 1398 + 10/12 -1/24, linetype = 'dashed')+
         ggtitle(label = PROV, subtitle = paste('month: ', M))+xlab("year")+xlab("death")
      
    }

    
  }
  
}

#first question: 

ggplot(ds[ym_num > 1398.5], aes(x = ds[ym_num > 1398.5]$ym_num,
                                y = ds[ym_num > 1398.5]$prov, fill = norm_excess_mortality))+
                                xlab("year")+ylab("Province")+geom_tile() + 
                                scale_fill_gradient(high = "red", low = "grey")

#second question 
print("total excess mortality: ")
print(sum(ds$excess_mortality))


#third question
for (i in 1:31) {
  PROV=unique(ds$prov)[i]
  print(paste("total excess mortality in ", PROV, " :", sum(ds[ds$prov == PROV, ]$excess_mortality)))
 
}


#last_question

ggplot(ds)+
  geom_point(aes(prov, norm_excess_mortality), size = 1)+
  ggtitle(label = "Normal Excess Mortality by Province")+
  xlab("province")+ylab("normal excess mortalities")


