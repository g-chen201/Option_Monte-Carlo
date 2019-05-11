# author: Guanlin Chen, Tianrui Zheng, Yabing Yang
S0 = 100
K = 100
r = 0.03
TT = 1
sig = 0.2

# price by formula
bs_call = function(S0, K, r, TT, sig) {
  d1 = (log(S0/K) + (r + 0.5*sig^2)*TT) / (sig*sqrt(TT))
  d2 = d1 - sig*sqrt(TT)
  S0*pnorm(d1) - K*exp(-r*TT)*pnorm(d2)
}
P_theory = bs_call(100,100,0.03,1,0.2)

# price by simulation with different times
cal_price = function(n){
  Zt = array(rnorm(n,0,1))
  St = S0 * exp((r-1/2 * sig^2) * TT + sig * sqrt(TT) * Zt)
  df = data.frame(Zt,St)
  df['_temp'] = df['St'] - 100
  df['0'] = 0
  df['Ct'] = apply(df[c('0','_temp')],1,max)
  ct_sd = apply(df['Ct'],2,sd)
  df['Ct_standard'] = (df['Ct'] - P_theory)/ct_sd
  price = exp(-r*TT) * apply(df['Ct'], 2, mean)
  CI_report = t.test(df['Ct_standard'])
  result = list(price,CI_report)
  return(result)
}

P_simu_1 = cal_price(500)[[1]][1]
CI_report_1 = cal_price(500)[[2]] # CI for confidence interval

P_simu_2 = cal_price(1500)[[1]][1]
CI_report_2 = cal_price(1000)[[2]]

P_simu_3 = cal_price(2000)[[1]][1]
CI_report_3 = cal_price(2000)[[2]]

P_simu_4 = cal_price(2500)[[1]][1]
CI_report_4 = cal_price(2500)[[2]]

P_simu_5 = cal_price(3000)[[1]][1]
CI_report_5 = cal_price(3000)[[2]]

