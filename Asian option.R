# author: Guanlin Chen, Tianrui Zheng, Yabing Yang

S0 = 100
K = 100
r = 0.03
t1 = 1
t2 = 1.5
t3 = 2
sig = 0.2
dt1 = t2 - t1
dt2 = t3 - t2



# use a function to generate Zt each time period, otherwise the Zt will be the same.
gen_Zt = function(n){
  Zt = array(rnorm(n,0,1))
  return(Zt)
}
S1 = S0 * exp((r-1/2 * sig^2) * t1 + sig * sqrt(t1) * gen_Zt(1000))
S2 = S1 * exp((r-1/2 * sig^2) * dt1 + sig * sqrt(dt1) * gen_Zt(1000))
S3 = S2 * exp((r-1/2 * sig^2) * dt2 + sig * sqrt(dt2) * gen_Zt(1000))
df = data.frame(S1,S2,S3)
df['payoff'] = apply(df, 1, mean)
df['_temp'] = df['payoff'] - 100
df['0'] = 0
df['Ct'] = apply(df[c('0','_temp')],1,max)
price = exp(-r*t2)*apply(df['Ct'],2,mean)

