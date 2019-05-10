# parameters
n = 10000
S0 = 100
K = 100
r = 0.03
TT = 1
sigma = 0.2

# Analytical pricing
# use our function to compute the exact price
# Black-Scholes formula for European call option
bs_call = function(S0, K, r, TT, sig) {
  d1 = (log(S0/K) + (r + 0.5*sig^2)*TT) / (sig*sqrt(TT))
  d2 = d1 - sig*sqrt(TT)
  S0*pnorm(d1) - K*exp(-r*TT)*pnorm(d2)
}
bs_call(S0, K, r, TT, sigma)

# Monte Carlo pricing
Z = rnorm(n)
ST = S0*exp((r-0.5*sigma^2)*TT + sigma*sqrt(TT)*Z)
payoff = pmax(ST-K, 0)
disc_payoff = exp(-r*TT)*payoff
mean(disc_payoff)