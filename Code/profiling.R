library('lineprof')
library('devtools')

load_all('EDISON')

dataset = simulateNetwork(l=60, q=20, cps=c(10,20))
profile = lineprof(EDISON.run(dataset$sim_data, num.iter=5000))
shine(profile)

