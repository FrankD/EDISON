library('devtools')

load_all('EDISON')

dataset = simulateNetwork(l=60, q=20, cps=c(10,20))
test = EDISON.run(dataset$sim_data, num.iter=5000, information.sharing='bino_hard')

