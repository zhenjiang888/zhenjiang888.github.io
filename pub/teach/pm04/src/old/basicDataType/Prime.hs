-- Generating all primes using the method by Eatosthenes

primes = map head (iterate sieve [2..])
sieve (p:xs) = [x | x<-xs, x `mod` p /= 0]

