# ----------------
# SUCESIONS
# ----------------

an <- function(n){
  (2*n+4)/n
}

an_tendeixe <- function(n){
  (1+1/n)^n
}

# Tendeix a 2 ..
plot(an(1:100))
abline(h=2, col="red")
# OK
an(1000000)
# ERR (aprox. 2 ...)
an(1000000000)

# Sucesión convergente a x:
# Dada una sucesión de números reales {xn} decimos que esta sucesión "converge al número x" (también se dice que la sucesión "tiene por límite x") si:
#    cuando x->inf, lim{an-l}=0 ... o be ...
#    ... Per tot e>0, existeix p € N : per tot n >= p, |an - l| < e 
  
# DEMO
# 1) Descriure |an - l|
# 2) Aillar la n de l'expressió |an - l| < e
  
# p.ex
# 1) | (2n+4)/n - 2 | = | 4/n | = 4/n
# 2) 4/n < e ; n > 4/e
#      p.ex si considero un error de 0.02, ho tindré a partir de ...
#           e=0.02 --> n > 4/0.02 --> a200;  an(200)=2.02


# e 2,7182...
exp(1)
an_tendeixe(10)
an_tendeixe(100)
an_tendeixe(1000)
an_tendeixe(1000000)
