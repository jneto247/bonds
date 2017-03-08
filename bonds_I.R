# função de cálculo do preço de título
# p = principal, r= taxa de cupom, ttm = vencimento, y = yield
bondprc <- function(p, r, ttm, y) {
  cf <- c(rep(p * r, ttm - 1), p * (1 + r))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + y)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  sum(cf$pv)
}

# Criando vetor de fluxo de caixa (valores simbólicos)
cf <- c(1,2,3,4,5)

# Criando função de avliação do valor do fundo
bval <- function(i, cf,
                 t=seq(along = cf)) 
  sum(cf / (1 + i)^t)

# Criando função ytm() usando uniroot
ytm <- function(cf) {
  uniroot(bval, c(0, 1), cf = cf)$root
}

# Use ytm() function to find yield
ytm(cf)

