library(psych)
library(GPArotation)
library(xtable)
library(dplyr)

subs.valid.bfi = read.csv("Datasets/submissionsBFI.csv")
subs.valid.bfi10 = subs.valid.bfi %>% dplyr::select(O4,O9,C1,C5,E2,E8,A1,A5,N2,N8)
subs.valid.hex = read.csv("Datasets/submissionsHexad.csv")

# Principal component analysis (PCA)
pca.execute = function(data, ncomps, rotate) {
  pca = principal(data, nfactors = ncomps, rotate = rotate)
  print(pca$loadings, cutoff = 0.2, sort = TRUE)
}
pca.execute(subs.valid.bfi, 5, "promax")
pca.execute(subs.valid.hex, 6, "promax")

# Mean absolute correlations
mean(abs(cor(subs.valid.bfi)))
mean(abs(cor(subs.valid.bfi10)))
mean(abs(cor(subs.valid.hex)))

# Kaiser-Meyer-Olkin
kmo.execute = function(data) {
  value = KMO(data)$MSA
  rate = "unacceptable"
  if (value >= 0.9)
    rate = "marvelous"
  else if (value >= 0.8)
    rate = "meritorious"
  else if (value >= 0.7)
    rate = "middling"
  else if (value >= 0.6)
    rate = "mediocre"
  else if (value >= 0.5)
    rate = "miserable"
  print(value)
  print(rate)
}
kmo.execute(subs.valid.bfi)
kmo.execute(subs.valid.bfi10)
kmo.execute(subs.valid.hex)

# Bartlett Sphericity
bartlett.execute = function(data) {
  value = cortest.bartlett(cor(data), nrow(data))
  p = value$p.value
  rate = "unsignificant"
  if (p < 0.05)
    rate = "significant"
  print(value)
  print(rate)
}
bartlett.execute(subs.valid.bfi)
bartlett.execute(subs.valid.bfi10)
bartlett.execute(subs.valid.hex)

# EFA factoring
efa.execute = function(data, nfactors, rotate = "promax", fm = "ml") {
  return(fa(
    cor(data),
    nfactors = nfactors,
    rotate = rotate,
    fm = fm
  ))
}
efa.bfi = efa.execute(subs.valid.bfi, 5)
print(efa.bfi, cut = 0.2)
efa.bfi10 = efa.execute(subs.valid.bfi10, 5, rotate = "geominQ")
print(efa.bfi10, cut = 0.13)
efa.hex = efa.execute(subs.valid.hex, 6)
print(efa.hex, cut = 0.2)

# Get the loadings of an EFA result
printLoadings = function(x,
                         digits = 3,
                         cutoff = 0.2,
                         sort = FALSE,
                         ...) {
  Lambda <- unclass(x)
  p <- nrow(Lambda)
  factors <- ncol(Lambda)
  if (sort) {
    mx <- max.col(abs(Lambda))
    ind <- cbind(1L:p, mx)
    mx[abs(Lambda[ind]) < 0.5] <- factors + 1
    Lambda <- Lambda[order(mx, 1L:p),]
  }
  cat("\nLoadings:\n")
  fx <- format(round(Lambda, digits))
  names(fx) <- NULL
  nc <- nchar(fx[1L], type = "c")
  fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
  newx <-
    print(fx, quote = FALSE, ...) # I assigned this to a variable
  vx <- colSums(x ^ 2)
  varex <- rbind(`SS loadings` = vx)
  if (is.null(attr(x, "covariance"))) {
    varex <- rbind(varex, `Proportion Var` = vx / p)
    if (factors > 1)
      varex <- rbind(varex, `Cumulative Var` = cumsum(vx / p))
  }
  cat("\n")
  print(round(varex, digits))
  invisible(newx) #previously returned x
}
efa.loadings = function(efa, cut = 0.2) {
  return(data.frame(printLoadings(efa$loadings, cutoff = cut)))
}

# Store a data frame in a tex file
bold = function(x) {
  paste('{\\textbf{', x, '}}', sep = '')
}
table.save = function(data, l, c, f) {
  print(
    xtable(
      x = data,
      label = l,
      caption = c
    ),
    file = f,
    sanitize.colnames.function = bold,
    booktabs = TRUE
  )
}

# Store the loadings of BFI and Hexad in a tex file
table.save(
  efa.loadings(efa.bfi),
  "tab:loadingsbfi",
  "Rotated factor loadings and communalities based on the correlation matrix of the BFI-44 data.",
  "Tables/loadingsBFI.tex"
)
table.save(
  efa.loadings(efa.bfi10, 0.13),
  "tab:loadingsbfi10",
  "Rotated factor loadings and communalities based on the correlation matrix of the BFI-10 data.",
  "Tables/loadingsBFI10.tex"
)
table.save(
  efa.loadings(efa.hex),
  "tab:loadingshex",
  "Rotated factor loadings and communalities based on the correlation matrix of the Hexad data.",
  "Tables/loadingsHexad.tex"
)

# Store the factor correlations of BFi and Hexad in a tex file
table.save(
  efa.bfi$Phi,
  "tab:corrsbfi",
  "Factor correlations resulting from the EFA for the BFI-44 data.",
  "Tables/corrsBFI.tex"
)
table.save(
  efa.bfi10$Phi,
  "tab:corrsbfi10",
  "Factor correlations resulting from the EFA for the BFI-10 data.",
  "Tables/corrsBFI10.tex"
)
table.save(
  efa.hex$Phi,
  "tab:corrshex",
  "Factor correlations resulting from the EFA for the Hexad data.",
  "Tables/corrsHexad.tex"
)

# Communalities
print(efa.bfi$communalities, digits = 2)
print(efa.hex$communalities, digits = 2)
efa.hex$communalities[efa.hex$communalities < 0.4]

# Cattell's scree test
scree(subs.valid.bfi)
scree(subs.valid.hex)

# Parallel analysis (PA)
pa.execute = function(data) {
  fa.parallel(data, fm = "ml")
}
pa.simulate = function(data, n) {
  tot = 0
  for(i in 1:n) {
    tot = tot + pa.execute(data)$nfact
  }
  print(tot)
  print(tot / n)
}
pa.execute(subs.valid.bfi)
pa.execute(subs.valid.hex)
pa.simulate(subs.valid.bfi, 100)
pa.simulate(subs.valid.hex, 100)

# Optimising BFI
subs.bfi.filtered = subs.valid.bfi %>% dplyr::select(-O7,-A5,-A2,-C2,-O2,-O3,-A9,-N6,-N1,-A6)
efa.bfi.filtered = efa.execute(subs.bfi.filtered, 5)
print(efa.bfi.filtered, cut = 0.16)
table.save(
  efa.loadings(efa.bfi.filtered),
  "tab:loadingsbfifiltered",
  "Standardized loadings (pattern matrix) based on the correlation matrix of the adapted BFI-44 data.",
  "Tables/loadingsBFIfiltered.tex"
)

# Optimising Hexad
#Removed: all F, Ph3, Ac1, P1, D2
subs.hex.filtered = subs.valid.hex %>% dplyr::select(-F1,-F2,-F3,-F4,-D2,-P1,-Ac1,-Ph3)
efa.hex.filtered = efa.execute(subs.hex.filtered, 4)
print(efa.hex.filtered, cut = 0.3)
table.save(
  efa.loadings(efa.hex.filtered),
  "tab:loadingshexfiltered",
  "Standardized loadings (pattern matrix) based on the correlation matrix of the adapted Hexad data.",
  "Tables/loadingsHexadfiltered.tex"
)

subs.sdt = subs.valid.hex %>% dplyr::select(F1,F2,F3,F4,Ac1,Ac2,Ac3,Ac4,S1,S2,S3,S4)
efa.sdt = efa.execute(subs.sdt, 3, rotate = "oblimin")
print(efa.sdt, cut = 0.23)