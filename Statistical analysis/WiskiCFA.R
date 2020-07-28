library(lavaan)
library(dplyr)
library(reshape2)
library(xtable)

subs.valid.bfi = read.csv("Datasets/submissionsBFI.csv")
subs.valid.bfi10 = subs.valid.bfi %>% dplyr::select(O4,O9,C1,C5,E2,E8,A1,A5,N2,N8)
subs.valid.hex = read.csv("Datasets/submissionsHexad.csv")

# CFA models
model.ffm = readLines("Statistical analysis/modelFFM.R")
model.hex = readLines("Statistical analysis/modelHexad.R")
model.ffm10 = readLines("Statistical analysis/modelFFM10.R")
model.ffm.filtered = readLines("Statistical analysis/modelFFMfiltered.R")
model.hex.filtered = readLines("Statistical analysis/modelHexadfiltered.R")

# CFA for BFI and Hexad
cfa.execute = function(data, model, e, gof) {
  cfa = cfa(model, data, estimator = e)
  fit = fitMeasures(cfa, gof)
  fit["chisq/df"] = fit["chisq"] / fit["df"]
  return(fit)
}
# goodness of fit indices for the CFA
cfa.gof = c("chisq", "df", "pvalue", "srmr", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "cfi", "tli", "gfi", "agfi")
gofs.bfi = cfa.execute(subs.valid.bfi, model.ffm, "ML", cfa.gof)
gofs.hex = cfa.execute(subs.valid.hex, model.hex, "ML", cfa.gof)
gofs.bfi10 = cfa.execute(subs.valid.bfi10, model.ffm10, "ML", cfa.gof)
gofs.bfi.filtered = cfa.execute(subs.valid.bfi, model.ffm.filtered, "ML", cfa.gof)
gofs.hex.filtered = cfa.execute(subs.valid.hex, model.hex.filtered, "ML", cfa.gof)

# Store goodness of fit indices in tex table
gofs.result = gofs.bfi10 %>%
  melt() %>%
  rename("BFI-10" = value) %>%
  cbind(gofs.bfi %>% melt()) %>%
  rename("BFI-44" = value) %>%
  cbind(gofs.hex %>% melt()) %>%
  rename("Hexad" = value) %>%
  mutate(Index = toupper(rownames(.))) %>%
  dplyr::select(c("Index", "BFI-10", "BFI-44", "Hexad"))
bold = function(x) {paste('{\\textbf{',x,'}}', sep ='')}
row.names(gofs.result) = NULL
print(
  xtable(gofs.result,
         label = "tab:gofsbfihex",
         caption = "Goodness of fit indices for BFI-10, BFI-44 and Hexad.",
         digits = 2,
         align = c("l","@{}l","r","r","r@{}")),
  file = "Tables/gofsBFIHexad.tex",
  sanitize.colnames.function=bold,
  booktabs = TRUE,
  include.rownames=FALSE
)

# Residuals for BFI and Hexad
get.residuals = function(data, model, e) {
  fit = cfa(model, data, estimator = e)
  return(resid(fit, type="standardized")$cov)
}
resids.bfi = get.residuals(subs.valid.bfi, model.ffm, "ml")
resids.hex = get.residuals(subs.valid.hex, model.hex, "ml")

# Threshold values according to http://statwiki.kolobkreations.com/index.php?title=Confirmatory_Factor_Analysis
cfa.pass.output = function(measures) {
  pass = c()
  pass["pvalue"] = measures["pvalue"] > 0.05
  pass["cfi"] = if (measures["cfi"] > 0.95)
    "great"
  else if (measures["cfi"] > 0.9)
    "traditional"
  else if (measures["cfi"] > 0.8)
    "sometimes permissible"
  else
    "FALSE"
  pass["gfi"] = measures["gfi"] > 0.95
  pass["agfi"] = measures["agfi"] > 0.8
  pass["srmr"] = measures["srmr"] < 0.09
  pass["rmsea"] = if (measures["rmsea"] < 0.05)
    "good"
  else if (measures["rmsea"] <= 0.10)
    "moderate"
  else
    "bad"
  pass["chisq/df"] = if (measures["chisq/df"] < 3)
    "good"
  else if (measures["chisq/df"] < 5)
    "sometimes permissible"
  else
    "FALSE"
  return(pass)
}
cfa.pass.output(gofs.bfi)
cfa.pass.output(gofs.bfi10)
cfa.pass.output(gofs.hex)

# Parameter estimates for BFI and Hexad
estimates = function(m, d) {
  solution = standardizedSolution(cfa(model = m, data = d))
  solution %>%
    dplyr::select(lhs,est.std)
}
estimates(model.ffm, subs.valid.bfi)
standardizedSolution(cfa(model = model.ffm10, data = subs.valid.bfi10))
estimates(model.hex, subs.valid.hex)
