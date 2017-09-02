require(ggplot2)
require(minpack.lm)

# calculate denisty historgam from a data table with columns:
# y, id, group
calcDensHist = function(x, n) {
  loc.res = hist(x$y, n, plot = FALSE)
  return(data.table(x = loc.res$mids, 
                    y = loc.res$density,
                    group = first(x$group)))
}

# https://stackoverflow.com/questions/29067916/r-error-function-erfz
# Abramovitz & Stegun 29.2.29
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)


# cell cycle model: G1 or G2 peak as gaussian
modelG.norm = function(x, A1 = 8200, x1 = 24, s1 = 2) {
  return( A1 * dnorm(x, x1, s1))
}

# Cell cycle model: S phase as a flat horizontal line
# Depth of overlap usually set at the mean of G1/2 peaks
# The steepness set by params z1, z2
# The
modelS.lin = function(x, a = 200, x1 = 24, d = 2, z1 = 1, z2 = 1) {
  return( (erf((x1 * d  - x)/sqrt(2*z1)) - erf( (x1 - x)/sqrt(2*z2) )) * a / ( 2 * (d-1) * x1))
}

# definition of a cell cycle model:
# G1 and G2 phases as single gaussian peaks
# S phase as zero order linear (erf and erfc on boundaries)
f.modelG12S0 = function(x, in.par = list(A1 = 1, x1 = 20, d = 2, s1 = 2, A2 = 0.5, s2 = 2, sa = 1)) {
  (modelG.norm(x, in.par[['A1']], in.par[['x1']], in.par[['s1']]) + 
    modelG.norm(x, in.par[['A2']], in.par[['x1']] * in.par[['d']], in.par[['s2']]) + 
    modelS.lin(x, in.par[['sa']], in.par[['x1']], in.par[['d']], 1, 1))}

# Fit model to data and return dt with coefficientts in rows and values of stats in columns
# The position of beginning and end of the S phase (params sx1 and sx2) is the same as peak means of G1 and G2, respectively
# The steepness of the rise and decay of S phase (params sz1 and sz2) is set to 1.
fit.modelG12S0 = function(in.data, in.start = list(G1 = 1, x1 = 20, d = 2, s1 = 2, G2 = .5, s2 = 2, S = 2)) {
  
  loc.fit = NULL
  loc.fit = try(
    nlsLM(data = in.data, 
                  formula = y ~ f.modelG12S0(x, list(A1 = G1, x1 = x1, d = d, s1 = s1, A2 = G2, s2 = s2, sa = S)), 
                  start = in.start,
                  lower = c(0, 0, 1.9, 0, 0, 0, 0), 
                  trace = FALSE),
  silent = TRUE)
  
  if (class(loc.fit) == "try-error")
    return(NULL)
  
  loc.sum = summary(loc.fit)
  loc.coeff = as.data.table(loc.sum$coefficients)
  loc.coeff[, coeff := rownames(loc.sum$coefficients)]
  loc.coeff[, group := first(in.data$group)]

  return(loc.coeff)
} 


# Calculate histogram based on fitted params for a given x.arg
# Returns dt with x and y values based on fitted model by group
# Arguments:
# dt.res - dt with fitted params for every group; 
#          this is typically a summary from fitting:
#      Estimate Std. Error   t value     Pr(>|t|) coeff group
# 1:  0.3931549 0.01607081  24.46392 6.480045e-32    A1     1
# 2: 20.0811601 0.08444782 237.79369 4.063059e-87    x1     1
# 3:  1.9000000 0.08116291  23.40971 6.381639e-31    s1     1
#
# in.expr - an expression to evaluate on parameters in coeff column of dt.res
#           (dt.res is later changed into wide format with params in separate columns)
#           e.g. f.modelG12S0(x, list(A1 = A1, x1 = x1, d = d, s1 = s1, A2 = A2, s2 = s2, sa = sa))
# in.group.col - name of the column for grouping
# in.params.col - name of the column with parameter names
# in.meas.col - name of the column with paramater values
# x.min, x.max - min and max of x-axis
# x.int - number of points to divide x.in - x.max interval
#
# Example f-n call:
#     loc.dt.fit.all = calcFittedHist(loc.dt.model, 
#                                     f.modelG12S0(x, list(A1 = A1, x1 = x1, d = d, s1 = s1, A2 = A2, s2 = s2, sa = sa)),
#                                     'group',
#                                     loc.y.min, 
#                                     loc.y.max)

calcFittedHist = function(dt.res, in.expr, 
                          in.group.col = 'group', 
                          in.params.col = 'coeff', 
                          in.meas.col = 'Estimate', 
                          x.min = 0, x.max = 1, x.int = NULL) {
  
  if(is.null(dt.res))
    return(NULL)
  
  # default number of plotting points: 100+1
  if(is.null(x.int))
    x.int = ((x.max - x.min) / 100.0)
  
  # Convert the table with fitted parameters to wide format
  # From columns: Estimate, Std. Error, t value, Pr(>|t|), coeff,  group
  # To columns (for instance): group, A1, A2, s1, s2, sa, x1, x2
  # Makes it easier to calculate the response by having parameters in separate columns
  loc.dt.res.cast = dcast(dt.res, paste0(in.group.col, ' ~ ', in.params.col), value.var = in.meas.col)
  
  # prepare x.arg for plotting histogram from fitted data
  loc.x.arg = seq(x.min, x.max, x.int)
  
  # prepare a dt with x.arg for every group
  # this is later merged with dt with fitted params
  loc.dt.x = data.table(x = rep(loc.x.arg, length(unique(loc.dt.res.cast[[in.group.col]]))),
                        xx = rep(loc.dt.res.cast[[in.group.col]], each = length(loc.x.arg)))
  setnames(loc.dt.x, 'xx', in.group.col)
  
  # merged dt contains x.arg for every group with columns holding fitted params
  loc.dt.resp.fit = merge(loc.dt.res.cast, loc.dt.x, by = in.group.col)
  
  # Add a column with an expression evaluated on columns with fitted params
  # This isn't very data-tably but calling it:
  # loc.dt.resp.fit[, y := eval(substitute(in.expr))]
  # doesn't work (see: https://stackoverflow.com/questions/11872499/create-an-expression-from-a-function-for-data-table-to-eval)
  # What works is:
  # loc.dt.resp.fit[, eval(substitute(in.expr))]
  # But then it's a new dt instead of an old dt with extra column.
  loc.dt.resp.fit[['y']] = eval(substitute(in.expr), loc.dt.resp.fit, parent.frame())
  loc.dt.resp.fit = loc.dt.resp.fit[, c(in.group.col, 'x', 'y'), with = FALSE]

  return(loc.dt.resp.fit)
}

# calculate cell-cycle percentages from fitted model
# This is for G1G2S0 model, therefore coeffiecients of interest are A1, A2, sa,
# for two gaussians and flat S-phase, respectively
# Arguments:
# inDT - dt with fitted params for every group; 
#          this is typically a summary from fitting:
#      Estimate Std. Error   t value     Pr(>|t|) coeff group
# 1:  0.3931549 0.01607081  24.46392 6.480045e-32    A1     1
# 2: 20.0811601 0.08444782 237.79369 4.063059e-87    x1     1
# 3:  1.9000000 0.08116291  23.40971 6.381639e-31    s1     1
#
# inParams - vector with names of porameters to select
#            ususally inDT contains multiple parameters fitted in the model;
#            calculating percentages requires only a couple of them
#
# inParamsNew - vector with new column names after casting,
#               e.g. inParams = c('A1', 'A2', 'sa') -> inParamsNew = c('G1', 'G2', 'S')
# 
# inParamsCol - name of the column that contains parameter names
#               necessary for selecting parameters
#
# inMeas - name of the column with parameter values
# inMeasSuff - suffix to add for the column name with percentages
# inGroup - name of the grouping column
calcCCperc = function(inDT, 
                      inParams = c('G1', 'G2', 'S'),
                      inParamsCol = 'coeff', 
                      inMeas = 'Estimate', 
                      inMeasSuff = '.perc', 
                      inGroup = 'group') {
  if (is.null(inDT))
    return(NULL)
  
  # extract rows with fitted A1, A2, and sa params
  locDT = inDT[get(inParamsCol) %in% inParams]
  
  # calculate percentages per group
  if (is.null(inGroup))
    locDT[, V1 := get(inMeas) / sum(get(inMeas)) * 100]
  else
    locDT[, V1 := get(inMeas) / sum(get(inMeas)) * 100, by = inGroup]

  setnames(locDT, 'V1', paste0(inMeas, inMeasSuff))
  
  
  # Convert the table with fitted parameters to wide format
  # From columns: Estimate, Std. Error, t value, Pr(>|t|), coeff, coeff.perc, group
  # To columns (for instance): group, A1, A2, s1, s2, sa, x1, x2
  # Makes it easier to calculate the response by having parameters in separate columns
  locDTcast = dcast(locDT, paste0(inGroup, ' ~ ', inParamsCol), value.var = paste0(inMeas, inMeasSuff))
  
  return(locDTcast)
}



# Standard plotting theme for ggplot
myGgplotTheme = theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.25),
    axis.line.y = element_line(color = "black", size = 0.25),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 14, face = "bold"),
    strip.text.y = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    legend.key = element_blank(),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(2, "lines"),
    legend.position = "right"
  )


# Generate artifical dataset for cell cycle fitting
userDataGen_cellCycle <- function(in.ncells = 1000, in.nsites = 8, in.ncond = 4) {  
  cat(file=stderr(), 'userDataGen: in\n')
  
  # boundaries of fluorescence data
  min.dapi = 10
  max.dapi = 50
  
  # normalized pdf to fit
  f.dens = function(x) { (modelG.norm(x, 1, 20, 2) + modelG.norm(x, .5, 40, 2) + modelS.lin(x, 1, 20, 2, 1, 1)) / (1 + 0.5 + 1) }
  
  # cdf
  f.cdf = function(x) integrate(f.dens, min.dapi, x)[[1]]
  
  # invert cdf
  f.inv <- function(y){uniroot(function(x) { f.cdf(x)-y}, interval=c(min.dapi, max.dapi))$root}
  f.inv <- Vectorize(f.inv)
  
  yy <- runif(in.ncells * in.nsites, 0, 1)   # random sample from U[0,1]
  zz <- f.inv(yy)
  
  dt.nuc = data.table(Metadata_Site = rep(1:in.nsites, each = in.ncells),
                      Metadata_TreatConc = rep(1:in.ncond, each = in.ncells * in.nsites / in.ncond),
                      objNuclei_Intensity_IntegratedIntensity_imDAPIcorrBg  = zz,
                      objNuclei_ObjectNumber = rep(1:in.ncells, in.nsites))
  
  return(dt.nuc)
}

