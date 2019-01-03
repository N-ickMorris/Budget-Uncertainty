# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

# identify the path that contains the following files:
  # rbm-coefficients-52B.csv
  # gni-country.csv
  # dosage-current.csv
  # production-cost.csv
input.files = "/shared/.../Fall-2017/Info-52B-14M/"
setwd(input.files)

# choose a name for a folder that will contain the ampl dat files and the doe.csv file
# the ampl dat files are what will be used to run the ABP model
# the doe.csv file is what defines the experimental design
folder = "DataIn2"

# choose a path to place "folder"
output.files = "/shared/.../Fall-2017/Drop-Cohort"

# create a vector that defines the ranges for the birth cohort drops
# this vector should contain numbers between, but not including, 0 and 1
# these numbers in the vector define the upper bounds of the ranges
# so for example a vector: c(0.1, 0.2, 0.3) would indicate these 3 ranges: [0.01, 0.1], [0.11, 0.2], [0.21, 0.3]
drops = c(0.12, 0.26, 0.4)

# create a vector that defines the ranges for the proportion of countries that face a cohort drop
# this vector should contain numbers between, but not including, 0 and 1
# these numbers in the vector define the upper bounds of the ranges
# so for example a vector: c(0.1, 0.2, 0.3) would indicate these 3 ranges: [0.01, 0.1], [0.11, 0.2], [0.21, 0.3]
impacted = c(0.2, 0.4, 0.6)

# assign a value of TRUE or FALSE to impact.all
# a value of TRUE means that 1 more level will be added to "impacted" where all countries face a random drop in cohort
impact.all = TRUE

# choose the number of replications
# these replications define how many times to randomly sample within each of the ranges defined in "drops"
# so for example if replications = 50 and drops = c(0.1, 0.2, 0.3), then 50 values within [0.01, 0.1] are sampled, 50 values within [0.11, 0.2] are sampled, and 50 values within [0.21, 0.3] are sampled
# the sampling is done using a uniform distribution with its lower and upper bounds defined by each of the ranges in "drops"
replications = 50

# -----------------------------------------------------------------------------------
# ---- Packages ---------------------------------------------------------------------
# -----------------------------------------------------------------------------------

# make sure you have installed all of these packages, or else the script file won't produce any results

{

# these are the packages needed for this script file

# data handling
require(data.table)

# parallel computing
require(foreach)
require(parallel)
require(doSNOW)

}

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# these are functions i made for this script file

# ---- prints out a dat file object in ampl syntax ----------------------------------

ampl = function(dat, object = "param", name = "c")
{
  # make sure the data is a data frame object
  dat = data.frame(dat)
  
  # every parameter/set object in an ampl dat file must end with a semicolon
  # so set up 1 semicolon to give to dat
  semicolon = c(";", rep(" ", ncol(dat) - 1))
  
  # add this semicolon as the last row of the data frame
  result = data.frame(rbind(dat, semicolon))
  
  # every parameter/set object in an ample dat file must begin with the name of the object and what it equals
  # for example: param c := 
  # so set up a header to give to dat
  header = c(paste(object, name, ":="), rep(" ", ncol(dat) - 1))
  
  # update the column names of dat to be the header we created
  colnames(result) = header
  
  # print out the result without any row names
  # print out the result left adjusted
  # print(result, right = FALSE, row.names = FALSE)
  
  return(result)	
}

}

# -----------------------------------------------------------------------------------
# ---- Import Data ------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# import the rbm-coefficients-52B.csv, gni-country.csv, dosage-current.csv, and production-cost.csv files
rbm = data.table(read.csv("rbm-coefficients-52B.csv"))
gni = data.table(read.csv("gni-country.csv"))
dosage = data.table(read.csv("dosage-current.csv"))
cost = data.table(read.csv("production-cost.csv"))

# extract the number of bundles from rbm
bundles = max(rbm$Bundle)

# extract the different market segments from gni
markets = sort(unique(gni$Markets))

# extract the number of countries from gni
countries = nrow(gni) / length(markets)

# extract the different MARR values from cost
marr = sort(unique(cost$MARR))

# update marr to be just the min and max value so the size of the experiment isn't so large
# marr = c(min(marr), max(marr))

}

# -----------------------------------------------------------------------------------
# ---- Building the Experiment ------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# lets build our experiment
  # the market segmentations will vary according to "markets"
  # a single market at a time will experience a random cohort drop
  # the severity of the cohort drop will vary across the levels defined by "drops"
  # a random portion of countries will face the cohort drop
  # the proportion of the countries that are impacted will vary across the levels defined by "impacted" and "impact.all"
  # the MARR will vary according to "marr"

# set up a loop for each market segment
# this will ensure all market segments are in our experiment
doe = foreach(i = markets) %do%
{
  # set up a loop for each market
  # this will ensure that only a single market at a time will face a cohort drop
  market.output = foreach(j = 1:i) %do%
  {
    # if impact.all is TRUE then add one more level to impacted indicating that all countries face a cohort drop
    if(impact.all)
    {
      impacted.adjusted = c(impacted, 1)
      
    } else
    {
      impacted.adjusted = impacted
    }
    
    # for Market j in Market Segmentation i, create all combination of:
      # cohort drops
      # countries impacted
      # MARR
    output = data.table(expand.grid(Upper_Cohort_Drop = drops, 
                                    Upper_Countries_Impacted = impacted.adjusted,
                                    MARR = marr,
                                    MarketID = j, 
                                    Markets = i,
                                    Replications = replications))
    
    return(output)
  }
  
  # combine the list of data.tables into one table
  doe.output = rbindlist(market.output)
  
  return(doe.output)
}

# combine the list of data.tables into one table
doe = rbindlist(doe)

# lets create a vector that tells us the lower bound for each value in drops
# compute the sequential differences between the values in drops
drop.diff = diff(drops)

# offset the differences by 1%
drop.diff = drop.diff - 0.01

# we know the first lower bound is 1%
drop.lower = 0.01

# use drop.diff to compute the remaining lower bounds
drop.lower = c(drop.lower, drops[-1] - drop.diff)

# make a table that maps the lower drop to the upper drop
drop.table = data.table(Upper_Cohort_Drop = drops, 
                        Lower_Cohort_Drop = drop.lower)

# lets create a vector that tells us the lower bound for each value in impacted
# compute the sequential differences between the values in impacted
impacted.diff = diff(impacted)

# offset the differences by 1%
impacted.diff = impacted.diff - 0.01

# we know the first lower bound is 1%
impacted.lower = 0.01

# use impacted.diff to compute the remaining lower bounds
impacted.lower = c(impacted.lower, impacted[-1] - impacted.diff)

# make a table that maps the lower impacted to the upper impacted
impacted.table = data.table(Upper_Countries_Impacted = impacted, 
                            Lower_Countries_Impacted = impacted.lower)

# if impact.all is TRUE then add one more row to impacted.table indicating a level where all countries face a cohort drop
if(impact.all)
{
  impacted.table = rbind(impacted.table,
                         data.table(Upper_Countries_Impacted = 1,
                                    Lower_Countries_Impacted = 1))
}

# give doe an ID column to indicate the row number
# we are doing this so we can retain the original order of rows after joining
doe[, ID := 1:nrow(doe)]

# make Upper_Cohort_Drop the key column in doe and drop.table
setkey(doe, Upper_Cohort_Drop)
setkey(drop.table, Upper_Cohort_Drop)

# join drop.table onto doe
doe = drop.table[doe]

# make Upper_Countries_Impacted the key column in doe and impacted.table
setkey(doe, Upper_Countries_Impacted)
setkey(impacted.table, Upper_Countries_Impacted)

# join drop.table onto doe
doe = impacted.table[doe]

# order doe by ID
doe = doe[order(ID)]

# remove ID from doe
doe[, ID := NULL]

# lets set up seeds for each row in doe such that our experiment is repeatable
set.seed(42)
doe.seeds = sample(1:(100 * nrow(doe)), nrow(doe))

# choose the number of workers and tasks for parallel processing (if you want to)
# i normally like to use 2/3 of the available cores
# heres how many core we have
detectCores()

# choose the number of workers
workers = max(1, floor((2/3) * detectCores()))

# set up a cluster for parallel processing if workers > 1, otherwise don't set up a cluster
if(workers > 1)
{
  # setup parallel processing
  cl = makeCluster(workers, type = "SOCK", outfile = "")
  registerDoSNOW(cl)
  
  # define %dopar% for parallel processing
  `%fun%` = `%dopar%`
  
} else
{
  # define %do% for sequential proccessing
  `%fun%` = `%do%`
}

# create the cohort drops for each country
doe.dat = foreach(i = 1:nrow(doe)) %fun%
{
  # load the packages we will need for these tasks
  require(data.table)
  
  # set up seeds for each replication
  set.seed(doe.seeds[i])
  rep.seeds = sample(1:(100 * replications), replications)
  
  # lets randomly choose cohort drops for each replication
  percent.drop = lapply(1:replications, function(k)
  {
    # pull the min value for a random cohort drop in scenario i
    drop.min.val = doe$Lower_Cohort_Drop[i]
    
    # pull the max value for a random cohort drop in scenario i
    drop.max.val = doe$Upper_Cohort_Drop[i]
    
    # pull the min value for random countries to impact in scenario i
    impacted.min.val = doe$Lower_Countries_Impacted[i]
    
    # pull the max value for random countries to impact in scenario i
    impacted.max.val = doe$Upper_Countries_Impacted[i]
    
    # set the seed from rep.seeds
    set.seed(rep.seeds[k])
    
    # randomly sample, from a uniform distribtuion, the cohort drops for each country
    random.drops = runif(n = countries, 
                         min = drop.min.val,
                         max = drop.max.val)
    
    # set the seed based on rep.seeds
    set.seed(rep.seeds[k])
    
    # randomly sample, from a uniform distribtuion, the portion of countries to impact
    random.impact = runif(n = 1, 
                          min = impacted.min.val,
                          max = impacted.max.val)
    
    # set the seed based on rep.seeds
    set.seed(rep.seeds[k] + 42)
    
    # randomly sample, from a uniform distribtuion, the portion of countries to impact
    random.impact = runif(n = 1, 
                          min = impacted.min.val,
                          max = impacted.max.val)
    
    # set the seed based on rep.seeds
    set.seed(rep.seeds[k] + 21)
    
    # randomly sample, from a binomial distribution, which countries get impacted
    country.impacted = rbinom(n = countries, 
                              size = 1, 
                              prob = random.impact)
    
    # create a table of random.drops and country.impacted
    output = data.table(PercentCohortDrop = random.drops,
                        CountryImpacted = country.impacted)
    
    return(output)
  })
  
  # apply the cohort drops to each country, then compute weighted gni, and then compute reservation price
  dat.list = lapply(1:replications, function(j)
  {
    # extract the subset of gni corresponding to row i of doe
    dat = data.table(gni[Markets == doe$Markets[i]])
    
    # create a column that indicates if a market is being impacted in this row of doe
    dat[, MarketImpacted := as.numeric(MarketID == doe$MarketID[i])]
    
    # create a column of the percent drop for each country
    dat[, PercentCohortDrop := percent.drop[[j]]$PercentCohortDrop]
    
    # create a column of the country impacted
    dat[, CountryImpacted := percent.drop[[j]]$CountryImpacted]
    
    # compute the new cohort due to the cohort drop, country impact, and market impact
    dat[, newCohort := round(AnnBirths * (1 - (PercentCohortDrop * MarketImpacted * CountryImpacted)), 0)]
    
    # compute the weighted gni for each market
    dat = dat[, .(WGNI = sum(newCohort * GNI) / sum(newCohort), 
                  newCohort = sum(newCohort),
                  MarketImpacted = mean(MarketImpacted),
                  Markets = mean(Markets)),
              by = MarketID]
    
    # create all combinations of bundles and markets
    comb = data.table(expand.grid(Bundle = 1:bundles,
                                  MarketID = dat$MarketID))
    
    # set Bundle as the key column in rbm and in comb
    setkey(rbm, Bundle)
    setkey(comb, Bundle)
    
    # join rbm onto comb
    comb = data.table(rbm[comb])
    
    # set MarketID as the key column in dat and in comb
    setkey(dat, MarketID)
    setkey(comb, MarketID)
    
    # join dat onto comb
    dat = data.table(dat[comb])
    
    # compute Rbm for each bundle in each market for market segmentation i
    dat[, "Rbm" := Intercept + (GNI_coef * WGNI)]
    
    # update dat to only have Bundle, MarketID, MarketImpacted, Rbm, WGNI, and newCohort as columns
    dat = dat[,.(Bundle, Markets, MarketID, MarketImpacted, Rbm, WGNI, newCohort)]
    
    # add doe parameters to dat
    dat[, Upper_Cohort_Drop := doe$Upper_Cohort_Drop[i]]
    dat[, Lower_Cohort_Drop := doe$Lower_Cohort_Drop[i]]
    dat[, Upper_Countries_Impacted := doe$Upper_Countries_Impacted[i]]
    dat[, Lower_Countries_Impacted := doe$Lower_Countries_Impacted[i]]
    dat[, MARR := doe$MARR[i]]
    dat[, Scenario := rep(i, nrow(dat))]
    dat[, Replication := rep(j, nrow(dat))]
    
    return(dat)
  })
  
  # combine the list of data tables into 1 data table
  dat.list = rbindlist(dat.list)
  
  return(dat.list)
}

# end the cluster if it was set up
if(workers > 1)
{
  stopCluster(cl)
  rm(cl)
}

# combine the list of data tables into 1 data table
doe.dat = rbindlist(doe.dat)

# lets ensure that the reservation price for a bundle is always higher in a higher income market
# we will do this with the following computation:
  # R[b, m + 1] = min(R[b, m] * 0.999, R[b, m + 1]) for every bundle b and market m

# lets create a new column, final.Rbm, to take on the adjusted values for Rbm
doe.dat[, final.Rbm := Rbm]

# order doe.dat by Markets, MarketID, Bundle
doe.dat = doe.dat[order(Markets, MarketID, Bundle)]

# lets ensure tiered pricing between market IDs for each bundle:
  # for every scenario and replication, shift the reservation prices by 1 market so that all market j and market j + 1 prices can be compared side by side
  # we have to do this shifting (max(markets) - 1) many times to compare all markets against eachother; for example: 
  # if the reservation prices of the highest income market in a 12 market system drop by a large enough amount,
  # then this may require all lower income markets to incrementally reduce their reservation prices to ensure market j prices > market j + 1 prices
for(i in 1:(max(markets) - 1))
{
  # shift the reservation prices by 1 market such that all market j and market j + 1 prices can be compared side by side
  # also multiply the shifted reservation prices by 0.999 so we can ensure that the reservation prices of market j + 1 are less than market j
  doe.dat[, shift.Rbm := 0.999 * shift(x = final.Rbm, n = bundles), by = .(Scenario, Replication)]
  
  # update the value of final.Rbm by taking the minimum value between final.Rbm and shift.Rbm
  doe.dat[, final.Rbm := pmin(final.Rbm, shift.Rbm, na.rm = TRUE)]
}

# remove shift.Rbm as a column in doe.dat
doe.dat[, shift.Rbm := NULL]

# compute the change in reservation price due to tiered pricing
doe.dat[, changeRbm := (final.Rbm - Rbm) / Rbm]

# remove objects we no longer need
if(workers == 1)
{
  rm(dat, dat.list, percent.drop, rep.seeds, j)
}

rm(doe.output, output, doe.seeds, `%fun%`, drop.diff, i, j, gni, rbm,
   market.output, markets, workers, bundles, drop.lower, countries,
   drops, replications, drop.table, impacted.lower, impacted.table)

# free up memoery in RAM
gc()

}

# -----------------------------------------------------------------------------------
# ---- Exporting the Data Files -----------------------------------------------------
# -----------------------------------------------------------------------------------

{

# create a fileID number for each data set
# lets create a column where we combine scenario and replication, to get a fileID indicator for each data set
doe.dat[, fileID := paste(Scenario, Replication, sep = "_")]

# make fileID into a factor data type
doe.dat[, fileID := factor(fileID, levels = unique(fileID))]

# now make fileID into a number based on the order of its levels
doe.dat[, fileID := as.numeric(fileID)]

# lets create an object DT that will hold the ampl dat file information
DT = data.table(doe.dat[, .(fileID, Markets, Bundle, MarketID, MARR, final.Rbm, newCohort, WGNI)])

# create a folder to put our ampl dat files in
if(!file.exists(file.path(output.files, folder)))
{
  dir.create(file.path(output.files, folder))
}

# write out each dat file into the folder we just created
# choose the number of workers
workers = max(1, floor((2/3) * detectCores()))

# set up a cluster for parallel processing if workers > 1, otherwise don't set up a cluster
if(workers > 1)
{
  # setup parallel processing
  cl = makeCluster(workers, type = "SOCK", outfile = "")
  registerDoSNOW(cl)
  
  # define %dopar% for parallel processing
  `%fun%` = `%dopar%`
  
} else
{
  # define %do% for sequential proccessing
  `%fun%` = `%do%`
}

# create the data files in parallel
foreach(i = 1:max(DT$fileID)) %fun%
{
  # load packages we need for our tasks
  require(data.table)
  
  # write out R{B,M} ~ reservation price per bundle per market
  write.table(ampl(DT[fileID == i, .(Bundle, MarketID, final.Rbm)], name = "R"), 
              file = paste0(output.files, "/", folder, "/", "fileID_", i, ".dat"), 
              quote = FALSE,
              row.names = FALSE)
  
  # extract the information for annual birth cohort and weighted gni per market
  DT.M = data.table(DT[fileID == i, .(MarketID, newCohort, WGNI)])
  
  # only keep the unique values
  DT.M = DT.M[!duplicated(DT.M)]
  
  # write out l{M} ~ annual birth cohort per market
  write.table(ampl(DT.M[,.(MarketID, newCohort)], name = "l"), 
              file = paste0(output.files, "/", folder, "/", "fileID_", i, ".dat"), 
              quote = FALSE,
              row.names = FALSE,
              append = TRUE)
  
  # write out gni_p{M} ~ weighted gni per market
  write.table(ampl(DT.M[,.(MarketID, WGNI)], name = "gni_p"), 
              file = paste0(output.files, "/", folder, "/", "fileID_", i, ".dat"), 
              quote = FALSE,
              row.names = FALSE,
              append = TRUE)
  
  # write out d{A,M} ~ dosage requirement per antigen per market
  write.table(ampl(dosage[Markets == DT[fileID == i]$Markets[1], .(Antigen, MarketID, DosePerChild)], name = "d"), 
              file = paste0(output.files, "/", folder, "/", "fileID_", i, ".dat"), 
              quote = FALSE,
              row.names = FALSE,
              append = TRUE)  
  
  # write out n_markets ~ the number of markets
  write.table(ampl(data.frame("n" = DT[fileID == i]$Markets[1]), name = "n_markets"), 
              file = paste0(output.files, "/", folder, "/", "fileID_", i, ".dat"), 
              quote = FALSE,
              row.names = FALSE,
              append = TRUE)
  
  # write out C{B} ~ the cost (MARR included) of providing each bundle
  write.table(ampl(cost[MARR == max(DT[fileID == i, MARR]), .(Bundle, Cost)], name = "C"), 
              file = paste0(output.files, "/", folder, "/", "fileID_", i, ".dat"), 
              quote = FALSE,
              row.names = FALSE,
              append = TRUE)
  
}

# end the cluster if it was set up
if(workers > 1)
{
  stopCluster(cl)
}

# write out the doe data to a csv file in the same location as the ampl dat files
write.csv(doe.dat, 
          file = paste0(output.files, "/", folder, "/", "doe.csv"), 
          quote = FALSE,
          row.names = FALSE)

# free memory in RAM
gc()

}




