# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

# identify the path that contains the following files:
  # base-rbm-52B.csv
  # wgnip-market.csv
  # dosage-current.csv
  # production-cost.csv
input.files = "/shared/.../Fall-2017/Info-52B-14M/"
setwd(input.files)

# choose a name for a folder that will contain the ampl dat files and the doe.csv file
# the ampl dat files are what will be used to run the ABP model
# the doe.csv file is what defines the experimental design
folder = "DataIn2"

# choose a path to place "folder"
output.files = "/shared/.../Fall-2017/Drop-Price"

# create a vector that defines the ranges for the reservation price drops
# this vector should contain numbers between, but not including, 0 and 1
# these numbers in the vector define the upper bounds of the ranges
# so for example a vector: c(0.1, 0.2, 0.3) would indicate these 3 ranges: [0.01, 0.1], [0.11, 0.2], [0.21, 0.3]
drops = c(0.12, 0.26, 0.4)

# create a vector that defines the ranges for the proportion of bundles that face a reservation price drop
# this vector should contain numbers between, but not including, 0 and 1
# these numbers in the vector define the upper bounds of the ranges
# so for example a vector: c(0.1, 0.2, 0.3) would indicate these 3 ranges: [0.01, 0.1], [0.11, 0.2], [0.21, 0.3]
impacted = c(0.2, 0.4, 0.6)

# assign a value of TRUE or FALSE to impact.all
# a value of TRUE means that 1 more level will be added to "impacted" where all bundles face a random drop in price
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

# import the base-rbm-52B.csv, wgnip-market.csv, dosage-current.csv, and production-cost.csv files
baserbm = data.table(read.csv("base-rbm-52B.csv"))
wgnip = data.table(read.csv("wgnip-market.csv"))
dosage = data.table(read.csv("dosage-current.csv"))
cost = data.table(read.csv("production-cost.csv"))

# extract the number of bundles from baserbm
bundles = max(baserbm$Bundle)

# extract the different market segments from baserbm
markets = sort(unique(baserbm$Markets))

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
  # a single market at a time will experience a random price drop
  # the severity of the price drop will vary across the levels defined by "drops"
  # a random portion of bundles will face the price drop
  # the proportion of the bundles that are impacted will vary across the levels defined by "impacted" and "impact.all"
  # the MARR will vary according to "marr"

# set up a loop for each market segment
# this will ensure all market segments are in our experiment
doe = foreach(i = markets) %do%
{
  # set up a loop for each market
  # this will ensure that only a single market at a time will face a price drop
  market.output = foreach(j = 1:i) %do%
  {
    # if impact.all is TRUE then add one more level to impacted indicating that all bundles face a price drop
    if(impact.all)
    {
      impacted.adjusted = c(impacted, 1)
      
    } else
    {
      impacted.adjusted = impacted
    }
    
    # for Market j in Market Segmentation i, create all combination of:
      # price drops
      # bundles impacted
      # MARR
    output = data.table(expand.grid(Upper_Price_Drop = drops, 
                                    Upper_Bundles_Impacted = impacted.adjusted,
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
drop.table = data.table(Upper_Price_Drop = drops, 
                        Lower_Price_Drop = drop.lower)

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
impacted.table = data.table(Upper_Bundles_Impacted = impacted, 
                            Lower_Bundles_Impacted = impacted.lower)

# if impact.all is TRUE then add one more row to impacted.table indicating a level where all bundles face a price drop
if(impact.all)
{
  impacted.table = rbind(impacted.table,
                         data.table(Upper_Bundles_Impacted = 1,
                                    Lower_Bundles_Impacted = 1))
}

# give doe an ID column to indicate the row number
# we are doing this so we can retain the original order of rows after joining
doe[, ID := 1:nrow(doe)]

# make Upper_Price_Drop the key column in doe and drop.table
setkey(doe, Upper_Price_Drop)
setkey(drop.table, Upper_Price_Drop)

# join drop.table onto doe
doe = drop.table[doe]

# make Upper_Bundles_Impacted the key column in doe and impacted.table
setkey(doe, Upper_Bundles_Impacted)
setkey(impacted.table, Upper_Bundles_Impacted)

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

# create the price drops for each bundle
doe.dat = foreach(i = 1:nrow(doe)) %fun%
{
  # load the packages we will need for these tasks
  require(data.table)
  
  # set up seeds for each replication
  set.seed(doe.seeds[i])
  rep.seeds = sample(1:(100 * replications), replications)
  
  # lets randomly choose prices drops for each replication
  percent.drop = lapply(1:replications, function(k)
  {
    # pull the min value for a random price drop in scenario i
    drop.min.val = doe$Lower_Price_Drop[i]
    
    # pull the max value for a random price drop in scenario i
    drop.max.val = doe$Upper_Price_Drop[i]
    
    # pull the min value for random bundles to impact in scenario i
    impacted.min.val = doe$Lower_Bundles_Impacted[i]
    
    # pull the max value for random bundles to impact in scenario i
    impacted.max.val = doe$Upper_Bundles_Impacted[i]
    
    # set the seed from rep.seeds
    set.seed(rep.seeds[k])
    
    # randomly sample, from a uniform distribtuion, the price drops for each bundle
    random.drops = runif(n = bundles, 
                         min = drop.min.val,
                         max = drop.max.val)
    
    # set the seed based on rep.seeds
    set.seed(rep.seeds[k])
    
    # randomly sample, from a uniform distribtuion, the portion of bundles to impact
    random.impact = runif(n = 1, 
                          min = impacted.min.val,
                          max = impacted.max.val)
    
    # set the seed based on rep.seeds
    set.seed(rep.seeds[k] + 42)
    
    # randomly sample, from a uniform distribtuion, the portion of bundles to impact
    random.impact = runif(n = 1, 
                          min = impacted.min.val,
                          max = impacted.max.val)
    
    # set the seed based on rep.seeds
    set.seed(rep.seeds[k] + 21)
    
    # randomly sample, from a binomial distribution, which bundles get impacted
    bundle.impacted = rbinom(n = bundles, 
                             size = 1, 
                             prob = random.impact)
    
    # create a table of random.drops and bundle.impacted
    output = data.table(PercentPriceDrop = random.drops,
                        BundleImpacted = bundle.impacted)
    
    return(output)
  })
  
  # apply the price drops to the baseline reservation price of each bundle
  dat.list = lapply(1:replications, function(j)
  {
    # extract the subset of baserbm corresponding to row i of doe
    dat = data.table(baserbm[Markets == doe$Markets[i]])
    
    # create a column that indicates if a market is being impacted in this row of doe
    dat[, MarketImpacted := as.numeric(MarketID == doe$MarketID[i])]
    
    # create a column of the percent drop for each bundle
    dat[, PercentPriceDrop := percent.drop[[j]]$PercentPriceDrop]
    
    # create a column of the bundle impacted
    dat[, BundleImpacted := percent.drop[[j]]$BundleImpacted]
    
    # compute the new reservation price due to the price drop, bundle impact, and market impact
    dat[, newRbm := baseRbm * (1 - (PercentPriceDrop * MarketImpacted * BundleImpacted))]
    
    # add doe parameters to dat
    dat[, Upper_Price_Drop := doe$Upper_Price_Drop[i]]
    dat[, Lower_Price_Drop := doe$Lower_Price_Drop[i]]
    dat[, Upper_Bundles_Impacted := doe$Upper_Bundles_Impacted[i]]
    dat[, Lower_Bundles_Impacted := doe$Lower_Bundles_Impacted[i]]
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

# lets create a new column, final.newRbm, to take on the adjusted values for newRbm
doe.dat[, final.newRbm := newRbm]

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
  doe.dat[, shift.newRbm := 0.999 * shift(x = final.newRbm, n = bundles), by = .(Scenario, Replication)]
  
  # update the value of final.newRbm by taking the minimum value between final.newRbm and shift.newRbm
  doe.dat[, final.newRbm := pmin(final.newRbm, shift.newRbm, na.rm = TRUE)]
}

# remove shift.newRbm as a column in doe.dat
doe.dat[, shift.newRbm := NULL]

# compute the change in reservation price due to tiered pricing
doe.dat[, changeRbm := (final.newRbm - newRbm) / newRbm]

# remove objects we no longer need
if(workers == 1)
{
  rm(dat, dat.list, percent.drop, rep.seeds, j)
}

rm(doe.output, output, doe.seeds, `%fun%`, baserbm, drop.diff, i, j,
   market.output, markets, workers, bundles, drop.lower, 
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
DT = data.table(doe.dat[, .(fileID, Markets, Bundle, MarketID, MARR, final.newRbm)])

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
  write.table(ampl(DT[fileID == i, .(Bundle, MarketID, final.newRbm)], name = "R"), 
              file = paste0(output.files, "/", folder, "/", "fileID_", i, ".dat"), 
              quote = FALSE,
              row.names = FALSE)
  
  # write out l{M} ~ annual birth cohort per market
  write.table(ampl(wgnip[Markets == DT[fileID == i]$Markets[1], .(MarketID, AnnBirths)], name = "l"), 
              file = paste0(output.files, "/", folder, "/", "fileID_", i, ".dat"), 
              quote = FALSE,
              row.names = FALSE,
              append = TRUE)
  
  # write out gni_p{M} ~ weighted gni per market
  write.table(ampl(wgnip[Markets == DT[fileID == i]$Markets[1], .(MarketID, WGNI)], name = "gni_p"), 
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




