# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

# input the paths to where the ExpOut, Info, DataIn, and Analysis folders are
exp.out.path = "/shared/kgcoe-research/gates/Budget-Uncertainty/R-Files/Summer-2018/Risk/ABPModular_FLin_Iterator/ExpOut"
info.path = "/shared/kgcoe-research/gates/Budget-Uncertainty/R-Files/Summer-2018/Risk/Data-Sets/Original"
info.path2 = "/shared/kgcoe-research/gates/Budget-Uncertainty/R-Files/Summer-2018/Risk/Data-Sets"
data.in.path = "/shared/kgcoe-research/gates/Budget-Uncertainty/R-Files/Summer-2018/Risk/ABPModular_FLin_Iterator/DataIn"
analysis.path = "/shared/kgcoe-research/gates/Budget-Uncertainty/R-Files/Summer-2018/Risk/ABPModular_FLin_Iterator/Analysis"

# set the current work directory
setwd(exp.out.path)

# create a name for a .txt file to log progress information while parallel processing
myfile = "log.txt"

# create the file (this will be in your work directory)
# i suggest opening this up in notepad or notepad++ so you can see how long each task takes when doing parallel processing tasks
file.create(myfile)

# open up a graphical window
x11()

# -----------------------------------------------------------------------------------
# ---- Packages ---------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# data handling
require(data.table)
require(tm)
require(pdist)
require(gtools)

# plotting
require(ggplot2)
require(gridExtra)
require(scales)
require(GGally)
require(lattice)

# modeling
require(fitdistrplus)
require(caret)
require(h2o)
require(MLmetrics)
  
# parallel computing
require(foreach)
require(parallel)
require(doSNOW)

}

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# ---- prints the data types of each column in a data frame -------------------------

types = function(dat)
{
  # make dat into a data.frame
  dat = data.frame(dat)
  
  # get the column names
  column = colnames(dat)
  
  # get the class of the columns
  data.type = sapply(1:ncol(dat), function(i) class(dat[,i]))
  
  # compute the number of levels for each column
  levels = sapply(1:ncol(dat), function(i) ifelse(data.type[i] == "factor", length(levels(droplevels(dat[,i]))), 0))
  
  return(data.frame(column, data.type, levels))
}

# ---- converts all columns to a character data type --------------------------------

tochar = function(dat)
{
  # make dat into a data.frame
  dat = data.frame(dat)
  
  # get the column names
  column = colnames(dat)
  
  # get the values in the columns and convert them to character data types
  values = lapply(1:ncol(dat), function(i) as.character(dat[,i]))
  
  # combine the values back into a data.frame
  dat = data.frame(do.call("cbind", values), stringsAsFactors = FALSE)
  
  # give dat its column names
  colnames(dat) = column
  
  return(dat)
}

# ---- a qualitative color scheme ---------------------------------------------------

mycolors = function(n)
{
  require(grDevices)
  return(colorRampPalette(c("#e41a1c", "#0099ff", "#4daf4a", "#984ea3", "#ff7f00", "#ff96ca", "#a65628"))(n))
}

# ---- emulates the default ggplot2 color scheme ------------------------------------

ggcolor = function(n, alpha = 1)
{
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# ---- builds a square confusion matrix ---------------------------------------------

confusion = function(ytrue, ypred)
{
  require(gtools)
  
  # make predicted and actual vectors into factors, if they aren't already
  if(class(ytrue) != "factor") ytrue = factor(ytrue)
  if(class(ypred) != "factor") ypred = factor(ypred)
  
  # combine their levels into one unique set of levels
  common.levels = mixedsort(unique(c(levels(ytrue), levels(ypred))))
  
  # give each vector the same levels
  ytrue = factor(ytrue, levels = common.levels)
  ypred = factor(ypred, levels = common.levels)
  
  # build the confusion matrix
  output = table("Actual" = ytrue, "Predicted" = ypred)
  
  # return the confusion matrix
  return(output)
}

}

# -----------------------------------------------------------------------------------
# ---- Prepare the Data -------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# do we need to prepare the data?
prepare.data = FALSE

if(prepare.data)
{
  # get the performance data
  performance = data.table(read.table("sum_performance_2018_06_03.txt", sep = ",", stringsAsFactors = FALSE, header = FALSE))
  
  # get the column names for the performance data
  p.names = names(data.table(read.table("sum_performance.txt", sep = ",", stringsAsFactors = FALSE, header = TRUE)))
  
  # assign the column names to performance
  setnames(performance, p.names)
  
  # do we need to build a single table of solution schedules?
  build.solution = TRUE
  
  if(build.solution)
  {
    # choose the number of workers and tasks for parallel processing (if you want to)
    workers = max(1, floor((2/3) * detectCores()))
    tasks = nrow(performance)
    
    # set up a cluster if workers > 1, otherwise don't set up a cluster
    if(workers > 1)
    {
      # setup parallel processing
      cl = makeCluster(workers, type = "SOCK", outfile = "")
      registerDoSNOW(cl)
      
      # define %dopar%
      `%fun%` = `%dopar%`
      
    } else
    {
      # define %do%
      `%fun%` = `%do%`
    }
    
    # get the solution data
    solution = foreach(i = 1:tasks) %fun%
    {
      # load packages we need for our tasks
      require(data.table)
      
      # get the solution data for experiment i
      output = data.table(read.table(paste0("out_fileID_", i, ".txt"), sep = ",", stringsAsFactors = FALSE, header = TRUE))
      
      # add a fileID column
      output[, fileID := as.numeric(i)]
      
      # free memory
      gc()
      
      return(output)
    }
    
    # end the cluster if it was set up
    if(workers > 1)
    {
      stopCluster(cl)
      rm(cl)
    }
    
    # combine the list of data tables into 1 table
    solution = rbindlist(solution)
    
    # export the solution data file
    write.csv(x = solution, file = "Solution Schedule.csv", row.names = FALSE)
    
  } else
  {
    # import the solution data file
    solution = data.table(read.csv(file = "Solution Schedule.csv", stringsAsFactors = FALSE))
  }
  
  # set the path to where the Info folder is
  setwd(info.path)
  
  # import the antigen to bundle mapping data
  bundle.antigen = data.table(read.csv("bundle-antigen-mapping-52B.csv", stringsAsFactors = FALSE))
  
  # import the antigen demand data
  antigen.demand = data.table(read.csv("dosage-current.csv", stringsAsFactors = FALSE))
  
  # set the path to where the DataIn folder is
  setwd(data.in.path)
  
  # get the doe table
  doe = data.table(read.csv("doe.csv", stringsAsFactors = FALSE))
  
  # extract the number of bundles
  bundles = max(doe$Bundle)
  
  # set up dat.short ~ the performance metrics for each run
  # extract the data from performance for dat.short
  performance.short = data.table(performance[, .(TSS, TCS, TPF)])
  
  # extract the data from doe for dat.short
  doe.short = data.table(doe[, .(Markets, MarketID, MarketImpacted, Upper_Cohort_Drop, Lower_Cohort_Drop, MARR, fileID)])
  
  # truncate doe.short by fileID
  doe.short = doe.short[, .(Markets = mean(Markets),
                            MarketImpacted = sum(MarketID * MarketImpacted) / bundles,
                            Upper_Cohort_Drop = mean(Upper_Cohort_Drop),
                            Lower_Cohort_Drop = mean(Lower_Cohort_Drop),
                            MARR = mean(MARR)),
                        by = fileID]
  
  # combine doe.short and performance.short
  dat.short = cbind(doe.short, performance.short)
  
  # create a Cohort_Drop variable
  dat.short[, Cohort_Drop := paste0(Lower_Cohort_Drop * 100, "%-", Upper_Cohort_Drop * 100, "%")]
  
  # update MARR to be a factor variable
  dat.short[, MARR := paste0(MARR * 100, "%")]
  
  # make Markets into a factor variable
  dat.short[, Markets := paste(Markets, "Markets")]
  
  # create a variable: Consumer Market Value = TCS / (TCS + TPF)
  dat.short[, CMV := TCS / (TCS + TPF)]
  
  # create a variable: Provider Market Value = TPF / (TCS + TPF)
  dat.short[, PMV := TPF / (TCS + TPF)]
  
  # remove unneeded variables
  dat.short[, c("Upper_Cohort_Drop", "Lower_Cohort_Drop") := NULL]
  
  # set up dat.long ~ the solution details for each run
  # order doe by fileID, MarketID, and Bundle
  doe = doe[order(fileID, MarketID, Bundle)]
  
  # order solution by fileID, Market_ID, and Bundle_ID
  solution = solution[order(fileID, Market_ID, Bundle_ID)]
  
  # combine doe and solution
  dat.long = cbind(doe, 
                   solution[,.(Bundle_Name, Selling_Qty, Produce_Bundle, Bundle_Demand, Birth_Cohort, Supply_Capacity, Selling_Price_Low, Selling_Price_High, Reservation_Price, Production_Cost)])
  
  # create a Cohort_Drop variable
  dat.long[, Cohort_Drop := paste0(Lower_Cohort_Drop * 100, "%-", Upper_Cohort_Drop * 100, "%")]
  
  # update MARR to be a factor variable
  dat.long[, MARR := paste0(MARR * 100, "%")]
  
  # make Markets into a factor variable
  dat.long[, Markets := paste(Markets, "Markets")]
  
  # compute Surplus_Low and Surplus_High
  dat.long[, Surplus_Low := (Reservation_Price - Selling_Price_Low) * Selling_Qty]
  dat.long[, Surplus_High := (Reservation_Price - Selling_Price_High) * Selling_Qty]
  
  # compute Revenue_Low and Revenue_High
  dat.long[, Revenue_Low := Selling_Price_Low * Selling_Qty]
  dat.long[, Revenue_High := Selling_Price_High * Selling_Qty]
  
  # only keep necessary variables
  dat.long = dat.long[,.(fileID, Scenario, Replication, Bundle, Bundle_Name, Produce_Bundle,
                         Markets, MarketID, MarketImpacted, Selling_Qty, Supply_Capacity, 
                         Production_Cost, Bundle_Demand, Birth_Cohort, Selling_Price_Low, Selling_Price_High, 
                         Reservation_Price, Surplus_Low, Surplus_High, 
                         Revenue_Low, Revenue_High, Cohort_Drop, MARR)]
  
  # set the current work directory
  setwd(analysis.path)
  
  # export the solution data files
  write.csv(x = dat.short, file = "Solution Data - Short.csv", row.names = FALSE)
  write.csv(x = dat.long, file = "Solution Data - Long.csv", row.names = FALSE)
  
  # remove objects we no longer need
  rm(doe.short, performance.short, doe, performance, solution, build.solution, p.names)
  
  # free memory
  gc()
  
} else
{
  # import the solution data files
  setwd(analysis.path)
  dat.short = data.table(read.csv(file = "Solution Data - Short.csv", stringsAsFactors = FALSE))
  dat.long = data.table(read.csv(file = "Solution Data - Long.csv", stringsAsFactors = FALSE))
}

}

# -----------------------------------------------------------------------------------
# ---- Plotting Surplus & Profit ----------------------------------------------------
# -----------------------------------------------------------------------------------

{

# update variables that should be factors in dat.short
dat.short[, Cohort_Drop := factor(Cohort_Drop, levels = unique(Cohort_Drop))]
dat.short[, MARR := factor(MARR, levels = unique(MARR))]
dat.short[, Markets := factor(Markets, levels = unique(Markets))]
dat.short[, MarketImpacted := factor(MarketImpacted, levels = unique(MarketImpacted))]

# create a plot of TSS across Markets and Cohort_Drop
TSS.plot.1 = ggplot(dat.short, aes(x = Cohort_Drop, y = TSS / 1e9, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Total Social Surplus") + 
  labs(x = "Cohort Drop", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TSS.plot.1

# create a plot of TSS across Markets and Cohort_Drop & MARR
TSS.plot.2 = ggplot(dat.short, aes(x = Cohort_Drop, y = TSS / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Total Social Surplus") + 
  labs(x = "Cohort Drop", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TSS.plot.2

# set up a pdf file to capture this plot
pdf("TSS-plot-1.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(TSS.plot.2)

# close off the connection
dev.off()

# create a plot of TCS across Markets and Cohort_Drop
TCS.plot.1 = ggplot(dat.short, aes(x = Cohort_Drop, y = TCS / 1e9, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Total Consumer Surplus") + 
  labs(x = "Cohort Drop", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TCS.plot.1

# create a plot of TCS across Markets and Cohort_Drop & MARR
TCS.plot.2 = ggplot(dat.short, aes(x = Cohort_Drop, y = TCS / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Total Consumer Surplus") + 
  labs(x = "Cohort Drop", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TCS.plot.2

# set up a pdf file to capture this plot
pdf("TCS-plot-1.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(TCS.plot.2)

# close off the connection
dev.off()

# create a plot of TPF across Markets and Cohort_Drop
TPF.plot.1 = ggplot(dat.short, aes(x = Cohort_Drop, y = TPF / 1e9, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Total Provider Profit") + 
  labs(x = "Cohort Drop", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TPF.plot.1

# create a plot of TPF across Markets and Cohort_Drop & MARR
TPF.plot.2 = ggplot(dat.short, aes(x = Cohort_Drop, y = TPF / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Total Provider Profit") + 
  labs(x = "Cohort Drop", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TPF.plot.2

# set up a pdf file to capture this plot
pdf("TPF-plot-1.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(TPF.plot.2)

# close off the connection
dev.off()

# create a plot of CMV across Markets and Cohort_Drop
CMV.plot.1 = ggplot(dat.short, aes(x = Cohort_Drop, y = CMV, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Consumer Market Value") + 
  labs(x = "Cohort Drop", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# CMV.plot.1

# create a plot of CMV across Markets and Cohort_Drop & MARR
CMV.plot.2 = ggplot(dat.short, aes(x = Cohort_Drop, y = CMV, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Consumer Market Value") + 
  labs(x = "Cohort Drop", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# CMV.plot.2

# set up a pdf file to capture this plot
pdf("CMV-plot-1.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(CMV.plot.2)

# close off the connection
dev.off()

# create a plot of PMV across Markets and Cohort_Drop
PMV.plot.1 = ggplot(dat.short, aes(x = Cohort_Drop, y = PMV, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Provider Market Value") + 
  labs(x = "Cohort Drop", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# PMV.plot.1

# create a plot of PMV across Markets and Cohort_Drop & MARR
PMV.plot.2 = ggplot(dat.short, aes(x = Cohort_Drop, y = PMV, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Provider Market Value") + 
  labs(x = "Cohort Drop", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# PMV.plot.2

# set up a pdf file to capture this plot
pdf("PMV-plot-1.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(PMV.plot.2)

# close off the connection
dev.off()

# lets look at how surplus changes between markets
DT = data.table(dat.long[, 
                         .(Surplus_Low = sum(Surplus_Low),
                           Surplus_High = sum(Surplus_High)),
                         by = .(fileID, Scenario, Markets, MarketID, MarketImpacted, Cohort_Drop, MARR)])

# compute Total_Surplus_Low and Total_Surplus_High based on fileID
DT[, Total_Surplus_Low := sum(Surplus_Low), by = .(fileID)]
DT[, Total_Surplus_High := sum(Surplus_High), by = .(fileID)]

# compute Surplus_Share_Low and Surplus_Share_High
DT[, Surplus_Share_Low := Surplus_Low / Total_Surplus_Low]
DT[, Surplus_Share_High := Surplus_High / Total_Surplus_High]

# compute MarketIDImpacted variable
DT[, MarketIDImpacted := MarketID * MarketImpacted]
DT[, MarketIDImpacted := max(MarketIDImpacted), by = .(fileID)]

# update variables that should be factors in DT
DT[, Cohort_Drop := factor(Cohort_Drop, levels = unique(Cohort_Drop))]
DT[, MARR := factor(MARR, levels = unique(MARR))]
DT[, Markets := factor(Markets, levels = unique(Markets))]
DT[, MarketID := factor(MarketID, levels = unique(MarketID))]
DT[, MarketImpacted := ifelse(MarketImpacted == 1, "Yes", "No")]
DT[, MarketImpacted := factor(MarketImpacted, levels = unique(MarketImpacted))]

# export the market surplus data
write.csv(x = DT, file = "Market Surplus Data.csv", row.names = FALSE)

# avergae across replications and set up a range between low and hugh porices

# decide which market system to evaluate
m = "2 Markets"

# create a plot of Surplus_Share_Low across MarketID and Cohort_Drop & MarketImpacted
surplus.low.plot.1 = ggplot(DT[Markets == m], aes(x = Scenario, y = Surplus_Share_Low, color = MarketImpacted, fill = MarketID, group = MarketID)) +
  geom_point(position = position_jitterdodge(jitter.width = 15), alpha = 1/2, size = 2) + 
  geom_polygon(color = NA, alpha = 5/12) + 
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("orangered", "cornflowerblue"))(length(unique(DT$MarketImpacted)))) +
  scale_fill_manual(values = colorRampPalette(c("lightseagreen", "orchid3"))(length(unique(unname(unlist(DT[Markets == m, .(MarketID)])))))) +
  ggtitle("Distribution of Consumer Surplus Under Low Price Points") + 
  labs(x = "Scenarios", y = "Value", color = "Budget Uncertainty", fill = "Market") + 
  facet_wrap(~paste("Cohort Drop:", Cohort_Drop), nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1),
         color = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# surplus.low.plot.1

# set up a pdf file to capture this plot
pdf(paste0("Surplus-Distribution-", gsub(" ", "-", m), "-Low-Prices-plot-1.pdf"), width = 16, height = 10, paper = "special") 

# call the plot
print(surplus.low.plot.1)

# close off the connection
dev.off()

# create a plot of Surplus_Share_Low across MarketID and Cohort_Drop & MarketImpacted
surplus.low.plot.2 = ggplot(DT[Markets == m], aes(x = Scenario, y = Surplus_Share_Low, color = MARR, fill = MarketID, group = MarketID)) +
  geom_point(position = position_jitterdodge(jitter.width = 15), alpha = 1/2, size = 2) + 
  geom_polygon(color = NA, alpha = 5/12) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(DT$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("lightseagreen", "orchid3"))(length(unique(unname(unlist(DT[Markets == m, .(MarketID)])))))) +
  ggtitle("Distribution of Consumer Surplus Under Low Price Points") + 
  labs(x = "Scenarios", y = "Value", color = "MARR", fill = "Market") + 
  facet_wrap(~paste("Cohort Drop:", Cohort_Drop), nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1),
         color = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# surplus.low.plot.2

# set up a pdf file to capture this plot
pdf(paste0("Surplus-Distribution-", gsub(" ", "-", m), "-Low-Prices-plot-2.pdf"), width = 16, height = 10, paper = "special") 

# call the plot
print(surplus.low.plot.2)

# close off the connection
dev.off()

# create a plot of Surplus_Share_High across MarketID and Cohort_Drop & MarketImpacted
surplus.high.plot.1 = ggplot(DT[Markets == m], aes(x = Scenario, y = Surplus_Share_High, color = MarketImpacted, fill = MarketID, group = MarketID)) +
  geom_point(position = position_jitterdodge(jitter.width = 10), alpha = 1/2, size = 2) + 
  geom_polygon(color = NA, alpha = 5/12) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("orangered", "cornflowerblue"))(length(unique(DT$MarketImpacted)))) +
  scale_fill_manual(values = colorRampPalette(c("lightseagreen", "orchid3"))(length(unique(unname(unlist(DT[Markets == m, .(MarketID)])))))) +
  ggtitle("Distribution of Consumer Surplus Under High Price Points") + 
  labs(x = "Scenarios", y = "Value", color = "Budget Uncertainty", fill = "Market") + 
  facet_wrap(~paste("Cohort Drop:", Cohort_Drop), nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1),
         color = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# surplus.high.plot.1

# set up a pdf file to capture this plot
pdf(paste0("Surplus-Distribution-", gsub(" ", "-", m), "-High-Prices-plot-1.pdf"), width = 16, height = 10, paper = "special") 

# call the plot
print(surplus.high.plot.1)

# close off the connection
dev.off()

# create a plot of Surplus_Share_High across MarketID and Cohort_Drop & MarketImpacted
surplus.high.plot.2 = ggplot(DT[Markets == m], aes(x = Scenario, y = Surplus_Share_High, color = MARR, fill = MarketID, group = MarketID)) +
  geom_point(position = position_jitterdodge(jitter.width = 10), alpha = 1/2, size = 2) + 
  geom_polygon(color = NA, alpha = 5/12) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(DT$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("lightseagreen", "orchid3"))(length(unique(unname(unlist(DT[Markets == m, .(MarketID)])))))) +
  ggtitle("Distribution of Consumer Surplus Under High Price Points") + 
  labs(x = "Scenarios", y = "Value", color = "MARR", fill = "Market") + 
  facet_wrap(~paste("Cohort Drop:", Cohort_Drop), nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1),
         color = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# surplus.high.plot.2

# set up a pdf file to capture this plot
pdf(paste0("Surplus-Distribution-", gsub(" ", "-", m), "-High-Prices-plot-2.pdf"), width = 16, height = 10, paper = "special") 

# call the plot
print(surplus.high.plot.2)

# close off the connection
dev.off()

}

# -----------------------------------------------------------------------------------
# ---- Merging Country Risk and Market Solutions ------------------------------------
# -----------------------------------------------------------------------------------

{

# do we need to join the country risk data onto the market solution data?
join.risk = FALSE

if(join.risk)
{
  # set up the work directory to import Country-Data.csv
  setwd(info.path2)
  dat.risk = data.table(read.csv("Country-Data.csv", stringsAsFactors = FALSE))
  
  # extract the 2, 4, 8, and 12 market systems from dat.risk
  dat.risk.2 = data.table(na.omit(dat.risk[Vaccine == "OPV",.(Country, Country_Risk, GNIpc, Births = Birth_Cohort, Birth_Mortality, Markets = "2 Markets", MarketID = abpMarkets2)]))
  dat.risk.4 = data.table(na.omit(dat.risk[Vaccine == "OPV",.(Country, Country_Risk, GNIpc, Births = Birth_Cohort, Birth_Mortality, Markets = "4 Markets", MarketID = abpMarkets4)]))
  dat.risk.8 = data.table(na.omit(dat.risk[Vaccine == "OPV",.(Country, Country_Risk, GNIpc, Births = Birth_Cohort, Birth_Mortality, Markets = "8 Markets", MarketID = abpMarkets8)]))
  dat.risk.12 = data.table(na.omit(dat.risk[Vaccine == "OPV",.(Country, Country_Risk, GNIpc, Births = Birth_Cohort, Birth_Mortality, Markets = "12 Markets", MarketID = abpMarkets12)]))
  
  # update dat.risk by combining the market system tables
  dat.risk = rbind(dat.risk.2, dat.risk.4, dat.risk.8, dat.risk.12)
  
  # give dat.risk and dat.long ID columns to preserve their current row order
  dat.long[, ID1 := 1:nrow(dat.long)]
  dat.risk[, ID2 := 1:nrow(dat.risk)]
  
  # join dat.risk onto dat.long
  setkey(dat.long, Markets, MarketID)
  setkey(dat.risk, Markets, MarketID)
  dat.risk.long = data.table(dat.risk[dat.long, allow.cartesian = TRUE])
  
  # order dat.risk, dat.long, and dat.risk.long by ID1 and ID2
  dat.risk.long = dat.risk.long[order(ID1, ID2)]
  dat.risk = dat.risk[order(ID2)]
  dat.long = dat.long[order(ID1)]
  
  # remove ID1 and ID2 from dat.long and dat.risk.long
  dat.long[, ID1 := NULL]
  dat.risk.long[, c("ID1", "ID2") := NULL]
  
  # give dat.short an ID column to preserve its current row order
  dat.short[, ID1 := 1:nrow(dat.short)]
  
  # join dat.risk onto dat.short
  setkey(dat.short, Markets)
  setkey(dat.risk, Markets)
  dat.risk.short = data.table(dat.risk[dat.short, allow.cartesian = TRUE])
  
  # order dat.risk, dat.short, and dat.risk.short by ID1 and ID2
  dat.risk.short = dat.risk.short[order(ID1, ID2)]
  dat.risk = dat.risk[order(ID2)]
  dat.short = dat.short[order(ID1)]
  
  # remove ID1 and ID2 from dat.risk, dat.short, and dat.risk.short
  dat.short[, ID1 := NULL]
  dat.risk[, ID2 := NULL]
  dat.risk.short[, c("ID1", "ID2") := NULL]
  
  # set up the work directory to the analysis folder
  setwd(analysis.path)
  
  # export dat.risk, dat.risk.short, and dat.risk.long
  write.csv(x = dat.risk, file = "Risk Data.csv", row.names = FALSE)
  write.csv(x = dat.risk.short, file = "Solution Data with Risk - Short.csv", row.names = FALSE)
  write.csv(x = dat.risk.long, file = "Solution Data with Risk - Long.csv", row.names = FALSE)
  
  # remove some objects
  rm(dat.risk.2, dat.risk.4, dat.risk.8, dat.risk.12)
  
  # clean up the ram
  gc()
  
} else
{
  # import the solution data files
  setwd(analysis.path)
  dat.risk = data.table(read.csv(file = "Risk Data.csv", stringsAsFactors = FALSE))
  dat.risk.short = data.table(read.csv(file = "Solution Data with Risk - Short.csv", stringsAsFactors = FALSE))
  # dat.risk.long = data.table(read.csv(file = "Solution Data with Risk - Long.csv", stringsAsFactors = FALSE))
}

}

# -----------------------------------------------------------------------------------
# ---- Plotting Risk ----------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# plot a pivot chart of Risk
rpivotTable(data = dat.risk, rows = "Markets", cols = "MarketID", vals = "Country_Risk", 
            aggregatorName = "Average", rendererName = "Line Chart")

# build a Four_Markets variable
dat.risk[, Four_Markets := ifelse(MarketID == 1, "HIC", ifelse(MarketID == 2, "HMIC", ifelse(MarketID == 3, "LMIC", ifelse(MarketID == 4, "LIC", MarketID))))]
dat.risk[, Four_Markets := factor(Four_Markets, levels = c("HIC", "HMIC", "LMIC", "LIC"))]

# plot a histogram of Risk
plot.risk = ggplot(dat.risk[Markets == "4 Markets"], aes(x = Country_Risk / 100, fill = Four_Markets)) +
  geom_histogram(color = "white", bins = 15) + 
  scale_x_continuous(label = percent) +
  ggtitle("Distribution of Country Risk in Markets") + 
  labs(x = "Risk", y = "Frequency") + 
  facet_wrap(~Four_Markets, nrow = 1) +
  coord_flip() + 
  theme_bw(30) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

plot.risk

# set up a pdf file to capture this plot
pdf("Risk-Distribution.pdf", width = 16, height = 10, paper = "special") 

# call the plot
print(plot.risk)

# close off the connection
dev.off()

}

# -----------------------------------------------------------------------------------
# ---- Build Regression Models to Predict TSS ---------------------------------------
# -----------------------------------------------------------------------------------

{

# should risk be used as a predictor?
use.risk = FALSE

if(use.risk)
{
  # update dat.risk.short for modeling
  dat = data.table(dat.risk.short[,.(CMV, Country_Risk, MARR = as.factor(MARR), Cohort_Drop = as.factor(Cohort_Drop), 
                                     Markets = as.factor(Markets), MarketID = as.factor(MarketID), 
                                     MarketImpacted = as.factor(MarketImpacted))])
  
} else
{
  # update dat.risk.short for modeling
  dat = data.table(dat.risk.short[,.(TSS, MARR = as.factor(MARR), Cohort_Drop = as.factor(Cohort_Drop), 
                                     Markets = as.factor(Markets), MarketID = as.factor(MarketID), 
                                     MarketImpacted = as.factor(MarketImpacted))])
}

# expand the factors out into binary variables
dat = data.table(model.matrix(~., data = dat)[,-1])

# ---- create a multi-class variable for TSS ----

# plot a histogram of TSS to create a categorical version of it
histogram(dat$TSS)

# adjust bin numbers as you like
num.bins = 10
histogram(dat$TSS, nint = num.bins)

# create a table indicating the frequency of observations in each bin
bin.table = table(cut(dat$TSS, breaks = num.bins))

# convert frequencies into percentages
bin.table = bin.table / sum(bin.table)

# look at the bins
bin.table

# lets create a categorical version of TSS
dat[, TSS_class := as.character(cut(dat$TSS, breaks = num.bins))]

# lets combine (1.32e+10,1.36e+10]  (1.36e+10,1.4e+10]  (1.4e+10,1.43e+10] and (1.43e+10,1.47e+10]
dat[, TSS_class := ifelse(TSS_class %in% c("(1.32e+10,1.36e+10]", "(1.36e+10,1.4e+10]", "(1.4e+10,1.43e+10]", "(1.43e+10,1.47e+10]"), 
                                   "(1.32e+10,1.47e+10]", 
                                   TSS_class)]

# create a table indicating the frequency of observations in each bin
bin.table = table(dat$TSS_class)

# convert frequencies into percentages
bin.table = bin.table / sum(bin.table)

# look at the bins
bin.table * 100

# set up the proper order of the classes
class.order = names(bin.table)

# convert TSS_class to a factor
dat[, TSS_class := factor(TSS_class, levels = class.order)]

# ---- build prediction models for TSS ---------------------------------------

# set up the number of workers to use for modeling
workers = max(1, floor((2/3) * detectCores()))

# initialize the h2o instance
h2o.init(nthreads = workers, max_mem_size = "12g")

# remove any objects in the h2o instance
h2o.removeAll()

# remove the progress bar when model building
h2o.no_progress()

{

# ---- split the data ----

# identify predictors (x) and response (y)
x = names(dat[, !c("TSS", "TSS_class"), with = FALSE])
y = "TSS"

# build the fold assignment
set.seed(42)
k.folds = 5
folds = createFolds(y = unname(unlist(dat[, y, with = FALSE])), k = k.folds)

# split up dat into train, valid, and test
train.rows = unname(unlist(lapply(1:(k.folds - 2), function(f) folds[[f]])))
train = data.table(dat[train.rows])

valid.rows = unname(unlist(folds[[k.folds - 1]]))
valid = data.table(dat[valid.rows])

test.rows = unname(unlist(folds[[k.folds]]))
test = data.table(dat[test.rows])

# split up YX.h2o into train, valid, and test
train.YX.h2o = as.h2o(train[, c(y, x), with = FALSE])
valid.YX.h2o = as.h2o(valid[, c(y, x), with = FALSE])
test.YX.h2o = as.h2o(test[, c(y, x), with = FALSE])

# ---- grid search for models ----

# set up hyperparameters of interest
glm.hyper.params = list(lambda = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0),
                        alpha = c(0, 0.5, 1))

# lets use a random grid search and specify a time limit and/or model limit
minutes = 15
glm.search.criteria = list(strategy = "RandomDiscrete", 
                           max_runtime_secs = minutes * 60, 
                           # max_models = 100, 
                           seed = 42)

# lets run a grid search for a good model with intercept = FALSE and standardize = FALSE
h2o.rm("glm.random.gridA")
glm.random.gridA = h2o.grid(algorithm = "glm",
                            grid_id = "glm.random.gridA",
                            y = y,
                            x = x,
                            training_frame = train.YX.h2o,
                            validation_frame = valid.YX.h2o,
                            early_stopping = TRUE,
                            nfolds = 5,
                            keep_cross_validation_predictions = TRUE,
                            fold_assignment = "Modulo",
                            family = "gaussian",
                            intercept = FALSE,
                            standardize = FALSE,
                            seed = 21,
                            solver = "COORDINATE_DESCENT",
                            hyper_params = glm.hyper.params,
                            search_criteria = glm.search.criteria)

# lets run a grid search for a good model with intercept = TRUE and standardize = FALSE
h2o.rm("glm.random.gridB")
glm.random.gridB = h2o.grid(algorithm = "glm",
                            grid_id = "glm.random.gridB",
                            y = y,
                            x = x,
                            training_frame = train.YX.h2o,
                            validation_frame = valid.YX.h2o,
                            early_stopping = TRUE,
                            nfolds = 5,
                            keep_cross_validation_predictions = TRUE,
                            fold_assignment = "Modulo",
                            family = "gaussian",
                            intercept = TRUE,
                            standardize = FALSE,
                            seed = 21,
                            solver = "COORDINATE_DESCENT",
                            hyper_params = glm.hyper.params,
                            search_criteria = glm.search.criteria)

# lets run a grid search for a good model with intercept = TRUE and standardize = TRUE
h2o.rm("glm.random.gridC")
glm.random.gridC = h2o.grid(algorithm = "glm",
                            grid_id = "glm.random.gridC",
                            y = y,
                            x = x,
                            training_frame = train.YX.h2o,
                            validation_frame = valid.YX.h2o,
                            early_stopping = TRUE,
                            nfolds = 5,
                            keep_cross_validation_predictions = TRUE,
                            fold_assignment = "Modulo",
                            family = "gaussian",
                            intercept = TRUE,
                            standardize = TRUE,
                            seed = 21,
                            solver = "COORDINATE_DESCENT",
                            hyper_params = glm.hyper.params,
                            search_criteria = glm.search.criteria)

# lets run a grid search for a good model with intercept = FALSE and standardize = TRUE
h2o.rm("glm.random.gridD")
glm.random.gridD = h2o.grid(algorithm = "glm",
                            grid_id = "glm.random.gridD",
                            y = y,
                            x = x,
                            training_frame = train.YX.h2o,
                            validation_frame = valid.YX.h2o,
                            early_stopping = TRUE,
                            nfolds = 5,
                            keep_cross_validation_predictions = TRUE,
                            fold_assignment = "Modulo",
                            family = "gaussian",
                            intercept = FALSE,
                            standardize = TRUE,
                            seed = 21,
                            solver = "COORDINATE_DESCENT",
                            hyper_params = glm.hyper.params,
                            search_criteria = glm.search.criteria)

# free up RAM
gc()

# rank each model in the random grids
glm.gridA = h2o.getGrid("glm.random.gridA", sort_by = "rmse", decreasing = FALSE)
glm.gridB = h2o.getGrid("glm.random.gridB", sort_by = "rmse", decreasing = FALSE)
glm.gridC = h2o.getGrid("glm.random.gridC", sort_by = "rmse", decreasing = FALSE)
glm.gridD = h2o.getGrid("glm.random.gridD", sort_by = "rmse", decreasing = FALSE)

# combine all the grid tables into one grid table that considers the options for intercept and standardize
glm.grid = rbind(cbind(data.table(glm.gridA@summary_table), intercept = "FALSE", standardize = "FALSE", search = 1),
                 cbind(data.table(glm.gridB@summary_table), intercept = "TRUE", standardize = "FALSE", search = 2),
                 cbind(data.table(glm.gridC@summary_table), intercept = "TRUE", standardize = "TRUE", search = 3),
                 cbind(data.table(glm.gridD@summary_table), intercept = "FALSE", standardize = "TRUE", search = 4))

# combine all the grid models
glm.grid.models = list(glm.gridA, glm.gridB, glm.gridC, glm.gridD)

# order grid by rmse
glm.grid = glm.grid[order(as.numeric(rmse), decreasing = FALSE)]

# get the summary table of the grid search
DT.glm.grid = data.table(glm.grid)

# set up the data types for each column in DT.grid for plotting purposes
DT.glm.grid = DT.glm.grid[, .(alpha = factor(as.numeric(gsub("[", "", gsub("]", "", alpha, fixed = TRUE), fixed = TRUE)), levels = c(1, 0.5, 0)),
                              lambda = factor(as.numeric(gsub("[", "", gsub("]", "", lambda, fixed = TRUE), fixed = TRUE)), levels = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0)),
                              intercept = as.factor(intercept),
                              standardize = as.factor(standardize),
                              model = removePunctuation(gsub("[A-z]+", "", model_ids)),
                              rmse = as.numeric(rmse),
                              search = as.numeric(search))]

# plot rmse v. standardize, lambda, and alpha to see which structure is most robust
plot.glm.grid = ggplot(DT.glm.grid, aes(x = lambda, y = rmse, color = standardize, fill = standardize)) + 
  # geom_boxplot() + 
  geom_jitter(size = 3, alpha = 2/3) + 
  # scale_y_continuous(labels = dollar) + 
  ggtitle("Cross Validation Error") + 
  labs(x = "Strength of Regularization", y = "RMSE", color = "Standardize", fill = "Standardize") + 
  facet_wrap(~paste("L1/L2 Distribution:", alpha), nrow = 1) +
  theme_bw(base_size = 25) +
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1, byrow = TRUE))

plot.glm.grid

# should we stack models?
stack.models = FALSE

# finalize the model
if(stack.models)
{
  # find the top k models from all grid searches
  k = 10
  glm.mod.list = lapply(1:k, function(i)
  {
    # get grid search (s), the model id (m), and the position of the model (p)
    s = DT.glm.grid$search[i]
    m = DT.glm.grid$model[i]
    p = which(removePunctuation(gsub("[A-z]+", "", unlist(glm.grid.models[[s]]@model_ids))) == m)
    
    # get the model of interest
    output = glm.grid.models[[s]]@model_ids[[p]]
    
    return(output)
  })
  
  # stack the top k models from all grid searches
  h2o.rm("glm.mod")
  glm.mod = h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train.YX.h2o,
                                validation_frame = valid.YX.h2o,
                                model_id = "glm.mod",
                                base_models = glm.mod.list)
  
} else
{
  # pick the top model from all grid searches
  glm.mod = h2o.getModel(glm.grid.models[[DT.glm.grid$search[1]]]@model_ids[[1]])
}

# ---- measure model quality ----

# make predictions on each data set
ynew.train = as.data.frame(predict(glm.mod, newdata = train.YX.h2o))$predict
ynew.valid = as.data.frame(predict(glm.mod, newdata = valid.YX.h2o))$predict
ynew.test = as.data.frame(predict(glm.mod, newdata = test.YX.h2o))$predict

# get the true values from each data set
ytrue.train = as.data.frame(train.YX.h2o)[,1]
ytrue.valid = as.data.frame(valid.YX.h2o)[,1]
ytrue.test = as.data.frame(test.YX.h2o)[,1]

# compute prediction metrics on each data set
glm.metrics.train = h2o.make_metrics(predicted = as.h2o(ynew.train), 
                                     actuals = as.h2o(ytrue.train))

glm.metrics.valid = h2o.make_metrics(predicted = as.h2o(ynew.valid), 
                                     actuals = as.h2o(ytrue.valid))

glm.metrics.test = h2o.make_metrics(predicted = as.h2o(ynew.test), 
                                    actuals = as.h2o(ytrue.test))

# free up RAM
gc()

# check out the prediction metrics
glm.metrics.train
glm.metrics.valid
glm.metrics.test

# fit the normal distribution to the residuals
glm.resid.train = fitdist(data = ytrue.train - ynew.train, distr = "norm")
glm.resid.valid = fitdist(data = ytrue.valid - ynew.valid, distr = "norm") 
glm.resid.test = fitdist(data = ytrue.test - ynew.test, distr = "norm") 

# ---- finalize output ----

# build a final metrics table for glm.mod
glm.mod.table = data.table(Model = "Regression",
                           Metric = c("R2", "RMSE"),
                           Train = c(glm.metrics.train@metrics$r2, glm.metrics.train@metrics$RMSE),
                           Valid = c(glm.metrics.valid@metrics$r2, glm.metrics.valid@metrics$RMSE),
                           Test = c(glm.metrics.test@metrics$r2, glm.metrics.test@metrics$RMSE))

# build a final grid search table
glm.grid.search = data.table(cbind(Model = rep("Regression", nrow(DT.glm.grid)), 
                                   DT.glm.grid))

# build a list of residual plots
glm.resid = list("Train" = glm.resid.train, 
                 "Valid" = glm.resid.valid, 
                 "Test" = glm.resid.test)

# build a list of final tables
glm.list.TSS = list(glm.mod.table, glm.grid.search, glm.resid, glm.mod)
names(glm.list.TSS) = c("Regression_Metrics", "Regression_Search", "Regression_Error", "Regression_Model")

# save the h2o model
save.glm.mod.TSS = h2o.saveModel(object = glm.mod, path = getwd(), force = TRUE)

# remove some objects
rm(DT.glm.grid, folds,
   glm.grid, glm.grid.models, glm.gridA, glm.gridB, glm.gridC, glm.gridD, glm.hyper.params, 
   glm.metrics.test, glm.metrics.train, glm.metrics.valid,
   glm.mod, glm.random.gridA, glm.resid.test, glm.resid.train, glm.resid.valid,
   glm.random.gridB, glm.random.gridC, glm.random.gridD, glm.search.criteria,
   plot.glm.grid, train, test, valid, train.rows, valid.rows, test.rows,
   test.YX.h2o, train.YX.h2o, valid.YX.h2o, glm.mod.table, glm.resid, k.folds, minutes, glm.grid.search,
   x, y, ynew.test, ynew.train, ynew.valid, ytrue.test, ytrue.train, ytrue.valid)

# free up RAM
gc()

}

# ---- build prediction models for TSS_class ---------------------------------------

{

# ---- split the data ----

# identify predictors (x) and response (y)
x = names(dat[, !c("TSS", "TSS_class"), with = FALSE])
y = "TSS_class"

# build the fold assignment
set.seed(42)
k.folds = 5
folds = createFolds(y = unname(unlist(dat[, y, with = FALSE])), k = k.folds)

# split up dat into train, valid, and test
train.rows = unname(unlist(lapply(1:(k.folds - 2), function(f) folds[[f]])))
train = data.table(dat[train.rows])

valid.rows = unname(unlist(folds[[k.folds - 1]]))
valid = data.table(dat[valid.rows])

test.rows = unname(unlist(folds[[k.folds]]))
test = data.table(dat[test.rows])

# split up YX.h2o into train, valid, and test
train.YX.h2o = as.h2o(train[, c(y, x), with = FALSE])
valid.YX.h2o = as.h2o(valid[, c(y, x), with = FALSE])
test.YX.h2o = as.h2o(test[, c(y, x), with = FALSE])

# ---- grid search for models ----

# set up hyperparameters of interest
glm.hyper.params = list(lambda = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0),
                        alpha = c(0, 0.5, 1))

# lets use a random grid search and specify a time limit and/or model limit
minutes = 15
glm.search.criteria = list(strategy = "RandomDiscrete", 
                           max_runtime_secs = minutes * 60, 
                           # max_models = 100, 
                           seed = 42)

# lets run a grid search for a good model with intercept = FALSE and standardize = FALSE
h2o.rm("glm.random.gridA")
glm.random.gridA = h2o.grid(algorithm = "glm",
                            grid_id = "glm.random.gridA",
                            y = y,
                            x = x,
                            training_frame = train.YX.h2o,
                            validation_frame = valid.YX.h2o,
                            early_stopping = TRUE,
                            nfolds = 5,
                            keep_cross_validation_predictions = TRUE,
                            fold_assignment = "Modulo",
                            family = "multinomial",
                            intercept = FALSE,
                            standardize = FALSE,
                            seed = 21,
                            solver = "COORDINATE_DESCENT",
                            hyper_params = glm.hyper.params,
                            search_criteria = glm.search.criteria)

# lets run a grid search for a good model with intercept = TRUE and standardize = FALSE
h2o.rm("glm.random.gridB")
glm.random.gridB = h2o.grid(algorithm = "glm",
                            grid_id = "glm.random.gridB",
                            y = y,
                            x = x,
                            training_frame = train.YX.h2o,
                            validation_frame = valid.YX.h2o,
                            early_stopping = TRUE,
                            nfolds = 5,
                            keep_cross_validation_predictions = TRUE,
                            fold_assignment = "Modulo",
                            family = "multinomial",
                            intercept = TRUE,
                            standardize = FALSE,
                            seed = 21,
                            solver = "COORDINATE_DESCENT",
                            hyper_params = glm.hyper.params,
                            search_criteria = glm.search.criteria)

# lets run a grid search for a good model with intercept = TRUE and standardize = TRUE
h2o.rm("glm.random.gridC")
glm.random.gridC = h2o.grid(algorithm = "glm",
                            grid_id = "glm.random.gridC",
                            y = y,
                            x = x,
                            training_frame = train.YX.h2o,
                            validation_frame = valid.YX.h2o,
                            early_stopping = TRUE,
                            nfolds = 5,
                            keep_cross_validation_predictions = TRUE,
                            fold_assignment = "Modulo",
                            family = "multinomial",
                            intercept = TRUE,
                            standardize = TRUE,
                            seed = 21,
                            solver = "COORDINATE_DESCENT",
                            hyper_params = glm.hyper.params,
                            search_criteria = glm.search.criteria)

# lets run a grid search for a good model with intercept = FALSE and standardize = TRUE
h2o.rm("glm.random.gridD")
glm.random.gridD = h2o.grid(algorithm = "glm",
                            grid_id = "glm.random.gridD",
                            y = y,
                            x = x,
                            training_frame = train.YX.h2o,
                            validation_frame = valid.YX.h2o,
                            early_stopping = TRUE,
                            nfolds = 5,
                            keep_cross_validation_predictions = TRUE,
                            fold_assignment = "Modulo",
                            family = "multinomial",
                            intercept = FALSE,
                            standardize = TRUE,
                            seed = 21,
                            solver = "COORDINATE_DESCENT",
                            hyper_params = glm.hyper.params,
                            search_criteria = glm.search.criteria)

# free up RAM
gc()

# rank each model in the random grids
glm.gridA = h2o.getGrid("glm.random.gridA", sort_by = "mean_per_class_error", decreasing = FALSE)
glm.gridB = h2o.getGrid("glm.random.gridB", sort_by = "mean_per_class_error", decreasing = FALSE)
glm.gridC = h2o.getGrid("glm.random.gridC", sort_by = "mean_per_class_error", decreasing = FALSE)
glm.gridD = h2o.getGrid("glm.random.gridD", sort_by = "mean_per_class_error", decreasing = FALSE)

# combine all the grid tables into one grid table that considers the options for intercept and standardize
glm.grid = rbind(cbind(data.table(glm.gridA@summary_table), intercept = "FALSE", standardize = "FALSE", search = 1),
                 cbind(data.table(glm.gridB@summary_table), intercept = "TRUE", standardize = "FALSE", search = 2),
                 cbind(data.table(glm.gridC@summary_table), intercept = "TRUE", standardize = "TRUE", search = 3),
                 cbind(data.table(glm.gridD@summary_table), intercept = "FALSE", standardize = "TRUE", search = 4))

# combine all the grid models
glm.grid.models = list(glm.gridA, glm.gridB, glm.gridC, glm.gridD)

# order grid by mean_per_class_error
glm.grid = glm.grid[order(as.numeric(mean_per_class_error), decreasing = FALSE)]

# get the summary table of the grid search
DT.glm.grid = data.table(glm.grid)

# set up the data types for each column in DT.grid for plotting purposes
DT.glm.grid = DT.glm.grid[, .(alpha = factor(as.numeric(gsub("[", "", gsub("]", "", alpha, fixed = TRUE), fixed = TRUE)), levels = c(1, 0.5, 0)),
                              lambda = factor(as.numeric(gsub("[", "", gsub("]", "", lambda, fixed = TRUE), fixed = TRUE)), levels = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0)),
                              intercept = as.factor(intercept),
                              standardize = as.factor(standardize),
                              model = removePunctuation(gsub("[A-z]+", "", model_ids)),
                              mean_per_class_error = as.numeric(mean_per_class_error),
                              search = as.numeric(search))]

# plot mean_per_class_error v. standardize, lambda, and alpha to see which structure is most robust
plot.glm.grid = ggplot(DT.glm.grid, aes(x = lambda, y = mean_per_class_error, color = standardize, fill = standardize)) + 
  # geom_boxplot() + 
  geom_jitter(size = 3, alpha = 2/3) + 
  # scale_y_continuous(labels = dollar) + 
  ggtitle("Cross Validation Error") + 
  labs(x = "Strength of Regularization", y = "Log Loss", color = "Standardize", fill = "Standardize") + 
  facet_wrap(~paste("L1/L2 Distribution:", alpha), nrow = 1) +
  theme_bw(base_size = 25) +
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1, byrow = TRUE))

plot.glm.grid

# should we stack models?
stack.models = FALSE

if(stack.models)
{
  # find the top k models from all grid searches
  k = 10
  glm.mod.list = lapply(1:k, function(i)
  {
    # get grid search (s), the model id (m), and the position of the model (p)
    s = DT.glm.grid$search[i]
    m = DT.glm.grid$model[i]
    p = which(removePunctuation(gsub("[A-z]+", "", unlist(glm.grid.models[[s]]@model_ids))) == m)
    
    # get the model of interest
    output = glm.grid.models[[s]]@model_ids[[p]]
    
    return(output)
  })
  
  # stack the top k models from all grid searches
  h2o.rm("glm.mod")
  glm.mod = h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train.YX.h2o,
                                validation_frame = valid.YX.h2o,
                                model_id = "glm.mod",
                                base_models = glm.mod.list)
  
} else
{
  # pick the top model from all grid searches
  glm.mod = h2o.getModel(glm.grid.models[[DT.glm.grid$search[1]]]@model_ids[[1]])
}

# ---- measure model quality ----

# make predictions on each data set
ynew.train = as.matrix(predict(glm.mod, newdata = train.YX.h2o)[,-1])
ynew.valid = as.matrix(predict(glm.mod, newdata = valid.YX.h2o)[,-1])
ynew.test = as.matrix(predict(glm.mod, newdata = test.YX.h2o)[,-1])

# get the true values from each data set
ytrue.train = as.data.frame(train.YX.h2o[,1])
ytrue.valid = as.data.frame(valid.YX.h2o[,1])
ytrue.test = as.data.frame(test.YX.h2o[,1])

# ---- compute multi log loss ----

# build a matrix indicating the true class values for each data set
ytrue.train.mat = model.matrix(~., data = ytrue.train)[,-1]
ytrue.train.mat = cbind(1 - rowSums(ytrue.train.mat), ytrue.train.mat)

ytrue.valid.mat = model.matrix(~., data = ytrue.valid)[,-1]
ytrue.valid.mat = cbind(1 - rowSums(ytrue.valid.mat), ytrue.valid.mat)

ytrue.test.mat = model.matrix(~., data = ytrue.test)[,-1]
ytrue.test.mat = cbind(1 - rowSums(ytrue.test.mat), ytrue.test.mat)

# compute the multi-class logarithmic loss for each data set
mll.train = MultiLogLoss(y_pred = ynew.train, y_true = ytrue.train.mat)
mll.valid = MultiLogLoss(y_pred = ynew.valid, y_true = ytrue.valid.mat)
mll.test = MultiLogLoss(y_pred = ynew.test, y_true = ytrue.test.mat)

# free up RAM
gc()

# ---- compute kappa ----

# get the predicted classes and actual classes for each data set
ynew.train.code = apply(ynew.train, 1, which.max) - 1
ytrue.train.code = factor(as.numeric(ytrue.train[,1]) - 1, levels = 0:(ncol(ytrue.train.mat) - 1))

ynew.valid.code = apply(ynew.valid, 1, which.max) - 1
ytrue.valid.code = factor(as.numeric(ytrue.valid[,1]) - 1, levels = 0:(ncol(ytrue.valid.mat) - 1))

ynew.test.code = apply(ynew.test, 1, which.max) - 1
ytrue.test.code = factor(as.numeric(ytrue.test[,1]) - 1, levels = 0:(ncol(ytrue.test.mat) - 1))

# build a square confusion matrix for each data set
conf.train = confusion(ytrue = ytrue.train.code, ypred = ynew.train.code)
conf.valid = confusion(ytrue = ytrue.valid.code, ypred = ynew.valid.code)
conf.test = confusion(ytrue = ytrue.test.code, ypred = ynew.test.code)

# get the total number of observations for each data set
n.train = sum(conf.train)
n.valid = sum(conf.valid)
n.test = sum(conf.test)

# get the vector of correct predictions for each data set 
dia.train = diag(conf.train)
dia.valid = diag(conf.valid)
dia.test = diag(conf.test)

# get the vector of the number of observations per class for each data set
rsum.train = rowSums(conf.train)
rsum.valid = rowSums(conf.valid)
rsum.test = rowSums(conf.test)

# get the vector of the number of predictions per class for each data set
csum.train = colSums(conf.train)
csum.valid = colSums(conf.valid)
csum.test = colSums(conf.test)

# get the proportion of observations per class for each data set
p.train = rsum.train / n.train
p.valid = rsum.valid / n.valid
p.test = rsum.test / n.test

# get the proportion of predcitions per class for each data set
q.train = csum.train / n.train
q.valid = csum.valid / n.valid
q.test = csum.test / n.test

# compute accuracy for each data set
acc.train = sum(dia.train) / n.train
acc.valid = sum(dia.valid) / n.valid
acc.test = sum(dia.test) / n.test

# compute expected accuracy for each data set
exp.acc.train = sum(p.train * q.train)
exp.acc.valid = sum(p.valid * q.valid)
exp.acc.test = sum(p.test * q.test)

# compute kappa for each data set
kap.train = (acc.train - exp.acc.train) / (1 - exp.acc.train)
kap.valid = (acc.valid - exp.acc.valid) / (1 - exp.acc.valid)
kap.test = (acc.test - exp.acc.test) / (1 - exp.acc.test)

# ---- compute one-vs-all metrics ----

# compute a binary confusion matrix for each class, for each data set
one.v.all.train = lapply(1:nrow(conf.train), function(i)
{
  # get the four entries of a binary confusion matrix
  v = c(conf.train[i,i], 
        rsum.train[i] - conf.train[i,i], 
        csum.train[i] - conf.train[i,i], 
        n.train - rsum.train[i] - csum.train[i] + conf.train[i,i]);
  
  # build the confusion matrix
  return(matrix(v, nrow = 2, byrow = TRUE))
})

one.v.all.valid = lapply(1:nrow(conf.valid), function(i)
{
  # get the four entries of a binary confusion matrix
  v = c(conf.valid[i,i], 
        rsum.valid[i] - conf.valid[i,i], 
        csum.valid[i] - conf.valid[i,i], 
        n.valid - rsum.valid[i] - csum.valid[i] + conf.valid[i,i]);
  
  # build the confusion matrix
  return(matrix(v, nrow = 2, byrow = TRUE))
})

one.v.all.test = lapply(1:nrow(conf.test), function(i)
{
  # get the four entries of a binary confusion matrix
  v = c(conf.test[i,i], 
        rsum.test[i] - conf.test[i,i], 
        csum.test[i] - conf.test[i,i], 
        n.test - rsum.test[i] - csum.test[i] + conf.test[i,i]);
  
  # build the confusion matrix
  return(matrix(v, nrow = 2, byrow = TRUE))
})

# sum up all of the matrices for each data set
one.v.all.train = Reduce('+', one.v.all.train)
one.v.all.valid = Reduce('+', one.v.all.valid)
one.v.all.test = Reduce('+', one.v.all.test)

# compute the micro average accuracy for each data set
micro.acc.train = sum(diag(one.v.all.train)) / sum(one.v.all.train)
micro.acc.valid = sum(diag(one.v.all.valid)) / sum(one.v.all.valid)
micro.acc.test = sum(diag(one.v.all.test)) / sum(one.v.all.test)

# get the macro accuracy for each data set
macro.acc.train = acc.train
macro.acc.valid = acc.valid
macro.acc.test = acc.test

# ---- finalize output ----

# build a final metrics table for glm.mod
glm.mod.table = data.table(Model = "Regression",
                           Metric = c("Log_Loss", "Kappa", "Macro_Accuracy", "Micro_Accuracy"),
                           Train = c(mll.train, kap.train, macro.acc.train, micro.acc.train),
                           Valid = c(mll.valid, kap.valid, macro.acc.valid, micro.acc.valid),
                           Test = c(mll.test, kap.test, macro.acc.test, micro.acc.test))

# build a final grid search table
glm.grid.search = data.table(cbind(Model = rep("Regression", nrow(DT.glm.grid)), 
                                   DT.glm.grid))

# update the row and column names of the confusion matrices
colnames(conf.train) = levels(as.data.frame(ytrue.train)[,1])
rownames(conf.train) = levels(as.data.frame(ytrue.train)[,1])

colnames(conf.valid) = levels(as.data.frame(ytrue.valid)[,1])
rownames(conf.valid) = levels(as.data.frame(ytrue.valid)[,1])

colnames(conf.test) = levels(as.data.frame(ytrue.test)[,1])
rownames(conf.test) = levels(as.data.frame(ytrue.test)[,1])

# build a list of confusion matrices
glm.confusion = list("Train" = conf.train, 
                     "Valid" = conf.valid, 
                     "Test" = conf.test)

# build a list of final tables
glm.list.TSS_class = list(glm.mod.table, glm.grid.search, glm.confusion, glm.mod)
names(glm.list.TSS_class) = c("Regression_Metrics", "Regression_Search", "Regression_Error", "Regression_Model")

# save the h2o model
save.glm.mod.TSS_class = h2o.saveModel(object = glm.mod, path = getwd(), force = TRUE)

# remove some objects
rm(DT.glm.grid, folds,
   glm.grid, glm.grid.models, glm.gridA, glm.gridB, glm.gridC, glm.gridD, glm.hyper.params, 
   dia.train, ynew.train.code, ytrue.train.code, ytrue.train.mat, 
   dia.valid, ynew.valid.code, ytrue.valid.code, ytrue.valid.mat, 
   dia.test, ynew.test.code, ytrue.test.code, ytrue.test.mat, 
   p.train, q.train, acc.train, exp.acc.train,
   p.valid, q.valid, acc.valid, exp.acc.valid,
   p.test, q.test, acc.test, exp.acc.test,
   conf.train, rsum.train, csum.train, n.train, 
   conf.valid, rsum.valid, csum.valid, n.valid, 
   conf.test, rsum.test, csum.test, n.test, 
   one.v.all.train, one.v.all.valid, one.v.all.test,
   mll.train, kap.train, macro.acc.train, micro.acc.train,
   mll.valid, kap.valid, macro.acc.valid, micro.acc.valid,
   mll.test, kap.test, macro.acc.test, micro.acc.test,
   glm.mod, glm.random.gridA, glm.random.gridB, glm.random.gridC, glm.random.gridD, glm.search.criteria,
   plot.glm.grid, train, test, valid, train.rows, valid.rows, test.rows,
   test.YX.h2o, train.YX.h2o, valid.YX.h2o,
   x, y, ynew.test, ynew.train, ynew.valid, ytrue.test, ytrue.train, ytrue.valid)

# clean up the data in the h2o cluster
h2o.removeAll()

# shut down the h2o cluster to free up RAM
h2o.shutdown(prompt = FALSE)

# free up RAM
gc()

}

# define the name of the h2o model file
model.name = "glm.random.gridC_model_2_with_Risk"

# load in the model file
glm.mod.load.TSS_class = h2o.loadModel(paste(analysis.path, model.name, sep = "/"))

}

#####################################################################
#### update all code below ##########################################
#####################################################################

# -----------------------------------------------------------------------------------
# ---- Clustering Solution Schedules ------------------------------------------------
# -----------------------------------------------------------------------------------

{

# do we need to cluster solution schedules?
build.cluster.solutions = TRUE

if(build.cluster.solutions)
{
  # choose the number of workers and tasks for parallel processing (if you want to)
  workers = max(1, floor((2/3) * detectCores()))
  markets = c("2 Markets", "4 Markets", "8 Markets", "12 Markets")
  tasks = length(markets)
  
  # set up a cluster if workers > 1, otherwise don't set up a cluster
  if(workers > 1)
  {
    # setup parallel processing
    cl = makeCluster(workers, type = "SOCK", outfile = "")
    registerDoSNOW(cl)
    
    # define %dopar%
    `%fun%` = `%dopar%`
    
  } else
  {
    # define %do%
    `%fun%` = `%do%`
  }
  
  # cluster the solution data into discrete  states of budget uncertainty
  cluster.solutions = foreach(i = 1:tasks) %fun%
  {
    # load packages we need for our tasks
    require(data.table)
    
    # get market system i from dat.long
    dat.market = data.table(dat.long[Markets == markets[i]])
    
    # get the solution data for experiment i
    output = data.table(read.table(paste0("out_fileID_", i, ".txt"), sep = ",", stringsAsFactors = FALSE, header = TRUE))
    
    # add a fileID column
    output[, fileID := as.numeric(i)]
    
    # free memory
    gc()
    
    return(output)
  }
  
  # end the cluster if it was set up
  if(workers > 1)
  {
    stopCluster(cl)
    rm(cl)
  }
  
  # combine the list of data tables into 1 table
  solution = rbindlist(solution)
  
  # export the solution data file
  write.csv(x = solution, file = "Solution Schedule.csv", row.names = FALSE)
  
} else
{
  # import the solution data file
  solution = data.table(read.csv(file = "Solution Schedule.csv", stringsAsFactors = FALSE))
}

}

# -----------------------------------------------------------------------------------
# ---- Clustering Surplus & Profit --------------------------------------------------
# -----------------------------------------------------------------------------------

{

# 


}




# -----------------------------------------------------------------------------------
# ---- Plotting Demand Satisfaction -------------------------------------------------
# -----------------------------------------------------------------------------------

{

# choose the number of workers and tasks for parallel processing (if you want to)
workers = max(1, floor((2/3) * detectCores()))
workers = 2
tasks = max(dat.long$fileID)

# set up a cluster if workers > 1, otherwise don't set up a cluster
if(workers > 1)
{
  # setup parallel processing
  cl = makeCluster(workers, type = "SOCK", outfile = "")
  registerDoSNOW(cl)
  
  # define %dopar%
  `%fun%` = `%dopar%`
  
  # write out start time to log file
  sink(myfile, append = TRUE)
  cat("\n------------------------------------------------\n")
  cat("computing demand satisfaction\n")
  cat(paste(workers, "workers started at", Sys.time(), "\n"))
  sink()
  
} else
{
  # define %do%
  `%fun%` = `%do%`
  
  # write out start time to log file
  sink(myfile, append = TRUE)
  cat("\n------------------------------------------------\n")
  cat("computing demand satisfaction\n")
  cat(paste("task 1 started at", Sys.time(), "\n"))
  sink()
}

# lets go through each fileID and see if the bundle production scehdule can satisfy the antigen demands of each market
dat.demand = foreach(i = 1:tasks) %fun%
{
  # load packages we need for our tasks
  require(data.table)
  
  # get the fileID of interest
  fileID.output = data.table(dat.long[fileID == i])
  
  # go thorugh each market and see what proportion of demand was met for each antigen
  market.output = lapply(1:fileID.output$Markets[1], function(j)
  {
    # get the market of interest
    antigen.output = data.table(fileID.output[MarketID == j])
    
    # go through each antigen and compute the demand and production
    antigen.output = lapply(unique(antigen.demand$Antigen), function(k)
    {
      # get the dosage demand of each antigen k for market j of fileID i
      dose.demand = mean(antigen.demand[Antigen == k, DosePerChild])
      
      # get the birth Price demand for market j of fileID i
      Price.demand = antigen.output$Cohort_Demand[1]
      
      # multiply the dose and Price demands to get the total demand for this antigen
      total.demand = Price.demand * dose.demand
      
      # get the bundles that can be used to satisfy antigen k
      k.bundles = bundle.antigen[Antigen == k, Bundle]
      
      # add up the production of all bundles in k.bundles to see how much got produced to satisfy this antigens total.demand
      total.production = sum(antigen.output[Bundle %in% k.bundles, Selling_Qty])
      
      # create an output table
      output = data.table(fileID = i,
                          Price_Drop = paste0(fileID.output$Lower_Price_Drop[1] * 100, "%-", fileID.output$Upper_Price_Drop[1] * 100, "%"),
                          Bundle_Impact = paste0(fileID.output$Lower_Bundles_Impacted[1] * 100, "%-", fileID.output$Upper_Bundles_Impacted[1] * 100, "%"),
                          MARR = paste0(fileID.output$MARR[1] * 100, "%"),
                          Markets = max(fileID.output$Markets),
                          MarketID = j,
                          Antigen = k,
                          Demand = total.demand,
                          Production = total.production)
      
      return(output)
    })
    
    # combine the list of tables into 1 table
    antigen.output = rbindlist(antigen.output)
    
    return(antigen.output)
  })
  
  # combine the list of tables into 1 table
  market.output = rbindlist(market.output)
  
  # export progress information
  sink(myfile, append = TRUE)
  cat(paste("task", i, "of", tasks, "finished at", Sys.time(), "\n"))
  sink()
  
  # free memory
  gc()
  
  return(market.output)
}

# write out end time to log file
sink(myfile, append = TRUE)
cat(paste(tasks, "tasks finished at", Sys.time(), "\n"))
sink()

# end the cluster if it was set up
if(workers > 1)
{
  stopCluster(cl)
  rm(cl)
}

# combine the list of tables into 1 table
dat.demand = rbindlist(dat.demand)

# make Price_Drop and Bundle_Impact into factors variable
dat.demand[, Price_Drop := factor(Price_Drop, levels = unique(Price_Drop))]
dat.demand[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]

# if 100%-100% exists in Bundle_Impact then replace it with 100%
if("100%-100%" %in% dat.demand$Bundle_Impact)
{
  dat.demand[, Bundle_Impact := gsub("100%-100%", "100%", Bundle_Impact)]
  dat.demand[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]
}

# update MARR to be a factor variable
dat.demand[, MARR := factor(MARR, levels = unique(MARR))]

# make Markets into a factor variable
dat.demand[, Markets := paste(Markets, "Markets")]
dat.demand[, Markets := factor(Markets, levels = unique(Markets))]

# update MarketID
dat.demand[, MarketID := paste("Market", MarketID)]
dat.demand[, MarketID := factor(MarketID, levels = unique(MarketID))]

# give dat.deamnd an ID variable so we can preserve the original order of rows
dat.demand[, ID := 1:nrow(dat.demand)]

# compute global Demand across each Antigen for each fileID
global.demand = data.table(dat.demand[, .(Global_Demand = sum(Demand), 
                                          Global_Production = sum(Production)), 
                                      by = .(fileID, Antigen)])

# make fileID and Antigen the key columns in both dat.demand and global.demand
setkey(dat.demand, fileID, Antigen)
setkey(global.demand, fileID, Antigen)

# join global.demand onto dat.demand
dat.demand = global.demand[dat.demand]

# order dat.demand by ID
dat.demand = dat.demand[order(ID)]

# remove ID from dat.demand
dat.demand[, ID := NULL]

# make Antigen into a factor variable
dat.demand[, Antigen := factor(Antigen, levels = unique(Antigen))]

# create a plot of global demand satisfaction across Markets and Antigens
global.demand.plot.1 = ggplot(dat.demand, aes(x = Antigen, y = Global_Production / Global_Demand, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Global Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

global.demand.plot.1

# create a plot of global demand satisfaction across Markets and Antigens & MARR
global.demand.plot.2 = ggplot(dat.demand, aes(x = Antigen, y = Global_Production / Global_Demand, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Global Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# global.demand.plot.2

# create a plot of global demand satisfaction across Markets and Antigens & Bundle_Impact
global.demand.plot.3 = ggplot(dat.demand, aes(x = Antigen, y = Global_Production / Global_Demand, color = Bundle_Impact, fill = Bundle_Impact)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Global Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "Bundles", fill = "Bundles") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# global.demand.plot.3

# create a plot of global demand satisfaction across Markets and Antigens & Price_Drop
global.demand.plot.4 = ggplot(dat.demand, aes(x = Antigen, y = Global_Production / Global_Demand, color = Price_Drop, fill = Price_Drop)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Global Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "Price Drop", fill = "Price Drop") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# global.demand.plot.4

# create a plot of market demand satisfaction across Markets and Antigens
market.demand.plot.1 = ggplot(dat.demand, aes(x = Antigen, y = Production / Demand, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Market Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# market.demand.plot.1

# create a plot of market demand satisfaction across Markets and Antigens & MARR
market.demand.plot.2 = ggplot(dat.demand, aes(x = Antigen, y = Production / Demand, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Market Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# market.demand.plot.2

# create a plot of market demand satisfaction across Markets and Antigens & Bundle_Impact
market.demand.plot.3 = ggplot(dat.demand, aes(x = Antigen, y = Production / Demand, color = Bundle_Impact, fill = Bundle_Impact)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Market Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "Bundles", fill = "Bundles") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# market.demand.plot.3

# create a plot of market demand satisfaction across Markets and Antigens & Price_Drop
market.demand.plot.4 = ggplot(dat.demand, aes(x = Antigen, y = Production / Demand, color = Price_Drop, fill = Price_Drop)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Market Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "Price Drop", fill = "Price Drop") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# market.demand.plot.4

# create a plot of demand satisfaction across MarketID for each market system
market.satisfaction.plots = lapply(unique(dat.demand$Markets), function(i)
  ggplot(dat.demand[Markets == i], aes(x = Antigen, y = Production / Demand, color = MarketID, fill = MarketID)) +
    geom_jitter(alpha = 1/3) +
    stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
    scale_y_continuous(label = percent) +
    ggtitle(paste0("Market Demand Satisfaction")) + 
    labs(x = "Antigen", y = "Value", color = "Price Drop", fill = "Price Drop") + 
    facet_wrap(~MarketID, nrow = ceiling(as.numeric(gsub(" Markets", "", i)) / 6)) +
    theme_bw(25) + 
    theme(legend.position = "none", 
          legend.key.size = unit(.25, "in"), 
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1)))

# check out the plots for each of our market systems
# market.satisfaction.plots[[1]]
# market.satisfaction.plots[[2]]
# market.satisfaction.plots[[3]]
# market.satisfaction.plots[[4]]

}

# -----------------------------------------------------------------------------------
# ---- Plotting Production Cost Recovery --------------------------------------------
# -----------------------------------------------------------------------------------

{

# lets compute what the revenue is for each bundle that is to be sold, and compare that to the total production cost
# create a copy of dat.long
dat.revenue = data.table(dat.long)  

# lets compute low and high revenue based on the low/high selling prices and production qty
dat.revenue[, Revenue_Low := Selling_Price_Low * Selling_Qty]
dat.revenue[, Revenue_High := Selling_Price_High * Selling_Qty]

# create Price_Drop and Bundle_Impact Variables
dat.revenue[, Price_Drop := paste0(Lower_Price_Drop * 100, "%-", Upper_Price_Drop * 100, "%")]
dat.revenue[, Bundle_Impact := paste0(Lower_Bundles_Impacted * 100, "%-", Upper_Bundles_Impacted * 100, "%")]

# make Price_Drop and Bundle_Impact into factors variable
dat.revenue[, Price_Drop := factor(Price_Drop, levels = unique(Price_Drop))]
dat.revenue[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]

# if 100%-100% exists in Bundle_Impact then replace it with 100%
if("100%-100%" %in% dat.demand$Bundle_Impact)
{
  dat.revenue[, Bundle_Impact := gsub("100%-100%", "100%", Bundle_Impact)]
  dat.revenue[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]
}

# make Markets into a factor variable
dat.revenue[, Markets := paste(Markets, "Markets")]
dat.revenue[, Markets := factor(Markets, levels = unique(Markets))]

# update MarketID
dat.revenue[, MarketID := paste("Market", MarketID)]
dat.revenue[, MarketID := factor(MarketID, levels = unique(MarketID))]

# make Bundle into a factor variable
dat.revenue[, Bundle := factor(Bundle, levels = unique(Bundle))]

# update MARR to be a factor variable
dat.revenue[, MARR := paste0(MARR * 100, "%")]
dat.revenue[, MARR := factor(MARR, levels = unique(MARR))]

# lets aggregate dat.revenue
dat.revenue = dat.revenue[, .(Revenue_Low = sum(Revenue_Low),
                              Revenue_High = sum(Revenue_High),
                              Production_Cost = mean(Production_Cost)),
                          by = .(fileID, Markets, Price_Drop, Bundle_Impact, MARR, Bundle)]

# lets melt Revenue and Production Cost together
dat.revenue = melt(dat.revenue,
                   measure.vars = c("Revenue_Low", "Production_Cost", "Revenue_High"),
                   variable.name = "Production")

# update the Production to have values without any _ 
dat.revenue[, Production := gsub("_", " ", Production)]

# make Production a factor variable
dat.revenue[, Production := factor(Production, levels = unique(Production))]

# aggregate dat.revenue by dropping Bundle as a variable
dat.revenue = dat.revenue[, .(value = sum(value)),
                          by = .(fileID, Markets, Price_Drop, Bundle_Impact, MARR, Production)]

# create a plot of revenue satisfaction across Markets and Production
revenue.plot.1 = ggplot(dat.revenue, aes(x = Production, y = value / 1e9, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Production Cost Recovery") + 
  labs(y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# revenue.plot.1

# create a plot of revenue satisfaction across Price_Drop and Production
revenue.plot.2 = ggplot(dat.revenue, aes(x = Production, y = value / 1e9, color = Price_Drop, fill = Price_Drop)) +
  geom_jitter(alpha = 1/3) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$Price_Drop)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$Price_Drop)))) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Production Cost Recovery") + 
  labs(y = "Value", color = "Price Drop", fill = "Price Drop") + 
  facet_wrap(~Price_Drop, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# revenue.plot.2

# create a plot of revenue satisfaction across Bundle_Impact and Production
revenue.plot.3 = ggplot(dat.revenue, aes(x = Production, y = value / 1e9, color = Bundle_Impact, fill = Bundle_Impact)) +
  geom_jitter(alpha = 1/3) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$Bundle_Impact)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$Bundle_Impact)))) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Production Cost Recovery") + 
  labs(x = "Bundle", y = "Value", color = "Bundles Impacted", fill = "Bundles Impacted") + 
  facet_wrap(~Bundle_Impact, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# revenue.plot.3

# create a plot of revenue satisfaction across MARR and Production
revenue.plot.4 = ggplot(dat.revenue, aes(x = Production, y = value / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$MARR)))) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Production Cost Recovery") + 
  labs(x = "Bundle", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Bundle_Impact, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# revenue.plot.4

}

# -----------------------------------------------------------------------------------
# ---- Plotting Dropped Constraints -------------------------------------------------
# -----------------------------------------------------------------------------------

{

# update exp.out.path so we can get the dropped contraints files
exp.out.path = paste0(exp.out.path, "/DropConst")

# update the work directory
setwd(exp.out.path)

# choose the number of workers and tasks for parallel processing (if you want to)
workers = 1
tasks = length(list.files())

# lets extract the files in the work directory and plot the data
if(tasks > 0)
{
  # set up a cluster if workers > 1, otherwise don't set up a cluster
  if(workers > 1)
  {
    # setup parallel processing
    cl = makeCluster(workers, type = "SOCK", outfile = "")
    registerDoSNOW(cl)
    
    # define %dopar%
    `%fun%` = `%dopar%`
    
  } else
  {
    # define %do%
    `%fun%` = `%do%`
  }
  
  # get the solution data
  dropped = foreach(i = list.files()) %fun%
  {
    # load packages we need for our tasks
    require(data.table)
    
    # get the dropped constraint data for file i
    output = data.table(read.table(i, stringsAsFactors = FALSE))
    
    # add a file name column
    output[, V3 := i]
    
    # remove the first column of output
    output[, V1 := NULL]
    
    # update the names of output
    setnames(output, c("Contraint", "File"))
    
    return(output)
  }
  
  # end the cluster if it was set up
  if(workers > 1)
  {
    stopCluster(cl)
    rm(cl)
  }
  
  # combine the list of data tables into 1 table
  dropped = rbindlist(dropped)
  
  # only keep the name of the Contraint
  dropped[, Contraint := sapply(1:nrow(dropped), function(i) 
    substr(x = dropped$Contraint[i], 
           start = 1, 
           stop = which(strsplit(dropped$Contraint[i], "")[[1]] == "[") - 1))]
                        
  # remove all numbers and punctuation form the Constriant column
  dropped[, Contraint := removePunctuation(removeNumbers(Contraint))]
  
  # remove the _drop.out phrase from the entries in File
  dropped[, File := gsub("_drop.out", "", File)]
  
  # remove the fileID_ phrase from the entries in File
  dropped[, File := as.numeric(gsub("fileID_", "", File))]
  
  # make a copy of dat.demand
  my.copy = data.table(dat.demand)
  
  # only keep fileID, Price_Drop, Bundles_Impacted, Markets
  my.copy = my.copy[,.(fileID, Price_Drop, Bundle_Impact, MARR, Markets)]
  
  # only keep the unique values of fileID, Price_Drop, Bundles_Impacted, Markets
  my.copy = my.copy[!duplicated(my.copy)]
  
  # make fileID the key column in my.copy, and File the key column in dropped
  setkey(my.copy, fileID)
  setkey(dropped, File)
  
  # join my.copy onto dropped
  dropped = my.copy[dropped]
  
  # aggregate dropped by fileID
  dropped = dropped[,.(Count = .N),
                    by = .(Price_Drop, Bundle_Impact, MARR, Markets, Contraint)]
  
  # create a plot of dropped constraints across Markets and Production
  dropped.plot.1 = ggplot(dropped, aes(x = Contraint, y = Count, color = Markets, fill = Markets)) +
    geom_jitter(alpha = 1, width = 0.25, size = 4) +
    stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
    ggtitle("Dropped Constraints") + 
    labs(y = "Count") + 
    facet_wrap(~Markets, nrow = 1) +
    theme_bw(25) + 
    theme(legend.position = "none", 
          legend.key.size = unit(.25, "in"), 
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))
  
  # dropped.plot.1
  
  # create a plot of dropped constraints across Price_Drop and Constraint
  dropped.plot.2 = ggplot(dropped, aes(x = Contraint, y = Count, color = Price_Drop, fill = Price_Drop)) +
    geom_jitter(alpha = 1, width = 0.25, size = 4) +
    scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$Price_Drop)))) +
    scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$Price_Drop)))) +
    ggtitle("Dropped Constraints") + 
    labs(y = "Count", color = "Price Drop", fill = "Price Drop") + 
    facet_wrap(~Price_Drop, nrow = 1) +
    theme_bw(25) + 
    theme(legend.position = "top", 
          legend.key.size = unit(.25, "in"), 
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))
  
  # dropped.plot.2
  
  # create a plot of dropped constraints across Bundle_Impact and Constraint
  dropped.plot.3 = ggplot(dropped, aes(x = Contraint, y = Count, color = Bundle_Impact, fill = Bundle_Impact)) +
    geom_jitter(alpha = 1, width = 0.25, size = 4) +
    scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$Bundle_Impact)))) +
    scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$Bundle_Impact)))) +
    ggtitle("Dropped Constraints") + 
    labs(y = "Count", color = "Bundles Impacted", fill = "Bundles Impacted") + 
    facet_wrap(~Bundles_Impacted, nrow = 1) +
    theme_bw(25) + 
    theme(legend.position = "top", 
          legend.key.size = unit(.25, "in"), 
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))
  
  # dropped.plot.3
  
  # create a plot of dropped constraints across MARR and Constraint
  dropped.plot.4 = ggplot(dropped, aes(x = Contraint, y = Count, color = MARR, fill = MARR)) +
    geom_jitter(alpha = 1, width = 0.25, size = 4) +
    scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$MARR)))) +
    scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.revenue$MARR)))) +
    ggtitle("Dropped Constraints") + 
    labs(y = "Count", color = "Bundles Impacted", fill = "Bundles Impacted") + 
    facet_wrap(~Bundles_Impacted, nrow = 1) +
    theme_bw(25) + 
    theme(legend.position = "top", 
          legend.key.size = unit(.25, "in"), 
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))
  
  # dropped.plot.4
}

}

# export TSS data for predictive modeling
write.csv(x = cbind(TSS = dat.short$TSS, 
                    data.table(model.matrix(~ ., dat.short[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
          file = "TSS-Drop-Price.csv", 
          row.names = FALSE)

# export TCS data for predictive modeling
write.csv(x = cbind(TCS = dat.short$TCS, 
                    data.table(model.matrix(~ ., dat.short[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
          file = "TCS-Drop-Price.csv", 
          row.names = FALSE)

# export TPF data for predictive modeling
write.csv(x = cbind(TPF = dat.short$TPF, 
                    data.table(model.matrix(~ ., dat.short[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
          file = "TPF-Drop-Price.csv", 
          row.names = FALSE)

# export demand data for predictive modeling
# write.csv(x = cbind(Global_Satisfaction = dat.demand$Global_Production / dat.demand$Global_Demand, 
#                     data.table(model.matrix(~ ., dat.demand[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
#           file = "Demand-Drop-Price-USA.csv", 
#           row.names = FALSE)

# DT = data.table(dcast(data = dat.revenue, 
#                       formula = fileID + Markets + Price_Drop + Bundle_Impact + MARR ~ Production, 
#                       value.var = "value"))

# setnames(DT, c("fileID", "Markets", "Price_Drop", "Bundle_Impact", "MARR", "Revenue_Low", "Production_Cost", "Revenue_High"))

# DT[, Return_Low := Revenue_Low - Production_Cost]
# DT[, Return_High := Revenue_High - Production_Cost]

# export Return_Low data for predictive modeling
# write.csv(x = cbind(Return_Low = DT$Return_Low, 
#                     data.table(model.matrix(~ ., DT[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
#           file = "Return-Low-Drop-Price-USA.csv", 
#           row.names = FALSE)

# export Revenue_High data for predictive modeling
# write.csv(x = cbind(Return_High = DT$Return_High, 
#                     data.table(model.matrix(~ ., DT[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
#           file = "Return-High-Drop-Price-USA.csv", 
#           row.names = FALSE)






###################

# -----------------------------------------------------------------------------------
# ---- Plotting Schedule Quality ----------------------------------------------------
# -----------------------------------------------------------------------------------

{

similarities = foreach(i = 1:max(doe.out$Scenario)) %do%
{
	# extract base and new selling quantities
	
	x = doe.out[Scenario == i, Base.Sell.Qty]
	y = doe.out[Scenario == i, New.Sell.Qty]
	
	# compute the cosine similarity
	
	qty = x %*% y / sqrt(x%*%x * y%*%y)
	
	# extract base and new low selling prices
	
	x = doe.out[Scenario == i, Base.Sell.Price.Low]
	y = doe.out[Scenario == i, New.Sell.Price.Low]
	
	# compute the cosine similarity
	
	low = x %*% y / sqrt(x%*%x * y%*%y)

	# extract base and new high selling prices
	
	x = doe.out[Scenario == i, Base.Sell.Price.High]
	y = doe.out[Scenario == i, New.Sell.Price.High]
	
	# compute the cosine similarity
	
	high = x %*% y / sqrt(x%*%x * y%*%y)
	
	# build a table of the results
	
	result = data.table(Scenario = i, 
						Market.Size = doe.out[Scenario == i, Market.Size][1],
						Market.Target = doe.out[Scenario == i, Market.Target][1],
						Drop = doe.out[Scenario == i, Drop][1],
						Sell.Qty = qty,
						Sell.Price.Low = low,
						Sell.Price.High = high)
						
	return(result)
}

similarities = rbindlist(similarities)
setnames(similarities, c("Scenario", "Market.Size", "Market.Target", "Drop", "Selling Quantity", "Low Selling Price", "High Selling Price"))
similarities

rm(x, y, qty, low, high, result, i)

DT = melt(similarities, measure.vars = c("Selling Quantity", "Low Selling Price", "High Selling Price"))
DT[, Drop := paste0(100 * (Drop - 0.19), "%-", 100 * Drop, "%")]

# plot the similarities of the schedules

similarities.Market.Size.barplot = ggplot(data = DT, aes(x = Scenario, y = value, fill = factor(Market.Size))) +
									geom_bar(stat = "identity", position = "dodge", color = "grey47") +
									scale_fill_brewer(palette = "YlOrRd") +
									scale_y_continuous(limits = c(0.25, 1), labels = percent, oob = rescale_none) + 
									facet_wrap(~variable) +
									labs(x = "Scenario", y = "Cosine Similarity", fill = "Market Segments") +
									ggtitle("Similarity of Solution Schedules") +
									theme_dark(base_size = 30) + 
									theme(legend.position = "top", legend.key.size = unit(.5, "in"), plot.title = element_text(hjust = 0.5)) +
									guides(fill = guide_legend(nrow = 1, override.aes = list(size = 2)))

similarities.Market.Size.barplot

similarities.Drop.barplot = ggplot(data = DT, aes(x = Scenario, y = value, fill = factor(Drop))) +
									geom_bar(stat = "identity", position = "dodge", color = "grey47") +
									scale_fill_brewer(palette = "YlOrRd") +
									scale_y_continuous(limits = c(0.25, 1), labels = percent, oob = rescale_none) + 
									facet_wrap(~variable) +
									labs(x = "Scenario", y = "Cosine Similarity", fill = "Random Drop in Reservation Price") +
									ggtitle("Similarity of Solution Schedules") +
									theme_dark(base_size = 30) + 
									theme(legend.position = "top", legend.key.size = unit(.5, "in"), plot.title = element_text(hjust = 0.5)) +
									guides(fill = guide_legend(nrow = 1, override.aes = list(size = 2)))

similarities.Drop.barplot

rm(DT)

}







