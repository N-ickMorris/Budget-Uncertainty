# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

# insert the path to where the ExpOut folder is
exp.out.path = "/shared/kgcoe-research/gates/Budget-Uncertainty/ABP-Files/ABPModular_FLin/ExpOut"
exp.out.path = "C:/Users/Nick Morris/Downloads/ABP/Drop-Price-52B-14M/ExpOut"
setwd(exp.out.path)

# create a name for a .txt file to log progress information while parallel processing
myfile = "log.txt"

# create the file (this will be in your work directory)
# i suggest opening this up in notepad or notepad++ so you can see how long each task takes when doing parallel processing tasks
file.create(myfile)

# open up a graphical window
# x11()
windows()

# -----------------------------------------------------------------------------------
# ---- Packages ---------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# data handling
require(data.table)
require(tm)

# plotting
require(ggplot2)
require(gridExtra)
require(scales)
require(GGally)
require(rattle)
require(rpart.plot)
require(RColorBrewer)

# parallel computing
require(foreach)
require(parallel)
require(doSNOW)

# modeling
require(rpart)
require(ranger)
  
}

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# these are functions i like to use

# ---- prints the data types of each column in a data frame -------------------------

types = function(dat)
{
  # make dat into a data.frame
  dat = data.frame(dat)
  
  # get the column names
  column = sapply(1:ncol(dat), function(i) colnames(dat)[i])
  
  # get the class of the columns
  data.type = sapply(1:ncol(dat), function(i) class(dat[,i]))
  
  # compute the number of levels for each column
  levels = sapply(1:ncol(dat), function(i) ifelse(data.type[i] == "factor", length(levels(droplevels(dat[,i]))), 0))
  
  return(data.frame(column, data.type, levels))
}

}

# -----------------------------------------------------------------------------------
# ---- Import the Data --------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# get the performance data
performance = data.table(read.table("sum_performance.txt", sep = ",", stringsAsFactors = FALSE, header = TRUE))

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

# import the antigen to bundle mapping data
bundle.antigen = data.table(read.csv("bundle-antigen-mapping-52B.csv", stringsAsFactors = FALSE))

# import the antigen demand data
antigen.demand = data.table(read.csv("dosage-current.csv", stringsAsFactors = FALSE))

# get the doe table
doe = data.table(read.csv("doe.csv", stringsAsFactors = FALSE))

# free memory
gc()

# extract the number of bundles
bundles = max(doe$Bundle)

# set up dat.short ~ the performance metrics for each run
# extract the data from performance for dat.short
performance.short = data.table(performance[, .(TSS, TCS, TPF)])

# extract the data from doe for dat.short
doe.short = data.table(doe[, .(Markets, MarketID, MarketImpacted, Upper_Price_Drop, Lower_Price_Drop, Upper_Bundles_Impacted, Lower_Bundles_Impacted, MARR, fileID)])

# truncate doe.short by fileID
doe.short = doe.short[, .(Markets = mean(Markets),
                          MarketImpacted = sum(MarketID * MarketImpacted) / bundles,
                          Upper_Price_Drop = mean(Upper_Price_Drop),
                          Lower_Price_Drop = mean(Lower_Price_Drop),
                          Upper_Bundles_Impacted = mean(Upper_Bundles_Impacted),
                          Lower_Bundles_Impacted = mean(Lower_Bundles_Impacted),
                          MARR = mean(MARR)),
                      by = fileID]

# combine doe.short and performance.short
dat.short = cbind(doe.short, performance.short)

# set up dat.long ~ the solution details for each run
# order doe by fileID, MarketID, and Bundle
doe = doe[order(fileID, MarketID, Bundle)]

# order solution by fileID, Market_ID, and Bundle_ID
solution = solution[order(fileID, Market_ID, Bundle_ID)]

# combine doe and solution
dat.long = cbind(doe, 
                 solution[,.(Selling_Qty, Dosage_Demand, Cohort_Demand, Selling_Price_Low, Selling_Price_High, Production_Cost)])

# remove objects we no longer need
rm(doe.short, performance.short, doe, performance, solution)

# free memory
gc()

}

# -----------------------------------------------------------------------------------
# ---- Plotting Surplus & Profit ----------------------------------------------------
# -----------------------------------------------------------------------------------

{

# create a Price_Drop factor variable
dat.short[, Price_Drop := paste0(Lower_Price_Drop * 100, "%-", Upper_Price_Drop * 100, "%")]
dat.short[, Price_Drop := factor(Price_Drop, levels = unique(Price_Drop))]

# create a Bundle_Impact factor variable
dat.short[, Bundle_Impact := paste0(Lower_Bundles_Impacted * 100, "%-", Upper_Bundles_Impacted * 100, "%")]
dat.short[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]

# if 100%-100% exists in Bundle_Impact then replace it with 100%
if("100%-100%" %in% dat.short$Bundle_Impact)
{
  dat.short[, Bundle_Impact := gsub("100%-100%", "100%", Bundle_Impact)]
  dat.short[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]
}

# update MARR to be a factor variable
dat.short[, MARR := paste0(MARR * 100, "%")]
dat.short[, MARR := factor(MARR, levels = unique(MARR))]

# make Markets into a factor variable
dat.short[, Markets := paste(Markets, "Markets")]
dat.short[, Markets := factor(Markets, levels = unique(Markets))]

# make MarketImpacted into a factor variable
dat.short[, MarketImpacted := factor(MarketImpacted, levels = unique(MarketImpacted))]

# create a variable: Consumer Market Value = TCS / (TCS + TPF)
dat.short[, CMV := TCS / (TCS + TPF)]

# create a variable: Provider Market Value = TPF / (TCS + TPF)
dat.short[, PMV := TPF / (TCS + TPF)]

# create a plot of TSS across Markets and Price_Drop
TSS.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = TSS / 1e9, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Total Social Surplus") + 
  labs(x = "Price Drop", y = "Value") + 
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

# create a plot of TSS across Markets and Price_Drop & MARR
TSS.plot.2 = ggplot(dat.short, aes(x = Price_Drop, y = TSS / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Total Social Surplus") + 
  labs(x = "Price Drop", y = "Value", color = "MARR", fill = "MARR") + 
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

# create a plot of TSS across Markets and Bundle_Impact
TSS.plot.3 = ggplot(dat.short, aes(x = Bundle_Impact, y = TSS / 1e9, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Total Social Surplus") + 
  labs(x = "Bundles Impacted", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TSS.plot.3

# create a plot of TSS across Markets and Bundle_Impact & MARR
TSS.plot.4 = ggplot(dat.short, aes(x = Bundle_Impact, y = TSS / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Total Social Surplus") + 
  labs(x = "Bundles Impacted", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TSS.plot.4

# create a plot of TCS across Markets and Price_Drop
TCS.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = TCS / 1e9, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Total Consumer Surplus") + 
  labs(x = "Price Drop", y = "Value") + 
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

# create a plot of TCS across Markets and Price_Drop & MARR
TCS.plot.2 = ggplot(dat.short, aes(x = Price_Drop, y = TCS / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Total Consumer Surplus") + 
  labs(x = "Price Drop", y = "Value", color = "MARR", fill = "MARR") + 
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

# create a plot of TCS across Markets and Bundle_Impact
TCS.plot.3 = ggplot(dat.short, aes(x = Bundle_Impact, y = TCS / 1e9, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Total Consumer Surplus") + 
  labs(x = "Bundles Impacted", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TCS.plot.3

# create a plot of TCS across Markets and Bundle_Impact & MARR
TCS.plot.4 = ggplot(dat.short, aes(x = Bundle_Impact, y = TCS / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Total Consumer Surplus") + 
  labs(x = "Bundles Impacted", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TCS.plot.4

# create a plot of TPF across Markets and Price_Drop
TPF.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = TPF / 1e9, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Total Provider Profit") + 
  labs(x = "Price Drop", y = "Value") + 
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

# create a plot of TPF across Markets and Price_Drop & MARR
TPF.plot.2 = ggplot(dat.short, aes(x = Price_Drop, y = TPF / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Total Provider Profit") + 
  labs(x = "Price Drop", y = "Value", color = "MARR", fill = "MARR") + 
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

# create a plot of TPF across Markets and Bundle_Impact
TPF.plot.3 = ggplot(dat.short, aes(x = Bundle_Impact, y = TPF / 1e9, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Total Provider Profit") + 
  labs(x = "Bundles Impacted", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TPF.plot.3

# create a plot of TPF across Markets and Bundle_Impact & MARR
TPF.plot.4 = ggplot(dat.short, aes(x = Bundle_Impact, y = TPF / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Total Provider Profit") + 
  labs(x = "Bundles Impacted", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TPF.plot.4

# create a plot of CMV across Markets and Price_Drop
CMV.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = CMV, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Consumer Market Value") + 
  labs(x = "Price Drop", y = "Value") + 
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

# create a plot of CMV across Markets and Price_Drop & MARR
CMV.plot.2 = ggplot(dat.short, aes(x = Price_Drop, y = CMV, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Consumer Market Value") + 
  labs(x = "Price Drop", y = "Value", color = "MARR", fill = "MARR") + 
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

# create a plot of CMV across Markets and Bundle_Impact
CMV.plot.3 = ggplot(dat.short, aes(x = Bundle_Impact, y = CMV, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Consumer Market Value") + 
  labs(x = "Bundles Impacted", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# CMV.plot.3

# create a plot of CMV across Markets and Bundle_Impact & MARR
CMV.plot.4 = ggplot(dat.short, aes(x = Bundle_Impact, y = CMV, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Consumer Market Value") + 
  labs(x = "Bundles Impacted", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# CMV.plot.4

# create a plot of PMV across Markets and Price_Drop
PMV.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = PMV, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Provider Market Value") + 
  labs(x = "Price Drop", y = "Value") + 
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

# create a plot of PMV across Markets and Price_Drop & MARR
PMV.plot.2 = ggplot(dat.short, aes(x = Price_Drop, y = PMV, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Provider Market Value") + 
  labs(x = "Price Drop", y = "Value", color = "MARR", fill = "MARR") + 
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

# create a plot of PMV across Markets and Bundle_Impact
PMV.plot.3 = ggplot(dat.short, aes(x = Bundle_Impact, y = PMV, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Provider Market Value") + 
  labs(x = "Bundles Impacted", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# PMV.plot.3

# create a plot of PMV across Markets and Bundle_Impact & MARR
PMV.plot.4 = ggplot(dat.short, aes(x = Bundle_Impact, y = PMV, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Provider Market Value") + 
  labs(x = "Bundles Impacted", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# PMV.plot.4

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


# -----------------------------------------------------------------------------------
# ---- Exporting Data ---------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# export data?
export.data = FALSE

if(export.data)
{
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
}

}

# -----------------------------------------------------------------------------------
# ---- Plotting Surplus by Market ---------------------------------------------------
# -----------------------------------------------------------------------------------

{

# create a copy of dat.long
dat.sur = data.table(dat.long)

# compute the total consumer surplus for each of the two price points
dat.sur[, Surplus_at_Price_Low := (newRbm - Selling_Price_Low) * Selling_Qty]
dat.sur[, Surplus_at_Price_High := (newRbm - Selling_Price_High) * Selling_Qty]

# add up the surplus across all bundles
dat.sur = dat.sur[, .(Surplus_at_Price_Low = sum(Surplus_at_Price_Low),
                      Surplus_at_Price_High = sum(Surplus_at_Price_High)), 
                  by = .(Scenario, Replication, MarketID, Markets, MarketImpacted, 
                         Upper_Price_Drop, Lower_Price_Drop, Upper_Bundles_Impacted,
                         Lower_Bundles_Impacted, MARR)]

# order dat.sur by scenarios
dat.sur = dat.sur[order(Scenario, Replication, Markets, MarketID)]

# compute the total consumer surplus for each scenario
dat.tcs = data.table(dat.sur[,.(Total_Surplus_at_Price_Low = sum(Surplus_at_Price_Low),
                                Total_Surplus_at_Price_High = sum(Surplus_at_Price_High)),
                             by = .(Scenario, Replication)])

# join dat.tcs onto dat.sur
setkey(dat.tcs, Scenario, Replication)
setkey(dat.sur, Scenario, Replication)
dat.sur = dat.tcs[dat.sur]

# order dat.sur by scenarios
dat.sur = dat.sur[order(Scenario, Replication, Markets, MarketID)]

# compute market share at each price point
dat.sur[, Market_Share_at_Price_Low := Surplus_at_Price_Low / Total_Surplus_at_Price_Low]
dat.sur[, Market_Share_at_Price_High := Surplus_at_Price_High / Total_Surplus_at_Price_High]

# create a Price_Drop factor variable
dat.sur[, Price_Drop := paste0(Lower_Price_Drop * 100, "%-", Upper_Price_Drop * 100, "%")]
dat.sur[, Price_Drop := factor(Price_Drop, levels = unique(Price_Drop))]

# create a Bundle_Impact factor variable
dat.sur[, Bundle_Impact := paste0(Lower_Bundles_Impacted * 100, "%-", Upper_Bundles_Impacted * 100, "%")]
dat.sur[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]

# if 100%-100% exists in Bundle_Impact then replace it with 100%
if("100%-100%" %in% dat.sur$Bundle_Impact)
{
  dat.sur[, Bundle_Impact := gsub("100%-100%", "100%", Bundle_Impact)]
  dat.sur[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]
}

# update MARR to be a factor variable
dat.sur[, MARR := paste0(MARR * 100, "%")]
dat.sur[, MARR := factor(MARR, levels = unique(MARR))]

# make Markets into a factor variable
dat.sur[, Markets := paste(Markets, "Markets")]
dat.sur[, Markets := factor(Markets, levels = unique(Markets))]

# compute which market is the imapcted market
dat.sur[, MarketImpacted := MarketImpacted * MarketID]
dat.target = data.table(dat.sur[,.(MarketImpacted_new = max(MarketImpacted)),
                                by = .(Scenario, Replication)])

# join dat.target onto dat.sur
setkey(dat.target, Scenario, Replication)
setkey(dat.sur, Scenario, Replication)
dat.sur = dat.target[dat.sur]

# update MarketImpacted and remove MarketImpacted_new
dat.sur[, MarketImpacted := MarketImpacted_new]
dat.sur[, MarketImpacted_new := NULL]

# remove unnecessary variables
dat.sur[, c("Upper_Price_Drop", "Lower_Price_Drop",
            "Upper_Bundles_Impacted", "Lower_Bundles_Impacted") := NULL]

# order dat.sur by scenarios
dat.sur = dat.sur[order(Scenario, Replication, Markets, MarketID)]

# create a plot of Market_Share_at_Price_Low across MarketID and Price_Drop
share.plot.1 = ggplot(dat.sur[Markets == "4 Markets"], aes(x = as.factor(MarketImpacted), y = Market_Share_at_Price_Low, color = as.factor(MarketID), fill = as.factor(MarketID))) +
  # geom_jitter(alpha = 1/3) +
  # scale_y_continuous(label = dollar_format(suffix = "B")) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9), alpha = 1/9) +
  scale_y_continuous(label = percent) +
  ggtitle("Market Share v. Income Effect\nLow Vaccine Prices") + 
  labs(x = "Market Impacted", y = "Share", color = "Market", fill = "Market") + 
  # facet_wrap(~paste("Price Drop:", Price_Drop), nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

share.plot.1

# create a plot of Market_Share_at_Price_High across MarketID and Price_Drop
share.plot.2a = ggplot(dat.sur[Markets == "4 Markets"], aes(x = as.factor(MarketImpacted), y = Market_Share_at_Price_High, color = as.factor(MarketID), fill = as.factor(MarketID))) +
  # geom_jitter(alpha = 1/3) +
  # scale_y_continuous(label = dollar_format(suffix = "B")) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9), alpha = 1/9) +
  scale_y_continuous(label = percent) +
  ggtitle("Market Share v. Income Effect\nLow Vaccine Prices") + 
  labs(x = "Market Impacted", y = "Share", color = "Market", fill = "Market") + 
  # facet_wrap(~paste("Price Drop:", Price_Drop), nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

share.plot.2a

# create a plot of Market_Share_at_Price_High across MarketID and Price_Drop
share.plot.2b = lapply(levels(dat.sur$Markets), function(l)
{
  p = ggplot(dat.sur[Markets == l & Price_Drop == "27%-40%"], aes(x = as.factor(MarketImpacted), y = Market_Share_at_Price_High, color = as.factor(MarketID), fill = as.factor(MarketID))) +
    # geom_jitter(alpha = 1/3) +
    # scale_y_continuous(label = dollar_format(suffix = "B")) +
    # geom_point(position = position_jitterdodge(dodge.width = 0.9), alpha = 1/9) +
    geom_boxplot(outlier.size = 0) +
    scale_y_continuous(label = percent) +
    ggtitle("Market Share v. Income Effect\nHigh Vaccine Prices") + 
    labs(x = "Market Impacted", y = "Share", color = "Market", fill = "Market") + 
    # facet_wrap(~paste("Price Drop:", Price_Drop), nrow = 1) +
    theme_bw(25) + 
    theme(legend.position = "top", 
          legend.key.size = unit(1/8, "in"), 
          plot.title = element_text(hjust = 0.5),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
    guides(color = guide_legend(override.aes = list(size = 5, linetype = 1, alpha = 1), nrow = 1))
  
  return(p)
})

share.plot.2b[[1]]
share.plot.2b[[2]]
share.plot.2b[[3]]
share.plot.2b[[4]]

# extract the subtset of data correpsonding to 4 markets
dat.tree = data.table(dat.sur[Markets == "4 Markets"])

# make MarketID and MarketImpacted into factor variables
dat.tree[, MarketID := as.factor(MarketID)]
dat.tree[, MarketImpacted := as.factor(MarketImpacted)]

# only keep the variables we need
dat.tree = dat.tree[,.(Market_Share_at_Price_Low, Market_Share_at_Price_High, 
                       Surplus_at_Price_Low, Surplus_at_Price_High,
                       MarketID, MarketImpacted, MARR, Price_Drop, Bundle_Impact)]

# make dat.tree into a model matrix
dat.tree = data.table(model.matrix(~., data = dat.tree)[,-1])

# create a copy of dat.tree with column names without % signs
dat.tree.copy = data.table(dat.tree)
setnames(dat.tree.copy, gsub("-", "_", gsub("%", "", names(dat.tree.copy))))

# build a random forest model to determine the least important variables to remove
mod.rf = ranger(Market_Share_at_Price_Low ~ . - Market_Share_at_Price_High - Surplus_at_Price_Low - Surplus_at_Price_High,
                data = dat.tree.copy, num.trees = 500, importance = "impurity", seed = 42)

# get varaible importance
imp.rf = sort(importance(mod.rf))

# check out imp.rf
imp.rf

# remove columns from dat.tree according to imp.rf
dat.tree[, c("MARR15%", "MARR10%", "Price_Drop13%-26%", "Price_Drop27%-40%", "MARR20%",
             "Bundle_Impact41%-60%", "Bundle_Impact21%-40%", "Bundle_Impact100%") := NULL]

# build a tree model of this data
mod.tree1 = rpart(Market_Share_at_Price_Low ~ . - Market_Share_at_Price_High - Surplus_at_Price_Low - Surplus_at_Price_High,
                  data = dat.tree, method = "anova", cp = 5e-4, minbucket = round(0.05 * nrow(dat.tree)))

mod.tree2 = rpart(Market_Share_at_Price_High ~ . - Market_Share_at_Price_Low - Surplus_at_Price_Low - Surplus_at_Price_High,
                  data = dat.tree, method = "anova", cp = 5e-4, minbucket = round(0.05 * nrow(dat.tree)))

mod.tree3 = rpart(Surplus_at_Price_Low / 1e9 ~ . - Market_Share_at_Price_Low - Market_Share_at_Price_High - Surplus_at_Price_High,
                  data = dat.tree, method = "anova", cp = 1e-4, minbucket = round(0.05 * nrow(dat.tree)))

mod.tree4 = rpart(Surplus_at_Price_High / 1e9 ~ . - Market_Share_at_Price_Low - Market_Share_at_Price_High - Surplus_at_Price_Low,
                  data = dat.tree, method = "anova", cp = 1e-4, minbucket = round(0.05 * nrow(dat.tree)))

fancyRpartPlot(mod.tree1, type = 2, palettes = "Reds", main = "A Decision Tree for Market Share\nLow Vaccine Pricing", sub = "")
fancyRpartPlot(mod.tree2, type = 2, palettes = "Reds", main = "A Decision Tree for Market Share\nHigh Vaccine Pricing", sub = "")
fancyRpartPlot(mod.tree3, type = 2, palettes = "Reds", main = "A Decision Tree for Consumer Surplus - Billions of USD\nLow Vaccine Pricing", sub = "")
fancyRpartPlot(mod.tree4, type = 2, palettes = "Reds", main = "A Decision Tree for Consumer Surplus - Billions of USD\nHigh Vaccine Pricing", sub = "")

# create a plot of Surplus_at_Price_Low across MarketID and Price_Drop
surplus.plot.1 = ggplot(dat.sur[Markets == "4 Markets"], aes(x = as.factor(MarketImpacted), y = Surplus_at_Price_Low / 1e9, color = as.factor(MarketID), fill = as.factor(MarketID))) +
  # geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9), alpha = 1/9) +
  # scale_y_continuous(label = percent) +
  ggtitle("Consumer Surplus v. Income Effect\nLow Vaccine Prices") + 
  labs(x = "Market Impacted", y = "Surplus (USD)", color = "Market", fill = "Market") + 
  # facet_wrap(~paste("Price Drop:", Price_Drop), nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

surplus.plot.1

# create a plot of Surplus_at_Price_High across MarketID and Price_Drop
surplus.plot.2 = ggplot(dat.sur[Markets == "4 Markets"], aes(x = as.factor(MarketImpacted), y = Surplus_at_Price_High / 1e9, color = as.factor(MarketID), fill = as.factor(MarketID))) +
  # geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9), alpha = 1/9) +
  # scale_y_continuous(label = percent) +
  ggtitle("Consumer Surplus v. Income Effect\nHigh Vaccine Prices") + 
  labs(x = "Market Impacted", y = "Surplus (USD)", color = "Market", fill = "Market") + 
  # facet_wrap(~paste("Price Drop:", Price_Drop), nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

surplus.plot.2



}



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







