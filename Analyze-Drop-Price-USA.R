# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

# insert the path to where the ExpOut folder is
# exp.out.path = "/shared/ ... /Fall-2017/ExpOut"
exp.out.path = "C:/ ... /Drop-Price-USA-52B-14M/ExpOut"
setwd(exp.out.path)

# insert the path to where the ExpOut-Baseline folder is
# exp.out.baseline.path = "/shared/ ... /Fall-2017/ExpOut-Baseline"
exp.out.baseline.path = "C:/ ... /Drop-Price-USA-52B-14M/ExpOut-Baseline"

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

# parallel computing
require(foreach)
require(parallel)
require(doSNOW)

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
# ---- Import the Experiment Data ---------------------------------------------------
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
      
      # get the birth cohort demand for market j of fileID i
      cohort.demand = antigen.output$Cohort_Demand[1]
      
      # multiply the dose and cohort demands to get the total demand for this antigen
      total.demand = cohort.demand * dose.demand
      
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

# global.demand.plot.1

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
revenue.plot.1 = ggplot(dat.revenue, aes(x = Production, y = value / 1e9, color = Production, fill = Production)) +
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
  facet_wrap(~Markets, nrow = 1) +
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
exp.out.path.new = paste0(exp.out.path, "/DropConst")

# update the work directory
setwd(exp.out.path.new)

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
# ---- Exporting Surplus & Profit Data ----------------------------------------------
# -----------------------------------------------------------------------------------

{

# update the work directory
setwd(exp.out.path)

# export TSS data for predictive modeling
write.csv(x = cbind(TSS = dat.short$TSS, 
                    data.table(model.matrix(~ ., dat.short[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
          file = "TSS-Drop-Price-USA.csv", 
          row.names = FALSE)

# export TCS data for predictive modeling
write.csv(x = cbind(TCS = dat.short$TCS, 
                    data.table(model.matrix(~ ., dat.short[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
          file = "TCS-Drop-Price-USA.csv", 
          row.names = FALSE)

# export TPF data for predictive modeling
write.csv(x = cbind(TPF = dat.short$TPF, 
                    data.table(model.matrix(~ ., dat.short[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
          file = "TPF-Drop-Price-USA.csv", 
          row.names = FALSE)

save.image("C:/Users/Nick Morris/Downloads/ABP/Drop-Price-USA-52B-14M/Drop-Price-USA.RData")

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

# -----------------------------------------------------------------------------------
# ---- Import the Baseline Data -----------------------------------------------------
# -----------------------------------------------------------------------------------

{

# update the work directory
setwd(exp.out.baseline.path)

# get the performance data
performance.baseline = data.table(read.table("sum_performance.txt", sep = ",", stringsAsFactors = FALSE, header = TRUE))

# get the number of fileIDs
tasks = nrow(performance.baseline)

# get the solution data
solution.baseline = foreach(i = 1:tasks) %do%
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

# combine the list of data tables into 1 table
solution.baseline = rbindlist(solution.baseline)

# get the doe table
doe.baseline = data.table(read.csv("doe.csv", stringsAsFactors = FALSE))

# free memory
gc()

# set up dat.baseline.short ~ the performance metrics for each run
# extract the data from performance for dat.baseline.short
performance.baseline.short = data.table(performance.baseline[, .(TSS, TCS, TPF)])

# extract the data from doe.baseline for dat.baseline.short
doe.baseline.short = data.table(doe.baseline[, .(Markets, MarketID, MARR, fileID)])

# truncate doe.baseline.short by fileID
doe.baseline.short = doe.baseline.short[, .(Markets = mean(Markets),
                                            MARR = mean(MARR)),
                                        by = fileID]

# combine doe.baseline.short and performance.baseline.short
dat.baseline.short = cbind(doe.baseline.short, performance.baseline.short)

# set up dat.baseline.long ~ the solution details for each run
# order doe.baseline by fileID, MarketID, and Bundle
doe.baseline = doe.baseline[order(fileID, MarketID, Bundle)]

# order solution.baseline by fileID, Market_ID, and Bundle_ID
solution.baseline = solution.baseline[order(fileID, Market_ID, Bundle_ID)]

# combine doe.baseline and solution.baseline
dat.baseline.long = cbind(doe.baseline, 
                          solution.baseline[,.(Selling_Qty, Dosage_Demand, Cohort_Demand, Selling_Price_Low, Selling_Price_High, Production_Cost)])

# remove objects we no longer need
rm(doe.baseline.short, performance.baseline.short, doe.baseline, performance.baseline, solution.baseline)

# free memory
gc()

}

# -----------------------------------------------------------------------------------
# ---- Plotting Change in Surplus & Profit ------------------------------------------
# -----------------------------------------------------------------------------------

{

# update MARR to be a factor variable
dat.baseline.short[, MARR := paste0(MARR * 100, "%")]
dat.baseline.short[, MARR := factor(MARR, levels = unique(MARR))]

# make Markets into a factor variable
dat.baseline.short[, Markets := paste(Markets, "Markets")]
dat.baseline.short[, Markets := factor(Markets, levels = unique(Markets))]

# create a variable: Consumer Market Value = TCS / (TCS + TPF)
dat.baseline.short[, CMV := TCS / (TCS + TPF)]

# create a variable: Provider Market Value = TPF / (TCS + TPF)
dat.baseline.short[, PMV := TPF / (TCS + TPF)]

# update the column names of dat.baseline.short
setnames(dat.baseline.short, c("fileID", "Markets", "MARR", "TSS.baseline", "TCS.baseline", "TPF.baseline", "CMV.baseline", "PMV.baseline"))

# give dat.short an ID column so we can maintain the original order of rows after joining
dat.short[, ID := 1:nrow(dat.short)]

# set Markets & MARR as the key columns in dat.short and dat.baseline.short
setkey(dat.short, Markets, MARR)
setkey(dat.baseline.short, Markets, MARR)

# join dat.baseline.short onto dat.short
dat.short = data.table(dat.baseline.short[, !"fileID"][dat.short])

# order dat.short by ID
dat.short = dat.short[order(ID)]

# remove the ID column
dat.short[, ID := NULL]

# compute the change in TSS, TCS, TPF, CMV, PMV
dat.short[, TSS.change := (TSS - TSS.baseline) / TSS.baseline]
dat.short[, TCS.change := (TCS - TCS.baseline) / TCS.baseline]
dat.short[, TPF.change := (TPF - TPF.baseline) / TPF.baseline]
dat.short[, CMV.change := CMV - CMV.baseline]
dat.short[, PMV.change := PMV - PMV.baseline]

# create a plot of TSS across Markets and Price_Drop
TSS.change.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = TSS.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Total Social Surplus") + 
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

# TSS.change.plot.1

# create a plot of TSS across Markets and Price_Drop & MARR
TSS.change.plot.2 = ggplot(dat.short, aes(x = Price_Drop, y = TSS.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Total Social Surplus") + 
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

# TSS.change.plot.2

# create a plot of TSS across Markets and Bundle_Impact
TSS.change.plot.3 = ggplot(dat.short, aes(x = Bundle_Impact, y = TSS.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Total Social Surplus") + 
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

# TSS.change.plot.3

# create a plot of TSS across Markets and Bundle_Impact & MARR
TSS.change.plot.4 = ggplot(dat.short, aes(x = Bundle_Impact, y = TSS.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Total Social Surplus") + 
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

# TSS.change.plot.4

# create a plot of TSS across Markets and Bundle_Impact & Price_Drop
TSS.change.plot.5 = ggplot(dat.short, aes(x = Bundle_Impact, y = TSS.change, color = Price_Drop, fill = Price_Drop)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$Price_Drop)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$Price_Drop)))) +
  ggtitle("Change in Total Social Surplus") + 
  labs(x = "Bundles Impacted", y = "Value", color = "Price Drop", fill = "Price Drop") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TSS.change.plot.5

# create a plot of TCS across Markets and Price_Drop
TCS.change.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = TCS.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Total Consumer Surplus") + 
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

# TCS.change.plot.1

# create a plot of TCS across Markets and Price_Drop & MARR
TCS.change.plot.2 = ggplot(dat.short, aes(x = Price_Drop, y = TCS.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Total Consumer Surplus") + 
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

# TCS.change.plot.2

# create a plot of TCS across Markets and Bundle_Impact
TCS.change.plot.3 = ggplot(dat.short, aes(x = Bundle_Impact, y = TCS.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Total Consumer Surplus") + 
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

# TCS.change.plot.3

# create a plot of TCS across Markets and Bundle_Impact & MARR
TCS.change.plot.4 = ggplot(dat.short, aes(x = Bundle_Impact, y = TCS.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Total Consumer Surplus") + 
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

# TCS.change.plot.4

# create a plot of TCS across Markets and Bundle_Impact & Price_Drop
TCS.change.plot.5 = ggplot(dat.short, aes(x = Bundle_Impact, y = TCS.change, color = Price_Drop, fill = Price_Drop)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$Price_Drop)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$Price_Drop)))) +
  ggtitle("Change in Total Consumer Surplus") + 
  labs(x = "Bundles Impacted", y = "Value", color = "Price Drop", fill = "Price Drop") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TCS.change.plot.5

# create a plot of TPF across Markets and Price_Drop
TPF.change.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = TPF.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Total Provider Profit") + 
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

# TPF.change.plot.1

# create a plot of TPF across Markets and Price_Drop & MARR
TPF.change.plot.2 = ggplot(dat.short, aes(x = Price_Drop, y = TPF.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Total Provider Profit") + 
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

# TPF.change.plot.2

# create a plot of TPF across Markets and Bundle_Impact
TPF.change.plot.3 = ggplot(dat.short, aes(x = Bundle_Impact, y = TPF.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Total Provider Profit") + 
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

# TPF.change.plot.3

# create a plot of TPF across Markets and Bundle_Impact & MARR
TPF.change.plot.4 = ggplot(dat.short, aes(x = Bundle_Impact, y = TPF.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Total Provider Profit") + 
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

# TPF.change.plot.4

# create a plot of TPF across Markets and Bundle_Impact & Price_Drop
TPF.change.plot.5 = ggplot(dat.short, aes(x = Bundle_Impact, y = TPF.change, color = Price_Drop, fill = Price_Drop)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$Price_Drop)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$Price_Drop)))) +
  ggtitle("Change in Total Provider Profit") + 
  labs(x = "Bundles Impacted", y = "Value", color = "Price Drop", fill = "Price Drop") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# TPF.change.plot.5

# create a plot of CMV across Markets and Price_Drop
CMV.change.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = CMV.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Consumer Market Value") + 
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

# CMV.change.plot.1

# create a plot of CMV across Markets and Price_Drop & MARR
CMV.change.plot.2 = ggplot(dat.short, aes(x = Price_Drop, y = CMV.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Consumer Market Value") + 
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

# CMV.change.plot.2

# create a plot of CMV across Markets and Bundle_Impact
CMV.change.plot.3 = ggplot(dat.short, aes(x = Bundle_Impact, y = CMV.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Consumer Market Value") + 
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

# CMV.change.plot.3

# create a plot of CMV across Markets and Bundle_Impact & MARR
CMV.change.plot.4 = ggplot(dat.short, aes(x = Bundle_Impact, y = CMV.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Consumer Market Value") + 
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

# CMV.change.plot.4

# create a plot of CMV across Markets and Bundle_Impact & Price_Drop
CMV.change.plot.5 = ggplot(dat.short, aes(x = Bundle_Impact, y = CMV.change, color = Price_Drop, fill = Price_Drop)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$Price_Drop)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$Price_Drop)))) +
  ggtitle("Change in Consumer Market Value") + 
  labs(x = "Bundles Impacted", y = "Value", color = "Price Drop", fill = "Price Drop") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# CMV.change.plot.5

# create a plot of PMV across Markets and Price_Drop
PMV.change.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = PMV.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Provider Market Value") + 
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

# PMV.change.plot.1

# create a plot of PMV across Markets and Price_Drop & MARR
PMV.change.plot.2 = ggplot(dat.short, aes(x = Price_Drop, y = PMV.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Provider Market Value") + 
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

# PMV.change.plot.2

# create a plot of PMV across Markets and Bundle_Impact
PMV.change.plot.3 = ggplot(dat.short, aes(x = Bundle_Impact, y = PMV.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Provider Market Value") + 
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

# PMV.change.plot.3

# create a plot of PMV across Markets and Bundle_Impact & MARR
PMV.change.plot.4 = ggplot(dat.short, aes(x = Bundle_Impact, y = PMV.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Provider Market Value") + 
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

# PMV.change.plot.4

# create a plot of PMV across Markets and Bundle_Impact & Price_Drop
PMV.change.plot.5 = ggplot(dat.short, aes(x = Bundle_Impact, y = PMV.change, color = Price_Drop, fill = Price_Drop)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$Price_Drop)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$Price_Drop)))) +
  ggtitle("Change in Provider Market Value") + 
  labs(x = "Bundles Impacted", y = "Value", color = "Price Drop", fill = "Price Drop") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# PMV.change.plot.5

}

# -----------------------------------------------------------------------------------
# ---- Plotting Change in Demand Satisfaction ---------------------------------------
# -----------------------------------------------------------------------------------

{

# compute the number tasks
tasks = max(dat.baseline.long$fileID)

# lets go through each fileID and see if the bundle production scehdule can satisfy the antigen demands of each market
dat.baseline.demand = foreach(i = 1:tasks) %do%
{
  # load packages we need for our tasks
  require(data.table)
  
  # get the fileID of interest
  fileID.output = data.table(dat.baseline.long[fileID == i])
  
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
      
      # get the birth cohort demand for market j of fileID i
      cohort.demand = antigen.output$Cohort_Demand[1]
      
      # multiply the dose and cohort demands to get the total demand for this antigen
      total.demand = cohort.demand * dose.demand
      
      # get the bundles that can be used to satisfy antigen k
      k.bundles = bundle.antigen[Antigen == k, Bundle]
      
      # add up the production of all bundles in k.bundles to see how much got produced to satisfy this antigens total.demand
      total.production = sum(antigen.output[Bundle %in% k.bundles, Selling_Qty])
      
      # create an output table
      output = data.table(fileID = i,
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
  
  # free memory
  gc()
  
  return(market.output)
}

# combine the list of tables into 1 table
dat.baseline.demand = rbindlist(dat.baseline.demand)

# update MARR to be a factor variable
dat.baseline.demand[, MARR := factor(MARR, levels = unique(MARR))]

# make Markets into a factor variable
dat.baseline.demand[, Markets := paste(Markets, "Markets")]
dat.baseline.demand[, Markets := factor(Markets, levels = unique(Markets))]

# update MarketID
dat.baseline.demand[, MarketID := paste("Market", MarketID)]
dat.baseline.demand[, MarketID := factor(MarketID, levels = unique(MarketID))]

# give dat.baseline.deamnd an ID variable so we can preserve the original order of rows
dat.baseline.demand[, ID := 1:nrow(dat.baseline.demand)]

# compute global Demand across each Antigen for each fileID
global.demand.baseline = data.table(dat.baseline.demand[, .(Global_Demand = sum(Demand), 
                                                            Global_Production = sum(Production)),
                                                        by = .(fileID, Antigen)])

# make fileID and Antigen the key columns in both dat.baseline.demand and global.demand.baseline
setkey(dat.baseline.demand, fileID, Antigen)
setkey(global.demand.baseline, fileID, Antigen)

# join global.demand.baseline onto dat.baseline.demand
dat.baseline.demand = global.demand.baseline[dat.baseline.demand]

# order dat.baseline.demand by ID
dat.baseline.demand = dat.baseline.demand[order(ID)]

# remove ID from dat.baseline.demand
dat.baseline.demand[, ID := NULL]

# make Antigen into a factor variable
dat.baseline.demand[, Antigen := factor(Antigen, levels = unique(Antigen))]

# rename the columns of dat.baseline.demand
setnames(dat.baseline.demand, c("fileID", "Antigen", "Global_Demand.baseline", "Global_Production.baseline", "MARR", "Markets", "MarketID", "Demand.baseline", "Production.baseline"))

# give dat.demand an ID variable so we can preserve the original order of rows
dat.demand[, ID := 1:nrow(dat.demand)]

# set Antigen, MARR, Markets, MarketID as the key columns in dat.demand and dat.baseline.demand
setkey(dat.demand, Antigen, MARR, Markets, MarketID)
setkey(dat.baseline.demand, Antigen, MARR, Markets, MarketID)

# join dat.baseline.demand onto dat.demand
dat.demand = data.table(dat.baseline.demand[, !"fileID"][dat.demand])

# order dat.short by ID
dat.demand = dat.demand[order(ID)]

# remove the ID column
dat.demand[, ID := NULL]

# compute global demand satisfaction
dat.demand[, Global_Satisfaction := Global_Production / Global_Demand]
dat.demand[, Global_Satisfaction.baseline := Global_Production.baseline / Global_Demand.baseline]

# compute market demand satisfaction
dat.demand[, Market_Satisfaction := Production / Demand]
dat.demand[, Market_Satisfaction.baseline := Production.baseline / Demand.baseline]

# compute the change in Global_Satisfaction, Market_Satisfaction
dat.demand[, Global_Satisfaction.change := Global_Satisfaction - Global_Satisfaction.baseline]
dat.demand[, Market_Satisfaction.change := Market_Satisfaction - Market_Satisfaction.baseline]

# create a plot of global demand satisfaction across Markets and Antigens
global.demand.change.plot.1 = ggplot(dat.demand, aes(x = Antigen, y = Global_Satisfaction.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Global Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# global.demand.change.plot.1

# create a plot of global demand satisfaction across Markets and Antigens & MARR
global.demand.change.plot.2 = ggplot(dat.demand, aes(x = Antigen, y = Global_Satisfaction.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Global Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# global.demand.change.plot.2

# create a plot of global demand satisfaction across Markets and Antigens & Bundle_Impact
global.demand.change.plot.3 = ggplot(dat.demand, aes(x = Antigen, y = Global_Satisfaction.change, color = Bundle_Impact, fill = Bundle_Impact)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Global Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "Bundles", fill = "Bundles") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# global.demand.change.plot.3

# create a plot of global demand satisfaction across Markets and Antigens & Price_Drop
global.demand.change.plot.4 = ggplot(dat.demand, aes(x = Antigen, y = Global_Satisfaction.change, color = Price_Drop, fill = Price_Drop)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Global Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "Price Drop", fill = "Price Drop") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# global.demand.change.plot.4

# create a plot of market demand satisfaction across Markets and Antigens
market.demand.change.plot.1 = ggplot(dat.demand, aes(x = Antigen, y = Market_Satisfaction.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Market Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# market.demand.change.plot.1

# create a plot of market demand satisfaction across Markets and Antigens & MARR
market.demand.change.plot.2 = ggplot(dat.demand, aes(x = Antigen, y = Market_Satisfaction.change, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Market Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# market.demand.change.plot.2

# create a plot of market demand satisfaction across Markets and Antigens & Bundle_Impact
market.demand.change.plot.3 = ggplot(dat.demand, aes(x = Antigen, y = Market_Satisfaction.change, color = Bundle_Impact, fill = Bundle_Impact)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Market Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "Bundles", fill = "Bundles") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# market.demand.change.plot.3

# create a plot of market demand satisfaction across Markets and Antigens & Price_Drop
market.demand.change.plot.4 = ggplot(dat.demand, aes(x = Antigen, y = Market_Satisfaction.change, color = Price_Drop, fill = Price_Drop)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.short$MARR)))) +
  ggtitle("Change in Market Demand Satisfaction") + 
  labs(x = "Antigen", y = "Value", color = "Price Drop", fill = "Price Drop") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# market.demand.change.plot.4

# create a plot of demand satisfaction across MarketID for each market system
market.satisfaction.change.plots = lapply(unique(dat.demand$Markets), function(i)
  ggplot(dat.demand[Markets == i], aes(x = Antigen, y = Market_Satisfaction.change, color = MarketID, fill = MarketID)) +
    geom_jitter(alpha = 1/3) +
    stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
    scale_y_continuous(label = percent) +
    ggtitle("Change in Market Demand Satisfaction") + 
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
# market.satisfaction.change.plots[[1]]
# market.satisfaction.change.plots[[2]]
# market.satisfaction.change.plots[[3]]
# market.satisfaction.change.plots[[4]]

}

# -----------------------------------------------------------------------------------
# ---- Plotting Baseline Production Cost Recovery -----------------------------------
# -----------------------------------------------------------------------------------

{

# lets compute what the revenue is for each bundle that is to be sold, and compare that to the total production cost
# create a copy of dat.baseline.long
dat.baseline.revenue = data.table(dat.baseline.long)  

# lets compute low and high revenue based on the low/high selling prices and production qty
dat.baseline.revenue[, Revenue_Low := Selling_Price_Low * Selling_Qty]
dat.baseline.revenue[, Revenue_High := Selling_Price_High * Selling_Qty]

# make Markets into a factor variable
dat.baseline.revenue[, Markets := paste(Markets, "Markets")]
dat.baseline.revenue[, Markets := factor(Markets, levels = unique(Markets))]

# update MarketID
dat.baseline.revenue[, MarketID := paste("Market", MarketID)]
dat.baseline.revenue[, MarketID := factor(MarketID, levels = unique(MarketID))]

# make Bundle into a factor variable
dat.baseline.revenue[, Bundle := factor(Bundle, levels = unique(Bundle))]

# update MARR to be a factor variable
dat.baseline.revenue[, MARR := paste0(MARR * 100, "%")]
dat.baseline.revenue[, MARR := factor(MARR, levels = unique(MARR))]

# lets aggregate dat.baseline.revenue
dat.baseline.revenue = dat.baseline.revenue[, .(Revenue_Low = sum(Revenue_Low),
                                                Revenue_High = sum(Revenue_High),
                                                Production_Cost = mean(Production_Cost)),
                                            by = .(fileID, Markets, MARR, Bundle)]

# lets melt Revenue and Production Cost together
dat.baseline.revenue = melt(dat.baseline.revenue,
                            measure.vars = c("Revenue_Low", "Production_Cost", "Revenue_High"),
                            variable.name = "Production")

# update the Production to have values without any _ 
dat.baseline.revenue[, Production := gsub("_", " ", Production)]

# make Production a factor variable
dat.baseline.revenue[, Production := factor(Production, levels = unique(Production))]

# aggregate dat.baseline.revenue by dropping Bundle as a variable
dat.baseline.revenue = dat.baseline.revenue[, .(value = sum(value)),
                                            by = .(fileID, Markets, MARR, Production)]

# create a plot of revenue satisfaction across Markets and Production
revenue.baseline.plot.1 = ggplot(dat.baseline.revenue, aes(x = Production, y = value / 1e9, color = Production, fill = Production)) +
  geom_jitter(alpha = 1, size = 4) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Baseline Production Cost Recovery") + 
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

# revenue.baseline.plot.1

# create a plot of revenue satisfaction across MARR and Production
revenue.baseline.plot.4 = ggplot(dat.baseline.revenue, aes(x = Production, y = value / 1e9, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1, size = 4) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.baseline.revenue$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.baseline.revenue$MARR)))) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Baseline Production Cost Recovery") + 
  labs(x = "Bundle", y = "Value", color = "MARR", fill = "MARR") + 
  facet_wrap(~Markets, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "top", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# revenue.baseline.plot.4

}



# -----------------------------------------------------------------------------------
# ---- Plotting Lower Income Welfare ------------------------------------------------
# -----------------------------------------------------------------------------------

{

# create a copy of dat.long
dat.liw = data.table(dat.long)

# compute 3 Price levels
dat.liw[, Price_Low := (newRbm - Selling_Price_Low) * Selling_Qty]
dat.liw[, Price_High := (newRbm - Selling_Price_High) * Selling_Qty]
dat.liw[, Price_Middle := (newRbm - mean(c(Selling_Price_Low, Selling_Price_High))) * Selling_Qty]







# create a copy of dat.liw
xxx = data.table(dat.liw)

# compute change in surplus
xxx[, New_Surplus_at_Low_Price := (newRbm - Selling_Price_Low) * Selling_Qty]
xxx[, New_Surplus_at_High_Price := (newRbm - Selling_Price_High) * Selling_Qty]
xxx[, New_Surplus_at_Middle_Price := (newRbm - mean(c(Selling_Price_Low, Selling_Price_High))) * Selling_Qty]

# create a Price_Drop factor variable
xxx[, Price_Drop := paste0(Lower_Price_Drop * 100, "%-", Upper_Price_Drop * 100, "%")]
xxx[, Price_Drop := factor(Price_Drop, levels = unique(Price_Drop))]

# create a Bundle_Impact factor variable
xxx[, Bundle_Impact := paste0(Lower_Bundles_Impacted * 100, "%-", Upper_Bundles_Impacted * 100, "%")]
xxx[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]

# if 100%-100% exists in Bundle_Impact then replace it with 100%
if("100%-100%" %in% xxx$Bundle_Impact)
{
  xxx[, Bundle_Impact := gsub("100%-100%", "100%", Bundle_Impact)]
  xxx[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]
}

# update MARR to be a factor variable
xxx[, MARR := paste0(MARR * 100, "%")]
xxx[, MARR := factor(MARR, levels = unique(MARR))]

# make Markets into a factor variable
xxx[, Markets := paste(Markets, "Markets")]
xxx[, Markets := factor(Markets, levels = unique(Markets))]

# make MarketImpacted into a factor variable
xxx[, MarketImpacted := factor(MarketImpacted, levels = unique(MarketImpacted))]

# 
xxx = data.table(xxx[Markets == "6 Markets",.(Scenario, Replication, Markets, MarketID, Bundle, Price_Drop, Bundle_Impact, MARR, newRbm, Selling_Qty, Selling_Price_Low, Selling_Price_High, New_Surplus_at_Low_Price, New_Surplus_at_Middle_Price, New_Surplus_at_High_Price)])

# 
write.csv(xxx, 
          "USA Experiment - Total Consumer Surplus for Lower Income Markets - Experiment Results.csv",
          row.names = FALSE)

# only keep the 6 market system and the lower income countries
dat.liw = dat.liw[Markets == 6 & MarketID %in% 5:6]

# aggregate dat.liw to the fileID level
dat.liw = dat.liw[, .(Markets = mean(Markets),
                      MarketImpacted = sum(MarketID * MarketImpacted) / bundles,
                      Upper_Price_Drop = mean(Upper_Price_Drop),
                      Lower_Price_Drop = mean(Lower_Price_Drop),
                      Upper_Bundles_Impacted = mean(Upper_Bundles_Impacted),
                      Lower_Bundles_Impacted = mean(Lower_Bundles_Impacted),
                      MARR = mean(MARR),
                      Price_Low = sum(Price_Low),
                      Price_Middle = sum(Price_Middle), 
                      Price_High = sum(Price_High)),
                  by = fileID]

# create a Price_Drop factor variable
dat.liw[, Price_Drop := paste0(Lower_Price_Drop * 100, "%-", Upper_Price_Drop * 100, "%")]
dat.liw[, Price_Drop := factor(Price_Drop, levels = unique(Price_Drop))]

# create a Bundle_Impact factor variable
dat.liw[, Bundle_Impact := paste0(Lower_Bundles_Impacted * 100, "%-", Upper_Bundles_Impacted * 100, "%")]
dat.liw[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]

# if 100%-100% exists in Bundle_Impact then replace it with 100%
if("100%-100%" %in% dat.liw$Bundle_Impact)
{
  dat.liw[, Bundle_Impact := gsub("100%-100%", "100%", Bundle_Impact)]
  dat.liw[, Bundle_Impact := factor(Bundle_Impact, levels = unique(Bundle_Impact))]
}

# update MARR to be a factor variable
dat.liw[, MARR := paste0(MARR * 100, "%")]
dat.liw[, MARR := factor(MARR, levels = unique(MARR))]

# make Markets into a factor variable
dat.liw[, Markets := paste(Markets, "Markets")]
dat.liw[, Markets := factor(Markets, levels = unique(Markets))]

# make MarketImpacted into a factor variable
dat.liw[, MarketImpacted := factor(MarketImpacted, levels = unique(MarketImpacted))]

# create a copy of dat.baseline.long
dat.baseline.liw = data.table(dat.baseline.long)

# compute 3 Price levels
dat.baseline.liw[, basePrice_Low := (baseRbm - Selling_Price_Low) * Selling_Qty]
dat.baseline.liw[, basePrice_High := (baseRbm - Selling_Price_High) * Selling_Qty]
dat.baseline.liw[, basePrice_Middle := (baseRbm - mean(c(Selling_Price_Low, Selling_Price_High))) * Selling_Qty]

# create a copy of dat.liw
xxx = data.table(dat.baseline.liw)

# compute change in surplus
xxx[, Baseline_Surplus_at_Low_Price := (baseRbm - Selling_Price_Low) * Selling_Qty]
xxx[, Baseline_Surplus_at_High_Price := (baseRbm - Selling_Price_High) * Selling_Qty]
xxx[, Baseline_Surplus_at_Middle_Price := (baseRbm - mean(c(Selling_Price_Low, Selling_Price_High))) * Selling_Qty]

# update MARR to be a factor variable
xxx[, MARR := paste0(MARR * 100, "%")]
xxx[, MARR := factor(MARR, levels = unique(MARR))]

# make Markets into a factor variable
xxx[, Markets := paste(Markets, "Markets")]
xxx[, Markets := factor(Markets, levels = unique(Markets))]

# 
xxx = data.table(xxx[Markets == "6 Markets",.(Scenario, Markets, MarketID, Bundle, MARR, baseRbm, Selling_Qty, Selling_Price_Low, Selling_Price_High,  Baseline_Surplus_at_Low_Price,  Baseline_Surplus_at_Middle_Price,  Baseline_Surplus_at_High_Price)])

# 
write.csv(xxx, 
          "USA Experiment - Total Consumer Surplus for Lower Income Markets - Baseline Results.csv",
          row.names = FALSE)















# only keep the 6 market system and lower income countires
dat.baseline.liw = dat.baseline.liw[Markets == 6 & MarketID %in% 5:6]

# aggregate dat.baseline.liw to the fileID level
dat.baseline.liw = dat.baseline.liw[, .(Markets = mean(Markets),
                      MARR = mean(MARR),
                      basePrice_Low = sum(basePrice_Low),
                      basePrice_Middle = sum(basePrice_Middle), 
                      basePrice_High = sum(basePrice_High)),
                  by = fileID]

# update MARR to be a factor variable
dat.baseline.liw[, MARR := paste0(MARR * 100, "%")]
dat.baseline.liw[, MARR := factor(MARR, levels = unique(MARR))]

# make Markets into a factor variable
dat.baseline.liw[, Markets := paste(Markets, "Markets")]
dat.baseline.liw[, Markets := factor(Markets, levels = unique(Markets))]

# set Markets & MARR as the key columns in dat.liw and dat.baseline.liw
setkey(dat.liw, Markets, MARR)
setkey(dat.baseline.liw, Markets, MARR)

# join dat.baseline.liw onto dat.liw
dat.liw = data.table(dat.baseline.liw[, !"fileID"][dat.liw])

# order dat.liw by fileID
dat.liw = dat.liw[order(fileID)]

# compute the change in TSS, TCS, TPF, CMV, PMV
dat.liw[, Low := (Price_Low - basePrice_Low)] #  / basePrice_Low]
dat.liw[, Middle := (Price_Middle - basePrice_Middle)] #  / basePrice_Middle]
dat.liw[, High := (Price_High - basePrice_High)] #  / basePrice_High]


xxx = data.table(dat.liw[,.(Markets, Price_Drop, Bundle_Impact, MARR, 
                     Price_Low, 
                     Low, 
                     Price_Middle, 
                     Middle, 
                     Price_High, 
                     High)])
setnames(xxx, c("Markets", "Price_Drop", "Bundle_Impact", "MARR", 
                    "New_Surplus_at_Low_Selling_Price", 
                    "Baseline_Surplus_at_Low_Selling_Price", 
                    "New_Surplus_at_Middle_Selling_Price", 
                    "Baseline_Surplus_at_Middle_Selling_Price", 
                    "New_Surplus_at_High_Selling_Price", 
                    "Baseline_Surplus_at_High_Selling_Price"))

write.csv(xxx, 
          "USA Experiment - Total Consumer Surplus for Lower Income Markets.csv",
          row.names = FALSE)




# lets convert the Price columns into long format
dat.liw = melt(dat.liw, measure.vars = c("Low", "Middle", "High"))

# rename the values in the variable column
dat.liw[, variable := factor(ifelse(variable == "Low", "Low Selling Price", 
                                    ifelse(variable == "Middle", "Middle Selling Price", 
                                           "High Selling Price")),
                             levels = c("High Selling Price", "Middle Selling Price", "Low Selling Price"))]

# create a plot of LIW across Markets and Price_Drop & MARR for Price_Low
LIW.plot.1 = ggplot(dat.liw, aes(x = Price_Drop, y = value / 1e9, color = variable, fill = variable)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = dollar_format(suffix = "B")) +
  ggtitle("Change in Lower Income Welfare") + 
  labs(x = "Price Drop", y = "Value") + 
  facet_wrap(~variable, nrow = 1) +
  theme_bw(25) + 
  theme(legend.position = "none", 
        legend.key.size = unit(.25, "in"), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(override.aes = list(size = 10, linetype = 1, alpha = 1), nrow = 1))

# LIW.plot.1







TSS.change.plot.1 = ggplot(dat.short, aes(x = Price_Drop, y = TSS.change, color = Markets, fill = Markets)) +
  geom_jitter(alpha = 1/3) +
  stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
  scale_y_continuous(label = percent) +
  ggtitle("Change in Total Social Surplus") + 
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
















# create a plot of LIW across Markets and Price_Drop & MARR for Surplus_Middle
LIW.plot.2 = ggplot(dat.liw, aes(x = Price_Drop, y = changeSurplus_Middle, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.liw$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.liw$MARR)))) +
  ggtitle("Change in Lower Income Welfare") + 
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

# LIW.plot.2

# create a plot of LIW across Markets and Price_Drop & MARR for Surplus_High
LIW.plot.3 = ggplot(dat.liw, aes(x = Price_Drop, y = changeSurplus_High, color = MARR, fill = MARR)) +
  geom_jitter(alpha = 1/3) +
  scale_y_continuous(label = percent) +
  scale_color_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.liw$MARR)))) +
  scale_fill_manual(values = colorRampPalette(c("cornflowerblue", "orangered"))(length(unique(dat.liw$MARR)))) +
  ggtitle("Change in Lower Income Welfare") + 
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

# LIW.plot.3

}







# -----------------------------------------------------------------------------------
# ---- Plotting Baseline Dropped Constraints ----------------------------------------
# -----------------------------------------------------------------------------------

{

# update exp.out.path so we can get the dropped contraints files
exp.out.baseline.path.new = paste0(exp.out.baseline.path, "/DropConst")

# update the work directory
setwd(exp.out.baseline.path.new)

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
  dropped.baseline = foreach(i = list.files()) %fun%
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
  dropped.baseline = rbindlist(dropped.baseline)
  
  # only keep the name of the Contraint
  dropped.baseline[, Contraint := sapply(1:nrow(dropped.baseline), function(i) 
    substr(x = dropped.baseline$Contraint[i], 
           start = 1, 
           stop = which(strsplit(dropped.baseline$Contraint[i], "")[[1]] == "[") - 1))]
  
  # remove all numbers and punctuation form the Constriant column
  dropped.baseline[, Contraint := removePunctuation(removeNumbers(Contraint))]
  
  # remove the _drop.out phrase from the entries in File
  dropped.baseline[, File := gsub("_drop.out", "", File)]
  
  # remove the fileID_ phrase from the entries in File
  dropped.baseline[, File := as.numeric(gsub("fileID_", "", File))]
  
  # make a copy of dat.baseline.demand
  my.copy = data.table(dat.baseline.demand)
  
  # only keep fileID, MARR, Markets
  my.copy = my.copy[,.(fileID, MARR, Markets)]
  
  # only keep the unique values of fileID, Price_Drop, Bundles_Impacted, Markets
  my.copy = my.copy[!duplicated(my.copy)]
  
  # make fileID the key column in my.copy, and File the key column in dropped.baseline
  setkey(my.copy, fileID)
  setkey(dropped.baseline, File)
  
  # join my.copy onto dropped.baseline
  dropped.baseline = my.copy[dropped.baseline]
  
  # aggregate dropped.baseline by fileID
  dropped.baseline = dropped.baseline[,.(Count = .N),
                    by = .(MARR, Markets, Contraint)]
  
  # create a plot of dropped.baseline constraints across Markets and Production
  dropped.baseline.plot.1 = ggplot(dropped.baseline, aes(x = Contraint, y = Count, color = Markets, fill = Markets)) +
    geom_jitter(alpha = 1, width = 0.25, size = 4) +
    stat_summary(fun.y = mean, color = "black", geom = "point", shape = 18, size = 5) +
    ggtitle("Baseline Dropped Constraints") + 
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
  
  # dropped.baseline.plot.1
}

}

# -----------------------------------------------------------------------------------
# ---- Exporting Change in Surplus & Profit Data ------------------------------------
# -----------------------------------------------------------------------------------

{

# update the work directory
setwd(exp.out.baseline.path)

# export TSS.change data for predictive modeling
write.csv(x = cbind(TSS.change = dat.short$TSS.change, 
                    data.table(model.matrix(~ ., dat.short[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
          file = "TSS-Change-Drop-Price-USA.csv", 
          row.names = FALSE)

# export TCS data for predictive modeling
write.csv(x = cbind(TCS.change = dat.short$TCS.change, 
                    data.table(model.matrix(~ ., dat.short[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
          file = "TCS-Change-Drop-Price-USA.csv", 
          row.names = FALSE)

# export TPF data for predictive modeling
write.csv(x = cbind(TPF.change = dat.short$TPF.change, 
                    data.table(model.matrix(~ ., dat.short[,.(Markets, Price_Drop, Bundle_Impact, MARR)])[,-1])), 
          file = "TPF-Change-Drop-Price-USA.csv", 
          row.names = FALSE)

save.image("C:/Users/Nick Morris/Downloads/ABP/Drop-Price-USA-52B-14M/Drop-Price-USA.RData")

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





#####################################################################################
############################# NOT UPDATED ###########################################
#####################################################################################

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







