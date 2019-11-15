######### Plots for satRday presentation ##########
#
# User: Anne
# Date: 15.11.2019
#
# Purpose: share code of the plots I used in my satRday presentation.
#
# Plots:
# 1. First basic violin plot.
# 2. Adding facets.
# 3. Adding mean + se error bars.
# 4. Changing to split violin plots, increasing text size.
# 5. Bonus: Bar graph.
#
# Possible libraries:
# library(tidyverse) # Always import this, needed for everything
# library(foreign)   # In case I want to import SPSS data
# library(ggpubr)    # Needed for stat_compare_means
# library(sciplot)   # Needed to calculate standard error (se)
# library(export)    # To export graphs as vector images



###############################################
######### 1. First basic violin plot. #########
###############################################

# Clear variables
rm(list=ls())

# Set working directory
setwd("C:/Users/Anne/R_analyses/")

# Activate libraries
library(tidyverse) #always import this
library(ggpubr) #needed for stat_compare_means
library(sciplot) #needed to calculate standard error (se)

# Read my CSV data
dat <- read.csv('yourfile.csv', header=TRUE, fill=TRUE, sep=";", check.names=FALSE,
                  blank.lines.skip = TRUE)
head(dat)


p <- ggplot(dat, aes(x=Group, y=Accuracy_perc, colour=TMR)) +
  geom_violin()+
  labs(y = "Accuracy (% correct)")+
  scale_colour_manual(values=c("#E69F00", "#56B4E9"))
p


destppt <- tempfile(tmpdir="C:/Users/Anne Koopman/Dropbox (PhD in Cardiff)/Experiment 2 - Remote Associations/Data/Normalisation_results/R_analyses/")
graph2ppt(file=destppt, append = TRUE, width=7, height=4.5)




#####################################
######### 2. Adding facets. #########
#####################################

# Clear variables
rm(list=ls())

# Set working directory
setwd("C:/Users/Anne/R_analyses/")

# Activate libraries
library(tidyverse) #always import this
library(ggpubr) #needed for stat_compare_means
library(sciplot) #needed to calculate standard error (se)

# Read my CSV data
dat <- read.csv('yourfile.csv', header=TRUE, fill=TRUE, sep=";", check.names=FALSE,
                blank.lines.skip = TRUE)
head(dat)


p <- ggplot(dat, aes(x=Group, y=Accuracy_perc, colour=TMR)) +
  geom_violin()+
  labs(y = "Accuracy (% correct)")+
  scale_colour_manual(values=c("#E69F00", "#56B4E9"))+
  facet_wrap(~Time, scales = "free_x")
p


destppt <- tempfile(tmpdir="C:/Users/Anne Koopman/Dropbox (PhD in Cardiff)/Experiment 2 - Remote Associations/Data/Normalisation_results/R_analyses/")
graph2ppt(file=destppt, append = TRUE, width=7, height=4.5)




###################################################
######### 3. Adding mean + se error bars. #########
###################################################

# Clear variables
rm(list=ls())

# Set working directory
setwd("C:/Users/Anne/R_analyses/")

# Activate libraries
library(tidyverse) #always import this
library(ggpubr) #needed for stat_compare_means
library(sciplot) #needed to calculate standard error (se)

# Read my CSV data
dat <- read.table('yourfile.csv', header=TRUE, fill=TRUE, sep=";", check.names=FALSE,
                  blank.lines.skip = TRUE)
head(dat)


#+++++++
# Function to calculate the mean & standard deviation to later plot the error bars
#+++++++
# This function is for use with stat_summary.

data_summ <- function(x) {
  m <- mean(x)
  ymin <- m-se(x)
  ymax <- m+se(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#+++++++
# Function to calculate the mean and the standard deviation for each group
#+++++++
# data : a data frame
# varname : the name of a column containing the variable to be summarised
# groupnames : vector of column names to be used as grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  require(dplyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = se(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}


# Calculate the summary data for my data frame
dat_summ <- data_summary(dat, varname="Accuracy_perc", 
                         groupnames=c("TMR", "Time","Group"))
head(dat_summ)

# Calculate the stats
compare_means(Accuracy_perc ~ TMR, data = dat, method = "t.test", paired = TRUE, group.by = c("Time","Group"))

# This creates an ordered factor. If we don't do this, ggplot wants to plot
# the facets in alphabetical order. In this case that's not a problem.
#dat$Time <- factor(dat$Time, levels=c("Morning","Two-week follow-up"))

# Actually plot my violin plots
# With Error bars for each group
# Points that specify the mean for each group

p <- ggplot(dat, aes(x=Group, y=Accuracy_perc, color=TMR)) +
  geom_violin()+
  labs(y = "Accuracy (% correct)")+
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  stat_summary(fun.data=data_summ,position=position_dodge(1))+
  geom_point(data = dat_summ, position=position_dodge(1))+
  geom_errorbar(data = dat_summ, aes(ymin=Accuracy_perc-se, ymax=Accuracy_perc+se), width=.1, position=position_dodge(1))+
  facet_wrap(~Time, scales = "free_x")
p

### If I'd want to draw the quantiles, I can add this bit of code in geom_violin():
#draw_quantiles = c(0.25, 0.5, 0.75)

destppt <- tempfile(tmpdir="C:/Users/Anne Koopman/Dropbox (PhD in Cardiff)/Experiment 2 - Remote Associations/Data/Normalisation_results/R_analyses/")
graph2ppt(file=destppt, append = TRUE, width=7, height=4.5)


############################################################################
######### 4. Changing to split violin plots, increasing text size. #########
############################################################################

# Clear variables
rm(list=ls())

# Set working directory
setwd("C:/Users/Anne/R_analyses/")

# Activate libraries
library(tidyverse) #always import this
library(ggpubr) #needed for stat_compare_means
library(sciplot) #needed to calculate standard error (se)

# Read my CSV data
dat <- read.table('yourfile.csv', header=TRUE, fill=TRUE, sep=";", check.names=FALSE,
                  blank.lines.skip = TRUE)
head(dat)


#+++++++
# Function to calculate the mean & standard deviation to later plot the error bars
#+++++++
# This function is for use with stat_summary.

data_summ <- function(x) {
  m <- mean(x)
  ymin <- m-se(x)
  ymax <- m+se(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

#+++++++
# Function to calculate the mean and the standard deviation for each group
#+++++++
# data : a data frame
# varname : the name of a column containing the variable to be summarised
# groupnames : vector of column names to be used as grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  require(dplyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = se(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#+++++++
# Function to make a split violin plot
#+++++++
# Taken from: https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

# Calculate the summary data for my data frame
dat_summ <- data_summary(dat, varname="Accuracy_perc", 
                         groupnames=c("TMR", "Time","Group"))
head(dat_summ)

# Calculate the stats
compare_means(Accuracy_perc ~ TMR, data = dat, method = "t.test", paired = TRUE, group.by = c("Time","Group"))

# This creates an ordered factor. If we don't do this, ggplot wants to plot
# the facets in alphabetical order. In this case that's not a problem.
#dat$Time <- factor(dat$Time, levels=c("Morning","Two-week follow-up"))

# Actually plot my violin plots
# With Error bars for each group
# Points that specify the mean for each group

p <- ggplot(dat, aes(x=Group, y=Accuracy_perc, fill=TMR)) +
  geom_split_violin()+
  #stat_compare_means(label = "p.signif", label.y = 100, method = "t.test", paired = TRUE, hide.ns = TRUE)+
  labs(y = "Accuracy (% correct)")+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  theme(text = element_text(size = 15))+
  #theme_pubclean() +
  #scale_x_discrete(limits=c("SWS","REM"))+
  #stat_summary(fun.data=data_summ,position=position_dodge(0.3))+
  #geom_line(data = dat_summ, aes(y=Accuracy_perc, group = TMR),position=position_dodge(0.9))+
  geom_point(data = dat_summ, position=position_dodge(0.3))+
  geom_errorbar(data = dat_summ, aes(ymin=Accuracy_perc-se, ymax=Accuracy_perc+se), width=.1, position=position_dodge(0.3))+
  scale_y_continuous(breaks=c(25,50,75,100),limits=c(13,100))+
  facet_wrap(~Time, scales = "free_x")
p

destppt <- tempfile(tmpdir="C:/Users/Anne Koopman/Dropbox (PhD in Cardiff)/Experiment 2 - Remote Associations/Data/Normalisation_results/R_analyses/")
graph2ppt(file=destppt, append = TRUE, width=7, height=4.5)

### If I'd want to draw the quantiles, I can add this bit of code in geom_violin():
#draw_quantiles = c(0.25, 0.5, 0.75)





########################################
######### 5. Bonus: Bar graph. #########
########################################

# Clear variables
rm(list=ls())

# Set working directory
setwd("C:/Users/Anne/R_analyses/")

# Activate libraries
library(tidyverse) #always import this
library(ggpubr) #needed for stat_compare_means
library(sciplot) #needed to calculate standard error (se)
library(export)

# Read my CSV data
dat <- read.table('yourfile.csv', header=TRUE, fill=TRUE, sep=";", check.names=FALSE,
                  blank.lines.skip = TRUE)
head(dat)


#+++++++
# Function to calculate the mean and the standard deviation for each group
#+++++++
# data : a data frame
# varname : the name of a column containing the variable to be summarised
# groupnames : vector of column names to be used as grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  require(dplyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = se(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}


# Calculate the summary data for my data frame
dat_summ <- data_summary(dat, varname="Accuracy_perc", 
                         groupnames=c("TMR", "Time","Group"))
head(dat_summ)

# Calculate the stats
compare_means(Accuracy_perc ~ TMR, data = dat, method = "t.test", paired = TRUE, group.by = c("Time","Group"))

# This creates an ordered factor. If we don't do this, ggplot wants to plot
# the facets in alphabetical order. In this case that's not a problem.
#dat$Time <- factor(dat$Time, levels=c("Morning","Two-week follow-up"))

# Actually plot my violin plots
# With Error bars for each group
# Points that specify the mean for each group

p <- ggplot(dat_summ, aes(x=Group, y=Accuracy_perc, fill=TMR)) +
  geom_bar(stat = "identity", position=position_dodge(), color="black")+
  stat_compare_means(data = dat, label = "p.signif", label.y = 70, method = "t.test", paired = TRUE, hide.ns = TRUE)+
  labs(y = "Accuracy (% correct)")+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  theme(text = element_text(size = 15))+
  #theme_pubclean() +
  #geom_point(data = dat_summ, position=position_dodge(0.9))+
  geom_errorbar(data = dat_summ, aes(ymin=Accuracy_perc-se, ymax=Accuracy_perc+se), width=.2, position=position_dodge(0.9))+
  #geom_point(data = dat, color="black", size=2, alpha=0.5, position=position_dodge(0.9))+
  coord_cartesian(ylim=c(40,75)) +
  facet_wrap(~Time, scales = "free_x")
p

destppt <- tempfile(tmpdir="C:/Users/Anne Koopman/Dropbox (PhD in Cardiff)/Experiment 2 - Remote Associations/Data/Normalisation_results/R_analyses/")
graph2ppt(file=destppt, append = TRUE, width=7, height=4.5)

### If I'd want to draw the quantiles, I can add this bit of code in geom_violin():
#draw_quantiles = c(0.25, 0.5, 0.75)

