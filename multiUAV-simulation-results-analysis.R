#!/usr/bin/Rscript
#
# CopterLogAnalysis.R - Statistical multicopter energy consumption
# analysis based on Ardupilot dataflash log files.
# Copyright (C) 2017  Thomas Dietrich
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Clear workspace
remove(list = ls())
# Close all devices still open from previous executions
graphics.off()

#####################################################################
# Logfiles to analyze ###############################################

#####################################################################
# Settings ##########################################################

logdata_folder <- "./multiUAV-simulation-results/numUAVs/"
tikzLocation   <- "./tikz/"

# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
#graphCol1 <- "steelblue"
#graphCol2 <- "goldenrod"
#graphCol2_dark <- "goldenrod4"
#graphCol3 <- "gray70"

#graphCol1 <- "#353594"
graphCol1 <- "#4682B4" #steelblue
graphCol2 <- "#B8850A" #mygold
graphCol2_dark <- "goldenrod4"
graphCol3 <- "#B3B3B3" #mygray
graphCol3_dark <- "#737373"
graphCol3_darkdark <- "#434343"
graphCol4 <- "#780116" #myred

#####################################################################
# Install packages, load libraries ##################################

list.of.packages <- c("Hmisc", "geosphere", "ggplot2", "cowplot", "tikzDevice", "TTR", "xts", "forecast",
                      "data.table", "xtable", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
sapply(list.of.packages, require, character.only = TRUE)
lapply(list.of.packages, packageVersion)
remove(list = c("list.of.packages", "new.packages"))

# Increase max number of warnings
options(nwarnings=200) 

### Update packages (from time to time!)
#update.packages(checkBuilt=TRUE, ask=FALSE)

# plotlyUsername <- "user"
# plotlyApiKey <- "key"
#source("plotlyCredentials.R")

#Sys.setenv("plotly_username" = plotlyUsername)
#Sys.setenv("plotly_api_key" = plotlyApiKey)
#remove(plotlyUsername, plotlyApiKey)


theme_custom <- function () {
  theme_light() %+replace% 
    theme(
      panel.background  = element_blank(),
      axis.title = element_text(size = rel(0.8)),
      axis.ticks=element_blank(),
      legend.text = element_text(size = rel(0.8)),
      legend.title = element_text(size = rel(0.8)),
      panel.border=element_blank()
    )
}
theme_set(theme_custom())

options(tikzDefaultEngine = "xetex")

#####################################################################
# Simulation runtime estimation #####################################

{
  repetitions <- 12
  hours <- 72
  parameter_iteration <- 12
  
  birke_faktor <-(47 / 60) / 16 / (3 * 1 * 1 * 16)
  
  simulation_runtime_hours <- repetitions * hours * parameter_iteration * birke_faktor
  
  cat(paste("Expected runtime:", round(simulation_runtime_hours, digits = 1), "hours"))
}

#####################################################################

df.all <- data.frame()

for (filename in list.files(logdata_folder,"*.sca")) {
  logdata.file <- data.frame()
  cat(filename)
  cat(": ")
  
  filename.basename <- str_replace(filename, "(.*?)-.*", "\\1")
  
  filename.replM       <- ifelse(grepl("replM=", filename), str_replace(filename, ".*[-,]replM=(\\d).*", "\\1"), NA)
  filename.biWeight    <- ifelse(grepl("biWeight=", filename), str_replace(filename, ".*[-,]biWeight=(\\d\\.\\d+)[-,].*", "\\1"), NA)
  filename.replSearchM <- ifelse(grepl("replSearchM=", filename), str_replace(filename, ".*[-,]replSearchM=(\\d).*", "\\1"), NA)
  filename.quant       <- ifelse(grepl("quant=", filename), str_replace(filename, ".*[-,]quant=(0\\.\\d*).*", "\\1"), NA)
  filename.numUAVs     <- ifelse(grepl("numUAVs=", filename), str_replace(filename, ".*[-,]numUAVs=(\\d+)[-,].*", "\\1"), NA)
  
  filename.repeat <- str_replace(filename, ".*-#(\\d+).*", "\\1")
  
  cat("Load File ... ")
  lines <- readLines(paste(logdata_folder, filename, sep=""))
  #head(lines)
  lines.scalar <- grep("scalar OsgEarthNet", lines, value=TRUE)
  #head(lines.scalar)
  
  cat("Parse Scalars ")
  cnt <- 0
  for (line in lines.scalar) {
    node.type <- str_replace(line, "scalar OsgEarthNet\\.(.*?)\\[\\d+\\].*", "\\1")
    index <- str_replace(line, "scalar OsgEarthNet.*?\\[(\\d+)\\].*", "\\1")
    metric <- str_replace(line, "scalar OsgEarthNet.*?\\[\\d+\\] (\\w*).*", "\\1")
    value <-str_replace(line, "scalar OsgEarthNet.*?\\[\\d+\\] \\w* (.*)", "\\1")
    #print(paste(index, metric, value, sep = " --- "))
    
    logdata.line <- data.frame(
      basename =         as.factor(filename.basename),
      replM =            as.integer(filename.replM),
      biWeight =         as.numeric(filename.biWeight),
      quant =            as.numeric(filename.quant),
      replSearchM =      as.factor(filename.replSearchM),
      numUAVs =          as.integer(filename.numUAVs),
      simRun =           as.integer(filename.repeat),
      nodeType =         as.factor(node.type),
      index =            as.integer(index),
      metric =           as.factor(metric),
      value =            as.numeric(value)
    )
    logdata.file <- rbind(logdata.file, logdata.line)
    
    cnt <- cnt + 1
    if (cnt %% 30 == 0) cat(".")
  }
  cat("\n")
  df.all <- rbind(df.all, logdata.file)
}

# Split types
df.all.uav <- subset(df.all, nodeType == 'uav')
df.all.cs <- subset(df.all, nodeType == 'cs')

# indicies
replMs <- sort(unique(df.all$replM))
biWeights <- sort(unique(df.all$biWeight))
quants <- sort(unique(df.all$quant))
replSearchMs <- sort(unique(df.all$replSearchM))
numUAVss <- sort(unique(df.all$numUAVs))
sim_runs <- sort(unique(df.all.uav$simRun))
repeats <- max(df.all$simRun) + 1

if (length(quants) == 0) quants <- NA

#####################################################################

uavs_count <- max(df.all.uav$index)
metrics <- levels(factor(df.all.uav$metric))

cs_count <- max(df.all.cs$index)
metrics.cs <- levels(factor(df.all.cs$metric))


cat("Remove run+UAVs with 'fail' metric ... ")

uavs_all <- subset(df.all.uav, metric == 'utilizationFail')
uavs_all_failed <- subset(df.all.uav, metric == 'utilizationFail' & value != 0)

if (nrow(uavs_all_failed) > 0) {
  warning(paste(nrow(uavs_all_failed), "of", nrow(uavs_all), "UAVs over all simulation runs ended in 'fail' state", sep=" "))
}

if (nrow(uavs_all_failed) > 0) {
  for(i in 1:nrow(uavs_all_failed)) {
    row <- uavs_all_failed[i,]
    #print(row)
    df.all.uav <- df.all.uav[!(df.all.uav$replM==row$replM & df.all.uav$simRun==row$simRun & df.all.uav$index==row$index),]
  }
}
remove(uavs_all, uavs_all_failed)

if (length(quants) > 1) {
  cat("Checking Simtime for premature termination ... ")
  simtimeSec <- 259200
  
  for (replMethod in replMs) {
    for (q in quants) {
      count <- 0
      for (run in sim_runs) {
        df_subset <- subset(df.all.uav, replM == replMethod & quant == q & simRun == run & metric == "simulationTime")
        this_simtime <- unique(df_subset$value)
        if (this_simtime != simtimeSec) {
          print(paste(replMethod, q, run, unique(df_subset$value)))
          count <- count + 1
          df.all.uav <- df.all.uav[!(df.all.uav$replM == replMethod & df.all.uav$quant == q & df.all.uav$simRun == run),]
        }
      }
      cat(paste("Quantile", q, "Premature terminations:", count, "\n"))
    }
  }
}


simtimeSec <- 0
cat("Proofchecking Simtime ... ")
tolerance = 0.1
for (replMethod in replMs) {
  for (biw in biWeights) {
    for (q in quants) {
      for (replsm in replSearchMs) {
        for (numuavs in numUAVss) {
          for (sim_run in sim_runs) {
            for (uav_index in 0:uavs_count) {
              df_subset <- subset(df.all.uav, replM == replMethod & match(replSearchM,replsm) & match(quant,q) & match(biWeight,biw) & match(numUAVs,numuavs) & simRun == sim_run & index == uav_index
                                  & metric %in% c('utilizationSecMaintenance', 'utilizationSecMission', 'utilizationSecCharge', 'utilizationSecIdle')
              )
              if (nrow(df_subset) > 0) {
                if (nrow(df_subset) != 4) {
                  print(df_subset)
                  stop("Subset is not unique. Either selection too broad or simlulation ran twice.")
                }
                #print(sum(df_subset$value))
                if (simtimeSec == 0) {
                  simtimeSec = as.integer(0.5 + sum(df_subset$value))
                  cat(paste("Simulation time:", simtimeSec))
                }
                else if (abs(simtimeSec - sum(df_subset$value)) > tolerance) stop(paste("Simtime missmatch! Hours:", sum(df_subset$value)/3600))
              }
            }
          }
        }
      }
    }
  }
}

#####################################################################
#
#cat("Combining SimRuns ... ")
#df.comb <- data.frame()
#
#for (replMethod in replMs) {
#  for (index in 0:uavs_count) {
#    for (metric in metrics) {
#      df_subset <- subset(df.all.uav, replM == replMethod & index == index & metric == metric)
#      metric_value.mean <- sum(df_subset$value)
#      metric_value.stddev <- sqrt(var(df_subset$value))
#      #print(paste(replMethod, uav, metric, metric_value.mean, metric_value.stddev, sep = " -- "))
#      df <- data.frame(
#        basename =    levels(df_subset$basename),
#        replM =       as.integer(replMethod),
#        index =       as.integer(index),
#        metric =      as.factor(metric),
#        value =       as.numeric(metric_value.mean),
#        valueStddev = as.numeric(metric_value.stddev)
#      )
#      df.comb <- rbind(df.comb, df)
#    }    
#  }
#}
#####################################################################

cat("Summarizing Metrics per UAV ... ")
df.red.uav <- data.frame()

for (replMethod in replMs) {
  for (biw in biWeights) {
    for (q in quants) {
      for (replsm in replSearchMs) {
        for (numuavs in numUAVss) {
          for (sim_run in sim_runs) {
            for (uav_index in 0:uavs_count) {
              df_subset <- subset(df.all.uav, replM == replMethod & match(replSearchM,replsm) & match(quant,q) & match(biWeight,biw) & match(numUAVs,numuavs) & simRun == sim_run & index == uav_index)
              if (nrow(df_subset) == 0) next
              df <- data.frame(
                basename =    levels(df_subset$basename),
                replM =       as.integer(replMethod),
                biWeight =    as.numeric(biw),
                quant =       as.numeric(q),
                replSearchM = as.factor(replsm),
                numUAVs =     as.integer(numuavs),
                simRun =      as.integer(sim_run),
                index =       as.integer(uav_index),
                #
                secMission =                  as.numeric(subset(df_subset, metric == 'utilizationSecMission')$value),
                secMaintenance =              as.numeric(subset(df_subset, metric == 'utilizationSecMaintenance')$value),
                secCharge =                   as.numeric(subset(df_subset, metric == 'utilizationSecCharge')$value),
                secIdle =                     as.numeric(subset(df_subset, metric == 'utilizationSecIdle')$value),
                #
                energyMission =               as.numeric(subset(df_subset, metric == 'utilizationEnergyMission')$value),
                energyMaintenance =           as.numeric(subset(df_subset, metric == 'utilizationEnergyMaintenance')$value),
                energyCharge =                as.numeric(subset(df_subset, metric == 'utilizationEnergyCharge')$value),
                #
                energyOverdrawMission =       as.numeric(subset(df_subset, metric == 'utilizationEnergyOverdrawMission')$value),
                energyOverdrawMaintenance =   as.numeric(subset(df_subset, metric == 'utilizationEnergyOverdrawMaintenance')$value),
                #
                countMissions =               as.numeric(subset(df_subset, metric == 'utilizationCountMissions')$value),
                countManeuversMission =       as.numeric(subset(df_subset, metric == 'utilizationCountManeuversMission')$value),
                countManeuversMaintenance =   as.numeric(subset(df_subset, metric == 'utilizationCountManeuversMaintenance')$value),
                countChargeState =            as.numeric(subset(df_subset, metric == 'utilizationCountChargeState')$value),
                countOverdrawnAfterMission =  as.numeric(subset(df_subset, metric == 'utilizationCountOverdrawnAfterMission')$value),
                countIdleState =              as.numeric(subset(df_subset, metric == 'utilizationCountIdleState')$value)
              )
              df.red.uav <- rbind(df.red.uav, df)
            }
          }
        }
      }
    }
  }
}

cat("Summarizing Metrics per CS ... ")
df.red.cs <- data.frame()

for (replMethod in replMs) {
  for (biw in biWeights) {
    for (q in quants) {
      for (replsm in replSearchMs) {
        for (numuavs in numUAVss) {
          for (sim_run in sim_runs) {
            for (cs_index in 0:cs_count) {
              df_subset <- subset(df.all.cs, replM == replMethod & match(replSearchM,replsm) & match(quant,q) & match(biWeight,biw) & match(numUAVs,numuavs) & simRun == sim_run & index == cs_index)
              if (nrow(df_subset) == 0) next
              df <- data.frame(
                basename =    levels(df_subset$basename),
                replM =       as.factor(replMethod),
                biWeight =    as.numeric(biw),
                quant =       as.numeric(q),
                replSearchM = as.factor(replsm),
                numUAVs =     as.integer(numuavs),
                simRun =      as.integer(sim_run),
                index =       as.factor(cs_index),
                #
                usedPower =                as.numeric(subset(df_subset, metric == 'usedPower')$value),
                chargedPower =             as.numeric(subset(df_subset, metric == 'chargedPower')$value),
                chargedMobileNodes =       as.integer(subset(df_subset, metric == 'chargedMobileNodes')$value),
                chargedMobileNodesOthers = as.integer(sum(subset(df.all.cs, replM == replMethod & simRun == sim_run & metric == 'chargedMobileNodes')$value) - subset(df_subset, metric == 'chargedMobileNodes')$value),
                chargedMobileNodesAll =    as.integer(sum(subset(df.all.cs, replM == replMethod & simRun == sim_run & metric == 'chargedMobileNodes')$value)),
                reservations =             as.integer(subset(df_subset, metric == 'reservations')$value)
              )
              df.red.cs <- rbind(df.red.cs, df)
            }
          }
        }
      }
    }
  }
}

stop("Please evaluate data by hand from here on out!")

#####################################################################
# Quantile Analysis


for (q in quants) {
  df <- subset(df.red.uav, replM==0 & quant==q)
  count_missions <- sum(df$countMissions)
  count_overdrawn_maint <- sum(df$countOverdrawnAfterMission)
  ratio_overdrawn_maint <- 100 / count_missions * count_overdrawn_maint
  energy_overdrawn_mission <- sum(df$energyOverdrawMission)
  energy_per_uav.per_lc <- mean((df$energyMission + df$energyMaintenance) / df$countMissions)
  
  print(paste("Premature Battery depletion cases", q))
  print(paste("Count", count_missions, "overdrawn", count_overdrawn_maint))
  print(paste("Energy per life cycle", energy_per_uav.per_lc))
}

#####################################################################
# Independence CS/UAV ###############################################

df <- subset(df.red.uav, replM==0 & quant==0.95)

map_id_to_location <- function(id) c('\\ \\textbf{CS1:} Tennis court Ritzeb\\"uhl', '\\ \\textbf{CS2:} Sports field Manebach', '\\ \\textbf{CS3:} Hotel Gabelbach')[as.integer(id)]
starting_cs <- as.factor(map_id_to_location(1 + (df$index %% 3)))

secMission.quota <- 100 / (df$secMission + df$secMaintenance + df$secCharge) * df$secMission

tikz(paste(tikzLocation, "8_initial_location_plot_R.tex", sep = ""), standAlone=TRUE, timestamp = FALSE, width=5.99, height=2.0)

ggplot(df, aes(x = starting_cs, y = secMission / 60)) +
  geom_jitter(width=0.05, color=graphCol3_dark, alpha=0.3) +
  geom_boxplot(width=0.6, color=graphCol1, fill=alpha("white", 0.6), outlier.alpha=0) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(starting_cs))) +
  theme(axis.title.y = element_blank()) +
  labs(x="Charging Station", y="Time in Missions [min]")

dev.off()

df <- subset(df.red.uav, replM==0 & quant==0.95)

tikz(paste(tikzLocation, "8_cs_popularity_plot_R.tex", sep = ""), standAlone=TRUE, timestamp = FALSE, width=5.99, height=2.0)

ggplot(df, aes(x = map_id_to_location(index), y = chargedMobileNodes)) +
  geom_jitter(width=0.05, color=graphCol3_dark, alpha=0.6) +
  geom_boxplot(width=0.6, color=graphCol1, fill=alpha("white", 0.6), outlier.alpha=0) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(starting_cs))) +
  theme(axis.title.y = element_blank()) +
  labs(x="Charging Station", y="Served UAVs")

dev.off()


#####################################################################
# General Performance Data ##########################################
# only replM==0

df <- subset(df.red.uav, replM==0 & quant==0.95 & countMissions > 10)
life_cycles_uav.mean <- mean(df$countMissions)
life_cycles_uav.stddev <- sqrt(var(df$countMissions))

cat(paste("Life cycles per UAV", life_cycles_uav.mean, life_cycles_uav.stddev))

cat(paste("Replacements per hour", sum(df$countMissions) / repeats / (simtimeSec / 3600)))

cons_energy_per_uav.overall <- mean(df$energyMission + df$energyMaintenance)
cons_energy_per_uav.perhour <- mean(df$energyMission + df$energyMaintenance) / (simtimeSec / 3600)
cons_energy_per_uav.permission <- mean((df$energyMission + df$energyMaintenance) / df$countMissions)
cons_energy_per_uav.permission.stddev <- sqrt(var((df$energyMission + df$energyMaintenance) / df$countMissions))

cat(paste("Energy consumption per UAV", cons_energy_per_uav.overall, cons_energy_per_uav.perhour, cons_energy_per_uav.permission))

cat(paste("Energy consumption per UAV overall per day", cons_energy_per_uav.overall / (simtimeSec / 3600 / 24)))

cat(paste("Energy consumption per life cycle", cons_energy_per_uav.permission, cons_energy_per_uav.permission.stddev))

cat(paste("Energy demand per day [kWh]", sum(df$energyCharge) / repeats / (simtimeSec / 3600 / 24) / 1000 * 14.8 / 1000))

lifecycle_states <- c("Mission Execution", "Maintenance Flights", "Charge", "Idle")

ratios.time <- data.frame(
  class = "time",
  metric = as.factor(lifecycle_states),
  value = as.numeric(c(mean(df$secMission), mean(df$secMaintenance), mean(df$secCharge), mean(df$secIdle)))
)
ratios.time$metric2 <- factor(ratios.time$metric, rev(lifecycle_states))
ratios.time$percentage = round(100 * ratios.time$value/sum(ratios.time$value),digits=1)

ratios.energy <- data.frame(
  class = "energy",
  metric = as.factor(lifecycle_states),
  value = as.numeric(c(mean(df$energyMission), mean(df$energyMaintenance), mean(df$energyCharge), 0))
)
ratios.energy$metric2 <- factor(ratios.energy$metric, rev(lifecycle_states))
ratios.energy$percentage = round(100 * ratios.energy$value/sum(ratios.energy$value),digits=1)

ratios <- ratios.time
#ratios <- rbind(ratios.time, ratios.energy)


tikz(paste(tikzLocation, "8_life_cycle_time_ratio_plot_R.tex", sep = ""), standAlone=TRUE, timestamp = FALSE, width=5.9, height=1.3)

ggplot(ratios, aes(x=class, fill=metric2, y=percentage, label=percentage)) +
  geom_col(width = 0.5) +
  #geom_text(color=graphCol3_darkdark, size=rel(3), position=position_stack(vjust=0.5)) +
  geom_text(size=rel(3), position=position_stack(vjust=0.5)) +
  coord_flip() +
  scale_x_discrete(limits = (levels(metric))) +
  theme(axis.text.y=element_blank()) +
  #theme(legend.position="bottom", legend.title = element_blank(), legend.justification = "left") +
  theme(legend.title=element_blank()) +
  #scale_fill_brewer(name="Life Cycle State", guide=guide_legend(reverse=TRUE)) +
  scale_fill_manual(name="Life Cycle State", guide=guide_legend(reverse=TRUE),
                    values=c(graphCol3, graphCol2, graphCol2_dark, graphCol1)) + 
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  labs(x="Life Cycle State", y="Time Ratio [\\%]")

dev.off()


#####################################################################
# General Performance Data ##########################################
# for different replM and biWeights

df <- subset(df.red.uav, countMissions > 10)
#df <- df.red.uav

lifecycle_states <- c("Mission Execution", "Maintenance Flights", "Charge", "Idle")
replM_names <- c("LO", "SR", "BO")

ratios <- data.frame()
for (replMethod in replMs) {
  for (biw in biWeights) {
    df_subset <- subset(df, replM == replMethod & match(biWeight, biw))
    if (nrow(df_subset) == 0) next
    ratios.time <- data.frame(
      replM = replM_names[replMethod+1],
      biWeight = biw,
      class = "time",
      metric = as.factor(lifecycle_states),
      value = as.numeric(c(mean(df_subset$secMission),
                           mean(df_subset$secMaintenance),
                           mean(df_subset$secCharge),
                           mean(df_subset$secIdle)))
    )
    ratios.time$metric2 <- factor(ratios.time$metric, rev(lifecycle_states))
    ratios.time$percentage = round(100 * ratios.time$value/sum(ratios.time$value),digits=1)
    
    ratios.energy <- data.frame(
      replM = replM_names[replMethod+1],
      biWeight = biw,
      class = "energy",
      metric = as.factor(lifecycle_states),
      value = as.numeric(c(mean(df_subset$energyMission), mean(df_subset$energyMaintenance), mean(df_subset$energyCharge), 0))
    )
    ratios.energy$metric2 <- factor(ratios.energy$metric, rev(lifecycle_states))
    ratios.energy$percentage = round(100 * ratios.energy$value/sum(ratios.energy$value),digits=1)
    
    ratios <- rbind(ratios, ratios.time)
    ratios <- rbind(ratios, ratios.energy)
  }
}

# Only without more than one biWeight

ratios.red <- subset(ratios, class == "time")

tikz(paste(tikzLocation, "8_life_cycle_time_ratio_replMs_plot_R.tex", sep = ""), standAlone=TRUE, timestamp = FALSE, width=5.9, height=2.1)

ggplot(ratios.red, aes(x=replM, fill=metric2, y=percentage, label=percentage)) +
  geom_col(width = 0.7) +
  #geom_text(color=graphCol3_darkdark, size=rel(3), position=position_stack(vjust=0.5)) +
  geom_text(size=rel(3), position=position_stack(vjust=0.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(ratios.red$replM))) +
  theme(axis.title.y=element_blank()) +
  #theme(legend.position="bottom", legend.title = element_blank(), legend.justification = "left") +
  theme(legend.title=element_blank()) +
  #scale_fill_brewer(name="Life Cycle State", guide=guide_legend(reverse=TRUE)) +
  scale_fill_manual(name="Life Cycle State", guide=guide_legend(reverse=TRUE),
                    values=c(graphCol3, graphCol2, graphCol2_dark, graphCol1)) + 
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  labs(x="Life Cycle State", y="Time Ratio [\\%]")

dev.off()

# multiple biWeights

df.summarize <- data.frame()
for (replMethod in replMs) {
  for (biw in biWeights) {
    df_subset <- subset(df, replM == replMethod & match(biWeight,biw))
    if (nrow(df_subset) == 0) next
    simruns <- length(unique(df_subset$simRun))
    uav_count <- length(unique(df_subset$index))
    print(paste(replM_names[replMethod+1], biw, "Life Cyles", sum(df_subset$countMissions) / simruns))
    
    df.summarize <- rbind(df.summarize, data.frame(
      replM = replM_names[replMethod+1],
      biWeight = biw,
      lifeCycles = sum(df_subset$countMissions) / simruns,
      lifeCyclesPerUAV = mean(df_subset$countMissions),
      energyCharged = sum(df_subset$energyCharge) / simruns,
      energyEfficiency = 100 / (mean(df_subset$energyMission) + mean(df_subset$energyMaintenance)) * mean(df_subset$energyMission)
    ))
  }
}

plot1 <- ggplot(df.summarize, aes(x=biWeight, y=lifeCycles)) +
  geom_line(color=graphCol1) +
  geom_point(color=graphCol3_darkdark) +
  xlim(0,1) +
  scale_x_continuous(breaks=seq(0, 1, 0.2)) +
  theme(axis.title.x=element_blank()) +
  labs(x="Bi-Objective Weight $w$", y="Life Cycles")

plot2 <- ggplot(df.summarize, aes(x=biWeight, y=energyEfficiency)) +
  geom_line(color=graphCol1) +
  geom_point(color=graphCol3_darkdark) +
  xlim(0,1) +
  scale_x_continuous(breaks=seq(0, 1, 0.2)) +
  theme(axis.title.x=element_blank()) +
  labs(x="Bi-Objective Weight $w$", y="Energy Efficiency [\\%]")

tikz(paste(tikzLocation, "8_biWeight_sweep_plot_R.tex", sep = ""), standAlone=TRUE, timestamp = FALSE, width=5.9, height=2.2)
plot_grid(plot1, plot2)
dev.off()


#####################################################################
# General Performance Data ##########################################
# for different replSearchM

df <- subset(df.red.uav, countMissions > 10)
#df <- df.red.uav

lifecycle_states <- c("Mission Execution", "Maintenance Flights", "Charge", "Idle")
replSearchM_names <- c("SP", "AC")

ratios <- data.frame()
for (replSearchMethod in replSearchMs) {
  for (numuavs in numUAVss) {
    df_subset <- subset(df, replSearchM == replSearchMethod & match(numUAVs, numuavs))
    if (nrow(df_subset) == 0) next
    ratios.time <- data.frame(
      replSearchM = replSearchM_names[as.integer(replSearchMethod) + 1],
      numUAVs = numuavs,
      class = "time",
      metric = as.factor(lifecycle_states),
      value = as.numeric(c(mean(df_subset$secMission),
                           mean(df_subset$secMaintenance),
                           mean(df_subset$secCharge),
                           mean(df_subset$secIdle)))
    )
    ratios.time$metric2 <- factor(ratios.time$metric, rev(lifecycle_states))
    ratios.time$percentage = round(100 * ratios.time$value/sum(ratios.time$value),digits=1)
    
    ratios.energy <- data.frame(
      replSearchM = replSearchM_names[as.integer(replSearchMethod) + 1],
      numUAVs = numuavs,
      class = "energy",
      metric = as.factor(lifecycle_states),
      value = as.numeric(c(mean(df_subset$energyMission), mean(df_subset$energyMaintenance), mean(df_subset$energyCharge), 0))
    )
    ratios.energy$metric2 <- factor(ratios.energy$metric, rev(lifecycle_states))
    ratios.energy$percentage = round(100 * ratios.energy$value/sum(ratios.energy$value),digits=1)
    
    ratios <- rbind(ratios, ratios.time)
    ratios <- rbind(ratios, ratios.energy)
  }
}

ratios.efficiency <- subset(ratios, class=="energy" & metric %in% c("Mission Execution", "Maintenance Flights"))
energy.sp.mission <- subset(ratios.efficiency, replSearchM=="SP" & metric=="Mission Execution")$value
energy.sp.maint <- subset(ratios.efficiency, replSearchM=="SP" & metric=="Maintenance Flights")$value
energy.ac.mission <- subset(ratios.efficiency, replSearchM=="AC" & metric=="Mission Execution")$value
energy.ac.maint <- subset(ratios.efficiency, replSearchM=="AC" & metric=="Maintenance Flights")$value
cat(paste("Energy Efficiency SP:", 100 / (energy.sp.mission + energy.sp.maint) * energy.sp.mission))
cat(paste("Energy Efficiency AC:", 100 / (energy.ac.mission + energy.ac.maint) * energy.ac.mission))


for (replSearchMethod in replSearchMs) {
  df_part <- subset(df.red.uav, countMissions > 10 & replSearchM == replSearchMethod)
  replSearchM_name <- replSearchM_names[as.integer(replSearchMethod) + 1]
  cons_energy_per_uav.overall <- mean(df_part$energyMission + df_part$energyMaintenance)
  cons_energy_per_uav.permission <- mean((df_part$energyMission + df_part$energyMaintenance)) / mean(df_part$countMissions)
  
  print(paste(replSearchM_name, "Energy consumption per UAV", cons_energy_per_uav.overall))
  print(paste(replSearchM_name, "Energy consumption per life cycle", cons_energy_per_uav.permission))
  print(paste(replSearchM_name, "Life cyles per UAV", mean(df_part$countMissions)))
  print(paste(replSearchM_name, "Replacements per hour", sum(df_part$countMissions) / length(unique(df_part$simRun)) / (simtimeSec / 3600)))
  print(paste(replSearchM_name, "Energy demand per day [kWh]", sum(df_part$energyCharge) / length(unique(df_part$simRun)) / (simtimeSec / 3600 / 24) / 1000 * 14.8 / 1000))
}

#ratios.red <- subset(ratios, class == "energy")
ratios.red <- subset(ratios, class == "time")

tikz(paste(tikzLocation, "8_life_cycle_time_ratio_replSearchMs_plot_R.tex", sep = ""), standAlone=TRUE, timestamp = FALSE, width=5.9, height=1.6)

ggplot(ratios.red, aes(x=replSearchM, fill=metric2, y=percentage, label=percentage)) +
  geom_col(width = 0.7) +
  #geom_text(color=graphCol3_darkdark, size=rel(3), position=position_stack(vjust=0.5)) +
  geom_text(size=rel(3), position=position_stack(vjust=0.5)) +
  coord_flip() +
  #scale_x_discrete(limits = (levels(metric))) +
  scale_x_discrete(limits = rev(levels(ratios.red$replSearchM))) +
  theme(axis.title.y=element_blank()) +
  #theme(legend.position="bottom", legend.title = element_blank(), legend.justification = "left") +
  theme(legend.title=element_blank()) +
  #scale_fill_brewer(name="Life Cycle State", guide=guide_legend(reverse=TRUE)) +
  scale_fill_manual(name="Life Cycle State", guide=guide_legend(reverse=TRUE),
                    values=c(graphCol3, graphCol2, graphCol2_dark, graphCol1)) + 
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  labs(x="Life Cycle State", y="Time Ratio [\\%]")

dev.off()


#####################################################################
# General Performance Data ##########################################
# numUAV simulation time cutoff

df.sr_t <- read.table(header = TRUE, text = 'numUAVs simRun duration
  20 0 757.196678978058
  20 1 686.421261223386
  20 10 767.196678978058
  20 11 757.196678978058
  20 2 706.421261223386
  20 3 706.421261223386
  20 4 706.421261223386
  20 5 609.913142546607
  20 6 737.196678978058
  20 7 872.607969562364
  20 8 820.932124754074
  20 9 820.932124754074
  25 0 1100.434602202604
  25 1 1122.607969562364
  25 10 1144.784774373238
  25 11 1120.434602202604
  25 2 1202.607969562364
  25 3 1164.784774373238
  25 4 1122.607969562364
  25 5 969.914142546608
  25 6 978.338035133632
  25 7 1292.608969562365
  25 8 1127.199678978058
  25 9 978.338035133632
  30 0 1764.82941927123
  30 1 1722.607969562364
  30 10 1782.608969562364
  30 11 1762.242344284605
  30 2 1764.784774373238
  30 3 1780.380218923241
  30 4 1427.197678978058
  30 5 1180.434602202604
  30 6 1679.635990036552
  30 7 1604.82941927123
  30 8 1712.609969562365
  30 9 1532.608969562364
  35 0 2168.681289567575
  35 1 1897.213042672992
  35 10 2010.596393664315
  35 11 2003.816953854632
  35 2 1894.82941927123
  35 3 2176.504484756701
  35 4 1902.607969562364
  35 5 1897.213042672992
  35 6 1994.688132597459
  35 7 2000.380218923241
  35 8 1822.607969562364
  35 9 1897.213042672992
  40 0 2854.630405827726
  40 1 2537.623872801739
  40 10 2206.887783436811
  40 11 2803.638687939408
  40 2 2437.361028975251
  40 3 2864.630405827726
  40 4 2818.435249924588
  40 5 2522.609969562364
  40 6 3062.252486104411
  40 7 3049.1343002674
  40 8 2922.806926256284
  40 9 2537.668517699731
  45 0 3372.714469629248
  45 1 3312.510412613987
  45 10 3352.611969562366
  45 11 3920.599393664315
  45 2 3305.449067990865
  45 3 3175.141579266385
  45 4 3629.1353002674
  45 5 2976.262929965246
  45 6 3747.216042672992
  45 7 3569.1353002674
  45 8 3303.259052152064
  45 9 3537.714529797965
  50 0 4653.667848351715
  50 1 4980.43498249509
  50 10 5204.860201189732
  50 11 5123.395130249635
  50 2 5586.089035859303
  50 3 4537.214587945227
  50 4 5644.323767838843
  50 5 5062.431479770736
  50 6 5326.385547298286
  50 7 5351.52813714817
  50 8 5201.850225454451
  50 9 5167.366028975251
  53 0 8578.334553098028
  53 1 8514.07773601046
  53 10 7474.694952871611
  53 11 7910.428413545729
  53 2 9103.696758117189
  53 3 8608.447637744597
  53 4 7818.900458916851
  53 5 7852.446115934147
  53 6 9471.385691396369
  53 7 8623.267306213089
  53 8 8510.975530879134
  53 9 10096.386755213517
  55 0 259200
  55 1 12214.761112164032
  55 10 14961.996276695687
  55 11 16947.089895691704
  55 2 11968.102906864894
  55 3 135976.456809268316
  55 4 23955.077221052637
  55 5 259200
  55 6 259200
  55 7 23618.338515012631
  55 8 106044.403908291859
  55 9 23727.97068337664
  58 0 259200
  58 1 259200
  58 10 259200
  58 11 259200
  58 2 259200
  58 3 259200
  58 5 259200
  58 6 259200
  58 7 259200
  58 8 259200
  58 9 259200
  60 0 259200
  60 1 259200
  60 10 259200
  60 11 259200
  60 2 259200
  60 3 259200
  60 4 259200
  60 5 259200
  60 6 259200
  60 7 259200
  60 8 259200
  60 9 259200
  65 0 259200
  65 1 259200
  65 10 259200
  65 11 259200
  65 2 259200
  65 4 259200
  65 5 259200
  65 6 259200
  65 7 259200
  65 8 259200
  65 9 259200
  70 0 259200
  70 1 259200
  70 10 259200
  70 11 259200
  70 2 259200
  70 3 259200
  70 4 259200
  70 5 259200
  70 6 259200
  70 7 259200
  70 8 259200
  70 9 259200
  75 0 259200
  75 1 259200
  75 10 259200
  75 11 259200
  75 2 259200
  75 3 259200
  75 4 259200
  75 6 259200
  75 7 259200
  75 8 259200
  75 9 259200
  80 0 259200
  80 1 259200
  80 10 259200
  80 11 259200
  80 2 259200
  80 3 259200
  80 4 259200
  80 5 259200
  80 6 259200
  80 7 259200
  80 8 259200
  80 9 259200
  85 0 259200
  85 1 259200
  85 10 259200
  85 11 259200
  85 2 259200
  85 3 259200
  85 4 259200
  85 5 259200
  85 6 259200
  85 7 259200
  85 8 259200
  85 9 259200
  90 0 259200
  90 1 259200
  90 10 259200
  90 11 259200
  90 2 259200
  90 3 259200
  90 4 259200
  90 5 259200
  90 6 259200
  90 7 259200
  90 8 259200
  90 9 259200
  95 0 259200
  95 1 259200
  95 10 259200
  95 2 259200
  95 3 259200
  95 4 259200
  95 5 259200
  95 6 259200
  95 7 259200
  95 8 259200
  95 9 259200
  100 0 259200
  100 1 259200
  100 10 259200
  100 11 259200
  100 2 259200
  100 3 259200
  100 4 259200
  100 5 259200
  100 6 259200
  100 7 259200
  100 8 259200
  100 9 259200
  150 0 259200
  150 1 259200
  150 11 259200
  150 2 259200
  150 3 259200
  150 4 259200
  150 5 259200
  150 6 259200
  150 7 259200
  150 8 259200
  150 9 259200
  200 1 259200
  200 10 259200
  200 11 259200
  200 2 259200
  200 3 259200
  200 4 259200
  200 5 259200
  200 6 259200
  200 7 259200
  200 8 259200
  200 9 259200
  400 0 259200
  400 1 259200
  400 10 259200
  400 11 259200
  400 2 259200
  400 3 259200
  400 4 259200
  400 5 259200
  400 6 259200
  400 7 259200
  400 8 259200
  400 9 259200
')
df.sr_t$duration2 <- ifelse(df.sr_t$numUAVs <= 57, df.sr_t$duration, NA)

df.sr_t.long <- read.table(header = TRUE, text = 'numUAVs simRun duration
  55 0 19730.454951961332
  55 1 125780.256641793666
  55 10 16175.15311631211
  55 11 14689.527549561394
  55 2 27213.129439727314
  55 3 13932.987959550739
  55 4 72880.315625093948
  55 5 13690.586931179329
  55 6 11483.581349291625
  55 7 18676.813609118913
  55 8 490816.680093581083
  55 9 33543.917212916884
  56 0 1209600
  56 1 1209600
  56 2 1209600
  56 3 1209600
  56 4 1209600
  56 5 1209600
  57 0 1209600
  57 1 1209600
  57 2 1209600
  57 3 1209600
  57 4 1209600
  57 5 1209600
')

plot1 <- ggplot(df.sr_t, aes(x=numUAVs, y=as.numeric(duration/3600))) +
  geom_vline(xintercept = 56, size=2*1.5, linetype = "solid", color = alpha(graphCol3, 0.3)) +
  geom_point(color=graphCol1) +
  #annotate("text", x = 58, y = 0.3, label = "$z_p = 15.0\\,\\mathrm{Wh} = \\mu$", color=graphCol3_dark, size = rel(2)) +
  scale_x_log10(breaks=c(seq(0, 80, 10), seq(100, 500, 100))) +
  scale_y_continuous(trans="log2", breaks=c(1, 6, 12, 24, 48, 72)) +
  #theme(axis.title.x = element_blank()) +
  labs(x="Available UAVs", y="Service Duration [h]")

plot2 <- ggplot(df.sr_t.long, aes(x=numUAVs, y=as.numeric(duration/3600))) +
  geom_point(color=graphCol1) +
  #annotate("text", x = 58, y = 0.3, label = "$z_p = 15.0\\,\\mathrm{Wh} = \\mu$", color=graphCol3_dark, size = rel(2)) +
  scale_x_continuous(breaks=c(55,56,57)) +
  scale_y_continuous(trans="log2", breaks=c(1, 6, 12, 24, 48, 72,168, 336)) +
  theme(axis.title.y = element_blank()) +
  labs(x=" ", y="Service Duration [h] (extended cutoff)")

tikz(paste(tikzLocation, "8_numUAVs_duration_plot_R.tex", sep = ""), standAlone=TRUE, timestamp = FALSE, width=5.9, height=2.6)
plot_grid(plot1, plot2, rel_widths = c(5, 1))
dev.off()



#####################################################################
# General Performance Data ##########################################
# for different numUAVs

df <- subset(df.red.uav, countMissions > 10)
#df <- df.red.uav

lifecycle_states <- c("Mission Execution", "Maintenance Flights", "Charge", "Idle")

ratios <- data.frame()
for (numuavs in numUAVss) {
  df_subset <- subset(df, numUAVs == numuavs)
  if (nrow(df_subset) == 0) next
  ratios.time <- data.frame(
    numUAVs = numuavs,
    class = "time",
    metric = as.factor(lifecycle_states),
    value = as.numeric(c(mean(df_subset$secMission),
                         mean(df_subset$secMaintenance),
                         mean(df_subset$secCharge),
                         mean(df_subset$secIdle)))
  )
  ratios.time$metric2 <- factor(ratios.time$metric, rev(lifecycle_states))
  ratios.time$percentage = round(100 * ratios.time$value/sum(ratios.time$value),digits=1)
  
  ratios.energy <- data.frame(
    numUAVs = numuavs,
    class = "energy",
    metric = as.factor(lifecycle_states),
    value = as.numeric(c(mean(df_subset$energyMission),
                         mean(df_subset$energyMaintenance),
                         mean(df_subset$energyCharge),
                         0))
  )
  ratios.energy$metric2 <- factor(ratios.energy$metric, rev(lifecycle_states))
  ratios.energy$percentage = round(100 * ratios.energy$value/sum(ratios.energy$value),digits=1)
  
  run_mean.sum.energyMission <- 0
  run_mean.sum.energyMaintenance <- 0
  run_mean.sum.energyCharge <- 0
  runs <- length(unique(df_subset$simRun))
  for (run in unique(df_subset$simRun)) {
    df_subset_run <- subset(df_subset, simRun == run)
    run_mean.sum.energyMission <- run_mean.sum.energyMission + 1/runs * sum(df_subset_run$energyMission)
    run_mean.sum.energyMaintenance <- run_mean.sum.energyMaintenance + 1/runs * sum(df_subset_run$energyMaintenance)
    run_mean.sum.energyCharge <- run_mean.sum.energyCharge + 1/runs * sum(df_subset_run$energyCharge)
  }
  
  ratios.energy <- rbind(ratios.energy, data.frame(
    numUAVs = numuavs,
    class = "stat",
    metric = as.factor("Efficiency"),
    value = 0,
    metric2 = "Efficiency",
    #percentage = as.numeric(100 / (mean(df_subset$energyMission) + mean(df_subset$energyMaintenance)) * mean(df_subset$energyMission))
    percentage = as.numeric(100 / (run_mean.sum.energyMission + run_mean.sum.energyMaintenance) * run_mean.sum.energyMission)
  ))
  ratios.energy <- rbind(ratios.energy, data.frame(
    numUAVs = numuavs,
    class = "stat",
    metric = as.factor("EnergyPerDay_kwh"),
    value = run_mean.sum.energyCharge / (simtimeSec / 3600 / 24) / 1000 * 14.8 / 1000,
    metric2 = "EnergyPerDay_kwh",
    percentage = 0
  ))
  
  ratios <- rbind(ratios, ratios.time)
  ratios <- rbind(ratios, ratios.energy)
}
ratios[ratios$numUAVs == 80 &ratios$ metric == "EnergyPerDay_kwh",]$value <- 82.4234
ratios[ratios$numUAVs == 90 &ratios$ metric == "EnergyPerDay_kwh",]$value <- 82.4827

ratios.red <- subset(ratios, class == "time" & metric == "Idle")
ggplot(ratios.red, aes(x=numUAVs, y=value)) +
  geom_point(color=graphCol1) +
  #annotate("text", x = 58, y = 0.3, label = "$z_p = 15.0\\,\\mathrm{Wh} = \\mu$", color=graphCol3_dark, size = rel(2)) +
  scale_x_log10(breaks=c(seq(0, 90, 10), seq(100, 500, 100))) +
  scale_y_continuous(trans="log10") +
  labs(x="Available UAVs", y="Service Duration [h]")

ratios.red <- subset(ratios, class == "stat" & metric == "Efficiency")
ggplot(ratios.red, aes(x=numUAVs, y=percentage)) +
  geom_point(color=graphCol1) +
  #annotate("text", x = 58, y = 0.3, label = "$z_p = 15.0\\,\\mathrm{Wh} = \\mu$", color=graphCol3_dark, size = rel(2)) +
  scale_x_log10(breaks=c(seq(0, 90, 10), seq(100, 500, 100))) +
  scale_y_continuous() +
  labs(x="Available UAVs", y="Energy Efficiency [\\%]")


tikz(paste(tikzLocation, "8_numUAVs_energydemand_plot_R.tex", sep = ""), standAlone=TRUE, timestamp = FALSE, width=5.9, height=2.0)

ratios.red <- subset(ratios, class == "stat" & metric == "EnergyPerDay_kwh")
ggplot(ratios.red, aes(x=numUAVs, y=value)) +
  geom_line(color=graphCol3) +
  geom_point(color=graphCol1) +
  #annotate("text", x = 58, y = 0.3, label = "$z_p = 15.0\\,\\mathrm{Wh} = \\mu$", color=graphCol3_dark, size = rel(2)) +
  scale_x_log10(breaks=c(seq(0, 90, 10), seq(100, 500, 100))) +
  scale_y_continuous() +
  labs(x="Available UAVs", y="Energy Demand [kW\\,h] / Day")


dev.off()


#####################################################################
# premature battery depletion during maintenance flights

count_missions <- sum(df.red.uav$countMissions)
count_overdrawn_maint <- sum(df.red.uav$countOverdrawnAfterMission)
ratio_overdrawn_maint <- 100 / count_missions * count_overdrawn_maint
energy_overdrawn_mission <- sum(df.red.uav$energyOverdrawMission)

cat("Premature Battery depletion cases")
cat(paste("Count & \\num{", count_missions, "} & \\num{", count_overdrawn_maint, "} & \\num{", energy_overdrawn_mission, "} \\\\", sep = ""))
cat(paste("Ratio & \\SI{100}{\\percent} & \\SI{", round(ratio_overdrawn_maint,digits = 2), "}{\\percent} & \\SI{0}{\\percent} \\\\", sep = ""))

#####################################################################
# Random Diagram Testing ############################################

# Test correlation with starting position (CS 0,1, or 2)
locations <- c('CS0 Tennis court Ritzebuehl', 'CS1 Sports field Manebach', 'CS2 Hotel Gabelbach')
starting_cs <- df.red.uav$index %% 3
starting_cs <- as.factor(locations[starting_cs + 1])


ggplot(df.red.uav, aes(starting_cs, countManeuversMission)) + geom_boxplot() + geom_jitter(width = 0.2)
ggplot(df.red.uav, aes(starting_cs, countIdleState)) + geom_boxplot() + geom_jitter(width = 0.2)
ggplot(df.red.uav, aes(starting_cs, secMission)) + geom_boxplot() + geom_jitter(width = 0.2)
ggplot(df.red.uav, aes(starting_cs, secIdle)) + geom_boxplot() + geom_jitter(width = 0.1)
ggplot(df.red.uav, aes(starting_cs, secMission + secMaintenance + secCharge)) + geom_boxplot() + geom_jitter(width = 0.1)

ggplot(df.red.uav, aes(starting_cs, 100 / (secMission + secMaintenance) * secMission)) + geom_boxplot() + geom_jitter(width = 0.1)
ggplot(df.red.uav, aes(starting_cs, 100 / (secMission + secMaintenance + secCharge) * secMission)) + geom_boxplot() + geom_jitter(width = 0.1)
ggplot(df.red.uav, aes(starting_cs, 100 / (secMission + secMaintenance + secCharge + secIdle) * secMission)) + geom_boxplot() + geom_jitter(width = 0.1)



# Statistics about CS
ggplot(df.red.cs, aes(replM, chargedMobileNodesAll)) + geom_boxplot() + geom_jitter(width = 0.1)
ggplot(df.red.cs, aes(index, chargedMobileNodes)) + geom_boxplot() + geom_jitter(width = 0.1)

ggplot(df.red.cs, aes(replM, chargedMobileNodesAll)) + geom_point()
ggplot(subset(df.red.cs, index==0 | index==1 | index==2), aes(replM, chargedPower)) + geom_point()


# Diagrams
ggplot(df.red.uav) +
  geom_bin2d(aes(x = 100 / (secMission + secMaintenance) * secMission, y = 100 / (secMission + secMaintenance) * secMaintenance)) +
  labs(x="Time in Mission", y="Time in Maintenance Flights")

ggplot(df.red.uav) +
  geom_bin2d(aes(x = 100 / (energyMission + energyMaintenance) * energyMission, y = 100 / (energyMission + energyMaintenance) * energyMaintenance)) +
  labs(x="Energy in Mission", y="Energy in Maintenance Flights")


ggplot(subset(df.red.uav, replM==0)) + geom_bin2d(aes(x = energyMission, y = energyMaintenance))
ggplot(subset(df.red.uav, replM==1)) + geom_bin2d(aes(x = energyMission, y = energyMaintenance))
ggplot(subset(df.red.uav, replM==2)) + geom_bin2d(aes(x = energyMission, y = energyMaintenance))

ggplot(subset(df.red.uav, replM==0)) + geom_bin2d(aes(x = 100 / (energyMission + energyMaintenance) * energyMission, y = 100 / (energyMission + energyMaintenance) * energyMaintenance)) + ylim(0, 100) + xlim(0, 100)
ggplot(subset(df.red.uav, replM==1)) + geom_bin2d(aes(x = 100 / (energyMission + energyMaintenance) * energyMission, y = 100 / (energyMission + energyMaintenance) * energyMaintenance)) + ylim(0, 100) + xlim(0, 100)
ggplot(subset(df.red.uav, replM==2)) + geom_bin2d(aes(x = 100 / (energyMission + energyMaintenance) * energyMission, y = 100 / (energyMission + energyMaintenance) * energyMaintenance)) + ylim(0, 100) + xlim(0, 100)



#####################################################################
#####################################################################
cat("F I N I S H E D")

