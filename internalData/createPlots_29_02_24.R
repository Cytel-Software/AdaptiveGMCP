# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

# inputDF <- read.csv("C:\\Users\\Anoop.Rawat\\Downloads\\simMAMSMEP_Wrapper_InputData.csv")
library(AdaptGMCP)
library(tidyverse)
############ ------------- workflow starts here when simulation output is given ----------------
# Read the simulation output
outDF <- read.csv("outDF_2024-02-22_20-23-33.csv", fileEncoding = "UTF-8-BOM")
# Read the template table
tableTemplate <- read.csv("internalData/TableTemplete_29_02_24.csv",fileEncoding = "UTF-8-BOM")

# Read the processed table
#Specify the target power
#Note: Must be same name as in the outDF
PowerType <- 'Disjunctive.Power'

#generate processed table
processedTables <- GeneratePowerTablePlots(strPowerType = PowerType, dfOutput = outDF, dfTableTemplate = tableTemplate)


########################## Bar charts for the powers ###########################
################### Specify the y-axis(power difference) ticks #################
Power_Ticks <- seq(0,1, 0.2)

p <- ggplot(data = processedTables$TableLong, aes(x = `Treatment Selection Rule`, y = value, fill = MAMS)) +
  geom_bar(stat = 'identity', position = "dodge") +
  labs(# title = "Side-by-Side Bar Chart",
       x = "Treatment Selection Rule",
       y = PowerType) +
  # scale_fill_viridis_b()
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
  ylim(Power_Ticks[1], Power_Ticks[length(Power_Ticks)])+
  scale_y_continuous(breaks=Power_Ticks)+
  # Adjust size parameter to change x axis label size
  facet_wrap(vars(Scenario))
p


###################### Bar charts for the difference in powers #######################
##################### Specify the y-axis(power difference) ticks #####################

Power_Diff_Ticks <- seq(0,0.2, 0.02)

pDiff <- ggplot(data = processedTables$TableWide %>% filter(!is.na(Difference)), aes(x = Scenario, y = Difference)) +
  geom_bar(stat = 'identity', position = "dodge",fill = "#56B4E9") +
  scale_fill_brewer() +
  labs(# title = "Side-by-Side Bar Chart",
    x = "Scenario",
    y = "Power Gain CER over PVcombo") +
  theme(legend.position = 'bottom') +
  geom_text(aes(label = round(Difference, 2), y = Difference), vjust = -0.5, size = 3) +  # Add text labels
  ylim(Power_Diff_Ticks[1], Power_Diff_Ticks[length(Power_Diff_Ticks)])+
  scale_y_continuous(breaks=Power_Diff_Ticks)+
  facet_wrap(vars(`Treatment Selection Rule`))

pDiff

# Save the graph as a PNG file
ggsave("Plots\\CumulVsStagewise.png", p, width = 10, height = 6, units = "in")
ggsave("Plots\\PowerGain.png", pDiff, width = 10, height = 6, units = "in")


######### --- individual plot cum vs stagewise ----
indPlot <- ggplot(data = processedTables$TableLong %>% filter(Scenario == 'S1'), aes(x = `Treatment Selection Rule`, y = value, fill = MAMS)) +
  geom_bar(stat = 'identity', position = "dodge") +
  labs(# title = "Side-by-Side Bar Chart",
    x = "Treatment Selection Rule",
    y = PowerType) +
  # scale_fill_viridis_b()
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
indPlot
ggsave("CumulVsStagewiseIndPlot.png", indPlot, width = 10, height = 6, units = "in")

############ ---- power gain -----------
PowerGainIndPlot <- ggplot(data = processedTables$TableWide %>%
                             filter(!is.na(Difference)) %>%
                             filter(`Treatment Selection Rule` == 'Conservative'), aes(x = Scenario, y = Difference)) +
  geom_bar(stat = 'identity', position = "dodge",fill = "#56B4E9") +
  scale_fill_brewer() +
  labs(# title = "Side-by-Side Bar Chart",
    x = "Scenario",
    y = "Power Gain CER over PVcombo") +
  theme(legend.position = 'bottom') +
  geom_text(aes(label = round(Difference, 2), y = Difference), vjust = -0.5, size = 3) +  # Add text labels
  ylim(Power_Diff_Ticks[1], Power_Diff_Ticks[length(Power_Diff_Ticks)])+
  scale_y_continuous(breaks=Power_Diff_Ticks)

PowerGainIndPlot
ggsave("PowerGainIndPlot.png", PowerGainIndPlot, width = 10, height = 6, units = "in")

