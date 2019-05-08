### Setup ###
#############
setwd("~/Documents/DW/076_078_ERDF")
#Load libraries
options(scipen=999)
library(tidyverse)
library('ggbeeswarm')
library("treemapify")
source("../000_Templates/chart_template.R")

#### Read data ####
###################

#Read financial implementation data
implement = read.csv("data/eu/ESIF_2014-2020_Finance_Implementation_Details_190411.csv", stringsAsFactors = F, na.strings = "") %>%
      filter(Fund == "ERDF", year == max(year)) %>% select(1:5,7:11,16:22)
#necessary columns: "ms", "MS_name", "CCI", "Programme.Title", "Ver", "Priority", "TO", "TO_short", "TO.Long", "Category_of_region", "EU_amount_planned", "National_Amount_planned", "Total_Amount_planned", "year", "EU_co_financing", "total_eligible_cost_decided_.selected.", "total_eligible_spending"
#join with NUTS Codes
tmp = read.csv("data/eu/ERDF_2014-2020_regions_match.csv", sep=";", stringsAsFactors = F, na.strings = "")
implement = left_join(implement, tmp, by = c("ms" = "MS", "MS_name" = "MS.Name", "CCI")); rm(tmp)

#Read planned finances data for better documentation of topics
planned = read.csv("data/eu/ESIF_2014-2020_FINANCES_PLANNED_DETAILS_190411.csv", stringsAsFactors = F, na.strings = "") %>%
  filter(Fund == "ERDF")

### Check validity ###
######################

implement$CCI %>% unique %>% length() #292

#check sums to find out what "total_eligible_xxx" columns mean
implement %>% summarize(total = sum(Total_Amount_planned), decided = sum(total_eligible_cost_decided_.selected.), spent = sum(total_eligible_spending))
#total_eligible_cost_decided_.selected. =   "decided on"  =   allocated to projects
#total_eligible_spending                =   "spent"       =   paid out to member states by the EU

implement$total_eligible_cost_decided_.selected.[implement$ms == "PL"] %>% sum(.)/10^9
#38.8 bn Euros already decided on


#### How much is planned vs. decided by country? ####
#####################################################

#### Share of EU money already decided on (Marimekko) ###

#Table: country, amount allocated to projects (incl. national), EU amount allocated (estimate), EU amount planned, share allocated, share not yet allocated
amount_state = implement %>% group_by(MS_name) %>%
  summarise(decided = sum(total_eligible_cost_decided_.selected.),
            decided.EU = decided * (sum(EU_amount_planned)/sum(Total_Amount_planned)),
            planned = sum(EU_amount_planned)) %>% arrange(-planned) %>%
  mutate(Decided = round(decided.EU/planned,3), `Not yet decided` = 1 - Decided, MS_name = factor(MS_name, levels = MS_name))
#average:
sum(amount_state$decided.EU) / sum(amount_state$planned)
#~75% of the money is already allocated to projects on average

#for chart: only top 10 countries, make factor levels for plotting
tmp = amount_state %>% arrange(-Decided) %>%
      filter(MS_name != "Interreg") %>% 
      mutate(MS_name = factor(MS_name, levels = MS_name)) %>% arrange(-planned) %>%
      slice(1:10) %>% gather(type, share, 5:6) %>% mutate(type = factor(type, levels = c("Not yet decided", "Decided")))

#Marimekko chart: Shares of EU money allocated to projects vs total EU amount planned
ggplot(tmp, aes(x = MS_name, y = share, width = planned, fill = type)) +
      geom_bar(stat = "identity", position = "fill") +                                                            #Marimekko bars
      geom_text(aes(y = share+0.005, label = ifelse(type == "Decided", paste0(round(share*100),"%"),"")), hjust = 0, size = 15) +     #Labels for percent paid
      geom_text(aes(y = 1.01, label = round(planned/1000000000,1)),  hjust=1.3, size = 15) +                                   #Labels for total planned amount
      facet_grid(MS_name~., scales = "free_y", space = "free_y", switch = "y") +                                  #Facet for Country labels and distance between bars
      scale_y_continuous(labels = scales::percent, limits = c(0,1.1), expand = c(0,0)) +                          #Make room for planned amount labels
      scale_fill_manual(values = c("#cbd2d8","#00a5ff")) +                                                        #Set colors
      coord_flip() + theme_void() + guides(fill=FALSE) +                                                          #Flip chart, remove junk, remove legend
      theme_dw() +
      theme(panel.spacing.y = unit(0.005, "npc"),                                                                 #Theme settings
            axis.text = element_text(angle=0), axis.text.y = element_blank(), axis.text.x = element_blank(),
            strip.text.y = element_text(angle = 180, hjust = 0))# +
#Save chart
ggsave("plots/original/ERDF_money_paid_share_marimekko.svg", device = "svg", scale = 10, width= 70, height= 80, units="mm")


#### How much goes toward which category of region? ###
#######################################################

### Table: State name, region, category of region, planned amount
amount_region = implement %>% filter(!is.na(Category_of_region)) %>% 
  group_by(MS_name, Region.Name, Category_of_region) %>%
  summarise(planned = sum(EU_amount_planned))

#Amount by category: sum, median and share of money by category in bn euros
amount_region %>% group_by(Category_of_region) %>% mutate(planned = planned/1000000000) %>% 
      summarise(sum = sum(planned), median = median(planned)) %>% mutate(share = round(sum/sum(sum),2)) %>% arrange(-sum)
#Less developed regionen get 65% of funds, get more absolute funding in median and sum

# For chart: Filter "Outermost or Northern Sparsely Populated" and "VOID", filter missing regions ####
tmp = amount_region %>% filter(!(Category_of_region %in% c("VOID","Outermost or Northern Sparsely Populated")), !is.na(Region.Name))
# Categorical Scatterplot: 1 point per region
ggplot(tmp, aes(x = Category_of_region, y = planned/1000000000)) + theme_dw() +
      geom_quasirandom(shape = 21, fill=dw_info[1], color = "#f1f3f5",
                       size = 15, stroke = 1, bandwidth = 0.05, groupOnX = T)
ggsave("plots/original/ERDF_money_region_category_scatter.svg", device = "svg", width= 19.2, height= 22, dpi = 100, units="in")

#### Funding by topic ####
##########################

# Topic by region category
amount_topic_region = planned %>% group_by(Category.of.region, To.short) %>% summarize(amount = sum(EU.Amount)) %>% mutate(share = amount/sum(amount))

#Check guidelines:
#In more developed regions, max. 80% of funds may be concentrated on 1 topic. Transition regions: max. 60%, less developed regiond: max. 50%
amount_topic_region %>% group_by(Category.of.region) %>% summarise(max.share = max(share), sum.share = sum(share)) #Passt.
#more developed regions should dedicate at least 20% of funds to low-carbon economy, Transition regions: 15%, less developed regiond: 12%
amount_topic_region %>% filter(To.short == "Low-Carbon Economy")
#yup!

#Topic distributon by category
tmp = amount_topic_region %>% filter(!(Category.of.region %in% c("VOID", "Outermost or Northern Sparsely Populated")))
tmp$To.short = factor(tmp$To.short, levels = rev(unique((tmp %>% arrange(-share))$To.short)))
ggplot(tmp, aes(y = share, x = To.short)) + geom_col() + facet_grid(. ~ Category.of.region) + coord_flip()
#Topic distributon for Poland
tmp = planned %>% group_by(To.short) %>% filter(MS.Name == "Poland") %>%
  summarize(amount = sum(EU.Amount)) %>% mutate(share = amount/sum(amount))
ggplot(tmp, aes(x = To.short, y = share)) + geom_col() + coord_flip()


### Overall topic distribution ####
amount_topic = planned %>% group_by(To.short) %>%
  summarize(amount = sum(EU.Amount)) %>% mutate(share = amount/sum(amount)) %>% arrange(-share)

#Tree Map bauen
ggplot(amount_topic %>% filter(To.short != "Multiple Thematic Objectives (ERDF/CF/ESF)"),
       aes(area = amount, label = To.short)) +
  geom_treemap(layout = "squarified", start = "topleft", fill = dw_info[1], size = 15, colour = "white") + theme_dw() +
  geom_treemap_text(start = "topleft", colour = "white", family = "Noto Sans", reflow = T, grow = F,
                    padding.x = unit(25, "points"), padding.y = unit(25, "points"), size = 70)
ggsave("plots/original/ERDF_topic_tree.svg", device = "svg", width= 19.2, height= 19.2, dpi = 100, units="in")


rm(tmp)
save.image("erdf_analysis_data.RData")

#### How does the budget relate to EU membership contributions ####
###################################################################
load("erdf_analysis_data.RData")
contr = read.csv("analysis/EU_MS_contributions.csv")[,c(1,7)] %>% rename(Budget.sum = total.1420) %>% 
      mutate(Budget.sum = Budget.sum*10^6, Budget.share = Budget.sum/sum(Budget.sum), MS = gsub("EL","GR", MS, fixed=T))
contr = implement %>% group_by(ms, MS_name) %>% summarize(ERDF.sum = sum(EU_amount_planned)) %>% ungroup %>%
  mutate(ERDF.share = ERDF.sum/sum(ERDF.sum)) %>% left_join(contr, by=c("ms"="MS")) %>% 
  mutate(diff.share = ERDF.share - Budget.share, diff.sum = ERDF.sum - (Budget.sum)) %>% 
  arrange(diff.share)
