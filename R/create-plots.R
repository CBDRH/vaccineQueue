#############################################
# Purpose: 
# Inputs:
# Outputs:
#############################################

########
# Setup
########

# Load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)

# Load scenario data
load('Data/queueTimesRPA.rda')
load('Data/utilTimesRPA.rda')
load('Data/queueTimesGP.rda')
load('Data/utilTimesGP.rda')

# Apply a factor label to the staffing size variable
queueTimesRPA$sizeF <- factor(queueTimesRPA$size, labels = c('21 staff', '42 staff', '63 staff'))
utilTimesRPA$sizeF <- factor(utilTimesRPA$size, labels = c('21 staff', '42 staff', '63 staff'))
queueTimesGP$sizeF <- factor(queueTimesGP$size, labels = c('4 staff', '8 staff', '12 staff'))
utilTimesGP$sizeF <- factor(utilTimesGP$size, labels = c('4 staff', '8 staff', '12 staff'))

nameRoot <- paste0(here::here(),'/Submission/TablesAndFigures/', Sys.Date(), '-')

##########################################################
# Figure 2. Estimated processing times with baseline spec
##########################################################

# Calculate median processing time (don't use thes in the end)
medianqTime1 <- queueTimesRPA %>%
  filter(test==2 & level == 1 & station=='Total') %>%
  summarise(t = median(mins)) %>%
  as.numeric()
medianqTime2 <- queueTimesGP %>%
  filter(test==2 & level == 1 & station=='Total') %>%
  summarise(t = median(mins)) %>%
  as.numeric()

# Fig2a
fig2a <- queueTimesRPA %>%
  filter(test==2 & level == 1 & station=='Total') %>%
  ggplot(aes(y = mins, x = sizeF)) +
  # geom_hline(aes(yintercept = medianqTime1), color = 'seagreen', size = 1.2, alpha = 0.2) +
  geom_boxplot() +
  #facet_wrap(~sizeF, nrow = 1) +
  scale_x_discrete(NULL) +
  scale_y_continuous("Processing time (minutess)", limits = c(0, 120), breaks = seq(0, 120, 30))

# Fig2b
fig2b <- queueTimesGP %>%
  filter(test==2 & level == 1 & station=='Total') %>%
  ggplot(aes(y = mins, x = sizeF)) +
  # geom_hline(aes(yintercept = medianqTime2), color = 'seagreen', size = 1.2, alpha = 0.2) +
  geom_boxplot() +
  #facet_wrap(~size, nrow = 1) +
  scale_x_discrete(NULL) +
  scale_y_continuous("Processing time (minutess)", limits = c(0, 120), breaks = seq(0, 120, 30))

# Combine and save
ggpubr::ggarrange(fig2a, fig2b, ncol = 2, labels = c('A', 'B'))
ggsave(file = paste0(nameRoot, 'fig2.pdf'), width = 6.3, height = 3.4, units = 'in')


##################################################
# Figure 3. Estimated daily throughput
##################################################

#Fig 3a
fig3a <-  queueTimesRPA %>%
  filter(test==2 & level == 1 & station=='Total') %>%
  group_by(sizeF, iter) %>%
  summarise(n = max(id)) %>%
  ggplot(aes(x = sizeF, y = n)) +
  geom_jitter(width = .2, shape = 1, color = 'grey40') +
  #geom_smooth(method = 'lm', colour = RColorBrewer::brewer.pal(5, 'Purples')[[5]]) +
  scale_x_discrete('Staff numbers') +
  scale_y_continuous('Daily vaccinations', limits = c(0, 1500), labels = scales::comma)

#Fig 3b
fig3b <- queueTimesGP %>%
  filter(test==2 & level == 1 & station=='Total') %>%
  group_by(sizeF, iter) %>%
  summarise(n = max(id)) %>%
  ggplot(aes(x = sizeF, y = n)) +
  geom_jitter(width = 0.2, shape = 1, color = 'grey40') +
  #geom_smooth(method = 'lm', colour = RColorBrewer::brewer.pal(5, 'Purples')[[5]]) +
  scale_x_discrete('Staff numbers') +
  scale_y_continuous(' ', limits = c(0, 300), labels = scales::comma)

# Combine and save
ggarrange(fig3a, fig3b, ncol = 2, labels = c('A', 'B'))
ggsave(file = paste0(nameRoot, 'fig3.pdf'), width = 6.3, height = 3.4, units = 'in')


##############################################################
# Figure 4. Estimated processing time with increased arrivals 
##############################################################

# Fig 4a
fig4a <- queueTimesRPA %>%
  filter(test==3 & station=='Total') %>%
  ggplot(aes(x = sizeF, y = mins, fill = factor(level))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(NULL) +
  scale_y_continuous('Total processing\ntime (minutes)', limits = c(0, 300), breaks = seq(0, 300, 100)) +
  scale_fill_grey('Arrivals\nincrease\n(per hour)', 
                    label = c('+0', '+10', '+20', '+30', '+40'),
                    start = 1, end = 0.5)
# Fig 4a
fig4b <- queueTimesGP %>%
  filter(test==3 & station=='Total') %>%
  ggplot(aes(x = sizeF, y = mins, fill = factor(level))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete('Number of healthcare staff') +
  scale_y_continuous('Total processing\ntime (minutes)', limits = c(0, 750), breaks = seq(0, 750, 250)) +
  scale_fill_grey('Arrivals\nincrease\n(per 10 mins)', 
                  label = c('+0', '+1', '+2', '+3', '+4'),
                  start = 1, end = 0.5)

# Combine and save
ggarrange(fig4a, fig4b, ncol = 1, labels = c('A', 'B'), align = 'hv')
ggsave(file = paste0(nameRoot, 'fig4.pdf'), width = 6.3, height = 4.25, units = 'in')


##############################################################
# Figure 5. Estimated processing time with decreasing staff
##############################################################

# Fig5a
fig5a <- queueTimesRPA %>%
  filter(test==2 & station=='Total') %>%
  ggplot(aes(x = sizeF, y = mins, fill = factor(level))) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(NULL) +
  scale_y_continuous('Total processing\ntime (minutes)', limits = c(0, NA)) +
  scale_fill_grey('Staff\nshortage', 
                  label = c('0', '1', '2', '3', '4'),
                  start = 1, end = 0.5)

# Fig5b
fig5b <- queueTimesGP %>%
  filter(sim %in% c('6', '7', '21', '22', '23', '24', '36', '37', '38', '39', '40'), station=='Total') %>%
  ggplot(aes(x = sizeF, y = mins, fill = factor(level))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(preserve = "single")) +
  scale_x_discrete('Number of healthcare staff') +
  scale_y_continuous('Total processing\ntime (minutes)', limits = c(0, NA)) +
  scale_fill_grey('Staff\nshortage', 
                  label = c('0', '1', '2', '3', '4'),
                  start = 1, end = 0.5)

# Combine and save
ggarrange(fig5a, fig5b, ncol = 1, labels = c('A', 'B'), align = 'hv')
ggsave(file = paste0(nameRoot, 'fig5.pdf'), width = 6.3, height = 4.25, units = 'in')



##################################################
# Figure S1. Processing time by arrival frequency
##################################################

# Vector of arrival loads (needs to match a-e simulations in simulations.R)
rpaArr <- c(rep(c(10, 35, 60, 85, 110), 1), rep(c(20, 70, 120, 170, 220), 1), rep(c(60, 120, 180, 240, 300), 1))
gpArr <- c(rep(c(1, 2, 3, 4, 5), 1), rep(c(1, 2, 4, 6, 10), 1), rep(c(2, 4, 6, 8, 12), 1))

# Fig S1(A)
figS1a <- queueTimesRPA %>% 
  filter(test==1 & station == 'Total') %>% 
  group_by(sizeF, level) %>% 
  summarise(median = median(mins)) %>% 
  ungroup() %>% 
  mutate(arr = rpaArr) %>% 
  ggplot(aes(y=arr, x=median)) + 
  annotate('rect', xmin = 30, xmax = 60, ymin = 0, ymax = 300, fill = 'grey50', alpha = 0.2) +  
  geom_point(size = 1.5)  + 
  facet_wrap(~sizeF, ncol = 3, scales = 'free') + 
  scale_x_continuous(' ', limits=c(0, 300), breaks = seq(0,300,100), labels = seq(0,300,100)) +
  scale_y_continuous('Arrivals every\n60 minutes', breaks = seq(0,300,100), labels = seq(0,300,100), limits=c(0, 300))

# Fig S1(B)
figS1b <- queueTimesGP %>% 
  filter(test==1 & station == 'Total') %>% 
  group_by(sizeF, level) %>% 
  summarise(median = median(mins)) %>% 
  ungroup() %>% 
  mutate(arr = gpArr) %>% 
  ggplot(aes(y=arr, x=median)) +  
  annotate('rect', xmin = 30, xmax = 60, ymin = 0, ymax = 15, fill = 'grey50', alpha = 0.2) + 
  geom_point(size = 1.5)  + 
  facet_wrap(~sizeF, ncol = 3, scales = 'free') + 
  scale_x_continuous('Median total processing time (minutes)', limits=c(0, 300)) +
  scale_y_continuous('Arrivals every\n10 minutes', breaks = scales::pretty_breaks(), limits=c(0, 15))

# Combine and save
ggarrange(figS1a, figS1b, ncol = 1, labels = c('A', 'B'), vjust = c(1.2, 0), hjust = -.2, align = 'v')
ggsave(file = paste0(nameRoot, 'figS1.pdf'), width = 6.3, height = 3.4, units = 'in')


####################################################
# Figure S2. Staff utilisation by arrival frequency
####################################################

# Vector of arrival loads (needs to match a-e simulations in simulations.R)
rpaArr <- c(rep(c(10, 35, 60, 85, 110), 1), rep(c(20, 70, 120, 170, 220), 1), rep(c(60, 120, 180, 240, 300), 1))
gpArr <- c(rep(c(1, 2, 3, 4, 5), 1), rep(c(1, 2, 4, 6, 10), 1), rep(c(2, 4, 6, 8, 12), 1))

# Fig S2(A)
figS2a <- utilTimesRPA %>% 
  filter(test==1 & station =='Vaccination') %>% 
  group_by(sizeF, station, level) %>% 
  summarise(mean = mean(util))  %>% 
  ungroup() %>% 
  mutate(arr = rpaArr) %>% 
  ggplot(aes(y=arr, x=mean)) + 
  annotate('rect', xmin = 0.5, xmax = 0.8, ymin = 0, ymax = 300, fill = 'grey50', alpha = 0.2) + 
  geom_point(size = 1.5) + 
  facet_wrap(~sizeF, ncol = 3, scales = 'free_y') + 
  scale_x_continuous(' ', limits=c(0, 1), breaks = seq(0,1,.2)) +
  scale_y_continuous('Arrivals every\n60 minutes', limits=c(0, 300))

# Fig S2(A)
figS2b <- utilTimesGP %>% 
  filter(test==1 & station =='Vaccination') %>% 
  group_by(sizeF, station, level) %>% 
  summarise(mean = mean(util))  %>% 
  ungroup() %>% 
  mutate(arr = gpArr) %>% 
  ggplot(aes(y=arr, x=mean)) + 
  annotate('rect', xmin = 0.5, xmax = 0.8, ymin = 0, ymax = 15, fill = 'grey50', alpha = 0.2) + 
  geom_point(size = 1.5) + 
  facet_wrap(~sizeF, ncol = 3, scales = 'free_y') + 
  scale_x_continuous('Staff utilisation factor', limits=c(0, 1), breaks = seq(0,1,.2)) +
  scale_y_continuous('Arrivals every\n10 minutes', breaks = scales::pretty_breaks(), limits=c(0, 15))

# Combine and save
ggpubr::ggarrange(figS2a, figS2b, ncol = 1, labels = c('A', 'B'), vjust = c(1.2, 0), hjust = -.2, align = 'v')
ggsave(file = paste0(nameRoot, 'figS2.pdf'), width = 6.3, height = 3.4, units = 'in')


#########################################
# Figure S3. Staff utilisation by stage
#########################################

# Fig S3(A)
figS3a <- utilTimesRPA %>%
  filter(test==2 & level == 1) %>%
  group_by(station, level, sizeF) %>%
  summarise(mean = mean(util)) %>%
  ggplot(aes(y = station, x = mean)) +
  annotate('rect', xmin = 0.5, xmax = 0.8, ymin = 0.5, ymax = 6.5, fill = 'grey50', alpha = 0.2) + 
  geom_point(size = 2) +
  facet_wrap(~sizeF, nrow = 1) +
  scale_y_discrete(NULL, limits = rev(levels(utilTimesRPA$station))) +
  scale_x_continuous("Average staff utilisation factor", labels = c('0', "", '.5', '', '1'), limits = c(0,1), breaks = seq(0,1,.25))

# Fig S3(B)
figS3b <- utilTimesGP %>%
  filter(test==2 & level == 1) %>%
  group_by(station, level, sizeF) %>%
  summarise(mean = mean(util)) %>%
  filter(station %in% c('Preparation', 'Registration', 'Vaccination', 'Observation')) %>%
  ggplot(aes(y = station, x = mean)) +
  annotate('rect', xmin = 0.5, xmax = 0.8, ymin = 0.5, ymax = 4.5, fill = 'grey50', alpha = 0.2) +
  geom_point(size = 2) +
  facet_wrap(~sizeF, nrow = 1) +
  scale_y_discrete(NULL, limits = rev(levels(utilTimesGP$station))) +
  scale_x_continuous("Average staff utilisation factor", labels = c('0', "", '.5', '', '1'), limits = c(0,1), breaks = seq(0,1,.25))

# Combine and save
ggpubr::ggarrange(figS3a, figS3b, ncol = 1, labels = c('A', 'B'), vjust = c(1.2, 0), hjust = -.2, align  = 'v')
ggsave(file = paste0(nameRoot, 'figS3.pdf'), width = 6.3, height = 3.4, units = 'in')

# ### JAGO ### #