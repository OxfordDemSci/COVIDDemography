
# R script to replicate Figure 1 in PNAS paper: 
# Dowd, J.B, Andriano, L., Brazel, D.M., Rotondi, V., Block, P., Ding, X., Liu, Y., Mills, M.C. (2020)
# "Demographic science aids in understanding the spread and fatality rates of COVID-19"
# Edited on 24th April 2020 by Liliana Andriano, liliana.andriano@sociology.ox.ac.uk
library(ggplot2);library(dplyr);library(cowplot);library(ggpubr)
load("data/df-figure1-Dowd-et-al-2020-PNAS.RData")

# ggplot code for population in Italy and Republic of Korea
p_it_kr<-ggplot(data = medians, aes(x = age, y = pop, fill = Country)) +
  geom_bar(data = medians %>% filter(sex == "Female", Country == "Italy"),
           stat = "identity",
           position = "identity", alpha = 1) +
  geom_bar(data = medians %>% filter(sex == "Male", Country == "Italy"),
           stat = "identity",
           position = "identity", alpha = 1) +
  geom_step(data =  medians %>% filter(sex == "Female", Country == "Republic of Korea"), 
            aes(x = ageno),color="Red",size=1.5, alpha = 1) +
  geom_step(data =  medians %>% filter(sex == "Male", Country == "Republic of Korea"), 
            aes(x = ageno),color="Red",size=1.5, alpha = 1) +
  scale_y_continuous(labels = function(x) as.character(abs(x))) +
  coord_flip() +
  theme_set(theme_cowplot(font_size=20)) + geom_hline(yintercept=0) + 
  labs(y = "Population in thousands", x="Age") + 
  scale_fill_manual(values = c("Blue","Red")) + 
  annotate("text", x = 11, y = -1, label = 'atop(bold("Male"))',hjust = 3, vjust = 1,parse=T,size=6.5) + 
  annotate("text", x = 11, y = -1, label = 'atop(bold("Female"))',hjust = -1, vjust = 1,parse=T,size=6.5)

# ggplot code for deaths in Italy and Republic of Korea
d_it_kr<-ggplot() +
  geom_bar(stat = 'identity') +
  geom_bar(data=medians[medians$Country=="Italy",],aes(age, dead,fill=Country,group=age), alpha = 1, stat = 'identity') +
  geom_bar(data=medians[medians$Country=="Republic of Korea",],aes(age, dead,fill=Country,group=age), alpha = 1, stat = 'identity') +
  scale_y_continuous(labels = function(x) as.character(abs(x))) +
  coord_flip() +
  theme_set(theme_cowplot(font_size=20)) + geom_hline(yintercept=0) + 
  labs(y = "Expected deaths in thousands", x="Age") + 
  scale_fill_manual(values = c("Blue","Red")) + 
  annotate("text", x = 11, y = 0, label = 'atop(bold("Male"))',hjust = 3, vjust = 1,parse=T,size=6.5) + 
  annotate("text", x = 11, y = 0, label = 'atop(bold("Female"))',hjust = -1, vjust = 1,parse=T,size=6.5)

# ggplot code for population in Brazil and Nigeria
p_br_ng<-ggplot(data = medians, aes(x = age, y = pop, fill = Country)) +
  geom_bar(data = medians %>% filter(sex == "Female", Country == "Brazil"),
           stat = "identity",
           position = "identity", alpha = 1) +
  geom_bar(data = medians %>% filter(sex == "Male", Country == "Brazil"),
           stat = "identity",
           position = "identity", alpha = 1) +
  geom_step(data =  medians %>% filter(sex == "Female", Country == "Nigeria"), 
            aes(x = ageno),color="Red",size=1.5, alpha = 1) +
  geom_step(data =  medians %>% filter(sex == "Male", Country == "Nigeria"), 
            aes(x = ageno),color="Red",size=1.5, alpha = 1) +
  scale_y_continuous(labels = function(x) as.character(abs(x))) +
  coord_flip() +
  theme_set(theme_cowplot(font_size=20)) + geom_hline(yintercept=0) + 
  labs(y = "Population in thousands", x="Age") + 
  scale_fill_manual(values = c("Blue","Red")) + 
  annotate("text", x = 11, y = -1, label = 'atop(bold("Male"))',hjust = 3, vjust = 1,parse=T,size=6.5) + 
  annotate("text", x = 11, y = -1, label = 'atop(bold("Female"))',hjust = -1, vjust = 1,parse=T,size=6.5)

# ggplot code for deaths in Brazil and Nigeria
d_br_ng<-ggplot() +
  geom_bar(stat = 'identity') +
  geom_bar(data=medians[medians$Country=="Brazil",],aes(age, dead,fill=Country,group=age), alpha = 1, stat = 'identity') +
  geom_bar(data=medians[medians$Country=="Nigeria",],aes(age, dead,fill=Country,group=age), alpha = 1, stat = 'identity') +
  scale_y_continuous(labels = function(x) as.character(abs(x))) +
  coord_flip() +
  theme_set(theme_cowplot(font_size=20)) + geom_hline(yintercept=0) + 
  labs(y = "Expected deaths in thousands", x="Age") + 
  scale_fill_manual(values = c("Blue","Red")) + 
  annotate("text", x = 11, y = 0, label = 'atop(bold("Male"))',hjust = 3, vjust = 1,parse=T,size=6.5) + 
  annotate("text", x = 11, y = 0, label = 'atop(bold("Female"))',hjust = -1, vjust = 1,parse=T,size=6.5)

# Figure 1
pdf(file = "figs/Figure1.pdf", width = 14, height = 14)
ggarrange(ggarrange(p_it_kr,d_it_kr, ncol=2,common.legend = TRUE, legend="bottom")
          ,ggarrange(p_br_ng,d_br_ng, ncol=2,common.legend = TRUE, legend="bottom")
          ,nrow = 2)
dev.off()
