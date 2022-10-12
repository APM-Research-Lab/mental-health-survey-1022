library(ggplot2)
library(readr)
library(dplyr)
library(here)

#data downloaded from CDC here: https://data.cdc.gov/NCHS/Indicators-of-Anxiety-or-Depression-Based-on-Repor/8pt5-q6wp
df <- read_csv("Indicators_of_Anxiety_or_Depression_Based_on_Reported_Frequency_of_Symptoms_During_Last_7_Days.csv")


### For figures showing anxiety and depression rates over time 
#select data for Minnesota and US national estimate, for depression and anxiety separately 
minn_anxiety <- df %>% filter(State %in% c('United States', 'Minnesota')) %>%
	filter(Subgroup %in% c('United States', 'Minnesota')) %>%
	filter(Phase >= 0) %>%
	filter(Indicator == 'Symptoms of Anxiety Disorder') %>%
	mutate(date = as.Date(`Time Period End Date`, format = '%m/%d/%Y'))

minn_depression <- df %>% filter(State %in% c('United States', 'Minnesota')) %>%
	filter(Subgroup %in% c('United States', 'Minnesota')) %>%
	filter(Phase >= 0) %>%
	filter(Indicator == 'Symptoms of Depressive Disorder') %>%
	mutate(date = as.Date(`Time Period End Date`, format = '%m/%d/%Y'))

# plot Minnesota anxiety and US anxiety over time
# place horizontal line for pre-pandemic level gathered via similar survey (NHIS), 
# cited by CDC here: https://www.cdc.gov/nchs/covid19/pulse/mental-health.htm
ggplot(minn_anxiety) + 
	geom_line(aes(x = date, y = Value, col = State), size = 1) + 
	geom_hline(aes(yintercept=8.1))+
	labs(title = 'American anxiety is back up to May 2020 levels', 
		 subtitle = 'Percentage of surveyed adults who report recent symptoms shown to be \nassociated with diagnosis of generalized anxiety disorder.',
		 caption = 'Results are from the U.S. Census Household Pulse Survey, conducted periodically during the \nCOVID-19 pandemic. Analysis and graph: Elisabeth Gawthrop | APM Research Lab.') + 
	xlab("") +
	ylab("Percent of respondents with symptoms") + 
	scale_color_manual(values = c('#b1d1e6', '#02334e')) +
	scale_x_date(date_breaks = '2 months', date_labels = '%b\n%y') +
	scale_y_continuous(labels = function(x) paste0(x, "%"),limits = c(0,40)) +
	theme_classic() + 
	theme(panel.grid.major = element_line('grey90'),
		  panel.grid.minor.x = element_line('grey80', linetype = 'dotted'),
		  panel.grid.minor.y = element_line('grey90'),
		  legend.position = 'top',
		  legend.title=element_blank(),
		  legend.margin = margin(b=-5),
		  legend.justification='left', 
		  plot.caption = element_text(hjust = 0),
		  text = element_text(family = 'Inter'))

# plot Minnesota depression and US depression over time
# place horizontal line for pre-pandemic level gathered via similar survey (NHIS), 
# cited by CDC here: https://www.cdc.gov/nchs/covid19/pulse/mental-health.htm
ggplot(minn_depression) + 
	geom_line(aes(x = date, y = Value, col = State), size = 1) + 
	geom_hline(aes(yintercept=6.5))+
	labs(title = 'Throughout pandemic, Minnesotans less depressed than national average', 
		 subtitle = 'Percentage of surveyed adults who report recent symptoms shown to be \nassociated with diagnosis of major depressive disorder.',
		 caption = 'Results are from the U.S. Census Household Pulse Survey, conducted periodically during the \nCOVID-19 pandemic. Analysis and graph: Elisabeth Gawthrop | APM Research Lab.') + 
	xlab("") +
	ylab("Percent of respondents with symptoms") + 
	scale_color_manual(values = c('#b1d1e6', '#02334e')) +
	scale_x_date(date_breaks = '2 months', date_labels = '%b\n%y') +
	scale_y_continuous(labels = function(x) paste0(x, "%"),limits = c(0,31)) +
	theme_classic() + 
	theme(panel.grid.major = element_line('grey90'),
		  panel.grid.minor.x = element_line('grey80', linetype = 'dotted'),
		  panel.grid.minor.y = element_line('grey90'),
		  legend.position = 'top',
		  legend.title=element_blank(),
		  legend.margin = margin(b=-5),
		  legend.justification='left', 
		  plot.caption = element_text(hjust = 0),
		  text = element_text(family = 'Inter'))
	
# plots exported as PDF and finalized in Illustrator


### For map showing anxiety and depression by state, and analysis points re: 
# how Minnesota compares to others states 

# first for depression
states_depressive <- df %>% filter(Group == 'By State') %>%
	filter(Indicator == 'Symptoms of Depressive Disorder') %>%
	group_by(State) %>%
	summarise(mean(Value))
# MN ranks #1 (least depressed)

# second for anxiety
states_anxiety <- df %>% filter(Group == 'By State') %>%
	filter(Indicator == 'Symptoms of Anxiety Disorder') %>%
	group_by(State) %>%
	summarise(mean(Value))
# MN ranks #3 (3rd-least depressed)

# third for anxiety and/or depression
states_both <- df %>% filter(Group == 'By State') %>%
	filter(Indicator == 'Symptoms of Anxiety Disorder or Depressive Disorder') %>%
	group_by(State) %>%
	summarise(mean(Value))
	
# export CSV for anxiety and/or depression for mapping in datawrapper
write.csv(states_both, 'states_both.csv')


### For figure showing rates of depression/anxiety by demographic group
# Filter for just United States as a whole and use 'subgroup' field to group by 
# demographic indicator. Calculate minimum, maximum and mean result for each 
# subgroup. Calculate min and max date to report when information was collected
# (varies by subgroup).
us_both_groups <- df %>%
	filter(State == "United States") %>%
	filter(Indicator == 'Symptoms of Anxiety Disorder or Depressive Disorder') %>%
	group_by(Subgroup) %>%
	filter(Phase >= 0) %>%
	mutate(date = as.Date(`Time Period End Date`, format = '%m/%d/%Y')) %>%
	mutate(mean = mean(Value, na.rm = TRUE)) %>%
	mutate(min = min(Value, na.rm = TRUE)) %>%
	mutate(max = max(Value, na.rm = TRUE)) %>%
	mutate(min_date = min(date, na.rm = TRUE)) %>%
	mutate(max_date = max(date, na.rm = TRUE)) 

# Rename subgroup titles to include dates when data collected according to
# previous step. Then select just variables needed and use unique() to remove 
#redundant data. Then export.
us_both_groups_export <- us_both_groups %>% 
	mutate(Group = recode(Group, 
						  `National Estimate` = 'National estimate - since May 2020',
						  `By Age` = 'By age - since May 2020',
						  `By Sex` = 'By sex - since May 2020',
						  `By Race/Hispanic ethnicity` = 'By race/Hispanic ethnicity - since May 2020',
						  `By Education` = 'By education - since May 2020', 
						  `By Disability status` = 'By disability status - since April 2021', 
						  `By Gender identity` = 'By gender identity - since August 2021',
						  `By Sexual orientation` = 'By sexual orientation - since August 2021')) %>%
	select(Indicator, Group, Subgroup,mean, min,max) %>%
	unique()

write.csv(us_both_groups_export, 'us_both_groups.csv')


### Select and plot anxiety and depression rates separately, for U.S. as a whole 
# (used in newsletter, not story)
us_anxiety_depression <- df %>% filter(State == "United States") %>%
	filter(Subgroup == 'United States') %>%
	filter(Phase >= 0) %>%
	filter(Indicator != 'Symptoms of Anxiety Disorder or Depressive Disorder')%>%
	mutate(date = as.Date(`Time Period End Date`, format = '%m/%d/%Y'))

ggplot(us_anxiety_depression) + 
	geom_line(aes(x = date, y = Value, col = Indicator), size = 1) + 
	geom_hline(aes(yintercept=8.1), linetype = 'dashed')+
	geom_hline(aes(yintercept=6.5),linetype = 'dashed')+
	labs(title = 'American anxiety is back up to May 2020 levels', 
		 subtitle = 'Percentage of surveyed adults who report recent symptoms shown to be \nassociated with diagnosis of generalized anxiety disorder or major depressive disorder.',
		 caption = 'Results are from the U.S. Census Household Pulse Survey, conducted periodically during the \nCOVID-19 pandemic. Analysis and graph: Elisabeth Gawthrop | APM Research Lab.') + 
	xlab("") +
	ylab("Percent of respondents with symptoms") + 
	scale_color_manual(values = c('#b1d1e6', '#02334e')) +
	scale_x_date(date_breaks = '2 months', date_labels = '%b\n%y') +
	scale_y_continuous(labels = function(x) paste0(x, "%"),limits = c(0,40)) +
	theme_classic() + 
	theme(panel.grid.major = element_line('grey90'),
		  panel.grid.minor.x = element_line('grey80', linetype = 'dotted'),
		  panel.grid.minor.y = element_line('grey90'),
		  legend.position = 'top',
		  legend.title=element_blank(),
		  legend.margin = margin(b=-5),
		  legend.justification='left', 
		  plot.caption = element_text(hjust = 0),
		  text = element_text(family = 'Inter'))

