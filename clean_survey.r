library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(cetcolor)

csv_files = dir('.')[grepl('.*csv$', dir('.'))]

data_list = lapply(csv_files, read.csv, stringsAsFactors=FALSE)

survey = bind_rows(data_list)

names(survey) = c(
  'timestamp',
  'food',
  'relationship_level',
  'alive',
  'biological',
  'three_words',
  'gender',
  'age_group',
  'mothers_day_child',
  'mothers_day_adult',
  'grlc'
)


expand_column <- function(df, column){
  table(stack(setNames(strsplit(as.character(df[,column]), ";\\s*"), seq_len(nrow(df))))[2:1])
}

build_cooccurrence_matrix <-function(x, min_colsum=3){
  #x = x[,-1]
  x = x[,colSums(x) >=min_colsum]
  vnames = colnames(x)
  com = matrix(nrow=ncol(x), ncol=ncol(x), 0)
  for (i in 1:ncol(x)){
    for (j in i:ncol(x)){
      v = sum(x[,i]*x[,j])
      com[i, j] = v
      com[j, i] = v
    }
  }
  rownames(com) = vnames
  colnames(com) = vnames
  return(com)
}

age_gender_pops = list(
  'Male'=list(
    'Under 18'=27,
    '18-24'= 13,
    '25-34'=20,
    '35-44'=22,
    '45-54'=18,
    '55-64'=11.5
  ),
  'Female'=list(
    'Under 18'=26,
    '18-24'=13,
    '25-34'=20,
    '35-44'=22,
    '45-54'=19,
    '55-64'=12.5
  )
)

groups = matrix(nrow=2, ncol=6)
groups[1,] = unlist(age_gender_pops[['Female']])
groups[2,] = unlist(age_gender_pops[['Male']])
group_proportions = groups/sum(groups)
survey_group_sizes = survey %>%
  group_by(gender, age_group) %>%
  summarize(count=n()) %>%
  ungroup () %>% 
  mutate(group_proportion=count/sum(count)) %>%
  
calculate_weights <- function(prop1, age_groups, genders){
  weights = numeric(length(prop1))
  for (i in 1:length(prop1)){
    row = (genders[i]=='Male')*1 + 1
    column = which(age_groups[i]==c('Under 18','18-24','25-34','35-44','45-54','55-64'))
    weights[i] = groups[row, column]/(prop1[i] + 0.01)
  }
  return(weights)
}

survey_group_sizes$weights = calculate_weights(survey_group_sizes$group_proportion, 
                                               survey_group_sizes$age_group, 
                                               survey_group_sizes$gender
)

# calculate weights overall
survey_weighted = survey %>% left_join(survey_group_sizes, by=c('gender','age_group'))
total_adult_weights = survey_weighted %>% filter(age_group != 'Under 18') %>% 
  summarize(a=sum(weights)) %>% as.numeric()

total_child_weights = sum(survey_weighted$weights)

adult_weights = survey_weighted[survey_weighted$age_group != 'Under 18','weights']
child_weights = survey_weighted$weights

# plot stuff done for mother
n_adults = nrow(survey %>% filter(age_group != 'Under 18')) 
adult_expanded = expand_column(survey %>% filter(age_group != 'Under 18'), 
                               'mothers_day_adult') %>% 
  as.data.frame.matrix() 
child_expanded = expand_column(survey, 'mothers_day_child') %>% 
  as.data.frame.matrix()

molten_vars = colnames(adult_expanded)
child_molten_vars = colnames(child_expanded)

adult_expanded$weight = adult_weights
child_expanded$weight = child_weights

adult_stuff = adult_expanded %>% melt(measure.vars=molten_vars) %>%
  group_by(variable) %>% 
  summarize(proportion=mean(value),
            weighted_proportion=sum(weight*value)/total_adult_weights) %>%
  ungroup() %>%
  filter(proportion > 0.03) %>%
  filter(!grepl('I am still under.*', variable)) %>%
  mutate(age='adult', label=paste(percent(proportion), sprintf('(%d)', n_adults)),
         weighted_label=percent(weighted_proportion))

child_stuff = child_expanded %>% melt(measure.vars=child_molten_vars) %>%
  group_by(variable) %>% 
  summarize(proportion=mean(value), 
            weighted_proportion=sum(weight * value)/total_child_weights) %>%
  ungroup() %>%
  filter(proportion > 0.03) %>%
  filter(!grepl('I am still under.*', variable)) %>%
  mutate(age='child', label=paste(percent(proportion), sprintf('(%d)', nrow(survey))),
         weighted_label=percent(weighted_proportion))


stuff = rbind(adult_stuff, child_stuff)
combined_stuff = stuff %>% group_by(value) %>%
  summarize(total=sum(proportion) * sum(1-(value=='Nothing special'))) %>%
  ungroup()

# order the levels
level_orders = as.character(combined_stuff$value)[order(combined_stuff$total)]
stuff$value = factor(as.character(stuff$value), levels=level_orders)

ggplot(stuff) + 
  geom_bar(aes(x=variable, y=proportion, fill=age), stat='identity', position='dodge') + 
  ylab('proportion') + xlab('Mothers Day Activity') + 
  coord_flip() + 
  geom_text(aes(x=variable, y=proportion, group=age, label=label), 
            position=position_dodge(width=1), hjust='inward') + 
  ggtitle('Prevalence of Mothers Day Activities and Gifts', 
          subtitle='among 56 sampled participants') + 
  scale_fill_manual('Age Celebrating', values=c('adult'='#DD4433', 'child'='#FFAAAA')) +
  scale_y_continuous(label=percent) +
  theme_bw() + 
  theme(axis.text=element_text(size=rel(1.3)),
        legend.title = element_text(size=rel(1.5)),
        legend.text = element_text(size=rel(1.3)))


# weighted plot

ggplot(stuff) + 
  geom_bar(aes(x=variable, y=weighted_proportion, fill=age), stat='identity', position='dodge') + 
  ylab('proportion') + xlab('Mothers Day Activity') + 
  coord_flip() + 
  geom_text(aes(x=variable, y=weighted_proportion, group=age, label=weighted_label), 
            position=position_dodge(width=1), hjust='inward') + 
  ggtitle('Prevalence of Mothers Day Activities and Gifts', 
          subtitle='among 56 sampled participants, weighting by gender and age group') + 
  scale_fill_manual('Age Celebrating', values=c('adult'='#DD4433', 'child'='#FFAAAA')) +
  scale_y_continuous(label=percent) +
  theme_bw() + 
  theme(axis.text=element_text(size=rel(1.3)),
        legend.title = element_text(size=rel(1.5)),
        legend.text = element_text(size=rel(1.3)))

# plot relationship
#acom = build_cooccurrence_matrix(adult_expanded)
#ccom = build_cooccurrence_matrix(child_expanded)

#macom = acom %>% melt() %>%
#  group_by(Var1, Var2) %>%
#  summarize(value=max(value)) %>% ungroup() %>%
#  mutate(proportion=value/nrow(adult_expanded), 
#         label=sprintf('%s (%d)', percent(proportion), nrow(adult_expanded))
#         ) 

#mccom = ccom %>% melt() %>%
#  group_by(Var1, Var2) %>%
#  summarize(value=max(value)) %>% ungroup() %>%
#  mutate(proportion=value/nrow(child_expanded),
#         label=sprintf('%s (%d)', percent(proportion), nrow(child_expanded)
#                       )
#  )

# ggplot(mccom %>% filter(Var1 != Var2)) + geom_tile(aes(x=Var1,y=Var2, fill=proportion)) + 
#  scale_fill_gradientn(colors=cet_pal(7, 'bmw'))

# words?

# plot demographics
demographics = survey %>% group_by(age_group, gender) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(proportion=total/sum(total),
         label=sprintf('%s (%d)', percent(proportion), total),
         age_group=factor(as.character(age_group), 
                          levels=c('Under 18','18-24','25-34','35-44','45-54','55-64')
         )
  )

ggplot(demographics) + 
  geom_bar(aes(x=age_group, y=proportion, fill=gender), 
           stat='identity', position='dodge') + 
  geom_text(aes(x=age_group, y=proportion, group=gender, label=label), 
            position=position_dodge(width=1),hjust='inward') +
  coord_flip() + 
  xlab('Age Group') + 
  scale_y_continuous(label=percent) + 
  ggtitle('Demographics of Responses') +
  scale_fill_manual('Gender', values=c('Female'='#FFAAAA','Male'='#EE77EE'))  +
  theme_bw() + 
  theme(axis.text=element_text(size=rel(1.3)),
        legend.title = element_text(size=rel(1.5)),
        legend.text = element_text(size=rel(1.3)))