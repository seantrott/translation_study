library(tidyverse)
library(lme4)

# Working directory
setwd("/Users/seantrott/Dropbox/UCSD/Research/Semantics/Translation/translation_study")

## Load item data
df_items = read_csv("data/items.csv")

df_items_gathered = df_items %>%
  pivot_longer(c(list_1, list_2, list_3, list_4), 
               names_to = "list", values_to = "version")


## Load data
df_e1 = read_csv("data/processed/translation_e1_processed.csv")
length(unique(df_e1$subject))


### Something is wrong: at least 1 more subject for plausibility than recall
### Also, both of them seem to be missing 1 subject? Figure out what's going on here...
table(df_e1$task) / 120


f = df_e1 %>%
  filter(task == "recall") %>%
  group_by(object) %>%
  summarise(count = n())

### Also allow task == "recall"
df_e1 = df_e1 %>%
  filter(task %in% c("recall", "plausibility") | type == "filler")

df_e1$response = fct_recode(
  factor(df_e1$key_press),
  "Yes" = "89",
  "No" = "78"
)


### Double-check this: Get accuracy/correcdt response
df_e1 = df_e1 %>%
  mutate(correct_response = case_when(
    type == "filler" ~ "No",
    is.na(type) ~ "Yes"
  )) %>%
  mutate(correct = correct_response == response)


### TODO: Identify ppts or items with low accuracy for fillers

### Identify ppts with low accuracy on plausibility
df_ppt_filler_accuracy_plaus = df_e1 %>%
  filter(type == "filler") %>%
  filter(task == "plausibility") %>%
  group_by(subject) %>%
  summarise(filler_accuracy_plaus = mean(correct))

df_ppt_filler_accuracy_recall = df_e1 %>%
  filter(type == "filler") %>%
  filter(task == "recall") %>%
  group_by(subject) %>%
  summarise(filler_accuracy_recall = mean(correct))

### Filter to critical items
df_critical = df_e1 %>%
  filter(shape %in% c('a', 'b')) %>%
  left_join(df_ppt_filler_accuracy_plaus, on = "subject") %>% ## Connect to accuracy %>%
  left_join(df_ppt_filler_accuracy_recall, on = "subject") ## Connect to accuracy %>%


### Remove ppts with low accuracy on plausibility 
## TODO: Identify threshold
## TODO: Also remove for low accuracy on recall task?
# df_critical = df_critical %>%
#   filter(accuracy > .75)

### Is something going wrong with the number of plausibility (83) vs. recall (81) items?
### Likely two things: 1) missing some label for recall data; and 2) issue happens while merging?
table(df_critical$task) / 60

### Likely a labeling issue; or some subjects don't have recall data?
f = df_critical %>%
  filter(task == "recall") %>%
  group_by(object) %>%
  summarise(count = n())



################ Now merge with item information

## Merge with items
df_merged = df_critical %>%
  left_join(df_items_gathered, on = c(list, object))

## Label whether or not shape/sentence is listed as matching
df_merged = df_merged %>%
  mutate(
    match = ifelse(
    version %in% c("aa", "bb"), "yes", "no"
    )
  )




############## Does "match" predict accuracy? 

df_merged %>%
  filter(task == "recall") %>%
  group_by(match) %>%
  summarise(accuracy = mean(correct))

df_merged %>%
  filter(task == "recall") %>% 
  mutate(correct_numeric = as.numeric(correct)) %>%
  ggplot(aes(x = match,
             y = correct_numeric)) +
  stat_summary (fun = function(x){mean(x)},
                fun.min = function(x){mean(x) - 2*sd(x)/sqrt(length(x))},
                fun.max = function(x){mean(x) + 2*sd(x)/sqrt(length(x))},
                geom= 'pointrange', 
                position=position_dodge(width=0.95)) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = .5, linetype = "dotted") +
  labs(x = "Matching shape",
       y = "Accuracy") +
  theme_minimal()


### Predict accuracy from match

df_merged_recall = df_merged %>%
  filter(task == "recall")

model_full = glmer(data = df_merged_recall,
                   correct ~ match + 
                     (1 + match | subject) +
                     (1 + match | object),
                   family = binomial())

model_reduced = glmer(data = df_merged_recall,
                   correct ~ 
                     (1 + match | subject) +
                     (1 + match | object),
                   family = binomial())

anova(model_full, model_reduced)


df_merged_recall$pred = predict(model_full, type = "response")

df_merged_recall %>%
  ggplot(aes(x = match,
             y = pred)) +
  geom_boxplot() +
  theme_minimal()


## Calculate d-prime

## Need to preserve fillers: 
## d' = false-alarm rate (p(Yes | No)) - hit-rate (i.e., p(Yes | Yes))
## Calculate d' for matching vs. non-matching items.


df_by_ppt = df_merged %>%
  filter(task == "recall") %>%
  group_by(subject, match) %>%
  summarise(hit_rate = mean(correct),
            filler_acc = mean(filler_accuracy_recall),
            false_alarm = 1 - mean(filler_accuracy_recall)) %>%
  ungroup() %>%
  mutate(z_h = scale(hit_rate),
         z_fa = scale(false_alarm)) %>%
  mutate(d_prime = z_h - z_fa,
         d_prime2 = hit_rate - false_alarm)

df_by_ppt %>%
  ggplot(aes(x = match,
             y = d_prime)) +
  stat_summary (fun = function(x){mean(x)},
                fun.min = function(x){mean(x) - 2*sd(x)/sqrt(length(x))},
                fun.max = function(x){mean(x) + 2*sd(x)/sqrt(length(x))},
                geom= 'pointrange', 
                position=position_dodge(width=0.95)) +
  labs(x = "Matching shape",
       y = "d'") +
  theme_minimal()



model_full = lmer(data = df_by_ppt,
                   d_prime2 ~ match + 
                     (1 | subject),
                  REML = FALSE)

model_reduced = lmer(data = df_by_ppt,
                  d_prime2 ~ 
                    (1 | subject),
                  REML = FALSE)

anova(model_full, model_reduced)