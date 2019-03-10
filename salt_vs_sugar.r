library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

### After exporting Quarry results, upload the CSV under 'files' tab to the right. Call that file here ###

quarry_results <- read.csv(file = '/cloud/project/quarryResults.csv', header=TRUE)

## Initial descriptive results below; uncomment if desired
#sapply(quarry_results,class)
#describe(quarry_results)

#Data Manipulation#
### Converting data types, some feature engineering ###

quarry_results$page_touched <- as.POSIXct(quarry_results$page_touched,tz = "UTC", "%Y-%m-%dT%H:%M:%OS")
quarry_results$rev_timestamp <- as.POSIXct(quarry_results$rev_timestamp,tz = "UTC", "%Y-%m-%dT%H:%M:%OS")
quarry_results$rev_minor_edit <- as.character(quarry_results$rev_minor_edit)

quarry_results <- mutate(quarry_results, rev_year = year(quarry_results$rev_timestamp))
quarry_results <- mutate(quarry_results, rev_date = as.Date(quarry_results$rev_timestamp, format = '%m%d%Y'))

quarry_results <- quarry_results %>%
                  group_by(page_title) %>%
                  mutate(ranked_revisions = dense_rank(row_number()))

quarry_results <- quarry_results %>%
                  group_by(page_title) %>%
                  mutate(prev_rev_len = lag(rev_len))

quarry_results <- quarry_results %>%
                  group_by(page_title) %>%
                  mutate(prev_rev_timestamp = lag(rev_timestamp))

quarry_results$length_rev_id <- quarry_results$rev_len - quarry_results$prev_rev_len

quarry_results$time_between_revs <- difftime(quarry_results$rev_timestamp,quarry_results$prev_rev_timestamp, units = 'secs')
quarry_results$time_between_revs_formatted <- seconds_to_period(quarry_results$time_between_revs)

### Subsetting quarry_results data frame into salt and sugar DFs ###

quarry_results_salt <-subset(quarry_results, page_id == '1605200')
quarry_results_sugar <-subset(quarry_results, page_id == '27712')

#describe(quarry_results_salt)
#describe(quarry_results_sugar)

### Plotting revisions over time ###

revisions_over_time <- quarry_results %>%
  group_by(page_title, rev_year) %>%
  summarize(revs_per_year = n_distinct(rev_id))

revisions_over_time_area_plot <- ggplot(revisions_over_time, aes(x = rev_year, y = revs_per_year), group = 1) + 
  geom_area(aes(color = page_title, fill = page_title), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#427289","#c6a150")) +
  scale_fill_manual(values = c("#427289","#c6a150")) +
  labs(x = "Year of Revision", y = "Revisions per Year", title = "Revisions per Year by Page") +
  theme_minimal() +
  theme(legend.position = 'bottom')

plot(revisions_over_time_area_plot)

### Major Revisions Over Time, Different Views ###

revisions_over_time_edit_type <- quarry_results %>%
  group_by(page_title, rev_year, rev_minor_edit) %>%
  summarize(revs_per_year = n_distinct(rev_id))

major_revisions <- subset(revisions_over_time_edit_type, rev_minor_edit == '0')
major_revisions <- merge(major_revisions, revisions_over_time, by="row.names",all.x=TRUE)
names(major_revisions) <- c('row_names','page_title','rev_year','rev_minor_edit','major_revs_per_year',
                            'page_title_2','rev_year_2','rev_per_year')
major_revisions <- subset(major_revisions, select =-c(row_names,page_title_2,rev_year_2))
major_revisions$pct_major_revs <- major_revisions$major_revs_per_year / major_revisions$rev_per_year

---
  
revisions_over_time_edit_type_plot <- ggplot(revisions_over_time_edit_type, aes(x = rev_year, y = revs_per_year), group = 1) +
  geom_bar(stat="identity",aes(color = page_title, fill = page_title), position = position_dodge(0.8)) + 
  facet_grid(rev_minor_edit~.) +
  scale_color_manual(values = c("#427289","#c6a150")) +
  scale_fill_manual(values = c("#427289","#c6a150")) +
  labs(x = "Year of Revision", y = "Revisions per Year", title = "Revisions per Year, Major vs. Minor") +
  theme_minimal() +
  theme(legend.position = 'bottom')

plot(revisions_over_time_edit_type_plot)

---

revisions_edit_pct_plot <- ggplot(major_revisions, aes(x = rev_year, y= pct_major_revs), group = 1) +
  geom_bar(stat='identity',aes(color = page_title, fill = page_title), alpha = 0.75, position = 'dodge') +
  scale_color_manual(values = c("#427289","#c6a150")) +
  scale_fill_manual(values = c("#427289","#c6a150")) +
  labs(x = "Year of Revision", y = "% of Revisions considered Non-Minor", title = "Non-Minor Revision Rate") +
  theme_minimal() +
  theme(legend.position = 'bottom')
  
plot(revisions_edit_pct_plot)

### User level visualizations ###

contributors_by_page <- quarry_results %>%
  group_by(page_title, rev_user_text) %>%
  summarize(revs_per_user = n_distinct(rev_id))
total_revs_by_page <- contributors_by_page %>%
  group_by(page_title) %>%
  summarize(total_revs = sum(revs_per_user))
contributors_by_page <- merge(contributors_by_page, total_revs_by_page, by.x = 'page_title', by.y = 'page_title', all.x = TRUE)
contributors_by_page$pct_contribution <- contributors_by_page$revs_per_user / contributors_by_page$total_revs
contributors_by_page <- contributors_by_page %>% arrange(page_title,desc(pct_contribution))
contributors_by_pct <- contributors_by_page %>% arrange(page_title,desc(pct_contribution)) %>% top_n(n = 100)

---

contributors_box_plot <- ggplot(contributors_by_page, aes(x = page_title, y = revs_per_user, color = page_title)) + 
  geom_boxplot() +
  scale_color_manual(values = c("#427289","#c6a150")) +
  labs(x = "Page", y = "# of Revisions / User", title = "Revisions Per User") +
  theme_minimal() +
  theme(legend.position = 'bottom')
  
plot(contributors_box_plot)

---

pct_contribution_box_plot <- ggplot(contributors_by_pct, aes(x = page_title, y = pct_contribution, color = page_title)) + 
  geom_boxplot() +
  scale_color_manual(values = c("#427289","#c6a150")) +
  labs(x = "Page", y = "Contribution Percent", title = "Contribution Percent by User") +
  theme_minimal() +
  theme(legend.position = 'bottom')

plot(pct_contribution_box_plot)

### Time Between Revisions Histogram ###

time_between_revs_hist <- ggplot(quarry_results, aes(time_between_revs, color = page_title, fill = page_title)) +
  geom_histogram(binwidth = 432000, position = 'dodge') +
  scale_color_manual(values = c("#427289","#c6a150")) +
  scale_fill_manual(values = c("#427289","#c6a150")) +
  scale_x_continuous(labels = comma) +
  labs(x = "Time (In Seconds) Between Revisions", y = "Count of Users", title = "Time Between Revisions by Page") +
  theme_minimal() +
  theme(legend.position = 'bottom')

plot(time_between_revs_hist)

### Revision Length Plot ###

revision_len_plot <- ggplot(quarry_results, aes(x = page_title, y = length_rev_id, color = page_title, fill = page_title)) +
  geom_boxplot(aes(color = page_title, fill = page_title), alpha = 0.5, position = 'dodge') +
  scale_color_manual(values = c("#427289","#c6a150")) +
  scale_fill_manual(values = c("#427289","#c6a150")) +
  labs(x = "Page", y = "Bytes Per Revision", title = "Page Title") +
  theme_minimal() +
  theme(legend.position = 'bottom')

plot(revision_len_plot)
