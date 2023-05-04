# Election Polls Graphs
source("R/libraries.R")
source("R/functions.R")


# Data import -------------------------------------------------------------

load("data/db_polls.RData")
load("data/usa_geomfeatures.RData")



# Feature Engineering -----------------------------------------------------

db_mean <- db_polls %>%
  filter(month == "October") %>% 
  group_by(name, code, region, electors) %>% 
  summarise(democratic = round(weighted.mean(democratic, volume), 2),
            republican = round(100 - democratic, 2)) %>%  
  ungroup() %>% 
  compute_margin(party_margin = 2)

party_colors <- c("#2E74C0", "#CCCCCC", "#CB454A")



# Tables ------------------------------------------------------------------

db_mean %>% 
  compute_margin(party_margin = 2) %>% 
  compute_polls(FALSE, "national") %>% 
  set_names(c("Party", "Sentiment (%)")) %>% 
  gt() %>%
  tab_header(title = "US Election Sentiment - National Level")

db_mean %>% 
  compute_margin(party_margin = 2) %>%
  compute_polls(FALSE, "regional") %>% 
  set_names(c("Region", "Party", "Sentiment (%)")) %>% 
  gt() %>%
  tab_header(title = "US Election Sentiment - Regional Level") 

db_mean %>% 
  compute_margin(party_margin = 2) %>% 
  select(name, democratic, republican) %>% 
  set_names(c("State", "Democratic (%)", "Republican (%)")) %>% 
  gt() %>%
  tab_header(title = "US Election Sentiment - State Level")



# Point Plot --------------------------------------------------------------

# by State
db_mean %>% 
  ggplot(mapping = aes(x = -margin, y = reorder(name, margin), col = party)) +
  geom_segment(aes(x = -margin, y = reorder(name, margin), 
                   xend = 0, yend = reorder(name, margin)), col = 1, alpha = 0.8) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, color = "gray30", alpha = 0.8) +
  scale_color_manual(values = party_colors) +
  scale_x_continuous(breaks = c(-100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100),
                     limits = c(-100, 100),
                     labels = c("100\n Biden", "75", "50", "25", "10", "0",
                                "10", "25", "50", "75", "100\n Trump")) +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  guides(color = FALSE) + 
  labs(x = "Margin", y = "", title = "US Elections - Sentiment Margin") +
  theme(axis.text = element_text(size = 10)) +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())
  

p1 <- ggplot(db_mean, mapping = aes(x = -margin, y = reorder(name, margin), col = party)) +
  geom_segment(aes(x = -margin, y = reorder(name, margin), 
                   xend = 0, yend = reorder(name, margin)), col = 1, alpha = 0.8) +
  geom_point_interactive(aes(tooltip = description_full), size = 2) +
  geom_vline(xintercept = 0, color = "gray30", alpha = 0.8) +
  scale_color_manual(values = party_colors) +
  scale_x_continuous(breaks = c(-100, -75, -50, -25, -10, 0, 10, 25, 50, 75, 100),
                     limits = c(-100, 100),
                     labels = c("100\n Biden", "75", "50", "25", "10", "0",
                                "10", "25", "50", "75", "100\n Trump")) +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  guides(color = FALSE) + 
  labs(x = "Margin", y = "") +
  theme(axis.text = element_text(size = 5)) +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())
girafe(ggobj = p1)



# Bar Plot ----------------------------------------------------------------

max(db_mean$democratic)

db_mean %>% 
  select(name, code, region, democratic, republican) %>% 
  pivot_longer(-c(name, code, region), names_to = "president", values_to = "value") %>% 
  mutate(party = case_when(president == "democratic" ~  "Democratic",
                           president == "republican" ~ "Republican", 
                           TRUE ~ NA_character_),
         value = if_else(party == "Democratic", -value, value)) %>% 
ggplot(mapping = aes(x = reorder(name, -value), y = value, fill = party)) +
  geom_col(width = 0.5, position = "identity") +
  coord_flip() +
  geom_hline(yintercept = 0, color = "gray30", alpha = 0.8) +
  geom_hline(yintercept = -50, color = party_colors[1], alpha = 0.6, size = 1) +
  geom_hline(yintercept = 50, color = party_colors[3], alpha = 0.6, size = 1) +
  scale_fill_manual(values = party_colors[c(1,3)]) +
  scale_y_continuous(breaks = c(-60, -50, -40, -25, 0, 25, 40, 50, 60),
                     limits = c(-60, 60),
                     labels = c("60\n Biden", "50", "40", "25", "0",
                                "25", "40", "50", "60\n Trump")) +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  guides(fill = FALSE) + 
  labs(x = "", y = "Sentiment (%)", title = "US Elections - Sentiment") +
  theme(axis.text = element_text(size = 10)) +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())

bar1 <- db_mean %>% 
  select(1:5) %>% 
  pivot_longer(-c(name, code, region), names_to = "president", values_to = "value") %>% 
  mutate(party = case_when(president == "Joe Biden" ~  "Democratic",
                           president == "Donald Trump" ~ "Republican"),
         value = if_else(party == "Democratic", -value, value)) %>% 
  ggplot(mapping = aes(x = reorder(name, -value), y = value, fill = party)) +
  geom_col_interactive(aes(tooltip = paste0(abs(value), "%")), width = 0.5, position = "identity") +
  coord_flip() +
  geom_hline(yintercept = 0, color = "gray30", alpha = 0.8) +
  geom_hline(yintercept = -50, color = party_colors[1], alpha = 0.6, size = 1) +
  geom_hline(yintercept = 50, color = party_colors[3], alpha = 0.6, size = 1) +
  scale_fill_manual(values = party_colors[c(1,3)]) +
  scale_y_continuous(breaks = c(-60, -50, -40, -25, 0, 25, 40, 50, 60),
                     limits = c(-60, 60),
                     labels = c("60\n Biden", "50", "40", "25", "0",
                                "25", "40", "50", "60\n Trump")) +
  facet_wrap(~ region, ncol = 2, scales = "free_y") +
  guides(fill = FALSE) + 
  labs(x = "", y = "Vote (%)") +
  theme(axis.text = element_text(size = 5)) +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())
girafe(ggobj = bar1)



db_mean %>% 
  mutate(value = case_when(party == "Republican" ~ republican,
                           party == "Democratic" ~ -democratic,
                           party == "Uncertain" ~ 50)) %>% 
  ggplot(mapping = aes(x = reorder(name, value), y = abs(value), fill = party)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = party_colors) +
  scale_y_continuous(breaks = c(40, 50, 55, 60), limits = c(40, 60), oob = rescale_none) +
  guides(fill = FALSE) + 
  labs(x = "", y = "Sentiment (%)", title = "US Elections - Sentiment") +
  theme(axis.text = element_text(size = 10), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())

bar2 <- db_mean %>% 
  mutate(value = case_when(party == "Republican" ~ republican,
                           party == "Democratic" ~ -democratic,
                           party == "Uncertain" ~ 50)) %>% 
  ggplot(mapping = aes(x = reorder(name, value), y = abs(value), fill = party)) +
  geom_col_interactive(aes(tooltip = description_full), width = 0.5, position = "identity") +
  scale_fill_manual(values = party_colors) +
  scale_y_continuous(breaks = c(40, 50, 55, 60), limits = c(40, 60), oob = rescale_none) +
  guides(fill = FALSE) + 
  labs(x = "", y = "Sentiment (%)", title = "US Elections - Sentiment") +
  theme(axis.text = element_text(size = 5), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())
girafe(ggobj = bar2)



# Time Series -------------------------------------------------------------

db_polls %>% 
  #filter(month == "September") %>% 
  group_by(date) %>% 
  summarise(democratic = round(weighted.mean(democratic, volume), 2),
            republican = round(100 - democratic, 2)) %>%  
  ungroup() %>% 
  pivot_longer(-date, names_to = "party", values_to = "values") %>% 
  ggplot(aes(x = date, y = values, col = party)) +
  #geom_line() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = c(49, 51), color = "gray30", alpha = 0.8) +
  scale_color_manual(values = party_colors[c(1, 3)]) +
  #guides(color = FALSE) + 
  labs(x = "", y = "Sentiment (%)", title = "US Elections - Sentiment Trend") +
  theme(axis.text = element_text(size = 15), legend.position = c(0.9, 0.9)) +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())

db_polls %>% 
  #filter(month == "September") %>% 
  rename(state_abbr = code) %>% 
  group_by(date, state_abbr) %>% 
  summarise(democratic = round(weighted.mean(democratic, volume), 2),
            republican = round(100 - democratic, 2)) %>%  
  ungroup() %>% 
  pivot_longer(-c(date, state_abbr), names_to = "party", values_to = "values") %>% 
  ggplot(aes(x = date, y = values, col = party)) +
  #geom_line() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = c(49, 51), color = "gray30", alpha = 0.8) +
  scale_color_manual(values = party_colors[c(1, 3)]) +
  facet_geo(~ state_abbr, grid = "us_state_grid1") +
  guides(color = FALSE) + 
  labs(x = "", y = "Sentiment (%)", title = "US Elections - Sentiment Trend") +
  theme(axis.text = element_text(size = 5)) +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())


dates <- as.Date(c("2020-09-15", "2020-10-07", "2020-10-31"))

db_chance <- db_polls %>%
  compute_margin(party_margin = 2) %>% 
  compute_daily_polls("national") 
db_chance %>% 
  ggplot(aes(x = date, y = proportion)) +
  geom_col(data = filter(db_chance, party == "Uncertain"), aes(fill = party), alpha = 0.8) +
  geom_label_repel(data = filter(db_chance, party == "Uncertain", date %in% dates), 
                   aes(label = proportion, fill = party), 
                   color = "white", size = 3.5, show.legend = FALSE, nudge_x = 2) +
  geom_line(data = filter(db_chance, party %in% c("Democratic", "Republican")), 
            aes(col = party)) +
  geom_smooth(data = filter(db_chance, party %in% c("Democratic", "Republican")),
              aes(col = party), se = FALSE) +
  geom_label_repel(data = filter(db_chance, party %in% c("Democratic", "Republican"), date %in% dates), 
                   aes(label = proportion, col = party), 
                   size = 3.5, show.legend = FALSE, nudge_y = 2) +
  geom_hline(yintercept = 50, color = "gray30", alpha = 0.6) +
  scale_color_manual(values = party_colors[c(1, 3)]) +
  scale_fill_manual(values = party_colors[2]) +
  scale_y_continuous(breaks = c(0, 25, 40, 45, 50, 55, 60, 75, 100),
                     limits = c(0, 100),
                     labels = c("0%", "25%", "40%", "45%", "50%", "55%", "60%", "75%", "100%")) +
  labs(x = "", y = "Chance of Winning (%)", title = "US Elections - Chance of Winning Trend", fill = "") +
  theme(axis.text = element_text(size = 15), legend.position = c(0.8, 0.9)) +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())

db_chance_region <- db_polls %>%
  compute_margin(party_margin = 2) %>% 
  compute_daily_polls("regional") 
db_chance_region %>% 
  ggplot(aes(x = date, y = proportion)) +
  geom_col(data = filter(db_chance_region, party == "Uncertain"), aes(fill = party), alpha = 0.8) +
  geom_label_repel(data = filter(db_chance_region, party == "Uncertain", date %in% dates), 
                   aes(label = proportion, fill = party), 
                   color = "white", size = 3.5, show.legend = FALSE, nudge_x = 2) +
  geom_line(data = filter(db_chance_region, party %in% c("Democratic", "Republican")), 
            aes(col = party)) +
  geom_smooth(data = filter(db_chance_region, party %in% c("Democratic", "Republican")),
              aes(col = party), se = FALSE) +
  geom_label_repel(data = filter(db_chance_region, party %in% c("Democratic", "Republican"), date %in% dates), 
                   aes(label = proportion, col = party), 
                   size = 3.5, show.legend = FALSE, nudge_y = 2) +
  geom_hline(yintercept = 50, color = "gray30", alpha = 0.6) +
  scale_color_manual(values = party_colors[c(1, 3)]) +
  scale_fill_manual(values = party_colors[2]) +
  facet_wrap(~ region) +
  scale_y_continuous(breaks = c(0, 25, 40, 45, 50, 55, 60, 75, 100),
                     limits = c(0, 100),
                     labels = c("0%", "25%", "40%", "45%", "50%", "55%", "60%", "75%", "100%")) +
  labs(x = "", y = "Chance of Winning (%)", title = "US Elections - Chance of Winning Trend", fill = "") +
  theme(axis.text = element_text(size = 15)) +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())



# Map ---------------------------------------------------------------------

usa_geom_feats %>% 
  left_join(db_mean, by = c("name", "code", "region", "electors")) %>% 
  ggplot() +
  geom_sf(aes(fill = -margin)) +
  scale_fill_gradient2(low = party_colors[1], mid = party_colors[2], high = party_colors[3], 
                       breaks = c(-15, -10, -5, 0, 5, 10, 15),
                       labels = paste0(abs(c(-15, -10, -5, 0, 5, 10, 15)), "%"),
                       limits = c(-20, 20)) +
  geom_sf_text(aes(label = code), size = 2) +
  labs(fill = "Margin rate", x = "", y = "", title = "US Elections - Sentiment Margin") +
  coord_sf() +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())

usa_geom_feats %>% 
  left_join(db_mean, by = c("name", "code", "region", "electors")) %>% 
  ggplot() +
  geom_sf(aes(fill = party)) +
  scale_fill_manual(values = party_colors) +
  geom_sf_text(aes(label = code), size = 3) +
  labs(fill = "Party", x = "", y = "", title = "US Elections - Party Sentiment") +
  coord_sf() +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())

m1 <- usa_geom_feats %>% 
  left_join(db_mean, by = c("name", "code", "region", "electors")) %>% 
  ggplot() +
  geom_sf_interactive(aes(tooltip = description_full, fill = -margin)) +
  scale_fill_gradient2(low = party_colors[1], mid = party_colors[2], high = party_colors[3], 
                       breaks = c(-15, -10, -5, 0, 5, 10, 15),
                       labels = paste0(abs(c(-15, -10, -5, 0, 5, 10, 15)), "%"),
                       limits = c(-20, 20)) +
  geom_sf_text(aes(label = code), size = 2) +
  labs(fill = "Margin rate", x = "", y = "") +
  coord_sf() +
  labs(caption = paste("Powered by ", "<img src='data/logo/logonew.png' width='40'/>")) +
  theme(plot.caption = element_markdown())
girafe(ggobj = m1)

