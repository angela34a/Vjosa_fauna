cbind(as.data.frame(shannondiv), as.factor(all_data$River)) %>% 
  as.data.frame() %>%
  rename("div"= "shannondiv","river"="as.factor(all_data$River)") %>%
  dplyr::filter(div!="0") %>% 
  ggplot( aes(x = river, y = div, group=river), fill = "gray") +
  geom_jitter(width = 0.2, size = 3, 
              shape=21, alpha = 0.3,
              fill = "gray", 
              color = "black") +
  geom_boxplot( alpha=0.5, outlier.shape = NA, fill ="gray") + 
  theme_bw()+
  theme(panel.grid = element_blank(),
    legend.position = "none",
        axis.title=element_text(size=11,face="bold"),
        #axis.text.x = element_text(angle = 90)
        axis.text.y = element_text(size=8,face="bold")) +
  labs(x = "River",
       y = "Shannon's diversity index",
       title = "Kruskal-Wallis, p = 0.64") -> plot_1_sh 




cbind(as.data.frame(richness), as.factor(all_data$River)) %>% 
  as.data.frame() %>%
  rename("div"= "richness","river"="as.factor(all_data$River)") %>%
  dplyr::filter(div!="0") %>% 
  ggplot( aes(x = river, y = div, group=river), fill = "gray") +
  geom_jitter(width = 0.2, size = 3, 
              shape=21, alpha = 0.3,
              fill = "gray", 
              color = "black") +
  geom_boxplot( alpha=0.5, outlier.shape = NA, fill ="gray") + 
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.title=element_text(size=11,face="bold"),
        #axis.text.x = element_text(angle = 90)
        axis.text.y = element_text(size=8,face="bold")) +
  labs(x = "River",
       y = "Richness",
       title = "Anova, p = 0.331") -> plot_1_ri

plot_1_ri + plot_1_sh


c27 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "violet", # orange
  "beige", "gold1",
  "palegreen2",
  "deeppink1",
 "#FB9A99", # lt pink
  "#CAB2D6", # lt purple
 "darkturquoise",
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon","yellow4", "orchid1",  "blue1", 
  "skyblue2", "green1",  "yellow3",
  "darkorange4","steelblue4",  "brown",
  "blue4", "#FF7F00"
)

all_data[,1:30]    %>%
  
  select(!c("SampleID", "Date")) %>% 
  mutate(River = all_data$River) %>% 
  pivot_longer(cols=-c("Month", "River"), 
               values_to = "abundance", 
               names_to = "species") %>%
  
  
  # to find relative abundance within each river  
  dplyr::group_by(Month,River) %>%
  mutate (rel_abund = abundance / sum(abundance)) %>%
  ungroup() %>%
  
  # mutate in a way to find average
 left_join(., sample_sizes, by = c("Month", "River")) %>% 
  dplyr::group_by(Month,River) %>%
  mutate (mean_abund = abundance / sample_size) %>%
  ungroup() %>% 

  # plot
  ggplot(aes(x = River, y = mean_abund, fill = species)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~Month, scales= "free_x") +
  theme_bw() +
  scale_fill_manual(values=c27) +
  theme(panel.grid = element_blank(), 
        strip.text = element_text(size = 11),
        legend.position="right",
        legend.text = element_text(size = 8),      # Adjust the text size
        legend.title = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        axis.title=element_text(size=11,face="bold"),
        axis.text.y = element_text(size=9),
        axis.text.x = element_text(angle = 90)) + 
  labs(y = "Mean abundance per number of samples", 
       fill = "Class") +
  guides(fill=guide_legend(ncol = 1))

sample_sizes <-   all_data %>% 
  group_by(Month, River) %>% 
    summarise(sample_size = n())
  
