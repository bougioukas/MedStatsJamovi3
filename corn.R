
library(tidyverse)
library(scales)
library(readr)
corn <- read_csv("data/corn.csv")

dat <- corn %>% 
 mutate(group = factor(group, levels = c(1, 2), 
                              labels=c('scalpel', 'corn plaster')),
        centre_a = factor(centre_a, levels = c(1, 2, 3, 4, 5,  9,  11 ),
                          labels = c('Central', 'Manor', 'Jordanthorpe', 'Limbrick',
                                     'First Park', 'Huddersfield', 'Darnall')),
        gender = factor(gender, levels = c(1, 2), 
                              labels=c('male', 'female'))
         
 )

table(corn$centre_a)
names(corn)


centres <- dat %>% 
  count(centre_a) %>% 
  mutate(percent = n / sum(n))

ggplot(centres, aes(x = centre_a, y = percent)) +
  geom_col(width=0.65, fill = "steelblue4") +
  geom_text(aes(label=paste0(round(percent*100, 1), "%")),
            vjust=1.6, color = "white", size = 5) +
  labs(x = "Treatment center", y = "Percent") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    axis.title=element_text(size = 14),
    axis.text=element_text(size = 14)
    
  )




