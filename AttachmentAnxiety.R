library(tidyverse)
library(magrittr)
set.seed(2)
n <- 300
d <- tibble(Attachment = factor(sample(1:3,
                                       size = n,
                                       replace = T),
                                labels = c("Secure","Avoidant","Ambivalent")),
            Anxiety_Physiological = rnorm(n, ifelse(Attachment == "Secure",50,65), 10),
            Anxiety_Questionnaire = Anxiety_Physiological + ifelse(Attachment == "Avoidant",-20,0) + rnorm(n,0,3)
            ) %>% 
  gather(key = "Measure", value = "Anxiety", -Attachment) %>% 
  mutate(Measure = factor(Measure, labels = c("Physiological","Questionnaire")) %>% 
           forcats::fct_rev())

readr::write_csv(d, "AttachmentAnxiety.csv")
ggplot(d, aes(Attachment, Anxiety)) +
  geom_violin(color = NA, aes(fill = Attachment),show.legend = F, alpha = 0.3) + 
  stat_summary() + 
  stat_summary(geom = "line", aes(group = 1)) + 
  facet_grid(Measure ~ . ) + 
  theme_grey(base_size = 16, "serif")
library(ggbeeswarm)
ggplot(d, aes(Attachment, Anxiety, color = Attachment)) +
  geom_quasirandom() +
  stat_summary(aes(group = 1)) + 
  stat_summary(geom = "line", aes(group = 1, color = NULL)) + 
  facet_grid(Measure ~ . ) + 
  theme_grey(base_size = 16, "serif")


d_Q <- d %>% filter(Measure == "Questionnaire")

meanAnxiety <- mean(d_Q$Anxiety)
mGroup <- d_Q %>% group_by(Attachment) %>% summarise(mAnxiety = mean(Anxiety))

d_Q %>% 
  left_join(mGroup, by = "Attachment") %>% 
  mutate(offX = vipor::offsetX(d_Q$Anxiety,d_Q$Attachment, method = "quasirandom")) %>% 
  ggplot(aes(color = Attachment)) +
  geom_segment(aes(x = as.numeric(Attachment) + offX, 
                   xend = as.numeric(Attachment) + offX,
                   y = mAnxiety,
                   yend = Anxiety,
                   color = Attachment),
               alpha = 0.25) + 
  geom_point(aes(as.numeric(Attachment) + offX, Anxiety), size = 0.5) + 
  theme_grey(base_size = 16, "serif") + 
  theme(legend.position = "none") + 
  geom_segment(data = mGroup,
               aes(x = as.numeric(Attachment) - 0.45, 
                   xend = as.numeric(Attachment) + 0.45, 
                   y = mAnxiety, 
                   yend = mAnxiety)) + 
  geom_segment(data = mGroup, 
               aes(x = as.numeric(Attachment), 
                   y = mAnxiety,
                   xend = as.numeric(Attachment),
                   yend = meanAnxiety),
               lwd = 1.5) + 
  geom_hline(yintercept = mAnxiety) + 
  ylab("Anxiety") + 
  scale_x_continuous("Attachment",
                     breaks = 1:3, 
                     labels = d$Attachment %>% levels,
                     minor_breaks = NULL) -> ggBetweenWithin

d_Q %>% 
  left_join(mGroup, by = "Attachment") %>% 
  mutate(offX = vipor::offsetX(d_Q$Anxiety,d_Q$Attachment, method = "quasirandom")) %>% 
  ggplot(aes(color = Attachment)) +
  geom_segment(aes(x = as.numeric(Attachment) + offX, 
                   xend = as.numeric(Attachment) + offX,
                   y = meanAnxiety,
                   yend = Anxiety,
                   color = Attachment),
               alpha = 0.25) + 
  geom_point(aes(as.numeric(Attachment) + offX, Anxiety), size = 0.5) + 
  theme_grey(base_size = 16, "serif") + 
  theme(legend.position = "none") + 
  geom_segment(data = mGroup,
               aes(x = as.numeric(Attachment) - 0.45, 
                   xend = as.numeric(Attachment) + 0.45, 
                   y = mAnxiety, 
                   yend = mAnxiety)) + 
  geom_hline(yintercept = mAnxiety) + 
  ylab("Anxiety") + 
  scale_x_continuous("Attachment",
                     breaks = 1:3, 
                     labels = d$Attachment %>% levels,
                     minor_breaks = NULL) -> ggBetweenWithin

ggplot(d %>% filter(Measure == "Questionnaire"), aes(Attachment, Anxiety)) +
  geom_violin(color = NA, aes(fill = Attachment),show.legend = F, alpha = 0.3) + 
  stat_summary() + 
  stat_summary(geom = "line", aes(group = 1)) + 
  theme_grey(base_size = 16, "serif")


library(officer)
library(rvg)
read_pptx("C:/Users/renee/Box Sync/Courses/Introduction to Statistics/Educ5325-18 ANOVA.pptx") %>% 
  add_slide(layout = "Title and Content", master = "Slate") %>% 
  ph_with_vg_at(code = print(ggBetweenWithin), 
                height = 7.5, 
                width = 9.33, 
                left = 0, 
                top = 0) %>% 
  print(target = "New.pptx") %>% 
  invisible()
pander::openFileInOS("New.pptx")


  
d_Q %>% left_join(mGroup, by = "Attachment") %>% 
  mutate(SS_W = (Anxiety - mAnxiety) ^ 2) %$%
  SS_W %>% 
  sum -> SS_W
d_Q %>% left_join(mGroup, by = "Attachment") %>% 
  mutate(SS_T = (Anxiety - meanAnxiety) ^ 2) %$%
  SS_T %>% 
  sum -> SS_T
SS_B <- SS_T - SS_W

k = 3
n = nrow(d_Q)
MS_B = SS_B / (k - 1)

MS_W = SS_W / (n - k)
Ftest <- MS_B / MS_W

aov(Anxiety ~ Attachment, d_Q) %>% summary


d_e <- d_Q %>% 
  left_join(mGroup, by = "Attachment") %>% 
  mutate(OverallMean = meanAnxiety,
         e_B = mAnxiety - OverallMean,
         e_W = Anxiety - mAnxiety,
         e_T = Anxiety - OverallMean)
sum(d_e$e_B ^ 2)
sum(d_e$e_W ^ 2)
sum(d_e$e_T ^ 2) / (n - 1)
SS_T / (n - 1)
MS_B + MS_W
