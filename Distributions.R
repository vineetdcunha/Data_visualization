library(ggplot2)

library(ggforce)

# Problem 1

# section a)
perplot <- PerceptionExperiment %>%
  ggplot(aes(x = factor(Test), y = Error))

perplot  + geom_violin() + geom_sina() + theme_bw() +
  labs(x = "Test",
       y = "Error",
       title = "Error vs. Test")


PerceptionExperiment$AbsError = abs(PerceptionExperiment$Error)

# section b)

install.packages('ggbeeswarm')

library(ggbeeswarm)

perplot2 <- PerceptionExperiment %>%
  ggplot(aes(x = factor(Test), y = AbsError))

perplot2  +   geom_violin() + geom_sina() + theme_bw() +
  labs(x = "Test",
       y = "Absolute Error",
       title = "Absolute Error vs. Test")

# section c)

library(dplyr)

FilteredPerceptionExperiment <-
  PerceptionExperiment %>% filter(between(Subject, 56, 73))


perplot5673 <- FilteredPerceptionExperiment %>%
  ggplot(aes(x = factor(Display), y = Response))

perplot5673 + geom_point() + theme_bw() +
  labs(x = "Display",
       y = "Response",
       title = "Response vs. Display")

# section d)

FilteredPerceptionExp <-
  PerceptionExperiment %>% filter(Test == "Vertical Distance, Non-Aligned") %>% filter(between(Subject, 56, 73))

FilteredPerceptionExp %>%
  ggplot(aes(x = factor(Trial), y = Response)) + geom_violin() + geom_jitter(col = ifelse(between(FilteredPerceptionExp$Subject, 56, 73) & FilteredPerceptionExp$Response == 1, "red", "black"),show.legend=TRUE) + theme_bw() +
  labs(x = "Trial",
       y = "Response",
       title = "Response vs. Trial")

FilteredPerceptionExp %>%
  ggplot(aes(x = factor(Test), y = Response)) + geom_violin() + geom_jitter(col = ifelse(between(FilteredPerceptionExp$Subject, 56, 73) & FilteredPerceptionExp$Response == 1, "red", "black"),show.legend=TRUE) + theme_bw() +
  labs(x = "Test",
       y = "Response",
       title = "Response vs. Test")
