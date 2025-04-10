library(tidyverse)
library(gridExtra)

f1 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURE_2/roost_sw.csv"
roost_sw <- read_csv(f1, col_names = TRUE)

head(roost_sw)

f2 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURE_2/cluster_sw.csv"
cluster_sw <- read_csv(f2, col_names = TRUE)

head(cluster_sw)

f3 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURE_2/partner_sw.csv"
partner_sw <- read_csv(f3, col_names = TRUE)

head(partner_sw)

#roost
plot1 <- ggplot() +
  geom_histogram(data = roost_sw, 
                 aes(x = roost_sw), 
                 bins = 30,
                 fill = "light blue",
                 colour = "black")+
  geom_vline(data = roost_sw, 
             aes(xintercept = 0.81),
             color = "red",
             linetype = "dashed")+
  xlab("switching rate (switches / day)")+
  ggtitle("(a) roost switching")

#cluster
plot2 <- ggplot() +
  geom_histogram(data = cluster_sw, 
                 aes(x = cluster_sw), 
                 bins = 30,
                 fill = "light blue",
                 colour = "black")+
  geom_vline(data = cluster_sw, 
             aes(xintercept = 4.46),
             color = "red",
             linetype = "dashed")+
  xlab("switching rate (switches / day)")+
  ggtitle("(b) cluster switching")

#partner
plot3 <- ggplot() +
  geom_histogram(data = partner_sw, 
                 aes(x = partner_sw), 
                 bins = 30,
                 fill = "light blue",
                 colour = "black")+
  geom_vline(data = partner_sw, 
             aes(xintercept = 14.46),
             color = "red",
             linetype = "dashed")+
  xlab("switching rate (switches / day)")+
  ggtitle("(c) partner switching")+
  xlim(0, 59.96)+
  ylim(0, 10)

#plot graphs
grid.arrange(plot1, plot2, plot3, ncol = 3)




#############


f4 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/partner-switching/partner_switching.csv"
partner_sw2 <- read_csv(f4, col_names = TRUE)

head(partner_sw2)

partner_sw3 <- partner_sw2 |>
  group_by(date, actor) |>
  summarise(total_switch = sum(switch, na.rm = TRUE)) |>
  group_by(actor) |>
  mutate(mean_total_switch = (total_switch))

# Plot the histogram with log transformation
plot4 <- ggplot() +
  geom_histogram(data = partner_sw3, 
                 aes(x = total_switch), 
                 bins = 30,
                 fill = "light blue",
                 colour = "black")+
  geom_vline(data = partner_sw3, 
             aes(xintercept = 14.46),
             color = "red",
             linetype = "dashed")+
  xlab("switching rate (switches / day)")+
  ggtitle("(c) partner switching")+
  xlim(0, 59.96)+
  ylim(0, 10)
plot4


#################






# Find the number of rows, and the maximum nnumber of rows between the three
r_rows <- nrow(roost_sw)
c_rows <- nrow(cluster_sw)
p_rows <- nrow(partner_sw)
max_row <- max(r_rows,c_rows,p_rows)

# Add NA rows until all datasets are the same length
if (r_rows != max_row) {
  roost_sw[r_rows + (max_row-r_rows),] <- NA
}
if (c_rows != max_row) {
  cluster_sw[c_rows + (max_row-c_rows),] <- NA
}
if (p_rows != max_row) {
  partner_sw[p_rows + (max_row-p_rows),] <- NA
}

# Merge the data and rearrange
merged_data <- cbind(roost_sw,cluster_sw,partner_sw)
merged_data <- merged_data[,-1]
merged_data <- merged_data[,-2]
merged_data <- merged_data[,-2]
merged_data <- merged_data[,-3]
merged_data <- merged_data[,-3]
merged_data <- merged_data[,-4]
colnames(merged_data) <- c("a","b","c")
merged_data$c <- merged_data$c * 24

# Find the mean of each column and add to data
Mean <- data.frame(colMeans(merged_data, na.rm = T))
colnames(Mean) <- 'value'
cols <- c("a","b","c")
Mean <- cbind(cols,Mean)

# Graph labels
label <- c(
  "a" = "(a) roost switching",
  "b" = "(b) cluster switching",
  "c" = "(c) partner switching"
)

# Graph theme
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(strip.background = element_blank(),strip.text = element_text(hjust = 0))

# Plot
ggplot() + 
  geom_histogram(data = gather(merged_data, cols, value), aes(x = value), bins = 30, colour = "black", fill = "light blue") +
  geom_vline(data = Mean, aes(xintercept = value), color = "red", linetype = "dashed") +
  facet_wrap(.~cols, scales = "free", labeller = as_labeller(label), nrow = 1) +
  xlab("switching rate (switches / day)")

######################################################

# figure 3

library(tidyverse)

f4 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURE_3/gd.csv"
gd <- read_csv(f4, col_names = TRUE)

f5 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURE_3/centrality.csv"
cent <- read_csv(f5, col_names = TRUE)

switch <- c(cent$roost_switch,gd$cs,gd$ps2)
groomed <- c(cent$centrality,gd$n,gd$n)

# Initialize type vector
type <- character(length(groomed))

# Define breakpoints
n1 <- length(cent$centrality)
n2 <- length(gd$cs)
n3 <- length(type)

# Assign types based on index ranges
type[1:n1] <- "a"
type[(n1+1):(n1+n2)] <- "b"
type[(n1+n2+1):n3] <- "c"

# Combine into dataframe
gd2 <- data.frame(type = type, switch = as.numeric(switch), groomed = as.numeric(groomed))

label <- c(
  "a" = "Roost Switching (switches / day)",
  "b" = "Cluster Switching (switches / day)",
  "c" = "Partner Switching (switches / hour)"
)

# Plot
ggplot(data = gd2, mapping =  aes(x = switch, y = groomed, color = type)) + 
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family="quasipoisson")) +
  ylab("Count of Partners Groomed")  +
  facet_wrap(~type, nrow = 1, scales = "free", strip.position = "bottom", labeller = as_labeller(label))







# figure 3
# line with  95% CI on slope... y is count of partners groomed
# (a) root switching
# (b) cluster switching
# (c) partner switching

library(asnipe)
library(tidyverse)
library(igraph)
library(stringi)
library(glmmTMB)
library(MASS)
library(performance)
rm(list = ls())

# Load centrality and switching rate data
gd <- read.csv("/Users/joy237/Desktop/ADA-REPO/Data-Analysis-Replication/gd.csv")
centrality <- read.csv("/Users/joy237/Desktop/ADA-REPO/Data-Analysis-Replication/centrality.csv")

# Make two lists of switching rate and grooming degree
switch <- c(centrality$roost_switch,gd$cs,gd$ps2)
count_groomed <- c(centrality$centrality,gd$n,gd$n)

# Make a third list of the type of movement
type <- rep(0,length(groomed))
for (i in 1:length(type)) {
  if (i <= length(centrality$centrality)) {
    type[i] <- "a" # Denote where roost switching data is stored
  } else {
    if (i <= (length(centrality$centrality)+length(gd$cs))) {
      type[i] <- "b" # Denote where cluster switching data is stored
    } else {
      if (i <= (length(centrality$centrality)+length(gd$cs)*2)) {
        type[i] <- "c" # Denote where partner switching data is stored
      }
    }
  }
}

# Combine lists into datafeame and make sure theyh are numeric
gd2 <- data.frame(cbind(type,switch,groomed))
gd2$switch <- as.numeric(gd2$switch)
gd2$groomed <- as.numeric(gd2$groomed)

# Graph label
label <- c(
  "a" = "Roost Switching (switches / day)",
  "b" = "Cluster Switching (switches / day)",
  "c" = "Partner Switching (switches / hour)"
)

# Graph theme
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(legend.position = "none", strip.text = element_text(size = 16), strip.placement = "outside", axis.title.x=element_blank(), strip.background = element_blank(), axis.title.y = element_text(size = 16))

# Plot
ggplot(data = gd2, mapping =  aes(x = switch, y = groomed, color = type)) + 
  geom_point() +
  geom_smooth(method = 'glm', method.args = list(family="quasipoisson")) +
  ylab("Count of Partners Groomed")  +
  facet_wrap(~type, nrow = 1, scales = "free", strip.position = "bottom", labeller = as_labeller(label))









# Run permutation tests and get statistic metrics, relationship metrics
#model <- glm(centrality~scale(roost_switch), family = "poisson", data=centrality) # Get the roost switching linear model
overdisp_fun <- function(model) { # Check for dispersion
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
#overdisp_fun(model)
model <- glmmTMB(centrality ~ scale(roost_switch), family = "nbinom1", data = centrality)
summary(model) # Look at statstics associated with the model
#1 - (model$deviance/model$null.deviance)
rootb <- rep(0,1000) # Initialize vector
for (i in 1:1000) { # Run 1000 permutations and find the R^2 value
  data2 <- centrality[sample(nrow(centrality),size = nrow(centrality),replace = T),]
  a <- summary(glmmTMB(centrality ~ scale(roost_switch), family = "nbinom1", data = data2))
  b <- a$coefficients[1]
  c <- data.frame(b)
  rootb[i] <- c$cond.Estimate[2]
}
quantile(rootb, probs = c(0.025,0.975)) # Find where 95% of the beta values lie
#model <- glm(n~scale(cs), family = "poisson", data=gd) # Get the cluster switching linear model
#overdisp_fun(model)
#1 - (model$deviance/model$null.deviance)
gd$scs <- as.numeric(scale(gd$cs))
model <- glmmTMB(n ~ scs, family = "nbinom1", data = gd)
summary(model) # Look at statistics
cootb <- rep(0,1000)
for (i in 1:1000) { # Run 1000 permutations and find the R^2 value
  data2 <- gd[sample(nrow(gd),size = nrow(gd),replace = T),]
  a <- summary(glmmTMB(n ~ scs, family = "nbinom1", data = data2))
  b <- a$coefficients[1]
  c <- data.frame(b)
  cootb[i] <- c$cond.Estimate[2]
}
quantile(cootb, probs = c(0.025,0.975)) # Find where 95% of the beta values lie
#model <- glm(n~scale(ps2), family = "poisson", data=gd)  # Get the within-cluster partner switching linear model
#overdisp_fun(model)
model <- glmmTMB(n ~ scale(ps2), family = "nbinom1", data = gd)
summary(model) # View statistics
pootb <- rep(0,1000)
for (i in 1:1000) { # # Run 1000 permutations and find the R^2 value
  data2 <- gd[sample(nrow(gd),size = nrow(gd),replace = T),]
  a <- summary(glmmTMB(n ~ scale(ps2), family = "nbinom1", data = data2))
  b <- a$coefficients[1]
  c <- data.frame(b)
  pootb[i] <- c$cond.Estimate[2]
}
quantile(pootb, probs = c(0.025,0.975)) # Find where 95% of the beta values lie
# Get statistics for the relation between cluster switching and partner switching
model <- glm(scale(cs)~scale(ps2), data=gd)
summary(model) # View statistics
poot <- rep(0,1000) # Initialize vector
pootb <- rep(0,1000)
for (i in 1:1000) { # # Run 1000 permutations and find the R^2 value
  data2 <- gd[sample(nrow(gd),size = nrow(gd),replace = T),]
  poot[i] <- summary(lm(scale(cs) ~ scale(ps2), data = data2))$r.squared
  pootb[i] <- summary(lm(scale(cs) ~ scale(ps2), data = data2))$coefficients[2]
}
quantile(poot, probs = c(0.025,0.975)) # Find where 95% of the R^2 values lie
quantile(pootb, probs = c(0.025,0.975)) # Find where 95% of the beta values lie
lm(data.frame(scale(model$model)))

######################################################

# Figure 4

library(tidyverse)

f6 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est0.csv"
degree_est00 <- read_csv(f6, col_names = TRUE) |>
  mutate(label= "(a) individually variable rates of switching roost, cluster, and partner across 200 bats")

f7 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est0.csv"
degree_est0 <- read_csv(f7, col_names = TRUE) |>
  mutate(label= "individually variable rates of switching roost, cluster, and partner across 200 bats")

f8 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est1.csv"
degree_est1 <- read_csv(f8, col_names = TRUE) |>
  mutate(label= "(a) individually variable rates of roost switching with 200 bats")

f9 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est2.csv"
degree_est2 <- read_csv(f9, col_names = TRUE) |>
  mutate(label= "(b) individually variable rates of cluster switching with 200 bats")

f10 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est3.csv"
degree_est3 <- read_csv(f10, col_names = TRUE) |>
  mutate(label= "(c) individually variable rates of partner switching with 200 bats")
  
f11 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est4.csv"
degree_est4 <- read_csv(f11, col_names = TRUE) |>
  mutate(label= "individually variable switching rates correlated across types for 200 bats")

f12 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est5.csv"
degree_est5 <- read_csv(f12, col_names = TRUE) |>
  mutate(label= "(a) individually variable rates of switching roost, cluster, and partner across 100 bats")

f13 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est6.csv"
degree_est6 <- read_csv(f13, col_names = TRUE) |>
  mutate(label= "(b) individually variable rates of roost switching with 100 bats")

f14 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est7.csv"
degree_est7 <- read_csv(f14, col_names = TRUE) |>
  mutate(label= "(c) individually variable rates of cluster switching with 100 bats")

f15 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est8.csv"
degree_est8 <- read_csv(f15, col_names = TRUE)|>
  mutate(label= "(d) individually variable rates of partner switching with 100 bats")

f16 <- "https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est9.csv"
degree_est9 <- read_csv(f16, col_names = TRUE) |>
  mutate(label= "(b) individually variable switching rates correlated across types for 100 bats")


d0 <- degree_est00
d1 <- rbind(degree_est1,degree_est2,degree_est3)
d2 <- rbind(degree_est0,degree_est1,degree_est2,degree_est3,degree_est4)
d3 <- rbind(degree_est5,degree_est6,degree_est7,degree_est8,degree_est9)


# plot figure 4
d0 %>% pivot_longer(scale.rs.:scale.ps.,
                    names_to = "t",
                    values_to = "coefficient") %>%
  # put them in order from big to small
  mutate(
    type = case_when(
      t == "scale.rs." ~ "1. roost switching",
      t == "scale.cs." ~ "2. cluster switching",
      t == "scale.ps." ~ "3. partner switching")) %>%
  mutate(type= fct_rev(factor(type))) %>% 
  ggplot(aes(x = type, y = coefficient, color = type)) +
  facet_wrap(~label, ncol=1)+
  geom_violin(width = 0.4) +
  geom_boxplot(width = 0.1, outlier.size = 0.25) +
  coord_flip()+
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0)) +
  theme(legend.position = "none") +
  xlab("") +
  ylab("effect on grooming degree (standardized coefficient)") +
  scale_color_manual(values= c("blue", "dark green", "red"))













base_deg_roost1 <-
  median(degree_est2$scale.rs.)
base_deg_roost2 <-
  median(degree_est3$scale.rs.)
base_deg_cluster1 <-
  median(degree_est1$scale.cs.)
base_deg_cluster2 <-
  median(degree_est3$scale.cs.)
base_deg_partner1 <-
  median(degree_est1$scale.ps.)
base_deg_partner2 <-
  median(degree_est2$scale.ps.)














# Figure 4 / Figure 5

rm(list = ls())

# load packages
library(igraph)
library(tidyverse)

# Load data
# degree_est0a <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est0.csv") %>%
#   mutate(label= "(a) individually variable rates of switching roost, cluster, and partner across 200 bats")
# degree_est0 <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est0.csv") %>%
#   mutate(label= "individually variable rates of switching roost, cluster, and partner across 200 bats")
# degree_est1 <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est1.csv") %>%
#   mutate(label= "(a) individually variable rates of roost switching with 200 bats")
# degree_est2 <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est2.csv") %>%
#   mutate(label= "(b) individually variable rates of cluster switching with 200 bats")
# degree_est3 <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est3.csv") %>%
#   mutate(label= "(c) individually variable rates of partner switching with 200 bats")
# degree_est4 <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est4.csv") %>%
#   mutate(label= "individually variable switching rates correlated across types for 200 bats")
# degree_est5 <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est5.csv") %>%
#   mutate(label= "(a) individually variable rates of switching roost, cluster, and partner across 100 bats")
# degree_est6 <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est6.csv") %>%
#   mutate(label= "(b) individually variable rates of roost switching with 100 bats")
# degree_est7 <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est7.csv") %>%
#   mutate(label= "(c) individually variable rates of cluster switching with 100 bats")
# degree_est8 <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est8.csv") %>%
#   mutate(label= "(d) individually variable rates of partner switching with 100 bats")
# degree_est9 <- read.csv("https://raw.githubusercontent.com/anonymousscientist8/scales-of-movement/refs/heads/main/graphs_and_analyses/FIGURES_4_5_S2_S3/degree_est9.csv") %>%
#   mutate(label= "(b) individually variable switching rates correlated across types for 100 bats")

# compile data
# d0 <- degree_est00
# d1 <- rbind(degree_est1,degree_est2,degree_est3)
# d2 <- rbind(degree_est0,degree_est1,degree_est2,degree_est3,degree_est4)
# d3 <- rbind(degree_est5,degree_est6,degree_est7,degree_est8,degree_est9)

# get reference effects across both population sizes
# 200
# base_deg_roost1 <-
#   median(degree_est2$scale.rs.)
# base_deg_roost2 <-
#   median(degree_est3$scale.rs.)
# base_deg_cluster1 <-
#   median(degree_est1$scale.cs.)
# base_deg_cluster2 <-
#   median(degree_est3$scale.cs.)
# base_deg_partner1 <-
#   median(degree_est1$scale.ps.)
# base_deg_partner2 <-
#   median(degree_est2$scale.ps.)
# 
# # plot figure 4
# d0 %>% pivot_longer(scale.rs.:scale.ps.,
#                     names_to = "t",
#                     values_to = "coefficient") %>%
#   # put them in order from big to small
#   mutate(
#     type = case_when(
#       t == "scale.rs." ~ "1. roost switching",
#       t == "scale.cs." ~ "2. cluster switching",
#       t == "scale.ps." ~ "3. partner switching")) %>%
#   mutate(type= fct_rev(factor(type))) %>% 
#   ggplot(aes(x = type, y = coefficient, color = type)) +
#   facet_wrap(~label, ncol=1)+
#   geom_violin(width = 0.4) +
#   geom_boxplot(width = 0.1, outlier.size = 0.25) +
#   coord_flip()+
#   theme_bw() +
#   theme(strip.text.x = element_text(angle = 0, hjust = 0)) +
#   theme(legend.position = "none") +
#   xlab("") +
#   ylab("effect on grooming degree (standardized coefficient)") +
#   scale_color_manual(values= c("blue", "dark green", "red"))

# plot figure 5
d1 %>%
  pivot_longer(scale.rs.:scale.ps.,
               names_to = "t",
               values_to = "coefficient") %>%
  # put them in order from big to small
  mutate(
    type = case_when(
      t == "scale.rs." ~ "1. roost switching",
      t == "scale.cs." ~ "2. cluster switching",
      t == "scale.ps." ~ "3. partner switching")) %>%
  mutate(type= fct_rev(factor(type))) %>%
  ggplot(aes(x = type, y = coefficient, color = type)) +
  facet_wrap(~label, ncol=1)+
  geom_violin(width = 0.4) +
  geom_boxplot(width = 0.1, outlier.size = 0.25) +
  geom_hline(yintercept = max(c(base_deg_roost1,base_deg_roost2)), linetype = 'dashed', color = 'red') +
  geom_hline(yintercept = max(c(base_deg_cluster1,base_deg_cluster2)), linetype = 'dashed', color = 'dark green') +
  geom_hline(yintercept = max(c(base_deg_partner1,base_deg_partner2)), linetype = 'dashed', color = 'blue') +
  coord_flip()+
  # theme_bw() +
  # theme(strip.text.x = element_text(angle = 0, hjust = 0)) +
  theme(legend.position = "none") +
  xlab("") +
  ylab("effect on grooming degree (standardized coefficient)")
# scale_color_manual(values= c("blue", "dark green", "red"))

