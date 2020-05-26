# Basic setting & load data
library(devtools)
library(wesanderson)
library(viridis)
library(tidyverse)
library(ggrepel)
library(lme4)
library(lmerTest)

# Load data
wvs6 <- readRDS("Data/wvs6.rds")

# Data cleasning
dat <- wvs6 %>% 
  filter(V2 == 840) %>% 
  select(V1, V2, V24, V102, V103, V104, V105, V106, V107, V256, V256B, V262)

state <- read_csv("Data/state_code.csv")
head(state)

dat <- merge(dat, state, by = "V256B", all.x = TRUE)
length(dat$V256B[is.na(dat$V256B)])

count_state <- dat %>% 
  group_by(state) %>% 
  count()

count_state <- count_state %>% 
  mutate(idx = ifelse(n >= 30, 1, 0))

nrow(count_state[count_state$idx == 1,])

count_state <- count_state[,-4]
count_state <- count_state %>% 
  mutate(idx2 = ifelse(n >= 50, 1, 0))

nrow(count_state[count_state$idx2 == 1,])



# Change column name
head(dat)
head(count_state)
dat <- dat %>% 
  group_by(state) %>% 
  mutate(n_state = n())

dat <- dat[, -c(1,2,3)]
head(dat)
colnames(dat) <- c("trust_most", "trust_family", "trust_neighbor", "trust_ind",
                    "trust_first", "trust_religion", "trust_nation", "region_code", "survey_year", "state", "n_state")

dat2 <- dat %>%
  drop_na() %>% 
  select(state, trust_most, trust_family, trust_neighbor, trust_ind, trust_first, trust_religion, trust_nation)

# Remove all negative values and recode "trust_most" to 0-1
dat2 <- dat2 %>% 
  mutate_all(funs(ifelse(. < 0, NA, .))) %>% 
  mutate(trust_most = ifelse(trust_most == 2, 0, trust_most))

# Recode trust level reversely
dat2 <- dat2 %>% 
  drop_na() %>% 
  mutate_at(vars(3:8), funs(ifelse(.==4, 1, 
                                    ifelse(.==3, 2,
                                           ifelse(.==2, 3, 4)))))

# Generate ingroup and outgroup trust constructs
head(dat2)
psych::alpha(dat2[,3:5])[1]
psych::alpha(dat2[,6:8])[1]

dat3 <- dat2 %>% 
  mutate(trust_ingroup= trust_family + trust_neighbor + trust_ind,
         trust_outgroup = trust_first + trust_religion + trust_nation)

# Multilevel regression (2 levels)
fit1 <- lmer(trust_most ~ trust_ingroup + trust_outgroup + (1|state), data = dat3)
summary(fit1)
fit2 <- lmer(trust_most ~ trust_ingroup + trust_outgroup + (1 + trust_ingroup + trust_outgroup|state), data = dat3)
summary(fit2)

ranefs <- ranef(fit2)
ranefs <- data.frame(ranefs$state)
ranefs$state <- rownames(ranefs)

# Generate Trust Radius
df_radius <- ranefs %>% 
  mutate(trust_radius = trust_outgroup - trust_ingroup)
head(df_radius)

# Calculate TR score

df_radius <- df_radius %>% 
  mutate(trscore = (trust_radius - min(trust_radius))/(max(trust_radius) - min(trust_radius))*10)
head(df_radius)


library(ggrepel)

# Trust level by country and each wave
head(dat3)
dat3 %>% 
  select(state, trust_most) %>% 
  group_by(state) %>% 
  summarise(trust_level = mean(trust_most)*10) -> df_level

head(df_radius)
head(df_level)

dat4 <- merge(df_radius, df_level, by = "state", all.x = TRUE)

head(dat4)
# Plot for trust level vs. trust radius
dat4 %>% 
  ggplot(aes(trscore, trust_level)) + geom_point() + theme_bw() +
  geom_text_repel(aes(label = state), segment.size  = 0.2, color = "grey50", nudge_x = .15) +
  labs(x = "Trust Radius (Standardized 0 to 10)", y = "Trust Level (%)")
head()
head(dat3)
df2 <- merge(df2, gdppc_w5[,c(1,3)], by = "country", all.x = TRUE)



df2[df2$country != "Taiwan",] %>% 
  ggplot(aes(trscore.w6, trustlevel.w6*10)) + geom_point() + theme_bw() +
  geom_text_repel(aes(label = country), segment.size  = 0.2, color = "grey50", nudge_x = .15) +
  labs(x = "Trust Radius (Standardized 0 to 10)", y = "Trust Level (%)")

head(df3.w5)
df3.w5 %>% 
  ggplot(aes(trscore.w5, trustlevel.w5)) + geom_point() + theme_bw() +
  geom_text_repel(aes(label = country), segment.size  = 0.2, segment.color = "grey50", nudge_x = .15)

df3.w6 %>% 
  ggplot(aes(trscore.w6, trustlevel.w6)) + geom_point() + theme_bw() +
  geom_text_repel(aes(label = country), segment.size  = 0.2, segment.color = "grey50", nudge_x = .15)

df2 %>% 
  ggplot(aes(trscore.w6, trustlevel.w6)) + geom_point() + theme_bw() +
  geom_text_repel(aes(label = country), segment.size  = 0.2, segment.color = "grey50", nudge_x = .15) +
  geom_smooth()

head(df2)
df2 %>% 
  select(country, outgroup.w5, outgroup.w6) %>% 
  reshape(varying = c("outgroup.w5", "outgroup.w6"), direction = "long", idvar = "country", v.names = "outgroup", timevar = "wave") -> outgroup

df2 %>% 
  select(country, ingroup.w5, ingroup.w6) %>% 
  reshape(varying = c("ingroup.w5", "ingroup.w6"), direction = "long", idvar = "country", v.names = "ingroup", timevar = "wave") -> ingroup

df2 %>% 
  select(country, trscore.w5, trscore.w6) %>% 
  reshape(varying = c("trscore.w5", "trscore.w6"), direction = "long", idvar = "country", v.names = "trscore", timevar = "wave") -> trscore

df2 %>% 
  select(country, trustlevel.w5, trustlevel.w6) %>% 
  reshape(varying = c("trustlevel.w5", "trustlevel.w6"), direction = "long", idvar = "country", v.names = "trustlevel", timevar = "wave") -> trustlevel

head(outgroup)
head(ingroup)

dat <- merge(outgroup, ingroup, by = c("country", "wave"))
dat <- merge(dat, trscore, by = c("country", "wave"))
dat <- merge(dat, trustlevel, by = c("country", "wave"))
head(dat)
head(wvs)
unique(wvs5$survey_year)

# Add survey_year by country and wave
head(wvs)
df.survey <- wvs %>% 
  select(country, wave, survey_year) %>% 
  group_by(wave, country) %>% 
  summarise(survey = mean(survey_year)) %>% 
  ungroup()

df.survey <- df.survey %>% 
  mutate(wave = ifelse(wave==5, 1, 2))

dat <- merge(dat, df.survey, by = c("country", "wave"), all.x = TRUE)
head(dat)

write.csv(dat, "dat_1.csv")

# Reload dat_1 file after revising country name for merging with World Bank data
dat <- read_csv("dat_1.csv")
dat2 <- read_csv("country_level_dta_20200418.csv")

head(dat)
head(dat2)
dat <- merge(dat, dat2, by.x = c("country", "survey"), by.y = c("country", "year"), all.x = TRUE)

write.csv(dat, "dat_2.csv")

# Reload dat_2.csv after imputing missing value
library(plm)
dat2 <- read_csv("dat_2.csv")
head(dat2)

# hdi
hdi <- read_csv("Data/hdi.csv")
head(hdi)
hdi <- gather(hdi, year, hdi, `1990`:`2018`)
head(dat2)

dat3 <- merge(dat2, hdi, by.x = c("country", "survey"), by.y = c("country", "year"), all.x = TRUE)

# Urban population (%)
urban <- read_csv("Data/urban.csv")
head(urban)
urban <- gather(urban, year, urban_pop, `2002`:`2018`)

dat3 <- merge(dat3, urban, by.x = c("country", "survey"), by.y = c("country", "year"), all.x = TRUE)

# Old population (%)
old <- read_csv("Data/old.csv")
head(old)
old <- gather(old, year, old_pop, `2001`:`2018`)
dat3 <- merge(dat3, old, by.x = c("country", "survey"), by.y = c("country", "year"), all.x = TRUE)

# Income share by top 10%
inc_10 <- read_csv("Data/inc_10.csv")
head(inc_10)
inc_10 <- gather(inc_10, year, inc_10, `1995`:`2018`)
dat3 <- merge(dat3, inc_10, by.x = c("country", "survey"), by.y = c("country", "year"), all.x = TRUE)

write.csv(dat3, "dat3.csv")
dat3 <- read_csv("dat3.csv")
###################3
# Panel Analysis

pm1 <- plm(growth ~ gini + hc + gdppc + governance + inflation, index = c("country", "wave"), model = "within", data = dat3)
summary(pm1)
hist(dat3$gini)
pm1 <- plm(growth ~ gini + log(gdppc) + governance + urban_pop + old_pop + hdi, index = c("country", "wave"), model = "within", data = dat3)
summary(pm1)

ols1 <- lm(growth ~ gini + log(gdppc) + governance + urban_pop + old_pop + hdi, data = dat3[dat3$wave==2,])
summary(ols1)

pm2 <- plm(governance ~ growth + gdppc + gini + trscore, index = c("country", "wave"), model = "within", data = dat3)
summary(pm2)

pm2 <- plm(governance ~ growth + trscore + gdppc + urban_pop + old_pop, index = c("country", "wave"), model = "within", data = dat3)
summary(pm2)


pm1 <- plm(growth ~ gini + log(gdppc) + governance + urban_pop + old_pop + hdi, index = c("country", "wave"), model = "within", data = dat3)

pm1 <- plm(growth ~ gini + governance + gdppc, index = c("country", "wave"), model = "within", data = dat2)
summary(pm1)

pm1.random <- plm(growth ~ gini + governance + gdppc, index = c("country", "wave"), model = "random", data = dat2)
summary(pm1.random)

pm2 <- plm(growth ~ gini + trscore + governance + gdppc, index = c("country", "wave"), model = "within", data = dat2)
summary(pm2)


pm1 <- plm(growth ~ hc, index = c("country", "wave"), model = "within", data = dat2)
summary(pm1)


#######
pm1 <- plm(growth ~ gini + governance + log(gdppc), index = c("country", "wave"), model = "within", data = dat3)
summary(pm1)
hist(dat3$growth)
hist(dat3$gdppc)
hist(log(dat3$gdppc))
hist(dat3$governance)
hist(dat3$hdi)

pm1 <- plm(growth ~ gini + governance + gdppc + urban_pop, index = c("country", "wave"), model = "within", data = dat3)
summary(pm1)

pm1 <- plm(growth ~ governance + gdppc + urban_pop, index = c("country", "wave"), model = "within", data = dat3)
summary(pm1)


pm1 <- plm(growth ~ hdi, index = c("country", "wave"), model = "within", data = dat3)
summary(pm1)

pm1 <- plm(growth ~ trscore, index = c("country", "wave"), model = "within", data = dat3)
summary(pm1)

pm1 <- plm(growth ~ gini + trustlevel + gini:trustlevel + governance + gdppc, index = c("country", "wave"), model = "within", data = dat3)
summary(pm1)


pm1.random <- plm(growth ~ gini + governance + gdppc, index = c("country", "wave"), model = "random", data = dat2)
summary(pm1.random)


#####################
# Governance and Trust
head(dat3)

# Binary trust
dat3 %>%
  mutate(dum_tr = ifelse(trscore >= median(trscore), 1, 0),
         dum_tl = ifelse(trustlevel >= median(trustlevel), 1, 0)) -> dat3

# gdppc for ini
gdppc_w5 <- dat3 %>% 
  filter(wave == 1) %>% 
  select(country, gdppc) %>% 
  mutate(gdppc_ini = gdppc)

dat3 <- merge(dat3, gdppc_w5[,c(1,3)], by = "country", all.x = TRUE)

# Main result for presentation (temporary)

pm1 <- plm(growth ~ gini, index = "country", model = "within", data = dat3)
summary(pm1)

pm2 <- plm(growth ~ gini + dum_tr + gini:dum_tr, index = "country", model = "within", data = dat3)
summary(pm2)

pm3 <- plm(growth ~ gini + dum_tr + gini:dum_tr + old_pop + hc, index = "country", model = "within", data = dat3)
summary(pm3)

pm4 <- plm(growth ~ gini + dum_tl + gini:dum_tl, index = "country", model = "within", data = dat3)
summary(pm4)

pm5 <- plm(growth ~ gini + dum_tl + gini:dum_tl + old_pop + hc, index = "country", model = "within", data = dat3)
summary(pm5)

stargazer(pm1, pm2, pm3, pm4, pm5, type = "latex", out = "table1.html", covariate.labels = c("Gini", "Trust radius", "Old population", "Human capital", "Gini x Trust radius", "Trust level", "Gini x Trust level"))


pm3 <- plm(growth ~ gini + dum_tl + gini:dum_tl, index = "country", model = "within", data = dat3)
summary(pm3)

pm2 <- plm(growth ~ gini + governance + rgdpe, index = "country", model = "within", data = dat3)
summary(pm2)

pm3 <- plm(growth ~ gini + governance + rgdpe + dum_tr + gini:dum_tr, index = "country", model = "within", data = dat3)

pm4 <- plm(growth ~ gini + governance + rgdpe + dum_tl + gini:dum_tl, index = "country", model = "within", data = dat3)
summary(pm4)

pm5 <- plm(growth ~ gini + governance + rgdpe + dum_tr + gini:dum_tr + , index = "country", model = "within", data = dat3)
summary(pm5)

pm3 <- plm(growth ~ gini + governance + rgdpe + old_pop, index = "country", model = "within", data = dat3)
summary(pm3)

pm4 <- plm(growth ~ gini + governance + gdppc + urban_pop + old_pop + invest + inflation + gini:dum_tr, index = "country", model = "within", data = dat3)
summary(pm4)

pm5 <- plm(growth ~ gini + governance + gdppc + urban_pop + old_pop + invest + inflation + gini:dum_tl, index = "country", model = "within", data = dat3)
summary(pm5)



library(stargazer)

# New plots
dat3 %>% 
  ggplot() + geom_point(aes(trscore, trustlevel, size = gdppc), shape = 21, alpha = 0.5, color = "black") + theme_bw() +
  facet_wrap(~wave) + scale_size_area(max_size = 15, breaks = c(10000, 30000, 50000), 
                                      labels = (c("10k", "30k", "50k"))) + 
  geom_text_repel(data = dat3, aes(x = trscore, y = trustlevel, label = country), segment.size  = 0.2, color = "grey50", nudge_x = .15) +
  labs(x = "Trust Radius (Standardized 0 to 10)", y = "Trust Level (%)", size = "GDP per capita") 


dat3 %>% 
  ggplot() + geom_point(aes(trscore, trustlevel, size = growth, fill = growth), shape = 21, alpha = 0.5, color = "black") + theme_bw() +
  facet_wrap(~wave, nrow = 2) + scale_size_area(max_size = 15, breaks = c(1, 5, 10), 
                                                labels = (c("1", "5", "10"))) + 
  geom_text_repel(data = dat3, aes(x = trscore, y = trustlevel, label = country), segment.size  = 0.1, color = "grey50", nudge_x = .15) +
  labs(x = "Trust Radius (Standardized 0 to 10)", y = "Trust Level (%)", size = "Economic Growth (%)") +
  guides(fill = 'none')





df2[df2$country != "Taiwan",] %>% 
  ggplot(aes(trscore.w5, trustlevel.w5*10)) + geom_point() + theme_bw() +
  geom_text_repel(aes(label = country), segment.size  = 0.2, color = "grey50", nudge_x = .15) +
  labs(x = "Trust Radius (Standardized 0 to 10)", y = "Trust Level (%)")
head()
head(dat3)
df2 <- merge(df2, gdppc_w5[,c(1,3)], by = "country", all.x = TRUE)


