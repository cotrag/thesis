

#### Spending Data ####
data_spending_thesis <- read_csv("independent_expenditure_2016.csv")


house_spending <- data_spending_thesis %>%
  filter(can_office == "H") %>%
  filter(cand_name == "CURBELO, CARLOS")

house_spending$exp_date <- dmy(house_spending$exp_date)



ggplot(house_spending, aes(x=exp_date)) +
  geom_line(aes(y=exp_amo))


# build a function with control-option-x
# chng all variables with control-shift-option-m

?arrange



#### Polling Data ####

cces <- read.dta("CCES16_Common_OUTPUT_Feb2018_VV.dta")


cces$CC16_410b

# ANES has a pre-vote pref for D or R, does not look like CCES has one

anes_2016 <- read_dta("anes_timeseries_2016.dta")

# house choices of people who INTEND to vote
anes_2016$V161040


anes_2016$V161039

anes_2016V161025x

# remove non-substantive variables
anes_2016_hvote <- anes_2016 %>%
  filter(V161040 == 1 | V161040 == 2 | V161040== 3) %>%
  filter(V161015b != -1) %>%
  filter(V161139 == 1 | V161139== 2 | V161139 == 3 | V161139 == 4 | V161139== 5)

anes_2016_hvote$V161040
# 1=Dem, 2=R, 3=Other

anes_2016_hvote %>%
  # group_by(V161015b)
  count(V161040)


anes_2016_hvote$V161015b

anes_2016_hvote_trim <- anes_2016_hvote %>%
  dplyr::select(V161040, V161015b)

anes_2016_hvote %>%
  group_by(V161015b) %>%
  count(V161040)

state_group <-  anes_2016_hvote %>%
  count(V161015b)
# This is not representative of state populations, need to do post stratification

anes_2016_hvote_state_ave <- anes_2016_hvote %>% group_by(V161015b) %>%
  summarise(ave_state = mean(V161015b))

state_group %>% print(n = Inf)


write.csv(state_group, "state_group_2.csv")



sg_merge <- read_csv("state_group_2.csv")
# This is the proportion of respondents by state, not representative, need to use post strat

sg_merge

# make the n proportion, can be thought of as "% of the sample is from that state"

sum(sg_merge$n)


sg_merge <- sg_merge %>%
  mutate(pct_sample = n/1211)


sg_merge

sum(sg_merge$pct_sample)

# manually add state labels so easier to read


write.csv(sg_merge, "state_lab_2.csv")


state_sample_props <- read_csv("state_lab_2.csv")


state_sample_props

state_pop_props <- read_csv("census_state_pop.csv")


state_pop_props


state_proportions <- left_join(state_sample_props, state_pop_props, by= c("State" = "State"))


state_proportions 




# let's start with building a model that meas. D vote intention by some demographic variables
# that would be found in the Census

anes_2016_hvote <- anes_2016_hvote %>%
  mutate(d_vote_int = ifelse(V161040==1, 1, 0))


anes_2016_hvote$d_vote_int

# d_vote_int is a dummy variable that goes to 1 for intention of voting for a D and 0 if not

anes_2016_hvote$V161139
# PRE: Current economy good or bad, 1              1. Very good
#2                   2. Good
#3   3. Neither good nor bad
#4                    4. Bad
#5               5. Very bad

anes_2016_hvote$V161019
# remove non-substantive
anes_2016_hvote <- anes_2016_hvote %>%
  filter(V161019 == 1 | V161019 == 2 | V161019 == 4)




model <- lmer(d_vote_int ~ V161019 + (1 | V161139) , data =anes_2016_hvote)

display(model)


model_2 <- glmer(d_vote_int ~ V161019 + (1 | V161139),family= binomial(link = "logit"), 
                 data =anes_2016_hvote)

display(model_2)

coef(model)

coef(model_2)






?lmer
?print.stanreg
















#### Betting Market Data ####
pi_markets <- read_csv("Price History By Requested Markets.csv")

pi_markets$HistoryDate <- mdy(pi_markets$HistoryDate)

dummy <- pi_markets


pi_markets$MarketId
pi_markets_grouped <- pi_markets %>%
  group_by(MarketId)


pi_markets_sg$CloseSharePrice

sapply(pi_markets, function(x) length(unique(x)))

# 112 unique market IDs

pi_markets_sg <- pi_markets %>%
  filter(MarketId == 2119)

head(pi_markets_sg)


pi_markets_sg$HistoryDate <- ymd(pi_markets_sg$HistoryDate)


ggplot(pi_markets_sg, aes(x=HistoryDate)) +
  geom_line(aes(y=CloseSharePrice))





# David Young
pi_markets_dy <- pi_markets %>%
  filter(MarketId == 2120)

head(pi_markets_dy)


pi_markets_dy$HistoryDate <- ymd(pi_markets_dy$HistoryDate)


ggplot(pi_markets_dy, aes(x=HistoryDate)) +
  geom_line(aes(y=CloseSharePrice))


house_spending_dy <- data_spending_thesis %>%
  filter(can_office == "H") %>%
  filter(cand_name == "Young, David") %>%
  filter(sup_opp == "S")

house_spending_dy$exp_date

house_spending_dy$exp_date <- dmy(house_spending_dy$exp_date)

house_spending_dy$exp_date


house_spending_dy <- house_spending_dy[order(as.Date(house_spending_dy$exp_date, format="%Y/%m/%d")),]


house_spending_dy <- house_spending_dy %>%
  mutate(cum_sp = cumsum(exp_amo))


ggplot(house_spending_dy, aes(x=exp_date)) +
  geom_line(aes(y=cum_sp))


pi_markets_dy$HistoryDate

house_spending_dy$exp_date

dy_joined <- full_join(pi_markets_dy, house_spending_dy, by= c("HistoryDate" = "exp_date"))
dy_joined$CloseSharePrice

ggplot(dy_joined , aes(x=HistoryDate)) +
  geom_line(aes(y=cum_sp)) + 
  geom_line(aes(y=CloseSharePrice))


model_dy <- lm(log(cum_sp) ~ log(CloseSharePrice), data = dy_joined)


summary(model_dy)


# Mia Love


pi_markets_ml <- pi_markets %>%
  filter(MarketId == 2254)

head(pi_markets_ml)


pi_markets_ml$HistoryDate <- ymd(pi_markets_ml$HistoryDate)

pi_markets_ml$HistoryDate
ggplot(pi_markets_ml, aes(x=HistoryDate)) +
  geom_line(aes(y=CloseSharePrice))





house_spending_ml <- data_spending_thesis %>%
  filter(can_office == "H") %>%
  filter(cand_name == "LOVE, MIA") %>%
  filter(sup_opp == "S")

house_spending_ml <- house_spending_ml %>%
  mutate(cum_sp = cumsum(exp_amo))

house_spending_ml$cum_sp


write.csv(house_spending_ml, "sml_spending.csv")


house_spending_ml$exp_date <- dmy(house_spending_ml$exp_date)

house_spending_ml$receipt_dat <- dmy(house_spending_ml$receipt_dat)

house_spending_ml <- house_spending_ml %>%
  filter(receipt_dat <= as.Date("2017-01-01"))




house_spending_ml <- house_spending_ml[order(as.Date(house_spending_ml$exp_date, format="%Y/%m/%d")),]

house_spending_ml <- house_spending_ml %>%
  mutate(cum_sp = cumsum(exp_amo))

house_spending_ml$exp_date


ggplot(house_spending_ml, aes(x=exp_date)) +
  geom_line(aes(y=cum_sp))



ml_joined <- full_join(pi_markets_ml, house_spending_ml, by= c("HistoryDate" = "exp_date"))

ggplot(ml_joined , aes(x=HistoryDate)) +
  geom_line(aes(y=cum_sp)) + 
  geom_line(aes(y=CloseSharePrice))


model_ml <- lm(log(cum_sp) ~ log(CloseSharePrice) + HistoryDate, data = ml_joined)


summary(model_ml)


plot(model_ml)

ts(ml_joined)

acf(ml_joined, na.action = na.pass)

?acf

par("mar")

par(mar=c( 5.1, 4.1, 4.1 ,2.1))


?arma



# This is the relationship between betting markets and spending. 1% increase in betting market
# odds is associated with a 2.78% increase in spending for this example, significant


# Poliquin, Bruce


pi_markets_bp <- pi_markets %>%
  filter(MarketId == 2270)

head(pi_markets_bp)


pi_markets_bp$HistoryDate <- ymd(pi_markets_bp$HistoryDate)

pi_markets_bp$HistoryDate
ggplot(pi_markets_bp, aes(x=HistoryDate)) +
  geom_line(aes(y=CloseSharePrice))


house_spending_bp <- data_spending_thesis %>%
  filter(can_office == "H") %>%
  filter(cand_name == "Poliquin, Bruce") %>%
  # filter(sup_opp == "S")
  
  
  # all of this spending is in OPPOSITION
  
  
  
house_spending_bp


house_spending_bp$exp_date

# there are NA's..... issue?


house_spending_bp$exp_date <- dmy(house_spending_bp$exp_date)

house_spending_bp$exp_date

house_spending_bp <- house_spending_bp[order(as.Date(house_spending_bp$exp_date, format="%Y/%m/%d")),]

house_spending_bp <- house_spending_bp %>%
  mutate(cum_sp = cumsum(exp_amo))

house_spending_bp$exp_date


ggplot(house_spending_bp, aes(x=exp_date)) +
  geom_line(aes(y=cum_sp))



bp_joined <- full_join(pi_markets_bp, house_spending_bp, by= c("HistoryDate" = "exp_date"))




model_bp <- lm(log(cum_sp) ~ log(CloseSharePrice), data = bp_joined)


summary(model_bp)

















# messing around

dummy <- pi_markets

n_dummy <- dummy %>%
  group_by(MarketId) %>%
  nest()


head(n_dummy)


?cumsum

# for reference
#house_spending_dy <- house_spending_dy %>%
#  mutate(cum_sp = cumsum(exp_amo))



hs_dummy_sen_house <- data_spending_thesis %>%
  filter(ele_type == "G") %>%
  filter(can_office == "H" | can_office == "S") %>%
  filter(sup_opp == "S")

n_hs_dummy_sen_house <- hs_dummy_sen_house %>%
  group_by(cand_name) %>%
  nest()

n_hs_dummy_sen_house

n_hs_dummy_sen_house %>% print(n = Inf)


csum_df <- function(df)mutate(cum_sp = cumsum(exp_amo))

new <-  n_hs_dummy_sen_house %>%
  mutate(cumulative = map(n_hs_dummy_sen_house, csum_df ))


hs_dummy_sen_house <- hs_dummy_sen_house  %>% 
  group_by(cand_name) %>% 
  mutate(csum = cumsum(exp_amo))



# look for acceptable merges (over a certain amount)



n_dummy %>% print(n = Inf)