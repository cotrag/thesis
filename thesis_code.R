# cmnd-shift-alt-m for easy code changes

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

rlang::last_error()
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

# ANES Party ID. 1=Dem, 2= Rep, 4=none/independent
anes_2016_hvote$V161019

# ANES self ID race. 1. White, non-Hispanic 2. Black, non-Hispanic 3. Asian, native Hawaiian or other Pacif Islr,non-Hispanic
# 4. Native American or Alaska Native, non-Hispanic 5. Hispanic
#6. Other non-Hispanic incl multiple races [WEB: blank 'Other' counted as a race]
anes_2016_hvote$V161310x


# remove non-substantive
anes_2016_hvote <- anes_2016_hvote %>%
  filter(V161019 == 1 | V161019 == 2 | V161019 == 4) %>%
  filter(V161310x== 1 | V161310x== 2 |V161310x== 3 |V161310x== 4 |V161310x== 5 |V161310x== 6)



model <- lmer(d_vote_int ~ V161019 + (1 | V161139) , data =anes_2016_hvote)

display(model)


model_2 <- glmer(d_vote_int ~ V161019 + (1 | V161139),family= binomial(link = "logit"), 
                 data =anes_2016_hvote)

display(model_2)

coef(model)

coef(model_2)

model_3 <- lmer(d_vote_int ~ V161019 + (1 | V161139) + (1|V161310x), 
                data =anes_2016_hvote)


display(model_3)

coef(model_3)





#### Before modeling, plot relationship between IE spending and MEAN market Price for a bunch of campaigns I would use####

# avg price of each market
pi_markets$MarketId


try_pi_markets_ai <- pi_markets %>%
   group_by(ContractId) %>%
  summarise(avg_price = mean(CloseSharePrice))

new_pi_add_avg <- left_join(pi_markets, try_pi_markets_ai)



# need to work on matching these with IE spending, will do in excel


write.xlsx(try_pi_markets_ai, file = "pi_markets_new_addmean.xlsx")
write.xlsx(new_pi_add_avg, file = "tot_pimkts_vis.xlsx")

avg_house_spend <- data_spending_thesis %>%
   filter(sup_opp == "S") %>%
  filter(can_office == "H") %>%
  group_by(cand_name) %>%
  summarise(totspend = sum(exp_amo))

write.xlsx(avg_house_spend, file = "house_spending_average.xlsx")


# did so and merged it, this only has markets where candidates are named,
# will add parties later

mean_spend_graph <- read_csv("mktprice_totspend_2016.csv")


mean_spend_graph$totspend

# below, not including races with not IE spending
mean_spend_graph_nz <- mean_spend_graph %>%
  filter(tot_spend != 0)



ggplot(mean_spend_graph, aes(avg_price, totspend)) +
  geom_point() + 
  geom_smooth() +
  theme_classic()


# party races harder, need to separate into each market ID, then each contract
# name, as one is D and the other is R. Will do this in excel, first looking
# up where money was actually spent on candidates then doing the above stated
# process. 
# provided data is from 2016, need to add spending data from 2018 to catch 
# the markets in them

#adding 2018

ie_spending_2018 <- read_csv("independent_expenditure_2018-2.csv")

ie_spending_2018 <- ie_spending_2018 %>%
  filter(can_office == "H") %>%
  filter(sup_opp == "S")

write.xlsx(ie_spending_2018, file = "spending_ie_2018.xlsx")

# summarize avg house spend in 2018


avg_house_spend_2018 <- ie_spending_2018 %>%
  group_by(cand_name) %>%
  summarise(totspend = sum(exp_amo))

write.xlsx(avg_house_spend_2018, file = "sum_spending_ie_2018.xlsx")


#new, updated vis "trim_pimkits_new_addmean.csv"


mean_spend_graph_update <- read_csv("trim_pimkits_new_addmean.csv")

mean_spend_graph_update
mean_spend_graph_update$totspend


mean_spend_graph_update_nz <- mean_spend_graph_update %>%
  filter(totspend != 0)



ggplot(mean_spend_graph_update, aes(avg_price, totspend)) +
  geom_point() + 
  geom_smooth() +
  labs(x= "Average Daily Closing Price", y= "Total IE Supporting Candidate") +
  scale_y_continuous(labels = scales::comma) +
  theme_classic()

# no 0's

ggplot(mean_spend_graph_update_nz, aes(avg_price, totspend)) +
  geom_point() + 
  geom_smooth() +
  labs(x= "Average Daily Closing Price", y= "Total IE Supporting Candidate") +
  scale_y_continuous(labels = scales::comma) +
  theme_gray()


mean_spend_graph_update_loutliers <- mean_spend_graph_update %>%
  filter(totspend <= 2000000)


ggplot(mean_spend_graph_update_loutliers, aes(avg_price, totspend)) +
  geom_point() + 
  geom_smooth() +
  labs(x= "Average Daily Closing Price", y= "Total IE Supporting Candidate") +
  scale_y_continuous(labels = scales::comma) +
  theme_gray()




# font stuff
font_add_google("Roboto", "Roboto")
font_paths()
font_files()
font_add("Roboto", )


# showtext_auto when doing ggsave


?font_add_google



# The chart above is for AVERAGE DAILY CLOSING PRICE, let's do one for the initial
# market open and one for the day before the election

pi_markets_op_price_graph <- pi_markets %>%
  group_by(ContractId) %>%
  slice(1)

pi_markets_op_price_graph_trim <- pi_markets_op_price_graph %>%
  dplyr::select(ContractId, OpenSharePrice)

pi_markets_op_price_graph_trim



merged_pi_markets_for_graphing <- left_join(mean_spend_graph_update, 
                        pi_markets_op_price_graph_trim, by = "ContractId")

merged_pi_markets_for_graphing


ggplot(merged_pi_markets_for_graphing, aes(avg_price, totspend)) +
  geom_point() + 
  geom_smooth() +
  labs(x= "Average Daily Closing Price", y= "Total IE Supporting Candidate") +
  scale_y_continuous(labels = scales::comma) +
  theme_gray()

ggplot(merged_pi_markets_for_graphing, aes(OpenSharePrice, totspend)) +
  geom_point() + 
  geom_smooth() +
  labs(x= "OpenSharePrice", y= "Total IE Supporting Candidate") +
  scale_y_continuous(labels = scales::comma) +
  theme_gray()



# close will be much harder, because they do no resolve on election day,
# will try out 10 days from market resoultion
pi_markets_cl_price_graph <- pi_markets %>%
  dplyr::group_by(ContractId) %>%
  dplyr::filter(row_number() >= (n() - 15)) %>%
  slice(1)


pi_markets_cl_price_graph

pi_markets_cl_price_graph_trim <- pi_markets_cl_price_graph %>%
  dplyr::select(ContractId, CloseSharePrice)

pi_markets_cl_price_graph_trim



merged_pi_markets_for_graphing <- left_join(merged_pi_markets_for_graphing, 
              pi_markets_cl_price_graph_trim, by = "ContractId")


merged_pi_markets_for_graphing 


avg_graph <- ggplot(merged_pi_markets_for_graphing, aes(avg_price, totspend)) +
  geom_point() + 
  geom_smooth() +
  labs(x= "Average Daily Closing Price", y= "Total IE Supporting Candidate") +
  scale_y_continuous(labels = scales::comma) +
  theme_gray()

open_graph <- ggplot(merged_pi_markets_for_graphing, aes(OpenSharePrice, totspend)) +
  geom_point() + 
  geom_smooth() +
  labs(x= "Market Opening Price", y= "Total IE Supporting Candidate") +
  scale_y_continuous(labels = scales::comma) +
  theme_gray()



res_graph <- ggplot(merged_pi_markets_for_graphing, aes(CloseSharePrice, totspend)) +
  geom_point() + 
  geom_smooth() +
  labs(x= "Price Near Resolution", y= "Total IE Supporting Candidate") +
  scale_y_continuous(labels = scales::comma) +
  theme_gray()


plot_grid(avg_graph, open_graph, res_graph)






head(pi_markets_op_price_graph)

new_pi_add_avg <- left_join(pi_markets, try_pi_markets_ai)















# messing around with charting

bbc_style <- function() {
  font <- "Helvetica"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=28,
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=22,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=18,
                                        color="#222222"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=18,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}

my_theme <- function () { 
  theme_minimal(base_size = 10, base_family = "Roboto") %+replace% 
    theme(axis.title = element_text(face = "bold"),
          axis.text = element_text(face = "italic"),
          plot.title = element_text(face = "bold",
                                    size = 12)
          
    )
}




#### Betting Market Data ####
pi_markets <- read_csv("Price History By Requested Markets.csv")

pi_markets$HistoryDate <- mdy(pi_markets$HistoryDate)

dummy <- pi_markets


pi_markets$MarketId
pi_markets_grouped <- pi_markets %>%
  group_by(MarketId)

pi_markets_sg
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
  geom_line(aes(y=CloseSharePrice)) + 
  theme_bw()





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

house_spending_ml

ggplot(house_spending_ml, aes(x=exp_date)) +
  geom_line(aes(y=cum_sp))  + 
  theme_bw() 



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

durbinWatsonTest(model_ml)

cochrane.orcutt(model_ml, convergence = 8, max.iter=100)


NeweyWest(model_ml)


??cochrane.orcutt

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
  
  
  
print(house_spending_bp)


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




model_bp <- lm(log(cum_sp) ~ log(CloseSharePrice) + HistoryDate, data = bp_joined)


summary(model_bp)

durbinWatsonTest(model_bp)

cochrane.orcutt(model_bp, convergence = 8, max.iter=100)


??durbanWatsonTest

# insignificant when looking @ spending in opposition


# Brad Ashford


pi_markets_ba <- pi_markets %>%
  filter(MarketId == 2293)

head(pi_markets_ba)


pi_markets_ba$HistoryDate <- ymd(pi_markets_ba$HistoryDate)

pi_markets_ba$HistoryDate

ggplot(pi_markets_ba, aes(x=HistoryDate)) +
  geom_line(aes(y=CloseSharePrice))


house_spending_ba <- data_spending_thesis %>%
  filter(can_office == "H") %>%
  filter(cand_name == "Ashford, Brad") %>%
   filter(sup_opp == "S")
  
  
  
  
  
print(house_spending_ba)


house_spending_ba$exp_date



house_spending_ba$exp_date <- dmy(house_spending_ba$exp_date)

house_spending_ba$exp_date

house_spending_ba <- house_spending_ba[order(as.Date(house_spending_ba$exp_date, format="%Y/%m/%d")),]

house_spending_ba <- house_spending_ba %>%
  mutate(cum_sp = cumsum(exp_amo))

house_spending_ba$exp_date


ggplot(house_spending_ba, aes(x=exp_date)) +
  geom_line(aes(y=cum_sp))



ba_joined <- full_join(pi_markets_ba, house_spending_ba, by= c("HistoryDate" = "exp_date"))




model_ba <- lm(log(cum_sp) ~ log(CloseSharePrice) + HistoryDate, data = ba_joined)


summary(model_ba)


plot(model_ba)



# William Hurd


pi_markets_wh <- pi_markets %>%
  filter(MarketId == 2294)

head(pi_markets_wh)


pi_markets_wh$HistoryDate <- ymd(pi_markets_wh$HistoryDate)

pi_markets_wh$HistoryDate
ggplot(pi_markets_wh, aes(x=HistoryDate)) +
  geom_line(aes(y=CloseSharePrice))


house_spending_wh <- data_spending_thesis %>%
  filter(can_office == "H") %>%
  filter(cand_name == "HURD, WILLIAM") %>%
  filter(sup_opp == "S")





print(house_spending_wh)


house_spending_wh$exp_date



house_spending_wh$exp_date <- dmy(house_spending_wh$exp_date)

house_spending_wh$exp_date

house_spending_wh <- house_spending_wh[order(as.Date(house_spending_wh$exp_date, format="%Y/%m/%d")),]

house_spending_wh <- house_spending_wh %>%
  mutate(cum_sp = cumsum(exp_amo))

house_spending_ba$exp_date


ggplot(house_spending_wh, aes(x=exp_date)) +
  geom_line(aes(y=cum_sp))



wh_joined <- full_join(pi_markets_wh, house_spending_wh, by= c("HistoryDate" = "exp_date"))




model_wh <- lm(log(cum_sp) ~ log(CloseSharePrice) + HistoryDate, data = wh_joined)


summary(model_wh)


extract_eq(model_wh, wrap = TRUE)


plot(model_wh)


bptest(model_wh)

dwtest(model_wh)



modelplot(model_wh)


modelsummary(model_ml, stars = TRUE)




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



devtools::install_github("gadenbuie/rsthemes")

rsthemes::install_rsthemes()

rsthemes::list_rsthemes()

rstudioapi::applyTheme("Horizon Dark {rsthemes}")



