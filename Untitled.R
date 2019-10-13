# Hypothesis

#------------------------------------
# 1. Income level affects Default
#------------------------------------
# 1. Avg_BILL_Amt,
# 2. Avg_Pmt_AMT,
# 3. Max_BILL_Amt,
# 4. Max_Pmt_Amt

lattice::histogram(~Avg_BILL_Amt|DEFAULT, data = train)
lattice::histogram(~log(Avg_PMT_Amt)|DEFAULT, data = train)


lattice::xyplot(log(ifelse(Avg_PMT_Amt<0,0.001,Avg_PMT_Amt)) ~ log(ifelse(Avg_BILL_Amt<0,0.001,Avg_BILL_Amt)), data = train,group = ifelse(DEFAULT==1,"Default","Non-Default"),auto.key = T,grid = T,xlab = "log(Average Bill Amount)", ylab = "log(Average Payment Amount)")

lattice::xyplot(log(ifelse(Max_Pmt_Amt<=0,0.001,Max_Pmt_Amt)) ~ log(ifelse(Max_Bill_Amt<=0,0.001,Max_Bill_Amt)), data = train,group = ifelse(DEFAULT==1,"Default","Non-Default"),auto.key = T,grid = T,xlab = "log(Max Bill Amount)", ylab = "log(Max Payment Amount)")

train %>% 
  ggplot(aes(x=log(ifelse(Avg_BILL_Amt<=0,0.001,Avg_BILL_Amt)),y=log(ifelse(Avg_PMT_Amt<=0,0.001,Avg_PMT_Amt)),col = DEFAULT)) + 
  xlab("log(Max Bill Amount)") +
  ylab("log(Max Payment Amount)")+
  geom_point(alpha = 0.4) + 
  theme_bw() -> scp

ggExtra::

train %>% 
  ggplot(aes(x=log(ifelse(Avg_BILL_Amt<=0,0.001,Avg_BILL_Amt)),col = DEFAULT, group = DEFAULT, fill = DEFAULT)) + geom_density(alpha = 0.3)
#-------------------------------------
#   2. Delinquencies
#-------------------------------------


train %>% 
  ggplot(aes(x=DLQ_Total,col = DEFAULT, group = DEFAULT, fill = DEFAULT)) + geom_bar(alpha = 0.3)

Information::create_infotables(data = creditdata %>% filter(train==1) %>% select(DLQ_Total, DEFAULT) %>% mutate(DEFAULT = as.numeric(DEFAULT)-1), y = "DEFAULT", parallel = T, bins = 5) ->dlqbin

Information::plot_infotables(dlqbin,"DLQ_Total") + coord_flip()

#--------------------------------
#     3. Utilization
#-------------------------------- 

# 1. util flag

table(train$DEFAULT,train$utilFlag) -> utilflagtbl
chisq.test(utilflagtbl)

# 2. ppk

train %>% ggplot(aes(x=log(ifelse(ppk <=0, 0.0000001,ppk)),group = DEFAULT, col = DEFAULT, fill = DEFAULT)) + geom_density(alpha = 0.4) + theme_bw()

# 3. Avg UTIL

train %>% ggplot(aes(x=Avg_UTIL,group = DEFAULT, col = DEFAULT, fill = DEFAULT)) + geom_density(alpha = 0.4) + theme_bw()

# 4. ppkwoe

train %>% ggplot(aes(x=ppkwoe,group = DEFAULT, col = DEFAULT, fill = DEFAULT )) + geom_histogram()

#--------------------------------
#    4. Balances
#--------------------------------