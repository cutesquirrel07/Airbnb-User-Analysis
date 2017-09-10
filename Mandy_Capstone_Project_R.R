library(readr)

user = read_csv("~/Desktop/users.csv")
require(ggplot2)
##############################################
##### Data Processing and Visualization ######
##############################################

user$id = as.numeric(user$language)
user$language = as.factor(user$language)
user$gender = as.factor(user$gender)
user$signup_method = as.factor(user$signup_method)
user$affiliate_channel = as.factor(user$affiliate_channel)
user$affiliate_provider = as.factor(user$affiliate_provider)
user$first_affiliate_tracked = as.factor(user$first_affiliate_tracked)
user$signup_app = as.factor(user$signup_app)
user$first_device_type = as.factor(user$first_device_type)
user$first_browser = as.factor(user$first_browser)
user$country_destination = as.factor(user$country_destination)

summary(user)

# date_account_created, divide them into year, month, day

library(stringr)
user$date_account_created = as.factor(user$date_account_created)
dac = as.data.frame(str_split_fixed(user$date_account_created, '/', 3))
dac

user['dac_year'] = dac[,3]
user['dac_month'] = dac[,1]
user['dac_day'] = dac[,2]

summary(user)

# Timestamp_first_active, divide them into year, month, day

user['tfa_year'] = substring(as.character(user['timestamp_first_active']), 3, 6)
user['tfa_month'] = substring(as.character(user['timestamp_first_active']), 7, 8)
user['tfa_day'] = substring(as.character(user['timestamp_first_active']), 9, 10)

summary(user)

# Date_first_booking, divide them into year, month, day
dfb = as.data.frame(str_split_fixed(user$date_first_booking, '/', 3))
dfb
user['dfb_year'] = dfb[,3]
user['dfb_month'] = dfb[,1]
user['dfb_day'] = dfb[,2]

summary(user)

# Gender
prop.table(table(user$gender))

### A lot of unknowns

# Conver unknowns to NA
user$gender = factor(ifelse(user$gender == "-unknown-", "", as.character(user$gender)))
summary(user$gender)
unique(user$gender)

ggplot(user, aes(user$gender)) +
  geom_bar(fill = "pink") +
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10))

# Age
summary(user$age)
boxplot(user$age)

### Max value 2014, Min 1 not reasonable

# 2014 might be a typo, correct it to a birth=year
user$age = ifelse((user$age >= 1915) & (user$age <= 2014), 2017-user$age, user$age)

summary(user$age)

# Suppose people using airbnb is from 16 to 100
user$age = ifelse((user$age >= 16) & (user$age <= 100), user$age, NA)

summary(user$age)

# Transfer age to age_bucket
user$age_bkt = as.character(cut(user$age, breaks=c(16,25,35,45,55,65,75,85,100),
                                labels=c('16-25','26-35', '36-45','46-55',
                                         '56-65','66-75','76-85','86+')))

# Language

prop.table(table(user$language))

### Most of the languages are english, the number is huge compared to other languages added.

user$lang = factor(ifelse(user$language == "en", "en", "non-en"))

ggplot(user, aes(user$lang)) +
  geom_bar(fill = "pink") +
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10))

# Affiliate Provider

summary(user$affiliate_provider)
### Some major players, a few small providers

# Select the major providers and combine the small ones
user$affiliate_prov = factor(ifelse(user$affiliate_provider == "direct", "direct",
                                    ifelse(user$affiliate_provider == "google", "google",
                                           ifelse(user$affiliate_provider == "craigslist", "craigslist",
                                                  ifelse(user$affiliate_provider == "bing", "bing",
                                                         ifelse(user$affiliate_provider == "facebook", "facebook",
                                                                ifelse(user$affiliate_provider == "facebook-open-graph", "facebook", "other")))))))



summary(user$affiliate_prov)
ggplot(user, aes(user$affiliate_prov)) +
  geom_bar(fill = "pink") +
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10))

# Country Destination

summary(user$country_destination)
prop.table(table(user$country_destination))

ggplot(user,aes(user$country_destination)) +
  geom_bar(fill = "light blue")+
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10)) +
  xlab("country destination")

### The number of NDF is so big that other countries become insignificant.
### A lot of countries in Europe.


# Combine US & Canada to NorthAmerica, combine european coutries to europe, and others
user$cd = factor(ifelse(user$country_destination == "US", "NorthAmerica",
                        ifelse(user$country_destination =="CA", "NorthAmerica",
                               ifelse(user$country_destination == "ES", "Europe",
                                      ifelse(user$country_destination == "FR", "Europe",
                                             ifelse(user$country_destination == "GB", "Europe",
                                                    ifelse(user$country_destination == "IT", "Europe",
                                                           ifelse(user$country_destination == "NL", "Europe",
                                                                  ifelse(user$country_destination == "PT", "Europe",
                                                                         ifelse(user$country_destination == "NDF", "NoBk", "Other"))))))))))
summary(user$cd)

ggplot(user,aes(user$cd)) +
  geom_bar(fill = "light blue")+
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10)) +
  xlab("country destination")


# First_Browser

user$fbr = factor(ifelse(user$first_browser == "Chrome", "Chrome",
                         ifelse(user$first_browser == "Chrome Mobile", "Chrome",
                                ifelse(user$first_browser == "Firefox", "Firefox",
                                       ifelse(user$first_browser == "Mobile Firefox", "Firefox",
                                            ifelse(user$first_browser == "IE", "IE",
                                                   ifelse(user$first_browser == "IE Mobile", "IE",
                                                          ifelse(user$first_browser == "-unknown", " ", "Other")
                                                          )))))))
summary(user$fbr)                                                  
unique(user$fbr)  

# First Affiliated Link

user$fat = factor(ifelse(user$first_affiliate_tracked == "linked", "linked",
                         ifelse(user$first_affiliate_tracked == "omg", "omg",
                                ifelse(user$first_affiliate_tracked == "untracked", "untracked", "other"))))
summary(user$fat)
unique(user$fat)

####################################
##### vs. country destination ######
####################################

# Gender vs. CD
df1 = as.data.frame(user %>%
                      count(gender,cd) %>%
                      ungroup %>%
                      group_by(gender) %>%
                      mutate(total=sum(n), pct=n/sum(n)))

ggplot(df1, aes(gender, y=pct, fill=cd))+
  geom_bar(stat="identity",position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10)) +
  xlab("Gender") + ylab("Percentage") + labs(fill="Country of Destinations") +
  scale_fill_brewer(palette = "Spectral")

### Gender doesn't seem like to have an significant effect on country of destinations.
### However, for users that doesn't like to disclose their gender seems to have more no-bookings.


# Age vs. CD

df2 = as.data.frame(user %>%
                      count(age_bkt,cd) %>%
                      ungroup %>%
                      group_by(age_bkt) %>%
                      mutate(total=sum(n), pct=n/sum(n)))

ggplot(df2, aes(age_bkt, y=pct, fill=cd))+
  geom_bar(stat="identity",position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10))+
  xlab("Age Bucket") + ylab("Percentage") +
  scale_fill_brewer(palette = "Set1")+
  theme_minimal()+ labs(fill="Country of Destinations")
# North America is the favorite destinations of all age groups.

df3 = as.data.frame(user %>%
                      count(cd,age_bkt) %>%
                      ungroup %>%
                      group_by(cd) %>%
                      mutate(total=sum(n), pct=n/sum(n)))

ggplot(df3, aes(cd, y=pct, fill=age_bkt))+
  geom_bar(stat="identity",position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10))+
  xlab("Country of Destinations") + ylab("Percentage") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+ labs(fill="Age Bucket")

# In all the categories, 26-35 is the core target users. 36-45 is the second core target.

df4 = as.data.frame(user %>%
                      count(signup_method,cd) %>%
                      ungroup %>%
                      group_by(signup_method) %>%
                      mutate(total=sum(n), pct=n/sum(n)))

ggplot(df4, aes(cd, y=pct, fill=signup_method))+
  geom_bar(stat="identity",position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10))+
  xlab("Country of Destinations") + ylab("Percentage") +
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()+ labs(fill="Signup Method")

### Most people who signed up through google doesn't book a trip on Airbnb.

df5 = as.data.frame(user %>%
                      count(cd,signup_method) %>%
                      ungroup %>%
                      group_by(cd) %>%
                      mutate(total=sum(n), pct=n/sum(n)))

ggplot(df5, aes(cd, y=pct, sprintf("%0.2f", round(pct, digits = 2)), fill=signup_method))+
  geom_bar(stat="identity",position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10))+
  xlab("Country of Destinations") + ylab("Percentage") +
  scale_fill_brewer(palette = "Set1")+
  theme_minimal()+ labs(fill="Signup_Method")

### Not a lot of people used google to signup. People tend to like sign up using facebook and basic.

#signup_app vs. cd
df6 = as.data.frame(user %>%
                      count(signup_app,cd) %>%
                      ungroup %>%
                      group_by(signup_app) %>%
                      mutate(total=sum(n), pct=n/sum(n)))

ggplot(df6, aes(signup_app, y=pct, fill=cd))+
  geom_bar(stat="identity",position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10)) +
  xlab("Gender") + ylab("Percentage") + labs(fill="Country of Destinations") +
  scale_fill_brewer(palette = "Spectral")
### North America is still the most popular destination for all users. 
### Europe is the second popular destination.

#affiliate_channel vs. cd
df7 = as.data.frame(user %>%
                      count(cd,affiliate_channel) %>%
                      ungroup %>%
                      group_by(cd) %>%
                      mutate(total=sum(n), pct=n/sum(n)))

ggplot(df7, aes(affiliate_channel, y=pct, fill=cd))+
  geom_bar(stat="identity",position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8)) +
  xlab("Affiliate_channel") + ylab("Percentage") + labs(fill="Country of Destinations") +
  scale_fill_brewer(palette = "Set3")

df8 = as.data.frame(user[user$affiliate_channel!="direct",]%>%
                      count(cd,affiliate_channel) %>%
                      ungroup %>%
                      group_by(cd) %>%
                      mutate(total=sum(n), pct=n/sum(n)))
                      
ggplot(df8, aes(cd, y=pct, fill=affiliate_channel))+
  geom_bar(stat="identity",position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=8)) +
  xlab("Affiliate_channel") + ylab("Percentage") + labs(fill="Affiliate_channel") +
  scale_fill_brewer(palette = "Set3")+
  geom_text(aes(label=affiliate_channel), position=position_dodge(width=0.9), vjust=-0.25, size =2)

# dfb_year vs. cd

df9 = as.data.frame(user[user$country_destination!="NDF",] %>%
                      count(dfb_year,cd) %>%
                      ungroup %>%
                      group_by(dfb_year) %>%
                      mutate(total=sum(n), pct=n/sum(n)))

ggplot(df9, aes(dfb_year, y=pct, fill=cd))+
  geom_bar(stat="identity",position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8)) +
  xlab("Year of Booking Made") + ylab("Percentage") + labs(fill="Country of Destinations") +
  scale_fill_brewer(palette = "Blues")

# North America was most popular in 2010, less popular during 2011 to 2015, but become more popular in 2015.
# Europe is losing its popylarity.

# dfb_month vs. cd
df10 = as.data.frame(user[user$country_destination!="NDF",] %>%
                      count(dfb_month,cd) %>%
                      ungroup %>%
                      group_by(dfb_month) %>%
                      mutate(total=sum(n), pct=n/sum(n)))

ggplot(df10, aes(dfb_month, y=pct, fill=cd))+
  geom_bar(stat="identity",position = "dodge") +
  theme_bw()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8)) +
  xlab("Year of Booking Made") + ylab("Percentage") + labs(fill="Country of Destinations") +
  scale_fill_brewer(palette = "Set1")

############################
##### Conversion Rate ######
############################


# Conversion Rate
user$booked = ifelse(user$country_destination =="NDF",0,1)

df11 = as.data.frame(user %>%
                      group_by(dac_year) %>%
                      summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))

ggplot(df11, aes(dac_year))+
  geom_line(aes(y=n),group=1, size=2, colour="blue")+
  geom_line(aes(y=conversion_rate*130000),group=1, size=2, colour="pink")+
  scale_y_continuous(breaks = seq(0,80000,10000), "Number of Accounts Created",
                     sec.axis = sec_axis(~./130000, "Conversion Rate"))+
  theme_bw()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=10))+ xlab("Account Created Date")
### The number of Account Created is increasing rapidly over the year.
### However, the conversion rate is declining.



# Gender vs. conversion rate

df12 = as.data.frame(user %>%
                      group_by(gender) %>%
                      summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))

ggplot(df12, aes(gender,conversion_rate))+
  geom_bar(stat="identity", fill="Pink", colour="dark green")+
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10))+ xlab("Gender") + ylab("Conversion Rate") +
  scale_y_continuous(breaks=seq(0,0.6,0.1)) +
  geom_line(group=1, size=1.5, colour="yellow")

### Conversion rate is about the same for females and males, but is higher for other.




# Age buket vs. conversion rate

df13 = as.data.frame(user %>%
                      group_by(age_bkt) %>%
                      summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))

ggplot(df13, aes(age_bkt,conversion_rate))+
  geom_bar(stat="identity", fill="light blue", colour="Orange")+
  theme_bw()+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=10))+ xlab("Age Bucket")+
  scale_y_continuous(breaks=seq(0,0.6,0.1)) +
  geom_line(group=1, size=1.5, colour="orange")

### Conversion rate is the highest for users in 16-25. 
### The general trend is that the conversion rate declines as the age increases.


# Affiliate_provider vs. conversion rate
df14 = as.data.frame(user %>%
                      group_by(affiliate_prov) %>%
                      summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))

ggplot(df14, aes(affiliate_prov))+
  geom_line(aes(y=n), group=1, size=1.5, colour="pink")+
  geom_line(aes(y=conversion_rate*1000000),group=1, size=1.5, colour="red")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5, color="black"))+
  scale_y_continuous("Number of Records", sec.axis = sec_axis(~./1000000, name="Conversion Rate"))
### Direct has significant higher conversion rate. Let's remove it.

# remove "direct"
df15 = as.data.frame(user[user$affiliate_prov!="direct",]%>%
                      group_by(affiliate_prov) %>%
                      summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))

ggplot(df15, aes(affiliate_prov))+
  geom_line(aes(y=n), group=1, size=1.5, colour="pink")+
  geom_line(aes(y=conversion_rate*50000),group=1, size=1.5, colour="red")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5, color="black"))+
  scale_y_continuous("Number of Records", sec.axis = sec_axis(~./50000, name="Conversion Rate"))+
  xlab("Affiliate Provider")

### Google has the best conversion rate.


# Affiliate Channel vs. Conversion Rate

df16 = as.data.frame(user %>%
                       group_by(affiliate_channel) %>%
                       summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))

ggplot(df16, aes(affiliate_channel))+
  geom_line(aes(y=n), group=1, size=1.5, colour="light blue")+
  geom_line(aes(y=conversion_rate*1000000),group=1, size=1.5, colour="green")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5, color="black"))+
  scale_y_continuous("Number of Records", sec.axis = sec_axis(~./1000000))

#Remove Direct
df17 = as.data.frame(user[user$affiliate_channel!="direct",]%>%
                       group_by(affiliate_channel) %>%
                       summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))
ggplot(df17, aes(affiliate_channel))+
  geom_line(aes(y=n), group=1, size=1.5, colour="blue")+
  geom_line(aes(y=conversion_rate*50000),group=1, size=1.5, colour="green")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5, color="black"))+
  scale_y_continuous("Number of Records", sec.axis = sec_axis(~./50000, name="Conversion Rate"))+
  xlab("Affiliate Channel")+
  scale_color_manual(values = c("Organge", "Yellow"))

### Sem-brand has the best conversion rate.



# Signup_method vs. Conversion Rate

df18 = as.data.frame(user %>%
                       group_by(signup_method) %>%
                       summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))

ggplot(df18, aes(signup_method))+
  geom_line(aes(y=n), group=1, size=1.5, colour="pink")+
  geom_line(aes(y=conversion_rate*1000000),group=1, size=1.5, colour="red")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5, color="black"))+
  scale_y_continuous("Number of Records", sec.axis = sec_axis(~./1000000, name="Conversion Rate"))

# Fewer users use google for signing up. The conversion rate for google is also low.


# First Device Type vs. Conversion Rate

df19 = as.data.frame(user %>%
                       group_by(first_device_type) %>%
                       summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))

ggplot(df19, aes(first_device_type))+
  geom_line(aes(y=n), group=1, size=1.5, colour="pink")+
  geom_line(aes(y=conversion_rate*1000000),group=1, size=1.5, colour="red")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5, color="black"))+
  scale_y_continuous("Number of Records", sec.axis = sec_axis(~./1000000, name="Conversion Rate"))

# Mac Desktop has the most users and highest conversion rate.


# First Affiliated Link vs. Conversion Rate
df20 = as.data.frame(user %>%
                       group_by(fat) %>%
                       summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))

ggplot(df20, aes(fat))+
  geom_line(aes(y=n), group=1, size=1.5, colour="pink")+
  geom_line(aes(y=conversion_rate*1000000),group=1, size=1.5, colour="red")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5, color="black"))+
  scale_y_continuous("Number of Records", sec.axis = sec_axis(~./1000000, name="Conversion Rate"))+
  xlab("First Affilated Link")


# First Browser vs. Conversion Rate
df21 = as.data.frame(user %>%
                       group_by(fbr) %>%
                       summarise(n=n(), booked = sum(booked), conversion_rate=booked/n))

ggplot(df21, aes(fbr))+
  geom_line(aes(y=n), group=1, size=1.5, colour="pink")+
  geom_line(aes(y=conversion_rate*1000000),group=1, size=1.5, colour="red")+
  theme(axis.text.x = element_text(angle=70, vjust=0.5, color="black"))+
  scale_y_continuous("Number of Records", sec.axis = sec_axis(~./1000000, name="Conversion Rate"))+
  xlab("First Browser")

# Other has the best conversion rate.

write.csv(user, file="user_v2.csv", row.names = FALSE)


                      