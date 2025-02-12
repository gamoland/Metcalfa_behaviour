
mod1 <- glmmTMB(Starters ~ Test + Year,
                data = Logregdata,
                family = "binomial")
summary()
table(Logregdata$Starters,interaction(Logregdata$Test,Logregdata$Year))

mod1a <- glmmTMB(Starters ~ Test,
                data = subset(Logregdata,Year==2023),
                family = "binomial")


