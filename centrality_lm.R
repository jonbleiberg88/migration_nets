gdp <- read.csv("data/gdp_per_cap.csv")
cent <- read.csv("data/eig_cent_scaled.csv")

merged_df <- merge(gdp, cent, by= "Country" )
no_uk <- merged_df[merged_df$Country != "United Kingdom",]

model_1 <- lm(cent_2010~I(X2010), data=no_uk)
summary(model_1)

sigs <- rep(NA, 7)
for(i in 0:6) {
  f <- as.formula(paste("cent_201",i,"~","I(X201",i,")", sep=""))
  model <- lm(f, data=merged_df)
  sigs[i+1] <- summary(model)$coefficients[2,'Pr(>|t|)']
}

sigs

sigs2 <- rep(NA, 7)
for(i in 0:6) {
  f <- as.formula(paste("cent_201",i,"~","I(X201",i,"^2)", sep=""))
  model <- lm(f, data=merged_df)
  sigs2[i+1] <- summary(model)$coefficients[2,'Pr(>|t|)']
}

sigs2

gdp <- read.csv("data/gdp_per_cap.csv")
cent_unscaled <- read.csv("data/eig_cent_unscaled.csv")

merged_df <- merge(gdp, cent_unscaled, by= "Country" )
no_uk <- merged_df[merged_df$Country != "United Kingdom",]

sigs <- rep(NA, 7)
for(i in 0:6) {
  f <- as.formula(paste("cent_201",i,"~","I(X201",i,")", sep=""))
  model <- lm(f, data=merged_df)
  sigs[i+1] <- summary(model)$coefficients[2,'Pr(>|t|)']
}

sigs

summary(model)

sigs <- rep(NA, 7)
for(i in 0:6) {
  edge_df <- read.csv(paste("data/edge_data/edge_df_201",i,".csv",sep = ""))
  model <- lm(scaled~ gdp_diff+I(cit_gdp*dest_gdp), data = edge_df)
  summary(model)
  sigs[i+1] <- summary(model)$coefficients[3,'Pr(>|t|)']
}
sigs





edge_df <- read.csv("data/edge_data/edge_df_2011.csv")
model <- lm(scaled~ I(cit_gdp*dest_gdp), data = edge_df)
summary(model)


idx <- order()


edge_2016 <- read.csv("data/edge_data/edge_df_2016.csv")
model_1 <- lm(scaled~I(gdp_diff), data=edge_2016)
summary(model_1)

model_2 <- lm(model_1$residuals ~ I(sqrt(edge_2016$cit_gdp*edge_2016$dest_gdp)))
summary(model_2)

plot(model_1$residuals, I(sqrt(edge_2016$cit_gdp*edge_2016$dest_gdp)),
     xlim=c(-0.01,0.01))



out_cent <- read.csv("data/out_cent_unscaled.csv")
merged_df <- merge(gdp, out_cent, by= "Country" )

a<-lm(cent_2010 ~ I(X2010^2), data=merged_df)     
summary(a)

sigs <- rep(NA, 7)
for(i in 0:6) {
  f <- as.formula(paste("cent_201",i,"~","I(X201",i,")", sep=""))
  model <- lm(f, data=merged_df)
  sigs[i+1] <- summary(model)$coefficients[2,'Pr(>|t|)']
}

sigs
