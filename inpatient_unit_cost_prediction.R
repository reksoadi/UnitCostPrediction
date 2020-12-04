# Load libraries
library(rio)
library(dplyr)
library(ggplot2)
library(lubridate)
library(glmertree)
library(modelr)
library(bestNormalize)


# Load data from Harvard google drive. Skip this if the data have been loaded.
source("load_data_from_drive.R")

# Import dataset
kunjunganFKL <- readRDS("data/kunjunganFKL.rds")
peserta <- readRDS("data/peserta.rds")
hh2018 <- import("data/2018_HH_raw.dta")
ind2018 <- import("data/2018_ind_raw.dta")

# Select and transform variables from Susenas data
ind2018uc <- ind2018 %>%
  mutate(NoPeserta = urut*100 + r401) %>%
  select(urut, UrutHH = r401, NoPeserta, KabFaskesTerdaftar = kabu, JenisKelamin = r405, Umur = r407, PBI = r1001_a, NonPBI = r1001_b, Outpatient = r1005, OutpPublicHospital = r1007_a, OutpPrivateHospital = r1007_b,
         OutpHealthIns_PBI = r1008_a, OutpHealthIns_NonPBI = r1008_b, 
         Inpatient = r1009, InpPublicHospital = r1010_a, InpPrivateHospital = r1010_b,
         InpHealthIns_PBI = r1012_a, InpHealthIns_NonPBI = r1012_b, inpatient_days = r1011,
         exp_cap, fwt) %>%
  mutate(JenisKelamin = ifelse(JenisKelamin == 1, "LAKI-LAKI", "PEREMPUAN"),
         SegmenPeserta = ifelse(PBI == "A", "Subsidized BPJS", ifelse(NonPBI == "B", "Non-subsidized BPJS", "No BPJS")),
         Outpatient = ifelse(Outpatient == 5 | is.na(Outpatient), FALSE, TRUE), Inpatient = ifelse(Inpatient == 1, TRUE, FALSE),
         SegmenPesertaRajal = ifelse(OutpHealthIns_PBI == "A", "Subsidized BPJS", ifelse(OutpHealthIns_NonPBI == "B", "Non-subsidized BPJS", "No BPJS")),
         SegmenPesertaRanap = ifelse(InpHealthIns_PBI == "A", "Subsidized BPJS", ifelse(InpHealthIns_NonPBI == "B", "Non-subsidized BPJS", "No BPJS")),
         PemilikFaskesRajal = ifelse(OutpPublicHospital == "A", "RS Pemerintah", ifelse(OutpPrivateHospital == "B", "RS Swasta", "Not hospital")),
         PemilikFaskesRanap = ifelse(InpPublicHospital == "A", "RS Pemerintah", ifelse(InpPrivateHospital == "B", "RS Swasta", "Not hospital")),
         Data = "Susenas")
ind2018_inpatient <- ind2018uc %>%
  filter(Inpatient == TRUE, PemilikFaskesRanap != "Not hospital", SegmenPesertaRanap != "No BPJS") %>%
  select(NoPeserta, KabFaskesTerdaftar, JenisKelamin, Umur, SegmenPeserta = SegmenPesertaRanap, PemilikFaskes = PemilikFaskesRanap, inpatient_days, Data)

# Find the best normalization method for BiayaVerifikasi
Biaya_cap <- kunjunganFKL %>% group_by(NoPeserta) %>% summarise(BiayaVerifikasi = sum(BiayaVerifikasi))
BiayaVer_norm <- bestNormalize(Biaya_cap$BiayaVerifikasi) # Best method: Ordered Quantile Normalization 
MASS::truehist(Biaya_cap$BiayaVerifikasi, main = "No transformation", nbins = 12)
MASS::truehist(BiayaVer_norm$x.t, main = "orderNorm transformation", nbins = 12)

## For inpatient only
inpatientFKL <- kunjunganFKL %>% 
  mutate(Year = year(TglDatang), inpatient_days = abs(time_length(interval(TglDatang, TglPulang), "day")),
         inpatient_days = ifelse(inpatient_days == 0, inpatient_days + 1, inpatient_days),
         psychoses = ifelse(substr(CBG_code, 1, 3) == "F-4", TRUE, FALSE)) %>%
  filter(TingkatLayanan == "RAWAT INAP TINGKAT LANJUT", Year == 2016)#, psychoses == FALSE)

inpatientBiaya_cap <- inpatientFKL %>% group_by(NoPeserta) %>% summarise(BiayaVerifikasi = sum(BiayaVerifikasi)/sum(inpatient_days))
inpatientBiayaVer_norm <- bestNormalize(inpatientBiaya_cap$BiayaVerifikasi) # Best method: Ordered Quantile Normalization
export(inpatientBiayaVer_norm, "normalization/inpatientBiayaVer_norm.rds")
MASS::truehist(inpatientBiaya_cap$BiayaVerifikasi, main = "No transformation", nbins = 12)
MASS::truehist(inpatientBiayaVer_norm$x.t, main = "orderNorm transformation", nbins = 12)

inpatientDays_cap <- inpatientFKL %>% group_by(NoPeserta) %>% summarise(inpatient_days = sum(inpatient_days))
inpatientDays_norm <- bestNormalize(inpatientDays_cap$inpatient_days) # Best method: Ordered Quantile Normalization 
MASS::truehist(inpatientDays_cap$inpatient_days, main = "No transformation", nbins = 12)
MASS::truehist(inpatientDays_norm$x.t, main = "orderNorm transformation", nbins = 12)



## For outpatient only
outpatientFKL <- kunjunganFKL %>% 
  mutate(Year = year(TglDatang)) %>%
  filter(TingkatLayanan == "RAWAT JALAN TINGKAT LANJUT", Year == 2016)

outpatientBiaya_cap <- outpatientFKL %>% group_by(NoPeserta) %>% summarise(BiayaVerifikasi = sum(BiayaVerifikasi))
outpatientBiayaVer_norm <- bestNormalize(outpatientBiaya_cap$BiayaVerifikasi) # Best method: Ordered Quantile Normalization 
MASS::truehist(outpatientBiaya_cap$BiayaVerifikasi, main = "No transformation", nbins = 12)
MASS::truehist(outpatientBiayaVer_norm$x.t, main = "orderNorm transformation", nbins = 12)


# Building a predictive model for inpatient data
## Exploratory
### Aggregate from visit level to individual level and select some potential predictors
inp_FKL <- inpatientFKL %>%
  group_by(ProvFaskesTerdaftar, KabFaskesTerdaftar = KabFaskesTerdaftar_code, PemilikFaskes, SegmenPeserta, KelasRawat, JenisKelamin, StatusKawin, NoPeserta) %>%
  summarise(BobotIndividu = max(BobotIndividu), Umur = mean(Umur), BiayaVerifikasi = sum(BiayaVerifikasi)/sum(inpatient_days), inpatient_days = sum(inpatient_days)) %>%
  ungroup %>%
  mutate(BiayaVerifikasi.t = predict(inpatientBiayaVer_norm, BiayaVerifikasi),
         SegmenPeserta = ifelse(SegmenPeserta %in% c("PBI APBN", "PBI APBD"), "Subsidized BPJS", "Non-subsidized BPJS"),
         PemilikFaskes = ifelse(PemilikFaskes == "SWASTA", "RS Swasta", "RS Pemerintah"),
         Data = "BPJS") %>%
  select(-ProvFaskesTerdaftar, 
         -BobotIndividu) %>%
  tidyr::drop_na() %>%
  bind_rows(ind2018_inpatient) %>%
  # tidyr::pivot_wider(names_from = KabFaskesTerdaftar, values_from = val_1, values_fill = list(val_1 = 0)) %>%
  mutate_at(vars(#ProvFaskesTerdaftar, 
    KabFaskesTerdaftar, 
    PemilikFaskes, SegmenPeserta, KelasRawat, JenisKelamin, StatusKawin), as.factor) #%>%
# mutate_at(vars(AcehBarat:TobaSamosir), as.numeric)

ind2018_inpatient <- inp_FKL %>% filter(Data == "Susenas") %>% select(-BiayaVerifikasi, -BiayaVerifikasi.t)
inp_FKL <- inp_FKL %>% filter(Data == "BPJS")

### Some plots for comparison (apparently, the predictors cannot provide adequate discriminatory value for the service cost)
ggplot(inp_FKL, aes(KabFaskesTerdaftar, BiayaVerifikasi.t)) + geom_boxplot()
ggplot(inp_FKL, aes(SegmenPeserta, BiayaVerifikasi.t)) + geom_boxplot()
ggplot(inp_FKL, aes(KelasRawat, BiayaVerifikasi.t)) + geom_boxplot()
ggplot(inp_FKL, aes(JenisKelamin, BiayaVerifikasi.t)) + geom_boxplot()
ggplot(inp_FKL, aes(StatusKawin, BiayaVerifikasi.t)) + geom_boxplot()
ggplot(inp_FKL, aes(Umur, BiayaVerifikasi.t)) + geom_point() + geom_smooth()

# Start building models using split method
## Split into test and train data
set.seed(2020)
inp_dat <- resample_partition(inp_FKL, c(test = 0.3, train = 0.7))

## Using LM model assuming no difference between districts
lm.mod1 <- lm(BiayaVerifikasi.t ~ SegmenPeserta + PemilikFaskes + JenisKelamin + StatusKawin + Umur + inpatient_days, data = inp_dat$train$data)
summary(lm.mod1)
lm.pred1 <- predict(lm.mod1, newdata = inp_dat$test$data)
lm.pred1 <- list(pred.t = lm.pred1, pred.x = predict(inpatientBiayaVer_norm, lm.pred1, inverse = T))
sqrt(sum((inp_dat$test$data$BiayaVerifikasi-lm.pred1$pred.x)^2)/nrow(inp_dat$test$data))
export(lm.mod1, "models/inpatient_lm_mod1.rds")

lm.mod2 <- lm(BiayaVerifikasi.t ~ SegmenPeserta + PemilikFaskes + JenisKelamin + StatusKawin + Umur, data = inp_dat$train$data)
summary(lm.mod2)
anova(lm.mod1, lm.mod2)
lm.pred2 <- predict(lm.mod2, newdata = inp_dat$test$data)
lm.pred2 <- list(pred.t = lm.pred2, pred.x = predict(inpatientBiayaVer_norm, lm.pred2, inverse = T))
sqrt(sum((inp_dat$test$data$BiayaVerifikasi-lm.pred2$pred.x)^2)/nrow(inp_dat$test$data))
export(lm.mod2, "models/inpatient_lm_mod2.rds")

### Using LMER model
#### LMER Model 1
lmer.mod1 <- lmer(BiayaVerifikasi.t ~ SegmenPeserta + PemilikFaskes + KelasRawat + JenisKelamin + StatusKawin + Umur +  (1 | KabFaskesTerdaftar), data = inp_dat$train$data)
summary(lmer.mod1)
rmse(lmer.mod1, inp_dat$train$data)
rsquare(lmer.mod1, inp_dat$train$data)

lmer.pred1 <- predict(lmer.mod1, newdata = inp_dat$test$data)
lmer.pred1 <- as.data.frame(list(pred.t = lmer.pred1, pred.x = predict(inpatientBiayaVer_norm, lmer.pred1, inverse = T), 
                                 data.t = inp_dat$test$data$BiayaVerifikasi.t, data.x = inp_dat$test$data$BiayaVerifikasi))
sqrt(sum((inp_dat$test$data$BiayaVerifikasi-lmer.pred1$pred.x)^2)/nrow(inp_dat$test$data))
sum(inp_dat$test$data$BiayaVerifikasi-lmer.pred1$pred.x)/nrow(inp_dat$test$data)
ggplot(lmer.pred1, aes(pred.x, data.x)) + geom_point() + geom_smooth()
export(lmer.mod1, "models/inpatient_lmer_mod1.rds")

##### Bootstrap
mySumm <- function(.) { s <- sigma(.)
c(beta =getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta"))) }
lmer.boot1 <- bootMer(lmer.mod1, mySumm, nsim = 100)
confint(lmer.boot1)

#### LMER Model 2
lmer.mod2 <- lmer(BiayaVerifikasi.t ~ SegmenPeserta + PemilikFaskes + JenisKelamin + StatusKawin + Umur + (1 | KabFaskesTerdaftar), data = inp_dat$train$data)
summary(lmer.mod2)
rmse(lmer.mod2, inp_dat$train$data)
rsquare(lmer.mod2, inp_dat$train$data)
anova(lmer.mod1, lmer.mod2) # Simpler model cannot provide better statistics
lmer.pred2 <- predict(lmer.mod2, newdata = inp_dat$test$data)
lmer.pred2 <- as.data.frame(list(pred.t = lmer.pred2, pred.x = predict(inpatientBiayaVer_norm, lmer.pred2, inverse = T), 
                                 data.t = inp_dat$test$data$BiayaVerifikasi.t, data.x = inp_dat$test$data$BiayaVerifikasi))
sqrt(sum((inp_dat$test$data$BiayaVerifikasi-lmer.pred2$pred.x)^2)/nrow(inp_dat$test$data))
sum(inp_dat$test$data$BiayaVerifikasi-lmer.pred2$pred.x)/nrow(inp_dat$test$data)
ggplot(lmer.pred2, aes(pred.x, data.x)) + geom_point() + geom_smooth()
export(lmer.mod2, "models/inpatient_lmer_mod2.rds")


### Using lmertree model
#### LMERTREE Model 1
lmertree.mod1 <- lmertree(BiayaVerifikasi.t ~ 1 | (1 | KabFaskesTerdaftar) | SegmenPeserta + Umur + PemilikFaskes + JenisKelamin + inpatient_days, data = inp_dat$train$data)
summary(lmertree.mod1)
rmse(lmertree.mod1, inp_dat$train$data)
rsquare(lmertree.mod1, inp_dat$train$data)
lmertree.pred1 <- predict(lmertree.mod1, newdata = inp_dat$test$data)
lmertree.pred1 <- as.data.frame(list(pred.t = lmertree.pred1, pred.x = predict(inpatientBiayaVer_norm, lmertree.pred1, inverse = T), 
                                     data.t = inp_dat$test$data$BiayaVerifikasi.t, data.x = inp_dat$test$data$BiayaVerifikasi))
sqrt(sum((inp_dat$test$data$BiayaVerifikasi-lmertree.pred1$pred.x)^2)/nrow(inp_dat$test$data))
sum(inp_dat$test$data$BiayaVerifikasi-lmertree.pred1$pred.x)/nrow(inp_dat$test$data)
ggplot(lmertree.pred1, aes(pred.x, data.x)) + geom_point() + geom_smooth()
export(lmertree.mod1, "models/inpatient_lmertree_mod1.rds")

lmertree.mod2 <- lmertree(BiayaVerifikasi.t ~ 1 | (1 | KabFaskesTerdaftar) | Umur + JenisKelamin, data = inp_dat$train$data)
summary(lmertree.mod2)
rmse(lmertree.mod2, inp_dat$train$data)
rsquare(lmertree.mod2, inp_dat$train$data)
lmertree.pred2 <- predict(lmertree.mod2, newdata = inp_dat$test$data)
lmertree.pred2 <- as.data.frame(list(pred.t = lmertree.pred2, pred.x = predict(inpatientBiayaVer_norm, lmertree.pred2, inverse = T), 
                                     data.t = inp_dat$test$data$BiayaVerifikasi.t, data.x = inp_dat$test$data$BiayaVerifikasi))
sqrt(sum((inp_dat$test$data$BiayaVerifikasi-lmertree.pred2$pred.x)^2)/nrow(inp_dat$test$data))
sum(inp_dat$test$data$BiayaVerifikasi-lmertree.pred2$pred.x)/nrow(inp_dat$test$data)
ggplot(lmertree.pred2, aes(pred.x, data.x)) + geom_point() + geom_smooth()
export(lmertree.mod2, "models/inpatient_lmertree_mod2.rds")


# Note: lmertree with 7 predictors and 1 random effects variable provide the best accuracy among all tested models even though the small r-square and 
# high error were found in all models. The error comes from underestimation of the unit cost



