# Merge prediction to Susenas data contoh 2018
library(rio)
library(dplyr)


# Load data from Harvard google drive. Skip this if the data have been loaded.
source("load_data_from_drive.R")

# I use individual raw data, the variables might vary for each year so manual checking is needed to make sure all necessary variables are there
ind2018 <- import("data/2018_ind_raw.dta")
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

# ----------------------------------------------------------------------------------------------------------------------------
# Inpatient cost prediction
## Take needed variables for inpatient cost prediction
ind2018_inpatient <- ind2018uc %>%
  filter(Inpatient == TRUE, PemilikFaskesRanap != "Not hospital", SegmenPesertaRanap != "No BPJS") %>%
  select(NoPeserta, KabFaskesTerdaftar, JenisKelamin, Umur, SegmenPeserta = SegmenPesertaRanap, PemilikFaskes = PemilikFaskesRanap, inpatient_days, Data)

## Load model data
inpatient_lmertree.mod1 <- readRDS("models/inpatient_lmertree_mod1.rds")
# Load normalization method
inpatientBiayaVer_norm <- readRDS("normalization/inpatientBiayaVer_norm.rds")

## Predict values based on model to the new dataset, noted that the prediction value is still transformed value using Ordered Quantile Normalization
ind2018_inp_ucpred <- predict(inpatient_lmertree.mod1, newdata = ind2018_inpatient, type = "response")
## Convert the prediction back to cost unit
ind2018_inp_ucpred_revtrans <- predict(inpatientBiayaVer_norm, ind2018_inp_ucpred, inverse = T)

## Combine the infromations
ind2018_inp <- ind2018_inpatient %>%
  mutate(InpatientUnitCost_daily = ind2018_inp_ucpred_revtrans,
         InpatientUnitCost_total = InpatientUnitCost_daily*inpatient_days) %>%
  # select(NoPeserta, District = KabFaskesTerdaftar, Age = Umur, Sex = JenisKelamin, BPJSType = SegmenPeserta, HospitalOwner = PemilikFaskes, InpatientDays = inpatient_days, InpatientUnitCost_daily, InpatientUnitCost_total)%>%
  select(NoPeserta, InpatientUnitCost_daily, InpatientUnitCost_total)


# -----------------------------------------------------------------------------------------------------------------------------
# Outpatient cost prediction
## Take needed variables for outpatient cost prediction
ind2018_outpatient <- ind2018uc %>%
  filter(Outpatient == TRUE, PemilikFaskesRajal != "Not hospital", SegmenPesertaRajal != "No BPJS") %>%
  select(NoPeserta, KabFaskesTerdaftar, JenisKelamin, Umur, SegmenPeserta = SegmenPesertaRajal, PemilikFaskes = PemilikFaskesRajal, Data)

## Load model data
outpatient_lmertree.mod1 <- readRDS("models/outpatient_lmertree_mod1.rds")
# Load normalization method
outpatientBiayaVer_norm <- readRDS("normalization/outpatientBiayaVer_norm.rds")

## Predict values based on model to the new dataset, noted that the prediction value is still transformed value using Ordered Quantile Normalization
ind2018_outp_ucpred <- predict(outpatient_lmertree.mod1, newdata = ind2018_outpatient, type = "response")
## Convert the prediction back to cost unit
ind2018_outp_ucpred_revtrans <- predict(outpatientBiayaVer_norm, ind2018_outp_ucpred, inverse = T)

## Combine the infromations
ind2018_outp <- ind2018_outpatient %>%
  mutate(OutpatientUnitCost = ind2018_outp_ucpred_revtrans) %>%
  select(NoPeserta, OutpatientUnitCost)

# -------------------------------------------------------------------------------------------------------------------------------
# Combine inpatient and outpatient data and export to dta
ind2018_unitcost <- full_join(ind2018_outp, ind2018_inp) %>% 
  tidyr::replace_na(list(InpatientUnitCost_daily = 0, InpatientUnitCost_total = 0, OutpatientUnitCost = 0)) %>%
  mutate(TotalUnitCost_monthly = InpatientUnitCost_total/12 + OutpatientUnitCost)
ind2018uc <- left_join(ind2018uc, ind2018_unitcost)
ind2018uc_filtered <- right_join(ind2018uc, ind2018_unitcost)

export(ind2018uc, "predictions/predUnitCost_fulldata.dta")
export(ind2018uc_filtered, "predictions/predUnitCost_outpatient_inpatient.dta")

