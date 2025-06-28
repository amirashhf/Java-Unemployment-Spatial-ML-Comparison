library(readxl)
library(sf)
library(dplyr)
library(stringr)
library(cowplot)
library(ggrepel)
library(car)
library(lmtest)
library(spdep)
library(spatialreg)
library(randomForest)
library(e1071)
library(rpart)
library(gbm)
library(ggplot2)
library(tidyr)
library(caret)
library(spatialRF)
library(FNN)



######  ======================= Data Preparation  ======================= ######

# Baca data Excel sebagai data.frame
data<- read_excel("C:/Users/natha/Desktop/Tugas 4 Spasial/Data Pulau Jawa 2023.xlsx")
names(data)


# Rename kolom
names(data)[2:10] <- c(
  "Kabupaten", "Latitude", "Longitude", "TPT", "TPAK", "UHH", "HLS", "Pengeluaran", "Persentase_Miskin"
)
colnames(data)

# Ubah menjadi objek `sf`
data <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

shp_paths <- list(
  "Jawa Barat" = "C:/Users/natha/Desktop/Tugas 4 Spasial/Shapefile2/RBI_50K_2023_Jawa Barat.shp",
  "DKI Jakarta" = "C:/Users/natha/Desktop/Tugas 4 Spasial/Shapefile2/RBI_50K_2023_DKI Jakarta.shp",
  "DI Yogyakarta" = "C:/Users/natha/Desktop/Tugas 4 Spasial/Shapefile2/RBI_50K_2023_Daerah Istimewa Yogyakarta.shp",
  "Jawa Timur" = "C:/Users/natha/Desktop/Tugas 4 Spasial/Shapefile2/RBI_50K_2023_Jawa Timur.shp",
  "Jawa Tengah" = "C:/Users/natha/Desktop/Tugas 4 Spasial/Shapefile2/RBI_50K_2023_Jawa Tengah.shp",
  "Banten" = "C:/Users/natha/Desktop/Tugas 4 Spasial/Shapefile2/RBI_50K_2023_Banten.shp"
)

shp_list <- lapply(shp_paths, st_read)

# Gabungkan semua ke dalam 1 objek petajawa
petajawa <- do.call(rbind, shp_list)

# Pastikan CRS sama
st_crs(data) <- st_crs(petajawa)

# Gabungkan berdasarkan nama kabupaten + handling data
combined_data <- left_join(petajawa, st_drop_geometry(data), by = c("WADMKK" = "Kabupaten"))


data$Kabupaten <- dplyr::recode(data$Kabupaten,
                                "Kota Proboliggo" = "Kota Probolinggo"
)
combined_data <- combined_data %>%
  filter(!is.na(TPT))

# Cek kecocokan
combined_data %>%
  filter(is.na(TPT)) %>%
  pull(WADMKK)

######  ======================= Statistika Deskriptif  ======================= ######

summary(data)

## ----------- Boxplot ----------- ##
warna_isi <- "#F1EDC9"
warna_garis <- "#BF8F43"

buat_boxplot <- function(data, kolom, judul, y_label) {
  ggplot(data, aes(x = "", y = !!sym(kolom))) +
    geom_boxplot(fill = warna_isi, color = warna_garis, width = 0.3) +
    theme_minimal() +
    labs(title = judul, y = y_label, x = NULL) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

plot_tpt <- buat_boxplot(combined_data, "TPT", "TPT (%)", "TPT (%)")
plot_tpak <- buat_boxplot(combined_data, "TPAK", "TPAK (%)", "TPAK (%)")
plot_uhh <- buat_boxplot(combined_data, "UHH", "Usia Harapan Hidup", "UHH (tahun)")
plot_hls <- buat_boxplot(combined_data, "HLS", "Harapan Lama Sekolah", "HLS (tahun)")
plot_pengeluaran <- buat_boxplot(combined_data, "Pengeluaran", "Pengeluaran", "Pengeluaran (rb/thn)")
plot_ppm <- buat_boxplot(combined_data, "Persentase_Miskin", "Penduduk Miskin (%)", "Persentase (%)")

print(plot_tpt)
print(plot_tpak)
print(plot_uhh)
print(plot_hls)
print(plot_pengeluaran)
print(plot_ppm)


## ----------- Peta Choropleth ----------- ##
palette_choropleth <- c(
  "#225E5E",  
  "#599395",  
  "#F1EDC9",  
  "#F6C863", 
  "#A87C3A"   
)

buat_peta_dengan_label <- function(data, kolom, judul, legenda_label) {
  data_label <- data %>%
    filter(!!sym(kolom) == max(!!sym(kolom)) | !!sym(kolom) == min(!!sym(kolom))) %>%
    mutate(centroid = st_centroid(geometry),
           lon = st_coordinates(centroid)[,1],
           lat = st_coordinates(centroid)[,2])
  
  ggplot(data) +
    geom_sf(aes(fill = !!sym(kolom)), color = "white", size = 0.2) +
    geom_label_repel(data = data_label,
                    aes(x = lon, y = lat, label = WADMKK),
                    size = 3,
                    color = "black",
                    nudge_y = 0.3,          # Geser ke utara
                    box.padding = 0.3,
                    segment.color = "gray50") +
    scale_fill_gradientn(colors = palette_choropleth) +
    theme_minimal() +
    labs(title = judul, fill = legenda_label) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
}



peta_tpt <- buat_peta_dengan_label(combined_data, "TPT", "Sebaran Tingkat Pengangguran Terbuka", "TPT (%)")
peta_tpak <- buat_peta_dengan_label(combined_data, "TPAK", "Sebaran Tingkat Partisipasi Angkatan Kerja", "TPAK (%)")
peta_uhh <- buat_peta_dengan_label(combined_data, "UHH", "Sebaran Usia Harapan Hidup", "UHH (tahun)")
peta_hls <- buat_peta_dengan_label(combined_data, "HLS", "Sebaran Harapan Lama Sekolah", "HLS (tahun)")
peta_pengeluaran <- buat_peta_dengan_label(combined_data, "Pengeluaran", "Sebaran Pengeluaran per Kapita", "Ribu Rupiah/Tahun")
peta_ppm <- buat_peta_dengan_label(combined_data, "Persentase_Miskin", "Sebaran Persentase Penduduk Miskin", "Persentase (%)")

print(peta_tpt)
print(peta_tpak)
print(peta_uhh)
print(peta_hls)
print(peta_pengeluaran)
print(peta_ppm)

## ----------- Barchart ----------- ##
warna_barchart <- "#5E9C9E" 

buat_barchart_top5 <- function(data, kolom, judul, y_label) {
  data_top5 <- data %>%
    slice_max(order_by = !!sym(kolom), n = 5) %>%
    arrange(!!sym(kolom))  # urutkan dari bawah ke atas
  
  ggplot(data_top5, aes(x = reorder(WADMKK, !!sym(kolom)), y = !!sym(kolom))) +
    geom_col(fill = warna_barchart) +
    coord_flip() +
    theme_minimal() +
    labs(title = judul, x = NULL, y = y_label) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text.y = element_text(size = 10)
    )
}

bar_tpt <- buat_barchart_top5(combined_data, "TPT", "Tingkat Pengangguran Terbuka", "TPT (%)")
bar_tpak <- buat_barchart_top5(combined_data, "TPAK", "Tingkat Partisipasi Angkatan Kerja", "TPAK (%)")
bar_uhh <- buat_barchart_top5(combined_data, "UHH", "Usia Harapan Hidup", "UHH (tahun)")
bar_hls <- buat_barchart_top5(combined_data, "HLS", "Harapan Lama Sekolah", "HLS (tahun)")
bar_pengeluaran <- buat_barchart_top5(combined_data, "Pengeluaran", "Pengeluaran per Kapita", "Ribu Rupiah/Tahun")
bar_ppm <- buat_barchart_top5(combined_data, "Persentase_Miskin", "Persentase Penduduk Miskin", "Persentase (%)")

print(bar_tpt)
print(bar_tpak)
print(bar_uhh)
print(bar_hls)
print(bar_pengeluaran)
print(bar_ppm)

######  ======================= Analisis Regresi Linear  ======================= ######

# Pastikan semua kolom numeric tidak ada NA
model_data <- combined_data %>%
  filter(!is.na(TPT), !is.na(TPAK), !is.na(UHH), !is.na(HLS), !is.na(Pengeluaran), !is.na(Persentase_Miskin))

# Jalankan model regresi linear
model <- lm(TPT ~ TPAK + UHH + HLS + Pengeluaran + Persentase_Miskin, data = model_data)
summary(model)

# Uji multikolinearitas
vif(model)

# Uji normalitas residu
residuals <- residuals(model)
shapiro.test(residuals)

# Uji homoskedastisitas
bptest(model)

# Uji autokorelasi
dwtest(model)


######  =======================  Uji Dependensi Spasial (Moran’s I)  ======================= ######


# Membuat matriks ketetanggaan dari polygon
system.time({
  nb <- poly2nb(combined_data)
})
system.time({
  lw <- nb2listw(nb, style = "W")
})

# Uji Moran's I pada TPT
moran.test(combined_data$TPT, listw = lw)

# Uji Moran's I untuk variabel independen
moran.test(combined_data$TPAK, listw = lw)
moran.test(combined_data$UHH, listw = lw)
moran.test(combined_data$HLS, listw = lw)
moran.test(combined_data$Pengeluaran, listw = lw)
moran.test(combined_data$Persentase_Miskin, listw = lw)

# Uji Moran's I pada residual OLS
resid_ols <- residuals(model)
moran.test(resid_ols, listw = lw)

###### =======================  Analisis Model Regresi Spasial  ======================= ######

# OLS model untuk uji LM
lm_model <- lm(TPT ~ TPAK + UHH + HLS + Pengeluaran + Persentase_Miskin, data = combined_data)

# Uji Lagrange Multiplier
lm_tests <- lm.RStests(lm_model, lw, test = c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))
print(lm_tests)

# SAR model
model_sar <- lagsarlm(TPT ~ TPAK + UHH + HLS + Pengeluaran + Persentase_Miskin,
                      data = combined_data, listw = lw)
summary(model_sar)
AIC(model_sar)
moran.test(residuals(model_sar), lw)

# SEM model
model_sem <- errorsarlm(TPT ~ TPAK + UHH + HLS + Pengeluaran + Persentase_Miskin,
                        data = combined_data, listw = lw)
summary(model_sem)
AIC(model_sem)
moran.test(residuals(model_sem), lw)

# SARMA model
model_sarma <- sacsarlm(TPT ~ TPAK + UHH + HLS + Pengeluaran + Persentase_Miskin,
                        data = combined_data, listw = lw)
summary(model_sarma)
AIC(model_sarma)
moran.test(residuals(model_sarma), lw)

###### =======================  Visualisasi Model Regresi Linear & Model Regresi Spasial  ======================= ######

## ----------- Peta Residual ----------- ##
combined_data$resid_ols   <- residuals(lm_model)
combined_data$resid_sar   <- residuals(model_sar)
combined_data$resid_sem   <- residuals(model_sem)
combined_data$resid_sarma <- residuals(model_sarma)

# Tambahkan centroid dan koordinat label
combined_data_centroid <- combined_data %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  )

label_ols <- combined_data_centroid %>% filter(abs(resid_ols) > 2)
label_sar <- combined_data_centroid %>% filter(abs(resid_sar) > 2)
label_sem <- combined_data_centroid %>% filter(abs(resid_sem) > 2)
label_sarma <- combined_data_centroid %>% filter(abs(resid_sarma) > 2)

buat_peta_residual_label <- function(data, resid_col, judul, label_data, warna = c("#E3E674", "#B0E0E6", "#E7754A")) {
  ggplot(data) +
    geom_sf(aes(fill = !!sym(resid_col)), color = "white") +
    geom_text_repel(data = label_data, aes(x = lon, y = lat, label = WADMKK), size = 3, color = "black") +
    scale_fill_gradient2(low = warna[1], mid = warna[2], high = warna[3], midpoint = 0) +
    theme_minimal() +
    labs(title = judul, fill = "Residual") +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}


peta_resid_ols_label <- buat_peta_residual_label(combined_data_centroid, "resid_ols", "Peta Residual Model OLS", label_ols)
peta_resid_sar_label <- buat_peta_residual_label(combined_data_centroid, "resid_sar", "Peta Residual Model SAR", label_sar)
peta_resid_sem_label <- buat_peta_residual_label(combined_data_centroid, "resid_sem", "Peta Residual Model SEM", label_sem)
peta_resid_sarma_label <- buat_peta_residual_label(combined_data_centroid, "resid_sarma", "Peta Residual Model SARMA", label_sarma)

print(peta_resid_ols_label)
print(peta_resid_sar_label)
print(peta_resid_sem_label)
print(peta_resid_sarma_label)

## ----------- Plot Prediksi ----------- ##
# Tambahkan prediksi ke dalam combined_data
combined_data <- combined_data %>%
  mutate(
    pred_ols = fitted(model),
    pred_sar = fitted(model_sar),
    pred_sem = fitted(model_sem),
    pred_sarma = fitted(model_sarma)
  )
plot_prediksi <- function(data, pred_col, title_text, warna = "#A3D5A9") {
  ggplot(data, aes(x = .data[[pred_col]], y = TPT)) +
    geom_point(color = warna, size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    theme_minimal() +
    labs(title = title_text, x = "Prediksi TPT", y = "TPT Aktual") +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}
plot_ols   <- plot_prediksi(combined_data, "pred_ols",   "Prediksi vs Observasi (OLS)", "#B0E0E6")
plot_sar   <- plot_prediksi(combined_data, "pred_sar",   "Prediksi vs Observasi (SAR)", "#A3D5A9")
plot_sem   <- plot_prediksi(combined_data, "pred_sem",   "Prediksi vs Observasi (SEM)", "#E3E674")
plot_sarma <- plot_prediksi(combined_data, "pred_sarma", "Prediksi vs Observasi (SARMA)", "#E7754A")

print(plot_ols)
print(plot_sar)
print(plot_sem)
print(plot_sarma)

# ===================== Machine Learning + Spatial Features ===================== #

# Siapkan fitur spasial: koordinat & lag TPT
combined_data <- combined_data %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  )

# Spatial lag variable (autocovariate)
combined_data$lag_TPT <- lag.listw(lw, combined_data$TPT)


# Data awal dengan fitur spasial
base_data <- combined_data %>%
  st_drop_geometry() %>%
  dplyr::select(TPT, TPAK, UHH, HLS, Pengeluaran, Persentase_Miskin, lon, lat, lag_TPT)

ml_data_rf <- base_data
ml_data_svr <- base_data
ml_data_tree <- base_data
ml_data_gbm <- base_data

# Random Forest
set.seed(123)
model_rf <- randomForest(TPT ~ ., data = ml_data_rf, ntree = 500)
ml_data_rf$pred <- predict(model_rf)
ml_data_rf$resid <- ml_data_rf$TPT - ml_data_rf$pred

# SVR
model_svr <- svm(TPT ~ ., data = ml_data_svr)
ml_data_svr$pred <- predict(model_svr)
ml_data_svr$resid <- ml_data_svr$TPT - ml_data_svr$pred

# Tree
model_tree <- rpart(TPT ~ ., data = ml_data_tree)
ml_data_tree$pred <- predict(model_tree)
ml_data_tree$resid <- ml_data_tree$TPT - ml_data_tree$pred

# GBM
set.seed(123)
model_gbm <- gbm(TPT ~ ., data = ml_data_gbm, distribution = "gaussian",
                 n.trees = 500, interaction.depth = 3, shrinkage = 0.05)
ml_data_gbm$pred <- predict(model_gbm, n.trees = 500)
ml_data_gbm$resid <- ml_data_gbm$TPT - ml_data_gbm$pred

# ------------------- Evaluasi Model -------------------

# Fungsi evaluasi metrik regresi
evaluate_model <- function(actual, predicted) {
  resid <- actual - predicted
  rmse <- sqrt(mean(resid^2))
  mae <- mean(abs(resid))
  r2 <- 1 - sum(resid^2) / sum((actual - mean(actual))^2)
  return(c(RMSE = rmse, MAE = mae, R2 = r2))
}

# Hitung metrik semua model
metrics_rf <- evaluate_model(ml_data_rf$TPT, ml_data_rf$pred)
metrics_svr <- evaluate_model(ml_data_svr$TPT, ml_data_svr$pred)
metrics_tree <- evaluate_model(ml_data_tree$TPT, ml_data_tree$pred)
metrics_gbm <- evaluate_model(ml_data_gbm$TPT, ml_data_gbm$pred)

# Gabungkan jadi data frame
eval_df <- data.frame(
  Model = c("Random Forest", "SVR", "Decision Tree", "GBM"),
  RMSE = c(metrics_rf["RMSE"], metrics_svr["RMSE"], metrics_tree["RMSE"], metrics_gbm["RMSE"]),
  MAE  = c(metrics_rf["MAE"],  metrics_svr["MAE"],  metrics_tree["MAE"],  metrics_gbm["MAE"]),
  R2   = c(metrics_rf["R2"],   metrics_svr["R2"],   metrics_tree["R2"],   metrics_gbm["R2"])
)

# Tampilkan tabel hasil
print(eval_df)

# Ubah ke long format untuk keperluan visualisasi
eval_long <- eval_df %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

# Visualisasi Barchart gabungan semua metrik
ggplot(eval_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Perbandingan Metrik Evaluasi Model ML",
    x = NULL,
    y = "Nilai",
    fill = "Metrik"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

######  ======================= Hyperparameter Tuning & Cross Validation ======================= ######
set.seed(123)

# Siapkan data
ml_data_cv <- base_data 

# Cross-validation 5-fold
ctrl <- trainControl(method = "cv", number = 5)

# Random Forest
tunegrid_rf <- expand.grid(.mtry = c(2, 3, 4, 5))
model_rf_cv <- train(TPT ~ ., data = ml_data_cv,
                     method = "rf",
                     trControl = ctrl,
                     tuneGrid = tunegrid_rf,
                     importance = TRUE)

# Support Vector Regression
tunegrid_svr <- expand.grid(
  sigma = c(0.01, 0.05, 0.1),
  C = c(0.1, 1, 10)
)
model_svr_cv <- train(
  TPT ~ ., data = ml_data_cv,
  method = "svmRadial",
  trControl = ctrl,
  tuneGrid = tunegrid_svr
)

# Decision Tree 
tunegrid_tree <- expand.grid(.cp = seq(0.001, 0.1, by = 0.005))
model_tree_cv <- train(TPT ~ ., data = ml_data_cv,
                       method = "rpart",
                       trControl = ctrl,
                       tuneGrid = tunegrid_tree)

# GBM 
tunegrid_gbm <- expand.grid(.n.trees = c(100, 200, 500),
                            .interaction.depth = c(1, 3, 5),
                            .shrinkage = c(0.01, 0.05, 0.1),
                            .n.minobsinnode = c(5, 10))
model_gbm_cv <- train(TPT ~ ., data = ml_data_cv,
                      method = "gbm",
                      trControl = ctrl,
                      tuneGrid = tunegrid_gbm,
                      verbose = FALSE)

# ------------------- Evaluasi Model -------------------

# Prediksi dari model hasil tuning
pred_rf_cv   <- predict(model_rf_cv, newdata = ml_data_cv)
pred_svr_cv  <- predict(model_svr_cv, newdata = ml_data_cv)
pred_tree_cv <- predict(model_tree_cv, newdata = ml_data_cv)
pred_gbm_cv  <- predict(model_gbm_cv, newdata = ml_data_cv)

# Fungsi evaluasi
evaluate_model <- function(actual, predicted) {
  resid <- actual - predicted
  rmse <- sqrt(mean(resid^2))
  mae <- mean(abs(resid))
  r2 <- 1 - sum(resid^2) / sum((actual - mean(actual))^2)
  return(c(RMSE = rmse, MAE = mae, R2 = r2))
}

# Hitung metrik evaluasi
metrics_rf_cv   <- evaluate_model(ml_data_cv$TPT, pred_rf_cv)
metrics_svr_cv  <- evaluate_model(ml_data_cv$TPT, pred_svr_cv)
metrics_tree_cv <- evaluate_model(ml_data_cv$TPT, pred_tree_cv)
metrics_gbm_cv  <- evaluate_model(ml_data_cv$TPT, pred_gbm_cv)

# Gabungkan ke dalam data frame
eval_df_cv <- data.frame(
  Model = c("Random Forest (CV)", "SVR (CV)", "Decision Tree (CV)", "GBM (CV)"),
  RMSE  = c(metrics_rf_cv["RMSE"],  metrics_svr_cv["RMSE"],  metrics_tree_cv["RMSE"],  metrics_gbm_cv["RMSE"]),
  MAE   = c(metrics_rf_cv["MAE"],   metrics_svr_cv["MAE"],   metrics_tree_cv["MAE"],   metrics_gbm_cv["MAE"]),
  R2    = c(metrics_rf_cv["R2"],    metrics_svr_cv["R2"],    metrics_tree_cv["R2"],    metrics_gbm_cv["R2"])
)

# Tampilkan hasil evaluasi setelah tuning
print(eval_df_cv)

# Ubah ke long format untuk keperluan visualisasi
eval_long_cv <- eval_df_cv %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

# Visualisasi Barchart gabungan semua metrik
ggplot(eval_long_cv, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Perbandingan Metrik Evaluasi Model ML (Tuning+CV)",
    x = NULL,
    y = "Nilai",
    fill = "Metrik"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

######  ======================= Machine Learning dengan SpastialRF ======================= ######

# Tambahkan koordinat centroid
combined_spatialrf <- combined_data %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  )

# Spatial lag (lag_TPT)
nb_spatialrf <- poly2nb(combined_spatialrf)
lw_spatialrf <- nb2listw(nb_spatialrf, style = "W")
combined_spatialrf$lag_TPT <- lag.listw(lw_spatialrf, combined_spatialrf$TPT)

# Siapkan data untuk spatialRF
data_rf_spatial <- combined_spatialrf %>%
  st_drop_geometry() %>%
  dplyr::select(TPT, TPAK, UHH, HLS, Pengeluaran, Persentase_Miskin, lon, lat, lag_TPT)

# Buat distance matrix (knn)
coords <- data_rf_spatial[, c("lon", "lat")]
distance_matrix <- as.matrix(dist(coords))

# Fit random forest biasa
model_rf_spatialbase <- rf(
  data = data_rf_spatial,
  dependent.variable.name = "TPT",
  predictor.variable.names = colnames(data_rf_spatial)[colnames(data_rf_spatial) != "TPT"],
  distance.matrix = distance_matrix,
  distance.thresholds = 0,
  verbose = TRUE
)

# Fit spatialRF
model_rf_spatial <- rf_spatial(
  model = model_rf_spatialbase,
  method = "mem.moran.sequential",
  distance.thresholds = c(0, 100000),
  n.cores = 1,
  verbose = TRUE
)

## ternyata: The model residuals are not spatially correlated, there is no need to fit a spatial model

######  ======================= Evaluasi Lanjutan: Random Forest CV ======================= ######

## ----------- Uji Moran's I Residual RF CV ----------- ##

combined_data$resid_rf_cv <- ml_data_cv$TPT - predict(model_rf_cv, newdata = ml_data_cv)
moran.test(combined_data$resid_rf_cv, listw = lw)

## ----------- Visualisasi Peta Residual RF CV ----------- ##

combined_data_rf <- combined_data %>%
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  )

label_rf <- combined_data_rf %>%
  filter(resid_rf_cv == max(resid_rf_cv, na.rm = TRUE) |
           resid_rf_cv == min(resid_rf_cv, na.rm = TRUE))

buat_peta_residual_rf <- function(data, resid_col, judul, label_data, warna = c("#E3E674", "#B0E0E6", "#E7754A")) {
  ggplot(data) +
    geom_sf(aes(fill = !!sym(resid_col)), color = "white", size = 0.2) +
    geom_text_repel(
      data = label_data,
      aes(x = lon, y = lat, label = WADMKK),
      size = 3, color = "black", segment.color = "gray40"
    ) +
    scale_fill_gradient2(low = warna[1], mid = warna[2], high = warna[3], midpoint = 0) +
    theme_minimal() +
    labs(title = judul, fill = "Residual") +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}

peta_resid_rf_cv <- buat_peta_residual_rf(
  data = combined_data_rf,
  resid_col = "resid_rf_cv",
  judul = "Peta Residual Model Random Forest (CV)",
  label_data = label_rf
)

print(peta_resid_rf_cv)

## ----------- Visualisasi Variable Importance Model RF CV ----------- ##

rf_importance <- varImp(model_rf_cv)$importance
rf_importance$Variable <- rownames(rf_importance)

ggplot(rf_importance, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "#6FA98B") +
  coord_flip() +
  labs(
    title = "Variable Importance - Random Forest (CV)",
    x = NULL,
    y = "Importance"
  ) +
  theme_minimal()

######  ======================= Perbandingan Semua Model ======================= ######

# Evaluasi untuk model spasial
metrics_ols   <- evaluate_model(combined_data$TPT, combined_data$pred_ols)
metrics_sar   <- evaluate_model(combined_data$TPT, combined_data$pred_sar)
metrics_sem   <- evaluate_model(combined_data$TPT, combined_data$pred_sem)
metrics_sarma <- evaluate_model(combined_data$TPT, combined_data$pred_sarma)

# AIC dari model spasial
aic_ols   <- AIC(lm_model)
aic_sar   <- AIC(model_sar)
aic_sem   <- AIC(model_sem)
aic_sarma <- AIC(model_sarma)

# List hasil metrik
hasil_perbandingan <- data.frame(
  Model = c("OLS", "SAR", "SEM", "SARMA", "Tree CV", "RF CV", "GBM CV", "SVR CV"),
  RMSE  = c(
    metrics_ols["RMSE"],
    metrics_sar["RMSE"],
    metrics_sem["RMSE"],
    metrics_sarma["RMSE"],
    metrics_tree_cv["RMSE"],
    metrics_rf_cv["RMSE"],
    metrics_gbm_cv["RMSE"],
    metrics_svr_cv["RMSE"]
  ),
  MAE   = c(
    metrics_ols["MAE"],
    metrics_sar["MAE"],
    metrics_sem["MAE"],
    metrics_sarma["MAE"],
    metrics_tree_cv["MAE"],
    metrics_rf_cv["MAE"],
    metrics_gbm_cv["MAE"],
    metrics_svr_cv["MAE"]
  ),
  R2    = c(
    metrics_ols["R2"],
    metrics_sar["R2"],
    metrics_sem["R2"],
    metrics_sarma["R2"],
    metrics_tree_cv["R2"],
    metrics_rf_cv["R2"],
    metrics_gbm_cv["R2"],
    metrics_svr_cv["R2"]
  ),
  AIC   = c(
    aic_ols,
    aic_sar,
    aic_sem,
    aic_sarma,
    NA, NA, NA, NA  # AIC tidak relevan untuk model ML
  )
)

# Tampilkan tabel
print(hasil_perbandingan)

