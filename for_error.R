
rm(list = ls())

#自分のsewdに変更
setwd("~/Desktop/zikkenmonday")


# install.packages("osrm")
# install.packages("readr")
# install.packages("googleway")
# install.packages("geosphere")
library(geosphere)
library(osrm)
library(dplyr)
library(readr)
# library(googleway)

locations <- read_csv("LocationData.csv")

locations$cluster_category <- cut(locations$cluster,
                                  breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10),
                                  labels = c("c1", "c2", "c3","c4", "c5", "c6","c7", "c8", "c9","c10"))

table(locations$cluster_category)



# 緯度・経度の名前を変更
locations <- locations %>%
  rename(
    lat = latitude,
    lon = longitude
  )



# OSRM サーバーの設定
options(osrm.server = "https://router.project-osrm.org/")
options(osrm.profile = "car")

# クラスタ名のリスト
cluster_list <- paste0("c", 1:10)

# 各クラスタに対してループ処理
for (cluster_name in cluster_list) {
  # 該当クラスタのデータを抽出し ID を付与
  d <- locations %>%
    filter(cluster_category == cluster_name) %>%
    mutate(id = as.character(row_number()))
  
  # データ数が2未満ならスキップ（from と to 両方必要）
  if (nrow(d) < 2) {
    warning(paste("クラスタ", cluster_name, "のデータが少なすぎます。スキップします。"))
    next
  }
  
  # 出発地（最後の1地点）と目的地（それ以外）を分離
  from <- d[nrow(d), ] %>%
    dplyr::select(id, lon, lat) %>%
    as.data.frame()
  rownames(from) <- from$id
  from$id <- NULL
  
  to <- d[1:(nrow(d) - 1), ] %>%
    dplyr::select(id, lon, lat) %>%
    as.data.frame()
  rownames(to) <- to$id
  to$id <- NULL
  
  # ランダムに最大268地点を to から選択
  set.seed(123)
  n_sample <- 267
  to_new <- to[sample(nrow(to), n_sample), ]
  
  # OSRM APIで距離計算
  route <- osrmTable(src = from, dst = to_new, measure = "distance")
  
  
  ### Haversine距離を計算（直線距離）
  from_coords <- as.numeric(from[1, c("lon", "lat")])
  to_coords <- to_new[, c("lon", "lat")]
  distances_m <- distHaversine(p1 = matrix(from_coords, nrow = 1), p2 = as.matrix(to_coords))
  
  df_distance <- data.frame(
    to_id = rownames(to_new),
    distance_m = distances_m
  )
  rownames(df_distance) <- df_distance$to_id
  df_distance$to_id <- NULL
  # df_matrix <- t(df_distance)
  rownames(df_distance) <- gsub("^X", "", rownames(df_distance))
  
  straight_file <- paste0("straight_distances_", cluster_name, ".csv")
  
  write.csv(df_distance, file = straight_file)
  
  ### 直線距離おわり
  
  # ファイル名（クラスタ名付き）で保存
  dist_file <- paste0("route_distances_", cluster_name, ".csv")
  transposed_file <- paste0("transposed_output_", cluster_name, ".csv")
  
  write.csv(route$distances, file = dist_file, row.names = TRUE)
  
  # データの転置と保存
  data <- read.csv(dist_file, header = TRUE, row.names = 1)
  transposed_data <- t(data)
  rownames(transposed_data) <- gsub("^X", "", rownames(transposed_data))
  write.csv(transposed_data, transposed_file)
  
  dis_data <- cbind(transposed_data, df_distance)
  colnames(dis_data) <- c("real", "straight")
  
  dis_file <- paste0("distance_data_", cluster_name, ".csv")
  write.csv(dis_data, dis_file)
  
  message(paste("クラスタ", cluster_name, "の処理が完了しました。"))
}


d1 <- read.csv("distance_data_c1.csv", header = TRUE, row.names = 1)
d2 <- read.csv("distance_data_c2.csv", header = TRUE, row.names = 1)
d3 <- read.csv("distance_data_c3.csv", header = TRUE, row.names = 1)
d4 <- read.csv("distance_data_c4.csv", header = TRUE, row.names = 1)
d5 <- read.csv("distance_data_c5.csv", header = TRUE, row.names = 1)
d6 <- read.csv("distance_data_c6.csv", header = TRUE, row.names = 1)
d7 <- read.csv("distance_data_c7.csv", header = TRUE, row.names = 1)
d8 <- read.csv("distance_data_c8.csv", header = TRUE, row.names = 1)
d9 <- read.csv("distance_data_c9.csv", header = TRUE, row.names = 1)
d10 <- read.csv("distance_data_c10.csv", header = TRUE, row.names = 1)

summary(d1)
summary(d2)
summary(d3)
summary(d4)
summary(d5)
summary(d6)
summary(d7)
summary(d8)
summary(d9)
summary(d10)
