## Library ####
library(shiny)
library(shinydashboard)
library(bslib)
library(leaflet)
library(pool) # untuk bekerja dengan banyak koneksi
library(glue) # untuk membuat query schema lebih dinamis
library(DBI)
library(RPostgres)
library(readxl)
library(dplyr)
library(sf)
library(mapview)
library(plotly)
library(tidyr)
library(jsonlite)l
library(colorspace)
library(shinyjs) # agar bisa menggunakan onclick
library(remotes)
library(DT)
library(reactable)
library(htmltools)
library(purrr)
library(lubridate)
library(stringr)
library(stringi)
library(leafgl)
library(rsconnect)


## Load Data ####
koneksi_database <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "forest_seed",
  host = "forestseed.crgqmswoorrt.ap-southeast-3.rds.amazonaws.com",     # atau IP server seperti "172.16.11.170"
  port = 5432,
  user = "postgres",      # ganti dengan username PostgreSQL kamu
  password = "D4t4b4s3F0r3st"   # ganti dengan password-nya
)

### Read Data Spatial ####

# Lokasi Administrasi 
#data_lokasi_administrasi <- st_read(
#koneksi_database, 
#query = "SELECT provinsi, kabupaten_kota, kecamatan, desa, geom_desa FROM t_lokasi_kerja_forest")

# Batas TN
data_tamannasional <- st_read(
  koneksi_database, 
  query = "SELECT * FROM public.t_batas_tn") %>%
  st_make_valid() %>%
  st_transform(4326)

# Area IPZ TNBBS
data_batas_area_ipz_tnbbs <- st_read(
  koneksi_database, 
  query = "SELECT * FROM schema_spatial.t_ipz_tnbbs_100ha_wgs84_ar") %>%
  st_make_valid() %>%
  st_transform(4326)

# KPH Aceh
data_batas_kph_aceh <- st_read(
  koneksi_database, 
  query = "SELECT * FROM schema_spatial.t_kph_aceh_pergupno1_2024_wgs84_ar") %>%
  st_make_valid() %>%
  st_transform(4326)

# Lahan Petani BBS
data_polygon_petani_bbs <- st_read(
  koneksi_database, 
  query = "SELECT * FROM schema_spatial.t_sl_lahan_kopi_ofis_baseline_wgs84_ar")%>%
  st_make_valid() %>%
  st_transform(4326) %>%
  st_cast("POLYGON")

# Batas TN
data_tamannasional <- st_read(
  koneksi_database, 
  query = "SELECT * FROM public.t_batas_tn")%>%
  st_make_valid() %>%
  st_transform(4326)

# Data Tabular Patroli
data_patroli <- st_read(
  koneksi_database,
  query = "
    SELECT *, geom_4326 AS geom
    FROM schema_patroli.t_patroli
    WHERE GeometryType(geom_4326) = 'POINT'
      AND ST_X(geom_4326) BETWEEN 90 AND 141
      AND ST_Y(geom_4326) BETWEEN -11 AND 6 ") %>% 
  st_transform(4326)  # Pastikan sudah WGS84

pilihan_obs_cat1 <- data_patroli %>%
  filter(observation_category_0 == "Aktivitas Manusia") %>%
  pull(observation_category_1) %>%
  unique() %>%
  sort() 

head(data_patroli$geom)

#### Pastikan geometri dibaca sebagai sf ####
ipz_tnbbs_sf <- st_as_sf(data_batas_area_ipz_tnbbs, wkt = "geom", crs = 4326)
batas_tamannasional_sf <- st_as_sf(data_tamannasional, wkt = "geom", crs = 4326)
polygon_petani_bbs_sf <- st_as_sf(data_polygon_petani_bbs, wkt = "geom", crs = 4326)
batas_kph_aceh_sf <- st_as_sf(data_batas_kph_aceh, wkt = "geom", crs = 4326)

# Tabular Data Patroli
# Daftar query dalam list
query_list <- list(
  data_smart_adopsi = "SELECT region, nama_upt, implementasi_smart, tahun FROM schema_patroli.t_smart_adoption",
  data_kategori_satwa = "SELECT * FROM schema_patroli.t_kategori_satwa",
  data_gridpatroli = "SELECT * FROM schema_patroli.t_grid_patroli",
  data_temuansatwa = "SELECT * FROM schema_patroli.view_summary_temuansatwa",
  data_temuanpatroli = "SELECT * FROM schema_patroli.view_summary_temuanpatroli",
  data_telur_maleo = "SELECT * FROM schema_maleo.t_entry_eggs",
  data_gangguan_maleo = "SELECT * FROM schema_maleo.t_entry_threat",
  data_temp_rain = "SELECT * FROM schema_maleo.t_entry_temp_rain",
  data_telur_per_grid = "SELECT * FROM schema_maleo.view_jumlah_telur_per_grid_ar",
  data_summary_total_petani = "SELECT * FROM schema_petani.view_summary_total_data_petani",
  data_kegiatan = "
    SELECT 
      jenis_kegiatan, judul_kegiatan, tanggal_mulai_kegiatan, tanggal_akhir_kegiatan,
      tempat_kegiatan, program_unit_terlibat, landscape, provinsi, kabupaten_kota,
      kecamatan, desa, dusun, jumlah_peserta_kegiatan, sumber_dana,
      detil_pendanaan, keterangan
    FROM schema_kegiatan.t_kegiatan
    ORDER BY id_kegiatan DESC",
  data_peserta = "
    SELECT nomor_handphone, nama_peserta, jenis_kelamin, asn_nonasn, 
           asal_instansi, posisi_jabatan
    FROM schema_kegiatan.t_peserta
    ORDER BY nama_peserta ASC",
  data_peserta_kegiatan = "SELECT * FROM schema_kegiatan.view_peserta_kegiatan",
  data_petani = "SELECT * FROM schema_petani.view_raw_data_petani_forestseed ORDER BY id_petani DESC",
  data_lahan_petani = "SELECT * FROM schema_petani.view_raw_data_lahan",
  data_kegiatan_training_gap = "SELECT * FROM schema_petani.view_raw_data_kegiatan_training",
  data_peserta_training_gap = "SELECT * FROM schema_petani.view_raw_data_peserta_training",
  data_petani_adopsi = "SELECT * FROM schema_petani.view_raw_data_petani_adopsi",
  data_petani_training_per_modul = "SELECT * FROM schema_petani.view_detail_petani_training",
  data_petani_adopsi_per_modul = "SELECT * FROM schema_petani.view_detail_petani_adopsi",
  data_kumulatif_data_peserta_training = "SELECT * FROM schema_petani.view_summary_kumulatif_data_peserta_training",
  data_kumulatif_data_petani_adopsi = "SELECT * FROM schema_petani.view_summary_kumulatif_data_petani_adopsi",
  data_petani_terlatih = "SELECT DISTINCT id_petani, landscape FROM schema_petani.view_detail_petani_training",
  data_petani_adopsigap = "SELECT DISTINCT id_petani, landscape FROM schema_petani.view_detail_petani_adopsi"
)

data_list <- lapply(query_list, function(q) dbGetQuery(koneksi_database, q))
list2env(data_list, envir = .GlobalEnv)

data_lahan_petani_terpetakan <- data_lahan_petani %>%
  filter(!is.na(longitude), !is.na(latitude))

# 3. Ubah jadi objek sf (asumsi: proyeksi awal EPSG:32748 = UTM Zone 48S)
sf_lahan_petani <- st_as_sf(data_lahan_petani_terpetakan, coords = c("longitude", "latitude"), crs = 32748)

# 4. Transformasi ke WGS84 (EPSG:4326) agar bisa ditampilkan di leaflet
sf_lahan_petani_wgs <- st_transform(sf_lahan_petani, crs = 4326)


poolClose(koneksi_database)

unit_terlibat_clean <- data_kegiatan$program_unit_terlibat |>
  strsplit(",") |>
  unlist() |>
  trimws() |>
  unique() |>
  sort()

options(shiny.host = "0.0.0.0")
options(shiny.port = 38383)