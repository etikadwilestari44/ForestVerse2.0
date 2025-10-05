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
library(jsonlite)
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
library(kableExtra)
library(xfun)
library(shinyWidgets)

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
  data_summary_total_petani = "SELECT * FROM schema_petani.view_summary_total_data_petani",
  data_peserta_kegiatan = "SELECT * FROM schema_kegiatan.view_peserta_kegiatan",
  data_petani = "SELECT * FROM schema_petani.view_raw_data_petani ORDER BY id_petani DESC",
  data_lahan_petani = "SELECT 
  t.*,
  ST_AsText(t.geom_polygon_lahan) AS geom_polygon_lahan_wkt,
  ST_AsText(t.geom_point_lahan)   AS geom_point_lahan_wkt FROM schema_petani.view_raw_data_lahan t",
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

# Polygon lahan petani dari kolom geom_polygon_lahan
polygon_petani_sf <- data_lahan_petani %>%
  filter(!is.na(geom_polygon_lahan_wkt)) %>%
  st_sf(
    .,
    geometry = st_as_sfc(.$geom_polygon_lahan_wkt, crs = 4326)
  ) %>%
  st_cast("POLYGON")   # pecah MULTIPOLYGON jadi POLYGON



# Titik lahan petani dari kolom geom_point_lahan
point_petani_sf <- data_lahan_petani %>%
  filter(!is.na(geom_point_lahan_wkt)) %>%           # hanya yang ada WKT
  st_sf(
    .,
    geometry = st_as_sfc(.$geom_point_lahan_wkt, crs = 4326)
  )
# Filter temuan kebun kopi & sawit
data_kebun <- data_patroli %>%
  select(
    landscape,
    patrol_id,
    patrol_start_date,
    patrol_end_date,
    station,
    patrol_transport_type,
    waypoint_id,
    waypoint_date,
    waypoint_time,
    x,
    y,
    geom,
    geom_4326,
    observation_category_1,
    lokasi,
    tipe_tutupan_lahan,
    pelanggaran,
    nama_pelaku,
    nama_pelaku_indikatif,
    asal,
    umur,
    jenis_kelamin,
    jumlah_pelaku,
    tindakan,
    tipe_temuan,
    usia_temuan,
    keaktifan,
    jumlah,
    satuan,
    keterangan
  )%>%
  dplyr::filter(
    observation_category_1 == 'Penggunaan Kawasan',
    landscape %in% c('Taman Nasional Bukit Barisan Selatan', 'Suaka Margasatwa Rawa Singkil'),
    tipe_temuan %in% c('Kebun kopi', 'Kebun Sawit')
  )

poolClose(koneksi_database)

unit_terlibat_clean <- data_kegiatan$program_unit_terlibat |>
  strsplit(",") |>
  unlist() |>
  trimws() |>
  unique() |>
  sort()
