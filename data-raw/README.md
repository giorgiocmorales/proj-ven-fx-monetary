# Data Provenance

This folder documents how raw data for *proj-ven-fx-monetary* is obtained and prepared.

| Dataset | Source | Script | Output (in /data) | Notes |
|----------|---------|--------|------------------|--------|
| BCV official FX | BCV API | scripts/1_Download_BCV_TC_RefSMC.R | data/raw/ves_usd_fx.csv | Main reference rate |
| BCV inverted index | BCV API | scripts/2_Download_BCV_TC_InvIndx.R | data/raw/ves_usd_fx_inv.csv | Inverse rate |
| Yadio alternative FX | Yadio API | scripts/3_Download_yadio_TC.R | data/raw/yadio_fx.csv | Alternative source |
| Consolidated dataset | - | scripts/4_consolidate_fx_data.R | data/output/ves_usd_fx_consolidated.csv | Combined dataset |
