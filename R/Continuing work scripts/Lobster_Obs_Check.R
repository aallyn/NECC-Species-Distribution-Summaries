##### Diagnosing what is going on with lobster in 1970s #####
# The problem: Currently, missing COG for lobster when we go to the `trawl_yearday.qmd` analysis and not sure why that is happening. Specifically, only have a spring COG for 76 and then neither a spring or fall COG for 77 and 78.

# General workflow
# All of the work starts with a call to gmRi::gmri_survdat_prep()
devtools::install_github("https://github.com/gulfofmaine/gmRi", force = TRUE)
library(gmRi)

# Load NEFSC Bottom Trawl Survey data ####
trawl_data <- gmri_survdat_prep(
    survdat_source = "most-recent",
    box_location = "cloudstorage"
)

# What do we have for total abundance and biomass by season and year for lobster?
# By svspp code (301)
t <- trawl_data |>
    group_by(svspp, est_year, season) |>
    summarize("Total_Abundance" = sum(abundance), "Total_Biomass" = sum(biomass_kg)) |>
    filter(svspp == 301)

# By comnmae
t2 <- trawl_data |>
    group_by(comname, est_year, season) |>
    summarize("Total_Abundance" = sum(abundance), "Total_Biomass" = sum(biomass_kg)) |>
    filter(comname == "american lobster")

# In both of these, you can see that from 1973-1976, the lobster observations are INCREDIBLY small (a drop from ~ 650 total abundance in fall of 73, to 1 in fall 74) and there are no records of lobster in 77 or 78, but then fall 79 jumps right back up.

# Has this always been an issue?
t3 <- load(paste0(cs_path("RES_Data", "NMFS_trawl"), "SURVDAT_archived/Survdat_Nye_allseason.RData"))

t3 <- survdat |>
    group_by(SVSPP, EST_YEAR, SEASON) |>
    summarize("Total_Abundance" = sum(ABUNDANCE), "Total_Biomass" = sum(BIOMASS)) |>
    filter(SVSPP == 301)
View(t3)

t4<- readRDS(paste0(cs_path("RES_Data", "NMFS_trawl"), "SURVDAT_current/survdat_lw.rds"))[["survdat"]]

t5 <- t4 |>
    group_by(SVSPP, YEAR, SEASON) |>
    summarize("Total_Abundance" = sum(ABUNDANCE), "Total_Biomass" = sum(BIOMASS)) |>
    filter(SVSPP == 301)
View(t5)

# Those start the same. Where do we lose the lobster in the cleaning process? Walking through surv_dat_prep line by line
trawldat <- t4
trawldat <- janitor::clean_names(trawldat)
path_fun <- boxpath_switch(box_location = "cloudstorage")
mills_path <- path_fun("mills")
nmfs_path <- path_fun("res", "NMFS_trawl")
has_comname <- "comname" %in% names(trawldat)
has_id_col <- "id" %in% names(trawldat)
has_towdate <- "est_towdate" %in% names(trawldat)
has_month <- "est_month" %in% names(trawldat)
has_year <- "est_year" %in% names(trawldat)
has_catchsex <- "catchsex" %in% names(trawldat)
has_decdeg <- "decdeg_beglat" %in% names(trawldat)
has_avg_depth <- "avgdepth" %in% names(trawldat)
if (has_comname == FALSE) {
        message("no comnames found, merging records in with spp_keys/sppclass.csv")
        spp_classes <- readr::read_csv(paste0(nmfs_path, "spp_keys/sppclass.csv"), 
            col_types = readr::cols())
        spp_classes <- janitor::clean_names(spp_classes)
        spp_classes <- dplyr::mutate(.data = spp_classes, comname = stringr::str_to_lower(common_name), 
            scientific_name = stringr::str_to_lower(scientific_name))
        spp_classes <- dplyr::distinct(spp_classes, svspp, comname, 
            scientific_name)
        trawldat <- dplyr::mutate(trawldat, svspp = stringr::str_pad(svspp, 
            3, "left", "0"))
        trawldat <- dplyr::left_join(trawldat, spp_classes, by = "svspp")
    }
if (has_id_col == FALSE) {
        message("creating station id from cruise-station-stratum fields")
        trawldat <- dplyr::mutate(.data = trawldat, cruise6 = stringr::str_pad(cruise6, 
            6, "left", "0"), station = stringr::str_pad(station, 
            3, "left", "0"), stratum = stringr::str_pad(stratum, 
            4, "left", "0"), id = stringr::str_c(cruise6, station, 
            stratum))
}
if (has_year == FALSE) {
        message("renaming year column to est_year")
        trawldat <- dplyr::rename(trawldat, est_year = year)
}
if (has_decdeg == FALSE) {
        message("renaming lat column to decdeg_beglat")
        trawldat <- dplyr::rename(trawldat, decdeg_beglat = lat)
}
if (has_decdeg == FALSE) {
        message("renaming lon column to decdeg_beglon")
        trawldat <- dplyr::rename(trawldat, decdeg_beglon = lon)
}
if (has_avg_depth == FALSE) {
        message("renaming depth column to avgdepth")
        trawldat <- dplyr::rename(trawldat, avgdepth = depth)
}
if (has_towdate == TRUE) {
        message("building month/day columns from est_towdate")
        trawldat <- dplyr::mutate(.data = trawldat, est_month = stringr::str_sub(est_towdate, 
            6, 7), est_month = as.numeric(est_month), est_day = stringr::str_sub(est_towdate, 
            -2, -1), est_day = as.numeric(est_day), .before = season)
}
trawldat <- dplyr::mutate(.data = trawldat, comname = tolower(comname), 
        id = format(id, scientific = FALSE), svspp = as.character(svspp), 
        svspp = stringr::str_pad(svspp, 3, "left", "0"), season = stringr::str_to_title(season), 
        strat_num = stringr::str_sub(stratum, 2, 3))
trawldat <- dplyr::rename(.data = trawldat, biomass_kg = biomass, 
        length_cm = length)
trawldat <- dplyr::mutate(.data = trawldat, biomass_kg = ifelse(biomass_kg == 
        0 & abundance > 0, 1e-04, biomass_kg), abundance = ifelse(abundance == 
        0 & biomass_kg > 0, 1, abundance))
    trawldat <- dplyr::filter(.data = trawldat, stratum >= 1010, 
        stratum <= 1760, stratum != 1310, stratum != 1320, stratum != 
            1330, stratum != 1350, stratum != 1410, stratum != 
            1420, stratum != 1490)
    trawldat <- dplyr::filter(.data = trawldat, !is.na(biomass_kg), 
        !is.na(abundance))
    trawldat <- dplyr::filter(.data = trawldat, !svspp %in% c(285:299, 
        305, 306, 307, 316, 323, 910:915, 955:961))
    trawldat <- dplyr::filter(trawldat, !svspp %in% c(0, 978, 
        979, 980, 998))
    strata_key <- list(`Georges Bank` = as.character(13:23), 
        `Gulf of Maine` = as.character(24:40), `Southern New England` = stringr::str_pad(as.character(1:12), 
            width = 2, pad = "0", side = "left"), `Mid-Atlantic Bight` = as.character(61:76))
    trawldat <- dplyr::mutate(trawldat, survey_area = dplyr::case_when(strat_num %in% 
        strata_key$`Georges Bank` ~ "GB", strat_num %in% strata_key$`Gulf of Maine` ~ 
        "GoM", strat_num %in% strata_key$`Southern New England` ~ 
        "SNE", strat_num %in% strata_key$`Mid-Atlantic Bight` ~ 
        "MAB", TRUE ~ "stratum not in key"))
    strata_select <- c(strata_key$`Georges Bank`, strata_key$`Gulf of Maine`, 
        strata_key$`Southern New England`, strata_key$`Mid-Atlantic Bight`)
    trawldat <- dplyr::filter(trawldat, strat_num %in% strata_select)
    trawldat <- dplyr::mutate(trawldat, stratum = as.character(stratum))
    if (has_catchsex == TRUE) {
        abundance_groups <- c("id", "comname", "catchsex", "abundance")
    } else {
        message("catchsex column not found, ignoring sex for numlen adjustments")
        abundance_groups <- c("id", "comname", "abundance")
    }
    abundance_check <- dplyr::group_by(trawldat, !!!rlang::syms(abundance_groups))
    abundance_check <- dplyr::summarise(.data = abundance_check, 
        abund_actual = sum(numlen), n_len_class = dplyr::n_distinct(length_cm), 
        .groups = "drop")
    conv_factor <- dplyr::distinct(trawldat, !!!rlang::syms(abundance_groups), 
        length_cm)
    conv_factor <- dplyr::inner_join(conv_factor, abundance_check, 
        by = abundance_groups)
    conv_factor <- dplyr::mutate(conv_factor, convers = abundance/abund_actual)
    survdat_processed <- dplyr::left_join(trawldat, conv_factor, 
        by = c(abundance_groups, "length_cm"))
    survdat_processed <- dplyr::mutate(survdat_processed, numlen_adj = numlen * 
        convers, .after = numlen)
    survdat_processed <- dplyr::select(survdat_processed, -c(abund_actual, 
        convers))
    rm(abundance_check, conv_factor, strata_key, strata_select)
    trawl_lens <- dplyr::filter(.data = survdat_processed, is.na(length_cm) == 
        FALSE, is.na(numlen) == FALSE, numlen_adj > 0)
    trawl_clean <- dplyr::distinct(.data = trawl_lens, id, svspp, 
        comname, catchsex, abundance, n_len_class, length_cm, 
        numlen, numlen_adj, biomass_kg, .keep_all = TRUE)
