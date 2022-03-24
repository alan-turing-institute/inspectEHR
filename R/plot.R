#' @title Plot CC-HIC event contribution
#'
#' @template param-x
#' @template param-reference-table
#' @param sites_col character vector containing HEX codes for the colour used to
#'   represent each site. The names of the vector should be the site names as
#'   they are to be displayed in the plot legend.
#'
#' @importFrom rlang .data quo !!
#' @importFrom stringr str_trunc
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab
#'   theme_minimal ggtitle guides guide_legend element_text
plot_contribution <- function(x = NULL,
                              reference_tbl = NULL,
                              sites_col = NULL) {

  code_name <- attr(x, "code_name")

  value_title <- str_trunc(
    qref[qref$code_name == code_name, "short_name", drop = TRUE], 40)
  subtitle <- gsub("NIHR_HIC_ICU_", "Contribution profile: ", code_name)
  x_lab <- "Date"

  if (is.null(sites_col)) {
    sites_col <- viridis_pal()(length(unique(x$site)))
    names(sites_col) <- unique(x$site)
  }

  data_path <- x %>%
    left_join(reference_tbl %>%
                select(.data$episode_id, .data$start_date),
              by = "episode_id") %>%
    group_by(
      .data$site,
      year = lubridate::year(.data$start_date),
      month = lubridate::month(.data$start_date)
    ) %>%
    summarise(
      event_count = n()
    ) %>%
    mutate(
      date = as_date(paste(year, month, "01", sep = "-"))
    )

  beggining <- reference_tbl %>%
    group_by(.data$site) %>%
    summarise(begin_ = as.Date(min(.data$start_date))) %>%
    mutate(begin_ = jitter_dates(.data$begin_))

  ending <- reference_tbl %>%
    group_by(.data$site) %>%
    summarise(end_ = as.Date(max(.data$start_date))) %>%
    mutate(end_ = jitter_dates(.data$end_))

  out <- ggplot(data = NULL) +
    ggplot2::geom_path(
      aes(
        x = .data$date,
        y = .data$event_count,
        group = .data$site,
        colour = .data$site),
      data = data_path) +
    ggplot2::geom_vline(
      aes(
        xintercept = .data$begin_,
        colour = .data$site),
      linetype = 2,
      data = beggining) +
    ggplot2::geom_vline(
      aes(
        xintercept = .data$end_,
        colour = .data$site),
      linetype = 2,
      data = ending) +
    ggplot2::lims(
      x = c(min(as.Date(reference_tbl$start_date)),
            max(as.Date(reference_tbl$start_date)) + lubridate::days(30))) +
    labs(y = "Monthly event count", x = x_lab) +
    theme_cchic() +
    theme(
      text = element_text(size = 8)
    ) +
    ggtitle(value_title, subtitle = subtitle)

  return(out)
}


#' @title Plot CC-HIC event eCDF
#'
#' @template param-x
#' @param sites_col character vector containing HEX codes for the colour used to
#'   represent each site. The names of the vector should be the site names as
#'   they are to be displayed in the plot legend.
#' @param col_name the target column name to display. Defaults to "value".
#'
#' @importFrom rlang .data quo !!
#' @importFrom stringr str_trunc
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes scale_colour_manual stat_ecdf xlab ylab
#'   theme_minimal ggtitle guides guide_legend
plot_ecdf <- function(x, sites_col = NULL, col_name = c("value", "datetime")) {

  col_name <- match.arg(col_name)
  code_name <- attr(x, "code_name")

  value_title <- str_trunc(
    qref[qref$code_name == code_name, "short_name", drop = TRUE], 40)

  if (col_name == "value") {
    subtitle <- gsub("NIHR_HIC_ICU_", "Event: ", code_name)
    x_lab <- qref[qref$code_name == code_name, "assumed_units",
                  drop = TRUE]
    if (is.na(x_lab)) x_lab <- "Units not defined"
  } else {
    subtitle <- "Time of contribution"
    x_lab <- "Time of day"
  }

  if (is.null(sites_col)) {
    sites_col <- viridis_pal()(length(unique(x$site)))
    names(sites_col) <- unique(x$site)
  }

  if (col_name == "value") {
    x <- x %>%
      mutate(plotting_value = .data$value)
  } else {
    x <- x %>%
      mutate(plotting_value = hms::as_hms(.data$datetime))
  }

  x <- x %>%
    mutate(site = factor(.data$site, levels = names(sites_col))) %>%
    ggplot(
      aes(
        x = .data$plotting_value,
        colour = .data$site)) +
    scale_colour_manual(
      values = sites_col) +
    stat_ecdf() +
    xlab(x_lab) +
    ylab("F(x)") +
    ggtitle(
      label = value_title,
      subtitle = subtitle) +
    theme_cchic() +
    theme(
      text = element_text(size = 8)
    ) +
    guides(
      colour = guide_legend(title = "Site"))

  return(x)
}


#' @title Plot CC-HIC event histogram
#'
#' Plot a histogram of a CC-HIC event.
#'
#' @template param-x
#' @param sites_col the HEX colour pallet for each site
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tidyr complete
#' @importFrom dplyr filter select group_by ungroup mutate summarise
#' @importFrom scales percent_format
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_y_continuous
#'   xlab ylab theme_minimal aes geom_linerange position_dodge geom_point
#'
#' @family plotting
#' @export
#' @md
plot_hist <- function(x, sites_col = NULL) {

  .code_name <- attr(x, "code_name")

  value_title <- str_trunc(
    qref[qref$code_name == .code_name, "short_name", drop = TRUE], 40)
  subtitle <- gsub("NIHR_HIC_ICU_", "Event: ", .code_name)
  x_lab <- qref[qref$code_name == .code_name, "assumed_units"]
  if (is.na(x_lab)) x_lab <- "Units not defined"

  if (is.null(sites_col)) {
    sites_col <- viridis_pal()(length(unique(x$site)))
    names(sites_col) <- unique(x$site)
  }

  all_value_levels <- qref %>%
    filter(.data$code_name == .code_name) %>%
    select(.data$possible_values) %>%
    unnest(cols = c(.data$possible_values)) %>%
    pull()

  x <- x %>%
    mutate(site = factor(.data$site, levels = names(sites_col))) %>%
    group_by(.data$site, .data$value) %>%
    summarise(n = n()) %>%
    group_by(.data$site) %>%
    mutate(total = sum(n)) %>%
    ungroup() %>%
    complete(site, value) %>%
    mutate(value = factor(.data$value, levels = all_value_levels)) %>%
    ggplot(aes(x = .data$value)) +
    geom_linerange(
      aes(
        colour = .data$site,
        ymin = 0,
        ymax = .data$n / .data$total,
        group = .data$site),
      position = position_dodge(width = 0.5)) +
    geom_point(
      aes(
        colour = .data$site,
        y = .data$n / .data$total),
      position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = sites_col) +
    scale_y_continuous(labels = percent_format()) +
    xlab(x_lab) +
    ylab("Percentage by site") +
    ggtitle(
      label = value_title,
      subtitle = subtitle) +
    theme_cchic() +
    theme(
      text = element_text(size = 8)
    ) +
    guides(fill = guide_legend(title = "Site"))

  return(x)
}


plot_time <- function(x, sites_col = NULL, col_name = "datetime") {
  plot_ecdf(x = x, sites_col = sites_col, col_name = col_name)
}

#' @title Plot Kolmogorov-Smirnov Test
#'
#' @details Produces a plot of the KS distances between sites. This is quite
#'   hard coded at the moment, and so could be improved in a future version
#'
#' @param x an object returned from [ks_test()]
#' @template param-reference-table
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr tibble mutate select bind_rows mutate_at vars funs
#' @importFrom ggplot2 aes geom_tile scale_fill_viridis_c theme_minimal coord_equal ylab xlab geom_text
#' @importFrom scales viridis_pal
#' @md
plot_ks <- function(x, reference_tbl) {

  .code_name <- attr(x, "code_name")

  value_title <- qref[qref$code_name == .code_name, "short_name", drop = TRUE]
  subtitle <- "Kolmogorov-Smirnov distance comparison"

  sites <- na.omit(unique(reference_tbl$site))
  df <- select(x, .data$site_a, .data$site_b, .data$statistic)

  ## Add in a manual KS distance from sites that are the same.
  ## Adds a visual clue of ground zero.
  df <- bind_rows(
    df,
    df %>% select(site_a = site_b, site_b = site_a, statistic),
    tibble(
      site_a = sites,
      site_b = sites,
      statistic = 0
    )
  )

  out <- df %>%
    ggplot(aes(x = .data$site_a, y = .data$site_b)) +
    geom_tile(aes(fill = .data$statistic)) +
    scale_fill_viridis_c(limits = c(0, 1)) +
    geom_text(aes(label = round(.data$statistic, 2)), colour = "white") +
    coord_equal() +
    ylab("Comparitor Site A") +
    xlab("Comparitor Site B") +
    guides(fill = guide_legend(title = "KS Distance")) +
    theme_cchic() +
    theme(
      text = element_text(size = 8)
    ) +
    ggtitle(value_title, subtitle = subtitle)

  return(out)
}


# For plotting invalid months
#
# for (i in 1:length(all_sites)) {
#
#   invalid_months %>%
#     mutate(x = (month*4.3)-1,
#            y = 4,
#            text = "!") %>%
#     filter(site == all_sites[i]) -> invalids
#
#   temp_data <-
#     retrieve_unique_cases(episodes, provenance) %>%
#     report_cases_byday(bysite = all_sites[i])
#
#   temp_cal <- create_calendar(temp_data)
#   temp_grid <- create_grid(temp_cal)
#
#   temp_cal %>%
#     ggplot() +
#     geom_tile(
#       aes(x = week_of_year, y = day_of_week, fill = episodes), colour = "#FFFFFF") +
#     scale_fill_gradientn(colors = c("#B5E384", "#FFFFBD", "#FFAE63", "#D61818"), na.value = "grey90") +
#     facet_grid(year~.) +
#     geom_text(aes(x = x, y = y, label = text), colour = "red", data = invalids) +
#     theme_minimal() +
#     theme(panel.grid.major=element_blank(),
#           plot.title = element_text(hjust = 0.5),
#           axis.text.x = element_blank(),
#           axis.title.y = element_blank(),
#           axis.title.x = element_blank()) +
#     geom_segment(aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
#                  colour = "black", size = 0.5, data = temp_grid) +
#     labs(title = paste0("Admission Calendar Heatmap for " , names(all_sites[i]))) +
#     ylab(label = "Day of Week") +
#     xlab(label = "Month") +
#     coord_equal() -> temp_plot
#
#   ggsave(temp_plot,
#          filename = paste0("~/Projects/dataQuality/plots/admission_", all_sites[i], "_valid.png"),
#          dpi = 300)
#
#   rm(temp_data, temp_cal, temp_grid, temp_plot)
#
# }
#
# rm(i)

#' Plot Calendar Heatmap
#'
#' Builds the ggplot layers required to correctly display the calendar heatmap
#'
#' @param heat_cal a table of class `heat_cal` returned from
#'   \code{\link{make_heatcal}}
#' @param scale_max maximum number of daily admissions to scale fill
#'
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradientn facet_grid
#' theme_minimal theme element_blank element_text geom_segment aes labs
#' ylab xlab coord_equal
#' @importFrom scales viridis_pal
#' @importFrom rlang .data
plot_heat_cal <- function(heat_cal, scale_max = NULL) {

  type <- attr(heat_cal, "type")
  #max_limit <- attr(heat_cal, "max")
  grid_lines <- attr(heat_cal, "grid")

  if (!is.null(scale_max)) {
    max_limit <- scale_max
  } else {
    max_limit <- max(heat_cal[type], na.rm = TRUE)
  }

  if (type == "episodes") {
    title <- "Admission Profile"
    subtitle <- paste0("Site: ", attr(heat_cal, "site"))
    guide_title <- "Admissions"
  } else {
    title <- "Event Profile"
    subtitle <- paste0("Site: ", attr(heat_cal, "site"))
    guide_title <- "Events"
  }

  heat_cal %>%
    ggplot() +
    geom_tile(aes(
      x = .data$week_of_year,
      y = .data$day_of_week,
      fill = .data[[type]]),
      colour = "#FFFFFF") +
    facet_grid(.data$year ~ .) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    geom_segment(
      aes(x = .data$x_start, y = .data$y_start,
          xend = .data$x_end, yend = .data$y_end),
      colour = "black", size = 0.5,
      lineend = "round", linejoin = "round",
      data = grid_lines
    ) +
    scale_fill_viridis_c(
      rescaler = function(x, to = c(0, 1), from = NULL) {
        if_else(
          x <= max_limit,
          scales::rescale(
            x, to = to, from = c(min(x, na.rm = TRUE), max_limit)), 1
      )},
      na.value = "grey60",
      limits = c(0, max_limit)) +
    labs(title = title, subtitle = subtitle) +
    ylab(label = "Day of Week") +
    xlab(label = "Month") +
    guides(fill = guide_legend(
      title = guide_title)) +
    coord_equal() +
    theme(
      text = element_text(size = 8)
    )

}

#' @title Plot missing CC-HIC events
#'
#' @param missing_events a missing events table returned from
#'    [evaluate_global_missingness()]
#'
#' @import patchwork
#' @importFrom ggplot2 ggplot aes geom_tile theme theme_minimal element_blank
#' element_text ylab xlab scale_fill_manual
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @md
plot_missing_events <- function(missing_events) {
  duo <- c("#666666", "#0183c4")

  # Check contiguous missing
  missing_events <- missing_events %>%
    filter(.data$eval_code == "VE_CP_02") %>%
    distinct(.data$site, .data$code_name) %>%
    mutate(contributed = "No")

  all_sites <- unique(missing_events$site)

  all_events <- tidyr::expand_grid(
    site = all_sites,
    code_name = qref$code_name
  )

  all_events <- all_events %>%
    left_join(missing_events,
              by = c("site", "code_name")) %>%
    mutate(contributed = if_else(
      is.na(.data$contributed), "Yes", .data$contributed
    )) %>%
    mutate(short_code = stringr::str_sub(
      .data$code_name, start = 14, end = 17
    ))

  missing_events_plot1 <- all_events %>%
    filter(.data$code_name %in% qref$code_name[1:128]) %>%
    ggplot(aes(
      x = .data$site,
      y = .data$short_code,
      fill = .data$contributed)) +
    geom_tile() +
    theme_minimal() +
    scale_fill_manual(values = duo) +
    theme(
      text = element_text(size = 8),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      axis.text.y = element_text(size = 6),
      legend.position = "none"
    ) +
    ylab("CC-HIC event code") +
    xlab("Site")

  missing_events_plot2 <- all_events %>%
    filter(.data$code_name %in% qref$code_name[129:255]) %>%
    ggplot(aes(x = .data$site,
               y = .data$short_code,
               fill = .data$contributed)) +
    geom_tile() +
    theme_minimal() +
    scale_fill_manual(values = duo) +
    theme(
      text = element_text(size = 8),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      axis.text.y = element_text(size = 6),
      axis.title.y = element_blank()
    ) +
    ylab("Code and Name of event") +
    xlab("Site")

  missing_events_plot1 + missing_events_plot2
}

#' Plot Chronology Violations
#'
#' @param df
#'
#' @importFrom ggplot2 ggplot geom_tile geom_text theme_minimal coord_fixed
#'   theme aes element_blank element_text scale_x_discrete scale_y_discrete
#'   element_line
#' @importFrom rlang .data
#' @importFrom tibble tribble
#' @importFrom dplyr mutate
#'
#' @return
#' @export
#'
#' @examples
plot_chronology <- function(df) {

  chrono_codes <- tribble(
    ~full_name, ~short_name,
    "date of birth", "dob",
    "hospital admission", "admission_hosp",
    "icu admission", "admission_icu",
    "ready for discharge", "ready_discharge_dttm",
    "withdrawal", "withdraw_dttm",
    "death", "death_dttm",
    "brain stem death", "bsd_dttm",
    "body removal", "body_removed_dttm",
    "icu discharge", "discharge_icu",
    "hospital discharge", "discharge_hosp",
    "today", "today"
  )

  df %>%
    mutate(
      `current event` = factor(.data$short_name,
                               levels = chrono_codes$short_name,
                               labels = chrono_codes$full_name),
      `next event` = factor(.data$name_next,
                            levels = chrono_codes$short_name,
                            labels = chrono_codes$full_name),
    ) %>%
    group_by(`current event`, `next event`) %>%
    tally() %>%
    ggplot(aes(x = .data$`current event`,
               y = .data$`next event`,
               fill = .data$n)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = .data$n),
              colour = "red", size = 4) +
    labs(x = "primary event", y = "secondary event") +
    theme_minimal() +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    coord_fixed() +
    theme(
      text = element_text(size = 8),
      panel.grid.major = element_line(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}

#' CC-HIC ggplot2 Theme
#'
#' @param ... further arguments to pass to [ggplot2::theme()]
#'
#' @export
#' @md
#'
#' @importFrom ggplot2 %+replace% theme theme_bw element_blank
theme_cchic <- function(...) {
  pct <- theme_bw(base_family = "sans", base_size = 8) %+replace%
    theme(
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      ...
    )
}
