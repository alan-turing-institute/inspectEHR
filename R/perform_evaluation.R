#' @title Perform CC-HIC Data Quality Evaluation
#'
#' @details Performs a full data quality evaluation for CC-HIC. This wraps all
#'   other functions in `inspectEHR` as necessary to evaluate the whole CC-HIC
#'   database in one easy sweep. This can take some time so please grab a
#'   coffee!
#'
#' @param connection a database connection object returned by [DBI::dbConnect()]
#' @param output_folder character vector length 1 for the file path name for
#'   plots to be stored. If left as `NULL` (the default) then no plots will be
#'   written out.
#' @param translate_site a lookup table to translate site names on export in a
#'   consistent way. The table requires two columns: `site` and `translation`.
#'   The `site` column should be a character vector with names of the existing
#'   sites as they are stored in the database (e.g. `c("GSTT", "RGT")` etc).
#'   The `translation` column should be a character vector with new names to
#'   obfuscate the real names (e.g. `c("Site A", "Site B")` etc). This is a 1:1
#'   mapping so doesn't confer any real privacy advantage, but is useful as a
#'   means to casually obscure data origins.
#' @param test_comparisons a lookup table with the following columns:
#'   * event_a (chr): `code_name` for event to be used on the left hand side
#'   of the evaluation in [evaluate_comparison()].
#'   * event_b (chr): `code_name` for event to be used on the right hand side
#'   of the evaluation in [evaluate_comparison()].
#'   * operation (chr): string to identify the comparison to be made. See the
#'   documentation in the [evaluate_comparison()] function.
#'
#'   The default behaviour is to evaluate the comparisons found in the
#'   `.comparisons_lookup` object. This object also serves to illustrate the
#'   correct format to use.
#' @param verbose logical flag to print progress to the console
#' @param .debug logical flag to use internal test data
#'
#' @importFrom dplyr select arrange pull left_join distinct collect tibble
#'   anti_join mutate copy_to summarise_all bind_rows
#' @importFrom DBI dbWriteTable dbConnect
#' @importFrom glue glue
#' @importFrom lubridate yday
#' @importFrom readr write_csv
#' @importFrom rlang inform abort
#' @importFrom scales viridis_pal
#' @importFrom tidyr pivot_longer expand_grid
#' @importFrom tidyselect everything
#' @importFrom tibble add_column
#' @importFrom cli cli_h1 cli_h2 cli_h3 cli_alert_info cli_alert_success
#' cli_alert cli_text
#' @importFrom purrr iwalk map
#' @importFrom ggplot2 ggsave
#' @importFrom RSQLite SQLite
#'
#' @return TRUE if completes without errors
#'
#' @export
#' @md
perform_evaluation <- function(
  connection = NULL,
  output_folder = NULL,
  translate_site = NULL,
  test_comparisons = NULL,
  verbose = TRUE
  #,.debug = FALSE
  ) {

  if (verbose) {
    cli_h1("Starting data quality evaluation")
    cli_alert_info("this can take some time, you may wish to grab a coffee...")
  }

  if (!is.null(output_folder)) {
    write_plots <- TRUE
    setup_folders(
      output_folder = output_folder,
      write_plots = write_plots)
  } else {
    write_plots <- FALSE
  }

  if (verbose) cli_h1("Starting episode evaluation")

  # Useful Tables
  core <- make_core(connection = connection
                    #,.debug = .debug
                    )
  reference <- make_reference(
    connection = connection,
    translate_site = translate_site
    #,.debug = .debug
    )

  if (verbose) cli_alert_success("Working tables built")

  if (is.null(translate_site)) {
    all_sites <- unique(reference$site)
  } else {
    all_sites <- translate_site$translation
  }

  if (write_plots) {
    ## Capture colour profile for consistency
    all_sites.col <- viridis_pal()(length(all_sites))
    names(all_sites.col) <- all_sites
    ## margins
    a4_size <- c(width = 210, height = 297)
    a4_margin <- c(left = 18, top = 15, right = 15, bottom = 15)

    full_width <- a4_size["width"] - (a4_margin["left"] + a4_margin["right"])
    full_height <- a4_size["height"] - (a4_margin["top"] + a4_margin["bottom"])
  }

  # Setup a list to contain useful tables
  tbls <- vector(mode = "list", length = 4)
  names(tbls) <- c("events", "episodes", "provenance", "variables")

  # if (.debug) {
  #   tbls[["episodes"]] <- .episodes
  #   tbls[["provenance"]] <- .provenance
  #   tbls[["variables"]] <- .variables
  #   tbls[["events"]] <- .events
  # } else {
    # Collect small tables into working memory
    tbls[["episodes"]] <- collect(tbl(connection, "episodes"))
    tbls[["provenance"]] <- collect(tbl(connection, "provenance"))
    tbls[["variables"]] <- collect(tbl(connection, "variables"))

    # Keep events table in db
    tbls[["events"]] <- tbl(connection, "events")
  # }

  # # Cases ----
  # # Gives a tibble of admission numbers (patients/episodes) by week
  # admissions_weekly <- weekly_admissions(reference)
  #
  # # Gives overall admission numbers (totals) for patients/episodes
  # admissions_by_unit <-
  #   total_unit_admissions(
  #     events_table = tbls[["events"]],
  #     reference_table = reference
  #   )
  #
  # write_csv(admissions_by_unit,
  #           file.path(output_folder, "data/admissions_by_unit.csv"))

  if (write_plots) {

    # Prepare data for plotting
    admission_heat_data <- all_sites %>%
      map(~ make_heatcal(reference, site = .x))
    names(admission_heat_data) <- all_sites

    admission_heat_data %>%
      iwalk(function(x, y) {

        years <- length(unique(x$year))

        plot_heat_cal(x, scale_max = 30) %>%
          ggsave(
            plot = .,
            filename = file.path(
              output_folder,
              "plots/episodes",
              paste0(y, "_admissions.pdf")
            ),
            height = (30*years)+20,
            width = 257,
            units = "mm"
          )
      }
      )

    if (verbose) cli_alert_success("Admission profiles drawn")
  }

  # Characterise episodes
  episode_length <-
    characterise_episodes(connection = connection
                          #,.debug = .debug
                          )
  episode_length <- evaluate_episodes(episode_length)

  if (verbose) cli_alert_success("Episode characterisation finished")

  # Write out this validation to the database
  episodes_quality <- attr(episode_length, "invalid_records")

  # Remove the event_qe table before writing out
  if (DBI::dbExistsTable(connection, "episodes_quality")) {
    DBI::dbRemoveTable(connection, "episodes_quality")
  }
  
  write_notify(connection = connection,
               target_name = "episodes_quality",
               local_table = episodes_quality,
               verbose = verbose)
  
  if (verbose) {
    cli_alert_success("Finished episode evaluation")
    cli_h2("Starting event evaluation")
    cli_h3("Evaluating missing events")
  }

  events_missing <- evaluate_global_missingness(
    core_tbl = core,
    reference_tbl = reference)

  # Remove the event_qe table before writing out
  if (DBI::dbExistsTable(connection, "events_missing")) {
    DBI::dbRemoveTable(connection, "events_missing")
  }

  write_notify(connection = connection,
               target_name = "events_missing",
               local_table = events_missing,
               verbose = verbose)

  if (write_plots) {
    # make a plot highlighting missing data
    me_plot <- plot_missing_events(events_missing)
    ggsave(
      filename = file.path(
        output_folder,
        "plots/events_missing.pdf"
      ),
      plot = me_plot,
      height = full_height,
      width = full_width,
      units = "mm"
    )
    rm(me_plot)
    if (verbose) cli_alert_success("Missing events drawn")
  }

  if (verbose) cli_alert_success("Finished evaluating missing events")

  # Remove the event_qe table before writing out
  if (DBI::dbExistsTable(connection, "events_quality")) {
    DBI::dbRemoveTable(connection, "events_quality")
  }

  if (verbose) cli_h2("Evaluating event chronology")

  chrono <- evaluate_chronology(connection = connection,
                                decompose = FALSE
                                #,.debug = .debug
                                )

  if (write_plots) {
    chrono_plot <- plot_chronology(chrono)
    tryCatch({
      ggsave(
        filename = file.path(
          output_folder,
          "plots/chrono_events.pdf"
        ),
        plot = chrono_plot,
        height = full_height/2,
        width = full_width,
        units = "mm"
      )
      if (verbose) cli_alert_success("Chronology drawn")
    },
    error = function(cond){
      print("Unable to save chrono events as pdf:")
      print(cond)
    })
    rm(chrono_plot)
  }

  chrono <- decompose_chronology(connection = connection,
                                 x = chrono
                                 #,.debug = .debug
                                 )

  write_notify(connection = connection,
               target_name = "events_quality",
               local_table = chrono,
               verbose = verbose)

  if (verbose) cli_alert_success("Finished evaluating event chronology")
  if (verbose) cli_h2("Evaluating all events on an individual basis")
  hic_codes <- sort(.variables$code_name)

  # Check to see if there is any data to extract
  # We can skip over these later.
  missing_this_event <- events_missing %>%
    filter(.data$eval_code == "VE_CP_02") %>%
    distinct(.data$code_name, .data$site) %>%
    group_by(.data$code_name) %>%
    tally() %>%
    filter(.data$n == length(all_sites)) %>%
    select(.data$code_name) %>%
    pull()

  # Some extracted data items we need to keep to perform a more
  # complex evaluation. I can name these in the `.keep` list.
  # This could be hanled better and more programatically in the future.
  if (is.null(test_comparisons)) {
    comparisons_lookup <- .comparisons_lookup
  } else {
    comparisons_lookup <- test_comparisons
  }

  comparisons_storage <- vector(
    mode = "list",
    length = length(
      unique(c(comparisons_lookup$event_a, comparisons_lookup$event_b))))
  names(comparisons_storage) <-
    unique(c(comparisons_lookup$event_a, comparisons_lookup$event_b))

  speak_utter_nonsense <- sample(hic_codes, 3)

  for (i in seq_along(hic_codes)) {

    if (verbose) cli_h3("Evaluating {hic_codes[i]}")

    if (hic_codes[i] %in% missing_this_event) {
      if (verbose) cli_alert("Skipping over {hic_codes[i]} as no data present")
      next
    }

    # Otherwise extract...
    if (verbose) cli_text("Extracting {hic_codes[i]}")
    df <- extract(core_table = core, code_name = hic_codes[i])

    # Hold onto extracted datasets that need comparison between two codes
    # Inefficient with memory, but for now, not a problem.
    if (hic_codes[i] %in% names(comparisons_storage)) {
      comparisons_storage[[hic_codes[i]]] <- df
    }

    # ... and evaluate
    if (verbose) cli_text("Testing {hic_codes[i]}")
    event_eval <- evaluate_events(df,
                                  los_table = episode_length,
                                  reference_tbl = reference)

    if (verbose) {
      if (hic_codes[i] %in% speak_utter_nonsense) {
        cli_text(.utter_nonsense())
      }
    }

    write_notify(connection = connection,
                 target_name = "events_quality",
                 local_table = event_eval,
                 append = TRUE)

    rm(event_eval)

    event_eval_missing <- evaluate_local_missingness(
      x = df,
      reference_tbl = reference
    )

    write_notify(connection = connection,
                 target_name = "events_missing",
                 local_table = event_eval_missing,
                 append = TRUE)

    rm(event_eval_missing)

    if (write_plots) {

      obj_class <- class(df)[1]

      ## Plots that EVERYTHING always gets:
      ## 1. Contribution plot
      ## 2. Value plot (either CDF or histogram, continuous/discrete data respectively)

      if (verbose) cli_text("Plotting {hic_codes[i]}")

      # Contribution plot of the event over time.
      # Full width plot
      c_plot <- plot_contribution(
        x = df,
        reference_tbl = reference,
        sites_col = all_sites.col)

      ggsave(
        plot = c_plot,
        filename = glue("{output_folder}plots/{hic_codes[i]}/{hic_codes[i]}_contibution.pdf"),
        width = full_width,
        height = full_height/3,
        units = "mm")

      rm(c_plot)

      ## The following plots are excluded from visualisations, either because
      ## They contain inherently identifying patterns (NHS number), or aren't
      ## Easy to visualise by standard means (microbiology)

      if (hic_codes[i] %in% paste0("NIHR_HIC_ICU_0",
        c(
          "001", # PAS Number
          "002", # CMP Number
          "003", # GP Code
          "004", # Treatment Function Code
          "005", # AMP Number
          "073", # NHS Number
          "076", # Post Code
          "399", # Primary ICNARC Code
          "088", # Secondary ICNARC Code
          "912" # Ultimate ICNARC Code
          ))) {
        next
      }

      # Value plot: CDF or histogram
      if (hic_codes[i] %in% .categorical_hic) {
        h_plot <- plot_hist(
          x = df,
          sites_col = all_sites.col)

        ggsave(
          plot = h_plot,
          filename = glue("{output_folder}plots/{hic_codes[i]}/{hic_codes[i]}_histogram.pdf"),
          width = full_width/2,
          height = full_height/3,
          units = "mm")

        rm(h_plot)
      } else {
        cdf_plot <- plot_ecdf(
          x = df,
          sites_col = all_sites.col,
          col_name = "value")

        ggsave(
          plot = cdf_plot,
          filename = glue("{output_folder}plots/{hic_codes[i]}/{hic_codes[i]}_ecdf.pdf"),
          width = full_width/2,
          height = full_height/3,
          units = "mm")

        rm(cdf_plot)
      }

      # 2d items can have plots that look at their temporal properties
      if (grepl("2d", obj_class)) {
        t_plot <- plot_time(
          x = df,
          sites_col = all_sites.col,
          col_name = "datetime")

        ggsave(
          plot = t_plot,
          filename = glue("{output_folder}plots/{hic_codes[i]}/{hic_codes[i]}_time.pdf"),
          width = full_width/2,
          height = full_height/3,
          units = "mm")

        rm(t_plot)
      }

      ks_pos <- qref[qref$code_name == hic_codes[i], "dist_compare", drop = TRUE]

      if (!is.na(ks_pos) && ks_pos == "ks") {
        ks_res <- ks_test(
          x = df,
          col_name = "value"
        )

        if (nrow(ks_res) > 0) {
          ks_plot <- plot_ks(
            x = ks_res,
            reference_tbl = reference)

          ggsave(
            plot = ks_plot,
            filename = glue("{output_folder}plots/{hic_codes[i]}/{hic_codes[i]}_ks.pdf"),
            width = full_width/2,
            height = full_height/3,
            units = "mm")

          rm(ks_plot)
        }


      }

    }

    if (verbose) cli_alert_success("Finished evaluating {hic_codes[i]}")

  }

  cli_h2("Starting event comparison evaluation")

  event_eval <- purrr::pmap_dfr(
    .l = list(
      comparisons_lookup$event_a,
      comparisons_lookup$event_b,
      comparisons_lookup$operation
    ),
    .f = ~ evaluate_comparison(
      event_1 = comparisons_storage[[..1]],
      event_2 = comparisons_storage[[..2]],
      relationship = ..3))

  write_notify(connection = connection,
               target_name = "events_quality",
               local_table = event_eval,
               append = TRUE)

  cli_alert_success("Finished event comparison evaluation")
  cli_alert_success("Finished event level evaluation")

  return(TRUE)
}

#' Setup output folders for data quality evaluation
#'
#' While some components of the data quality evaluation are stored alongside
#' the original data, some components are best exported and viewed externally.
#' This sets up a standard folder structure to recieve these exports.
#'
#' @param output_folder
#'
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success
#' @importFrom purrr walk
setup_folders <- function(output_folder = NULL, write_plots = TRUE) {
  if (is.null(output_folder)) {
    abort("An output folder must be supplied")
  }

  cli_h1("Performing folder setup")

  if (!dir.exists(output_folder)) {
    cli_alert_info("Path `{output_folder}` does not exist. Creating directory")
    dir.create(output_folder)
  }

  if (!dir.exists(file.path(output_folder, "plots")) & write_plots) {
    dir.create(file.path(output_folder, "plots"))
  }
  if (!dir.exists(paste0(output_folder, "data"))) {
    dir.create(file.path(output_folder, "data"))
  }

  hic_codes <- sort(qref$code_name)

  hic_codes %>%
    walk(function(x) {
      if (!dir.exists(file.path(output_folder, "plots", x))) {
        dir.create(file.path(output_folder, "plots", x))
      }
    })

  if (!dir.exists(file.path(output_folder, "plots/episodes"))) {
    dir.create(file.path(output_folder, "plots/episodes"))
  }

  cli_alert_success("Folder setup completed successfully")
}
