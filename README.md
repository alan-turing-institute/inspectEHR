
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inspectEHR <a href='https://cc-hic.github.io/inspectEHR/'><img src='logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle
Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/)
<!-- badges: end -->

## Overview

`inspectEHR` is a quality evaluation tool for CC-HIC. The goal of this
package is to provide a core set of functions to detect erroneous or
otherwise questionable data within CC-HIC, so that the end researcher
can handle this information explicitly.

The design ethos of `inspectEHR` is to apply a comprehensive
interpretation of the Khan data quality evaluation framework.

The full default evaluation is largely automated, and can be performed
by use of the `perform_evaluation()` function. The output from this
function is stored in the CC-HIC database as metadata, so that
downstream research can use a consistent set of rules when faced with
erroneous data patterns.

Helper functions for extracting and wrangling data have also been
developed. These live in the sister package `wranglEHR`, which must be
installed for inspectEHR to function correctly.

## Installation

``` r
# install directly from github with
remotes::install_github("DocEd/inspectEHR")
```

## Usage

``` r
library(inspectEHR)

## Create a connection to the CC-HIC database. (I leave this blank here for security).
ctn <- DBI::dbConnect()

perform_evaluation(
  connection = ctn,
  output_folder = "/data/cchic/ed/phd/data_quality_evaluation/tmp/",
  verbose = TRUE)

# I recommend working in `verbose` mode so you can track progress of the evaluation.

DBI::dbDisconnect(ctn)
```

## Getting help

If you find a bug, please file a minimal reproducible example on
[github](https://github.com/DocEd/inspectEHR/issues).

## Reporting Data Quality Issues

Please submit an issue and tag it with “data quality”. Data quality
issues are often related to a specific site. If this is the case, please
also tag the site.

## Data Quality Rules

The data quality rules are largely based upon standards set by
OHDSI<sup>1</sup>, and the consensus guidelines by Khan et
al.<sup>2</sup>. The CC-HIC currently uses an episode centric model. As
such, many of the data quality checks are based around this way of
thinking. As we move to OMOP (a patient centric model) many of these
will change accordingly. General conventions follow that:

-   Verification refers to an internal check of the data, possibly
    against a data specification or domain knowledge.
-   Validation refers to an external check of the data, against a
    secondary resource that is known to be trusted.

### Episode Verification

-   Internal consistency check of episode
    -   Each episode has a unique identifier
    -   Each episode has a start date
    -   Each episode has an end date defined by one of:
        -   End of episode date time (often administrative in nature and
            used with caution)
        -   On unit death date time from any means
            -   Death
            -   Brain stem Death (BSD)
            -   “Body Removal”: strongly suggesting death
        -   The last measured bedside physiological result (HR and SpO2)
    -   Death (other than BSD) cannot occur before the last caputre of a
        direct physiological measure (e.g. a heart rate, or a blood
        pressure), but could occur before the return of a lab measure
        (e.g. a CRP or a pathology result)
    -   Records that are flagged as “open” are dropped.

### Event/Value Verification

-   Value conformance: data conforms to accepted type and formatting
    constraints
-   Relational conformance: unique keys are not duplicated and there are
    no orphaned rows in the database
-   Computational conformance: calculated and contributed values agree
    with oneanother
    -   e.g. BMI (derived from height and weight) in plausible range and
        agree with a contributed BMI
    -   e.g. APACHE in plausible range and agree with a contributed
        APACHE
-   Completeness: contributed data items match local capability
    (i.e. missingness only occurs when the data doesn’t exist)
-   Plausibility:
    -   Uniqueness plausibility: descriptions of singular events/objects
        are not duplicated
    -   Atemporal plausibility:
        -   Events occur within their episode (within a reasonable
            buffer time)
        -   Events fall within an accepted range, follow the expected
            distribution and agree with internal/local knowledge
        -   Distributions for independent measures of the same fact are
            in agreement:
            -   e.g. Systolic \> MAP \> Diastolic
            -   e.g. blood gas sodium and lab sodium
            -   e.g. menorrhagia diagnosis only linked to female sex
        -   Repeated measures of the same event show the expected
            variability
    -   Temporal plausibility:
        -   Observed values, and sequences of values that represent
            state transitions, correspond to an expected temporal
            property:
            -   e.g. Hosp Admission -\> ICU Admission -\> ICU Discharge
                -\> Hosp Discharge
        -   Value density over time are consistent with local
            expectations
            -   e.g. busy and quiet periods are reflected in the data

### Event Validation

The same principles listed above can be retested under a validation
framework. That is to seak an external resource to validate the values
in question. An aim is to check the data against the ICNARC reports,
which will allows for some external validation against a gold standard
resource. At present, the validation that is performed is to compaire
all sites against each other. In this sense, each site is used as a
validation check against the others. Distrepancies should either be due
to systemic errors, or differences in casemix.

------------------------------------------------------------------------

1.  <https://www.ohdsi.org/analytic-tools/achilles-for-data-characterization/>
2.  Kahn, Michael G.; Callahan, Tiffany J.; Barnard, Juliana; Bauck,
    Alan E.; Brown, Jeff; Davidson, Bruce N.; Estiri, Hossein; Goerg,
    Carsten; Holve, Erin; Johnson, Steven G.; Liaw, Siaw-Teng;
    Hamilton-Lopez, Marianne; Meeker, Daniella; Ong, Toan C.; Ryan,
    Patrick; Shang, Ning; Weiskopf, Nicole G.; Weng, Chunhua; Zozus,
    Meredith N.; and Schilling, Lisa (2016) “A Harmonized Data Quality
    Assessment Terminology and Framework for the Secondary Use of
    Electronic Health Record Data,” eGEMs (Generating Evidence & Methods
    to improve patient outcomes): Vol. 4: Iss. 1, Article 18.
