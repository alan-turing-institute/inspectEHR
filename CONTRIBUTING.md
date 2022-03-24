# Contributing to inspectEHR

inpsectEHR needs your help!
There are three main areas where contributions would be useful:

1. Documentation
1. Writing unit tests
1. Expanding data quality evaluation framework

## Documentation

If you find something that is poorly explained (or missing) from the
documentation, feel free to submit a pull request to update the documentation.

## Unit Tests

The current testign suite could be improved.
Please submit a pull request to add new unit tests that cover new areas of
functionality.

## Expanding the framework

While I have aimed to write the quality evaluations as generally as possible,
it is inevitable that they are more focused toward my own goals.
If there are areas that are not being evaluated by the framework, feel free to
write your own extensions and submit a pull request.
The most straightforward way to do this would be to write an evaluative function
according to the following specification:

- An evaluative function should follow the `evaluation_x()` naming convention,
where `x` is a descriptive word to identify the evaluation being performed.
- An evaluation should expect as its first argument a dataframe returned by the `extract()` function.
- An evaluation should return one of two dataframes:
  - Concerning data that is exists, but thought to be erroneous, a dataframe with 7 columnns: `site`, `episode_id`, `event_id`, `code_name`, `value`, `eval_code` and `description`.
  - Converning data that is thought to be erroneous because it doesn't exist, a dataframe with 6 columns:
  `site`, `code_name`, `year`, `month`, `eval_code`, `description`.
- Returns a tibble that passes through `is_event_evaluation()` returning `TRUE`
- Can accept other objects to support the evaluation, but please use the second argument onwards.
- Conforms to S3 object orientation where methods are written for each of the classes
found found in the `.preserved_classes` object. I.e. the evaluation should work against every possible data item in the CC-HIC database without throwing an error, even if it performs no action and returns an empty dataframe.

Please make sure you write documentation and unit tests for any new
functionality you want to bring in.
