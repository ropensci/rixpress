Hello,

I addressed comments by Konstanze Lauseker

- in DESCRIPTION, I have written software names between single quotes
- in DESCRIPTION, I have removed DAG and use the full words instead
- I have uncomment lines of code in examples
- Examples wrapped in \dontrun{} all have side-effects, which is why using \dontrun{} is likely the best approach
- Tests that depend on snapshot files should now be correctly skipped on CRAN and no other test should write anywhere anymore

I have also test using win-builder: there is only one NOTE
about this being a new submission. I found nothing regarding
detritus.
