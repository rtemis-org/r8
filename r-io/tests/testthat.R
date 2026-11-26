# testthat.R
# ::R8.io::
# 2026- EDG rtemis.org

# Runs the tests in tests/testthat during R CMD check.

library(testthat)
library(R8.io)

test_check("R8.io")
