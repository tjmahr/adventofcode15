add_day <- function(day, title = "Title") {
  XX <- sprintf("%02d", day)
  X <- day
  data <- list(XX = XX, X = X, Title = title)

  file <- system.file("templates", "dayXX.R", package = "adventofcode15")
  text <- readLines(file)

  outfile <- paste0("day", XX, ".R")
  outpath <- file.path("./R", outfile)
  if (!file.exists(outpath)) {
    file.create(outpath)
    outlines <- whisker::whisker.render(text, data)
    writeLines(outlines, outpath)
  }

  file2 <- system.file("templates", "run-dayXX.R", package = "adventofcode15")
  text2 <- readLines(file2)

  outfile2 <- paste0("run-day", XX, ".R")
  outpath2 <- file.path("./inst", outfile2)

  outfile3 <- paste0("input", XX, ".txt")
  outpath3 <- file.path("./inst", outfile3)

  if (!file.exists(outpath2)) {
    file.create(outpath2)
    outlines2 <- whisker::whisker.render(text2, data)
    writeLines(outlines2, outpath2)
  }

  if (!file.exists(outpath3)) {
    file.create(outpath3)
  }

  usethis::use_test(paste0("day", XX))
  rstudioapi::navigateToFile(outpath2)
  rstudioapi::navigateToFile(outpath)
}
