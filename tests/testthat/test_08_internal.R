setup({
  unlink("project-176.motus")
  unlink("SG-3115BBBK0782.motus")
})

teardown({
  unlink("project-176.motus")
  unlink("SG-3115BBBK0782.motus")
})

test_that("hitsByBatchProject doesn't fail on extra columns", {
  sample_auth()
  file.copy(system.file("extdata", "project-176.motus", package = "motus"), ".")
  tags <- tagme(176, new = FALSE, update = FALSE)
  DBI_Execute(tags, "DELETE FROM projBatch")
  DBI_Execute(tags, "DELETE FROM hits")
  
  expect_silent(h0 <- srvHitsForTagProject(projectID = 176, 
                                           batchID = 53, 
                                           hitID = 0))
  expect_message(h1 <- hitsForBatchProject(tags, 
                                           projectID = 176, 
                                           batchID = 53,
                                           batchMsg = "temp"))
  expect_gt(h1, 0)
  expect_true(dplyr::all_equal(h0, dplyr::tbl(tags, "hits") %>% 
                                 dplyr::collect() %>% 
                                 as.data.frame(), 
                               convert = TRUE))
  
  # Expect extra columns to NOT result in an error
  DBI_Execute(tags, "DELETE FROM projBatch")
  DBI_Execute(tags, "DELETE FROM hits")
  m <- mockery::mock(dplyr::mutate(h0, EXTRA_COL = 0), data.frame())
  with_mock("motus:::srvHitsForTagProject" = m,
            expect_message(hitsForBatchProject(tags, 
                                               projectID = 176, 
                                               batchID = 53,
                                               batchMsg = "temp")))
})

test_that("hitsByBatchReceiver doesn't fail on extra columns", {
  skip_if_no_auth()
  unlink("SG-3115BBBK0782.motus")
  f <- system.file("extdata", "SG-3115BBBK0782.motus", package = "motus")
  skip_if_no_file(f)
  file.copy(f, ".")
  
  tags <- tagme("SG-3115BBBK0782", new = FALSE, update = FALSE)
  b <- DBI_Query(tags, "SELECT batchID FROM hits LIMIT 1")
  DBI_Execute(tags, "DELETE FROM hits")

  expect_silent(h0 <- srvHitsForReceiver(batchID = b, hitID = 0))
  expect_true(nrow(h0) > 0)
  expect_message(hitsForBatchReceiver(tags, batchID = b, batchMsg = "temp"))
  expect_true(dplyr::all_equal(h0, 
                               dplyr::tbl(tags, "hits") %>% 
                                 dplyr::collect(), 
                               convert = TRUE))
  
  # Expect extra columns to NOT result in an error
  DBI_Execute(tags, "DELETE FROM hits")
  m <- mockery::mock(dplyr::mutate(h0, EXTRA_COL = 0), data.frame())
  with_mock("motus:::srvHitsForReceiver" = m,
            expect_message(hitsForBatchReceiver(tags, 
                                                batchID = 53,
                                                batchMsg = "temp")))
})
