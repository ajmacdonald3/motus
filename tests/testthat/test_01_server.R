context("Test Server Access")


# tagme() errors appropriately --------------------------------------------
test_that("tagme() errors appropriately", {
  skip_on_cran()
  skip_on_appveyor()
  skip_on_travis()
  
  unlink("project-10.motus")
  unlink("CTT-5031194D3168")
  
  sample_auth()
  
  expect_error(expect_message(tagme(projRecv = 10, new = TRUE, update = TRUE), 
                              "updateMotusDb"),
               "You do not have permission")
  
  expect_error(expect_message(tagme(projRecv = "CTT-5031194D3168", 
                                    new = TRUE, update = TRUE), 
                              "updateMotusDb"),
               "Either") #...
  
  unlink("project-10.motus")
  unlink("CTT-5031194D3168.motus")
})


# Projects downloads ---------------------------------------
test_that("tagme() downloads data - Projects", {
  skip_on_cran()
  skip_on_appveyor()
  
  unlink("project-176.motus")
  
  sample_auth()
  
  expect_message(tags <- tagme(projRecv = 176, new = TRUE, update = TRUE)) %>%
    expect_is("src_SQLiteConnection")

})


# Receivers download ------------------------------------------------------
test_that("tagme() downloads data - Receivers", {
  skip_on_cran()
  skip_on_appveyor()
  skip_if_no_auth()
  
  unlink("SG-3115BBBK1127.motus")
  expect_message(tagme("SG-3115BBBK1127", new = TRUE, update = TRUE)) %>%
    expect_s3_class("src_sql")
  unlink("SG-3115BBBK1127.motus")
})


# Projects countOnly = TRUE ---------------------------------------------------
test_that("tagme with countOnly (tellme) - Projects", {
  skip_on_cran()
  
  sample_auth()
  
  file.copy(system.file("extdata", "project-176.motus", package = "motus"), ".")
  
  expect_silent(tagme(projRecv = 176, new = FALSE, 
                      update = TRUE, countOnly = TRUE)) %>%
    expect_is("data.frame")
  
  expect_silent(tellme(projRecv = 176, new = FALSE)) %>%
    expect_is("data.frame")
  
  unlink("project-176.motus")
})

# Receivers countOnly = TRUE ---------------------------------------------------
test_that("tagme with countOnly (tellme) - Receivers", {
  skip_on_cran()
  skip_if_no_auth()
  
  unlink("SG-3115BBBK1127.motus")
  expect_silent(tellme("SG-3115BBBK1127", new = TRUE)) %>%
    expect_is("data.frame")
  unlink("SG-3115BBBK1127.motus")
})


# Timeouts ----------------------------------------------------------------
test_that("srvQuery handles time out graciously", {
  
  sample_auth()
  
  # https://stackoverflow.com/questions/100841/artificially-create-a-connection-timeout-error
  expect_message(
    expect_error(srvQuery(API = motus_vars$API_PROJECT_AMBIGUITIES_FOR_TAG_PROJECT, 
                          params = list(projectID = 176),
                          url = motus_vars$dataServerURL, timeout = 0.01),
                 "The server is not responding"),
    "The server did not respond within 0.01s. Trying again...")
})


# srvAuth errors ----------------------------------------------------------
test_that("srvAuth handles errors informatively", {
  motusLogout()
  sessionVariable(name = "userLogin", val = "motus.samp")
  sessionVariable(name = "userPassword", val = "motus.samp")
  
  expect_error(srvAuth(), "Authentication failed")
})


# srvAuth package version --------------------------------------------------

test_that("srvAuth errors/warns/passes on package version", {
  sample_auth()
  expect_silent(srvAuth())
  expect_true(!is.null(motus_vars$currentPkgVersion))
    
  v <- package_version(motus_vars$currentPkgVersion)
  v <- c(v, v, v)
  v[[c(1,1)]] <- as.numeric(v[[c(1,1)]]) - 1
  v[[c(3,1)]] <- as.numeric(v[[c(3,1)]]) + 1
  v <- as.character(v)
  
  with_mock("motus:::pkg_version" = mockery::mock(v[1]), 
            expect_warning(srvAuth()))
  with_mock("motus:::pkg_version" = mockery::mock(v[2]), 
            expect_silent(srvAuth()))
  with_mock("motus:::pkg_version" = mockery::mock(v[3]), 
            expect_silent(srvAuth()))
})
