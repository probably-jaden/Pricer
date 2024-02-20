test_that("Durable Data Wrangling works", {
  dp <- dplyr::tibble(wtp = c(64, 18, 46, 92, 110, 138, 113, 89, 0, 258, 205, 0, 18, 202, 46, 258, 0, 141, 0, 46, 61, 101, 64, 215, 95, 43, 46, 46, 132, 21, 18, 113, 9, 18, 21, 18, 104, 6, 0, 101, 6, 224, 322, 18, 316, 156, 104, 322, 285, 208, 316, 0, 288, 95, 6, 52, 46, 0, 18, 64, 98, 248, 18, 110, 0, 67, 0, 18, 0, 89, 132, 101, 18, 215, 18, 0, 0, 104, 285, 3, 46, 141, 322, 291, 89, 0, 101, 113, 67, 3, 132, 215, 224, 291, 9, 291, 267, 6, 6, 61, 178, 285, 64, 126, 0, 101, 15))

  demandDurable_dp <- demandDurable(dp)

  val_durableDP <- dp %>%
    group_by(wtp) %>%
    summarize(count = n()) %>%
    arrange(desc(wtp)) %>%
    mutate(quantity = cumsum(count)) %>%
    mutate(revenue = wtp * quantity)



  expect_equal(demandDurable_dp, val_durableDP)
})
