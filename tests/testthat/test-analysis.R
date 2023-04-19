get_data = function () {
  data = dplyr::tibble(
    Origin=rep(c("SFO", "ORD", "DCA", "PHX", "BOS", "RDU"), 20),
    Destination=rep(c("DFW", "MCI", "MIA", "ABQ", "IAH", "SJU"), 20),
    Passengers=rep(1:6, 20),
    Carrier=rep(c("United", "American", "Delta", "Southwest"), 30)
  )
  return(data)
}

test_that("Market shares sum to 1", {
  data = get_data()
  shares = market_shares(data, Carrier, Origin)
  total_shares = group_by(shares, Origin) %>% summarize(overall=sum(market_share))
  # six airports, so six shares
  expect_equal(total_shares$overall, rep(1, 6))
})

test_that("Error when passengers column missing", {
  data = get_data() %>% dplyr::select(-Passengers)
  expect_error(busiest_routes(data, Origin, Destination))
})
