
KILOMETERS_PER_MILE = 1.609

#' @export
busiest_routes = function (dataframe, origincol, destcol) {
  stopifnot(all(dataframe$Passengers >= 1))
  stopifnot(all(!is.na(dataframe$Passengers)))

  # Now, we can see what the most popular air route is, by summing up the number of
  # passengers carried.
  pairs = group_by(dataframe, {{ origincol }}, {{ destcol }}) %>%
    summarize(Passengers=sum(Passengers), distance_km=first(Distance) * KILOMETERS_PER_MILE)
  arrange(pairs, -Passengers)

  # we see that LAX-JFK (Los Angeles to New York Kennedy) is represented separately
  # from JFK-LAX. We'd like to combine these two. Create airport1 and airport2 fields
  # with the first and second airport in alphabetical order.
  pairs = mutate(
    pairs,
    airport1 = if_else({{ origincol }} < {{ destcol }}, {{ origincol }}, {{ destcol }}),
    airport2 = if_else({{ origincol }} < {{ destcol }}, {{ destcol }}, {{ origincol }})
  )

  pairs = group_by(pairs, airport1, airport2) %>%
    summarize(Passengers=sum(Passengers), distance_km=first(distance_km))

  return(arrange(pairs, -Passengers))
}

#' @export
market_shares = function (dataframe, carriercol, origincol) {
  mkt_shares = group_by(dataframe, {{ carriercol }}, {{ origincol }}) %>%
    summarize(Passengers=sum(Passengers)) %>%
    group_by({{ origincol }}) %>%
    mutate(market_share=Passengers/sum(Passengers), total_passengers=sum(Passengers)) %>%
    ungroup()

  return(mkt_shares)
}
