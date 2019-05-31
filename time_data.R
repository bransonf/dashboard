# timeline for cardiff

timeline <- data.frame(
  id      = 1:4,
  content = c("Homicides: 177", "Homicides: 260"  ,"Ranged item", "Item four"),
  start   = c("1990-01-01", "1991-01-01", "2016-01-20", "2016-02-14 15:00:00"),
  end     = c("1990-12-31", "1991-12-31", "2016-02-04", NA)
)

timevis::timevis(timeline)




homicides <- data.frame(
  year = c(1990:2018),
  n_homicides = c(177, 260, 231, 267, 248, 204, 166,153,113,130,123,148,119,73,155,128,128,138,160,144,142,113,113,120,159,188,188,205,187)
)




