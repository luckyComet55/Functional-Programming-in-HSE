data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Info Info = EQ
cmp Info _ = LT
cmp Warning Info = GT
cmp Warning Warning = EQ
cmp Warning Error = LT
cmp Error Error = EQ
cmp Error _ = GT