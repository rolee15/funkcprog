data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

isWinter :: Month -> Bool
isWinter Dec = True
isWinter Jan = True
isWinter Feb = True
isWinter _   = False

data Time = T Int Int Int

showTime :: Time -> String
showTime (T h m s) = show h ++ ":" ++ show m ++ "." ++ show s