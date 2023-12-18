addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes Nothing  _        = Nothing
addMaybes _        Nothing  = Nothing
addMaybes (Just x) (Just y) = Just (x + y)