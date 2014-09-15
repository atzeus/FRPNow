module Race where


data Race a b = L a | R b | Tie a b

combineMaybe (Just a) (Just b) = Just (Tie a b)
combineMaybe (Just a) Nothing  = Just (L a)
combineMaybe Nothing (Just b)  = Just (R b)
combineMaybe Nothing Nothing = Nothing

