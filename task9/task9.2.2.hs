lookups :: (Eq k) => k -> [(k, v)] -> [v]
lookups key vals = do
                    a <- filter (\(k, _) -> k == key) vals
                    return (snd a)