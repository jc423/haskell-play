replicatePlay :: Int -> a -> [a]
replicatePlay 0 _ = []
replicatePlay n x = x : replicatePlay (n - 1) x
