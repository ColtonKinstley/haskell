data PeaNum = Succ PeaNum |  Zero
    deriving (Eq)

incr :: PeaNum -> PeaNum
incr = Succ

decr :: PeaNum -> PeaNum 
decr (Succ n) = n

instance Show PeaNum where
    show = go 0 where 
        go acc Zero = show acc
        go acc p = go (acc+1) (decr p)
