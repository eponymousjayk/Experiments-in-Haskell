epsilon = 0.0000000001

zeta x = zeta' 1 0
         where zeta' n c = if (diff < epsilon)
                            then c
                            else zeta' (n + 1) next
                           where next = c + (1/(n**x))
                                 diff' = next - c
                                 diff = if diff' < 0 then (-diff') else (diff')
