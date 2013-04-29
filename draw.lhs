> {-# LANGUAGE NoMonomorphismRestriction #-}
> 
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> 
> main = defaultMain $ circle1
> circle1 = (circle 1) # fc blue
>                               # lw 0.05
>                               # lc purple
>                               # dashing [0.2,0.05] 0
>                               # pad 10