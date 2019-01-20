module ShowCont where


showCont :: Show a => Cont String a -> String
showCont m = runCont m show
