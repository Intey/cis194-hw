import Xor

setError = putStrLn
setOk = putStrLn "."

main = do
  if not (xor [True])
     then setError "Singleton"
     else setOk
  if (xor [True, True])
     then setError "Double true"
     else setOk
  if not (xor [True, True, True])
     then setError "Triple true"
     else setOk
  if (xor [True, True, True, True])
     then setError "Quad true"
     else setOk
  if not (xor [True, False])
     then setError "True and False"
     else setOk
  if not (xor [False, True])
     then setError "False and True"
     else setOk
  if not (xor [False, True, False])
     then setError "True between False"
     else setOk
  if (xor [False, True, False, True, False])
     then setError "2 true between False"
     else setOk
  if (xor [True, False, True, False, True, False, True])
     then setError "true around False"
     else setOk
  if (xor [True, False, False, True, True, False, True])
     then setError "Pairs"
     else setOk
  if not (xor [True, True, False, True, True, False, True])
     then setError "Start Pairs"
     else setOk
  if (xor [True, False, False, True, False, True, True])
     then setError "End Pairs"
     else setOk
  if (xor [False, False, False])
     then setError "triple False"
     else setOk
  if (xor [False, False, False, False])
     then setError "Quad False"
     else setOk
  putStrLn "TestDone"
