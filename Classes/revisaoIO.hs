import Text.Printf
import System.IO (hSetEcho, stdin)


-- Questao 1

putStr'' :: String -> IO ()
putStr'' [] = return ()
putStr'' s = sequence_ [putChar c | c <- s]

-- Questao 2

somador :: IO ()
somador = do putStr "Quantos números? "
             n    <- readLn :: IO Int
             soma <- acumular n 0
             putStr $ printf "O total é %d" soma

acumular :: Int -> Int -> IO Int
acumular 0 acc = return acc
acumular n acc = do x <- readLn :: IO Int
                    acumular (n - 1) (x + acc)

-- Questao 3

somador2 :: IO ()
somador2 = do putStr "Quantos números? "
              n <- readLn :: IO Int
              ns <- sequence [readLn :: IO Int | _ <- [1 .. n]]
              putStr $ show $ sum ns

-- Questao 4
obterChar:: IO Char
obterChar = do hSetEcho stdin False
               x <- getChar
               hSetEcho stdin True
               return x

obterLinha :: IO String
obterLinha = do x <- obterChar
                if x == '\n' then
                    do putChar x
                       return []
                else
                    if x == '\DEL' then
                        do putStr "\b \b"
                           return "\DEL"
                    else
                        do putChar x
                           xs <- obterLinha
                           if xs == "\DEL" then
                              do obterLinha
                           else
                              return (x:xs)