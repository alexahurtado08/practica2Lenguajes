-- 1) Obtener periodo (3)
semestreIngreso :: Int -> Int
semestreIngreso x = rem x 10

añoIngreso :: Int -> Int
añoIngreso x = 2000 + div (x - semestreIngreso x) 10

periodoIngreso :: Int -> String
periodoIngreso x = show (añoIngreso x) ++ "-" ++ show (semestreIngreso x)

-- 2) Obtener área de estudio (01-99)
crearListaDiv :: Int -> [Int]
crearListaDiv n = filter (\x -> mod n x == 0) [1..(n-1)]

sumarLista :: Num a => [a] -> a
sumarLista xs = sum xs

obtenerSumaAliquot :: Int -> String
obtenerSumaAliquot n =
    let suma = sumarLista (crearListaDiv n)
    in if n < suma
        then "Administrative"
        else if n > suma
            then "Humanities"
            else "Engineering"

-- 3) Obtener id (num + x + par o impar) (001-999)
definirParImpar :: Int -> String
definirParImpar x = 
    if mod x 2 == 0
        then " even"
        else " odd"

obtenerId :: Int -> String
obtenerId x = "num" ++ show x ++ definirParImpar x

main :: IO ()
main = do
    cedula <- getLine
    let primerosTres = read (take 3 cedula) :: Int
        siguientesDos = read (take 2 (drop 3 cedula)) :: Int
        ultimosTres = read (drop 5 cedula) :: Int
    
    let result = (periodoIngreso primerosTres) ++ " " ++ (obtenerSumaAliquot siguientesDos) ++ " " ++ (obtenerId ultimosTres)
    putStrLn result




