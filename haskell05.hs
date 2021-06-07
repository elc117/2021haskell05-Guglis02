bmi :: Float -> Float -> String
bmi peso altura =
    let imc = peso / (altura ^ 2)
    in if imc <= 18.5 then "ABAIXO" else if imc >= 30 then "ACIMA" else "NORMAL"

bmi' :: Float -> Float -> String
bmi' peso altura = if imc <= 18.5 then "ABAIXO" else if imc >= 30 then "ACIMA" else "NORMAL"
    where imc = peso / (altura ^ 2)

--cpfValid :: [Int] -> Bool
--cpfValid cpf =
-- let digits = take 9 cpf
--     dv1 = cpfDV digits [10,9..]
--     dv2 = cpfDV (digits ++ [dv1]) [11,10..]
--  in dv1 == cpf !! 9 && dv2 == cpf !! 10

cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
    where  
        digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]
    

--cpfDV :: [Int] -> [Int] -> Int
--cpfDV digits mults = if expr < 2 then 0 else 11-expr
-- where expr = (sum $ zipWith (*) digits mults) `mod` 11

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults = 
    let expr = (sum $ zipWith (*) digits mults) `mod` 11
    in if expr < 2 then 0 else 11-expr


andTable :: [(Bool, Bool, Bool)]
andTable = [(p, q, p && q) | p <- [True,False], q <- [True,False]]