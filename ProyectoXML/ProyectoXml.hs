import Data.List.Split
data Device = Device {idD:: String
					 ,	user_agent::String
					 ,	fall_back::String} deriving(Show)
data Group = Group {idG:: String}deriving(Show)
data Capability = Capability {	name::String
							 , 	value:: String}deriving(Show)

main = do
	fileContents <- readFile "nuevo.xml"
  	mapM_ funcion (lines fileContents)

eliminar ::[String]->[String]
eliminar [] = []
eliminar (x:xs)=do
	if x==""
	then []++eliminar xs
	else [x]++eliminar xs

funcion :: String -> IO()
funcion cadena = do
	let lista = splitOneOf ("<>= \\\"") cadena
	let lista1 = eliminar lista
	putStrLn $ show(lista1)