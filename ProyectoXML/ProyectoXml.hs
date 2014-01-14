import Data.List.Split
import Data.List
import System.IO
import System.Exit
import Debug.Trace

data Device = Device {idD:: String
					 ,	user_agent::String
					 ,	fall_back::String} deriving(Show)
data Group = Group {idG:: String}deriving(Show)
data Capability = Capability {	name::String
							 , 	value:: String}deriving(Show)

setDevice ::[String] -> Device
setDevice [] = Device "" "" ""
setDevice all@(a:b:cs) = do
	Device a b (head cs)

getIdDevice :: Device -> String
getIdDevice (Device did ua fb) = did

setGroup ::[String] -> Group
setGroup [] = Group ""
setGroup lista = do
	Group (head lista)

getIdGroup :: Group -> String
getIdGroup (Group gid) = gid

setCapability ::[String] -> Capability
setCapability [] = Capability "" ""
setCapability all@(x:xs) = do
	Capability x (head xs)

getNameCapability :: Capability -> String
getNameCapability (Capability name value) = name

deviceFunction :: [String]->[String]
deviceFunction [] = []
deviceFunction all@(x:xs) = do
	if x=="id"
	then [head xs]++deviceFunction xs
	else if x == "user_agent"
		then [head xs]++deviceFunction xs
		else if x == "fall_back"
			then [head xs]++deviceFunction xs
			else deviceFunction xs

groupFunction :: [String]->[String]
groupFunction [] = []
groupFunction all@(x:xs) = do
	if x=="id"
	then [head xs]++groupFunction xs
	else groupFunction xs

capabilityFunction :: [String]->[String]
capabilityFunction [] = []
capabilityFunction all@(x:xs) = do
	if x=="name"
	then [head xs]++capabilityFunction xs
	else if x == "value"
		then [head xs]++capabilityFunction xs
		else capabilityFunction xs

main = do
	putStrLn "\t\t\t**************************************"
	putStrLn "\t\t\t     ANALIZADOR SEMANTICO"
	putStrLn "\t\t\t  José Antonio Vélez Gómez"
	putStrLn "\t\t\t  Leonel Fernando Ramirez  Gonzalez"
	putStrLn "\t\t\t  Kevin Guillermo Campuzano Castillo"
	putStrLn "\t\t\t**************************************"
	contentFile <- readFile "wurfl-2.3.xml"
  	file <- lineFile contentFile
  	removeLines file

lineFile :: String -> IO [String]
lineFile cadenaArchivo = return (lines cadenaArchivo)

menuPrincipal ::[String] ->String-> IO()
menuPrincipal [] _ = putStrLn "\t\t\tNo exixten Devices con esa Capability"
menuPrincipal list nameCapability= do
	putStrLn "\n\t\t\t\tINGRESE LA OPCION"
	putStrLn "\t\t1) Cuantos Devices tienen la Capability ingresada"
	putStrLn "\t\t2) Cuales Devices tienen la Capability ingresada"
	putStrLn "\t\t3) Desea SALIR"
	putStr "\t\tIngrese su opcion: "
	number <- getLine
	if number == "1"
	then putStrLn $ "Existen "++show(length list) ++" Device"
	else if  number == "2"
		then putStrLn $ "\tLos Devices que tienen la Capability :"++show(nameCapability)++"\n\n"++show( list)
		else if number == "3"
			then exitSuccess
			else menuPrincipal list nameCapability

removeLines :: [String] -> IO ()
removeLines [] = return ()
removeLines (x:xs) = do
				if isInfixOf "<devices" x 
				then do
						putStr "\nIngrese todas los Capability que desee consultar en el Device: "
						nameCapability <- getLine
						let listNew = removeEmpty xs
						let list = listDevice listNew (Device "" "" "") (Group "") nameCapability
						menuPrincipal list nameCapability
				else
					removeLines xs
suman ::Int->Int->Int
suman  n m = n+m

listCapability :: String -> [String]
listCapability [] = []
listCapability cadena = removeEmpty( splitOneOf(",; \"") cadena)

listDevice :: [String] -> Device -> Group -> String-> [String]
listDevice [] _ _ _ = []
listDevice (x:xs) device group capabilityUser= do 	
					let 	list = splitOneOf ("<>= \\\"\t\n") x
					let 	list1 = removeEmpty list

					if ( (head list1) == "device" )
					then do 
								let device = setDevice( deviceFunction( list1 ) )
								[]++listDevice xs device group capabilityUser
					else if ( (head list1) == "group" ) 
						then do
									let group = setGroup( groupFunction( list1 ) )
									[]++listDevice xs device group capabilityUser
						else if ( (head list1) == "capability" ) 
							then do
										let capability = setCapability( capabilityFunction( list1 ) )
										let nameCapability = getNameCapability(capability)
										let idDevice = getIdDevice(device)
										
										if nameCapability == capabilityUser
										then do [idDevice]++listDevice xs device group capabilityUser
										else []++listDevice xs device group capabilityUser
							else []++listDevice xs device group capabilityUser

removeEmpty ::[String]->[String]
removeEmpty [] = []
removeEmpty (x:xs)=do
	if x==""
	then []++removeEmpty xs
	else [x]++removeEmpty xs