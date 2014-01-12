import Data.List.Split
import Data.List
import System.IO
import System.Exit

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

removeLines :: [String] -> IO ()
removeLines [] = return ()
removeLines (x:xs) = do
				if isInfixOf "<device" x 
				then do
						putStrLn "\nARCHIVO XML"
						putStr "Ingrese la Capability del Device: "
						nameCapability <- getLine
						let listNew = removeEmpty xs
						let list = listDevice listNew (Device "" "" "") (Group "") nameCapability
						menuPrincipal list
				else
					removeLines xs

listDevice :: [String] -> Device -> Group -> String-> [String]
listDevice [] _ _ _ = []
listDevice (x:xs) device group capabilityUser= do 	
					let 	list = splitOneOf ("<>= \\\"") x
					let 	list1 = removeEmpty list

					if ( (head list1) == "device" )
						then do 
								let device = setDevice( deviceFunction( list1 ) )
								let idDevice = getIdDevice(device)
								[]++listDevice xs device group capabilityUser
						else if ( (head list1) == "group" ) 
							then do
									let group = setGroup( groupFunction( list1 ) )
									let idGroup = getIdGroup(group)
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