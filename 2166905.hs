--
-- MATHFUN
-- UP2166905
--

import Text.Printf
import Data.List
import Data.Maybe
import Data.Char


--
-- Types (define City type here)
--

data City = City Name Location [Population]
    deriving (Show, Read)

type Name = String

data Location = Location North East
    deriving (Show, Read)

type North = Int

type East = Int

type Population = Int

type Spacing = (Int, Int, Int, Int, Int)

data CityTableHeaders = Name | North | East | CurrentPopulation | LastYearPopulation

formatCityTableHeaders :: CityTableHeaders -> String
formatCityTableHeaders Name = "Name"
formatCityTableHeaders North = "Location North"
formatCityTableHeaders East = "Location East"
formatCityTableHeaders CurrentPopulation = "Current Populocation"
formatCityTableHeaders LastYearPopulation = "Last Years Population"

testData :: [City]
testData = 
    [
        City "Amsterdam"    (Location 52   5)     [1158, 1149, 1140, 1132],
        City "Athens"       (Location 38  24)     [3153, 3153, 3154, 3156],
        City "Berlin"       (Location 53  13)     [3567, 3562, 3557, 3552],
        City "Bucharest"    (Location 44  26)     [1794, 1803, 1812, 1821],
        City "London"       (Location 52   0)     [9426, 9304, 9177, 9046],
        City "Madrid"       (Location 0   4)     [6669, 6618, 6559, 6497],
        City "Paris"        (Location 49   2)     [11079, 11017, 10958, 10901],
        City "Rome"         (Location 42  13)     [4278, 4257, 4234, 4210],
        City "Vienna"       (Location 48  16)     [1945, 1930, 1915, 1901],
        City "Warsaw"       (Location 52  21)     [1790, 1783, 1776, 1768]
    ]

getCityNames :: [City] -> [String]
getCityNames citys = [name | (City name _ _) <- citys]

getCityNorth :: [City] -> [String]
getCityNorth citys = [show north | (City _ (Location north _) _) <- citys]

getCityEast :: [City] -> [String]
getCityEast citys = [show east | (City _ (Location east _) _) <- citys]

getCityFirstPopInMillions :: [City] -> [String]
getCityFirstPopInMillions citys = [getPopulationFromYearInMillions 0 pops | (City _ _ pops) <- citys]

getCitySecondPopInMillions :: [City] -> [String]
getCitySecondPopInMillions citys = [getPopulationFromYearInMillions 1 pops | (City _ _ pops) <- citys]

getPopulationFromCityYearInMillions :: String -> Int -> [City] -> String
getPopulationFromCityYearInMillions _ _ [] = "No Data"
getPopulationFromCityYearInMillions name year ((City cityName _ population):rest)
    | map toLower name == map toLower cityName = getPopulationFromYearInMillions year population
    | otherwise = getPopulationFromCityYearInMillions name year rest


getPopulationFromYearInMillions :: Int -> [Int] -> String
getPopulationFromYearInMillions _ [] = "No Data"
getPopulationFromYearInMillions 0 (population:rest) = formatPopulation population
getPopulationFromYearInMillions year (population:rest) = getPopulationFromYearInMillions (year - 1) rest

formatPopulation :: Int -> String
formatPopulation pop = (show (roundToThreeDP (fromIntegral pop / 1000)) :: String) ++ "m"

roundToThreeDP :: Double -> Double
roundToThreeDP num = (fromIntegral . round $ num * f) / f
    where f = 10^3

getColumnWidth :: [String] -> Int -> Int
getColumnWidth [] _ = 0
getColumnWidth strings buffer = foldr (max . length) 0 strings + buffer

formatCitysTable :: [City] -> String
formatCitysTable citys = formatCitysTableWithSpacing citys (getColumnWidth (formatCityTableHeaders Name:getCityNames citys) 2, 
                                                            getColumnWidth (formatCityTableHeaders North:getCityNorth citys) 2, 
                                                            getColumnWidth (formatCityTableHeaders East:getCityEast citys) 2, 
                                                            getColumnWidth (formatCityTableHeaders CurrentPopulation:getCityFirstPopInMillions citys) 2, 
                                                            getColumnWidth (formatCityTableHeaders LastYearPopulation:getCitySecondPopInMillions citys) 2)
    where
        formatCitysTableWithSpacing :: [City] -> Spacing -> String
        formatCitysTableWithSpacing citys spacing = printf "%s\n%s\n%s"
                                                    (getTitleLine spacing)
                                                    (getLineSeparator spacing)
                                                    (formatCitys citys spacing)
            where
                getTitleLine :: Spacing -> String
                getTitleLine spacing = printf (getStringFormatWithSpaces spacing)
                                       (formatCityTableHeaders Name)
                                       (formatCityTableHeaders North)
                                       (formatCityTableHeaders East)
                                       (formatCityTableHeaders CurrentPopulation)
                                       (formatCityTableHeaders LastYearPopulation)
                    where
                        getStringFormatWithSpaces :: Spacing -> String
                        getStringFormatWithSpaces (s1,s2,s3,s4,s5) = "%-" ++ show s1 ++ "s%-" ++ show s2 ++ "s%-" ++ show s3 ++ "s%-" ++ show s4 ++ "s%-" ++ show s4 ++ "s"

                getLineSeparator :: Spacing -> String
                getLineSeparator spacing = createSeparatorLine (getSeparatorLength spacing)
                    where
                        createSeparatorLine :: Int -> String
                        createSeparatorLine 0 = ""
                        createSeparatorLine n = "-" ++ createSeparatorLine (n - 1)

                        getSeparatorLength :: Spacing -> Int
                        getSeparatorLength (s1,s2,s3,s4,s5) = s1 + s2 + s3 + s4 + s5

        formatCitys :: [City] -> Spacing -> String
        formatCitys [] _ = ""
        formatCitys [city] spacing = formatCity city spacing
        formatCitys (city:rest) spacing = formatCity city spacing ++ "\n" ++ formatCitys rest spacing

        formatCity :: City -> Spacing -> String
        formatCity (City name (Location north east) populations) spacing = printf (getStringFormatWithSpaces spacing)
                                                                                name
                                                                                north
                                                                                east
                                                                                (getPopulationFromYearInMillions 0 populations)
                                                                                (getPopulationFromYearInMillions 1 populations)
            where
                getStringFormatWithSpaces :: Spacing -> String
                getStringFormatWithSpaces (s1,s2,s3,s4,s5) = "%-" ++ show s1 ++ "s%-" ++ show s2 ++ "d%-" ++ show s3 ++ "d%-" ++ show s4 ++ "s%-" ++ show s4 ++ "s"
                                                          
addNewPopulationFigures :: [(City, Int)] -> [City]
addNewPopulationFigures [] = []
addNewPopulationFigures ((city, newPopulation):rest) = addNewPopulation (city, newPopulation):addNewPopulationFigures rest

addNewPopulation :: (City, Int) -> City
addNewPopulation (City name location population, newPopulation) = City name location (newPopulation:population)

insertNewCity :: City -> [City] -> [City]
insertNewCity (City newName newLocation newPopulations) ((City name location populations):rest)
    | length populations /= length newPopulations = City name location populations:rest
    | otherwise = sortBy (\(City firstName _ _) (City secondName _ _) -> if firstName == min firstName secondName then LT else GT)
                         (City newName newLocation newPopulations:City name location populations:rest)

getPopulationGrowthFiguresFromName :: String -> [City] -> [Int]
getPopulationGrowthFiguresFromName name citys 
    | isNothing (getCityFromName name citys) = []
    | otherwise = getPopulationGrowthFigures (fromJust  (getCityFromName name citys))

getPopulationGrowthFigures :: City -> [Int]
getPopulationGrowthFigures (City _ _ populations) = populationGrowthFigures populations
    where
        populationGrowthFigures :: [Int] -> [Int]
        populationGrowthFigures [] = []
        populationGrowthFigures [first] = []
        populationGrowthFigures (first:second:rest) = (first - second):populationGrowthFigures (second:rest)

getCityFromName :: String -> [City] -> Maybe City
getCityFromName _ [] = Nothing
getCityFromName name ((City cityName location population):rest)
    | map toLower name == map toLower cityName = Just (City cityName location population)
    | otherwise = getCityFromName name rest

getCityFromLocation :: Location -> [City] -> Maybe City
getCityFromLocation _ [] = Nothing
getCityFromLocation (Location northLocation eastLocation) ((City name (Location northCity eastCity) populations):rest) = 
    if northLocation == northCity && eastLocation == eastCity
    then Just (City name (Location northCity eastCity) populations)
    else getCityFromLocation (Location northLocation eastLocation) rest

getNameOfClosestCityWithLargerPopulation :: Location -> Int -> [City] -> String
getNameOfClosestCityWithLargerPopulation _ _ [] = "No City"
getNameOfClosestCityWithLargerPopulation location pop citys = getName (getClosestCityWithLargerPopulation location pop citys)
    where
        getName :: Maybe City -> String
        getName Nothing = "No City"
        getName (Just (City name _ _)) = name

getClosestCityWithLargerPopulation :: Location -> Int -> [City] -> Maybe City
getClosestCityWithLargerPopulation (Location north east) population citys = first (sortBy (\(City _ (Location firstNorth firstEast) _) (City _ (Location secondNorth secondEast) _) -> 
                                                                                   if pythag (firstNorth - north) (firstEast - east) < 
                                                                                      pythag (secondNorth - north) (secondEast - east)
                                                                                   then LT 
                                                                                   else GT)
                                                                                   (filter (\(City _ _ (currentPop:rest)) -> currentPop > population) citys))
    where
        pythag :: Int -> Int -> Double
        pythag n m = sqrt ((fromIntegral n^2) + (fromIntegral m^2))

        first :: [City] -> Maybe City
        first [] = Nothing
        first (firstCity:rest) = Just firstCity

toListOfTuples :: [a] -> [b] -> [(a,b)]
toListOfTuples [] [] = []
toListOfTuples (firstA:restA) (firstB:restB) = (firstA,firstB):toListOfTuples restA restB

--
--  Demo
--

demo :: Int -> IO ()
demo 1 = putStrLn (show (getCityNames testData) :: String)
demo 2 = putStrLn (show (getPopulationFromCityYearInMillions "Berlin" 1 testData) :: String)
demo 3 = putStrLn (formatCitysTable testData)
demo 4 = putStrLn (formatCitysTable (addNewPopulationFigures (toListOfTuples testData [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800])))
    -- output the data (as for (iii)) after it has been updated with the
         -- following new population figures (the first is for Amsterdam, etc.)
         -- [1200,3200,3600,1800,9500,6800,11100,4300,2000,1800]
demo 5 = putStrLn (formatCitysTable (insertNewCity (City "Stockholm" (Location 59 18) [1657, 1633, 1608, 1583]) testData))
    -- show the data (as for (iii)) after adding "Stockholm" (59N, 18E) 
         -- with population figures [1657, 1633, 1608, 1583]
demo 6 = putStrLn (show (getPopulationGrowthFiguresFromName "Athens" testData) :: String)
    -- output a list of annual growth figures for "Athens"
demo 7 = putStrLn (show (getNameOfClosestCityWithLargerPopulation (Location 45 8) 4000 testData) :: String)
    -- output the nearest city to location (45N, 8E) with 
         -- a population above 4m people
demo 8 = createMap testData
    -- output the population map


--
-- Screen Utilities (use these to do the population map)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text

convertLocationToScreenPosition :: Location -> ScreenPosition
convertLocationToScreenPosition (Location north east) = (convertEastToX east, convertNorthToY north)
    where
        convertEastToX :: Int -> Int
        convertEastToX east = convert east (-11) 40 80

        convertNorthToY :: Int -> Int
        convertNorthToY north = 50 - convert north 36 60 50

        convert :: Int -> Int -> Int -> Int -> Int
        convert input minBefore maxBefore maxAfter = round ((fromIntegral(input - minBefore) / fromIntegral (maxBefore - minBefore)) * fromIntegral maxAfter)
        -- ((input - (min east/north)) / (max east/north - min east/north)) * max x/y

createMap :: [City] -> IO ()
createMap citys = do
    clearScreen
    addCitysToMap citys

addCitysToMap :: [City] -> IO ()
addCitysToMap [] = return ()
addCitysToMap (city:rest) = do
    addCityToMap city
    addCitysToMap rest
 
addCityToMap :: City -> IO ()
addCityToMap (City name location (curentPopulation:rest)) =
    writeAt (convertLocationToScreenPosition location) (formatCityForMap (City name location (curentPopulation:rest)))

formatCityForMap :: City -> String
formatCityForMap (City name _ populations) = printf "+ %s %s"
                                             name
                                             (getPopulationFromYearInMillions 0 populations)


--
-- Your user interface (and loading/saving) code goes here
--
 

main :: IO ()
main = do
    clearScreen
    citysStr <- readFile "cities.txt"
    let citys = read citysStr :: [City]
    displayCityNames citys
    mainMenu citys


exit :: [City] -> IO ()
exit citys = writeFile "cities.txt" (show citys)


getCitys :: IO [City]
getCitys = do
    return testData


mainMenu :: [City] -> IO ()
mainMenu citys = do
    putStrLn "Welcome to the main menu!"
    putStrLn "Please Enter:"
    putStrLn "1) To display the list of cities"
    putStrLn "2) To view a cities population by year"
    putStrLn "3) To display all the city data"
    putStrLn "4) To update the data with new population figures"
    putStrLn "5) To add a new city"
    putStrLn "6) To view the annual population growth figures for a city"
    putStrLn "7) To view the closest city with a bigger population than the given location and population"
    putStrLn "8) To draw the map"
    putStrLn "9) To exit the program."
    option <- getLine
    case option of
        "1" -> do
            displayCityNames citys
            getLine
            mainMenu citys
        "2" -> do
            populationFromYearAndNameMenu citys
            mainMenu citys
        "3" -> do
            displayCityTable citys
            getLine
            clearScreen
            mainMenu citys
        "4" -> do
            citys <- updatePopulationFigures citys
            getLine
            clearScreen
            mainMenu citys
        "5" -> do
            citys <- addNewCity citys
            getLine
            clearScreen
            mainMenu citys
        "6" -> do
            growthFigures citys
            mainMenu citys
        "7" -> do
            closestCityWithBiggerPopulation citys
            mainMenu citys
        "8" -> do
            createMap citys
            getLine
            clearScreen
            mainMenu citys
        "9" -> do
            exit citys
        _ -> do
            putStrLn "Please enter a number from 1-9"
            mainMenu citys


displayCityNames :: [City] -> IO ()
displayCityNames citys = do
    let cityNames = getCityNames citys
    displayNames cityNames
        where
            displayNames :: [String] -> IO ()
            displayNames [] = return ()
            displayNames (name:rest) = do
                putStrLn name
                displayNames rest

isInt :: String -> Bool
isInt ""  = False
isInt ('-':xs) = isIntOrEmpty xs
isInt xs  = isIntOrEmpty xs

isIntOrEmpty :: String -> Bool
isIntOrEmpty "" = True
isIntOrEmpty (c:xs) = isDigit c && isIntOrEmpty xs

populationFromYearAndNameMenu :: [City] -> IO ()
populationFromYearAndNameMenu citys = do
    clearScreen
    name <- getCityName citys
    case name of
        Nothing -> return ()
        _ -> do
            year <- getValidYear (fromJust (getCityFromName (fromJust name) citys))
            case year of
                Nothing -> return ()
                _ -> do
                    let population = getPopulationFromCityYearInMillions (fromJust name) (fromJust year) citys
                    putStrLn (printf "The population of %s %d years ago was %s" (fromJust name) (fromJust year) population)
                    getLine
                    return ()
        where
            getCityName :: [City] -> IO (Maybe String)
            getCityName citys = do
                putStrLn ""
                putStrLn "From here you can view the population of a city in a given year"
                putStrLn "Please Enter:"
                putStrLn "The name of the city you want to view"
                putStrLn "or to go back enter nothing"
                name <- getLine
                if name == "" 
                    then 
                        return Nothing
                    else do
                        let city = getCityFromName name citys
                        if isNothing city
                            then do
                                putStrLn "There is no city found with that name"
                                getCityName citys
                            else
                                return (Just (getNameFromCity (fromJust city)))
                                    where
                                        getNameFromCity :: City -> String
                                        getNameFromCity (City name _ _) = name

            getValidYear :: City -> IO (Maybe Int)
            getValidYear (City name location populations) = do
                let numOfYears = length populations
                if numOfYears == 0
                    then do
                        putStrLn ""
                        putStrLn "That city has no population data to view"
                        return Nothing
                    else do
                        putStrLn ""
                        putStrLn "Please enter how many years ago you want to view where 0 is the current year"
                        putStrLn (printf "This city has %d %s avalible to view" numOfYears (if numOfYears == 1 then "year" else "years"))
                        putStrLn (printf "(Enter 0-%d)" (numOfYears - 1))
                        strYear <- getLine
                        if not (isInt strYear)
                            then do
                                invalidYear (City name location populations)
                            else do
                                let year = read strYear :: Int
                                if year <= (numOfYears - 1) && year >= 0
                                    then 
                                        return (Just year)
                                    else do
                                        invalidYear (City name location populations)
                            where 
                                invalidYear :: City -> IO (Maybe Int)
                                invalidYear city = do
                                    putStrLn "That is not a valid year for thie city"
                                    getValidYear city
                

displayCityTable :: [City] -> IO ()
displayCityTable citys = do
    clearScreen
    putStrLn (formatCitysTable citys)

updatePopulationFigures :: [City] -> IO [City]
updatePopulationFigures citys = do
    clearScreen
    putStrLn "Here you can enter a new set of population figures for each city"
    putStrLn "You must give a population for every city listed in order to update the information"
    putStrLn "Enter nothing at any time to cancel and go back to the main menu"
    newPopulations <- getNewPopulationSet citys
    if isNothing newPopulations
        then do
            putStrLn "Adding new populations has been cancelled"
            return citys
        else do
            let updatedCitys = addNewPopulationFigures (fromJust newPopulations)
            putStrLn "The new population data has been added successfully!"
            return updatedCitys
        where
            getNewPopulationSet :: [City] -> IO (Maybe [(City, Int)])
            getNewPopulationSet [] = return (Just [])
            getNewPopulationSet ((City name location populations):rest) = do
                putStrLn (printf "Please enter the new population for %s in thousands" name)
                newPopulationStr <- getLine
                if newPopulationStr == ""
                    then
                        return Nothing
                    else do
                        if not (isInt newPopulationStr)
                            then
                                invalidPopulation (City name location populations:rest)
                            else do
                                let newPopulation = read newPopulationStr :: Int
                                if newPopulation < 0
                                    then
                                        invalidPopulation (City name location populations:rest)
                                    else do
                                        restOfPopulations <- getNewPopulationSet rest
                                        if isNothing restOfPopulations
                                            then
                                                return Nothing
                                            else
                                                return (Just ((City name location populations, newPopulation):fromJust restOfPopulations))
                            where
                                invalidPopulation :: [City] -> IO (Maybe [(City, Int)])
                                invalidPopulation citys = do
                                    putStrLn "That is not a valid population"
                                    getNewPopulationSet citys

addNewCity :: [City] -> IO [City]
addNewCity citys = do
    putStrLn "Here you can enter a new city"
    putStrLn "You must give the same number of years of population data that the current cities already have"
    putStrLn (printf "This is currently %d" (getNumberOfPopulationFigures citys))
    putStrLn "Enter nothing at any time to cancel and go back to the main menu"
    maybeName <- getCityName citys
    if isNothing maybeName
        then
            cancelMessage citys
        else do
            let name = fromJust maybeName
            maybeLocation <- getLocation citys
            if isNothing maybeLocation
                then 
                    cancelMessage citys
                else do
                    let location = fromJust maybeLocation
                    maybePopulation <- getPopulationFigures citys
                    if isNothing maybePopulation
                        then 
                            cancelMessage citys
                        else do
                            let population = fromJust maybePopulation
                            putStrLn (printf "%s has been added successfully!" name)
                            return (insertNewCity (City name location population) citys)

    where
        getNumberOfPopulationFigures :: [City] -> Int
        getNumberOfPopulationFigures [] = 0
        getNumberOfPopulationFigures ((City _ _ populations):rest) = length populations

        cancelMessage :: [City] -> IO [City]
        cancelMessage citys = do
            putStrLn "Adding a new city has been cancelled"
            return citys

        getCityName :: [City] -> IO (Maybe String)
        getCityName citys = do
            putStrLn "Please enter the name of the city"
            name <- getLine
            if name == ""
                then
                    return Nothing
                else
                    if isJust (getCityFromName name citys)
                        then
                            invalidName citys
                        else
                            return (Just name)
            where
                invalidName :: [City] -> IO (Maybe String)
                invalidName citys = do
                    putStrLn "A city with that name has already been added"
                    getCityName citys

        getLocation :: [City] -> IO (Maybe Location)
        getLocation citys = do
            maybeNorth <- getCoordinate "north" 90 (-90)
            if isNothing maybeNorth
                then
                    return Nothing
                else do
                    let north = fromJust maybeNorth
                    maybeEast <- getCoordinate "east" 180 (-180)
                    if isNothing maybeEast
                        then
                            return Nothing
                        else do
                            let east = fromJust maybeEast
                            let location = Location north east
                            if isJust (getCityFromLocation location citys)
                                then do
                                    invalidLocation citys
                                else do
                                    return (Just location)
            where               
                invalidLocation :: [City] -> IO (Maybe Location)
                invalidLocation citys = do
                    putStrLn "A city with that location has already been added"
                    getLocation citys

                getCoordinate :: String -> Int -> Int -> IO (Maybe Int)
                getCoordinate coordinateDirection max min = do
                    putStrLn (printf "Please enter the %s coordinate of the city" coordinateDirection)
                    putStrLn (printf "It must be beween %d and %d" max min)
                    coordinateStr <- getLine
                    if coordinateStr == ""
                        then
                            return Nothing
                        else
                            if not (isInt coordinateStr)
                                then
                                    invalidCoordinate coordinateDirection max min
                                else do
                                    let coordinate = read coordinateStr :: Int
                                    if not (coordinate <= max && coordinate >= min)
                                        then
                                            invalidCoordinate coordinateDirection max min
                                        else
                                            return (Just coordinate)
                    where 
                        invalidCoordinate :: String -> Int -> Int -> IO (Maybe Int)
                        invalidCoordinate coordinateDirection max min = do
                            putStrLn (printf "That is not a valid %s coordinate" coordinateDirection)
                            getCoordinate coordinateDirection max min

        getPopulationFigures :: [City] -> IO (Maybe [Int])
        getPopulationFigures citys = do
            maybePopulations <- getPopulations (getNumberOfPopulationFigures citys - 1)
            if isNothing maybePopulations
                then
                    return Nothing
                else do
                    let populations = fromJust maybePopulations
                    return (Just (reverse populations))

            where
                getPopulations :: Int -> IO (Maybe [Int])
                getPopulations yearsAgo
                    | yearsAgo < 0 = return (Just [])
                    | otherwise = do
                        if yearsAgo == 0
                            then
                                putStrLn (printf "Please enter the population of the city in thousands %s" "for the current year")
                            else do
                                let yearsAgoText = printf "%d years ago" yearsAgo :: String
                                putStrLn (printf "Please enter the population of the city in thousands %s" yearsAgoText)
                        populationStr <- getLine
                        if populationStr == ""
                            then 
                                return Nothing
                            else
                                if not (isInt populationStr)
                                    then
                                        invalidPopulation yearsAgo
                                    else do
                                        let population = read populationStr :: Int
                                        if population < 0 
                                            then
                                                invalidPopulation yearsAgo
                                            else do
                                                maybePopulations <- getPopulations (yearsAgo - 1)
                                                if isNothing maybePopulations
                                                    then 
                                                        return Nothing
                                                    else do
                                                        let populations = population:fromJust maybePopulations
                                                        return (Just populations)

                                    where
                                        invalidPopulation :: Int -> IO (Maybe [Int])
                                        invalidPopulation yearsAgo = do
                                            putStrLn "That is not a valid population"
                                            getPopulations yearsAgo


data GrowthFiguresFormats = CurrentYear | PreviousYear | Other

growthFiguresFormats :: GrowthFiguresFormats -> String
growthFiguresFormats CurrentYear = "Current years growth: "
growthFiguresFormats PreviousYear = "Growth %d year ago: "
growthFiguresFormats Other = "Growth %d years ago: "

growthFigures :: [City] -> IO ()
growthFigures citys = do
    putStrLn ""
    putStrLn "Here you can see the growth figures for the cities"
    putStrLn "Please enter a city name"
    putStrLn "or nothing to exit"
    name <- getLine
    if name == ""
        then 
            return ()
        else do
            let maybeCity = getCityFromName name citys
            if isNothing maybeCity
                then
                    invalidName citys
                else do
                    let city = fromJust maybeCity
                    displayGrowthFigures city
                    getLine
                    return ()
    where
        invalidName :: [City] -> IO ()
        invalidName citys = do
            putStrLn "There is no city found with that name"
            getLine
            growthFigures citys

        displayGrowthFigures :: City -> IO ()
        displayGrowthFigures (City name location populations) = do
            let growthFigures = getPopulationGrowthFigures (City name location populations)
            putStrLn (printf "The growth figures for %s are" name)
            let formatedGrowthFigures = formatFigures growthFigures
            let spacing = getSpacing growthFigures
            displayFigures formatedGrowthFigures 0 spacing
            where
                displayFigures :: [String] -> Int -> Int-> IO ()
                displayFigures [] _ _ = return ()
                displayFigures (growth:rest) yearsAgo spacing = do
                    if yearsAgo == 0
                        then do
                            let format = printf ("%-" ++ show spacing ++ "s") (growthFiguresFormats CurrentYear)
                            putStrLn (printf (format ++ "%s") growth)
                    else if yearsAgo == 1
                        then do
                            let growthFormat = printf (growthFiguresFormats PreviousYear) yearsAgo :: String
                            let format = printf ("%-" ++ show spacing ++ "s") growthFormat
                            putStrLn (printf (format ++ "%s") growth)
                        else do
                            let growthFormat = printf (growthFiguresFormats Other) yearsAgo :: String
                            let format = printf ("%-" ++ show spacing ++ "s") growthFormat
                            putStrLn (printf (format ++ "%s") growth)
                    displayFigures rest (yearsAgo + 1) spacing

                formatFigures :: [Int] -> [String]
                formatFigures [] = []
                formatFigures (figure:rest) = printf "%s Thousand" (show figure):formatFigures rest

                getSpacing :: [Int] -> Int
                getSpacing (current:previous:rest) = 
                    getColumnWidth (growthFiguresFormats CurrentYear:
                    printf (growthFiguresFormats PreviousYear) previous:
                    getSpacingOther rest 2) 0
                    where
                        getSpacingOther :: [Int] -> Int -> [String]
                        getSpacingOther [] _ = []
                        getSpacingOther (other:rest) i = printf (growthFiguresFormats Other) i:getSpacingOther rest (i + 1)


closestCityWithBiggerPopulation :: [City] -> IO ()
closestCityWithBiggerPopulation citys = do
    clearScreen
    putStrLn "Here you can find the clostest city to a point on the globe where the city has a larger population than the specified amount"
    putStrLn "To exit at any point enter nothing"
    maybeNorth <- getCoordinate "north" 90 (-90)
    if isNothing maybeNorth
        then
            return ()
        else do
            let north = fromJust maybeNorth
            maybeEast <- getCoordinate "east" 180 (-180)
            if isNothing maybeEast
                then
                    return ()
                else do
                    let east = fromJust maybeEast
                    let location = Location north east
                    maybePopulation <- getPopulation
                    if isNothing maybePopulation
                        then
                            return ()
                        else do
                            let population = fromJust maybePopulation
                            let maybeClosestCity = getClosestCityWithLargerPopulation location population citys
                            if isNothing maybeClosestCity
                                then do
                                    putStrLn (printf "There is no city with a larger population %d" population)
                                    getLine
                                    return ()
                                else do
                                    let (City cityName (Location cityNorth cityEast) (cityPopulation:rest)) = fromJust maybeClosestCity
                                    putStrLn (printf "The closest city to %dN %dE with a population larger than %d is %s which had a population of %d and is located at %dN %dE"
                                              north
                                              east
                                              population
                                              cityName
                                              cityPopulation
                                              cityNorth
                                              cityEast
                                              )
                                    getLine
                                    return ()
    where
        getCoordinate :: String -> Int -> Int -> IO (Maybe Int)
        getCoordinate coordinateDirection max min = do
            putStrLn (printf "Please enter the %s coordinate of the city" coordinateDirection)
            putStrLn (printf "It must be beween %d and %d" max min)
            coordinateStr <- getLine
            if coordinateStr == ""
                then
                    return Nothing
                else
                    if not (isInt coordinateStr)
                        then
                            invalidCoordinate coordinateDirection max min
                        else do
                            let coordinate = read coordinateStr :: Int
                            if not (coordinate <= max && coordinate >= min)
                                then
                                    invalidCoordinate coordinateDirection max min
                                else
                                    return (Just coordinate)
        
        invalidCoordinate :: String -> Int -> Int -> IO (Maybe Int)
        invalidCoordinate coordinateDirection max min = do
            putStrLn (printf "That is not a valid %s coordinate" coordinateDirection)
            getCoordinate coordinateDirection max min

        getPopulation :: IO (Maybe Int)
        getPopulation = do
            putStrLn (printf "Please enter the population for in thousands")
            newPopulationStr <- getLine
            if newPopulationStr == ""
                then
                    return Nothing
                else do
                    if not (isInt newPopulationStr)
                        then
                            invalidPopulation
                        else do
                            let population = read newPopulationStr :: Int
                            if population < 0
                                then
                                    invalidPopulation
                                else do
                                    return (Just population)
            where
                invalidPopulation :: IO (Maybe Int)
                invalidPopulation = do
                    putStrLn "That is not a valid population"
                    getPopulation