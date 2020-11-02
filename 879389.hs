-- || 
-- || Discrete Mathematics and Functional Programming 
-- || Functional Programming Assignment 2018/2019 
-- || Student 879389 
-- || 

import Data.List 
import Data.Char
import Data.Ord 


-- || 
-- ||  Define types
-- || 

type Title  = String 
type Artist = String 
type Year   = Int 
type Sales  = Int 

type Album  = (Title, Artist, Year, Sales) 


-- || 
-- || Test Database for the system 
-- || 

testData :: [Album]
testData = [("Greatest Hits", "Queen", 1981, 6300000),
    ("Gold: Greatest Hits", "ABBA", 1992, 5400000),
    ("Sgt. Pepper's Lonely Hearts Club Band","The Beatles",1967,5340000),
    ("21","Adele",2011,5110000),
    ("(What's the Story) Morning Glory?","Oasis",1995,4940000),
    ("Thriller","Michael Jackson",1982,4470000),
    ("The Dark Side of the Moon","Pink Floyd",1973,4470000),
    ("Brothers in Arms","Dire Straits",1985,4350000),
    ("Bad","Michael Jackson",1987,4140000),
    ("Rumours","Fleetwood Mac",1977,4090000),
    ("Greatest Hits II","Queen",1991,3990000),
    ("Back to Black","Amy Winehouse",2006,3940000),
    ("The Immaculate Collection","Madonna",1990,3700000),
    ("25","Adele",2015,3500000),
    ("Stars","Simply Red",1991,3450000),
    ("Come On Over","Shania Twain",1998,3430000),
    ("x","Ed Sheeran",2014,3380000),
    ("Legend","Bob Marley",1984,3380000),
    ("Bat Out of Hell","Meat Loaf",1977,3370000),
    ("Back to Bedlam","James Blunt",2004,3360000),
    ("Urban Hymns","The Verve",1997,3340000),
    ("Bridge over Troubled Water","Simon & Garfunkel",1970,3260000),
    ("1","The Beatles",2000,3230000),
    ("Spirit","Leona Lewis",2007,3170000),
    ("Crazy Love","Michael Bublé",2009,3130000),
    ("No Angel","Dido",2000,3090000),
    ("White Ladder","David Gray",1998,3020000),
    ("The Fame","Lady Gaga",2009,2990000),
    ("Only by the Night","Kings of Leon",2008,2980000),
    ("A Rush of Blood to the Head","Coldplay",2002,2960000),
    ("Talk on Corners","The Corrs",1997,2960000),
    ("Spice", "Spice Girls", 1996, 2960000),
    ("Life for Rent", "Dido", 2003, 2900000),
    ("Beautiful World", "Take That", 2006, 2880000),
    ("The Joshua Tree", "U2",1987,2880000),
    ("Hopes and Fears","Keane",2004,2860000),
    ("The War of the Worlds","Jeff Wayne",1978,2800000),
    ("X&Y","Coldplay",2005,2790000),
    ("Jagged Little Pill","Alanis Morissette",1995,2780000),
    ("Tubular Bells","Mike Oldfield",1973,2760000),
    ("Scissor Sisters","Scissor Sisters",2004,2760000),
    ("...But Seriously","Phil Collins",1989,2750000),
    ("Tracy Chapman","Tracy Chapman",1988,2710000),
    ("Parachutes","Coldplay",2000,2710000),
    ("The Man Who","Travis",1999,2687500),
    ("Greatest Hits","ABBA",1975,2606000),
    ("I've Been Expecting You","Robbie Williams",1998, 2586500),
    ("Come Away with Me","Norah Jones",2002,2556650),
    ("Graceland","Paul Simon",1986,2500000),
    ("Ladies & Gentlemen: The Best of","George Michael",1998,2500000)
    ]

-- || 
-- || Functional Code 
-- || 

-- || 
-- || (i) 
-- ||

albumsToString :: [Album] -> String
albumsToString [] = ""
albumsToString ((ti, ar, yr, sa):xs) = "\nTitle: " ++ ti ++ "\nArtist: " ++ ar  ++ "\nYear: " ++ show ( yr ) ++ "\nNumber of Sales: " ++ show(sa) ++ "\n" ++ albumsToString xs

-- || 
-- || (ii) 
-- ||

top10 :: [Album] -> [Album] 
top10 xs = take 10 xs 

-- ||
-- || (iii)  
-- ||

albumsBetweenYear ::  Int -> Int -> [Album] -> [Album]
albumsBetweenYear year yer db = [ (ti, ar, yr, sa) 
           | (ti, ar, yr, sa) <- db, yr >= year && yr <= yer]
-- ||
-- || (iv)
-- ||

albumsPrefix :: String -> [Album] -> [Album] 
albumsPrefix x db = [(ti, ar, yr, sa)
              | (ti, ar, yr, sa) <- db, isPrefixOf x ti ] 

-- ||
-- || (v) 
-- || 

artistsSalesToString :: String -> [Album] -> String
artistsSalesToString art db = "Artist: " ++ art  ++ "\tNumber of Sales: " ++ show(calculateTotal art db)

calculateTotal :: String -> [Album] -> Int
calculateTotal sal db = sum(map(\(_,_,_,x) -> x) (albumsWithSpecific sal db))

albumsWithSpecific :: String -> [Album] -> [Album]
albumsWithSpecific x db = [(ti, ar, yr, sa) | (ti, ar, yr, sa) <- db, isPrefixOf x ar ]

-- ||
-- || (vi) 
-- || 

formatCount :: [Album] -> String
formatCount [] = ""
formatCount ((ti,ar,yr,sa):xs) = "\nArtist:" ++ ar ++ " \t\t\tNumber of Times in Top 50: " ++ show(checkCount ar  testData) ++ "\n" ++ formatCount xs


checkCount :: String -> [Album] -> Int
checkCount _ [] = 0
checkCount art ((ti,ar,yr,sa):xs)
           |(art == ar) = 1 + checkCount  art xs 
           | otherwise = 0 + checkCount art xs  
-- || 
-- || (vii) 
-- || 

remove50thAlbum :: [Album] -> [Album] 
remove50thAlbum ((ti, ar, yr, sa):xs) = init xs

addNewAlbum :: String -> String -> Int -> Int -> [Album] 
addNewAlbum ti ar yr sa = [(ti, ar, yr, sa)]  

-- ||
-- || (viii)
-- || 

addSales :: String -> String -> Int -> [Album] -> [Album]
addSales tit art incre db = [if ti == tit && ar == art then (ti, ar, yr, sa + incre) else (ti, ar, yr, sa) | (ti, ar, yr, sa) <- db ]


-- || 
-- || Demo function to test the system functionality 
-- || 

demo :: Int -> IO () 
-- Convert the lust into a single string which, if output using putStrLn, will display the data formatted neatly into four columns 
demo 1  = putStrLn (albumsToString testData)
-- || Give The top ten albums in descending order of sales 
demo 2 = putStrLn (albumsToString (top10 testData))
-- || Give all albums that were released between two given years (inclusive) 
demo 3  = putStrLn (albumsToString (albumsBetweenYear 2008 2009 testData))
-- || Give all albums whose titles begin with a given prefix 
demo 4 = putStrLn (albumsToString (albumsPrefix "Ba" testData))
-- || Give the total sales figure for a given artist
demo 5  = putStrLn(artistsSalesToString "Adele" testData)
-- || Give a list of Artists' names with the number of albums they have in the top 50 
demo 6  = putStrLn ( formatCount testData)
-- || Remove the 50th album and add a given album into the list 
demo 7 = putStrLn (albumsToString ((remove50thAlbum testData ++ addNewAlbum "The Marshall Mathers LP" "Eminem" 2000 2460000)))
-- || Increase sales figure for one of the albums 
demo 8  = putStrLn (albumsToString(addSales "x" "Ed Sheeran" 500000 testData) )

-- ||
-- || User Interface and File I/O 
-- || 

main :: IO ()
main = do
    albumData <- loadDB
    putStrLn("========================================================")
    putStrLn(" UP879389's Album Database")
    putStrLn("========================================================\n")
    mainMenu albumData

loadDB :: IO [Album]
loadDB = do
    albumData <- readFile "test-data.txt"
    return (read albumData)

mainMenu :: [Album] -> IO()
mainMenu albumDB = do
  putStrLn("========================================================")
  putStrLn("Welcome to the Top 50 Album Database\n" ++ "\nEnter the number for the required option:")
  putStrLn("========================================================")
  putStrLn(" 1 | Show all Albums")
  putStrLn(" 2 | Show the top 10 best selling albums")
  putStrLn(" 3 | Show albums released between two years")
  putStrLn(" 4 | Search albums by a prefix")
  putStrLn(" 5 | Show sales of a particular artist")
  putStrLn(" 6 | Show number of times each artist appears in the top 50")
  putStrLn(" 7 | Remove the last album and update with a new album to the top 50")
  putStrLn(" 8 | Increase an album's sales figures")
  putStrLn(" 0 | Save and Exit")
  putStrLn("========================================================\n")
  putStr("Please insert your option: ")
  option <- getLine
  putStrLn("\n")
  chosenOption option albumDB
  
chosenOption :: String -> [Album] -> IO ()
-- Save the database and exit the UI
chosenOption "0" albumDB = saveAndExit albumDB
chosenOption "1" albumDB = showAllAlbums albumDB
chosenOption "2" albumDB = showTop10Albums albumDB
chosenOption "3" albumDB = getAlbumsBetween albumDB
chosenOption "4" albumDB = getAlumPrefix albumDB
chosenOption "5" albumDB = showArtistSales albumDB
chosenOption "6" albumDB = artistAppearance albumDB
--chosenOption "7" albumDB = showNewAlbum albumDB
chosenOption _ albumDB = do
  putStrLn (errorMessage "option" )
  mainMenu albumDB
  
-- || i 
showAllAlbums :: [Album] -> IO()
showAllAlbums albumDB =
 do
  putStrLn("========================================================")
  putStrLn("View the top 50 albums within the database\n")
  putStrLn( albumsToString albumDB )
  putStrLn("========================================================\n")
  mainMenu albumDB

-- ii  
showTop10Albums :: [Album] -> IO()
showTop10Albums albumDB =
 do
  putStrLn("========================================================")
  putStrLn("View the top 10 albums within the database\n")
  putStrLn (albumsToString (top10 albumDB))
  putStrLn("========================================================\n")
  mainMenu albumDB

-- iii 
getAlbumsBetween :: [Album] -> IO()
getAlbumsBetween albumDB =
 do
  putStrLn("=================================================")
  putStrLn("View all albums released between range specified.\n")
  putStrLn("=================================================\n")
  putStr("Enter 1st year: ")
  year1 <- getLine
  putStr("Enter 2nd year: ")
  year2 <- getLine
  putStrLn("\n")
  if year1 == "" && year2 ==""
  then do
    putStrLn(errorMessage "input")
    getAlbumsBetween albumDB
  else do
      let firstYear = (read year1 :: Int)
      let secondYear = (read year2 :: Int)
      if (firstYear >= 1960 && firstYear <= 2100) && (secondYear >= 1960 && secondYear <= 2100)
      then do
      putStrLn("==================================================")
      putStrLn("All albums released between "++ show( firstYear ) ++ " and " ++ show(secondYear))
      putStrLn("==================================================")
      let result = albumsBetweenYear firstYear secondYear albumDB

      if result /= []
      then do
        putStrLn(albumsToString( result ))
        mainMenu albumDB
      else do
        putStrLn("There are no albums released between "++ show( firstYear ) ++ " and " ++ show(secondYear) ++ "\n")
        putStrLn("========================================================\n")
        mainMenu albumDB
      else do
        putStrLn(errorMessage "year")
        getAlbumsBetween albumDB
-- iv
getAlumPrefix :: [Album] -> IO()
getAlumPrefix albumDB =
 do
  putStrLn("========================================================")
  putStrLn("View all albums with given prefix.")
  putStr("Enter prefix of album in search: ")
  album <- getLine
  if album == ""
  then do
    putStrLn(errorMessage "input")
    getAlumPrefix albumDB
  else do
   putStrLn("\nAll album titles with inputted prefix: "++ album ++"\n")
   putStrLn (albumsToString (albumsPrefix album albumDB))
  putStrLn("========================================================\n")
  mainMenu albumDB

-- v 
showArtistSales :: [Album] -> IO()
showArtistSales albumDB =
 do
  putStrLn("========================================================")
  putStrLn("View amount of Artist sales.")
  putStr("Enter artist name: ")
  artist <- getLine
  if artist == ""
  then do
    putStrLn(errorMessage "string")
    showArtistSales albumDB
  else do
    putStrLn("\nArtist and Number of Sales: "++ artist ++"\n")
    putStrLn(artistsSalesToString artist albumDB)
    putStrLn("========================================================\n")
    mainMenu albumDB

-- vi 
artistAppearance :: [Album] -> IO()
artistAppearance albumDB =
 do
  putStrLn("========================================================")
  putStrLn("View number of times each Artist appears in Top 50.")
  putStrLn("========================================================\n")
  putStrLn (formatCount albumDB)
  putStrLn("========================================================\n")
  mainMenu albumDB
  
saveAndExit :: [Album] -> IO()
saveAndExit albumDB =
  do
  putStrLn("========================================================")
  putStrLn("Saving the Database")
  -- Avoids the issue with Parse Error, Force writing ALL of the filmDB before continuing.
  length albumDB `seq` writeFile "album-data.txt" ( show albumDB )
  putStrLn("Saved the Database successfully")
  putStrLn("Exiting the system")
  putStrLn("========================================================\n")

errorMessage :: String -> String
errorMessage "int" = "\n[Error]: The value entered was not the expected value (Number / Int)\n[Error]: Option is being reset, please try again.\n"
errorMessage "string" = "\n[Error]: The value entered was not the expected value (Word / String)\n[Error]: Option is being reset, please try again.\n"
errorMessage "input" = "\n[Error]: There was no input given!\n[Error]: Option is being reset, please try again.\n"
errorMessage "year" = "\n[Error]: The year is not valid, enter a value after year 1900 - 2018!\n[Error]: Option is being reset, please try again.\n"
errorMessage "option" = "\n[Error]: An incorrect action was given, please try again!\n"
errorMessage _ = ""
  
  


  


