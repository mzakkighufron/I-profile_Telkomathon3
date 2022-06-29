module Data where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
--import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)
import Data.Int
import Data.Time
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime,utcTimeToPOSIXSeconds )


data Persona
    = Persona
        {
        internetId :: Int
        , nama :: String
        , noKtp :: Int
        , alamat :: String
        , kecepatan :: Int
        , gangguan :: Int
        , tagihan :: Int
        , paket :: String
        }
    | UnknownItem
    deriving (Show, Eq)

data Status = TAG| GGN | UPK | UPP | NEW | ERR deriving (Show, Read)

data Changelog
    = Changelog
        { inetId :: Int
        , name :: String
        , timestamp :: UTCTime
        , status :: Status
        , ggn :: Int
        , tgn :: Int
        }
    | Unknown
    deriving (Show)


addNewCustomer :: [Persona] -> String -> Int -> String -> Int -> String -> IO [Persona]
addNewCustomer oldPersonaList nama noKtp alamat kecepatan paket = do
    let lastId =
            if null oldPersonaList
                then 0
                else internetId $ last oldPersonaList
        newId = lastId + 1
        newPersona =
            Persona
                { internetId = newId
                , nama = nama
                , noKtp = noKtp
                , alamat = alamat
                , kecepatan = kecepatan
                , gangguan = 0
                , tagihan = 0
                , paket = paket
                }
    let newPersonaList = oldPersonaList ++ [newPersona]
    return newPersonaList

upgradeKecepatan :: [Persona] -> Int -> Int -> IO [Persona]
upgradeKecepatan oldPersonaList choice kecepatan = do
    let customerExist = find (\item -> (internetId item) == choice) oldPersonaList

        extractItem :: Maybe Persona -> Persona
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        replaceItem :: [Persona] -> Persona -> Int -> [Persona]
        replaceItem [] chosenItem kecepatan = []
        replaceItem (item : rest) chosenItem kecepatan 
            | item == chosenItem = [item{kecepatan = kecepatan}] ++ replaceItem rest chosenItem kecepatan
            | otherwise = [item] ++ replaceItem rest chosenItem kecepatan

    let upgradedPersonaList =
            if (extractItem customerExist) == UnknownItem
                then oldPersonaList
                else replaceItem oldPersonaList (extractItem customerExist) kecepatan
    if (extractItem customerExist) == UnknownItem
        then putStrLn "Pelanggan Tidak Ditemukan, Periksa Kembali Nomor Internet Pelanggan"
        else putStrLn "Berhasil Mengubah Data Pelanggan" 
    return upgradedPersonaList

upgradePaket :: [Persona] -> Int -> String -> IO [Persona]
upgradePaket oldPersonaList choice paket = do
    let customerExist = find (\item -> internetId item == choice) oldPersonaList

        extractItem :: Maybe Persona -> Persona
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        replaceItem :: [Persona] -> Persona -> String -> [Persona]
        replaceItem [] chosenItem paket = []
        replaceItem (item : rest) chosenItem paket 
            | item == chosenItem = [item{paket = paket}] ++ replaceItem rest chosenItem paket
            | otherwise = [item] ++ replaceItem rest chosenItem paket


    let upgradedPersonaList =
            if (extractItem customerExist) == UnknownItem
                then oldPersonaList
                else replaceItem oldPersonaList (extractItem customerExist) paket
    if (extractItem customerExist) == UnknownItem
        then putStrLn "Pelanggan Tidak Ditemukan, Periksa Kembali Nomor Internet Pelanggan"
        else putStrLn "Berhasil Mengubah Data Pelanggan"  
    return upgradedPersonaList

bayarTagihan :: [Persona] -> Int -> Int -> IO [Persona]
bayarTagihan oldPersonaList choice newtagihan = do
    let customerExist = find (\item -> internetId item == choice) oldPersonaList
        
        extractItem :: Maybe Persona -> Persona
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        replaceItem :: [Persona] -> Persona -> Int -> [Persona]
        replaceItem [] chosenItem paket = []
        replaceItem (item : rest) chosenItem paket 
            | item == chosenItem = [item{tagihan = tagihan item + newtagihan}] ++ replaceItem rest chosenItem newtagihan
            | otherwise = [item] ++ replaceItem rest chosenItem newtagihan


    let upgradedPersonaList =
            if (extractItem customerExist) == UnknownItem
                then oldPersonaList
                else replaceItem oldPersonaList (extractItem customerExist) newtagihan
    if (extractItem customerExist) == UnknownItem
        then putStrLn "Pelanggan Tidak Ditemukan, Periksa Kembali Nomor Internet Pelanggan"
        else putStrLn "Berhasil Membayar tagihan"  
    
    return upgradedPersonaList

laporGangguan :: [Persona] -> Int -> Int -> IO [Persona]
laporGangguan oldPersonaList choice newgangguan = do
    let customerExist = find (\item -> internetId item == choice) oldPersonaList
        
        extractItem :: Maybe Persona -> Persona
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        replaceItem :: [Persona] -> Persona -> Int -> [Persona]
        replaceItem [] chosenItem paket = []
        replaceItem (item : rest) chosenItem paket 
            | item == chosenItem = [item{gangguan = gangguan item + newgangguan}] ++ replaceItem rest chosenItem newgangguan
            | otherwise = [item] ++ replaceItem rest chosenItem newgangguan


    let upgradedPersonaList =
            if (extractItem customerExist) == UnknownItem
                then oldPersonaList
                else replaceItem oldPersonaList (extractItem customerExist) newgangguan
    if (extractItem customerExist) == UnknownItem
        then putStrLn "Pelanggan Tidak Ditemukan, Periksa Kembali Nomor Internet Pelanggan"
        else putStrLn "Berhasil Membayar tagihan"  
    
    return upgradedPersonaList

parsePersona :: [Persona] -> IO ()
parsePersona persona = do
    let convertToLog :: [Persona] -> String
        convertToLog [] = ""
        convertToLog (item : rest) =
            show (internetId item)
                ++ " "
                ++ nama item
                ++ " "
                ++ show (noKtp item)
                ++ " "
                ++ alamat item
                ++ " "
                ++ show (kecepatan item)
                ++ " "
                ++ show (gangguan item)
                ++ " "
                ++ show (tagihan item)
                ++ " "
                ++ paket item
                ++ "\n"
                ++ convertToLog rest
    let parsedPersona = init $ convertToLog persona -- using init to remove the last \n at the end of the .log
    writeFile "log/Customer.log" parsedPersona

parseItem :: String -> [Persona]
parseItem rawContent = map parseSingleItem (lines rawContent)

parseSingleItem :: String -> Persona
parseSingleItem str = case words str of
    (i : n : t : a : k : p : g : h) -> makeItem i n t a k p g h
    _ -> UnknownItem

makeItem :: String -> String -> String -> String -> String -> String -> String -> [String] -> Persona
makeItem internetId nama noKtp alamat kecepatan gangguan tagihan paket  =
    Persona
        { internetId = read internetId
        , nama = nama
        , noKtp = read noKtp
        , alamat = alamat
        , kecepatan = read kecepatan
        , gangguan = read gangguan
        , tagihan = read tagihan 
        , paket =  unwords paket
        }

secondSinceEpoch :: UTCTime -> Int
secondSinceEpoch =
    floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

makeLogMessage :: Persona -> String -> IO Changelog
makeLogMessage item status = do
    u <- getCurrentTime
    let currentTime = u
        message =
            if item == UnknownItem
                then
                    Changelog
                        { inetId = 0
                        , name  = ""
                        , timestamp = currentTime
                        , status = ERR
                        , ggn = 0
                        , tgn = 0
                        }
                else
                    Changelog
                        { inetId = internetId item
                        , name = nama item
                        , timestamp = currentTime
                        , status = read status :: Status
                        , ggn = gangguan item
                        , tgn = tagihan item
                        }
    return message

parseLogMessage :: Changelog -> IO ()
parseLogMessage changelog = do
    u <- getCurrentTime
    let currentTime = secondSinceEpoch u
    let parsedLogMessage =
            "InternetID: "
                ++ show (inetId changelog)
                ++ " | Status: "
                ++ show (status changelog)
                ++ " | Nama: "
                ++ show (name changelog)
                ++ " | Timestamp: "
                ++ show currentTime
                ++ " | Jmlh Gangguan:"
                ++ show (ggn changelog)
                ++ " | Jmlh Tagihan lunas:"
                ++ show (tgn changelog)
                ++ "\n"
    appendFile "log/Changelog.log" parsedLogMessage
