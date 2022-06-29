module Main where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List (find) 
import Data (Persona (UnknownItem) ,Changelog ,internetId, nama , noKtp, alamat, kecepatan, paket, gangguan, tagihan, parsePersona, secondSinceEpoch ,addNewCustomer, makeLogMessage, parseLogMessage, parseItem,parseSingleItem, makeItem, upgradeKecepatan, upgradePaket, bayarTagihan, laporGangguan)
import Pembantu (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)


runProgram :: [Persona] -> IO ()
runProgram customer = do
    putStrLn "\n\n\n================================== Database Indihome Customer =================================="
    putStrLn $ replicate 96 '='
    putStrLn $ showData customer
    putStrLn "(a) Show all Data  (b) Upgrade Customer  (c) Bayar Tagihan  (d) Lapor Gangguan  (e) Add Customer"
    putStrLn "=====================================      (f) Exit Program      ==============================="
    choice <- prompt "Masukkan Pilihan: "
    case choice of
        "a" -> do
            putStrLn "\n"
            putStrLn $ showAllData customer
            empty <- prompt "Tekan enter untuk kembali"
            runProgram customer
        "b" -> do
            putStrLn "\nApa Yang Akan Anda Upgrade "
            putStrLn "\n(a) Upgrade Kecepatan Internet \n(b) Upgrade Paket Internet"
            pilihan <- prompt "\nMasukkan Pilihan: "
            case pilihan of
                "a" -> do
                    -- Masukkan InternetID
                    putStr "Masukkan InternetID: "
                    hFlush stdout
                    choice <- do
                        result <- runMaybeT maybeReadInt
                        case result of 
                            (Just a) -> return a
                            Nothing -> return 0

                    -- Masukkan Kecepatan
                    putStr "Masukkan Kecepatan terbaru: "
                    hFlush stdout
                    kecepatan <- do
                        result <- runMaybeT maybeReadInt
                        case result of
                            (Just a) -> return a
                            Nothing -> return 0
            
                    newUpgradedCustomer <- upgradeKecepatan customer choice kecepatan
                    parsePersona newUpgradedCustomer

                    let changedItem = find (\item -> internetId item == choice) newUpgradedCustomer
                        extractItem :: Maybe Persona -> Persona
                        extractItem (Just a) = a
                        extractItem Nothing = UnknownItem

                    let extractedItem = extractItem changedItem

                    logMessage <-
                        if extractedItem == UnknownItem
                            then makeLogMessage extractedItem "ERR"
                            else makeLogMessage (extractedItem{kecepatan = kecepatan}) "UPK"

                    parseLogMessage logMessage
                    emptyPrompt <- prompt "Tekan Enter untuk melanjutkan"
                    runProgram newUpgradedCustomer       
                "b" -> do
                    -- Masukkan InternetID
                    putStr "Masukkan InternetID: "
                    hFlush stdout
                    choice <- do
                        result <- runMaybeT maybeReadInt
                        case result of
                            (Just a) -> return a
                            Nothing -> return 0

                    -- Masukkan Paket
                    paket <- prompt "Masukkan Paket terbaru: "
            
                    newUpgradedCustomer <- upgradePaket customer choice paket
                    parsePersona newUpgradedCustomer

                    let changedItem = find (\item -> internetId item == choice) newUpgradedCustomer
                        extractItem :: Maybe Persona -> Persona
                        extractItem (Just a) = a
                        extractItem Nothing = UnknownItem

                    let extractedItem = extractItem changedItem
                
                    logMessage <-
                        if extractedItem == UnknownItem
                            then makeLogMessage extractedItem "ERR"
                            else makeLogMessage (extractedItem{paket = paket}) "UPP"

                    parseLogMessage logMessage
                    emptyPrompt <- prompt "Tekan Enter untuk melanjutkan" 
                    runProgram newUpgradedCustomer      
        "c" -> do
            putStrLn "Kamu Ingin Membayar Tagihan, silahkan ikuti langkah berikut "
            -- Masukkan InternetID
            putStr "Masukkan InternetID: "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            
            putStr "Masukkan jumlah tagihan: "
            hFlush stdout
            tagihan <- do
                    result <- runMaybeT maybeReadInt
                    case result of
                        (Just a) -> return 1
                        Nothing -> return 0
            newUpgradedCustomer <- bayarTagihan customer choice tagihan
            parsePersona newUpgradedCustomer

            let changedItem = find (\item -> internetId item == choice) newUpgradedCustomer
                extractItem :: Maybe Persona -> Persona
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            let extractedItem = extractItem changedItem

            logMessage <-
                if extractedItem == UnknownItem
                    then makeLogMessage extractedItem "ERR"
                    else makeLogMessage (extractedItem{tagihan = tagihan}) "TAG"

            parseLogMessage logMessage
            emptyPrompt <- prompt "Tekan Enter untuk melanjutkan"
            runProgram newUpgradedCustomer
        "d" -> do
            putStrLn "Kamu Ingin Melaporkan Gangguan, silahkan ikuti langkah berikut "
            -- Masukkan InternetID
            putStr "Masukkan InternetID: "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            
            putStr "Masukkan keluhan Anda: "
            hFlush stdout
            gangguan <- do
                    result <- runMaybeT maybeReadInt
                    case result of
                        (Just a) -> return 1
                        Nothing -> return 1
            newUpgradedCustomer <- laporGangguan customer choice gangguan
            parsePersona newUpgradedCustomer

            let changedItem = find (\item -> internetId item == choice) newUpgradedCustomer
                extractItem :: Maybe Persona -> Persona
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            let extractedItem = extractItem changedItem

            logMessage <-
                if extractedItem == UnknownItem
                    then makeLogMessage extractedItem "ERR"
                    else makeLogMessage (extractedItem{gangguan = gangguan}) "GGN"

            parseLogMessage logMessage
            emptyPrompt <- prompt "Tekan Enter untuk melanjutkan"
            runProgram newUpgradedCustomer
        "e" -> do
            putStrLn "\nKamu Ingin Menambahkan Data Pelanggan, Silahkan isi Data Dibawah ini: "
            nama <- prompt "Nama Pelanggan: "
            putStrLn "No KTP Pelanggan: "
            hFlush stdout
            noKtp <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            alamat <- prompt "Alamat Pelanggan: "
            putStrLn "Kecepatan Internet:"
            hFlush stdout
            kecepatan <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            paket <- prompt "Paket Internet:"
            newCustomer <- addNewCustomer customer nama noKtp alamat kecepatan paket
            parsePersona newCustomer
            logMessage <- makeLogMessage (last newCustomer) "NEW"
            parseLogMessage logMessage
            runProgram newCustomer
        "f" -> do
            putStrLn "Menutup Aplikasi..."
            putStrLn "Bye!"
        _ -> do
            putStrLn "Kata Kunci Salah, Silahkan masukkan kembali"
            runProgram customer

showAllData :: [Data.Persona] -> String
showAllData [] = replicate 96 '='
showAllData (item : rest) =
    "InternetID: " ++ show (Data.internetId item)
        ++ "\nNama: "
        ++ Data.nama item
        ++ "\nNomor KTP: "
        ++ show (Data.noKtp item)
        ++ "\nAlamat: "
        ++ Data.alamat item
        ++ "\nKecepatan Internet:"
        ++ show (Data.kecepatan item)
        ++ "\nPaket Internet:"
        ++ Data.paket item
        ++ "\nJmlh Gangguan:"
        ++ show (Data.gangguan item)
        ++ "\nJmlh Tagihan Lunas:"
        ++ show (Data.tagihan item)
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showAllData rest

showData :: [Persona] -> String
showData item = showDataFunc (length item) (take 2 item)
  where
    showDataFunc count [] = case count of
        0 -> "Data Customer Tidak Ditemukan\n" ++ replicate 96 '='
        1 -> "\n" ++ replicate 96 '='
        2 -> "\n" ++ replicate 96 '='
        _ -> "...and " ++ show (count - 2) ++ " more." ++ "\n" ++ replicate 96 '='
    showDataFunc count (item : rest) =
        "Internet ID: " ++ show (internetId item)
            ++ "\nNama: "
            ++ nama item
            ++ "\nNomor KTP: "
            ++ show (noKtp item)
            ++ "\nAlamat: "
            ++ alamat item
            ++ "\nKecepatan Internet:"
            ++ show (kecepatan item)
            ++ "\nPaket Internet:"
            ++ paket item
            ++ "\nJmlh Gangguan:"
            ++ show (gangguan item)
            ++ "\nJmlh Tagihan Lunas:"
            ++ show (tagihan item)
            ++ "\n"
            ++ replicate 29 '-'
            ++ "\n"
            ++ showDataFunc count rest


main :: IO ()
main = do
    customer <- fmap parseItem (readFile "log/Customer.log")
    runProgram customer