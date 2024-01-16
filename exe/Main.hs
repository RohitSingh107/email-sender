{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Configuration.Dotenv        (defaultConfig, loadFile)
import           Data.MIME.Types             (defaultmtd, guessType)
import qualified Data.Text                   as T
import           Data.Text.Internal.Builder  (fromText, toLazyText)
import qualified Data.Text.Internal.Lazy     as LT
import           Data.Text.IO                (hGetContents, hGetLine)
import           Network.HaskellNet.Auth     (AuthType (LOGIN), UserName)
import           Network.HaskellNet.SMTP     (authenticate, closeSMTP, sendMail)
import           Network.HaskellNet.SMTP.SSL (connectSMTPSSL)
import           Network.Mail.Mime           (Address (Address, addressEmail, addressName),
                                              Mail (Mail, mailBcc, mailCc, mailFrom, mailHeaders, mailParts, mailTo),
                                              addAttachments, plainPart)
-- import           System.Directory
import           System.Environment          (getEnv, getArgs)
import           System.Exit                 (die)
import           System.IO                   (BufferMode (BlockBuffering),
                                              IOMode (ReadMode), hClose,
                                              hSetBuffering, openFile)
import System.Console.CmdArgs (cmdArgs, Data, Typeable)

from :: Address
from = "rohitsingh.mait@gmail.com"

server :: String
server = "smtp.gmail.com"

username :: UserName
username = "rohitsingh.mait@gmail.com"

authType :: AuthType
authType = LOGIN

emailsDirectory :: String
emailsDirectory = "/home/rohits/mydata/code/git_repos/email-sender/emails/"

attachmentDirectory :: T.Text
attachmentDirectory = "/home/rohits/mydata/code/git_repos/email-sender/attachments/"

formatAddress :: T.Text -> Address
formatAddress s = Address {addressEmail = s, addressName = Nothing}

formatAttachment :: T.Text -> (T.Text, FilePath)
formatAttachment file = (mimeType, path)
  where
    mimeType :: T.Text
    mimeType = case guessType defaultmtd False (T.unpack file) of
      (Nothing, _) -> "text/plain" -- If unknown assuming it's plain text
      (Just t, _)  -> T.pack t
    path :: FilePath
    path = T.unpack $ attachmentDirectory <> file

sendEmail :: [Address] -> T.Text -> LT.Text -> [(T.Text, FilePath)] -> IO ()
sendEmail to subject body attachments = do
  password <- getEnv "PASS"
  let newMail =
        Mail
          { mailFrom = from,
            mailTo = to,
            mailCc = [],
            mailBcc = [from],
            mailHeaders = [("Subject", subject)],
            mailParts = [[plainPart body]]
          }
  -- print attachments
  newMail' <- addAttachments attachments newMail
  conn <- connectSMTPSSL server
  authSucceed <- authenticate authType username password conn

  if authSucceed
    then sendMail newMail' conn
    else -- >> putStrLn "Email sent successfully :)"
      die "Authentication failed."

  closeSMTP conn

processEmail :: String -> IO ()
processEmail email = do
  fh <- openFile (emailsDirectory <> email <> ".txt") ReadMode
  hSetBuffering fh (BlockBuffering Nothing)

  addressLine <- hGetLine fh
  let addresses = map formatAddress $ T.words addressLine
  -- putStrLn $ "Address: " <> address

  attachmentsName <- hGetLine fh
  let attachments = map formatAttachment $ T.words attachmentsName
  -- putStrLn $ "Attachment path: " <> attachmentsName

  subject <- hGetLine fh
  -- putStrLn $ "Subject is: " <> subject

  content <- hGetContents fh
  -- print content

  sendEmail addresses subject (toLazyText $ fromText content) attachments

  hClose fh

-- print $ "Email: " <> email <> " sent."

emailsToSend :: [String]
emailsToSend = ["juspay"]


data PathArgs  = PathArgs {
  attdir :: !String,
  emaildir :: !String
} deriving (Show, Data, Typeable)

pathArgs :: PathArgs
pathArgs = PathArgs {
    attdir = T.unpack attachmentDirectory
  , emaildir = emailsDirectory
}

main :: IO ()
main = do
  loadFile defaultConfig

  args <- cmdArgs pathArgs
  print args

  -- mapM_ processEmail emailsToSend
  putStrLn "All emails sent successfully!"
