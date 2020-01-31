import Control.Exception

canICatch :: Exception e => e
          -> IO (Either SomeException ())
canICatch e =
  try $ throwIO e
