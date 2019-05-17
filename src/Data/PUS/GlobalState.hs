{-|
Module      : Data.PUS.GlobalState
Description : Represents the global state of the encoders
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

Contains the 'AppState' and 'GlobalState' types which encapsulate the 
complete state of the used RIO monad. The 'GlobalState' consists of 
the PUS Config and several TVars, which contain the transient application
state (or library state)
|-}
{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , RecordWildCards
    , NoImplicitPrelude
    , FlexibleInstances
    , MultiParamTypeClasses
#-}
module Data.PUS.GlobalState
    ( GlobalState
    , AppState
    , glsConfig
    , glsState
    , glsLogFunc
    , glsRaiseEvent
    , newGlobalState
    , nextADCount
    )
where


import           RIO                     hiding ( to
                                                , view
                                                )

import           UnliftIO.STM                   ( )

import           Data.PUS.Config
import           Data.PUS.PUSState
import           Data.PUS.Events


-- | The AppState is just a type alias
type AppState = TVar PUSState

-- | The 'GlobalState' contains the configuration, several TVars to 
-- transient state and some functions which must be provided by the
-- user of the library. Currently for logging and raising events
data GlobalState = GlobalState {
    glsConfig :: !Config
    , glsState :: !AppState

    , glsRaiseEvent :: Event -> IO ()
    , glsLogFunc :: !LogFunc
}

-- | Constructor for the global state. Takes a configuration, a 
-- logging function as specified by the RIO library and a raiseEvent 
-- function to report events to the application
newGlobalState ::
    Config
    -> LogFunc
    -> (Event -> IO ())
    -> IO GlobalState
newGlobalState cfg logErr raiseEvent = do
    st <- defaultPUSState
    tv <- newTVarIO st
    let state = GlobalState { glsConfig     = cfg
                            , glsState      = tv
                            , glsRaiseEvent = raiseEvent
                            , glsLogFunc    = logErr
                            }
    pure state

-- | returns the next counter value for TC transfer frames
-- in AD transmission mode 
nextADCount :: AppState -> STM Word8
nextADCount st = do
    state <- readTVar st
    let (newSt, cnt) = nextADCnt state
    writeTVar st newSt
    pure cnt

-- | Instance of the logging function for the global state
instance HasLogFunc GlobalState where
    logFuncL = lens glsLogFunc (\c lf -> c {glsLogFunc = lf})
