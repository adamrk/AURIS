{-# LANGUAGE OverloadedStrings
    , BangPatterns
    , TemplateHaskell
    , NoImplicitPrelude
    , RecordWildCards
#-}
module GUI.MainWindow
  ( MainWindowFluid(..)
  , MainWindow(..)
  , TMPacketTabFluid(..)
  , mwWindow
  , mwOpenFile
  , mwSaveFile
  , mwProgress
  , mwTabs
  , mwTMPTab
  , tmpTabButtonAdd
  , tmpTable
  , tmpModel
  , createTMPTab
  , mwAddPUSPacket
  )
where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Graphics.UI.FLTK.LowLevel.FLTKHS

import           Model.PUSPacketModel
import           GUI.PUSPacketTable
import           GUI.Colors

import           Data.PUS.ExtractedDU
import           Data.PUS.PUSPacket



data TMPacketTabFluid = TMPacketTabFluid {
    _tmpfTabButtonAdd :: Ref Button
    , _tmpfTabGroup :: Ref Group
}

data TMPacketTab = TMPacketTab {
    _tmpTabButtonAdd :: Ref Button
    , _tmpTable :: Ref TableRow
    , _tmpModel :: PUSPacketModel
}
makeLenses ''TMPacketTab

tmpTabAddRow :: TMPacketTab -> ExtractedDU PUSPacket -> IO ()
tmpTabAddRow tab pkt = do
  GUI.PUSPacketTable.addRow (tab ^. tmpTable) (tab ^. tmpModel) pkt


createTMPTab :: TMPacketTabFluid -> IO TMPacketTab
createTMPTab TMPacketTabFluid {..} = do
  model <- createPUSPacketModel
  table <- setupTable _tmpfTabGroup model
  mcsGroupSetColor _tmpfTabGroup

  pure $ TMPacketTab _tmpfTabButtonAdd table model


data MainWindowFluid = MainWindowFluid {
    _mfWindow :: Ref Window
    , _mfOpenFile :: Ref MenuItemBase
    , _mfSaveFile :: Ref MenuItemBase
    , _mfProgress :: Ref Progress
    , _mfTabs :: Ref Tabs
    , _mfTMPTab :: TMPacketTabFluid
    }
makeLenses ''MainWindowFluid


data MainWindow = MainWindow {
    _mwWindow :: Ref Window
    , _mwOpenFile :: Ref MenuItemBase
    , _mwSaveFile :: Ref MenuItemBase
    , _mwProgress :: Ref Progress
    , _mwTabs :: Ref Tabs
    , _mwTMPTab :: TMPacketTab
    }
makeLenses ''MainWindow


mwAddPUSPacket :: MainWindow -> ExtractedDU PUSPacket -> IO ()
mwAddPUSPacket window pkt = do
    tmpTabAddRow (window ^. mwTMPTab) pkt