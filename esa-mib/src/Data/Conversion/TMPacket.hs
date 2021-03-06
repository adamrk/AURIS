module Data.Conversion.TMPacket
  ( convertPacket
  , convertPackets
  , charToPIDEvent
  , picSeachIndexFromPIC
  , generateVPDLookup
  )
where

import           RIO
import qualified RIO.HashMap                   as HM
import qualified RIO.List                      as L
import qualified RIO.Text                      as T
import qualified RIO.Vector                    as V
import           Data.Text.Short                ( ShortText )
import qualified Data.Text.Short               as ST
import           Data.HashTable.ST.Basic        ( IHashTable )
import qualified Data.HashTable.ST.Basic       as HT

import           Data.MIB.PID
import           Data.MIB.TPCF
import           Data.MIB.PLF
import           Data.MIB.PIC
import           Data.MIB.VPD
import           Data.MIB.Types

import           Data.TM.TMPacketDef
import           Data.TM.TMParameterDef
import           Data.TM.PIVals

import           General.Types
import           General.PUSTypes
import           General.TriState
import           General.APID
import           General.Time
import           General.TimeSpan

import           Data.Conversion.Types




-- | convert packet information from a SCOS-2000 compatible MIB into a 
-- 'TMPacketDef'. If there is a non-recoverable error, returns @Left error@
-- else a @Right (Warnings, [TMPacketDef])@
convertPackets
  :: HashMap Word32 TPCFentry
  -> Vector PLFentry
  -> IHashTable Int VarParams
  -> IHashTable ShortText TMParameterDef
  -> Vector PIDentry
  -> Either Text (Warnings, [TMPacketDef])
convertPackets tpcfs plfs vpds paramHT =
  handleTriState . map (convertPacket tpcfs plfs vpds paramHT) . V.toList


-- | convert a single packet from a SCOS-2000 compatible MIB into a 
-- 'TMPacketDef'
convertPacket
  :: HashMap Word32 TPCFentry
  -> Vector PLFentry
  -> IHashTable Int VarParams
  -> IHashTable ShortText TMParameterDef
  -> PIDentry
  -> TriState Text Text (Warnings, TMPacketDef)
convertPacket tpcfs plfs vpds paramHT pid@PIDentry {..} =
  case if _pidTPSD == -1 then getFixedParams else getVariableParams pid vpds of
    TError err -> TError err
    TWarn  err -> TWarn err
    TOk (warnings, params) ->
      createPacket pid (HM.lookup _pidSPID tpcfs) warnings params

 where
  createPacket PIDentry {..} tpcf warnings params =
    let name = case tpcf of
          Just TPCFentry {..} -> _tpcfName
          Nothing             -> ""
    in  TOk
          ( warnings
          , TMPacketDef
            { _tmpdSPID    = SPID _pidSPID
            , _tmpdName    = name
            , _tmpdDescr   = _pidDescr
            , _tmpdType    = mkPUSType (fromIntegral _pidType)
            , _tmpdSubType = mkPUSSubType (fromIntegral _pidSubType)
            , _tmpdApid    = APID (fromIntegral _pidAPID)
            , _tmpdPI1Val  = _pidP1Val
            , _tmpdPI2Val  = _pidP2Val
            , _tmpdUnit    = _pidUnit
            , _tmpdTime    = getPidTime pid
            , _tmpdInter   = toSpan _pidInter
            , _tmpdValid   = charDefaultToBool _pidValid
            , _tmpdCheck   = getDefaultInt _pidCheck /= 0
            , _tmpdEvent = charToPIDEvent (getDefaultChar _pidEvent) _pidEventID
            , _tmpdParams  = params
            }
          )

  toSpan :: Maybe Int -> Maybe (TimeSpn MilliSeconds)
  toSpan = fmap (mkTimeSpan MilliSeconds . fromIntegral)



  getFixedParams =
    let (errs, pls) =
            partitionEithers
              . map (convertPacketLocation paramHT)
              . L.sort
              . filter (\x -> _pidSPID == _plfSPID x)
              . toList
              $ plfs
    in  if null errs
          then TOk (Nothing, TMFixedParams (V.fromList pls))
          else TError (T.concat (L.intersperse ("\n" :: Text) errs))






convertPacketLocation
  :: IHashTable ShortText TMParameterDef
  -> PLFentry
  -> Either Text TMParamLocation
convertPacketLocation ht plf@PLFentry {..} = case HT.ilookup ht _plfName of
  Just x -> Right TMParamLocation
    { _tmplName      = _plfName
    , _tmplOffset    = mkOffset (ByteOffset _plfOffBy) (BitOffset _plfOffBi)
    , _tmplTime      = fromMilli (fromIntegral (getDefaultInt _plfTime)) True
    , _tmplSuperComm = convertSuperComm plf
    , _tmplParam     = x
    }
  Nothing ->
    Left
      $  "PLF: Parameter "
      <> ST.toText _plfName
      <> " defined in plf.dat not found"


convertSuperComm :: PLFentry -> Maybe SuperCommutated
convertSuperComm PLFentry {..} = case (_plfNbOcc, _plfLgOcc, _plfTdOcc) of
  (Just n, Just lg, Just td) -> Just SuperCommutated
    { _scNbOcc = n
    , _scLgOcc = mkBitSize lg
    , _scTdOcc = mkTimeSpan MilliSeconds (fromIntegral td)
    }
  _ -> Nothing


getVariableParams
  :: PIDentry
  -> IHashTable Int VarParams
  -> TriState Text Text (Maybe Text, TMPacketParams)
getVariableParams PIDentry {..} vpds=

  case HT.ilookup vpds _pidTPSD of 
    Nothing -> TError $ "Could not find TPSD " <> T.pack (show _pidTPSD) <> " in VPD table"
    Just struct -> 
      TOk (Nothing , TMVariableParams { _tmvpTPSD    = _pidTPSD
                                      , _tmvpDfhSize = fromIntegral _pidDfhSize
                                      , _tmvpParams  = struct 
                                      }
          )



generateVPDLookup :: Vector VPDentry 
  -> IHashTable ShortText TMParameterDef 
  -> Either Text (IHashTable Int VarParams)
generateVPDLookup vpds parDefs = 
  let tpsds = map (func . L.sortBy c) . L.groupBy tpsdComp . toList $ vpds
      c v1 v2 = compare (vpdPos v1) (vpdPos v2)
      tpsdComp v1 v2 = vpdTpsd v1 == vpdTpsd v2 
      func [] = (-1, [])
      func l@(x:_) = (vpdTpsd x, l)

      converted = map conv tpsds
      conv (tpsd, vps) = (tpsd, partitionEithers (map (convertVPD parDefs) vps))
      errs = concatMap (fst . snd) converted 
      errorMsgs = "Error on converting VPDs: " <> T.intercalate "\n" errs

      final = HT.fromList $ map conv2 converted 
      conv2 (!tpsd, (_, !r)) = (tpsd, generateVarParams r)
  in
  if not (null errs) 
    then Left errorMsgs
    else Right final




generateVarParams :: [TMVarParamDef] -> VarParams
generateVarParams = go
 where
  go :: [TMVarParamDef] -> VarParams
  go [] = VarParamsEmpty
  go (v@TMVarParamDef { _tmvpNat = TMVarNothing } : vs) = VarNormal v (go vs)
  go (v@TMVarParamDef { _tmvpNat = TMVarGroup n } : vs) =
    let n' = fromIntegral n in VarGroup v (go (take n' vs)) (go (drop n' vs))
  go (TMVarParamDef { _tmvpNat = TMVarFixedRep rep n } : vs) =
    let n' = fromIntegral n in VarFixed rep (go (take n' vs)) (go (drop n' vs))
  go (v@TMVarParamDef { _tmvpNat = TMVarChoice } : _ ) = VarChoice v
  go (v@TMVarParamDef { _tmvpNat = TMVarPidRef } : vs) = VarPidRef v (go vs)


convertVPD
  :: IHashTable ShortText TMParameterDef
  -> VPDentry
  -> Either Text TMVarParamDef
convertVPD parDefs VPDentry {..} = case HT.ilookup parDefs vpdName of
  Nothing ->
    Left
      $  "Cannot find requested parameter "
      <> ST.toText vpdName
      <> " in PCF table"
  Just parDef -> Right TMVarParamDef
    { _tmvpName     = vpdName
    , _tmvpNat      = modifier vpdGrpSize vpdFixRep vpdChoice vpdPidRef
    , _tmvpDisDesc  = vpdDisDesc
    , _tmvpDisp     = vpdWidth > 0
    , _tmvpJustify  = align vpdJustify
    , _tmvpNewline  = getDefaultChar vpdNewLine == 'Y'
    , _tmvpDispCols = dispFormat vpdDChar
    , _tmvpRadix    = radix vpdForm
    , _tmvpOffset   = BitOffset (getDefaultInt vpdOffset)
    , _tmvpParam    = parDef
    }
 where
  modifier (DefaultTo grp) (DefaultTo fixed) (CharDefaultTo choice) (CharDefaultTo pidref)
    | fixed > 0
    = TMVarFixedRep (fromIntegral fixed) (fromIntegral grp)
    | grp > 0
    = TMVarGroup (fromIntegral grp)
    | choice == 'Y'
    = TMVarChoice
    | pidref == 'Y'
    = TMVarPidRef
    | otherwise
    = TMVarNothing

  align (CharDefaultTo 'R') = TMVarRight
  align (CharDefaultTo 'C') = TMVarCenter
  align _                   = TMVarLeft

  dispFormat (DefaultTo 1) = TMVarDispNameVal
  dispFormat (DefaultTo 2) = TMVarDispNameValDesc
  dispFormat _             = TMVarDispValue

  radix (CharDefaultTo 'N') = TMVarNormal
  radix (CharDefaultTo 'D') = TMVarDecimal
  radix (CharDefaultTo 'H') = TMVarHex
  radix (CharDefaultTo 'O') = TMVarOctal
  radix (CharDefaultTo 'B') = TMVarBinary
  radix _                   = TMVarNormal

charToPIDEvent :: Char -> ShortText -> PIDEvent
charToPIDEvent 'N' _    = PIDNo
charToPIDEvent 'I' evid = PIDInfo evid
charToPIDEvent 'W' evid = PIDWarning evid
charToPIDEvent 'A' evid = PIDAlarm evid
charToPIDEvent _   _    = PIDNo



picVecToCriteria :: Vector PICentry -> Vector PacketIDCriteria
picVecToCriteria = V.map conv
 where
  conv pic@PICentry {..} = PacketIDCriteria
    { _pidcAPID    = APID <$> _picApid
    , _pidcType    = mkPUSType _picType
    , _pidcSubType = mkPUSSubType _picSubType
    , _pidcPIs     = convertPIC pic
    }


picSeachIndexFromPIC :: Vector PICentry -> PICSearchIndex
picSeachIndexFromPIC pics =
  let p = picVecToCriteria pics in mkPICSearchIndex p


convertPIC :: PICentry -> TMPIDefs
convertPIC PICentry {..} =
  let
    pi1 = if _picPI1Off == -1
      then Nothing
      else
        Just
          (TMPIDef (mkByteOffset _picPI1Off)
                   (mkBitSize (fromIntegral _picPI1Width))
          )
    pi2 = if _picPI2Off == -1
      then Nothing
      else
        Just
          (TMPIDef (mkByteOffset _picPI2Off)
                   (mkBitSize (fromIntegral _picPI2Width))
          )
  in
    TMPIDefs pi1 pi2


