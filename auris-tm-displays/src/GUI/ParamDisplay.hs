module GUI.ParamDisplay
  ( ParamDisplay(..)
  , paramDispInsertValues
  , paramDispAddParameterDef
  )
where


import           RIO

import           Data.TM.Parameter

import           GUI.Graph

import           GUI.NameDescrTable             ( TableValue )


data ParamDisplay =
  GraphDisplay GraphWidget
  | ANDDisplay


paramDispInsertValues :: ParamDisplay -> Vector TMParameter -> IO ()
paramDispInsertValues (GraphDisplay gw) values =
  graphInsertParamValue gw values
paramDispInsertValues ANDDisplay _values = return ()


paramDispAddParameterDef :: ParamDisplay -> Vector TableValue -> IO ()
paramDispAddParameterDef (GraphDisplay gw) values =
  addParamFromSelector gw values
paramDispAddParameterDef ANDDisplay _values = return ()

