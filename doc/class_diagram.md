Class diagram
================

## Class diagram and inheritance

``` mermaid
classDiagram
class GiottoDB {[VIRTUAL]}
class backendInfo {
  +driver_call: character,
  +db_path: character
  +hash = character
}
class dbData {
  [VIRTUAL]
  
  +data: ANY
  +hash: character
  +remote_name: character
}
class dbMatrix {
  +data: ANY
  +hash: character
  +remote_name: character
  +dim_names: list
  +dims: integer
}
class dbDataFrame {
  +data: ANY
  +hash: character
  +remote_name: character
}
class dbSpatProxyData {
  [VIRTUAL]
  
  +data: ANY
  +hash: character
  +remote_name: character
  +attribute: dbDataFrame
  +extent: SpatExtent
}
class dbPolygonProxy {
  +data: ANY
  +hash: character
  +remote_name: character
  +attribute: dbDataFrame
  +extent: SpatExtent
  +n_poly: integer
}
class dbPointsProxy {
  +data: ANY
  +hash: character
  +remote_name: character
  +attribute: dbDataFrame
  +extent: SpatExtent
  +n_point: integer
}

GiottoDB --> dbData
GiottoDB --> backendInfo
dbData --> dbDataFrame
dbData --> dbMatrix
dbData --> dbSpatProxyData
dbSpatProxyData --> dbPolygonProxy
dbSpatProxyData --> dbPointsProxy
```
