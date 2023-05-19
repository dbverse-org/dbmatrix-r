Class diagram
================

## Class diagram and inheritance

``` mermaid
classDiagram
class GiottoDB {[VIRTUAL]}
class backendInfo {
  +driver_call: character
  +db_path: character
  +hash: character
  -reconnectBackend()
}
class dbData {
  [VIRTUAL]
  +data: tbl_Pool
  +hash: character
  +remote_name: character
  -reconnect()
}
class dbMatrix {
  +dim_names: list
  +dims: integer
  -Arith()
  -Ops()
  -Math()
  -col*()
  -row*()
  -t()
}
class dbDataFrame 
class dbSpatProxyData {
  [VIRTUAL]
  +attribute: dbDataFrame
  +extent: SpatExtent
  -spatialquery()
}
class dbPolygonProxy {
  +n_poly: integer
}
class dbPointsProxy {
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
