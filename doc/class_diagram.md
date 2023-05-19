Class diagram
================

## Class diagram and inheritance

``` mermaid
classDiagram
class GiottoDB {[VIRTUAL]}
class backendInfo {
  +driver_call: &#39;character&#39;,
  +db_path: &#39;character&#39;,
  +hash = &#39;character&#39;
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

GiottoDB --&gt; dbData
GiottoDB --&gt; backendInfo
dbData --&gt; dbDataFrame
dbData --&gt; dbMatrix
dbData --&gt; dbSpatProxyData
dbSpatProxyData --&gt; dbPolygonProxy
dbSpatProxyData --&gt; dbPointsProxy
```
