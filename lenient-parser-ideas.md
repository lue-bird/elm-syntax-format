  - ```elm
    function parameters : Type = result
    ```
    →
    ```elm
    function : Type
    function parameters =
        result
    ```
  - ```elm
    { field0 value, field1 value }
    ```
    →
    ```elm
    { field0 = value, field1 = value }
    ```
    or
    ```elm
    { field0 : value, field1 : value }
    ```
  - ```elm
    { field0, field1 }
    ```
    →
    ```elm
    { field0 = field0, field1 = field1 }
    ```
  - ```elm
    f | g | h
    ```
    →
    ```elm
    f |> g |> h
    ```
  - ```elm
    3 |> String.toInt
    of case
        Nothing ->
            0
          
        Just n ->
            n
    ```
    →
    ```elm
    case 3 |> String.toInt of
        Nothing ->
            0
          
        Just n ->
            n
    ```
