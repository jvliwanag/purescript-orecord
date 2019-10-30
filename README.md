# purescript-orecord

A data type for records with required and optional values.

NOTE: Documentation is WIP

# Example

# Usage for FFI

A common JavaScript API pattern is defining a function with inputs and outputs as objects having required and optional members.

Take for example a JavaScript SMS API with the following specifications:

`sendSMS(SMSRequest)`

### Request
| Key | Description | Type | Required |
| --- | --- | --- | --- |
| apiKey | Your API Key | String | Y |
| from | Name of sender | String | N |
| to | Number of receipient | String | Y |
| text | Message  | String | Y |
| ttl | Duration in seconds the delivery will be attempted | Int | N |

Following the specification, we split up the keys between required and optional and define an `ORecord`.

```purescript
-- src/SMS.purs

type SMSRequest =
  ORecord ( apiKey :: String
          , to :: String
          , text :: String
          )
          ( from :: String
          , ttl :: Int
          )
```

We can create an FFI code as follows:

```js
// src/SMS.js
var smslib = require('smslib');
exports.sendSMS = function (request) {
  function () {
    return smslib.sendSMS(request);
  }
}

```

```purescript
-- src/SMS.purs

...

foreign import sendSMS :: SMSRequest -> Effect Unit
```

### Response
| Key | Description | Type | Required |
| --- | --- | --- | --- |
| id | Message ID | String | Y |
| accepted | True if message was accepted | Boolean | Y |
