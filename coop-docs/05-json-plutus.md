# Reversible embedding from JSON to Plutus Data

Publishers would like to store and provide their Fact Statements in JSON format
([ECMA-404](https://www.ecma-international.org/publications-and-standards/standards/ecma-404/)),
but the Cardano Ledger ([Alonzo
CDDL](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/alonzo/test-suite/cddl-files/alonzo.cddl))
requires utxo datums to be serialized in the Plutus Data format
([PlutusCore.Data](https://github.com/input-output-hk/plutus/blob/master/plutus-core/plutus-core/src/PlutusCore/Data.hs)).

In the COOP framework, we have adopted the following reversible embedding from
JSON values to Plutus Data values. This embedding allows any JSON value to be
converted into a Plutus Data value and allows the converted value to be
converted back to the original JSON value. However, not all Plutus Data values
can be converted to JSON by the embedding -- this is fine because the intended
use for the embedding is to convert JSON to Plutus Data, publish the Plutus Data
on the Cardano blockchain, read the Plutus Data from the Cardano blockchain, and
convert it back to the original JSON.

A JSON value can be of any of the following:

- **Object** -- a collection of zero or more name-value pairs. Names must be
  strings, while values can be of any JSON type.
- **Array** -- a sequence of zero or more values.
- **Number** -- a floating-point number, excluding any numbers that cannot be
  represented using digits (e.g. Infinity and NaN).
- **String** -- a sequence of UTF-8 code points.
- **`true`**
- **`false`**
- **`null`**

A Plutus Data value can be any of the following:

- **Constructor** -- an integer-tagged sequence of values. This is intended to
  be used to represent sum types, which are types that provide multiple possible
  options for their values and use a different tag for each option. For example,
  the result of a fallible numeric  calculation can be represented as either a
  textual description of an error or a numeric correct result of the
  calculation.
- **Map** -- a collection of zero or more value-value pairs.
- **List** -- a sequence of zero or more values.
- **Integer** -- a whole number than can be zero (0), positive (1, 2, 3, ...),
  or negative (-1, -2, -3, ...).
- **Bytestring** -- a sequence of bytes.

A JSON value can be converted to a Plutus Data value as follows:

- A JSON **Object** is converted into a Plutus Data **Map**. For each name-value
  pair in the JSON Object, the name (a JSON String) is converted into a Plutus
  Data Bytestring and the value is converted into a corresponding Plutus Data
  value.
- A JSON **Array** is converted into a Plutus Data **List**. Each value in the
  JSON Array is converted into a corresponding Plutus Data value.
- A JSON **Number** is converted into either a Plutus Data **Integer** or a
  Plutus Data **Constructor**:
  - If the JSON Number can be safely converted into an integer without rounding,
    then it is converted into a Plutus Data Integer.
  - Otherwise, the JSON Number is converted into a Plutus Data Constructor
    tagged by the integer `3`. The JSON Number's significand is placed as a
    Plutus Data Integer into the first position of the Constructor, and the JSON
    Number's base-10 exponent is place as a Plutus Data Integer into the second
    position.
- A JSON **String** is converted into a Plutus Data **Bytestring** by encoding
  the sequence of UTF-8 code points into a sequence of bytes.
- A JSON **`true`** value is converted into a Plutus Data **Constructor** tagged
  by the integer **`1`**, with an empty sequence of values.
- A JSON **`false`** value is converted into a Plutus Data **Constructor**
  tagged by the integer **`0`**, with an empty sequence of values.
- A JSON **`null`** value is converted into a Plutus Data **Constructor** tagged
  by the integer **`2`**, with an empty sequence of values.

A Plutus Data value that was derived via the embedding from a JSON value can
always be converted back to that JSON value:

- A Plutus Data **Constructor** is converted into a corresponding JSON value
  based on its integer tag:
  - If the tag is **`0`**, then it is converted into a JSON **`false`** value.
  - If the tag is **`1`**, then it is converted into a JSON **`true`** value.
  - If the tag is **`2`**, then it is converted into a JSON **`null`** value.
  - If the tag is **`3`**, then a JSON **Number** is constructed using the
    significand in the Constructor's first position and the base-10 exponent in
    the Constructor's second position.
- A Plutus Data **Map** is converted into a JSON **Object**. For each
  value-value pair in the Plutus Data Map, the first value is converted into a
  JSON String and the second value is converted into a corresponding JSON value.
- A Plutus Data **List** is converted into a JSON **Array**. Each value in the
  Plutus Data List is converted into a corresponding JSON value.
- A Plutus Data **Integer** is converted into a JSON **Number**, with the
  base-10 exponent set to 0.
- A Plutus Data **Bytestring** is converted into a JSON **String** by decoding
  the sequence of bytes into a sequence of UTF-8 code points.

The conversion into JSON will fail for the following Plutus Data values:

- A Plutus Data **Constructor** tagged by a different integer than 0, 1, 2, or
  3.
- A Plutus Data **Constructor** tagged by the integer 3 that does not contain
  exactly two Plutus Data Integers in its sequence of values.
- A Plutus Data **Map** that contains a value-value pair whose first value
  cannot be converted into a JSON String.
- A Plutus Data **Bytestring** that cannot be decoded into a sequence of UTF-8
  code points.

