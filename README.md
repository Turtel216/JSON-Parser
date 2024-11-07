# JSON-Parser

### A JSON Parser written in haskell

## Overview

This project is a simple JSON parser implemented in Haskell. It parses JSON data into a ``JsonValue`` data structure and includes custom parsing combinators. This implementation is basic but modular, allowing for easy extension and improvement. It is based on the video tutorial [JSON Parser 100% From Scratch in Haskell (only 111 lines)](https://www.youtube.com/watch?v=N9RUqGYuGfw&t=2973s). Every extension to this tutorial will be listed in the [To-Do](#todo) section

## Features

The parser currently supports:

- **Null** values: Parses ``null``.
- **Boolean** values: Parses ``true`` and ``false``.
- **Numbers**: Parses integer values.
- **Strings**: Parses basic string values without escape sequences.
- **Arrays**: Parses arrays of JSON values.
- **Objects**: Parses JSON objects with string keys.

The parser is implemented using a custom ``Parser`` type that follows functional parsing techniques, including ``Applicative``, ``Functor``, and ``Alternative`` instances, enabling composition of complex parsers from simpler ones.

<a name="todo"></a>

## To-Do

The following features are yet to be implemented:

- [x] Add **Testing**
- [ ] **Floating Point Numbers**: Currently, only integers are supported in ``JsonNumber``. Parsing for floating-point numbers has not been implemented.
- [ ] **Error Reporting**: The parser only returns ``Nothing`` for errors without any indication of what went wrong. Adding detailed error reporting would make the parser more user-friendly and easier to debug.
- [ ] **String Escape Sequences**: The ``jsonString`` parser currently does not handle escape sequences (e.g., \", \\, \n). This needs to be added for full JSON compliance.
