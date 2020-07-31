0.8.7
-----

- Support for servant-0.18
   - Hint for migration: if you get errors about `ambiguous type
     variable ‘context0’ arising from a use of ‘mock’`, try calling
     `mock` with explicit type information about the context: `mock
     api (Proxy @'[])`, not `mock api Proxy`.

0.8.6
-----

- Support for servant-0.17

0.8.5
-----

- Support for servant-0.15

0.8.4
-----

- Support for servant-0.13

0.8.3
-----

- Support for servant-0.12
   - Add `HasMock (Description d :> api)` and `HasMock (Summary d :> api)`
     instances

0.8.2
-----

- Support for servant-0.11
    - Add `HasMock EmptyAPI` instance

0.8.1.2
-------

- Support for servant-0.10
- Fix test with hspec-wai-0.8

0.8.1.1
-------

- Fix tests compiling with servant-0.9
