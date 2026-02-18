# TODO

- [ ] format
- [ ] lint
- [ ] fix _Library_Interface_ in .gpr file
```
   for Library_Interface use (
      "Uhppoted",
      "Uhppoted.Lib",
      "Uhppoted.Lib.Types",
      "Uhppoted.Lib.Decode"
   );
```
- [ ] integration tests
    - [ ] cleanup listen logic
    - [ ] `test_suite.adb:8:21: warning: use of an anonymous access type allocator [-gnatw_a]`

- [x] find-controllers
    - [ ] timeout parameter

- [ ] get-controller
    - [x] controller ID
    - [ ] controller struct
    - [ ] CLI
    - [ ] integration test
    - [ ] timeout parameter
    - [ ] validate controller ID in response

- [ ] UDP
    - [ ] broadcast timeout parameter
    - [ ] send timeout parameter
    - [ ] sendto
    - [ ] controlled types for sockets/selectors

- [ ] CLI
    - [ ] command line args
    - [ ] handle exceptions
    - [ ] pretty print

- https://stackoverflow.com/questions/67969309/ada-customise-image
- http://www.ada-auth.org/standards/22rm/html/rm-4-10.html
