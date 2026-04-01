![build](https://github.com/uhppoted/uhppoted-lib-ada/workflows/build/badge.svg)

# uhppoted-lib-ada

Standalone ADA library for the UHPPOTE access controllers.

A basic example CLI illustrating the use of the library can be found in the
[examples](https://github.com/uhppoted/uhppoted-lib-ada/tree/main/examples/cli) folder.


## Release Notes

**In Development**


## Installation

## Development

Requirements:
- gnat 15.1.2+
- Alire 2.1.0+

### Building from source

Assuming you have `Alire`, `gnat 15.12+` and `make` installed:

```
git clone https://github.com/uhppoted/uhppoted-lib-ada/uhppoted-lib-ada.git
cd uhppoted-lib-ada
make build-all
```

If you prefer building manually:
```
git clone https://github.com/uhppoted/uhppoted-lib-ada/uhppoted-lib-ada.git
cd uhppoted-lib-ada
cd lib && alr build
cd ../examples/cli && alr build
cd ../integration-tests && alr build
```

## API

The API is documented in the [API.md](API.md) file - for convenience the available functions are listed below:

- [`Find_Controllers`](API.md#find_controllers)
- [`Get_Controller`](API.md#get_controller)
- [`Set_IPv4`](API.md#set_ipv4)
- [`Get_Time`](API.md#get_time)
- [`Set_Time`](API.md#set_time)
- [`Get_Listener`](API.md#get_listener)
- [`Set_Listener`](API.md#set_listener)
- [`Get_Status`](API.md#get_status)
- [`Get_Door`](API.md#get_door)
- [`Set_Door`](API.md#set_door)
- [`Set_Door_Passcodes`](API.md#set_door_passcodes)
- [`Open_Door`](API.md#open_door)
- [`Get_Cards`](API.md#get_cards)


## License

[MIT](https://github.com/uhppoted/uhpoted-lib-ada/blob/master/LICENSE). 

