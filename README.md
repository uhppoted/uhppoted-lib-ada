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

**Notes:**
1. Building on MacOS Tahoe generates the following warning:
```
`clang: warning: overriding deployment version from '16.0' to '26.0' [-Woverriding-deployment-version]`
```

This is known bug in the GNAT FSF builds which mistakenly interprets the OS version for Tahoe (v26.x) as version 16.0:
   - https://forum.ada-lang.io/t/gnat-toolchain-now-broken-after-os-upgrade-to-macos-26-tahoe-arm64/3843/8
   - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=120645
   - https://github.com/alire-project/GNAT-FSF-builds/issues/94


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
- [`Get_Card`](API.md#get_card)
- [`Get_Card_At_Index`](API.md#get_card_at_index)
- [`Put_Card`](API.md#put_card)
- [`Delete_Card`](API.md#delete_card)
- [`Delete_All_Cards`](API.md#delete_all_cards)
- [`Get_Event`](API.md#get_event)
- [`Get_Event_Index`](API.md#get_event_index)
- [`Set_Event_Index`](API.md#set_event_index)
- [`Record_Special_Events`](API.md#record_special_events)


## License

[MIT](https://github.com/uhppoted/uhpoted-lib-ada/blob/master/LICENSE). 

