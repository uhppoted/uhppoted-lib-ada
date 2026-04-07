# API
- [`Find_Controllers`](#find_controllers)
- [`Get_Controller`](#get_controller)
- [`Set_IPv4`](#set_ipv4)
- [`Get_Time`](#get_time)
- [`Set_Time`](#set_time)
- [`Get_Listener`](#get_listener)
- [`Set_Listener`](#set_listener)
- [`Get_Status`](#get_status)
- [`Get_Door`](#get_door)
- [`Set_Door`](#set_door)
- [`Set_Door_Passcodes`](#set_door_passcodes)
- [`Open_Door`](#open_door)
- [`Get_Cards`](#get_cards)
- [`Get_Card`](#get_card)
- [`Get_Card_At_Index`](#get_card_at_index)

---
Invoking an API function requires an instance of the `UHPPOTE` struct initialised with the information required
to access a controller:

```
with Uhppoted.Lib;
...
   U : constant Uhppoted.Lib.UHPPOTE := (
      Bind_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Any_Inet_Addr,
         Port => 0),

      Broadcast_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("192.168.1.255"),
         Port => 60000),

      Listen_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Any_Inet_Addr,
         Port => 60001),

      Debug => True);
...
where:

Bind_Addr        IPv4 address to which to bind the UDP socket
Broadcast_Addr   IPv4 address:port for broadcast UDP packets
Listen_Addr      IPv4 address:port for events from controller
Debug            Displays the controller requests/responses if true.
```

e.g.:
```
with GNAT.Sockets;
with Uhppoted.Lib;
...
   U : constant Uhppoted.Lib.UHPPOTE := (
      Bind_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Any_Inet_Addr,
         Port => 0),

      Broadcast_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("192.168.1.255"),
         Port => 60000),

      Listen_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Any_Inet_Addr,
         Port => 60001),

      Debug => True);
...
    controllers := Uhppoted.Lib.Find_Controllers(U);
...
```

All API functions other than `Find_Controllers` and `Listen` have two variants, taking either an `Unsigned_32` controller ID or a `Controller`
record as an argument, e.g.:

- ```function Get_Controller (U : Uhppoted.Lib.UHPPOTE, C : Unsigned_32; Timeout : Duration) return Uhppoted.Lib.Controller_Record;```
- ```function Get_Controller (U : Uhppoted.Lib.UHPPOTE, C : Controller;  Timeout : Duration) return Uhppoted.Lib.Controller_Record;```

where:

- `C : Unsigned_32` is the controller serial number
- `C : Controller` is a record with:
   ```
   type Controller is record
      Controller : Interfaces.Unsigned_32;
      DestAddr   : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
      Protocol   : Protocol_Type := Default;
   end record;

   - Controller is the controller serial number
   - DestAddr   is the controller IPv4 address:port
   - Protocol   is Default, Connected_UDP or TCP
   ```

Notes:
1. The variant with `C : Unsigned_32` uses UDP broadcast i.e. can only be used with controllers on the local LAN.
2. `DestAddr := No_Sock_Addr` uses UDP broadcast i.e. can only be used with controllers on the local LAN.
3. `DestAddr := <valid IPv4 address:port> with Default or Connected_UDP` uses UDP _send-to_ i.e. not restricted to controllers on the local LAN.
3. `DestAddr := <valid IPv4 address:port> with TCP` uses TCP/IP i.e. not restricted to controllers on the local LAN.

### Notes


## Functions

### `Find_Controllers`

**Find_Controllers** retrieves a list of all UHPPOTE controllers accessible via UDP broadcast on the local LAN.

```
function Find_Controllers (U : Uhppoted.Lib.UHPPOTE; Timeout : Duration) return Uhppoted.Lib.Controller_Record_List;

where:
- U   UHPPOTE   UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc
- Timeout  Duration     Operation timeout (defaults to 2.5s).
```
Returns a list of `Controller_Record`:
```
   type Controller_Record is record
      ID       : Unsigned_32;
      Address  : IPv4;
      Netmask  : IPv4;
      Gateway  : IPv4;
      MAC      : MAC_Address;
      Firmware : Version;
      Date     : DateOnly;
   end record;
```

### `Get_Controller`

**Get_Controller** retrieves the controller information for a single UHPPOTE controller.

```
function Get_Controller (U       : UHPPOTE;
                         C       : Unsigned_32; 
                         Timeout : Duration) return Controller_Record;

function Get_Controller (U       : Uhppoted.Lib.UHPPOTE;
                         C       : Controller;
                         Timeout : Duration) return Controller_Record;

where:
- U        UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C        Unsigned_32     Controller serial number.
- C        Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
```

Returns a `Controller_Record`:
```
   type Controller_Record is record
      ID       : Unsigned_32;
      Address  : IPv4;
      Netmask  : IPv4;
      Gateway  : IPv4;
      MAC      : MAC_Address;
      Firmware : Version;
      Date     : DateOnly;
   end record;
```

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Set_IPV4`

**Set_IPv4** sets the controller IPv4 address, netmask and gateway.

```
function Set_IPv4 (U       : UHPPOTE;
                   C       : Unsigned_32;
                   Addr    : GNAT.Sockets.Inet_Addr_Type;
                   Netmask : GNAT.Sockets.Inet_Addr_Type;
                   Gateway : GNAT.Sockets.Inet_Addr_Type;
                   Timeout : Duration) return Boolean;

function Set_IPv4 (U       : UHPPOTE;
                   C       : Controller;
                   Addr    : GNAT.Sockets.Inet_Addr_Type;
                   Netmask : GNAT.Sockets.Inet_Addr_Type;
                   Gateway : GNAT.Sockets.Inet_Addr_Type;
                   Timeout : Duration) return Boolean;

where:
- U        UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C        Unsigned_32     Controller serial number.
- C        Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.

- Addr     Inet_Addr_Type  Controller IPv4 address.
- Netmask  Inet_Addr_Type  Controller IPv4 subnet mask.
- Gateway  Inet_Addr_Type  Controller IPv4 gateway address.
- Timeout  Duration        Operation timeout (defaults to 2.5s).
```

Returns `True`.

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Get_Time`

**Get_Time** retrieves the controller date/time.

```
function Get_Time (U       : UHPPOTE;
                   C       : Unsigned_32; 
                   Timeout : Duration) return Controller_Record;

function Get_Time (U       : Uhppoted.Lib.UHPPOTE;
                   C       : Controller;
                   Timeout : Duration) return Controller_Record;

where:
- U        UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C        Unsigned_32     Controller serial number.
- C        Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
```

Returns a `DateTime`:
```
   type DateTime is record
      Year   : Unsigned_16;
      Month  : Unsigned_8;
      Day    : Unsigned_8;
      Hour   : Unsigned_8;
      Minute : Unsigned_8;
      Second : Unsigned_8;
   end record;
```

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Set_Time`

**Set_Time** sets the controller date/time.

```
function Set_Time (U       : UHPPOTE;
                   C       : Unsigned_32; 
                   DT      : DateTime;
                   Timeout : Duration) return Controller_Record;

function Set_Time (U       : Uhppoted.Lib.UHPPOTE;
                   C       : Controller;
                   DT      : DateTime;
                   Timeout : Duration) return Controller_Record;

where:
- U        UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C        Unsigned_32     Controller serial number.
- C        Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
- DT       DateTime        Date/time record initialised with the year, month, day, etc.

```

Returns a `DateTime`:
```
   type DateTime is record
      Year   : Unsigned_16;
      Month  : Unsigned_8;
      Day    : Unsigned_8;
      Hour   : Unsigned_8;
      Minute : Unsigned_8;
      Second : Unsigned_8;
   end record;
```

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Get_Listener`

**Get_Listener** retrieves the controller event listener address:port and auto-send interval.

```
function Get_Listener (U       : UHPPOTE;
                       C       : Unsigned_32; 
                       Timeout : Duration) return Listener_Record;

function Get_Listener (U       : Uhppoted.Lib.UHPPOTE;
                       C       : Controller;
                       Timeout : Duration) return Listener_Record;

where:
- U        UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C        Unsigned_32     Controller serial number.
- C        Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
```

Returns a `Listener_Record`:
```
   type Listener_Record is record
      AddrPort : GNAT.Sockets.Sock_Addr_Type;
      Interval : Unsigned_8;
   end record;
```

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Set_Listener`

**Set_Listener** set the controller event listener address:port and auto-send interval.

```
function Set_Listener (U        : UHPPOTE;
                       C        : Unsigned_32; 
                       Listener : GNAT.Sockets.Sock_Addr_Type;
                       Interval : Unsigned_8;
                       Timeout  : Duration) return Boolean;

function Set_Listener (U        : Uhppoted.Lib.UHPPOTE;
                       C        : Controller;
                       Listener : GNAT.Sockets.Sock_Addr_Type;
                       Interval : Unsigned_8;
                       Timeout  : Duration) return Boolean;

where:
- U         UHPPOTE                      UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C         Unsigned_32                  Controller serial number.
- C         Controller                   Controller record initialised with the controller ID, IPv4 address:port and protocol.
- Listener  GNAT.Sockets.Sock_Addr_Type  Event listener IPv4 address:port.
- Interval  Unsigned_8                   Auto-send interval (seconds). 0 for no auto-send.
```

Returns `True` if the controller event listener and auto-send interval were configured.

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Get_Status`

**Get_Status** retrieves the controller current state.

```
function Get_Status (U       : UHPPOTE;
                     C       : Unsigned_32; 
                     Timeout : Duration) return Controller_Status;

function Get_Status (U       : Uhppoted.Lib.UHPPOTE;
                     C       : Controller;
                     Timeout : Duration) return Controller_Status;

where:
- U        UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C        Unsigned_32     Controller serial number.
- C        Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
```

Returns a `Controller_Status`:
```
   type Controller_Status is record
      System_Date_Time : DateTime;
      Doors            : Doors_Type;
      Alarms           : Alarms_Type;
      System_Error     : Unsigned_8;
      Special_Info     : Unsigned_8;
      Event            : Event_Type;
   end record;

where:

   type Door_Type is record
      Open     : Boolean;
      Button   : Boolean;
      Unlocked : Boolean;
   end record;

   type Doors_Type is array (1 .. 4) of Door_Type;

   type Alarms_Type is record
      Flags       : Unsigned_8;
      Fire        : Boolean;
      Lock_Forced : Boolean;
   end record;

   type Event_Type is record
      Index          : Unsigned_32;
      Event          : Unsigned_8;
      Timestamp      : DateTime;
      Door           : Unsigned_8;
      Direction      : Unsigned_8;
      Card           : Unsigned_32;
      Access_Granted : Boolean;
      Reason         : Unsigned_8;
   end record;
```

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Get_Door`

**Get_Door** retrieves a door control mode and open delay.

```
function Get_Door (U       : UHPPOTE;
                   C       : Unsigned_32; 
                   Door    : Unsigned_8;
                   Timeout : Duration) return Door_Record;

function Get_Door (U       : Uhppoted.Lib.UHPPOTE;
                   C       : Controller;
                   Door    : Unsigned_8;
                   Timeout : Duration) return Returns a `Door_Record`:
;

where:
- U        UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C        Unsigned_32     Controller serial number.
- C        Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
- Door     Unsigned_8      Door ID [1..4].
```

Returns a `Door_Record`:
```
   type Door_Record is record
      Mode      : Unsigned_8;
      OpenDelay : Unsigned_8;
   end record;
```

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Set_Door`

**Set_Door** sets a door control mode and open delay.

```
function Set_Door (U         : UHPPOTE;
                   C         : Unsigned_32; 
                   Door      : Unsigned_8;
                   Mode      : Control_Mode;
                   OpenDelay : Unsigned_8;
                   Timeout   : Duration) return Door_Record;

function Set_Door (U         : Uhppoted.Lib.UHPPOTE;
                   C         : Controller;
                   Door      : Unsigned_8;
                   Mode      : Control_Mode;
                   OpenDelay : Unsigned_8;
                   Timeout   : Duration) return Door_Record;

where:
- U          UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C          Unsigned_32     Controller serial number.
- C          Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
- Door       Unsigned_8      Door ID [1..4].
- Mode       Control_Mode    Normally_Open, Normally_Closed or Controlled.
- OpenDelay  Unsigned_8      Door unlocked delay (seconds).
```

Returns a `Door_Record`:
```
   type Door_Record is record
      Mode      : Unsigned_8;
      OpenDelay : Unsigned_8;
   end record;
```

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Set_Door_Passcodes`

**Set_Door_Passcodes** sets the supervisor override codes for a door.

```
function Set_Door_Passcodes (U         : UHPPOTE;
                             C         : Unsigned_32; 
                             Door      : Unsigned_8;
                             Passcodes : Passcodes_List;
                             Timeout   : Duration) return Boolean;

function Set_Door_Passcodes (U         : Uhppoted.Lib.UHPPOTE;
                             C         : Controller;
                             Door      : Unsigned_8;
                             Passcodes : Passcodes_List;
                             Timeout   : Duration) return Boolean;

where:
- U          UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C          Unsigned_32     Controller serial number.
- C          Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
- Door       Unsigned_8      Door ID [1..4].
- Passcodes  Passcodes_List  List of up to 4 supervisor passcodes (in the range [0..999999]).
```

Returns `True` if the supervisor passcodes were accepted.

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Open_Door`

**Open_Door** remotely unlocks a door.

```
function Open_Door (U         : UHPPOTE;
                    C         : Unsigned_32; 
                    Door      : Unsigned_8;
                    Timeout   : Duration) return Boolean;

function Open_Door (U         : Uhppoted.Lib.UHPPOTE;
                    C         : Controller;
                    Door      : Unsigned_8;
                    Timeout   : Duration) return Boolean;

where:
- U          UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C          Unsigned_32     Controller serial number.
- C          Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
- Door       Unsigned_8      Door ID [1..4].
```

Returns `True` if the door was unlocked.

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Get_Cards`

**Get_Cards** retrieves the number of cards stored on a controller.

```
function Get_Cards (U         : UHPPOTE;
                    C         : Unsigned_32; 
                    Timeout   : Duration) return Unsigned_32;

function Get_Cards (U         : Uhppoted.Lib.UHPPOTE;
                    C         : Controller;
                    Timeout   : Duration) return Unsigned_32;

where:
- U          UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C          Unsigned_32     Controller serial number.
- C          Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
```

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Get_Card`

**Get_Card** retrieves the card record for a card number.

```
function Get_Card (U         : UHPPOTE;
                   C         : Unsigned_32;
                   Card      : Unsigned_32;
                   Timeout   : Duration) return Unsigned_32;

function Get_Card (U         : Uhppoted.Lib.UHPPOTE;
                   C         : Controller;
                   Card      : Unsigned_32
                   Timeout   : Duration) return Unsigned_32;

where:
- U          UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C          Unsigned_32     Controller serial number.
- C          Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
- Card       Unsigned_32     Card number.
```

Returns a `Card_Record`:
```
   typeCard_Record is record
      Card       : Unsigned_32;
      Start_Date : DateOnly;
      End_Date   : DateOnly;
      Door_1     : Unsigned_8;
      Door_2     : Unsigned_8;
      Door_3     : Unsigned_8;
      Door_4     : Unsigned_8;
      PIN        : Unsigned_32;
   end record;
```

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect


### `Get_Card_At_Index`

**Get_Card_at_Index** retrieves the card record stored at an index.

```
function Get_Card_At_Index (U         : UHPPOTE;
                            C         : Unsigned_32;
                            Index     : Unsigned_32;
                            Timeout   : Duration) return Unsigned_32;

function Get_Card_At_Index (U         : Uhppoted.Lib.UHPPOTE;
                            C         : Controller;
                            Index     : Unsigned_32
                            Timeout   : Duration) return Unsigned_32;

where:
- U          UHPPOTE         UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc.
- C          Unsigned_32     Controller serial number.
- C          Controller      Controller record initialised with the controller ID, IPv4 address:port and protocol.
- Index      Unsigned_32     Card record index.
```

Returns a `Card_Record`:
```
   typeCard_Record is record
      Card       : Unsigned_32;
      Start_Date : DateOnly;
      End_Date   : DateOnly;
      Door_1     : Unsigned_8;
      Door_2     : Unsigned_8;
      Door_3     : Unsigned_8;
      Door_4     : Unsigned_8;
      PIN        : Unsigned_32;
   end record;
```

Raises:
- `Timeout_Error` if the controller does not respond
- `Invalid_Response_Error` if the returned response is incorrect
- `Card_Not_Found_Error` if there is no record at the index
- `Card_Deleted_Error` if the record at the index has been deleted
