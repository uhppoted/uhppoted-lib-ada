# API
- [`Find_Controllers`](#find_controllers)
- [`Get_Controller`](#get_controller)
- [`Set_IPv4`](#set_ipv4)

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

