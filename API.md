# API
- [`FindControllers`](#findcontrollers)

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

### Notes


## Functions

### `Find_Controllers`
Find_Controllers_ retrieves a list of all UHPPOTE controllers accessible via UDP broadcast on the local LAN.
```
function Find_Controllers (U : Uhppoted.Lib.UHPPOTE) return Uhppoted.Lib.Controller_List;

where:
- U   UHPPOTE   UHPPOTE struct initialised with the bind, broadcast and listen addresses, etc
```
Returns a list of `Controller`:
```
   type Controller is record
      ID       : Unsigned_32;
      Address  : IPv4;
      Netmask  : IPv4;
      Gateway  : IPv4;
      MAC      : MAC_Address;
      Firmware : Version;
      Date     : DateOnly;
   end record;
```


