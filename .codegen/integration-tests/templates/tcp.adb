with AUnit.Assertions;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub;
with Uhppoted.Lib.Integration_Tests.Expected;

package body Uhppoted.Lib.Integration_Tests.TCP is
   use AUnit.Assertions;

   U : constant UHPPOTE := (
      Bind_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("0.0.0.0"),
         Port => 0),

      Broadcast_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("255.255.255.255"),
         Port => 60014),

      Listen_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("0.0.0.0"),
         Port => 60001),

      Debug => False);

   overriding function Name (T : Integration_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("TCP tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
{{- range $ix,$test := .Tests }}
      Register_Routine (T, {{ printf "Test_%v'Access," .Name | rpad 32 }} "{{ .Name }}");{{end}}
   end Register_Tests;

   task body Listen is
   begin
      Uhppoted.Lib.Integration_Tests.Stub.ListenTCP (Port => 60003);
   end Listen;
{{ range $ix,$test := .Tests }}
   procedure Test_{{ printf "%v" .Name }} (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => {{ index .Args 0 }},
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => 60003),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant {{ .Returns.Type }} := {{ .Function }} (U, C{{ range $arg := (slice .Args  1) }}, {{ $arg }}{{ end }});
   begin
      Assert (V = Expected.{{ .Name }}, "invalid result" & V'Image);
   end Test_{{ printf "%v" .Name}};
{{end}}
end Uhppoted.Lib.Integration_Tests.TCP;
