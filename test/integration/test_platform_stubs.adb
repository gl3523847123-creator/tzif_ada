pragma Ada_2022;
--  ======================================================================
--  Test_Platform_Stubs
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Integration tests for platform-specific functionality.
--
--  Notes:
--    - Windows platform is fully implemented using Win32 API
--    - This test verifies platform package structure and error handling
--    - On non-Windows platforms, the Windows package exports types only
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Infrastructure.Platform;
with TZif.Infrastructure.Platform.Windows;
with TZif.Domain.Error;
procedure Test_Platform_Stubs is
   use TZif.Infrastructure.Platform;
   use TZif.Infrastructure.Platform.Windows;
   use TZif.Domain.Error;
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  [PASS] " & Test_Name);
      else
         Put_Line ("  [FAIL] " & Test_Name);
      end if;
   end Assert;
   --  =====================================================================
   --  Test: Windows Read_Symbolic_Link (cross-platform build verification)
   --  =====================================================================
   --  On non-Windows platforms, this verifies the Windows package compiles.
   --  On Windows, this would query the actual system timezone via Win32 API
   --  and map it to an IANA zone ID using CLDR data.
   --  =====================================================================
   procedure Test_Windows_Platform is
      Result : constant Platform_String_Result :=
        Read_Symbolic_Link ("/some/path");
   begin
      Put_Line ("Test: Windows platform Read_Symbolic_Link");
      --  On non-Windows: may return error (no Win32 API)
      --  On Windows: returns Ok with IANA zone ID or Error if unknown TZ
      if String_Result.Is_Ok (Result) then
         Put_Line ("  Windows timezone detected: " &
           Platform_Strings.To_String (String_Result.Value (Result)));
         Assert (True, "Windows Read_Symbolic_Link returned Ok");
      else
         declare
            Err : constant Error_Type := String_Result.Error_Info (Result);
         begin
            Put_Line ("  Error (expected on non-Windows): " &
              Error_Message (Err));
            Assert
              (Err.Kind = IO_Error or Err.Kind = Internal_Error,
               "Error should be IO_Error or Internal_Error");
         end;
      end if;
   end Test_Windows_Platform;
begin
   --  Run all tests
   Test_Windows_Platform;
   --  Summary
   Put_Line ("====================================================");
   Put_Line
     ("  Results:" & Pass_Count'Image & " /" & Test_Count'Image & " passed");
   if Pass_Count = Test_Count then
      Put_Line ("  Status: ALL TESTS PASSED");
   else
      Put_Line ("  Status: FAILURES DETECTED");
   end if;
   Put_Line ("====================================================");
   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Pass_Count);
   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Platform_Stubs;