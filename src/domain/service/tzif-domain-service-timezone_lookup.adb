pragma Ada_2022;
--  ===========================================================================
--  Tzif.Domain.Service.Timezone_Lookup
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Timezone Lookup - domain logic and operations.
--
--  ===========================================================================

package body TZif.Domain.Service.Timezone_Lookup is

   --  ========================================================================
   --  Find_Type_Index_At_Time (Helper)
   --  ========================================================================
   --  Find the timezone type index applicable at the given time.
   --  Returns 0 if no transitions, otherwise returns the type_index from
   --  the last transition that occurred before or at the given time.
   --  ========================================================================

   function Find_Type_Index_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Natural
   is
   begin
      --  No transitions: use first type (index 0)
      if Transition_Count (Data) = 0 then
         return 0;
      end if;

      --  Time before first transition: use first transition's type
      if Time < Data.Transitions.First_Element.Time then
         return Data.Transitions.First_Element.Type_Index;
      end if;

      --  Binary search for last transition <= Time
      declare
         Low          : Natural := Data.Transitions.First_Index;
         High         : Natural := Data.Transitions.Last_Index;
         Mid          : Natural;
         Result_Index : Natural := Data.Transitions.First_Element.Type_Index;
      begin
         while Low <= High loop
            Mid := Low + (High - Low) / 2;

            if Data.Transitions.Element (Mid).Time <= Time then
               --  This transition applies, but there might be a later one
               Result_Index := Data.Transitions.Element (Mid).Type_Index;
               Low          := Mid + 1;
            else
               --  This transition is too late
               if Mid > Data.Transitions.First_Index then
                  High := Mid - 1;
               else
                  exit;
               end if;
            end if;
         end loop;

         return Result_Index;
      end;
   end Find_Type_Index_At_Time;

   --  ========================================================================
   --  Find_UTC_Offset_At_Time
   --  ========================================================================

   function Find_UTC_Offset_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type)
      return UTC_Offset_Option
   is
      Type_Index : constant Natural := Find_Type_Index_At_Time (Data, Time);
   begin
      --  Type indices from TZif file directly match vector indices
      --  (vector starts at index 0, same as TZif type indices)
      --  Return None if no timezone types available
      if Natural (Data.Timezone_Types.Length) = 0 then
         return UTC_Offset_Options.None;
      elsif Type_Index in
          Data.Timezone_Types.First_Index .. Data.Timezone_Types.Last_Index
      then
         return
           UTC_Offset_Options.New_Some
             (Data.Timezone_Types.Element (Type_Index).UTC_Offset);
      else
         --  Invalid type index - return None rather than silent fallback
         return UTC_Offset_Options.None;
      end if;
   end Find_UTC_Offset_At_Time;

   --  ========================================================================
   --  Is_DST_At_Time
   --  ========================================================================

   function Is_DST_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Boolean_Option
   is
      Type_Index : constant Natural := Find_Type_Index_At_Time (Data, Time);
   begin
      --  Type indices from TZif file directly match vector indices
      --  Return None if no timezone types available
      if Natural (Data.Timezone_Types.Length) = 0 then
         return Boolean_Options.None;
      elsif Type_Index in
          Data.Timezone_Types.First_Index .. Data.Timezone_Types.Last_Index
      then
         return
           Boolean_Options.New_Some
             (Data.Timezone_Types.Element (Type_Index).Is_DST);
      else
         --  Invalid type index - return None rather than silent fallback
         return Boolean_Options.None;
      end if;
   end Is_DST_At_Time;

   --  ========================================================================
   --  Get_Abbreviation_At_Time
   --  ========================================================================

   function Get_Abbreviation_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type)
      return Abbreviation_Option
   is
      Type_Index : constant Natural := Find_Type_Index_At_Time (Data, Time);
   begin
      --  Type indices from TZif file directly match vector indices
      --  Return None if no timezone types available
      if Natural (Data.Timezone_Types.Length) = 0 then
         return Abbreviation_Options.None;
      elsif Type_Index in
          Data.Timezone_Types.First_Index .. Data.Timezone_Types.Last_Index
      then
         return
           Abbreviation_Options.New_Some
             (Data.Timezone_Types.Element (Type_Index).Abbreviation);
      else
         --  Invalid type index - return None rather than silent fallback
         return Abbreviation_Options.None;
      end if;
   end Get_Abbreviation_At_Time;

end TZif.Domain.Service.Timezone_Lookup;
